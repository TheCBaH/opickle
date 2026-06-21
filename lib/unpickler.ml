(* The pickle stack machine.

   One pass over the byte stream (via {!Src}), executing opcodes against a value
   stack, a mark "metastack", and a memo table, until [STOP] yields the final
   value.  Opcode set and per-opcode behavior follow CPython's
   [Lib/pickle.py] / [Lib/pickletools.py].

   Reconstruction opcodes (GLOBAL, REDUCE, NEWOBJ, BUILD, ...) are not executed;
   they build the structural {!Value.t} nodes [Global]/[Reduce]/[Object]. *)

open Value

(* ── String / number decoding helpers ───────────────────────────────────── *)

(* Encode a Unicode scalar value as UTF-8 into [buf]. *)
let add_utf8 buf cp =
  if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else if cp < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

(* latin-1 (one byte per code point) -> UTF-8.  Used for proto-0/1 [str]. *)
let latin1_to_utf8 s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> add_utf8 buf (Char.code c)) s;
  Buffer.contents buf

let is_octal c = c >= '0' && c <= '7'

let hex_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

(* Decode a Python repr-style byte string (the body between the quotes of a
   proto-0 STRING): undo backslash escapes, yielding raw bytes. *)
let escape_decode s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c <> '\\' || !i = n - 1 then (
      Buffer.add_char buf c;
      incr i)
    else begin
      incr i;
      let e = s.[!i] in
      match e with
      | 'n' ->
          Buffer.add_char buf '\n';
          incr i
      | 't' ->
          Buffer.add_char buf '\t';
          incr i
      | 'r' ->
          Buffer.add_char buf '\r';
          incr i
      | '\\' ->
          Buffer.add_char buf '\\';
          incr i
      | '\'' ->
          Buffer.add_char buf '\'';
          incr i
      | '"' ->
          Buffer.add_char buf '"';
          incr i
      | 'a' ->
          Buffer.add_char buf '\007';
          incr i
      | 'b' ->
          Buffer.add_char buf '\b';
          incr i
      | 'f' ->
          Buffer.add_char buf '\012';
          incr i
      | 'v' ->
          Buffer.add_char buf '\011';
          incr i
      | '0' .. '7' ->
          let j = ref !i and v = ref 0 in
          while !j < n && !j < !i + 3 && is_octal s.[!j] do
            v := (!v * 8) + (Char.code s.[!j] - Char.code '0');
            incr j
          done;
          Buffer.add_char buf (Char.chr (!v land 0xFF));
          i := !j
      | 'x'
        when !i + 2 < n && hex_val s.[!i + 1] >= 0 && hex_val s.[!i + 2] >= 0 ->
          Buffer.add_char buf
            (Char.chr ((hex_val s.[!i + 1] * 16) + hex_val s.[!i + 2]));
          i := !i + 3
      | _ ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf e;
          incr i
    end
  done;
  Buffer.contents buf

(* Strip one layer of surrounding single- or double-quote characters
   (proto-0 STRING / UNICODE). *)
let strip_quotes s =
  let n = String.length s in
  if n >= 2 && (s.[0] = '\'' || s.[0] = '"') && s.[n - 1] = s.[0] then
    String.sub s 1 (n - 2)
  else s

(* Decode a proto-0 UNICODE line using the Python raw-unicode-escape codec:
   bytes pass through as latin-1, while [\uXXXX] / [\UXXXXXXXX] denote code
   points. *)
let raw_unicode_escape s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  (* read [count] hex digits starting at [start] *)
  let read_hex start count =
    let v = ref 0 in
    for k = 0 to count - 1 do
      let h = if start + k < n then hex_val s.[start + k] else -1 in
      if h < 0 then raise Exit;
      v := (!v * 16) + h
    done;
    !v
  in
  (try
     while !i < n do
       let c = s.[!i] in
       if c = '\\' && !i + 1 < n && s.[!i + 1] = 'u' then (
         add_utf8 buf (read_hex (!i + 2) 4);
         i := !i + 6)
       else if c = '\\' && !i + 1 < n && s.[!i + 1] = 'U' then (
         add_utf8 buf (read_hex (!i + 2) 8);
         i := !i + 10)
       else (
         add_utf8 buf (Char.code c);
         incr i)
     done
   with Exit ->
     (* malformed escape: fall back to latin-1 for the remainder *)
     while !i < n do
       add_utf8 buf (Char.code s.[!i]);
       incr i
     done);
  Buffer.contents buf

(* ── Integer decoding ───────────────────────────────────────────────────── *)

(* Long division of a big-endian decimal digit array by [d]; returns the
   remainder and overwrites [digits] with the quotient (still big-endian). *)
let divmod_digits digits d =
  let rem = ref 0 in
  Array.iteri
    (fun i x ->
      let cur = (!rem * 10) + x in
      digits.(i) <- cur / d;
      rem := cur mod d)
    digits;
  !rem

let all_zero digits = Array.for_all (fun x -> x = 0) digits

(* Build a {!Value.t} from a decimal integer literal (sign optional). *)
let value_of_decimal s =
  match Int64.of_string_opt s with
  | Some i -> Int i
  | None ->
      let negative = String.length s > 0 && s.[0] = '-' in
      let start =
        if negative || (String.length s > 0 && s.[0] = '+') then 1 else 0
      in
      let digits =
        Array.init
          (String.length s - start)
          (fun k -> Char.code s.[start + k] - Char.code '0')
      in
      let bytes = Buffer.create 16 in
      while not (all_zero digits) do
        Buffer.add_char bytes (Char.chr (divmod_digits digits 256))
      done;
      if Buffer.length bytes = 0 then Buffer.add_char bytes '\000';
      Bigint { negative; magnitude = Buffer.contents bytes }

(* Build a {!Value.t} from a two's-complement little-endian byte string
   (LONG1 / LONG4). *)
let value_of_long_bytes data =
  let n = String.length data in
  if n = 0 then Int 0L
  else begin
    let negative = Char.code data.[n - 1] land 0x80 <> 0 in
    if n <= 8 then begin
      (* sign-extend into an int64 *)
      let v = ref 0L in
      for i = n - 1 downto 0 do
        v :=
          Int64.logor (Int64.shift_left !v 8)
            (Int64.of_int (Char.code data.[i]))
      done;
      if negative && n < 8 then
        v := Int64.logor !v (Int64.shift_left (-1L) (n * 8));
      Int !v
    end
    else if not negative then Bigint { negative = false; magnitude = data }
    else begin
      (* magnitude = two's complement: invert and add one *)
      let b = Bytes.of_string data in
      let carry = ref 1 in
      for i = 0 to n - 1 do
        let v = (lnot (Char.code (Bytes.get b i)) land 0xFF) + !carry in
        Bytes.set b i (Char.chr (v land 0xFF));
        carry := v lsr 8
      done;
      Bigint { negative = true; magnitude = Bytes.unsafe_to_string b }
    end
  end

(* ── The machine ────────────────────────────────────────────────────────── *)

type t = {
  src : Src.t;
  mutable stack : Value.t list;  (** head is the top of stack *)
  mutable metastack : Value.t list list;  (** saved stacks, one per open MARK *)
  memo : (int, Value.t) Hashtbl.t;
}

let err m fmt = Error.raise_at (Src.pos m.src) fmt
let push m v = m.stack <- v :: m.stack

let pop m =
  match m.stack with
  | x :: r ->
      m.stack <- r;
      x
  | [] -> err m "pop from empty stack"

let top m =
  match m.stack with x :: _ -> x | [] -> err m "expected a value on the stack"

let mark m =
  m.metastack <- m.stack :: m.metastack;
  m.stack <- []

(* Items pushed since the most recent MARK, in insertion order; restores the
   stack saved by that MARK. *)
let pop_mark m =
  let items = List.rev m.stack in
  (match m.metastack with
  | s :: r ->
      m.stack <- s;
      m.metastack <- r
  | [] -> err m "pop_mark without a matching mark");
  items

let memo_get m i =
  match Hashtbl.find_opt m.memo i with
  | Some v -> v
  | None -> err m "memo key %d not found" i

let memo_put m i = Hashtbl.replace m.memo i (top m)

let to_pairs m items =
  let rec loop = function
    | k :: v :: rest -> (k, v) :: loop rest
    | [] -> []
    | [ _ ] -> err m "odd number of items for dict/setitems"
  in
  loop items

let expect_list m = function
  | List r -> r
  | _ -> err m "APPEND(S) target is not a list"

let expect_dict m = function
  | Dict r -> r
  | _ -> err m "SETITEM(S) target is not a dict"

let expect_set m = function
  | Set r -> r
  | _ -> err m "ADDITEMS target is not a set"

let run_op m op =
  let s = m.src in
  let c = Char.chr op in
  if c = Opcode.proto then begin
    let v = Src.uint1 s in
    if v > 5 then err m "unsupported pickle protocol %d" v
  end
  else if c = Opcode.frame then ignore (Src.uint8 s)
  else if c = Opcode.stop then raise Exit (* integers / bool / none *)
  else if c = Opcode.none then push m None
  else if c = Opcode.newtrue then push m (Bool true)
  else if c = Opcode.newfalse then push m (Bool false)
  else if c = Opcode.int then
    begin match Src.line s with
    | "00" -> push m (Bool false)
    | "01" -> push m (Bool true)
    | l -> push m (value_of_decimal l)
    end
  else if c = Opcode.long then begin
    let l = Src.line s in
    let l =
      if String.length l > 0 && l.[String.length l - 1] = 'L' then
        String.sub l 0 (String.length l - 1)
      else l
    in
    push m (value_of_decimal l)
  end
  else if c = Opcode.binint then push m (Int (Int64.of_int (Src.int4 s)))
  else if c = Opcode.binint1 then push m (Int (Int64.of_int (Src.uint1 s)))
  else if c = Opcode.binint2 then push m (Int (Int64.of_int (Src.uint2 s)))
  else if c = Opcode.long1 then
    push m (value_of_long_bytes (Src.take s (Src.uint1 s)))
  else if c = Opcode.long4 then
    push m (value_of_long_bytes (Src.take s (Src.uint4 s))) (* floats *)
  else if c = Opcode.float then push m (Float (float_of_string (Src.line s)))
  else if c = Opcode.binfloat then push m (Float (Src.float8_be s)) (* bytes *)
  else if c = Opcode.binbytes then push m (Bytes (Src.take s (Src.uint4 s)))
  else if c = Opcode.short_binbytes then
    push m (Bytes (Src.take s (Src.uint1 s)))
  else if c = Opcode.binbytes8 then push m (Bytes (Src.take s (Src.uint8 s)))
  else if c = Opcode.bytearray8 then
    push m (Bytearray (Src.take s (Src.uint8 s))) (* strings (py2 str) *)
  else if c = Opcode.string then
    push m (Str (latin1_to_utf8 (escape_decode (strip_quotes (Src.line s)))))
  else if c = Opcode.binstring then
    push m (Str (latin1_to_utf8 (Src.take s (Src.uint4 s))))
  else if c = Opcode.short_binstring then
    push m (Str (latin1_to_utf8 (Src.take s (Src.uint1 s)))) (* unicode *)
  else if c = Opcode.unicode then push m (Str (raw_unicode_escape (Src.line s)))
  else if c = Opcode.short_binunicode then
    push m (Str (Src.take s (Src.uint1 s)))
  else if c = Opcode.binunicode then push m (Str (Src.take s (Src.uint4 s)))
  else if c = Opcode.binunicode8 then push m (Str (Src.take s (Src.uint8 s)))
    (* buffers (proto 5, out-of-band) *)
  else if c = Opcode.next_buffer then
    err m "NEXT_BUFFER: out-of-band buffers are not supported"
  else if c = Opcode.readonly_buffer then ()
    (* wraps the top object; structurally a no-op *)
    (* lists *)
  else if c = Opcode.empty_list then push m (List (ref []))
  else if c = Opcode.list then push m (List (ref (pop_mark m)))
  else if c = Opcode.append then begin
    let x = pop m in
    let r = expect_list m (top m) in
    r := !r @ [ x ]
  end
  else if c = Opcode.appends then begin
    let items = pop_mark m in
    let r = expect_list m (top m) in
    r := !r @ items
  end (* tuples *)
  else if c = Opcode.empty_tuple then push m (Tuple [||])
  else if c = Opcode.tuple then push m (Tuple (Array.of_list (pop_mark m)))
  else if c = Opcode.tuple1 then
    let a = pop m in
    push m (Tuple [| a |])
  else if c = Opcode.tuple2 then
    let b = pop m in
    let a = pop m in
    push m (Tuple [| a; b |])
  else if c = Opcode.tuple3 then
    let cc = pop m in
    let b = pop m in
    let a = pop m in
    push m (Tuple [| a; b; cc |]) (* dicts *)
  else if c = Opcode.empty_dict then push m (Dict (ref []))
  else if c = Opcode.dict then push m (Dict (ref (to_pairs m (pop_mark m))))
  else if c = Opcode.setitem then begin
    let v = pop m in
    let k = pop m in
    let r = expect_dict m (top m) in
    r := !r @ [ (k, v) ]
  end
  else if c = Opcode.setitems then begin
    let pairs = to_pairs m (pop_mark m) in
    let r = expect_dict m (top m) in
    r := !r @ pairs
  end (* sets *)
  else if c = Opcode.empty_set then push m (Set (ref []))
  else if c = Opcode.additems then begin
    let items = pop_mark m in
    let r = expect_set m (top m) in
    r := !r @ items
  end
  else if c = Opcode.frozenset then push m (Frozenset (pop_mark m))
    (* stack ops *)
  else if c = Opcode.pop then ignore (pop m)
  else if c = Opcode.dup then push m (top m)
  else if c = Opcode.mark then mark m
  else if c = Opcode.pop_mark then ignore (pop_mark m) (* memo *)
  else if c = Opcode.get then push m (memo_get m (int_of_string (Src.line s)))
  else if c = Opcode.binget then push m (memo_get m (Src.uint1 s))
  else if c = Opcode.long_binget then push m (memo_get m (Src.uint4 s))
  else if c = Opcode.put then memo_put m (int_of_string (Src.line s))
  else if c = Opcode.binput then memo_put m (Src.uint1 s)
  else if c = Opcode.long_binput then memo_put m (Src.uint4 s)
  else if c = Opcode.memoize then memo_put m (Hashtbl.length m.memo)
    (* extension registry *)
  else if c = Opcode.ext1 then push m (Ext (Src.uint1 s))
  else if c = Opcode.ext2 then push m (Ext (Src.uint2 s))
  else if c = Opcode.ext4 then push m (Ext (Src.uint4 s))
    (* globals / reduce / object construction *)
  else if c = Opcode.global then begin
    let modul = Src.line s in
    let name = Src.line s in
    push m (Global { modul; name })
  end
  else if c = Opcode.stack_global then begin
    let name = pop m in
    let modul = pop m in
    match (modul, name) with
    | Str modul, Str name -> push m (Global { modul; name })
    | _ -> err m "STACK_GLOBAL expects two strings"
  end
  else if c = Opcode.reduce then begin
    let args = pop m in
    let func = pop m in
    push m (Reduce { func; args })
  end
  else if c = Opcode.build then begin
    let state = pop m in
    match top m with
    | Object o -> o.state <- Some state
    | inst ->
        ignore (pop m);
        push m (Object { cls = inst; args = Tuple [||]; state = Some state })
  end
  else if c = Opcode.newobj then begin
    let args = pop m in
    let cls = pop m in
    push m (Object { cls; args; state = None })
  end
  else if c = Opcode.newobj_ex then begin
    let kwargs = pop m in
    let args = pop m in
    let cls = pop m in
    let args =
      match kwargs with
      | Dict { contents = [] } -> args
      | _ -> Tuple [| args; kwargs |]
    in
    push m (Object { cls; args; state = None })
  end
  else if c = Opcode.inst then begin
    let modul = Src.line s in
    let name = Src.line s in
    let args = pop_mark m in
    push m
      (Object
         {
           cls = Global { modul; name };
           args = Tuple (Array.of_list args);
           state = None;
         })
  end
  else if c = Opcode.obj then
    begin match pop_mark m with
    | cls :: args ->
        push m (Object { cls; args = Tuple (Array.of_list args); state = None })
    | [] -> err m "OBJ with empty mark"
    end (* persistent ids *)
  else if c = Opcode.persid then push m (Persistent (Str (Src.line s)))
  else if c = Opcode.binpersid then
    let id = pop m in
    push m (Persistent id)
  else err m "unknown opcode 0x%02x" op

(* Decode a single pickle from [src]. *)
let run src =
  let m = { src; stack = []; metastack = []; memo = Hashtbl.create 64 } in
  let rec loop () =
    match run_op m (Src.byte src) with
    | () -> loop ()
    | exception Exit -> top m (* STOP *)
  in
  loop ()
