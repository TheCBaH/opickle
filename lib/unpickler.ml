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
let escape_decode ~fail s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c <> '\\' then (
      Buffer.add_char buf c;
      incr i)
    else begin
      incr i;
      if !i = n then fail "trailing escape in STRING";
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
      | 'x' -> fail "invalid hexadecimal escape in STRING"
      | _ ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf e;
          incr i
    end
  done;
  Buffer.contents buf

(* Strip one layer of surrounding single- or double-quote characters
   (proto-0 STRING / UNICODE). *)
let strip_quotes ~fail s =
  let n = String.length s in
  if n >= 2 && (s.[0] = '\'' || s.[0] = '"') && s.[n - 1] = s.[0] then
    String.sub s 1 (n - 2)
  else fail "STRING requires matching quotes"

(* Decode a proto-0 UNICODE line using the Python raw-unicode-escape codec:
   bytes pass through as latin-1, while [\uXXXX] / [\UXXXXXXXX] denote code
   points. *)
let raw_unicode_escape ~fail s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  (* read [count] hex digits starting at [start] *)
  let read_hex start count =
    let v = ref 0 in
    for k = 0 to count - 1 do
      let h = if k < n - start then hex_val s.[start + k] else -1 in
      if h < 0 then fail "invalid raw Unicode escape";
      v := (!v * 16) + h;
      if !v > 0x10FFFF then fail "Unicode code point out of range"
    done;
    !v
  in
  while !i < n do
    let c = s.[!i] in
    if c = '\\' && !i + 1 < n then
      begin match s.[!i + 1] with
      | 'u' ->
          add_utf8 buf (read_hex (!i + 2) 4);
          i := !i + 6
      | 'U' ->
          add_utf8 buf (read_hex (!i + 2) 8);
          i := !i + 10
      | '\\' ->
          Buffer.add_string buf "\\\\";
          i := !i + 2
      | _ ->
          Buffer.add_char buf c;
          incr i
      end
    else begin
      add_utf8 buf (Char.code c);
      incr i
    end
  done;
  Buffer.contents buf

let valid_pickle_unicode s =
  let len = String.length s in
  let code i = Char.code s.[i] in
  let cont i = code i land 0xC0 = 0x80 in
  let rec loop i =
    if i = len then true
    else
      let first = code i in
      if first < 0x80 then loop (i + 1)
      else if first >= 0xC2 && first <= 0xDF then
        len - i >= 2 && cont (i + 1) && loop (i + 2)
      else if first >= 0xE0 && first <= 0xEF then
        len - i >= 3
        && cont (i + 1)
        && cont (i + 2)
        && (first <> 0xE0 || code (i + 1) >= 0xA0)
        && loop (i + 3)
      else if first >= 0xF0 && first <= 0xF4 then
        len - i >= 4
        && cont (i + 1)
        && cont (i + 2)
        && cont (i + 3)
        && (first <> 0xF0 || code (i + 1) >= 0x90)
        && (first <> 0xF4 || code (i + 1) < 0x90)
        && loop (i + 4)
      else false
  in
  loop 0

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
let value_of_decimal ~charge ~fail s =
  let len = String.length s in
  let negative = len > 0 && s.[0] = '-' in
  let start = if len > 0 && (negative || s.[0] = '+') then 1 else 0 in
  if start = len then fail ();
  for i = start to len - 1 do
    if s.[i] < '0' || s.[i] > '9' then fail ()
  done;
  match Int64.of_string_opt s with
  | Some i -> Int i
  | None ->
      let digits =
        Array.init (len - start) (fun k ->
            Char.code s.[start + k] - Char.code '0')
      in
      let bytes = Buffer.create 16 in
      while not (all_zero digits) do
        charge (Array.length digits);
        Buffer.add_char bytes (Char.chr (divmod_digits digits 256))
      done;
      if Buffer.length bytes = 0 then Buffer.add_char bytes '\000';
      Bigint { negative; magnitude = Buffer.contents bytes }

(* Build a {!Value.t} from a two's-complement little-endian byte string
   (LONG1 / LONG4). *)
let value_of_long_bytes data =
  let rec minimal n =
    if n < 2 then n
    else
      let hi = Char.code data.[n - 1] in
      let next_negative = Char.code data.[n - 2] land 0x80 <> 0 in
      if (hi = 0 && not next_negative) || (hi = 0xFF && next_negative) then
        minimal (n - 1)
      else n
  in
  let n = minimal (String.length data) in
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
    else if not negative then begin
      let size = if data.[n - 1] = '\000' then n - 1 else n in
      let magnitude =
        if size = String.length data then data else String.sub data 0 size
      in
      Bigint { negative = false; magnitude }
    end
    else begin
      (* magnitude = two's complement: invert and add one *)
      let b = Bytes.of_string (String.sub data 0 n) in
      let carry = ref 1 in
      for i = 0 to n - 1 do
        let v = (lnot (Char.code (Bytes.get b i)) land 0xFF) + !carry in
        Bytes.set b i (Char.chr (v land 0xFF));
        carry := v lsr 8
      done;
      let size = if Bytes.get b (n - 1) = '\000' then n - 1 else n in
      Bigint { negative = true; magnitude = Bytes.sub_string b 0 size }
    end
  end

(* ── The machine ────────────────────────────────────────────────────────── *)

type legacy_strings = Latin1 | Raw_bytes

type t = {
  src : Src.t;
  mutable stack : Value.t list;  (** head is the top of stack *)
  mutable stack_len : int;
  mutable live_items : int;
  mutable metastack : (Value.t list * int) list;
  mutable marks : int;
  mutable opcodes : int;
  mutable work : int;
  memo : (int64, Value.t) Hashtbl.t;
  legacy_strings : legacy_strings;
  strict_stack : bool;
}

exception Stop

let err m fmt = Error.raise_at (Src.pos m.src) fmt

let limited m what =
  Error.raise_at ~kind:Limit (Src.pos m.src) "%s limit exceeded" what

let charge m n =
  if n > m.work then limited m "decode work";
  m.work <- m.work - n

let push m v =
  if m.live_items >= (Src.limits m.src).stack_items then limited m "stack item";
  m.stack <- v :: m.stack;
  m.stack_len <- m.stack_len + 1;
  m.live_items <- m.live_items + 1

let emit m value =
  let node =
    try Value.make value
    with Value.Container_limit -> limited m "container item"
  in
  push m node

let pop m =
  match m.stack with
  | x :: r ->
      m.stack <- r;
      m.stack_len <- m.stack_len - 1;
      m.live_items <- m.live_items - 1;
      x
  | [] -> err m "pop from empty stack"

let top m =
  match m.stack with x :: _ -> x | [] -> err m "expected a value on the stack"

let mark m =
  if m.marks >= (Src.limits m.src).marks then limited m "mark";
  m.metastack <- (m.stack, m.stack_len) :: m.metastack;
  m.stack <- [];
  m.stack_len <- 0;
  m.marks <- m.marks + 1

let drop_mark m =
  match m.metastack with
  | (stack, len) :: rest ->
      m.live_items <- m.live_items - m.stack_len;
      m.stack <- stack;
      m.stack_len <- len;
      m.metastack <- rest;
      m.marks <- m.marks - 1
  | [] -> err m "pop_mark without a matching mark"

(* Items pushed since the most recent MARK, in insertion order; restores the
   stack saved by that MARK. *)
let pop_mark m =
  let items = m.stack in
  if m.stack_len > Sys.max_array_length then limited m "container item";
  charge m m.stack_len;
  drop_mark m;
  List.rev items

let memo_get m i =
  match Hashtbl.find_opt m.memo i with
  | Some v -> v
  | None -> err m "memo key %Ld not found" i

let memo_put m i =
  let value = top m in
  if
    Hashtbl.length m.memo >= (Src.limits m.src).memo_entries
    && not (Hashtbl.mem m.memo i)
  then limited m "memo entry";
  Hashtbl.replace m.memo i value

let to_pairs m items =
  let rec loop acc = function
    | k :: v :: rest -> loop ((k, v) :: acc) rest
    | [] -> List.rev acc
    | [ _ ] -> err m "odd number of items for dict/setitems"
  in
  loop [] items

let decimal m s =
  let s = String.trim s in
  charge m (String.length s);
  if String.length s > 0 then begin
    let sign = if s.[0] = '-' || s.[0] = '+' then 1 else 0 in
    if
      String.length s - sign
      > min Sys.max_array_length (Src.limits m.src).text_digits
    then limited m "decimal digit"
  end;
  value_of_decimal ~charge:(charge m)
    ~fail:(fun () -> err m "invalid decimal integer")
    s

let is_digit c = c >= '0' && c <= '9'

let text_float m raw =
  let s = String.trim raw in
  let len = String.length s in
  let skip_sign i =
    if i < len && (s.[i] = '+' || s.[i] = '-') then i + 1 else i
  in
  let rec digits i = if i < len && is_digit s.[i] then digits (i + 1) else i in
  let first = skip_sign 0 in
  let whole = digits first in
  let frac =
    if whole < len && s.[whole] = '.' then digits (whole + 1) else whole
  in
  let valid =
    if whole > first || frac - whole > 1 then
      if frac < len && (s.[frac] = 'e' || s.[frac] = 'E') then
        let start = skip_sign (frac + 1) in
        start < len && is_digit s.[start] && digits start = len
      else frac = len
    else
      match String.lowercase_ascii (String.sub s first (len - first)) with
      | "nan" | "inf" | "infinity" -> true
      | _ -> false
  in
  if not valid then err m "invalid floating-point number";
  match float_of_string_opt s with
  | Some number -> number
  | None -> err m "invalid floating-point number"

let text_memo_id m s =
  let s = String.trim s in
  let len = String.length s in
  let start = if len > 0 && s.[0] = '+' then 1 else 0 in
  let rec digits i = i = len || (is_digit s.[i] && digits (i + 1)) in
  if start = len || not (digits start) then err m "invalid decimal memo index";
  match Int64.of_string_opt s with
  | Some key when key >= 0L -> key
  | _ -> err m "memo index out of range"

let legacy_string m raw =
  match m.legacy_strings with
  | Latin1 -> Str (latin1_to_utf8 raw)
  | Raw_bytes -> Bytes raw

let unicode m raw =
  if valid_pickle_unicode raw then Str raw else err m "invalid Unicode payload"

let ascii m label s =
  if not (String.for_all (fun c -> Char.code c < 128) s) then
    err m "%s requires ASCII" label;
  s

let tuple_args m label args =
  if not (Value.is_tuple args) then err m "%s expects tuple arguments" label;
  args

let extension m code =
  if code <= 0l then err m "extension code must be positive";
  emit m (Ext code)

let mutate m label target operation =
  charge m 1;
  let accepted =
    try Value.apply target operation
    with Value.Container_limit -> limited m "container item"
  in
  if not accepted then err m "%s does not accept this target" label

let instance m constructor = emit m (Object { constructor; operations = [] })

let run_op m op =
  let s = m.src in
  let c = Char.chr op in
  if c = Opcode.proto then begin
    let v = Src.uint1 s in
    if v > 5 then
      Error.raise_at ~kind:Unsupported (Src.pos s)
        "unsupported pickle protocol %d" v
  end
  else if c = Opcode.frame then Src.frame s
  else if c = Opcode.stop then begin
    Src.require_frame_boundary s "STOP";
    ignore (top m);
    if m.strict_stack && (m.stack_len <> 1 || m.marks <> 0) then
      err m "STOP expects one value and no open marks";
    raise Stop
  end (* integers / bool / none *)
  else if c = Opcode.none then emit m None
  else if c = Opcode.newtrue then emit m (Bool true)
  else if c = Opcode.newfalse then emit m (Bool false)
  else if c = Opcode.int then
    begin match Src.line s with
    | "00" -> emit m (Bool false)
    | "01" -> emit m (Bool true)
    | l -> emit m (decimal m l)
    end
  else if c = Opcode.long then begin
    let l = String.trim (Src.line s) in
    let l =
      if String.length l > 0 && l.[String.length l - 1] = 'L' then
        String.sub l 0 (String.length l - 1)
      else l
    in
    emit m (decimal m l)
  end
  else if c = Opcode.binint then emit m (Int (Int64.of_int32 (Src.int32 s)))
  else if c = Opcode.binint1 then emit m (Int (Int64.of_int (Src.uint1 s)))
  else if c = Opcode.binint2 then emit m (Int (Int64.of_int (Src.uint2 s)))
  else if c = Opcode.long1 then
    emit m (value_of_long_bytes (Src.payload s (Src.uint1 s)))
  else if c = Opcode.long4 then
    emit m (value_of_long_bytes (Src.payload s (Src.length32 s))) (* floats *)
  else if c = Opcode.float then emit m (Float (text_float m (Src.line s)))
  else if c = Opcode.binfloat then emit m (Float (Src.float8_be s)) (* bytes *)
  else if c = Opcode.binbytes then emit m (Bytes (Src.payload s (Src.uint4 s)))
  else if c = Opcode.short_binbytes then
    emit m (Bytes (Src.payload s (Src.uint1 s)))
  else if c = Opcode.binbytes8 then emit m (Bytes (Src.payload s (Src.uint8 s)))
  else if c = Opcode.bytearray8 then
    emit m (Bytearray (Src.payload s (Src.uint8 s))) (* strings (py2 str) *)
  else if c = Opcode.string then begin
    let body = strip_quotes ~fail:(fun msg -> err m "%s" msg) (Src.line s) in
    let raw = escape_decode ~fail:(fun msg -> err m "%s" msg) body in
    emit m (legacy_string m raw)
  end
  else if c = Opcode.binstring then
    emit m (legacy_string m (Src.payload s (Src.length32 s)))
  else if c = Opcode.short_binstring then
    emit m (legacy_string m (Src.payload s (Src.uint1 s))) (* unicode *)
  else if c = Opcode.unicode then
    emit m
      (Str (raw_unicode_escape ~fail:(fun msg -> err m "%s" msg) (Src.line s)))
  else if c = Opcode.short_binunicode then
    emit m (unicode m (Src.payload s (Src.uint1 s)))
  else if c = Opcode.binunicode then
    emit m (unicode m (Src.payload s (Src.uint4 s)))
  else if c = Opcode.binunicode8 then
    emit m (unicode m (Src.payload s (Src.uint8 s)))
    (* buffers (proto 5, out-of-band) *)
  else if c = Opcode.next_buffer then
    Error.raise_at ~kind:Unsupported (Src.pos s)
      "NEXT_BUFFER: out-of-band buffers are not supported"
  else if c = Opcode.readonly_buffer then
    begin match (top m).shape with
    | Scalar (Bytes _ | Readonly_buffer _) -> ()
    | Scalar (Bytearray _) ->
        let value = pop m in
        emit m (Readonly_buffer value)
    | _ -> err m "READONLY_BUFFER expects a buffer"
    end (* lists *)
  else if c = Opcode.empty_list then emit m (List [])
  else if c = Opcode.list then emit m (List (pop_mark m))
  else if c = Opcode.append then begin
    let x = pop m in
    mutate m "APPEND" (top m) (Append x)
  end
  else if c = Opcode.appends then begin
    let items = pop_mark m in
    let target = top m in
    List.iter (fun item -> mutate m "APPENDS" target (Append item)) items
  end (* tuples *)
  else if c = Opcode.empty_tuple then emit m (Tuple [||])
  else if c = Opcode.tuple then emit m (Tuple (Array.of_list (pop_mark m)))
  else if c = Opcode.tuple1 then
    let a = pop m in
    emit m (Tuple [| a |])
  else if c = Opcode.tuple2 then
    let b = pop m in
    let a = pop m in
    emit m (Tuple [| a; b |])
  else if c = Opcode.tuple3 then
    let cc = pop m in
    let b = pop m in
    let a = pop m in
    emit m (Tuple [| a; b; cc |]) (* dicts *)
  else if c = Opcode.empty_dict then emit m (Dict [])
  else if c = Opcode.dict then emit m (Dict (to_pairs m (pop_mark m)))
  else if c = Opcode.setitem then begin
    let v = pop m in
    let k = pop m in
    mutate m "SETITEM" (top m) (Set_item (k, v))
  end
  else if c = Opcode.setitems then begin
    let pairs = to_pairs m (pop_mark m) in
    let target = top m in
    List.iter (fun (k, v) -> mutate m "SETITEMS" target (Set_item (k, v))) pairs
  end (* sets *)
  else if c = Opcode.empty_set then emit m (Set [])
  else if c = Opcode.additems then begin
    let items = pop_mark m in
    let target = top m in
    List.iter (fun item -> mutate m "ADDITEMS" target (Add_item item)) items
  end
  else if c = Opcode.frozenset then emit m (Frozenset (pop_mark m))
    (* stack ops *)
  else if c = Opcode.pop then
    match m.stack with [] -> drop_mark m | _ -> ignore (pop m)
  else if c = Opcode.dup then push m (top m)
  else if c = Opcode.mark then mark m
  else if c = Opcode.pop_mark then drop_mark m (* memo *)
  else if c = Opcode.get then push m (memo_get m (text_memo_id m (Src.line s)))
  else if c = Opcode.binget then
    push m (memo_get m (Int64.of_int (Src.uint1 s)))
  else if c = Opcode.long_binget then push m (memo_get m (Src.uint32 s))
  else if c = Opcode.put then memo_put m (text_memo_id m (Src.line s))
  else if c = Opcode.binput then memo_put m (Int64.of_int (Src.uint1 s))
  else if c = Opcode.long_binput then memo_put m (Src.uint32 s)
  else if c = Opcode.memoize then
    memo_put m (Int64.of_int (Hashtbl.length m.memo)) (* extension registry *)
  else if c = Opcode.ext1 then extension m (Int32.of_int (Src.uint1 s))
  else if c = Opcode.ext2 then extension m (Int32.of_int (Src.uint2 s))
  else if c = Opcode.ext4 then extension m (Src.int32 s)
    (* globals / reduce / object construction *)
  else if c = Opcode.global then begin
    let modul = Src.line s in
    let name = Src.line s in
    if not (String.is_valid_utf_8 modul && String.is_valid_utf_8 name) then
      err m "invalid UTF-8 in GLOBAL";
    emit m (Global { modul; name })
  end
  else if c = Opcode.stack_global then begin
    let name = pop m in
    let modul = pop m in
    match (Value.as_string modul, Value.as_string name) with
    | Some modul, Some name -> emit m (Global { modul; name })
    | _ -> err m "STACK_GLOBAL expects two strings"
  end
  else if c = Opcode.reduce then begin
    let args = tuple_args m "REDUCE" (pop m) in
    let func = pop m in
    instance m (Reduce { func; args })
  end
  else if c = Opcode.build then begin
    let state = pop m in
    mutate m "BUILD" (top m) (Build state)
  end
  else if c = Opcode.newobj then begin
    let args = tuple_args m "NEWOBJ" (pop m) in
    let cls = pop m in
    instance m (New { cls; args; kwargs = None })
  end
  else if c = Opcode.newobj_ex then begin
    let kwargs = pop m in
    if not (Value.is_dict kwargs) then
      err m "NEWOBJ_EX expects a keyword dictionary";
    let args = tuple_args m "NEWOBJ_EX" (pop m) in
    let cls = pop m in
    instance m (New { cls; args; kwargs = Some kwargs })
  end
  else if c = Opcode.inst then begin
    let modul = ascii m "INST" (Src.line s) in
    let name = ascii m "INST" (Src.line s) in
    let args = Value.make (Tuple (Array.of_list (pop_mark m))) in
    let cls = Value.make (Global { modul; name }) in
    instance m (Legacy_instantiate { cls; args })
  end
  else if c = Opcode.obj then
    begin match pop_mark m with
    | cls :: items ->
        let args = Value.make (Tuple (Array.of_list items)) in
        instance m (Legacy_instantiate { cls; args })
    | [] -> err m "OBJ with empty mark"
    end (* persistent ids *)
  else if c = Opcode.persid then
    emit m (Persistent (Value.make (Str (ascii m "PERSID" (Src.line s)))))
  else if c = Opcode.binpersid then
    let id = pop m in
    emit m (Persistent id)
  else err m "unknown opcode 0x%02x" op

(* Decode a single pickle from [src]. *)
let run ?memo ?(legacy_strings = Latin1) ?(strict_stack = false) src =
  let limits = Src.limits src in
  let memo = match memo with Some memo -> memo | None -> Hashtbl.create 64 in
  let m =
    {
      src;
      stack = [];
      stack_len = 0;
      live_items = 0;
      metastack = [];
      marks = 0;
      opcodes = limits.opcodes;
      work = limits.work;
      memo;
      legacy_strings;
      strict_stack;
    }
  in
  let rec loop () =
    Src.start_opcode src;
    let opcode_pos = Src.pos src in
    match
      try
        if m.opcodes = 0 then limited m "opcode";
        m.opcodes <- m.opcodes - 1;
        charge m 1;
        run_op m (Src.byte src)
      with Error.E error ->
        raise (Error.E { error with opcode_pos = Some opcode_pos })
    with
    | () -> loop ()
    | exception Stop -> top m
  in
  loop ()
