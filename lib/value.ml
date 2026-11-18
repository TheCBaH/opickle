exception Container_limit

module Vector = struct
  type 'a t = { mutable data : 'a array; mutable length : int }

  let create () = { data = [||]; length = 0 }

  let push t value =
    let capacity = Array.length t.data in
    if t.length = capacity then begin
      if capacity = Sys.max_array_length then raise Container_limit;
      let next =
        if capacity > Sys.max_array_length / 2 then Sys.max_array_length
        else max 4 (2 * capacity)
      in
      let data = Array.make next value in
      Array.blit t.data 0 data 0 t.length;
      t.data <- data
    end;
    t.data.(t.length) <- value;
    t.length <- t.length + 1

  let of_list = function
    | [] -> create ()
    | items ->
        let data = Array.of_list items in
        { data; length = Array.length data }

  let to_list t =
    let output = ref [] in
    for i = t.length - 1 downto 0 do
      output := t.data.(i) :: !output
    done;
    !output
end

type t = { id : int64; shape : shape }

and view =
  | None
  | Bool of bool
  | Int of int64
  | Bigint of { negative : bool; magnitude : string }
  | Float of float
  | Bytes of string
  | Bytearray of string
  | Readonly_buffer of t
  | Str of string
  | List of t list
  | Tuple of t array
  | Dict of (t * t) list
  | Set of t list
  | Frozenset of t list
  | Global of { modul : string; name : string }
  | Object of { constructor : constructor; operations : operation list }
  | Persistent of t
  | Ext of int32

and constructor =
  | Reduce of { func : t; args : t }
  | New of { cls : t; args : t; kwargs : t option }
  | Legacy_instantiate of { cls : t; args : t }

and operation = Append of t | Set_item of t * t | Add_item of t | Build of t

and shape =
  | Scalar of view
  | Sequence of t Vector.t
  | Mapping of (t * t) Vector.t
  | Members of t Vector.t
  | Instance of constructor * operation Vector.t

let next_id = Atomic.make 0L

let rec fresh_id () =
  let id = Atomic.get next_id in
  if id = Int64.max_int then
    failwith "Opickle: exhausted graph node identifiers";
  if Atomic.compare_and_set next_id id (Int64.succ id) then id else fresh_id ()

let make v =
  let shape =
    match v with
    | List items -> Sequence (Vector.of_list items)
    | Dict items -> Mapping (Vector.of_list items)
    | Set items -> Members (Vector.of_list items)
    | Object { constructor; operations } ->
        Instance (constructor, Vector.of_list operations)
    | _ -> Scalar v
  in
  { id = fresh_id (); shape }

let id node = node.id
let same_node a b = a == b

let view node =
  match node.shape with
  | Scalar (Tuple a) -> Tuple (Array.copy a)
  | Scalar view -> view
  | Sequence items -> List (Vector.to_list items)
  | Mapping entries -> Dict (Vector.to_list entries)
  | Members items -> Set (Vector.to_list items)
  | Instance (constructor, operations) ->
      Object { constructor; operations = Vector.to_list operations }

let as_int node = match node.shape with Scalar (Int n) -> Some n | _ -> None

let as_string node =
  match node.shape with Scalar (Str s) -> Some s | _ -> None

let is_tuple node =
  match node.shape with Scalar (Tuple _) -> true | _ -> false

let is_dict node = match node.shape with Mapping _ -> true | _ -> false

let dict_find key node =
  match node.shape with
  | Mapping entries ->
      let rec seek i : t option =
        if i < 0 then None
        else
          let k, v = entries.data.(i) in
          match as_string k with
          | Some s when String.equal key s -> Some v
          | _ -> seek (i - 1)
      in
      seek (entries.length - 1)
  | _ -> None

let apply node operation =
  match (node.shape, operation) with
  | Sequence items, Append item | Members items, Add_item item ->
      Vector.push items item;
      true
  | Mapping entries, Set_item (key, value) ->
      Vector.push entries (key, value);
      true
  | Instance (_, operations), operation ->
      Vector.push operations operation;
      true
  | _ -> false

(* ── Big-integer decimal rendering ──────────────────────────────────────── *)

(* Convert a little-endian magnitude to a decimal string, without depending on
   an arbitrary-precision library.  Classic base-256 -> base-10 conversion:
   fold the bytes most-significant first, doing [acc = acc*256 + byte] on a
   little-endian array of decimal digits. *)
let magnitude_to_decimal (m : string) : string =
  let n = String.length m in
  if n = 0 then "0"
  else begin
    let digits =
      ref [ 0 ]
      (* little-endian decimal digits *)
    in
    (* little-endian list of the decimal digits of [c] *)
    let rec flush c = if c = 0 then [] else (c mod 10) :: flush (c / 10) in
    for i = n - 1 downto 0 do
      let carry = ref (Char.code m.[i]) in
      let out =
        List.rev_map
          (fun d ->
            let v = (d * 256) + !carry in
            carry := v / 10;
            v mod 10)
          !digits
      in
      digits := List.rev_append out (flush !carry)
    done;
    let buf = Buffer.create 16 in
    List.iter
      (fun d -> Buffer.add_char buf (Char.chr (d + Char.code '0')))
      (List.rev !digits);
    (* strip leading zeros *)
    let s = Buffer.contents buf in
    let i = ref 0 in
    while !i < String.length s - 1 && s.[!i] = '0' do
      incr i
    done;
    String.sub s !i (String.length s - !i)
  end

let bigint_to_string ~negative magnitude =
  let d = magnitude_to_decimal magnitude in
  if negative && d <> "0" then "-" ^ d else d

let escape_codepoint buf cp =
  if cp <= 0xFF then Printf.bprintf buf "\\x%02x" cp
  else if cp <= 0xFFFF then Printf.bprintf buf "\\u%04x" cp
  else Printf.bprintf buf "\\U%08x" cp

let display_control cp =
  cp < 0x20
  || (0x7F <= cp && cp <= 0x9F)
  || cp = 0xAD || cp = 0x061C
  || (0x200B <= cp && cp <= 0x200F)
  || (0x2028 <= cp && cp <= 0x202E)
  || (0x2060 <= cp && cp <= 0x206F)
  || cp = 0xFEFF

let escape ~binary ~quote s =
  let buf = Buffer.create (min 256 (String.length s)) in
  let len = String.length s in
  let byte i = Char.code s.[i] in
  let rec loop i =
    if i < len then begin
      let next =
        match s.[i] with
        | '\\' ->
            Buffer.add_string buf "\\\\";
            i + 1
        | '\n' ->
            Buffer.add_string buf "\\n";
            i + 1
        | '\r' ->
            Buffer.add_string buf "\\r";
            i + 1
        | '\t' ->
            Buffer.add_string buf "\\t";
            i + 1
        | c when quote = Some c ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c;
            i + 1
        | _ when binary ->
            let c = byte i in
            if c >= 0x20 && c < 0x7F then Buffer.add_char buf s.[i]
            else escape_codepoint buf c;
            i + 1
        | _
          when len - i >= 3
               && byte i = 0xED
               && byte (i + 1) >= 0xA0
               && byte (i + 1) <= 0xBF
               && byte (i + 2) land 0xC0 = 0x80 ->
            let cp =
              (0xD lsl 12)
              lor ((byte (i + 1) land 0x3F) lsl 6)
              lor (byte (i + 2) land 0x3F)
            in
            escape_codepoint buf cp;
            i + 3
        | _ ->
            let decoded = String.get_utf_8_uchar s i in
            if Uchar.utf_decode_is_valid decoded then begin
              let width = Uchar.utf_decode_length decoded in
              let cp = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
              if display_control cp then escape_codepoint buf cp
              else Buffer.add_substring buf s i width;
              i + width
            end
            else begin
              escape_codepoint buf (byte i);
              i + 1
            end
      in
      loop next
    end
  in
  Option.iter (Buffer.add_char buf) quote;
  loop 0;
  Option.iter (Buffer.add_char buf) quote;
  Buffer.contents buf

let quote ~binary s =
  let quote =
    if String.contains s '\'' && not (String.contains s '"') then '"' else '\''
  in
  escape ~binary ~quote:(Some quote) s

let float_repr f =
  if not (Float.is_finite f) then Printf.sprintf "%g" f
  else if Float.is_integer f && Float.abs f < 1e16 then Printf.sprintf "%.1f" f
  else
    let rec search digits =
      let s = Printf.sprintf "%.*g" digits f in
      if digits = 17 || float_of_string s = f then s else search (digits + 1)
    in
    search 1

type print_limits = {
  depth : int;
  visits : int;
  output_bytes : int;
  decimal_bytes : int;
}

let default_print_limits =
  {
    depth = 128;
    visits = 100_000;
    output_bytes = 1_048_576;
    decimal_bytes = 512;
  }

exception Display_limit

let utf8_prefix s length =
  let rec prefix n =
    if n > 0 && n < String.length s && Char.code s.[n] land 0xC0 = 0x80 then
      prefix (n - 1)
    else n
  in
  prefix length

let render ~limits ~margin root =
  if
    limits.depth < 0 || limits.depth > 512 || limits.visits < 0
    || limits.output_bytes < 0 || limits.decimal_bytes < 0
  then invalid_arg "Opickle: invalid printer limits";
  let output = Buffer.create (min 256 limits.output_bytes) in
  let emit s start len =
    let available = limits.output_bytes - Buffer.length output in
    if len > available then begin
      let stop = utf8_prefix s (start + available) in
      Buffer.add_substring output s start (max 0 (stop - start));
      raise Display_limit
    end;
    Buffer.add_substring output s start len
  in
  let visits = ref limits.visits and text_bytes = ref limits.output_bytes in
  let active = Hashtbl.create 16 in
  let scalar f s =
    if String.length s > !text_bytes then raise Display_limit;
    let rendered = f s in
    text_bytes := !text_bytes - String.length rendered;
    if !text_bytes < 0 then raise Display_limit;
    rendered
  in
  let ppf = Format.make_formatter emit (fun () -> ()) in
  Format.pp_set_margin ppf margin;
  let comma ppf () = Format.fprintf ppf ",@ " in
  let rec go depth ppf node =
    if !visits = 0 then raise Display_limit;
    decr visits;
    if Hashtbl.mem active node.id then
      Format.pp_print_string ppf
        (match node.shape with
        | Sequence _ -> "[...]"
        | Mapping _ | Members _ -> "{...}"
        | _ -> "<cycle>")
    else if depth > limits.depth then Format.pp_print_string ppf "..."
    else begin
      Hashtbl.add active node.id ();
      let child = go (depth + 1) in
      (match node.shape with
      | Scalar None -> Format.pp_print_string ppf "None"
      | Scalar (Bool b) ->
          Format.pp_print_string ppf (if b then "True" else "False")
      | Scalar (Int i) -> Format.fprintf ppf "%Ld" i
      | Scalar (Bigint { negative; magnitude }) ->
          if String.length magnitude > min limits.decimal_bytes !text_bytes then
            Format.fprintf ppf "<%sinteger: %d bytes>"
              (if negative then "negative " else "")
              (String.length magnitude)
          else
            Format.pp_print_string ppf
              (scalar (bigint_to_string ~negative) magnitude)
      | Scalar (Float f) -> Format.pp_print_string ppf (float_repr f)
      | Scalar (Bytes s) ->
          Format.fprintf ppf "b%s" (scalar (quote ~binary:true) s)
      | Scalar (Bytearray s) ->
          Format.fprintf ppf "bytearray(b%s)" (scalar (quote ~binary:true) s)
      | Scalar (Readonly_buffer value) ->
          Format.fprintf ppf "<readonly buffer %a>" child value
      | Scalar (Str s) ->
          Format.pp_print_string ppf (scalar (quote ~binary:false) s)
      | Scalar (Tuple a) ->
          let sep = if Array.length a = 1 then "," else "" in
          Format.fprintf ppf "@[(%a%s)@]" (array_items child) a sep
      | Sequence items ->
          Format.fprintf ppf "@[[%a]@]" (vector_items child) items
      | Members items ->
          if items.length = 0 then Format.pp_print_string ppf "set()"
          else Format.fprintf ppf "@[{%a}@]" (vector_items child) items
      | Mapping entries ->
          Format.fprintf ppf "@[{%a}@]" (vector_items (pair child)) entries
      | Scalar (Frozenset items) ->
          if items = [] then Format.pp_print_string ppf "frozenset()"
          else
            Format.fprintf ppf "@[frozenset({%a})@]"
              (Format.pp_print_list ~pp_sep:comma child)
              items
      | Scalar (Global { modul; name }) ->
          let name = scalar (escape ~binary:false ~quote:None) name in
          let modul = scalar (escape ~binary:false ~quote:None) modul in
          Format.fprintf ppf "<global %s.%s>" modul name
      | Instance (constructor, operations) ->
          let (label, fn, args, kwargs) : string * t * t * t option =
            match constructor with
            | Reduce { func; args } -> ("reduce", func, args, None)
            | New { cls; args; kwargs } -> ("object", cls, args, kwargs)
            | Legacy_instantiate { cls; args } ->
                ("legacy object", cls, args, None)
          in
          Format.fprintf ppf "@[%,<%s %a@ %a" label child fn child args;
          Option.iter
            (fun value -> Format.fprintf ppf "@ kwargs=%a" child value)
            kwargs;
          for i = 0 to operations.length - 1 do
            match operations.data.(i) with
            | Build state -> Format.fprintf ppf "@ state=%a" child state
            | Append item -> Format.fprintf ppf "@ append=%a" child item
            | Add_item item -> Format.fprintf ppf "@ add=%a" child item
            | Set_item (key, value) ->
                Format.fprintf ppf "@ set=%a" (pair child) (key, value)
          done;
          Format.fprintf ppf ">@]"
      | Scalar (Persistent id) ->
          Format.fprintf ppf "@[%,<persid %a>@]" child id
      | Scalar (Ext code) -> Format.fprintf ppf "<ext %ld>" code
      | Scalar (List _ | Dict _ | Set _ | Object _) -> assert false);
      Hashtbl.remove active node.id
    end
  and array_items pp ppf items =
    for i = 0 to Array.length items - 1 do
      if i <> 0 then comma ppf ();
      pp ppf items.(i)
    done
  and pair pp ppf (k, v) = Format.fprintf ppf "@[%a:@ %a@]" pp k pp v
  and vector_items :
      'a.
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a Vector.t ->
      unit =
   fun pp ppf items ->
    for i = 0 to items.length - 1 do
      if i <> 0 then comma ppf ();
      pp ppf items.data.(i)
    done
  in
  match
    go 0 ppf root;
    Format.pp_print_flush ppf ()
  with
  | () -> Buffer.contents output
  | exception Display_limit ->
      let prefix = Buffer.contents output in
      let marker = String.sub "..." 0 (min 3 limits.output_bytes) in
      let keep =
        min (String.length prefix) (limits.output_bytes - String.length marker)
      in
      String.sub prefix 0 (utf8_prefix prefix keep) ^ marker

let pp ?(limits = default_print_limits) ppf value =
  Format.pp_print_string ppf
    (render ~limits ~margin:(Format.pp_get_margin ppf ()) value)

let to_string ?(limits = default_print_limits) value =
  render ~limits ~margin:78 value
