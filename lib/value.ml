(* The decoded value model.

   This mirrors the Python object model reachable through the pickle opcode set
   (see CPython's [Lib/pickletools.py] and [Doc/library/pickle.rst]).  Values
   that require executing Python code to fully realise — globals, [__reduce__]
   results, freshly constructed objects, persistent ids and extension codes —
   are kept structurally rather than evaluated.

   Containers ([List], [Dict], [Set]) are mutable [ref] cells: pickle memoises a
   container's identity *before* populating it, so a recursive structure holds a
   memo reference back to the very ref that is later filled in. *)

type t =
  | None
  | Bool of bool
  | Int of int64
  | Bigint of { negative : bool; magnitude : string }
      (** little-endian bytes *)
  | Float of float
  | Bytes of string
  | Bytearray of string
  | Str of string  (** unicode text, stored as UTF-8 *)
  | List of t list ref
  | Tuple of t array
  | Dict of (t * t) list ref
  | Set of t list ref
  | Frozenset of t list
  | Global of { modul : string; name : string }
  | Reduce of { func : t; args : t }
  | Object of { cls : t; args : t; mutable state : t option }
  | Persistent of t
  | Ext of int

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
        List.map
          (fun d ->
            let v = (d * 256) + !carry in
            carry := v / 10;
            v mod 10)
          !digits
      in
      digits := out @ flush !carry
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

(* ── Scalar rendering helpers ────────────────────────────────────────────── *)

let quote_bytes s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\'' -> Buffer.add_string buf "\\'"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when c >= ' ' && c <= '~' -> Buffer.add_char buf c
      | c -> Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c)))
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let quote_str s =
  (* [s] is UTF-8; render printable bytes literally, escape control bytes. *)
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\'' -> Buffer.add_string buf "\\'"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

(* Python-style float repr: integral floats keep a trailing ".0". *)
let float_repr f =
  if Float.is_integer f && Float.abs f < 1e16 then Printf.sprintf "%.1f" f
  else
    let s = Printf.sprintf "%.17g" f in
    (* prefer the shortest round-tripping representation *)
    let short = Printf.sprintf "%.15g" f in
    if float_of_string short = f then short else s

(* ── Pretty-printer ─────────────────────────────────────────────────────── *)

let pp ppf root =
  let comma ppf () = Format.fprintf ppf ",@ " in
  let rec go seen ppf v =
    let cyclic v = List.memq v seen in
    match v with
    | None -> Format.pp_print_string ppf "None"
    | Bool b -> Format.pp_print_string ppf (if b then "True" else "False")
    | Int i -> Format.fprintf ppf "%Ld" i
    | Bigint { negative; magnitude } ->
        Format.pp_print_string ppf (bigint_to_string ~negative magnitude)
    | Float f -> Format.pp_print_string ppf (float_repr f)
    | Bytes s -> Format.fprintf ppf "b%s" (quote_bytes s)
    | Bytearray s -> Format.fprintf ppf "bytearray(b%s)" (quote_bytes s)
    | Str s -> Format.pp_print_string ppf (quote_str s)
    | Tuple a ->
        let sep = if Array.length a = 1 then "," else "" in
        Format.fprintf ppf "@[(%a%s)@]" (elems seen) (Array.to_list a) sep
    | List _ when cyclic v -> Format.pp_print_string ppf "[...]"
    | List r -> Format.fprintf ppf "@[[%a]@]" (elems (v :: seen)) !r
    | Set _ when cyclic v -> Format.pp_print_string ppf "{...}"
    | Set r -> (
        match !r with
        | [] -> Format.pp_print_string ppf "set()"
        | r -> Format.fprintf ppf "@[{%a}@]" (elems (v :: seen)) r)
    | Frozenset l -> Format.fprintf ppf "@[frozenset({%a})@]" (elems seen) l
    | Dict _ when cyclic v -> Format.pp_print_string ppf "{...}"
    | Dict r -> Format.fprintf ppf "@[{%a}@]" (pairs (v :: seen)) !r
    | Global { modul; name } -> Format.fprintf ppf "<global %s.%s>" modul name
    | Reduce { func; args } ->
        Format.fprintf ppf "@[%,<reduce %a@ %a>@]" (go seen) func (go seen) args
    | Object { cls; args; state } ->
        Format.fprintf ppf "@[%,<object %a@ %a%a>@]" (go seen) cls (go seen)
          args (opt_state seen) state
    | Persistent id -> Format.fprintf ppf "@[%,<persid %a>@]" (go seen) id
    | Ext code -> Format.fprintf ppf "<ext %d>" code
  and elems seen ppf items =
    Format.pp_print_list ~pp_sep:comma (go seen) ppf items
  and pairs seen ppf items =
    Format.pp_print_list ~pp_sep:comma (pair seen) ppf items
  and pair seen ppf (k, v) =
    Format.fprintf ppf "@[%a:@ %a@]" (go seen) k (go seen) v
  and opt_state seen ppf = function
    | Option.None -> ()
    | Option.Some s -> Format.fprintf ppf "@ state=%a" (go seen) s
  in
  go [] ppf root

let to_string v =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
