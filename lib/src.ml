(* A buffered byte source on top of a [Bytesrw.Bytes.Reader.t].

   [Bytesrw] hands out byte slices in a pull fashion.  The unpickler, however,
   wants to read one opcode byte at a time, then a handful of fixed-width or
   newline-terminated arguments.  This module keeps the "current" slice around
   and refills it from the reader when exhausted, while tracking the absolute
   stream position for error reporting. *)

open Bytesrw

type t = {
  reader : Bytes.Reader.t;
  mutable buf : bytes;  (** bytes backing the current slice *)
  mutable off : int;  (** next index to read within [buf] *)
  mutable len : int;  (** index past the last valid byte in [buf] *)
  mutable pos : int64;
  mutable unread_budget : int64;
  mutable frame_remaining : int64 option;
  check_frames : bool;
  limits : Limits.t;
}

let of_reader ?(limits = Limits.unlimited) ?(check_frames = true) reader =
  Limits.validate limits;
  {
    reader;
    buf = Bytes.create 0;
    off = 0;
    len = 0;
    pos = Int64.of_int (Bytes.Reader.pos reader);
    unread_budget = limits.input_bytes;
    frame_remaining = None;
    check_frames;
    limits;
  }

let pos t = t.pos
let limits t = t.limits

let start_opcode t =
  if t.frame_remaining = Some 0L then t.frame_remaining <- None

let require_frame_boundary t label =
  match t.frame_remaining with
  | Some n when n > 0L ->
      Error.raise_at t.pos "%s before the end of the current frame" label
  | _ -> ()

let check_input t n =
  let n = Int64.of_int n in
  (match t.frame_remaining with
  | Some remaining when n > remaining ->
      Error.raise_at t.pos "operand crosses a frame boundary"
  | _ -> ());
  if n > t.unread_budget then
    Error.raise_at ~kind:Limit t.pos "input byte limit exceeded";
  if t.pos > Int64.sub Int64.max_int n then
    Error.raise_at ~kind:Limit t.pos "stream position out of range"

let advance t n =
  check_input t n;
  t.off <- t.off + n;
  let n = Int64.of_int n in
  t.pos <- Int64.add t.pos n;
  t.unread_budget <- Int64.sub t.unread_budget n;
  match t.frame_remaining with
  | Some remaining -> t.frame_remaining <- Some (Int64.sub remaining n)
  | None -> ()

(* Pull the next non-empty slice into the buffer.  Returns [false] at end of
   data. *)
let refill t =
  let slice =
    try Bytes.Reader.read t.reader
    with (Sys_error _ | Bytes.Stream.Error _) as exn ->
      Error.raise_at ~kind:Input t.pos "%s" (Printexc.to_string exn)
  in
  if Bytes.Slice.is_eod slice then false
  else begin
    t.buf <- Bytes.Slice.bytes slice;
    t.off <- Bytes.Slice.first slice;
    t.len <- Bytes.Slice.first slice + Bytes.Slice.length slice;
    true
  end

let available t = t.len - t.off

(* Read a single byte, or raise on end of data. *)
let byte t =
  check_input t 1;
  if t.off >= t.len && not (refill t) then
    Error.raise_at ~kind:Truncated (pos t) "unexpected end of input";
  let c = Stdlib.Bytes.get_uint8 t.buf t.off in
  advance t 1;
  c

(* Read exactly [n] bytes, spanning slice boundaries as needed. *)
let take t n =
  if n < 0 then Error.raise_at t.pos "negative operand length";
  if n > Sys.max_string_length then
    Error.raise_at ~kind:Limit t.pos "operand exceeds platform string size";
  check_input t n;
  if n = 0 then ""
  else if n <= available t then begin
    let s = Stdlib.Bytes.sub_string t.buf t.off n in
    advance t n;
    s
  end
  else begin
    let out = Buffer.create (min n 4096) in
    let rec loop remaining =
      if remaining = 0 then Buffer.contents out
      else if available t = 0 && not (refill t) then
        Error.raise_at ~kind:Truncated (pos t)
          "unexpected end of input: %d operand bytes missing" remaining
      else
        let chunk = min remaining (available t) in
        Buffer.add_subbytes out t.buf t.off chunk;
        advance t chunk;
        loop (remaining - chunk)
    in
    loop n
  end

let payload t n =
  if n > t.limits.payload_bytes then
    Error.raise_at ~kind:Limit t.pos "payload byte limit exceeded";
  take t n

(* Read bytes up to and excluding the next ['\n'], consuming the newline. *)
let line t =
  let b = Buffer.create 32 in
  let limit = min Sys.max_string_length t.limits.line_bytes in
  let rec loop () =
    check_input t 1;
    if available t = 0 && not (refill t) then
      Error.raise_at ~kind:Truncated (pos t)
        "unexpected end of input: missing newline";
    let can_read = available t in
    let remaining = limit - Buffer.length b in
    let budget = Int64.min (Int64.of_int can_read) t.unread_budget in
    let budget =
      match t.frame_remaining with
      | None -> Int64.to_int budget
      | Some n -> Int64.to_int (Int64.min budget n)
    in
    let scan_len = if remaining < budget then remaining + 1 else budget in
    let eol =
      let rec scan i =
        if i = scan_len then None
        else if Stdlib.Bytes.get t.buf (t.off + i) = '\n' then Some i
        else scan (i + 1)
      in
      scan 0
    in
    match eol with
    | Some n ->
        check_input t (n + 1);
        Buffer.add_subbytes b t.buf t.off n;
        advance t (n + 1);
        Buffer.contents b
    | None ->
        if scan_len > remaining then begin
          advance t remaining;
          Error.raise_at ~kind:Limit t.pos "line byte limit exceeded"
        end;
        if scan_len < can_read then begin
          advance t scan_len;
          check_input t 1
        end;
        check_input t scan_len;
        Buffer.add_subbytes b t.buf t.off scan_len;
        advance t scan_len;
        loop ()
  in
  loop ()

(* ── Fixed-width little-endian / big-endian numeric readers ──────────────── *)

let uint1 t = byte t

let uint2 t =
  let s = take t 2 in
  Stdlib.Bytes.get_uint16_le (Stdlib.Bytes.unsafe_of_string s) 0

(* Unsigned 32-bit as an OCaml int.

   NOT `land 0xFFFF_FFFF`: that literal is itself -1 wherever int is 32 bits
   (js_of_ocaml), so the mask becomes a no-op and a word with the high bit set
   comes back negative -- a negative length or memo index, silently. Values in
   [2^31, 2^32) have no int representation there at all, so the honest answer is
   to raise, exactly as uint8 does for a 64-bit length that does not fit. *)
let uint4 t =
  let s = take t 4 in
  let b = Stdlib.Bytes.unsafe_of_string s in
  let v = Stdlib.Bytes.get_int32_le b 0 in
  match Int32.unsigned_to_int v with
  | Some n -> n
  | None ->
      Error.raise_at (pos t) "4-byte length %lu does not fit in a native int" v

let int32 t =
  let s = take t 4 in
  String.get_int32_le s 0

let int4 t = Int32.to_int (int32 t)
let uint32 t = Int64.logand (Int64.of_int32 (int32 t)) 0xFFFF_FFFFL

let length32 t =
  let n = int32 t in
  match Int32.unsigned_to_int n with
  | _ when n < 0l -> Error.raise_at (pos t) "negative operand length"
  | Some n -> n
  | None -> Error.raise_at ~kind:Limit (pos t) "operand exceeds platform size"

let frame t =
  (* A FRAME header starts outside the preceding frame. *)
  (match t.frame_remaining with
  | None -> ()
  | Some _ -> Error.raise_at t.pos "FRAME inside a frame");
  let n = String.get_int64_le (take t 8) 0 in
  if n < 0L then Error.raise_at t.pos "frame length is out of range";
  if t.check_frames then t.frame_remaining <- Some n

(* Unsigned 64-bit length as an OCaml int; raises if it does not fit. *)
let uint8 t =
  let s = take t 8 in
  let v = Stdlib.Bytes.get_int64_le (Stdlib.Bytes.unsafe_of_string s) 0 in
  if Int64.compare v 0L < 0 || Int64.compare v (Int64.of_int max_int) > 0 then
    Error.raise_at (pos t) "8-byte length %Lu does not fit in a native int" v;
  Int64.to_int v

(* IEEE-754 double, big-endian (network order), as used by BINFLOAT. *)
let float8_be t =
  let s = take t 8 in
  let bits = Stdlib.Bytes.get_int64_be (Stdlib.Bytes.unsafe_of_string s) 0 in
  Int64.float_of_bits bits
