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
  mutable base : int;  (** absolute position of [buf.(0)]'s logical start *)
}

let of_reader reader =
  { reader; buf = Bytes.create 0; off = 0; len = 0; base = 0 }

(* Absolute byte offset of the next byte to be consumed. *)
let pos t = t.base + t.off

(* Pull the next non-empty slice into the buffer.  Returns [false] at end of
   data. *)
let refill t =
  let slice = Bytes.Reader.read t.reader in
  if Bytes.Slice.is_eod slice then false
  else begin
    t.base <- pos t;
    t.buf <- Bytes.Slice.bytes slice;
    t.off <- Bytes.Slice.first slice;
    t.len <- Bytes.Slice.first slice + Bytes.Slice.length slice;
    (* [base] is the absolute position of [buf.(off)], so subtract [off]. *)
    t.base <- t.base - t.off;
    true
  end

let available t = t.len - t.off

(* Read a single byte, or raise on end of data. *)
let byte t =
  if t.off >= t.len && not (refill t) then
    Error.raise_at (pos t) "unexpected end of input";
  let c = Stdlib.Bytes.get_uint8 t.buf t.off in
  t.off <- t.off + 1;
  c

(* Read exactly [n] bytes, spanning slice boundaries as needed. *)
let take t n =
  if n = 0 then ""
  else if t.off + n <= t.len then begin
    (* Fast path: wholly inside the current slice. *)
    let s = Stdlib.Bytes.sub_string t.buf t.off n in
    t.off <- t.off + n;
    s
  end
  else begin
    let out = Stdlib.Bytes.create n in
    let rec loop written =
      if written = n then ()
      else begin
        if available t = 0 && not (refill t) then
          Error.raise_at (pos t) "unexpected end of input: wanted %d bytes" n;
        let chunk = min (n - written) (available t) in
        Stdlib.Bytes.blit t.buf t.off out written chunk;
        t.off <- t.off + chunk;
        loop (written + chunk)
      end
    in
    loop 0;
    Stdlib.Bytes.unsafe_to_string out
  end

(* Read bytes up to and excluding the next ['\n'], consuming the newline. *)
let line t =
  let b = Buffer.create 32 in
  let rec loop () =
    if available t = 0 && not (refill t) then
      Error.raise_at (pos t) "unexpected end of input: missing newline";
    let nl =
      let rec scan i =
        if i >= t.len then -1
        else if Stdlib.Bytes.get t.buf i = '\n' then i
        else scan (i + 1)
      in
      scan t.off
    in
    if nl >= 0 then begin
      Buffer.add_subbytes b t.buf t.off (nl - t.off);
      t.off <- nl + 1;
      Buffer.contents b
    end
    else begin
      Buffer.add_subbytes b t.buf t.off (t.len - t.off);
      t.off <- t.len;
      loop ()
    end
  in
  loop ()

(* ── Fixed-width little-endian / big-endian numeric readers ──────────────── *)

let uint1 t = byte t

let uint2 t =
  let s = take t 2 in
  Stdlib.Bytes.get_uint16_le (Stdlib.Bytes.unsafe_of_string s) 0

(* Unsigned 32-bit as an OCaml int (safe on 64-bit platforms). *)
let uint4 t =
  let s = take t 4 in
  let b = Stdlib.Bytes.unsafe_of_string s in
  Int32.to_int (Stdlib.Bytes.get_int32_le b 0) land 0xFFFF_FFFF

(* Signed 32-bit. *)
let int4 t =
  let s = take t 4 in
  Int32.to_int (Stdlib.Bytes.get_int32_le (Stdlib.Bytes.unsafe_of_string s) 0)

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
