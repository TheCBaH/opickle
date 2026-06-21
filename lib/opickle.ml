(* Public entry points for unpickling Python pickle streams.

   The library decodes a single pickle (protocols 0-5) into a {!Value.t}.  Input
   is consumed through [bytesrw]'s {!Bytesrw.Bytes.Reader}, so any source it can
   wrap — strings, channels, files, filters — can be unpickled. *)

module Value = Value
module Error = Error

(* [Src] and [Unpickler] are internal building blocks, re-exported so they can
   be unit-tested in isolation and reused for advanced streaming. *)
module Src = Src
module Unpickler = Unpickler
open Bytesrw

let of_reader_exn reader = Unpickler.run (Src.of_reader reader)

let of_reader reader =
  try Ok (of_reader_exn reader) with Error.E e -> Result.Error e

let of_string_exn s = of_reader_exn (Bytes.Reader.of_string s)
let of_string s = of_reader (Bytes.Reader.of_string s)
let of_in_channel_exn ic = of_reader_exn (Bytes.Reader.of_in_channel ic)
let of_in_channel ic = of_reader (Bytes.Reader.of_in_channel ic)

let of_file_exn path =
  In_channel.with_open_bin path (fun ic -> of_in_channel_exn ic)

let of_file path = try Ok (of_file_exn path) with Error.E e -> Result.Error e
