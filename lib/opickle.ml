(* Public entry points for unpickling Python pickle streams.

   The library decodes a single pickle (protocols 0-5) into a {!Value.t}.  Input
   is consumed through [bytesrw]'s {!Bytesrw.Bytes.Reader}, so any source it can
   wrap — strings, channels, files, filters — can be unpickled. *)

module Value = Value
module Error = Error
module Limits = Limits

(* [Src] and [Unpickler] are internal building blocks, re-exported so they can
   be unit-tested in isolation and reused for advanced streaming. *)
module Src = Src
module Unpickler = Unpickler
open Bytesrw

let of_reader_exn ?limits reader = Unpickler.run (Src.of_reader ?limits reader)

let of_reader ?limits reader =
  try Ok (of_reader_exn ?limits reader) with Error.E e -> Result.Error e

let of_string_exn ?limits s = of_reader_exn ?limits (Bytes.Reader.of_string s)
let of_string ?limits s = of_reader ?limits (Bytes.Reader.of_string s)

let of_in_channel_exn ?limits ic =
  of_reader_exn ?limits (Bytes.Reader.of_in_channel ic)

let of_in_channel ?limits ic = of_reader ?limits (Bytes.Reader.of_in_channel ic)

let of_file_exn ?limits path =
  try In_channel.with_open_bin path (of_in_channel_exn ?limits)
  with Sys_error msg -> Error.raise_at ~kind:Input 0L "%s" msg

let of_file ?limits path =
  try Ok (of_file_exn ?limits path) with Error.E e -> Result.Error e
