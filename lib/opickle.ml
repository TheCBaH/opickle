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

type legacy_strings = Unpickler.legacy_strings = Latin1 | Raw_bytes

let of_reader_exn ?limits ?legacy_strings ?check_frames reader =
  Unpickler.run ?legacy_strings (Src.of_reader ?limits ?check_frames reader)

let of_reader ?limits ?legacy_strings ?check_frames reader =
  try Ok (of_reader_exn ?limits ?legacy_strings ?check_frames reader)
  with Error.E e -> Result.Error e

let of_string_exn ?limits ?legacy_strings ?check_frames s =
  of_reader_exn ?limits ?legacy_strings ?check_frames (Bytes.Reader.of_string s)

let of_string ?limits ?legacy_strings ?check_frames s =
  of_reader ?limits ?legacy_strings ?check_frames (Bytes.Reader.of_string s)

let of_in_channel_exn ?limits ?legacy_strings ?check_frames ic =
  of_reader_exn ?limits ?legacy_strings ?check_frames
    (Bytes.Reader.of_in_channel ic)

let of_in_channel ?limits ?legacy_strings ?check_frames ic =
  of_reader ?limits ?legacy_strings ?check_frames
    (Bytes.Reader.of_in_channel ic)

let of_file_exn ?limits ?legacy_strings ?check_frames path =
  try
    In_channel.with_open_bin path
      (of_in_channel_exn ?limits ?legacy_strings ?check_frames)
  with Sys_error msg -> Error.raise_at ~kind:Input 0L "%s" msg

let of_file ?limits ?legacy_strings ?check_frames path =
  try Ok (of_file_exn ?limits ?legacy_strings ?check_frames path)
  with Error.E e -> Result.Error e
