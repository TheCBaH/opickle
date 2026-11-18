module Value = Value
module Error = Error
module Limits = Limits

module Private = struct
  module Src = Src
end

open Bytesrw

type legacy_strings = Unpickler.legacy_strings = Latin1 | Raw_bytes

let as_result f arg = try Ok (f arg) with Error.E error -> Error error

let of_reader_exn ?limits ?legacy_strings ?check_frames ?(exact = false) reader
    =
  let src = Src.of_reader ?limits ?check_frames reader in
  let value = Unpickler.run ?legacy_strings ~strict_stack:exact src in
  if exact && not (Src.at_end src) then
    Error.raise_at (Src.pos src) "trailing data after STOP";
  Src.release src;
  value

let of_reader ?limits ?legacy_strings ?check_frames ?exact reader =
  as_result (of_reader_exn ?limits ?legacy_strings ?check_frames ?exact) reader

let of_string_exn ?limits ?legacy_strings ?check_frames ?exact s =
  of_reader_exn ?limits ?legacy_strings ?check_frames ?exact
    (Bytes.Reader.of_string s)

let of_string ?limits ?legacy_strings ?check_frames ?exact s =
  as_result (of_string_exn ?limits ?legacy_strings ?check_frames ?exact) s

let of_in_channel_exn ?limits ?legacy_strings ?check_frames ?exact ic =
  of_reader_exn ?limits ?legacy_strings ?check_frames ?exact
    (Bytes.Reader.of_in_channel ic)

let of_in_channel ?limits ?legacy_strings ?check_frames ?exact ic =
  as_result (of_in_channel_exn ?limits ?legacy_strings ?check_frames ?exact) ic

let of_file_exn ?limits ?legacy_strings ?check_frames ?exact path =
  try
    In_channel.with_open_bin path
      (of_in_channel_exn ?limits ?legacy_strings ?check_frames ?exact)
  with Sys_error msg -> Error.raise_at ~kind:Input 0L "%s" msg

let of_file ?limits ?legacy_strings ?check_frames ?exact path =
  as_result (of_file_exn ?limits ?legacy_strings ?check_frames ?exact) path

module Session = struct
  type memo_mode = Independent | Shared

  type t = {
    src : Src.t;
    memo : (int64, Value.t) Hashtbl.t option;
    legacy_strings : legacy_strings;
    strict_stack : bool;
    mutable failed : bool;
  }

  let of_reader ?limits ?(legacy_strings = Latin1) ?check_frames
      ?(strict_stack = false) ?(memo = Independent) reader =
    let src = Src.of_reader ?limits ?check_frames reader in
    let memo =
      match memo with Independent -> None | Shared -> Some (Hashtbl.create 64)
    in
    { src; memo; legacy_strings; strict_stack; failed = false }

  let of_in_channel ?limits ?legacy_strings ?check_frames ?strict_stack ?memo ic
      =
    of_reader ?limits ?legacy_strings ?check_frames ?strict_stack ?memo
      (Bytes.Reader.of_in_channel ic)

  let pos session = Src.pos session.src

  let check_failed session =
    if session.failed then
      Error.raise_at ~kind:Input (pos session) "decoder session failed"

  let reset_memo session =
    if session.failed then invalid_arg "Opickle: decoder session failed";
    Option.iter Hashtbl.clear session.memo

  let load_exn session =
    check_failed session;
    Src.begin_pickle session.src;
    match
      if Src.at_end session.src then None
      else
        Some
          (Unpickler.run ?memo:session.memo
             ~legacy_strings:session.legacy_strings
             ~strict_stack:session.strict_stack session.src)
    with
    | value -> value
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        session.failed <- true;
        Option.iter Hashtbl.clear session.memo;
        Printexc.raise_with_backtrace exn backtrace

  let load = as_result load_exn
end
