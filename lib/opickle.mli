(** Structural reader for Python pickle protocols 0 to 5. Python callables,
    constructors, persistent IDs and extension codes are represented as data.
    Protocol 5 supports in-band data. *)

module Error : sig
  type kind = Truncated | Malformed | Unsupported | Limit | Input
  type t = { kind : kind; pos : int64; opcode_pos : int64 option; msg : string }

  exception E of t

  val to_string : t -> string
  (** [pos] is the detection cursor in the supplied reader's coordinate space.
      [opcode_pos] gives the start of the opcode, when there is one. *)
end

module Limits : sig
  type t = {
    payload_bytes : int;
    line_bytes : int;
    text_digits : int;
    input_bytes : int64;
    stack_items : int;
    marks : int;
    memo_entries : int;
    opcodes : int;
    work : int;
  }

  val bounded : t
  (** Starting point for untrusted data. Set limits to suit the application.
      [line_bytes] excludes the newline. [stack_items] includes all saved
      stacks. [work] counts interpreter operations and selected repeated work
      such as decimal conversion. *)

  val unlimited : t
  (** Default. Uses platform representation limits. *)
end

module Value : sig
  type t

  type view =
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

  val view : t -> view
  (** Container views snapshot their immediate entries, preserving operation
      order and duplicate dictionary entries. Referenced nodes retain their
      identities. Unicode uses UTF-8 with surrogate preservation. Big integer
      magnitudes are unsigned, little-endian and canonical. *)

  val id : t -> int64
  (** Process-local identity for walking graphs. *)

  val same_node : t -> t -> bool
  val as_int : t -> int64 option
  val as_string : t -> string option

  val dict_find : string -> t -> t option
  (** Returns the last entry whose key is a matching [Str]. *)

  type print_limits = {
    depth : int;
    visits : int;
    output_bytes : int;
    decimal_bytes : int;
  }

  val default_print_limits : print_limits
  val pp : ?limits:print_limits -> Format.formatter -> t -> unit

  val to_string : ?limits:print_limits -> t -> string
  (** Bounded diagnostics. [depth] is between 0 and 512 inclusive. The printer
      shows back edges and abbreviates large integers and truncated output. *)
end

type legacy_strings =
  | Latin1
  | Raw_bytes
      (** Legacy Python 2 string opcodes map to [Str] using Latin-1 by default.
          [Raw_bytes] preserves them as [Bytes]. *)

val of_reader_exn :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  Bytesrw.Bytes.Reader.t ->
  Value.t
(** Decodes one value and pushes unconsumed bytes back on success. Frame checks
    are enabled by default. [exact] requires one value, no remaining marks and
    EOF immediately after [STOP]. A caller must abandon the current position on
    failure. Programming exceptions raised by user-supplied readers propagate.
*)

val of_reader :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  Bytesrw.Bytes.Reader.t ->
  (Value.t, Error.t) result

val of_string_exn :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  string ->
  Value.t

val of_string :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  string ->
  (Value.t, Error.t) result

val of_in_channel_exn :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  in_channel ->
  Value.t
(** Creates a buffered reader. Use a session for repeated channel loads. *)

val of_in_channel :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  in_channel ->
  (Value.t, Error.t) result

val of_file_exn :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  string ->
  Value.t

val of_file :
  ?limits:Limits.t ->
  ?legacy_strings:legacy_strings ->
  ?check_frames:bool ->
  ?exact:bool ->
  string ->
  (Value.t, Error.t) result

module Session : sig
  type t
  type memo_mode = Independent | Shared

  val of_reader :
    ?limits:Limits.t ->
    ?legacy_strings:legacy_strings ->
    ?check_frames:bool ->
    ?strict_stack:bool ->
    ?memo:memo_mode ->
    Bytesrw.Bytes.Reader.t ->
    t

  val of_in_channel :
    ?limits:Limits.t ->
    ?legacy_strings:legacy_strings ->
    ?check_frames:bool ->
    ?strict_stack:bool ->
    ?memo:memo_mode ->
    in_channel ->
    t

  val pos : t -> int64
  val reset_memo : t -> unit
  val load_exn : t -> Value.t option

  val load : t -> (Value.t option, Error.t) result
  (** [None] means clean EOF between pickles. The session owns the buffered
      reader for the duration of use. In [Shared] mode, successive loads share
      node identity and a later pickle can mutate earlier nodes. Limits reset on
      each load except that memo entries are counted across shared loads. A
      failed load poisons the session. *)
end

module Private : sig
  (** Unstable test helpers. *)
  module Src : sig
    type t

    val of_reader :
      ?limits:Limits.t -> ?check_frames:bool -> Bytesrw.Bytes.Reader.t -> t

    val pos : t -> int64
    val byte : t -> int
    val take : t -> int -> string
    val line : t -> string
    val uint2 : t -> int
    val int4 : t -> int
    val uint4 : t -> int
    val uint8 : t -> int
    val float8_be : t -> float
  end
end
