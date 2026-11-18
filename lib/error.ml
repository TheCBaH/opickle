(* Errors raised while unpickling. *)

type kind = Truncated | Malformed | Unsupported | Limit | Input

type t = {
  kind : kind;
  pos : int64;  (** byte offset where the problem was detected *)
  opcode_pos : int64 option;
  msg : string;
}

exception E of t

let raise_at ?(kind = Malformed) pos fmt =
  Printf.ksprintf
    (fun msg -> raise (E { kind; pos; opcode_pos = None; msg }))
    fmt

let to_string { pos; msg; _ } =
  Printf.sprintf "pickle error at byte %Ld: %s" pos msg
