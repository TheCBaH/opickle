(* Errors raised while unpickling. *)

type t = {
  pos : int;  (** byte offset in the stream where the problem was detected *)
  msg : string;
}

exception E of t

let raise_at pos fmt = Printf.ksprintf (fun msg -> raise (E { pos; msg })) fmt

let to_string { pos; msg } =
  Printf.sprintf "pickle error at byte %d: %s" pos msg
