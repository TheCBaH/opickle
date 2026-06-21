(* Decode one or more pickle files and print the resulting values.

   Used both as a convenience tool and as the driver for the cram smoke tests
   over the fixture pickles. *)

let dump_file path =
  match Opickle.of_file path with
  | Ok v ->
      print_endline (Opickle.Value.to_string v);
      true
  | Error e ->
      prerr_endline (Opickle.Error.to_string e);
      false

let main files =
  let ok = List.fold_left (fun acc f -> dump_file f && acc) true files in
  if ok then 0 else 1

open Cmdliner

let files =
  let doc = "Pickle file(s) to decode." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Decode Python pickle files and print their values." in
  let info = Cmd.info "pickle_dump" ~doc in
  Cmd.v info Term.(const main $ files)

let () = exit (Cmd.eval' cmd)
