module B = Bytesrw.Bytes
module E = Opickle.Error
module S = Opickle.Session
module V = Opickle.Value

let expect_int label want = function
  | Ok value when V.as_int value = Some want -> ()
  | Ok _ -> failwith (label ^ ": unexpected value")
  | Error error -> failwith (label ^ ": " ^ E.to_string error)

let expect_error kind pos opcode_pos = function
  | Error { E.kind = actual_kind; pos = actual_pos; opcode_pos = actual_op; _ }
    when actual_kind = kind && actual_pos = pos && actual_op = opcode_pos ->
      ()
  | Error error -> failwith ("unexpected diagnostic: " ^ E.to_string error)
  | Ok _ -> failwith "invalid stream was accepted"

let next session =
  match S.load session with
  | Ok (Some value) -> value
  | Ok None -> failwith "unexpected end of stream"
  | Error error -> failwith (E.to_string error)

let expect_end position session =
  match S.load session with
  | Ok None when S.pos session = position -> ()
  | Error error -> failwith (E.to_string error)
  | Ok _ -> failwith "expected clean end of stream at the final offset"

let%test_unit "prefix reader decodes preserve caller-owned unread bytes" =
  List.iter
    (fun length ->
      let reader = Test_reader.reused ~pos:23 length "I7\n.I8\n." in
      expect_int "first prefix" 7L (Opickle.of_reader reader);
      assert (B.Reader.pos reader = 27);
      expect_int "second prefix" 8L (Opickle.of_reader reader);
      assert (B.Reader.pos reader = 31))
    [ 1; 3; 8 ]

let%test_unit "exact decode checks EOF, remaining stack and open marks" =
  expect_int "single exact value" 7L (Opickle.of_string ~exact:true "I7\n.");
  expect_int "prefix value" 7L (Opickle.of_string "I7\n.I8\n.");
  expect_error Malformed 4L None (Opickle.of_string ~exact:true "I7\n.I8\n.");
  expect_error Malformed 3L (Some 2L) (Opickle.of_string ~exact:true "NN.");
  expect_error Malformed 3L (Some 2L) (Opickle.of_string ~exact:true "(N.")

let%test_unit "sessions continue through single and reused slices" =
  List.iter
    (fun length ->
      let reader = Test_reader.reused ~pos:23 length "I7\n.I8\n." in
      let session = S.of_reader reader in
      expect_int "first load" 7L (Ok (next session));
      assert (S.pos session = 27L);
      expect_int "second load" 8L (Ok (next session));
      expect_end 31L session;
      expect_end 31L session)
    [ 1; 3; 8 ]

let%test_unit "explicit shared memo preserves aliases and supports reset" =
  let data = "]q\x00.h\x00.h\x00." in
  let shared = S.of_reader ~memo:Shared (Test_reader.reused 3 data) in
  let first = next shared in
  let second = next shared in
  assert (V.same_node first second);
  S.reset_memo shared;
  expect_error Malformed 9L (Some 7L) (S.load shared);
  expect_error Input 9L None (S.load shared);
  let independent = S.of_reader (B.Reader.of_string data) in
  ignore (next independent);
  expect_error Malformed 6L (Some 4L) (S.load independent);
  expect_error Input 6L None (S.load independent)

let%test_unit "partial next object is a truncation and poisons the session" =
  let session = S.of_reader (Test_reader.reused 1 "N.I7") in
  ignore (next session);
  expect_error Truncated 4L (Some 2L) (S.load session);
  expect_error Input 4L None (S.load session)

let%test_unit "channel session owns its buffered reader across loads" =
  let path = Filename.temp_file "opickle-session" ".pickle" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          Out_channel.output_string channel "I7\n.I8\n.");
      In_channel.with_open_bin path (fun channel ->
          let session = S.of_in_channel channel in
          expect_int "channel first load" 7L (Ok (next session));
          expect_int "channel second load" 8L (Ok (next session));
          expect_end 8L session))
