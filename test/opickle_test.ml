let render = function
  | Ok v -> Opickle.Value.to_string v
  | Error e -> Opickle.Error.to_string e

let%expect_test "of_string happy path" =
  print_string (render (Opickle.of_string "]q\x00(K\x01K\x02e."));
  [%expect "[1, 2]"]

let%expect_test "of_string surfaces errors" =
  print_string (render (Opickle.of_string "\xff."));
  [%expect "pickle error at byte 1: unknown opcode 0xff"]

let expect_error ?limits input kind pos opcode_pos =
  match Opickle.of_string ?limits input with
  | Error error ->
      assert (error.kind = kind && error.pos = pos);
      assert (error.opcode_pos = Some opcode_pos)
  | Ok _ -> assert false

let%test_unit "records where malformed and truncated operands are detected" =
  expect_error "I2x\n." Malformed 4L 0L;
  expect_error "Fbad\n." Malformed 5L 0L;
  expect_error "gunknown\n." Malformed 9L 0L;
  expect_error "C\x03ab" Truncated 4L 0L

let%test_unit "checks limits before reading or converting operands" =
  let limits = Opickle.Limits.unlimited in
  expect_error ~limits:{ limits with payload_bytes = 2 } "C\x03abc." Limit 2L 0L;
  expect_error ~limits:{ limits with line_bytes = 1 } "I22\n." Limit 1L 0L;
  expect_error ~limits:{ limits with text_digits = 1 } "I22\n." Limit 4L 0L;
  expect_error ~limits:{ limits with input_bytes = 1L } "N." Limit 1L 1L;
  expect_error ~limits:{ limits with stack_items = 1 } "N(Nt." Limit 3L 2L;
  expect_error ~limits:{ limits with marks = 0 } "(Nt." Limit 1L 0L;
  expect_error ~limits:{ limits with memo_entries = 0 } "Np0\n." Limit 4L 1L;
  expect_error ~limits:{ limits with opcodes = 1 } "N." Limit 1L 1L;
  expect_error ~limits:{ limits with work = 1 } "(Nt." Limit 1L 1L;
  assert (
    Result.is_ok
      (Opickle.of_string ~limits:{ limits with line_bytes = 2 } "I22\n."))

let%expect_test "POP can discard an empty marked segment" =
  print_string (render (Opickle.of_string "N(0."));
  [%expect "None"]

let%test_unit "reader Exit propagates independently of STOP" =
  let reader = Bytesrw.Bytes.Reader.make (fun () -> raise Exit) in
  match Opickle.of_reader reader with exception Exit -> () | _ -> assert false
