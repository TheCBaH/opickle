let render = function
  | Ok v -> Opickle.Value.to_string v
  | Error e -> Opickle.Error.to_string e

let%expect_test "of_string happy path" =
  print_string (render (Opickle.of_string "]q\x00(K\x01K\x02e."));
  [%expect "[1, 2]"]

let%expect_test "of_string surfaces errors" =
  print_string (render (Opickle.of_string "\xff."));
  [%expect "pickle error at byte 1: unknown opcode 0xff"]
