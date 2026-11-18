module V = Opickle.Value

let decode = Opickle.of_string_exn
let show s = print_endline (V.to_string (decode s))

let%expect_test "scalars" =
  List.iter show
    [
      "N.";
      "I01\n.";
      "I00\n.";
      "I42\n.";
      "I-7\n.";
      "F1.0\n.";
      "F-1.25\n.";
      "Vhi\n.";
    ];
  [%expect
    {|
    None
    True
    False
    42
    -7
    1.0
    -1.25
    'hi' |}]

let%expect_test "big integer decimal" =
  List.iter show
    [
      "L18446744073709551616L\n.";
      "L100000000000000000000L\n.";
      "L-100000000000000000000L\n.";
    ];
  [%expect
    {|
    18446744073709551616
    100000000000000000000
    -100000000000000000000 |}]

let%expect_test "containers and bytes" =
  List.iter show
    [
      "(I1\nI2\nI3\nl.";
      "I1\n\x85.";
      ").";
      "(Va\nI1\nd.";
      "\x8f.";
      "C\x04\x00\x55\xaa\xff.";
      "(\x91.";
    ];
  [%expect
    {|
    [1, 2, 3]
    (1,)
    ()
    {'a': 1}
    set()
    b'\x00U\xaa\xff'
    frozenset() |}]

let%expect_test "diagnostic escaping and round-tripping floats" =
  List.iter show
    [
      "Vit's\n.";
      "V\\u007f\\u0080\\ud800\n.";
      "cmodule\nname\r\n.";
      "F0.1234567890123456\n.";
    ];
  [%expect
    {|
    "it's"
    '\x7f\x80\ud800'
    <global module.name\r>
    0.1234567890123456 |}]

let%expect_test
    "recursive list and object state render without expanding cycles" =
  show "]q\x00h\x00a.";
  show "cm\nf\n)Rq\x00h\x00b.";
  [%expect {|
    [[...]]
    <reduce <global m.f> () state=<cycle>> |}]

let%test_unit "independent depth, visit, output and decimal rendering bounds" =
  let limits = V.default_print_limits in
  let graph = decode "]q\x00K\x01a]q\x01h\x00ah\x00a]h\x01ah\x01a." in
  assert (V.to_string ~limits:{ limits with depth = 0 } graph = "[..., ...]");
  let printed =
    V.to_string ~limits:{ limits with visits = 3; output_bytes = 8 } graph
  in
  assert (String.ends_with ~suffix:"..." printed && String.length printed <= 8);
  let text =
    V.to_string ~limits:{ limits with output_bytes = 4 } (decode "Vabcdefgh\n.")
  in
  assert (text = "...");
  let n =
    V.to_string
      ~limits:{ limits with decimal_bytes = 1 }
      (decode "L18446744073709551616L\n.")
  in
  assert (n = "<integer: 9 bytes>")
