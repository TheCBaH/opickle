(* Decode hand-assembled pickle fragments through the public entry point and
   render the result.  End-to-end decoding of the real fixture pickles lives in
   the cram test [dump.t]. *)
let decode s =
  match Opickle.of_string s with
  | Ok v -> Opickle.Value.to_string v
  | Error e -> Opickle.Error.to_string e

let%expect_test "integers, bool, none" =
  List.iter
    (fun s -> print_endline (decode s))
    [
      "N.";
      (* None *)
      "\x88.";
      (* NEWTRUE *)
      "\x89.";
      (* NEWFALSE *)
      "I42\n.";
      (* INT 42 *)
      "K\x07.";
      (* BININT1 7 *)
      "M\x00\x01.";
      (* BININT2 256 *)
      "J\xff\xff\xff\xff.";
      (* BININT -1 *)
      "I01\n.";
      (* INT 01 -> True *)
      "I00\n." (* INT 00 -> False *);
    ];
  [%expect
    {|
    None
    True
    False
    42
    7
    256
    -1
    True
    False |}]

let%expect_test "long / bigint" =
  (* LONG1 with a single byte 0x80 = -128; LONG decimal with trailing L *)
  print_endline (decode "\x8a\x01\x80.");
  print_endline (decode "L123456789012345678901234L\n.");
  [%expect {|
    -128
    123456789012345678901234 |}]

let%expect_test "floats" =
  print_endline (decode "F-1.25\n.");
  print_endline (decode "G\xbf\xf4\x00\x00\x00\x00\x00\x00.");
  [%expect {|
    -1.25
    -1.25 |}]

let%expect_test "strings, bytes, unicode" =
  print_endline (decode "S'a\\nb'\n.");
  (* proto-0 STRING with escape *)
  print_endline (decode "U\x03abc.");
  (* SHORT_BINSTRING *)
  print_endline (decode "\x8c\x05hello.");
  (* SHORT_BINUNICODE *)
  print_endline (decode "C\x04\x00\x55\xaa\xff.");
  (* SHORT_BINBYTES *)
  print_endline (decode "V\\u0041bc\n.");
  (* proto-0 UNICODE with raw-unicode-escape: A = 'A' *)
  [%expect
    {|
    'a\nb'
    'abc'
    'hello'
    b'\x00U\xaa\xff'
    'Abc' |}]

let%expect_test "list with memo and append" =
  (* EMPTY_LIST, BINPUT 0, MARK, 1, 2, 3, APPENDS, STOP *)
  print_string (decode "]q\x00(K\x01K\x02K\x03e.");
  [%expect "[1, 2, 3]"]

let%expect_test "tuple, dict, set" =
  print_endline (decode "(K\x01K\x02t.");
  (* TUPLE 1 2 *)
  print_endline (decode "}q\x00X\x01\x00\x00\x00aq\x01K\x01s.");
  (* {'a': 1} via SETITEM *)
  print_endline (decode "\x8f\x94(K\x01K\x02\x90.");
  (* EMPTY_SET MEMOIZE MARK 1 2 ADDITEMS *)
  [%expect {|
    (1, 2)
    {'a': 1}
    {1, 2} |}]

let%expect_test "recursive list via memo GET" =
  (* l=[]; l.append(l)  ->  EMPTY_LIST BINPUT0 MARK BINGET0 APPENDS STOP *)
  print_string (decode "]q\x00(h\x00e.");
  [%expect "[[...]]"]

let%expect_test "global and reduce kept structurally" =
  print_endline (decode "cmodule\nName\n.");
  (* GLOBAL, EMPTY_TUPLE, REDUCE, STOP *)
  print_endline (decode "cm\nf\n)R.");
  [%expect {|
    <global module.Name>
    <reduce <global m.f> ()> |}]

let%expect_test "unknown opcode is reported with position" =
  print_string (decode "\xff.");
  [%expect "pickle error at byte 1: unknown opcode 0xff"]
