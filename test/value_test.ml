open Opickle.Value

let%expect_test "scalars" =
  List.iter
    (fun v -> print_endline (to_string v))
    [
      None;
      Bool true;
      Bool false;
      Int 42L;
      Int (-7L);
      Float 1.0;
      Float (-1.25);
      Str "hi";
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
  let bigint negative magnitude = to_string (Bigint { negative; magnitude }) in
  print_endline (bigint false "\x00\x01");
  (* 256 *)
  print_endline (bigint false "\xff\xff");
  (* 65535 *)
  (* 2^64 = 18446744073709551616, little-endian = eight 0x00 then 0x01 *)
  print_endline (bigint false "\x00\x00\x00\x00\x00\x00\x00\x00\x01");
  (* 10^20, little-endian of 0x056BC75E2D63100000 *)
  print_endline (bigint false "\x00\x00\x10\x63\x2d\x5e\xc7\x6b\x05");
  print_endline (bigint true "\x80\x00");
  (* magnitude 0x0080 = 128, negated *)
  [%expect
    {|
    256
    65535
    18446744073709551616
    100000000000000000000
    -128 |}]

let%expect_test "containers and bytes" =
  print_endline (to_string (List (ref [ Int 1L; Int 2L; Int 3L ])));
  print_endline (to_string (Tuple [| Int 1L |]));
  print_endline (to_string (Tuple [||]));
  print_endline (to_string (Dict (ref [ (Str "a", Int 1L) ])));
  print_endline (to_string (Set (ref [])));
  print_endline (to_string (Bytes "\x00\x55\xaa\xff"));
  [%expect
    {|
    [1, 2, 3]
    (1,)
    ()
    {'a': 1}
    set()
    b'\x00U\xaa\xff' |}]

let%expect_test "recursive list renders with ellipsis" =
  (* Build the cycle the way the unpickler does: one node, memoised, referenced
     from inside its own ref cell (so the node is physically reused). *)
  let r = ref [] in
  let node = List r in
  r := [ node ];
  print_string (to_string node);
  [%expect "[[...]]"]

let%expect_test "non-data nodes" =
  print_endline
    (to_string (Global { modul = "copyreg"; name = "_reconstructor" }));
  print_endline
    (to_string
       (Reduce { func = Global { modul = "m"; name = "f" }; args = Tuple [||] }));
  print_endline (to_string (Persistent (Int 3L)));
  [%expect
    {|
    <global copyreg._reconstructor>
    <reduce <global m.f> ()>
    <persid 3> |}]
