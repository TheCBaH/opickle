open Opickle

(* Build a source over [s], forcing tiny slices so the multi-slice paths of
   [take]/[line] are exercised even on short inputs. *)
let of_test_string ?(slice_length = 1) s =
  Src.of_reader (Bytesrw.Bytes.Reader.of_string ~slice_length s)

let%expect_test "byte and position tracking" =
  let t = of_test_string "ABC" in
  Printf.printf "pos=%d " (Src.pos t);
  let a = Src.byte t in
  let b = Src.byte t in
  let c = Src.byte t in
  Printf.printf "%c%c%c " (Char.chr a) (Char.chr b) (Char.chr c);
  Printf.printf "pos=%d" (Src.pos t);
  [%expect "pos=0 ABC pos=3"]

let%expect_test "byte past end raises" =
  let t = of_test_string "x" in
  ignore (Src.byte t);
  (try ignore (Src.byte t) with Error.E e -> print_string (Error.to_string e));
  [%expect "pickle error at byte 1: unexpected end of input"]

let%expect_test "take spans slice boundaries" =
  let t = of_test_string ~slice_length:2 "hello world" in
  print_string (Src.take t 5);
  print_char ' ';
  print_string (Src.take t 6);
  [%expect "hello  world"]

let%expect_test "line consumes newline, leaves remainder" =
  let t = of_test_string "first\nsecond\n" in
  print_endline (Src.line t);
  print_endline (Src.line t);
  [%expect {|
    first
    second |}]

let%expect_test "fixed-width numeric readers" =
  (* uint2 0x0102 LE = 513; int4 of 0xFFFFFFFF = -1; uint4 of same = 2^32-1. *)
  Printf.printf "uint2=%d\n" (Src.uint2 (of_test_string "\x01\x02"));
  Printf.printf "int4=%d\n" (Src.int4 (of_test_string "\xff\xff\xff\xff"));
  Printf.printf "uint4=%d\n" (Src.uint4 (of_test_string "\xff\xff\xff\xff"));
  Printf.printf "uint8=%d\n"
    (Src.uint8 (of_test_string "\x05\x00\x00\x00\x00\x00\x00\x00"));
  [%expect {|
    uint2=513
    int4=-1
    uint4=4294967295
    uint8=5 |}]

let%expect_test "float8_be round-trips a double" =
  (* struct.pack(">d", -1.25) = b"\xbf\xf4\x00\x00\x00\x00\x00\x00" *)
  Printf.printf "%g"
    (Src.float8_be (of_test_string "\xbf\xf4\x00\x00\x00\x00\x00\x00"));
  [%expect "-1.25"]
