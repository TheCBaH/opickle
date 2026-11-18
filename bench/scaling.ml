module V = Opickle.Value

let append_list count =
  let out = Buffer.create ((count * 4) + 3) in
  Buffer.add_string out "(l";
  for _ = 1 to count do
    Buffer.add_string out "I7\na"
  done;
  Buffer.add_char out '.';
  Buffer.contents out

let batched_list count =
  let out = Buffer.create ((count * 2) + 3) in
  Buffer.add_string out "\x80\x02]";
  let left = ref count in
  while !left > 0 do
    let batch = min !left 1000 in
    Buffer.add_char out '(';
    for _ = 1 to batch do
      Buffer.add_string out "K\x07"
    done;
    Buffer.add_char out 'e';
    left := !left - batch
  done;
  Buffer.add_char out '.';
  Buffer.contents out

let binary_payload size =
  let prefix = Bytes.of_string "\x80\x03B\x00\x00\x00\x00" in
  Bytes.set_int32_le prefix 3 (Int32.of_int size);
  Bytes.unsafe_to_string prefix ^ String.make size 'x' ^ "."

let decimal digits = "L" ^ String.make digits '7' ^ "L\n."

let check_list size value =
  match V.view value with
  | V.List items when List.length items = size ->
      List.for_all (fun value -> V.as_int value = Some 7L) items
  | _ -> false

let check_payload size value =
  match V.view value with
  | V.Bytes bytes ->
      String.length bytes = size && String.for_all (( = ) 'x') bytes
  | _ -> false

let check_decimal _ value =
  match V.view value with
  | V.Bigint { negative = false; magnitude } -> magnitude <> ""
  | _ -> false

let sample decode iterations =
  Gc.full_major ();
  let start = Sys.time () in
  let allocation = Gc.allocated_bytes () in
  for _ = 1 to iterations do
    ignore (Sys.opaque_identity (decode ()))
  done;
  (Sys.time () -. start, Gc.allocated_bytes () -. allocation)

let benchmark ?slice_length family data size check =
  let decode () =
    let reader = Bytesrw.Bytes.Reader.of_string ?slice_length data in
    Opickle.of_reader_exn reader
  in
  if not (check size (decode ())) then failwith ("invalid benchmark: " ^ family);
  let rec calibrate iterations =
    let elapsed, _ = sample decode iterations in
    if elapsed >= 0.02 || iterations >= 131072 then iterations
    else calibrate (iterations * 2)
  in
  let iterations = calibrate 1 in
  for index = 1 to 5 do
    let elapsed, allocation = sample decode iterations in
    let per_decode = float_of_int iterations in
    Printf.printf "%s,%d,%d,%d,%d,%.9f,%.0f\n%!" family size
      (String.length data) index iterations (elapsed /. per_decode)
      (allocation /. per_decode)
  done

let () =
  Printf.eprintf "OCaml %s (%d-bit), 5 CPU-time samples per workload\n%!"
    Sys.ocaml_version Sys.word_size;
  print_endline
    "workload,size,input_bytes,sample,iterations,cpu_seconds,allocated_bytes";
  List.iter
    (fun count ->
      benchmark "append-list" (append_list count) count check_list;
      benchmark "batched-list" (batched_list count) count check_list)
    [ 256; 1024; 4096; 16384 ];
  List.iter
    (fun size ->
      let data = binary_payload size in
      benchmark "contiguous-bytes" data size check_payload;
      benchmark ~slice_length:4096 "sliced-bytes" data size check_payload)
    [ 4096; 65536; 1048576; 4194304 ];
  List.iter
    (fun digits -> benchmark "decimal" (decimal digits) digits check_decimal)
    [ 32; 128; 512; 2048 ]
