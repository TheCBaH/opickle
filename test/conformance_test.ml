module V = Opickle.Value
module E = Opickle.Error
module B = Bytesrw.Bytes
module C = Conformance_vectors

let rec project : V.t -> C.value = function
  | V.None -> Null
  | V.Bool b -> Bool b
  | V.Int n -> Int n
  | V.Bigint { negative; magnitude } -> Bigint (negative, magnitude)
  | V.Float f -> Float (Int64.bits_of_float f)
  | V.Bytes b -> Bytes b
  | V.Bytearray b -> Bytearray b
  | V.Readonly_buffer value -> Readonly_buffer (project value)
  | V.Str s -> Text s
  | V.List r -> List (List.map project !r)
  | V.Tuple a -> Tuple (List.map project (Array.to_list a))
  | V.Dict r -> Dict (List.map (fun (k, v) -> (project k, project v)) !r)
  | V.Set r -> Set (List.sort compare (List.map project !r))
  | V.Frozenset l -> Frozenset (List.sort compare (List.map project l))
  | V.Global { modul; name } -> Global (modul, name)
  | V.Reduce { func; args } -> Reduce (project func, project args)
  | V.Object { cls; args; state } ->
      Object (project cls, project args, Option.map project state)
  | V.Persistent id -> Persistent (project id)
  | V.Ext code -> Extension code

let reused_reader slice_length data =
  let first = 2 in
  let backing = Bytes.create (slice_length + first + 1) in
  let pos = ref 0 in
  B.Reader.make ~slice_length (fun () ->
      let length = min slice_length (String.length data - !pos) in
      if length = 0 then B.Slice.eod
      else begin
        Bytes.fill backing 0 (Bytes.length backing) '\xFF';
        Bytes.blit_string data !pos backing first length;
        pos := !pos + length;
        B.Slice.make backing ~first ~length
      end)

let for_readers data f =
  f "single slice" (B.Reader.of_string ~slice_length:(String.length data) data);
  List.iter
    (fun slice_length ->
      f
        (Printf.sprintf "slice %d" slice_length)
        (B.Reader.of_string ~slice_length data);
      f
        (Printf.sprintf "reused slice %d" slice_length)
        (reused_reader slice_length data))
    [ 1; 2; 3; 7 ]

let failure name reader data detail =
  failwith
    (Printf.sprintf "%s via %s (%s, seed %d, data %S): %s" name reader
       C.generator C.seed data detail)

let check_value name data expected =
  for_readers data (fun reader source ->
      match Opickle.of_reader source with
      | Ok actual ->
          if project actual <> expected then
            failure name reader data "value differs from expected structure"
      | Error err -> failure name reader data (E.to_string err))

let check_external name data opcode_pos =
  for_readers data (fun reader source ->
      match Opickle.of_reader source with
      | Error { E.kind = Unsupported; opcode_pos = Some start; pos; _ }
        when start = Int64.of_int opcode_pos
             && pos = Int64.of_int (opcode_pos + 1) ->
          ()
      | Error err -> failure name reader data (E.to_string err)
      | Ok _ -> failure name reader data "external buffer was accepted")

let%test_unit "opcode fixtures keep semantics across reader layouts" =
  List.iter
    (fun (name, data, expected) ->
      match expected with
      | C.Value value -> check_value name data value
      | C.External_buffer start -> check_external name data start)
    C.cases

let%test_unit "seeded CPython values agree over protocols 0–5" =
  List.iter
    (fun (name, data, expected) ->
      match expected with
      | C.Value value -> check_value name data value
      | C.External_buffer _ -> failure name "generator" data "unexpected buffer")
    C.random_cases

let%test_unit "every proper prefix of a supported vector is truncated" =
  List.iter
    (fun (name, data, expected) ->
      match expected with
      | C.External_buffer _ -> ()
      | C.Value _ ->
          for cut = 0 to String.length data - 1 do
            let prefix = String.sub data 0 cut in
            let reader = reused_reader 3 prefix in
            match Opickle.of_reader reader with
            | Error { E.kind = Truncated; pos; _ } when pos = Int64.of_int cut
              ->
                ()
            | Error err ->
                failure name "truncated prefix" prefix (E.to_string err)
            | Ok _ ->
                failure name "truncated prefix" prefix
                  "prefix decoded successfully"
          done)
    C.cases

let%test_unit "malformed stack and memo operations report instruction offsets" =
  List.iter
    (fun (name, data, opcode_pos, pos) ->
      for_readers data (fun reader source ->
          match Opickle.of_reader source with
          | Error { E.kind = Malformed; opcode_pos = Some start; pos = at; _ }
            when start = opcode_pos && at = pos ->
              ()
          | Error err -> failure name reader data (E.to_string err)
          | Ok _ -> failure name reader data "invalid instruction was accepted"))
    [
      ("unmatched mark", "N1.", 1L, 2L);
      ("missing memo key", "g4\n.", 0L, 3L);
      ("odd dict entries", "(Nd.", 2L, 3L);
      ("wrong stack type", "NNa.", 2L, 3L);
      ("readonly non-buffer", "\x80\x05N\x98.", 3L, 4L);
    ]

let framed length data =
  let header = Bytes.of_string "\x80\x04\x95\000\000\000\000\000\000\000\000" in
  Bytes.set_int64_le header 3 length;
  Bytes.unsafe_to_string header ^ data

let%test_unit "frame validation distinguishes fixed operands, lines and stop" =
  List.iter
    (fun (name, data, expected, opcode_pos, pos) ->
      for_readers data (fun reader source ->
          match Opickle.of_reader source with
          | Error { E.kind = Malformed; opcode_pos = Some start; pos = at; _ }
            when start = opcode_pos && at = pos ->
              ()
          | Error err -> failure name reader data (E.to_string err)
          | Ok _ -> failure name reader data "frame violation was accepted");
      for_readers data (fun reader source ->
          match Opickle.of_reader ~check_frames:false source with
          | Ok value when project value = expected -> ()
          | Ok _ -> failure name reader data "unexpected permissive decode"
          | Error err -> failure name reader data (E.to_string err)))
    [
      ( "fixed-width operand crosses frame",
        framed 1L "K\x07.",
        C.Int 7L,
        11L,
        12L );
      ("line operand crosses frame", framed 2L "I7\n.", C.Int 7L, 11L, 13L);
      ("early STOP within frame", framed 3L "N.N", C.Null, 12L, 13L);
    ]

let%test_unit "legacy strings can preserve bytes without affecting Unicode" =
  let data = "(U\x01\xffV\\u00ff\nt." in
  for_readers data (fun reader source ->
      match Opickle.of_reader ~legacy_strings:Raw_bytes source with
      | Ok value when project value = C.Tuple [ C.Bytes "\xff"; C.Text "ÿ" ] ->
          ()
      | Ok _ ->
          failure "legacy string policy" reader data
            "expected raw bytes and Unicode"
      | Error err ->
          failure "legacy string policy" reader data (E.to_string err))

let%test_unit "memoized values retain identity through reader refills" =
  List.iter
    (fun (name, data) ->
      for_readers data (fun reader source ->
          match (name, Opickle.of_reader source) with
          | "shared-list", Ok (V.List { contents = [ first; second ] })
            when first == second ->
              ()
          | "recursive-list", Ok (V.List { contents = [ child ] } as parent)
            when child == parent ->
              ()
          | "self-state", Ok (V.Object { state = Some child; _ } as parent)
            when child == parent ->
              ()
          | _, Ok _ -> failure name reader data "memo identity was lost"
          | _, Error err -> failure name reader data (E.to_string err)))
    C.graph_cases
