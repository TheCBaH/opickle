module V = Opickle.Value
module E = Opickle.Error
module B = Bytesrw.Bytes
module C = Conformance_vectors

let rec project value : C.value =
  match V.view value with
  | V.None -> Null
  | V.Bool b -> Bool b
  | V.Int n -> Int n
  | V.Bigint { negative; magnitude } -> Bigint (negative, magnitude)
  | V.Float f -> Float (Int64.bits_of_float f)
  | V.Bytes b -> Bytes b
  | V.Bytearray b -> Bytearray b
  | V.Readonly_buffer value -> Readonly_buffer (project value)
  | V.Str s -> Text s
  | V.List items -> List (List.map project items)
  | V.Tuple a -> Tuple (List.map project (Array.to_list a))
  | V.Dict entries ->
      Dict (List.map (fun (k, v) -> (project k, project v)) entries)
  | V.Set items -> Set (List.sort compare (List.map project items))
  | V.Frozenset l -> Frozenset (List.sort compare (List.map project l))
  | V.Global { modul; name } -> Global (modul, name)
  | V.Object { constructor; operations } ->
      Object
        (project_constructor constructor, List.map project_operation operations)
  | V.Persistent id -> Persistent (project id)
  | V.Ext code -> Extension code

and project_constructor : V.constructor -> C.constructor = function
  | V.Reduce { func; args } -> Reduce (project func, project args)
  | V.New { cls; args; kwargs } ->
      New (project cls, project args, Option.map project kwargs)
  | V.Legacy_instantiate { cls; args } -> Legacy (project cls, project args)

and project_operation : V.operation -> C.operation = function
  | V.Append value -> Append (project value)
  | V.Set_item (key, value) -> Set_item (project key, project value)
  | V.Add_item value -> Add_item (project value)
  | V.Build state -> Build (project state)

let for_readers data f =
  f "single slice" (B.Reader.of_string ~slice_length:(String.length data) data);
  List.iter
    (fun slice_length ->
      f
        (Printf.sprintf "slice %d" slice_length)
        (B.Reader.of_string ~slice_length data);
      f
        (Printf.sprintf "reused slice %d" slice_length)
        (Test_reader.reused slice_length data))
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
            let reader = Test_reader.reused 3 prefix in
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

let%test_unit "bounded edits of generated values stay inside the result API" =
  let rng = Random.State.make [| C.seed; 77 |] in
  let limits : Opickle.Limits.t =
    {
      input_bytes = 512L;
      payload_bytes = 128;
      line_bytes = 128;
      text_digits = 64;
      marks = 32;
      stack_items = 128;
      memo_entries = 128;
      opcodes = 128;
      work = 1024;
    }
  in
  List.iter
    (fun (name, original, _) ->
      for sample = 1 to 6 do
        let index = Random.State.int rng (String.length original) in
        let edited =
          if Random.State.bool rng then
            String.sub original 0 index
            ^ String.sub original (index + 1)
                (String.length original - index - 1)
          else
            let bytes = Bytes.of_string original in
            Bytes.set bytes index (Char.chr (Random.State.int rng 128));
            Bytes.unsafe_to_string bytes
        in
        let name = Printf.sprintf "%s mutation %d" name sample in
        let input_end = Int64.of_int (String.length edited) in
        let reader = Test_reader.reused 3 edited in
        match Opickle.of_reader ~limits reader with
        | Ok _ -> ()
        | Error error
          when error.pos >= 0L && error.pos <= input_end
               && Option.fold ~none:true
                    ~some:(fun pos -> pos >= 0L && pos <= error.pos)
                    error.opcode_pos ->
            ()
        | Error error ->
            failure name "bounded mutation" edited (E.to_string error)
        | exception exn ->
            failure name "bounded mutation" edited (Printexc.to_string exn)
      done)
    C.random_cases

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

let same_node left right = V.same_node left right && V.id left = V.id right

let check_graph name value =
  match (name, V.view value) with
  | "shared-list", V.List [ first; second ] -> same_node first second
  | "recursive-list", V.List [ child ] -> same_node child value
  | "self-state", V.Object { operations = [ V.Build child ]; _ } ->
      same_node child value
  | "reduce-build-alias", V.List [ first; second ] when same_node first second
    ->
      begin match V.view first with
      | V.Object { constructor = V.Reduce _; operations = [ V.Build state ] } ->
          project state = C.Dict [ (C.Text "attr", C.Int 5L) ]
      | _ -> false
      end
  | _ -> false

let%test_unit "memoized values retain identity through reader refills" =
  List.iter
    (fun (name, data) ->
      for_readers data (fun reader source ->
          match Opickle.of_reader source with
          | Ok value when check_graph name value -> ()
          | Ok _ -> failure name reader data "memo identity or state was lost"
          | Error err -> failure name reader data (E.to_string err)))
    C.graph_cases

let%test_unit "string dictionary lookup uses the most recent entry" =
  let _, data, _ =
    List.find (fun (name, _, _) -> name = "repeated-dict-key") C.cases
  in
  let value = Opickle.of_string_exn data in
  match V.dict_find "a" value with
  | Some found when V.as_int found = Some 8L -> ()
  | _ -> failure "repeated-dict-key" "string" data "expected the last value"
