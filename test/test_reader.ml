module B = Bytesrw.Bytes

let reused ?pos slice_length data =
  let first = 2 in
  let backing = Bytes.create (slice_length + first + 1) in
  let offset = ref 0 in
  B.Reader.make ?pos ~slice_length (fun () ->
      let length = min slice_length (String.length data - !offset) in
      if length = 0 then B.Slice.eod
      else begin
        Bytes.fill backing 0 (Bytes.length backing) '\xFF';
        Bytes.blit_string data !offset backing first length;
        offset := !offset + length;
        B.Slice.make backing ~first ~length
      end)
