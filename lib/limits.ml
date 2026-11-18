type t = {
  payload_bytes : int;
  line_bytes : int;
  text_digits : int;
  input_bytes : int64;
  stack_items : int;
  marks : int;
  memo_entries : int;
  opcodes : int;
  work : int;
}

let unlimited =
  {
    payload_bytes = Sys.max_string_length;
    line_bytes = Sys.max_string_length;
    text_digits = Sys.max_string_length;
    input_bytes = Int64.max_int;
    stack_items = max_int;
    marks = max_int;
    memo_entries = max_int;
    opcodes = max_int;
    work = max_int;
  }

let bounded =
  {
    payload_bytes = 16 * 1024 * 1024;
    line_bytes = 64 * 1024;
    text_digits = 4300;
    input_bytes = 64_000_000L;
    stack_items = 1_000_000;
    marks = 1024;
    memo_entries = 1_000_000;
    opcodes = 4_000_000;
    work = 64_000_000;
  }

let validate limits =
  if
    limits.payload_bytes < 0 || limits.line_bytes < 0 || limits.text_digits < 0
    || limits.input_bytes < 0L || limits.stack_items < 0 || limits.marks < 0
    || limits.memo_entries < 0 || limits.opcodes < 0 || limits.work < 0
  then invalid_arg "Opickle: negative decoder limit"
