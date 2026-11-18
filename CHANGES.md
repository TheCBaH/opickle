# Changes

## Unreleased

- Use `Value.view value` before matching on a decoded value. Container views
  contain ordered snapshots and child nodes, and `Value.id` identifies aliases.
  Migrate former `Reduce` matches to `Object { constructor = Reduce _; _ }`.
  Object mutations now live in the ordered `operations` list.
- Replace direct stream-machine calls with `Session` for concatenated pickles
  and shared memo state. `Private.Src` is an unstable test interface.
- `Error.pos` is now an `int64` detection offset. Errors also include the
  opcode offset when available and a machine-readable kind.
- Add decode limits, bounded diagnostic rendering, exact-input validation,
  frame checks and an option to read Python 2 strings as bytes.
- Preserve ordered object mutations, shared and self-referential state,
  constructor families and keyword arguments.
- Test from a standalone source archive with native and js_of_ocaml suites.
