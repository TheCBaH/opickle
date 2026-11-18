# CLAUDE.md

`opickle` is a read-only OCaml structural parser for Python pickle 0–5.
It reads through `bytesrw` and preserves graph identity. Python callables and
constructors remain data.

## Commands

The Makefile runs `make build`, `make runtest`, `make promote` and
`make format` with opam. Use `opam exec -- dune build @runtest-js` for
JavaScript tests. Fixtures and tests run from a standalone source archive.

## Layout

- `lib/` contains the byte source, opcode interpreter, graph and documented
  public API in `opickle.mli`.
- `bin/pickle_dump.ml` provides a bounded diagnostic CLI.
- `test/` contains inline tests, cram tests and CPython-generated conformance
  fixtures. See [test/fixtures.md](test/fixtures.md).

Use [the pinned CPython source](https://github.com/python/cpython/blob/f8714494a9d54910b5142e0b3fe6eb5875e8c09d/Lib/pickle.py)
as the reference and keep Python reconstruction symbolic.
