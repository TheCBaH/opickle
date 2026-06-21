# CLAUDE.md

`opickle` is an OCaml library that parses the Python pickle format (protocols
0–5) into a value tree, reading input through `bytesrw`. Parser only — no
serialization.

## Commands

Everything runs through `opam exec` via the Makefile: `make build`,
`make runtest`, `make promote` (update expect/cram snapshots), `make format`.
CI runs build + runtest + format, then fails if `git status` is not pristine —
so keep snapshots promoted and run `make format` before finishing.

## Layout

- `lib/` — the `opickle` library: `opcode.ml` (opcode bytes), `src.ml` (buffered
  byte source over `Bytesrw.Bytes.Reader`), `value.ml` (value model + `pp`),
  `error.ml`, `unpickler.ml` (the stack machine over all 68 opcodes + Python
  string codecs), `opickle.ml` (public `of_*` facade; re-exports `Src`/
  `Unpickler` for testing).
- `bin/pickle_dump.ml` — CLI used by the cram tests.
- `test/` — `*_test.ml` inline `ppx_expect` tests (one per lib module) plus
  `dump.t`, a cram smoke test running `pickle_dump` over the fixtures.
- `modules/` — submodules: `cpython` is **the** format reference; `serde-pickle`
  supplies fixture pickles only (`test/data/*.pickle`) — do not read its source.

## Conventions

- **No `Obj`.** Cycle detection in `value.ml` uses physical equality (`==`).
- `Value.pp` uses `Format` boxes; long structures wrap at the margin.
- Reconstruction opcodes (`Global`/`Reduce`/`Object`/`Persistent`/`Ext`) are
  surfaced structurally, never executed.
- Big integers use `Value.Bigint` (sign + LE magnitude), no `zarith`.
- CPython is the source of truth for opcode behavior (`Lib/pickle.py` is the
  readable reference).
- Expect tests live in `test/`, not in `lib/`. Single-line expectations use a
  plain string (`[%expect "..."]`); multi-line use `{| ... |}`.

## Gotcha

OCaml lexes string/char literals **inside comments** — a stray `"` or `'` (e.g.
an apostrophe) breaks the build with "Comment not terminated". Keep comment
punctuation balanced.
