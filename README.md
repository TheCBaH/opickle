# opickle

A Python [pickle](https://docs.python.org/3/library/pickle.html) parser for OCaml.

`opickle` decodes a pickle byte stream into an OCaml value tree. It implements
the pickle stack machine for **all protocol versions, 0 through 5**, and reads
its input through [bytesrw](https://erratique.ch/software/bytesrw), so any source
that library can wrap (strings, channels, files, filters) can be unpickled.

The opcode set and per-opcode semantics follow CPython's reference
implementation (`Lib/pickle.py`, `Lib/pickletools.py`, `Modules/_pickle.c`).

## Status

Read/parse only — this is not a pickler (no serialization). Opcodes that would
require running Python to fully realize a value (`GLOBAL`, `REDUCE`, `NEWOBJ`,
`BUILD`, `INST`, persistent ids, extension codes) are **kept structurally**
rather than executed, so the caller can interpret them as needed.

## Usage

### Library

```ocaml
match Opickle.of_file "data.pickle" with
| Ok v -> print_endline (Opickle.Value.to_string v)
| Error e -> prerr_endline (Opickle.Error.to_string e)
```

Entry points (each has an `_exn` variant that raises `Opickle.Error.E`):

```ocaml
val of_reader     : Bytesrw.Bytes.Reader.t -> (Value.t, Error.t) result
val of_string     : string -> (Value.t, Error.t) result
val of_in_channel : in_channel -> (Value.t, Error.t) result
val of_file       : string -> (Value.t, Error.t) result
```

### CLI

`pickle_dump` decodes one or more pickle files and prints the rendered values:

```console
$ pickle_dump data.pickle
{None: None, 1000: 100000, 'string': 'string', (1, 2): (1, 2, 3), ...}
```

## Value model

`Opickle.Value.t` covers the builtin Python types reachable through pickle:
`None`, `Bool`, `Int` (`int64`), `Bigint` (arbitrary precision, no external
dependency), `Float`, `Bytes`, `Bytearray`, `Str` (UTF-8), `List`, `Tuple`,
`Dict`, `Set`, `Frozenset`, plus the structural nodes `Global`, `Reduce`,
`Object`, `Persistent` and `Ext`.

Containers are mutable, so self-referential pickles (the memo points a container
at itself) decode correctly; the pretty-printer renders cycles as `[...]` /
`{...}` the way Python's `repr` does.

## Building

The project builds with [dune](https://dune.build/) and depends on `bytesrw` and
`cmdliner`. A devcontainer with the full toolchain is provided under
`.devcontainer/`.

```console
make build      # dune build
make runtest    # dune runtest (inline expect tests + cram smoke tests)
make promote    # update expect/cram snapshots in place
make format     # dune fmt
```

## Tests

`test/` holds one [`ppx_expect`](https://github.com/janestreet/ppx_expect) file
per library module (`src_test.ml`, `value_test.ml`, …) exercising each in
isolation, plus a [cram](https://dune.readthedocs.io/en/stable/tests.html#cram-tests)
test (`dump.t`) that runs `pickle_dump` over a set of fixture pickles (generated
by CPython for protocols 0–5, including Python 2 pickles, recursive structures,
and an unresolvable global) and snapshots the output.

The fixtures live in the `serde-pickle` submodule (`modules/serde-pickle`), used
only as a source of test data. The `cpython` submodule is the format reference.

## License

See [LICENSE](LICENSE).
