# opickle

`opickle` reads Python pickle protocols 0–5 into an OCaml graph. It uses
[bytesrw](https://erratique.ch/software/bytesrw) to read from byte strings,
channels, files and filters. Python calls and names remain symbolic. Protocol 5
supports in-band buffers, including bytearrays. External buffers require a
provider API and currently return `Error.Unsupported`.

The wire reference is [CPython's pickle implementation at f871449](https://github.com/python/cpython/tree/f8714494a9d54910b5142e0b3fe6eb5875e8c09d/Lib)
and the accompanying [C implementation](https://github.com/python/cpython/blob/f8714494a9d54910b5142e0b3fe6eb5875e8c09d/Modules/_pickle.c).

## Reading a pickle

```ocaml
match Opickle.of_file ~limits:Opickle.Limits.bounded ~exact:true "data.pickle" with
| Ok value -> print_endline (Opickle.Value.to_string value)
| Error error -> prerr_endline (Opickle.Error.to_string error)
```

Use `of_string`, `of_reader`, `of_in_channel` or `of_file` and their
exception-raising variants. By default each call reads one pickle prefix and
checks frame boundaries. `~exact:true` requires a single stack result at
`STOP`, no open marks and no trailing bytes. `~check_frames:false` accepts
legacy streams whose frame lengths should be ignored. Errors report a kind,
the detection cursor and, where relevant, the opcode's starting byte offset.
Reader coordinates begin at the supplied reader's current position. File and
bytesrw stream failures are reported as input errors. Unrelated exceptions
raised by caller-supplied readers propagate.

A successful prefix read leaves a caller-owned `Bytes.Reader.t` at the end
of the pickle, even when the reader supplied a slice containing more bytes.
For repeated channel loads or persistent memo state, use `Session`.

```ocaml
let session =
  Opickle.Session.of_reader ~memo:Opickle.Session.Shared
    ~limits:Opickle.Limits.bounded reader
in
match Opickle.Session.load session with
| Ok (Some value) -> print_endline (Opickle.Value.to_string value)
| Ok None -> ()
| Error error -> prerr_endline (Opickle.Error.to_string error)
```

The default session resets its memo for each pickle. Shared sessions preserve
aliases across loads and can update previously returned nodes. A failed load
ends the session. When decoding data from an untrusted source, use explicit
limits sized for the application. Decode limits and printer limits are separate.

## Inspecting values

`Value.view` exposes scalars, containers, and symbolic constructors. Nodes
may be shared or cyclic. Use `Value.same_node` to test identity or
`Value.id` to track visited nodes. A container view snapshots its immediate
entries. Dict entries retain duplicates in insertion order, and
`Value.dict_find` finds the last matching string key.

`Value.Object` retains the constructor family and its ordered operations,
including arguments, keyword arguments where present, appends, item assignments
and `BUILD`. A call whose result is discarded by the pickle program is absent
from the returned graph. Python-level side effects, including custom state
setters, require interpretation beyond the graph. The decoder never imports or
executes them.

Legacy Python 2 string opcodes preserve bytes with
`~legacy_strings:Raw_bytes`. The default converts their Latin-1 bytes to
unicode. Unicode pickle opcodes validate UTF-8 and preserve lone surrogates.
Extension codes are positive `int32` values. Big integer magnitudes are
canonical unsigned little-endian bytes.

`pickle_dump file.pickle` prints a bounded diagnostic rendering. It includes
cycle markers and escapes non-printable bytes and control characters. The
rendering is intended for inspection. It follows Python syntax for common
scalars and containers.

## Building and testing

Use the devcontainer or install the package's opam dependencies. The library
uses `bytesrw`, the CLI uses `cmdliner` and the test suite uses `ppx_expect`
and Python 3. The explicit JavaScript suite also requires Node and
`js_of_ocaml-compiler`. Formatting uses ocamlformat 0.29.0.

```sh
make build
make runtest
opam exec -- dune build @runtest-js
make format
```

The tests run from a source archive and cover structural values, decoder
errors, CPython-generated input, and slice boundaries. See the
[fixture inventory](test/fixtures.md) and [changes](CHANGES.md).

## License

See [LICENSE](LICENSE).
