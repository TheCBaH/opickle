# CLI fixture coverage

`dump_fixtures.py` creates all the `dump.t` inputs. The mixed and recursive
values use the system Python 3 pickler. CI uses Python 3.13. The legacy
vectors are handwritten from the pickle opcode definitions. The unresolved
callable is a handwritten `GLOBAL` and `REDUCE`.

| Previous serde-pickle family | Replacement | Coverage retained |
| --- | --- | --- |
| `tests_py3_proto0–5` | `mixed_0–5` | Scalars and bigints, bytes and Unicode, mutable bytes, native and reduced containers, constructor arguments, `BUILD` state and enum reduction. |
| `tests_py2_proto0–2` | `legacy_0–2` with `mixed_0–2` | Python 2 quoted and binary strings, Unicode and Latin-1, and the older data, container and reconstruction opcodes. |
| `test_recursive_proto0–5` | `recursive_0–5` | Memo identity through a list and tuple cycle. |
| `test_unresolvable_global` | `opaque` | Structural representation of an unresolved global and call. |

The original fixtures exercise 48 distinct opcodes. The replacement exercises
all 48 and adds `BINSTRING`. This was measured with Python 3.13.5
`pickletools` against the former fixture pin `af29620`. For opcode enumeration
only, the non-ASCII `STRING` escape in `legacy_0` was replaced by an ASCII
payload so Python 3's disassembler could read it. The CLI test uses the
unmodified fixture.
