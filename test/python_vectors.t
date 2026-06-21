Cross-check opickle's pretty-printer against the real CPython pickler: for
each named case in genpickle.py, build a pickle of that Python value at
every protocol, decode it with the OCaml unpickler (via pickle_dump), and
compare the result against Python's own repr() of the original value. The
"want" side here is computed by Python, not hand-written, so a divergence
is a genuine bug, not a stale golden string.

  $ check() {
  >   case=$1; shift
  >   want=$(python3 genpickle.py repr "$case")
  >   for p in "$@"; do
  >     python3 genpickle.py "$p" "$case" v.pickle
  >     got=$(pickle_dump v.pickle)
  >     if [ "$got" = "$want" ]; then printf '%s proto%s: ok (%s)\n' "$case" "$p" "$got"
  >     else printf '%s proto%s: FAIL python=<%s> ocaml=<%s>\n' "$case" "$p" "$want" "$got"; fi
  >   done
  > }

None, bool, int (mirrors "integers, bool, none" in unpickler_test.ml) -
opickle never wraps these in a reduce/global surrogate, so the rendering
must equal Python's repr() at every protocol:

  $ check none        0 1 2 3 4 5
  none proto0: ok (None)
  none proto1: ok (None)
  none proto2: ok (None)
  none proto3: ok (None)
  none proto4: ok (None)
  none proto5: ok (None)
  $ check true        0 1 2 3 4 5
  true proto0: ok (True)
  true proto1: ok (True)
  true proto2: ok (True)
  true proto3: ok (True)
  true proto4: ok (True)
  true proto5: ok (True)
  $ check false       0 1 2 3 4 5
  false proto0: ok (False)
  false proto1: ok (False)
  false proto2: ok (False)
  false proto3: ok (False)
  false proto4: ok (False)
  false proto5: ok (False)
  $ check int_small   0 1 2 3 4 5
  int_small proto0: ok (42)
  int_small proto1: ok (42)
  int_small proto2: ok (42)
  int_small proto3: ok (42)
  int_small proto4: ok (42)
  int_small proto5: ok (42)
  $ check int_mid     0 1 2 3 4 5
  int_mid proto0: ok (256)
  int_mid proto1: ok (256)
  int_mid proto2: ok (256)
  int_mid proto3: ok (256)
  int_mid proto4: ok (256)
  int_mid proto5: ok (256)
  $ check int_neg     0 1 2 3 4 5
  int_neg proto0: ok (-1)
  int_neg proto1: ok (-1)
  int_neg proto2: ok (-1)
  int_neg proto3: ok (-1)
  int_neg proto4: ok (-1)
  int_neg proto5: ok (-1)

Bigints (mirrors "long / bigint"):

  $ check bigint_neg  0 1 2 3 4 5
  bigint_neg proto0: ok (-128)
  bigint_neg proto1: ok (-128)
  bigint_neg proto2: ok (-128)
  bigint_neg proto3: ok (-128)
  bigint_neg proto4: ok (-128)
  bigint_neg proto5: ok (-128)
  $ check bigint_huge 0 1 2 3 4 5
  bigint_huge proto0: ok (123456789012345678901234)
  bigint_huge proto1: ok (123456789012345678901234)
  bigint_huge proto2: ok (123456789012345678901234)
  bigint_huge proto3: ok (123456789012345678901234)
  bigint_huge proto4: ok (123456789012345678901234)
  bigint_huge proto5: ok (123456789012345678901234)

Floats (mirrors "floats"):

  $ check float       0 1 2 3 4 5
  float proto0: ok (-1.25)
  float proto1: ok (-1.25)
  float proto2: ok (-1.25)
  float proto3: ok (-1.25)
  float proto4: ok (-1.25)
  float proto5: ok (-1.25)

Strings, including a non-ASCII string that forces the proto-0 UNICODE opcode
through a real raw-unicode-escape \u-escape - the same codec path as the
hand-rolled "Abc" fragment in unpickler_test.ml:

  $ check str_escape    0 1 2 3 4 5
  str_escape proto0: ok ('a\nb')
  str_escape proto1: ok ('a\nb')
  str_escape proto2: ok ('a\nb')
  str_escape proto3: ok ('a\nb')
  str_escape proto4: ok ('a\nb')
  str_escape proto5: ok ('a\nb')
  $ check str_nonascii  0 1 2 3 4 5
  str_nonascii proto0: ok ('Āabc')
  str_nonascii proto1: ok ('Āabc')
  str_nonascii proto2: ok ('Āabc')
  str_nonascii proto3: ok ('Āabc')
  str_nonascii proto4: ok ('Āabc')
  str_nonascii proto5: ok ('Āabc')

bytes: CPython only gained a dedicated BINBYTES/SHORT_BINBYTES opcode in
protocol 3 - at protocol 0-2 a bytes object pickles itself via a
_codecs.encode reduce (see the "structural surrogates" section below), so
the repr() comparison only holds from protocol 3 onward:

  $ check bytes_short 3 4 5
  bytes_short proto3: ok (b'\x00U\xaa\xff')
  bytes_short proto4: ok (b'\x00U\xaa\xff')
  bytes_short proto5: ok (b'\x00U\xaa\xff')

List, tuple, dict (mirrors "list with memo and append" and "tuple, dict,
set"):

  $ check list_simple  0 1 2 3 4 5
  list_simple proto0: ok ([1, 2, 3])
  list_simple proto1: ok ([1, 2, 3])
  list_simple proto2: ok ([1, 2, 3])
  list_simple proto3: ok ([1, 2, 3])
  list_simple proto4: ok ([1, 2, 3])
  list_simple proto5: ok ([1, 2, 3])
  $ check tuple_pair    0 1 2 3 4 5
  tuple_pair proto0: ok ((1, 2))
  tuple_pair proto1: ok ((1, 2))
  tuple_pair proto2: ok ((1, 2))
  tuple_pair proto3: ok ((1, 2))
  tuple_pair proto4: ok ((1, 2))
  tuple_pair proto5: ok ((1, 2))
  $ check dict_simple   0 1 2 3 4 5
  dict_simple proto0: ok ({'a': 1})
  dict_simple proto1: ok ({'a': 1})
  dict_simple proto2: ok ({'a': 1})
  dict_simple proto3: ok ({'a': 1})
  dict_simple proto4: ok ({'a': 1})
  dict_simple proto5: ok ({'a': 1})

set: dedicated EMPTY_SET/ADDITEMS/FROZENSET opcodes only exist from
protocol 4 - below that a set pickles via a `(set, (list(self),))` reduce
(see below), so the repr() comparison only holds from protocol 4 onward:

  $ check set_simple   4 5
  set_simple proto4: ok ({1, 2})
  set_simple proto5: ok ({1, 2})

Recursive list via memo (mirrors "recursive list via memo GET"). Python's
own repr() already renders self-reference as "[[...]]", same as opickle's
cycle detection:

  $ check recursive_list 0 1 2 3 4 5
  recursive_list proto0: ok ([[...]])
  recursive_list proto1: ok ([[...]])
  recursive_list proto2: ok ([[...]])
  recursive_list proto3: ok ([[...]])
  recursive_list proto4: ok ([[...]])
  recursive_list proto5: ok ([[...]])

Structural surrogates: by design opickle never executes GLOBAL/REDUCE/
OBJECT/PERSISTENT - it surfaces them as <global ...>/<reduce ...>/
<object ...>/<persid ...> rather than reconstructing the real Python
object (see CLAUDE.md). There is no repr() to compare against here, so
these stay as plain promoted snapshots; they still cross-check that real
CPython output decodes the way unpickler_test.ml's hand-rolled fragments
say it should.

bytes/set below protocol 3/4 (the reduce-based legacy encoding), a class
pickled as a bare GLOBAL (or STACK_GLOBAL at proto>=4, which no hand-rolled
fragment covers), a __reduce__ instance, and a NEWOBJ/copy_reg instance:

  $ for p in 0 1 2 3 4 5; do
  >   python3 genpickle.py $p bytes_short v.pickle
  >   printf 'bytes_short proto%d: ' "$p"; pickle_dump v.pickle
  >   python3 genpickle.py $p set_simple v.pickle
  >   printf 'set_simple proto%d: ' "$p"; pickle_dump v.pickle
  >   python3 genpickle.py $p class_object v.pickle
  >   printf 'class_object proto%d: ' "$p"; pickle_dump v.pickle
  >   python3 genpickle.py $p reduce_instance v.pickle
  >   printf 'reduce_instance proto%d: ' "$p"; pickle_dump v.pickle
  >   python3 genpickle.py $p class_instance v.pickle
  >   printf 'class_instance proto%d: ' "$p"; pickle_dump v.pickle
  > done
  bytes_short proto0: <reduce <global _codecs.encode> ('\x00Uªÿ', 'latin1')>
  set_simple proto0: <reduce <global __builtin__.set> ([1, 2],)>
  class_object proto0: <global __main__.Sample>
  reduce_instance proto0: <reduce <global __main__.ReduceSample> ()>
  class_instance proto0: <object <reduce <global copy_reg._reconstructor>
          (<global __main__.Sample>, <global __builtin__.object>, None)>
  () state={'attr': 5}>
  bytes_short proto1: <reduce <global _codecs.encode> ('\x00Uªÿ', 'latin1')>
  set_simple proto1: <reduce <global __builtin__.set> ([1, 2],)>
  class_object proto1: <global __main__.Sample>
  reduce_instance proto1: <reduce <global __main__.ReduceSample> ()>
  class_instance proto1: <object <reduce <global copy_reg._reconstructor>
          (<global __main__.Sample>, <global __builtin__.object>, None)>
  () state={'attr': 5}>
  bytes_short proto2: <reduce <global _codecs.encode> ('\x00Uªÿ', 'latin1')>
  set_simple proto2: <reduce <global __builtin__.set> ([1, 2],)>
  class_object proto2: <global __main__.Sample>
  reduce_instance proto2: <reduce <global __main__.ReduceSample> ()>
  class_instance proto2: <object <global __main__.Sample> () state={'attr': 5}>
  bytes_short proto3: b'\x00U\xaa\xff'
  set_simple proto3: <reduce <global builtins.set> ([1, 2],)>
  class_object proto3: <global __main__.Sample>
  reduce_instance proto3: <reduce <global __main__.ReduceSample> ()>
  class_instance proto3: <object <global __main__.Sample> () state={'attr': 5}>
  bytes_short proto4: b'\x00U\xaa\xff'
  set_simple proto4: {1, 2}
  class_object proto4: <global __main__.Sample>
  reduce_instance proto4: <reduce <global __main__.ReduceSample> ()>
  class_instance proto4: <object <global __main__.Sample> () state={'attr': 5}>
  bytes_short proto5: b'\x00U\xaa\xff'
  set_simple proto5: {1, 2}
  class_object proto5: <global __main__.Sample>
  reduce_instance proto5: <reduce <global __main__.ReduceSample> ()>
  class_instance proto5: <object <global __main__.Sample> () state={'attr': 5}>

Persistent id via a custom Pickler.persistent_id (PERSID at proto 0,
BINPERSID at proto>=1) - not exercised anywhere else in the test suite.
PERSID is always textual even though the id pushed here is the int 3,
hence the quoting difference at protocol 0:

  $ for p in 0 1 2 3 4 5; do
  >   python3 genpickle.py $p persistent_id v.pickle
  >   printf 'proto%d: ' "$p"; pickle_dump v.pickle
  > done
  proto0: <persid '3'>
  proto1: <persid 3>
  proto2: <persid 3>
  proto3: <persid 3>
  proto4: <persid 3>
  proto5: <persid 3>
