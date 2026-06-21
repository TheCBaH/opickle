Decode every fixture pickle and snapshot the rendered value.

Python 3 fixtures across all protocols (the big mixed test_object dict):

  $ pickle_dump tests_py3_proto0.pickle
  {None: None, False: (False, True), 1000: 100000,
  100000000000000000000: 100000000000000000000, 1.0: 1.0,
  <reduce <global _codecs.encode> ('bytes', 'latin1')>:
  <reduce <global _codecs.encode> ('bytes', 'latin1')>, 'string': 'string',
  (1, 2): (1, 2, 3),
  <reduce <global __builtin__.frozenset> ([0, 42],)>:
  <reduce <global __builtin__.frozenset> ([0, 42],)>,
  ():
  [[1, 2, 3], <reduce <global __builtin__.set> ([0, 42],)>, {},
  <reduce <global __builtin__.bytearray>
  (<reduce <global _codecs.encode> ('\x00Uªÿ', 'latin1')>,)>],
  7:
  <object <reduce <global copy_reg._reconstructor>
          (<global __main__.Class>, <global __builtin__.object>, None)>
  () state={'attr': 5}>,
  8:
  <reduce <global copy_reg._reconstructor>
  (<global __main__.NamedTuple>, <global __builtin__.tuple>, ('abc', 10))>,
  9:
  <object <reduce <global copy_reg._reconstructor>
          (<global __main__.DataClass>, <global __builtin__.object>, None)>
  () state={'type': 'abcd', 'quantity': 100}>,
  42: <reduce <global __main__.NormalEnum> (30,)>,
  43: <reduce <global __main__.ByValueEnum> (20,)>}
  $ pickle_dump tests_py3_proto1.pickle
  {None: None, False: (False, True), 1000: 100000,
  100000000000000000000: 100000000000000000000, 1.0: 1.0,
  <reduce <global _codecs.encode> ('bytes', 'latin1')>:
  <reduce <global _codecs.encode> ('bytes', 'latin1')>, 'string': 'string',
  (1, 2): (1, 2, 3),
  <reduce <global __builtin__.frozenset> ([0, 42],)>:
  <reduce <global __builtin__.frozenset> ([0, 42],)>,
  ():
  [[1, 2, 3], <reduce <global __builtin__.set> ([0, 42],)>, {},
  <reduce <global __builtin__.bytearray>
  (<reduce <global _codecs.encode> ('\x00Uªÿ', 'latin1')>,)>],
  7:
  <object <reduce <global copy_reg._reconstructor>
          (<global __main__.Class>, <global __builtin__.object>, None)>
  () state={'attr': 5}>,
  8:
  <reduce <global copy_reg._reconstructor>
  (<global __main__.NamedTuple>, <global __builtin__.tuple>, ('abc', 10))>,
  9:
  <object <reduce <global copy_reg._reconstructor>
          (<global __main__.DataClass>, <global __builtin__.object>, None)>
  () state={'type': 'abcd', 'quantity': 100}>,
  42: <reduce <global __main__.NormalEnum> (30,)>,
  43: <reduce <global __main__.ByValueEnum> (20,)>}
  $ pickle_dump tests_py3_proto2.pickle
  {None: None, False: (False, True), 1000: 100000,
  100000000000000000000: 100000000000000000000, 1.0: 1.0,
  <reduce <global _codecs.encode> ('bytes', 'latin1')>:
  <reduce <global _codecs.encode> ('bytes', 'latin1')>, 'string': 'string',
  (1, 2): (1, 2, 3),
  <reduce <global __builtin__.frozenset> ([0, 42],)>:
  <reduce <global __builtin__.frozenset> ([0, 42],)>,
  ():
  [[1, 2, 3], <reduce <global __builtin__.set> ([0, 42],)>, {},
  <reduce <global __builtin__.bytearray>
  (<reduce <global _codecs.encode> ('\x00Uªÿ', 'latin1')>,)>],
  7: <object <global __main__.Class> () state={'attr': 5}>,
  8: <object <global __main__.NamedTuple> ('abc', 10)>,
  9:
  <object <global __main__.DataClass> ()
  state={'type': 'abcd', 'quantity': 100}>,
  42: <reduce <global __main__.NormalEnum> (30,)>,
  43: <reduce <global __main__.ByValueEnum> (20,)>}
  $ pickle_dump tests_py3_proto3.pickle
  {None: None, False: (False, True), 1000: 100000,
  100000000000000000000: 100000000000000000000, 1.0: 1.0, b'bytes': b'bytes',
  'string': 'string', (1, 2): (1, 2, 3),
  <reduce <global builtins.frozenset> ([0, 42],)>:
  <reduce <global builtins.frozenset> ([0, 42],)>,
  ():
  [[1, 2, 3], <reduce <global builtins.set> ([0, 42],)>, {},
  <reduce <global builtins.bytearray> (b'\x00U\xaa\xff',)>],
  7: <object <global __main__.Class> () state={'attr': 5}>,
  8: <object <global __main__.NamedTuple> ('abc', 10)>,
  9:
  <object <global __main__.DataClass> ()
  state={'type': 'abcd', 'quantity': 100}>,
  42: <reduce <global __main__.NormalEnum> (30,)>,
  43: <reduce <global __main__.ByValueEnum> (20,)>}
  $ pickle_dump tests_py3_proto4.pickle
  {None: None, False: (False, True), 1000: 100000,
  100000000000000000000: 100000000000000000000, 1.0: 1.0, b'bytes': b'bytes',
  'string': 'string', (1, 2): (1, 2, 3),
  frozenset({0, 42}): frozenset({0, 42}),
  ():
  [[1, 2, 3], {0, 42}, {},
  <reduce <global builtins.bytearray> (b'\x00U\xaa\xff',)>],
  7: <object <global __main__.Class> () state={'attr': 5}>,
  8: <object <global __main__.NamedTuple> ('abc', 10)>,
  9:
  <object <global __main__.DataClass> ()
  state={'type': 'abcd', 'quantity': 100}>,
  42: <reduce <global __main__.NormalEnum> (30,)>,
  43: <reduce <global __main__.ByValueEnum> (20,)>}
  $ pickle_dump tests_py3_proto5.pickle
  {None: None, False: (False, True), 1000: 100000,
  100000000000000000000: 100000000000000000000, 1.0: 1.0, b'bytes': b'bytes',
  'string': 'string', (1, 2): (1, 2, 3),
  frozenset({0, 42}): frozenset({0, 42}),
  (): [[1, 2, 3], {0, 42}, {}, bytearray(b'\x00U\xaa\xff')],
  7: <object <global __main__.Class> () state={'attr': 5}>,
  8: <object <global __main__.NamedTuple> ('abc', 10)>,
  9:
  <object <global __main__.DataClass> ()
  state={'type': 'abcd', 'quantity': 100}>,
  42: <reduce <global __main__.NormalEnum> (30,)>,
  43: <reduce <global __main__.ByValueEnum> (20,)>}

Python 2 fixtures (protocols 0-2):

  $ pickle_dump tests_py2_proto0.pickle
  {False: (False, True), 1.0: 1.0,
  100000000000000000000: 100000000000000000000,
  7:
  <object <reduce <global copy_reg._reconstructor>
          (<global __main__.Class>, <global __builtin__.object>, None)>
  () state={'attr': 5}>,
  <reduce <global __builtin__.frozenset> ([0, 42],)>:
  <reduce <global __builtin__.frozenset> ([0, 42],)>, 'string': 'string',
  (1, 2): (1, 2, 3), None: None, 1000: 100000, 'bytes': 'bytes',
  ():
  [[1, 2, 3], <reduce <global __builtin__.set> ([0, 42],)>, {},
  <reduce <global __builtin__.bytearray> ('\x00Uªÿ', 'latin-1')>]}
  $ pickle_dump tests_py2_proto1.pickle
  {False: (False, True), 1.0: 1.0,
  100000000000000000000: 100000000000000000000,
  7:
  <object <reduce <global copy_reg._reconstructor>
          (<global __main__.Class>, <global __builtin__.object>, None)>
  () state={'attr': 5}>,
  <reduce <global __builtin__.frozenset> ([0, 42],)>:
  <reduce <global __builtin__.frozenset> ([0, 42],)>, 'string': 'string',
  (1, 2): (1, 2, 3), None: None, 1000: 100000, 'bytes': 'bytes',
  ():
  [[1, 2, 3], <reduce <global __builtin__.set> ([0, 42],)>, {},
  <reduce <global __builtin__.bytearray> ('\x00Uªÿ', 'latin-1')>]}
  $ pickle_dump tests_py2_proto2.pickle
  {False: (False, True), 1.0: 1.0,
  100000000000000000000: 100000000000000000000,
  7: <object <global __main__.Class> () state={'attr': 5}>,
  <reduce <global __builtin__.frozenset> ([0, 42],)>:
  <reduce <global __builtin__.frozenset> ([0, 42],)>, 'string': 'string',
  (1, 2): (1, 2, 3), None: None, 1000: 100000, 'bytes': 'bytes',
  ():
  [[1, 2, 3], <reduce <global __builtin__.set> ([0, 42],)>, {},
  <reduce <global __builtin__.bytearray> ('\x00Uªÿ', 'latin-1')>]}

Self-referential structure (cycle handling) across all protocols:

  $ pickle_dump test_recursive_proto0.pickle
  [([[...]],)]
  $ pickle_dump test_recursive_proto1.pickle
  [([[...]],)]
  $ pickle_dump test_recursive_proto2.pickle
  [([[...]],)]
  $ pickle_dump test_recursive_proto3.pickle
  [([[...]],)]
  $ pickle_dump test_recursive_proto4.pickle
  [([[...]],)]
  $ pickle_dump test_recursive_proto5.pickle
  [([[...]],)]

A reduce/global that does not resolve to anything is kept structurally:

  $ pickle_dump test_unresolvable_global.pickle
  <reduce <global __main__.ReduceClass> ()>
