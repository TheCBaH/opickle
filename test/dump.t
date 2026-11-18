Exercise the CLI with fixtures generated from this source archive.

  $ python3 dump_fixtures.py

Mixed values written by Python 3 at all supported protocol versions.

  $ for protocol in 0 1 2 3 4 5; do
  >   printf 'protocol %s\n' "$protocol"
  >   pickle_dump "mixed_$protocol.pickle"
  > done
  protocol 0
  {'numbers': (None, False, True, 256, -5000, 18889465931478580854784, 1.5),
  'text': 'café', 'bytes': <reduce <global _codecs.encode> ('ink', 'latin1')>,
  'containers':
  ([4, 5], <reduce <global __builtin__.set> ([11, 7],)>,
  <reduce <global __builtin__.frozenset> ([7],)>, {}, ()),
  'mutable_bytes':
  <reduce <global __builtin__.bytearray>
  (<reduce <global _codecs.encode> ('\x00ÿ', 'latin1')>,)>,
  'objects':
  (<object <reduce <global copy_reg._reconstructor>
           (<global __main__.Sample>, <global __builtin__.object>, None)>
   () state={'level': 3, 'name': 'sample'}>,
  <reduce <global copy_reg._reconstructor>
  (<global __main__.Point>, <global __builtin__.tuple>, ('A', 4))>,
  <reduce <global __main__.Direction> (1,)>)}
  protocol 1
  {'numbers': (None, False, True, 256, -5000, 18889465931478580854784, 1.5),
  'text': 'café', 'bytes': <reduce <global _codecs.encode> ('ink', 'latin1')>,
  'containers':
  ([4, 5], <reduce <global __builtin__.set> ([11, 7],)>,
  <reduce <global __builtin__.frozenset> ([7],)>, {}, ()),
  'mutable_bytes':
  <reduce <global __builtin__.bytearray>
  (<reduce <global _codecs.encode> ('\x00ÿ', 'latin1')>,)>,
  'objects':
  (<object <reduce <global copy_reg._reconstructor>
           (<global __main__.Sample>, <global __builtin__.object>, None)>
   () state={'level': 3, 'name': 'sample'}>,
  <reduce <global copy_reg._reconstructor>
  (<global __main__.Point>, <global __builtin__.tuple>, ('A', 4))>,
  <reduce <global __main__.Direction> (1,)>)}
  protocol 2
  {'numbers': (None, False, True, 256, -5000, 18889465931478580854784, 1.5),
  'text': 'café', 'bytes': <reduce <global _codecs.encode> ('ink', 'latin1')>,
  'containers':
  ([4, 5], <reduce <global __builtin__.set> ([11, 7],)>,
  <reduce <global __builtin__.frozenset> ([7],)>, {}, ()),
  'mutable_bytes':
  <reduce <global __builtin__.bytearray>
  (<reduce <global _codecs.encode> ('\x00ÿ', 'latin1')>,)>,
  'objects':
  (<object <global __main__.Sample> () state={'level': 3, 'name': 'sample'}>,
  <object <global __main__.Point> ('A', 4)>,
  <reduce <global __main__.Direction> (1,)>)}
  protocol 3
  {'numbers': (None, False, True, 256, -5000, 18889465931478580854784, 1.5),
  'text': 'café', 'bytes': b'ink',
  'containers':
  ([4, 5], <reduce <global builtins.set> ([11, 7],)>,
  <reduce <global builtins.frozenset> ([7],)>, {}, ()),
  'mutable_bytes': <reduce <global builtins.bytearray> (b'\x00\xff',)>,
  'objects':
  (<object <global __main__.Sample> () state={'level': 3, 'name': 'sample'}>,
  <object <global __main__.Point> ('A', 4)>,
  <reduce <global __main__.Direction> (1,)>)}
  protocol 4
  {'numbers': (None, False, True, 256, -5000, 18889465931478580854784, 1.5),
  'text': 'café', 'bytes': b'ink',
  'containers': ([4, 5], {11, 7}, frozenset({7}), {}, ()),
  'mutable_bytes': <reduce <global builtins.bytearray> (b'\x00\xff',)>,
  'objects':
  (<object <global __main__.Sample> () state={'level': 3, 'name': 'sample'}>,
  <object <global __main__.Point> ('A', 4)>,
  <reduce <global __main__.Direction> (1,)>)}
  protocol 5
  {'numbers': (None, False, True, 256, -5000, 18889465931478580854784, 1.5),
  'text': 'café', 'bytes': b'ink',
  'containers': ([4, 5], {11, 7}, frozenset({7}), {}, ()),
  'mutable_bytes': bytearray(b'\x00\xff'),
  'objects':
  (<object <global __main__.Sample> () state={'level': 3, 'name': 'sample'}>,
  <object <global __main__.Point> ('A', 4)>,
  <reduce <global __main__.Direction> (1,)>)}

Python 2 text and binary string forms, including Latin-1 bytes and Unicode.
Normalize line breaks for this case because Format's Unicode width varies
between supported compiler versions.

  $ for protocol in 0 1 2; do
  >   rendered=$(pickle_dump "legacy_$protocol.pickle") || exit $?
  >   printf '%s\n' "$(printf '%s' "$rendered" | tr '\n' ' ')"
  > done
  ['café', 'café', <reduce <global __builtin__.bytearray> ('Bÿ', 'latin-1')>]
  ['café', 'café', 'wide', <reduce <global __builtin__.bytearray> ('Bÿ', 'latin-1')>]
  ['café', 'café', 'wide', <reduce <global __builtin__.bytearray> ('Bÿ', 'latin-1')>]

A recursive list reached through an immutable tuple at all protocol versions.

  $ for protocol in 0 1 2 3 4 5; do
  >   pickle_dump "recursive_$protocol.pickle"
  > done
  [([[...]],)]
  [([[...]],)]
  [([[...]],)]
  [([[...]],)]
  [([[...]],)]
  [([[...]],)]

An unresolved global used as a callable is kept structurally.

  $ pickle_dump opaque.pickle
  <reduce <global fixture_only.Unknown> ()>
