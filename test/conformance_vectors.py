"""Emit portable OCaml conformance vectors using CPython and pickletools."""

import io
import pickle
import pickletools
import random
import struct
import sys


class Sample:
    def __init__(self):
        self.attr = 5


class WithKeywords:
    def __getnewargs_ex__(self):
        return (3,), {"name": "sample"}


def set_state(obj, state):
    obj.attr = state


class WithStateSetter:
    def __reduce__(self):
        return (WithStateSetter, (), 5, None, None, set_state)


class SelfState:
    def __getstate__(self):
        return self


class PersistentIdPickler(pickle.Pickler):
    def persistent_id(self, obj):
        return "token" if isinstance(obj, Sample) else None


def ocaml_string(value):
    if isinstance(value, str):
        value = value.encode("utf-8")
    body = "".join(
        chr(byte)
        if 32 <= byte <= 126 and byte not in (34, 92)
        else f"\\{byte:03}"
        for byte in value
    )
    return f'"{body}"'


def ocaml_list(values):
    return "[" + "; ".join(values) + "]"


def frame(data):
    return b"\x95" + struct.pack("<Q", len(data)) + data


def expected_value(value):
    if value is None:
        return "Null"
    if isinstance(value, bool):
        return f"Bool {str(value).lower()}"
    if isinstance(value, int):
        if -(1 << 63) <= value < (1 << 63):
            return f"Int ({value}L)"
        magnitude = abs(value).to_bytes((abs(value).bit_length() + 7) // 8, "little")
        negative = str(value < 0).lower()
        return f"Bigint ({negative}, {ocaml_string(magnitude)})"
    if isinstance(value, float):
        bits = struct.unpack(">q", struct.pack(">d", value))[0]
        return f"Float ({bits}L)"
    if isinstance(value, str):
        return f"Text {ocaml_string(value)}"
    if isinstance(value, bytes):
        return f"Bytes {ocaml_string(value)}"
    if isinstance(value, bytearray):
        return f"Bytearray {ocaml_string(value)}"
    if isinstance(value, list):
        return "List " + ocaml_list(map(expected_value, value))
    if isinstance(value, tuple):
        return "Tuple " + ocaml_list(map(expected_value, value))
    if isinstance(value, dict):
        return "Dict " + ocaml_list(
            f"({expected_value(k)}, {expected_value(v)})" for k, v in value.items()
        )
    if isinstance(value, (set, frozenset)):
        constructor = "Set" if isinstance(value, set) else "Frozenset"
        return constructor + " " + ocaml_list(map(expected_value, sorted(value)))
    raise TypeError(type(value))


SEED = 0x0C1C


def random_value(rng, depth, protocol):
    scalars = [None, True, False, 0, 42, -255, 1.25, -0.0, "", "text", "é\n\\U"]
    if protocol >= 3:
        scalars += [b"", b"\x00\x93\xff"]
    if depth == 0:
        return rng.choice(scalars)
    constructor = rng.randrange(4)
    if constructor == 3:
        return rng.choice(scalars)
    values = [random_value(rng, depth - 1, protocol) for _ in range(rng.randrange(4))]
    if constructor == 0:
        return values
    if constructor == 1:
        return tuple(values)
    return {f"k{index}": value for index, value in enumerate(values)}


def main():
    covered = set()
    names = set()

    def check_opcodes(name, data, required):
        if name in names:
            raise AssertionError(f"duplicate vector: {name}")
        names.add(name)
        instructions = list(pickletools.genops(data))
        opcodes = {opcode.name for opcode, _, _ in instructions}
        if instructions[-1][0].name != "STOP" or instructions[-1][2] != len(data) - 1:
            raise AssertionError(f"{name}: STOP must end the fixture")
        missing = set(required) - opcodes
        if missing:
            raise AssertionError(f"{name}: missing {sorted(missing)}, got {sorted(opcodes)}")
        covered.update(opcodes)

    def emit(name, data, outcome, required):
        check_opcodes(name, data, required)
        print(f"  ({ocaml_string(name)}, {ocaml_string(data)}, {outcome});")

    def wire(name, data, value, *required):
        emit(name, data, f"Value ({value})", required)

    def generated(name, value, protocol, *required):
        wire(name, pickle.dumps(value, protocol), expected_value(value), *required)

    print(
        """type value =
  | Null
  | Bool of bool
  | Int of int64
  | Bigint of bool * string
  | Float of int64
  | Text of string
  | Bytes of string
  | Bytearray of string
  | Readonly_buffer of value
  | List of value list
  | Tuple of value list
  | Dict of (value * value) list
  | Set of value list
  | Frozenset of value list
  | Global of string * string
  | Reduce of value * value
  | Object of value * value * value option
  | Persistent of value
  | Extension of int32

type outcome = Value of value | External_buffer of int

let cases = ["""
    )

    generated("null", None, 0, "NONE", "STOP")
    generated("text-integer", 42, 0, "INT")
    generated("text-bool", True, 0, "INT")
    generated("text-long", 2**40, 0, "LONG")
    generated("text-float", -1.25, 0, "FLOAT")
    generated("text-unicode", "ē", 0, "UNICODE")
    generated("text-list", [5], 0, "LIST", "APPEND", "PUT")
    generated("text-dict", {"a": 1}, 0, "DICT", "SETITEM")
    generated("text-tuple", (3,), 0, "TUPLE", "MARK")
    generated("binary-integers", [1, 256, -255], 2, "BININT", "BININT1", "BININT2")
    generated("signed-binint", [-(2**31), 2**31 - 1], 2, "BININT")
    generated("binary-long", -(2**39), 2, "LONG1")
    generated("text-bigint", [2**63, -(2**63 + 1), -(2**71)], 0, "LONG")
    generated("binary-bigint", [2**63, -(2**63 + 1), -(2**71)], 2, "LONG1")
    generated("binary-float", -0.0, 2, "BINFLOAT")
    generated("binary-bool", [True, False], 2, "NEWTRUE", "NEWFALSE")
    generated(
        "binary-tuple",
        [(), (3,), (4, 5), (6, 7, 8)],
        2,
        "EMPTY_TUPLE",
        "TUPLE1",
        "TUPLE2",
        "TUPLE3",
    )
    generated("binary-bytes", b"b" * 256, 3, "BINBYTES")
    generated("short-bytes", b"\x00\xff", 3, "SHORT_BINBYTES")
    generated("binary-unicode", "ē" * 128, 4, "BINUNICODE")
    generated("short-unicode", "ē", 4, "SHORT_BINUNICODE", "MEMOIZE", "FRAME", "PROTO")
    generated("binary-list", [5, 7], 2, "EMPTY_LIST", "APPENDS")
    generated("binary-dict", {"a": 1, "b": 2}, 2, "EMPTY_DICT", "SETITEMS")
    generated("set", {1, 5}, 4, "EMPTY_SET", "ADDITEMS")
    generated("frozenset", frozenset({1, 5}), 4, "FROZENSET")
    generated("bytearray", bytearray(b"\x00\x93\xff"), 5, "BYTEARRAY8")
    wire(
        "multiple-frames",
        b"\x80\x04" + frame(b"(K\x07") + frame(b"K\x08t."),
        "Tuple [Int 7L; Int 8L]",
        "FRAME",
        "MARK",
        "TUPLE",
    )
    wire(
        "unframed-operand",
        b"\x80\x04" + frame(b"(") + b"B\x03\0\0\0abc" + frame(b"t."),
        'Tuple [Bytes "abc"]',
        "FRAME",
        "BINBYTES",
    )
    wire("empty-frame", b"\x80\x04" + frame(b"") + b"N.", "Null", "FRAME")

    # Earlier producers emitted these short historical forms. The 64-bit
    # lengths deliberately carry tiny payloads so they test the wire fields.
    wire("legacy-string", b"S'text'\n.", 'Text "text"', "STRING")
    wire("legacy-binstring", b"T\x04\x00\x00\x00text.", 'Text "text"', "BINSTRING")
    wire("legacy-short-binstring", b"U\x04text.", 'Text "text"', "SHORT_BINSTRING")
    wire(
        "wide-binary-unicode",
        b"\x80\x04\x8d\x02\0\0\0\0\0\0\0\xc4\x93.",
        'Text "\\196\\147"',
        "BINUNICODE8",
    )
    wire(
        "wide-binary-bytes",
        b"\x80\x04\x8e\x02\0\0\0\0\0\0\0ab.",
        'Bytes "ab"',
        "BINBYTES8",
    )
    wire(
        "readonly-bytes",
        b"\x80\x05C\x02ab\x98.",
        'Bytes "ab"',
        "SHORT_BINBYTES",
        "READONLY_BUFFER",
    )
    wire(
        "readonly-bytearray",
        b"\x80\x05\x96\x02\0\0\0\0\0\0\0ab\x98.",
        'Readonly_buffer (Bytearray "ab")',
        "BYTEARRAY8",
        "READONLY_BUFFER",
    )
    wire("wide-long", b"\x80\x02\x8b\x02\0\0\0\x00\x01.", "Int 256L", "LONG4")
    wire("pop", b"K\x07K\x090.", "Int 7L", "POP")
    wire("pop-mark", b"K\x07(K\x091.", "Int 7L", "POP_MARK")
    wire("dup", b"\x80\x02K\x072\x86.", "Tuple [Int 7L; Int 7L]", "DUP")
    wire("get", b"(I7\np11\ng11\nt.", "Tuple [Int 7L; Int 7L]", "GET")
    wire("binget", b"\x80\x02K\x07q\x020h\x02.", "Int 7L", "BINPUT", "BINGET")
    wire(
        "long-binget",
        b"\x80\x02K\x07r,\x01\0\0" b"0j,\x01\0\0.",
        "Int 7L",
        "LONG_BINPUT",
        "LONG_BINGET",
    )
    wire(
        "wide-memo-key",
        b"\x80\x02K\x07r\xff\xff\xff\xff" b"0j\xff\xff\xff\xff.",
        "Int 7L",
        "LONG_BINPUT",
        "LONG_BINGET",
    )
    wire("ext-1", b"\x80\x02\x82\x07.", "Extension 7l", "EXT1")
    wire("ext-2", b"\x80\x02\x83,\x01.", "Extension 300l", "EXT2")
    wire("ext-4", b"\x80\x02\x84p\x11\x01\x00.", "Extension 70000l", "EXT4")
    wire(
        "signed-ext-4",
        b"\x80\x02\x84\xff\xff\xff\x7f.",
        "Extension 2147483647l",
        "EXT4",
    )
    wire(
        "opaque-global",
        b"cnonexistent_module\nSymbol\n.",
        'Global ("nonexistent_module", "Symbol")',
        "GLOBAL",
    )
    wire(
        "inst",
        b"(i__main__\nSample\n.",
        'Object (Global ("__main__", "Sample"), Tuple [], None)',
        "INST",
    )
    wire(
        "obj",
        b"\x80\x02(c__main__\nSample\no.",
        'Object (Global ("__main__", "Sample"), Tuple [], None)',
        "OBJ",
    )
    wire("stack-global", pickle.dumps(Sample, 4), 'Global ("__main__", "Sample")', "STACK_GLOBAL")
    wire(
        "object-state",
        pickle.dumps(Sample(), 4),
        'Object (Global ("__main__", "Sample"), Tuple [], Some (Dict [(Text "attr", Int 5L)]))',
        "NEWOBJ",
        "BUILD",
    )
    wire(
        "keyword-arguments",
        pickle.dumps(WithKeywords(), 4),
        'Object (Global ("__main__", "WithKeywords"), '
        'Tuple [Tuple [Int 3L]; Dict [(Text "name", Text "sample")]], None)',
        "NEWOBJ_EX",
    )
    wire(
        "explicit-state-setter",
        pickle.dumps(WithStateSetter(), 5),
        'Reduce (Global ("__main__", "WithStateSetter"), Tuple [])',
        "REDUCE",
        "POP",
    )

    for protocol, opcode in [(0, "PERSID"), (1, "BINPERSID")]:
        stream = io.BytesIO()
        PersistentIdPickler(stream, protocol).dump(Sample())
        wire(
            f"persistent-id-{protocol}",
            stream.getvalue(),
            'Persistent (Text "token")',
            opcode,
        )

    for value, name, required in [
        (bytearray(b"ab"), "writable-buffer", ["NEXT_BUFFER"]),
        (b"ab", "readonly-buffer", ["NEXT_BUFFER", "READONLY_BUFFER"]),
    ]:
        data = pickle.dumps(pickle.PickleBuffer(value), 5, buffer_callback=lambda _: None)
        emit(name, data, "External_buffer 2", required)

    print("]")
    version = ".".join(map(str, sys.version_info[:3]))
    print(f'let generator = "{sys.implementation.name} {version}"')
    print(f"let seed = {SEED}")
    print("let random_cases = [")
    rng = random.Random(SEED)
    for protocol in range(6):
        for index in range(12):
            value = random_value(rng, 3, protocol)
            generated(f"generated-{protocol}-{index}", value, protocol)
    print("]")

    print("let graph_cases = [")
    recursive_list = []
    recursive_list.append(recursive_list)
    for name, data, required in [
        ("shared-list", pickle.dumps([[]] * 2, 4), ["BINGET"]),
        ("recursive-list", pickle.dumps(recursive_list, 4), ["EMPTY_LIST", "BINGET"]),
        ("self-state", pickle.dumps(SelfState(), 4), ["BUILD", "BINGET"]),
    ]:
        check_opcodes(name, data, required)
        print(f"  ({ocaml_string(name)}, {ocaml_string(data)});")
    print("]")

    protocol_opcodes = {op.name for op in pickletools.opcodes if op.proto <= 5}
    if missing := protocol_opcodes - covered:
        raise AssertionError(f"protocol 0–5 opcodes missing from vectors: {sorted(missing)}")


if __name__ == "__main__":
    main()
