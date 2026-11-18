#!/usr/bin/env python3
"""Generate the standalone CLI fixtures for dump.t."""

from collections import namedtuple
from enum import Enum
from pathlib import Path
import pickle
import struct


class Sample:
    def __init__(self):
        self.level = 3
        self.name = "sample"


class Direction(Enum):
    NORTH = 1


Point = namedtuple("Point", ["name", "coordinate"])


def mixed():
    return {
        "numbers": (None, False, True, 256, -5000, 2**74, 1.5),
        "text": "caf\u00e9",
        "bytes": b"ink",
        "containers": ([4, 5], {7, 11}, frozenset({7}), {}, ()),
        "mutable_bytes": bytearray(b"\x00\xff"),
        "objects": (Sample(), Point("A", 4), Direction.NORTH),
    }


def recursive():
    root = []
    root.append(([root],))
    return root


def legacy(protocol):
    bytearray_global = b"c__builtin__\nbytearray\n"
    if protocol == 0:
        prefix = b"(lS'caf\\xe9'\naVcaf\\u00e9\na"
        bytearray_args = b"(S'B\\xff'\nS'latin-1'\nt"
        return prefix + bytearray_global + bytearray_args + b"Ra."
    args = b"U\x04caf\xe9X\x05\x00\x00\x00caf\xc3\xa9"
    wide_str = b"T" + struct.pack("<i", 4) + b"wide"
    bytearray_args = b"(U\x02B\xffU\x07latin-1t"
    header = b"\x80\x02" if protocol == 2 else b""
    return header + b"](" + args + wide_str + bytearray_global + bytearray_args + b"Re."


for protocol in range(6):
    Path(f"mixed_{protocol}.pickle").write_bytes(pickle.dumps(mixed(), protocol))
    Path(f"recursive_{protocol}.pickle").write_bytes(
        pickle.dumps(recursive(), protocol)
    )

for protocol in range(3):
    Path(f"legacy_{protocol}.pickle").write_bytes(legacy(protocol))

Path("opaque.pickle").write_bytes(b"cfixture_only\nUnknown\n(tR.")
