#!/usr/bin/env python3
"""Pickle a named test value at a given protocol, for python_vectors.t.

Run as: genpickle.py <protocol> <case> <outfile>

Each case mirrors a hand-assembled opcode fragment in unpickler_test.ml, so
the cram test can check that the hand-rolled bytes decode to the same Value
that a real CPython pickler emits for the same logical value.
"""
import pickle
import sys


class Sample:
    """Pickled via copy_reg._reconstructor (proto<2) or NEWOBJ (proto>=2)."""

    def __init__(self):
        self.attr = 5


class ReduceSample:
    """__reduce__ is surfaced structurally as <reduce ...>, never executed."""

    def __reduce__(self):
        return (ReduceSample, ())


class PersistentId:
    def __init__(self, pid):
        self.pid = pid


class PersistentPickler(pickle.Pickler):
    def persistent_id(self, obj):
        return obj.pid if isinstance(obj, PersistentId) else None


def recursive_list():
    rec = []
    rec.append(rec)
    return rec


CASES = {
    "none": lambda: None,
    "true": lambda: True,
    "false": lambda: False,
    "int_small": lambda: 42,
    "int_mid": lambda: 256,
    "int_neg": lambda: -1,
    "bigint_neg": lambda: -128,
    "bigint_huge": lambda: 123456789012345678901234,
    "float": lambda: -1.25,
    "str_escape": lambda: "a\nb",
    "bytes_short": lambda: b"\x00U\xaa\xff",
    "str_nonascii": lambda: "Āabc",
    "list_simple": lambda: [1, 2, 3],
    "tuple_pair": lambda: (1, 2),
    "dict_simple": lambda: {"a": 1},
    "set_simple": lambda: {1, 2},
    "recursive_list": recursive_list,
    "class_instance": Sample,
    "reduce_instance": ReduceSample,
    "class_object": lambda: Sample,
}


def main():
    if sys.argv[1] == "repr":
        # Ground truth for the "direct" cases in python_vectors.t: values
        # that pickle without ever going through a reduce/global/object
        # surrogate, so opickle's rendering must match Python's own repr()
        # verbatim, at every protocol.
        print(repr(CASES[sys.argv[2]]()))
        return
    proto, case, outfile = sys.argv[1], sys.argv[2], sys.argv[3]
    with open(outfile, "wb") as f:
        if case == "persistent_id":
            PersistentPickler(f, protocol=int(proto)).dump(PersistentId(3))
        else:
            pickle.dump(CASES[case](), f, protocol=int(proto))


if __name__ == "__main__":
    main()
