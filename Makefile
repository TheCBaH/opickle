BUILD_OPTIONS ?=

build:
	opam exec -- dune build $(BUILD_OPTIONS)

runtest test:
	opam exec -- dune runtest $(BUILD_OPTIONS)

# Update inline-expect and cram snapshots in place.
promote:
	opam exec -- dune runtest --auto-promote $(BUILD_OPTIONS)

format:
	opam exec -- dune fmt

utop:
	opam exec -- dune utop lib

clean:
	opam exec -- dune clean

.PHONY: build runtest test promote format utop clean
