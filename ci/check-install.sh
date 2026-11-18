#!/usr/bin/env bash
set -euo pipefail

client=$(mktemp -d)
trap 'rm -rf -- "$client"' EXIT
cd "$client"

printf '(lang dune 3.7)\n' > dune-project
printf '(executable (name client) (libraries opickle))\n' > dune

cat > client.ml <<'ML'
let () =
  match Opickle.of_string "I42\n." with
  | Ok value when Opickle.Value.to_string value = "42" -> ()
  | _ -> failwith "installed opickle returned an unexpected value"
ML

opam exec -- dune exec ./client.exe
printf 'I42\n.' > data.pickle
test "$(opam exec -- pickle_dump data.pickle)" = 42
