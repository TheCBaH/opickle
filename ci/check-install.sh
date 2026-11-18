#!/usr/bin/env bash
set -euo pipefail

client=$(mktemp -d)
trap 'rm -rf -- "$client"' EXIT

printf '(lang dune 3.7)\n' > "$client/dune-project"
printf '(executable (name client) (libraries opickle))\n' > "$client/dune"

cat > "$client/client.ml" <<'ML'
let () =
  match Opickle.of_string "I42\n." with
  | Ok value when Opickle.Value.to_string value = "42" -> ()
  | _ -> failwith "installed opickle returned an unexpected value"
ML

opam exec -- dune exec --root "$client" ./client.exe
printf 'I42\n.' > "$client/data.pickle"
test "$(opam exec -- pickle_dump "$client/data.pickle")" = 42
