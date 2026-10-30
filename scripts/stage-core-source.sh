#!/bin/sh
# Stage compiler/cil-stdlib.lisp and the generation of compiler/dotcl.core next
# to the packed core (runtime/), so a library build in an installed dotcl can
# compile the core functions its body uses. Usage: stage-core-source.sh <repo-root>
set -eu
ROOT="${1%/}"
rm -f "$ROOT/runtime/cil-stdlib.lisp" "$ROOT/runtime/cil-stdlib.generation"
gen=$(dotnet run --project "$ROOT/runtime/runtime.csproj" -- --core "$ROOT/compiler/dotcl.core" \
  --eval '(princ (dotcl::%core-generation))' < /dev/null)
case "$gen" in
  ?*-?*) ;;
  *) echo "stage-core-source: no core generation: '$gen'" >&2; exit 1 ;;
esac
printf '%s' "$gen" > "$ROOT/runtime/cil-stdlib.generation"
cp "$ROOT/compiler/cil-stdlib.lisp" "$ROOT/runtime/cil-stdlib.lisp"
