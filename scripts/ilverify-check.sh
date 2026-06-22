#!/usr/bin/env bash
# ilverify-check.sh — assert dotcl emits VERIFIABLE CIL.
#
# dotcl's emitter must produce verifiable IL: no covariant calls (a base
# LispObject passed where a derived type such as Symbol is required, with no
# castclass), no stack-type mismatches. CoreCLR's JIT tolerates unverifiable IL,
# but strict AOT C++ codegens (Unity IL2CPP / WebGL) reject it — and a 25-minute
# IL2CPP build is a terrible feedback loop. This runs ilverify on two compiled
# fasls (a freshly-generated core = all of cil-stdlib + the compiler, plus the
# stress fixture exercising emit-heavy user-code paths) and fails on any error.
#
# Note: we verify the System.Private.CoreLib-targeted fasls. The netstandard
# retarget (compile-file :corlib-name) only rewrites the AssemblyRef row, not the
# method bodies, so IL verifiability is identical either way.
#
# Requires: dotnet-ilverify (install: dotnet tool install --global dotnet-ilverify).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT="$ROOT/build/ilverify"
mkdir -p "$OUT"

ILVERIFY="$(command -v ilverify || echo "$HOME/.dotnet/tools/ilverify")"
if [ ! -x "$ILVERIFY" ] && ! command -v ilverify >/dev/null 2>&1; then
  echo "ERROR: ilverify not found. Install with:" >&2
  echo "  dotnet tool install --global dotnet-ilverify" >&2
  exit 127
fi

RUNTIME_DLL="$ROOT/runtime/bin/Release/net10.0/DotCL.Runtime.dll"
if [ ! -f "$RUNTIME_DLL" ]; then
  echo "=== Building Release runtime (needed as an ilverify reference) ==="
  dotnet build "$ROOT/runtime/DotCL.Runtime.csproj" -c Release -f net10.0 -v q
fi

# Locate the net10 shared framework (holds System.Private.CoreLib etc.).
# `dotnet --list-runtimes` prints: "Microsoft.NETCore.App <ver> [<path>]"
# where <path> may contain spaces (e.g. C:\Program Files\...). Take the last
# net10 line, pull the version (2nd field) and the bracketed path separately,
# convert backslashes to forward slashes, and join as <path>/<ver>.
FWLINE="$(dotnet --list-runtimes | grep 'Microsoft.NETCore.App 10\.' | tail -1)"
FWVER="$(echo "$FWLINE" | awk '{print $2}')"
FWPATH="$(echo "$FWLINE" | sed -e 's/^[^[]*\[//' -e 's/\]$//' -e 's#\\#/#g')"
FWDIR="$FWPATH/$FWVER"
if [ ! -d "$FWDIR" ]; then
  echo "ERROR: could not locate the .NET 10 shared framework dir (got: $FWDIR)" >&2
  exit 1
fi

echo "=== Generating a fresh core + compiling the IL-verify stress fixture ==="
dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
  --asm "$ROOT/compiler/cil-out.sil" \
  --eval "(progn
            (dotcl:sil-to-fasl \"compiler/cil-out.sil\" \"build/ilverify/core.dll\" \"ilvcore\")
            (compile-file \"test/ilverify/stress.lisp\" :output-file \"build/ilverify/stress.dll\" :module-name \"ilvstress\")
            (dotcl:quit 0))"

# Build the -r reference list (every framework dll + the runtime).
REFS=()
for f in "$FWDIR"/*.dll; do REFS+=(-r "$f"); done
REFS+=(-r "$RUNTIME_DLL")

rc=0
for asm in core stress; do
  echo "=== ilverify $asm.dll ==="
  if "$ILVERIFY" "$OUT/$asm.dll" -s System.Private.CoreLib "${REFS[@]}"; then
    echo "  $asm.dll: VERIFIED"
  else
    echo "  $asm.dll: VERIFICATION FAILED (unverifiable IL — IL2CPP/WebGL would reject this)" >&2
    rc=1
  fi
done

exit $rc
