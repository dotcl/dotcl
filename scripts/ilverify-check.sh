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

# The Release runtime is the reference every fasl is verified against, so it has
# to match the source the fasls were compiled from. Building only when absent
# left a stale reference behind: a newly added runtime method showed up as
# "Missing method" in a fasl that was perfectly fine. The build is incremental.
RUNTIME_DLL="$ROOT/runtime/bin/Release/net10.0/DotCL.Runtime.dll"
echo "=== Building Release runtime (the ilverify reference) ==="
dotnet build "$ROOT/runtime/DotCL.Runtime.csproj" -c Release -f net10.0 -v q

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

# A real library compiled by dotcl, when one is available. The fixtures above are
# written for this check and so only cover what someone thought to write down;
# asdf is 15k lines of ordinary Lisp nobody wrote for us. Both defects this gate
# was blind to (a covariant Symbol call, an i4 default radix) showed up here
# first. Skipped rather than failed when asdf has not been set up.
ASDF_FASL="$ROOT/contrib/asdf/asdf.fasl"
ASSEMBLIES=(core stress)
if [ ! -f "$ASDF_FASL" ]; then
  echo "note: contrib/asdf/asdf.fasl absent — skipping the real-library check"
  echo "      (make setup-asdf && make compile-asdf-fasl)"
elif [ "$ASDF_FASL" -ot "$ROOT/compiler/cil-out.sil" ]; then
  # The fasl carries the code generation of the compiler that built it, so an
  # old one reports on old codegen: it can fail for a defect already fixed, or
  # pass over one just introduced. Either way it is not evidence about the tree
  # under test. Skip rather than judge. (CI always builds it fresh.)
  echo "note: contrib/asdf/asdf.fasl is older than compiler/cil-out.sil —"
  echo "      skipping the real-library check (it would verify stale codegen)."
  echo "      Refresh it with: rm contrib/asdf/asdf.fasl && make compile-asdf-fasl"
else
  cp "$ASDF_FASL" "$OUT/asdf.dll"
  ASSEMBLIES+=(asdf)
fi

# Build the -r reference list (every framework dll + the runtime).
REFS=()
for f in "$FWDIR"/*.dll; do REFS+=(-r "$f"); done
REFS+=(-r "$RUNTIME_DLL")

# ILVerify miscounts the stack at a filter that is nested in another clause's
# handler — a shape ordinary Lisp produces, and one Roslyn hits too. The filter
# drops exactly those and nothing else; see scripts/ilverify-filter.
FILTER="dotnet run --project $ROOT/scripts/ilverify-filter/ilverify-filter.csproj -v q --"
dotnet build "$ROOT/scripts/ilverify-filter/ilverify-filter.csproj" -v q >/dev/null

rc=0
for asm in "${ASSEMBLIES[@]}"; do
  echo "=== ilverify $asm.dll ==="
  set +e
  "$ILVERIFY" "$OUT/$asm.dll" -s System.Private.CoreLib "${REFS[@]}" \
    | $FILTER "$OUT/$asm.dll"
  status=${PIPESTATUS[1]}
  set -e
  if [ "$status" -eq 0 ]; then
    echo "  $asm.dll: VERIFIED"
  else
    echo "  $asm.dll: VERIFICATION FAILED (unverifiable IL — IL2CPP/WebGL would reject this)" >&2
    rc=1
  fi
done

exit $rc
