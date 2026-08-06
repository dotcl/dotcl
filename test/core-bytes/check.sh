#!/bin/sh
# Booting from a core held in memory — DotclHost.LoadCore(byte[]).
#
# A host with no filesystem (a browser fetches the core over HTTP) has no path to
# hand the loader, and an emit-free runtime has no other way in: assembling SIL
# needs Reflection.Emit, so the core is the only entry. This check builds a tiny
# host that reads the core itself and passes the bytes, and asserts that a REPL's
# worth of Lisp then works.
#
# Run against both runtimes:
#   1. the normal build            — the overload must not have broken the usual path
#   2. -p:DotclNoEmit=true         — the configuration a browser actually gets
# Case 2 is the one that matters; case 1 is there so a desktop regression shows up
# here rather than only in whatever ships.
#
# Usage: check.sh <repo-root>
set -eu
# Absolute: the generated project lives outside the tree and references the
# runtime by path, so a relative ROOT would resolve against its own directory.
ROOT="$(cd "${1%/}" && pwd)"
# dotnet (a Windows exe) needs Windows-style paths in the csproj; an MSYS
# `/c/...` path silently resolves to nothing.
win() { if command -v cygpath >/dev/null 2>&1; then cygpath -m "$1"; else echo "$1"; fi; }

if [ ! -f "$ROOT/compiler/dotcl.core" ]; then
  echo "  SKIP: compiler/dotcl.core not built (make compile-core-fasl)"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/Program.cs" <<'CSEOF'
// Read the core ourselves and hand over the bytes: after this line no path is
// involved, which is the situation a browser host is in.
using DotCL;

byte[] image = File.ReadAllBytes(args[0]);
Startup.Initialize();
DotclHost.LoadCore(image);
Console.WriteLine($"BYTES {image.Length}");

foreach (var form in new[] {
    "(+ 1 2)",
    "(progn (defun cb-sq (x) (* x x)) (cb-sq 7))",
    "(loop for i from 1 to 4 collect (* i i))",
    "(progn (defclass cb-c () ((s :initform 5 :accessor cb-s))) (cb-s (make-instance 'cb-c)))",
    "(handler-case (error \"boom\") (error () :caught))",
})
    Console.WriteLine($"EVAL {DotclHost.EvalString(form)}");
CSEOF

cat > "$WORK/corebytes.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <AssemblyName>corebytes</AssemblyName>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="DotCL.Runtime">
      <HintPath>DotCL.Runtime.dll</HintPath>
    </Reference>
  </ItemGroup>
</Project>
CSPROJEOF

expect_ok() { # $1 = label, $2 = program output
  printf '%s\n' "$2" | grep -q '^BYTES [0-9][0-9]*$' \
    || { echo "FAIL ($1): core was not read"; printf '%s\n' "$2"; exit 1; }
  for want in 'EVAL 3' 'EVAL 49' 'EVAL (1 4 9 16)' 'EVAL 5' 'EVAL :CAUGHT'; do
    printf '%s\n' "$2" | grep -qF "$want" \
      || { echo "FAIL ($1): missing '$want'"; printf '%s\n' "$2"; exit 1; }
  done
  echo "PASS ($1): booted from memory; defun / loop / defclass / handler-case all evaluate"
}

run_with() { # $1 = label, $2... = extra build args for the runtime library
  label="$1"; shift
  rm -rf "$WORK/lib" "$WORK/bin"
  dotnet build "$(win "$ROOT/runtime/DotCL.Runtime.csproj")" -c Release -f net10.0 \
      "$@" -o "$(win "$WORK/lib")" > "$WORK/build.log" 2>&1 \
    || { echo "FAIL ($label): building the runtime"; tail -20 "$WORK/build.log"; exit 1; }
  cp "$WORK/lib/DotCL.Runtime.dll" "$WORK/DotCL.Runtime.dll"
  ( cd "$WORK" && dotnet build corebytes.csproj -c Release -o bin ) > "$WORK/host.log" 2>&1 \
    || { echo "FAIL ($label): building the host"; tail -20 "$WORK/host.log"; exit 1; }
  out="$(dotnet "$WORK/bin/corebytes.dll" "$(win "$ROOT/compiler/dotcl.core")" 2>&1)" \
    || { echo "FAIL ($label): the host itself failed"; printf '%s\n' "$out"; exit 1; }
  expect_ok "$label" "$out"
}

echo "=== normal runtime ==="
run_with "emit"
echo "=== emit-free runtime (what a browser gets) ==="
run_with "emit-free" -p:DotclNoEmit=true
# The emit-free library must really be emit-free, or the case above proved nothing.
grep -aq FaslAssembler "$WORK/lib/DotCL.Runtime.dll" \
  && { echo "FAIL: the emit-free build still contains the emitter"; exit 1; }
echo "PASS: the emit-free build contains no emitter"

echo "ALL-CORE-BYTES-CHECKS-PASSED"
