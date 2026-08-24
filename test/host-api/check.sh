#!/bin/sh
# The embedding API's entry points, exercised the way a C# host actually meets
# them: a fresh process, no Lisp-side setup, names written the way they appear
# in the Lisp source.
#
# Each case here was a defect found by embedding dotcl in a .NET 10 file-based
# app (`dotnet run app.cs`), where all three showed up in the first ten lines a
# newcomer writes:
#
#   1. EnsureCore() with no prior Initialize() died with a NullReferenceException
#      from inside Startup.Sym, naming nothing.
#   2. Call("greet") could not find (defun greet ...) -- the reader upcased the
#      symbol, so only Call("GREET") worked.
#   3. On a host with dynamic code turned off (a NativeAOT publish, and the
#      file-based-app default), the first eval threw a raw
#      PlatformNotSupportedException out of AssemblyBuilder.
#
# Usage: check.sh <repo-root>
set -eu
ROOT="$(cd "${1%/}" && pwd)"
win() { if command -v cygpath >/dev/null 2>&1; then cygpath -m "$1"; else echo "$1"; fi; }

if [ ! -f "$ROOT/compiler/dotcl.core" ]; then
  echo "  SKIP: compiler/dotcl.core not built (make compile-core-fasl)"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/Program.cs" <<'CSEOF2'
using DotCL;

// EnsureCore is the documented first call. Initialize() is deliberately NOT
// called here: it must not be a hidden precondition.
try { DotclHost.EnsureCore(); Console.WriteLine("CORE ok"); }
catch (Exception e) { Console.WriteLine($"CORE {e.GetType().Name}: {e.Message}"); }

Console.WriteLine($"DYNCODE {System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported}");

try
{
    DotclHost.EvalString("(defun greet (who) (format nil \"hello ~a\" who))");
    // A name is a symbol name, matched exactly. The reader upcased GREET.
    Console.WriteLine($"CALL-EXACT {DotclHost.ToClr<string>(DotclHost.Call("GREET", "world"))}");
    Console.WriteLine($"CALL-QUALIFIED {DotclHost.ToClr<string>(DotclHost.Call("COMMON-LISP-USER::GREET", "world"))}");

    // The source spelling does not resolve -- but the error says what to write.
    try { DotclHost.Call("greet", "world"); Console.WriteLine("CASE-MISS none"); }
    catch (InvalidOperationException e) { Console.WriteLine($"CASE-MISS {e.Message}"); }

    // Exact matching is what makes both of these reachable at all.
    DotclHost.EvalString("(defun |lower| () :lowercase)");
    DotclHost.EvalString("(defun lower () :upcased)");
    Console.WriteLine($"LOWER {DotclHost.Call("lower")} UPPER {DotclHost.Call("LOWER")}");

    // An unqualified name means the current package, nothing else.
    DotclHost.EvalString("(defpackage :mylib (:use :cl) (:export #:entry))");
    DotclHost.EvalString("(in-package :mylib) (defun entry () :from-mylib)");
    DotclHost.EvalString("(in-package :cl-user)");
    Console.WriteLine($"PACKAGE {DotclHost.CurrentPackage}");
    try { DotclHost.Call("ENTRY"); Console.WriteLine("ELSEWHERE none"); }
    catch (InvalidOperationException e) { Console.WriteLine($"ELSEWHERE {e.Message}"); }
    Console.WriteLine($"QUALIFIED {DotclHost.Call("MYLIB:ENTRY")}");
    DotclHost.CurrentPackage = "MYLIB";
    Console.WriteLine($"AFTER-SET {DotclHost.CurrentPackage} {DotclHost.Call("ENTRY")}");
    DotclHost.CurrentPackage = "COMMON-LISP-USER";
}
catch (LispErrorException e) { Console.WriteLine($"EVAL-REFUSED {e.Message}"); }
CSEOF2

emit_csproj() { # $1 = extra property block
  cat > "$WORK/hostapi.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <AssemblyName>hostapi</AssemblyName>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="DotCL.Runtime">
      <HintPath>DotCL.Runtime.dll</HintPath>
    </Reference>
  </ItemGroup>
</Project>
CSPROJEOF
}

build_and_run() { # $1 = label
  label="$1"
  ( cd "$WORK" && dotnet build hostapi.csproj -c Release -o bin ) > "$WORK/host.log" 2>&1 \
    || { echo "FAIL ($label): building the host"; tail -20 "$WORK/host.log"; exit 1; }
  cp "$ROOT/compiler/dotcl.core" "$WORK/bin/dotcl.core"
  dotnet "$WORK/bin/hostapi.dll" 2>&1
}

want() { # $1 = label, $2 = output, $3 = expected line
  printf '%s\n' "$2" | grep -qF "$3" \
    || { echo "FAIL ($1): missing '$3'"; printf '%s\n' "$2"; exit 1; }
}

rm -rf "$WORK/lib"
dotnet build "$(win "$ROOT/runtime/DotCL.Runtime.csproj")" -c Release -f net10.0 \
    -o "$(win "$WORK/lib")" > "$WORK/build.log" 2>&1 \
  || { echo "FAIL: building the runtime"; tail -20 "$WORK/build.log"; exit 1; }
cp "$WORK/lib/DotCL.Runtime.dll" "$WORK/DotCL.Runtime.dll"

echo "=== ordinary host ==="
emit_csproj
out="$(build_and_run "host")"
want "host" "$out" "CORE ok"
want "host" "$out" "DYNCODE True"
want "host" "$out" "CALL-EXACT hello world"
want "host" "$out" "CALL-QUALIFIED hello world"
# The miss is the interesting half: it must name the spelling that works.
want "host" "$out" "CASE-MISS"
want "host" "$out" '"GREET" does exist'
want "host" "$out" "LOWER :LOWERCASE UPPER :UPCASED"
want "host" "$out" "PACKAGE COMMON-LISP-USER"
want "host" "$out" "ELSEWHERE"
want "host" "$out" "defined in MYLIB"
want "host" "$out" "QUALIFIED :FROM-MYLIB"
want "host" "$out" "AFTER-SET MYLIB :FROM-MYLIB"
echo "PASS (host): names match exactly, unqualified means the current package, and a miss says what to write"
echo "=== host with dynamic code disabled (NativeAOT / file-based app) ==="
cat > "$WORK/runtimeconfig.template.json" <<'RCEOF'
{
  "configProperties": {
    "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported": false
  }
}
RCEOF
emit_csproj
out="$(build_and_run "no-dyncode")"
want "no-dyncode" "$out" "DYNCODE False"
want "no-dyncode" "$out" "EVAL-REFUSED"
want "no-dyncode" "$out" "requires runtime code generation"
printf '%s\n' "$out" | grep -q "PlatformNotSupportedException" \
  && { echo "FAIL (no-dyncode): the raw .NET exception still reaches the host"; printf '%s\n' "$out"; exit 1; }
echo "PASS (no-dyncode): refused with a Lisp condition that names the cause, not a raw platform exception"

echo "ALL-HOST-API-CHECKS-PASSED"
