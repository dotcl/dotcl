#!/bin/sh
# Regression files that keep Lisp state in thread statics, frames, multiple
# values, structures and CLOS, run with a gen0 budget small enough that a GC
# happens every few hundred KB of allocation, in two hosts:
#
#   cli   the dotcl CLI
#   host  a C# host that uses up the runtime's inline TLS area before dotcl
#         starts and runs Lisp on a separate large-stack thread, as the MSBuild
#         task does; dotcl's per-call thread statics then live in a GC heap
#         array that moves
#
# A GC hole shows up as a crash, a wrong result or a broken frame chain.
# Left out: main-thread.lisp (a host has no main-thread loop) and
# cf-fasl-data-structs.lisp (starts the CLI as a child).
#
# Usage: check.sh <repo-root> [gen0size-hex]
set -eu
ROOT="$(cd "${1%/}" && pwd)"
GEN0="${2:-30000}"
win() { if command -v cygpath >/dev/null 2>&1; then cygpath -m "$1"; else echo "$1"; fi; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

{
  echo '(load "test/framework.lisp")'
  grep -oE '^\(load(-skip-under-interp)? "test/regression/[^"]+"' "$ROOT/test/regression/run.lisp" \
    | grep -oE 'test/regression/[^"]+' \
    | grep -E 'clos|struct|values|mvb|mv-|multiple|thread|tls|frame|backtrace|special|generic|gf-|defmethod|unwind|throw|handler' \
    | grep -vE '/(main-thread|cf-fasl-data-structs)\.lisp$' \
    | sed 's/.*/(load "&")/'
  echo '(do-tests-summary)'
  echo '(format t "~&GC-STRESS-FAILS ~a~%" *fail-count*)'
} > "$WORK/driver.lisp"
echo "files: $(grep -c 'test/regression/' "$WORK/driver.lisp")"

dotnet build "$(win "$ROOT/runtime/runtime.csproj")" -c Release -v q --nologo > "$WORK/build.log" 2>&1 \
  || { echo "FAIL: building the runtime"; tail -20 "$WORK/build.log"; exit 1; }
BIN="$ROOT/runtime/bin/Release/net10.0"

mkdir -p "$WORK/host"
cp "$BIN/DotCL.Runtime.dll" "$WORK/host/"
cat > "$WORK/host/Program.cs" <<'CSEOF'
using DotCL;

static class H0 { [ThreadStatic] public static long V; }
static class H1 { [ThreadStatic] public static long V; }
static class H2 { [ThreadStatic] public static long V; }
static class H3 { [ThreadStatic] public static long V; }
static class H4 { [ThreadStatic] public static long V; }
static class H5 { [ThreadStatic] public static long V; }
static class H6 { [ThreadStatic] public static long V; }
static class H7 { [ThreadStatic] public static long V; }
static class P
{
    static void TakeTls() { H0.V = 1; H1.V = 1; H2.V = 1; H3.V = 1; H4.V = 1; H5.V = 1; H6.V = 1; H7.V = 1; }
    static int Main(string[] args)
    {
        TakeTls();
        Exception err = null;
        var boot = new Thread(() =>
        {
            try
            {
                DotclHost.Initialize();
                Runtime.ContribExtraSearchPaths.Add(args[2]);
                DotclHost.LoadCore(args[0]);
            }
            catch (Exception e) { err = e; }
        }, 256 << 20);
        boot.Start(); boot.Join();
        if (err != null) { Console.WriteLine($"BOOT {err}"); return 2; }
        var run = new Thread(() =>
        {
            try { DotclHost.EvalString($"(load \"{args[1]}\")"); }
            catch (Exception e) { err = e; }
        }, 256 << 20);
        run.Start(); run.Join();
        if (err != null) { Console.WriteLine($"RUN {err}"); return 3; }
        return 0;
    }
}
CSEOF
cat > "$WORK/host/gchost.csproj" <<'CSPROJEOF'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="DotCL.Runtime"><HintPath>DotCL.Runtime.dll</HintPath></Reference>
  </ItemGroup>
</Project>
CSPROJEOF
( cd "$WORK/host" && dotnet build gchost.csproj -c Release -o bin -v q --nologo ) > "$WORK/host.log" 2>&1 \
  || { echo "FAIL: building the host"; tail -20 "$WORK/host.log"; exit 1; }

fail=0
check() { # $1 = label, $2 = log, $3 = exit code
  fails="$(sed -n 's/^GC-STRESS-FAILS //p' "$2" | tr -d '\r')"
  if [ "$3" -eq 0 ] && [ "$fails" = "0" ]; then
    echo "PASS ($1): $(grep -E 'PASSED .* TOTAL' "$2" | tail -1)"
  else
    echo "FAIL ($1): exit $3, failures '${fails:-none reported}'"
    grep -E 'FAIL|Exception|fault|BOOT|RUN' "$2" | head -20
    fail=1
  fi
}

cd "$ROOT"
set +e
DOTNET_GCgen0size="$GEN0" timeout 1200 "$BIN/runtime" --core "$ROOT/compiler/dotcl.core" "$WORK/driver.lisp" \
  < /dev/null > "$WORK/cli.log" 2>&1
check cli "$WORK/cli.log" $?
DOTNET_GCgen0size="$GEN0" timeout 1200 dotnet "$WORK/host/bin/gchost.dll" "$ROOT/compiler/dotcl.core" "$(win "$WORK/driver.lisp")" \
  "$(win "$ROOT/contrib")" \
  < /dev/null > "$WORK/host.log" 2>&1
check host "$WORK/host.log" $?
set -e

[ "$fail" -eq 0 ] && echo "ALL-GC-STRESS-CHECKS-PASSED"
exit "$fail"
