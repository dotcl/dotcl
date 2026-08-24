#!/bin/sh
# project-core composition check: a Lisp LIBRARY consumed by another project.
#
# Two consumer shapes, both against the in-tree runtime (no NuGet):
#   1. a plain C# app that references the library  — the library's facade boots
#      the runtime and loads the library's own manifest
#   2. a dotcl app (it has its own .asd, so it loads its own manifest first) that
#      also calls into the library — here the library's fasl must still load, and
#      the core must NOT be loaded twice
#
# Shape 2 used to be broken two ways: the deployed manifest name was fixed
# (dotcl-deps.txt), so the app's overwrote the library's and the library's fasl
# was deployed but never listed; and a second LoadFromManifest re-loaded
# dotcl.core, which redefines CL functions ("package COMMON-LISP is locked").
#
# Usage: check.sh <repo-root>
set -eu
# Absolute: the generated csproj files live outside the tree and reference the
# runtime by path, so a relative ROOT would resolve against their own directory.
ROOT="$(cd "${1%/}" && pwd)"
# dotnet (a Windows exe) needs Windows-style paths in the csproj; an MSYS
# `/c/...` path silently resolves to nothing.
win() { if command -v cygpath >/dev/null 2>&1; then cygpath -m "$1"; else echo "$1"; fi; }
RT="$(win "$ROOT/runtime/DotCL.Runtime.csproj")"
RTRUN="$(win "$ROOT/runtime/runtime.csproj")"
TARGETS="$(win "$ROOT/runtime/build/Dotcl.targets")"
CORE="$(win "$ROOT/compiler/dotcl.core")"

if [ ! -f "$ROOT/compiler/dotcl.core" ]; then
  echo "  SKIP: compiler/dotcl.core not built (make compile-core-fasl)"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fail=0
note() { echo "  FAIL: $1"; fail=1; }

# --- the Lisp library -------------------------------------------------------
mkdir -p "$WORK/Lib"
cat > "$WORK/Lib/Lib.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/Lib.asd</DotclProjectAsd>
    <DotclRuntimeProject>$RTRUN</DotclRuntimeProject>
    <DotclBaseCore>$CORE</DotclBaseCore>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$RT" />
  </ItemGroup>
  <Import Project="$TARGETS" />
</Project>
EOF
cat > "$WORK/Lib/Lib.asd" <<'EOF'
(defsystem "Lib"
  :components ((:file "lib")))
EOF
cat > "$WORK/Lib/lib.lisp" <<'EOF'
(defpackage :lib (:use :cl) (:export #:lib-greet))
(in-package :lib)
(defun lib-greet (name) (format nil "lib greets ~a" name))
EOF
cat > "$WORK/Lib/Facade.cs" <<'EOF'
using System;
using System.IO;
using DotCL;

namespace Lib
{
    public static class Lisp
    {
        private static readonly object Gate = new object();
        private static bool _loaded;

        public static void EnsureLoaded()
        {
            if (_loaded) return;
            lock (Gate)
            {
                if (_loaded) return;
                DotclHost.Initialize();
                // This library's own manifest, not dotcl-deps.txt: a consuming
                // dotcl app owns that name in the shared output directory.
                DotclHost.LoadFromManifest(
                    Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "Lib.deps.txt"));
                _loaded = true;
            }
        }

        public static string Greet(string name)
        {
            EnsureLoaded();
            return DotclHost.ToClr<string>(DotclHost.Call("LIB:LIB-GREET", name));
        }
    }
}
EOF

# --- shape 1: plain C# consumer --------------------------------------------
mkdir -p "$WORK/PlainApp"
cat > "$WORK/PlainApp/PlainApp.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$WORK/Lib/Lib.csproj")" />
  </ItemGroup>
</Project>
EOF
cat > "$WORK/PlainApp/Program.cs" <<'EOF'
using System;
class Program
{
    static void Main() => Console.WriteLine(Lib.Lisp.Greet("plain"));
}
EOF

# --- shape 2: dotcl app (own .asd) that also uses the library ---------------
mkdir -p "$WORK/DotclApp"
cat > "$WORK/DotclApp/DotclApp.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/PcApp.asd</DotclProjectAsd>
    <DotclRuntimeProject>$RTRUN</DotclRuntimeProject>
    <DotclBaseCore>$CORE</DotclBaseCore>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$WORK/Lib/Lib.csproj")" />
  </ItemGroup>
  <Import Project="$TARGETS" />
</Project>
EOF
cat > "$WORK/DotclApp/PcApp.asd" <<'EOF'
(defsystem "PcApp"
  :components ((:file "app")))
EOF
cat > "$WORK/DotclApp/app.lisp" <<'EOF'
(defpackage :app (:use :cl) (:export #:app-main))
(in-package :app)
(defun app-main () "app ran")
EOF
cat > "$WORK/DotclApp/Program.cs" <<'EOF'
using System;
using System.IO;
using DotCL;

class Program
{
    static void Main()
    {
        DotclHost.Initialize();
        DotclHost.LoadFromManifest(
            Path.Combine(AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt"));
        Console.WriteLine(DotclHost.ToClr<string>(DotclHost.Call("APP:APP-MAIN")));
        // The library boots on an already-loaded core: its manifest repeats
        // dotcl.core, which must be skipped rather than loaded again.
        Console.WriteLine(Lib.Lisp.Greet("dotcl app"));
    }
}
EOF

# --- build + run ------------------------------------------------------------
echo "  building library + both consumers..."
dotnet build "$(win "$WORK/PlainApp/PlainApp.csproj")" -c Debug -v quiet --nologo > "$WORK/build1.log" 2>&1 \
  || { cat "$WORK/build1.log"; note "PlainApp build failed"; }
dotnet build "$(win "$WORK/DotclApp/DotclApp.csproj")" -c Debug -v quiet --nologo > "$WORK/build2.log" 2>&1 \
  || { cat "$WORK/build2.log"; note "DotclApp build failed"; }

libbundle="$WORK/Lib/bin/Debug/net10.0/dotcl-fasl"
[ -f "$libbundle/Lib.deps.txt" ] || note "library did not write a per-project manifest (Lib.deps.txt)"

appbundle="$WORK/DotclApp/bin/Debug/net10.0/dotcl-fasl"
[ -f "$appbundle/dotcl-deps.txt" ] || note "app manifest missing from output"
[ -f "$appbundle/Lib.deps.txt" ] || note "library manifest did not reach the app's output"
[ -f "$appbundle/Lib.fasl" ] || note "library fasl did not reach the app's output"

out1="$(dotnet run --project "$(win "$WORK/PlainApp/PlainApp.csproj")" -c Debug --no-build 2>&1)" || true
case "$out1" in
  *"lib greets plain"*) ;;
  *) echo "$out1"; note "plain C# consumer could not call the library" ;;
esac

out2="$(dotnet run --project "$(win "$WORK/DotclApp/DotclApp.csproj")" -c Debug --no-build 2>&1)" || true
case "$out2" in
  *"app ran"*) ;;
  *) echo "$out2"; note "dotcl app did not run its own Lisp" ;;
esac
case "$out2" in
  *"lib greets dotcl app"*) ;;
  *) echo "$out2"; note "dotcl app could not call the referenced Lisp library" ;;
esac

# --- dependency resolution reports what it cannot find ----------------------
#
# The walk over :depends-on used to call ASDF inside IGNORE-ERRORS and skip
# anything that came back false. NIL and an error mean different things there:
# NIL is "(:feature :x ...) with :x absent, this dependency does not apply", an
# error is "this dependency was declared and cannot be found". Treating the
# second as the first wrote a manifest that silently lacked the system — exit 0,
# no output, a build that "succeeded" and an application missing a library.
echo "  checking dependency resolution diagnostics..."
# The built runner: the file is runtime.exe on Windows and runtime elsewhere, and
# which configuration exists depends on what was built last. Probe, and fall back
# to `dotnet run` so this works in a tree that has only sources built.
DOTCL_EXE=""
for cand in "$ROOT/runtime/bin/Release/net10.0/runtime" \
            "$ROOT/runtime/bin/Release/net10.0/runtime.exe" \
            "$ROOT/runtime/bin/Debug/net10.0/runtime" \
            "$ROOT/runtime/bin/Debug/net10.0/runtime.exe"; do
  if [ -x "$cand" ]; then DOTCL_EXE="$cand"; break; fi
done
dotcl_run() {
  if [ -n "$DOTCL_EXE" ]; then
    "$DOTCL_EXE" "$@"
  else
    dotnet run --project "$(win "$ROOT/runtime/runtime.csproj")" -v q --nologo -- "$@"
  fi
}

mkdir -p "$WORK/missingdep"
cat > "$WORK/missingdep/missingdep.asd" <<'EOF'
(defsystem "missingdep" :depends-on ("no-such-system-anywhere")
  :components ((:file "missingdep")))
EOF
echo '(defpackage :missingdep (:use :cl))' > "$WORK/missingdep/missingdep.lisp"

if out="$(dotcl_run build "$(win "$WORK/missingdep/missingdep.asd")" --resolve-deps 2>&1)"; then
  note "resolve-deps succeeded for a system whose dependency does not exist"
else
  case "$out" in
    *no-such-system-anywhere*missingdep*|*missingdep*no-such-system-anywhere*) ;;
    *) echo "$out"; note "resolve-deps failed without naming the system or the dependency" ;;
  esac
fi

# The other direction: an unmet :feature dependency is not a missing one, and
# must not stop the build.
mkdir -p "$WORK/featuredep"
cat > "$WORK/featuredep/featuredep.asd" <<'EOF'
(defsystem "featuredep"
  :depends-on ((:feature :no-such-feature-here "no-such-system-anywhere"))
  :components ((:file "featuredep")))
EOF
echo '(defpackage :featuredep (:use :cl))' > "$WORK/featuredep/featuredep.lisp"

dotcl_run build "$(win "$WORK/featuredep/featuredep.asd")" --resolve-deps > /dev/null 2>&1 \
  || note "resolve-deps failed on a dependency whose :feature is absent (it does not apply, so it is not missing)"

if [ "$fail" -eq 0 ]; then
  echo "  OK: Lisp library composes with a plain C# app and with a dotcl app"
else
  exit 1
fi
