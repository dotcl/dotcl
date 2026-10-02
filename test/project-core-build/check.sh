#!/bin/sh
# The MSBuild integration users actually get: DotCL.Runtime.ProjectCore.targets
# driving the IN-PROCESS tasks (DotclResolveDeps / DotclCompileProject) out of
# DotCL.Build.Tasks.dll.
#
# Nothing else here covers that path. test/project-compose builds with the legacy
# Dotcl.targets, which shells out to the runner instead, so a defect confined to
# the task assembly -- its arguments, its error reporting, the package-relative
# paths the targets resolve against it -- shows up in neither. This check assembles
# the package layout the targets expect and builds against it:
#
#   pkg/build/DotCL.Runtime.ProjectCore.targets
#   pkg/tasks/DotCL.Build.Tasks.dll
#   pkg/lib/net10.0/DotCL.Runtime.dll
#   pkg/dotclassets/{dotcl.core,contrib/}
#
# It is the layout, not a real .nupkg: packing takes minutes and would test the
# packaging rather than the targets. The paths are the ones the targets compute
# from MSBuildThisFileDirectory, so a change to that layout breaks this too.
#
# Three cases. The first two go together, and the second is the one worth
# having. A build that fails must say what to do next: a dependency that cannot
# be found is the first wall a new user hits, and the remedy (DotclAsdSearchPath)
# is a build property they have no reason to know about. The third publishes
# trimmed and runs the result: the .NET types the Lisp sources name have to
# survive the trimmer without the project listing them by hand.
#
# Usage: check.sh <repo-root>
set -eu

# A missing prerequisite is a convenience skip when this is run by hand, but in
# CI a skip is indistinguishable from a pass: the gate quietly stops gating and
# nothing in the log says so. DOTCL_CI=1 (set at the job level in
# .github/workflows/ci.yml) makes it a failure instead.
skip_or_fail() {
  echo "$1"
  if [ "${DOTCL_CI:-}" = "1" ]; then
    echo "  DOTCL_CI=1: a skipped check counts as a failure here" >&2
    exit 1
  fi
}
ROOT="$(cd "${1%/}" && pwd)"
win() { if command -v cygpath >/dev/null 2>&1; then cygpath -m "$1"; else echo "$1"; fi; }

if [ ! -f "$ROOT/compiler/dotcl.core" ]; then
  skip_or_fail "  SKIP: compiler/dotcl.core not built (make compile-core-fasl)"
  exit 0
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
fail=0
note() { echo "  FAIL: $1"; fail=1; }

echo "=== building the task assembly and the runtime ==="
dotnet build "$(win "$ROOT/runtime/build-tasks/DotCL.Build.Tasks.csproj")" -c Release -v q --nologo \
  > "$WORK/tasks.log" 2>&1 || { echo "FAIL: building DotCL.Build.Tasks"; tail -20 "$WORK/tasks.log"; exit 1; }
dotnet build "$(win "$ROOT/runtime/DotCL.Runtime.csproj")" -c Release -f net10.0 -v q --nologo \
  > "$WORK/runtime.log" 2>&1 || { echo "FAIL: building DotCL.Runtime"; tail -20 "$WORK/runtime.log"; exit 1; }

PKG="$WORK/pkg"
mkdir -p "$PKG/build" "$PKG/tasks" "$PKG/lib/net10.0" "$PKG/dotclassets"
cp "$ROOT/runtime/build/DotCL.Runtime.ProjectCore.targets" "$PKG/build/"
cp "$ROOT/runtime/build-tasks/bin/Release/net10.0/DotCL.Build.Tasks.dll" "$PKG/tasks/"
cp "$ROOT/runtime/bin/Release/net10.0/DotCL.Runtime.dll" "$PKG/lib/net10.0/"
cp "$ROOT/compiler/dotcl.core" "$PKG/dotclassets/"
[ -d "$ROOT/contrib" ] && cp -r "$ROOT/contrib" "$PKG/dotclassets/contrib"

# The dependency, placed where only an explicit search path can find it.
mkdir -p "$WORK/ext"
cat > "$WORK/ext/pcbext.asd" <<'EOF'
(defsystem "pcbext" :components ((:file "pcbext")))
EOF
cat > "$WORK/ext/pcbext.lisp" <<'EOF'
(defpackage :pcbext (:use :cl) (:export #:greet))
(in-package :pcbext)
(defun greet () "from-pcbext")
EOF

mkdir -p "$WORK/app"
cat > "$WORK/app/app.asd" <<'EOF'
(defsystem "app" :depends-on ("pcbext") :components ((:file "app")))
EOF
cat > "$WORK/app/app.lisp" <<'EOF'
(defpackage :app (:use :cl) (:export #:run))
(in-package :app)
(defun run () (pcbext:greet))
EOF
cat > "$WORK/app/Program.cs" <<'EOF'
class P { static void Main() { System.Console.WriteLine("built"); } }
EOF

emit_csproj() { # $1 = extra ItemGroup content (may be empty)
  cat > "$WORK/app/app.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <AssemblyName>pcbapp</AssemblyName>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/app.asd</DotclProjectAsd>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$ROOT/runtime/DotCL.Runtime.csproj")" />
    $1
  </ItemGroup>
  <Import Project="$(win "$PKG/build/DotCL.Runtime.ProjectCore.targets")" />
</Project>
CSPROJEOF
  rm -rf "$WORK/app/obj" "$WORK/app/bin"
}

echo "=== [1] a dependency that cannot be found ==="
emit_csproj ""
if dotnet build "$(win "$WORK/app/app.csproj")" -c Debug --nologo > "$WORK/build1.log" 2>&1; then
  note "the build succeeded although the declared dependency does not exist"
else
  # Naming the system and the dependency is the diagnosis; naming the property is
  # the part that gets the reader unstuck, and it is the part that regressed
  # historically (the failure used to surface as a reader error about a package).
  grep -q 'pcbext' "$WORK/build1.log" \
    || note "the error does not name the dependency"
  grep -q 'DotclAsdSearchPath' "$WORK/build1.log" \
    || note "the error does not say how to make the system findable"
  grep -q 'CL_SOURCE_REGISTRY' "$WORK/build1.log" \
    || note "the error does not mention that CL_SOURCE_REGISTRY is not read during build"
  grep -q 'Package "PCBEXT" not found' "$WORK/build1.log" \
    && note "the failure still surfaces as a missing package rather than a missing system"
fi
[ "$fail" -eq 0 ] && echo "  PASS: the build says what is missing and what to do about it"

echo "=== [2] the remedy the message names ==="
emit_csproj "<DotclAsdSearchPath Include=\"$(win "$WORK/ext")\" />"
if dotnet build "$(win "$WORK/app/app.csproj")" -c Debug --nologo > "$WORK/build2.log" 2>&1; then
  echo "  PASS: adding DotclAsdSearchPath builds the project"
else
  note "the project does not build even with DotclAsdSearchPath set"
  tail -20 "$WORK/build2.log"
fi

echo "=== [3] a trimmed publish keeps the .NET types the Lisp sources name ==="
# The fasl reaches PcbTrim.Greeter and System.Globalization.ISOWeek only through
# reflection, so the trimmer sees no use of either. Nothing in C# touches
# Greeter, so without the descriptor the build writes it is removed outright.
mkdir -p "$WORK/trim"
cat > "$WORK/trim/trimapp.asd" <<'EOF'
(defsystem "trimapp" :components ((:file "trimapp")))
EOF
cat > "$WORK/trim/trimapp.lisp" <<'EOF'
(defpackage :trimapp (:use :cl) (:export #:run))
(in-package :trimapp)
(defun run ()
  (format nil "~a / ~a"
          (dotnet:invoke (dotnet:new "PcbTrim.Greeter") "Hello" "lisp")
          (dotnet:static "System.Globalization.ISOWeek" "GetYear"
                         (dotnet:new "System.DateTime" 2026 1 1))))
EOF
cat > "$WORK/trim/Program.cs" <<'EOF'
namespace PcbTrim
{
    public class Greeter { public string Hello(string n) => "hello " + n; }
    static class P
    {
        static void Main()
        {
            DotCL.DotclHost.Initialize();
            DotCL.DotclHost.LoadFromManifest(System.IO.Path.Combine(
                System.AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt"));
            System.Console.WriteLine("result: " + DotCL.DotclHost.Call("TRIMAPP:RUN"));
        }
    }
}
EOF
# PublishTrimmed goes in the project, not on the command line: a global property
# would reach the netstandard2.0 build of the runtime, which cannot be trimmed.
cat > "$WORK/trim/trimapp.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <AssemblyName>pcbtrim</AssemblyName>
    <PublishTrimmed>true</PublishTrimmed>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/trimapp.asd</DotclProjectAsd>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$ROOT/runtime/DotCL.Runtime.csproj")" />
  </ItemGroup>
  <Import Project="$(win "$PKG/build/DotCL.Runtime.ProjectCore.targets")" />
</Project>
CSPROJEOF
RID="$(dotnet --info | awk '/^ *RID:/{print $2; exit}')"
if dotnet publish "$(win "$WORK/trim/trimapp.csproj")" -c Release -r "$RID" --self-contained \
     -o "$(win "$WORK/trim/pub")" --nologo > "$WORK/publish3.log" 2>&1; then
  exe="$WORK/trim/pub/pcbtrim"
  [ -f "$exe.exe" ] && exe="$exe.exe"
  out="$("$exe" < /dev/null 2>&1 || true)"
  case "$out" in
    *'result: "hello lisp / 2026"'*) echo "  PASS: the trimmed app calls both types" ;;
    *) note "the trimmed app lost a type the Lisp sources name"; echo "$out" | head -5 ;;
  esac
else
  note "the trimmed publish failed"
  tail -20 "$WORK/publish3.log"
fi

if [ "$fail" -eq 0 ]; then
  echo "ALL-PROJECT-CORE-BUILD-CHECKS-PASSED"
else
  exit 1
fi
