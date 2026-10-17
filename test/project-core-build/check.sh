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
# Six cases. The first two go together, and the second is the one worth
# having. A build that fails must say what to do next: a dependency that cannot
# be found is the first wall a new user hits, and the remedy (DotclAsdSearchPath)
# is a build property they have no reason to know about. [2b] rebuilds that
# project unchanged and then edited: only the edit recompiles the root. The third publishes
# trimmed and runs the result: the .NET types the Lisp sources name have to
# survive the trimmer without the project listing them by hand. The fourth
# depends on a contrib stub (dotcl-thread) and runs the result. The fifth
# publishes that with PublishReadyToRun: the fasls must ship as native code.
# The sixth publishes through DotCL.Runtime.targets, which copies the whole
# contrib tree: only the modules the manifest requires may be published.
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
cp "$ROOT/runtime/build/DotCL.Runtime.ProjectCore.targets" "$ROOT/runtime/build/DotCL.Runtime.targets" \
   "$ROOT/runtime/build/DotCL.Runtime.props" "$PKG/build/"
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

  echo "=== [2b] a second build with nothing changed does not recompile the root ==="
  # The root compile is incremental on its Inputs. A directory among them (the
  # search path) counts as a missing input file to MSBuild, which then runs the
  # target on every build. A source edit must still recompile it.
  # Where the bundle dir lands depends on how the targets are imported (this
  # layout imports them before IntermediateOutputPath is set), so look for it.
  root_fasl="$(find "$WORK/app" -name app.fasl ! -path '*/bin/*' | head -1)"
  stamp="$WORK/stamp2"
  sleep 1; touch "$stamp"
  if dotnet build "$(win "$WORK/app/app.csproj")" -c Debug --nologo > "$WORK/build2b.log" 2>&1; then
    if [ -z "$root_fasl" ] || [ ! -f "$root_fasl" ]; then
      note "no root fasl after the first build"
    elif [ -n "$(find "$root_fasl" -newer "$stamp")" ]; then
      note "the root was recompiled although nothing changed"
    else
      echo "  PASS: the unchanged root is not recompiled"
    fi
    sleep 1; touch "$stamp"; sleep 1
    echo '(defun run2 () (run))' >> "$WORK/app/app.lisp"
    if dotnet build "$(win "$WORK/app/app.csproj")" -c Debug --nologo > "$WORK/build2c.log" 2>&1 \
       && [ -n "$(find "$root_fasl" -newer "$stamp")" ]; then
      echo "  PASS: editing a root source recompiles the root"
    else
      note "editing a root source did not recompile the root"
    fi
  else
    note "the second build failed"
    tail -20 "$WORK/build2b.log"
  fi
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

echo "=== [4] a dependency on a contrib stub is loaded at build time and at start ==="
# dotcl-thread's .asd is a REQUIRE-SYSTEM stub with no components. The dependency
# reads its package, so the build has to REQUIRE it before compiling that, and
# the manifest has to say "require dotcl-thread" ahead of the dependency's fasl,
# or the application fails to start with Package "DOTCL-THREAD" not found.
mkdir -p "$WORK/ctr/ext" "$WORK/ctr/app"
cat > "$WORK/ctr/ext/ctrext.asd" <<'EOF'
(defsystem "ctrext" :depends-on ("dotcl-thread") :components ((:file "ctrext")))
EOF
cat > "$WORK/ctr/ext/ctrext.lisp" <<'EOF'
(defpackage :ctrext (:use :cl) (:export #:probe))
(in-package :ctrext)
(defun probe () (if (fboundp 'dotcl-thread:make-thread) "threads" "none"))
EOF
cat > "$WORK/ctr/app/ctrapp.asd" <<'EOF'
(defsystem "ctrapp" :depends-on ("ctrext") :components ((:file "ctrapp")))
EOF
cat > "$WORK/ctr/app/ctrapp.lisp" <<'EOF'
(defpackage :ctrapp (:use :cl) (:export #:run))
(in-package :ctrapp)
(defun run () (ctrext:probe))
EOF
cat > "$WORK/ctr/app/Program.cs" <<'EOF'
static class P
{
    static void Main()
    {
        DotCL.DotclHost.Initialize();
        DotCL.DotclHost.LoadFromManifest(System.IO.Path.Combine(
            System.AppContext.BaseDirectory, "dotcl-fasl", "dotcl-deps.txt"));
        System.Console.WriteLine("result: " + DotCL.DotclHost.Call("CTRAPP:RUN"));
    }
}
EOF
# The contrib tree beside the application is what DotCL.Runtime.targets copies
# for a package consumer; this layout imports only the project-core targets.
cat > "$WORK/ctr/app/ctrapp.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <AssemblyName>pcbctr</AssemblyName>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/ctrapp.asd</DotclProjectAsd>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$ROOT/runtime/DotCL.Runtime.csproj")" />
    <DotclAsdSearchPath Include="$(win "$WORK/ctr/ext")" />
    <None Include="$(win "$PKG/dotclassets/contrib")/**/*"
          Link="contrib/%(RecursiveDir)%(Filename)%(Extension)"
          CopyToOutputDirectory="PreserveNewest" />
  </ItemGroup>
  <Import Project="$(win "$PKG/build/DotCL.Runtime.ProjectCore.targets")" />
</Project>
CSPROJEOF
if dotnet build "$(win "$WORK/ctr/app/ctrapp.csproj")" -c Debug --nologo > "$WORK/build4.log" 2>&1; then
  exe="$WORK/ctr/app/bin/Debug/net10.0/pcbctr"
  [ -f "$exe.exe" ] && exe="$exe.exe"
  out="$("$exe" < /dev/null 2>&1 || true)"
  case "$out" in
    *'result: "threads"'*) echo "  PASS: the contrib is required at build time and at start" ;;
    *) note "the application did not start with the contrib loaded"; echo "$out" | head -5 ;;
  esac
else
  note "the project with a contrib dependency did not build"
  tail -20 "$WORK/build4.log"
fi

echo "=== [5] a ReadyToRun publish compiles the published fasls in place ==="
# Same sources as [4]. The application reports how many of its fasls carry
# native code (a ManagedNativeHeader) and dotcl:r2r-stats. A file that is not
# an assembly stands in for a fasl crossgen2 cannot compile: the publish must
# warn and ship it unchanged. DotclReadyToRunFasls=false keeps every fasl IL.
mkdir -p "$WORK/r2r"
cp "$WORK/ctr/app/ctrapp.asd" "$WORK/ctr/app/ctrapp.lisp" "$WORK/r2r/"
echo 'not an assembly' > "$WORK/r2r/broken.fasl"
cat > "$WORK/r2r/Program.cs" <<'EOF'
using System;
using System.IO;
using System.Linq;
using System.Reflection.PortableExecutable;
static class P
{
    static bool IsR2R(string f)
    {
        using var pe = new PEReader(File.OpenRead(f));
        return pe.PEHeaders.CorHeader.ManagedNativeHeaderDirectory.Size > 0;
    }
    static void Main()
    {
        var root = AppContext.BaseDirectory;
        DotCL.DotclHost.Initialize();
        DotCL.DotclHost.LoadFromManifest(Path.Combine(root, "dotcl-fasl", "dotcl-deps.txt"));
        Console.WriteLine("result: " + DotCL.DotclHost.Call("CTRAPP:RUN"));
        Console.WriteLine("r2r-stats: " + DotCL.DotclHost.EvalString("(dotcl:r2r-stats)"));
        var fasls = Directory.GetFiles(Path.Combine(root, "dotcl-fasl"))
            .Where(f => f.EndsWith(".fasl") || Path.GetFileName(f) == "dotcl.core")
            .Concat(Directory.GetFiles(Path.Combine(root, "contrib"), "*.fasl", SearchOption.AllDirectories))
            .ToList();
        Console.WriteLine($"native: {fasls.Count(IsR2R)}/{fasls.Count}");
    }
}
EOF
cat > "$WORK/r2r/r2rapp.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <AssemblyName>pcbr2r</AssemblyName>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/ctrapp.asd</DotclProjectAsd>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$ROOT/runtime/DotCL.Runtime.csproj")" />
    <DotclAsdSearchPath Include="$(win "$WORK/ctr/ext")" />
    <None Include="$(win "$PKG/dotclassets/contrib")/**/*.fasl"
          Link="contrib/%(RecursiveDir)%(Filename)%(Extension)"
          CopyToOutputDirectory="PreserveNewest" />
    <None Include="broken.fasl" Link="extra/broken.fasl" CopyToOutputDirectory="PreserveNewest" />
  </ItemGroup>
  <Import Project="$(win "$PKG/build/DotCL.Runtime.ProjectCore.targets")" />
</Project>
CSPROJEOF
r2r_publish() { # $1 = output dir, $2 = log, $3.. = extra properties
  out_dir="$1"; log="$2"; shift 2
  dotnet publish "$(win "$WORK/r2r/r2rapp.csproj")" -c Release -r "$RID" -p:PublishReadyToRun=true "$@" \
    -o "$(win "$out_dir")" --nologo > "$log" 2>&1
}
r2r_run_exe() {
  exe="$1"
  [ -f "$exe.exe" ] && exe="$exe.exe"
  "$exe" < /dev/null 2>&1 || true
}
r2r_run() { r2r_run_exe "$1/pcbr2r"; }
if r2r_publish "$WORK/r2r/pub" "$WORK/publish5.log"; then
  out="$(r2r_run "$WORK/r2r/pub")"
  native="$(echo "$out" | sed -n 's/^native: //p' | tr -d '\r')"
  case "$out" in
    *'result: "threads"'*) ;;
    *) note "the ReadyToRun publish does not start"; echo "$out" | head -5 ;;
  esac
  case "$native" in
    0/*|'') note "no published fasl is native code ($native)" ;;
    *) [ "${native%/*}" = "${native#*/}" ] && echo "  PASS: every published fasl is native code ($native)" \
         || note "some published fasls are still IL ($native)" ;;
  esac
  case "$out" in
    *'r2r-stats: (0 '*) note "no fasl was loaded as native code: $(echo "$out" | grep r2r-stats)" ;;
  esac
  if grep -q 'crossgen2 failed on extra' "$WORK/publish5.log" \
     && cmp -s "$WORK/r2r/broken.fasl" "$WORK/r2r/pub/extra/broken.fasl"; then
    echo "  PASS: a fasl crossgen2 cannot compile warns and ships unchanged"
  else
    note "a fasl crossgen2 cannot compile did not warn, or was not shipped unchanged"
  fi
else
  note "the ReadyToRun publish failed"
  tail -20 "$WORK/publish5.log"
fi
if r2r_publish "$WORK/r2r/pub-il" "$WORK/publish5b.log" -p:DotclReadyToRunFasls=false; then
  case "$(r2r_run "$WORK/r2r/pub-il")" in
    *'native: 0/'*) echo "  PASS: DotclReadyToRunFasls=false publishes the fasls as IL" ;;
    *) note "DotclReadyToRunFasls=false still compiled fasls" ;;
  esac
else
  note "the publish with DotclReadyToRunFasls=false failed"
  tail -20 "$WORK/publish5b.log"
fi

echo "=== [6] a publish ships only the contrib modules the manifest requires ==="
# A contrib module that REQUIREs another at load time must declare it in its
# .asd: the manifest lists what the walk finds there, and nothing else is
# published.
for lisp in "$ROOT"/contrib/*/*.lisp; do
  mod="$(basename "$lisp" .lisp)"
  asd="$(dirname "$lisp")/$mod.asd"
  [ -f "$asd" ] || continue
  for dep in $(sed -n 's/^(require "\([^"]*\)").*/\1/p' "$lisp"); do
    grep -q "\"$dep\"" "$asd" || note "contrib/$mod requires $dep at load time but $mod.asd does not depend on it"
  done
done
mkdir -p "$WORK/sel"
cat > "$WORK/sel/selapp.asd" <<'EOF'
(defsystem "selapp" :depends-on ("dotcl-socket") :components ((:file "selapp")))
EOF
cat > "$WORK/sel/selapp.lisp" <<'EOF'
(defpackage :selapp (:use :cl) (:export #:run))
(in-package :selapp)
(defun run ()
  (let ((server (dotcl-socket:make-server-socket "127.0.0.1" 0)))
    (unwind-protect (if (plusp (dotcl-socket:local-port server)) "listening" "no port")
      (dotcl-socket:socket-close server))))
EOF
sed 's/CTRAPP:RUN/SELAPP:RUN/' "$WORK/r2r/Program.cs" > "$WORK/sel/Program.cs"
cat > "$WORK/sel/selapp.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <AssemblyName>pcbsel</AssemblyName>
    <DotclProjectAsd>\$(MSBuildProjectDirectory)/selapp.asd</DotclProjectAsd>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$(win "$ROOT/runtime/DotCL.Runtime.csproj")" />
  </ItemGroup>
  <Import Project="$(win "$PKG/build/DotCL.Runtime.props")" />
  <Import Project="$(win "$PKG/build/DotCL.Runtime.targets")" />
</Project>
CSPROJEOF
contrib_dirs() { (cd "$1/contrib" 2>/dev/null && ls | tr '\n' ' '); }
if dotnet publish "$(win "$WORK/sel/selapp.csproj")" -c Release -r "$RID" -p:PublishReadyToRun=true \
     -o "$(win "$WORK/sel/pub")" --nologo > "$WORK/publish6.log" 2>&1; then
  dirs="$(contrib_dirs "$WORK/sel/pub")"
  if [ "$dirs" = "dotcl-bsd-sockets dotcl-socket " ]; then
    echo "  PASS: only the required contrib modules and what they require are published"
  else
    note "the published contrib modules are '$dirs', not dotcl-bsd-sockets and dotcl-socket"
  fi
  out="$(r2r_run_exe "$WORK/sel/pub/pcbsel")"
  native="$(echo "$out" | sed -n 's/^native: //p' | tr -d '\r')"
  case "$out" in
    *'result: "listening"'*) echo "  PASS: the narrowed publish starts and loads its contrib modules" ;;
    *) note "the narrowed publish does not start"; echo "$out" | head -5 ;;
  esac
  case "$native" in
    0/*|'') note "no fasl of the narrowed publish is native code ($native)" ;;
    *) [ "${native%/*}" = "${native#*/}" ] && echo "  PASS: the narrowed set is ReadyToRun ($native)" \
         || note "some fasls of the narrowed publish are still IL ($native)" ;;
  esac
else
  note "the narrowed publish failed"
  tail -20 "$WORK/publish6.log"
fi
if dotnet publish "$(win "$WORK/sel/selapp.csproj")" -c Release -r "$RID" -p:DotclPublishAllContribs=true \
     -o "$(win "$WORK/sel/pub-all")" --nologo > "$WORK/publish6b.log" 2>&1; then
  dirs="$(contrib_dirs "$WORK/sel/pub-all")"
  if [ "$dirs" = "$(contrib_dirs "$PKG/dotclassets")" ]; then
    echo "  PASS: DotclPublishAllContribs=true publishes every contrib module"
  else
    note "DotclPublishAllContribs=true published '$dirs', not every contrib module"
  fi
else
  note "the publish with DotclPublishAllContribs=true failed"
  tail -20 "$WORK/publish6b.log"
fi

if [ "$fail" -eq 0 ]; then
  echo "ALL-PROJECT-CORE-BUILD-CHECKS-PASSED"
else
  exit 1
fi
