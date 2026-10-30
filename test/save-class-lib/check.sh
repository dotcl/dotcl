#!/bin/sh
# save-class-library (stage 1) check: dotcl emits a .NET DLL whose public type a
# SEPARATE C# app references at compile time with clean native signatures.
#
# Round-trip:
#   1. emit:    a C# harness references DotCL.Runtime and calls
#               DynamicClassBuilder.DefineMinimalClass(saveToPath: ...) to write
#               a facade DLL (public class + native-signature methods).
#   2. consume: a second C# app references that DLL and does
#               `new MyLib.Calculator().Add(2,3)`. If it COMPILES, the emitted
#               type is genuinely C#-referenceable (the stage 1 deliverable). We
#               assert compile, not run: the facade body still dispatches to a
#               Lisp lambda, so running needs the Lisp loaded (stage 2 removes that).
#   3. corlib:  assert the DLL references the netstandard facade, not
#               System.Private.CoreLib (the retarget that makes it referenceable).
#
# PersistedAssemblyBuilder needs .NET 9+; skips cleanly otherwise (but not under
# DOTCL_CI=1 -- see skip_or_fail below).
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
ROOT="${1%/}"
RT="$ROOT/runtime/DotCL.Runtime.csproj"
# dotnet (a Windows exe) needs a Windows-style path in the csproj; an MSYS
# `/c/...` ProjectReference silently resolves to nothing (CS0246 on DotCL).
if command -v cygpath >/dev/null 2>&1; then RT="$(cygpath -m "$RT")"; fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
DLL="$WORK/DotclLib.dll"

fail=0
note() { echo "  FAIL: $1"; fail=1; }

# --- emit harness -----------------------------------------------------------
mkdir -p "$WORK/emit"
cat > "$WORK/emit/emit.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
cat > "$WORK/emit/Program.cs" <<EOF
using System;
using System.Collections.Generic;
using DotCL;
using DotCL.Emitter;
class Emit {
    static void Main(string[] argv) {
        DotclHost.Initialize();
        var body = Nil.Instance; // stage 1 facade: method body dispatches here
        var methods = new List<DynamicClassBuilder.MethodSpec> {
            new DynamicClassBuilder.MethodSpec("Add", typeof(int),
                new Type[] { typeof(int), typeof(int) }, body),
            new DynamicClassBuilder.MethodSpec("Greet", typeof(string),
                new Type[] { typeof(string) }, body),
        };
        DynamicClassBuilder.DefineMinimalClass(
            "MyLib.Calculator", baseType: null, fields: null, attributes: null,
            methods: methods, saveToPath: argv[0]);
        Console.WriteLine("EMIT-OK");
    }
}
EOF

echo "=== emit facade DLL ==="
if ! (cd "$WORK/emit" && dotnet run -- "$DLL" 2>"$WORK/emit.log" | grep -q EMIT-OK); then
  if grep -q "PlatformNotSupported\|requires .NET 9" "$WORK/emit.log" 2>/dev/null; then
    skip_or_fail "SKIP: PersistedAssemblyBuilder unavailable (needs .NET 9+)"; exit 0
  fi
  echo "  emit failed:"; tail -5 "$WORK/emit.log"; exit 1
fi
[ -f "$DLL" ] || { note "no DLL produced"; echo "save-class-lib: FAIL"; exit 1; }

# --- corlib retarget check --------------------------------------------------
# A referenceable DLL must NOT depend on System.Private.CoreLib. (grep -a, not
# `strings`, which MSYS lacks; the assembly-ref names are ASCII in the metadata.)
if grep -aq "System.Private.CoreLib" "$DLL"; then
  note "DLL still references System.Private.CoreLib (retarget missing)"
fi
grep -aq "netstandard" "$DLL" || note "DLL does not reference the netstandard facade"

# --- consume harness (the stage 1 deliverable: C# compiles against it) ----------
mkdir -p "$WORK/consume"
cat > "$WORK/consume/consume.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="DotclLib"><HintPath>../DotclLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
cat > "$WORK/consume/Program.cs" <<EOF
using System;
using MyLib;
class Consume {
    static void Main() {
        var calc = new Calculator();
        int s = calc.Add(2, 3);
        string g = calc.Greet("world");
        Console.WriteLine(s + " " + g);
    }
}
EOF

echo "=== consume: C# references the emitted type ==="
if (cd "$WORK/consume" && dotnet build 2>"$WORK/consume.log" | grep -q "Build succeeded"); then
  echo "  consumer compiled against the dotcl-emitted DLL"
else
  note "consumer failed to compile against the emitted DLL"; tail -8 "$WORK/consume.log"
fi

# --- library (aggregation): MANY types -> ONE DLL ----------------------------
# The stage 1 prototype was "1 define-class -> 1 assembly"; a real library needs several
# types in one DLL. BeginLibrary/AddClass/Save is that aggregation unit. Here
# two classes go into a single MyPack.dll and a SEPARATE C# app references BOTH
# from that one DLL. The emit harness also asserts the two Types share one
# Assembly (the aggregation is real, not two assemblies with the same name).
LIB="$WORK/MyPack.dll"
mkdir -p "$WORK/emit2"
cat > "$WORK/emit2/emit2.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
cat > "$WORK/emit2/Program.cs" <<EOF
using System;
using System.Collections.Generic;
using DotCL;
using DotCL.Emitter;
class Emit2 {
    static void Main(string[] argv) {
        DotclHost.Initialize();
        var body = Nil.Instance; // facade: bodies dispatch to Lisp
        var lib = DynamicClassBuilder.BeginLibrary(argv[0], "MyPack", new Version(1, 2, 3, 0));
        var t1 = lib.AddClass("MyPack.Calculator", methods: new List<DynamicClassBuilder.MethodSpec> {
            new DynamicClassBuilder.MethodSpec("Add", typeof(int),
                new Type[] { typeof(int), typeof(int) }, body),
        });
        var t2 = lib.AddClass("MyPack.Greeter", methods: new List<DynamicClassBuilder.MethodSpec> {
            new DynamicClassBuilder.MethodSpec("Hello", typeof(string),
                new Type[] { typeof(string) }, body),
        });
        // A function library: defun -> public static. IsStatic:true.
        var t3 = lib.AddClass("MyPack.MathOps", methods: new List<DynamicClassBuilder.MethodSpec> {
            new DynamicClassBuilder.MethodSpec("Square", typeof(int),
                new Type[] { typeof(int) }, body, IsStatic: true),
        });
        if (!ReferenceEquals(t1.Assembly, t2.Assembly) || !ReferenceEquals(t1.Assembly, t3.Assembly)) {
            Console.WriteLine("DIFF-ASM"); return;
        }
        lib.Save();
        Console.WriteLine("EMIT2-OK");
    }
}
EOF

echo "=== emit library DLL (3 types incl. static fn, 1 assembly) ==="
if ! (cd "$WORK/emit2" && dotnet run -- "$LIB" 2>"$WORK/emit2.log" | grep -q EMIT2-OK); then
  note "library emit failed (3 types -> 1 DLL)"; tail -8 "$WORK/emit2.log"
fi
[ -f "$LIB" ] || note "no library DLL produced"
if grep -aq "System.Private.CoreLib" "$LIB" 2>/dev/null; then
  note "library DLL still references System.Private.CoreLib (retarget missing)"
fi

mkdir -p "$WORK/consume2"
cat > "$WORK/consume2/consume2.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="MyPack"><HintPath>../MyPack.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
cat > "$WORK/consume2/Program.cs" <<EOF
using System;
using MyPack;
class Consume2 {
    static void Main() {
        int s = new Calculator().Add(2, 3);   // type 1 from the library
        string g = new Greeter().Hello("world"); // type 2, SAME DLL
        int q = MathOps.Square(5);             // type 3: public STATIC function
        Console.WriteLine(s + " " + g + " " + q);
    }
}
EOF

echo "=== consume: C# references all 3 types (incl. static fn) from the one library DLL ==="
if (cd "$WORK/consume2" && dotnet build 2>"$WORK/consume2.log" | grep -q "Build succeeded"); then
  echo "  consumer compiled against all 3 types in the aggregated DLL"
else
  note "consumer failed to compile against the aggregated library DLL"; tail -8 "$WORK/consume2.log"
fi

# --- Lisp-driven emit: ACTUAL dotcl code writes a facade DLL ----------------
# The C# harnesses above call DynamicClassBuilder directly. This proves the
# Lisp surface: dotnet:%define-class with a save-to-path (arg 12) emitted from
# real dotcl code produces a C#-referenceable DLL. Needs the built runtime +
# a cross-compiled cil-out.sil.
SIL="$ROOT/compiler/cil-out.sil"
if [ ! -f "$SIL" ]; then
  skip_or_fail "SKIP-LISP: no cil-out.sil (run 'make cross-compile' first)"
else
  LLIB="$WORK/MyLisp.dll"
  LLIB_W="$LLIB"; LISPF="$WORK/emit.lisp"; LISPF_W="$LISPF"
  if command -v cygpath >/dev/null 2>&1; then
    LLIB_W="$(cygpath -m "$LLIB")"; LISPF_W="$(cygpath -m "$LISPF")"
  fi
  cat > "$LISPF" <<EOF
;; dotnet:%define-class arg 12 = save-to-path -> emit a saved facade DLL.
;; args: full-name base fields attrs methods ctor-body props ifaces events
;;       ctor-param-types base-ctor-indices ctor-specs save-to-path
(dotnet:%define-class
  "MyLisp.Widget"
  nil nil nil
  (list (list "Bump" "System.Int32" (list "System.Int32")
              (lambda (self x) (declare (ignore self)) (+ x 1))))
  nil nil nil nil nil nil nil
  "$LLIB_W")
(format t "LISP-EMIT-OK~%")
EOF
  echo "=== emit facade DLL from ACTUAL dotcl (Lisp) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$LISPF_W" 2>"$WORK/lispemit.log" | grep -q LISP-EMIT-OK); then
    note "Lisp-driven emit failed"; tail -8 "$WORK/lispemit.log"
  fi
  [ -f "$LLIB" ] || note "no Lisp-emitted DLL produced"
  if [ -f "$LLIB" ] && grep -aq "System.Private.CoreLib" "$LLIB"; then
    note "Lisp-emitted DLL still references System.Private.CoreLib"
  fi

  mkdir -p "$WORK/consume3"
  cat > "$WORK/consume3/consume3.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="MyLisp"><HintPath>../MyLisp.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume3/Program.cs" <<EOF
using System;
using MyLisp;
class Consume3 {
    static void Main() {
        int r = new Widget().Bump(41); // type emitted by dotcl Lisp code
        Console.WriteLine(r);
    }
}
EOF
  echo "=== consume: C# references the dotcl-Lisp-emitted type ==="
  if (cd "$WORK/consume3" && dotnet build 2>"$WORK/consume3.log" | grep -q "Build succeeded"); then
    echo "  consumer compiled against the Lisp-emitted DLL"
  else
    note "consumer failed to compile against the Lisp-emitted DLL"; tail -8 "$WORK/consume3.log"
  fi

  # --- Lisp-driven LIBRARY: (dotnet:library ...) aggregates types+static fn ---
  # The dotnet:library macro (contrib/dotnet-class) expands to a single
  # dotnet:%save-library call: MANY types (two instance classes + a static
  # function module) into ONE .dll, from real dotcl code. A separate C# app
  # references all three from that one library: the multi-type/static analogue
  # of the single-type test above, exercised through the Lisp surface.
  CLIB="$WORK/MyLibPack.dll"
  CLIB_W="$CLIB"; LIBF="$WORK/lib.lisp"; LIBF_W="$LIBF"
  CONTRIB="$ROOT/contrib/dotnet-class/dotnet-class.lisp"; CONTRIB_W="$CONTRIB"
  if command -v cygpath >/dev/null 2>&1; then
    CLIB_W="$(cygpath -m "$CLIB")"; LIBF_W="$(cygpath -m "$LIBF")"
    CONTRIB_W="$(cygpath -m "$CONTRIB")"
  fi
  cat > "$LIBF" <<EOF
(load "$CONTRIB_W")
(dotnet:library ("MyLibPack" :version "2.1.0.0" :path "$CLIB_W")
  (:class "MyLibPack.Calculator" ()
    (:methods ("Add" ((a Int32) (b Int32)) :returns Int32 (+ a b))))
  (:class "MyLibPack.Greeter" ()
    (:methods ("Hello" ((who String)) :returns String
      (concatenate 'string "Hi " who))))
  (:module "MyLibPack.MathOps"
    (:functions ("Square" ((x Int32)) :returns Int32 (* x x)))))
(format t "LISP-LIB-OK~%")
EOF
  echo "=== emit LIBRARY DLL from ACTUAL dotcl (dotnet:library macro) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$LIBF_W" 2>"$WORK/lislib.log" | grep -q LISP-LIB-OK); then
    note "Lisp-driven library emit failed"; tail -8 "$WORK/lislib.log"
  fi
  [ -f "$CLIB" ] || note "no Lisp-emitted library DLL produced"
  if [ -f "$CLIB" ] && grep -aq "System.Private.CoreLib" "$CLIB"; then
    note "Lisp-emitted library DLL still references System.Private.CoreLib"
  fi

  mkdir -p "$WORK/consume4"
  cat > "$WORK/consume4/consume4.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="MyLibPack"><HintPath>../MyLibPack.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume4/Program.cs" <<EOF
using System;
using MyLibPack;
class Consume4 {
    static void Main() {
        int s = new Calculator().Add(2, 3);       // instance class 1
        string g = new Greeter().Hello("world");  // instance class 2, same DLL
        int q = MathOps.Square(5);                 // static function module
        Console.WriteLine(s + " " + g + " " + q);
    }
}
EOF
  echo "=== consume: C# references all 3 types from the dotcl-Lisp library DLL ==="
  if (cd "$WORK/consume4" && dotnet build 2>"$WORK/consume4.log" | grep -q "Build succeeded"); then
    echo "  consumer compiled against the aggregated dotcl-Lisp library"
  else
    note "consumer failed to compile against the dotcl-Lisp library DLL"; tail -8 "$WORK/consume4.log"
  fi

  # --- Lisp-driven ENUM + CONST: STANDALONE types (no runtime/Lisp needed) ----
  # Enums and const holders are pure metadata, unlike the method facades above.
  # The consumer here references AND RUNS them with NO DotCL.Runtime reference at
  # all: real, executable .NET metadata rather than facades that dispatch back
  # into Lisp. Also asserts enum auto-increment (Blue == 2) and const values.
  EPACK="$WORK/EnumPack.dll"
  EPACK_W="$EPACK"; ENUMF="$WORK/enum.lisp"; ENUMF_W="$ENUMF"
  if command -v cygpath >/dev/null 2>&1; then
    EPACK_W="$(cygpath -m "$EPACK")"; ENUMF_W="$(cygpath -m "$ENUMF")"
  fi
  cat > "$ENUMF" <<EOF
(load "$CONTRIB_W")
(dotnet:library ("EnumPack" :version "1.0.0.0" :path "$EPACK_W")
  (:enum "EnumPack.Color" :doc "Primary colors." "Red" "Green" "Blue")
  (:enum "EnumPack.Flags" :underlying Int32 ("A" 1) ("B" 2) ("C" 4) "D")
  ;; const holder: also standalone metadata (literals inline into the consumer)
  (:constants "EnumPack.Config"
    ("MaxRetries" Int32 7)
    ("ApiUrl" String "https://example.com"))
  ;; struct: a standalone value type (public fields, no dispatch)
  (:struct "EnumPack.Point" ("X" Int32) ("Y" Int32)))
(format t "ENUM-EMIT-OK~%")
EOF
  echo "=== emit ENUM+CONST+STRUCT library from dotcl (standalone metadata) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$ENUMF_W" 2>"$WORK/enum.log" | grep -q ENUM-EMIT-OK); then
    note "Lisp-driven enum emit failed"; tail -8 "$WORK/enum.log"
  fi
  [ -f "$EPACK" ] || note "no enum DLL produced"
  if [ -f "$EPACK" ] && grep -aq "System.Private.CoreLib" "$EPACK"; then
    note "enum DLL still references System.Private.CoreLib"
  fi
  # :doc on EnumPack.Color writes a sidecar EnumPack.xml the C# compiler auto-loads
  # for IntelliSense. Assert it lands with the member id + summary.
  EXML="${EPACK%.dll}.xml"
  if [ -f "$EXML" ]; then
    grep -q "T:EnumPack.Color" "$EXML" && grep -q "Primary colors." "$EXML" \
      || note "EnumPack.xml missing expected T:EnumPack.Color summary"
  else
    note "no sidecar EnumPack.xml produced for :doc"
  fi

  mkdir -p "$WORK/consume5"
  # NOTE: deliberately NO ProjectReference to DotCL.Runtime: the enum must be
  # standalone. If this compiles+runs, the enum carries no runtime dependency.
  cat > "$WORK/consume5/consume5.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="EnumPack"><HintPath>../EnumPack.dll</HintPath></Reference>
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume5/Program.cs" <<EOF
using System;
using EnumPack;
class Consume5 {
    static void Main() {
        // Blue == 2 (auto-increment); Flags.C == 4, Flags.D == 5 (auto after 4);
        // const MaxRetries == 7, ApiUrl round-trips; struct Point{3,4} => 25.
        var p = new Point { X = 3, Y = 4 };
        Console.WriteLine((int)Color.Blue + " " + (int)Flags.C + " " + (int)Flags.D
                          + " " + Config.MaxRetries + " " + Config.ApiUrl
                          + " " + (p.X * p.X + p.Y * p.Y));
    }
}
EOF
  echo "=== consume: C# references+RUNS enum+const+struct with NO runtime ==="
  if (cd "$WORK/consume5" && dotnet build 2>"$WORK/consume5.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume5" && dotnet run 2>/dev/null | tr -d '\r')"
    if [ "$OUT" = "2 4 5 7 https://example.com 25" ]; then
      echo "  standalone enum+const+struct ran: $OUT"
    else
      note "consumer ran but printed '$OUT' (expected '2 4 5 7 https://example.com 25')"
    fi
  else
    note "standalone enum/const/struct consumer failed to compile"; tail -8 "$WORK/consume5.log"
  fi

  # --- Lisp-driven EXCEPTION TYPE: STANDALONE (throw/catch, no runtime) --------
  # A class deriving System.Exception whose ctor only forwards its message to
  # base emits no Lisp dispatch, so the type is standalone. The consumer here
  # THROWS and CATCHES it with NO DotCL.Runtime reference: a CL condition mapped
  # to a real .NET exception a C# app uses natively.
  XPACK="$WORK/ExcPack.dll"
  XPACK_W="$XPACK"; XF="$WORK/exc.lisp"; XF_W="$XF"
  if command -v cygpath >/dev/null 2>&1; then
    XPACK_W="$(cygpath -m "$XPACK")"; XF_W="$(cygpath -m "$XF")"
  fi
  cat > "$XF" <<EOF
(load "$CONTRIB_W")
(dotnet:library ("ExcPack" :version "1.0.0.0" :path "$XPACK_W")
  (:class "ExcPack.MyError" ("System.Exception")
    (:ctor ((msg String)) (:base msg))))
(format t "EXC-EMIT-OK~%")
EOF
  echo "=== emit EXCEPTION-TYPE library from dotcl (standalone) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$XF_W" 2>"$WORK/exc.log" | grep -q EXC-EMIT-OK); then
    note "Lisp-driven exception-type emit failed"; tail -8 "$WORK/exc.log"
  fi
  [ -f "$XPACK" ] || note "no exception-type DLL produced"

  mkdir -p "$WORK/consume6"
  # NO ProjectReference to DotCL.Runtime: the exception must be standalone.
  cat > "$WORK/consume6/consume6.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="ExcPack"><HintPath>../ExcPack.dll</HintPath></Reference>
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume6/Program.cs" <<EOF
using System;
using ExcPack;
class Consume6 {
    static void Main() {
        try { throw new MyError("boom"); }
        catch (MyError e) {
            Console.WriteLine("caught " + e.Message + " " + (e is Exception));
        }
    }
}
EOF
  echo "=== consume: C# THROWS+CATCHES the dotcl exception with NO runtime ==="
  if (cd "$WORK/consume6" && dotnet build 2>"$WORK/consume6.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume6" && dotnet run 2>/dev/null | tr -d '\r')"
    if [ "$OUT" = "caught boom True" ]; then
      echo "  standalone exception thrown+caught: $OUT"
    else
      note "exception consumer ran but printed '$OUT' (expected 'caught boom True')"
    fi
  else
    note "standalone exception consumer failed to compile"; tail -8 "$WORK/consume6.log"
  fi

  # --- Lisp-driven INTERFACE: STANDALONE contract a C# class implements --------
  # An interface is pure signature metadata. Here dotcl defines IShape and a C#
  # class IMPLEMENTS it (class Circle : IShape) and is used through the interface;
  # all with NO DotCL.Runtime reference. Proves the emitted interface is a real,
  # implementable .NET contract.
  IPACK="$WORK/IfacePack.dll"
  IPACK_W="$IPACK"; IFF="$WORK/iface.lisp"; IFF_W="$IFF"
  if command -v cygpath >/dev/null 2>&1; then
    IPACK_W="$(cygpath -m "$IPACK")"; IFF_W="$(cygpath -m "$IFF")"
  fi
  cat > "$IFF" <<EOF
(load "$CONTRIB_W")
(dotnet:library ("IfacePack" :version "1.0.0.0" :path "$IPACK_W")
  (:interface "IfacePack.IShape"
    ("Area" () :returns Double)
    ("Scale" ((factor Double)) :returns Void))
  ;; delegate: a standalone callback type (runtime-provided machinery)
  (:delegate "IfacePack.BinaryOp" ((a Int32) (b Int32)) :returns Int32))
(format t "IFACE-EMIT-OK~%")
EOF
  echo "=== emit INTERFACE+DELEGATE library from dotcl (standalone contracts) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$IFF_W" 2>"$WORK/iface.log" | grep -q IFACE-EMIT-OK); then
    note "Lisp-driven interface emit failed"; tail -8 "$WORK/iface.log"
  fi
  [ -f "$IPACK" ] || note "no interface DLL produced"

  mkdir -p "$WORK/consume7"
  # NO ProjectReference to DotCL.Runtime: a C# class implements the interface.
  cat > "$WORK/consume7/consume7.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="IfacePack"><HintPath>../IfacePack.dll</HintPath></Reference>
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume7/Program.cs" <<EOF
using System;
using IfacePack;
class Square : IShape {
    public double Side;
    public double Area() { return Side * Side; }
    public void Scale(double f) { Side *= f; }
}
class Consume7 {
    static void Main() {
        IShape s = new Square { Side = 2 };
        s.Scale(3);              // Side => 6
        BinaryOp op = (a, b) => a + b;   // dotcl-defined delegate type
        Console.WriteLine(s.Area() + " " + op(40, 2)); // 36 42
    }
}
EOF
  echo "=== consume: C# IMPLEMENTS the dotcl interface + uses the delegate, NO runtime ==="
  if (cd "$WORK/consume7" && dotnet build 2>"$WORK/consume7.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume7" && dotnet run 2>/dev/null | tr -d '\r')"
    if [ "$OUT" = "36 42" ]; then
      echo "  C# implemented the interface + invoked the delegate: $OUT"
    else
      note "interface/delegate consumer ran but printed '$OUT' (expected '36 42')"
    fi
  else
    note "interface/delegate consumer failed to compile"; tail -8 "$WORK/consume7.log"
  fi

  # --- Lisp-driven BAKED FUNCTIONS: bodies compiled into the DLL (stage 2) ---
  # A :functions entry named by a symbol is compiled into the library itself,
  # with a module initializer that boots the runtime. The consumer references
  # DotCL.Runtime and the DLL, nothing else: no .lisp, no core. It RUNS the
  # functions, including a recursive one, a call to a library-internal helper,
  # and a Lisp error surfacing as a .NET exception.
  BPACK="$WORK/FibLib.dll"
  BPACK_W="$BPACK"; BF="$WORK/baked.lisp"; BF_W="$BF"
  if command -v cygpath >/dev/null 2>&1; then
    BPACK_W="$(cygpath -m "$BPACK")"; BF_W="$(cygpath -m "$BF")"
  fi
  cat > "$BF" <<EOF
(load "$CONTRIB_W")
(defpackage :fib-lib (:use :cl))
(in-package :fib-lib)
(dotnet:library ("FibLib" :version "1.0.0.0" :path "$BPACK_W")
  (:defun square (x) (* x x))
  (:module "FibLib.MathOps"
    (:functions
      (fib ((n Int64)) :returns Int64
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
      (sum-squares ((a Int64) (b Int64)) :returns Int64 (+ (square a) (square b)))
      (greet ((who String)) :returns String (concatenate 'string "Hi " who))
      (half ((x Double)) :returns Double (/ x 2))
      (even-p ((n Int64)) :returns Boolean (evenp n))
      (checked ((n Int64)) :returns Int64
        (if (minusp n) (error "negative: ~a" n) n)))))
(format t "BAKE-EMIT-OK~%")
EOF
  echo "=== emit BAKED-FUNCTION library from dotcl (bodies compiled in) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$BF_W" 2>"$WORK/baked.log" | grep -q BAKE-EMIT-OK); then
    note "Lisp-driven baked-function emit failed"; tail -8 "$WORK/baked.log"
  fi
  [ -f "$BPACK" ] || note "no baked-function DLL produced"

  mkdir -p "$WORK/consume8"
  # DotCL.Runtime only: no core, no .lisp is copied or loaded.
  cat > "$WORK/consume8/consume8.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="FibLib"><HintPath>../FibLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume8/Program.cs" <<EOF
using System;
using FibLib;
class Consume8 {
    static void Main() {
        string err;
        try { MathOps.Checked(-1); err = "none"; }
        catch (Exception e) { err = e.GetType().Name + ":" + e.Message; }
        Console.WriteLine(MathOps.Fib(30) + " " + MathOps.SumSquares(3, 4) + " "
            + MathOps.Greet("C#") + " " + MathOps.Half(5.0) + " "
            + MathOps.EvenP(10) + " " + err + " core=" + DotCL.DotclHost.CoreLoaded);
    }
}
EOF
  echo "=== consume: C# RUNS the baked functions with DotCL.Runtime only (no Lisp, no core) ==="
  if (cd "$WORK/consume8" && dotnet build 2>"$WORK/consume8.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume8" && dotnet run 2>/dev/null </dev/null | tr -d '\r')"
    EXP="832040 25 Hi C# 2.5 True LispErrorException:negative: -1 core=False"
    if [ "$OUT" = "$EXP" ]; then
      echo "  baked functions ran: $OUT"
    else
      note "baked consumer printed '$OUT' (expected '$EXP')"
    fi
    if find "$WORK/consume8/bin" \( -name 'dotcl.core' -o -name '*.sil' \) | grep -q .; then
      note "baked consumer output carries a core"
    fi
  else
    note "baked-function consumer failed to compile"; tail -8 "$WORK/consume8.log"
  fi

  # --- NuGet package: PackageReference is the only thing the consumer writes ---
  # The same kind of library packed with :package, next to a DotCL.Runtime
  # package, on a throwaway local feed. The consumer project names no DLL and no
  # project: a PackageReference to FibLib restores the DLL and (through the
  # nuspec dependency) DotCL.Runtime from the feed alone.
  FEED="$WORK/feed"; PK="$WORK/pk"; mkdir -p "$FEED" "$PK"
  PKF="$WORK/pack.lisp"; PKF_W="$PKF"; PK_W="$PK"; FEED_W="$FEED"
  if command -v cygpath >/dev/null 2>&1; then
    PKF_W="$(cygpath -m "$PKF")"; PK_W="$(cygpath -m "$PK")"; FEED_W="$(cygpath -m "$FEED")"
  fi
  cat > "$PKF" <<EOF
(load "$CONTRIB_W")
(defpackage :fib-pk (:use :cl))
(in-package :fib-pk)
(dotnet:library ("FibLib" :version "1.0.0.0" :path "$PK_W/FibLib.dll"
                 :package "$FEED_W" :authors "dotcl check" :description "Fibonacci")
  (:module "FibLib.MathOps"
    (:functions
      (fib ((n Int64)) :returns Int64
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))))
(format t "PACK-EMIT-OK~%")
EOF
  echo "=== pack the library as a nupkg (dotnet:library :package) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$PKF_W" 2>"$WORK/pack.log" </dev/null | grep -q PACK-EMIT-OK); then
    note "nupkg emit failed"; tail -8 "$WORK/pack.log"
  fi
  [ -f "$FEED/FibLib.1.0.0.nupkg" ] || note "no FibLib.1.0.0.nupkg produced"
  unzip -Z1 "$FEED/FibLib.1.0.0.nupkg" 2>/dev/null | grep -q '^lib/netstandard2.0/FibLib.dll$' \
    || note "nupkg lacks lib/netstandard2.0/FibLib.dll"
  unzip -p "$FEED/FibLib.1.0.0.nupkg" FibLib.nuspec 2>/dev/null | grep -q 'id="DotCL.Runtime" version="\[' \
    || note "nuspec lacks the DotCL.Runtime dependency"

  echo "=== pack DotCL.Runtime onto the same feed ==="
  if ! dotnet pack "$RT" -c Release -o "$FEED" >"$WORK/rtpack.log" 2>&1 </dev/null; then
    note "dotnet pack DotCL.Runtime failed"; tail -8 "$WORK/rtpack.log"
  fi

  mkdir -p "$WORK/consume9"
  cat > "$WORK/consume9/nuget.config" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local" value="$FEED_W" />
  </packageSources>
</configuration>
EOF
  cat > "$WORK/consume9/consume9.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="FibLib" Version="1.0.0" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume9/Program.cs" <<EOF
using System;
using FibLib;
class Consume9 {
    static void Main() {
        Console.WriteLine(MathOps.Fib(30) + " core=" + DotCL.DotclHost.CoreLoaded);
    }
}
EOF
  echo "=== consume: PackageReference FibLib only (no DLL path, no project reference) ==="
  if (cd "$WORK/consume9" && NUGET_PACKAGES="$WORK/pkgs" dotnet build 2>"$WORK/consume9.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume9" && NUGET_PACKAGES="$WORK/pkgs" dotnet run --no-build 2>/dev/null </dev/null | tr -d '\r')"
    if [ "$OUT" = "832040 core=False" ]; then
      echo "  package consumer ran: $OUT"
    else
      note "package consumer printed '$OUT' (expected '832040 core=False')"
    fi
  else
    note "package consumer failed to build"; tail -8 "$WORK/consume9.log"
  fi

  # --- Lisp-driven BAKED CLASSES: method / ctor bodies compiled into the DLL --
  # A :methods entry named by a symbol is compiled in as a function taking self
  # first, and the public instance method calls it directly. The consumer news
  # the class, calls methods, reads and writes a field and a property, with
  # DotCL.Runtime only: no .lisp, no core.
  CPACK="$WORK/ClsLib.dll"
  CPACK_W="$CPACK"; CF="$WORK/cls.lisp"; CF_W="$CF"
  if command -v cygpath >/dev/null 2>&1; then
    CPACK_W="$(cygpath -m "$CPACK")"; CF_W="$(cygpath -m "$CF")"
  fi
  cat > "$CF" <<EOF
(load "$CONTRIB_W")
(defpackage :cls-lib (:use :cl))
(in-package :cls-lib)
(dotnet:library ("ClsLib" :version "1.0.0.0" :path "$CPACK_W")
  (:class "ClsLib.Counter" ()
    (:fields ("Count" Int32))
    (:properties ("Label" String))
    (:ctor ((start Int32))
      (dotnet:%set-invoke self "Count" start)
      (dotnet:%set-invoke self "Label" "ctr"))
    (:methods
      (add ((n Int32)) :returns Int32
        (dotnet:%set-invoke self "Count" (+ (dotnet:invoke self "Count") n))
        (dotnet:invoke self "Count"))
      (reset () :returns Void
        (dotnet:%set-invoke self "Count" 0))
      (check ((n Int32)) :returns Int32
        (if (minusp n) (error "negative: ~a" n) n))))
  (:class "ClsLib.Point" ()
    (:fields ("X" Double) ("Y" Double))
    (:ctor ((x Double) (y Double))
      (dotnet:%set-invoke self "X" x)
      (dotnet:%set-invoke self "Y" y))
    (:methods
      (dist ((other "ClsLib.Point")) :returns Double
        (let ((dx (- (dotnet:invoke self "X") (dotnet:invoke other "X")))
              (dy (- (dotnet:invoke self "Y") (dotnet:invoke other "Y"))))
          (sqrt (+ (* dx dx) (* dy dy))))))))
(format t "CLS-EMIT-OK~%")
EOF
  echo "=== emit BAKED-CLASS library from dotcl (method and ctor bodies compiled in) ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$CF_W" 2>"$WORK/cls.log" | grep -q CLS-EMIT-OK); then
    note "Lisp-driven baked-class emit failed"; tail -8 "$WORK/cls.log"
  fi
  [ -f "$CPACK" ] || note "no baked-class DLL produced"

  mkdir -p "$WORK/consume9"
  cat > "$WORK/consume9/consume9.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="ClsLib"><HintPath>../ClsLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume9/Program.cs" <<EOF
using System;
using ClsLib;
class Consume9 {
    static void Main() {
        var c = new Counter(5);
        int a = c.Add(3);
        c.Count += 10;
        int b = c.Add(1);
        string label = c.Label;
        c.Label = "set";
        string err;
        try { c.Check(-1); err = "none"; }
        catch (Exception e) { err = e.GetType().Name + ":" + e.Message; }
        double d = new Point(0, 0).Dist(new Point(3, 4));
        string afterLabel = c.Label;
        c.Reset();
        Console.WriteLine(a + " " + b + " " + label + " " + afterLabel + " " + c.Count + " "
            + d + " " + err + " core=" + DotCL.DotclHost.CoreLoaded);
    }
}
EOF
  echo "=== consume: C# news the classes and calls them with DotCL.Runtime only ==="
  if (cd "$WORK/consume9" && dotnet build 2>"$WORK/consume9.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume9" && dotnet run 2>/dev/null </dev/null | tr -d '\r')"
    EXP="8 19 ctr set 0 5 LispErrorException:negative: -1 core=False"
    if [ "$OUT" = "$EXP" ]; then
      echo "  baked classes ran: $OUT"
    else
      note "baked-class consumer printed '$OUT' (expected '$EXP')"
    fi
    if find "$WORK/consume9/bin" \( -name 'dotcl.core' -o -name '*.sil' \) | grep -q .; then
      note "baked-class consumer output carries a core"
    fi
  else
    note "baked-class consumer failed to compile"; tail -8 "$WORK/consume9.log"
  fi

  # --- Lisp-driven BAKED CORE FUNCTIONS: the core functions a body reaches ---
  # SIGNUM, UNION, COMPLEMENT, #'+ ... are written in Lisp in the core. The
  # library takes a copy of each one its bodies reach (and of what those call),
  # under names of its own, so the consumer still needs DotCL.Runtime only.
  SPACK="$WORK/ShakeLib.dll"
  SPACK_W="$SPACK"; SF="$WORK/shake.lisp"; SF_W="$SF"
  if command -v cygpath >/dev/null 2>&1; then
    SPACK_W="$(cygpath -m "$SPACK")"; SF_W="$(cygpath -m "$SF")"
  fi
  cat > "$SF" <<EOF
(load "$CONTRIB_W")
(defpackage :shake-lib (:use :cl))
(in-package :shake-lib)
(dotnet:library ("ShakeLib" :version "1.0.0.0" :path "$SPACK_W")
  (:module "ShakeLib.Ops"
    (:functions
      (sign ((n Int64)) :returns Int64 (signum n))
      (union-sum ((n Int64)) :returns Int64
        (reduce #'+ (union (list 1 2 3) (list n 3))))
      (odd-count ((n Int64)) :returns Int64
        (length (remove-if (complement #'evenp) (loop for i below n collect i))))
      (sorted ((n Int64)) :returns String
        (format nil "~a" (sort (mapcar (lambda (x) (- n x)) (list 3 1 2)) #'<))))))
(format t "SHAKE-EMIT-OK ~a~%" (length dotnet::*library-core-functions*))
EOF
  echo "=== emit library whose bodies call core functions (copied in) ==="
  SOUT="$(dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$SF_W" 2>"$WORK/shake.log" </dev/null | tr -d '\r')"
  case "$SOUT" in
    *SHAKE-EMIT-OK*) echo "  core functions copied in: ${SOUT##*SHAKE-EMIT-OK }, $(wc -c <"$SPACK" | tr -d ' ') bytes (FibLib $(wc -c <"$BPACK" | tr -d ' ') bytes)" ;;
    *) note "core-function library emit failed"; tail -8 "$WORK/shake.log" ;;
  esac

  mkdir -p "$WORK/consume10"
  cat > "$WORK/consume10/consume10.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="ShakeLib"><HintPath>../ShakeLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume10/Program.cs" <<EOF
using System;
using ShakeLib;
class Consume10 {
    static void Main() {
        Console.WriteLine(Ops.Sign(-5) + " " + Ops.Sign(7) + " " + Ops.UnionSum(9) + " "
            + Ops.OddCount(10) + " " + Ops.Sorted(10) + " core=" + DotCL.DotclHost.CoreLoaded);
    }
}
EOF
  echo "=== consume: C# runs bodies that call core functions, with DotCL.Runtime only ==="
  if (cd "$WORK/consume10" && dotnet build 2>"$WORK/consume10.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume10" && dotnet run 2>"$WORK/consume10.err" </dev/null | tr -d '\r')"
    EXP="-1 1 15 5 (7 8 9) core=False"
    if [ "$OUT" = "$EXP" ]; then
      echo "  core functions ran: $OUT"
    else
      note "core-function consumer printed '$OUT' (expected '$EXP')"; tail -8 "$WORK/consume10.err"
    fi
  else
    note "core-function consumer failed to compile"; tail -8 "$WORK/consume10.log"
  fi

  # --- An ASDF system as the source of a library ----------------------------
  # greeter (three files and a dependency, text-util) names its dotnet:library
  # form as a (:dotnet-library ...) component. Building the system compiles in
  # each function of greeter and of text-util that the library's bodies reach,
  # so the C# consumer still needs DotCL.Runtime only. Built through
  # dotcl:build-library :system and through (asdf:make ...), and a body that
  # reaches a special variable of the system is refused naming the calls.
  YDIR="$WORK/sys"; mkdir -p "$YDIR/text-util" "$YDIR/greeter"
  YPACK="$WORK/SysLib.dll"; YPACK_W="$YPACK"; YDIR_W="$YDIR"
  YF="$WORK/sys.lisp"; YF_W="$YF"; YBAD_W="$WORK/bad/SysLib.dll"
  if command -v cygpath >/dev/null 2>&1; then
    YPACK_W="$(cygpath -m "$YPACK")"; YDIR_W="$(cygpath -m "$YDIR")"; YF_W="$(cygpath -m "$YF")"
    YBAD_W="$(cygpath -m "$YBAD_W")"
  fi
  cat > "$YDIR/text-util/text-util.asd" <<'LISP'
(defsystem "text-util" :components ((:file "text-util")))
LISP
  cat > "$YDIR/text-util/text-util.lisp" <<'LISP'
(defpackage :text-util (:use :cl) (:export #:split-words #:clamp #:+max-repeat+))
(in-package :text-util)
(defconstant +max-repeat+ 3)
(defun clamp (x lo hi) (max lo (min x hi)))
(defun split-words (s)
  (let ((words '()) (start nil))
    (dotimes (i (length s))
      (if (char= (char s i) #\Space)
          (when start (push (subseq s start i) words) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq s start) words))
    (nreverse words)))
(defun unused () (error "not reached"))
LISP
  cat > "$YDIR/greeter/greeter.asd" <<'LISP'
(defsystem "greeter"
  :defsystem-depends-on ("dotnet-library")
  :build-operation "dotnet-library-op"
  :build-pathname "bin/SysLib"
  :depends-on ("text-util")
  :serial t
  :components ((:file "package") (:file "words") (:file "greet")
               (:dotnet-library "library")))
LISP
  echo '(defpackage :greeter (:use :cl :text-util))' > "$YDIR/greeter/package.lisp"
  cat > "$YDIR/greeter/words.lisp" <<'LISP'
(in-package :greeter)
(defun capitalize (w)
  (if (zerop (length w)) w
      (concatenate 'string (string-upcase (subseq w 0 1)) (string-downcase (subseq w 1)))))
(defun title-case (s) (format nil "~{~a~^ ~}" (mapcar #'capitalize (split-words s))))
(defparameter *greeting* "Hello")
(defun greeting () *greeting*)
LISP
  cat > "$YDIR/greeter/greet.lisp" <<'LISP'
(in-package :greeter)
(defmacro define-repeater (name word)
  `(defun ,name (n) (loop repeat (clamp n 1 +max-repeat+) collect ,word)))
(define-repeater hellos "Hello")
(defun greet (name times)
  (format nil "~{~a~^ ~}, ~a!" (hellos times) (title-case name)))
LISP
  cat > "$YDIR/greeter/library.lisp" <<'LISP'
(in-package :greeter)
(dotnet:library ("SysLib" :version "1.0.0.0")
  (:module "SysLib.Greeter"
    (:functions
      (greet-name ((name String) (times Int32)) :returns String :name "Greet"
        (greet name times))
      (title ((s String)) :returns String (title-case s))
      (word-count ((s String)) :returns Int32 (length (split-words s))))))
LISP
  cat > "$YF" <<LISP
(require "asdf")
(push #p"$YDIR_W/text-util/" asdf:*central-registry*)
(push #p"$YDIR_W/greeter/" asdf:*central-registry*)
(let ((written (dotcl:build-library :system "greeter" :output "$YPACK_W")))
  (format t "SYS-EMIT-OK ~a ~a~%" (length dotnet::*library-source-functions*)
          (and (probe-file written) t)))
(format t "SYS-FUNCTIONS~{ ~(~a~)~}~%"
        (sort (mapcar #'symbol-name dotnet::*library-source-functions*) #'string<))
(asdf:load-system "greeter")
(format t "SYS-LOAD-WROTE ~a~%" (and (probe-file "$YDIR_W/greeter/bin/SysLib.dll") t))
(asdf:make "greeter")
(format t "SYS-MAKE-WROTE ~a~%" (and (probe-file "$YDIR_W/greeter/bin/SysLib.dll") t))
(with-open-file (o "$YDIR_W/greeter/library.lisp" :direction :output :if-exists :supersede)
  (write-string "(in-package :greeter)
(dotnet:library (\"SysLib\")
  (:module \"SysLib.Greeter\" (:functions (hi () :returns String (greeting)))))" o))
(format t "SYS-SPECIAL ~a~%"
        (handler-case (progn (dotcl:build-library :system "greeter" :output "$YBAD_W") :written)
          (error (e) (let ((m (princ-to-string e)))
                       (if (and (search "HI calls GREETING" m) (search "*GREETING*" m)) :refused m)))))
LISP
  echo "=== emit library from an ASDF system and the system it depends on ==="
  YOUT="$(dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$YF_W" 2>"$WORK/sys.log" </dev/null | tr -d '\r')"
  echo "$YOUT" | grep '^SYS-' | sed 's/^/  /'
  case "$YOUT" in
    *"SYS-EMIT-OK 6 T"*) ;;
    *) note "library from an ASDF system: emit failed"; tail -8 "$WORK/sys.log" ;;
  esac
  case "$YOUT" in
    *"SYS-FUNCTIONS capitalize clamp greet hellos split-words title-case"*) ;;
    *) note "library from an ASDF system: compiled in other functions than expected" ;;
  esac
  case "$YOUT" in
    *"SYS-LOAD-WROTE NIL"*"SYS-MAKE-WROTE T"*) ;;
    *) note "asdf:load-system wrote the library, or asdf:make did not" ;;
  esac
  case "$YOUT" in
    *"SYS-SPECIAL REFUSED"*) ;;
    *) note "a body reaching a special variable of the system was not refused with its path" ;;
  esac

  mkdir -p "$WORK/consume13"
  cat > "$WORK/consume13/consume13.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="SysLib"><HintPath>../SysLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume13/Program.cs" <<EOF
using System;
using SysLib;
class Consume13 {
    static void Main() {
        Console.WriteLine(Greeter.Greet("ada lovelace", 7) + " | " + Greeter.Title("the  quick fox")
            + " | " + Greeter.WordCount(" a b  c ") + " core=" + DotCL.DotclHost.CoreLoaded);
    }
}
EOF
  echo "=== consume: C# runs the system's functions, with DotCL.Runtime only ==="
  if (cd "$WORK/consume13" && dotnet build 2>"$WORK/consume13.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume13" && dotnet run 2>"$WORK/consume13.err" </dev/null | tr -d '\r')"
    EXP="Hello Hello Hello, Ada Lovelace! | The Quick Fox | 3 core=False"
    if [ "$OUT" = "$EXP" ]; then
      echo "  system functions ran: $OUT"
    else
      note "system-library consumer printed '$OUT' (expected '$EXP')"; tail -8 "$WORK/consume13.err"
    fi
  else
    note "system-library consumer failed to compile"; tail -8 "$WORK/consume13.log"
  fi

  # --- Calling convention of compiled-in code ---------------------------------
  # Library code calls the runtime's calling convention through LibraryAbi:
  # optional / key / rest arguments, calls of 5+ arguments, multiple values,
  # closures, handlers, and tail calls between functions (a million deep).
  APACK="$WORK/AbiLib.dll"
  APACK_W="$APACK"; AF="$WORK/abi.lisp"; AF_W="$AF"
  if command -v cygpath >/dev/null 2>&1; then
    APACK_W="$(cygpath -m "$APACK")"; AF_W="$(cygpath -m "$AF")"
  fi
  cat > "$AF" <<EOF
(load "$CONTRIB_W")
(defpackage :abi-lib (:use :cl))
(in-package :abi-lib)
(dotnet:library ("AbiLib" :version "1.0.0.0" :path "$APACK_W")
  (:defun opt (a &optional (b 10) (c 20)) (+ a b c))
  (:defun kw (a &key (x 1) (y 2)) (list a x y))
  (:defun rst (&rest xs) (length xs))
  (:defun six (a b c d e f) (+ a b c d e f))
  (:defun two (n) (values n (* 2 n)))
  (:defun ping (n) (if (= n 0) :done (pong (- n 1))))
  (:defun pong (n) (if (= n 0) :done (ping (- n 1))))
  (:defun toss (n) (throw :tag (* n 5)))
  (:module "AbiLib.Ops"
    (:functions
      (args ((n Int64)) :returns String
        (format nil "~a ~a ~a ~a" (+ (opt n) (opt n 1) (opt n 1 2)) (kw n :y 5)
                (+ (rst) (rst n n n) (apply #'rst (list 1 2)))
                (+ (six n n n n n n) (funcall #'six 1 2 3 4 5 6))))
      (mv ((n Int64)) :returns Int64 (multiple-value-bind (a b) (two n) (+ a b)))
      (bump ((n Int64)) :returns Int64
        (let ((k 0)) (flet ((add () (incf k n))) (add) (add) k)))
      (guard ((n Int64)) :returns String
        (handler-case (if (< n 0) (error "neg") (format nil "ok ~a" n))
          (error (e) (format nil "caught ~a" e))))
      (mutual ((n Int64)) :returns String (string (ping n)))
      (thrown ((n Int64)) :returns String
        (format nil "~a ~a ~a" (catch :tag (when (> n 0) (throw :tag (* n 3))) -1)
                (catch :other (catch :tag (throw :other n)) -2)
                (catch :tag (toss n))))
      (cleanup ((n Int64)) :returns String
        (let ((log nil))
          (catch :out
            (unwind-protect (when (> n 0) (throw :out nil))
              (push :cleaned log)))
          (format nil "~a" log)))
      (bound ((n Int64)) :returns String
        (let ((seen nil))
          (handler-case
              (handler-bind ((error (lambda (e) (push (princ-to-string e) seen))))
                (error "bound ~a" n))
            (error () (format nil "~{~a~}" seen)))))
      (mvs ((n Int64)) :returns String
        (multiple-value-bind (q r) (floor n 7)
          (format nil "~a ~a ~a ~a ~a" q r
                  (multiple-value-list (values n (1+ n) (+ n 2)))
                  (nth-value 1 (values 1 n))
                  (multiple-value-call #'list (values 1 2) (values n)))))
      (prog1m ((n Int64)) :returns Int64
        (let ((c 0)) (multiple-value-prog1 (values n 2) (incf c)) (+ c n)))
      (boom ((n Int64)) :returns Int64 (if (> n 0) (error "boom ~a" n) 0))
      (blk ((n Int64)) :returns Int64
        (block done (mapc (lambda (x) (when (> x n) (return-from done x))) (list 1 5 9)) -1))
      (tag ((n Int64)) :returns Int64
        (let ((i 0)) (tagbody top (incf i) (when (< i n) (go top))) i))
      (rsc ((n Int64)) :returns String
        (restart-case (invoke-restart 'again n) (again (x) (format nil "again ~a" x))))
      (ign ((n Int64)) :returns Int64
        (if (ignore-errors (error "x")) 1 n)))))
(format t "ABI-EMIT-OK~%")
EOF
  echo "=== emit library exercising the calling convention ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$AF_W" 2>"$WORK/abi.log" </dev/null | grep -q ABI-EMIT-OK); then
    note "calling-convention library emit failed"; tail -8 "$WORK/abi.log"
  fi
  mkdir -p "$WORK/consume11"
  cat > "$WORK/consume11/consume11.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="AbiLib"><HintPath>../AbiLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume11/Program.cs" <<EOF
using System;
using AbiLib;
class Consume11 {
    static void Main() {
        Console.WriteLine(Ops.Args(2) + " | " + Ops.Mv(5) + " " + Ops.Bump(4) + " " + Ops.Guard(-1) + " "
            + Ops.Guard(2) + " " + Ops.Mutual(1000001) + " / " + Ops.Thrown(2) + " | " + Ops.Cleanup(1) + " " + Ops.Cleanup(0) + " | " + Ops.Bound(7)
            + " | " + Ops.Mvs(20) + " | " + Ops.Prog1m(9) + " / " + Boom()
            + " / " + Ops.Blk(4) + " " + Ops.Tag(5) + " " + Ops.Rsc(3) + " " + Ops.Ign(7));
    }
    static string Boom() {
        try { var z = Ops.Boom(0); Ops.Boom(3); return z + " no error"; }
        catch (Exception e) { return e.GetType().Name + ":" + e.Message; }
    }
}
EOF
  echo "=== consume: C# runs the calling-convention library ==="
  if (cd "$WORK/consume11" && dotnet build 2>"$WORK/consume11.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume11" && dotnet run 2>"$WORK/consume11.err" </dev/null | tr -d '\r')"
    EXP="60 (2 1 5) 5 33 | 15 8 caught neg ok 2 DONE / 6 2 10 | (CLEANED) (CLEANED) | bound 7 | 2 6 (20 21 22) 20 (1 2 20) | 10 / LispErrorException:boom 3 / 5 5 again 3 7"
    if [ "$OUT" = "$EXP" ]; then
      echo "  calling convention ran: $OUT"
    else
      note "calling-convention consumer printed '$OUT' (expected '$EXP')"; tail -8 "$WORK/consume11.err"
    fi
  else
    note "calling-convention consumer failed to compile"; tail -8 "$WORK/consume11.log"
  fi

  # --- Callbacks and events: delegates cross both ways, a class raises events ---
  # C# hands Func / Action / its own delegate types to compiled-in bodies, which
  # funcall them; a body returns a function (a lambda, a closure, #'name) as a
  # delegate; a class declares events a C# consumer subscribes to with +=, raises
  # them from its Lisp body, and subscribes to the events of a C# object.
  DPACK="$WORK/CbLib.dll"
  DPACK_W="$DPACK"; DF="$WORK/cb.lisp"; DF_W="$DF"
  if command -v cygpath >/dev/null 2>&1; then
    DPACK_W="$(cygpath -m "$DPACK")"; DF_W="$(cygpath -m "$DF")"
  fi
  cat > "$DF" <<EOF
(load "$CONTRIB_W")
(defpackage :cb-lib (:use :cl))
(in-package :cb-lib)
(dotnet:library ("CbLib" :version "1.0.0.0" :path "$DPACK_W")
  (:defun square (x) (* x x))
  (:delegate "CbLib.Step" ((n Int32) (label String)) :returns Void)
  (:class "CbLib.Worker" ()
    (:events ("Progress" "System.Action\`1[System.Int32]")
             ("Stepped" "CbLib.Step")
             ("Finished" "System.EventHandler"))
    (:fields ("Total" Int32) ("Hook" Object))
    (:properties ("Callback" "System.Func\`2[System.Int32,System.Int32]"))
    (:methods
      (run ((n Int32)) :returns Void
        (dotimes (i n)
          (dotnet:%set-invoke self "Total" (+ (dotnet:invoke self "Total") i))
          (dotnet:invoke self "OnProgress" i)
          (dotnet:invoke self "OnStepped" i "s"))
        (dotnet:invoke self "OnFinished" (dotnet:static "System.EventArgs" "Empty")))
      (call-back ((x Int32)) :returns Int32
        (funcall (dotnet:invoke self "Callback") x))
      (watch ((src "System.ComponentModel.INotifyPropertyChanged")) :returns Void
        (let ((h (lambda (s e) (declare (ignore s e))
                   (dotnet:%set-invoke self "Total" (+ 1000 (dotnet:invoke self "Total"))))))
          (dotnet:%set-invoke self "Hook" h)
          (dotnet:add-event src "PropertyChanged" h)))
      (unwatch ((src "System.ComponentModel.INotifyPropertyChanged")) :returns Void
        (dotnet:remove-event src "PropertyChanged" (dotnet:invoke self "Hook")))))
  (:module "CbLib.Ops"
    (:functions
      (apply-fn ((f "System.Func\`2[System.Int32,System.Int32]") (x Int32)) :returns Int32
        (funcall f x))
      (apply-act ((f "System.Action\`1[System.String]") (s String)) :returns Void
        (funcall f s))
      (apply-step ((f "CbLib.Step")) :returns Void (funcall f 7 "x"))
      (sum-with ((f "System.Func\`2[System.Int32,System.Int32]") (n Int32)) :returns Int32
        (loop for i below n sum (funcall f i)))
      (map-with ((f "System.Func\`2[System.Int32,System.Int32]")) :returns String
        (format nil "~a" (mapcar f (list 1 2 3))))
      (make-adder ((n Int32)) :returns "System.Func\`2[System.Int32,System.Int32]"
        (lambda (x) (+ x n)))
      (make-counter () :returns "System.Func\`1[System.Int32]"
        (let ((n 0)) (lambda () (incf n))))
      (squarer () :returns "System.Func\`2[System.Int32,System.Int32]" #'square)
      (compose ((f "System.Func\`2[System.Int32,System.Int32]") (g "System.Func\`2[System.Int32,System.Int32]"))
          :returns "System.Func\`2[System.Int32,System.Int32]"
        (lambda (x) (funcall f (funcall g x))))
      (same ((f "System.Func\`2[System.Int32,System.Int32]"))
          :returns "System.Func\`2[System.Int32,System.Int32]" f)
      (bad-fn () :returns "System.Func\`2[System.Int32,System.Int32]"
        (lambda (x) (if (minusp x) (error "negative: ~a" x) x)))
      (to-step () :returns "CbLib.Step"
        (lambda (n l) (declare (ignore n l)) nil)))))
(format t "CB-EMIT-OK~%")
EOF
  echo "=== emit library with callbacks and events ==="
  if ! (dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
          --asm "$SIL" "$DF_W" 2>"$WORK/cb.log" </dev/null | grep -q CB-EMIT-OK); then
    note "callback library emit failed"; tail -8 "$WORK/cb.log"
  fi
  mkdir -p "$WORK/consume12"
  cat > "$WORK/consume12/consume12.csproj" <<EOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="CbLib"><HintPath>../CbLib.dll</HintPath></Reference>
    <ProjectReference Include="$RT" />
  </ItemGroup>
</Project>
EOF
  cat > "$WORK/consume12/Program.cs" <<'EOF'
using System;
using System.ComponentModel;
using CbLib;
class Notifier : INotifyPropertyChanged {
    public event PropertyChangedEventHandler PropertyChanged;
    public void Fire() { PropertyChanged?.Invoke(this, new PropertyChangedEventArgs("x")); }
}
class Consume12 {
    static string Err(Action a) {
        try { a(); return "none"; }
        catch (Exception e) { return e.GetType().Name + ":" + e.Message; }
    }
    static void Main() {
        string got = "";
        Ops.ApplyAct(s => got += s, "hi");
        Ops.ApplyStep((n, l) => got += n + l);
        Console.WriteLine(Ops.ApplyFn(x => x * 2, 21) + " " + got + " " + Ops.SumWith(x => x * x, 4)
            + " " + Ops.MapWith(x => x + 100));
        var counter = Ops.MakeCounter(); counter(); counter();
        Func<int, int> id = x => x;
        Console.WriteLine(Ops.MakeAdder(10)(5) + " " + counter() + " " + Ops.Squarer()(9) + " "
            + Ops.Compose(x => x + 1, Ops.Squarer())(3) + " " + ReferenceEquals(Ops.Same(id), id));
        Console.WriteLine(Err(() => Ops.BadFn()(-3)) + " " + Err(() => Ops.BadFn()(3))
            + " " + Err(() => Ops.ApplyFn(x => throw new InvalidOperationException("boom"), 1)));
        Ops.ToStep()(1, "a");

        var w = new Worker();
        int sum = 0; string steps = ""; bool fin = false;
        Action<int> h = i => sum += i;
        w.Progress += h;
        w.Stepped += (n, l) => steps += n + l + ";";
        w.Finished += (s, e) => fin = ReferenceEquals(s, w);
        w.Run(4);
        int first = sum;
        w.Progress -= h;
        w.Run(3);
        Console.WriteLine(first + " " + sum + " " + steps + " " + fin + " " + w.Total);
        w.Callback = x => x + 1000;
        var nt = new Notifier();
        w.Watch(nt);
        nt.Fire(); nt.Fire();
        int watched = w.Total;
        w.Unwatch(nt);
        nt.Fire();
        Console.WriteLine(w.CallBack(5) + " " + watched + " " + w.Total
            + " core=" + DotCL.DotclHost.CoreLoaded);
    }
}
EOF
  echo "=== consume: C# passes delegates in, takes functions out, subscribes to events ==="
  if (cd "$WORK/consume12" && dotnet build 2>"$WORK/consume12.log" | grep -q "Build succeeded"); then
    OUT="$(cd "$WORK/consume12" && dotnet run 2>"$WORK/consume12.err" </dev/null | tr -d '\r')"
    EXP="42 hi7x 14 (101 102 103)
15 3 81 10 True
LispErrorException:negative: -3 none InvalidOperationException:boom
6 6 0s;1s;2s;3s;0s;1s;2s; True 9
1005 2009 2009 core=False"
    if [ "$OUT" = "$EXP" ]; then
      echo "  callbacks and events ran: $(echo "$OUT" | tr '\n' '|')"
    else
      note "callback consumer printed '$OUT' (expected '$EXP')"; tail -8 "$WORK/consume12.err"
    fi
  else
    note "callback consumer failed to compile"; tail -8 "$WORK/consume12.log"
  fi

  # A delegate the marshalling cannot carry is refused when the library is built.
  cat > "$WORK/cberr.lisp" <<EOF
(load "$CONTRIB_W")
(defpackage :cb-err (:use :cl))
(in-package :cb-err)
(defun try (label form)
  (format t "~a: ~a~%" label
          (handler-case (progn (eval form) "BUILT")
            (error (e) (substitute #\\Space #\\Newline (princ-to-string e))))))
(defmacro lib (&rest members)
  \`(quote (dotnet:library ("CbErr" :path "$(dirname "$DPACK_W")/CbErr.dll")
     (:delegate "CbErr.RefD" ((n "System.Int32&")) :returns Void)
     ,@members)))
(try "span" (lib (:module "CbErr.Ops" (:functions
  (f ((a "System.Buffers.ReadOnlySpanAction\`2[System.Byte,System.Int32]")) :returns Int32 1)))))
(try "ref" (lib (:module "CbErr.Ops" (:functions (f ((a "CbErr.RefD")) :returns Int32 1)))))
(try "open" (lib (:module "CbErr.Ops" (:functions (f ((a "System.Func\`2")) :returns Int32 1)))))
(try "plain" (lib (:module "CbErr.Ops" (:functions (f () :returns "System.Delegate" (lambda (x) x))))))
(try "event" (lib (:class "CbErr.C" () (:events ("E" "CbErr.RefD")))))
EOF
  echo "=== a delegate type that cannot be marshalled is an error when the library is built ==="
  dotnet run --project "$ROOT/runtime/runtime.csproj" -- --asm "$SIL" "$WORK/cberr.lisp" \
      2>/dev/null </dev/null | tr -d '\r' >"$WORK/cberr.out" || true
  grep -q "^span: .*parameter 1 is a ref struct" "$WORK/cberr.out" || note "ref struct delegate was not refused"
  grep -q "^ref: .*parameter 1 is passed by reference" "$WORK/cberr.out" || note "by-reference delegate was not refused"
  grep -q "^open: .*open generic type" "$WORK/cberr.out" || note "open generic delegate was not refused"
  grep -q "^plain: .*plain System.Delegate" "$WORK/cberr.out" || note "System.Delegate return was not refused"
  grep -q "^event: .*passed by reference" "$WORK/cberr.out" || note "by-reference event delegate was not refused"
  sed 's/^/  /' "$WORK/cberr.out" | cut -c1-150

  # --- The runtime surface a baked DLL calls, and the record of it -----------
  # Every DLL that calls into DotCL.Runtime names a set of its types, methods and
  # fields. Each such DLL carries the version and source digest of the runtime it
  # was built with and a hash of that set, and its module initializer refuses to
  # run (naming the members) when this runtime lacks one of them.
  SURF="$WORK/surface.lisp"; SURF_W="$SURF"
  SURFOUT="$WORK/surface.txt"; SURFOUT_W="$SURFOUT"
  if command -v cygpath >/dev/null 2>&1; then
    SURF_W="$(cygpath -m "$SURF")"; SURFOUT_W="$(cygpath -m "$SURFOUT")"
  fi
  cat > "$SURF" <<EOF
(let ((all nil))
  (dolist (spec '(("MyLibPack" "$CLIB_W") ("EnumPack" "$EPACK_W") ("ExcPack" "$XPACK_W")
                  ("IfacePack" "$IPACK_W") ("FibLib" "$BPACK_W") ("ClsLib" "$CPACK_W")
                  ("ShakeLib" "$SPACK_W") ("AbiLib" "$APACK_W") ("CbLib" "$DPACK_W")
                  ("SysLib" "$YPACK_W")))
    (let* ((s (dotcl:library-runtime-surface (second spec)))
           (g (dotcl:library-runtime-surface (second spec) t))
           (groups (mapcar (lambda (n) (count-if (lambda (e) (eql 0 (search n e))) g))
                           '("boundary " "abi " "primitive " "special " "internal "))))
      (format t "SURFACE ~a ~d: boundary ~d, abi ~d, primitive ~d, special ~d, internal ~d~%"
              (first spec) (length s) (first groups) (second groups) (third groups)
              (fourth groups) (fifth groups))
      (dolist (e g) (when (eql 0 (search "internal " e)) (format t "INTERNAL ~a ~a~%" (first spec) e)))
      (setf all (union all s :test #'string=))))
  (format t "SURFACE-TOTAL ~d~%" (length all))
  (with-open-file (o "$SURFOUT_W" :direction :output :if-exists :supersede)
    (dolist (e (sort (copy-list all) #'string<)) (format o "~a~%" e))))
EOF
  echo "=== runtime surface of the baked DLLs ==="
  dotnet run --project "$ROOT/runtime/runtime.csproj" -- \
      --asm "$SIL" "$SURF_W" 2>"$WORK/surface.log" </dev/null | tr -d '\r' >"$WORK/surface.out" \
    || note "surface listing failed"
  sed 's/^/  /' "$WORK/surface.out"
  [ -s "$SURFOUT" ] || { note "no surface listing"; tail -8 "$WORK/surface.log"; }
  # Compiled-in code reaches the compiler's internals only through LibraryAbi.
  if grep -q "^INTERNAL " "$WORK/surface.out"; then
    note "a compiled-in library calls runtime internals directly, not through LibraryAbi"
  fi
  # AbiLib uses catch / throw, handler-case / handler-bind, unwind-protect, restart-case,
  # multiple values and non-local exits: none of their support is named directly.
  if ! grep -q "^SURFACE AbiLib .*special 0, internal 0" "$WORK/surface.out"; then
    note "AbiLib names the support of a control or value operator directly"
  fi
  if [ -n "${DOTCL_SURFACE_DUMP:-}" ]; then cp "$SURFOUT" "$DOTCL_SURFACE_DUMP"; fi

  for key in dotcl-baked-runtime-version dotcl-baked-runtime-source dotcl-runtime-surface; do
    grep -aq "$key" "$BPACK" || note "FibLib.dll has no $key record"
  done
  if grep -aq "sha256:xxxx" "$BPACK" || ! grep -aEq "sha256:[0-9a-f]{64} n=[0-9]{5}" "$BPACK"; then
    note "FibLib.dll surface record was not filled in"
  fi

  # A runtime built from other source that still has every member: silent. One
  # that lacks a member: the module initializer fails and names it. Both are
  # simulated by editing a copy of FibLib.dll: the key that records the build's
  # source digest is renamed (so it no longer matches), and for the second case
  # a runtime method the library calls is renamed to one that does not exist.
  TAMPER="$(find "$WORK/consume8/bin" -name FibLib.dll | head -1)"
  if [ -n "$TAMPER" ]; then
    cp "$BPACK" "$WORK/FibLib.orig"
    perl -0777 -pe 's/dotcl-baked-runtime-source/dotcl-baked-runtime-sourcf/g' "$WORK/FibLib.orig" > "$TAMPER"
    OUT="$(cd "$WORK/consume8" && dotnet run --no-build 2>"$WORK/tamper1.err" </dev/null | tr -d '\r')"
    FIBEXP="832040 25 Hi C# 2.5 True LispErrorException:negative: -1 core=False"
    if [ "$OUT" = "$FIBEXP" ]; then
      echo "  other runtime source, no member missing: ran silently"
    else
      note "a library built from other runtime source failed: '$OUT'"; tail -4 "$WORK/tamper1.err"
    fi
    perl -0777 -pe 's/dotcl-baked-runtime-source/dotcl-baked-runtime-sourcf/g; s/ResolveBakedFunction/ResolveBakedFunctioo/g' "$WORK/FibLib.orig" > "$TAMPER"
    (cd "$WORK/consume8" && dotnet run --no-build >/dev/null 2>"$WORK/tamper2.err" </dev/null) && TRC=0 || TRC=$?
    if [ "$TRC" -ne 0 ] && grep -aq "ResolveBakedFunctioo" "$WORK/tamper2.err" \
         && grep -aq "does not have" "$WORK/tamper2.err"; then
      echo "  runtime lacking a member: failed with a message naming it"
    else
      note "a runtime lacking a member did not fail with a naming message"; tail -6 "$WORK/tamper2.err"
    fi
    cp "$BPACK" "$TAMPER"
  else
    note "no FibLib.dll in the consumer output to edit"
  fi
fi

if [ "$fail" -ne 0 ]; then echo "save-class-lib: FAIL"; exit 1; fi
echo "save-class-lib: OK"
