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
#               assert compile, not run — the facade body still dispatches to a
#               Lisp lambda, so running needs the Lisp loaded (stage 2 removes that).
#   3. corlib:  assert the DLL references the netstandard facade, not
#               System.Private.CoreLib (the retarget that makes it referenceable).
#
# PersistedAssemblyBuilder needs .NET 9+; skips cleanly otherwise.
#
# Usage: check.sh <repo-root>
set -eu
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
    echo "SKIP: PersistedAssemblyBuilder unavailable (needs .NET 9+)"; exit 0
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

# --- library (aggregation): MANY types → ONE DLL ----------------------------
# The stage 1 prototype was "1 define-class → 1 assembly"; a real library needs several
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
        // A function library: defun → public static. IsStatic:true.
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
  note "library emit failed (3 types → 1 DLL)"; tail -8 "$WORK/emit2.log"
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
  echo "SKIP-LISP: no cil-out.sil (run 'make cross-compile' first)"
else
  LLIB="$WORK/MyLisp.dll"
  LLIB_W="$LLIB"; LISPF="$WORK/emit.lisp"; LISPF_W="$LISPF"
  if command -v cygpath >/dev/null 2>&1; then
    LLIB_W="$(cygpath -m "$LLIB")"; LISPF_W="$(cygpath -m "$LISPF")"
  fi
  cat > "$LISPF" <<EOF
;; dotnet:%define-class arg 12 = save-to-path → emit a saved facade DLL.
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
  # references all three from that one library — the multi-type/static analogue
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
  # all — real, executable .NET metadata rather than facades that dispatch back
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
  ;; const holder — also standalone metadata (literals inline into the consumer)
  (:constants "EnumPack.Config"
    ("MaxRetries" Int32 7)
    ("ApiUrl" String "https://example.com"))
  ;; struct — a standalone value type (public fields, no dispatch)
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
  # NOTE: deliberately NO ProjectReference to DotCL.Runtime — the enum must be
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
  # THROWS and CATCHES it with NO DotCL.Runtime reference — a CL condition mapped
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
  # NO ProjectReference to DotCL.Runtime — the exception must be standalone.
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
  # class IMPLEMENTS it (class Circle : IShape) and is used through the interface
  # — all with NO DotCL.Runtime reference. Proves the emitted interface is a real,
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
  ;; delegate — a standalone callback type (runtime-provided machinery)
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
  # NO ProjectReference to DotCL.Runtime — a C# class implements the interface.
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
fi

if [ "$fail" -ne 0 ]; then echo "save-class-lib: FAIL"; exit 1; fi
echo "save-class-lib: OK"
