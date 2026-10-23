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
# The lifecycle host below lives in its own tree, not under $WORK: the SDK globs
# **/*.cs, so a second Program.cs anywhere beneath the first project would be
# compiled into it (two sets of top-level statements).
WORK2="$(mktemp -d)"
trap 'rm -rf "$WORK" "$WORK2"' EXIT

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
    // A name is read as the Lisp reader reads a symbol, so the source spelling
    // and the upcased one both name GREET.
    Console.WriteLine($"CALL-READ {DotclHost.ToClr<string>(DotclHost.Call("greet", "world"))}");
    Console.WriteLine($"CALL-UPPER {DotclHost.ToClr<string>(DotclHost.Call("GREET", "world"))}");
    Console.WriteLine($"CALL-QUALIFIED {DotclHost.ToClr<string>(DotclHost.Call("common-lisp-user::greet", "world"))}");

    // |...| keeps the case, so a lowercase symbol is reachable beside its
    // upcased namesake.
    DotclHost.EvalString("(defun |lower| () :lowercase)");
    DotclHost.EvalString("(defun lower () :upcased)");
    Console.WriteLine($"LOWER {DotclHost.Call("|lower|")} UPPER {DotclHost.Call("lower")}");

    // A miss on a name that exists in another case says how to write it.
    DotclHost.EvalString("(defun |mixedCase| () :mixed)");
    try { DotclHost.Call("mixedcase"); Console.WriteLine("CASE-MISS none"); }
    catch (InvalidOperationException e) { Console.WriteLine($"CASE-MISS {e.Message}"); }

    // Register reads its name the same way.
    DotclHost.Register("|hostLower|", _ => "registered-lower");
    DotclHost.Register("host-upper", _ => "registered-upper");
    Console.WriteLine($"REGISTER {DotclHost.ToClr(DotclHost.EvalString("(list (|hostLower|) (host-upper))"))}");

    // An unqualified name means the current package, nothing else.
    DotclHost.EvalString("(defpackage :mylib (:use :cl) (:export #:entry))");
    DotclHost.EvalString("(in-package :mylib) (defun entry () :from-mylib)");
    DotclHost.EvalString("(in-package :cl-user)");
    Console.WriteLine($"PACKAGE {DotclHost.CurrentPackage}");
    try { DotclHost.Call("ENTRY"); Console.WriteLine("ELSEWHERE none"); }
    catch (InvalidOperationException e) { Console.WriteLine($"ELSEWHERE {e.Message}"); }
    Console.WriteLine($"QUALIFIED {DotclHost.Call("mylib:entry")}");
    DotclHost.CurrentPackage = "mylib";
    Console.WriteLine($"AFTER-SET {DotclHost.CurrentPackage} {DotclHost.Call("ENTRY")}");
    DotclHost.CurrentPackage = "COMMON-LISP-USER";
}
catch (LispErrorException e) { Console.WriteLine($"EVAL-REFUSED {e.Message}"); }
CSEOF2

emit_csproj() { # $1 = directory to write into (default: $WORK)
  dir="${1:-$WORK}"
  mkdir -p "$dir"
  [ "$dir" = "$WORK" ] || cp "$WORK/DotCL.Runtime.dll" "$dir/DotCL.Runtime.dll"
  cat > "$dir/hostapi.csproj" <<CSPROJEOF
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

build_and_run() { # $1 = label, $2 = directory (default: $WORK)
  label="$1"
  dir="${2:-$WORK}"
  # To stderr: every caller runs this inside $(...), which would swallow a
  # failure message on stdout and leave the script exiting in silence.
  ( cd "$dir" && dotnet build hostapi.csproj -c Release -o bin ) > "$dir/host.log" 2>&1 \
    || { echo "FAIL ($label): building the host" >&2; tail -20 "$dir/host.log" >&2; exit 1; }
  cp "$ROOT/compiler/dotcl.core" "$dir/bin/dotcl.core"
  dotnet "$dir/bin/hostapi.dll" 2>&1
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
want "host" "$out" "CALL-READ hello world"
want "host" "$out" "CALL-UPPER hello world"
want "host" "$out" "CALL-QUALIFIED hello world"
want "host" "$out" "LOWER :LOWERCASE UPPER :UPCASED"
# The miss must name the spelling that works.
want "host" "$out" 'mixedCase is written "|mixedCase|"'
want "host" "$out" '"registered-lower" "registered-upper"' 
want "host" "$out" "PACKAGE COMMON-LISP-USER"
want "host" "$out" "ELSEWHERE"
want "host" "$out" "defined in MYLIB"
want "host" "$out" "QUALIFIED :FROM-MYLIB"
want "host" "$out" "AFTER-SET MYLIB :FROM-MYLIB"
echo "PASS (host): names are read as the reader reads them, unqualified means the current package, and a miss says what to write"

# -- lifecycle: initialization races and a failed core load -----------------
# Its own directory and process: both facts below are about what a FRESH
# process does, and the run above has already initialized and loaded a core.
echo "=== lifecycle (concurrent Initialize, failed LoadCore) ==="
LIFE="$WORK2/lifecycle"
mkdir -p "$LIFE"
cat > "$LIFE/Program.cs" <<'CSEOF3'
using DotCL;

// Eight threads reach Initialize at once, the way a host with several entry
// points into Lisp does. Nobody may throw, and the bootstrap runs once.
Console.WriteLine($"INIT-COUNT-BEFORE {DotclHost.InitializeCount}");
var start = new ManualResetEventSlim(false);
var errors = new System.Collections.Concurrent.ConcurrentQueue<string>();
var threads = new Thread[8];
for (int i = 0; i < threads.Length; i++)
{
    threads[i] = new Thread(() =>
    {
        start.Wait();
        try { DotclHost.Initialize(); }
        catch (Exception e) { errors.Enqueue($"{e.GetType().Name}: {e.Message}"); }
    });
    threads[i].Start();
}
start.Set();
foreach (var t in threads) t.Join();
foreach (var e in errors) Console.WriteLine($"INIT-ERROR {e}");
Console.WriteLine($"INIT-ERRORS {errors.Count}");
Console.WriteLine($"INIT-COUNT {DotclHost.InitializeCount}");

// A core load that fails must leave CoreLoaded false -- otherwise EnsureCore
// becomes a no-op and the host runs on an image that was never booted.
Console.WriteLine($"LC-BEFORE {DotclHost.CoreLoaded}");
try
{
    DotclHost.LoadCore(Path.Combine(AppContext.BaseDirectory, "no-such-file.core"));
    Console.WriteLine("LC-MISSING none");
}
catch (Exception e) { Console.WriteLine($"LC-MISSING {e.GetType().Name}"); }
Console.WriteLine($"LC-AFTER-FAILURE {DotclHost.CoreLoaded}");

// And the next attempt, with a core that exists, still works.
DotclHost.LoadCore(Path.Combine(AppContext.BaseDirectory, "dotcl.core"));
Console.WriteLine($"LC-AFTER-SUCCESS {DotclHost.CoreLoaded}");
Console.WriteLine($"LC-EVAL {DotclHost.ToClr<string>(DotclHost.EvalString("(format nil \"~a\" (+ 1 2))"))}");
CSEOF3
emit_csproj "$LIFE"
out="$(build_and_run "lifecycle" "$LIFE")"
want "lifecycle" "$out" "INIT-COUNT-BEFORE 0"
want "lifecycle" "$out" "INIT-ERRORS 0"
want "lifecycle" "$out" "INIT-COUNT 1"
want "lifecycle" "$out" "LC-BEFORE False"
want "lifecycle" "$out" "LC-MISSING FileNotFoundException"
want "lifecycle" "$out" "LC-AFTER-FAILURE False"
want "lifecycle" "$out" "LC-AFTER-SUCCESS True"
want "lifecycle" "$out" "LC-EVAL 3"
echo "PASS (lifecycle): concurrent Initialize bootstraps once, a failed LoadCore leaves the host loadable"

# -- concurrent EnsureCore -----------------------------------------------------
# Several components that each make sure a core is there, called from their own
# threads at once. The core must be loaded once: a second load signals "package
# COMMON-LISP is locked", and two loads at the same time corrupted a collection.
# A fresh process per round, since the core loads once per process; the race
# did not show every time, so several rounds.
echo "=== concurrent EnsureCore ==="
ENS="$WORK2/ensurecore"
mkdir -p "$ENS"
cat > "$ENS/Program.cs" <<'CSEOF6'
using DotCL;

var start = new ManualResetEventSlim(false);
var errors = new System.Collections.Concurrent.ConcurrentQueue<string>();
var threads = new Thread[8];
for (int i = 0; i < threads.Length; i++)
{
    threads[i] = new Thread(() =>
    {
        start.Wait();
        try { DotclHost.EnsureCore(); }
        catch (Exception e) { errors.Enqueue($"{e.GetType().Name}: {e.Message}"); }
    });
    threads[i].Start();
}
start.Set();
foreach (var t in threads) t.Join();
foreach (var e in errors) Console.WriteLine($"EC-ERROR {e}");
Console.WriteLine($"EC-ERRORS {errors.Count}");
Console.WriteLine($"EC-INIT-COUNT {DotclHost.InitializeCount}");
Console.WriteLine($"EC-LOADED {DotclHost.CoreLoaded}");
Console.WriteLine($"EC-EVAL {DotclHost.ToClr<string>(DotclHost.EvalString("(format nil \"~a\" (+ 1 2))"))}");
CSEOF6
emit_csproj "$ENS"
out="$(build_and_run "ensurecore" "$ENS")"
round=1
while :; do
  want "ensurecore round $round" "$out" "EC-ERRORS 0"
  want "ensurecore round $round" "$out" "EC-INIT-COUNT 1"
  want "ensurecore round $round" "$out" "EC-LOADED True"
  want "ensurecore round $round" "$out" "EC-EVAL 3"
  [ "$round" -ge 20 ] && break
  round=$((round + 1))
  out="$(dotnet "$ENS/bin/hostapi.dll" 2>&1)"
done
echo "PASS (ensurecore): 8 threads calling EnsureCore at once load the core once, 20 rounds"

# -- what a condition looks like on the .NET side ---------------------------
# Its own process for the same reason: the debugger hook is process-wide state.
echo "=== conditions (typed exception, wrapped .NET exception, handled in Lisp) ==="
COND="$WORK2/conditions"
mkdir -p "$COND"
cat > "$COND/Program.cs" <<'CSEOF4'
using DotCL;

DotclHost.EnsureCore();
DotclHost.SetThrowingDebuggerHook();

// (c) A Lisp ERROR arrives as a condition, not as a string.
try { DotclHost.EvalString("(error \"boom ~a\" 42)"); Console.WriteLine("LISP-ERR none"); }
catch (DotclConditionException e)
{
    Console.WriteLine($"LISP-ERR type={e.ConditionType} msg={e.Message} clr={(e.ClrException == null ? "null" : e.ClrException.GetType().Name)}");
    // The condition object is live: Lisp can still read it.
    DotclHost.Register("host-condition", _ => e.Condition);
    Console.WriteLine($"LISP-ERR-REPORT {DotclHost.ToClr<string>(DotclHost.EvalString("(princ-to-string (host-condition))"))}");
    Console.WriteLine($"LISP-ERR-TYPEP {DotclHost.EvalString("(typep (host-condition) 'simple-error)")}");
}

// (d) A .NET exception raised through interop keeps the original exception.
// The runtime throws such a failure itself (a LispErrorException, without
// running the debugger hook); with the typed hook installed the host entry point
// hands it over as a DotclConditionException all the same, so one catch covers
// both kinds of failure.
const string clrBoom = "(dotnet:invoke (dotnet:new \"System.Collections.ArrayList\") \"RemoveAt\" 5)";
try { DotclHost.EvalString(clrBoom); Console.WriteLine("CLR-RAW none"); }
catch (DotclConditionException e)
{
    var inner = e.ClrException;
    Console.WriteLine($"CLR-RAW type={e.ConditionType} clr={(inner == null ? "null" : inner.GetType().Name)} via={e.InnerException?.GetType().Name}");
}
catch (LispErrorException) { Console.WriteLine("CLR-RAW untyped"); }

// The same through Call, and a runtime error that is not from .NET.
DotclHost.EvalString("(defun host-car (x) (car x))");
try { DotclHost.Call("HOST-CAR", 5); Console.WriteLine("CALL-RAW none"); }
catch (DotclConditionException e) { Console.WriteLine($"CALL-RAW type={e.ConditionType}"); }

// A host call nested inside Lisp (Lisp -> host -> Lisp) is not converted: the
// Lisp frames in between still see the original condition and handle it.
DotclHost.Register("host-nested", _ => DotclHost.EvalString(clrBoom));
var nested = DotclHost.EvalString("(handler-case (host-nested) (error (c) (if (typep c 'error) :handled-in-lisp :other)))");
Console.WriteLine($"NESTED {nested}");

// Signalled as a condition (a Lisp handler re-signals it, or any code calls
// ERROR on it), the same failure reaches the hook -- and the .NET exception is
// still attached to it there.
try
{
    DotclHost.EvalString($"(handler-case {clrBoom} (error (c) (error c)))");
    Console.WriteLine("CLR-ERR none");
}
catch (DotclConditionException e)
{
    var inner = e.ClrException;
    Console.WriteLine($"CLR-ERR type={e.ConditionType} clr={(inner == null ? "null" : inner.GetType().Name)}");
}

// (e) A condition the Lisp side handles never reaches the host.
try
{
    var v = DotclHost.EvalString("(handler-case (error \"caught inside\") (error (c) (format nil \"handled: ~a\" c)))");
    Console.WriteLine($"HANDLED {DotclHost.ToClr<string>(v)}");
}
catch (Exception e) { Console.WriteLine($"HANDLED escaped {e.GetType().Name}"); }

// The old string-only behaviour is still selectable for a host written against it.
DotclHost.SetThrowingDebuggerHook(false);
try { DotclHost.EvalString("(error \"legacy ~a\" 7)"); Console.WriteLine("LEGACY none"); }
catch (InvalidOperationException e) { Console.WriteLine($"LEGACY {e.Message}"); }
// ... and there a runtime-raised failure arrives as it always did.
try { DotclHost.EvalString(clrBoom); Console.WriteLine("LEGACY-CLR none"); }
catch (LispErrorException e) { Console.WriteLine($"LEGACY-CLR {e.Condition.ConditionTypeName}"); }
CSEOF4
emit_csproj "$COND"
out="$(build_and_run "conditions" "$COND")"
want "conditions" "$out" "LISP-ERR type=SIMPLE-ERROR msg=boom 42 clr=null"
want "conditions" "$out" "LISP-ERR-REPORT boom 42"
want "conditions" "$out" "LISP-ERR-TYPEP T"
want "conditions" "$out" "CLR-RAW type=ERROR clr=ArgumentOutOfRangeException via=LispErrorException"
want "conditions" "$out" "CALL-RAW type=TYPE-ERROR"
want "conditions" "$out" "NESTED :HANDLED-IN-LISP"
want "conditions" "$out" "CLR-ERR type=ERROR clr=ArgumentOutOfRangeException"
want "conditions" "$out" "HANDLED handled:"
printf '%s\n' "$out" | grep -q "HANDLED escaped" \
  && { echo "FAIL (conditions): a condition handled in Lisp still reached the host"; printf '%s\n' "$out"; exit 1; }
want "conditions" "$out" "LEGACY SIMPLE-ERROR: legacy 7"
want "conditions" "$out" "LEGACY-CLR ERROR"
echo "PASS (conditions): the condition object reaches the host, a wrapped .NET exception survives, and Lisp-handled conditions do not escape"

echo "=== values, specials, output streams ==="
VAL="$WORK2/values"
mkdir -p "$VAL"
cat > "$VAL/Program.cs" <<'CSEOF5'
using DotCL;

DotclHost.EnsureCore();

// --- multiple values -------------------------------------------------------
// Call returns the primary value only; CallMv keeps them all. FLOOR is the
// smallest function where the second value is the point.
var q = DotclHost.Call("FLOOR", 7, 2);
var all = DotclHost.CallMv("FLOOR", 7, 2);
Console.WriteLine($"MV-PRIMARY {DotclHost.ToClr(q)} MV-COUNT {all.Length} "
                  + $"MV-0 {DotclHost.ToClr(all[0])} MV-1 {DotclHost.ToClr(all[1])}");

// A function returning nothing gives an empty array, not one NIL: the
// difference a host could not see before.
DotclHost.EvalString("(defun nothing () (values))");
Console.WriteLine($"MV-NONE {DotclHost.CallMv("NOTHING").Length}");

// The ordinary single-value case is one element, never null.
var one = DotclHost.CallMv("LIST", 1, 2);
Console.WriteLine($"MV-ONE {one.Length} {one[0] is not null}");

// Call and EvalString hand back the primary value itself, not a wrapper for
// the values: a host that checks the type sees a Fixnum. No values at all is NIL.
Console.WriteLine($"MV-CALL-TYPE {q is Fixnum fq && fq.Value == 3} {DotclHost.Call("NOTHING") is Nil}");
Console.WriteLine($"MV-EVAL-TYPE {DotclHost.EvalString("(floor 7 2)") is Fixnum} "
                  + $"{DotclHost.EvalString("(values)") is Nil}");

// EvalStringMv keeps the values of the LAST form.
var ev = DotclHost.EvalStringMv("(values :a :b :c)");
Console.WriteLine($"MV-EVAL {ev.Length} {DotclHost.ToClr(ev[2])}");

// --- the AssemblyLoad handler before Runtime is initialized -----------------
// The handler can run inside Runtime's static initializer (resolving the types it
// references loads assemblies, on the same thread), before the extension-method
// index is assigned. Put the index in that state and call what the handler calls.
{
    var flags = System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static;
    var field = typeof(DotCL.Runtime).GetField("_extensionMethodsByName", flags)!;
    var saved = field.GetValue(null);
    string res;
    field.SetValue(null, null);
    try
    {
        DotCL.Runtime.ClearExtensionMethodCache();
        typeof(DotCL.Runtime).GetMethod("MarkTypeCacheDirty", flags)!.Invoke(null, null);
        res = "ok";
    }
    catch (Exception e) { res = (e.InnerException ?? e).GetType().Name; }
    finally { field.SetValue(null, saved); }
    Console.WriteLine($"PRE-INIT-HANDLER {res}");
}

// --- special variables -----------------------------------------------------
DotclHost.EvalString("(defparameter *host-var* 41)");
Console.WriteLine($"SP-GET {DotclHost.ToClr(DotclHost.GetSpecial("*HOST-VAR*"))}");
DotclHost.SetSpecial("*HOST-VAR*", 42);
Console.WriteLine($"SP-SET {DotclHost.ToClr(DotclHost.EvalString("*host-var*"))}");

// Qualified names resolve like Call's do, and a built-in special is reachable.
Console.WriteLine($"SP-QUALIFIED {DotclHost.ToClr(DotclHost.GetSpecial("COMMON-LISP:*PRINT-BASE*"))}");
DotclHost.SetSpecial("CL:*PRINT-BASE*", 16);
Console.WriteLine($"SP-RADIX {DotclHost.ToClr(DotclHost.EvalString("(format nil \"~a\" 255)"))}");
DotclHost.SetSpecial("CL:*PRINT-BASE*", 10);

// A name nothing defines yet is created, so the Lisp side can read it back.
DotclHost.SetSpecial("*HOST-MADE-THIS*", "from the host");
Console.WriteLine($"SP-NEW {DotclHost.ToClr(DotclHost.EvalString("*host-made-this*"))}");

try { DotclHost.GetSpecial("*NEVER-BOUND-AT-ALL*"); Console.WriteLine("SP-UNBOUND none"); }
catch (InvalidOperationException e) { Console.WriteLine($"SP-UNBOUND {e.Message}"); }

// --- output streams --------------------------------------------------------
var buf = new System.IO.StringWriter();
DotclHost.SetStandardOutput(buf);
DotclHost.EvalString("(princ \"captured\") (terpri)");
DotclHost.SetStandardOutput(null);
Console.WriteLine($"OUT-CAPTURED {buf.ToString().Trim()}");
Console.WriteLine("OUT-RESTORED still on console");

var ebuf = new System.IO.StringWriter();
DotclHost.SetErrorOutput(ebuf);
DotclHost.EvalString("(format *error-output* \"diagnostic\")");
DotclHost.SetErrorOutput(null);
Console.WriteLine($"ERR-CAPTURED {ebuf.ToString().Trim()}");
CSEOF5
emit_csproj "$VAL"
out="$(build_and_run "values" "$VAL")"
want "values" "$out" "MV-PRIMARY 3 MV-COUNT 2 MV-0 3 MV-1 1"
want "values" "$out" "MV-NONE 0"
want "values" "$out" "MV-ONE 1 True"
want "values" "$out" "MV-EVAL 3 :C"
want "values" "$out" "MV-CALL-TYPE True True"
want "values" "$out" "MV-EVAL-TYPE True True"
want "values" "$out" "PRE-INIT-HANDLER ok"
want "values" "$out" "SP-GET 41"
want "values" "$out" "SP-SET 42"
want "values" "$out" "SP-QUALIFIED 10"
want "values" "$out" "SP-RADIX FF"
want "values" "$out" "SP-NEW from the host"
want "values" "$out" "SP-UNBOUND"
want "values" "$out" "is unbound"
want "values" "$out" "OUT-CAPTURED captured"
want "values" "$out" "OUT-RESTORED still on console"
want "values" "$out" "ERR-CAPTURED diagnostic"
echo "  ok: 14 assertions"

# -- DESTROY-THREAD on a host thread ------------------------------------------
# The host thread outlives the destroy and goes back to host code: only the Lisp
# it is running may be stopped, never a later wait in C# or a later call.
echo "=== destroy-thread on host threads ==="
DES="$WORK2/destroy"
mkdir -p "$DES"
cat > "$DES/Program.cs" <<'CSEOF7'
using DotCL;

DotclHost.EnsureCore();
DotclHost.EvalString(@"(progn
  (defvar *victim* nil)
  (defun save-self () (setf *victim* (dotcl:current-thread)) t)
  (defun destroy-victim () (dotcl:destroy-thread *victim*))
  (defun spin (n) (let ((s 0)) (dotimes (i n s) (setf s (logand (+ s i) #xffff)))))
  (defun lisp-sleep (s) (sleep s) :slept-through))");

string Try(Action a)
{
    try { a(); return "ok"; }
    catch (Exception e) { return e.GetType().Name; }
}

void Victim(string label, Func<List<string>, Action> body)
{
    var saved = new ManualResetEventSlim();
    var lines = new List<string>();
    var t = new Thread(() =>
    {
        DotclHost.Call("SAVE-SELF"); saved.Set();
        body(lines)();
    });
    t.Start(); saved.Wait(); Thread.Sleep(200);
    DotclHost.Call("DESTROY-VICTIM");
    t.Join();
    Console.WriteLine($"{label} {string.Join(" ", lines)}");
}

// Outside Lisp: nothing reaches the host code or the next call.
Victim("IN-CSHARP", lines => () =>
{
    lines.Add("sleep=" + Try(() => Thread.Sleep(800)));
    lines.Add("call=" + Try(() => DotclHost.Call("SPIN", 1000)));
    lines.Add("sleep2=" + Try(() => Thread.Sleep(100)));
});

// Computing in Lisp: the call is stopped, and nothing is left over.
Victim("COMPUTING", lines => () =>
{
    lines.Add("call=" + Try(() => DotclHost.Call("SPIN", 2_000_000_000)));
    lines.Add("sleep=" + Try(() => Thread.Sleep(100)));
    lines.Add("call2=" + Try(() => DotclHost.Call("SPIN", 10)));
    lines.Add("sleep2=" + Try(() => Thread.Sleep(100)));
});

// In a Lisp wait: the same.
Victim("LISP-SLEEP", lines => () =>
{
    lines.Add("call=" + Try(() => DotclHost.Call("LISP-SLEEP", 30)));
    lines.Add("sleep=" + Try(() => Thread.Sleep(100)));
    lines.Add("call2=" + Try(() => DotclHost.Call("SPIN", 10)));
});
CSEOF7
emit_csproj "$DES"
out="$(build_and_run "destroy" "$DES")"
want "destroy" "$out" "IN-CSHARP sleep=ok call=ok sleep2=ok"
want "destroy" "$out" "COMPUTING call=ThreadInterruptedException sleep=ok call2=ok sleep2=ok"
want "destroy" "$out" "LISP-SLEEP call=ThreadInterruptedException sleep=ok call2=ok"
echo "PASS (destroy): destroying a host thread stops only the Lisp it runs"

# -- the runtime in a load context of its own -----------------------------------
# A plugin host or a COM in-process server loads DotCL.Runtime into a context
# that is not the default one. The core, fasls and emitted assemblies must bind
# to that copy of the runtime, not to a second one in the default context. The
# host does not reference the runtime; it loads it from lib/ by reflection.
echo "=== runtime in a custom AssemblyLoadContext ==="
ALC="$WORK2/alc"
mkdir -p "$ALC/contrib/alc-probe"
cat > "$ALC/Program.cs" <<'CSEOF8'
using System.Reflection;
using System.Runtime.Loader;

var baseDir = AppContext.BaseDirectory;
string L(string p) => p.Replace('\\', '/');
var lib = Path.Combine(baseDir, "lib");
var ctx = new PluginContext(lib, args.Length > 0 && args[0] == "collectible");
var rt = ctx.LoadFromAssemblyPath(Path.Combine(lib, "DotCL.Runtime.dll"));
var host = rt.GetType("DotCL.DotclHost", true)!;
var paths = (List<string>)rt.GetType("DotCL.Runtime", true)!.GetField("ContribExtraSearchPaths")!.GetValue(null)!;
paths.Add(Path.Combine(baseDir, "contrib"));
paths.Add(Environment.GetEnvironmentVariable("ALC_REPO_CONTRIB")!);
var eval = host.GetMethod("EvalString", new[] { typeof(string) })!;

string Eval(string src)
{
    try { return eval.Invoke(null, new object[] { src })?.ToString() ?? "null"; }
    catch (TargetInvocationException e) { return $"ERROR {e.InnerException!.GetType().Name}: {e.InnerException.Message}"; }
}

try
{
    host.GetMethod("Initialize", Type.EmptyTypes)!.Invoke(null, null);
    host.GetMethod("LoadCore", new[] { typeof(string) })!.Invoke(null, new object[] { Path.Combine(baseDir, "dotcl.core") });
    Console.WriteLine("ALC-CORE ok");
}
catch (TargetInvocationException e) { Console.WriteLine($"ALC-CORE {e.InnerException!.GetType().Name}: {e.InnerException.Message}"); }

Console.WriteLine($"ALC-EVAL {Eval("(+ 1 2)")}");
Console.WriteLine($"ALC-COMPILE {Eval($"(progn (compile-file \"{L(baseDir)}contrib/alc-probe/alc-probe.lisp\") (compile-file \"{L(baseDir)}user.lisp\") :compiled)")}");
Console.WriteLine($"ALC-LOAD {Eval($"(progn (load \"{L(baseDir)}user.fasl\") (alc-user-fn 6))")}");
Console.WriteLine($"ALC-REQUIRE {Eval("(progn (require \"alc-probe\") (funcall (intern \"HELLO\" \"ALC-PROBE\")))")}");
Console.WriteLine($"ALC-RELOAD {Eval($"(progn (load \"{L(baseDir)}user.fasl\") (alc-user-fn 1))")}");
Eval("(require \"dotnet-class\")");
Console.WriteLine($"ALC-CLASS {Eval("(progn (dotnet:define-class \"AlcProbe.Greeter\" () (:methods (\"Greet\" ((n \"System.String\")) :returns \"System.String\" (format nil \"hi-~a\" n)))) (dotnet:invoke (dotnet:new \"AlcProbe.Greeter\") \"Greet\" \"alc\"))")}");

int copies = AppDomain.CurrentDomain.GetAssemblies().Count(a => a.GetName().Name == "DotCL.Runtime");
Console.WriteLine($"ALC-RUNTIME-COPIES {copies}");
Console.WriteLine($"ALC-IN-DEFAULT {AssemblyLoadContext.Default.Assemblies.Count(a => a.GetName().Name == "DotCL.Runtime")}");

class PluginContext : AssemblyLoadContext
{
    readonly string _dir;
    public PluginContext(string dir, bool collectible) : base("plugin", collectible) => _dir = dir;
    protected override Assembly Load(AssemblyName name)
    {
        var p = Path.Combine(_dir, name.Name + ".dll");
        return File.Exists(p) ? LoadFromAssemblyPath(p) : null;
    }
}
CSEOF8
cat > "$ALC/contrib/alc-probe/alc-probe.lisp" <<'LEOF1'
(defpackage :alc-probe (:use :cl) (:export #:hello))
(in-package :alc-probe)
(defun hello () :contrib-fasl)
(provide "alc-probe")
LEOF1
cat > "$ALC/user.lisp" <<'LEOF2'
(defun alc-user-fn (x) (* x 7))
LEOF2
cat > "$ALC/hostapi.csproj" <<CSPROJEOF
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>disable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <AssemblyName>hostapi</AssemblyName>
  </PropertyGroup>
</Project>
CSPROJEOF
( cd "$ALC" && dotnet build hostapi.csproj -c Release -o bin ) > "$ALC/host.log" 2>&1 \
  || { echo "FAIL (alc): building the host"; tail -20 "$ALC/host.log"; exit 1; }
cp -R "$WORK/lib" "$ALC/bin/lib"
cp -R "$ALC/contrib" "$ALC/bin/contrib"
cp "$ALC/user.lisp" "$ALC/bin/user.lisp"
cp "$ROOT/compiler/dotcl.core" "$ALC/bin/dotcl.core"
for mode in plain collectible; do
  out="$(ALC_REPO_CONTRIB="$(win "$ROOT/contrib")" dotnet "$ALC/bin/hostapi.dll" $mode 2>&1)"
  want "alc $mode" "$out" "ALC-CORE ok"
  want "alc $mode" "$out" "ALC-EVAL 3"
  want "alc $mode" "$out" "ALC-COMPILE :COMPILED"
  want "alc $mode" "$out" "ALC-LOAD 42"
  want "alc $mode" "$out" "ALC-REQUIRE :CONTRIB-FASL"
  want "alc $mode" "$out" "ALC-RELOAD 7"
  want "alc $mode" "$out" 'ALC-CLASS "hi-alc"'
  want "alc $mode" "$out" "ALC-RUNTIME-COPIES 1"
  want "alc $mode" "$out" "ALC-IN-DEFAULT 0"
done
echo "PASS (alc): core, compiled and contrib fasls and define-class bind to the runtime's own load context, plain and collectible"

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
