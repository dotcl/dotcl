using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Emit-side (System.Reflection.Emit) half of <see cref="CilAssembler"/>: walks an
/// instruction list (S-expression data from the Lisp compiler) and translates each
/// instruction to ILGenerator calls. Excluded from the netstandard2.0 build
/// (Compile Remove in DotCL.Runtime.csproj): an emit-free target can load and run
/// precompiled .fasl images but cannot JIT new code. The emit-free
/// registry/constant-pool/guards live in CilAssembler.cs.
/// </summary>
public partial class CilAssembler
{
    internal ILGenerator _il = null!;
    private readonly Dictionary<string, LocalBuilder> _locals = new();
    private readonly Dictionary<string, Label> _labels = new();

    /// <summary>The exact type the instruction just emitted left on the stack, when
    /// it is known. Set by the symbol loaders (whose runtime method returns Symbol)
    /// and read by the next instruction only: a one-instruction lookback, not a
    /// stack model. Lets a redundant castclass be dropped instead of emitted.</summary>
    private Type? _lastPushedType;

    // FASL mode: emit constants inline in IL instead of using constant pool
    internal bool _faslMode;
    // TypeBuilder for FASL mode: used to define closure body methods
    internal TypeBuilder? _faslTypeBuilder;
    // The shared body of the &key function whose typed entry this assembler is
    // building (SIL :call-key-shared). Null everywhere else.
    internal MethodInfo? _keyShared;
    private static int _faslClosureCount;

    // Debug info: when non-null, a (:line SL SC EL EC) instruction records
    // (ILOffset, startLine, startCol, endLine, endCol) here so FaslAssembler can
    // emit sequence points spanning the exact source form. Deduped by (startLine,
    // startCol) within a method. Null (the default) -> (:line ...) is a no-op.
    internal List<(int offset, int sl, int sc, int el, int ec)>? _seqPoints;
    private (int sl, int sc) _lastSeqPos = (-1, -1);

    // Debug info: when non-null, a (:local-var LOCALNAME SOURCENAME) instruction
    // records (localSlotIndex, SOURCENAME) here so FaslAssembler can emit a PDB
    // LocalVariable row and the debugger's Locals window shows the source name.
    // Null -> the marker is a no-op. Only user lexicals are marked (the compiler
    // emits these only for non-temp, non-boxed, non-special bindings).
    internal List<(int index, string name)>? _localVars;

    // Nested LocalScope tracking. (:scope-begin) opens a scope at the current IL
    // offset; (:local-var ...) names go to the innermost open scope (or _localVars
    // = the method-wide scope if none is open, e.g. parameters); (:scope-end)
    // closes it, recording [start, length). Nested lets nest their scopes, so the
    // PDB can disambiguate shadowed names. Collected only in debug.
    private readonly Stack<(int start, List<(int index, string name)> vars)> _scopeStack = new();
    internal List<(int start, int length, List<(int index, string name)> vars)>? _completedScopes;

    // One recorded body method's debug info (top-level function or nested
    // closure/lambda). Collected into FaslStructInternMap.DebugSink so both kinds
    // of body reach the PDB uniformly.
    internal readonly record struct DebugMethodInfo(
        System.Reflection.Emit.MethodBuilder Method,
        List<(int offset, int sl, int sc, int el, int ec)> Points,
        List<(int index, string name)> Locals,
        List<(int start, int length, List<(int index, string name)> vars)> Scopes,
        int IlLength);

    /// <summary>Assemble a FASL body method into <paramref name="innerAsm"/>,
    /// collecting its debug info (sequence points / named locals / scopes) into the
    /// shared DebugSink when debug is on. Used by the closure/lambda emitters so
    /// their bodies get PDB entries just like top-level function bodies.</summary>
    private void AssembleFaslBody(System.Reflection.Emit.MethodBuilder method,
                                  CilAssembler innerAsm, LispObject bodyInstrs)
    {
        var sink = _faslStructMap?.DebugSink;
        if (sink != null)
        {
            innerAsm._seqPoints = new();
            innerAsm._localVars = new();
            innerAsm._completedScopes = new();
        }
        innerAsm.Assemble(bodyInstrs);
        if (sink != null)
            sink.Add(new DebugMethodInfo(method, innerAsm._seqPoints!, innerAsm._localVars!,
                                         innerAsm._completedScopes!, innerAsm._il.ILOffset));
    }

    // --- Emit-side assembly ---

    // A DynamicMethod that emits too much IL (empirically a few MB) throws
    // InvalidProgramException at JIT time (first invocation) with no hint of the
    // cause. When that happens for a method whose IL is large, re-raise a clear,
    // actionable "form too large" error instead of the opaque one. A SMALL method
    // that InvalidPrograms is a genuine codegen bug, not a size limit, so it is
    // re-thrown unchanged.
    private const int LargeMethodIl = 500_000;

    private static LispErrorException TooLargeError(int size) =>
        new LispErrorException(new LispProgramError(
            $"form too large to compile: it emits {size} bytes of CIL for a single method, " +
            "exceeding the CLR's per-method size limit. Split it into smaller pieces; e.g. " +
            "compile/eval statements individually or in chunks, or move sub-parts into their " +
            "own functions."));

    private static LispObject AssembleAndRunSingle(LispObject instrList)
    {
        EnsureEmitAllowed("eval/compile of a compound form");
        if (IsLoneFunctionForm(instrList)) return AssembleLoneFunction(instrList);
        var asm = new CilAssembler();
        asm._runOnceInMemory = !HasBackwardBranch(instrList);
        var dm = new DynamicMethod("toplevel", typeof(LispObject),
            Type.EmptyTypes, typeof(CilAssembler).Module, true);
        asm._il = dm.GetILGenerator().ForLibrary();
        int savedUnit = _currentUnitId;
        var savedDms = _currentUnitDms;
        int unitId = BeginUnit();
        var holder = new List<object>();
        _currentUnitId = unitId;
        _currentUnitDms = holder;
        try { asm.Assemble(instrList); }
        finally { _currentUnitId = savedUnit; _currentUnitDms = savedDms; }
        // Publish weakly only if the unit produced closures; closure-free units
        // (the majority) leave no map entry at all.
        if (holder.Count > 0) RegisterUnit(unitId, holder);
        var fn = (Func<LispObject>)dm.CreateDelegate(typeof(Func<LispObject>));
        // Runtime.Eval serializes COMPILATION on _evalLock (the compiler/assembler
        // are not thread-safe), but the compiled form is now built: running it must
        // NOT hold the lock: the form may block waiting on other threads (e.g. an
        // lparallel `each` over APL execute `execute`) that themselves need the lock to
        // compile, which deadlocks. Release the lock across the run and re-acquire
        // after, keeping the outer `lock` balanced. (Nested evals each release
        // at their own AssembleAndRunSingle, so the count is 1 here.)
        bool releasedEvalLock = Runtime.SerializeEval
                                && System.Threading.Monitor.IsEntered(Runtime._evalLock);
        if (releasedEvalLock) System.Threading.Monitor.Exit(Runtime._evalLock);
        LispObject result;
        try
        {
            try { result = fn(); }
            catch (InvalidProgramException) when (asm._il.ILOffset > LargeMethodIl)
            {
                throw TooLargeError(asm._il.ILOffset);
            }
        }
        finally { if (releasedEvalLock) System.Threading.Monitor.Enter(Runtime._evalLock); }
        // Keep the holder alive across the unit's one-shot run so any closures it
        // builds can resolve their body DMs; afterward only LispFunction.RetainUnit
        // keeps it alive (transient units become collectible here).
        GC.KeepAlive(holder);
        return result;
    }

    /// <summary>Whether the instructions only make a function and return it:
    /// ((:MAKE-FUNCTION[-DIRECT] ...) (:CALL "Runtime.UnwrapMv") (:RET)), what
    /// COMPILE of a lambda expression and EVAL of #'(LAMBDA ...) produce.</summary>
    private static bool IsLoneFunctionForm(LispObject instrList) =>
        instrList is Cons c1 && c1.Car is Cons mk && mk.Car is Symbol op
        && (op.Name == "MAKE-FUNCTION" || op.Name == "MAKE-FUNCTION-DIRECT")
        && c1.Cdr is Cons c2 && c2.Car is Cons call && call.Car is Symbol cop && cop.Name == "CALL"
        && call.Cdr is Cons ca && ca.Car is LispString cs && cs.Value == "Runtime.UnwrapMv" && ca.Cdr is Nil
        && c2.Cdr is Cons c3 && c3.Car is Cons ret && ret.Car is Symbol rop && rop.Name == "RET"
        && ret.Cdr is Nil && c3.Cdr is Nil;

    /// <summary>The function a lone MAKE-FUNCTION form makes. The function object
    /// is built while its instruction is assembled; the one-shot method around
    /// it would only load it from the unit and return it, and JIT-compiling that
    /// method with full optimization cost about as much as the function body.
    /// The load it emits goes into a method that is never compiled.</summary>
    private static LispObject AssembleLoneFunction(LispObject instrList)
    {
        var asm = new CilAssembler();
        var dm = new DynamicMethod("toplevel_unused", typeof(LispObject),
            Type.EmptyTypes, typeof(CilAssembler).Module, true);
        asm._il = dm.GetILGenerator().ForLibrary();
        int savedUnit = _currentUnitId;
        var savedDms = _currentUnitDms;
        int unitId = BeginUnit();
        var holder = new List<object>();
        _currentUnitId = unitId;
        _currentUnitDms = holder;
        try { asm.Assemble(new Cons(((Cons)instrList).Car, Nil.Instance)); }
        finally { _currentUnitId = savedUnit; _currentUnitDms = savedDms; }
        RegisterUnit(unitId, holder);
        return (LispObject)holder[holder.Count - 1];
    }

    /// <summary>
    /// Split instruction list at :toplevel-boundary markers.
    /// Returns null if no boundaries found.
    /// </summary>
    internal static List<LispObject>? SplitAtBoundaries(LispObject instrList)
    {
        bool hasBoundary = false;
        var cur = instrList;
        while (cur is Cons c)
        {
            if (c.Car is Cons inner && inner.Car is Symbol sym && sym.Name == "TOPLEVEL-BOUNDARY")
            {
                hasBoundary = true;
                break;
            }
            cur = c.Cdr;
        }
        if (!hasBoundary) return null;

        var segments = new List<LispObject>();
        LispObject current = Nil.Instance;
        var instrs = new List<LispObject>();

        cur = instrList;
        while (cur is Cons c)
        {
            if (c.Car is Cons inner && inner.Car is Symbol sym && sym.Name == "TOPLEVEL-BOUNDARY")
            {
                // End current segment
                if (instrs.Count > 0)
                {
                    LispObject seg = Nil.Instance;
                    for (int i = instrs.Count - 1; i >= 0; i--)
                        seg = new Cons(instrs[i], seg);
                    segments.Add(seg);
                    instrs.Clear();
                }
            }
            else
            {
                instrs.Add(c.Car);
            }
            cur = c.Cdr;
        }
        // Last segment
        if (instrs.Count > 0)
        {
            LispObject seg = Nil.Instance;
            for (int i = instrs.Count - 1; i >= 0; i--)
                seg = new Cons(instrs[i], seg);
            segments.Add(seg);
        }

        return segments;
    }

    // Delegates bound to closure-body DynamicMethods, one per closure SITE.
    // The delegate does not depend on the closure's environment: env is passed as
    // the body's first argument at call time, not bound into the delegate. So the
    // same delegate serves every closure the site ever creates, and CreateDelegate
    // -- which cost about 380 ns and the bulk of 512 B on EVERY closure creation --
    // runs once.
    // Keyed by the unit holder rather than by unitId so the cache is collected with
    // the unit; a static id-keyed table would pin the delegates and ids are reused.
    // A dictionary rather than an array indexed by dmIndex: a unit's constant
    // holder grows as more code is assembled into it, so any size fixed when the
    // cache was first built could be outrun by a later site.
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<
        List<object>, System.Collections.Concurrent.ConcurrentDictionary<int, Delegate>>
        _closureDelegates = new();

    // The lambda list of an in-memory closure site. A closure's body delegate is
    // shared by every closure its site makes (see GetClosureDelegate), so the list
    // is kept per delegate rather than stored on each closure as it is made: the
    // creation path stays as it was. The DynamicMethod table carries the list from
    // assembly until the delegate exists. Both are weak, so a collected unit takes
    // its entries with it.
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<
        DynamicMethod, LispObject> _closureDmLambdaLists = new();
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<
        Delegate, LispObject> _closureLambdaLists = new();

    private static void RecordClosureLambdaList(DynamicMethod dm, LispObject? lambdaList)
    {
        if (lambdaList == null) return;
        var ll = InternLambdaList(lambdaList, null);
        _closureDmLambdaLists.GetValue(dm, _ => ll);
    }

    // A method of its own: a lambda capturing a local of GetClosureDelegate would
    // make the compiler allocate its display class on every call there, which is
    // every closure creation, not only the first of a site.
    private static void CarryClosureLambdaList(DynamicMethod dm, Delegate made)
    {
        if (_closureDmLambdaLists.TryGetValue(dm, out var ll))
            _closureLambdaLists.GetValue(made, _ => ll);
    }

    /// <summary>The lambda list of the in-memory closure site whose body delegate
    /// is DEL, or null.</summary>
    internal static LispObject? ClosureSiteLambdaList(Delegate del)
        => _closureLambdaLists.TryGetValue(del, out var ll) ? ll : null;

    private static ConstructorInfo? _lazy__lambdaListAttrCtor;
    private static ConstructorInfo _lambdaListAttrCtor => _lazy__lambdaListAttrCtor ??=
        typeof(LispLambdaListAttribute).GetConstructor(new[] { typeof(string), typeof(string) })!;

    /// <summary>Put an anonymous function's lambda list on the FASL method its
    /// delegate calls, as text (see LispLambdaListAttribute).</summary>
    private static void SetFaslLambdaList(MethodBuilder method, LispObject? lambdaList)
    {
        if (lambdaList == null) return;
#if NET9_0_OR_GREATER
        if (LibraryAbiIL.Active) return;
#endif
        var pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
        method.SetCustomAttribute(new CustomAttributeBuilder(_lambdaListAttrCtor,
            new object?[] { FaslLambdaListText(lambdaList, null), pkg?.Name }));
    }

    private static Delegate GetClosureDelegate(List<object> holder, int dmIndex, Type delType)
    {
        var cache = _closureDelegates.GetValue(
            holder, static _ => new System.Collections.Concurrent.ConcurrentDictionary<int, Delegate>());
        // The type check is defensive: a site's signature is fixed by the emitter,
        // so a mismatch cannot normally happen, and returning a wrong-typed
        // delegate would be far worse than rebuilding one.
        if (cache.TryGetValue(dmIndex, out var cached) && cached.GetType() == delType) return cached;
        var dm = (DynamicMethod)holder[dmIndex];
        var made = dm.CreateDelegate(delType);
        CarryClosureLambdaList(dm, made);
        // Racing threads just build two equivalent delegates; either is correct.
        cache[dmIndex] = made;
        return made;
    }

    // MakeClosure and MakeClosureDirect are not inlined: their callers are
    // in-memory methods (COMPILE, EVAL), JIT-compiled with full optimization on
    // first call, and inlining the unit lookup, the delegate cache and the
    // closure factory into each one made a closure-building COMPILE spend about
    // 2 ms in the JIT.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public static LispObject MakeClosure(object[] env, int unitId, int dmIndex, int arity)
    {
        var holder = TryGetUnitHolder(unitId)
            ?? throw new LispErrorException(new LispProgramError(
                "internal: closure compilation unit was collected while still callable"));
        var closureDel = (Func<object[], LispObject[], LispObject>)GetClosureDelegate(
            holder, dmIndex, typeof(Func<object[], LispObject[], LispObject>));
        // Pin the unit so this closure can still resolve sibling DMs (e.g. a
        // nested closure built when it is later called), and so the unit's JIT
        // code outlives the enclosing function exactly as long as this closure does.
        return new LispFunction(closureDel, env, null, arity) { RetainUnit = holder };
    }

    // Direct-params variant of MakeClosure: the body DM was emitted with
    // signature (object[] env, LispObject a0..aN-1), so bind the matching
    // per-arity delegate and build the LispFunction via MakeDirectClosure
    // (which installs _funcN for the direct path plus an args-array wrapper
    // that runs the same Runtime.CheckArityExact the args-array body used to).
    /// <summary>The delegate type of a direct-params closure body of ARITY:
    /// (object[] env, LispObject a0..aN-1) -> LispObject. Shared by the in-process
    /// path (MakeClosureDirect) and the .fasl emitter, which have to agree on the
    /// signature down to the type identity.</summary>
    internal static Type DirectClosureDelegateType(int arity) => arity switch
    {
        0 => typeof(Func<object[], LispObject>),
        1 => typeof(Func<object[], LispObject, LispObject>),
        2 => typeof(Func<object[], LispObject, LispObject, LispObject>),
        3 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject>),
        4 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject>),
        5 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
        6 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
        _ => throw new LispErrorException(new LispProgramError(
            $"internal: direct closure unsupported arity {arity}"))
    };

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public static LispObject MakeClosureDirect(object[] env, int unitId, int dmIndex, int arity, string fnName)
    {
        var holder = TryGetUnitHolder(unitId)
            ?? throw new LispErrorException(new LispProgramError(
                "internal: closure compilation unit was collected while still callable"));
        Type delType = arity switch
        {
            0 => typeof(Func<object[], LispObject>),
            1 => typeof(Func<object[], LispObject, LispObject>),
            2 => typeof(Func<object[], LispObject, LispObject, LispObject>),
            3 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject>),
            4 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject>),
            5 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
            6 => typeof(Func<object[], LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
            _ => throw new LispErrorException(new LispProgramError(
                $"internal: MakeClosureDirect unsupported arity {arity}"))
        };
        var fn = LispFunction.MakeDirectClosure(
            GetClosureDelegate(holder, dmIndex, delType), env, fnName);
        // Pin the unit: same lifetime contract as MakeClosure.
        fn.RetainUnit = holder;
        return fn;
    }

    // Emit IL that loads a unit-scoped constant and casts it to LispObject;
    // the collectible counterpart of (ldc.i4 idx; call GetConstant). Used for
    // MAKE-FUNCTION lambda objects so a transient compile's lambda is reclaimed
    // with its unit instead of pinned in the global pool forever .
    private void EmitLoadUnitConstant(int idx)
    {
        _il.Emit(OpCodes.Ldc_I4, _currentUnitId);
        _il.Emit(OpCodes.Ldc_I4, idx);
        _il.Emit(OpCodes.Call, _getUnitConstant);
        EmitAs(typeof(LispObject));
    }

    // Store a freshly built lambda in the current unit and emit IL to load it.
    // The lambda pins the holder: while the lambda is reachable its body can run,
    // and a body can reach into its unit (a nested MAKE-FUNCTION / MAKE-CLOSURE
    // loads a unit constant), so dropping the holder first faults.
    //
    // This used to pin only when the lambda's body had itself added constants
    // (holder.Count > beforeCount), on the theory that a body which added none
    // cannot reach the unit. That predicate is wrong somewhere: cl-bench's
    // clos/defclass reliably reached GetUnitConstant from an unpinned
    // lambda_direct and died with "compilation-unit constant was collected while
    // still loadable" -- one eval built 217 lambdas of which only the last was
    // pinned. Rather than repair the predicate, pin unconditionally: it is the
    // sound rule, and it costs collectibility only for a unit that still has a
    // reachable function, which is exactly the case that must not be collected.
    private void PushUnitFunction(LispFunction fn, int beforeCount)
    {
        var holder = _currentUnitDms;
        if (holder != null) fn.RetainUnit = holder;
        EmitLoadUnitConstant(AddUnitConstant(fn));
    }

    // Register the run-once function re-registration constant (defun / (setf NAME) /
    // function definitions emit IL that re-registers fn after fmakunbound at runtime)
    // in the collectible per-unit store when a unit is active, so redefining or
    // fmakunbound-ing the name lets the old LispFunction, and the DynamicMethod JIT
    // code behind it, collect instead of being pinned in the global _constants pool
    // forever (the Coalton GB-working-set leak; the closure-DM fix moved only inner
    // lambdas, leaving top-level definition constants here). Falls back to the global
    // pool if no unit is active. If fn's body built closures into this unit, pin the
    // holder onto fn (mirrors AddConstant) so they stay resolvable while fn is
    // callable.
    private (bool unit, int idx) AddReRegisterConstant(LispFunction fn)
    {
        if (_currentUnitDms is { } holder)
        {
            if (fn.RetainUnit == null && holder.Count > 0) fn.RetainUnit = holder;
            return (true, AddUnitConstant(fn));
        }
        return (false, AddConstant(fn));
    }

    // Emit IL that loads the constant produced by AddReRegisterConstant and casts it
    // to LispFunction, leaving it on the stack.
    private void EmitLoadReRegisterConstant((bool unit, int idx) c)
    {
        if (c.unit)
        {
            _il.Emit(OpCodes.Ldc_I4, _currentUnitId);
            _il.Emit(OpCodes.Ldc_I4, c.idx);
            _il.Emit(OpCodes.Call, _getUnitConstant);
        }
        else
        {
            _il.Emit(OpCodes.Ldc_I4, c.idx);
            _il.Emit(OpCodes.Call, _getConstant);
        }
        EmitAs(typeof(LispFunction));
    }

    // --- Assembly ---

    /// <summary>Whether BODY's first instruction, past local declarations and
    /// source markers, is the call that takes the caller's value mode. Only such
    /// a body may be marked as a function's direct entry
    /// (LispFunction.MarkMvModeEntry): the mode must be taken before any Lisp
    /// code runs.</summary>
    internal static bool BodyReadsMvMode(LispObject body)
    {
        for (var cur = body; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is not Cons ic || ic.Car is not Symbol op) return false;
            if (op.Name == "DECLARE-LOCAL" || op.Name == "LINE") continue;
            return op.Name == "CALL" && ic.Cdr is Cons a
                && GetString(a.Car) == "MultipleValues.TakeMode";
        }
        return false;
    }

    internal void Assemble(LispObject instructions)
    {
        bool hoistTypes = false;
        if (_assembleDepth == 0)
        {
            if (!_localSlotsPlanned)
            {
                _localSlotsPlanned = true;
                try { instructions = ElideStackTemps(instructions); }
                catch { /* keep the instructions as they are */ }
                try { PlanLocalSlots(instructions); }
                catch { _localSlotOf = null; _localSlotBuilders = null; }
                hoistTypes = true;
            }
            else
            {
                // A second body on the same assembler: the plan only knew the
                // first one, so declare everything new on its own from here.
                _localSlotOf = null;
            }
        }
        if (_assembleDepth == 0 && _selfFrameName != null && _selfFrameLocal == null)
            EmitSelfFrameProlog();
        if (hoistTypes) HoistStructTypeConstants(instructions);
        _assembleDepth++;
        try { AssembleBody(instructions); }
        finally { _assembleDepth--; }
    }

    // The structure name a checked slot access compares against, loaded once
    // at entry into a local: from the constant pool it is a volatile read the
    // JIT cannot hoist out of a loop.
    private Dictionary<Cons, LocalBuilder>? _hoistedTypeSites;

    private void HoistStructTypeConstants(LispObject instructions)
    {
        Dictionary<Symbol, LocalBuilder>? locals = null;
        for (var cur = instructions; cur is Cons c && c.Cdr is Cons next; cur = c.Cdr)
        {
            if (c.Car is not Cons lc || lc.Car is not Symbol lop || lop.Name != "LOAD-CONST"
                || lc.Cdr is not Cons la || la.Car is not Symbol sym) continue;
            if (next.Car is not Cons cc || cc.Car is not Symbol cop || cop.Name != "CALL"
                || cc.Cdr is not Cons ca) continue;
            var m = GetString(ca.Car);
            if (m != "Runtime.StructRefIT" && m != "Runtime.StructSetIT" && m != "Runtime.StructOf")
                continue;
            locals ??= new Dictionary<Symbol, LocalBuilder>(ReferenceEqualityComparer.Instance);
            if (!locals.TryGetValue(sym, out var lb))
            {
                lb = _il.DeclareLocal(typeof(Symbol));
                EmitLoadConst(sym);
                if (_lastPushedType != typeof(Symbol))
                    _il.Emit(OpCodes.Castclass, typeof(Symbol));
                _lastPushedType = null;
                _il.Emit(OpCodes.Stloc, lb);
                locals[sym] = lb;
            }
            (_hoistedTypeSites ??= new Dictionary<Cons, LocalBuilder>(ReferenceEqualityComparer.Instance))[lc] = lb;
        }
    }

    private void AssembleBody(LispObject instructions)
    {
        FindTailSites(instructions);
        if (_typedSelfMethod != null) FindTypedTailSites(instructions);
        var cur = instructions;
        bool dead = false;
        while (cur is Cons c)
        {
            var instr = c.Car;
            if (dead)
            {
                // Skip dead code until a reachable point
                if (instr is Cons ic && ic.Car is Symbol ds)
                {
                    var dn = ds.Name;
                    if (dn == "LABEL" || dn == "BEGIN-CATCH-BLOCK"
                        || dn == "BEGIN-FILTER-BLOCK" || dn == "BEGIN-FILTER-HANDLER"
                        || dn == "BEGIN-FINALLY-BLOCK" || dn == "END-EXCEPTION-BLOCK"
                        || dn == "BEGIN-EXCEPTION-BLOCK")
                    {
                        dead = false;
                    }
                    else if (dn == "DECLARE-LOCAL")
                    {
                        // Locals must still be declared even in dead code
                        AssembleOne(instr);
                        cur = c.Cdr;
                        continue;
                    }
                    else
                    {
                        cur = c.Cdr;
                        continue;
                    }
                }
                else
                {
                    cur = c.Cdr;
                    continue;
                }
            }
            // A string literal reaches the assembler as (:LDSTR s) (:NEWOBJ
            // "LispString"): a .sil file is text, so it cannot carry the live
            // object the way an in-process compile does. Taken literally that
            // builds a fresh LispString every time the expression is EVALUATED.
            // The literal is a constant (CLHS 3.7.1), so hoist it: one object per
            // distinct value, built once. This is the whole cost of a string
            // literal in the core, which the interpreter's dispatch runs per call.
            if (c.Cdr is Cons nextCell && TryEmitStringLiteral(instr, nextCell.Car))
            {
                cur = nextCell.Cdr;
                continue;
            }
            if (_typedTailSites != null && _typedTailSites.Contains(c)
#if NET9_0_OR_GREATER
                // A library's typed call to another function is an ordinary call.
                && !(LibraryAbiIL.Active && instr is Cons lc0 && lc0.Car is Symbol ls0
                     && (ls0.Name == "CALL-TYPED-ENTRY" || ls0.Name == "CALL-TYPED-SYM"))
#endif
                )
            {
                // A typed body's call whose result is the body's result: to
                // itself, or to another function's typed entry.
                if (instr is Cons tc0 && tc0.Car is Symbol ts0
                    && (ts0.Name == "CALL-TYPED-ENTRY" || ts0.Name == "CALL-TYPED-SYM"))
                    EmitTypedTailInvoke(GetString(Cadr(tc0)), ts0.Name == "CALL-TYPED-SYM");
                else
                {
                    _il.Emit(OpCodes.Tailcall);
                    _il.Emit(OpCodes.Call, _typedSelfMethod!);
                    _il.Emit(OpCodes.Ret);
                }
                dead = true;
                cur = c.Cdr;
                continue;
            }
            if (_tailSites != null)
            {
                if (_tailSites.Contains(c))
                {
                    EmitTailInvoke(instr);
                    dead = true;   // it ends in RET
                    cur = c.Cdr;
                    continue;
                }
                // An older compiler marked the last call itself: the site
                // above emits its own prefix.
                if (instr is Cons pc && pc.Car is Symbol ps && ps.Name == "TAIL-PREFIX"
                    && c.Cdr is Cons nc && _tailSites.Contains(nc))
                {
                    cur = c.Cdr;
                    continue;
                }
            }
            if (_hoistedTypeSites != null && instr is Cons hc
                && _hoistedTypeSites.TryGetValue(hc, out var hoisted))
            {
                _il.Emit(OpCodes.Ldloc, hoisted);
                cur = c.Cdr;
                continue;
            }
            AssembleOne(instr);
            if (instr is Cons fc && fc.Car is Symbol fs && fs.Name == "BEGIN-FINALLY-BLOCK"
                && FinallyRunsLisp(c.Cdr))
                EmitResumeFrames();
            // Check if this instruction makes subsequent code unreachable
            if (instr is Cons tc && tc.Car is Symbol ts)
            {
                var tn = ts.Name;
                if (tn == "LEAVE" || tn == "THROW" || tn == "RET" || tn == "RETHROW")
                    dead = true;
            }
            cur = c.Cdr;
        }
    }

    /// <summary>
    /// Emit (:LDSTR s) (:NEWOBJ "LispString") as a load of one shared object, or
    /// return false if the pair is not that. LDSTR is the only SIL op that leaves
    /// a raw System.String on the stack, so an adjacent NEWOBJ "LispString" can
    /// only be consuming this one.
    /// </summary>
    private bool TryEmitStringLiteral(LispObject instr, LispObject next)
    {
        if (instr is not Cons c || c.Car is not Symbol op || op.Name != "LDSTR") return false;
        if (next is not Cons n || n.Car is not Symbol nop || nop.Name != "NEWOBJ") return false;
        if (GetString(Cadr(n)) != "LispString") return false;
        var value = GetString(Cadr(c));
        if (_faslMode)
        {
            // A FASL cannot reach the process-local constant pool, so the shared
            // object is a static field filled by the type initializer.
            if (_faslStructMap?.UninternedTypeBuilder == null
                || _faslStructMap.SymFnSiteInitIl == null) return false;
            _il.Emit(OpCodes.Ldsfld, _faslStructMap.GetOrCreateStringField(value));
            return true;
        }
        EmitConstantAs(AddConstant(new LispString(value)), typeof(LispString));
        return true;
    }

    private void AssembleOne(LispObject instr)
    {
        if (instr is not Cons c) return;
        var opSym = c.Car as Symbol;
        if (opSym == null) return;
        var op = opSym.Name;

        // What the PREVIOUS instruction left on the stack, when we know it exactly.
        // Only the symbol loaders set it (they call a method whose return type is
        // Symbol), and it survives exactly one instruction: long enough for the
        // CASTCLASS that almost always follows to notice it has nothing to do.
        var prevPushed = _lastPushedType;
        _lastPushedType = null;

        switch (op)
        {
            case "LINE":
                // Debug marker: record a sequence point spanning the source form at
                // the current IL offset. No-op unless collecting (debug build), and
                // deduped so repeated forms at one source position make one point.
                if (_seqPoints != null)
                {
                    var sp = ListToArray(c);
                    int sl = GetInt(sp[1]), sc = GetInt(sp[2]),
                        el = GetInt(sp[3]), ec = GetInt(sp[4]);
                    if ((sl, sc) != _lastSeqPos)
                    {
                        _seqPoints.Add((_il.ILOffset, sl, sc, el, ec));
                        _lastSeqPos = (sl, sc);
                    }
                }
                break;
            case "LOCAL-VAR":
                // Debug marker: name a user lexical local for the PDB. Look up the
                // slot the compiler declared and record (index, source-name) in the
                // innermost open scope (method-wide _localVars if none open).
                if (_localVars != null
                    && _locals.TryGetValue(GetSymbolName(Cadr(c)), out var lvBuilder))
                    (_scopeStack.Count > 0 ? _scopeStack.Peek().vars : _localVars)
                        .Add((lvBuilder.LocalIndex, GetString(Caddr(c))));
                break;
            case "FRAME-ENTER":
                EmitFrameEnter(Cadr(c) is Nil ? null : GetString(Cadr(c)));
                break;
            case "FRAME-SET":
                EmitFrameSet(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "DebugFrame.Set");
                break;
            case "FRAME-SET-BOX":
                EmitFrameSet(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "DebugFrame.SetBox");
                break;
            case "FRAME-SET-LONG":
                EmitFrameSetNative(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "Fixnum.Make", false);
                break;
            case "FRAME-SET-DOUBLE":
                EmitFrameSetNative(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "DoubleFloat", true);
                break;
            case "FRAME-SET-SINGLE":
                EmitFrameSetNative(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "SingleFloat", true);
                break;
            case "FRAME-SET-DECIMAL":
                // Raw System.Decimal slot: box into a LispDecimal on the way into
                // the frame, the same as the native float slots.
                EmitFrameSetNative(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "LispDecimal", true);
                break;
            case "FRAME-SET-CHAR":
                // Raw character-code slot: rebuild the character so the debugger
                // shows a character rather than the code.
                EmitFrameSetNative(GetString(Cadr(c)), GetSymbolName(Caddr(c)), "LispChar.Make", false,
                                   convI4: true);
                break;
            case "SCOPE-BEGIN":
                if (_localVars != null)
                    _scopeStack.Push((_il.ILOffset, new List<(int, string)>()));
                break;
            case "SCOPE-END":
                if (_localVars != null && _scopeStack.Count > 0)
                {
                    var (scStart, scVars) = _scopeStack.Pop();
                    _completedScopes!.Add((scStart, _il.ILOffset - scStart, scVars));
                }
                break;
            case "LDC-I4":
                _il.Emit(OpCodes.Ldc_I4, GetInt(Cadr(c)));
                break;
            case "LDC-I8":
                _il.Emit(OpCodes.Ldc_I8, GetLong(Cadr(c)));
                break;
            case "LDSTR":
                _il.Emit(OpCodes.Ldstr, Track(GetString(Cadr(c))));
                break;
            case "LDSFLD":
                EmitLdsfld(GetString(Cadr(c)));
                break;
            case "LDFLD":
                _il.Emit(OpCodes.Ldfld, ResolveInstanceField(GetString(Cadr(c))));
                break;
            case "STFLD":
                _il.Emit(OpCodes.Stfld, ResolveInstanceField(GetString(Cadr(c))));
                break;
            case "LDNULL":
                _il.Emit(OpCodes.Ldnull);
                break;
            case "POP":
                _il.Emit(OpCodes.Pop);
                break;
            case "DUP":
                _il.Emit(OpCodes.Dup);
                break;
            case "RET":
                if (_selfEnvBody && !_afterTailPrefix)
                {
                    _il.Emit(OpCodes.Ldarg_0);
                    _il.Emit(OpCodes.Call, s_gcKeepAlive);
                }
                EmitLeaveOwnFrame();
                _il.Emit(OpCodes.Ret);
                break;
            case "CALL":
                if (_coverageHit != null) EmitCoverageHit(GetString(Cadr(c)));
                if (!TryEmitLeafCall(GetString(Cadr(c))))
                    EmitCall(GetString(Cadr(c)));
                break;
            case "CALLVIRT":
                if (_coverageHit != null) EmitCoverageHit(GetString(Cadr(c)));
                if (!TryEmitEntryCall(GetString(Cadr(c))))
                    EmitCallvirt(GetString(Cadr(c)));
                break;
            case "DOTNET-CALL-DIRECT":
                EmitDotnetCallDirect(GetString(Cadr(c)), GetString(Caddr(c)));
                break;
            case "DOTNET-CALL-DIRECT-LOCALS":
                EmitDotnetCallDirectLocals(c);
                break;
            case "DOTNET-STATIC-DIRECT-LOCALS":
                EmitDotnetStaticDirectLocals(c);
                break;
            case "CALL-KEY-SHARED":
                // A typed entry of a &key function calling the function's one
                // shared body. The arguments are already on the stack.
                if (_keyShared == null)
                    throw new InvalidOperationException("CALL-KEY-SHARED outside a &key entry");
                _il.Emit(OpCodes.Call, _keyShared);
                break;
            case "CALL-SAME-FILE":
                EmitCallSameFile(c);
                break;
            case "SAME-FILE-IS":
                EmitSameFileIs(c);
                break;
            case "TAIL-SELF":
                // Read by FindTailSites; no code of its own.
                break;
            case "TYPED-TAIL-SELF":
                // Read by FindTypedTailSites; no code of its own.
                break;
            case "TAIL-PREFIX":
                // CIL tail-call prefix. Must be immediately followed by a call/callvirt/calli
                // whose next instruction is `ret`. The compiler no longer emits it (calls in
                // tail position are found by FindTailSites); .sil written by an older one
                // still has it before a function body's final call. A body that
                // unlinks its own frame before RET cannot take it.
                if (_selfFrameLocal == null) { _il.Emit(OpCodes.Tailcall); _afterTailPrefix = true; }
                break;
            case "NEWOBJ":
                EmitNewobj(GetString(Cadr(c)));
                break;
            case "CASTCLASS":
                EmitCastclass(GetString(Cadr(c)), prevPushed);
                break;
            case "ISINST":
                _il.Emit(OpCodes.Isinst, ResolveType(GetString(Cadr(c))));
                break;
            case "DECLARE-LOCAL":
                DeclareLocal(GetSymbolName(Cadr(c)), GetString(Caddr(c)));
                break;
            case "LDLOC":
                _il.EmitLoc(OpCodes.Ldloc, GetLocal(GetSymbolName(Cadr(c))));
                break;
            case "STLOC":
                _il.EmitLoc(OpCodes.Stloc, GetLocal(GetSymbolName(Cadr(c))));
                break;
            case "LDLOCA":
                _il.EmitLoc(OpCodes.Ldloca, GetLocal(GetSymbolName(Cadr(c))));
                break;
            case "LDARG":
                if (_typedForm is { SelfFree: true } tf && GetInt(Cadr(c)) == 0) EmitTypedSelfDepth(tf);
                else EmitLdarg(GetInt(Cadr(c)));
                break;
            case "BR":
                _il.Emit(OpCodes.Br, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BRFALSE":
                _il.Emit(OpCodes.Brfalse, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BRTRUE":
                _il.Emit(OpCodes.Brtrue, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BEQ":
                _il.Emit(OpCodes.Beq, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "CEQ":
                _il.Emit(OpCodes.Ceq);
                break;
            case "BGT":
                _il.Emit(OpCodes.Bgt, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BLT":
                _il.Emit(OpCodes.Blt, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BGE":
                _il.Emit(OpCodes.Bge, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BLE":
                _il.Emit(OpCodes.Ble, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BNE":
                _il.Emit(OpCodes.Bne_Un, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            // Unsigned (integer) / unordered (float) branches and the unsigned
            // arithmetic and narrowing conversions. The compiler does not emit
            // these; they come from C# spliced in by dotnet-cs:inline-cs, whose
            // IL Roslyn writes.
            case "BGE-UN":
                _il.Emit(OpCodes.Bge_Un, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BGT-UN":
                _il.Emit(OpCodes.Bgt_Un, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BLE-UN":
                _il.Emit(OpCodes.Ble_Un, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "BLT-UN":
                _il.Emit(OpCodes.Blt_Un, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "SHR-UN":
                _il.Emit(OpCodes.Shr_Un);
                break;
            case "DIV-UN":
                _il.Emit(OpCodes.Div_Un);
                break;
            case "REM-UN":
                _il.Emit(OpCodes.Rem_Un);
                break;
            case "CONV-I1":
                _il.Emit(OpCodes.Conv_I1);
                break;
            case "CONV-I2":
                _il.Emit(OpCodes.Conv_I2);
                break;
            case "CONV-U1":
                _il.Emit(OpCodes.Conv_U1);
                break;
            case "CONV-U2":
                _il.Emit(OpCodes.Conv_U2);
                break;
            case "CONV-U4":
                _il.Emit(OpCodes.Conv_U4);
                break;
            case "CONV-U8":
                _il.Emit(OpCodes.Conv_U8);
                break;
            case "ADD":
                _il.Emit(OpCodes.Add);
                break;
            case "SUB":
                _il.Emit(OpCodes.Sub);
                break;
            case "MUL":
                _il.Emit(OpCodes.Mul);
                break;
            case "UNBOX-FIXNUM":
                // Expects LispObject on stack; leaves long value.
                // Throws InvalidCastException if not a Fixnum: caller must
                // guarantee this (e.g. via (the fixnum ...) declaration).
                _il.Emit(OpCodes.Castclass, typeof(Fixnum));
                _il.Emit(OpCodes.Call, _methodCache["Fixnum.get_Value"]);
                break;
            case "UNBOX-DOUBLE":
                _il.Emit(OpCodes.Castclass, typeof(DoubleFloat));
                _il.Emit(OpCodes.Call, _methodCache["DoubleFloat.get_Value"]);
                break;
            case "UNBOX-SINGLE":
                _il.Emit(OpCodes.Castclass, typeof(SingleFloat));
                _il.Emit(OpCodes.Call, _methodCache["SingleFloat.get_Value"]);
                break;
            // LispDecimal -> raw System.Decimal, and native decimal ops.
            case "UNBOX-DECIMAL":
                _il.Emit(OpCodes.Castclass, typeof(LispDecimal));
                _il.Emit(OpCodes.Call, _methodCache["LispDecimal.get_Value"]);
                break;
            case "DECIMAL-ADD":
                _il.Emit(OpCodes.Call, _methodCache["Decimal.op_Addition"]);
                break;
            case "DECIMAL-SUB":
                _il.Emit(OpCodes.Call, _methodCache["Decimal.op_Subtraction"]);
                break;
            case "DECIMAL-MUL":
                _il.Emit(OpCodes.Call, _methodCache["Decimal.op_Multiply"]);
                break;
            case "DECIMAL-DIV":
                _il.Emit(OpCodes.Call, _methodCache["Decimal.op_Division"]);
                break;
            case "DECIMAL-NEG":
                _il.Emit(OpCodes.Call, _methodCache["Decimal.op_UnaryNegation"]);
                break;
            case "LDC-R8":
                _il.Emit(OpCodes.Ldc_R8, GetDouble(Cadr(c)));
                break;
            case "LDC-R4":
                _il.Emit(OpCodes.Ldc_R4, (float)GetDouble(Cadr(c)));
                break;
            case "DIV":
                _il.Emit(OpCodes.Div);
                break;
            case "AND":
                _il.Emit(OpCodes.And);
                break;
            case "OR":
                _il.Emit(OpCodes.Or);
                break;
            case "XOR":
                _il.Emit(OpCodes.Xor);
                break;
            case "NOT":
                _il.Emit(OpCodes.Not);
                break;
            case "SHL":
                _il.Emit(OpCodes.Shl);
                break;
            case "SHR":
                _il.Emit(OpCodes.Shr);
                break;
            case "CLT":
                _il.Emit(OpCodes.Clt);
                break;
            case "CGT":
                _il.Emit(OpCodes.Cgt);
                break;
            // The unordered forms, for floats only: true when the comparison
            // holds OR the operands are unordered (either is NaN). <= is emitted
            // as "not greater-unordered" rather than "not greater", so a NaN
            // makes it false instead of true. (On integers these opcodes mean
            // unsigned comparison; nothing emits them for integers.)
            case "CGT-UN":
                _il.Emit(OpCodes.Cgt_Un);
                break;
            case "CLT-UN":
                _il.Emit(OpCodes.Clt_Un);
                break;
            case "LABEL":
                _il.MarkLabel(GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "SWITCH":
                EmitSwitch(Cadr(c));
                break;
            case "NEWARR":
                EmitNewarr(GetString(Cadr(c)));
                break;
            case "CONST-STR-ARRAY":
                EmitConstStringArray(Cadr(c));
                break;
            // Element access straight against a concrete buffer, for a local
            // declared (simple-array <integer type> (*)). The CLR bounds-checks
            // the access itself, so the index only has to be narrowed to i4.
            case "LDELEM-I8":
                _il.Emit(OpCodes.Ldelem_I8);
                break;
            case "LDELEM-I4":
                _il.Emit(OpCodes.Ldelem_I4);
                _il.Emit(OpCodes.Conv_I8);
                break;
            case "LDELEM-U2":
                _il.Emit(OpCodes.Ldelem_U2);
                _il.Emit(OpCodes.Conv_I8);
                break;
            case "LDELEM-U1":
                _il.Emit(OpCodes.Ldelem_U1);
                _il.Emit(OpCodes.Conv_I8);
                break;
            case "STELEM-I8":
                _il.Emit(OpCodes.Stelem_I8);
                break;
            // The same against a double[] / float[] buffer, for a local declared
            // (simple-array double-float (*)) / (simple-array single-float (*)).
            // The value travels as an r8 either way, as it does through the
            // per-element helpers: a float element is widened on the way out
            // and narrowed on the way in.
            case "LDELEM-R8":
                _il.Emit(OpCodes.Ldelem_R8);
                break;
            case "LDELEM-R4":
                _il.Emit(OpCodes.Ldelem_R4);
                _il.Emit(OpCodes.Conv_R8);
                break;
            case "STELEM-R8":
                _il.Emit(OpCodes.Stelem_R8);
                break;
            case "STELEM-R4":
                _il.Emit(OpCodes.Conv_R4);
                _il.Emit(OpCodes.Stelem_R4);
                break;
            case "STELEM-I4":
                _il.Emit(OpCodes.Conv_I4);
                _il.Emit(OpCodes.Stelem_I4);
                break;
            case "STELEM-U2":
                _il.Emit(OpCodes.Conv_U2);
                _il.Emit(OpCodes.Stelem_I2);
                break;
            case "STELEM-U1":
                _il.Emit(OpCodes.Conv_U1);
                _il.Emit(OpCodes.Stelem_I1);
                break;
            case "LDELEM-REF":
                _il.Emit(OpCodes.Ldelem_Ref);
                break;
            case "STELEM-REF":
                _il.Emit(OpCodes.Stelem_Ref);
                break;
            case "LDLEN":
                _il.Emit(OpCodes.Ldlen);
                break;
            case "CONV-I8":
                _il.Emit(OpCodes.Conv_I8);
                break;
            case "REM":
                _il.Emit(OpCodes.Rem);
                break;
            case "NEG":
                _il.Emit(OpCodes.Neg);
                break;
            // Narrow an int64 subscript to a native int for ldelem/stelem. The
            // CLR bounds-checks the native-int value, so an out-of-range
            // subscript is still caught -- conv.i4 would wrap it first.
            case "CONV-I":
                _il.Emit(OpCodes.Conv_I);
                break;
            case "CONV-I4":
                _il.Emit(OpCodes.Conv_I4);
                break;
            case "CONV-R4":
                _il.Emit(OpCodes.Conv_R4);
                break;
            case "CONV-R8":
                _il.Emit(OpCodes.Conv_R8);
                break;
            case "BEGIN-EXCEPTION-BLOCK":
                _il.BeginExceptionBlock();
                break;
            case "BEGIN-CATCH-BLOCK":
                _il.BeginCatchBlock(ResolveType(GetString(Cadr(c))));
                EmitResumeFrames();
                break;
            // Filter-based handler: the filter block runs with the exception on the
            // stack and must leave 1 (handle) or 0 (keep unwinding); the handler
            // block that follows starts with the exception on the stack, as a catch
            // block does. Lets a tag-matching construct decline without catching and
            // rethrowing: see ControlFlowFilters.
            case "BEGIN-FILTER-BLOCK":
                _il.BeginExceptFilterBlock();
                break;
            case "BEGIN-FILTER-HANDLER":
                _il.BeginCatchBlock(null);
                EmitResumeFrames();
                break;
            case "BEGIN-FINALLY-BLOCK":
                _il.BeginFinallyBlock();
                break;
            case "END-EXCEPTION-BLOCK":
                _il.EndExceptionBlock();
                break;
            case "THROW":
                _il.Emit(OpCodes.Throw);
                break;
            case "RETHROW":
                _il.Emit(OpCodes.Rethrow);
                break;
            case "LEAVE":
                _il.Emit(OpCodes.Leave, GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "LOAD-CONST":
                EmitLoadConst(Cadr(c));
                break;
            case "LOAD-CONST-CACHED":
                EmitCachedConst(Cadr(c));
                break;
            case "LOAD-SYM":
            {
                // Resolve at runtime (not assembly time) so later
                // defmethod-direct registrations on the symbol's home
                // package are picked up. Pre-resolving here would pin
                // a Function-less placeholder in the constants pool
                // before the defun has run.
                var symName = GetString(Cadr(c));
                // Resolution is still at run time, through a per-site cell
                // (Startup.SymSite) that remembers a hit Sym has cached. The
                // bare `ldstr; call Startup.Sym` hashed the name on every
                // execution, which a call to a Lisp-defined CL function (LDB,
                // DPB, BYTE-SIZE, ...) pays once per call.
                if (_faslMode)
                {
                    if (_faslStructMap?.SymFnSiteInitIl != null
                        && _faslStructMap.UninternedTypeBuilder != null)
                    {
                        _il.Emit(OpCodes.Ldsfld, _faslStructMap.GetOrCreateSymSiteField(symName));
                        _il.Emit(OpCodes.Call, _symSiteResolve);
                    }
                    else
                    {
                        _il.Emit(OpCodes.Ldstr, Track(symName));
                        _il.Emit(OpCodes.Call, _methodCache["Startup.Sym"]);
                    }
                }
                else
                {
                    EmitConstantAs(AddConstant(new Startup.SymSite(symName)),
                                   typeof(Startup.SymSite));
                    _il.Emit(OpCodes.Call, _symSiteResolve);
                }
                // Startup.Sym returns Symbol, and a Symbol is usable wherever a
                // LispObject is wanted, so the widening cast was never needed;
                // and it forced the narrowing one that usually follows to be a
                // second cast rather than a no-op.
                _lastPushedType = typeof(Symbol);
                break;
            }
            case "LOAD-SYM-FN":
            {
                var symName = GetString(Cadr(c));
                // Optional package name (Caddr c) for home-package-first resolution.
                var rest = Cddr(c);
                var symPkg = rest is Cons r && r.Car is not Nil ? GetString(r.Car) : null;
                if (_faslMode)
                {
                    // The constant pool is not serialized into a .fasl, so the cell
                    // lives in a static field of the generated type instead, filled
                    // by its type initializer.
                    if (_faslStructMap?.SymFnSiteInitIl != null
                        && _faslStructMap.UninternedTypeBuilder != null)
                    {
                        _il.Emit(OpCodes.Ldsfld,
                                 _faslStructMap.GetOrCreateSymFnSiteField(symName, symPkg));
                        _il.Emit(OpCodes.Call, _symFnSiteResolve);
                    }
                    else
                    {
                        // No type to hang a field on (split/inner assemblers):
                        // go through SymFn's own memo table as before.
                        _il.Emit(OpCodes.Ldstr, Track(symName));
                        if (symPkg is null) _il.Emit(OpCodes.Ldnull);
                        else _il.Emit(OpCodes.Ldstr, Track(symPkg));
                        _il.Emit(OpCodes.Call, _methodCache["Startup.SymFn(string, string)"]);
                    }
                }
                else
                {
                    // JIT mode: one rooted cache cell per call site, filled on first
                    // execution. Same pattern as READER-IC / WRITER-IC. Takes the
                    // string-keyed memo lookup out of every function call.
                    int idx = AddConstant(new Startup.SymFnSite(symName, symPkg));
                    _il.Emit(OpCodes.Ldc_I4, idx);
                    _il.Emit(OpCodes.Call, _fnSiteSymbol);
                }
                // Both arms leave a Symbol (SymFnSite.Resolve and Startup.SymFn
                // both return one), so the widening cast was dead: and this is
                // the call-site path, so it was dead on every named call.
                _lastPushedType = typeof(Symbol);
                break;
            }
            case "LOAD-SYM-KEYWORD":
            {
                EmitKeyword(GetString(Cadr(c)));
                _lastPushedType = typeof(Symbol);
                break;
            }
            case "LOAD-SYM-PKG":
            {
                var spName = GetString(Cadr(c));
                var spPkg = GetString(Caddr(c));
                if (_faslMode) _faslStructMap?.RecordSymbolReference(spName, spPkg);
                // The symbol is interned and permanent: resolve it once per site
                // instead of running a string-keyed package lookup on every
                // execution (a special-variable read in a loop paid one per
                // iteration -- 10.6% of a call-heavy profile).
                EmitInternedSymbol(spName, spPkg);
                _lastPushedType = typeof(Symbol);
                break;
            }
            case "READER-IC":
            case "SLOT-VALUE-IC":
            {
                // (reader obj) inline-cached simple slot read. OBJ is
                // already on the stack (LispObject). Call Runtime.ReaderIC(obj, cell),
                // where `cell` is a per-call-site monomorphic inline cache.
                // SLOT-VALUE-IC is (slot-value obj 'name), the cell holding NAME.
                var ricName = GetString(Cadr(c));
                var ricPkg = GetString(Caddr(c));
                var ricMethod = _methodCache[op == "READER-IC" ? "Runtime.ReaderIC" : "Runtime.SlotValueIC"];
                if (_faslMode)
                {
                    // The global constant pool is not serialized into a .fasl, so the
                    // cell lives in a static field of its own for this site (see
                    // EmitFaslInlineCacheCell), or, with no type to hang one on, a
                    // fresh cell per call (correct: it just always misses and
                    // re-resolves the accessor, i.e. no worse than a normal GF call).
                    EmitFaslInlineCacheCell(ricName, ricPkg, typeof(ReaderCache));
                    _il.Emit(OpCodes.Call, ricMethod);
                }
                else
                {
                    // JIT mode: one rooted cell in the constant pool per call site, so
                    // the monomorphic (class -> slot-index) fill persists across calls.
                    var cell = new ReaderCache(Startup.SymInPkg(ricName, ricPkg));
                    EmitConstantAs(AddConstant(cell), typeof(ReaderCache));
                    _il.Emit(OpCodes.Call, ricMethod);
                }
                break;
            }
            case "WRITER-IC":
            {
                // (setf (accessor obj) newval) inline-cached simple slot write. NEWVAL and
                // OBJ are already on the stack in that order (the (SETF name) GF's own
                // argument order). Call Runtime.WriterIC(newval, obj, cell). Cell handling
                // matches READER-IC: a rooted constant-pool cell in JIT mode, a static
                // field per site in FASL mode.
                var wicName = GetString(Cadr(c));
                var wicPkg = GetString(Caddr(c));
                var wicMethod = _methodCache["Runtime.WriterIC"];
                if (_faslMode)
                {
                    EmitFaslInlineCacheCell(wicName, wicPkg, typeof(WriterCache));
                    _il.Emit(OpCodes.Call, wicMethod);
                }
                else
                {
                    var cell = new WriterCache(Startup.SymInPkg(wicName, wicPkg));
                    EmitConstantAs(AddConstant(cell), typeof(WriterCache));
                    _il.Emit(OpCodes.Call, wicMethod);
                }
                break;
            }
            case "GF-IC1":
                EmitGFCallIC1(c);
                break;
            case "LOAD-ENV":
                EmitLoadEnv(GetInt(Cadr(c)));
                break;
            case "LOAD-ARG":
                if (_directParams)
                {
                    // Direct-params closure body: param i is a real CLR arg at
                    // slot i+1 (slot 0 is the env, or the function: see
                    // EmitSelfOrEnvPrologue).
                    EmitLdarg(GetInt(Cadr(c)) + 1);
                }
                else
                {
                    // Load args[i]: args is arg 1 (LispObject[]) for closures
                    _il.Emit(OpCodes.Ldarg_1);
                    _il.Emit(OpCodes.Ldc_I4, GetInt(Cadr(c)));
                    _il.Emit(OpCodes.Ldelem_Ref);
                }
                break;

            // Composite instructions
            case "DEFMETHOD":
                HandleDefmethod(c);
                break;
            case "DEFMETHOD-DIRECT":
                HandleDefmethodDirect(c);
                break;
            case "DEFMETHOD-NATIVE":
                HandleDefmethodNative(c);
                break;
            case "DEFMETHOD-TYPED":
                HandleDefmethodTyped(c);
                break;
            case "CALL-TYPED-SELF":
                // A non-tail self-call in a typed entry's body: the receiver and
                // the raw arguments are on the stack; the method being assembled
                // is the callee.
                if (_typedSelfMethod == null) throw new Exception("CALL-TYPED-SELF outside a typed entry body");
                if (!_typedForm!.SelfFree) EmitTypedSelfCount(_typedForm);
                _il.Emit(OpCodes.Call, _typedSelfMethod);
                break;
            case "CALL-TYPED-ENTRY":
            {
                // (:call-typed-entry "LD>L"): a call to a global function through
                // its typed entry. The LispFunction and the raw arguments are on
                // the stack; the raw result is left.
                var sig = GetString(Cadr(c));
                _il.Emit(OpCodes.Ldc_I4, TypedEntrySig.Code(sig));
                _il.Emit(OpCodes.Call, LispFunction.TypedCallMethod(sig));
                break;
            }
            case "CALL-TYPED-SYM":
            {
                // (:call-typed-sym "LD>L"): as CALL-TYPED-ENTRY, with the
                // function's Symbol on the stack in place of the function.
                var sig = GetString(Cadr(c));
                _il.Emit(OpCodes.Ldc_I4, TypedEntrySig.Code(sig));
                _il.Emit(OpCodes.Call, LispFunction.TypedSymCallMethod(sig));
                break;
            }
            case "MAKE-FUNCTION":
                HandleMakeFunction(c);
                break;
            case "MAKE-FUNCTION-DIRECT":
                HandleMakeFunctionDirect(c);
                break;
            case "MAKE-CLOSURE":
                HandleMakeClosure(c);
                break;
            case "ONCE":
                HandleOnce(c);
                break;

            default:
                throw new Exception($"CilAssembler: unknown opcode {op}");
        }
    }

    // --- Label / local management ---

    private Label GetOrDefineLabel(string name)
    {
        if (_labels.TryGetValue(name, out var label)) return label;
        label = _il.DefineLabel();
        _labels[name] = label;
        return label;
    }

    // Frame-locals mode: the DebugFrame this body opened at entry. One
    // CilAssembler instance emits one method (every body path constructs a fresh
    // one), so this slot belongs to that method. Null when the compiler emitted no
    // (:frame-enter), frame-locals off, or a context with no body prologue such
    // as a top-level form, in which case (:frame-set ...) is a no-op rather than
    // IL referring to a slot that does not exist.
    private LocalBuilder? _dbgFrame;

    // OWNNAME is the body's own runtime function name, or null for a lambda /
    // closure body. DebugFrames.Enter compares it with the innermost call-stack
    // frame to tell whether this body has a frame of its own.
    private void EmitFrameEnter(string? ownName)
    {
        if (_dbgFrame != null) return;
        _dbgFrame = _il.DeclareLocal(typeof(DebugFrame));
        if (ownName == null) _il.Emit(OpCodes.Ldnull);
        else _il.Emit(OpCodes.Ldstr, Track(ownName));
        EmitCall("DebugFrames.Enter");
        _il.EmitLoc(OpCodes.Stloc, _dbgFrame);
    }

    private void EmitFrameSet(string sourceName, string localName, string method)
    {
        if (_dbgFrame == null || !_locals.TryGetValue(localName, out var slot)) return;
        _il.EmitLoc(OpCodes.Ldloc, _dbgFrame);
        _il.Emit(OpCodes.Ldstr, Track(sourceName));
        _il.EmitLoc(OpCodes.Ldloc, slot);
        EmitCall(method);
    }

    /// A native-rep slot (Int64 / Double / Single) holds a raw value, so it is boxed
    /// into the matching Lisp object on the way into the debug frame: the frame
    /// hands out LispObjects. Debug-only cost (one box per store).
    private void EmitFrameSetNative(string sourceName, string localName, string conv, bool newobj,
                                    bool convI4 = false)
    {
        if (_dbgFrame == null || !_locals.TryGetValue(localName, out var slot)) return;
        _il.EmitLoc(OpCodes.Ldloc, _dbgFrame);
        _il.Emit(OpCodes.Ldstr, Track(sourceName));
        _il.EmitLoc(OpCodes.Ldloc, slot);
        // A character-code slot is an int64 (the width the comparison path
        // wants) and LispChar.Make takes a char, so it narrows first.
        if (convI4) _il.Emit(OpCodes.Conv_I4);
        if (newobj) EmitNewobj(conv); else EmitCall(conv);
        EmitCall("DebugFrame.Set");
    }

    private LocalBuilder GetLocal(string name)
    {
        if (_locals.TryGetValue(name, out var local)) return local;
        throw new Exception($"Undeclared local: {name}");
    }

    private void DeclareLocal(string name, string typeName)
    {
        if (_locals.ContainsKey(name)) return; // already declared
        var t = ResolveType(typeName);
        if (TryDeclareSharedLocal(name, t)) return;
        var local = _il.DeclareLocal(t);
        _locals[name] = local;
    }

    // --- Type resolution ---

    private static Type ResolveType(string name)
    {
        if (_typeCache.TryGetValue(name, out var t)) return t;
        if (name.StartsWith("V128.", StringComparison.Ordinal) && Tables.TrySimdType(name, out t))
            return t!;
        throw new Exception($"Unknown type: {name}");
    }

    // --- Lisp list helpers (internal for FaslAssembler reuse) ---

    internal static LispObject Car(LispObject obj) =>
        obj is Cons c ? c.Car : throw new Exception("CAR of non-cons");
    internal static LispObject Cdr(LispObject obj) =>
        obj is Cons c ? c.Cdr : Nil.Instance;
    internal static LispObject Cadr(LispObject obj) => Car(Cdr(obj));
    internal static LispObject Caddr(LispObject obj) => Car(Cdr(Cdr(obj)));
    internal static LispObject Cddr(LispObject obj) => Cdr(Cdr(obj));

    internal static string GetString(LispObject obj)
    {
        return obj switch
        {
            LispString s => s.Value,
            Symbol sym => sym.Name,
            // char-vectors embedded in forms (eval context): extract char content
            LispVector v when v.IsCharVector => v.ToCharString(),
            _ => obj.ToString()
        };
    }

    internal static string GetSymbolName(LispObject obj)
    {
        return obj switch
        {
            Symbol sym => sym.Name,
            LispString s => s.Value,
            _ => obj.ToString()
        };
    }

    internal static int GetInt(LispObject obj)
    {
        return obj switch
        {
            Fixnum f => (int)f.Value,
            _ => throw new Exception($"Expected integer, got {obj}")
        };
    }

    internal static long GetLong(LispObject obj)
    {
        return obj switch
        {
            Fixnum f => f.Value,
            _ => throw new Exception($"Expected integer, got {obj}")
        };
    }

    internal static double GetDouble(LispObject obj)
    {
        return obj switch
        {
            DoubleFloat df => df.Value,
            SingleFloat sf => sf.Value,
            Fixnum f => (double)f.Value,
            _ => throw new Exception($"Expected number, got {obj}")
        };
    }

    private static LispObject[] ListToArray(LispObject list)
    {
        var result = new List<LispObject>();
        var current = list;
        while (current is Cons c)
        {
            result.Add(c.Car);
            current = c.Cdr;
        }
        return result.ToArray();
    }
}
