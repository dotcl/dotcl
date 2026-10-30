using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Eval ---

    /// <summary>
    /// Process-wide lock that serializes the compile/run path inside Eval.
    /// dotcl's mutable runtime state (*macros* table, CLOS dispatch, cons
    /// internals, hashtable contents, compiler state) is not yet fully
    /// race-free, so we make Eval implicitly single-threaded at the
    /// runtime level. Hosts (DotclHost callers, McpServer, ASP.NET
    /// controllers) do not need to add their own _evalLock.
    /// `lock` is reentrant on the same thread, so Lisp -> C# -> Lisp
    /// callbacks (e.g. dotnet:funcall, condition handlers) work fine.
    /// Removing this serialization is future work; the preparation to make
    /// individual operations race-free is already in place.
    /// </summary>
    internal static readonly object _evalLock = new();

    /// <summary>
    /// When true (default), Eval serializes the compound-form compile/run path on
    /// <see cref="_evalLock"/>. Opt out (DOTCL_PARALLEL_EVAL=1, or
    /// dotcl:set-parallel-eval) to run eval concurrently: the runtime state the
    /// compile/run path touches is individually race-safe (Symbol/Package/Modules
    /// concurrent-safe; CLOS method lists copy-on-write; constant pool + function
    /// registry locked; *macros* a synchronized table), so many threads can
    /// compile/call without the global lock. Proving full race-freedom is a
    /// non-goal: user-level cons/array mutation stays the caller's responsibility,
    /// as in any CL. Intended for hosts that call compiled functions from many
    /// threads (e.g. a probe/REPL attached to a multi-threaded .NET service).
    /// </summary>
    internal static bool SerializeEval =
        System.Environment.GetEnvironmentVariable("DOTCL_PARALLEL_EVAL")
            is not ("1" or "true" or "TRUE" or "yes");

    public static LispObject Eval(LispObject form)
    {
        if (!Compat.TryEnsureSufficientExecutionStackWithMargin())
            throw new LispErrorException(new LispStorageCondition("Stack overflow in eval"));
        // Fast path for self-evaluating forms: skip full compilation AND
        // skip the eval lock. These return paths touch no shared mutable
        // runtime state (Number/LispChar/LispString/T/Nil are immutable;
        // Symbol value read via per-thread DynamicBindings is already
        // thread-safe). Avoiding the lock here keeps tight inner loops
        // (e.g. SBCL's define-type-vop using #'eval as a reduce key) cheap.
        // Every atom other than a symbol evaluates to itself (CLHS 3.1.2.1.3):
        // pathnames, vectors, structures, instances and so on, not only the
        // common literal types. Compiling one of those produced a one-shot
        // method whose whole job was to return the literal, and asdf's
        // central-registry search evaluates every registry entry (usually a
        // pathname) on every system lookup: hundreds per quickload.
        if (form is not Cons && form is not Symbol)
            return form;
        // Read via DynamicBindings so the per-thread dynamic binding (Phase B)
        // is honored. Reading sym.Value directly would miss a thread-local
        // binding (e.g. (let ((*x* ...)) (eval '*x*))).
        if (form is Symbol sym2 && DynamicBindings.TryGet(sym2, out var sym2val))
            return sym2val;
#if DOTCL_EMIT
        // A top level PROGN is split into its subforms (see EvalProgn), and the
        // split has to happen here, before _evalLock is taken. Each subform is
        // an Eval of its own, which takes the lock for its compilation and lets
        // go of it while the compiled code runs. Done inside the lock, the outer
        // hold stayed while every subform ran: code run from a PROGN kept other
        // threads out of EVAL for as long as it ran, and a REPL started from a
        // PROGN kept them out for the whole session.
        if (form is Cons pc && pc.Car is Symbol ps && ps.Name == "PROGN" && !UseInterpreter())
            return EvalProgn(pc);
#endif
        // Compound forms hit the compiler + assembler, both of which mutate
        // shared runtime state. Serialize from here unless the host opted into
        // parallel eval (see SerializeEval).
        if (SerializeEval)
            lock (_evalLock)
                return EvalCompound(form);
        return EvalCompound(form);
    }

#if DOTCL_EMIT
    // (progn a b) evaluates a and THEN b. Handing the whole progn to the
    // compiler compiles b before a has run, so anything a installs at run
    // time that b's compilation needs, a setf expander from DEFSETF, a
    // symbol macro from DEFINE-SYMBOL-MACRO, is not there yet, and b
    // compiles against the wrong (or no) definition. Evaluating the
    // subforms one at a time is what the tree-walk interpreter already
    // does, and what the same expression does when it comes from a file
    // (COMPILE-FILE splits a top level PROGN per CLHS 3.2.3.1).
    // Matched by name, like every other special-operator check in this
    // file and in the compiler's dispatcher: dotcl resolves operators by
    // name throughout, so a stricter test here would make EVAL disagree
    // with what compiling the same form does.
    private static LispObject EvalProgn(Cons pc)
    {
        // PROGN returns the values of its LAST form and discards the rest.
        // Discarding has to include the thread value state: a subform that
        // published NO values -- a :VOID foreign call, a (VALUES) -- left
        // COUNT at 0, and a last form that publishes nothing of its own (a
        // constant, a variable reference) then inherited that count, so
        // (progn (values) x) answered no values instead of X. The sentinel a
        // reset leaves means "one value, the one returned", which is exactly
        // what such a form produces. The reset before the loop covers the
        // empty body, whose value is one NIL.
        LispObject last = Nil.Instance;
        MultipleValues.Reset();
        for (var body = pc.Cdr; body is Cons bc; body = bc.Cdr)
        {
            last = Eval(bc.Car);
            if (bc.Cdr is Cons) MultipleValues.Reset();
        }
        return last;   // multiple values of the last form ride along
    }
#endif

    private static LispObject EvalCompound(LispObject form)
    {
        // An unmatched THROW must leave EVAL as a CONTROL-ERROR (ansi-test
        // THROW-ERROR: (signals-error (throw (gensym) nil) control-error)), and
        // that conversion has to happen on BOTH evaluator paths. It used to wrap
        // the compiled branch only, so under the interpreter the raw .NET
        // CatchThrowException escaped all the way to Program.Main and killed the
        // process: the ansi-test run died there rather than reporting a failure.
        try
        {
#if DOTCL_EMIT
            // :interpret mode routes compound forms through the emit-free tree-walk
            // interpreter; :compile (default) uses the CIL compiler below.
            if (UseInterpreter())
                return MiniEval(form);
            // Use eval-specific compile path that preserves MvReturn at tail
            // so callers can observe multiple values from form.
            var instrList = CompileTopLevelEval(form);
            return DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
#else
            // No Reflection.Emit in this build: the tree-walk interpreter is the
            // only way to evaluate a compound form.
            return MiniEval(form);
#endif
        }
        catch (CatchThrowException cte)
        {
            // If a matching (catch tag ...) exists outside this eval, let the
            // exception propagate so the outer catch can handle it; otherwise it is
            // the CLHS 5.2 control-error. Shared with LOAD's boundary so the two
            // cannot answer differently (UnmatchedThrowError).
            var unmatched = Runtime.UnmatchedThrowError(cte);
            if (unmatched == null) throw;
            throw unmatched;
        }
    }

    /// <summary>True when eval should use the tree-walk interpreter
    /// (dotcl:*evaluator-mode* is :interpret). Only consulted on emit builds;
    /// emit-free builds always interpret.</summary>
    private static bool UseInterpreter()
    {
        var sym = Startup.SymInPkg("*EVALUATOR-MODE*", "DOTCL");
        return DynamicBindings.Get(sym) is Symbol s && s.Name == "INTERPRET";
    }

    /// <summary>Invoke the Lisp tree-walk interpreter %mini-eval on FORM with an
    /// empty lexical environment. %mini-eval is defined in the compiler sources
    /// (package DOTCL.CIL-COMPILER) and is present in the loaded core.</summary>
    private static LispObject MiniEval(LispObject form) => MiniEval(form, Nil.Instance);

    /// <summary>%MINI-EVAL of FORM in the interpreter environment ENV.</summary>
    private static LispObject MiniEval(LispObject form, LispObject env)
    {
        var fn = Startup.SymInPkg("%MINI-EVAL", "DOTCL.CIL-COMPILER").Function as LispFunction
            ?? throw new LispErrorException(new LispProgramError(
                "tree-walk interpreter (%mini-eval) is unavailable: the core is not loaded"));
        // _evalLock serializes COMPILATION, and must not be held while a form
        // RUNS: a form that blocks waiting on other threads which themselves need
        // to compile deadlocks. The compiled path releases it around the
        // run inside AssembleAndRunSingle: but the interpreter never goes there,
        // so on this path the lock stayed held for the whole evaluation and that
        // same deadlock came back whenever eval interprets. Interpreting IS
        // running (no compiler or assembler is involved), so release for the whole
        // call, same shape as AssembleAndRunSingle and keeping the outer `lock`
        // balanced.
        bool releasedEvalLock = SerializeEval
                                && System.Threading.Monitor.IsEntered(_evalLock);
        if (releasedEvalLock) System.Threading.Monitor.Exit(_evalLock);
        // EVAL runs FORM in the null lexical environment, but the interpreter's
        // SYMBOL-MACROLET carries its bindings in the special *SYMBOL-MACROS*.
        // An EVAL reached from inside such a body must not see them, so the
        // special is rebound to NIL for the call when it holds anything.
        var smSym = Startup.Sym("*SYMBOL-MACROS*");
        bool boundSm = DynamicBindings.TryGet(smSym, out var smVal) && smVal is not Nil;
        if (boundSm) DynamicBindings.Push(smSym, Nil.Instance);
        try { return fn.Invoke(form, env); }
        finally
        {
            if (boundSm) DynamicBindings.Pop(smSym);
            if (releasedEvalLock) System.Threading.Monitor.Enter(_evalLock);
        }
    }

    /// <summary>
    /// EVAL of a top level form the compiler evaluates for its compile-time
    /// effect (a DEFMACRO's expander, the early definition of a DEFUN). Inside a
    /// top level MACROLET or SYMBOL-MACROLET the form's body may use their local
    /// macros. The compiler sees them while it compiles the body, so compiled
    /// code has them expanded; the tree-walk interpreter expands a function body
    /// when it runs, after they are gone. Under :INTERPRET the form is therefore
    /// interpreted in an environment that holds them.
    /// </summary>
    internal static LispObject EvalAtCompileTime(LispObject form)
    {
#if DOTCL_EMIT
        if (UseInterpreter()
            && DynamicBindings.Get(Startup.Sym("*MACROEXPAND-SCOPE*")) is Cons
            && Startup.SymInPkg("%COMPILE-SCOPE-MINI-ENV", "DOTCL.CIL-COMPILER").Function
                   is LispFunction envFn)
            return MiniEval(form, envFn.Invoke());
#endif
        return Eval(form);
    }

    /// <summary>
    /// Best-effort eval: returns T on success, NIL on error.
    /// Used by compile-eval-when to eval defvar/defmacro at compile time
    /// without crashing if init-forms reference not-yet-defined functions.
    /// Bind *compile-file-mode* to NIL during the inner eval so that the
    /// recursive compile-form for FORM does not retrigger the defun
    /// handler's compile-time eval branch: that would recurse on the
    /// same defun until the depth guard fires.
    /// </summary>
    public static LispObject TryEval(LispObject form)
    {
        var cfmSym = Startup.Sym("*COMPILE-FILE-MODE*");
        DynamicBindings.Push(cfmSym, Nil.Instance);
        var early = EarlyDefunTarget(form);
        try
        {
            if (DeferredDefun.TryInstall(form)) return T.Instance;
            EvalAtCompileTime(form);
            return T.Instance;
        }
        catch
        {
            return Nil.Instance;
        }
        finally
        {
            DynamicBindings.Pop(cfmSym);
            if (early.Sym != null) NoteEarlyDefun(early.Sym, early.Setf, early.Old);
        }
    }

    // The early definition of a top level DEFUN during COMPILE-FILE (see
    // TryEval) is only there for a macro later in the same file that calls the
    // function while it expands. Most early definitions are never called, yet
    // compiling each one cost as much as compiling the DEFUN for the fasl
    // itself: a quarter of a cold quickload of serapeum went to them. So a name that is
    // not defined yet gets a stand-in that compiles the DEFUN the first time it
    // is called. Anything unusual (a name already fbound, a macro, a locked
    // package, an uninterned or malformed name) is evaluated right away as
    // before.
    private sealed class DeferredDefun
    {
        private readonly LispObject _form;
        private readonly Symbol _sym;
        private readonly bool _setf;
        private LispFunction? _real;
        private bool _compiling;
        private LispFunction _stub = null!;

        private DeferredDefun(LispObject form, Symbol sym, bool setf)
        {
            _form = form; _sym = sym; _setf = setf;
        }

        internal static bool TryInstall(LispObject form)
        {
            if (!CompileFileInProgress) return false;
            // Inside a top level MACROLET or SYMBOL-MACROLET the body is evaluated
            // now, as before deferral existed: its local macros are only in
            // effect while that form is being compiled, and a later evaluation
            // would expand the body without them.
            if (DynamicBindings.Get(Startup.Sym("*MACROEXPAND-SCOPE*")) is Cons) return false;
            if (form is not Cons c || c.Car is not Symbol op || op.Name != "DEFUN"
                || c.Cdr is not Cons nc || nc.Cdr is not Cons)
                return false;
            Symbol sym;
            bool setf;
            if (nc.Car is Symbol s) { sym = s; setf = false; }
            else if (nc.Car is Cons sc && sc.Car is Symbol ss && ss.Name == "SETF"
                     && sc.Cdr is Cons sc2 && sc2.Car is Symbol sn && sc2.Cdr is Nil)
            { sym = sn; setf = true; }
            else return false;
            if (sym.HomePackage == null || sym.HomePackage.IsLocked) return false;
            if ((setf ? sym.SetfFunction : sym.Function) != null) return false;
            if (!setf && MacroFunction(sym) is not Nil) return false;
            var d = new DeferredDefun(form, sym, setf);
            string name = setf ? "(SETF " + sym.Name + ")" : sym.Name;
            d._stub = new LispFunction(d.Call, name, -1);
            if (setf) sym.SetfFunction = d._stub;
            else sym.Function = d._stub;
            if (s_pendingDeferredTS is { Count: > 0 } pending) pending.Peek().Add(d);
            return true;
        }

        /// <summary>A macro named SYM is being defined again while stand-ins are still
        /// waiting for their first call. A stand-in whose DEFUN mentions SYM is
        /// evaluated now, so its body expands with the definitions in effect where
        /// the DEFUN was (SYM as it was before, or as a function call when it was
        /// not a macro yet), as the immediate evaluation used to.</summary>
        internal static void ForcePendingMentioning(Symbol sym)
        {
            if (s_pendingDeferredTS is not { Count: > 0 } stack) return;
            var list = stack.Peek();
            if (list.Count == 0) return;
            for (int i = list.Count - 1; i >= 0; i--)
            {
                var d = list[i];
                if (d._real != null) { list.RemoveAt(i); continue; }
                if (!FormMentions(d._form, sym)) continue;
                list.RemoveAt(i);
                try { d.Force(); } catch { /* the first call reports it, as before */ }
            }
        }

        private static bool FormMentions(LispObject form, Symbol sym)
        {
            var seen = new HashSet<Cons>(ReferenceEqualityComparer.Instance);
            var todo = new Stack<LispObject>();
            todo.Push(form);
            while (todo.Count > 0)
            {
                var x = todo.Pop();
                while (x is Cons c)
                {
                    if (!seen.Add(c)) break;
                    if (ReferenceEquals(c.Car, sym)) return true;
                    if (c.Car is Cons) todo.Push(c.Car);
                    x = c.Cdr;
                }
                if (ReferenceEquals(x, sym)) return true;
            }
            return false;
        }

        private LispObject Call(LispObject[] args) => (_real ?? Force()).Invoke(args);

        private LispFunction Force()
        {
            lock (this)
            {
                if (_real != null) return _real;
                // The DEFUN's own compilation called it (a macro in its body):
                // the early evaluation found the function undefined there too.
                if (_compiling)
                    return Runtime.UndefinedFunctionError(_sym);
                _compiling = true;
                // Whatever the name holds now (this stand-in, or by then the
                // loaded fasl's definition) is what it holds afterwards: the
                // evaluation only supplies the function.
                var before = _setf ? _sym.SetfFunction : _sym.Function;
                var cfmSym = Startup.Sym("*COMPILE-FILE-MODE*");
                DynamicBindings.Push(cfmSym, Nil.Instance);
                try
                {
                    Eval(_form);
                    var made = _setf ? _sym.SetfFunction : _sym.Function;
                    if (made is LispFunction f && !ReferenceEquals(made, before)
                        && !ReferenceEquals(made, _stub))
                        _real = f;
                }
                finally
                {
                    DynamicBindings.Pop(cfmSym);
                    if (_setf) _sym.SetfFunction = before;
                    else _sym.Function = before;
                    _compiling = false;
                }
                return _real ?? Runtime.UndefinedFunctionError(_sym);
            }
        }
    }

    // The early evaluation of a top level DEFUN during COMPILE-FILE (the
    // compiler's try-eval, so that macros later in the same file can call the
    // function) must not outlive the compilation: CLHS 3.2.3.1 gives a plain
    // DEFUN no compile-time effect. A name that was not fbound before the
    // compilation is taken care of by the strip at the end of COMPILE-FILE. A
    // name that already had a definition kept the new one, so compiling a file
    // replaced a live function the program was still using. That is recorded
    // here and put back at the end of COMPILE-FILE, unless something else has
    // replaced the definition meanwhile (an explicit :COMPILE-TOPLEVEL
    // evaluation, a nested LOAD), which is a real definition and stays.
    private static (Symbol? Sym, bool Setf, LispObject? Old) EarlyDefunTarget(LispObject form)
    {
        var stack = s_earlyDefunsTS;
        if (stack == null || stack.Count == 0 || s_compileFilePreSetsTS is not { Count: > 0 } pre)
            return (null, false, null);
        if (form is not Cons c || c.Car is not Symbol op || op.Name != "DEFUN"
            || c.Cdr is not Cons nc)
            return (null, false, null);
        var defs = pre.Peek();
        if (nc.Car is Symbol name)
            return defs.KeepsFn(name) ? (name, false, name.Function) : (null, false, null);
        if (nc.Car is Cons sc && sc.Car is Symbol setfSym && setfSym.Name == "SETF"
            && sc.Cdr is Cons sc2 && sc2.Car is Symbol sname)
            return defs.KeepsSetf(sname) ? (sname, true, sname.SetfFunction) : (null, false, null);
        return (null, false, null);
    }

    private static void NoteEarlyDefun(Symbol sym, bool setf, LispObject? old)
    {
        var stack = s_earlyDefunsTS;
        if (stack == null || stack.Count == 0) return;
        var d = stack.Peek();
        LispObject? now = setf ? sym.SetfFunction : sym.Function;
        if (ReferenceEquals(now, old)) return;
        var key = (sym, setf);
        // The first early DEFUN of a name saw the definition to go back to.
        d[key] = d.TryGetValue(key, out var e) ? (e.Old, now) : (old, now);
    }

    [ThreadStatic]
    private static Stack<Dictionary<(Symbol, bool), (LispObject? Old, LispObject? New)>>? s_earlyDefunsTS;

    // The DEFUN stand-ins of each COMPILE-FILE in progress that have not been
    // called yet (see DeferredDefun.ForcePendingMentioning).
    [ThreadStatic]
    private static Stack<List<DeferredDefun>>? s_pendingDeferredTS;

    private static void RestoreEarlyDefuns(Dictionary<(Symbol, bool), (LispObject? Old, LispObject? New)> d)
    {
        foreach (var ((sym, setf), (old, @new)) in d)
        {
            if (setf)
            {
                if (ReferenceEquals(sym.SetfFunction, @new)) sym.SetfFunction = old;
            }
            else if (ReferenceEquals(sym.Function, @new))
                sym.Function = old;
        }
    }

    // Cache for IsShadowEntry (RegisterMiscBuiltins): the compiler's
    // *SYMBOL-MACRO-SHADOW-MARKER* object, looked up on first use.
    static LispObject? _symbolMacroShadowMarker;

    // The expansions %MINI-MACROEXPAND-1-CACHED remembers, keyed by the form's
    // cons. Weak: an entry goes away with the form. An entry is replaced as a
    // whole, so a reader on another thread sees an old one or a new one.
    sealed class MiniExpansion
    {
        public readonly MiniExpansionTokens Tokens;
        public readonly bool EnvFree;
        public readonly LispObject Expansion;
        public MiniExpansion(MiniExpansionTokens tokens, bool envFree, LispObject expansion)
        { Tokens = tokens; EnvFree = envFree; Expansion = expansion; }
    }
    sealed class MiniExpansionBox { public volatile MiniExpansion? Entry; }

    // What a remembered expansion of a form with operator SYM was made from.
    readonly struct MiniExpansionTokens
    {
        public readonly int Epoch;
        public readonly long Tables;
        public readonly LispObject? FunctionBinding;
        public readonly LispHashTable? GsmTable;
        public readonly int GsmVersion;
        public bool Usable => Tables != -1;
        // The compiler's table of global symbol macros has an entry: an expansion
        // may then depend on the lexical variables in scope.
        public bool GlobalSymbolMacros => GsmTable != null && GsmTable.Count > 0;
        MiniExpansionTokens(int epoch, long tables, LispObject? fn, LispHashTable? gsm, int gsmVersion)
        { Epoch = epoch; Tables = tables; FunctionBinding = fn; GsmTable = gsm; GsmVersion = gsmVersion; }
        public static MiniExpansionTokens Current(Symbol sym)
        {
            var gsm = GlobalSymbolMacroTable();
            return new MiniExpansionTokens(
                System.Threading.Volatile.Read(ref MacroEpoch), Startup.MacroTablesVersion(),
                sym.Function, gsm, gsm?.Version ?? 0);
        }
        public bool SameAs(in MiniExpansionTokens o)
            => Epoch == o.Epoch && Tables == o.Tables
               && ReferenceEquals(FunctionBinding, o.FunctionBinding)
               && ReferenceEquals(GsmTable, o.GsmTable) && GsmVersion == o.GsmVersion;
    }

    static readonly LispObject MiniExpansionMiss = new Cons(Nil.Instance, Nil.Instance);

    // The compiler's *GLOBAL-SYMBOL-MACROS* table (DEFINE-SYMBOL-MACRO as the
    // compiler records it), or null before the compiler is loaded.
    static Symbol? _gsmTableSym;
    static LispHashTable? GlobalSymbolMacroTable()
    {
        var sym = _gsmTableSym;
        if (sym == null)
        {
            var pkg = Package.FindPackage("DOTCL-INTERNAL");
            if (pkg == null) return null;
            var (found, status) = pkg.FindSymbol("*GLOBAL-SYMBOL-MACROS*");
            if (found == null || status == SymbolStatus.None) return null;
            _gsmTableSym = sym = found;
        }
        return sym.Value as LispHashTable;
    }

    // True when TREE contains a symbol naming a global symbol macro, in the
    // compiler's table or the runtime's. A tree too large to walk (or circular)
    // counts as mentioning one.
    static bool MentionsGlobalSymbolMacro(LispObject tree, LispHashTable? gsm)
    {
        var names = gsm == null ? null : GsmNameSet(gsm);
        var stack = _mentionsStack ??= new Stack<LispObject>();
        stack.Clear();
        stack.Push(tree);
        int budget = 5000;
        while (stack.Count > 0)
        {
            if (--budget < 0) return true;
            var x = stack.Pop();
            while (x is Cons c)
            {
                if (--budget < 0) return true;
                if (c.Car is Cons) stack.Push(c.Car);
                else if (c.Car is Symbol s1 && IsGlobalSymbolMacroName(s1, names)) return true;
                x = c.Cdr;
            }
            if (x is Symbol s2 && IsGlobalSymbolMacroName(s2, names)) return true;
        }
        return false;
    }

    [ThreadStatic] static Stack<LispObject>? _mentionsStack;

    static bool IsGlobalSymbolMacroName(Symbol sym, HashSet<Symbol>? names)
        => (names != null && names.Contains(sym)) || TryGetGlobalSymbolMacro(sym, out _);

    // The symbol keys of the compiler's global symbol-macro table, copied once
    // per table version so the walk above does not take the table's lock per node.
    sealed class GsmNames
    {
        public readonly LispHashTable Table; public readonly int Version; public readonly HashSet<Symbol> Names;
        public GsmNames(LispHashTable t, int v, HashSet<Symbol> n) { Table = t; Version = v; Names = n; }
    }
    static volatile GsmNames? _gsmNames;
    static HashSet<Symbol> GsmNameSet(LispHashTable gsm)
    {
        int v = gsm.Version;
        var c = _gsmNames;
        if (c != null && ReferenceEquals(c.Table, gsm) && c.Version == v) return c.Names;
        var set = new HashSet<Symbol>(SymbolIdentityComparer.Instance);
        foreach (var kv in gsm.Entries)
            if (kv.Key is Symbol k) set.Add(k);
        _gsmNames = new GsmNames(gsm, v, set);
        return set;
    }

    static readonly ConditionalWeakTable<Cons, MiniExpansionBox> _miniExpansions = new();
    static readonly ConditionalWeakTable<LispObject, StrongBox<LispObject?>> _miniMemo = new();
}
