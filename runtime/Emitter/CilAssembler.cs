namespace DotCL.Emitter;

/// <summary>
/// Emit-free half of CilAssembler: the function registry, constant pool, and
/// runtime-code-generation guards. This half compiles on every target,
/// including netstandard2.0, where it has no dependency on
/// System.Reflection.Emit. The IL-emitting half lives in CilAssembler.Emit.cs
/// (Compile Remove'd on netstandard2.0). A precompiled .fasl image — already
/// IL — reaches GetFunction*/RegisterFunction*/GetConstant here at load/run
/// time without ever touching the emitter.
/// </summary>
public partial class CilAssembler
{
    // Function registry is per-symbol (sym.Function / sym.SetfFunction).
    // The former flat `_functions` ConcurrentDictionary was removed —
    // Startup.Sym(name).Function is now the sole source of truth.

    // Constant pool for non-inline literals. Stored as a raw object[] so
    // indexed reads are lock-free (one volatile load of the array reference
    // then a plain array access). Additions go through AddConstant which
    // copy-on-writes a new array under _constantsLock.
    private static object[] _constants = Array.Empty<object>();
    private static int _constantsSize;
    private static readonly object _constantsLock = new();

    /// <summary>Precompiled-only mode: when true, any attempt to generate code at
    /// runtime (eval/compile of compound forms, dotnet:define-class, native FFI
    /// thunks) throws instead of emitting. Lets a host run a precompiled-only
    /// image and fail loudly if something tries to JIT — the same constraint an
    /// AOT/IL2CPP target imposes. Running already-compiled code is unaffected.</summary>
    public static bool PrecompiledOnly;

    /// <summary>Throws if <see cref="PrecompiledOnly"/> is set. Call at runtime
    /// code-generation entry points.</summary>
    internal static void EnsureEmitAllowed(string what)
    {
        if (PrecompiledOnly)
            throw new LispErrorException(new LispProgramError(
                $"precompiled-only mode (dotcl:precompiled-only): {what} requires runtime " +
                "code generation, which is disabled; only precompiled code can run here"));
    }

    // --- Public API ---

    public static LispObject AssembleAndRun(LispObject instrList)
    {
#if DOTCL_EMIT
        // Check for :toplevel-boundary markers — split and run each segment
        // individually so that defvar values are available for subsequent
        // macro expansion within the same eval-when block.
        var segments = SplitAtBoundaries(instrList);
        if (segments != null)
        {
            LispObject result = Nil.Instance;
            foreach (var segment in segments)
                result = AssembleAndRunSingle(segment);
            return result;
        }
        return AssembleAndRunSingle(instrList);
#else
        throw new LispErrorException(new LispProgramError(
            "this runtime was built without System.Reflection.Emit; eval/compile of new " +
            "code is unavailable — only precompiled .fasl code can run here"));
#endif
    }

    public static void Reset()
    {
        lock (_constantsLock)
        {
            _constants = Array.Empty<object>();
            _constantsSize = 0;
        }
    }

    public static LispFunction GetFunction(string name)
    {
        // (SETF NAME) form: look up SetfFunction on the target symbol.
        // compile-named-call emits (:ldstr "(SETF NAME)") (:call "CilAssembler.GetFunction")
        // for non-symbol names like (setf foo). After removing the _functions fallback,
        // we must route to SetfFunction explicitly here.
        // Cross-package bridge: search all packages for a symbol with SetfFunction set,
        // since the defun may have been registered in a package other than CL or DOTCL-INTERNAL.
        if (name.StartsWith("(SETF ", StringComparison.Ordinal) && name.EndsWith(")"))
        {
            var targetName = name.Substring(6, name.Length - 7);
            // Try current-package symbol first
            var targetSym = Startup.Sym(targetName);
            if (targetSym.SetfFunction is LispFunction setfFn0) return setfFn0;
            // Cross-package search: find any symbol by this name with SetfFunction
            foreach (var pkg in Package.AllPackages)
            {
                var (other, status) = pkg.FindSymbol(targetName);
                if (status != SymbolStatus.None && other.SetfFunction is LispFunction setfFn)
                    return setfFn;
            }
            throw new LispErrorException(new LispUndefinedFunction(
                new Cons(Startup.Sym("SETF"), new Cons(targetSym, Nil.Instance))));
        }
        var sym = Startup.Sym(name);
        if (sym.Function is LispFunction symFn) return symFn;
        // Cross-package bridge: uninterned-fixup calls this during FASL loading when
        // *PACKAGE* may differ from the package where the function was registered
        // (e.g. LEXICAL-CONTEXTS vs DOTCL-INTERNAL). Search all packages (same as
        // GetFunctionBySymbol / CoerceToFunction).
        foreach (var pkg in Package.AllPackages)
        {
            if (pkg == sym.HomePackage) continue;
            var (other, status) = pkg.FindSymbol(name);
            if (status != SymbolStatus.None && other.Function is LispFunction otherFn)
                return otherFn;
        }
        throw new LispErrorException(new LispUndefinedFunction(sym));
    }

    /// <summary>
    /// Symbol-based function lookup. sym.Function is primary. If empty,
    /// fall back to any same-named symbol in another package that has a
    /// Function — replaces the old _functions flat
    /// table as a cross-package bridge. Caches the result on sym.Function
    /// to make subsequent lookups O(1).
    /// </summary>
    public static LispFunction GetFunctionBySymbol(Symbol sym)
    {
        if (sym.Function is LispFunction symFn) return symFn;
        foreach (var pkg in Package.AllPackages)
        {
            if (pkg == sym.HomePackage) continue;
            var (other, status) = pkg.FindSymbol(sym.Name);
            if (status != SymbolStatus.None && other.Function is LispFunction otherFn)
            {
                sym.Function = otherFn;   // cache for future lookups
                return otherFn;
            }
        }
        throw new LispErrorException(new LispUndefinedFunction(sym));
    }

    /// <summary>Symbol-based setf function lookup — sym.SetfFunction is authoritative.</summary>
    public static LispFunction GetSetfFunctionBySymbol(Symbol sym)
    {
        if (sym.SetfFunction is LispFunction setfFn) return setfFn;
        // Fallback: check GF registry in case sym.SetfFunction was not set but the GF
        // was registered via RegisterGF (can happen when cil-out.sil is generated by
        // an older dotcl host that produces slightly different DEFGENERIC output).
        var setfName = new Cons(Startup.Sym("SETF"), new Cons(sym, Nil.Instance));
        if (Runtime.FindGF(setfName) is LispFunction gfFn)
        {
            sym.SetfFunction = gfFn; // cache for subsequent lookups
            return gfFn;
        }
        throw new LispErrorException(new LispUndefinedFunction(setfName));
    }

    /// <summary>
    /// Try to get a function without signaling any conditions.
    /// Returns null if not found. Safe to call even with active handler-binds.
    /// </summary>
    public static LispFunction? TryGetFunction(string name)
    {
        if (name.StartsWith("(SETF ", StringComparison.Ordinal) && name.EndsWith(")"))
        {
            var targetName = name.Substring(6, name.Length - 7);
            if (Startup.Sym(targetName).SetfFunction is LispFunction fn) return fn;
            foreach (var pkg in Package.AllPackages)
            {
                var (other, status) = pkg.FindSymbol(targetName);
                if (status != SymbolStatus.None && other.SetfFunction is LispFunction setfFn)
                    return setfFn;
            }
            return null;
        }
        return Startup.Sym(name).Function as LispFunction;
    }

    public static void UnregisterFunction(string name)
    {
        Startup.Sym(name).Function = null;
    }

    /// <summary>Register a function on a specific Symbol object (package-aware).</summary>
    public static void RegisterFunctionOnSymbol(Symbol sym, LispFunction fn)
    {
        // GenericFunctions may replace CL symbols (e.g. for gray-streams wrapping).
        // The DEFGENERIC macro already warns when replacing a CL standard function.
        if (fn is not GenericFunction)
            Runtime.CheckPackageLock(sym, "DEFUN");
        sym.Function = fn;
    }

    /// <summary>
    /// Package-aware function registration that protects inherited CL symbols.
    /// If the symbol's home package is CL but defPkg is different, skip function slot update
    /// (a foreign-package defun must not overwrite an inherited CL symbol).
    /// </summary>
    public static void RegisterFunctionOnSymbolGuarded(Symbol sym, LispFunction fn, string defPkg)
    {
        Runtime.CheckPackageLock(sym, "DEFUN");
        var homePkg = sym.HomePackage;
        bool isForeignCL = homePkg != null && homePkg.Name != defPkg
            && homePkg.Name == "COMMON-LISP";
        // Both branches set sym.Function so the current symbol reference dispatches
        // correctly. The "isForeignCL" case used to skip the flat-table write to
        // protect host built-ins; with sym.Function as the sole source, the guard
        // is implicit (the CL symbol still has its own Function slot intact).
        sym.Function = fn;
    }

    /// <summary>
    /// Register a (setf name) function on the target Symbol object.
    /// sym.SetfFunction is authoritative storage.
    /// </summary>
    public static void RegisterSetfFunctionOnSymbol(Symbol sym, LispFunction fn)
    {
        sym.SetfFunction = fn;
    }

    public static void RegisterFunction(string name, LispFunction fn)
    {
        // Handle (SETF NAME) functions: register on the target symbol's SetfFunction
        // slot so that #'(setf name) / GetSetfFunctionBySymbol can find them.
        if (name.StartsWith("(SETF ", StringComparison.Ordinal) && name.EndsWith(")"))
        {
            var targetName = name.Substring(6, name.Length - 7);
            var targetSym = Startup.Sym(targetName);
            targetSym.SetfFunction = fn;
            return;
        }
        // Use bridge-free lookup: only CL + DOTCL-INTERNAL.
        // Prevents RegisterFunction from silently overwriting another package's
        // Function slot via the cross-package bridge.
        var checkedSym = Startup.SymForRegistration(name);
        Runtime.CheckPackageLock(checkedSym, "DEFUN");  // may throw if locked
        checkedSym.Function = fn;
    }

    public static int AddConstant(object value)
    {
        lock (_constantsLock)
        {
            int idx = _constantsSize;
            if (idx >= _constants.Length)
            {
                // Copy-on-write grow: allocate new array, copy old, assign.
                // Readers holding an old reference still see a consistent
                // snapshot (old array contents are never mutated, array
                // reference swap is atomic on CLR).
                int newCap = _constants.Length == 0 ? 16 : _constants.Length * 2;
                var grown = new object[newCap];
                Array.Copy(_constants, grown, _constantsSize);
                grown[idx] = value;
                System.Threading.Volatile.Write(ref _constants, grown);
            }
            else
            {
                _constants[idx] = value;
            }
            System.Threading.Volatile.Write(ref _constantsSize, idx + 1);
            return idx;
        }
    }

    // Lock-free indexed read. Takes one volatile load of the array reference
    // (to observe any grow-related swap) and then a plain array access.
    // Reclaims a per-call Monitor acquire on every global function call
    // (LOAD-SYM -> GetConstant), which was a measurable bottleneck.
    [System.Runtime.CompilerServices.MethodImpl(
        System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static object GetConstant(int index) =>
        System.Threading.Volatile.Read(ref _constants)[index];
}
