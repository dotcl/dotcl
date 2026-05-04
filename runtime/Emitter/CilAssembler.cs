using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Thin assembler: walks an instruction list (S-expression data from the Lisp compiler)
/// and translates each instruction to ILGenerator calls.
/// </summary>
public class CilAssembler
{
    internal ILGenerator _il = null!;
    private readonly Dictionary<string, LocalBuilder> _locals = new();
    private readonly Dictionary<string, Label> _labels = new();

    // FASL mode: emit constants inline in IL instead of using constant pool
    internal bool _faslMode;
    // TypeBuilder for FASL mode — used to define closure body methods
    internal TypeBuilder? _faslTypeBuilder;
    private static int _faslClosureCount;

    // Function registry is per-symbol (sym.Function / sym.SetfFunction) as of
    // D683 / #113 Phase 3. The former flat `_functions` ConcurrentDictionary
    // was removed — Startup.Sym(name).Function is now the sole source of truth.

    // Constant pool for non-inline literals. Stored as a raw object[] so
    // indexed reads are lock-free (one volatile load of the array reference
    // then a plain array access). Additions go through AddConstant which
    // copy-on-writes a new array under _constantsLock.
    private static object[] _constants = Array.Empty<object>();
    private static int _constantsSize;
    private static readonly object _constantsLock = new();

    // --- Public API ---

    public static LispObject AssembleAndRun(LispObject instrList)
    {
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
    }

    private static LispObject AssembleAndRunSingle(LispObject instrList)
    {
        var asm = new CilAssembler();
        var dm = new DynamicMethod("toplevel", typeof(LispObject),
            Type.EmptyTypes, typeof(CilAssembler).Module, true);
        asm._il = dm.GetILGenerator();
        asm.Assemble(instrList);
        var fn = (Func<LispObject>)dm.CreateDelegate(typeof(Func<LispObject>));
        return fn();
    }

    /// <summary>
    /// Split instruction list at :toplevel-boundary markers.
    /// Returns null if no boundaries found.
    /// </summary>
    private static List<LispObject>? SplitAtBoundaries(LispObject instrList)
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
        // (SETF NAME) form: look up SetfFunction on the target symbol (D697).
        // compile-named-call emits (:ldstr "(SETF NAME)") (:call "CilAssembler.GetFunction")
        // for non-symbol names like (setf foo). After D683 removed _functions fallback,
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
    /// Function (D683, #113 Phase 3) — replaces the old _functions flat
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
        throw new LispErrorException(new LispUndefinedFunction(
            new Cons(Startup.Sym("SETF"), new Cons(sym, Nil.Instance))));
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
        Runtime.CheckPackageLock(sym, "DEFUN");
        sym.Function = fn;
    }

    /// <summary>
    /// Package-aware function registration that protects inherited CL symbols.
    /// If the symbol's home package is CL but defPkg is different, skip function slot update
    /// (a foreign-package defun must not overwrite an inherited CL symbol, D427).
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
        // slot so that #'(setf name) / GetSetfFunctionBySymbol can find them (D693).
        if (name.StartsWith("(SETF ", StringComparison.Ordinal) && name.EndsWith(")"))
        {
            var targetName = name.Substring(6, name.Length - 7);
            var targetSym = Startup.Sym(targetName);
            targetSym.SetfFunction = fn;
            return;
        }
        // Use bridge-free lookup: only CL + DOTCL-INTERNAL.
        // Prevents RegisterFunction from silently overwriting another package's
        // Function slot via the cross-package bridge (#158/D918).
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

    public static LispObject MakeClosure(object[] env, int dmIndex, int arity)
    {
        var dm = (DynamicMethod)System.Threading.Volatile.Read(ref _constants)[dmIndex];
        var closureDel = (Func<object[], LispObject[], LispObject>)dm.CreateDelegate(
            typeof(Func<object[], LispObject[], LispObject>));
        return new LispFunction(closureDel, env, null, arity);
    }

    // --- Assembly ---

    internal void Assemble(LispObject instructions)
    {
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
            AssembleOne(instr);
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

    private void AssembleOne(LispObject instr)
    {
        if (instr is not Cons c) return;
        var opSym = c.Car as Symbol;
        if (opSym == null) return;
        var op = opSym.Name;

        switch (op)
        {
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
                _il.Emit(OpCodes.Ret);
                break;
            case "CALL":
                EmitCall(GetString(Cadr(c)));
                break;
            case "CALLVIRT":
                EmitCallvirt(GetString(Cadr(c)));
                break;
            case "TAIL-PREFIX":
                // CIL tail-call prefix. Must be immediately followed by a call/callvirt/calli
                // whose next instruction is `ret`. CLR JIT may or may not honor it, but it's
                // a safe hint. The compiler only emits this when *in-tail-position* is T and
                // the call site isn't inside a try/finally region (D683).
                _il.Emit(OpCodes.Tailcall);
                break;
            case "NEWOBJ":
                EmitNewobj(GetString(Cadr(c)));
                break;
            case "CASTCLASS":
                EmitCastclass(GetString(Cadr(c)));
                break;
            case "DECLARE-LOCAL":
                DeclareLocal(GetSymbolName(Cadr(c)), GetString(Caddr(c)));
                break;
            case "LDLOC":
                _il.Emit(OpCodes.Ldloc, GetLocal(GetSymbolName(Cadr(c))));
                break;
            case "STLOC":
                _il.Emit(OpCodes.Stloc, GetLocal(GetSymbolName(Cadr(c))));
                break;
            case "LDARG":
                EmitLdarg(GetInt(Cadr(c)));
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
                // Throws InvalidCastException if not a Fixnum — caller must
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
            case "LABEL":
                _il.MarkLabel(GetOrDefineLabel(GetSymbolName(Cadr(c))));
                break;
            case "SWITCH":
                EmitSwitch(Cadr(c));
                break;
            case "NEWARR":
                EmitNewarr(GetString(Cadr(c)));
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
            case "CONV-I4":
                _il.Emit(OpCodes.Conv_I4);
                break;
            case "CONV-R4":
                _il.Emit(OpCodes.Conv_R4);
                break;
            case "BEGIN-EXCEPTION-BLOCK":
                _il.BeginExceptionBlock();
                break;
            case "BEGIN-CATCH-BLOCK":
                _il.BeginCatchBlock(ResolveType(GetString(Cadr(c))));
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
            case "LOAD-SYM":
            {
                // Resolve at runtime (not assembly time) so later
                // defmethod-direct registrations on the symbol's home
                // package are picked up. Pre-resolving here would pin
                // a Function-less placeholder in the constants pool
                // before the defun has run (D683, #113 Phase 3).
                var symName = GetString(Cadr(c));
                _il.Emit(OpCodes.Ldstr, _faslMode ? Track(symName) : symName);
                _il.Emit(OpCodes.Call, _methodCache["Startup.Sym"]);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            }
            case "LOAD-SYM-KEYWORD":
            {
                var kwName = GetString(Cadr(c));
                if (_faslMode)
                {
                    _il.Emit(OpCodes.Ldstr, Track(kwName));
                    _il.Emit(OpCodes.Call, _methodCache["Startup.Keyword"]);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                else
                {
                    var sym = Startup.Keyword(kwName);
                    int idx = AddConstant(sym);
                    _il.Emit(OpCodes.Ldc_I4, idx);
                    _il.Emit(OpCodes.Call, _getConstant);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                break;
            }
            case "LOAD-SYM-PKG":
            {
                var spName = GetString(Cadr(c));
                var spPkg = GetString(Caddr(c));
                if (_faslMode)
                {
                    _il.Emit(OpCodes.Ldstr, Track(spName));
                    _il.Emit(OpCodes.Ldstr, Track(spPkg));
                    _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                else
                {
                    var sym = Startup.SymInPkg(spName, spPkg);
                    int idx = AddConstant(sym);
                    _il.Emit(OpCodes.Ldc_I4, idx);
                    _il.Emit(OpCodes.Call, _getConstant);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                break;
            }
            case "LOAD-ENV":
                EmitLoadEnv(GetInt(Cadr(c)));
                break;
            case "LOAD-ARG":
                // Load args[i]: args is arg 1 (LispObject[]) for closures
                _il.Emit(OpCodes.Ldarg_1);
                _il.Emit(OpCodes.Ldc_I4, GetInt(Cadr(c)));
                _il.Emit(OpCodes.Ldelem_Ref);
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
            case "MAKE-FUNCTION":
                HandleMakeFunction(c);
                break;
            case "MAKE-FUNCTION-DIRECT":
                HandleMakeFunctionDirect(c);
                break;
            case "MAKE-CLOSURE":
                HandleMakeClosure(c);
                break;

            default:
                throw new Exception($"CilAssembler: unknown opcode {op}");
        }
    }

    // --- Composite instructions ---

    private void HandleDefmethod(Cons instr)
    {
        // (:defmethod "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
        var plist = instr.Cdr;
        var name = GetString(Car(plist));
        plist = Cdr(plist); // skip name

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAMS":
                    var cur = val;
                    while (cur is Cons lc)
                    {
                        paramNames.Add(GetString(lc.Car));
                        cur = lc.Cdr;
                    }
                    break;
                case "BODY":
                    bodyInstrs = val;
                    break;
                case "PKG":
                    defPkg = GetString(val);
                    break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("DEFMETHOD: missing :body");

        if (_faslMode && _faslTypeBuilder != null)
        {
            // FASL mode: emit body into the persisted TypeBuilder and registration IL
            // into the current ILGenerator. No DynamicMethod / _constants pool allowed
            // because the resulting .fasl must load in a fresh process where those are
            // both unavailable.
            int faslId = Interlocked.Increment(ref _faslClosureCount);
            FaslAssembler.EmitDefmethodInto(_faslTypeBuilder, _il, _faslStructMap!,
                name, paramNames.Count, bodyInstrs, defPkg, faslId);
            return;
        }

        // Compile the body as DynamicMethod(LispObject[] args) -> LispObject
        var dm = new DynamicMethod(name, typeof(LispObject),
            new[] { typeof(LispObject[]) }, typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = dm.GetILGenerator();
        innerAsm.Assemble(bodyInstrs);

        var del = (Func<LispObject[], LispObject>)dm.CreateDelegate(
            typeof(Func<LispObject[], LispObject>));

        var fn = new LispFunction(del, name, paramNames.Count);
        // Store SIL on function when dotcl:*save-sil* is true
        try
        {
            var saveSilSym = Startup.SymInPkg("*SAVE-SIL*", "DOTCL");
            if (DynamicBindings.Get(saveSilSym) is not Nil)
                fn.Sil = bodyInstrs;
        }
        catch { }

        // Register on the correct symbol for package-aware lookup.
        // If :pkg was specified, use that package (the defun name's home package).
        // Otherwise fall back to *package* (D115 fix for flat namespace collision).
        // Check foreign CL BEFORE updating flat table to prevent overwriting host builtins (D427).
        Symbol? pkgSym = null;
        bool isForeignCL2 = false;
        Symbol? setfTargetSym = null;  // for (SETF NAME) functions: the target NAME symbol
        if (!name.StartsWith("("))  // regular function names
        {
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackage(defPkg);
                else
                    pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(name);
                    // Don't overwrite inherited CL-package symbol functions (D421).
                    var homePkg2 = s.HomePackage;
                    isForeignCL2 = homePkg2 != null && homePkg2 != pkg
                        && homePkg2.Name == "COMMON-LISP";
                    if (!isForeignCL2)
                    {
                        s.Function = fn;
                        pkgSym = s;
                    }
                }
            }
            catch { /* ignore errors during bootstrap */ }
        }
        else if (name.StartsWith("(SETF ") && name.EndsWith(")"))
        {
            // (SETF NAME): register fn on the target NAME symbol's SetfFunction slot (#58 Phase 1)
            var targetName = name.Substring(6, name.Length - 7);
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackage(defPkg);
                else
                    pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(targetName);
                    s.SetfFunction = fn;
                    setfTargetSym = s;
                }
            }
            catch { /* ignore errors during bootstrap */ }
        }

        // Emit runtime code to re-register the function. This is needed when
        // fmakunbound precedes defun in the same progn — the assembly-time
        // registration above gets undone by fmakunbound at runtime, so we must
        // re-register after fmakunbound has executed.
        int fnIdx = AddConstant(fn);
        if (pkgSym != null)
        {
            // Symbol-based runtime re-registration (package-aware)
            int symIdx = AddConstant(pkgSym);
            _il.Emit(OpCodes.Ldc_I4, symIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(Symbol));
            _il.Emit(OpCodes.Ldc_I4, fnIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispFunction));
            _il.Emit(OpCodes.Call, _registerFunctionOnSymbol);
        }
        if (setfTargetSym != null)
        {
            // Setf function runtime re-registration via sym.SetfFunction (#58 Phase 1)
            int setfSymIdx = AddConstant(setfTargetSym);
            _il.Emit(OpCodes.Ldc_I4, setfSymIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(Symbol));
            _il.Emit(OpCodes.Ldc_I4, fnIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispFunction));
            _il.Emit(OpCodes.Call, _registerSetfFunctionOnSymbol);
        }
    }

    private void HandleMakeFunction(Cons instr)
    {
        // (:make-function :param-count N :body (...) [:name "NAME"])
        var plist = instr.Cdr;
        int paramCount = 0;
        LispObject? bodyInstrs = null;
        string? fnName = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAM-COUNT": paramCount = GetInt(val); break;
                case "BODY": bodyInstrs = val; break;
                case "NAME": fnName = val is LispString s ? s.Value : null; break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("MAKE-FUNCTION: missing :body");

        if (_faslMode && _faslTypeBuilder != null)
        {
            int fnId = Interlocked.Increment(ref _faslClosureCount);
            string methodName = $"fn_{fnId}";
            var method = _faslTypeBuilder.DefineMethod(methodName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), new[] { typeof(LispObject[]) });
            var innerAsm = new CilAssembler();
            innerAsm._il = method.GetILGenerator();
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _faslTypeBuilder;
            innerAsm._faslStructMap = _faslStructMap;
            innerAsm.Assemble(bodyInstrs);

            // Push new LispFunction(delegate, name, arity)
            _il.Emit(OpCodes.Ldnull);
            _il.Emit(OpCodes.Ldftn, method);
            _il.Emit(OpCodes.Newobj, typeof(Func<LispObject[], LispObject>)
                .GetConstructor(new[] { typeof(object), typeof(IntPtr) })!);
            if (fnName != null)
                _il.Emit(OpCodes.Ldstr, Track(fnName));
            else
                _il.Emit(OpCodes.Ldnull);
            _il.Emit(OpCodes.Ldc_I4, paramCount);
            _il.Emit(OpCodes.Newobj, typeof(LispFunction)
                .GetConstructor(new[] { typeof(Func<LispObject[], LispObject>), typeof(string), typeof(int) })!);
        }
        else
        {
            var dm = new DynamicMethod("lambda", typeof(LispObject),
                new[] { typeof(LispObject[]) }, typeof(CilAssembler).Module, true);
            var innerAsm = new CilAssembler();
            innerAsm._il = dm.GetILGenerator();
            innerAsm.Assemble(bodyInstrs);

            var del = (Func<LispObject[], LispObject>)dm.CreateDelegate(
                typeof(Func<LispObject[], LispObject>));
            var fn = new LispFunction(del, fnName, paramCount);

            // Push the function onto the stack
            int idx = AddConstant(fn);
            _il.Emit(OpCodes.Ldc_I4, idx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispObject));
        }
    }

    private void HandleMakeFunctionDirect(Cons instr)
    {
        // (:make-function-direct :param-count N :body (...) [:name "NAME"])
        // Body uses (:ldarg 0), (:ldarg 1), ... for direct param access (no array)
        var plist = instr.Cdr;
        int paramCount = 0;
        LispObject? bodyInstrs = null;
        string? fnName = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAM-COUNT": paramCount = GetInt(val); break;
                case "BODY": bodyInstrs = val; break;
                case "NAME": fnName = val is LispString s ? s.Value : null; break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("MAKE-FUNCTION-DIRECT: missing :body");
        if (paramCount > 8) throw new Exception("MAKE-FUNCTION-DIRECT: param-count > 8 not supported");

        if (_faslMode && _faslTypeBuilder != null)
        {
            // FASL mode: create body + wrapper on TypeBuilder, push LispFunction
            var directParamTypes = new Type[paramCount];
            for (int i = 0; i < paramCount; i++) directParamTypes[i] = typeof(LispObject);

            int fnId = Interlocked.Increment(ref _faslClosureCount);
            string bodyName = $"fnd_body_{fnId}";
            var bodyMethod = _faslTypeBuilder.DefineMethod(bodyName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), directParamTypes);
            var innerAsm = new CilAssembler();
            innerAsm._il = bodyMethod.GetILGenerator();
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _faslTypeBuilder;
            innerAsm._faslStructMap = _faslStructMap;
            innerAsm.Assemble(bodyInstrs);

            // Create wrapper: (LispObject[] args) -> LispObject
            string wrapperName = $"fnd_{fnId}";
            var wrapperMethod = _faslTypeBuilder.DefineMethod(wrapperName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), new[] { typeof(LispObject[]) });
            var wil = wrapperMethod.GetILGenerator();
            wil.Emit(OpCodes.Ldstr, Track(fnName ?? "lambda"));
            wil.Emit(OpCodes.Ldarg_0);
            wil.Emit(OpCodes.Ldc_I4, paramCount);
            wil.Emit(OpCodes.Call, typeof(Runtime).GetMethod("CheckArityExact")!);
            for (int i = 0; i < paramCount; i++)
            {
                wil.Emit(OpCodes.Ldarg_0);
                wil.Emit(OpCodes.Ldc_I4, i);
                wil.Emit(OpCodes.Ldelem_Ref);
            }
            wil.Emit(OpCodes.Call, bodyMethod);
            wil.Emit(OpCodes.Ret);

            // Push new LispFunction(delegate, name, arity)
            _il.Emit(OpCodes.Ldnull);
            _il.Emit(OpCodes.Ldftn, wrapperMethod);
            _il.Emit(OpCodes.Newobj, typeof(Func<LispObject[], LispObject>)
                .GetConstructor(new[] { typeof(object), typeof(IntPtr) })!);
            if (fnName != null)
                _il.Emit(OpCodes.Ldstr, Track(fnName));
            else
                _il.Emit(OpCodes.Ldnull);
            _il.Emit(OpCodes.Ldc_I4, paramCount);
            _il.Emit(OpCodes.Newobj, typeof(LispFunction)
                .GetConstructor(new[] { typeof(Func<LispObject[], LispObject>), typeof(string), typeof(int) })!);

            // Install _funcN typed-delegate fast path so direct calls don't pay
            // array-alloc + wrapper overhead. Stack has the LispFunction; dup, build
            // typed delegate, call SetDirectDelegate (returns void), leaving fn on stack.
            if (paramCount >= 0 && paramCount <= 8)
            {
                _il.Emit(OpCodes.Dup);
                _il.Emit(OpCodes.Ldnull);
                _il.Emit(OpCodes.Ldftn, bodyMethod);
                _il.Emit(OpCodes.Newobj, FaslAssembler.TypedFuncCtors[paramCount]);
                _il.Emit(OpCodes.Callvirt, FaslAssembler.SetDirectDelegateMI);
            }
            return;
        }

        // Create DynamicMethod with direct LispObject params
        var directParamTypes2 = new Type[paramCount];
        for (int i = 0; i < paramCount; i++) directParamTypes2[i] = typeof(LispObject);

        var directDm = new DynamicMethod("lambda_direct", typeof(LispObject),
            directParamTypes2, typeof(CilAssembler).Module, true);
        var innerAsm2 = new CilAssembler();
        innerAsm2._il = directDm.GetILGenerator();
        innerAsm2.Assemble(bodyInstrs);

        // Create array-based wrapper for backward compat (funcall, apply, etc.)
        // Wrapper includes arity check for proper error reporting
        var wrapName = fnName ?? "lambda";
        Func<LispObject[], LispObject> arrayDel;
        switch (paramCount)
        {
            case 0:
            {
                var d = (Func<LispObject>)directDm.CreateDelegate(typeof(Func<LispObject>));
                var n = wrapName;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 0); return d(); };
                var fn = new LispFunction(arrayDel, fnName, paramCount);
                fn._func0 = d;
                int idx = AddConstant(fn);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                return;
            }
            case 1:
            {
                var d = (Func<LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject>));
                var n = wrapName;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 1); return d(args[0]); };
                var fn = new LispFunction(arrayDel, fnName, paramCount);
                fn._func1 = d;
                int idx = AddConstant(fn);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                return;
            }
            case 2:
            {
                var d = (Func<LispObject, LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject, LispObject>));
                var n = wrapName;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 2); return d(args[0], args[1]); };
                var fn = new LispFunction(arrayDel, fnName, paramCount);
                fn._func2 = d;
                int idx = AddConstant(fn);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                return;
            }
            case 3:
            {
                var d = (Func<LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject, LispObject, LispObject>));
                var n = wrapName;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 3); return d(args[0], args[1], args[2]); };
                var fn = new LispFunction(arrayDel, fnName, paramCount);
                fn._func3 = d;
                int idx = AddConstant(fn);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                return;
            }
            case 4:
            {
                var d = (Func<LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject>));
                var n = wrapName;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 4); return d(args[0], args[1], args[2], args[3]); };
                var fn = new LispFunction(arrayDel, fnName, paramCount);
                fn._func4 = d;
                int idx = AddConstant(fn);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                return;
            }
            default:
            {
                // Cases 5-8: generic direct-param path
                var delegateType = paramCount switch
                {
                    5 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    6 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    7 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    8 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    _ => throw new Exception($"MAKE-FUNCTION-DIRECT: unsupported param-count {paramCount}")
                };
                var d = directDm.CreateDelegate(delegateType);
                var n = wrapName;
                var pc = paramCount;
                arrayDel = args => { Runtime.CheckArityExact(n, args, pc); return ((dynamic)d).Invoke(args[0], args[1], args[2], args[3], args[4], pc > 5 ? args[5] : null!, pc > 6 ? args[6] : null!, pc > 7 ? args[7] : null!); };
                // Build proper wrapper that only passes the right number of args
                Func<LispObject[], LispObject> properWrapper = paramCount switch
                {
                    5 => args => { Runtime.CheckArityExact(n, args, 5); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d)(args[0], args[1], args[2], args[3], args[4]); },
                    6 => args => { Runtime.CheckArityExact(n, args, 6); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d)(args[0], args[1], args[2], args[3], args[4], args[5]); },
                    7 => args => { Runtime.CheckArityExact(n, args, 7); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d)(args[0], args[1], args[2], args[3], args[4], args[5], args[6]); },
                    8 => args => { Runtime.CheckArityExact(n, args, 8); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d)(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]); },
                    _ => throw new Exception("unreachable")
                };
                var fn = new LispFunction(properWrapper, fnName, paramCount);
                switch (paramCount)
                {
                    case 5: fn._func5 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d; break;
                    case 6: fn._func6 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d; break;
                    case 7: fn._func7 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d; break;
                    case 8: fn._func8 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)d; break;
                }
                int idx = AddConstant(fn);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                return;
            }
        }
    }

    private void HandleDefmethodDirect(Cons instr)
    {
        // (:defmethod-direct "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
        // Body uses (:ldarg 0), (:ldarg 1), ... for direct param access
        var plist = instr.Cdr;
        var name = GetString(Car(plist));
        plist = Cdr(plist); // skip name

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAMS":
                    var cur = val;
                    while (cur is Cons lc)
                    {
                        paramNames.Add(GetString(lc.Car));
                        cur = lc.Cdr;
                    }
                    break;
                case "BODY":
                    bodyInstrs = val;
                    break;
                case "PKG":
                    defPkg = GetString(val);
                    break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("DEFMETHOD-DIRECT: missing :body");

        int paramCount = paramNames.Count;
        if (paramCount > 8) throw new Exception("DEFMETHOD-DIRECT: param-count > 8 not supported");

        if (_faslMode && _faslTypeBuilder != null)
        {
            // FASL mode: emit body/wrapper into the persisted TypeBuilder and registration IL
            // (including _funcN direct-call fast path) into the current ILGenerator.
            int faslId = Interlocked.Increment(ref _faslClosureCount);
            FaslAssembler.EmitDefmethodDirectInto(_faslTypeBuilder, _il, _faslStructMap!,
                name, paramCount, bodyInstrs, defPkg, faslId);
            return;
        }

        // Create DynamicMethod with direct LispObject params
        var directParamTypes = new Type[paramCount];
        for (int i = 0; i < paramCount; i++) directParamTypes[i] = typeof(LispObject);

        var directDm = new DynamicMethod(name + "_direct", typeof(LispObject),
            directParamTypes, typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = directDm.GetILGenerator();
        innerAsm.Assemble(bodyInstrs);

        // Create array-based wrapper with arity check and LispFunction with direct delegate
        Func<LispObject[], LispObject> arrayDel;
        LispFunction fn;
        switch (paramCount)
        {
            case 0:
            {
                var d = (Func<LispObject>)directDm.CreateDelegate(typeof(Func<LispObject>));
                var n = name;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 0); return d(); };
                fn = new LispFunction(arrayDel, name, paramCount);
                fn._func0 = d;
                break;
            }
            case 1:
            {
                var d = (Func<LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject>));
                var n = name;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 1); return d(args[0]); };
                fn = new LispFunction(arrayDel, name, paramCount);
                fn._func1 = d;
                break;
            }
            case 2:
            {
                var d = (Func<LispObject, LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject, LispObject>));
                var n = name;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 2); return d(args[0], args[1]); };
                fn = new LispFunction(arrayDel, name, paramCount);
                fn._func2 = d;
                break;
            }
            case 3:
            {
                var d = (Func<LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject, LispObject, LispObject>));
                var n = name;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 3); return d(args[0], args[1], args[2]); };
                fn = new LispFunction(arrayDel, name, paramCount);
                fn._func3 = d;
                break;
            }
            case 4:
            {
                var d = (Func<LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(
                    typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject>));
                var n = name;
                arrayDel = args => { Runtime.CheckArityExact(n, args, 4); return d(args[0], args[1], args[2], args[3]); };
                fn = new LispFunction(arrayDel, name, paramCount);
                fn._func4 = d;
                break;
            }
            default:
            {
                // Cases 5-8: generic direct-param path
                var delegateType = paramCount switch
                {
                    5 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    6 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    7 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    8 => typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
                    _ => throw new Exception($"DEFMETHOD-DIRECT: unsupported param-count {paramCount}")
                };
                var del = directDm.CreateDelegate(delegateType);
                var n = name;
                var pc = paramCount;
                Func<LispObject[], LispObject> wrapper = paramCount switch
                {
                    5 => args => { Runtime.CheckArityExact(n, args, 5); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del)(args[0], args[1], args[2], args[3], args[4]); },
                    6 => args => { Runtime.CheckArityExact(n, args, 6); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del)(args[0], args[1], args[2], args[3], args[4], args[5]); },
                    7 => args => { Runtime.CheckArityExact(n, args, 7); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del)(args[0], args[1], args[2], args[3], args[4], args[5], args[6]); },
                    8 => args => { Runtime.CheckArityExact(n, args, 8); return ((Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del)(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]); },
                    _ => throw new Exception("unreachable")
                };
                arrayDel = wrapper;
                fn = new LispFunction(arrayDel, name, paramCount);
                switch (paramCount)
                {
                    case 5: fn._func5 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del; break;
                    case 6: fn._func6 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del; break;
                    case 7: fn._func7 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del; break;
                    case 8: fn._func8 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)del; break;
                }
                break;
            }
        }

        // Store SIL on function when dotcl:*save-sil* is true
        try
        {
            var saveSilSym = Startup.SymInPkg("*SAVE-SIL*", "DOTCL");
            if (DynamicBindings.Get(saveSilSym) is not Nil)
                fn.Sil = bodyInstrs;
        }
        catch { }

        // Register on the correct symbol for package-aware lookup.
        // If :pkg was specified, use that package (the defun name's home package).
        // Otherwise fall back to *package*.
        // Check foreign CL BEFORE updating flat table to prevent overwriting host builtins (D427).
        Symbol? pkgSym = null;
        bool isForeignCL = false;
        Symbol? setfTargetSym = null;  // for (SETF NAME) functions: the target NAME symbol
        if (!name.StartsWith("("))
        {
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackage(defPkg);
                else
                    pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(name);
                    // Don't overwrite inherited CL-package symbol functions from other packages.
                    // e.g. SB-C uses CL, so (defun compile-file ...) in SB-C would
                    // overwrite CL:COMPILE-FILE — protect the host's built-in functions.
                    var homePkg = s.HomePackage;
                    isForeignCL = homePkg != null && homePkg != pkg
                        && homePkg.Name == "COMMON-LISP";
                    // Always update sym.Function for runtime override dispatch
                    s.Function = fn;
                    if (!isForeignCL)
                    {
                        pkgSym = s;
                    }
                    // isForeignCL: leave pkgSym null to skip runtime re-registration and _functions
                }
            }
            catch { /* ignore errors during bootstrap */ }
        }
        else if (name.StartsWith("(SETF ") && name.EndsWith(")"))
        {
            // (SETF NAME): register fn on the target NAME symbol's SetfFunction slot (D697)
            // HandleDefun had this, but HandleDefunDirect was missing it — causing
            // (defun (setf foo) ...) to silently fail to register when use-direct=true.
            var targetName = name.Substring(6, name.Length - 7);
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackage(defPkg);
                else
                    pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(targetName);
                    s.SetfFunction = fn;
                    setfTargetSym = s;
                }
            }
            catch { /* ignore errors during bootstrap */ }
        }

        // Emit runtime re-registration
        int fnIdx = AddConstant(fn);
        if (pkgSym != null)
        {
            int symIdx = AddConstant(pkgSym);
            _il.Emit(OpCodes.Ldc_I4, symIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(Symbol));
            _il.Emit(OpCodes.Ldc_I4, fnIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispFunction));
            _il.Emit(OpCodes.Call, _registerFunctionOnSymbol);
        }
        if (setfTargetSym != null)
        {
            // Setf function runtime re-registration via sym.SetfFunction (#58 Phase 1)
            int setfSymIdx = AddConstant(setfTargetSym);
            _il.Emit(OpCodes.Ldc_I4, setfSymIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(Symbol));
            _il.Emit(OpCodes.Ldc_I4, fnIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispFunction));
            _il.Emit(OpCodes.Call, _registerSetfFunctionOnSymbol);
        }
    }

    // (:defmethod-native "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
    // Body uses Int64 params (declared via (:declare-local KEY "Int64")).
    // Self-calls via (:callvirt "LispFunction.InvokeNativeN") leave long on stack.
    private void HandleDefmethodNative(Cons instr)
    {
        var plist = instr.Cdr;
        var name = GetString(Car(plist));
        plist = Cdr(plist);

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAMS":
                    var cur = val;
                    while (cur is Cons lc) { paramNames.Add(GetString(lc.Car)); cur = lc.Cdr; }
                    break;
                case "BODY": bodyInstrs = val; break;
                case "PKG": defPkg = GetString(val); break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("DEFMETHOD-NATIVE: missing :body");
        int paramCount = paramNames.Count;
        if (paramCount < 1 || paramCount > 4)
            throw new Exception($"DEFMETHOD-NATIVE: param-count {paramCount} not supported (1-4 only)");

        if (_faslMode && _faslTypeBuilder != null)
        {
            int faslId = Interlocked.Increment(ref _faslClosureCount);
            FaslAssembler.EmitDefmethodNativeInto(_faslTypeBuilder, _il, _faslStructMap!,
                name, paramCount, bodyInstrs, defPkg, faslId);
            return;
        }

        // Native DynamicMethod: LispObject Name_native(long p0, ...)
        // Long params avoid arg boxing; body returns LispObject (arithmetic already boxes via Fixnum.Make).
        var nativeParamTypes = new Type[paramCount];
        for (int i = 0; i < paramCount; i++) nativeParamTypes[i] = typeof(long);
        var nativeDm = new DynamicMethod(name + "_native", typeof(LispObject), nativeParamTypes,
            typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = nativeDm.GetILGenerator();
        innerAsm.Assemble(bodyInstrs);

        // Create LispFunction with wrapper lambdas (unbox args; native returns LispObject directly)
        LispFunction fn;
        switch (paramCount)
        {
            case 1:
            {
                var d = (Func<long, LispObject>)nativeDm.CreateDelegate(typeof(Func<long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 1); return d(((Fixnum)args[0]).Value); },
                    name, 1);
                fn._func1 = a => d(((Fixnum)a).Value);
                fn._nativeFunc1 = d;
                break;
            }
            case 2:
            {
                var d = (Func<long, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<long, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 2); return d(((Fixnum)args[0]).Value, ((Fixnum)args[1]).Value); },
                    name, 2);
                fn._func2 = (a, b) => d(((Fixnum)a).Value, ((Fixnum)b).Value);
                fn._nativeFunc2 = d;
                break;
            }
            case 3:
            {
                var d = (Func<long, long, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<long, long, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 3); return d(((Fixnum)args[0]).Value, ((Fixnum)args[1]).Value, ((Fixnum)args[2]).Value); },
                    name, 3);
                fn._func3 = (a, b, c) => d(((Fixnum)a).Value, ((Fixnum)b).Value, ((Fixnum)c).Value);
                fn._nativeFunc3 = d;
                break;
            }
            case 4:
            {
                var d = (Func<long, long, long, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<long, long, long, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 4); return d(((Fixnum)args[0]).Value, ((Fixnum)args[1]).Value, ((Fixnum)args[2]).Value, ((Fixnum)args[3]).Value); },
                    name, 4);
                fn._func4 = (a, b, c, dd) => d(((Fixnum)a).Value, ((Fixnum)b).Value, ((Fixnum)c).Value, ((Fixnum)dd).Value);
                fn._nativeFunc4 = d;
                break;
            }
            default: throw new Exception("unreachable");
        }

        // SIL storage
        try
        {
            var saveSilSym = Startup.SymInPkg("*SAVE-SIL*", "DOTCL");
            if (DynamicBindings.Get(saveSilSym) is not Nil)
                fn.Sil = bodyInstrs;
        }
        catch { }

        // Symbol registration (same as HandleDefmethodDirect)
        Symbol? pkgSym = null;
        Symbol? setfTargetSym = null;
        if (!name.StartsWith("("))
        {
            try
            {
                Package? pkg = defPkg != null ? Package.FindPackage(defPkg)
                    : DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(name);
                    var homePkg = s.HomePackage;
                    bool isForeignCL = homePkg != null && homePkg != pkg && homePkg.Name == "COMMON-LISP";
                    s.Function = fn;
                    if (!isForeignCL) pkgSym = s;
                }
            }
            catch { }
        }
        else if (name.StartsWith("(SETF ") && name.EndsWith(")"))
        {
            var targetName = name.Substring(6, name.Length - 7);
            try
            {
                Package? pkg = defPkg != null ? Package.FindPackage(defPkg)
                    : DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(targetName);
                    s.SetfFunction = fn;
                    setfTargetSym = s;
                }
            }
            catch { }
        }

        int fnIdx = AddConstant(fn);
        if (pkgSym != null)
        {
            int symIdx = AddConstant(pkgSym);
            _il.Emit(OpCodes.Ldc_I4, symIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(Symbol));
            _il.Emit(OpCodes.Ldc_I4, fnIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispFunction));
            _il.Emit(OpCodes.Call, _registerFunctionOnSymbol);
        }
        if (setfTargetSym != null)
        {
            int setfSymIdx = AddConstant(setfTargetSym);
            _il.Emit(OpCodes.Ldc_I4, setfSymIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(Symbol));
            _il.Emit(OpCodes.Ldc_I4, fnIdx);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispFunction));
            _il.Emit(OpCodes.Call, _registerSetfFunctionOnSymbol);
        }
    }

    private void HandleMakeClosure(Cons instr)
    {
        // Stack already has: object[] env (built by preceding instructions)
        // (:make-closure :param-count N :env-size M :env-map (...) :body (...))
        var plist = instr.Cdr;
        int paramCount = 0;
        int envSize = 0;
        LispObject? bodyInstrs = null;
        LispObject? envMap = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAM-COUNT": paramCount = GetInt(val); break;
                case "ENV-SIZE": envSize = GetInt(val); break;
                case "ENV-MAP": envMap = val; break;
                case "BODY": bodyInstrs = val; break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("MAKE-CLOSURE: missing :body");

        // Determine which env slots are boxed from env-map
        var boxedSlots = new HashSet<int>();
        if (envMap != null)
        {
            var cur = envMap;
            while (cur is Cons mc)
            {
                // Each entry is (name index "boxed"|"value")
                if (mc.Car is Cons entry)
                {
                    var entryList = ListToArray(entry);
                    if (entryList.Length >= 3)
                    {
                        int slotIdx = GetInt(entryList[1]);
                        string kind = GetString(entryList[2]);
                        if (kind == "boxed")
                            boxedSlots.Add(slotIdx);
                    }
                }
                cur = mc.Cdr;
            }
        }

        if (_faslMode && _faslTypeBuilder != null)
        {
            // FASL mode: create a static method on TypeBuilder for the closure body
            int closureId = Interlocked.Increment(ref _faslClosureCount);
            string closureName = $"closure_{closureId}";
            var closureMethod = _faslTypeBuilder.DefineMethod(closureName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject),
                new[] { typeof(object[]), typeof(LispObject[]) });

            var innerAsm = new CilAssembler();
            innerAsm._il = closureMethod.GetILGenerator();
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _faslTypeBuilder;
            innerAsm._faslStructMap = _faslStructMap;
            innerAsm._boxedEnvSlots = boxedSlots;
            innerAsm.Assemble(bodyInstrs);

            // Stack has: object[] env
            // Emit: new LispFunction(closureDel, env, null, paramCount)
            // First store env in a local, then build delegate
            var envLocal = _il.DeclareLocal(typeof(object[]));
            _il.Emit(OpCodes.Stloc, envLocal);

            // new Func<object[], LispObject[], LispObject>(null, &closureMethod)
            _il.Emit(OpCodes.Ldnull);
            _il.Emit(OpCodes.Ldftn, closureMethod);
            var closureDelegateCtor = typeof(Func<object[], LispObject[], LispObject>)
                .GetConstructor(new[] { typeof(object), typeof(IntPtr) })!;
            _il.Emit(OpCodes.Newobj, closureDelegateCtor);

            // Load env
            _il.Emit(OpCodes.Ldloc, envLocal);
            // null name
            _il.Emit(OpCodes.Ldnull);
            // arity
            _il.Emit(OpCodes.Ldc_I4, paramCount);
            // new LispFunction(closureDel, env, name, arity)
            var lispFuncClosureCtor = typeof(LispFunction).GetConstructor(new[] {
                typeof(Func<object[], LispObject[], LispObject>),
                typeof(object[]), typeof(string), typeof(int) })!;
            _il.Emit(OpCodes.Newobj, lispFuncClosureCtor);
        }
        else
        {
            // DynamicMethod mode (original path)
            var dm = new DynamicMethod("lambda_closure", typeof(LispObject),
                new[] { typeof(object[]), typeof(LispObject[]) },
                typeof(CilAssembler).Module, true);
            var innerAsm = new CilAssembler();
            innerAsm._il = dm.GetILGenerator();

            innerAsm._boxedEnvSlots = boxedSlots;
            innerAsm.Assemble(bodyInstrs);

            // Store DynamicMethod in constant pool
            int dmIdx = AddConstant(dm);

            // Stack: object[] env (from caller's instructions)
            // Call MakeClosure(env, dmIdx, paramCount)
            _il.Emit(OpCodes.Ldc_I4, dmIdx);
            _il.Emit(OpCodes.Ldc_I4, paramCount);
            _il.Emit(OpCodes.Call, _makeClosure);
        }
    }

    // For closure body: which env slots hold boxed (LispObject[]) values
    private HashSet<int>? _boxedEnvSlots;

    private void EmitLoadEnv(int index)
    {
        _il.Emit(OpCodes.Ldarg_0); // object[] env
        _il.Emit(OpCodes.Ldc_I4, index);
        _il.Emit(OpCodes.Ldelem_Ref);
        if (_boxedEnvSlots != null && _boxedEnvSlots.Contains(index))
            _il.Emit(OpCodes.Castclass, typeof(LispObject[]));
        else
            _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    // --- Method/field/type resolution ---

    private void EmitLdsfld(string name)
    {
        if (_fieldCache.TryGetValue(name, out var fi))
        {
            _il.Emit(OpCodes.Ldsfld, fi);
            return;
        }
        throw new Exception($"Unknown field: {name}");
    }

    private void EmitCall(string name)
    {
        if (_methodCache.TryGetValue(name, out var mi))
        {
            _il.Emit(OpCodes.Call, mi);
            return;
        }
        throw new Exception($"Unknown method: {name}");
    }

    private void EmitCallvirt(string name)
    {
        if (_methodCache.TryGetValue(name, out var mi))
        {
            _il.Emit(OpCodes.Callvirt, mi);
            return;
        }
        throw new Exception($"Unknown method for callvirt: {name}");
    }

    private void EmitNewobj(string name)
    {
        if (_ctorCache.TryGetValue(name, out var ci))
        {
            _il.Emit(OpCodes.Newobj, ci);
            return;
        }
        throw new Exception($"Unknown constructor: {name}");
    }

    private void EmitCastclass(string name)
    {
        var t = ResolveType(name);
        _il.Emit(OpCodes.Castclass, t);
    }

    private void EmitNewarr(string name)
    {
        var t = ResolveType(name);
        _il.Emit(OpCodes.Newarr, t);
    }

    private void EmitLdarg(int n)
    {
        switch (n)
        {
            case 0: _il.Emit(OpCodes.Ldarg_0); break;
            case 1: _il.Emit(OpCodes.Ldarg_1); break;
            case 2: _il.Emit(OpCodes.Ldarg_2); break;
            case 3: _il.Emit(OpCodes.Ldarg_3); break;
            default: _il.Emit(OpCodes.Ldarg, n); break;
        }
    }

    private void EmitSwitch(LispObject labelList)
    {
        var labels = new List<Label>();
        var cur = labelList;
        while (cur is Cons c)
        {
            labels.Add(GetOrDefineLabel(GetSymbolName(c.Car)));
            cur = c.Cdr;
        }
        _il.Emit(OpCodes.Switch, labels.ToArray());
    }

    private void EmitLoadConst(LispObject val)
    {
        if (_faslMode)
        {
            EmitLoadConstInline(val);
            return;
        }
        // Store the LispObject literal in the constant pool
        int idx = AddConstant(val);
        _il.Emit(OpCodes.Ldc_I4, idx);
        _il.Emit(OpCodes.Call, _getConstant);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    private int _inlineDepth;
    private const int MaxInlineDepth = 500;
    private HashSet<LispStruct>? _inlineVisited;
    private bool _skipStructIntern; // skip intern key for structs inside hash table values
    // Shared across all CilAssemblers in one FASL to deduplicate struct intern keys
    internal FaslStructInternMap? _faslStructMap;

    /// <summary>
    /// Shared map from LispStruct reference identity to short intern keys.
    /// Avoids O(N^2) struct intern key blowup for deep inheritance hierarchies.
    /// </summary>
    internal class FaslStructInternMap
    {
        private readonly Dictionary<LispStruct, string> _map =
            new(ReferenceEqualityComparer.Instance);
        private int _counter;
        private readonly string _prefix;
        // Track unique strings emitted via Ldstr for this assembly
        private readonly HashSet<string> _uniqueStrings = new();
        public long UniqueStringBytes { get; private set; }

        // Per-FASL uninterned symbol deduplication: same Symbol object → same static field.
        // Set by FaslAssembler after construction.
        internal System.Reflection.Emit.TypeBuilder? UninternedTypeBuilder;
        internal System.Reflection.Emit.ILGenerator? UninternedInitIl;
        private readonly Dictionary<Symbol, System.Reflection.Emit.FieldBuilder> _uninternedFields =
            new(ReferenceEqualityComparer.Instance);
        private static readonly System.Reflection.ConstructorInfo _symbolCtor2 =
            typeof(Symbol).GetConstructor(new[] { typeof(string), typeof(Package) })!;
        private int _uninternedCounter;

        public FaslStructInternMap(string modulePrefix)
        {
            _prefix = modulePrefix;
        }

        public string GetOrCreate(LispStruct ls)
        {
            if (!_map.TryGetValue(ls, out var key))
            {
                key = _prefix + "." + (_counter++);
                _map[ls] = key;
                LispStruct.PreRegisterIntern(key, ls);
            }
            return key;
        }

        /// <summary>
        /// Get or create a static field for an uninterned symbol so that all uses within
        /// this FASL resolve to the SAME Symbol object (preserves EQ-ness across make-load-form).
        /// </summary>
        public System.Reflection.Emit.FieldBuilder GetOrCreateUninternedSymbolField(Symbol sym)
        {
            if (_uninternedFields.TryGetValue(sym, out var field))
                return field;
            var tb = UninternedTypeBuilder!;
            var il = UninternedInitIl!;
            field = tb.DefineField($"_gsym_{_uninternedCounter++}",
                typeof(Symbol), System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            // Emit init: new Symbol(name, null); stsfld field
            il.Emit(System.Reflection.Emit.OpCodes.Ldstr, sym.Name);
            il.Emit(System.Reflection.Emit.OpCodes.Ldnull);
            il.Emit(System.Reflection.Emit.OpCodes.Newobj, _symbolCtor2);
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, field);
            _uninternedFields[sym] = field;
            return field;
        }

        /// <summary>Track a string being emitted via Ldstr. Returns the string.</summary>
        public string TrackString(string s)
        {
            if (_uniqueStrings.Add(s))
                UniqueStringBytes += (long)(s.Length * 2 + 4);
            return s;
        }
    }

    private string Track(string s) =>
        _faslStructMap?.TrackString(s) ?? s;

    /// <summary>Emit IL to construct a constant value inline (for FASL mode).</summary>
    internal void EmitLoadConstInline(LispObject val)
    {
        _inlineDepth++;
        try
        {
        if (_inlineDepth > MaxInlineDepth)
        {
            // Too deep — fallback to constant pool (won't work across processes)
            int idx2 = AddConstant(val);
            _il.Emit(OpCodes.Ldc_I4, idx2);
            _il.Emit(OpCodes.Call, _getConstant);
            _il.Emit(OpCodes.Castclass, typeof(LispObject));
            return;
        }
        switch (val)
        {
            case Nil:
                _il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
                break;
            case T:
                _il.Emit(OpCodes.Ldsfld, typeof(T).GetField("Instance")!);
                break;
            case Fixnum f:
                _il.Emit(OpCodes.Ldc_I8, f.Value);
                _il.Emit(OpCodes.Call, _methodCache["Fixnum.Make"]);
                break;
            case LispString s:
                _il.Emit(OpCodes.Ldstr, Track(s.Value));
                _il.Emit(OpCodes.Newobj, _ctorCache["LispString"]);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case Symbol sym:
                if (sym.HomePackage?.Name == "KEYWORD")
                {
                    _il.Emit(OpCodes.Ldstr, Track(sym.Name));
                    _il.Emit(OpCodes.Call, _methodCache["Startup.Keyword"]);
                }
                else if (sym.HomePackage != null)
                {
                    _il.Emit(OpCodes.Ldstr, Track(sym.Name));
                    _il.Emit(OpCodes.Ldstr, Track(sym.HomePackage.Name));
                    _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
                }
                else if (_faslMode && _faslStructMap?.UninternedTypeBuilder != null)
                {
                    // FASL mode: uninterned symbols must be deduplicated so the same
                    // Symbol object is used everywhere in this assembly (preserves EQ-ness
                    // across make-load-form boundaries — e.g. gensym'd ctor names in defcontext).
                    var field = _faslStructMap.GetOrCreateUninternedSymbolField(sym);
                    _il.Emit(OpCodes.Ldsfld, field);
                }
                else
                {
                    // AssembleAndRun (non-FASL): each occurrence creates a new Symbol.
                    _il.Emit(OpCodes.Ldstr, Track(sym.Name));
                    _il.Emit(OpCodes.Ldnull); // null for homePackage
                    _il.Emit(OpCodes.Newobj, typeof(Symbol).GetConstructor(new[] { typeof(string), typeof(Package) })!);
                }
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispChar lc:
                _il.Emit(OpCodes.Ldc_I4, (int)lc.Value);
                _il.Emit(OpCodes.Call, typeof(LispChar).GetMethod("Make")!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case SingleFloat sf:
                _il.Emit(OpCodes.Ldc_R4, sf.Value);
                _il.Emit(OpCodes.Newobj, typeof(SingleFloat).GetConstructor(new[] { typeof(float) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case DoubleFloat df:
                _il.Emit(OpCodes.Ldc_R8, df.Value);
                _il.Emit(OpCodes.Newobj, typeof(DoubleFloat).GetConstructor(new[] { typeof(double) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case Cons cons:
            {
                // Flatten proper list to avoid deep CDR recursion
                var cars = new List<LispObject>();
                LispObject tail = cons;
                while (tail is Cons c) { cars.Add(c.Car); tail = c.Cdr; }
                var consCtor = typeof(Cons).GetConstructor(new[] { typeof(LispObject), typeof(LispObject) })!;
                // Emit tail (nil for proper list, or dotted cdr)
                EmitLoadConstInline(tail);
                // Build cons cells right-to-left: stack has accumulated cdr
                for (int ci = cars.Count - 1; ci >= 0; ci--)
                {
                    // Stack: cdr; need car on top for new Cons(car, cdr)
                    // Use a temp local to swap
                    var tmpName = $"__cons_tmp_{_inlineDepth}_{ci}";
                    if (!_locals.TryGetValue(tmpName, out var tmpLocal))
                    {
                        tmpLocal = _il.DeclareLocal(typeof(LispObject));
                        _locals[tmpName] = tmpLocal;
                    }
                    _il.Emit(OpCodes.Stloc, tmpLocal); // save cdr
                    EmitLoadConstInline(cars[ci]);       // push car
                    _il.Emit(OpCodes.Ldloc, tmpLocal);   // push cdr
                    _il.Emit(OpCodes.Newobj, consCtor);
                }
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            }
            case LispVector vec:
                // Build vector inline.
                // Use ElementAt()/Length so bit-vectors work: their elements live in
                // _bitData, not _elements (_elements is empty for bit-vectors).
                int vecLen = vec.Length;
                _il.Emit(OpCodes.Ldc_I4, vecLen);
                _il.Emit(OpCodes.Newarr, typeof(LispObject));
                for (int i = 0; i < vecLen; i++)
                {
                    _il.Emit(OpCodes.Dup);
                    _il.Emit(OpCodes.Ldc_I4, i);
                    EmitLoadConstInline(vec.ElementAt(i));
                    _il.Emit(OpCodes.Stelem_Ref);
                }
                if (vec._dimensions != null)
                {
                    // Multi-dimensional array: preserve rank/dimensions via the
                    // (LispObject[], int[], string) ctor. Without this, #2A(...)
                    // literals in compile-file output come back as flat 1D
                    // vectors (D736 / issue #149).
                    var dims = vec._dimensions;
                    _il.Emit(OpCodes.Ldc_I4, dims.Length);
                    _il.Emit(OpCodes.Newarr, typeof(int));
                    for (int di = 0; di < dims.Length; di++)
                    {
                        _il.Emit(OpCodes.Dup);
                        _il.Emit(OpCodes.Ldc_I4, di);
                        _il.Emit(OpCodes.Ldc_I4, dims[di]);
                        _il.Emit(OpCodes.Stelem_I4);
                    }
                    _il.Emit(OpCodes.Ldstr, vec.ElementTypeName);
                    _il.Emit(OpCodes.Newobj,
                        typeof(LispVector).GetConstructor(
                            new[] { typeof(LispObject[]), typeof(int[]), typeof(string) })!);
                }
                else if (vec.ElementTypeName != "T")
                {
                    // 1D vector with custom element-type (SINGLE-FLOAT, BIT, etc.):
                    // preserve via (LispObject[], string) ctor. Keeps _dimensions null
                    // so the vector stays a proper 1D type (issue #150).
                    _il.Emit(OpCodes.Ldstr, vec.ElementTypeName);
                    _il.Emit(OpCodes.Newobj,
                        typeof(LispVector).GetConstructor(
                            new[] { typeof(LispObject[]), typeof(string) })!);
                }
                else
                {
                    _il.Emit(OpCodes.Newobj, _ctorCache["LispVector"]);
                }
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case Bignum bn:
                // BigInteger.Parse(string) -> new Bignum(BigInteger)
                _il.Emit(OpCodes.Ldstr, bn.Value.ToString());
                _il.Emit(OpCodes.Call, typeof(System.Numerics.BigInteger).GetMethod("Parse", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Newobj, typeof(Bignum).GetConstructor(new[] { typeof(System.Numerics.BigInteger) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case Ratio rat:
                // Ratio.Make(BigInteger.Parse(num), BigInteger.Parse(den))
                _il.Emit(OpCodes.Ldstr, rat.Numerator.ToString());
                _il.Emit(OpCodes.Call, typeof(System.Numerics.BigInteger).GetMethod("Parse", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Ldstr, rat.Denominator.ToString());
                _il.Emit(OpCodes.Call, typeof(System.Numerics.BigInteger).GetMethod("Parse", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Call, typeof(Ratio).GetMethod("Make")!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispPathname pn:
                // Build via LispPathname.FromString(namestring)
                _il.Emit(OpCodes.Ldstr, pn.ToNamestring());
                _il.Emit(OpCodes.Call, typeof(LispPathname).GetMethod("FromString", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispStruct ls:
                _inlineVisited ??= new HashSet<LispStruct>(ReferenceEqualityComparer.Instance);
                if (!_inlineVisited.Add(ls))
                {
                    // Circular reference — fall back to constant pool
                    int idxCyc = AddConstant(val);
                    _il.Emit(OpCodes.Ldc_I4, idxCyc);
                    _il.Emit(OpCodes.Call, _getConstant);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                    break;
                }
                try
                {
                    if (_skipStructIntern)
                    {
                        // Inside hash table values: skip interning to save string space
                        EmitLoadConstInline(ls.TypeName);
                        var slotsArr2 = ls.Slots;
                        _il.Emit(OpCodes.Ldc_I4, slotsArr2.Length);
                        _il.Emit(OpCodes.Newarr, typeof(LispObject));
                        for (int si = 0; si < slotsArr2.Length; si++)
                        {
                            _il.Emit(OpCodes.Dup);
                            _il.Emit(OpCodes.Ldc_I4, si);
                            EmitLoadConstInline(slotsArr2[si]);
                            _il.Emit(OpCodes.Stelem_Ref);
                        }
                        _il.Emit(OpCodes.Newobj, typeof(LispStruct).GetConstructor(
                            new[] { typeof(Symbol), typeof(LispObject[]) })!);
                        _il.Emit(OpCodes.Castclass, typeof(LispObject));
                    }
                    else
                    {
                        // Top-level: use intern cache for EQ preservation
                        // Use short reference-identity-based key if available (avoids huge keys
                        // for deeply nested struct hierarchies that blow up the UserString heap)
                        string internKey = _faslStructMap != null
                            ? _faslStructMap.GetOrCreate(ls)
                            : ComputeStructInternKey(ls);
                        if (_faslStructMap == null)
                            LispStruct.PreRegisterIntern(internKey, ls);
                        _il.Emit(OpCodes.Ldstr, Track(internKey));
                        EmitLoadConstInline(ls.TypeName);
                        var slotsArr = ls.Slots;
                        _il.Emit(OpCodes.Ldc_I4, slotsArr.Length);
                        _il.Emit(OpCodes.Newarr, typeof(LispObject));
                        for (int si = 0; si < slotsArr.Length; si++)
                        {
                            _il.Emit(OpCodes.Dup);
                            _il.Emit(OpCodes.Ldc_I4, si);
                            EmitLoadConstInline(slotsArr[si]);
                            _il.Emit(OpCodes.Stelem_Ref);
                        }
                        _il.Emit(OpCodes.Call, typeof(LispStruct).GetMethod("Intern",
                            new[] { typeof(string), typeof(LispObject), typeof(LispObject[]) })!);
                        _il.Emit(OpCodes.Castclass, typeof(LispObject));
                    }
                }
                finally { _inlineVisited.Remove(ls); }
                break;
            case Package pkg:
                // Build via Package.FindPackage(name)
                _il.Emit(OpCodes.Ldstr, pkg.Name);
                _il.Emit(OpCodes.Call, typeof(Package).GetMethod("FindPackage", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispHashTable ht:
            {
                // Small hash tables: inline construction (≤20 entries)
                // Large ones: in FASL mode, emit as helper methods (self-contained, no _constants).
                // In non-FASL (AssembleAndRun), fall back to constant pool (same-process, ok).
                const int MaxHtInline = 20;
                var htEntries = ht.Entries.ToList();
                if (htEntries.Count > MaxHtInline)
                {
                    if (_faslMode && _faslTypeBuilder != null)
                    {
                        EmitFaslLargeHashTable(ht, htEntries);
                    }
                    else
                    {
                        int idxHt = AddConstant(val);
                        _il.Emit(OpCodes.Ldc_I4, idxHt);
                        _il.Emit(OpCodes.Call, _getConstant);
                        _il.Emit(OpCodes.Castclass, typeof(LispObject));
                    }
                    break;
                }
                // Inline hash table: create empty, populate entries
                // Skip struct interning inside HT values to save UserString heap
                bool prevSkip = _skipStructIntern;
                _skipStructIntern = true;
                try
                {
                    _il.Emit(OpCodes.Ldstr, ht.TestName);
                    _il.Emit(OpCodes.Newobj, typeof(LispHashTable).GetConstructor(new[] { typeof(string) })!);
                    var setMethod = typeof(LispHashTable).GetMethod("Set",
                        new[] { typeof(LispObject), typeof(LispObject) })!;
                    foreach (var entry in htEntries)
                    {
                        _il.Emit(OpCodes.Dup);
                        EmitLoadConstInline(entry.Key);
                        EmitLoadConstInline(entry.Value);
                        _il.Emit(OpCodes.Callvirt, setMethod);
                    }
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                finally { _skipStructIntern = prevSkip; }
                break;
            }
            case LispFunction fn:
            {
                // Look up named function by symbol at load time
                if (fn.Name != null)
                {
                    _il.Emit(OpCodes.Ldstr, fn.Name);
                    _il.Emit(OpCodes.Call, typeof(CilAssembler).GetMethod("GetFunction", new[] { typeof(string) })!);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                else
                {
                    // Anonymous function — fall back to constant pool
                    int idxFn = AddConstant(val);
                    _il.Emit(OpCodes.Ldc_I4, idxFn);
                    _il.Emit(OpCodes.Call, _getConstant);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                break;
            }
            default:
                // Fallback: use constant pool (won't work across processes, but
                // allows compilation to proceed for unsupported constant types)
                try { Console.Error.WriteLine($"[FASL WARNING] Unsupported constant type in fasl: {val.GetType().Name}"); }
                catch { /* ignore ToString errors */ }
                int idx = AddConstant(val);
                _il.Emit(OpCodes.Ldc_I4, idx);
                _il.Emit(OpCodes.Call, _getConstant);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
        }
        }
        finally { _inlineDepth--; }
    }

    /// <summary>
    /// FASL mode: emit a large hash table as a set of static helper methods in the FASL assembly.
    /// Splits entries into chunks to stay within .NET IL method size limits (~64KB body).
    /// Each chunk method takes a LispHashTable, populates a slice, returns void.
    /// The top-level method creates the HT and calls all chunk methods.
    /// This makes the FASL self-contained (no _constants cross-process dependency).
    /// </summary>
    private void EmitFaslLargeHashTable(LispHashTable ht, List<KeyValuePair<LispObject, LispObject>> entries)
    {
        const int ChunkSize = 1500; // ~1500 entries × ~30 bytes/entry = ~45KB, safely under 64KB IL limit
        int htId = Interlocked.Increment(ref _faslClosureCount);
        var setMethod = typeof(LispHashTable).GetMethod("Set",
            new[] { typeof(LispObject), typeof(LispObject) })!;
        var htType = typeof(LispHashTable);
        var htCtor = htType.GetConstructor(new[] { typeof(string) })!;

        // Emit chunk methods: static void _ht_N_chunk_C(LispHashTable ht)
        var chunkMethods = new List<MethodBuilder>();
        for (int chunkStart = 0; chunkStart < entries.Count; chunkStart += ChunkSize)
        {
            int chunkEnd = Math.Min(chunkStart + ChunkSize, entries.Count);
            int chunkId = chunkMethods.Count;
            var chunkMethod = _faslTypeBuilder!.DefineMethod(
                $"_ht_{htId}_chunk_{chunkId}",
                MethodAttributes.Private | MethodAttributes.Static,
                typeof(void), new[] { typeof(LispHashTable) });
            chunkMethods.Add(chunkMethod);

            var chunkIl = chunkMethod.GetILGenerator();
            // Create a temporary inner assembler to emit keys/values into this method
            var inner = new CilAssembler();
            inner._il = chunkIl;
            inner._faslMode = true;
            inner._faslTypeBuilder = _faslTypeBuilder;
            inner._faslStructMap = _faslStructMap;
            inner._skipStructIntern = true;

            for (int i = chunkStart; i < chunkEnd; i++)
            {
                chunkIl.Emit(OpCodes.Ldarg_0);          // push ht
                inner.EmitLoadConstInline(entries[i].Key);
                inner.EmitLoadConstInline(entries[i].Value);
                chunkIl.Emit(OpCodes.Callvirt, setMethod);
            }
            chunkIl.Emit(OpCodes.Ret);
        }

        // Emit top-level builder: static LispHashTable _ht_N()
        var builderMethod = _faslTypeBuilder!.DefineMethod(
            $"_ht_{htId}",
            MethodAttributes.Private | MethodAttributes.Static,
            typeof(LispHashTable), Type.EmptyTypes);
        var builderIl = builderMethod.GetILGenerator();
        builderIl.Emit(OpCodes.Ldstr, ht.TestName);
        builderIl.Emit(OpCodes.Newobj, htCtor);
        var htLocal = builderIl.DeclareLocal(typeof(LispHashTable));
        builderIl.Emit(OpCodes.Stloc, htLocal);
        foreach (var chunkMethod in chunkMethods)
        {
            builderIl.Emit(OpCodes.Ldloc, htLocal);
            builderIl.Emit(OpCodes.Call, chunkMethod);
        }
        builderIl.Emit(OpCodes.Ldloc, htLocal);
        builderIl.Emit(OpCodes.Ret);

        // Call the builder method from the current context
        _il.Emit(OpCodes.Call, builderMethod);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    /// <summary>Compute a deterministic content key for LispStruct interning.</summary>
    private static string ComputeStructInternKey(LispStruct ls)
    {
        var sb = new System.Text.StringBuilder();
        var visited = new HashSet<LispStruct>(ReferenceEqualityComparer.Instance);
        AppendInternKey(sb, ls, 0, visited);
        return sb.ToString();
    }

    private static void AppendInternKey(System.Text.StringBuilder sb, LispObject obj, int depth, HashSet<LispStruct> visited)
    {
        if (depth > 20) { sb.Append("…"); return; }
        switch (obj)
        {
            case Nil: sb.Append("N"); break;
            case T: sb.Append("T"); break;
            case Fixnum f: sb.Append('I'); sb.Append(f.Value); break;
            case LispString s: sb.Append('"'); sb.Append(s.Value); sb.Append('"'); break;
            case LispChar c: sb.Append('\\'); sb.Append((int)c.Value); break;
            case SingleFloat sf: sb.Append('F'); sb.Append(BitConverter.SingleToInt32Bits(sf.Value)); break;
            case DoubleFloat df: sb.Append('D'); sb.Append(BitConverter.DoubleToInt64Bits(df.Value)); break;
            case Symbol sym:
                sb.Append(sym.HomePackage?.Name ?? "#");
                sb.Append(':');
                sb.Append(sym.Name);
                break;
            case LispStruct ls:
                if (!visited.Add(ls)) { sb.Append("#CYCLE"); break; }
                sb.Append("#S(");
                AppendInternKey(sb, ls.TypeName, depth + 1, visited);
                foreach (var slot in ls.Slots)
                {
                    sb.Append(' ');
                    AppendInternKey(sb, slot, depth + 1, visited);
                }
                sb.Append(')');
                visited.Remove(ls);
                break;
            case Cons cons:
                sb.Append('(');
                AppendInternKey(sb, cons.Car, depth + 1, visited);
                sb.Append('.');
                AppendInternKey(sb, cons.Cdr, depth + 1, visited);
                sb.Append(')');
                break;
            case Bignum bn: sb.Append('B'); sb.Append(bn.Value); break;
            case Ratio rat: sb.Append('R'); sb.Append(rat.Numerator); sb.Append('/'); sb.Append(rat.Denominator); break;
            case LispVector vec:
                sb.Append("#(");
                for (int i = 0; i < vec._elements.Length; i++)
                {
                    if (i > 0) sb.Append(' ');
                    AppendInternKey(sb, vec._elements[i], depth + 1, visited);
                }
                sb.Append(')');
                break;
            default:
                sb.Append(obj.GetType().Name);
                sb.Append('#');
                sb.Append(obj.GetHashCode());
                break;
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

    private LocalBuilder GetLocal(string name)
    {
        if (_locals.TryGetValue(name, out var local)) return local;
        throw new Exception($"Undeclared local: {name}");
    }

    private void DeclareLocal(string name, string typeName)
    {
        if (_locals.ContainsKey(name)) return; // already declared
        var t = ResolveType(typeName);
        var local = _il.DeclareLocal(t);
        _locals[name] = local;
    }

    // --- Type resolution ---

    private static Type ResolveType(string name)
    {
        if (_typeCache.TryGetValue(name, out var t)) return t;
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

    // --- Static resolution tables ---

    private static readonly Dictionary<string, MethodInfo> _methodCache;
    private static readonly Dictionary<string, FieldInfo> _fieldCache;
    private static readonly Dictionary<string, ConstructorInfo> _ctorCache;
    private static readonly Dictionary<string, Type> _typeCache;
    private static readonly MethodInfo _getConstant;
    private static readonly MethodInfo _makeClosure;
    private static readonly MethodInfo _registerFunction;
    private static readonly MethodInfo _registerFunctionOnSymbol;
    private static readonly MethodInfo _registerSetfFunctionOnSymbol;

    static CilAssembler()
    {
        _methodCache = new Dictionary<string, MethodInfo>
        {
            // Fixnum
            ["Fixnum.Make"] = typeof(Fixnum).GetMethod("Make")!,

            // LispChar
            ["LispChar.Make"] = typeof(LispChar).GetMethod("Make")!,

            // Runtime - arithmetic
            ["Runtime.Add"] = typeof(Runtime).GetMethod("Add")!,
            ["Runtime.Subtract"] = typeof(Runtime).GetMethod("Subtract")!,
            ["Runtime.Increment"] = typeof(Runtime).GetMethod("Increment")!,
            ["Runtime.Decrement"] = typeof(Runtime).GetMethod("Decrement")!,
            ["Runtime.Multiply"] = typeof(Runtime).GetMethod("Multiply")!,
            ["Runtime.MultiplyFixnum"] = typeof(Runtime).GetMethod("MultiplyFixnum")!,
            ["Runtime.Divide"] = typeof(Runtime).GetMethod("Divide")!,
            ["Runtime.AddN"] = typeof(Runtime).GetMethod("AddN")!,
            ["Runtime.SubtractN"] = typeof(Runtime).GetMethod("SubtractN")!,
            ["Runtime.MultiplyN"] = typeof(Runtime).GetMethod("MultiplyN")!,
            ["Runtime.DivideN"] = typeof(Runtime).GetMethod("DivideN")!,

            // Runtime - comparison
            ["Runtime.GreaterThan"] = typeof(Runtime).GetMethod("GreaterThan")!,
            ["Runtime.LessThan"] = typeof(Runtime).GetMethod("LessThan")!,
            ["Runtime.GreaterEqual"] = typeof(Runtime).GetMethod("GreaterEqual")!,
            ["Runtime.LessEqual"] = typeof(Runtime).GetMethod("LessEqual")!,
            ["Runtime.NumEqual"] = typeof(Runtime).GetMethod("NumEqual")!,
            ["Runtime.NumNotEqual"] = typeof(Runtime).GetMethod("NumNotEqual")!,
            ["Runtime.NumEqualN"] = typeof(Runtime).GetMethod("NumEqualN")!,
            ["Runtime.NumNotEqualN"] = typeof(Runtime).GetMethod("NumNotEqualN")!,
            ["Runtime.LessThanN"] = typeof(Runtime).GetMethod("LessThanN")!,
            ["Runtime.GreaterThanN"] = typeof(Runtime).GetMethod("GreaterThanN")!,
            ["Runtime.LessEqualN"] = typeof(Runtime).GetMethod("LessEqualN")!,
            ["Runtime.GreaterEqualN"] = typeof(Runtime).GetMethod("GreaterEqualN")!,

            // Runtime - bool-returning comparisons (fused comparison+branch)
            ["Runtime.IsTrueGt"] = typeof(Runtime).GetMethod("IsTrueGt")!,
            ["Runtime.IsTrueLt"] = typeof(Runtime).GetMethod("IsTrueLt")!,
            ["Runtime.IsTrueGe"] = typeof(Runtime).GetMethod("IsTrueGe")!,
            ["Runtime.IsTrueLe"] = typeof(Runtime).GetMethod("IsTrueLe")!,
            ["Runtime.IsTrueNumEq"] = typeof(Runtime).GetMethod("IsTrueNumEq")!,
            ["Runtime.IsTrueZerop"] = typeof(Runtime).GetMethod("IsTrueZerop")!,
            ["Runtime.IsTrueMinusp"] = typeof(Runtime).GetMethod("IsTrueMinusp")!,
            ["Runtime.IsTruePlusp"] = typeof(Runtime).GetMethod("IsTruePlusp")!,
            ["Runtime.IsTrueEq"] = typeof(Runtime).GetMethod("IsTrueEq")!,
            ["Runtime.IsTrueEql"] = typeof(Runtime).GetMethod("IsTrueEql")!,
            ["Runtime.IsTrueEqual"] = typeof(Runtime).GetMethod("IsTrueEqual")!,
            ["Runtime.IsTrueTypep"] = typeof(Runtime).GetMethod("IsTrueTypep")!,
            ["Runtime.IsTrueConsp"] = typeof(Runtime).GetMethod("IsTrueConsp")!,
            ["Runtime.IsTrueAtom"] = typeof(Runtime).GetMethod("IsTrueAtom")!,
            ["Runtime.IsTrueListp"] = typeof(Runtime).GetMethod("IsTrueListp")!,
            ["Runtime.IsTrueNumberp"] = typeof(Runtime).GetMethod("IsTrueNumberp")!,
            ["Runtime.IsTrueIntegerp"] = typeof(Runtime).GetMethod("IsTrueIntegerp")!,
            ["Runtime.IsTrueSymbolp"] = typeof(Runtime).GetMethod("IsTrueSymbolp")!,
            ["Runtime.IsTrueStringp"] = typeof(Runtime).GetMethod("IsTrueStringp")!,
            ["Runtime.IsTrueCharacterp"] = typeof(Runtime).GetMethod("IsTrueCharacterp")!,
            ["Runtime.IsTrueFunctionp"] = typeof(Runtime).GetMethod("IsTrueFunctionp")!,

            // Runtime - equality
            ["Runtime.Eq"] = typeof(Runtime).GetMethod("Eq")!,
            ["Runtime.Eql"] = typeof(Runtime).GetMethod("Eql")!,
            ["Runtime.Equal"] = typeof(Runtime).GetMethod("Equal")!,

            // Runtime - list ops
            ["Runtime.Car"] = typeof(Runtime).GetMethod("Car")!,
            ["Runtime.Cdr"] = typeof(Runtime).GetMethod("Cdr")!,
            ["Runtime.MakeCons"] = typeof(Runtime).GetMethod("MakeCons")!,
            ["Runtime.CheckUnaryArity"] = typeof(Runtime).GetMethod("CheckUnaryArity")!,
            ["Runtime.CheckBinaryArity"] = typeof(Runtime).GetMethod("CheckBinaryArity")!,
            ["Runtime.ProgramError"] = typeof(Runtime).GetMethod("ProgramError", new[] { typeof(string) })!,
            ["Runtime.CheckArityExact"] = typeof(Runtime).GetMethod("CheckArityExact")!,
            ["Runtime.CheckArityMin"] = typeof(Runtime).GetMethod("CheckArityMin")!,
            ["Runtime.CheckArityMax"] = typeof(Runtime).GetMethod("CheckArityMax")!,
            ["Runtime.CheckNoUnknownKeys"] = typeof(Runtime).GetMethod("CheckNoUnknownKeys")!,
            ["Runtime.CheckNoUnknownKeys2"] = typeof(Runtime).GetMethod("CheckNoUnknownKeys2")!,
            ["Runtime.List"] = typeof(Runtime).GetMethod("List")!,
            ["Runtime.ListStar"] = typeof(Runtime).GetMethod("ListStar")!,
            ["Runtime.Append"] = typeof(Runtime).GetMethod("Append")!,
            ["Runtime.Length"] = typeof(Runtime).GetMethod("Length")!,
            ["Runtime.Rplaca"] = typeof(Runtime).GetMethod("Rplaca")!,
            ["Runtime.Rplacd"] = typeof(Runtime).GetMethod("Rplacd")!,
            ["Runtime.Nreverse"] = typeof(Runtime).GetMethod("Nreverse")!,
            ["Runtime.Nth"] = typeof(Runtime).GetMethod("Nth")!,
            ["Runtime.Nthcdr"] = typeof(Runtime).GetMethod("Nthcdr")!,
            ["Runtime.Last"] = typeof(Runtime).GetMethod("Last")!,
            ["Runtime.Nconc2"] = typeof(Runtime).GetMethod("Nconc2")!,
            ["Runtime.Butlast"] = typeof(Runtime).GetMethod("Butlast")!,
            ["Runtime.CopyList"] = typeof(Runtime).GetMethod("CopyList")!,
            ["Runtime.Member"] = typeof(Runtime).GetMethod("Member")!,
            ["Runtime.MemberEq"] = typeof(Runtime).GetMethod("MemberEq")!,
            ["Runtime.Assoc"] = typeof(Runtime).GetMethod("Assoc")!,
            ["Runtime.AssocEq"] = typeof(Runtime).GetMethod("AssocEq")!,
            ["Runtime.Cadr"] = typeof(Runtime).GetMethod("Cadr")!,
            ["Runtime.Cddr"] = typeof(Runtime).GetMethod("Cddr")!,
            ["Runtime.Caar"] = typeof(Runtime).GetMethod("Caar")!,
            ["Runtime.Cdar"] = typeof(Runtime).GetMethod("Cdar")!,
            ["Runtime.Caddr"] = typeof(Runtime).GetMethod("Caddr")!,

            // Runtime - predicates
            ["Runtime.Not"] = typeof(Runtime).GetMethod("Not")!,
            ["Runtime.Atom"] = typeof(Runtime).GetMethod("Atom")!,
            ["Runtime.Consp"] = typeof(Runtime).GetMethod("Consp")!,
            ["Runtime.Listp"] = typeof(Runtime).GetMethod("Listp")!,
            ["Runtime.Numberp"] = typeof(Runtime).GetMethod("Numberp")!,
            ["Runtime.Integerp"] = typeof(Runtime).GetMethod("Integerp")!,
            ["Runtime.Symbolp"] = typeof(Runtime).GetMethod("Symbolp")!,
            ["Runtime.Stringp"] = typeof(Runtime).GetMethod("Stringp")!,
            ["Runtime.Characterp"] = typeof(Runtime).GetMethod("Characterp")!,
            ["Runtime.Functionp"] = typeof(Runtime).GetMethod("Functionp")!,
            ["Runtime.Rationalp"] = typeof(Runtime).GetMethod("Rationalp")!,
            ["Runtime.Floatp"] = typeof(Runtime).GetMethod("Floatp")!,
            ["Runtime.Complexp"] = typeof(Runtime).GetMethod("Complexp")!,
            ["Runtime.Vectorp"] = typeof(Runtime).GetMethod("Vectorp")!,
            ["Runtime.Hash_table_p"] = typeof(Runtime).GetMethod("Hash_table_p")!,
            ["Runtime.Packagep"] = typeof(Runtime).GetMethod("Packagep")!,
            ["Runtime.Keywordp"] = typeof(Runtime).GetMethod("Keywordp")!,
            ["Runtime.TypeOf"] = typeof(Runtime).GetMethod("TypeOf")!,
            ["Runtime.Typep"] = typeof(Runtime).GetMethod("Typep")!,
            ["Runtime.SetChar"] = typeof(Runtime).GetMethod("SetChar")!,
            ["Runtime.SetElt"] = typeof(Runtime).GetMethod("SetElt")!,
            ["Runtime.Putf"] = typeof(Runtime).GetMethod("Putf")!,
            ["Runtime.IsTruthy"] = typeof(Runtime).GetMethod("IsTruthy")!,
            ["Runtime.CharAccess"] = typeof(Runtime).GetMethod("CharAccess")!,
            ["Runtime.CharSet"] = typeof(Runtime).GetMethod("CharSet")!,
            ["Runtime.CharEqual"] = typeof(Runtime).GetMethod("CharEqual")!,
            ["Runtime.Maphash"] = typeof(Runtime).GetMethod("Maphash")!,

            // Runtime - math
            ["Runtime.Abs"] = typeof(Runtime).GetMethod("Abs")!,
            ["Runtime.Mod"] = typeof(Runtime).GetMethod("Mod")!,
            ["Runtime.Rem"] = typeof(Runtime).GetMethod("Rem")!,
            ["Runtime.FloorOp"] = typeof(Runtime).GetMethod("FloorOp")!,
            ["Runtime.TruncateOp"] = typeof(Runtime).GetMethod("TruncateOp")!,
            ["Runtime.CeilingOp"] = typeof(Runtime).GetMethod("CeilingOp")!,
            ["Runtime.RoundOp"] = typeof(Runtime).GetMethod("RoundOp")!,
            ["Runtime.Min"] = typeof(Runtime).GetMethod("Min")!,
            ["Runtime.Max"] = typeof(Runtime).GetMethod("Max")!,
            ["Runtime.Gcd"] = typeof(Runtime).GetMethod("Gcd")!,
            ["Runtime.Lcm"] = typeof(Runtime).GetMethod("Lcm")!,
            ["Runtime.Expt"] = typeof(Runtime).GetMethod("Expt")!,
            ["Runtime.Logior"] = typeof(Runtime).GetMethod("Logior")!,
            ["Runtime.Logand"] = typeof(Runtime).GetMethod("Logand")!,
            ["Runtime.Logxor"] = typeof(Runtime).GetMethod("Logxor")!,
            ["Runtime.Logior2"] = typeof(Runtime).GetMethod("Logior2")!,
            ["Runtime.Logand2"] = typeof(Runtime).GetMethod("Logand2")!,
            ["Runtime.Logxor2"] = typeof(Runtime).GetMethod("Logxor2")!,
            ["Runtime.Lognot"] = typeof(Runtime).GetMethod("Lognot")!,
            ["Runtime.Ash"] = typeof(Runtime).GetMethod("Ash")!,
            ["Runtime.IntegerLength"] = typeof(Runtime).GetMethod("IntegerLength")!,
            ["Runtime.Logbitp"] = typeof(Runtime).GetMethod("Logbitp")!,

            // Runtime - I/O
            ["Runtime.Print"] = typeof(Runtime).GetMethod("Print")!,
            ["Runtime.Print2"] = typeof(Runtime).GetMethod("Print2")!,
            ["Runtime.Prin1"] = typeof(Runtime).GetMethod("Prin1")!,
            ["Runtime.Prin12"] = typeof(Runtime).GetMethod("Prin12")!,
            ["Runtime.Princ"] = typeof(Runtime).GetMethod("Princ")!,
            ["Runtime.Princ2"] = typeof(Runtime).GetMethod("Princ2")!,
            ["Runtime.Terpri"] = typeof(Runtime).GetMethod("Terpri")!,
            ["Runtime.FreshLine"] = typeof(Runtime).GetMethod("FreshLine")!,

            // Runtime - string
            ["Runtime.StringUpcase"] = typeof(Runtime).GetMethod("StringUpcase")!,
            ["Runtime.StringDowncase"] = typeof(Runtime).GetMethod("StringDowncase")!,
            ["Runtime.StringTrim"] = typeof(Runtime).GetMethod("StringTrim")!,
            ["Runtime.StringLeftTrim"] = typeof(Runtime).GetMethod("StringLeftTrim")!,
            ["Runtime.StringRightTrim"] = typeof(Runtime).GetMethod("StringRightTrim")!,
            ["Runtime.CharCode"] = typeof(Runtime).GetMethod("CharCode")!,
            ["Runtime.CodeChar"] = typeof(Runtime).GetMethod("CodeChar")!,
            ["Runtime.DigitCharP"] = typeof(Runtime).GetMethod("DigitCharP")!,

            // Runtime - symbol
            ["Runtime.SymbolName"] = typeof(Runtime).GetMethod("SymbolName")!,
            ["Runtime.SymbolPackage"] = typeof(Runtime).GetMethod("SymbolPackage")!,
            ["Runtime.SetSymbolValue"] = typeof(Runtime).GetMethod("SetSymbolValue")!,
            ["Runtime.GetProp"] = typeof(Runtime).GetMethod("GetProp")!,
            ["Runtime.PutProp"] = typeof(Runtime).GetMethod("PutProp")!,
            ["Runtime.Remprop"] = typeof(Runtime).GetMethod("Remprop")!,
            ["Runtime.CopySymbol"] = typeof(Runtime).GetMethod("CopySymbol")!,
            ["Runtime.CopySymbolFull"] = typeof(Runtime).GetMethod("CopySymbolFull")!,

            // Runtime - string comparison
            ["Runtime.StringEq"] = typeof(Runtime).GetMethod("StringEq")!,
            ["Runtime.StringLt"] = typeof(Runtime).GetMethod("StringLt")!,
            ["Runtime.StringGt"] = typeof(Runtime).GetMethod("StringGt")!,
            ["Runtime.StringLe"] = typeof(Runtime).GetMethod("StringLe")!,
            ["Runtime.StringGe"] = typeof(Runtime).GetMethod("StringGe")!,
            ["Runtime.StringNotEq"] = typeof(Runtime).GetMethod("StringNotEq")!,
            ["Runtime.String"] = typeof(Runtime).GetMethod("String")!,

            // Runtime - sequence operations
            ["Runtime.Elt"] = typeof(Runtime).GetMethod("Elt")!,
            ["Runtime.Subseq"] = typeof(Runtime).GetMethod("Subseq")!,
            ["Runtime.Concatenate"] = typeof(Runtime).GetMethod("Concatenate")!,
            ["Runtime.Sort"] = typeof(Runtime).GetMethod("Sort")!,
            ["Runtime.Reverse"] = typeof(Runtime).GetMethod("Reverse")!,
            ["Runtime.Coerce"] = typeof(Runtime).GetMethod("Coerce")!,
            ["Runtime.Search"] = typeof(Runtime).GetMethod("Search")!,

            // Runtime - higher-order
            ["Runtime.Apply"] = typeof(Runtime).GetMethod("Apply")!,
            ["Runtime.Mapcar"] = typeof(Runtime).GetMethod("Mapcar")!,
            ["Runtime.MapcarN"] = typeof(Runtime).GetMethod("MapcarN")!,

            // Runtime - rest args
            ["Runtime.CollectRestArgs"] = typeof(Runtime).GetMethod("CollectRestArgs")!,

            // Runtime - hash table
            ["Runtime.MakeHashTable"] = typeof(Runtime).GetMethod("MakeHashTable")!,
            ["Runtime.MakeHashTable0"] = typeof(Runtime).GetMethod("MakeHashTable0")!,
            ["Runtime.Gethash"] = typeof(Runtime).GetMethod("Gethash")!,
            ["Runtime.Puthash"] = typeof(Runtime).GetMethod("Puthash")!,
            ["Runtime.Remhash"] = typeof(Runtime).GetMethod("Remhash")!,

            // Runtime - values
            ["Runtime.Values"] = typeof(Runtime).GetMethod("Values")!,
            ["Runtime.MultipleValuesList"] = typeof(Runtime).GetMethod("MultipleValuesList")!,
            ["Runtime.MultipleValuesList1"] = typeof(Runtime).GetMethod("MultipleValuesList1")!,
            ["Runtime.UnwrapMv"] = typeof(Runtime).GetMethod("UnwrapMv")!,

            // MultipleValues
            ["MultipleValues.Reset"] = typeof(MultipleValues).GetMethod("Reset")!,
            ["MultipleValues.Primary"] = typeof(MultipleValues).GetMethod("Primary")!,
            ["MultipleValues.SaveCount"] = typeof(MultipleValues).GetMethod("SaveCount")!,
            ["MultipleValues.SaveValues"] = typeof(MultipleValues).GetMethod("SaveValues")!,
            ["MultipleValues.RestoreSaved"] = typeof(MultipleValues).GetMethod("RestoreSaved")!,

            // Runtime - typep / error
            ["Runtime.Subtypep"] = typeof(Runtime).GetMethod("Subtypep")!,
            ["Runtime.Aref"] = typeof(Runtime).GetMethod("Aref")!,
            ["Runtime.ArefSet"] = typeof(Runtime).GetMethod("ArefSet")!,
            ["Runtime.ArefMulti"] = typeof(Runtime).GetMethod("ArefMulti")!,
            ["Runtime.ArefSetMulti"] = typeof(Runtime).GetMethod("ArefSetMulti")!,
            ["Runtime.Aref2D"] = typeof(Runtime).GetMethod("Aref2D")!,
            ["Runtime.ArefSet2D"] = typeof(Runtime).GetMethod("ArefSet2D")!,
            ["Runtime.Aref3D"] = typeof(Runtime).GetMethod("Aref3D")!,
            ["Runtime.ArefSet3D"] = typeof(Runtime).GetMethod("ArefSet3D")!,
            ["Runtime.LispError"] = typeof(Runtime).GetMethod("LispError")!,

            // Runtime - vector push
            ["Runtime.VectorPushExtend2"] = typeof(Runtime).GetMethod("VectorPushExtend2")!,
            ["Runtime.VectorPushExtendVoid2"] = typeof(Runtime).GetMethod("VectorPushExtendVoid2")!,
            ["Runtime.VectorPush2"] = typeof(Runtime).GetMethod("VectorPush2")!,

            // Runtime - struct operations
            ["Runtime.MakeStruct"] = typeof(Runtime).GetMethod("MakeStruct")!,
            ["Runtime.StructRef"] = typeof(Runtime).GetMethod("StructRef")!,
            ["Runtime.StructRefI"] = typeof(Runtime).GetMethod("StructRefI")!,
            ["Runtime.StructSet"] = typeof(Runtime).GetMethod("StructSet")!,
            ["Runtime.StructSetI"] = typeof(Runtime).GetMethod("StructSetI")!,
            ["Runtime.StructTypep"] = typeof(Runtime).GetMethod("StructTypep")!,
            ["Runtime.CopyStruct"] = typeof(Runtime).GetMethod("CopyStruct")!,

            // Runtime - &key helper
            ["Runtime.FindKeyArg"] = typeof(Runtime).GetMethod("FindKeyArg")!,
            ["Runtime.FindKeyArgByName"] = typeof(Runtime).GetMethod("FindKeyArgByName")!,

            // Startup
            ["Startup.Sym"] = typeof(Startup).GetMethod("Sym")!,
            ["Startup.SymInPkg"] = typeof(Startup).GetMethod("SymInPkg")!,
            ["Startup.Keyword"] = typeof(Startup).GetMethod("Keyword")!,

            // DynamicBindings
            ["DynamicBindings.Get"] = typeof(DynamicBindings).GetMethod("Get")!,
            ["DynamicBindings.Set"] = typeof(DynamicBindings).GetMethod("Set")!,
            ["DynamicBindings.Push"] = typeof(DynamicBindings).GetMethod("Push")!,
            ["DynamicBindings.Pop"] = typeof(DynamicBindings).GetMethod("Pop")!,
            ["DynamicBindings.SetIfUnbound"] = typeof(DynamicBindings).GetMethod("SetIfUnbound")!,

            // LispFunction
            ["LispFunction.Invoke"] = typeof(LispFunction).GetMethod("Invoke")!,
            ["LispFunction.Invoke0"] = typeof(LispFunction).GetMethod("Invoke0")!,
            ["LispFunction.Invoke1"] = typeof(LispFunction).GetMethod("Invoke1")!,
            ["LispFunction.Invoke2"] = typeof(LispFunction).GetMethod("Invoke2")!,
            ["LispFunction.Invoke3"] = typeof(LispFunction).GetMethod("Invoke3")!,
            ["LispFunction.Invoke4"] = typeof(LispFunction).GetMethod("Invoke4")!,
            ["LispFunction.Invoke5"] = typeof(LispFunction).GetMethod("Invoke5")!,
            ["LispFunction.Invoke6"] = typeof(LispFunction).GetMethod("Invoke6")!,
            ["LispFunction.Invoke7"] = typeof(LispFunction).GetMethod("Invoke7")!,
            ["LispFunction.Invoke8"] = typeof(LispFunction).GetMethod("Invoke8")!,
            ["LispFunction.InvokeNative1"] = typeof(LispFunction).GetMethod("InvokeNative1")!,
            ["LispFunction.InvokeNative2"] = typeof(LispFunction).GetMethod("InvokeNative2")!,
            ["LispFunction.InvokeNative3"] = typeof(LispFunction).GetMethod("InvokeNative3")!,
            ["LispFunction.InvokeNative4"] = typeof(LispFunction).GetMethod("InvokeNative4")!,
            ["LispFunction.get_RawFunction"] =
                typeof(LispFunction).GetProperty("RawFunction")!.GetGetMethod()!,

            // CilAssembler statics
            ["CilAssembler.GetFunction"] = typeof(CilAssembler).GetMethod("GetFunction")!,
            ["CilAssembler.GetFunctionBySymbol"] = typeof(CilAssembler).GetMethod("GetFunctionBySymbol")!,
            ["CilAssembler.GetSetfFunctionBySymbol"] = typeof(CilAssembler).GetMethod("GetSetfFunctionBySymbol")!,
            ["CilAssembler.GetConstant"] = typeof(CilAssembler).GetMethod("GetConstant")!,
            ["CilAssembler.MakeClosure"] = typeof(CilAssembler).GetMethod("MakeClosure")!,
            ["CilAssembler.RegisterFunctionOnSymbol"] = typeof(CilAssembler).GetMethod("RegisterFunctionOnSymbol")!,
            ["CilAssembler.RegisterSetfFunctionOnSymbol"] = typeof(CilAssembler).GetMethod("RegisterSetfFunctionOnSymbol")!,
            ["CilAssembler.RegisterFunction"] = typeof(CilAssembler).GetMethod("RegisterFunction")!,

            // Runtime - symbol utilities
            ["Runtime.SetSymbolConstant"] = typeof(Runtime).GetMethod("SetSymbolConstant")!,
            ["Runtime.MarkSpecial"] = typeof(Runtime).GetMethod("MarkSpecial")!,
            ["Runtime.SetVariableDocumentation"] = typeof(Runtime).GetMethod("SetVariableDocumentation")!,

            // Runtime - error/signal/warn
            ["Runtime.LispErrorFormat"] = typeof(Runtime).GetMethod("LispErrorFormat")!,
            ["Runtime.LispSignal"] = typeof(Runtime).GetMethod("LispSignal")!,
            ["Runtime.LispSignalFormat"] = typeof(Runtime).GetMethod("LispSignalFormat")!,
            ["Runtime.LispWarn"] = typeof(Runtime).GetMethod("LispWarn")!,
            ["Runtime.LispWarnFormat"] = typeof(Runtime).GetMethod("LispWarnFormat")!,

            // HandlerClusterStack
            ["HandlerClusterStack.PushCluster"] = typeof(HandlerClusterStack).GetMethod("PushCluster")!,
            ["HandlerClusterStack.PopCluster"] = typeof(HandlerClusterStack).GetMethod("PopCluster")!,

            // RestartClusterStack
            ["RestartClusterStack.PushCluster"] = typeof(RestartClusterStack).GetMethod("PushCluster")!,
            ["RestartClusterStack.PopCluster"] = typeof(RestartClusterStack).GetMethod("PopCluster")!,
            ["RestartClusterStack.FindRestart"] = typeof(RestartClusterStack).GetMethod("FindRestartByName")!,
            ["RestartClusterStack.ComputeRestarts"] = typeof(RestartClusterStack).GetMethod("ComputeRestarts")!,

            // RestartInvocationException properties
            ["RestartInvocationException.get_Tag"] =
                typeof(RestartInvocationException).GetProperty("Tag")!.GetGetMethod()!,
            ["RestartInvocationException.get_Arguments"] =
                typeof(RestartInvocationException).GetProperty("Arguments")!.GetGetMethod()!,

            // LispRestart property setters
            ["LispRestart.set_ReportFunction"] =
                typeof(LispRestart).GetProperty("ReportFunction")!.GetSetMethod()!,
            ["LispRestart.set_InteractiveFunction"] =
                typeof(LispRestart).GetProperty("InteractiveFunction")!.GetSetMethod()!,
            ["LispRestart.set_NameSymbol"] =
                typeof(LispRestart).GetProperty("NameSymbol")!.GetSetMethod()!,
            ["LispRestart.set_TestFunction"] =
                typeof(LispRestart).GetProperty("TestFunction")!.GetSetMethod()!,

            // Runtime - restart
            ["Runtime.RestartArg"] = typeof(Runtime).GetMethod("RestartArg")!,
            ["Runtime.RestartArgsToList"] = typeof(Runtime).GetMethod("RestartArgsToList")!,
            ["Runtime.RestartArgsAsList"] = typeof(Runtime).GetMethod("RestartArgsAsList")!,
            ["Runtime.RestartKeyArg"] = typeof(Runtime).GetMethod("RestartKeyArg")!,
            ["Runtime.InvokeRestart"] = typeof(Runtime).GetMethod("InvokeRestart")!,
            ["Runtime.FindRestart"] = typeof(Runtime).GetMethod("FindRestart", new[] { typeof(LispObject) })!,
            ["Runtime.FindRestartN"] = typeof(Runtime).GetMethod("FindRestartN")!,
            ["Runtime.ComputeRestarts"] = typeof(Runtime).GetMethod("ComputeRestarts", Type.EmptyTypes)!,
            ["Runtime.ComputeRestartsN"] = typeof(Runtime).GetMethod("ComputeRestartsN")!,

            // CLOS operations
            ["Runtime.FindClass"] = typeof(Runtime).GetMethod("FindClass")!,
            ["Runtime.FindClassOrNil"] = typeof(Runtime).GetMethod("FindClassOrNil")!,
            ["Runtime.RegisterClass"] = typeof(Runtime).GetMethod("RegisterClass")!,
            ["Runtime.MakeClass"] = typeof(Runtime).GetMethod("MakeClass")!,
            ["Runtime.MakeSlotDef"] = typeof(Runtime).GetMethod("MakeSlotDef")!,
            ["Runtime.MakeSlotDefWithAllocation"] = typeof(Runtime).GetMethod("MakeSlotDefWithAllocation")!,
            ["Runtime.SetClassDefaultInitargs"] = typeof(Runtime).GetMethod("SetClassDefaultInitargs")!,
            ["Runtime.ClassOf"] = typeof(Runtime).GetMethod("ClassOf")!,
            ["Runtime.ClassName"] = typeof(Runtime).GetMethod("ClassName")!,
            ["Runtime.MakeInstanceRaw"] = typeof(Runtime).GetMethod("MakeInstanceRaw")!,
            ["Runtime.SlotValue"] = typeof(Runtime).GetMethod("SlotValue")!,
            ["Runtime.SetSlotValue"] = typeof(Runtime).GetMethod("SetSlotValue")!,
            ["Runtime.Boundp"] = typeof(Runtime).GetMethod("Boundp")!,
            ["Runtime.SymbolValue"] = typeof(Runtime).GetMethod("SymbolValue")!,
            ["Runtime.Fdefinition"] = typeof(Runtime).GetMethod("Fdefinition")!,
            ["Runtime.Getenv"] = typeof(Runtime).GetMethod("Getenv")!,
            ["Runtime.SlotBoundp"] = typeof(Runtime).GetMethod("SlotBoundp")!,
            ["Runtime.SlotExists"] = typeof(Runtime).GetMethod("SlotExists")!,
            ["Runtime.MakeInstanceWithInitargs"] = typeof(Runtime).GetMethod("MakeInstanceWithInitargs")!,
            ["Runtime.ChangeClass"] = typeof(Runtime).GetMethod("ChangeClass")!,

            // CLOS generic function operations
            ["Runtime.MakeGF"] = typeof(Runtime).GetMethod("MakeGF")!,
            ["Runtime.RegisterGF"] = typeof(Runtime).GetMethod("RegisterGF")!,
            ["Runtime.ClearDefgenericInlineMethods"] = typeof(Runtime).GetMethod("ClearDefgenericInlineMethods")!,
            ["Runtime.MarkDefgenericInlineMethod"] = typeof(Runtime).GetMethod("MarkDefgenericInlineMethod")!,
            ["Runtime.SetMethodCombination"] = typeof(Runtime).GetMethod("SetMethodCombination")!,
            ["Runtime.SetMethodCombinationOrder"] = typeof(Runtime).GetMethod("SetMethodCombinationOrder")!,
            ["Runtime.SetMethodCombinationArgs"] = typeof(Runtime).GetMethod("SetMethodCombinationArgs")!,
            ["Runtime.SetGFLambdaListInfo"] = typeof(Runtime).GetMethod("SetGFLambdaListInfo")!,
            ["Runtime.SetMethodLambdaListInfo"] = typeof(Runtime).GetMethod("SetMethodLambdaListInfo")!,
            ["Runtime.FindGF"] = typeof(Runtime).GetMethod("FindGF")!,
            ["Runtime.MakeMethod"] = typeof(Runtime).GetMethod("MakeMethod")!,
            ["Runtime.AddMethod"] = typeof(Runtime).GetMethod("AddMethod")!,
            ["Runtime.RemoveMethod"] = typeof(Runtime).GetMethod("RemoveMethod")!,
            ["Runtime.ComputeApplicableMethods"] = typeof(Runtime).GetMethod("ComputeApplicableMethods")!,
            ["Runtime.GetGFMethods"] = typeof(Runtime).GetMethod("GetGFMethods")!,
            ["Runtime.MethodSpecializers"] = typeof(Runtime).GetMethod("MethodSpecializers")!,
            ["Runtime.MethodQualifiers"] = typeof(Runtime).GetMethod("MethodQualifiers")!,
            ["Runtime.MethodFunction"] = typeof(Runtime).GetMethod("MethodFunction")!,
            ["Runtime.CallNextMethod"] = typeof(Runtime).GetMethod("CallNextMethod")!,
            ["Runtime.NextMethodP"] = typeof(Runtime).GetMethod("NextMethodP")!,

            // Runtime - package operations
            ["Runtime.MakePackage"] = typeof(Runtime).GetMethod("MakePackage")!,
            ["Runtime.PackageUse"] = typeof(Runtime).GetMethod("PackageUse")!,
            ["Runtime.PackageExport"] = typeof(Runtime).GetMethod("PackageExport")!,
            ["Runtime.PackageExternalSymbolsList"] = typeof(Runtime).GetMethod("PackageExternalSymbolsList")!,
            ["Runtime.PackageAllSymbolsList"] = typeof(Runtime).GetMethod("PackageAllSymbolsList")!,
            ["Runtime.HashTablePairs"] = typeof(Runtime).GetMethod("HashTablePairs")!,
            ["Runtime.CoerceToFunction"] = typeof(Runtime).GetMethod("CoerceToFunction")!,
            ["Runtime.PackageImport"] = typeof(Runtime).GetMethod("PackageImport")!,
            ["Runtime.PackageShadow"] = typeof(Runtime).GetMethod("PackageShadow")!,
            ["Runtime.PackageNickname"] = typeof(Runtime).GetMethod("PackageNickname")!,
            ["Runtime.FindPackage"] = typeof(Runtime).GetMethod("FindPackage")!,
            ["Runtime.PackageName"] = typeof(Runtime).GetMethod("PackageName")!,
            ["Runtime.PackageErrorPackage"] = typeof(Runtime).GetMethod("PackageErrorPackage")!,
            ["Runtime.InternSymbol"] = typeof(Runtime).GetMethod("InternSymbol")!,
            ["Runtime.InternSymbolV"] = typeof(Runtime).GetMethod("InternSymbolV")!,
            ["Runtime.MakePackageK"] = typeof(Runtime).GetMethod("MakePackageK")!,
            ["Runtime.DeletePackage"] = typeof(Runtime).GetMethod("DeletePackage")!,
            ["Runtime.RenamePackage"] = typeof(Runtime).GetMethod("RenamePackage")!,
            ["Runtime.FindSymbolL"] = typeof(Runtime).GetMethod("FindSymbolL")!,
            ["Runtime.UninternSymbol"] = typeof(Runtime).GetMethod("UninternSymbol")!,
            ["Runtime.UnexportSymbol"] = typeof(Runtime).GetMethod("UnexportSymbol")!,
            ["Runtime.UnusePackage"] = typeof(Runtime).GetMethod("UnusePackage")!,
            ["Runtime.ShadowingImport"] = typeof(Runtime).GetMethod("ShadowingImport")!,
            ["Runtime.PackageUsedByList"] = typeof(Runtime).GetMethod("PackageUsedByList")!,
            ["Runtime.PackageUseListL"] = typeof(Runtime).GetMethod("PackageUseListL")!,
            ["Runtime.PackageShadowingSymbols"] = typeof(Runtime).GetMethod("PackageShadowingSymbols")!,
            ["Runtime.PackageNicknamesList"] = typeof(Runtime).GetMethod("PackageNicknamesList")!,
            ["Runtime.ListAllPackages"] = typeof(Runtime).GetMethod("ListAllPackages", Type.EmptyTypes)!,
            ["Runtime.ListAllPackagesV"] = typeof(Runtime).GetMethod("ListAllPackagesV")!,
            ["Runtime.Random"] = typeof(Runtime).GetMethod("Random", new[] { typeof(LispObject) })!,
            ["Runtime.Random2"] = typeof(Runtime).GetMethod("Random2")!,

            // Runtime - format
            ["Runtime.Format"] = typeof(Runtime).GetMethod("Format")!,

            // Runtime - file I/O
            ["Runtime.OpenFile"] = typeof(Runtime).GetMethod("OpenFile")!,
            ["Runtime.CloseStream"] = typeof(Runtime).GetMethod("CloseStream")!,
            ["Runtime.OpenStreamP"] = typeof(Runtime).GetMethod("OpenStreamP")!,
            ["Runtime.ReadCharNoHang"] = typeof(Runtime).GetMethod("ReadCharNoHang")!,
            ["Runtime.Listen"] = typeof(Runtime).GetMethod("Listen")!,
            ["Runtime.ClearInput"] = typeof(Runtime).GetMethod("ClearInput")!,
            ["Runtime.WriteByte"] = typeof(Runtime).GetMethod("WriteByte")!,
            ["Runtime.ReadPreservingWhitespace"] = typeof(Runtime).GetMethod("ReadPreservingWhitespace")!,
            ["Runtime.ReadLine"] = typeof(Runtime).GetMethod("ReadLine", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.ReadChar"] = typeof(Runtime).GetMethod("ReadChar", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.PeekChar"] = typeof(Runtime).GetMethod("PeekChar", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.UnreadChar"] = typeof(Runtime).GetMethod("UnreadChar")!,
            ["Runtime.WriteChar"] = typeof(Runtime).GetMethod("WriteChar")!,
            ["Runtime.WriteString"] = typeof(Runtime).GetMethod("WriteString", new[] { typeof(LispObject[]) })!,
            ["Runtime.WriteLine"] = typeof(Runtime).GetMethod("WriteLine", new[] { typeof(LispObject[]) })!,
            ["Runtime.Directory"] = typeof(Runtime).GetMethod("LispDirectory")!,
            ["Runtime.ProbeFile"] = typeof(Runtime).GetMethod("ProbeFile")!,
            ["Runtime.Truename"] = typeof(Runtime).GetMethod("Truename")!,

            ["Runtime.DeleteFile"] = typeof(Runtime).GetMethod("DeleteFile")!,
            ["Runtime.FileAuthor"] = typeof(Runtime).GetMethod("FileAuthor")!,
            ["Runtime.FileErrorPathname"] = typeof(Runtime).GetMethod("FileErrorPathname")!,
            ["Runtime.RenameFile"] = typeof(Runtime).GetMethod("RenameFile")!,
            ["Runtime.FileWriteDate"] = typeof(Runtime).GetMethod("FileWriteDate")!,

            // Runtime - string streams
            ["Runtime.MakeStringOutputStream"] = typeof(Runtime).GetMethod("MakeStringOutputStream")!,
            ["Runtime.MakeStringOutputStreamToString"] = typeof(Runtime).GetMethod("MakeStringOutputStreamToString")!,
            ["Runtime.GetOutputStreamString"] = typeof(Runtime).GetMethod("GetOutputStreamString")!,
            ["Runtime.MakeStringInputStream"] = typeof(Runtime).GetMethod("MakeStringInputStream", new[] { typeof(LispObject[]) })!,

            // Runtime - pathname
            ["Runtime.MakePathname"] = typeof(Runtime).GetMethod("MakePathname")!,
            ["Runtime.MakePathnameFromParts"] = typeof(Runtime).GetMethod("MakePathnameFromParts")!,
            ["Runtime.Pathname"] = typeof(Runtime).GetMethod("Pathname")!,
            ["Runtime.Namestring"] = typeof(Runtime).GetMethod("Namestring")!,
            ["Runtime.PathnameDirectory"] = typeof(Runtime).GetMethod("PathnameDirectory")!,
            ["Runtime.PathnameName"] = typeof(Runtime).GetMethod("PathnameName")!,
            ["Runtime.PathnameType"] = typeof(Runtime).GetMethod("PathnameType")!,
            ["Runtime.PathnameHost"] = typeof(Runtime).GetMethod("PathnameHost")!,
            ["Runtime.PathnameDevice"] = typeof(Runtime).GetMethod("PathnameDevice")!,
            ["Runtime.PathnameVersion"] = typeof(Runtime).GetMethod("PathnameVersion")!,
            ["Runtime.MergePathnames"] = typeof(Runtime).GetMethod("MergePathnames", new[] { typeof(LispObject), typeof(LispObject) })!,

            // Runtime - read
            ["Runtime.ReadFromStream"] = typeof(Runtime).GetMethod("ReadFromStream")!,
            ["Runtime.ReadFromString"] = typeof(Runtime).GetMethod("ReadFromString")!,

            // Runtime - load / eval
            ["Runtime.Load"] = typeof(Runtime).GetMethod("Load", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.Eval"] = typeof(Runtime).GetMethod("Eval")!,
            ["Runtime.TryEval"] = typeof(Runtime).GetMethod("TryEval")!,
            ["Runtime.Gensym"] = typeof(Runtime).GetMethod("Gensym")!,
            ["Runtime.Gensym0"] = typeof(Runtime).GetMethod("Gensym0")!,

            // LispErrorException property
            ["LispErrorException.get_Condition"] =
                typeof(LispErrorException).GetProperty("Condition")!.GetGetMethod()!,

            // HandlerCaseInvocationException properties
            ["HandlerCaseInvocationException.get_Tag"] =
                typeof(HandlerCaseInvocationException).GetProperty("Tag")!.GetGetMethod()!,
            ["HandlerCaseInvocationException.get_ClauseIndex"] =
                typeof(HandlerCaseInvocationException).GetProperty("ClauseIndex")!.GetGetMethod()!,
            ["HandlerCaseInvocationException.get_Condition"] =
                typeof(HandlerCaseInvocationException).GetProperty("Condition")!.GetGetMethod()!,

            // Startup.MakeHandlerCaseFunction
            ["Startup.MakeHandlerCaseFunction"] =
                typeof(Startup).GetMethod("MakeHandlerCaseFunction")!,

            // System.Exception property
            ["System.Exception.get_Message"] =
                typeof(System.Exception).GetProperty("Message")!.GetGetMethod()!,

            // Wrap .NET exceptions as Lisp conditions
            ["Runtime.WrapDotNetException"] =
                typeof(Runtime).GetMethod("WrapDotNetException")!,
            ["Runtime.RewrapNonLispException"] =
                typeof(Runtime).GetMethod("RewrapNonLispException")!,
            ["Runtime.IsLispControlFlowException"] =
                typeof(Runtime).GetMethod("IsLispControlFlowException")!,
            ["Runtime.ThrowControlError"] =
                typeof(Runtime).GetMethod("ThrowControlError")!,

            // Fixnum.Value getter — used by compile-as-long for fast-path
            // fixnum arithmetic on (the fixnum ...) expressions.
            ["Fixnum.get_Value"] =
                typeof(Fixnum).GetProperty("Value")!.GetGetMethod()!,
            ["DoubleFloat.get_Value"] =
                typeof(DoubleFloat).GetProperty("Value")!.GetGetMethod()!,
            ["SingleFloat.get_Value"] =
                typeof(SingleFloat).GetProperty("Value")!.GetGetMethod()!,

            // BlockReturnException properties
            ["BlockReturnException.get_Tag"] =
                typeof(BlockReturnException).GetProperty("Tag")!.GetGetMethod()!,
            ["BlockReturnException.get_Value"] =
                typeof(BlockReturnException).GetProperty("Value")!.GetGetMethod()!,

            // CatchThrowException properties and factory
            ["CatchThrowException.get_Tag"] =
                typeof(CatchThrowException).GetProperty("Tag")!.GetGetMethod()!,
            ["CatchThrowException.get_Value"] =
                typeof(CatchThrowException).GetProperty("Value")!.GetGetMethod()!,
            ["CatchThrowException.Get"] =
                typeof(CatchThrowException).GetMethod("Get")!,

            // CatchTagStack methods
            ["CatchTagStack.Push"] =
                typeof(CatchTagStack).GetMethod("Push")!,
            ["CatchTagStack.Pop"] =
                typeof(CatchTagStack).GetMethod("Pop")!,
            ["CatchTagStack.HasMatchingCatch"] =
                typeof(CatchTagStack).GetMethod("HasMatchingCatch")!,

            // GoException properties
            ["GoException.get_TagbodyId"] =
                typeof(GoException).GetProperty("TagbodyId")!.GetGetMethod()!,
            ["GoException.get_TargetLabel"] =
                typeof(GoException).GetProperty("TargetLabel")!.GetGetMethod()!,
        };

        _fieldCache = new Dictionary<string, FieldInfo>
        {
            ["Nil.Instance"] = typeof(Nil).GetField("Instance")!,
            ["T.Instance"] = typeof(T).GetField("Instance")!,
        };

        _ctorCache = new Dictionary<string, ConstructorInfo>
        {
            ["LispString"] = typeof(LispString).GetConstructor(new[] { typeof(string) })!,
            ["DoubleFloat"] = typeof(DoubleFloat).GetConstructor(new[] { typeof(double) })!,
            ["SingleFloat"] = typeof(SingleFloat).GetConstructor(new[] { typeof(float) })!,
            ["LispProgramError"] = typeof(LispProgramError).GetConstructor(new[] { typeof(string) })!,
            ["LispErrorException"] = typeof(LispErrorException)
                .GetConstructor(new[] { typeof(LispCondition) })!,
            ["LispVector"] = typeof(LispVector).GetConstructor(new[] { typeof(LispObject[]) })!,
            ["Object"] = typeof(object).GetConstructor(Type.EmptyTypes)!,
            ["BlockReturnException"] = typeof(BlockReturnException)
                .GetConstructor(new[] { typeof(object), typeof(LispObject) })!,
            ["CatchThrowException"] = typeof(CatchThrowException)
                .GetConstructor(new[] { typeof(LispObject), typeof(LispObject) })!,
            ["GoException"] = typeof(GoException)
                .GetConstructor(new[] { typeof(object), typeof(int) })!,
            ["HandlerBinding"] = typeof(HandlerBinding)
                .GetConstructor(new[] { typeof(LispObject), typeof(LispFunction) })!,
            ["LispRestart"] = typeof(LispRestart)
                .GetConstructor(new[] { typeof(string), typeof(Func<LispObject[], LispObject>),
                                        typeof(string), typeof(object), typeof(bool) })!,
        };

        _typeCache = new Dictionary<string, Type>
        {
            ["LispObject"] = typeof(LispObject),
            ["LispObject[]"] = typeof(LispObject[]),
            ["LispFunction"] = typeof(LispFunction),
            ["Symbol"] = typeof(Symbol),
            ["Object"] = typeof(object),
            ["Object[]"] = typeof(object[]),
            ["Int32"] = typeof(int),
            ["Int64"] = typeof(long),
            ["Double"] = typeof(double),
            ["Single"] = typeof(float),
            ["Boolean"] = typeof(bool),
            ["String"] = typeof(string),
            ["BlockReturnException"] = typeof(BlockReturnException),
            ["CatchThrowException"] = typeof(CatchThrowException),
            ["GoException"] = typeof(GoException),
            ["LispErrorException"] = typeof(LispErrorException),
            ["HandlerCaseInvocationException"] = typeof(HandlerCaseInvocationException),
            ["LispCondition"] = typeof(LispCondition),
            ["HandlerBinding"] = typeof(HandlerBinding),
            ["HandlerBinding[]"] = typeof(HandlerBinding[]),
            ["RestartInvocationException"] = typeof(RestartInvocationException),
            ["LispRestart"] = typeof(LispRestart),
            ["LispRestart[]"] = typeof(LispRestart[]),
            ["Func<LispObject[],LispObject>"] = typeof(Func<LispObject[], LispObject>),
            ["LispInstance"] = typeof(LispInstance),
            ["LispClass"] = typeof(LispClass),
            ["LispPathname"] = typeof(LispPathname),
            ["LispFileStream"] = typeof(LispFileStream),
            ["LispStringOutputStream"] = typeof(LispStringOutputStream),
            ["System.Exception"] = typeof(System.Exception),
        };

        _getConstant = typeof(CilAssembler).GetMethod("GetConstant")!;
        _makeClosure = typeof(CilAssembler).GetMethod("MakeClosure")!;
        _registerFunction = typeof(CilAssembler).GetMethod("RegisterFunction")!;
        _registerFunctionOnSymbol = typeof(CilAssembler).GetMethod("RegisterFunctionOnSymbol")!;
        _registerSetfFunctionOnSymbol = typeof(CilAssembler).GetMethod("RegisterSetfFunctionOnSymbol")!;
    }
}
