using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Compiles SIL instruction lists into a persisted .NET assembly (.fasl).
/// Uses PersistedAssemblyBuilder (.NET 9+) to emit static methods.
/// </summary>
public class FaslAssembler
{
    private readonly PersistedAssemblyBuilder _ab;
    private readonly ModuleBuilder _mb;
    private readonly TypeBuilder _tb;
    private readonly ILGenerator _initIl;
    private int _methodCount;
    private readonly CilAssembler.FaslStructInternMap _structInternMap;

    // --- Cached reflection refs (static readonly, shared across instances) ---

    internal static readonly ConstructorInfo ArrayFuncCtor =
        typeof(Func<LispObject[], LispObject>)
            .GetConstructor(new[] { typeof(object), typeof(IntPtr) })!;
    internal static readonly ConstructorInfo LispFuncCtor =
        typeof(LispFunction)
            .GetConstructor(new[] { typeof(Func<LispObject[], LispObject>), typeof(string), typeof(int) })!;
    internal static readonly MethodInfo RegisterFunctionMI =
        typeof(CilAssembler).GetMethod("RegisterFunction")!;
    internal static readonly MethodInfo RegisterOnSymbolMI =
        typeof(CilAssembler).GetMethod("RegisterFunctionOnSymbol")!;
    internal static readonly MethodInfo RegisterOnSymbolGuardedMI =
        typeof(CilAssembler).GetMethod("RegisterFunctionOnSymbolGuarded")!;
    internal static readonly MethodInfo RegisterSetfOnSymbolMI =
        typeof(CilAssembler).GetMethod("RegisterSetfFunctionOnSymbol")!;
    internal static readonly MethodInfo SymInPkgMI =
        typeof(Startup).GetMethod("SymInPkg")!;
    internal static readonly MethodInfo SetDirectDelegateMI =
        typeof(LispFunction).GetMethod("SetDirectDelegate")!;
    internal static readonly MethodInfo CheckArityExactMI =
        typeof(Runtime).GetMethod("CheckArityExact")!;

    // Typed delegate constructors for arities 0..8 (index 0 = Func<LispObject>, etc.)
    internal static readonly ConstructorInfo[] TypedFuncCtors = BuildTypedFuncCtors();

    private static ConstructorInfo[] BuildTypedFuncCtors()
    {
        var ctorArgs = new[] { typeof(object), typeof(IntPtr) };
        var types = new Type[]
        {
            typeof(Func<LispObject>),
            typeof(Func<LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
            typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
        };
        var result = new ConstructorInfo[9];
        for (int i = 0; i < 9; i++)
            result[i] = types[i].GetConstructor(ctorArgs)!;
        return result;
    }

    public FaslAssembler(string moduleName)
    {
        _ab = new PersistedAssemblyBuilder(
            new AssemblyName(moduleName), typeof(object).Assembly);
        _mb = _ab.DefineDynamicModule(moduleName);
        _tb = _mb.DefineType("CompiledModule",
            TypeAttributes.Public | TypeAttributes.Class);

        var initMethod = _tb.DefineMethod("ModuleInit",
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), Type.EmptyTypes);
        _initIl = initMethod.GetILGenerator();

        _structInternMap = new CilAssembler.FaslStructInternMap(moduleName);
    }

    /// <summary>Expose the TypeBuilder for CilAssembler FASL-mode branches that need to define methods.</summary>
    internal TypeBuilder TypeBuilder => _tb;
    internal CilAssembler.FaslStructInternMap StructInternMap => _structInternMap;

    private static void ThrowWithStringDiag(Exception ex, CilAssembler.FaslStructInternMap map, string where)
    {
        Console.Error.WriteLine($"[FaslAssembler] Error in {where}: {ex.GetType().Name}: {ex.Message}");
        Console.Error.WriteLine($"[FaslAssembler] Unique string bytes tracked: {map.UniqueStringBytes:N0}");
        Console.Error.Flush();
    }

    /// <summary>SIL 命令リストを処理して .fasl に追加</summary>
    public void AddTopLevelForm(LispObject instrList)
    {
        try { AddTopLevelFormImpl(instrList); }
        catch (Exception ex) { ThrowWithStringDiag(ex, _structInternMap, "AddTopLevelForm"); throw; }
    }

    private void AddTopLevelFormImpl(LispObject instrList)
    {
        // Check if this form contains DEFMETHOD/DEFMETHOD-DIRECT and whether
        // any branch/label OR any local-variable declaration appears at the
        // outer level. If so, splitting at defmethod boundaries would orphan
        // labels from their branches, or orphan (:ldloc X)/(:stloc X) from
        // their (:declare-local X) — each helper has its own label and local
        // table. Symptoms: "Label N has not been marked" (D719),
        // "Undeclared local: X_N" (D721).
        bool hasDefmethod = false;
        bool hasBranch = false;
        bool hasOuterLocal = false;
        var cur = instrList;
        while (cur is Cons c)
        {
            if (c.Car is Cons inner && inner.Car is Symbol sym)
            {
                if (sym.Name == "DEFMETHOD-DIRECT" || sym.Name == "DEFMETHOD")
                    hasDefmethod = true;
                else if (sym.Name == "DECLARE-LOCAL"
                         || sym.Name == "LDLOC" || sym.Name == "STLOC"
                         || sym.Name == "LDLOCA")
                    hasOuterLocal = true;
                else if (sym.Name.Length > 1 && sym.Name[0] == 'B'
                         && (sym.Name == "BR" || sym.Name == "BRFALSE"
                             || sym.Name == "BRTRUE" || sym.Name == "BEQ"
                             || sym.Name == "BGT" || sym.Name == "BLT"
                             || sym.Name == "BGE" || sym.Name == "BLE"
                             || sym.Name == "BNE-UN" || sym.Name == "BNE_UN"))
                    hasBranch = true;
                else if (sym.Name == "LEAVE")
                    hasBranch = true;
            }
            cur = c.Cdr;
        }

        if (hasDefmethod && (hasBranch || hasOuterLocal))
        {
            // Cannot safely split — emit the whole form into one helper method.
            // _faslMode DEFMETHOD handling still extracts the nested body into
            // its own persisted method; only the outer label/local chain
            // stays inline in the helper. This preserves helper-scoped tables
            // (unlike AddMonolithicForm which would append to _initIl
            // and strand subsequent forms after a premature :RET).
            EmitToplevelHelper(instrList);
            return;
        }

        if (hasDefmethod)
        {
            // Split into contiguous segments of non-defmethod / defmethod instructions,
            // flushing each segment in source order. This preserves execution order
            // so that e.g. (progn (fmakunbound 'x) (defun x ...)) clears then registers,
            // not the other way around. (D674)
            LispObject? pendingHead = null;  // non-defmethod instructions accumulated so far, forward order
            LispObject? pendingTail = null;  // last Cons of pendingHead for O(1) append
            cur = instrList;
            while (cur is Cons c)
            {
                if (c.Car is Cons inner && inner.Car is Symbol sym
                    && (sym.Name == "DEFMETHOD-DIRECT" || sym.Name == "DEFMETHOD"))
                {
                    // Flush any pending non-defmethod instructions BEFORE this defmethod
                    if (pendingHead != null)
                    {
                        EmitToplevelHelper(pendingHead);
                        pendingHead = null;
                        pendingTail = null;
                    }
                    var (name, paramNames, bodyInstrs, defPkg) = ParseDefmethodForm(inner);
                    int id = _methodCount++;
                    if (sym.Name == "DEFMETHOD-DIRECT")
                        EmitDefmethodDirectInto(_tb, _initIl, _structInternMap,
                            name, paramNames.Count, bodyInstrs, defPkg, id);
                    else
                        EmitDefmethodInto(_tb, _initIl, _structInternMap,
                            name, paramNames.Count, bodyInstrs, defPkg, id);
                }
                else
                {
                    // Append to pending list (forward order)
                    var node = new Cons(c.Car, Nil.Instance);
                    if (pendingHead == null)
                    {
                        pendingHead = node;
                        pendingTail = node;
                    }
                    else
                    {
                        ((Cons)pendingTail!).Cdr = node;
                        pendingTail = node;
                    }
                }
                cur = c.Cdr;
            }

            // Flush any trailing non-defmethod instructions
            if (pendingHead != null)
            {
                EmitToplevelHelper(pendingHead);
            }
        }
        else
        {
            // Non-defmethod top-level form: create a helper method and call it from ModuleInit
            EmitToplevelHelper(instrList);
        }
    }

    /// <summary>
    /// Emit a monolithic top-level form into _initIl, without splitting.
    /// Unlike AddTopLevelForm (which segments at defmethod boundaries), this
    /// assembles the whole form into the init method. Required for forms where
    /// locals/labels span the entire body (e.g. cil-out.sil — the cross-compiled
    /// core). Relies on CilAssembler's _faslMode branches in
    /// HandleDefmethod/HandleDefmethodDirect to emit persisted body methods.
    /// </summary>
    public void AddMonolithicForm(LispObject instrList)
    {
        try
        {
            var innerAsm = new CilAssembler();
            innerAsm._il = _initIl;
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _tb;
            innerAsm._faslStructMap = _structInternMap;
            innerAsm.Assemble(instrList);
            // If the form doesn't end in :ret, the Save() tail-Ret handles it.
        }
        catch (Exception ex) { ThrowWithStringDiag(ex, _structInternMap, "AddMonolithicForm"); throw; }
    }

    private void EmitToplevelHelper(LispObject instrList)
    {
        int id = _methodCount++;
        string methodName = "_toplevel_" + id;
        var method = _tb.DefineMethod(methodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), Type.EmptyTypes);

        var innerAsm = new CilAssembler();
        innerAsm._il = method.GetILGenerator();
        innerAsm._faslMode = true;
        innerAsm._faslTypeBuilder = _tb;
        innerAsm._faslStructMap = _structInternMap;
        try { innerAsm.Assemble(instrList); }
        catch (Exception ex) { ThrowWithStringDiag(ex, _structInternMap, methodName); throw; }

        // If the segment is a split fragment from AddTopLevelForm (no trailing :RET),
        // append Ldsfld Nil + Ret so the method is CIL-valid. Split fragments end with
        // balanced stack (POP after each side-effect form), so no stack cleanup needed.
        if (!EndsWithRet(instrList))
        {
            innerAsm._il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
            innerAsm._il.Emit(OpCodes.Ret);
        }

        _initIl.Emit(OpCodes.Call, method);
        _initIl.Emit(OpCodes.Pop);
    }

    private static bool EndsWithRet(LispObject instrList)
    {
        LispObject? last = null;
        var cur = instrList;
        while (cur is Cons c) { last = c.Car; cur = c.Cdr; }
        return last is Cons lc && lc.Car is Symbol s && s.Name == "RET";
    }

    // --- Shared parsing helper used by both FaslAssembler and CilAssembler FASL branch ---

    internal static (string name, List<string> paramNames, LispObject body, string? defPkg)
        ParseDefmethodForm(Cons instr)
    {
        // Parse: (:defmethod[-direct] "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
        var plist = instr.Cdr;
        var name = CilAssembler.GetString(CilAssembler.Car(plist));
        plist = CilAssembler.Cdr(plist);

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;

        while (plist is Cons pc)
        {
            var key = CilAssembler.GetSymbolName(pc.Car);
            var val = CilAssembler.Cadr(pc);
            switch (key)
            {
                case "PARAMS":
                    var pcur = val;
                    while (pcur is Cons lc)
                    {
                        paramNames.Add(CilAssembler.GetString(lc.Car));
                        pcur = lc.Cdr;
                    }
                    break;
                case "BODY":
                    bodyInstrs = val;
                    break;
                case "PKG":
                    defPkg = CilAssembler.GetString(val);
                    break;
            }
            plist = CilAssembler.Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("FASL DEFMETHOD: missing :body");
        return (name, paramNames, bodyInstrs, defPkg);
    }

    // --- Core static emitters, callable from both FaslAssembler and CilAssembler FASL mode ---

    /// <summary>
    /// Emit a DEFMETHOD-DIRECT (typed-param body + array wrapper) into the given
    /// TypeBuilder, with registration IL written to `initIl`. Includes _funcN
    /// typed-delegate assignment for direct-call fast path.
    /// </summary>
    internal static void EmitDefmethodDirectInto(
        TypeBuilder tb, ILGenerator initIl, CilAssembler.FaslStructInternMap structMap,
        string name, int paramCount, LispObject bodyInstrs, string? defPkg, int id)
    {
        if (paramCount > 8)
            throw new Exception($"FASL DEFMETHOD-DIRECT: param-count {paramCount} > 8 not supported");

        // 1. Body method with direct typed params: static LispObject Name_body(LispObject p0, ...)
        var directParamTypes = new Type[paramCount];
        for (int i = 0; i < paramCount; i++) directParamTypes[i] = typeof(LispObject);

        string bodyMethodName = SanitizeName(name) + "_body_" + id;
        var bodyMethod = tb.DefineMethod(bodyMethodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), directParamTypes);

        var innerAsm = new CilAssembler();
        innerAsm._il = bodyMethod.GetILGenerator();
        innerAsm._faslMode = true;
        innerAsm._faslTypeBuilder = tb;
        innerAsm._faslStructMap = structMap;
        innerAsm.Assemble(bodyInstrs);

        // 2. Array-arg wrapper: static LispObject Name(LispObject[] args)
        string wrapperName = SanitizeName(name) + "_" + id;
        var wrapperMethod = tb.DefineMethod(wrapperName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), new[] { typeof(LispObject[]) });
        var wil = wrapperMethod.GetILGenerator();

        wil.Emit(OpCodes.Ldstr, name);
        wil.Emit(OpCodes.Ldarg_0);
        wil.Emit(OpCodes.Ldc_I4, paramCount);
        wil.Emit(OpCodes.Call, CheckArityExactMI);

        for (int i = 0; i < paramCount; i++)
        {
            wil.Emit(OpCodes.Ldarg_0);
            wil.Emit(OpCodes.Ldc_I4, i);
            wil.Emit(OpCodes.Ldelem_Ref);
        }
        wil.Emit(OpCodes.Call, bodyMethod);
        wil.Emit(OpCodes.Ret);

        // 3. Registration IL (includes _funcN for direct-call fast path).
        EmitRegistrationInto(initIl, name, wrapperMethod, paramCount, defPkg, bodyMethod);
    }

    /// <summary>
    /// Emit a DEFMETHOD (array-arg body only, no direct path) into the given
    /// TypeBuilder with registration IL in `initIl`. Arity is validated inside
    /// the body (not by a wrapper) because DEFMETHOD bodies have the flexibility
    /// to accept rest/optional args.
    /// </summary>
    internal static void EmitDefmethodInto(
        TypeBuilder tb, ILGenerator initIl, CilAssembler.FaslStructInternMap structMap,
        string name, int paramCount, LispObject bodyInstrs, string? defPkg, int id)
    {
        string methodName = SanitizeName(name) + "_" + id;
        var method = tb.DefineMethod(methodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), new[] { typeof(LispObject[]) });

        var innerAsm = new CilAssembler();
        innerAsm._il = method.GetILGenerator();
        innerAsm._faslMode = true;
        innerAsm._faslTypeBuilder = tb;
        innerAsm._faslStructMap = structMap;
        innerAsm.Assemble(bodyInstrs);

        // No _funcN for plain DEFMETHOD — body signature is LispObject[] -> LispObject.
        EmitRegistrationInto(initIl, name, method, paramCount, defPkg, directBodyMethod: null);
    }

    /// <summary>
    /// Emit IL to register a function: build Func&lt;LispObject[], LispObject&gt;
    /// delegate from wrapperMethod, wrap in LispFunction, optionally install a
    /// typed _funcN delegate (when directBodyMethod != null and arity ≤ 8),
    /// then register on the appropriate symbols.
    ///
    /// For (SETF NAME): emits RegisterSetfFunctionOnSymbol.
    /// For defPkg != null: RegisterFunctionOnSymbolGuarded (protects inherited CL symbols).
    /// For defPkg == null: RegisterFunction + RegisterFunctionOnSymbol(CL-USER).
    /// </summary>
    private static void EmitRegistrationInto(
        ILGenerator il, string name, MethodBuilder wrapperMethod, int paramCount,
        string? defPkg, MethodBuilder? directBodyMethod)
    {
        var fnLocal = il.DeclareLocal(typeof(LispFunction));

        // new Func<LispObject[], LispObject>(null, &wrapperMethod)
        il.Emit(OpCodes.Ldnull);
        il.Emit(OpCodes.Ldftn, wrapperMethod);
        il.Emit(OpCodes.Newobj, ArrayFuncCtor);

        // new LispFunction(del, name, arity)
        il.Emit(OpCodes.Ldstr, name);
        il.Emit(OpCodes.Ldc_I4, paramCount);
        il.Emit(OpCodes.Newobj, LispFuncCtor);
        il.Emit(OpCodes.Stloc, fnLocal);

        // Install _funcN for direct-call fast path when we have a typed body method.
        if (directBodyMethod != null && paramCount >= 0 && paramCount <= 8)
        {
            il.Emit(OpCodes.Ldloc, fnLocal);
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ldftn, directBodyMethod);
            il.Emit(OpCodes.Newobj, TypedFuncCtors[paramCount]);
            il.Emit(OpCodes.Callvirt, SetDirectDelegateMI);
        }

        // Dispatch registration by name shape.
        if (name.StartsWith("(SETF ") && name.EndsWith(")"))
        {
            // (SETF X): register on X's SetfFunction slot.
            var targetName = name.Substring(6, name.Length - 7);
            il.Emit(OpCodes.Ldstr, targetName);
            il.Emit(OpCodes.Ldstr, defPkg ?? "CL-USER");
            il.Emit(OpCodes.Call, SymInPkgMI);
            il.Emit(OpCodes.Ldloc, fnLocal);
            il.Emit(OpCodes.Call, RegisterSetfOnSymbolMI);
        }
        else if (defPkg != null)
        {
            // Package-aware: guarded variant protects inherited CL symbols (D427).
            il.Emit(OpCodes.Ldstr, name);
            il.Emit(OpCodes.Ldstr, defPkg);
            il.Emit(OpCodes.Call, SymInPkgMI);
            il.Emit(OpCodes.Ldloc, fnLocal);
            il.Emit(OpCodes.Ldstr, defPkg);
            il.Emit(OpCodes.Call, RegisterOnSymbolGuardedMI);
        }
        else
        {
            // No explicit package: register via CL-path lookup + also on CL-USER symbol.
            il.Emit(OpCodes.Ldstr, name);
            il.Emit(OpCodes.Ldloc, fnLocal);
            il.Emit(OpCodes.Call, RegisterFunctionMI);

            il.Emit(OpCodes.Ldstr, name);
            il.Emit(OpCodes.Ldstr, "CL-USER");
            il.Emit(OpCodes.Call, SymInPkgMI);
            il.Emit(OpCodes.Ldloc, fnLocal);
            il.Emit(OpCodes.Call, RegisterOnSymbolMI);
        }
    }

    /// <summary>.fasl ファイルに保存</summary>
    public void Save(string outputPath)
    {
        // return Nil.Instance
        _initIl.Emit(OpCodes.Ldsfld,
            typeof(Nil).GetField("Instance")!);
        _initIl.Emit(OpCodes.Ret);

        _tb.CreateType();
        try
        {
            _ab.Save(outputPath);
        }
        catch (Exception ex) when (ex.Message.Contains("UserString"))
        {
            Console.Error.WriteLine($"[FaslAssembler] UserString heap exceeded for {outputPath}");
            Console.Error.WriteLine($"[FaslAssembler] Unique string bytes tracked: {_structInternMap.UniqueStringBytes:N0}");
            throw;
        }
    }

    internal static string SanitizeName(string name)
    {
        // Replace chars invalid in .NET method names
        return name.Replace('-', '_').Replace('*', 'X').Replace('(', 'L').Replace(')', 'R')
                   .Replace(' ', '_').Replace('%', 'P');
    }
}
