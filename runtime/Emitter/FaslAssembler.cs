using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Compiles SIL instruction lists into a persisted .NET assembly (.fasl).
/// Uses PersistedAssemblyBuilder (.NET 9+) to emit static methods.
/// </summary>
public class FaslAssembler
{
#if NET9_0_OR_GREATER
    private readonly PersistedAssemblyBuilder _ab;
#endif
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
    internal static readonly MethodInfo GetFunctionBySymbolMI =
        typeof(CilAssembler).GetMethod("GetFunctionBySymbol")!;
    internal static readonly MethodInfo GetSetfFunctionBySymbolMI =
        typeof(CilAssembler).GetMethod("GetSetfFunctionBySymbol")!;
    internal static readonly MethodInfo SetDirectDelegateMI =
        typeof(LispFunction).GetMethod("SetDirectDelegate")!;
    internal static readonly MethodInfo SetNativeDelegateMI =
        typeof(LispFunction).GetMethod("SetNativeDelegate")!;
    internal static readonly MethodInfo CheckArityExactMI =
        typeof(Runtime).GetMethod("CheckArityExact")!;
    internal static readonly MethodInfo FixnumValueGetterMI =
        typeof(Fixnum).GetProperty("Value")!.GetGetMethod()!;
    internal static readonly MethodInfo FixnumMakeMI =
        typeof(Fixnum).GetMethod("Make")!;

    // Typed delegate constructors for arities 0..8 (index 0 = Func<LispObject>, etc.)
    internal static readonly ConstructorInfo[] TypedFuncCtors = BuildTypedFuncCtors();
    // Native delegate constructors for arities 1..4. The leading LispFunction is the
    // self argument threaded through native self-calls; index 0 =
    // Func<LispFunction,long,LispObject>, etc.
    internal static readonly ConstructorInfo[] NativeTypedFuncCtors = BuildNativeTypedFuncCtors();

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

    private static ConstructorInfo[] BuildNativeTypedFuncCtors()
    {
        var ctorArgs = new[] { typeof(object), typeof(IntPtr) };
        var types = new Type[]
        {
            typeof(Func<LispFunction, long, LispObject>),
            typeof(Func<LispFunction, long, long, LispObject>),
            typeof(Func<LispFunction, long, long, long, LispObject>),
            typeof(Func<LispFunction, long, long, long, long, LispObject>),
        };
        var result = new ConstructorInfo[4];
        for (int i = 0; i < 4; i++)
            result[i] = types[i].GetConstructor(ctorArgs)!;
        return result;
    }

    public FaslAssembler(string moduleName)
    {
#if !NET9_0_OR_GREATER
        throw new PlatformNotSupportedException(
            "FASL emission (compile-file) requires .NET 9+ (PersistedAssemblyBuilder); " +
            "this runtime build runs precompiled .fasl only");
#else
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
        // Wire TypeBuilder + init ILGenerator so CilAssembler can deduplicate uninterned symbols.
        _structInternMap.UninternedTypeBuilder = _tb;
        _structInternMap.UninternedInitIl = _initIl;
#endif
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

    /// <summary>Process a SIL instruction list and append it to the .fasl</summary>
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
        // table. Symptoms: "Label N has not been marked",
        // "Undeclared local: X_N".
        bool hasDefmethod = false;
        bool hasBranch = false;
        bool hasOuterLocal = false;
        var cur = instrList;
        while (cur is Cons c)
        {
            if (c.Car is Cons inner && inner.Car is Symbol sym)
            {
                if (sym.Name == "DEFMETHOD-DIRECT" || sym.Name == "DEFMETHOD" || sym.Name == "DEFMETHOD-NATIVE")
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
            // not the other way around.
            LispObject? pendingHead = null;  // non-defmethod instructions accumulated so far, forward order
            LispObject? pendingTail = null;  // last Cons of pendingHead for O(1) append
            cur = instrList;
            while (cur is Cons c)
            {
                if (c.Car is Cons inner && inner.Car is Symbol sym
                    && (sym.Name == "DEFMETHOD-DIRECT" || sym.Name == "DEFMETHOD" || sym.Name == "DEFMETHOD-NATIVE"))
                {
                    // Flush any pending non-defmethod instructions BEFORE this defmethod
                    if (pendingHead != null)
                    {
                        EmitToplevelHelper(pendingHead);
                        pendingHead = null;
                        pendingTail = null;
                    }
                    var (name, paramNames, bodyInstrs, defPkg, selfArg0) = ParseDefmethodForm(inner);
                    int id = _methodCount++;
                    if (sym.Name == "DEFMETHOD-DIRECT")
                        EmitDefmethodDirectInto(_tb, _initIl, _structInternMap,
                            name, paramNames.Count, bodyInstrs, defPkg, id, selfArg0);
                    else if (sym.Name == "DEFMETHOD-NATIVE")
                        EmitDefmethodNativeInto(_tb, _initIl, _structInternMap,
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

    private static readonly System.Reflection.MethodInfo _evalMI =
        typeof(Runtime).GetMethod("Eval", new[] { typeof(LispObject) })!;

    /// <summary>Emit a FASL top-level entry that calls Runtime.Eval(form) at load time.
    /// Used to emit deferred make-load-form init forms after all creation forms.</summary>
    public void EmitEvalTopLevel(LispObject form)
    {
        int id = _methodCount++;
        string methodName = "_initform_" + id;
        var method = _tb.DefineMethod(methodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), Type.EmptyTypes);
        var il = method.GetILGenerator();
        var innerAsm = new CilAssembler();
        innerAsm._il = il;
        innerAsm._faslMode = true;
        innerAsm._faslTypeBuilder = _tb;
        innerAsm._faslStructMap = _structInternMap;
        innerAsm.EmitLoadConstInline(form);
        il.Emit(OpCodes.Call, _evalMI);
        il.Emit(OpCodes.Pop);
        il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
        il.Emit(OpCodes.Ret);
        _initIl.Emit(OpCodes.Call, method);
        _initIl.Emit(OpCodes.Pop);
    }

    /// <summary>Emit init forms whose creation dependencies are now satisfied. Call after each AddTopLevelForm.</summary>
    public void FlushInitForms()
    {
        foreach (var (_, initForm) in _structInternMap.PopEagerInitForms())
            EmitEvalTopLevel(initForm);
    }

    /// <summary>Emit any remaining init forms (e.g. cycle members). Call once before Save.</summary>
    public void FlushRemainingInitForms()
    {
        foreach (var (_, initForm) in _structInternMap.FlushRemainingInitForms())
            EmitEvalTopLevel(initForm);
    }

    // --- Shared parsing helper used by both FaslAssembler and CilAssembler FASL branch ---

    internal static (string name, List<string> paramNames, LispObject body, string? defPkg, bool selfArg0)
        ParseDefmethodForm(Cons instr)
    {
        // Parse: (:defmethod[-direct] "NAME" [:pkg "PKG"] [:self T] :params ("P1" ...) :body (...))
        var plist = instr.Cdr;
        var name = CilAssembler.GetString(CilAssembler.Car(plist));
        plist = CilAssembler.Cdr(plist);

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;
        bool selfArg0 = false;

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
                case "SELF":
                    selfArg0 = val is not Nil;  // self threaded as arg0
                    break;
            }
            plist = CilAssembler.Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("FASL DEFMETHOD: missing :body");
        return (name, paramNames, bodyInstrs, defPkg, selfArg0);
    }

    // --- Core static emitters, callable from both FaslAssembler and CilAssembler FASL mode ---

    /// <summary>
    /// Emit a DEFMETHOD-DIRECT (typed-param body + array wrapper) into the given
    /// TypeBuilder, with registration IL written to `initIl`. Includes _funcN
    /// typed-delegate assignment for direct-call fast path.
    /// </summary>
    internal static void EmitDefmethodDirectInto(
        TypeBuilder tb, ILGenerator initIl, CilAssembler.FaslStructInternMap structMap,
        string name, int paramCount, LispObject bodyInstrs, string? defPkg, int id,
        bool selfArg0 = false)
    {
        if (paramCount > 8)
            throw new Exception($"FASL DEFMETHOD-DIRECT: param-count {paramCount} > 8 not supported");

        // 1. Body method with direct typed params: static LispObject Name_body(LispObject p0, ...)
        // When selfArg0, arg0 is the self LispFunction (threaded so a non-tail
        // self-call reaches its receiver from arg0 instead of re-resolving #'NAME each
        // recursive entry); the direct params then start at ldarg 1.
        var directParamTypes = new Type[selfArg0 ? paramCount + 1 : paramCount];
        if (selfArg0)
        {
            directParamTypes[0] = typeof(LispFunction);
            for (int i = 0; i < paramCount; i++) directParamTypes[i + 1] = typeof(LispObject);
        }
        else
        {
            for (int i = 0; i < paramCount; i++) directParamTypes[i] = typeof(LispObject);
        }

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

        // selfArg0: resolve self via the symbol once for this slow apply/array path
        // (the hot _funcN delegate is bound to fn; the array wrapper builds fn so can't
        // capture it). Mirrors EmitDefmethodNativeInto's wrapper.
        if (selfArg0)
        {
            bool isSetf = name.StartsWith("(SETF ") && name.EndsWith(")");
            if (isSetf)
            {
                wil.Emit(OpCodes.Ldstr, name.Substring(6, name.Length - 7));
                wil.Emit(OpCodes.Ldstr, defPkg ?? "CL-USER");
                wil.Emit(OpCodes.Call, SymInPkgMI);
                wil.Emit(OpCodes.Call, GetSetfFunctionBySymbolMI);
            }
            else
            {
                wil.Emit(OpCodes.Ldstr, name);
                wil.Emit(OpCodes.Ldstr, defPkg ?? "CL-USER");
                wil.Emit(OpCodes.Call, SymInPkgMI);
                wil.Emit(OpCodes.Call, GetFunctionBySymbolMI);
            }
        }
        for (int i = 0; i < paramCount; i++)
        {
            wil.Emit(OpCodes.Ldarg_0);
            wil.Emit(OpCodes.Ldc_I4, i);
            wil.Emit(OpCodes.Ldelem_Ref);
        }
        wil.Emit(OpCodes.Call, bodyMethod);
        wil.Emit(OpCodes.Ret);

        // 3. Registration IL (includes _funcN for direct-call fast path). selfArg0 binds
        // the direct delegate's target to fn (open-instance, self bound).
        EmitRegistrationInto(initIl, name, wrapperMethod, paramCount, defPkg, bodyMethod,
            selfBound: selfArg0);
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
    /// Emit a DEFMETHOD-NATIVE (native long→long body) into the TypeBuilder.
    /// Generates: native body method (long params), direct LispObject wrapper,
    /// array wrapper, and registration with both _funcN and _nativeFuncN set.
    /// </summary>
    internal static void EmitDefmethodNativeInto(
        TypeBuilder tb, ILGenerator initIl, CilAssembler.FaslStructInternMap structMap,
        string name, int paramCount, LispObject bodyInstrs, string? defPkg, int id)
    {
        if (paramCount < 1 || paramCount > 4)
            throw new Exception($"FASL DEFMETHOD-NATIVE: param-count {paramCount} not supported (1-4)");

        // arg0 of every method below is the self LispFunction, threaded so a native
        // self-call reaches its receiver from arg0 instead of re-resolving #'NAME from
        // its symbol each recursive entry. The direct/array wrappers are bound
        // as delegates with `fn` as target (EmitRegistrationInto), so they receive self
        // implicitly; the native delegate is unbound and gets self from InvokeNativeN.

        // 1. Native body: static LispObject Name_native_N(LispFunction self, long p0, ...)
        // Long params avoid arg boxing; body returns LispObject (arithmetic boxes via Fixnum.Make).
        var nativeParamTypes = new Type[paramCount + 1];
        nativeParamTypes[0] = typeof(LispFunction);
        for (int i = 0; i < paramCount; i++) nativeParamTypes[i + 1] = typeof(long);

        string nativeMethodName = SanitizeName(name) + "_native_" + id;
        var nativeMethod = tb.DefineMethod(nativeMethodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), nativeParamTypes);

        var nativeAsm = new CilAssembler();
        nativeAsm._il = nativeMethod.GetILGenerator();
        nativeAsm._faslMode = true;
        nativeAsm._faslTypeBuilder = tb;
        nativeAsm._faslStructMap = structMap;
        nativeAsm.Assemble(bodyInstrs);

        // 2. Direct LispObject wrapper: static LispObject Name_direct_N(LispFunction self, LispObject p0, ...)
        var directParamTypes = new Type[paramCount + 1];
        directParamTypes[0] = typeof(LispFunction);
        for (int i = 0; i < paramCount; i++) directParamTypes[i + 1] = typeof(LispObject);

        string directMethodName = SanitizeName(name) + "_direct_" + id;
        var directMethod = tb.DefineMethod(directMethodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), directParamTypes);
        var dil = directMethod.GetILGenerator();

        dil.Emit(OpCodes.Ldarg_0);                   // self
        for (int i = 0; i < paramCount; i++)
        {
            dil.Emit(OpCodes.Ldarg, i + 1);
            dil.Emit(OpCodes.Castclass, typeof(Fixnum));
            dil.Emit(OpCodes.Call, FixnumValueGetterMI);
        }
        dil.Emit(OpCodes.Call, nativeMethod);
        // native body already returns LispObject; no Fixnum.Make wrapping needed
        dil.Emit(OpCodes.Ret);

        // 3. Array wrapper: static LispObject Name_wrap_N(LispObject[] args)
        // This is the slow fallback (apply / arity-mismatched calls); the hot paths are
        // _funcN (self bound to fn) and _nativeFuncN (self passed by InvokeNativeN). Since
        // the LispFunction is built FROM this delegate, it can't capture fn — so resolve
        // self here via the symbol once per array call (cheap relative to the array path).
        bool isSetf = name.StartsWith("(SETF ") && name.EndsWith(")");
        string wrapperName = SanitizeName(name) + "_" + id;
        var wrapperMethod = tb.DefineMethod(wrapperName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), new[] { typeof(LispObject[]) });
        var wil = wrapperMethod.GetILGenerator();

        wil.Emit(OpCodes.Ldstr, name);
        wil.Emit(OpCodes.Ldarg_0);                   // args array
        wil.Emit(OpCodes.Ldc_I4, paramCount);
        wil.Emit(OpCodes.Call, CheckArityExactMI);

        // self = (Get[Setf]FunctionBySymbol)(SymInPkg(selfName, pkg))
        if (isSetf)
        {
            wil.Emit(OpCodes.Ldstr, name.Substring(6, name.Length - 7));
            wil.Emit(OpCodes.Ldstr, defPkg ?? "CL-USER");
            wil.Emit(OpCodes.Call, SymInPkgMI);
            wil.Emit(OpCodes.Call, GetSetfFunctionBySymbolMI);
        }
        else
        {
            wil.Emit(OpCodes.Ldstr, name);
            wil.Emit(OpCodes.Ldstr, defPkg ?? "CL-USER");
            wil.Emit(OpCodes.Call, SymInPkgMI);
            wil.Emit(OpCodes.Call, GetFunctionBySymbolMI);
        }
        for (int i = 0; i < paramCount; i++)
        {
            wil.Emit(OpCodes.Ldarg_0);
            wil.Emit(OpCodes.Ldc_I4, i);
            wil.Emit(OpCodes.Ldelem_Ref);
        }
        wil.Emit(OpCodes.Call, directMethod);
        wil.Emit(OpCodes.Ret);

        // 4. Registration: _funcN = directMethod (self bound to fn), _nativeFuncN =
        // nativeMethod (self passed by InvokeNativeN). selfBound shifts the direct
        // delegate's target from null to fn.
        EmitRegistrationInto(initIl, name, wrapperMethod, paramCount, defPkg,
            directBodyMethod: directMethod, nativeBodyMethod: nativeMethod,
            selfBound: true);
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
        string? defPkg, MethodBuilder? directBodyMethod, MethodBuilder? nativeBodyMethod = null,
        bool selfBound = false)
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
        // For native (selfBound) functions, directBodyMethod takes a leading LispFunction
        // self param; bind it to fn so the open delegate signature still matches Func<...>.
        // Non-native direct methods have no self param and bind to null.
        if (directBodyMethod != null && paramCount >= 0 && paramCount <= 8)
        {
            il.Emit(OpCodes.Ldloc, fnLocal);
            if (selfBound) il.Emit(OpCodes.Ldloc, fnLocal); else il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ldftn, directBodyMethod);
            il.Emit(OpCodes.Newobj, TypedFuncCtors[paramCount]);
            il.Emit(OpCodes.Callvirt, SetDirectDelegateMI);
        }

        // Install _nativeFuncN for native long→long fast path
        if (nativeBodyMethod != null && paramCount >= 1 && paramCount <= 4)
        {
            il.Emit(OpCodes.Ldloc, fnLocal);
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ldftn, nativeBodyMethod);
            il.Emit(OpCodes.Newobj, NativeTypedFuncCtors[paramCount - 1]);
            il.Emit(OpCodes.Callvirt, SetNativeDelegateMI);
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
            // Package-aware: guarded variant protects inherited CL symbols.
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

    /// <summary>Write the assembled .fasl to the given output path. When
    /// <paramref name="retargetCorlib"/> is non-null, the saved image's corlib
    /// reference is rewritten to that facade (only "netstandard" is supported) so
    /// the fasl loads on BCLs without System.Private.CoreLib (Unity IL2CPP/WebGL).</summary>
    public void Save(string outputPath, string? retargetCorlib = null)
    {
#if !NET9_0_OR_GREATER
        throw new PlatformNotSupportedException(
            "FASL emission (compile-file) requires .NET 9+; this runtime build runs precompiled .fasl only");
#else
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
        if (retargetCorlib != null)
            FaslCorlibRetarget.RetargetCorlib(outputPath, retargetCorlib);
#endif
    }

    internal static string SanitizeName(string name)
    {
        // Replace chars invalid in .NET method names
        return name.Replace('-', '_').Replace('*', 'X').Replace('(', 'L').Replace(')', 'R')
                   .Replace(' ', '_').Replace('%', 'P');
    }
}
