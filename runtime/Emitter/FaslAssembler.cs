using System.Reflection;
using System.Reflection.Emit;
#if NET9_0_OR_GREATER
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
#endif

namespace DotCL.Emitter;

/// <summary>
/// Compiles SIL instruction lists into a persisted .NET assembly (.fasl).
/// Uses PersistedAssemblyBuilder (.NET 9+) to emit static methods.
/// </summary>
public class FaslAssembler
{
    // Writing a .fasl needs PersistedAssemblyBuilder, which is .NET 9+. On an
    // older target framework the whole implementation below compiles out and
    // these fields are left declared but never written — which is exactly what
    // CS0649 / CS0169 report. Keeping the declarations (rather than guarding
    // them too) keeps the class shape identical across target frameworks, so
    // only the assignments move.
#pragma warning disable CS0649, CS0169
#if NET9_0_OR_GREATER
    private readonly PersistedAssemblyBuilder _ab;
#endif
    private readonly ModuleBuilder _mb;
    private readonly TypeBuilder _tb;
    private readonly ILGenerator _initIl;
    // Type initializer: fills the per-call-site symbol caches (see
    // FaslStructInternMap.GetOrCreateSymFnSiteField). Closed in Save.
    private readonly ILGenerator _cctorIl;
    private int _methodCount;
    private readonly CilAssembler.FaslStructInternMap _structInternMap;
#pragma warning restore CS0649, CS0169

    // --- Debug info (opt-in) ---
    // When enabled, Save() emits a Portable PDB alongside the .fasl with one
    // sequence point per compiled function body at the source line of its
    // top-level form, so a debugger can bind a breakpoint on a defun and show
    // source-mapped stack frames. Off by default; the emit path is unchanged
    // (still _ab.Save) so the hot compile path stays byte-for-byte identical.
    private bool _emitDebug;
    private string? _debugSourcePath;
    // Project build over a concatenated unit: (startLine, path) per source file,
    // where startLine is the file's first line within the concat. Non-null selects
    // multi-document PDB — each method is attributed to the file its body came from
    // and its concat lines are remapped to that file's own line numbers. Null keeps
    // the single-document path (plain compile-file of one .lisp).
    private (int startLine, string path)[]? _debugLineMap;
    private int _currentFormLine;
    // Recorded body methods (top-level functions AND nested closures/lambdas) are
    // collected into the shared _structInternMap.DebugSink so both kinds reach the
    // PDB uniformly; see RecordBodyMethod (top-level) and CilAssembler.AssembleFaslBody
    // (closures).

    /// <summary>Enable Portable PDB emission for this fasl, mapping bodies to
    /// <paramref name="sourcePath"/>. No-op on runtimes without the emitter.</summary>
    public void EnableDebugInfo(string sourcePath)
    {
        _emitDebug = true;
        _debugSourcePath = sourcePath;
        _structInternMap.DebugSink = new();
    }

    /// <summary>Enable Portable PDB emission for a concatenated project build,
    /// mapping each method back to its originating source file via
    /// <paramref name="lineMap"/> (one document per file). No-op without the emitter.</summary>
    public void EnableDebugInfoMap((int startLine, string path)[] lineMap)
    {
        _emitDebug = true;
        _debugLineMap = lineMap;
        _structInternMap.DebugSink = new();
    }

    // Record a just-assembled body method with its collected debug info (read off
    // the inner assembler). Passed as onBodyMethod to the static emitters only when
    // debug is on. If the body produced no sequence points (e.g. no literal cons
    // forms), fall back to a single point at the top-level form's line so the
    // function is still breakable.
    private void RecordBodyMethod(MethodBuilder m, CilAssembler asm)
    {
        var points = asm._seqPoints ?? new List<(int, int, int, int, int)>();
        if (points.Count == 0) points.Add((0, _currentFormLine, 1, _currentFormLine, 1));
        var localVars = asm._localVars ?? new List<(int, string)>();
        var scopes = asm._completedScopes ?? new List<(int, int, List<(int, string)>)>();
        _structInternMap.DebugSink?.Add(
            new CilAssembler.DebugMethodInfo(m, points, localVars, scopes, asm._il.ILOffset));
    }

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
    internal static readonly MethodInfo PreinternSymbolMI =
        typeof(Startup).GetMethod("PreinternSymbol")!;
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

#if NET9_0_OR_GREATER
    /// <summary>Map an arbitrary module name onto characters valid in an
    /// AssemblyName simple-name. Anything outside [A-Za-z0-9_.-] (which includes
    /// the '=' / ',' the AssemblyName parser reserves) becomes '_'. Uniqueness
    /// is preserved by the caller's guid suffix, so collisions from the mapping
    /// don't matter.</summary>
    private static string SanitizeModuleName(string name)
    {
        if (string.IsNullOrEmpty(name)) return "M";
        var sb = new System.Text.StringBuilder(name.Length);
        foreach (var ch in name)
            sb.Append(char.IsLetterOrDigit(ch) || ch == '_' || ch == '.' || ch == '-' ? ch : '_');
        return sb.ToString();
    }
#endif

    public FaslAssembler(string moduleName)
    {
#if !NET9_0_OR_GREATER
        // A Lisp condition, not a raw CLR exception: the caller is Lisp code that
        // called COMPILE-FILE, and it should be able to handle this like any other
        // error. Same class the emit-free CilAssembler path uses for "this build
        // cannot generate code".
        throw new LispErrorException(new LispProgramError(
            "FASL emission (compile-file) requires .NET 9+ (PersistedAssemblyBuilder); " +
            "this runtime build runs precompiled .fasl only"));
#else
        // An AssemblyName simple-name rejects characters the parser reads as
        // attribute syntax (e.g. '=' and ',' from "name=value, ..."), so a source
        // file whose basename contains one — e.g. serapeum's vector=.lisp, giving
        // module name "vector=_<guid>" — made new AssemblyName throw "The given
        // assembly name was invalid.". The module name is only an internal
        // assembly / LTV-namespace identifier (a guid suffix keeps it unique), so
        // replacing the offending characters is lossless in practice.
        moduleName = SanitizeModuleName(moduleName);
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
        // Lets the map roll literal fields over onto fresh holder types once
        // CompiledModule has taken its share (see MaxFieldsPerHolder).
        _structInternMap.ModuleBuilder = _mb;
        // Call-site symbol caches go in the TYPE INITIALIZER, not ModuleInit: the
        // fields are read by compiled bodies, and a body can be reached from
        // anywhere in ModuleInit, so "initialized earlier in ModuleInit" is not a
        // guarantee we can make. Defining a .cctor also clears beforefieldinit, so
        // the CLR runs it before the first static access to this type.
        _cctorIl = _tb.DefineTypeInitializer().GetILGenerator();
        _structInternMap.SymFnSiteInitIl = _cctorIl;
#endif
    }

    /// <summary>Expose the TypeBuilder for CilAssembler FASL-mode branches that need to define methods.</summary>
    internal TypeBuilder TypeBuilder => _tb;
    internal CilAssembler.FaslStructInternMap StructInternMap => _structInternMap;

    private static void ThrowWithStringDiag(Exception ex, CilAssembler.FaslStructInternMap map, string where)
    {
        Console.Error.WriteLine($"[FaslAssembler] Error in {where}: {ex.GetType().Name}: {ex.Message}");
        Console.Error.WriteLine($"[FaslAssembler] Unique string bytes tracked: {map.UniqueStringBytes:N0}");
        if (Environment.GetEnvironmentVariable("DOTCL_FASL_DIAG") != null)
            Console.Error.WriteLine($"[FaslAssembler] STACK:\n{ex.StackTrace}");
        Console.Error.Flush();
    }

    /// <summary>Process a SIL instruction list and append it to the .fasl</summary>
    public void AddTopLevelForm(LispObject instrList)
    {
        try { AddTopLevelFormImpl(instrList); }
        catch (Exception ex) { ThrowWithStringDiag(ex, _structInternMap, "AddTopLevelForm"); throw; }
    }

    /// <summary>As <see cref="AddTopLevelForm(LispObject)"/>, tagging any function
    /// bodies emitted for this form with <paramref name="sourceLine"/> for PDB
    /// (only used when debug info is enabled).</summary>
    public void AddTopLevelForm(LispObject instrList, int sourceLine)
    {
        _currentFormLine = sourceLine;
        AddTopLevelForm(instrList);
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
                    var onBody = _emitDebug ? RecordBodyMethod : (Action<MethodBuilder, CilAssembler>?)null;
                    if (sym.Name == "DEFMETHOD-DIRECT")
                        EmitDefmethodDirectInto(_tb, _initIl, _structInternMap,
                            name, paramNames.Count, bodyInstrs, defPkg, id, selfArg0, onBody);
                    else if (sym.Name == "DEFMETHOD-NATIVE")
                        EmitDefmethodNativeInto(_tb, _initIl, _structInternMap,
                            name, paramNames.Count, bodyInstrs, defPkg, id, onBody);
                    else
                        EmitDefmethodInto(_tb, _initIl, _structInternMap,
                            name, paramNames.Count, bodyInstrs, defPkg, id, onBody);
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
    /// assembles the whole form into the init method. For forms whose
    /// locals/labels span the entire body — a .sil produced before the
    /// cross-compiler segmented its output at (:TOPLEVEL-BOUNDARY); current
    /// ones go through AddTopLevelForm per segment. Only one such form can be
    /// added: its :RET ends the init method. Relies on CilAssembler's _faslMode
    /// branches in HandleDefmethod/HandleDefmethodDirect to emit persisted body
    /// methods.
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
        // Intentionally not source-mapped: _toplevel helpers carry compiler glue
        // (e.g. a defun's return-name tail, or registration side effects that run
        // at module-load time). Mapping them would make a breakpoint on a defun's
        // line spuriously hit at load. Only real function bodies get sequence
        // points (function granularity). Top-level side-effect forms are a
        // later step.

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
        bool selfArg0 = false, Action<MethodBuilder, CilAssembler>? onBodyMethod = null)
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
        if (onBodyMethod != null) { innerAsm._seqPoints = new(); innerAsm._localVars = new(); innerAsm._completedScopes = new(); }
        innerAsm.Assemble(bodyInstrs);
        if (onBodyMethod != null) onBodyMethod(bodyMethod, innerAsm);

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
        string name, int paramCount, LispObject bodyInstrs, string? defPkg, int id,
        Action<MethodBuilder, CilAssembler>? onBodyMethod = null)
    {
        string methodName = SanitizeName(name) + "_" + id;
        var method = tb.DefineMethod(methodName,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), new[] { typeof(LispObject[]) });
        // Name the raw args array so the debugger's Locals shows "args" rather
        // than an unnamed "value" alongside the source-named parameters.
        method.DefineParameter(1, System.Reflection.ParameterAttributes.None, "args");

        var innerAsm = new CilAssembler();
        innerAsm._il = method.GetILGenerator();
        innerAsm._faslMode = true;
        innerAsm._faslTypeBuilder = tb;
        innerAsm._faslStructMap = structMap;
        if (onBodyMethod != null) { innerAsm._seqPoints = new(); innerAsm._localVars = new(); innerAsm._completedScopes = new(); }
        innerAsm.Assemble(bodyInstrs);
        if (onBodyMethod != null) onBodyMethod(method, innerAsm);

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
        string name, int paramCount, LispObject bodyInstrs, string? defPkg, int id,
        Action<MethodBuilder, CilAssembler>? onBodyMethod = null)
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
        if (onBodyMethod != null) { nativeAsm._seqPoints = new(); nativeAsm._localVars = new(); nativeAsm._completedScopes = new(); }
        nativeAsm.Assemble(bodyInstrs);
        if (onBodyMethod != null) onBodyMethod(nativeMethod, nativeAsm);

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

#if NET9_0_OR_GREATER
    /// <summary>
    /// Emit load-time pre-interning for every symbol this fasl names (see
    /// Startup.PreinternSymbol). Called at the END of ModuleInit so the file's own
    /// defpackage forms have already run — a symbol whose package this fasl
    /// defines is then interned in the right place, and one whose package is
    /// missing is skipped by the guard in PreinternSymbol.
    ///
    /// The calls go into chunked helper methods rather than straight into
    /// ModuleInit: a large source file names thousands of symbols, and ~11 IL
    /// bytes each would push ModuleInit (which also carries the top-level forms)
    /// toward the method-size limit.
    /// </summary>
    private void EmitPreinternSymbols()
    {
        const int chunkSize = 256;
        var syms = _structInternMap.PreinternSymbols;
        if (syms.Count == 0) return;
        int index = 0, chunk = 0;
        ILGenerator? chunkIl = null;
        foreach (var (name, pkg) in syms)
        {
            if (index % chunkSize == 0)
            {
                if (chunkIl != null) chunkIl.Emit(OpCodes.Ret);
                var m = _tb.DefineMethod($"PreinternSymbols_{chunk++}",
                    MethodAttributes.Public | MethodAttributes.Static,
                    typeof(void), Type.EmptyTypes);
                chunkIl = m.GetILGenerator();
                _initIl.Emit(OpCodes.Call, m);
            }
            chunkIl!.Emit(OpCodes.Ldstr, name);
            chunkIl.Emit(OpCodes.Ldstr, pkg);
            chunkIl.Emit(OpCodes.Call, PreinternSymbolMI);
            index++;
        }
        chunkIl!.Emit(OpCodes.Ret);
    }
#endif

    /// <summary>Write the assembled .fasl to the given output path. When
    /// <paramref name="retargetCorlib"/> is non-null, the saved image's corlib
    /// reference is rewritten to that facade (only "netstandard" is supported) so
    /// the fasl loads on BCLs without System.Private.CoreLib (Unity IL2CPP/WebGL).</summary>
    public void Save(string outputPath, string? retargetCorlib = null)
    {
#if !NET9_0_OR_GREATER
        throw new LispErrorException(new LispProgramError(
            "FASL emission (compile-file) requires .NET 9+; this runtime build runs precompiled .fasl only"));
#else
        EmitPreinternSymbols();

        // return Nil.Instance
        _initIl.Emit(OpCodes.Ldsfld,
            typeof(Nil).GetField("Instance")!);
        _initIl.Emit(OpCodes.Ret);

        // Close the type initializer (empty when the module named no functions).
        _cctorIl.Emit(OpCodes.Ret);

        // Literal holders spilled off CompiledModule, each with its own
        // initializer. Created before CompiledModule, which references them.
        foreach (var (holder, cctor) in _structInternMap.OverflowHolders)
        {
            cctor.Emit(OpCodes.Ret);
            holder.CreateType();
        }

        _tb.CreateType();
        StampCoreGeneration();
        try
        {
            if (_emitDebug)
                SaveWithDebugInfo(outputPath);
            else
                _ab.Save(outputPath);
        }
        catch (Exception ex) when (ex.Message.Contains("UserString"))
        {
            Console.Error.WriteLine($"[FaslAssembler] UserString heap exceeded for {outputPath}");
            Console.Error.WriteLine($"[FaslAssembler] Unique string bytes tracked: {_structInternMap.UniqueStringBytes:N0}");
            throw;
        }
        // PersistedAssemblyBuilder writes exception clauses in an order the CLR
        // rejects when one exception block is nested inside another's handler
        // (an unwind-protect whose cleanup contains handler-case / catch / …).
        FaslEhOrder.Fix(outputPath);
        if (retargetCorlib != null)
            FaslCorlibRetarget.RetargetCorlib(outputPath, retargetCorlib);
#endif
    }

    /// <summary>When set, the generation this assembly IS rather than the one that
    /// built it. A core stamps its own identity (derived from the .sil it embodies);
    /// every ordinary .fasl stamps the running core's.
    ///
    /// Declared OUTSIDE the NET9_0_OR_GREATER block below on purpose: sil-to-fasl in
    /// Startup sets it on every target framework, while only the stamping itself is
    /// version-specific. Inside the block it compiled on net10.0 and vanished on
    /// net8.0, which nothing local builds — the break surfaced at pack time.</summary>
    internal string? SelfGeneration;

#if NET9_0_OR_GREATER
    /// <summary>Record which compiler generation produced this fasl, as an
    /// assembly-level [AssemblyMetadata("dotcl-core-generation", ...)]. The loader
    /// compares it with the running one and warns on a mismatch: a fasl carries the
    /// code generation of the compiler that built it, so one built before a codegen
    /// fix keeps the old behaviour and the fix looks inert. No stamp is written when
    /// the generation cannot be determined (a core loaded from memory) — an absent
    /// stamp is treated as "unknown", never as a mismatch.</summary>
    private void StampCoreGeneration()
    {
        try
        {
            var gen = SelfGeneration ?? Startup.CoreGeneration();
            if (gen == null) return;
            var ctor = typeof(System.Reflection.AssemblyMetadataAttribute)
                .GetConstructor(new[] { typeof(string), typeof(string) });
            if (ctor != null)
                _ab.SetCustomAttribute(new CustomAttributeBuilder(
                    ctor, new object[] { Startup.CoreGenerationKey, gen }));
        }
        catch { /* best-effort: the fasl is still valid without the stamp */ }
    }

    /// <summary>
    /// Save the .fasl (a PE image) together with a sidecar Portable PDB.
    /// Instead of PersistedAssemblyBuilder.Save (which builds the PE for us), we
    /// generate the metadata + IL stream ourselves so we can attach a debug
    /// directory pointing at a PDB. The PDB carries one document (the source
    /// file) and one sequence point per recorded body method, at the source line
    /// of its top-level form — enough to bind a breakpoint on a defun and show
    /// source-mapped frames. Finer, per-expression stepping is a later step.
    /// </summary>
    private void SaveWithDebugInfo(string outputPath)
    {
        // Mark the assembly debuggable with the JIT optimizer disabled. Without
        // this the JIT optimizes the fasl's methods and a debugger shows "optimized
        // code" — locals get elided and stepping is unreliable even with a valid
        // PDB. Must be set before GenerateMetadata so it lands in the metadata.
        try
        {
            var dbgCtor = typeof(System.Diagnostics.DebuggableAttribute)
                .GetConstructor(new[] { typeof(System.Diagnostics.DebuggableAttribute.DebuggingModes) });
            if (dbgCtor != null)
                _ab.SetCustomAttribute(new CustomAttributeBuilder(dbgCtor, new object[]
                {
                    System.Diagnostics.DebuggableAttribute.DebuggingModes.Default
                    | System.Diagnostics.DebuggableAttribute.DebuggingModes.DisableOptimizations
                }));
        }
        catch { /* best-effort: a valid PDB still helps even if this fails */ }

        var asmMetadata = _ab.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData);
        int methodDefCount = asmMetadata.GetRowCounts()[(int)TableIndex.MethodDef];

        var pdb = new MetadataBuilder();

        // Build the source document(s). A plain compile-file maps to one document;
        // a concatenated project build (with a line map) makes one document per
        // source file. AddSourceDocument attaches the SHA-256 checksum so a
        // debugger can verify the on-disk source and won't refuse to bind.
        DocumentHandle singleDoc = default;
        Dictionary<string, DocumentHandle>? docByPath = null;
        if (_debugLineMap != null)
        {
            docByPath = new(StringComparer.OrdinalIgnoreCase);
            foreach (var (_, path) in _debugLineMap)
                if (!docByPath.ContainsKey(path))
                    docByPath[path] = AddSourceDocument(pdb, path);
        }
        else
        {
            singleDoc = AddSourceDocument(pdb, _debugSourcePath ?? outputPath);
        }

        // Map MethodDef row id -> collected debug info. MetadataToken is only
        // assigned after GenerateMetadata.
        var infoByRow = new Dictionary<int, (List<(int offset, int sl, int sc, int el, int ec)> points,
            List<(int index, string name)> localVars,
            List<(int start, int length, List<(int index, string name)> vars)> scopes,
            int ilLength)>();
        foreach (var dm in _structInternMap.DebugSink ?? new())
        {
            int row = MetadataTokens.GetRowNumber(
                MetadataTokens.MethodDefinitionHandle(dm.Method.MetadataToken));
            if (row > 0) infoByRow[row] = (dm.Points, dm.Locals, dm.Scopes, dm.IlLength);
        }

        // MethodDebugInformation is parallel to MethodDef: one row per method in
        // order, nil for methods we didn't map. LocalScope/LocalVariable rows are
        // added in the same ascending-row loop so the LocalScope table stays sorted
        // by method (a Portable-PDB validity requirement), and each scope's
        // LocalVariable rows are contiguous.
        for (int row = 1; row <= methodDefCount; row++)
        {
            if (infoByRow.TryGetValue(row, out var info) && info.points.Count > 0)
            {
                // Pick this method's document and, for a project build, remap its
                // concat line numbers to the originating file's own lines. A defun
                // body (and its nested closures) comes from one file, so a single
                // document per method suffices — no in-method document switching.
                DocumentHandle methodDoc;
                List<(int offset, int sl, int sc, int el, int ec)> pts;
                if (_debugLineMap != null)
                {
                    // Remap concat line numbers (start and end) to the originating
                    // file's own lines; the document comes from the method's first point.
                    methodDoc = MapConcatLine(_debugLineMap, docByPath!, info.points[0].sl).doc;
                    pts = new List<(int, int, int, int, int)>(info.points.Count);
                    foreach (var (off, sl, sc, el, ec) in info.points)
                        pts.Add((off,
                                 MapConcatLine(_debugLineMap, docByPath!, sl).line, sc,
                                 MapConcatLine(_debugLineMap, docByPath!, el).line, ec));
                }
                else { methodDoc = singleDoc; pts = info.points; }

                pdb.AddMethodDebugInformation(methodDoc,
                    pdb.GetOrAddBlob(EncodeSequencePoints(pts)));

                // Build the scope list: the method-wide scope [0, ilLength) holding
                // parameters, plus each nested let/let* scope. LocalScope rows must
                // be sorted by (StartOffset asc, Length desc) within a method — an
                // enclosing scope precedes those it contains. Skip empty scopes.
                var scopes = new List<(int start, int length, List<(int index, string name)> vars)>();
                if (info.localVars.Count > 0)
                    scopes.Add((0, info.ilLength > 0 ? info.ilLength : 1, info.localVars));
                foreach (var sc in info.scopes)
                    if (sc.vars.Count > 0)
                        scopes.Add((sc.start, sc.length > 0 ? sc.length : 1, sc.vars));
                scopes.Sort((a, b) =>
                    a.start != b.start ? a.start.CompareTo(b.start) : b.length.CompareTo(a.length));

                foreach (var sc in scopes)
                {
                    LocalVariableHandle firstVar = default;
                    bool first = true;
                    foreach (var (index, name) in sc.vars)
                    {
                        var h = pdb.AddLocalVariable(LocalVariableAttributes.None, index, pdb.GetOrAddString(name));
                        if (first) { firstVar = h; first = false; }
                    }
                    pdb.AddLocalScope(
                        method: MetadataTokens.MethodDefinitionHandle(row),
                        importScope: default,
                        variableList: firstVar,
                        // No local constants — point one past the (empty) table.
                        constantList: MetadataTokens.LocalConstantHandle(1),
                        startOffset: sc.start,
                        length: sc.length);
                }
            }
            else
                pdb.AddMethodDebugInformation(default, default);
        }

        var pdbBuilder = new PortablePdbBuilder(pdb, asmMetadata.GetRowCounts(), entryPoint: default);
        var pdbBlob = new BlobBuilder();
        BlobContentId pdbId = pdbBuilder.Serialize(pdbBlob);
        string pdbPath = Path.ChangeExtension(outputPath, ".pdb");
        using (var pdbStream = new FileStream(pdbPath, FileMode.Create, FileAccess.Write))
            pdbBlob.WriteContentTo(pdbStream);

        var dbgDir = new DebugDirectoryBuilder();
        dbgDir.AddCodeViewEntry(pdbPath, pdbId, pdbBuilder.FormatVersion);

        var peBuilder = new ManagedPEBuilder(
            header: new PEHeaderBuilder(
                imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll),
            metadataRootBuilder: new MetadataRootBuilder(asmMetadata),
            ilStream: ilStream,
            mappedFieldData: fieldData,
            debugDirectoryBuilder: dbgDir);
        var peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);
        using (var peStream = new FileStream(outputPath, FileMode.Create, FileAccess.Write))
            peBlob.WriteContentTo(peStream);
    }

    /// <summary>Add a source-file document to the PDB with its SHA-256 checksum.
    /// The hash lets a debugger verify the on-disk source matches; best-effort —
    /// an unreadable file yields a document with no hash.</summary>
    private static DocumentHandle AddSourceDocument(MetadataBuilder pdb, string path)
    {
        GuidHandle hashAlg = default;
        BlobHandle hashBlob = default;
        try
        {
            if (File.Exists(path))
            {
                byte[] sha = System.Security.Cryptography.SHA256.HashData(File.ReadAllBytes(path));
                // ECMA/Portable-PDB SHA-256 document-hash algorithm GUID.
                hashAlg = pdb.GetOrAddGuid(new Guid("8829d00f-11b8-4213-878b-770e8597ac16"));
                hashBlob = pdb.GetOrAddBlob(sha);
            }
        }
        catch { hashAlg = default; hashBlob = default; }
        // No standard Common Lisp language GUID exists; leave unset. A debugger
        // binds by document path + sequence points regardless.
        return pdb.AddDocument(pdb.GetOrAddDocumentName(path), hashAlg, hashBlob, language: default);
    }

    /// <summary>Map a line in the concatenated build unit back to its source file's
    /// document and own line number. The file is the last one whose start line does
    /// not exceed <paramref name="concatLine"/>.</summary>
    private static (DocumentHandle doc, int line) MapConcatLine(
        (int startLine, string path)[] map, Dictionary<string, DocumentHandle> docByPath, int concatLine)
    {
        int idx = 0;
        for (int i = 0; i < map.Length; i++)
            if (map[i].startLine <= concatLine) idx = i; else break;
        var (start, path) = map[idx];
        int src = concatLine - start + 1;
        return (docByPath[path], src < 1 ? 1 : src);
    }

    /// <summary>Encode a Portable-PDB sequence-points blob for a method. Each point
    /// spans (line,1)..(line,2) at its IL offset. Header carries local-signature
    /// row id 0 (we emit no local scopes yet). Points must be ordered by IL offset
    /// (they are: collected in emission order) with strictly non-decreasing offset.</summary>
    private static BlobBuilder EncodeSequencePoints(List<(int offset, int sl, int sc, int el, int ec)> points)
    {
        var b = new BlobBuilder();
        b.WriteCompressedInteger(0);       // LocalSignature row id (none)

        int prevOffset = 0, prevSl = 0, prevSc = 0;
        bool first = true;
        foreach (var (offset, slRaw, scRaw, elRaw, ecRaw) in points)
        {
            // Clamp to a well-formed span: 1-based, End >= Start (a degenerate or
            // reversed span from a reader edge case would make an invalid PDB).
            int sl = slRaw < 1 ? 1 : slRaw;
            int sc = scRaw < 1 ? 1 : scRaw;
            int el = elRaw < sl ? sl : elRaw;
            int ec = ecRaw;
            // Collapse a multi-line span to its start line. A point that spans
            // several lines makes VS bind a breakpoint set on ANY of them to this
            // point, so an outer multi-line form (defun/let/flet) would swallow
            // breakpoints meant for its inner body; the inner forms have their own
            // single-line points, and binding must resolve to the innermost one.
            // Same-line column precision (the point of per-column points) is kept.
            if (el > sl) { el = sl; ec = sc + 1; }
            if (el == sl && ec <= sc) ec = sc + 1;   // DeltaColumns must be > 0
            if (ec < 1) ec = sc + 1;

            // ILOffset: absolute for the first record, delta thereafter. A delta of
            // 0 is illegal for non-first points, so skip a duplicate offset.
            if (first)
                b.WriteCompressedInteger(offset);
            else
            {
                int dOffset = offset - prevOffset;
                if (dOffset <= 0) continue;
                b.WriteCompressedInteger(dOffset);
            }

            int dLines = el - sl;
            b.WriteCompressedInteger(dLines);                          // DeltaLines
            // DeltaColumns = EndColumn - StartColumn: unsigned (and > 0) when the
            // span is on one line, signed when it crosses lines.
            if (dLines == 0)
                b.WriteCompressedInteger(ec - sc);
            else
                b.WriteCompressedSignedInteger(ec - sc);

            if (first)
            {
                b.WriteCompressedInteger(sl);                         // StartLine
                b.WriteCompressedInteger(sc);                         // StartColumn
            }
            else
            {
                b.WriteCompressedSignedInteger(sl - prevSl);          // ΔStartLine
                b.WriteCompressedSignedInteger(sc - prevSc);          // ΔStartColumn
            }

            prevOffset = offset;
            prevSl = sl;
            prevSc = sc;
            first = false;
        }
        return b;
    }
#endif

    internal static string SanitizeName(string name)
    {
        // Replace chars invalid in .NET method names
        return name.Replace('-', '_').Replace('*', 'X').Replace('(', 'L').Replace(')', 'R')
                   .Replace(' ', '_').Replace('%', 'P');
    }
}
