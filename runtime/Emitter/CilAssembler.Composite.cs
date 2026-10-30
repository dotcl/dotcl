using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    // --- Composite instructions ---

    /// <summary>
    /// Re-intern the variable names in a :LAMBDA-LIST directive into PKG.
    ///
    /// The compiler writes them uninterned so the SIL text does not depend on
    /// which package the parameter happened to be interned in on the host that
    /// produced it -- a DEFSTRUCT accessor's parameter lands in DOTCL-INTERNAL
    /// under an SBCL cross-compile and in the current package under self-host,
    /// which made the two generations differ byte for byte. Reading them back
    /// interned keeps the stored value what a tool should print: A, not #:A.
    ///
    /// Only symbols are touched, and lambda-list keywords (&amp;OPTIONAL, &amp;KEY) are
    /// already interned so they pass through. Default forms are data and are
    /// copied structurally without interpretation.
    /// </summary>
    private static LispObject InternLambdaList(LispObject ll, Package? pkg)
    {
        switch (ll)
        {
            case Cons c:
                return new Cons(InternLambdaList(c.Car, pkg), InternLambdaList(c.Cdr, pkg));
            // FindSymbol, never Intern: this runs for every compiled DEFUN, and
            // creating a symbol here would be a visible side effect of recording
            // display data -- (defstruct (s (:conc-name nil)) otherpkg::a) then
            // showed A interned in the reading package, which the conc-name
            // tests check does NOT happen. If the name is not already present,
            // keep the uninterned symbol: it prints as the bare name, which is
            // all a tool needs.
            case Symbol sym when sym.HomePackage == null && sym.Name.Length > 0:
                var target = pkg ?? Runtime.CurrentPackage("DEFMETHOD");
                var (found, status) = target.FindSymbol(sym.Name);
                return (found != null && status != SymbolStatus.None) ? found : sym;
            default:
                return ll;
        }
    }

    /// <summary>A :LAMBDA-LIST directive as the text a FASL carries: the names
    /// resolved as in memory, printed relative to the package the loader reads
    /// the text in (the function name's home package, or *PACKAGE* when there
    /// is none). Written uninterned, a name the loading process has never
    /// interned could not be found again.</summary>
    internal static string FaslLambdaListText(LispObject ll, string? defPkg)
    {
        var pkg = (defPkg != null ? Package.FindPackagePhysical(defPkg) : null)
                  ?? Runtime.CurrentPackage("DEFMETHOD");
        var pkgSym = Startup.Sym("*PACKAGE*");
        DynamicBindings.Push(pkgSym, pkg);
        try { return Runtime.FormatObject(InternLambdaList(ll, pkg), true); }
        finally { DynamicBindings.Pop(pkgSym); }
    }

    /// <summary>The shared body of a &amp;key function, (:shared NPARAMS BODY) among its
    /// direct delegates: a method of its own that takes the key values as arguments,
    /// which the array entry and the typed entries call (SIL :call-key-shared), so it
    /// is built before either. Null when there is none.</summary>
    private static DynamicMethod? BuildKeySharedDm(string name, LispObject? directDelegates,
                                                   out LispObject? sharedInstrs)
    {
        DynamicMethod? keyShared = null;
        sharedInstrs = null;
        for (var dd = directDelegates; dd is Cons ddc; dd = ddc.Cdr)
        {
            if (ddc.Car is not Cons spec || Car(spec) is Fixnum) continue;
            sharedInstrs = Caddr(spec);
            int nShared = (int)((Fixnum)Cadr(spec)).Value;
            var sharedTypes = new Type[nShared];
            for (int i = 0; i < nShared; i++) sharedTypes[i] = typeof(LispObject);
            keyShared = new DynamicMethod(name + "_keybody", typeof(LispObject),
                sharedTypes, typeof(CilAssembler).Module, true);
            new CilAssembler { _il = keyShared.GetILGenerator().ForLibrary() }.Assemble(Caddr(spec));
        }
        return keyShared;
    }

    /// <summary>Install the typed entries ((ARITY SELF-P BODY [COPY]) ...) of an
    /// &amp;optional / &amp;key / &amp;rest function on FN. The array entry still backs
    /// APPLY and every other arity. The fasl path builds the same entries as methods
    /// (FaslAssembler.BuildDirectDelegateMethods).</summary>
    private static void AttachDirectDelegatesDm(LispFunction fn, string name,
                                                LispObject? directDelegates, DynamicMethod? keyShared,
                                                bool noFrame = true)
    {
        for (var dd = directDelegates; dd is Cons ddc; dd = ddc.Cdr)
        {
            if (ddc.Car is not Cons spec || Car(spec) is not Fixnum) continue;
            int arity = (int)((Fixnum)Car(spec)).Value;
            bool selfP = Cadr(spec) is not Nil;
            var ddBody = Caddr(spec);
            // A &key function's required-only entry may carry a full copy of
            // the body as a fourth element. A DynamicMethod is never inlined
            // into its caller, so here the call to the shared body would stay
            // a real call on the commonest shape; a fasl's entry calls it and
            // lets the JIT inline it, and ignores the copy.
            if (Cdr(Cdr(Cdr(spec))) is Cons { Car: Cons copy })
                ddBody = copy;
            // A self-recursive body reads its params from ldarg 1.. with the
            // function itself threaded in as arg0 (BuildSelfDirectFunction does
            // the same); the _funcN delegate then binds `fn` as that leading
            // arg via CreateDelegate(type, fn). Non-self bodies read from ldarg
            // 0.. and need no bound target.
            bool selfFramed = CanSelfFrame(name, arity, noFrame);
            bool withSelf = selfP || selfFramed;
            int nParams = withSelf ? arity + 1 : arity;
            var directParamTypes = new Type[nParams];
            int off = withSelf ? 1 : 0;
            if (withSelf) directParamTypes[0] = typeof(LispFunction);
            for (int i = 0; i < arity; i++) directParamTypes[off + i] = typeof(LispObject);
            var ddm = new DynamicMethod(name + "_opt" + arity, typeof(LispObject),
                directParamTypes, typeof(CilAssembler).Module, true);
            var ddAsm = new CilAssembler { _il = ddm.GetILGenerator().ForLibrary(), _keyShared = keyShared };
            if (selfFramed)
            {
                ddAsm._selfFrameName = string.Intern(name);
                ddAsm._selfFrameArgc = arity;
                ddAsm._selfFrameUnit = true;
                if (!selfP) ddAsm._argShift = 1;
            }
            ddAsm.Assemble(ddBody);
            var ddFuncArgs = new Type[arity + 1];
            for (int i = 0; i <= arity; i++) ddFuncArgs[i] = typeof(LispObject);
            var ddType = System.Linq.Expressions.Expression.GetFuncType(ddFuncArgs);
            fn.SetDirectDelegate(withSelf ? ddm.CreateDelegate(ddType, fn)
                                          : ddm.CreateDelegate(ddType));
            if (selfFramed) fn.SetArityEntry(DynamicMethodEntry(ddm), arity);
            if (BodyReadsMvMode(ddBody)) fn.MarkMvModeEntry(arity);
        }
    }

    private void HandleDefmethod(Cons instr)
    {
        // (:defmethod "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
        var plist = instr.Cdr;
        var name = GetString(Car(plist));
        bool noFrame = false;
        plist = Cdr(plist); // skip name

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;
        LispObject? directDelegates = null;  // ((arity body) ...) for &optional direct delegates
        // The lambda list the user wrote. Development information only (SLIME/SLY
        // autodoc, DESCRIBE); nothing in the call path reads it. It rides the
        // directive rather than emitted code so a redefinition loop does not add
        // a constant-pool entry per definition.
        LispObject? lambdaList = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "NO-FRAME": noFrame = true; break;
                case "PARAMS":
                    var cur = val;
                    while (cur is Cons lc)
                    {
                        paramNames.Add(GetString(lc.Car));
                        cur = lc.Cdr;
                    }
                    break;
                case "LAMBDA-LIST":
                    lambdaList = val;
                    break;
                case "BODY":
                    bodyInstrs = val;
                    break;
                case "PKG":
                    defPkg = GetString(val);
                    break;
                case "DIRECT-DELEGATES":
                    directDelegates = val;
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
                name, paramNames.Count, bodyInstrs, defPkg, faslId, noFrame: noFrame,
                lambdaList: lambdaList != null ? FaslLambdaListText(lambdaList, defPkg) : null,
                directDelegates: directDelegates);
            return;
        }

        // Compile the body as DynamicMethod(LispObject[] args) -> LispObject
        var dm = new DynamicMethod(name, typeof(LispObject),
            new[] { typeof(LispObject[]) }, typeof(CilAssembler).Module, true);
        var keyShared = BuildKeySharedDm(name, directDelegates, out var sharedInstrs);

        var innerAsm = new CilAssembler();
        innerAsm._il = dm.GetILGenerator().ForLibrary();
        innerAsm._keyShared = keyShared;
        innerAsm.Assemble(bodyInstrs);

        var del = (Func<LispObject[], LispObject>)dm.CreateDelegate(
            typeof(Func<LispObject[], LispObject>));

        var fn = new LispFunction(del, name, paramNames.Count);
        if (lambdaList != null)
            fn.StoredLambdaList = InternLambdaList(lambdaList, defPkg != null ? Package.FindPackagePhysical(defPkg) : null);
        if (noFrame) fn.SuppressDebugFrame();

        AttachDirectDelegatesDm(fn, name, directDelegates, keyShared, noFrame);

        if (noFrame) fn.SuppressDebugFrame();

        // Store SIL on function when dotcl:*save-sil* is true
        try
        {
            var saveSilSym = Startup.SymInPkg("*SAVE-SIL*", "DOTCL");
            // With a shared body the array entry only forwards to it, so the
            // body worth inspecting is the shared one.
            if (DynamicBindings.Get(saveSilSym) is not Nil)
                fn.Sil = sharedInstrs ?? bodyInstrs;
        }
        catch { }

        // Register on the correct symbol for package-aware lookup.
        // If :pkg was specified, use that package (the defun name's home package).
        // Otherwise fall back to *package* (avoids flat namespace collision).
        // Check foreign CL BEFORE updating flat table to prevent overwriting host builtins.
        Symbol? pkgSym = null;
        bool isForeignCL2 = false;
        Symbol? setfTargetSym = null;  // for (SETF NAME) functions: the target NAME symbol
        if (!IsCompoundFunctionName(name))  // regular function names (incl. symbols whose name starts with '(')
        {
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackagePhysical(defPkg);
                else
                    pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(name);
                    // Don't overwrite inherited CL-package symbol functions.
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
            // (SETF NAME): register fn on the target NAME symbol's SetfFunction slot (Phase 1)
            var targetName = name.Substring(6, name.Length - 7);
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackagePhysical(defPkg);
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
        // fmakunbound precedes defun in the same progn: the assembly-time
        // registration above gets undone by fmakunbound at runtime, so we must
        // re-register after fmakunbound has executed.
        var fnc = AddReRegisterConstant(fn);
        if (pkgSym != null)
        {
            // Symbol-based runtime re-registration (package-aware).
            // Look up symbol by name+package inline to avoid accumulating symbols in _constants.
            _il.Emit(OpCodes.Ldstr, pkgSym.Name);
            _il.Emit(OpCodes.Ldstr, pkgSym.HomePackage?.Name ?? defPkg ?? "");
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            // SymInPkg returns Symbol; the cast that used to be here was a no-op.
            EmitLoadReRegisterConstant(fnc);
            _il.Emit(OpCodes.Call, _registerFunctionOnSymbol);
        }
        if (setfTargetSym != null)
        {
            // Setf function runtime re-registration via sym.SetfFunction (Phase 1)
            _il.Emit(OpCodes.Ldstr, setfTargetSym.Name);
            _il.Emit(OpCodes.Ldstr, setfTargetSym.HomePackage?.Name ?? defPkg ?? "");
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            // SymInPkg returns Symbol; the cast that used to be here was a no-op.
            EmitLoadReRegisterConstant(fnc);
            _il.Emit(OpCodes.Call, _registerSetfFunctionOnSymbol);
        }
    }

    /// <summary>FASL mode only: begin the "build this function once" guard around a
    /// capture-free LAMBDA / FLET / LABELS. Emits `cache ?? { ... }` and returns the
    /// label the constructed value falls through to; the caller must finish with
    /// EndFaslFunctionCache.
    ///
    /// In-process assembly builds the LispFunction at assembly time and loads it as
    /// a constant, so a capture-free local function costs nothing per call. The FASL
    /// emitter had no equivalent: it emitted the construction itself, so every call
    /// of a FASL-compiled function containing one allocated a fresh LispFunction, a
    /// delegate and a wrapper -- 376 bytes a call, measured. Everything ASDF builds
    /// is a FASL, so that was every library.
    ///
    /// One instance per code site is what "captures nothing" means, and it is what
    /// the in-process path already produces, so this also removes a divergence
    /// between the two. Racing threads may build two and store either; both are
    /// equivalent, exactly as for the closure-delegate cache.</summary>
    private System.Reflection.Emit.Label BeginFaslFunctionCache(
        int fnId, out System.Reflection.Emit.FieldBuilder field)
    {
        field = _faslTypeBuilder!.DefineField($"fn_cache_{fnId}", typeof(LispFunction),
            FieldAttributes.Public | FieldAttributes.Static);
        var done = _il.DefineLabel();
        _il.Emit(OpCodes.Ldsfld, field);
        _il.Emit(OpCodes.Dup);
        _il.Emit(OpCodes.Brtrue, done);
        _il.Emit(OpCodes.Pop);
        return done;
    }

    private void EndFaslFunctionCache(System.Reflection.Emit.FieldBuilder field,
                                      System.Reflection.Emit.Label done)
    {
        _il.Emit(OpCodes.Dup);
        _il.Emit(OpCodes.Stsfld, field);
        _il.MarkLabel(done);
    }

    /// <summary>(:once INSTR...): run INSTR..., which leave one LispObject, the
    /// first time this point is reached, and push that same object every time
    /// after. The value lives in a static field of the FASL type, or in a cell
    /// held by the compilation unit. INSTR... must not need an empty stack (no
    /// exception blocks): the compiler emits a hoisted function and a call.</summary>
    private void HandleOnce(Cons instr)
    {
        var body = instr.Cdr;
        var done = _il.DefineLabel();
        if (_faslMode && _faslTypeBuilder != null)
        {
            int id = Interlocked.Increment(ref _faslClosureCount);
            var field = _faslTypeBuilder.DefineField($"once_{id}", typeof(LispObject),
                FieldAttributes.Public | FieldAttributes.Static);
            _il.Emit(OpCodes.Ldsfld, field);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Brtrue, done);
            _il.Emit(OpCodes.Pop);
            Assemble(body);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Stsfld, field);
            _il.MarkLabel(done);
            return;
        }
        var cellLocal = _il.DeclareLocal(typeof(OnceCell));
        var valueLocal = _il.DeclareLocal(typeof(LispObject));
        _il.Emit(OpCodes.Ldc_I4, _currentUnitId);
        _il.Emit(OpCodes.Ldc_I4, AddUnitConstant(new OnceCell()));
        _il.Emit(OpCodes.Call, _getUnitConstant);
        EmitAs(typeof(OnceCell));
        _il.EmitLoc(OpCodes.Stloc, cellLocal);
        _il.EmitLoc(OpCodes.Ldloc, cellLocal);
        _il.Emit(OpCodes.Ldfld, typeof(OnceCell).GetField(nameof(OnceCell.Value))!);
        _il.Emit(OpCodes.Dup);
        _il.Emit(OpCodes.Brtrue, done);
        _il.Emit(OpCodes.Pop);
        Assemble(body);
        _il.EmitLoc(OpCodes.Stloc, valueLocal);
        _il.EmitLoc(OpCodes.Ldloc, cellLocal);
        _il.EmitLoc(OpCodes.Ldloc, valueLocal);
        _il.Emit(OpCodes.Stfld, typeof(OnceCell).GetField(nameof(OnceCell.Value))!);
        _il.EmitLoc(OpCodes.Ldloc, valueLocal);
        _il.MarkLabel(done);
    }

    private void HandleMakeFunction(Cons instr)
    {
        // (:make-function :param-count N :body (...) [:name "NAME"] [:ll-shape "o2k1r"])
        // :ll-shape is a diagnostic lambda-list shape tag from the compiler,
        // embedded in the DynamicMethod name ("lambda_o2k1r") so InvokeSlow
        // statistics (AnonOriginTag) can break "<anon:lambda>" down by shape.
        var plist = instr.Cdr;
        int paramCount = 0;
        LispObject? bodyInstrs = null;
        string? fnName = null;
        bool noFrame = false;
        string llShape = "";
        LispObject? lambdaList = null;   // development info; see SetFaslLambdaList
        LispObject? directDelegates = null;

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAM-COUNT": paramCount = GetInt(val); break;
                case "BODY": bodyInstrs = val; break;
                case "NAME": fnName = val is LispString s ? s.Value : null; break;
                case "NO-FRAME": noFrame = true; break;
                case "LL-SHAPE": llShape = val is LispString ls ? ls.Value : ""; break;
                case "LAMBDA-LIST": lambdaList = val; break;
                case "DIRECT-DELEGATES": directDelegates = val; break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("MAKE-FUNCTION: missing :body");

        if (_faslMode && _faslTypeBuilder != null)
        {
            int fnId = Interlocked.Increment(ref _faslClosureCount);
            var cacheDone = BeginFaslFunctionCache(fnId, out var cacheField);
            string methodName = $"fn_{fnId}";
            var method = _faslTypeBuilder.DefineMethod(methodName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), new[] { typeof(LispObject[]) });
            var innerAsm = new CilAssembler();
            innerAsm._il = method.GetILGenerator().ForLibrary();
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _faslTypeBuilder;
            innerAsm._faslStructMap = _faslStructMap;
            var keyShared = FaslAssembler.BuildKeySharedMethod(_faslTypeBuilder, _faslStructMap!, methodName, directDelegates);
            innerAsm._keyShared = keyShared;
            AssembleFaslBody(method, innerAsm, bodyInstrs);
            SetFaslLambdaList(method, lambdaList);
            var extraDirect = FaslAssembler.BuildDirectDelegateMethods(_faslTypeBuilder, _faslStructMap!, methodName, directDelegates, keyShared);

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
            FaslAssembler.EmitInstallDirectOnStack(_il, extraDirect);
            EndFaslFunctionCache(cacheField, cacheDone);
        }
        else
        {
            var dm = new DynamicMethod(llShape.Length > 0 ? "lambda_" + llShape : "lambda",
                typeof(LispObject),
                new[] { typeof(LispObject[]) }, typeof(CilAssembler).Module, true);
            var keyShared = BuildKeySharedDm(dm.Name, directDelegates, out _);
            var innerAsm = new CilAssembler();
            innerAsm._il = dm.GetILGenerator().ForLibrary();
            innerAsm._keyShared = keyShared;
            int before = _currentUnitDms?.Count ?? 0;
            innerAsm.Assemble(bodyInstrs);

            var del = (Func<LispObject[], LispObject>)dm.CreateDelegate(
                typeof(Func<LispObject[], LispObject>));
            var fn = new LispFunction(del, fnName, paramCount);
            if (lambdaList != null) fn.StoredLambdaList = InternLambdaList(lambdaList, null);
            AttachDirectDelegatesDm(fn, dm.Name, directDelegates, keyShared);

            // Push the function onto the stack (unit-scoped so a dropped compile
            // result is collectible: S5).
            if (noFrame) fn.SuppressDebugFrame();
            PushUnitFunction(fn, before);
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
        bool noFrame = false;
        LispObject? lambdaList = null;   // development info; see SetFaslLambdaList

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "PARAM-COUNT": paramCount = GetInt(val); break;
                case "BODY": bodyInstrs = val; break;
                case "NAME": fnName = val is LispString s ? s.Value : null; break;
                case "NO-FRAME": noFrame = true; break;
                case "LAMBDA-LIST": lambdaList = val; break;
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
            var cacheDone = BeginFaslFunctionCache(fnId, out var cacheField);
            string bodyName = $"fnd_body_{fnId}";
            var bodyMethod = _faslTypeBuilder.DefineMethod(bodyName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), directParamTypes);
            var innerAsm = new CilAssembler();
            innerAsm._il = bodyMethod.GetILGenerator().ForLibrary();
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _faslTypeBuilder;
            innerAsm._faslStructMap = _faslStructMap;
            bool fndEntry = fnName == null && paramCount <= 4;
            AssembleFaslBody(bodyMethod, innerAsm, bodyInstrs);

            // Create wrapper: (LispObject[] args) -> LispObject
            string wrapperName = $"fnd_{fnId}";
            var wrapperMethod = _faslTypeBuilder.DefineMethod(wrapperName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), new[] { typeof(LispObject[]) });
            var wil = wrapperMethod.GetILGenerator().ForLibrary();
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
            SetFaslLambdaList(wrapperMethod, lambdaList);

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
                if (BodyReadsMvMode(bodyInstrs))
                {
                    _il.Emit(OpCodes.Dup);
                    _il.Emit(OpCodes.Ldc_I4, paramCount);
                    _il.Emit(OpCodes.Callvirt, FaslAssembler.MarkMvModeEntryMI);
                }
                if (fndEntry)
                {
                    // Keep the LispFunction-typed value for the fn_cache store.
                    _il.Emit(OpCodes.Dup);
                    _il.Emit(OpCodes.Ldftn, DefineFndEntry(_faslTypeBuilder, $"fnd_entry_{fnId}", bodyMethod, paramCount));
                    _il.Emit(OpCodes.Ldc_I4, paramCount);
                    _il.Emit(OpCodes.Call, _withClosureEntry);
                    _il.Emit(OpCodes.Pop);
                }
            }
            EndFaslFunctionCache(cacheField, cacheDone);
            return;
        }

        // Create DynamicMethod with direct LispObject params
        var directParamTypes2 = new Type[paramCount];
        for (int i = 0; i < paramCount; i++) directParamTypes2[i] = typeof(LispObject);

        var directDm = new DynamicMethod("lambda_direct", typeof(LispObject),
            directParamTypes2, typeof(CilAssembler).Module, true);
        var innerAsm2 = new CilAssembler();
        innerAsm2._il = directDm.GetILGenerator().ForLibrary();
        int before = _currentUnitDms?.Count ?? 0;
        innerAsm2.Assemble(bodyInstrs);
        bool mvEntry = BodyReadsMvMode(bodyInstrs);
        var storedLambdaList = lambdaList != null ? InternLambdaList(lambdaList, null) : null;

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
                if (noFrame) fn.SuppressDebugFrame();
                if (mvEntry) fn.MarkMvModeEntry(paramCount);
                fn.StoredLambdaList = storedLambdaList;
            PushUnitFunction(fn, before);
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
                if (noFrame) fn.SuppressDebugFrame();
                if (mvEntry) fn.MarkMvModeEntry(paramCount);
                fn.StoredLambdaList = storedLambdaList;
            PushUnitFunction(fn, before);
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
                if (noFrame) fn.SuppressDebugFrame();
                if (mvEntry) fn.MarkMvModeEntry(paramCount);
                fn.StoredLambdaList = storedLambdaList;
            PushUnitFunction(fn, before);
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
                if (noFrame) fn.SuppressDebugFrame();
                if (mvEntry) fn.MarkMvModeEntry(paramCount);
                fn.StoredLambdaList = storedLambdaList;
            PushUnitFunction(fn, before);
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
                if (noFrame) fn.SuppressDebugFrame();
                if (mvEntry) fn.MarkMvModeEntry(paramCount);
                fn.StoredLambdaList = storedLambdaList;
            PushUnitFunction(fn, before);
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
                if (noFrame) fn.SuppressDebugFrame();
                if (mvEntry) fn.MarkMvModeEntry(paramCount);
                fn.StoredLambdaList = storedLambdaList;
            PushUnitFunction(fn, before);
                return;
            }
        }
    }

    /// <summary>The :FORWARD of a :DEFMETHOD-DIRECT form: 0 none, 1 :DIRECT, 2 :INLINE.</summary>
    internal static int SameFileForwardKind(Cons instr)
    {
        for (var p = Cdr(instr.Cdr); p is Cons pc && pc.Cdr is Cons pv; p = pv.Cdr)
            if (pc.Car is Symbol k && k.Name == "FORWARD")
                return pv.Car is Symbol v ? (v.Name == "INLINE" ? 2 : v.Name == "DIRECT" ? 1 : 0) : 0;
        return 0;
    }

    /// <summary>BODY without its (:TAIL-SELF ...) mark: no call in it is made a tail call.</summary>
    internal static LispObject WithoutTailSelf(LispObject body)
    {
        LispObject head = Nil.Instance;
        Cons? tail = null;
        for (var cur = body; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is Cons ic && ic.Car is Symbol s && s.Name == "TAIL-SELF") continue;
            var cell = new Cons(c.Car, Nil.Instance);
            if (tail == null) head = cell; else tail.Cdr = cell;
            tail = cell;
        }
        return head;
    }

    private void HandleDefmethodDirect(Cons instr)
    {
        // (:defmethod-direct "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
        // Body uses (:ldarg 0), (:ldarg 1), ... for direct param access
        var plist = instr.Cdr;
        var name = GetString(Car(plist));
        bool noFrame = false;
        plist = Cdr(plist); // skip name

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;
        bool selfArg0 = false;
        LispObject? lambdaList = null;   // development info; see HandleDefmethod

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "NO-FRAME": noFrame = true; break;
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
                case "SELF":
                    // this direct fn does a non-tail self-call; the body expects
                    // the self LispFunction as arg0 (params shifted to ldarg 1..n).
                    selfArg0 = val is not Nil;
                    break;
                case "LAMBDA-LIST":
                    lambdaList = val;
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
                name, paramCount, bodyInstrs, defPkg, faslId, selfArg0, noFrame: noFrame,
                lambdaList: lambdaList != null ? FaslLambdaListText(lambdaList, defPkg) : null,
                forward: SameFileForwardKind(instr));
            return;
        }

        // Create DynamicMethod with direct LispObject params. When selfArg0, the
        // method takes a leading LispFunction self so a non-tail self-call reaches the
        // receiver from arg0 instead of re-resolving #'NAME each recursive entry.
        bool selfFramed = CanSelfFrame(name, paramCount, noFrame);
        var directParamTypes = new Type[selfArg0 || selfFramed ? paramCount + 1 : paramCount];
        if (selfArg0 || selfFramed)
        {
            directParamTypes[0] = typeof(LispFunction);
            for (int i = 0; i < paramCount; i++) directParamTypes[i + 1] = typeof(LispObject);
        }
        else
        {
            for (int i = 0; i < paramCount; i++) directParamTypes[i] = typeof(LispObject);
        }

        var directDm = new DynamicMethod(name + "_direct", typeof(LispObject),
            directParamTypes, typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = directDm.GetILGenerator().ForLibrary();
        bool leaf = selfFramed && !selfArg0 && paramCount <= 4 && IsDirectLeaf(bodyInstrs, false);
        if (leaf)
        {
            innerAsm._directLeaf = (string.Intern(name), paramCount);
            innerAsm._argShift = 1;
        }
        else if (selfFramed)
        {
            innerAsm._selfFrameName = string.Intern(name);
            innerAsm._selfFrameArgc = paramCount;
            innerAsm._selfFrameUnit = true;
            if (!selfArg0) innerAsm._argShift = 1;
        }
        innerAsm.Assemble(bodyInstrs);

        // Create array-based wrapper with arity check and LispFunction with direct delegate
        Func<LispObject[], LispObject> arrayDel;
        LispFunction fn;
        if (selfArg0 || selfFramed)
        {
            fn = BuildSelfDirectFunction(directDm, name, paramCount);
            if (selfFramed) fn.SetSelfFramedEntry(DynamicMethodEntry(directDm), paramCount);
            if (leaf) fn.MarkLeafEntry(paramCount);
        }
        else
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

        if (lambdaList != null)
            fn.StoredLambdaList = InternLambdaList(lambdaList, defPkg != null ? Package.FindPackagePhysical(defPkg) : null);
        if (noFrame) fn.SuppressDebugFrame();
        if (BodyReadsMvMode(bodyInstrs)) fn.MarkMvModeEntry(paramCount);

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
        // Check foreign CL BEFORE updating flat table to prevent overwriting host builtins.
        Symbol? pkgSym = null;
        bool isForeignCL = false;
        Symbol? setfTargetSym = null;  // for (SETF NAME) functions: the target NAME symbol
        if (!IsCompoundFunctionName(name))
        {
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackagePhysical(defPkg);
                else
                    pkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
                if (pkg != null)
                {
                    var (s, _) = pkg.Intern(name);
                    // Don't overwrite inherited CL-package symbol functions from other packages.
                    // e.g. SB-C uses CL, so (defun compile-file ...) in SB-C would
                    // overwrite CL:COMPILE-FILE: protect the host's built-in functions.
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
            // (SETF NAME): register fn on the target NAME symbol's SetfFunction slot
            // HandleDefun had this, but HandleDefunDirect was missing it: causing
            // (defun (setf foo) ...) to silently fail to register when use-direct=true.
            var targetName = name.Substring(6, name.Length - 7);
            try
            {
                Package? pkg;
                if (defPkg != null)
                    pkg = Package.FindPackagePhysical(defPkg);
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
        var fnc = AddReRegisterConstant(fn);
        if (pkgSym != null)
        {
            _il.Emit(OpCodes.Ldstr, pkgSym.Name);
            _il.Emit(OpCodes.Ldstr, pkgSym.HomePackage?.Name ?? defPkg ?? "");
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            // SymInPkg returns Symbol; the cast that used to be here was a no-op.
            EmitLoadReRegisterConstant(fnc);
            _il.Emit(OpCodes.Call, _registerFunctionOnSymbol);
        }
        if (setfTargetSym != null)
        {
            _il.Emit(OpCodes.Ldstr, setfTargetSym.Name);
            _il.Emit(OpCodes.Ldstr, setfTargetSym.HomePackage?.Name ?? defPkg ?? "");
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            // SymInPkg returns Symbol; the cast that used to be here was a no-op.
            EmitLoadReRegisterConstant(fnc);
            _il.Emit(OpCodes.Call, _registerSetfFunctionOnSymbol);
        }
    }

    // Build the LispFunction for a self-threaded direct function, arities 0-8.
    // directDm is the static method LispObject M(LispFunction self, LispObject p0...).
    // _funcN gets a closed delegate over fn (self bound) so a non-tail self-call
    // (LispFunction.InvokeN -> _funcN) reaches the receiver with no per-entry symbol
    // lookup. The array wrapper is the slow apply/arity-mismatch fallback; it can't
    // capture fn at construction so it uses fnRef, assigned right after construction.
    private static LispFunction BuildSelfDirectFunction(DynamicMethod directDm, string name, int paramCount)
    {
        var n = name;
        LispFunction fnRef = null!;
        LispFunction fn;
        switch (paramCount)
        {
            case 0:
            {
                var o = (Func<LispFunction, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 0); return o(fnRef); }, name, 0);
                fnRef = fn;
                fn._func0 = (Func<LispObject>)directDm.CreateDelegate(typeof(Func<LispObject>), fn);
                break;
            }
            case 1:
            {
                var o = (Func<LispFunction, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 1); return o(fnRef, args[0]); }, name, 1);
                fnRef = fn;
                fn._func1 = (Func<LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject>), fn);
                break;
            }
            case 2:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 2); return o(fnRef, args[0], args[1]); }, name, 2);
                fnRef = fn;
                fn._func2 = (Func<LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject>), fn);
                break;
            }
            case 3:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 3); return o(fnRef, args[0], args[1], args[2]); }, name, 3);
                fnRef = fn;
                fn._func3 = (Func<LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject>), fn);
                break;
            }
            case 4:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 4); return o(fnRef, args[0], args[1], args[2], args[3]); }, name, 4);
                fnRef = fn;
                fn._func4 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject>), fn);
                break;
            }
            case 5:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 5); return o(fnRef, args[0], args[1], args[2], args[3], args[4]); }, name, 5);
                fnRef = fn;
                fn._func5 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>), fn);
                break;
            }
            case 6:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 6); return o(fnRef, args[0], args[1], args[2], args[3], args[4], args[5]); }, name, 6);
                fnRef = fn;
                fn._func6 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>), fn);
                break;
            }
            case 7:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 7); return o(fnRef, args[0], args[1], args[2], args[3], args[4], args[5], args[6]); }, name, 7);
                fnRef = fn;
                fn._func7 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>), fn);
                break;
            }
            case 8:
            {
                var o = (Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispFunction, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>));
                fn = new LispFunction(args => { Runtime.CheckArityExact(n, args, 8); return o(fnRef, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]); }, name, 8);
                fnRef = fn;
                fn._func8 = (Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>)directDm.CreateDelegate(typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>), fn);
                break;
            }
            default:
                throw new Exception($"DEFMETHOD-DIRECT :self unsupported param-count {paramCount}");
        }
        return fn;
    }

    // (:defmethod-native "NAME" [:pkg "PKG"] :params ("P1" ...) :body (...))
    // Body uses Int64 params (declared via (:declare-local KEY "Int64")).
    // Self-calls via (:callvirt "LispFunction.InvokeNativeN") leave long on stack.
    private void HandleDefmethodNative(Cons instr)
    {
        var plist = instr.Cdr;
        var name = GetString(Car(plist));
        bool noFrame = false;
        plist = Cdr(plist);

        var paramNames = new List<string>();
        LispObject? bodyInstrs = null;
        string? defPkg = null;
        LispObject? lambdaList = null;   // development info; see HandleDefmethod

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "NO-FRAME": noFrame = true; break;
                case "PARAMS":
                    var cur = val;
                    while (cur is Cons lc) { paramNames.Add(GetString(lc.Car)); cur = lc.Cdr; }
                    break;
                case "LAMBDA-LIST": lambdaList = val; break;
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
                name, paramCount, bodyInstrs, defPkg, faslId, noFrame: noFrame,
                lambdaList: lambdaList != null ? FaslLambdaListText(lambdaList, defPkg) : null);
            return;
        }

        // Native DynamicMethod: LispObject Name_native(LispFunction self, long p0, ...)
        // arg0 is the LispFunction itself, threaded through so a native self-call reaches
        // the receiver from arg0 instead of re-resolving #'NAME from its symbol each call
        // (the old per-call self-fn prelude). Long params avoid arg boxing; body
        // returns LispObject (arithmetic already boxes via Fixnum.Make).
        var nativeParamTypes = new Type[paramCount + 1];
        nativeParamTypes[0] = typeof(LispFunction);
        for (int i = 0; i < paramCount; i++) nativeParamTypes[i + 1] = typeof(long);
        var nativeDm = new DynamicMethod(name + "_native", typeof(LispObject), nativeParamTypes,
            typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = nativeDm.GetILGenerator().ForLibrary();
        innerAsm.Assemble(bodyInstrs);

        // Create LispFunction with wrapper lambdas (unbox args; native returns LispObject
        // directly). Wrappers pass `fn` as the leading self argument. fnRef is assigned
        // after construction so the array-arg lambda can capture it before any call runs.
        LispFunction fn;
        LispFunction fnRef = null!;
        switch (paramCount)
        {
            case 1:
            {
                var d = (Func<LispFunction, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<LispFunction, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 1); return d(fnRef, Runtime.TypedArgL(args[0])); },
                    name, 1);
                fnRef = fn;
                fn._func1 = a => d(fn, Runtime.TypedArgL(a));
                fn.SetNativeDelegate(d);
                break;
            }
            case 2:
            {
                var d = (Func<LispFunction, long, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<LispFunction, long, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 2); return d(fnRef, Runtime.TypedArgL(args[0]), Runtime.TypedArgL(args[1])); },
                    name, 2);
                fnRef = fn;
                fn._func2 = (a, b) => d(fn, Runtime.TypedArgL(a), Runtime.TypedArgL(b));
                fn.SetNativeDelegate(d);
                break;
            }
            case 3:
            {
                var d = (Func<LispFunction, long, long, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<LispFunction, long, long, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 3); return d(fnRef, Runtime.TypedArgL(args[0]), Runtime.TypedArgL(args[1]), Runtime.TypedArgL(args[2])); },
                    name, 3);
                fnRef = fn;
                fn._func3 = (a, b, c) => d(fn, Runtime.TypedArgL(a), Runtime.TypedArgL(b), Runtime.TypedArgL(c));
                fn.SetNativeDelegate(d);
                break;
            }
            case 4:
            {
                var d = (Func<LispFunction, long, long, long, long, LispObject>)nativeDm.CreateDelegate(typeof(Func<LispFunction, long, long, long, long, LispObject>));
                var n = name;
                fn = new LispFunction(
                    args => { Runtime.CheckArityExact(n, args, 4); return d(fnRef, Runtime.TypedArgL(args[0]), Runtime.TypedArgL(args[1]), Runtime.TypedArgL(args[2]), Runtime.TypedArgL(args[3])); },
                    name, 4);
                fnRef = fn;
                fn._func4 = (a, b, c, dd) => d(fn, Runtime.TypedArgL(a), Runtime.TypedArgL(b), Runtime.TypedArgL(c), Runtime.TypedArgL(dd));
                fn.SetNativeDelegate(d);
                break;
            }
            default: throw new Exception("unreachable");
        }

        FinishInMemoryDefmethod(fn, name, defPkg, lambdaList, noFrame, bodyInstrs);
    }

    // The part of a DEFMETHOD-NATIVE / DEFMETHOD-TYPED in memory that comes after the
    // LispFunction is built: development info, the debugger-frame policy, the saved
    // SIL and the registration on the name.
    private void FinishInMemoryDefmethod(LispFunction fn, string name, string? defPkg,
        LispObject? lambdaList, bool noFrame, LispObject bodyInstrs)
    {
        if (lambdaList != null)
            fn.StoredLambdaList = InternLambdaList(lambdaList, defPkg != null ? Package.FindPackagePhysical(defPkg) : null);
        if (noFrame) fn.SuppressDebugFrame();

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
        if (!IsCompoundFunctionName(name))
        {
            try
            {
                Package? pkg = defPkg != null ? Package.FindPackagePhysical(defPkg)
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
                Package? pkg = defPkg != null ? Package.FindPackagePhysical(defPkg)
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

        var fnc = AddReRegisterConstant(fn);
        if (pkgSym != null)
        {
            _il.Emit(OpCodes.Ldstr, pkgSym.Name);
            _il.Emit(OpCodes.Ldstr, pkgSym.HomePackage?.Name ?? defPkg ?? "");
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            // SymInPkg returns Symbol; the cast that used to be here was a no-op.
            EmitLoadReRegisterConstant(fnc);
            _il.Emit(OpCodes.Call, _registerFunctionOnSymbol);
        }
        if (setfTargetSym != null)
        {
            _il.Emit(OpCodes.Ldstr, setfTargetSym.Name);
            _il.Emit(OpCodes.Ldstr, setfTargetSym.HomePackage?.Name ?? defPkg ?? "");
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            // SymInPkg returns Symbol; the cast that used to be here was a no-op.
            EmitLoadReRegisterConstant(fnc);
            _il.Emit(OpCodes.Call, _registerSetfFunctionOnSymbol);
        }
    }

    private void HandleMakeClosure(Cons instr)
    {
        // Stack already has: object[] env (built by preceding instructions)
        // (:make-closure :param-count N :env-size M :env-map (...) :body (...)
        //                [:direct t :fn-name "NAME"])
        // :direct marks a required-only (<=6 params) closure whose body reads
        // params exclusively via :load-arg and omits the arity-check prefix;
        // eligible for the per-arity direct-delegate path.
        var plist = instr.Cdr;
        int paramCount = 0;
        int envSize = 0;
        LispObject? bodyInstrs = null;
        LispObject? envMap = null;
        bool direct = false;
        string fnName = "";
        LispObject? lambdaList = null;   // development info; see SetFaslLambdaList

        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "LAMBDA-LIST": lambdaList = val; break;
                case "PARAM-COUNT": paramCount = GetInt(val); break;
                case "ENV-SIZE": envSize = GetInt(val); break;
                case "ENV-MAP": envMap = val; break;
                case "BODY": bodyInstrs = val; break;
                case "DIRECT": direct = val is not Nil; break;
                case "FN-NAME": fnName = GetString(val); break;
            }
            plist = Cddr(pc);
        }

        if (bodyInstrs == null) throw new Exception("MAKE-CLOSURE: missing :body");

        // Determine which env slots are boxed from env-map. "boxed" cells are
        // LispObject[1] (the normal representation, and labels function cells);
        // "lispbox" cells are the debug-only LispBox class (data variables under
        // DOTCL_EMIT_PDB: see LispBox). They cast differently on load.
        var boxedSlots = new HashSet<int>();
        var lispboxSlots = new HashSet<int>();
        if (envMap != null)
        {
            var cur = envMap;
            while (cur is Cons mc)
            {
                // Each entry is (name index "boxed"|"lispbox"|"value")
                if (mc.Car is Cons entry)
                {
                    var entryList = ListToArray(entry);
                    if (entryList.Length >= 3)
                    {
                        int slotIdx = GetInt(entryList[1]);
                        string kind = GetString(entryList[2]);
                        if (kind == "boxed")
                            boxedSlots.Add(slotIdx);
                        else if (kind == "lispbox")
                            lispboxSlots.Add(slotIdx);
                    }
                }
                cur = mc.Cdr;
            }
        }

        if (_faslMode && _faslTypeBuilder != null)
        {
            // A .fasl closure gets the same per-arity direct body the in-process
            // path gets. Emitting it on the args-array signature meant every call
            // to a closure defined in a compiled library went through the
            // args-array wrapper, one LispObject[] allocated per call, because
            // _funcN can only be installed when the body's signature carries the
            // arguments. That is the whole difference between a closure a session
            // compiled and the same closure loaded from a .fasl.
            bool faslDirect = direct && paramCount <= 6;
            int closureId = Interlocked.Increment(ref _faslClosureCount);
            string closureName = $"closure_{closureId}";
            Type[] closureParams;
            if (faslDirect)
            {
                closureParams = new Type[paramCount + 1];
                closureParams[0] = typeof(object[]);
                for (int i = 1; i < closureParams.Length; i++) closureParams[i] = typeof(LispObject);
            }
            else
            {
                closureParams = new[] { typeof(object[]), typeof(LispObject[]) };
            }
            var closureMethod = _faslTypeBuilder.DefineMethod(closureName,
                MethodAttributes.Public | MethodAttributes.Static,
                typeof(LispObject), closureParams);
            // Name the plumbing parameters so the debugger's Locals shows "env" /
            // "args" instead of two entries called "value" (unnamed SRE params).
            closureMethod.DefineParameter(1, ParameterAttributes.None, "env");
            if (faslDirect)
                for (int i = 0; i < paramCount; i++)
                    closureMethod.DefineParameter(i + 2, ParameterAttributes.None, $"a{i}");
            else
                closureMethod.DefineParameter(2, ParameterAttributes.None, "args");

            var innerAsm = new CilAssembler();
            innerAsm._il = closureMethod.GetILGenerator().ForLibrary();
            innerAsm._faslMode = true;
            innerAsm._faslTypeBuilder = _faslTypeBuilder;
            innerAsm._faslStructMap = _faslStructMap;
            innerAsm._boxedEnvSlots = boxedSlots;
            innerAsm._lispboxEnvSlots = lispboxSlots;
            // A :direct body reads its parameters as arguments (:load-arg i ->
            // ldarg i+1) and omits the compiler's arity-check prefix, because the
            // delegate signature makes argc structural and MakeDirectClosure's
            // args-array wrapper runs the same Runtime.CheckArityExact. When the
            // body cannot take that signature (more than 6 parameters), it falls
            // back to the args-array shape and the omitted check is put back.
            if (faslDirect) innerAsm._directParams = true;
            else if (direct) EmitDirectFallbackArityCheck(innerAsm._il, fnName, paramCount);
            bool closureEntry = faslDirect && paramCount <= 4;
            // The entry does not run InvokeN's stack check, so the body does
            // (twice as often through InvokeN, which is harmless).
            if (closureEntry) innerAsm._il.Emit(OpCodes.Call, _closureEntryCheck);
            AssembleFaslBody(closureMethod, innerAsm, bodyInstrs);
            SetFaslLambdaList(closureMethod, lambdaList);

            // Stack has: object[] env
            // First store env in a local, then build delegate
            var envLocal = _il.DeclareLocal(typeof(object[]));
            _il.EmitLoc(OpCodes.Stloc, envLocal);

            var closureDelType = faslDirect
                ? DirectClosureDelegateType(paramCount)
                : typeof(Func<object[], LispObject[], LispObject>);
            // The body delegate is over a static method with no target, so one
            // per code site serves every closure the site makes: it is kept in a
            // static field of the FASL type and built the first time. Building it
            // per closure was a 64-byte delegate on top of every closure. Racing
            // threads may build two and store either; both are equivalent, as for
            // the in-process closure-delegate cache.
            var delField = _faslTypeBuilder.DefineField($"closure_del_{closureId}",
                closureDelType, FieldAttributes.Public | FieldAttributes.Static);
            var haveDel = _il.DefineLabel();
            _il.Emit(OpCodes.Ldsfld, delField);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Brtrue, haveDel);
            _il.Emit(OpCodes.Pop);
            _il.Emit(OpCodes.Ldnull);
            _il.Emit(OpCodes.Ldftn, closureMethod);
            _il.Emit(OpCodes.Newobj,
                closureDelType.GetConstructor(new[] { typeof(object), typeof(IntPtr) })!);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Stsfld, delField);
            _il.MarkLabel(haveDel);

            _il.EmitLoc(OpCodes.Ldloc, envLocal);
            if (faslDirect)
            {
                // LispFunction.MakeDirectClosure(del, env, fnName): the same
                // factory MakeClosureDirect uses, which installs _funcN plus the
                // arity-checking args-array wrapper. No RetainUnit here: the body
                // lives in the loaded assembly, not in a collectible unit holder.
                _il.Emit(OpCodes.Ldstr, Track(fnName));
                _il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod("MakeDirectClosure",
                    new[] { typeof(Delegate), typeof(object[]), typeof(string) })!);
                if (BodyReadsMvMode(bodyInstrs)) EmitWithMvModeEntry(_il, paramCount);
                if (closureEntry)
                {
                    _il.Emit(OpCodes.Ldftn, DefineClosureEntry(_faslTypeBuilder, closureName + "_entry", closureMethod, paramCount));
                    _il.Emit(OpCodes.Ldc_I4, paramCount);
                    _il.Emit(OpCodes.Call, _withClosureEntry);
                }
            }
            else
            {
                // new LispFunction(closureDel, env, null, paramCount)
                _il.Emit(OpCodes.Ldnull);   // name
                _il.Emit(OpCodes.Ldc_I4, paramCount);
                _il.Emit(OpCodes.Newobj, typeof(LispFunction).GetConstructor(new[] {
                    typeof(Func<object[], LispObject[], LispObject>),
                    typeof(object[]), typeof(string), typeof(int) })!);
            }
        }
        else if (direct && paramCount <= 6)
        {
            // Direct-params mode: body DM signature is (object[] env,
            // LispObject a0..aN-1); :load-arg i maps to ldarg (i+1). The
            // compiler only marks :direct on required-only bodies that read
            // params exclusively via :load-arg (and omits the arity-check
            // prefix: MakeDirectClosure's args-array wrapper performs it).
            // Up to four parameters the body is also CallN's entry: its first
            // argument is the function (from CallN), whose environment it reads
            // and which it holds until it returns (it may be the only reference
            // to the unit), or the environment itself (from the delegate, whose
            // caller holds the function).
            bool withEntry = paramCount <= 4 && !s_noUnitClosureEntry && s_dmDescriptor != null;
            var paramTypes = new Type[paramCount + 1];
            paramTypes[0] = withEntry ? typeof(object) : typeof(object[]);
            for (int i = 1; i < paramTypes.Length; i++) paramTypes[i] = typeof(LispObject);
            var dm = new DynamicMethod("lambda_closure_direct", typeof(LispObject),
                paramTypes, typeof(CilAssembler).Module, true);
            var innerAsm = new CilAssembler();
            innerAsm._il = dm.GetILGenerator().ForLibrary();
            innerAsm._boxedEnvSlots = boxedSlots;
            innerAsm._lispboxEnvSlots = lispboxSlots;
            innerAsm._directParams = true;
            innerAsm._selfEnvBody = withEntry;
            if (withEntry) innerAsm.EmitSelfOrEnvPrologue();
            innerAsm.Assemble(bodyInstrs);
            RecordClosureLambdaList(dm, lambdaList);

            int dmIdx = AddUnitConstant(dm);

            // Stack: object[] env (from caller's instructions)
            // Call MakeClosureDirect(env, unitId, dmIdx, paramCount, fnName)
            _il.Emit(OpCodes.Ldc_I4, _currentUnitId);
            _il.Emit(OpCodes.Ldc_I4, dmIdx);
            _il.Emit(OpCodes.Ldc_I4, paramCount);
            _il.Emit(OpCodes.Ldstr, fnName);
            _il.Emit(OpCodes.Call, _makeClosureDirect);
            if (BodyReadsMvMode(bodyInstrs)) EmitWithMvModeEntry(_il, paramCount);
            if (withEntry)
            {
                var entry = DynamicMethodEntry(dm);
                if (entry != IntPtr.Zero)
                {
                    _il.Emit(OpCodes.Ldc_I8, (long)entry);
                    _il.Emit(OpCodes.Conv_I);
                    _il.Emit(OpCodes.Ldc_I4, paramCount);
                    _il.Emit(OpCodes.Call, _withClosureEntry);
                }
            }
        }
        else
        {
            // DynamicMethod mode (original path)
            var dm = new DynamicMethod("lambda_closure", typeof(LispObject),
                new[] { typeof(object[]), typeof(LispObject[]) },
                typeof(CilAssembler).Module, true);
            var innerAsm = new CilAssembler();
            innerAsm._il = dm.GetILGenerator().ForLibrary();

            innerAsm._boxedEnvSlots = boxedSlots;
            innerAsm._lispboxEnvSlots = lispboxSlots;
            // Defensive: a :direct body assembled on the args-array signature
            // (can only happen if the arity gate above and the compiler's gate
            // ever disagree) must get its omitted arity check back.
            if (direct) EmitDirectFallbackArityCheck(innerAsm._il, fnName, paramCount);
            innerAsm.Assemble(bodyInstrs);
            RecordClosureLambdaList(dm, lambdaList);

            // Store DynamicMethod in this compilation unit's holder rather
            // than the global constant pool, so the closure body DM is reclaimed
            // with the unit once no function can still call into it.
            int dmIdx = AddUnitConstant(dm);

            // Stack: object[] env (from caller's instructions)
            // Call MakeClosure(env, unitId, dmIdx, paramCount)
            _il.Emit(OpCodes.Ldc_I4, _currentUnitId);
            _il.Emit(OpCodes.Ldc_I4, dmIdx);
            _il.Emit(OpCodes.Ldc_I4, paramCount);
            _il.Emit(OpCodes.Call, _makeClosure);
        }
    }

    private static readonly MethodInfo _withMvModeEntry =
        typeof(LispFunction).GetMethod("WithMvModeEntry")!;
    private static readonly MethodInfo _withClosureEntry =
        typeof(LispFunction).GetMethod("WithClosureEntry")!;
    private static readonly MethodInfo _closureEntryCheck =
        typeof(LispFunction).GetMethod("ClosureEntryCheck")!;
    private static readonly MethodInfo _getEnvironment =
        typeof(LispFunction).GetProperty("Environment")!.GetGetMethod()!;

    /// <summary>A .fasl closure body's entry for LispFunction.CallN.</summary>
    private static MethodBuilder DefineClosureEntry(TypeBuilder tb, string name, MethodBuilder body, int paramCount)
    {
        var types = new Type[paramCount + 1];
        types[0] = typeof(LispFunction);
        for (int i = 1; i < types.Length; i++) types[i] = typeof(LispObject);
        var m = tb.DefineMethod(name, MethodAttributes.Public | MethodAttributes.Static, typeof(LispObject), types);
        var il = m.GetILGenerator().ForLibrary();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Call, _getEnvironment);
        for (int i = 1; i <= paramCount; i++) il.Emit(OpCodes.Ldarg_S, (byte)i);
        il.Emit(OpCodes.Call, body);
        il.Emit(OpCodes.Ret);
        return m;
    }

    private static readonly bool s_noUnitClosureEntry =
        Environment.GetEnvironmentVariable("DOTCL_NO_UNIT_CLOSURE_ENTRY") == "1";

    private static readonly MethodInfo s_getType = typeof(object).GetMethod("GetType")!;
    private static readonly MethodInfo s_typeFromHandle = typeof(Type).GetMethod("GetTypeFromHandle")!;
    private static readonly MethodInfo s_typeEquals = typeof(Type).GetMethod("op_Equality")!;

    /// <summary>Start of a closure body whose first argument is the function or
    /// its environment: the stack check InvokeN makes, and the environment.
    /// The exact-type test compiles to one method-table compare.</summary>
    private void EmitSelfOrEnvPrologue()
    {
        _il.Emit(OpCodes.Call, _closureEntryCheck);
        var fromEnv = _il.DefineLabel();
        var done = _il.DefineLabel();
        _envLocal = _il.DeclareLocal(typeof(object[]));
        _il.Emit(OpCodes.Ldarg_0);
        _il.Emit(OpCodes.Callvirt, s_getType);
        _il.Emit(OpCodes.Ldtoken, typeof(object[]));
        _il.Emit(OpCodes.Call, s_typeFromHandle);
        _il.Emit(OpCodes.Call, s_typeEquals);
        _il.Emit(OpCodes.Brtrue, fromEnv);
        _il.Emit(OpCodes.Ldarg_0);
        _il.Emit(OpCodes.Castclass, typeof(LispFunction));
        _il.Emit(OpCodes.Call, _getEnvironment);
        _il.Emit(OpCodes.Br, done);
        _il.MarkLabel(fromEnv);
        _il.Emit(OpCodes.Ldarg_0);
        _il.Emit(OpCodes.Castclass, typeof(object[]));
        _il.MarkLabel(done);
        _il.EmitLoc(OpCodes.Stloc, _envLocal);
    }

    /// <summary>The CallN entry of a .fasl lambda with no closed-over variables.</summary>
    private static MethodBuilder DefineFndEntry(TypeBuilder tb, string name, MethodBuilder body, int paramCount)
    {
        var types = new Type[paramCount + 1];
        types[0] = typeof(LispFunction);
        for (int i = 1; i < types.Length; i++) types[i] = typeof(LispObject);
        var m = tb.DefineMethod(name, MethodAttributes.Public | MethodAttributes.Static, typeof(LispObject), types);
        var il = m.GetILGenerator().ForLibrary();
        // The stack check InvokeN makes. It is here and not in the body: every
        // other way into the body (InvokeN, a generic function's frame) checks.
        il.Emit(OpCodes.Call, _closureEntryCheck);
        for (int i = 1; i <= paramCount; i++) il.Emit(OpCodes.Ldarg_S, (byte)i);
        il.Emit(OpCodes.Call, body);
        il.Emit(OpCodes.Ret);
        return m;
    }

    /// <summary>Mark the function on the stack: its ARITY entry, a body that
    /// takes a value mode on entry (see BodyReadsMvMode).</summary>
    private static void EmitWithMvModeEntry(ILGenerator il, int arity)
    {
        il.Emit(OpCodes.Ldc_I4, arity);
        il.Emit(OpCodes.Call, _withMvModeEntry);
    }

    // A :direct closure body omits the compiler's arity-check prefix (the
    // direct-delegate wrapper performs it at call time). When such a body is
    // nevertheless assembled on the args-array signature (FASL mode, or the
    // defensive fallback above), emit the exact 4-instruction check the
    // compiler would have emitted, so the argc-mismatch error is unchanged.
    private static void EmitDirectFallbackArityCheck(ILGenerator il, string fnName, int paramCount)
    {
        il.Emit(OpCodes.Ldstr, fnName);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Ldc_I4, paramCount);
        il.Emit(OpCodes.Call, _checkArityExact);
    }

    // For closure body: which env slots hold boxed (LispObject[]) values
    private HashSet<int>? _boxedEnvSlots;

    // For closure body: which env slots hold debug-only LispBox cells (data
    // variables under DOTCL_EMIT_PDB: see LispBox). Disjoint from _boxedEnvSlots.
    private HashSet<int>? _lispboxEnvSlots;

    // For direct-params closure bodies: :load-arg i maps to ldarg (i+1)
    // (real CLR params) instead of args-array indexing. Set only while
    // assembling a :direct :make-closure body.
    private bool _directParams;

    // A direct-params body whose first argument is the function or its
    // environment, and which holds that argument until it returns.
    private bool _selfEnvBody;
    private LocalBuilder? _envLocal;
    private static readonly MethodInfo s_gcKeepAlive = typeof(GC).GetMethod("KeepAlive")!;

    private void EmitLoadEnv(int index)
    {
        if (_envLocal != null) _il.EmitLoc(OpCodes.Ldloc, _envLocal);
        else _il.Emit(OpCodes.Ldarg_0); // object[] env
        _il.Emit(OpCodes.Ldc_I4, index);
        _il.Emit(OpCodes.Ldelem_Ref);
        if (_lispboxEnvSlots != null && _lispboxEnvSlots.Contains(index))
            _il.Emit(OpCodes.Castclass, typeof(LispBox));
        else if (_boxedEnvSlots != null && _boxedEnvSlots.Contains(index))
            _il.Emit(OpCodes.Castclass, typeof(LispObject[]));
        else
            _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }
}
