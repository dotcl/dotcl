using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Generic function operations ---

    // Use Symbol objects as keys (not string names) so that same-named symbols
    // in different packages (e.g. ASDF:FIND-SYSTEM vs QL-DIST:FIND-SYSTEM) are distinct.
    // For (SETF ...) names, a synthetic key symbol is used via the name's accessor symbol.
    private static readonly ConcurrentDictionary<Symbol, GenericFunction> _gfRegistry = new(SymbolIdentityComparer.Instance);

    /// <summary>Comparer that uses ReferenceEquals for Symbol identity (same object = same key).</summary>
    private class SymbolIdentityComparer : IEqualityComparer<Symbol>
    {
        public static readonly SymbolIdentityComparer Instance = new();
        public bool Equals(Symbol? x, Symbol? y) => ReferenceEquals(x, y);
        public int GetHashCode(Symbol obj) => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj);
    }

    private static string SetfKeyFor(Symbol accessor)
    {
        // Only DOTCL-INTERNAL (and null) symbols get the bare "(SETF NAME)" key;
        // all other packages (including COMMON-LISP) get a package-qualified key.
        // This ensures that the FindGF setf fallback, which searches for bare
        // "(SETF NAME)" keys, can only ever match C#-startup-registered GFs
        // (whose accessor is always a DOTCL-INTERNAL symbol), never user-created
        // GFs like (setf cl:documentation).  Without this, (setf acclimation:doc)
        // bleeds into (setf cl:documentation) via the fallback.
        var pkg = accessor.HomePackage;
        if (pkg == null || pkg.Name == "DOTCL-INTERNAL")
            return $"(SETF {accessor.Name})";
        return $"(SETF {pkg.Name}:{accessor.Name})";
    }

    /// <summary>Key symbol -> accessor, for every key symbol SetfKeySymbol has handed
    /// out. The key symbol is what a (SETF accessor) generic function carries as its
    /// name internally; this is how the name is turned back into the list the user
    /// wrote wherever it leaves the runtime.</summary>
    private static readonly ConcurrentDictionary<Symbol, Symbol> _setfKeyAccessors = new(SymbolIdentityComparer.Instance);

    /// <summary>The key symbol for (SETF accessor), remembering which accessor it stands for.</summary>
    internal static Symbol SetfKeySymbol(Symbol accessor)
    {
        var key = Startup.Sym(SetfKeyFor(accessor));
        _setfKeyAccessors.TryAdd(key, accessor);
        return key;
    }

    /// <summary>The function name as the user spells it: (SETF accessor) for a setf
    /// key symbol, the symbol itself otherwise.</summary>
    internal static LispObject PublicFunctionName(Symbol name)
        => _setfKeyAccessors.TryGetValue(name, out var accessor)
            ? new Cons(Startup.Sym("SETF"), new Cons(accessor, Nil.Instance))
            : name;

    private static Symbol ToFunctionNameSymbol(LispObject name, string context)
    {
        if (name is Symbol sym) return sym;
        // (setf foo) -> intern a stable symbol named "(SETF FOO)" for identity-based registry
        if (name is Cons c && c.Car is Symbol setfSym && setfSym.Name == "SETF"
            && c.Cdr is Cons c2 && c2.Car is Symbol accessor)
        {
            return SetfKeySymbol(accessor);
        }
        throw new LispErrorException(new LispTypeError($"{context}: invalid function name", name));
    }

    public static LispObject MakeGF(LispObject name, LispObject arity)
    {
        var sym = ToFunctionNameSymbol(name, "MAKE-GF");
        int ar = arity is Fixnum f ? (int)f.Value : -1;
        return NewDispatchingGF(sym, ar);
    }

    private static LispMethod FallbackMethodOf(GenericFunction gf, int requiredCount)
    {
        var fb = gf.FallbackFunction!;
        if (gf.FallbackMethod is { } m && ReferenceEquals(m.Function, fb)) return m;
        var tClass = FindClass(Startup.Sym("T"));
        var specs = new LispObject[requiredCount];
        for (int i = 0; i < specs.Length; i++) specs[i] = tClass;
        m = new LispMethod(specs, Array.Empty<LispObject>(), fb);
        m.IgnoresNextMethodState = true;
        gf.FallbackMethod = m;
        return m;
    }

    private static readonly object s_gfRegisterLock = new();

    /// <summary>DEFMETHOD's auto-creation: register GF under NAME unless a generic
    /// function is there already, and return the one that is registered.</summary>
    public static LispObject RegisterGFIfAbsent(LispObject name, LispObject gfObj)
    {
        lock (s_gfRegisterLock)
        {
            if (FindGF(name) is GenericFunction existing) return existing;
            return RegisterGF(name, gfObj);
        }
    }

    public static LispObject RegisterGF(LispObject name, LispObject gfObj)
    {
        var sym = ToFunctionNameSymbol(name, "REGISTER-GF");
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("REGISTER-GF: not a generic function", gfObj));
        lock (s_gfRegisterLock)
            RegisterGFLocked(name, sym, gf);
        return gfObj;
    }

    private static void RegisterGFLocked(LispObject name, Symbol sym, GenericFunction gf)
    {
        _gfRegistry[sym] = gf;
        // When replacing an ordinary function, save it as a fallback so the GF
        // dispatcher can call the original C# implementation for types that have no
        // applicable method (e.g. built-in streams when Gray-stream methods exist).
        if (sym.Function is LispFunction existing && existing is not GenericFunction)
            gf.FallbackFunction ??= existing;
        // Also install as the symbol's function so calls dispatch through the GF.
        // This bypasses CheckPackageLock since extending a CL generic function with
        // user-defined methods (defmethod auto-create case) is allowed even when
        // the CL package is locked.
        sym.Function = gf;
        // For (setf accessor) GFs: ALSO install on the target symbol's SetfFunction slot
        // so that #'(setf accessor) and GetSetfFunctionBySymbol can find it.
        if (name is Cons c && c.Car is Symbol setfKw && setfKw.Name == "SETF"
            && c.Cdr is Cons c2 && c2.Car is Symbol accessor)
        {
            accessor.SetfFunction = gf;
        }
    }

    /// <summary>
    /// Remove a symbol's GF registry entry (used by compile-file cleanup to ensure
    /// that when the compiled fasl is loaded, %find-gf returns NIL and the GF is
    /// properly re-registered with sym.Function set).
    /// </summary>
    public static void RemoveGfRegistryEntry(Symbol sym, bool isSetf = false)
    {
        if (!isSetf)
        {
            _gfRegistry.TryRemove(sym, out _);
        }
        // A setf GF is registered under the key symbol SetfKeyFor builds (what
        // FindGF looks up), not under the accessor symbol. The old loop here
        // matched the accessor symbol itself, so stripping a compile-time
        // (SETF ACC) removed the registry entry of the READER generic function
        // ACC instead: compile-file of a DEFCLASS whose :accessor names an
        // existing generic function made the later load create a fresh one, and
        // every method defined before it was lost.
        else
        {
            _gfRegistry.TryRemove(Startup.Sym(SetfKeyFor(sym)), out _);
        }
    }

    /// <summary>
    /// Remove methods that were defined by inline :method in a previous defgeneric form.
    /// CLHS: "methods defined by previous defgeneric forms are removed."
    /// </summary>
    public static LispObject ClearDefgenericInlineMethods(LispObject gfObj)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("CLEAR-DEFGENERIC-INLINE-METHODS: not a generic function", gfObj));
        // Copy-on-write removal of defgeneric-inline methods under MethodsLock.
        lock (gf.MethodsLock)
        {
            var cur = gf.Methods;
            var kept = new List<LispMethod>(cur.Count);
            foreach (var m in cur)
                if (!m.IsFromDefgenericInline) kept.Add(m);
            if (kept.Count != cur.Count)
                gf.ReplaceMethods(kept.ToArray());
        }
        return gfObj;
    }

    /// <summary>
    /// Mark a method as having been defined by an inline :method in defgeneric.
    /// </summary>
    public static LispObject MarkDefgenericInlineMethod(LispObject gfObj, LispObject methodObj)
    {
        if (methodObj is LispMethod m)
            m.IsFromDefgenericInline = true;
        return methodObj;
    }

    /// <summary>
    /// Set lambda list info on a generic function for congruence checking (CLHS 7.6.4).
    /// Args: gf, required-count, optional-count, has-rest, has-key, has-allow-other-keys
    /// </summary>
    public static LispObject SetGFLambdaListInfo(LispObject[] args)
    {
        if (args[0] is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("SET-GF-LAMBDA-LIST-INFO: not a generic function", args[0]));

        int newRequiredCount = args[1] is Fixnum f1 ? (int)f1.Value : 0;
        int newOptionalCount = args[2] is Fixnum f2 ? (int)f2.Value : 0;
        bool newHasRest = args[3] is not Nil;
        bool newHasKey = args[4] is not Nil;
        bool newHasAllowOtherKeys = args.Length > 5 && args[5] is not Nil;
        var newKeywordNames = new List<string>();
        if (args.Length > 6 && args[6] is not Nil)
        {
            var cur = args[6];
            while (cur is Cons c)
            {
                if (c.Car is Symbol sym)
                    newKeywordNames.Add(sym.Name);
                cur = c.Cdr;
            }
        }

        // CLHS: If defgeneric is re-evaluated and existing methods have lambda lists
        // not congruent with the new GF lambda list, signal program-error.
        if (gf.LambdaListInfoSet && gf.Methods.Count > 0)
        {
            // Temporarily set new values to use CheckLambdaListCongruence
            int oldReq = gf.RequiredCount, oldOpt = gf.OptionalCount;
            bool oldRest = gf.HasRest, oldKey = gf.HasKey, oldAOK = gf.HasAllowOtherKeys;
            var oldKwNames = gf.KeywordNames;

            gf.RequiredCount = newRequiredCount;
            gf.OptionalCount = newOptionalCount;
            gf.HasRest = newHasRest;
            gf.HasKey = newHasKey;
            gf.HasAllowOtherKeys = newHasAllowOtherKeys;
            gf.KeywordNames = newKeywordNames;

            try
            {
                foreach (var method in gf.Methods)
                    CheckLambdaListCongruence(gf, method);
            }
            catch
            {
                // Restore old values on failure
                gf.RequiredCount = oldReq;
                gf.OptionalCount = oldOpt;
                gf.HasRest = oldRest;
                gf.HasKey = oldKey;
                gf.HasAllowOtherKeys = oldAOK;
                gf.KeywordNames = oldKwNames;
                throw;
            }
        }
        else
        {
            gf.RequiredCount = newRequiredCount;
            gf.OptionalCount = newOptionalCount;
            gf.HasRest = newHasRest;
            gf.HasKey = newHasKey;
            gf.HasAllowOtherKeys = newHasAllowOtherKeys;
            gf.KeywordNames = newKeywordNames;
        }

        // Argument precedence order: a permutation of required-parameter
        // indices. Absent/empty -> null = natural left-to-right order. Reset on
        // every defgeneric re-evaluation.
        int[]? apo = null;
        if (args.Length > 7 && args[7] is not Nil)
        {
            var apoList = new List<int>();
            var cur = args[7];
            while (cur is Cons c)
            {
                if (c.Car is Fixnum fi) apoList.Add((int)fi.Value);
                cur = c.Cdr;
            }
            if (apoList.Count > 0) apo = apoList.ToArray();
        }
        gf.ArgumentPrecedenceOrder = apo;
        // Lambda-list / precedence changes alter dispatch ordering, so a warm
        // monomorphic cache would otherwise return stale results (ANSI
        // ENSURE-GENERIC-FUNCTION.8: re-ensuring with :argument-precedence-order).
        gf.InvalidateCache();

        // Store the full lambda-list as written so MOP readers return the actual
        // parameter names instead of gensym placeholders. Standard GFs
        // created via %make-gf otherwise have no lambda-list.
        if (args.Length > 8 && args[8] is not Nil)
            gf.StoredLambdaList = args[8];

        gf.LambdaListInfoSet = true;
        return args[0];
    }

    /// <summary>
    /// Set lambda list info on a method for congruence checking (CLHS 7.6.4).
    /// Args: method, required-count, optional-count, has-rest, has-key, has-allow-other-keys, keyword-names-list
    /// </summary>
    public static LispObject SetMethodLambdaListInfo(LispObject[] args)
    {
        if (args[0] is not LispMethod m)
            throw new LispErrorException(new LispTypeError("SET-METHOD-LAMBDA-LIST-INFO: not a method", args[0]));
        m.RequiredCount = args[1] is Fixnum f1 ? (int)f1.Value : 0;
        m.OptionalCount = args[2] is Fixnum f2 ? (int)f2.Value : 0;
        m.HasRest = args[3] is not Nil;
        m.HasKey = args[4] is not Nil;
        m.HasAllowOtherKeys = args.Length > 5 && args[5] is not Nil;
        // Parse keyword names from optional 7th argument (a list of keyword symbols)
        m.KeywordNames = new List<string>();
        if (args.Length > 6 && args[6] is not Nil)
        {
            var cur = args[6];
            while (cur is Cons c)
            {
                if (c.Car is Symbol sym)
                    m.KeywordNames.Add(sym.Name);
                cur = c.Cdr;
            }
        }
        // Optional 8th argument: the unspecialized lambda list DEFMETHOD was
        // written with. A FASL compiled before this argument existed leaves it
        // out, and METHOD-LAMBDA-LIST then rebuilds a list from the arity.
        if (args.Length > 7)
            m.StoredLambdaList = args[7];
        // Optional 9th argument: DEFMETHOD found that the body cannot reach its
        // next-method state (see LispMethod.IgnoresNextMethodState). Absent from a
        // FASL compiled before the analysis existed, which then keeps the full
        // protocol.
        m.IgnoresNextMethodState = args.Length > 8 && args[8] is not Nil;
        return args[0];
    }

    /// <summary>
    /// Check lambda list congruence between a method and its generic function (CLHS 7.6.4).
    /// Signals program-error if not congruent.
    /// </summary>
    private static void CheckLambdaListCongruence(GenericFunction gf, LispMethod method)
    {
        if (!gf.LambdaListInfoSet) return; // No info stored; skip check

        // Rule 1: Same number of required parameters
        if (gf.RequiredCount != method.RequiredCount)
            throw new LispErrorException(new LispProgramError(
                $"The method lambda list for {gf.Name.Name} has {method.RequiredCount} required " +
                $"parameter(s) but the generic function requires {gf.RequiredCount}"));

        // Rule 2: Optional parameter count must match exactly (CLHS 7.6.4),
        // unless the method has &rest or &key which subsumes optional parameters.
        if (method.OptionalCount != gf.OptionalCount && !(method.HasRest || method.HasKey))
            throw new LispErrorException(new LispProgramError(
                $"The method lambda list for {gf.Name.Name} has {method.OptionalCount} optional " +
                $"parameter(s) but the generic function has {gf.OptionalCount}"));

        // Rule 3: If ANY lambda list mentions &rest or &key, EACH must mention one or both
        // (bidirectional check per CLHS 7.6.4)
        if ((gf.HasRest || gf.HasKey) && !(method.HasRest || method.HasKey))
            throw new LispErrorException(new LispProgramError(
                $"The method lambda list for {gf.Name.Name} must accept &rest or &key " +
                $"arguments because the generic function does"));

        // Rule 3 reverse: if method has &key/&rest but GF doesn't, that's also a congruency error
        if ((method.HasRest || method.HasKey) && !(gf.HasRest || gf.HasKey))
            throw new LispErrorException(new LispProgramError(
                $"The method lambda list for {gf.Name.Name} accepts &rest or &key " +
                $"arguments but the generic function does not"));

        // Rule 4: If the GF lambda list mentions &key, each method must accept all
        // of the keyword names mentioned in the GF lambda list. A method satisfies this
        // if it has &allow-other-keys, or &rest (without &key), or explicitly lists
        // each GF keyword. Like SBCL, the rule is not applied when the GF lambda list
        // itself has &allow-other-keys: every call then skips keyword checking
        // (CLHS 7.6.5), so a method that ignores some GF keywords still works.
        if (gf.HasKey && gf.KeywordNames.Count > 0 && !gf.HasAllowOtherKeys)
        {
            // Method with &allow-other-keys accepts everything
            if (!method.HasAllowOtherKeys)
            {
                // Method with &rest but no &key accepts everything (CLHS 7.6.4 note)
                if (!(method.HasRest && !method.HasKey))
                {
                    foreach (var kw in gf.KeywordNames)
                    {
                        if (!method.KeywordNames.Contains(kw))
                            throw new LispErrorException(new LispProgramError(
                                $"The method lambda list for {gf.Name.Name} does not accept " +
                                $"the keyword argument :{kw} required by the generic function"));
                    }
                }
            }
        }
    }

    public static LispObject SetGFDeclarations(LispObject gfObj, LispObject declarations)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError(
                "SET-GF-DECLARATIONS: not a generic function", gfObj));
        gf.Declarations = declarations;
        return gfObj;
    }

    public static LispObject SetMethodCombination(LispObject gfObj, LispObject mcName)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION: not a generic function", gfObj));
        if (mcName is not Symbol mcSym)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION: not a symbol", mcName));
        // The standard combination is the absence of a built-in one.
        gf.MethodCombination = mcSym.Name == "STANDARD" ? null : mcSym;
        return gfObj;
    }

    public static LispObject SetMethodCombinationOrder(LispObject gfObj, LispObject order)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION-ORDER: not a generic function", gfObj));
        if (order is Symbol sym && sym.Name == "MOST-SPECIFIC-LAST")
            gf.MostSpecificFirst = false;
        else
            gf.MostSpecificFirst = true;
        return gfObj;
    }

    public static LispObject SetMethodCombinationArgs(LispObject gfObj, LispObject argsList)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION-ARGS: not a generic function", gfObj));
        var args = new List<LispObject>();
        var cur = argsList;
        while (cur is Cons c) { args.Add(c.Car); cur = c.Cdr; }
        gf.MethodCombinationArgs = args.ToArray();
        return gfObj;
    }

    public static LispObject FindGF(LispObject name)
    {
        Symbol sym;
        string? setfBaseName = null;
        if (name is Symbol s)
            sym = s;
        else if (name is Cons c && c.Car is Symbol setfSym && setfSym.Name == "SETF"
                 && c.Cdr is Cons c2 && c2.Car is Symbol accessor)
        {
            sym = Startup.Sym(SetfKeyFor(accessor));
            setfBaseName = accessor.Name; // save base name for fallback
        }
        else
            return Nil.Instance;
        // The generic function a name denotes is its current global function
        // definition (CLHS DEFMETHOD / ENSURE-GENERIC-FUNCTION). A GF installed
        // under a second name with (setf symbol-function) or (setf fdefinition)
        // is never in the registry under that name; without this check DEFMETHOD
        // on the alias made a new, unrelated generic function.
        if (name is Symbol fs && fs.Function is GenericFunction fgf)
            return fgf;
        if (setfBaseName != null && name is Cons fc && fc.Cdr is Cons fc2
            && fc2.Car is Symbol facc && facc.SetfFunction is GenericFunction sgf)
            return sgf;
        if (_gfRegistry.TryGetValue(sym, out var gf))
            return gf;
        // Fallback: name-based search for GFs registered under a different package.
        // Restricted to DOTCL-INTERNAL entries only: handles C#-startup GFs registered
        // under DOTCL-INTERNAL::NAME when Lisp code refers to the same name from another
        // package (e.g. CL-USER::SLOT-VALUE-USING-CLASS finding DOTCL-INTERNAL's GF).
        // The bare "(SETF NAME)" canonical key is ONLY used for DOTCL-INTERNAL accessors
        // (by SetfKeyFor); CL and other packages now get package-qualified keys, so this
        // fallback can never accidentally match user-created GFs like (setf cl:documentation).
        if (setfBaseName != null)
        {
            string canonicalKey = $"(SETF {setfBaseName})";
            foreach (var entry in _gfRegistry)
                if (entry.Key.Name == canonicalKey
                    && (entry.Key.HomePackage == null || entry.Key.HomePackage.Name == "DOTCL-INTERNAL"))
                    return entry.Value;
        }
        else
        {
            // Only DOTCL-INTERNAL: a GF whose name lost its package (the package
            // was deleted) is not the GF of a same-named symbol defined later in
            // another package; matching it made DEFCLASS take the dead GF for a
            // new reader, and the reader stayed undefined.
            string symName = sym.Name;
            foreach (var entry in _gfRegistry)
                if (entry.Key.Name == symName
                    && entry.Key.HomePackage?.Name == "DOTCL-INTERNAL")
                    return entry.Value;
        }
        return Nil.Instance;
    }

    public static LispObject MakeMethod(LispObject specializers, LispObject qualifiers, LispObject fn)
    {
        // specializers is a list of LispClass objects
        var specs = new List<LispObject>();
        var cur = specializers;
        while (cur is Cons c)
        {
            specs.Add(c.Car);
            cur = c.Cdr;
        }

        // qualifiers is a list of symbols
        // Any non-list atom is a qualifier (CLHS DEFMETHOD), T and numbers included.
        var quals = new List<LispObject>();
        cur = qualifiers;
        while (cur is Cons c2)
        {
            if (c2.Car is not Cons && c2.Car is not Nil)
                quals.Add(c2.Car);
            cur = c2.Cdr;
        }

        if (fn is not LispFunction func)
            throw new LispErrorException(new LispTypeError("MAKE-METHOD: function required", fn));

        return new LispMethod(specs.ToArray(), quals.ToArray(), func);
    }

    internal static GenericFunction? AddMethodGf;
    internal static LispMethod? AddMethodDefault;

    /// <summary>What DEFMETHOD calls to add its method. AMOP has DEFMETHOD go through the
    /// generic function ADD-METHOD, so a method of the program on it (specialised on the
    /// generic function's class) runs. Until there is one, and for a generic function it
    /// does not apply to, the method is added directly.</summary>
    public static LispObject AddMethodProtocol(LispObject gfObj, LispObject methodObj)
    {
        var amGf = AddMethodGf;
        if (amGf != null && amGf.Methods.Count > 1 && gfObj is GenericFunction gf
            && AddMethodOverridden(amGf, gf))
            return amGf.Invoke(new[] { gfObj, methodObj });
        return AddMethod(gfObj, methodObj);
    }

    private static bool AddMethodOverridden(GenericFunction amGf, GenericFunction gf)
    {
        var cls = ClassOf(gf) as LispClass;
        if (cls == null) return false;
        foreach (var m in amGf.Methods)
        {
            if (ReferenceEquals(m, AddMethodDefault) || m.Specializers.Length == 0) continue;
            var spec = m.Specializers[0];
            if (spec is LispClass sc)
            {
                if (!IsSystemClass(sc, "T") && Array.IndexOf(cls.ClassPrecedenceList, sc) >= 0) return true;
            }
            else if (spec is EqlSpecializer es && ReferenceEquals(es.Object, gf))
                return true;
        }
        return false;
    }

    public static LispObject AddMethod(LispObject gfObj, LispObject methodObj)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("ADD-METHOD: not a generic function", gfObj));
        if (methodObj is not LispMethod method)
            throw new LispErrorException(new LispTypeError("ADD-METHOD: not a method", methodObj));

        // CLHS: If the method object is a method object of another generic function, signal error.
        // Snapshot Owner once: a concurrent REMOVE-METHOD can null it between reads, and a
        // non-atomic `Owner != null && Owner != gf ... Owner.Name` would then deref null (NRE).
        var owner = method.Owner;
        if (owner != null && owner != gf)
            throw new LispErrorException(new LispError(
                $"ADD-METHOD: method already belongs to generic function {owner.Name.Name}"));

        // Check lambda list congruence (CLHS 7.6.4)
        CheckLambdaListCongruence(gf, method);

        // Raised before the method becomes visible and the method epoch moves,
        // so no class can cache "no protocol" against the new epoch.
        NoteSlotProtocolMethod(gf, method);
        NotePrintObjectMethod(gf, method);

        // Copy-on-write under MethodsLock so a concurrent dispatch always sees a
        // consistent method array. Replace an existing method with matching
        // specializers+qualifiers, else append.
        lock (gf.MethodsLock)
        {
            var cur = gf.Methods;  // snapshot (current array)
            bool done = false;
            for (int i = 0; i < cur.Count; i++)
            {
                if (MethodSignatureMatches(cur[i], method))
                {
                    cur[i].Owner = null; // Clear old method's owner
                    var replaced = cur.ToArray();
                    replaced[i] = method;
                    method.Owner = gf;
                    gf.ReplaceMethods(replaced);
                    done = true;
                    break;
                }
            }
            if (!done)
            {
                var appended = new LispMethod[cur.Count + 1];
                for (int i = 0; i < cur.Count; i++) appended[i] = cur[i];
                appended[cur.Count] = method;
                method.Owner = gf;
                gf.ReplaceMethods(appended);
            }
            gf.InvalidateCache();
            gf.RecomputeAccessorFlags();
        }
        // A new INITIALIZE-INSTANCE / SHARED-INITIALIZE method invalidates the
        // per-class make-instance fast-path caches (else already-instantiated
        // classes silently skip it).
        InvalidateSimpleInitCaches(gf);
        InvalidateSlotAccessShortcuts(gf);
        NotifyDirectMethod(method, adding: true);
        NotifyDiscriminatingFunction(gf);
        NotifyDependents(gf, Startup.Sym("ADD-METHOD"), method);
        NoteInvocationProtocolMethodChange(gf);
        return gf;
    }

    // Called after ADD-METHOD / REMOVE-METHOD on INITIALIZE-INSTANCE or
    // SHARED-INITIALIZE. Those per-class caches decide whether make-instance can
    // take its custom-method-free fast path; a method added after instances were
    // already made must invalidate them, or the new method is silently skipped.
    private static void InvalidateSimpleInitCaches(GenericFunction gf)
    {
        if (gf.Name.Name == "INITIALIZE-INSTANCE" || gf.Name.Name == "SHARED-INITIALIZE")
        {
            foreach (var c in _classRegistry.Values)
            {
                c.SimpleInitChecked = false;
                c.SharedInitSimpleChecked = false;
                c.CachedValidInitargKeys = null; // method keys affect valid initargs (CLHS 7.1.2)
                c.CachedHasCustomInitMethods = null; // the fast-path gate itself
            }
        }
    }

    // Called after ADD-METHOD / REMOVE-METHOD. An accessor generic function's
    // dispatch cache can hold a reader or writer shortcut that reads the slot
    // vector directly, chosen because no slot-access method applied to the class
    // when the entry was made (UsesSlotProtocol). Those entries are not keyed on
    // the method epoch, so a method added to (or removed from) one of the
    // *-USING-CLASS generic functions has to clear them in every generic function.
    private static void InvalidateSlotAccessShortcuts(GenericFunction gf)
    {
        if (SlotProtocolGFs.Contains(gf)) InvalidateAllDispatchCaches();
    }

    /// <summary>After a DEFCLASS redefinition has made its accessors: remove the accessor
    /// methods of the previous definition that the new one did not make again (CLHS
    /// 4.3.6: a reader or writer the new definition leaves out is removed).</summary>
    public static LispObject PruneStaleAccessors(LispObject clsObj)
    {
        if (clsObj is LispClass cls && cls.StaleAccessorMethods is { } stale)
        {
            cls.StaleAccessorMethods = null;
            foreach (var (gf, m) in stale)
                RemoveMethod(gf, m);
        }
        return Nil.Instance;
    }

    public static LispObject RemoveMethod(LispObject gfObj, LispObject methodObj)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("REMOVE-METHOD: not a generic function", gfObj));
        if (methodObj is not LispMethod method)
            throw new LispErrorException(new LispTypeError("REMOVE-METHOD: not a method", methodObj));
        // Copy-on-write removal under MethodsLock.
        lock (gf.MethodsLock)
        {
            var cur = gf.Methods;
            int idx = -1;
            for (int i = 0; i < cur.Count; i++)
                if (ReferenceEquals(cur[i], method)) { idx = i; break; }
            if (idx >= 0)
            {
                var arr = new LispMethod[cur.Count - 1];
                for (int i = 0, j = 0; i < cur.Count; i++)
                    if (i != idx) arr[j++] = cur[i];
                method.Owner = null;
                gf.ReplaceMethods(arr);
                ForgetMethodSource(gf, method);
                gf.InvalidateCache();
                gf.RecomputeAccessorFlags();
            }
        }
        // Removing an INITIALIZE-INSTANCE / SHARED-INITIALIZE method also changes
        // the make-instance fast-path eligibility: invalidate the per-class caches.
        InvalidateSimpleInitCaches(gf);
        InvalidateSlotAccessShortcuts(gf);
        NotifyDirectMethod(method, adding: false);
        NotifyDiscriminatingFunction(gf);
        NotifyDependents(gf, Startup.Sym("REMOVE-METHOD"), method);
        NoteInvocationProtocolMethodChange(gf);
        return gf;
    }

    public static LispObject ComputeApplicableMethods(LispObject gfObj, LispObject argList)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("COMPUTE-APPLICABLE-METHODS: not a generic function", gfObj));

        // Convert arg list to array
        var args = new List<LispObject>();
        var cur = argList;
        while (cur is Cons c)
        {
            args.Add(c.Car);
            cur = c.Cdr;
        }

        // Find applicable methods (same logic as DispatchGF)
        var applicable = new List<LispMethod>();
        var argsArray = args.ToArray();
        foreach (var method in gf.Methods)
        {
            if (IsMethodApplicable(method, argsArray))
                applicable.Add(method);
        }

        // Sort by specificity
        applicable.Sort((a, b) => CompareMethodSpecificity(a, b, argsArray));

        // Build result list
        LispObject result = Nil.Instance;
        for (int i = applicable.Count - 1; i >= 0; i--)
            result = new Cons(applicable[i], result);
        return result;
    }

    /// <summary>AMOP COMPUTE-EFFECTIVE-METHOD for the STANDARD method combination.
    /// METHODS are the applicable methods in most-specific-first order. Returns the
    /// canonical effective-method form built from CALL-METHOD / MAKE-METHOD with the
    /// actual LispMethod objects embedded (CLHS 7.6.6.2 standard combination).</summary>
    public static LispObject ComputeEffectiveMethodStandard(LispObject methodList)
    {
        var around = new List<LispObject>();
        var before = new List<LispObject>();
        var after = new List<LispObject>();
        var primary = new List<LispObject>();
        for (var cur = methodList; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is not LispMethod m) continue;
            if (m.Qualifiers.Length == 0) { primary.Add(m); continue; }
            if (m.Qualifiers.Length == 1)
            {
                switch (LispMethod.QualifierName(m.Qualifiers[0]))
                {
                    case "AROUND": around.Add(m); continue;
                    case "BEFORE": before.Add(m); continue;
                    case "AFTER": after.Add(m); continue;
                }
            }
            throw new LispErrorException(new LispError(
                "COMPUTE-EFFECTIVE-METHOD: method with invalid qualifiers for standard combination"));
        }
        if (primary.Count == 0)
            throw new LispErrorException(new LispError(
                "COMPUTE-EFFECTIVE-METHOD: no applicable primary method"));

        var callMethodSym = Startup.Sym("CALL-METHOD");
        var makeMethodSym = Startup.Sym("MAKE-METHOD");
        var prognSym = Startup.Sym("PROGN");
        var mvp1Sym = Startup.Sym("MULTIPLE-VALUE-PROG1");

        // (call-method M (next...))
        LispObject CallMethod(LispObject m, IEnumerable<LispObject> nexts) =>
            Runtime.List(callMethodSym, m, Runtime.List(nexts.ToArray()));

        // primary: most-specific primary called with the rest as its next-methods.
        var primaryCall = CallMethod(primary[0], primary.Skip(1));

        // Wrap with before (for effect, before) and after (for effect, least-specific-first).
        LispObject core = primaryCall;
        if (after.Count > 0)
        {
            var parts = new List<LispObject> { mvp1Sym, primaryCall };
            for (int i = after.Count - 1; i >= 0; i--)            // least-specific-first
                parts.Add(CallMethod(after[i], Array.Empty<LispObject>()));
            core = Runtime.List(parts.ToArray());
        }
        if (before.Count > 0)
        {
            var parts = new List<LispObject> { prognSym };
            foreach (var b in before)                            // most-specific-first
                parts.Add(CallMethod(b, Array.Empty<LispObject>()));
            parts.Add(core);
            core = Runtime.List(parts.ToArray());
        }

        if (around.Count == 0) return core;

        // (call-method around0 (around1 ... aroundN (make-method core)))
        var nexts = new List<LispObject>();
        for (int i = 1; i < around.Count; i++) nexts.Add(around[i]);
        nexts.Add(Runtime.List(makeMethodSym, core));
        return CallMethod(around[0], nexts);
    }

    /// <summary>AMOP COMPUTE-APPLICABLE-METHODS-USING-CLASSES: like
    /// COMPUTE-APPLICABLE-METHODS but the arguments are given as classes, so it
    /// returns (values methods definitive-p). definitive-p is NIL when an applicable
    /// method has an EQL specializer, since EQL applicability can't be decided from a
    /// class alone: the caller must fall back to COMPUTE-APPLICABLE-METHODS.</summary>
    public static LispObject ComputeApplicableMethodsUsingClasses(LispObject gfObj, LispObject classList)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError(
                "COMPUTE-APPLICABLE-METHODS-USING-CLASSES: not a generic function", gfObj));
        var classes = new List<LispClass?>();
        for (var cur = classList; cur is Cons c; cur = c.Cdr)
            classes.Add(c.Car as LispClass);

        bool definitive = true;
        var applicable = new List<LispMethod>();
        foreach (var m in gf.Methods)
        {
            bool ok = true;
            for (int i = 0; i < m.Specializers.Length; i++)
            {
                var spec = m.Specializers[i];
                var argCls = i < classes.Count ? classes[i] : null;
                if (spec is LispClass specCls)
                {
                    if (specCls.Name.Name == "T") continue;
                    if (argCls == null || (Array.IndexOf(argCls.ClassPrecedenceList, specCls) < 0
                                           && !DotNetVarianceApplicable(specCls, argCls)))
                    { ok = false; break; }
                }
                else if (EqlSpecializerValue(spec) is { } eqlObj)
                {
                    // An (eql OBJ) method can only apply when an argument IS obj, which
                    // is possible only if obj is an instance of argCls. If so the result
                    // is non-definitive (a specific arg of this class might or might not
                    // be obj); otherwise the method simply never applies for this class.
                    var objCls = ClassOf(eqlObj) as LispClass;
                    if (argCls != null && objCls != null
                        && Array.IndexOf(objCls.ClassPrecedenceList, argCls) >= 0)
                        definitive = false;
                    else
                    { ok = false; break; }
                }
            }
            if (ok) applicable.Add(m);
        }
        applicable.Sort((a, b) => CompareMethodSpecificityByClasses(a, b, classes));
        LispObject result = Nil.Instance;
        for (int i = applicable.Count - 1; i >= 0; i--)
            result = new Cons(applicable[i], result);
        return MultipleValues.Values2(result, definitive ? T.Instance : Nil.Instance);
    }

    /// <summary>CompareMethodSpecificity variant driven by an explicit class list
    /// (for COMPUTE-APPLICABLE-METHODS-USING-CLASSES) instead of live argument values.</summary>
    private static int CompareMethodSpecificityByClasses(LispMethod a, LispMethod b, List<LispClass?> classes)
    {
        int n = Math.Min(a.Specializers.Length, b.Specializers.Length);
        int[]? apo = a.Owner?.ArgumentPrecedenceOrder;
        for (int k = 0; k < n; k++)
        {
            int i = (apo != null && k < apo.Length && apo[k] < n) ? apo[k] : k;
            if (ReferenceEquals(a.Specializers[i], b.Specializers[i])) continue;
            bool aIsEql = EqlSpecializerValue(a.Specializers[i]) != null;
            bool bIsEql = EqlSpecializerValue(b.Specializers[i]) != null;
            if (aIsEql && !bIsEql) return -1;
            if (!aIsEql && bIsEql) return 1;
            if (aIsEql && bIsEql) continue;
            if (a.Specializers[i] is LispClass clsA && b.Specializers[i] is LispClass clsB)
            {
                var argClass = i < classes.Count ? classes[i] : null;
                if (argClass != null)
                {
                    int ra = SpecializerRank(clsA, argClass);
                    int rb = SpecializerRank(clsB, argClass);
                    if (ra != rb) return ra < rb ? -1 : 1;
                    if (ra != int.MaxValue)
                    {
                        int v = CompareByDotNetAssignability(clsA, clsB);
                        if (v != 0) return v;
                    }
                }
                return string.Compare(clsA.Name.Name, clsB.Name.Name, StringComparison.Ordinal);
            }
        }
        return 0;
    }

    /// <summary>DEFCLASS hook: tag the unqualified accessor method of GF that is
    /// specialized on CLS with the effective slot-definition named SLOTNAME, so
    /// DOTCL-MOP:ACCESSOR-METHOD-SLOT-DEFINITION can recover it. No-op if the pieces
    /// can't be resolved (keeps DEFCLASS robust).</summary>
    public static LispObject RegisterAccessorMethod(LispObject gfObj, LispObject clsObj, LispObject slotNameObj)
    {
        if (gfObj is GenericFunction gf && clsObj is LispClass cls && slotNameObj is Symbol slotName)
        {
            SlotDefinition? slotd = null;
            slotd = cls.FindEffectiveSlot(slotName);
            // Where the class sits in the method's specializers says which way the
            // accessor goes: a reader takes the object first, a writer takes the value
            // first. The generic function's name does not -- a :writer slot option
            // names one like any other function, only an :accessor writer is (SETF x).
            int classPosition = -1;
            if (slotd != null)
                foreach (var m in gf.Methods)
                {
                    int at = Array.IndexOf(m.Specializers, cls);
                    if (m.Qualifiers.Length == 0 && at >= 0)
                    { m.AccessorSlot = slotd; classPosition = at; cls.AccessorMethods.Add((gf, m)); break; }
                }
            // The AccessorSlot tag is what RecomputeAccessorFlags keys on; the method
            // was already added (with AccessorSlot null then), so recompute now.
            gf.RecomputeAccessorFlags();
            // The method is an accessor method: STANDARD-READER-METHOD when it reads,
            // STANDARD-WRITER-METHOD when it writes (AMOP; SBCL does the same). Only
            // the class it reports changes; a method that already has a class of its
            // own (a user method class) keeps it. STANDARD-METHOD itself is not one:
            // DEFMETHOD records it once any GENERIC-FUNCTION-METHOD-CLASS method exists.
            if (slotd != null && classPosition >= 0)
            {
                foreach (var m in gf.Methods)
                    if (ReferenceEquals(m.AccessorSlot, slotd)
                        && (m.MetaClass == null || IsSystemClass(m.MetaClass, "STANDARD-METHOD"))
                        && FindClassOrNil(Startup.Sym(
                               classPosition == 0 ? "STANDARD-READER-METHOD" : "STANDARD-WRITER-METHOD"))
                           is LispClass amc)
                    { m.MetaClass = amc; break; }
            }
            // AMOP has class initialization ask the metaclass what class an accessor
            // method should be. dotcl picks the class itself (above), so the answer is
            // not used to build anything -- what the protocol buys here is the call.
            // A writer generic function is named (SETF name).
            if (slotd != null && classPosition >= 0
                && AccessorMethodClassHook?.Invoke(cls, slotd, classPosition == 0) is { } chosen)
            {
                // The class the protocol named is the class of the accessor method.
                foreach (var m in gf.Methods)
                    if (ReferenceEquals(m.AccessorSlot, slotd)) { m.MetaClass = chosen; break; }
            }
        }
        return Nil.Instance;
    }

    /// <summary>The interned EQL specializers, keyed by the specialized object with
    /// EQL as the test -- which is what INTERN-EQL-SPECIALIZER means by "the same".
    /// Built on first use so nothing about it runs during the bootstrap.</summary>
    private static LispHashTable? _eqlSpecializers;
    private static readonly object _eqlSpecializerLock = new();

    /// <summary>AMOP INTERN-EQL-SPECIALIZER: the specializer metaobject for OBJ, the
    /// same one every time two objects are EQL, so EQ answers the question callers
    /// actually ask.</summary>
    public static LispObject InternEqlSpecializer(LispObject obj)
    {
        lock (_eqlSpecializerLock)
        {
            _eqlSpecializers ??= new LispHashTable("EQL");
            if (_eqlSpecializers.TryGet(obj, out var found) && found is EqlSpecializer)
                return found;
            var made = new EqlSpecializer(obj);
            _eqlSpecializers.Set(obj, made);
            return made;
        }
    }

    /// <summary>The object an EQL specializer specializes on, or null when the
    /// specializer is a class rather than an EQL one. Both representations are read
    /// here: the EQL-SPECIALIZER metaobject INTERN-EQL-SPECIALIZER hands out, and the
    /// list (EQL object) that a caller writing a specializer by hand produces.</summary>
    internal static LispObject? EqlSpecializerValue(LispObject spec)
        => spec is EqlSpecializer es ? es.Object
           : spec is Cons c && c.Car is Symbol s && s.Name == "EQL" && c.Cdr is Cons v
             ? v.Car : null;

    private static bool MethodSignatureMatches(LispMethod a, LispMethod b)
    {
        if (a.Specializers.Length != b.Specializers.Length) return false;
        if (a.Qualifiers.Length != b.Qualifiers.Length) return false;
        for (int i = 0; i < a.Specializers.Length; i++)
        {
            var sa = a.Specializers[i];
            var sb = b.Specializers[i];
            if (ReferenceEquals(sa, sb)) continue;
            // Classes are interned, so identity settles them; an EQL specializer is a
            // fresh (EQL value) cons on every DEFMETHOD, so two definitions of the same
            // method are never the same object. Comparing those by identity meant a
            // redefinition was appended as a SECOND method instead of replacing the
            // first, and the original kept winning. CLHS 7.6.2: EQL specializers agree
            // when their values are EQL.
            var va = EqlSpecializerValue(sa);
            var vb = EqlSpecializerValue(sb);
            if (va != null && vb != null && IsTrueEql(va, vb)) continue;
            return false;
        }
        for (int i = 0; i < a.Qualifiers.Length; i++)
            if (!LispMethod.QualifierSame(a.Qualifiers[i], b.Qualifiers[i])) return false;
        return true;
    }

    public static LispObject GetGFMethods(LispObject gfObj)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("GF-METHODS: not a generic function", gfObj));
        LispObject result = Nil.Instance;
        for (int i = gf.Methods.Count - 1; i >= 0; i--)
            result = new Cons(gf.Methods[i], result);
        return result;
    }

    public static LispObject MethodSpecializers(LispObject methodObj)
    {
        if (methodObj is not LispMethod m)
            throw new LispErrorException(new LispTypeError("METHOD-SPECIALIZERS: not a method", methodObj));
        LispObject result = Nil.Instance;
        for (int i = m.Specializers.Length - 1; i >= 0; i--)
            result = new Cons(m.Specializers[i], result);
        return result;
    }

    public static LispObject MethodQualifiers(LispObject methodObj)
    {
        if (methodObj is not LispMethod m)
            throw new LispErrorException(new LispTypeError("METHOD-QUALIFIERS: not a method", methodObj));
        LispObject result = Nil.Instance;
        for (int i = m.Qualifiers.Length - 1; i >= 0; i--)
            result = new Cons(m.Qualifiers[i], result);
        return result;
    }

    /// <summary>The next methods of the call in progress, as AMOP hands them to a
    /// method function. Empty outside a dispatch.</summary>
    internal static LispObject CurrentNextMethods()
    {
        var chain = _nextMethodChain;
        if (chain == null) return Nil.Instance;
        LispObject result = Nil.Instance;
        for (int i = chain.Count - 1; i >= _nextMethodIndex; i--)
            if (i >= 0 && i < chain.Count) result = new Cons(chain[i], result);
        return result;
    }

    public static LispObject MethodFunction(LispObject methodObj)
    {
        if (methodObj is not LispMethod m)
            throw new LispErrorException(new LispTypeError("METHOD-FUNCTION: not a method", methodObj));
        // AMOP: a method function takes the arguments as a list and the next methods
        // as a list. dotcl's own take them spread, which is what dispatch calls, so
        // what is handed out is a view over that -- built once and kept, so
        // METHOD-FUNCTION answers the same object every time.
        if (m.ProcessedParameterFunction is { } amop) return amop;
        var raw = m.Function;
        var view = new LispFunction(args =>
        {
            var callArgs = new List<LispObject>();
            if (args.Length > 0)
                for (var c = args[0]; c is Cons cc; c = cc.Cdr) callArgs.Add(cc.Car);
            return raw.Invoke(callArgs.ToArray());
        }, "METHOD-FUNCTION view", -1);
        m.ProcessedParameterFunction = view;
        return view;
    }

    /// <summary>
    /// Dispatch through a GF's methods if any are applicable, otherwise call the default function.
    /// Used for C#-created GFs that have a default behavior but also support defmethod.
    /// </summary>
    /// <summary>A generic function was called and no method is applicable: call
    /// NO-APPLICABLE-METHOD with the generic function and the arguments (CLHS 7.6.6),
    /// so a method a program defines on it decides what happens. Its default method
    /// signals NO-APPLICABLE-METHOD-ERROR.</summary>
    internal static LispObject CallNoApplicableMethod(GenericFunction gf, LispObject[] args)
    {
        if (Startup.Sym("NO-APPLICABLE-METHOD").Function is LispFunction nam
            && !ReferenceEquals(nam, gf))
        {
            var namArgs = new LispObject[args.Length + 1];
            namArgs[0] = gf;
            Array.Copy(args, 0, namArgs, 1, args.Length);
            return nam.Invoke(namArgs);
        }
        throw NoApplicableMethodError(gf, args);
    }

    /// <summary>The error NO-APPLICABLE-METHOD's default method signals. Falls back to
    /// a plain ERROR before the condition class is defined (early in startup).</summary>
    internal static LispErrorException NoApplicableMethodError(LispObject gf, LispObject[] args)
    {
        var gfName = gf is GenericFunction g ? g.Name.ToString() : gf.ToString();
        var message = $"No applicable method for generic function {gfName}";
        var type = Startup.Sym("NO-APPLICABLE-METHOD-ERROR");
        if (FindClassOrNil(type) is LispClass
            && MakeConditionFromType(type, new LispObject[] {
                   Startup.Keyword("FORMAT-CONTROL"),
                   new LispString("No applicable method for generic function ~S when called with arguments ~S."),
                   Startup.Keyword("FORMAT-ARGUMENTS"),
                   new Cons(gf, new Cons(List(args), Nil.Instance)) }) is LispCondition cond)
            return new LispErrorException(cond);
        return new LispErrorException(new LispError(message));
    }

    public static LispObject DispatchGFOrDefault(
        GenericFunction gf, LispObject[] args, Func<LispObject[], LispObject> defaultFn)
    {
        if (gf.Methods.Count > 0)
        {
            foreach (var method in gf.Methods)
            {
                if (IsMethodApplicable(method, args))
                    return DispatchGF(gf, args);
            }
        }
        return defaultFn(args);
    }

    /// <summary>
    /// Standard method combination dispatch.
    /// Called when a GF is invoked.
    /// </summary>
    /// <summary>Get the CLOS class for a dispatch argument (for cache keying).</summary>
    // Memoized dispatch classes for the hottest builtin argument types. ClassOf
    // on a builtin resolves Startup.Sym("NAME") + a registry hash lookup on
    // EVERY call, and ArgDispatchClass runs for every argument of every GF
    // dispatch (cache compare + applicability checks): e.g. (gf instance i)
    // with a fixnum i paid the FIXNUM lookup per call. Builtin classes are
    // registered once at startup and CL forbids redefining them, so a one-shot
    // memo is safe. Types with special ClassOf handling (conditions, .NET
    // objects, class/GF/method metaobjects, vectors) fall through unchanged.
    private static LispClass? _dcFixnum, _dcBignum, _dcNull, _dcSymbol, _dcCons,
        _dcString, _dcCharacter, _dcDoubleFloat;

    private static LispClass? ArgDispatchClass(LispObject obj) => obj switch
    {
        LispInstance inst => inst.Class,
        _ when LispStruct.TryAs(obj, out var st) => StructDispatchClass(st),
        Fixnum => _dcFixnum ??= FindClassOrNil(Startup.Sym("FIXNUM")) as LispClass,
        Bignum => _dcBignum ??= FindClassOrNil(Startup.Sym("BIGNUM")) as LispClass,
        Nil => _dcNull ??= FindClassOrNil(Startup.Sym("NULL")) as LispClass,
        Symbol or T => _dcSymbol ??= FindClassOrNil(Startup.Sym("SYMBOL")) as LispClass,
        Cons => _dcCons ??= FindClassOrNil(Startup.Sym("CONS")) as LispClass,
        LispString => _dcString ??= FindClassOrNil(Startup.Sym("STRING")) as LispClass,
        LispChar => _dcCharacter ??= FindClassOrNil(Startup.Sym("CHARACTER")) as LispClass,
        DoubleFloat => _dcDoubleFloat ??= FindClassOrNil(Startup.Sym("DOUBLE-FLOAT")) as LispClass,
        LispVector v => VectorDispatchClass(v),
        _ => ClassOf(obj) as LispClass
    };

    // Indexed by the kinds CLASSOF tells arrays apart by.
    private static readonly LispClass?[] _dcArray = new LispClass?[7];

    private static LispClass? VectorDispatchClass(LispVector v)
    {
        int k;
        if (v.Rank != 1) k = 0;
        else
        {
            var et = v.ElementTypeName;
            if (et.Length == 1) k = 6;
            else if (et == "BIT") k = v.IsSimple ? 1 : 2;
            else if (et is "BASE-CHAR" or "STANDARD-CHAR") k = v.IsSimple ? 3 : 4;
            else if (et is "CHARACTER" or "NIL") k = 5;
            else k = 6;
        }
        return _dcArray[k] ??= ClassOf(v) as LispClass;
    }

    /// <summary>The class of a structure instance: FIND-CLASS of its type name, read
    /// from the name's memo when that is current. Structure arguments are common in
    /// dispatch (every DEFSTRUCT type hierarchy that methods specialize on), and this
    /// is the memo check FINDCLASSORNIL makes, without its call and type test.</summary>
    private static LispClass? StructDispatchClass(LispStruct st)
    {
        var memo = st.TypeName.ClassMemo;
        if (memo != null && memo.Epoch == System.Threading.Volatile.Read(ref ClassRegistry.Epoch))
            return memo.AsClass;
        return FindClassOrNil(st.TypeName) as LispClass;
    }

    /// <summary>Validate keyword arguments against the union of keywords accepted by
    /// the applicable methods plus GF-level keywords (CLHS 7.6.5). Signals program-error
    /// for an unknown keyword unless :allow-other-keys t is passed or some applicable
    /// method allows other keys. Must run on BOTH the cache-hit and cache-miss dispatch
    /// paths: earlier it lived only on the cache-miss path, so a warm monomorphic cache
    /// silently skipped the check (ANSI DEFMETHOD.ERROR.14/15).</summary>
    /// <summary>The keyword-portion checks that do not depend on which methods apply:
    /// an even number of pairs, symbol keys, and the :ALLOW-OTHER-KEYS escape. False
    /// when the unknown-keyword check is to be skipped.</summary>
    private static bool KeywordPortionNeedsCheck(GenericFunction gf, LispObject[] args, int keyStart)
    {
        // CLHS 3.5.1.6: the keyword portion must be an even number of pairs whose
        // keys are symbols. (sym 1 2) and (sym 1 :y) [no value] are program-errors.
        int keyLen = args.Length - keyStart;
        if ((keyLen & 1) != 0)
            throw new LispErrorException(new LispProgramError(
                $"{gf.Name.Name}: odd number of keyword arguments"));
        for (int i = keyStart; i < args.Length; i += 2)
        {
            if (args[i] is not Symbol)
                throw new LispErrorException(new LispProgramError(
                    $"{gf.Name.Name}: keyword argument key is not a symbol: {args[i]}"));
        }

        // :allow-other-keys: only the FIRST occurrence's value is honored (CLHS 3.4.1.4.1).
        for (int i = keyStart; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS"
                && ks.HomePackage?.Name == "KEYWORD")
            {
                if (args[i + 1] is not Nil) return false; // suppress unknown-key check
                break;                                    // first wins; nil -> validate
            }
        }
        return true;
    }

    private static void ValidateGenericKeywords(GenericFunction gf, IReadOnlyList<LispMethod> applicable, LispObject[] args)
    {
        if (!(gf.LambdaListInfoSet && gf.HasKey && !gf.HasAllowOtherKeys)) return;
        int keyStart = gf.RequiredCount + gf.OptionalCount;
        if (args.Length <= keyStart) return;
        if (!KeywordPortionNeedsCheck(gf, args, keyStart)) return;

        // Check if any applicable method has &allow-other-keys or &rest (without &key)
        for (int mi = 0; mi < applicable.Count; mi++)
        {
            var m = applicable[mi];
            if (m.HasAllowOtherKeys || (m.HasRest && !m.HasKey)) return;
        }

        // The accepted keywords used to be collected into a fresh HashSet here, on
        // every call that passed a keyword: the set, the two arrays it grows, and an
        // enumerator over the applicable list (an interface, so foreach boxes one) --
        // all to test membership against the handful of names a method declares. Scan
        // the lists instead. The counts are tiny (a few keywords, a few applicable
        // methods) and nothing is allocated.
        for (int i = keyStart; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks2 && !AcceptsKeyword(gf, applicable, ks2.Name))
                throw new LispErrorException(new LispProgramError(
                    $"{gf.Name.Name}: invalid keyword argument :{ks2.Name}"));
        }
    }

    /// <summary>Keyword validation against a warm cache entry, reading its method
    /// lists where they lie. They used to be concatenated into one fresh List whose
    /// only purpose was this check, rebuilt on every call that passed a keyword.</summary>
    private static void ValidateGenericKeywords(GenericFunction gf, CachedDispatch cached, LispObject[] args)
    {
        int keyStart = gf.RequiredCount + gf.OptionalCount;
        if (args.Length <= keyStart) return;
        if (!KeywordPortionNeedsCheck(gf, args, keyStart)) return;

        if (cached.Applicable is { } appl)
        {
            if (ListAcceptsAnyKeyword(appl)) return;
        }
        else if (ListAcceptsAnyKeyword(cached.Around) || ListAcceptsAnyKeyword(cached.Before)
                 || ListAcceptsAnyKeyword(cached.Primary) || ListAcceptsAnyKeyword(cached.After)
                 || EqlMethodsAcceptAnyKeyword(cached, args))
            return;

        for (int i = keyStart; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && !EntryAcceptsKeyword(gf, cached, args, ks.Name))
                throw new LispErrorException(new LispProgramError(
                    $"{gf.Name.Name}: invalid keyword argument :{ks.Name}"));
        }
    }

    private static bool ListAcceptsAnyKeyword(List<LispMethod> methods)
    {
        for (int i = 0; i < methods.Count; i++)
        {
            var m = methods[i];
            if (m.HasAllowOtherKeys || (m.HasRest && !m.HasKey)) return true;
        }
        return false;
    }

    private static bool EqlMethodsAcceptAnyKeyword(CachedDispatch cached, LispObject[] args)
    {
        if (cached.EqlMethods == null) return false;
        foreach (var m in cached.EqlMethods)
            if (IsMethodApplicable(m, args)
                && (m.HasAllowOtherKeys || (m.HasRest && !m.HasKey))) return true;
        return false;
    }

    private static bool ListHasKeyword(List<LispMethod> methods, string name)
    {
        for (int i = 0; i < methods.Count; i++)
        {
            var names = methods[i].KeywordNames;
            for (int j = 0; j < names.Count; j++)
                if (names[j] == name) return true;
        }
        return false;
    }

    private static bool EntryAcceptsKeyword(GenericFunction gf, CachedDispatch cached,
                                            LispObject[] args, string name)
    {
        if (name == "ALLOW-OTHER-KEYS") return true;
        if (cached.Applicable is { } appl)
        {
            if (ListHasKeyword(appl, name)) return true;
        }
        else
        {
            if (ListHasKeyword(cached.Around, name) || ListHasKeyword(cached.Before, name)
                || ListHasKeyword(cached.Primary, name) || ListHasKeyword(cached.After, name))
                return true;
            if (cached.EqlMethods != null)
                foreach (var m in cached.EqlMethods)
                {
                    if (!IsMethodApplicable(m, args)) continue;
                    var names = m.KeywordNames;
                    for (int j = 0; j < names.Count; j++)
                        if (names[j] == name) return true;
                }
        }
        var gfNames = gf.KeywordNames;
        for (int i = 0; i < gfNames.Count; i++)
            if (gfNames[i] == name) return true;
        return false;
    }

    /// <summary>Whether NAME is a keyword some applicable method or the generic
    /// function itself declares. :ALLOW-OTHER-KEYS is always accepted (CLHS
    /// 3.4.1.4.1).</summary>
    private static bool AcceptsKeyword(GenericFunction gf,
                                       IReadOnlyList<LispMethod> applicable, string name)
    {
        if (name == "ALLOW-OTHER-KEYS") return true;
        for (int mi = 0; mi < applicable.Count; mi++)
        {
            var names = applicable[mi].KeywordNames;
            for (int i = 0; i < names.Count; i++)
                if (names[i] == name) return true;
        }
        var gfNames = gf.KeywordNames;
        for (int i = 0; i < gfNames.Count; i++)
            if (gfNames[i] == name) return true;
        return false;
    }

    /// <summary>add ENTRY to GF's N-way dispatch cache. Rebuilds an immutable
    /// array with ENTRY at the front (most-recent), dropping any existing entry that
    /// has the same argument classes (ENTRY replaces it) and capping at
    /// DispatchCacheWidth, then publishes it with one volatile write: a concurrent
    /// reader sees the whole old or whole new array, never a partial one. Racing fills
    /// may lose an entry (a future miss re-fills it); a race with InvalidateCache can
    /// leave a briefly-stale entry, exactly as the previous single-entry cache did.</summary>
    private static void AddDispatchCache(GenericFunction gf, CachedDispatch entry)
    {
        // Decide the arity-1 fast shape once, here, where the entry is complete:
        // DISPATCHGF1 then reads one bool instead of re-deriving it per call.
        entry.ComputePlainPrimaryChain();
        var old = gf.DispatchCache;
        var list = new List<CachedDispatch>(GenericFunction.DispatchCacheWidth) { entry };
        if (old != null)
        {
            foreach (var e in old)
            {
                if (list.Count >= GenericFunction.DispatchCacheWidth) break;
                if (SameArgTypes(e.ArgTypes, entry.ArgTypes)) continue; // replaced by ENTRY
                list.Add(e);
            }
        }
        gf.DispatchCache = list.ToArray();
        if ((entry.PlainPrimaryChain && entry.Primary[0].IgnoresNextMethodState && entry.ArgTypes.Length <= 2)
            || (entry.HasEqlSpecializers && EqlLooseEligible(entry)))
            gf.LooseCache ??= new GenericFunction.LooseSlot?[8];

        if (entry.ArgTypes.Length >= 1)
        {
            int epoch = GenericFunction.MethodEpoch;
            int width = entry.ArgTypes.Length;
            var table = gf.DispatchByClasses;
            if (table == null || table.Epoch != epoch || table.Width != width)
                gf.DispatchByClasses = table = new GenericFunction.DispatchTable(epoch, width);
            table.Add(entry.ArgTypes, entry);
        }
    }

    private static readonly Func<LispObject, LispClass?> s_argDispatchClass = ArgDispatchClass;

    /// <summary>The second-level entry for a call whose front-cache scan missed, or
    /// null.</summary>
    private static CachedDispatch? DispatchTableHit(GenericFunction gf, LispObject[] args)
    {
        var table = gf.DispatchByClasses;
        if (table == null || args.Length < table.Width || table.Epoch != GenericFunction.MethodEpoch)
            return null;
        return table.LookupArgs(args, s_argDispatchClass);
    }

    private static bool SameArgTypes(LispClass?[] a, LispClass?[] b)
    {
        if (a.Length != b.Length) return false;
        for (int i = 0; i < a.Length; i++)
            if (!ReferenceEquals(a[i], b[i])) return false;
        return true;
    }

    /// <summary>Run a cache hit that has :around methods: the arounds wrap a
    /// before/primary/after combination, which CALL-NEXT-METHOD reaches through the
    /// closure passed as the fallback.
    ///
    /// This lives in its own method so that DISPATCHGF has no lambda capturing its
    /// locals. C# allocates a closure's display class where the captured variables
    /// are declared, on entry to the scope, not where the lambda is written, so
    /// writing this inline made every dispatch, :around or not, allocate a display
    /// class it almost never used. That was 56 B of the ~110 B a GF call allocated,
    /// and it survived returning early from the dispatcher, which is what made it
    /// hard to place.</summary>
    private static LispObject InvokeAroundCombination(
        GenericFunction gf, CachedDispatch cached, List<LispMethod> primary, LispObject[] args)
        => InvokeAroundCombination(gf, cached.Around, cached.Before, primary, cached.After, args);

    /// <summary>As above, for the cache-miss path, which holds the four method lists
    /// in locals rather than in a cache entry.</summary>
    private static LispObject InvokeAroundCombination(
        GenericFunction gf, List<LispMethod> around, List<LispMethod> before,
        List<LispMethod> primary, List<LispMethod> after, LispObject[] args)
    {
        var savedGf = _currentGF;
        _currentGF = gf;
        try
        {
            return InvokeWithNextMethods(around, 0, args,
                       a => InvokeStandardCombination(gf, before, primary, after, a));
        }
        finally
        {
            _currentGF = savedGf;
        }
    }

    /// <summary>Sorts methods by specificity for one call's arguments. A class rather
    /// than the lambda it replaces: a lambda over ARGS captures the dispatcher's own
    /// parameter, and C# then allocates a display class on entry to DISPATCHGF; on
    /// every call, including the cache hits that never sort anything.</summary>
    private sealed class SpecificityComparer : IComparer<LispMethod>
    {
        private readonly LispObject[] _args;
        private readonly bool _reverse;
        public SpecificityComparer(LispObject[] args, bool reverse)
        { _args = args; _reverse = reverse; }
        public int Compare(LispMethod? a, LispMethod? b)
            => _reverse ? CompareMethodSpecificity(b!, a!, _args)
                        : CompareMethodSpecificity(a!, b!, _args);
    }

    private static void ParseLambdaListIntoGF(GenericFunction gf, LispObject ll)
    {
        int req = 0; bool rest = false; bool key = false;
        var cur = ll;
        while (cur is Cons c)
        {
            var sym = c.Car as Symbol;
            if (sym?.Name == "&REST" || sym?.Name == "&BODY") { rest = true; }
            else if (sym?.Name == "&KEY") { key = true; }
            else if (sym?.Name == "&OPTIONAL" || sym?.Name == "&AUX" ||
                     sym?.Name == "&ALLOW-OTHER-KEYS") { }
            else if (sym != null && sym.Name[0] != '&') { if (!rest && !key) req++; }
            cur = c.Cdr;
        }
        gf.RequiredCount = req;
        gf.HasRest = rest;
        gf.HasKey = key;
        gf.LambdaListInfoSet = true;
        gf.StoredLambdaList = ll;
    }

    private static void ParseLambdaListIntoMethod(LispMethod m, LispObject ll)
    {
        int req = 0; bool rest = false; bool key = false;
        var cur = ll;
        while (cur is Cons c)
        {
            var elem = c.Car;
            Symbol? sym = elem is Symbol s ? s : elem is Cons sc ? sc.Car as Symbol : null;
            if (sym?.Name == "&REST" || sym?.Name == "&BODY") { rest = true; }
            else if (sym?.Name == "&KEY") { key = true; }
            else if (sym?.Name == "&OPTIONAL" || sym?.Name == "&AUX" ||
                     sym?.Name == "&ALLOW-OTHER-KEYS") { }
            else if (sym != null && sym.Name[0] != '&') { if (!rest && !key) req++; }
            cur = c.Cdr;
        }
        m.RequiredCount = req;
        m.HasRest = rest;
        m.HasKey = key;
    }

    private static LispObject[] CollectList(LispObject lst)
    {
        var result = new List<LispObject>();
        var cur = lst;
        while (cur is Cons c) { result.Add(c.Car); cur = c.Cdr; }
        return result.ToArray();
    }
}
