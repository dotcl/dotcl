using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    /// <summary>
    /// Dispatch for built-in operator method combinations (CLHS 7.6.6.4).
    /// Methods qualified with the operator name are the "primary" methods.
    /// :AROUND methods work as in standard combination.
    /// No :BEFORE/:AFTER methods allowed.
    /// </summary>
    /// <summary>
    /// Dispatch for long-form method combinations (CLHS 7.6.6.2).
    /// Categorizes methods into groups, calls the body function to get an effective method form,
    /// then evaluates that form.
    /// </summary>
    private static LispObject DispatchLongFormCombination(
        GenericFunction gf, LongFormMethodCombination mc, List<LispMethod> applicable, LispObject[] args)
    {
        // Build mc-args as a Lisp list (needed for spec-function and body-function)
        LispObject mcArgsList = Nil.Instance;
        if (gf.MethodCombinationArgs != null)
        {
            for (int i = gf.MethodCombinationArgs.Length - 1; i >= 0; i--)
                mcArgsList = MakeCons(gf.MethodCombinationArgs[i], mcArgsList);
        }

        // Compute dynamic group specs from SpecFunction (allows lambda-list vars like :order order)
        var specList = mc.SpecFunction!.Invoke(new LispObject[] { mcArgsList });
        var groupSpecs = ParseMethodGroupSpecs(specList);

        // Categorize methods into groups according to specs
        var groups = new List<List<LispMethod>>();
        var assigned = new HashSet<LispMethod>();

        foreach (var spec in groupSpecs)
        {
            var group = new List<LispMethod>();
            foreach (var m in applicable)
            {
                if (assigned.Contains(m)) continue;
                if (MethodGroupMatches(spec, m))
                {
                    group.Add(m);
                    assigned.Add(m);
                }
            }

            group.Sort((a, b) => CompareMethodSpecificity(a, b, args));
            if (spec.Order == "MOST-SPECIFIC-LAST")
                group.Reverse();

            if (spec.Required && group.Count == 0)
                throw new LispErrorException(new LispError(
                    $"No applicable methods for required method group {spec.Name} " +
                    $"in method combination for {gf.Name.Name}"));

            groups.Add(group);
        }

        foreach (var m in applicable)
        {
            if (!assigned.Contains(m))
            {
                var qualStr = m.Qualifiers.Length > 0 ? m.Qualifiers[0].ToString() : "(unqualified)";
                throw new LispErrorException(new LispError(
                    $"No method group matches qualifier {qualStr} " +
                    $"in method combination {gf.MethodCombination!.Name} for {gf.Name.Name}"));
            }
        }

        // Build method groups as a Lisp list of lists
        LispObject groupsList = Nil.Instance;
        for (int gi = groups.Count - 1; gi >= 0; gi--)
        {
            LispObject methodList = Nil.Instance;
            for (int i = groups[gi].Count - 1; i >= 0; i--)
                methodList = MakeCons(groups[gi][i], methodList);
            groupsList = MakeCons(methodList, groupsList);
        }

        if (mc.ArgumentsInfo == null)
        {
            // Registered by an older compiler: the body function takes the argument
            // values and the form is run by the private evaluator.
            LispObject gfArgsList = Nil.Instance;
            for (int i = args.Length - 1; i >= 0; i--)
                gfArgsList = MakeCons(args[i], gfArgsList);
            var legacyForm = mc.BodyFunction!.Invoke(new LispObject[] { mcArgsList, groupsList, gfArgsList });
            var savedGf = _currentGF;
            _currentGF = gf;
            try { return EvalEffectiveMethodForm(legacyForm, args); }
            finally { _currentGF = savedGf; }
        }

        // The effective method form is Lisp code (CLHS 7.6.6.2). It depends only on
        // the method groups, so it is compiled once per set of groups.
        var keyItems = new List<object>();
        keyItems.Add(gf);
        foreach (var g in groups)
        {
            keyItems.Add(Nil.Instance);
            foreach (var m in g) keyItems.Add(m);
        }
        var key = new EffectiveMethodKey(keyItems.ToArray());
        if (!mc.Cache.TryGetValue(key, out var emFn))
        {
            var freshVars = (mc.ArgumentsInfo is Cons i1 && i1.Cdr is Cons i2 && i2.Cdr is Cons i3)
                ? i3.Car : Nil.Instance;
            var effectiveMethodForm = mc.BodyFunction!.Invoke(
                new LispObject[] { mcArgsList, groupsList, gf, freshVars });
            emFn = CompileEffectiveMethodForm(effectiveMethodForm, mc.ArgumentsInfo);
            // Methods that were removed or redefined leave stale entries behind;
            // bound the table rather than track them.
            if (mc.Cache.Count > 512) mc.Cache.Clear();
            mc.Cache[key] = emFn;
        }
        var savedGf2 = _currentGF;
        _currentGF = gf;
        try { return emFn.Invoke(args); }
        finally { _currentGF = savedGf2; }
    }

    /// <summary>Compile an effective method form into a function of the generic
    /// function.s arguments, with CALL-METHOD and MAKE-METHOD as local macros.</summary>
    internal static LispFunction CompileEffectiveMethodForm(LispObject form, LispObject argumentsInfo)
    {
        var compiler = Startup.Sym("%COMPILE-EFFECTIVE-METHOD").Function as LispFunction
            ?? throw new LispErrorException(new LispError(
                "effective method: %COMPILE-EFFECTIVE-METHOD is not defined"));
        var fn = compiler.Invoke(new LispObject[] { form, argumentsInfo });
        if (fn is MvReturn mv) fn = mv.PrimaryValue;
        return fn as LispFunction
            ?? throw new LispErrorException(new LispTypeError(
                "effective method: compilation did not return a function", fn));
    }

    /// <summary>Runtime half of CALL-METHOD: run METHOD with NEXT as its next-method
    /// list on the argument list ARGS.</summary>
    internal static LispObject CallMethodRt(LispObject method, LispObject next, LispObject argList)
    {
        var args = new List<LispObject>();
        for (var a = argList; a is Cons ac; a = ac.Cdr) args.Add(ac.Car);
        if (method is LispMethod lm)
        {
            var chain = new List<LispMethod> { lm };
            for (var rest = next; rest is Cons nc; rest = nc.Cdr)
            {
                if (nc.Car is LispMethod nm) chain.Add(nm);
                else throw new LispErrorException(new LispTypeError(
                    "CALL-METHOD: not a method in the next-method list", nc.Car));
            }
            return InvokeWithNextMethods(chain, 0, args.ToArray(), null);
        }
        if (method is LispFunction fn)
            return fn.Invoke(args.ToArray());
        throw new LispErrorException(new LispTypeError("CALL-METHOD: invalid method object", method));
    }

    private static bool IsMethodGroupOption(LispObject x)
        => x is Symbol k && k.HomePackage?.Name == "KEYWORD"
           && (k.Name == "DESCRIPTION" || k.Name == "ORDER" || k.Name == "REQUIRED");

    private static bool MethodGroupMatches(MethodGroupSpec spec, LispMethod m)
    {
        foreach (var pat in spec.Patterns)
        {
            if (pat is Symbol ps && pat is not Nil)
            {
                if (ps.Name == "*") return true;
                if (ps.Function is LispFunction pf)
                {
                    LispObject quals = Nil.Instance;
                    for (int i = m.Qualifiers.Length - 1; i >= 0; i--)
                        quals = new Cons(m.Qualifiers[i], quals);
                    var groupOk = Runtime.IsTruthy(pf.Invoke(new[] { quals }));
                    MultipleValues.Reset();
                    if (groupOk) return true;
                    continue;
                }
                // Not a predicate: read as the name of a single qualifier, as
                // this runtime always has.
                if (m.Qualifiers.Length == 1 && LispMethod.QualifierName(m.Qualifiers[0]) == ps.Name) return true;
                continue;
            }
            if (QualifierPatternMatches(pat, m.Qualifiers, 0)) return true;
        }
        return false;
    }

    /// <summary>CLHS 7.6.6.x qualifier pattern: a list matched element by element,
    /// where * matches any one qualifier and a * tail matches any rest.</summary>
    private static bool QualifierPatternMatches(LispObject pat, LispObject[] quals, int i)
    {
        while (true)
        {
            if (pat is Symbol tail && pat is not Nil && tail.Name == "*") return true;
            if (pat is Nil) return i == quals.Length;
            if (pat is not Cons pc || i >= quals.Length) return false;
            if (!(pc.Car is Symbol el && el.Name == "*")
                && !ReferenceEquals(pc.Car, quals[i])
                && !(pc.Car is Symbol ps && quals[i] is Symbol qs && ps.Name == qs.Name
                     && ReferenceEquals(ps.HomePackage, qs.HomePackage))
                && !(pc.Car is not Symbol && IsTrueEql(pc.Car, quals[i])))
                return false;
            pat = pc.Cdr;
            i++;
        }
    }

    private static List<MethodGroupSpec> ParseMethodGroupSpecs(LispObject specList)
    {
        var result = new List<MethodGroupSpec>();
        var cur = specList;
        while (cur is Cons sc)
        {
            var spec = sc.Car;
            var gs = new MethodGroupSpec();
            if (spec is Cons specCons)
            {
                gs.Name = (specCons.Car is Symbol gsSym) ? gsSym.Name : specCons.Car.ToString();
                if (specCons.Cdr is Cons r2)
                {
                    // One or more patterns, up to the first option keyword.
                    LispObject opts = r2;
                    while (opts is Cons pc && !IsMethodGroupOption(pc.Car))
                    {
                        gs.Patterns.Add(pc.Car);
                        opts = pc.Cdr;
                    }

                    while (opts is Cons oc)
                    {
                        if (oc.Car is Symbol kw)
                        {
                            var val = (oc.Cdr is Cons vc) ? vc.Car : Nil.Instance;
                            if (kw.Name == "ORDER" || kw.Name == ":ORDER")
                            {
                                if (val is Symbol vs && vs.Name == "MOST-SPECIFIC-LAST")
                                    gs.Order = "MOST-SPECIFIC-LAST";
                            }
                            else if (kw.Name == "REQUIRED" || kw.Name == ":REQUIRED")
                            {
                                gs.Required = val is not Nil;
                            }
                            opts = (oc.Cdr is Cons vc2) ? vc2.Cdr : Nil.Instance;
                        }
                        else opts = oc.Cdr;
                    }
                }
            }
            result.Add(gs);
            cur = sc.Cdr;
        }
        return result;
    }

    /// <summary>Run an effective method form on a call.s arguments. The private
    /// evaluator below is the one dispatch already uses; this is the entry point
    /// DOTCL-MOP:COMPUTE-EFFECTIVE-METHOD-FUNCTION hands out as a closure, so the
    /// two cannot read the same form differently.</summary>
    public static LispObject ApplyEffectiveMethodForm(LispObject form, LispObject[] args)
        => EvalEffectiveMethodForm(form, args);

    /// <summary>
    /// Evaluate an effective method form from a long-form method combination.
    /// Handles CALL-METHOD and MAKE-METHOD special forms.
    /// </summary>
    private static LispObject EvalEffectiveMethodForm(LispObject form, LispObject[] args)
    {
        if (form is Cons c)
        {
            if (c.Car is Symbol sym)
            {
                if (sym.Name == "CALL-METHOD")
                {
                    // (call-method method next-method-list). The next-method list
                    // is what CALL-NEXT-METHOD inside the method walks, so it has
                    // to be installed as the chain rather than dropped: without it
                    // a long-form method combination silently loses
                    // CALL-NEXT-METHOD while the standard one keeps it.
                    var methodObj = (c.Cdr is Cons mc1) ? mc1.Car : Nil.Instance;
                    var nextForm = (c.Cdr is Cons mc2 && mc2.Cdr is Cons mc3)
                        ? mc3.Car : Nil.Instance;
                    if (methodObj is LispMethod method)
                    {
                        var chain = new List<LispMethod> { method };
                        for (var rest = nextForm; rest is Cons nc; rest = nc.Cdr)
                        {
                            if (nc.Car is LispMethod nextMethod) { chain.Add(nextMethod); continue; }
                            // (make-method form): a method whose body is the form.
                            // The standard combination builds exactly this for the
                            // around chain -- (call-method around ((make-method
                            // (call-method primary nil)))) -- so without it
                            // CALL-NEXT-METHOD out of an :around method has nowhere
                            // to go. The body re-enters this evaluator, which
                            // installs the inner form.s own chain.
                            if (nc.Car is Cons mm && mm.Car is Symbol mms
                                && mms.Name == "MAKE-METHOD" && mm.Cdr is Cons mmBody)
                            {
                                var body = mmBody.Car;
                                var synthetic = (LispMethod)MakeMethod(Nil.Instance, Nil.Instance,
                                    new LispFunction(inner => EvalEffectiveMethodForm(body, inner),
                                        "MAKE-METHOD body", -1));
                                chain.Add(synthetic);
                            }
                        }
                        return InvokeWithNextMethods(chain, 0, args, null);
                    }
                    if (methodObj is LispFunction fn)
                        return fn.Invoke(args);
                    throw new LispErrorException(new LispError($"CALL-METHOD: invalid method object {methodObj}"));
                }
                if (sym.Name == "VECTOR")
                {
                    // (vector expr1 expr2 ...) - evaluate each and make a vector
                    var elems = new List<LispObject>();
                    var cur = c.Cdr;
                    while (cur is Cons vc)
                    {
                        elems.Add(EvalEffectiveMethodForm(vc.Car, args));
                        cur = vc.Cdr;
                    }
                    return new LispVector(elems.ToArray());
                }
                if (sym.Name == "QUOTE")
                {
                    return (c.Cdr is Cons qc) ? qc.Car : Nil.Instance;
                }
                if (sym.Name == "PROGN")
                {
                    LispObject result = Nil.Instance;
                    var cur = c.Cdr;
                    while (cur is Cons pc)
                    {
                        result = EvalEffectiveMethodForm(pc.Car, args);
                        cur = pc.Cdr;
                    }
                    return result;
                }
            }
            // Unknown form: return as data (already evaluated by the body function's backquote)
            return form;
        }
        // Atoms (including symbols, numbers, etc): return as-is
        // These are already the result of backquote substitution in the body function
        return form;
    }

    private static LispObject DispatchBuiltinCombination(
        GenericFunction gf, List<LispMethod> applicable, LispObject[] args)
    {
        PartitionBuiltinCombination(gf, applicable, args, out var combined, out var around);
        return InvokeBuiltinCombination(gf, combined, around, args);
    }

    /// <summary>Split the applicable methods of an operator method combination into
    /// the operator's methods and the :around methods, each sorted. The result
    /// depends only on the argument classes, so a dispatch cache entry keeps it and
    /// a hit reuses it instead of re-sorting on every call.</summary>
    private static void PartitionBuiltinCombination(
        GenericFunction gf, List<LispMethod> applicable, LispObject[] args,
        out List<LispMethod> combinedMethods, out List<LispMethod> aroundMethods)
    {
        string mcName = gf.MethodCombination!.Name;
        combinedMethods = new List<LispMethod>();
        aroundMethods = new List<LispMethod>();

        foreach (var m in applicable)
        {
            // CLHS 7.6.6.2 (short-form / operator method combination): every applicable
            // method must be qualified by exactly the combination name or :around.
            // An unqualified method or any other qualifier (e.g. a stray `nonsense`)
            // is an invalid-method-error, not silently ignored. ANSI
            // DEFGENERIC-METHOD-COMBINATION.APPEND.13.
            bool isAround = m.Qualifiers.Length == 1 && LispMethod.QualifierName(m.Qualifiers[0]) == "AROUND";
            bool isOperator = m.Qualifiers.Length == 1 && LispMethod.QualifierName(m.Qualifiers[0]) == mcName;
            if (isAround)
                aroundMethods.Add(m);
            else if (isOperator)
                combinedMethods.Add(m);
            else
                throw new LispErrorException(new LispError(
                    $"{gf.Name.Name}: method with invalid qualifiers {Runtime.List(m.Qualifiers.Cast<LispObject>().ToArray())} " +
                    $"for the {mcName} method combination"));
        }

        if (combinedMethods.Count == 0)
            throw new LispErrorException(new LispError(
                $"No applicable {mcName} method for generic function {gf.Name.Name}"));

        // Sort: most specific first (default order)
        combinedMethods.Sort((a, b) => CompareMethodSpecificity(a, b, args));
        // Reverse for most-specific-last
        if (!gf.MostSpecificFirst)
            combinedMethods.Reverse();
        if (aroundMethods.Count > 1)
            aroundMethods.Sort((a, b) => CompareMethodSpecificity(a, b, args));
    }

    private static LispObject InvokeBuiltinCombination(
        GenericFunction gf, List<LispMethod> combinedMethods, List<LispMethod> aroundMethods,
        LispObject[] args)
    {
        ref var nm = ref Nm;
        var savedGf = nm.Gf;
        nm.Gf = gf;
        try
        {
            if (aroundMethods.Count == 0)
                return RunBuiltinOperator(gf, combinedMethods, args);
            // Takes nextArgs so an :around method's (call-next-method ...) with explicit
            // arguments forwards them to the combined operator body (CLHS 7.6.6.1).
            return InvokeWithNextMethods(aroundMethods, 0, args,
                nextArgs => RunBuiltinOperator(gf, combinedMethods, nextArgs));
        }
        finally
        {
            nm.Gf = savedGf;
        }
    }

    private static LispObject RunBuiltinOperator(
        GenericFunction gf, List<LispMethod> combinedMethods, LispObject[] nextArgs)
    {
        string mcName = gf.MethodCombination!.Name;
        // Resolve custom method combination to its operator name
        string operatorName = mcName;
        bool identityWithOneArg = operatorName is "AND" or "OR" or "PROGN" or "NCONC" or "MAX" or "MIN" or "+";
        if (TryGetMC(_methodCombinationRegistry, gf.MethodCombination!, out var regEntry))
        {
            operatorName = regEntry.Operator;
            identityWithOneArg = regEntry.IdentityWithOneArg;
        }
        // :identity-with-one-argument - skip operator when single method
        if (identityWithOneArg && combinedMethods.Count == 1)
            return InvokeBuiltinMethod(combinedMethods[0], nextArgs);
        int last = combinedMethods.Count - 1;
        switch (operatorName)
        {
            case "+":
            {
                LispObject result = Fixnum.Make(0);
                foreach (var m in combinedMethods)
                    result = Arithmetic.Add(AsNumber(result), AsNumber(PrimaryOf(InvokeBuiltinMethod(m, nextArgs))));
                return OneValue(result);
            }
            case "*":
            {
                LispObject result = Fixnum.Make(1);
                foreach (var m in combinedMethods)
                    result = Arithmetic.Multiply(AsNumber(result), AsNumber(PrimaryOf(InvokeBuiltinMethod(m, nextArgs))));
                return OneValue(result);
            }
            case "MIN":
            case "MAX":
            {
                int sign = operatorName == "MAX" ? 1 : -1;
                LispObject result = PrimaryOf(InvokeBuiltinMethod(combinedMethods[0], nextArgs));
                for (int i = 1; i < combinedMethods.Count; i++)
                {
                    var val = PrimaryOf(InvokeBuiltinMethod(combinedMethods[i], nextArgs));
                    if (Arithmetic.Compare(AsNumber(val), AsNumber(result)) * sign > 0)
                        result = val;
                }
                return OneValue(result);
            }
            case "AND":
            {
                for (int i = 0; i < last; i++)
                    if (PrimaryOf(InvokeBuiltinMethod(combinedMethods[i], nextArgs)) is Nil)
                        return OneValue(Nil.Instance);
                MultipleValues.Reset();
                return InvokeBuiltinMethod(combinedMethods[last], nextArgs);
            }
            case "OR":
            {
                for (int i = 0; i < last; i++)
                {
                    var result = PrimaryOf(InvokeBuiltinMethod(combinedMethods[i], nextArgs));
                    if (result is not Nil) return OneValue(result);
                }
                MultipleValues.Reset();
                return InvokeBuiltinMethod(combinedMethods[last], nextArgs);
            }
            case "PROGN":
            {
                for (int i = 0; i < last; i++)
                    InvokeBuiltinMethod(combinedMethods[i], nextArgs);
                MultipleValues.Reset();
                return InvokeBuiltinMethod(combinedMethods[last], nextArgs);
            }
            case "LIST":
            {
                var results = new LispObject[combinedMethods.Count];
                for (int i = 0; i < results.Length; i++)
                    results[i] = PrimaryOf(InvokeBuiltinMethod(combinedMethods[i], nextArgs));
                LispObject result = Nil.Instance;
                for (int i = results.Length - 1; i >= 0; i--)
                    result = MakeCons(results[i], result);
                return OneValue(result);
            }
            case "NCONC":
            {
                LispObject result = Nil.Instance;
                foreach (var m in combinedMethods)
                    result = NconcTwo(result, PrimaryOf(InvokeBuiltinMethod(m, nextArgs)));
                return OneValue(result);
            }
            case "APPEND":
            {
                var results = new LispObject[combinedMethods.Count];
                for (int i = 0; i < results.Length; i++)
                    results[i] = PrimaryOf(InvokeBuiltinMethod(combinedMethods[i], nextArgs));
                LispObject result = results[last];
                for (int i = last - 1; i >= 0; i--)
                    result = Append(results[i], result);
                return OneValue(result);
            }
            default:
                // CLHS DEFINE-METHOD-COMBINATION short form: the operator may be
                // any function, macro or special operator, applied as
                // (operator (call-method m1) (call-method m2) ...).
                if (TryGetMC(_methodCombinationOperatorSymbols, gf.MethodCombination!, out var opSym))
                    return CallShortFormOperator(opSym, combinedMethods, nextArgs);
                throw new LispErrorException(new LispError(
                    $"Unknown method combination operator: {operatorName} (combination: {mcName})"));
        }
    }

    private static LispObject PrimaryOf(LispObject v) => v is MvReturn mv ? mv.PrimaryValue : v;

    /// <summary>Run an operator-combination method, which has no next method.</summary>
    private static LispObject InvokeBuiltinMethod(LispMethod m, LispObject[] args)
    {
        if (m.IgnoresNextMethodState) return m.Function.Invoke(args);
        ref var nm = ref Nm;
        var savedMethod = nm.Method;
        var savedChain = nm.Chain;
        var savedIndex = nm.Index;
        var savedArgs = nm.Args;
        var savedFallback = nm.Fallback;
        var savedNmp = nm.CapturedNmp;
        var savedCnm = nm.CapturedCnm;
        nm.Method = m;
        nm.Chain = null;
        nm.Index = 0;
        nm.Args = args;
        nm.Fallback = null;
        nm.CapturedNmp = _defaultNmp;
        nm.CapturedCnm = null;
        try { return m.Function.Invoke(args); }
        finally
        {
            nm.Method = savedMethod;
            nm.Chain = savedChain;
            nm.Index = savedIndex;
            nm.Args = savedArgs;
            nm.Fallback = savedFallback;
            nm.CapturedNmp = savedNmp;
            nm.CapturedCnm = savedCnm;
        }
    }

    /// <summary>V as the one value of the combination, which the method calls may have
    /// published values of their own past.</summary>
    private static LispObject OneValue(LispObject v)
    {
        MultipleValues.Reset();
        return v;
    }

    /// <summary>Apply a short-form combination's own operator to the combined
    /// methods. A function gets the methods' values, computed most specific first
    /// as argument evaluation would. A macro or special operator decides itself
    /// which of the method calls run, so it gets the calls as forms and the whole
    /// form is evaluated.</summary>
    private static LispObject CallShortFormOperator(Symbol opSym, List<LispMethod> methods, LispObject[] nextArgs)
    {
        bool isMacro = MacroFunction(opSym) is not Nil;
        bool isSpecial = SpecialOperatorP(opSym) is not Nil;
        if (!isMacro && !isSpecial && opSym.Function is LispFunction fn)
        {
            var values = new LispObject[methods.Count];
            for (int i = 0; i < methods.Count; i++)
                values[i] = PrimaryOf(InvokeBuiltinMethod(methods[i], nextArgs));
            MultipleValues.Reset();
            return fn.Invoke(values);
        }
        var funcall = Startup.Sym("FUNCALL");
        LispObject calls = Nil.Instance;
        for (int i = methods.Count - 1; i >= 0; i--)
        {
            var m = methods[i];
            var thunk = new LispFunction(_ => InvokeBuiltinMethod(m, nextArgs));
            calls = MakeCons(Runtime.List(funcall, thunk), calls);
        }
        return Eval(MakeCons(opSym, calls));
    }

    /// <summary>Destructively append b to the end of a (nconc for two lists).</summary>
    private static LispObject NconcTwo(LispObject a, LispObject b)
    {
        if (a is Nil) return b;
        if (a is not Cons ca)
            throw new LispErrorException(new LispTypeError("NCONC: not a list", a));
        var last = ca;
        while (last.Cdr is Cons next)
            last = next;
        last.Cdr = b;
        return a;
    }

    private static LispObject InvokeStandardCombination(
        GenericFunction gf, List<LispMethod> before, List<LispMethod> primary,
        List<LispMethod> after, LispObject[] args)
    {
        ref var nm = ref Nm;
        var savedGf = nm.Gf;
        nm.Gf = gf;
        try
        {
            // Fast path: single primary, no before/after -> minimal next-method setup
            if (before.Count == 0 && after.Count == 0 && primary.Count == 1)
            {
                if (primary[0].IgnoresNextMethodState) return primary[0].Function.Invoke(args);
                var savedMethod = nm.Method;
                var savedChain = nm.Chain;
                var savedIndex = nm.Index;
                var savedArgs = nm.Args;
                var savedFallback = nm.Fallback;
                nm.Method = primary[0];
                nm.Chain = primary;
                nm.Index = 1;
                nm.Args = args;
                nm.Fallback = null;
                // No next method: a capture builds from this chain state.
                var savedCapturedNmp = nm.CapturedNmp;
                var savedCapturedCnm = nm.CapturedCnm;
                nm.CapturedNmp = _defaultNmp;
                nm.CapturedCnm = null;
                try
                {
                    return primary[0].Function.Invoke(args);
                }
                finally
                {
                    nm.Method = savedMethod;
                    nm.Chain = savedChain;
                    nm.Index = savedIndex;
                    nm.Args = savedArgs;
                    nm.Fallback = savedFallback;
                    nm.CapturedNmp = savedCapturedNmp;
                    nm.CapturedCnm = savedCapturedCnm;
                }
            }

            // :before methods (most specific first)
            foreach (var m in before)
                InvokeAux(m, args);
            if (before.Count != 0) MultipleValues.Reset();

            // Primary methods with call-next-method chain
            var result = InvokeWithNextMethods(primary, 0, args, null);
            if (after.Count == 0) return result;

            // :after methods (least specific first: already sorted that way)
            var held = MultipleValues.Hold(result);
            foreach (var m in after)
                InvokeAux(m, args);
            MultipleValues.RestoreHeld(held);
            return result;
        }
        finally
        {
            nm.Gf = savedGf;
        }
    }

    /// <summary>The next-method state of the method invocation in progress on this
    /// thread. Every method call through a combination saves and restores it.</summary>
    private struct NextMethodState
    {
        internal GenericFunction? Gf;
        internal LispMethod? Method;
        internal List<LispMethod>? Chain;
        internal LispObject[]? Args;
        /// <summary>The arguments of an invocation that came through one of the
        /// loose-argument entry points, which have no array to publish. Read only
        /// through <see cref="CurrentGFArgs"/>, and only meaningful while Args is
        /// null; Argc says how many of them are live.</summary>
        internal LispObject? Arg0, Arg1, Arg2, Arg3;
        internal Func<LispObject[], LispObject>? Fallback;
        internal LispObject? CapturedCnm, CapturedNmp;
        internal int Index, Argc;
    }

    [ThreadStatic] private static NextMethodState[]? _nmRoot;

    private static ref NextMethodState Nm
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => ref (_nmRoot ??= new NextMethodState[1])[0];
    }

    private static ref GenericFunction? _currentGF => ref Nm.Gf;
    private static ref LispMethod? _currentMethod => ref Nm.Method;
    private static ref List<LispMethod>? _nextMethodChain => ref Nm.Chain;
    private static ref int _nextMethodIndex => ref Nm.Index;
    private static ref LispObject[]? _currentGFArgs => ref Nm.Args;
    private static ref LispObject? _currentGFArg0 => ref Nm.Arg0;
    private static ref LispObject? _currentGFArg1 => ref Nm.Arg1;
    private static ref LispObject? _currentGFArg2 => ref Nm.Arg2;
    private static ref LispObject? _currentGFArg3 => ref Nm.Arg3;
    private static ref int _currentGFArgc => ref Nm.Argc;
    private static ref Func<LispObject[], LispObject>? _nextMethodFallback => ref Nm.Fallback;
    private static ref LispObject? _capturedCnm => ref Nm.CapturedCnm;
    private static ref LispObject? _capturedNmp => ref Nm.CapturedNmp;

    /// <summary>The arguments of the invocation in progress, as a list. CALL-NEXT-METHOD
    /// with no arguments passes these on, and that is the only reader; so the arity-1
    /// path can skip building the array and have it materialised here instead, on the
    /// rare call that asks. An invocation with no next method never gets this far
    /// (the captured closure signals first), so the cost lands only where a real chain
    /// continues.</summary>
    private static LispObject[] CurrentGFArgs()
    {
        var args = _currentGFArgs;
        if (args != null) return args;
        return _currentGFArgc switch
        {
            1 => new[] { _currentGFArg0! },
            2 => new[] { _currentGFArg0!, _currentGFArg1! },
            3 => new[] { _currentGFArg0!, _currentGFArg1!, _currentGFArg2! },
            4 => new[] { _currentGFArg0!, _currentGFArg1!, _currentGFArg2!, _currentGFArg3! },
            _ => System.Array.Empty<LispObject>()
        };
    }
    // The fallback runs the "next method" when the next-method chain (e.g. the
    // :around list) is exhausted: typically the before/primary/after combination.
    // It takes the actual arguments so that (call-next-method ...) with EXPLICIT
    // arguments propagates them to that combination (CLHS 7.6.6.1), rather than
    // capturing the original generic-function arguments.

    // Per-invocation CALL-NEXT-METHOD / NEXT-METHOD-P closures, captured by a
    // method body at entry via the %CAPTURED-CALL-NEXT-METHOD / %CAPTURED-NEXT-METHOD-P
    // intrinsics (CapturedCnm/CapturedNmp below). These MUST be thread-local: they
    // used to be published by mutating the GLOBAL symbol-functions of
    // CALL-NEXT-METHOD / NEXT-METHOD-P, which a second thread dispatching
    // concurrently would clobber: the victim method then captured the other
    // thread's closure (or the fast-path "no next method" stub) and signalled a
    // spurious "CALL-NEXT-METHOD: no next method" / wrong dispatch (the
    // residual race after the method list became copy-on-write).


    // Default CALL-NEXT-METHOD outside a method invocation.
    private static readonly LispFunction _defaultCnm = new LispFunction(
        a => CallNoNextMethod(_currentGF, _currentMethod,
                              a.Length > 0 ? a : CurrentGFArgs()),
        "CALL-NEXT-METHOD", -1);
    // Arity is checked here too: this is what CapturedNmp hands back when the
    // invocation has no next-method chain, so without it (next-method-p nil)
    // would still be silently accepted on that path (CLHS: exactly 0 arguments).
    private static readonly LispFunction _defaultNmp = new LispFunction(
        a => {
            if (a.Length != 0)
                throw new LispErrorException(new LispProgramError(
                    $"NEXT-METHOD-P: wrong number of arguments: {a.Length} (expected 0)"));
            return Nil.Instance;
        }, "NEXT-METHOD-P", 0);

    /// <summary>Intrinsic: the current method's captured CALL-NEXT-METHOD closure
    /// (thread-local). Emitted for the defmethod-body capture instead of
    /// (symbol-function 'call-next-method), which read a process-global field.
    ///
    /// The closure is built LAZILY from the thread-local chain state that
    /// InvokeWithNextMethods installs before invoking the method body. The
    /// defmethod compiler only emits this capture (in the body prologue, before
    /// anything can disturb the thread state) when the body actually mentions
    /// CALL-NEXT-METHOD / NEXT-METHOD-P: so methods that never use them pay no
    /// per-dispatch closure allocation at all. The lazily built closure snapshots
    /// the same values the previous eager version captured, keeping indefinite
    /// extent (CLHS 7.6.6.1).</summary>
    public static LispObject CapturedCnm()
    {
        var cached = _capturedCnm;
        if (cached != null) return cached;
        var chain = _nextMethodChain;
        var currentMethod = _currentMethod;
        if (chain == null && currentMethod == null) return _defaultCnm;
        var closureChain = chain;
        var closureIdx = chain == null ? 0 : _nextMethodIndex;
        var closureArgs = CurrentGFArgs();
        var closureFallback = _nextMethodFallback;
        var closureGf = _currentGF;
        var cnm = new LispFunction(
            cnmArgs => {
                var actualArgs = cnmArgs.Length > 0 ? cnmArgs : closureArgs;
                if (cnmArgs.Length > 0 && currentMethod != null
                    && !IsMethodApplicable(currentMethod, actualArgs))
                    throw new LispErrorException(new LispProgramError(
                        "CALL-NEXT-METHOD: changed arguments are not applicable to the current method"));
                return CallNextMethodWithChain(closureChain, closureIdx, actualArgs,
                                               closureFallback, closureGf, currentMethod);
            },
            "CALL-NEXT-METHOD", -1);
        _capturedCnm = cnm; // memoized until InvokeWithNextMethods restores the frame
        return cnm;
    }
    /// <summary>Intrinsic: the current method's captured NEXT-METHOD-P closure
    /// (thread-local). Lazily built like CapturedCnm.</summary>
    public static LispObject CapturedNmp()
    {
        var cached = _capturedNmp;
        if (cached != null) return cached;
        var chain = _nextMethodChain;
        if (chain == null) return _defaultNmp;
        var closureChain = chain;
        var closureIdx = _nextMethodIndex;
        var closureFallback = _nextMethodFallback;
        var nmp = new LispFunction(
            nmpArgs => {
                if (nmpArgs.Length != 0)
                    throw new LispErrorException(new LispProgramError(
                        $"NEXT-METHOD-P: wrong number of arguments: {nmpArgs.Length} (expected 0)"));
                return (closureIdx < closureChain.Count || closureFallback != null)
                    ? (LispObject)T.Instance : Nil.Instance;
            },
            "NEXT-METHOD-P", 0);
        _capturedNmp = nmp;
        return nmp;
    }

    private static LispObject InvokeWithNextMethods(
        List<LispMethod> methods, int startIdx, LispObject[] args,
        Func<LispObject[], LispObject>? fallback, GenericFunction? gf = null)
    {
        if (methods[startIdx].IgnoresNextMethodState) return methods[startIdx].Function.Invoke(args);
        ref var nm = ref Nm;
        var savedGf = nm.Gf;
        var savedMethod = nm.Method;
        var savedChain = nm.Chain;
        var savedIndex = nm.Index;
        var savedArgs = nm.Args;
        var savedFallback = nm.Fallback;

        if (gf != null) nm.Gf = gf;
        nm.Method = methods[startIdx];
        nm.Chain = methods;
        nm.Index = startIdx + 1;
        nm.Args = args;
        nm.Fallback = fallback;

        // Indefinite-extent NEXT-METHOD-P / CALL-NEXT-METHOD closures (CLHS
        // 7.6.6.1, 7.6.6.2) are built LAZILY by CapturedNmp/CapturedCnm from the
        // thread state installed above: clearing the captured slots here marks
        // "this invocation has not built its closures yet". Method bodies that
        // never mention CALL-NEXT-METHOD / NEXT-METHOD-P (the defmethod compiler
        // emits the capture only when the body uses them) thus pay no closure
        // allocation per dispatch.
        var savedCapturedNmp = nm.CapturedNmp;
        var savedCapturedCnm = nm.CapturedCnm;
        nm.CapturedNmp = null;
        nm.CapturedCnm = null;

        try
        {
            return methods[startIdx].Function.Invoke(args);
        }
        finally
        {
            nm.Gf = savedGf;
            nm.Method = savedMethod;
            nm.Chain = savedChain;
            nm.Index = savedIndex;
            nm.Args = savedArgs;
            nm.Fallback = savedFallback;
            nm.CapturedNmp = savedCapturedNmp;
            nm.CapturedCnm = savedCapturedCnm;
        }
    }

    /// <summary>
    /// Call next method using captured chain state (for indefinite extent closures).
    /// </summary>
    private static LispObject CallNextMethodWithChain(
        List<LispMethod>? chain, int idx, LispObject[] args,
        Func<LispObject[], LispObject>? fallback, GenericFunction? gf, LispMethod? method)
    {
        if (chain != null && idx < chain.Count)
            return InvokeWithNextMethods(chain, idx, args, fallback, gf);
        if (fallback != null)
            return fallback(args);
        return CallNoNextMethod(gf, method, args);
    }

    /// <summary>
    /// (CALL-NEXT-METHOD) with no arguments, from a body whose invocation came in
    /// on the loose-argument path. Runs the next method on the same loose slots,
    /// so a chain of N methods builds no argument array at any step -- the array
    /// form had to materialise one per link just to pass the arguments along
    /// unchanged, which is what the no-argument call means.
    ///
    /// Hands over to the array form for everything it cannot run itself: an
    /// invocation that already holds an array, an exhausted chain (where the
    /// fallback runs), or an arity the loose slots do not cover.
    /// </summary>
    public static LispObject CallNextMethodLoose()
    {
        ref var nm = ref Nm;
        var chain = nm.Chain;
        if (chain == null) return CallNextMethod();
        int idx = nm.Index;
        if (nm.Args != null || idx >= chain.Count
            || nm.Argc < 1 || nm.Argc > 4)
            return CallNextMethod();

        var savedIndex = nm.Index;
        var savedMethod = nm.Method;
        var savedCapturedNmp = nm.CapturedNmp;
        var savedCapturedCnm = nm.CapturedCnm;
        nm.Index = idx + 1;
        nm.Method = chain[idx];
        // The next body builds its own closures from the chain state if it
        // captures, exactly as INVOKEWITHNEXTMETHODS arranges for the array path.
        nm.CapturedNmp = null;
        nm.CapturedCnm = null;
        try
        {
            var fn = chain[idx].Function;
            return nm.Argc switch
            {
                1 => fn.Invoke1(nm.Arg0!),
                2 => fn.Invoke2(nm.Arg0!, nm.Arg1!),
                3 => fn.Invoke3(nm.Arg0!, nm.Arg1!, nm.Arg2!),
                _ => fn.Invoke4(nm.Arg0!, nm.Arg1!, nm.Arg2!, nm.Arg3!)
            };
        }
        finally
        {
            nm.Index = savedIndex;
            nm.Method = savedMethod;
            nm.CapturedNmp = savedCapturedNmp;
            nm.CapturedCnm = savedCapturedCnm;
        }
    }

    /// <summary>Call NO-NEXT-METHOD with the generic function, method and arguments.</summary>
    private static LispObject CallNoNextMethod(
        GenericFunction? gf, LispMethod? method, LispObject[] args)
    {
        if (gf == null || method == null
            || Startup.Sym("NO-NEXT-METHOD").Function is not LispFunction nnm)
            throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method"));
        var nnmArgs = new LispObject[args.Length + 2];
        nnmArgs[0] = gf;
        nnmArgs[1] = method;
        Array.Copy(args, 0, nnmArgs, 2, args.Length);
        return nnm.Invoke(nnmArgs);
    }

    public static LispObject CallNextMethod(params LispObject[] args)
    {
        var chain = _nextMethodChain;
        LispObject[] actualArgs = args.Length > 0 ? args : CurrentGFArgs();

        // CLHS 7.6.6.1: When call-next-method is called with arguments,
        // the ordered set of applicable methods must be the same as for the
        // original arguments. Check that the new arguments are applicable to
        // the current method (the one that called call-next-method).
        var currentMethod = _currentMethod
            ?? (chain != null && _nextMethodIndex > 0
                ? chain[_nextMethodIndex - 1] : null);
        if (args.Length > 0 && currentMethod != null
            && !IsMethodApplicable(currentMethod, actualArgs))
        {
            throw new LispErrorException(new LispError(
                "CALL-NEXT-METHOD: changed arguments are not applicable to the current method"));
        }

        if (chain != null && _nextMethodIndex < chain.Count)
        {
            return InvokeWithNextMethods(chain, _nextMethodIndex, actualArgs, _nextMethodFallback);
        }
        else if (_nextMethodFallback != null)
        {
            return _nextMethodFallback(actualArgs);
        }
        else
        {
            return CallNoNextMethod(_currentGF, currentMethod, actualArgs);
        }
    }

    public static LispObject NextMethodP()
    {
        if (_nextMethodChain != null && _nextMethodIndex < _nextMethodChain.Count)
            return T.Instance;
        if (_nextMethodFallback != null)
            return T.Instance;
        return Nil.Instance;
    }

    /// <summary>Is SPECCLS applicable to an argument of ARGCLS through an assignability
    /// the class precedence list does not carry? .NET generic variance is the case that
    /// matters: List&lt;String&gt; implements IEnumerable&lt;String&gt;, and the covariant
    /// IEnumerable&lt;out T&gt; makes that assignable to IEnumerable&lt;Object&gt;; but
    /// enumerating it in the CPL would mean spelling out every instantiation over every
    /// supertype of every type argument. Asking the CLR the question only for the
    /// specializers a generic function actually has keeps it bounded.
    ///
    /// Only reached after the CPL check has already failed, and only when both sides are
    /// .NET classes, so ordinary CLOS dispatch is untouched.</summary>
    private static bool DotNetVarianceApplicable(LispClass specCls, LispClass? argCls)
    {
        var specType = specCls.DotNetType;
        var argType = argCls?.DotNetType;
        if (specType == null || argType == null) return false;
        // An open generic definition (List<T>) is a wildcard, not a real type: its
        // assignability is meaningless and the CPL already pairs it with each closed
        // instantiation.
        if (specType.ContainsGenericParameters || argType.ContainsGenericParameters) return false;
        return specType.IsAssignableFrom(argType);
    }

    /// <summary>Position of SPEC in ARGCLS's class precedence list, doubled so a
    /// variance-only match can be ranked between two CPL entries. A specializer the CPL
    /// does not mention but .NET says the argument is assignable to ranks immediately
    /// ahead of T: it is a genuine supertype, so it must beat the catch-all method, and
    /// it loses to everything the CPL actually spells out. Unrelated specializers rank
    /// last (int.MaxValue) and are left to the caller's name ordering.</summary>
    private static int SpecializerRank(LispClass spec, LispClass argCls)
    {
        var cpl = argCls.ClassPrecedenceList;
        int idx = Array.IndexOf(cpl, spec);
        if (idx >= 0) return idx * 2;
        if (!DotNetVarianceApplicable(spec, argCls)) return int.MaxValue;
        int tIdx = cpl.Length;
        for (int i = 0; i < cpl.Length; i++)
            if (cpl[i].Name.Name == "T") { tIdx = i; break; }
        return tIdx * 2 - 1;
    }

    /// <summary>Rank two .NET specializers neither of which the argument's CPL mentions
    /// (both reached the method through variance): the one assignable to the other is
    /// the more specific, e.g. IEnumerable&lt;String&gt; beats IEnumerable&lt;Object&gt;
    /// for a List&lt;String&gt; argument. 0 when they are unrelated or not .NET classes,
    /// leaving the caller's deterministic name ordering in charge.</summary>
    private static int CompareByDotNetAssignability(LispClass clsA, LispClass clsB)
    {
        var a = clsA.DotNetType;
        var b = clsB.DotNetType;
        if (a == null || b == null || a == b) return 0;
        bool aFromB = a.IsAssignableFrom(b);
        bool bFromA = b.IsAssignableFrom(a);
        if (aFromB && !bFromA) return 1;   // b is the narrower type
        if (bFromA && !aFromB) return -1;  // a is the narrower type
        return 0;
    }

    /// <summary>Whether a protocol generic function has a method other than its own
    /// default that applies to these arguments. A hook that only checks the method
    /// count learns that somebody, somewhere, specialised the protocol; this tells it
    /// whether the specialisation applies to the metaobject in hand.</summary>
    internal static bool HasSpecialisedMethodFor(GenericFunction gf, LispMethod defaultMethod,
                                                 LispObject[] args)
    {
        foreach (var m in gf.MethodsArray)
            if (!ReferenceEquals(m, defaultMethod) && IsMethodApplicable(m, args))
                return true;
        return false;
    }

    private static bool IsMethodApplicable(LispMethod method, LispObject[] args)
    {
        for (int i = 0; i < method.Specializers.Length; i++)
        {
            if (i >= args.Length) return false;
            var spec = method.Specializers[i];
            if (spec is LispClass cls)
            {
                // T class matches everything
                if (cls.Name.Name == "T") continue;
                // Use CPL-based check so that metaclass dispatch (LispClass as arg)
                // works correctly. ArgDispatchClass returns ClassOf(obj) for non-instances,
                // which includes the Metaclass for custom-metaclass LispClass objects.
                var argClass = ArgDispatchClass(args[i]);
                if (argClass != null)
                {
                    bool found = false;
                    foreach (var c in argClass.ClassPrecedenceList)
                        if (ReferenceEquals(c, cls)) { found = true; break; }
                    if (!found && !DotNetVarianceApplicable(cls, argClass)) return false;
                }
                else if (!IsTruthy(Typep(args[i], cls.Name)))
                    return false;
            }
            // EQL specializer: the metaobject, or the (EQL value) list
            else if (EqlSpecializerValue(spec) is { } eqlValue)
            {
                if (!IsTrueEql(args[i], eqlValue))
                    return false;
            }
        }
        return true;
    }

    private static int CompareMethodSpecificity(LispMethod a, LispMethod b, LispObject[] args)
    {
        // More specific = class appears earlier in CPL of the argument's class.
        // Parameters are weighed in the GF's argument-precedence-order (CLHS
        // 7.6.6.1.2); null = natural left-to-right order. Both methods
        // share the same owner GF, so reading it off `a` is sufficient.
        int n = Math.Min(a.Specializers.Length, b.Specializers.Length);
        int[]? apo = a.Owner?.ArgumentPrecedenceOrder;
        for (int k = 0; k < n; k++)
        {
            int i = (apo != null && k < apo.Length && apo[k] < n) ? apo[k] : k;
            if (ReferenceEquals(a.Specializers[i], b.Specializers[i])) continue;

            // EQL specializer is always more specific than a class specializer (CLHS 7.6.6.2)
            bool aIsEql = EqlSpecializerValue(a.Specializers[i]) != null;
            bool bIsEql = EqlSpecializerValue(b.Specializers[i]) != null;
            if (aIsEql && !bIsEql) return -1; // a (EQL) is more specific
            if (!aIsEql && bIsEql) return 1;  // b (EQL) is more specific
            if (aIsEql && bIsEql) continue;   // both EQL, move to next parameter

            if (a.Specializers[i] is LispClass clsA && b.Specializers[i] is LispClass clsB)
            {
                // Get CPL of actual argument's class (works for built-in types too)
                LispClass? argClass = null;
                if (i < args.Length)
                {
                    var classObj = ClassOf(args[i]);
                    if (classObj is LispClass lc2) argClass = lc2;
                }
                if (argClass != null)
                {
                    int ra = SpecializerRank(clsA, argClass);
                    int rb = SpecializerRank(clsB, argClass);
                    if (ra != rb) return ra < rb ? -1 : 1;
                    // Same rank: both got here through .NET variance, which the CPL
                    // cannot order.
                    if (ra != int.MaxValue)
                    {
                        int v = CompareByDotNetAssignability(clsA, clsB);
                        if (v != 0) return v;
                    }
                }
                // Fallback: compare by name (arbitrary but deterministic)
                return string.Compare(clsA.Name.Name, clsB.Name.Name, StringComparison.Ordinal);
            }
        }
        return 0;
    }
}
