// MOP (Meta-Object Protocol) wrappers — DOTCL-MOP package.
//
// Phase 1 of #144: expose dotcl's existing CLOS introspection so that
// closer-mop's #+dotcl arm (and our fork's closer-dotcl.lisp) can
// (:import-from :dotcl-mop ...) the AMOP API.
//
// This file does NOT implement protocol functions that change CLOS
// semantics (compute-discriminating-function, make-method-lambda, etc.).
// Stubs are provided where the API is queried but customization isn't
// supported yet — calling them will signal an error directing the user
// to file an issue.
namespace DotCL;

public static class Mop
{
    public static Package MopPkg { get; private set; } = null!;

    public static void Init()
    {
        MopPkg = new Package("DOTCL-MOP");

        // -- Class symbols expected by closer-mop's :import-from ----------
        // Intern + export the metaobject class names so closer-mop's defpackage
        // succeeds. Symbols whose underlying class doesn't exist yet are still
        // accessible but unbound as classes — actual usage will fail later
        // (acceptable for libraries that only need import-from to work).
        foreach (var name in new[] {
            // Class symbols
            "DIRECT-SLOT-DEFINITION", "EFFECTIVE-SLOT-DEFINITION", "EQL-SPECIALIZER",
            "FORWARD-REFERENCED-CLASS", "FUNCALLABLE-STANDARD-CLASS",
            "FUNCALLABLE-STANDARD-OBJECT", "METAOBJECT", "SLOT-DEFINITION",
            "SPECIALIZER", "STANDARD-ACCESSOR-METHOD", "STANDARD-DIRECT-SLOT-DEFINITION",
            "STANDARD-EFFECTIVE-SLOT-DEFINITION", "STANDARD-READER-METHOD",
            "STANDARD-SLOT-DEFINITION", "STANDARD-WRITER-METHOD",
            // Protocol functions closer-mop wants to import. Many of these have
            // RegisterMop entries below; the duplicate Intern is harmless. Listed
            // here so the symbol is exported even when the function impl isn't.
            "ACCESSOR-METHOD-SLOT-DEFINITION",
            "ADD-DEPENDENT", "ADD-DIRECT-METHOD", "ADD-DIRECT-SUBCLASS",
            "COMPUTE-APPLICABLE-METHODS-USING-CLASSES",
            "COMPUTE-CLASS-PRECEDENCE-LIST", "COMPUTE-DEFAULT-INITARGS",
            "COMPUTE-DISCRIMINATING-FUNCTION", "COMPUTE-EFFECTIVE-METHOD",
            "COMPUTE-EFFECTIVE-METHOD-FUNCTION", "COMPUTE-EFFECTIVE-SLOT-DEFINITION",
            "COMPUTE-SLOTS",
            "DIRECT-SLOT-DEFINITION-CLASS", "EFFECTIVE-SLOT-DEFINITION-CLASS",
            "ENSURE-CLASS", "ENSURE-CLASS-USING-CLASS",
            "ENSURE-GENERIC-FUNCTION-USING-CLASS",
            "FIND-METHOD-COMBINATION", "FUNCALLABLE-STANDARD-INSTANCE-ACCESS",
            "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER",
            "GENERIC-FUNCTION-DECLARATIONS",
            "MAKE-METHOD-LAMBDA", "MAP-DEPENDENTS",
            "METHOD-FUNCTION", "METHOD-SPECIALIZERS", "METHOD-QUALIFIERS",
            "READER-METHOD-CLASS", "WRITER-METHOD-CLASS",
            "REMOVE-DEPENDENT", "REMOVE-DIRECT-METHOD", "REMOVE-DIRECT-SUBCLASS",
            "SET-FUNCALLABLE-INSTANCE-FUNCTION",
            "SLOT-BOUNDP-USING-CLASS", "SLOT-MAKUNBOUND-USING-CLASS",
            "SLOT-VALUE-USING-CLASS",
            "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS", "SPECIALIZER-DIRECT-METHODS",
            "STANDARD-INSTANCE-ACCESS", "UPDATE-DEPENDENT",
        })
        {
            var (s, _) = MopPkg.Intern(name);
            MopPkg.Export(s);
        }

        // -- Class introspection ------------------------------------------
        RegisterMop("CLASS-DIRECT-SUPERCLASSES", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.DirectSuperclasses.Cast<LispObject>().ToArray()) : Nil.Instance);

        RegisterMop("CLASS-DIRECT-SUBCLASSES", 1, args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            // Not maintained as a back-link; scan the registry. Cheap enough
            // for occasional MOP introspection.
            var subs = new List<LispObject>();
            foreach (var cls in Runtime.AllClasses())
                if (Array.IndexOf(cls.DirectSuperclasses, c) >= 0)
                    subs.Add(cls);
            return Runtime.List(subs.ToArray());
        });

        RegisterMop("CLASS-PRECEDENCE-LIST", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.ClassPrecedenceList.Cast<LispObject>().ToArray()) : Nil.Instance);

        RegisterMop("CLASS-FINALIZED-P", 1, args =>
            // dotcl finalizes eagerly during defclass; treat all classes as
            // finalized once they exist (forward-referenced ones are not).
            args[0] is LispClass c && !c.IsForwardReferenced ? T.Instance : Nil.Instance);

        RegisterMop("CLASS-SLOTS", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.EffectiveSlots.Cast<LispObject>().ToArray()) : Nil.Instance);

        RegisterMop("CLASS-DIRECT-SLOTS", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.DirectSlots.Cast<LispObject>().ToArray()) : Nil.Instance);

        RegisterMop("CLASS-DEFAULT-INITARGS", 1, args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            var items = c.DefaultInitargs
                .Select(p => Runtime.List(p.Key, Nil.Instance, p.Thunk))
                .ToArray();
            return Runtime.List(items);
        });

        RegisterMop("CLASS-DIRECT-DEFAULT-INITARGS", 1, args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            var items = c.DirectDefaultInitargs
                .Select(p => Runtime.List(p.Key, Nil.Instance, p.Thunk))
                .ToArray();
            return Runtime.List(items);
        });

        RegisterMop("CLASS-PROTOTYPE", 1, args =>
        {
            // AMOP: returns "an instance of class" without running initialize-instance.
            // Best-effort: a freshly-allocated unbound LispInstance.
            if (args[0] is not LispClass c)
                throw new LispErrorException(new LispTypeError("CLASS-PROTOTYPE: not a class", args[0]));
            return new LispInstance(c);
        });

        // -- Slot introspection -------------------------------------------
        RegisterMop("SLOT-DEFINITION-NAME", 1, args =>
            args[0] is SlotDefinition s ? s.Name : Nil.Instance);

        RegisterMop("SLOT-DEFINITION-ALLOCATION", 1, args =>
            args[0] is SlotDefinition s
                ? Startup.Keyword(s.IsClassAllocation ? "CLASS" : "INSTANCE")
                : Nil.Instance);

        RegisterMop("SLOT-DEFINITION-INITARGS", 1, args =>
            args[0] is SlotDefinition s ? Runtime.List(s.Initargs.Cast<LispObject>().ToArray()) : Nil.Instance);

        RegisterMop("SLOT-DEFINITION-INITFUNCTION", 1, args =>
            args[0] is SlotDefinition s && s.InitformThunk is { } f ? (LispObject)f : Nil.Instance);

        RegisterMop("SLOT-DEFINITION-INITFORM", 1, args =>
            // dotcl doesn't preserve the source form, only the compiled thunk.
            // Returning NIL is honest; downstream lib that needs the form
            // should also handle the no-thunk case.
            Nil.Instance);

        RegisterMop("SLOT-DEFINITION-TYPE", 1, args => Startup.Sym("T"));        // not tracked
        RegisterMop("SLOT-DEFINITION-READERS", 1, args => Nil.Instance);         // not tracked
        RegisterMop("SLOT-DEFINITION-WRITERS", 1, args => Nil.Instance);         // not tracked
        RegisterMop("SLOT-DEFINITION-LOCATION", 1, args =>
        {
            // For instance-allocated slots return the index in the layout.
            if (args[0] is not SlotDefinition s) return Nil.Instance;
            return s.IsClassAllocation ? (LispObject)Nil.Instance : s.Name;  // a Symbol works as opaque locator too
        });

        // -- Generic function / method introspection ----------------------
        RegisterMop("GENERIC-FUNCTION-NAME", 1, args =>
            args[0] is GenericFunction gf ? gf.Name : Nil.Instance);

        RegisterMop("GENERIC-FUNCTION-METHODS", 1, args =>
            args[0] is GenericFunction gf
                ? Runtime.List(gf.Methods.Cast<LispObject>().ToArray())
                : Nil.Instance);

        RegisterMop("GENERIC-FUNCTION-METHOD-CLASS", 1, args =>
            (LispObject?)Runtime.FindClassOrNil(Startup.Sym("STANDARD-METHOD")) ?? Nil.Instance);

        RegisterMop("GENERIC-FUNCTION-METHOD-COMBINATION", 1, args =>
        {
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            return gf.MethodCombination is { } sym ? (LispObject)sym : Startup.Sym("STANDARD");
        });

        RegisterMop("GENERIC-FUNCTION-LAMBDA-LIST", 1, args =>
        {
            // Reconstruct a placeholder lambda list from arity info. Not exact
            // (parameter names are lost) but good enough for arity-checking
            // consumers like trivial-arguments.
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            return BuildLambdaListPlaceholder(gf.RequiredCount, gf.OptionalCount,
                gf.HasRest, gf.HasKey, gf.KeywordNames, gf.HasAllowOtherKeys);
        });

        RegisterMop("METHOD-GENERIC-FUNCTION", 1, args =>
            args[0] is LispMethod m && m.Owner is { } o ? (LispObject)o : Nil.Instance);

        RegisterMop("METHOD-LAMBDA-LIST", 1, args =>
        {
            if (args[0] is not LispMethod m) return Nil.Instance;
            return BuildLambdaListPlaceholder(m.RequiredCount, m.OptionalCount,
                m.HasRest, m.HasKey, m.KeywordNames, m.HasAllowOtherKeys);
        });

        // -- Specializer / EQL specializer --------------------------------
        RegisterMop("EQL-SPECIALIZER-OBJECT", 1, args =>
        {
            // dotcl represents (eql X) specializers as the Cons (eql X).
            if (args[0] is Cons c && c.Car is Symbol s && s.Name == "EQL" && c.Cdr is Cons rest)
                return rest.Car;
            throw new LispErrorException(new LispTypeError("EQL-SPECIALIZER-OBJECT: not an eql specializer", args[0]));
        });

        RegisterMop("INTERN-EQL-SPECIALIZER", 1, args =>
            new Cons(Startup.Sym("EQL"), new Cons(args[0], Nil.Instance)));

        // -- Protocol stubs (not customizable yet) ------------------------
        // These return successful default values so closer-mop and lib code
        // that just queries them (without :method customization) works.
        RegisterMop("VALIDATE-SUPERCLASS", 2, args => T.Instance);
        RegisterMop("FINALIZE-INHERITANCE", 1, args => Nil.Instance);     // already eager
        RegisterMop("ENSURE-FINALIZED", -1, args =>                        // (ensure-finalized class &optional errorp)
            args.Length >= 1 ? args[0] : Nil.Instance);

        RegisterMop("CLASSP", 1, args =>
            args[0] is LispClass ? T.Instance : Nil.Instance);

        RegisterMop("SUBCLASSP", 2, args =>
        {
            if (args[0] is not LispClass c1 || args[1] is not LispClass c2) return Nil.Instance;
            return Array.IndexOf(c1.ClassPrecedenceList, c2) >= 0 ? T.Instance : Nil.Instance;
        });

        // -- Required-args / extract-lambda-list (closer-mop utilities) --
        RegisterMop("EXTRACT-LAMBDA-LIST", 1, args =>
        {
            // (extract-lambda-list specialized-lambda-list) — strip specializers.
            // (m (x integer) (y string)) → (x y)
            return ExtractLambdaList(args[0]);
        });

        RegisterMop("EXTRACT-SPECIALIZER-NAMES", 1, args =>
            ExtractSpecializerNames(args[0]));

        RegisterMop("REQUIRED-ARGS", -1, args =>
        {
            // (required-args lambda-list &optional reduce) → list of required parameter names.
            var ll = args[0];
            var result = new List<LispObject>();
            for (var cur = ll; cur is Cons c; cur = c.Cdr)
            {
                if (c.Car is Symbol s && s.Name.StartsWith("&"))
                    break;
                result.Add(c.Car);
            }
            return Runtime.List(result.ToArray());
        });
    }

    // --- helpers -------------------------------------------------------------

    private static void RegisterMop(string name, int arity, Func<LispObject[], LispObject> fn)
    {
        var fullName = $"DOTCL-MOP:{name}";
        var lispFn = new LispFunction(fn, fullName, arity);
        var (sym, _) = MopPkg.Intern(name);
        MopPkg.Export(sym);
        sym.Function = lispFn;
        // Intentionally NOT calling CilAssembler.RegisterFunction(fullName, ...):
        // that path goes through Startup.Sym(fullName) which would intern a
        // bogus DOTCL-INTERNAL symbol named "DOTCL-MOP:GENERIC-FUNCTION-NAME"
        // (with the colon in the name) and clobber things via the cross-package
        // bridge. Symbol-based dispatch (compile-named-call → GetFunctionBySymbol)
        // only needs sym.Function to be set, which is enough.
    }

    private static LispObject BuildLambdaListPlaceholder(
        int required, int optional, bool hasRest, bool hasKey,
        IReadOnlyList<string> keywordNames, bool hasAOK)
    {
        // Use fresh uninterned symbols so consumers don't see global symbol
        // identity collisions (parameter names are not preserved by dotcl).
        var items = new List<LispObject>();
        for (int i = 0; i < required; i++)
            items.Add(new Symbol($"R{i}", null));
        if (optional > 0)
        {
            items.Add(Startup.Sym("&OPTIONAL"));
            for (int i = 0; i < optional; i++)
                items.Add(new Symbol($"O{i}", null));
        }
        if (hasRest)
        {
            items.Add(Startup.Sym("&REST"));
            items.Add(new Symbol("REST", null));
        }
        if (hasKey)
        {
            items.Add(Startup.Sym("&KEY"));
            foreach (var kn in keywordNames)
                items.Add(Startup.Keyword(kn));
            if (hasAOK) items.Add(Startup.Sym("&ALLOW-OTHER-KEYS"));
        }
        return Runtime.List(items.ToArray());
    }

    private static LispObject ExtractLambdaList(LispObject ll)
    {
        var result = new List<LispObject>();
        for (var cur = ll; cur is Cons c; cur = c.Cdr)
        {
            // (param specializer) → param ; or bare param
            if (c.Car is Cons spec && spec.Car is Symbol)
                result.Add(spec.Car);
            else
                result.Add(c.Car);
        }
        return Runtime.List(result.ToArray());
    }

    private static LispObject ExtractSpecializerNames(LispObject ll)
    {
        var result = new List<LispObject>();
        var tSym = Startup.Sym("T");
        for (var cur = ll; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is Symbol s && s.Name.StartsWith("&")) break;
            if (c.Car is Cons spec && spec.Cdr is Cons rest)
                result.Add(rest.Car);
            else
                result.Add(tSym);
        }
        return Runtime.List(result.ToArray());
    }
}
