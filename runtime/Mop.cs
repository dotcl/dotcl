// MOP (Meta-Object Protocol) wrappers — DOTCL-MOP package.
//
// Expose dotcl's existing CLOS introspection so that
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
        RegisterMop("CLASS-DIRECT-SUPERCLASSES", 1, args => Runtime.ClassDirectSuperclasses(args[0]));

        RegisterMop("CLASS-DIRECT-SUBCLASSES", 1, args => Runtime.ClassDirectSubclasses(args[0]));

        RegisterMop("CLASS-PRECEDENCE-LIST", 1, args => Runtime.ClassPrecedenceListOf(args[0]));

        RegisterMop("CLASS-FINALIZED-P", 1, args => Runtime.ClassFinalizedP(args[0]));

        RegisterMop("CLASS-SLOTS", 1, args => Runtime.ClassSlots(args[0]));

        RegisterMop("CLASS-DIRECT-SLOTS", 1, args => Runtime.ClassDirectSlots(args[0]));

        RegisterMop("CLASS-DEFAULT-INITARGS", 1, args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            var items = c.DefaultInitargs
                .Select(p => Runtime.List(p.Key, p.Form, p.Thunk))
                .ToArray();
            return Runtime.List(items);
        });

        RegisterMop("CLASS-DIRECT-DEFAULT-INITARGS", 1, args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            var items = c.DirectDefaultInitargs
                .Select(p => Runtime.List(p.Key, p.Form, p.Thunk))
                .ToArray();
            return Runtime.List(items);
        });

        // AMOP: "an instance of class" without running initialize-instance, and the
        // SAME instance every call (memoized on the class) — EQL-method dispatch
        // (e.g. McCLIM define-presentation-method) relies on that identity.
        RegisterMop("CLASS-PROTOTYPE", 1, args => Runtime.ClassPrototypeOf(args[0]));

        // -- Slot introspection -------------------------------------------
        RegisterMop("SLOT-DEFINITION-NAME", 1, args => Runtime.SlotDefinitionName(args[0]));

        RegisterMop("SLOT-DEFINITION-ALLOCATION", 1, args => Runtime.SlotDefinitionAllocation(args[0]));

        RegisterMop("SLOT-DEFINITION-INITARGS", 1, args => Runtime.SlotDefinitionInitargs(args[0]));

        RegisterMop("SLOT-DEFINITION-INITFUNCTION", 1, args => Runtime.SlotDefinitionInitfunction(args[0]));

        RegisterMop("SLOT-DEFINITION-INITFORM", 1, args => Runtime.SlotDefinitionInitform(args[0]));

        RegisterMop("SLOT-DEFINITION-TYPE", 1, args => Runtime.SlotDefinitionType(args[0]));
        RegisterMop("SLOT-DEFINITION-READERS", 1, args => Runtime.SlotDefinitionReaders(args[0]));
        RegisterMop("SLOT-DEFINITION-WRITERS", 1, args => Runtime.SlotDefinitionWriters(args[0]));
        RegisterMop("SLOT-DEFINITION-LOCATION", 1, args =>
        {
            // AMOP: a non-negative integer index into the instance layout for
            // instance-allocated slots; NIL for :class allocation.
            if (args[0] is not SlotDefinition s) return Nil.Instance;
            return s.Location >= 0 ? (LispObject)Fixnum.Make(s.Location) : Nil.Instance;
        });

        // STANDARD-INSTANCE-ACCESS (instance location): read the slot at the given
        // integer layout index directly, bypassing slot-value-using-class.
        //
        // FUNCALLABLE-STANDARD-INSTANCE-ACCESS is the same access on dotcl: an
        // instance of a FUNCALLABLE-STANDARD-CLASS class is an ordinary
        // LispInstance with the same slot vector, the metaclass being recognised
        // without the instance being callable yet. The pair is registered on one
        // implementation so the two cannot drift; only the name in the error
        // differs, so a caller is told the function they actually called.
        RegisterMop("STANDARD-INSTANCE-ACCESS", 2,
            args => InstanceSlotRead("STANDARD-INSTANCE-ACCESS", args[0], args[1]));
        RegisterMop("FUNCALLABLE-STANDARD-INSTANCE-ACCESS", 2,
            args => InstanceSlotRead("FUNCALLABLE-STANDARD-INSTANCE-ACCESS", args[0], args[1]));
        // (setf (standard-instance-access instance location) new-value)
        // setf-function arg order: (new-value instance location)
        foreach (var accessor in new[] { "STANDARD-INSTANCE-ACCESS",
                                         "FUNCALLABLE-STANDARD-INSTANCE-ACCESS" })
        {
            var (accessorSym, _) = MopPkg.Intern(accessor);
            var who = $"(SETF {accessor})";
            accessorSym.SetfFunction = new LispFunction(
                args => InstanceSlotWrite(who, args[1], args[2], args[0]),
                $"(SETF DOTCL-MOP:{accessor})", 3);
        }

        // -- Generic function / method introspection ----------------------
        RegisterMop("GENERIC-FUNCTION-NAME", 1, args => Runtime.GenericFunctionName(args[0]));

        RegisterMop("GENERIC-FUNCTION-METHODS", 1, args => Runtime.GenericFunctionMethods(args[0]));

        // (setf (generic-function-name gf) new-name): AMOP defines this as
        // reinitialization, not a field write, so that a dependent or a
        // reinitialize-instance method sees it. Going through the generic function
        // also means the discriminating function is recomputed the same way it is
        // for any other reinitialization.
        {
            var (nameSym, _) = MopPkg.Intern("GENERIC-FUNCTION-NAME");
            nameSym.SetfFunction = new LispFunction(args =>
            {
                // setf-function arg order: (new-value generic-function)
                if (args[1] is not GenericFunction target)
                    throw new LispErrorException(new LispTypeError(
                        "(SETF GENERIC-FUNCTION-NAME): not a generic function", args[1]));
                if (Startup.Sym("REINITIALIZE-INSTANCE").Function is LispFunction reinit)
                    reinit.Invoke(new LispObject[] { target, Startup.Keyword("NAME"), args[0] });
                return args[0];
            }, "(SETF DOTCL-MOP:GENERIC-FUNCTION-NAME)", 2);
        }

        RegisterMopGF("GENERIC-FUNCTION-METHOD-CLASS", 1,
            new LispClass[] { (LispClass)Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION")) },
            args => (LispObject?)Runtime.FindClassOrNil(Startup.Sym("STANDARD-METHOD"))
                    ?? Nil.Instance);

        // DEFMETHOD asks the generic function what class its methods are. The hook
        // stays quiet until somebody has specialised the protocol, so an ordinary
        // DEFMETHOD does not pay a generic-function call per method.
        {
            var methodClassGf = (GenericFunction)MopPkg.Intern("GENERIC-FUNCTION-METHOD-CLASS").Item1.Function!;
            Runtime.MethodClassHook = gf =>
            {
                if (methodClassGf.Methods.Count <= 1) return null;
                return MultipleValues.Primary(methodClassGf.Invoke(new LispObject[] { gf }))
                       as LispClass;
            };
        }

        RegisterMop("GENERIC-FUNCTION-METHOD-COMBINATION", 1, args =>
        {
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            return gf.MethodCombination is { } sym ? (LispObject)sym : Startup.Sym("STANDARD");
        });

        RegisterMop("GENERIC-FUNCTION-DECLARATIONS", 1, args =>
            args[0] is GenericFunction gfd ? gfd.Declarations : Nil.Instance);

        RegisterMop("GENERIC-FUNCTION-LAMBDA-LIST", 1, args =>
        {
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            if (gf.StoredLambdaList != null) return gf.StoredLambdaList;
            // Reconstruct a placeholder lambda list from arity info when no stored
            // lambda-list, and keep it: rebuilding per call handed out a different
            // list of fresh uninterned symbols every time.
            return gf.PlaceholderLambdaList ??= BuildLambdaListPlaceholder(
                gf.RequiredCount, gf.OptionalCount,
                gf.HasRest, gf.HasKey, gf.KeywordNames, gf.HasAllowOtherKeys);
        });

        RegisterMop("GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER", 1, args =>
        {
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            // Collect required-parameter names from stored lambda-list
            var ll = gf.StoredLambdaList ?? (gf.PlaceholderLambdaList ??= BuildLambdaListPlaceholder(gf.RequiredCount,
                gf.OptionalCount, gf.HasRest, gf.HasKey, gf.KeywordNames, gf.HasAllowOtherKeys));
            LispObject result = Nil.Instance;
            var reqNames = new List<LispObject>();
            var cur = ll;
            while (cur is Cons c)
            {
                if (c.Car is Symbol sym && sym.Name[0] != '&') reqNames.Add(sym);
                else break;
                cur = c.Cdr;
            }
            // Return in argument-precedence-order when declared, else declaration order.
            int[]? apo = gf.ArgumentPrecedenceOrder;
            if (apo != null)
            {
                for (int k = apo.Length - 1; k >= 0; k--)
                    if (apo[k] >= 0 && apo[k] < reqNames.Count)
                        result = new Cons(reqNames[apo[k]], result);
            }
            else
            {
                for (int i = reqNames.Count - 1; i >= 0; i--)
                    result = new Cons(reqNames[i], result);
            }
            return result;
        });

        RegisterMop("METHOD-GENERIC-FUNCTION", 1, args => Runtime.MethodGenericFunction(args[0]));

        RegisterMop("METHOD-LAMBDA-LIST", 1, args => Runtime.MethodLambdaList(args[0]));

        // -- Specializer / EQL specializer --------------------------------
        RegisterMop("EQL-SPECIALIZER-OBJECT", 1, args =>
            Runtime.EqlSpecializerValue(args[0])
            ?? throw new LispErrorException(new LispTypeError(
                   "EQL-SPECIALIZER-OBJECT: not an eql specializer", args[0])));

        RegisterMop("INTERN-EQL-SPECIALIZER", 1, args => Runtime.InternEqlSpecializer(args[0]));

        // ACCESSOR-METHOD-SLOT-DEFINITION (method) → the slot-definition the accessor
        // reads/writes. DEFCLASS tags reader/writer/accessor methods with their slot
        // via %REGISTER-ACCESSOR-METHOD; ordinary methods return NIL.
        RegisterMop("ACCESSOR-METHOD-SLOT-DEFINITION", 1, args =>
            args[0] is LispMethod m && m.AccessorSlot is { } sd ? (LispObject)sd : Nil.Instance);

        // METHOD-FUNCTION (method) → the method's function object. AMOP: the function
        // called with (args next-methods); dotcl's method Function already follows the
        // call-method calling convention used by the dispatcher.
        // AMOP shape: (args next-methods). Runtime.MethodFunction hands out the view
        // over dotcl's spread-argument one, or the object a user passed with :FUNCTION.
        RegisterMop("METHOD-FUNCTION", 1, args =>
            args[0] is LispMethod ? Runtime.MethodFunction(args[0]) : Nil.Instance);

        // READER-METHOD-CLASS / WRITER-METHOD-CLASS (class slotd &rest initargs) →
        // the class of accessor methods. dotcl creates plain standard methods for
        // accessors, so the metaobject class is STANDARD-METHOD.
        // Generic, so a metaclass can answer with an accessor method class of its own.
        // dotcl builds plain standard methods, which is what the default answers.
        {
            var classCls0 = (LispClass)Runtime.FindClass(Startup.Sym("CLASS"));
            var anyCls0 = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("READER-METHOD-CLASS", -1, new LispClass[] { classCls0, anyCls0 },
                args => (LispObject?)Runtime.FindClassOrNil(Startup.Sym("STANDARD-METHOD"))
                        ?? Nil.Instance);
            RegisterMopGF("WRITER-METHOD-CLASS", -1, new LispClass[] { classCls0, anyCls0 },
                args => (LispObject?)Runtime.FindClassOrNil(Startup.Sym("STANDARD-METHOD"))
                        ?? Nil.Instance);
        }

        // SPECIALIZER-DIRECT-METHODS (specializer) → every method that has SPECIALIZER
        // in its specializer list. No back-link is kept, so scan all GFs' methods.
        // Specializers are LispClass (eq) or (eql X) conses (structural compare).
        RegisterMop("SPECIALIZER-DIRECT-METHODS", 1, args =>
        {
            var spec = args[0];
            var found = new List<LispObject>();
            foreach (var gf in Runtime.AllGenericFunctions())
                foreach (var m in gf.Methods)
                    if (SpecializerListContains(m.Specializers, spec))
                        found.Add(m);
            return Runtime.List(found.ToArray());
        });

        // SPECIALIZER-DIRECT-GENERIC-FUNCTIONS (specializer) → every GF that has a
        // method specialized on SPECIALIZER (each GF at most once).
        RegisterMop("SPECIALIZER-DIRECT-GENERIC-FUNCTIONS", 1, args =>
        {
            var spec = args[0];
            var found = new List<LispObject>();
            foreach (var gf in Runtime.AllGenericFunctions())
                foreach (var m in gf.Methods)
                    if (SpecializerListContains(m.Specializers, spec))
                    {
                        found.Add(gf);
                        break;
                    }
            return Runtime.List(found.ToArray());
        });

        // COMPUTE-* introspection: dotcl finalizes classes eagerly, so the "computed"
        // result is the already-stored effective value. Exposed as plain functions so
        // closer-mop callers (which expect SBCL-style availability) resolve them.
        RegisterMopGF("COMPUTE-SLOTS", 1,
            new LispClass[] { (LispClass)Runtime.FindClass(Startup.Sym("CLASS")) },
            args => args[0] is LispClass c
                ? Runtime.List(c.EffectiveSlots.Cast<LispObject>().ToArray())
                : Nil.Instance);

        // Finalization asks COMPUTE-SLOTS for the effective slots. The default method
        // reports what the class already computed, so specialising it is how a
        // metaclass reorders, adds or drops slots.
        //
        // The gate is per class, not per image: the question is whether a method other
        // than the default applies to THIS class, so one metaclass specialising the
        // protocol does not put every DEFCLASS in the image through a generic-function
        // call for an answer it just produced itself.
        {
            var computeSlotsGf = (GenericFunction)MopPkg.Intern("COMPUTE-SLOTS").Item1.Function!;
            var computeSlotsDefault = computeSlotsGf.Methods.Count > 0
                                      ? computeSlotsGf.Methods[0] : null;
            Runtime.ComputeSlotsHook = cls =>
            {
                var methods = computeSlotsGf.Methods;
                if (methods.Count <= 1) return null;
                var meta = Runtime.ClassOf(cls) as LispClass;
                if (meta == null) return null;
                bool customized = false;
                foreach (var m in methods)
                {
                    if (ReferenceEquals(m, computeSlotsDefault)) continue;
                    if (m.Specializers.Length > 0 && m.Specializers[0] is LispClass spec
                        && Array.IndexOf(meta.ClassPrecedenceList, spec) >= 0)
                    { customized = true; break; }
                }
                if (!customized) return null;
                var answer = MultipleValues.Primary(
                    computeSlotsGf.Invoke(new LispObject[] { cls }));
                var slots = new List<SlotDefinition>();
                for (var c = answer; c is Cons cc; c = cc.Cdr)
                {
                    // Anything that is not a slot definition means the method answered
                    // with something this cannot lay out; keep the class's own list
                    // rather than build a layout nobody can read.
                    if (cc.Car is not SlotDefinition sd) return null;
                    slots.Add(sd);
                }
                return slots.ToArray();
            };
        }

        RegisterMop("COMPUTE-CLASS-PRECEDENCE-LIST", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.ClassPrecedenceList.Cast<LispObject>().ToArray()) : Nil.Instance);

        RegisterMopGF("COMPUTE-DEFAULT-INITARGS", 1,
            new LispClass[] { (LispClass)Runtime.FindClass(Startup.Sym("CLASS")) },
            args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            var items = c.DefaultInitargs
                .Select(p => Runtime.List(p.Key, p.Form, p.Thunk))
                .ToArray();
            return Runtime.List(items);
        });

        // COMPUTE-APPLICABLE-METHODS-USING-CLASSES (gf classes) → (values methods
        // definitive-p). definitive-p is NIL when applicability depends on an EQL
        // specializer (undecidable from a class), so the caller falls back to
        // COMPUTE-APPLICABLE-METHODS on the actual arguments.
        RegisterMopGF("COMPUTE-APPLICABLE-METHODS-USING-CLASSES", 2,
            new LispClass[] { (LispClass)Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION")),
                              (LispClass)Runtime.FindClass(Startup.Sym("T")) },
            args => Runtime.ComputeApplicableMethodsUsingClasses(args[0], args[1]));

        // MAKE-METHOD-LAMBDA (gf method lambda-expression environment): AMOP has
        // DEFMETHOD go through this, and portable metaobject code specialises it to
        // wrap method bodies. dotcl's version hands the lambda expression back
        // unchanged, which is what it always did -- the point of the change is that
        // it is now a generic function, so specialising it is possible at all.
        //
        // The same function object is installed on the DOTCL-INTERNAL symbol of the
        // same name, which held the earlier flat registration: one name, one
        // behaviour.
        {
            var gfCls4 = (LispClass)Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
            var anyCls4 = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("MAKE-METHOD-LAMBDA", 4,
                new LispClass[] { gfCls4, anyCls4, anyCls4, anyCls4 },
                args => args.Length > 2 ? args[2] : Nil.Instance);
            var mopMml = MopPkg.Intern("MAKE-METHOD-LAMBDA").Item1;
            var (internalMml, internalStatus) = Startup.Internal.FindSymbol("MAKE-METHOD-LAMBDA");
            if (internalStatus != SymbolStatus.None && mopMml.Function is LispFunction mmlFn)
                internalMml.Function = mmlFn;

            // DEFMETHOD runs its method lambda through MAKE-METHOD-LAMBDA and compiles
            // what comes back, which is how a generic function class wraps method
            // bodies. Quiet until somebody has specialised it: the default method hands
            // the form back unchanged, so calling it for every DEFMETHOD would be pure
            // cost. The lambda handed over is dotcl's own, with the arguments spread --
            // METHOD-FUNCTION converts at the boundary, and that is where the AMOP shape
            // (arguments list, next methods) is produced.
            Runtime.MakeMethodLambdaHook = (gf, form) =>
            {
                if (mopMml.Function is not GenericFunction mmlGf || mmlGf.Methods.Count <= 1)
                    return null;
                LispObject prototype = Nil.Instance;
                if (Runtime.MethodClassHook?.Invoke(gf) is { IsBuiltIn: false } methodClass)
                    prototype = methodClass.Prototype;
                var answer = MultipleValues.Primary(
                    mmlGf.Invoke(new LispObject[] { gf, prototype, form, Nil.Instance }));
                return answer is Cons c && ReferenceEquals(c.Car, Startup.Sym("LAMBDA"))
                       ? answer : null;
            };
        }

        // COMPUTE-DISCRIMINATING-FUNCTION (gf): AMOP has the generic function call
        // whatever this returns. The default method hands back dotcl's own dispatch
        // as a function object; a user method specializing on its own generic
        // function class replaces it, and the answer is installed the way
        // SET-FUNCALLABLE-INSTANCE-FUNCTION installs one.
        //
        // The install only happens once someone HAS specialized it (method count
        // above the default one). Otherwise every generic function in the image
        // would end up wrapped in a closure that only calls the dispatcher it
        // already had, losing the arity fast paths for nothing.
        {
            var gfCls3 = (LispClass)Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
            RegisterMopGF("COMPUTE-DISCRIMINATING-FUNCTION", 1, new LispClass[] { gfCls3 },
                args => args[0] is GenericFunction gf
                    ? (LispObject)Runtime.StandardDiscriminatingFunction(gf)
                    : Nil.Instance);
            var computeDiscriminating =
                (GenericFunction)MopPkg.Intern("COMPUTE-DISCRIMINATING-FUNCTION").Item1.Function!;
            var defaultMethod = computeDiscriminating.Methods[0];
            Runtime.DiscriminatingFunctionHook = target =>
            {
                if (computeDiscriminating.Methods.Count <= 1) return;
                // The protocol generic function dispatches through the very
                // mechanism being installed here; leaving it alone keeps a method on
                // it from having to run in order to compute its own dispatch.
                if (ReferenceEquals(target, computeDiscriminating)) return;
                // Whether a method exists is not the question -- whether one applies
                // to THIS generic function is. Asking the cheap question instead put
                // every generic function defined after the first specialisation into
                // a closure that only called the dispatcher it already had, which
                // costs the arity fast paths and one argument array a call.
                var applicable = Runtime.ComputeApplicableMethods(
                    computeDiscriminating, Runtime.List(target));
                if (applicable is not Cons applicableList
                    || ReferenceEquals(applicableList.Car, defaultMethod))
                    return;
                if (MultipleValues.Primary(computeDiscriminating.Invoke(new LispObject[] { target }))
                    is LispFunction computed)
                    target.DispatchFunction = computed;
            };
        }

        // COMPUTE-EFFECTIVE-METHOD (gf method-combination methods) → the effective
        // method form for the STANDARD method combination (CLHS 7.6.6.2). METHODS is
        // the applicable list most-specific-first. Non-standard combinations are not
        // synthesized here (dotcl's own dispatcher handles them internally).
        RegisterMopGF("COMPUTE-EFFECTIVE-METHOD", 3,
            new LispClass[] { (LispClass)Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION")),
                              (LispClass)Runtime.FindClass(Startup.Sym("T")),
                              (LispClass)Runtime.FindClass(Startup.Sym("T")) },
            args =>
        {
            var comb = args[1];
            // Standard combination is named STANDARD or passed as NIL.
            if (comb is Symbol cs && cs.Name != "STANDARD" && cs.Name != "NIL")
                throw new LispErrorException(new LispError(
                    $"COMPUTE-EFFECTIVE-METHOD: only the STANDARD method combination is supported, got {cs.Name}"));
            return Runtime.ComputeEffectiveMethodStandard(args[2]);
        });

        // ENSURE-CLASS-USING-CLASS (class name &rest initargs): the functional core of
        // DEFCLASS. dotcl's ENSURE-CLASS already creates-or-reinitializes by name
        // (RegisterClass copies into an existing/forward-ref class), so the class arg
        // (nil when not yet defined) is informational — delegate by name + initargs.
        RegisterMop("ENSURE-CLASS-USING-CLASS", -1, args =>
        {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError(
                    "ENSURE-CLASS-USING-CLASS: requires class and name"));
            if (Startup.Sym("ENSURE-CLASS").Function is not LispFunction ec)
                throw new LispErrorException(new LispProgramError("ENSURE-CLASS unavailable"));
            var rest = new LispObject[args.Length - 1];      // name + initargs
            Array.Copy(args, 1, rest, 0, args.Length - 1);
            return ec.Invoke(rest);
        });

        // ENSURE-GENERIC-FUNCTION-USING-CLASS (gf name &rest initargs): functional core
        // of DEFGENERIC. ENSURE-GENERIC-FUNCTION already reinitializes an existing GF
        // (looked up by name via fboundp), so the gf arg is informational.
        RegisterMop("ENSURE-GENERIC-FUNCTION-USING-CLASS", -1, args =>
        {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError(
                    "ENSURE-GENERIC-FUNCTION-USING-CLASS: requires gf and name"));
            if (Startup.Sym("ENSURE-GENERIC-FUNCTION").Function is not LispFunction egf)
                throw new LispErrorException(new LispProgramError("ENSURE-GENERIC-FUNCTION unavailable"));
            var rest = new LispObject[args.Length - 1];      // name + initargs
            Array.Copy(args, 1, rest, 0, args.Length - 1);
            return egf.Invoke(rest);
        });

        // -- Protocol GFs (extensible via defmethod) ----------------------
        // VALIDATE-SUPERCLASS: default = standard-class/standard-class → T, else NIL
        {
            var cls2     = (LispClass)Runtime.FindClass(Startup.Sym("CLASS"));
            var stdCls2  = (LispClass)Runtime.FindClass(Startup.Sym("STANDARD-CLASS"));
            var builtIn2 = (LispClass)Runtime.FindClass(Startup.Sym("BUILT-IN-CLASS"));
            // (class class) → NIL  (least-specific default)
            RegisterMopGF("VALIDATE-SUPERCLASS", 2,
                new LispClass[] { cls2, cls2 }, args => Nil.Instance);
            // (standard-class standard-class) → T
            RegisterMopGFMethod("VALIDATE-SUPERCLASS",
                new LispClass[] { stdCls2, stdCls2 }, args => T.Instance);
        }
        // FINALIZE-INHERITANCE: dotcl finalizes eagerly; default method is a no-op
        RegisterMopGF("FINALIZE-INHERITANCE", 1,
            new LispClass[] { (LispClass)Runtime.FindClass(Startup.Sym("CLASS")) },
            args => Nil.Instance);

        // -- Slot-definition-class protocol (AMOP) ------------------------
        // These drive custom slot-definition classes (e.g. McCLIM's class-with-dynamic-slots).
        // Defaults specialize on CLASS so user metaclasses override via more-specific methods.
        // Only invoked for classes with a custom metaclass (see Runtime.MakeClassCore /
        // LispClass.ComputeEffectiveSlots), so standard CLOS is untouched.
        {
            var classCls = (LispClass)Runtime.FindClass(Startup.Sym("CLASS"));
            var tCls = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            var stdDirect = (LispClass)Runtime.FindClass(Startup.Sym("STANDARD-DIRECT-SLOT-DEFINITION"));
            var stdEffective = (LispClass)Runtime.FindClass(Startup.Sym("STANDARD-EFFECTIVE-SLOT-DEFINITION"));

            // DIRECT-SLOT-DEFINITION-CLASS (class &rest initargs) → STANDARD-DIRECT-SLOT-DEFINITION
            RegisterMopGF("DIRECT-SLOT-DEFINITION-CLASS", -1,
                new LispClass[] { classCls }, args => stdDirect);
            // EFFECTIVE-SLOT-DEFINITION-CLASS (class &rest initargs) → STANDARD-EFFECTIVE-SLOT-DEFINITION
            RegisterMopGF("EFFECTIVE-SLOT-DEFINITION-CLASS", -1,
                new LispClass[] { classCls }, args => stdEffective);
            // COMPUTE-EFFECTIVE-SLOT-DEFINITION (class name direct-slot-definitions)
            // The default builds the standard merged effective slot, then consults
            // EFFECTIVE-SLOT-DEFINITION-CLASS; a non-standard result becomes the slotd's
            // MetaClass and its extra Lisp slots are initialized (their initforms run inside
            // any dynamic binding a user method established before call-next-method).
            RegisterMopGF("COMPUTE-EFFECTIVE-SLOT-DEFINITION", 3,
                new LispClass[] { classCls, tCls, tCls }, args =>
            {
                if (args[0] is not LispClass cls)
                    throw new LispErrorException(new LispTypeError(
                        "COMPUTE-EFFECTIVE-SLOT-DEFINITION: not a class", args[0]));
                var name = args[1] as Symbol ?? Startup.Sym(args[1].ToString() ?? "NIL");
                var defs = new List<SlotDefinition>();
                for (var c = args[2]; c is Cons cc; c = cc.Cdr)
                    if (cc.Car is SlotDefinition sd) defs.Add(sd);
                var eslot = LispClass.BuildEffectiveSlot(name, defs);

                if (Startup.Sym("EFFECTIVE-SLOT-DEFINITION-CLASS").Function is LispFunction esdcFn)
                {
                    // AMOP passes the slot's initargs here, and :allocation is the one
                    // a metaclass dispatches on to give the slot a class of its own.
                    var effClass = esdcFn.Invoke(new LispObject[] {
                        cls, Startup.Keyword("NAME"), name,
                        Startup.Keyword("ALLOCATION"), eslot.AllocationKeyword,
                        Startup.Keyword("DOCUMENTATION"), eslot.Documentation });
                    if (effClass is LispClass ec && !ReferenceEquals(ec, stdEffective))
                    {
                        eslot.MetaClass = ec;
                        Runtime.InitializeSlotdExtraSlots(eslot, ec, null);
                    }
                }
                return eslot;
            });
        }
        // ADD-DIRECT-METHOD / REMOVE-DIRECT-METHOD: AMOP has ADD-METHOD and
        // REMOVE-METHOD call these once per specializer, and the default methods
        // record the link that SPECIALIZER-DIRECT-METHODS reports. dotcl keeps no
        // such link -- SPECIALIZER-DIRECT-METHODS scans every generic function --
        // so the default methods have nothing to record and the value of the
        // protocol here is the call: a portable metaobject class can specialize
        // them and keep its own registry.
        //
        // The hook only dispatches once someone HAS specialized them (method count
        // above the default one). Otherwise every DEFMETHOD in the image would pay
        // one generic-function call per specializer to reach a no-op.
        {
            var tCls = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("ADD-DIRECT-METHOD", 2, new LispClass[] { tCls, tCls },
                args => Nil.Instance);
            RegisterMopGF("REMOVE-DIRECT-METHOD", 2, new LispClass[] { tCls, tCls },
                args => Nil.Instance);
            var addDirect = (GenericFunction)MopPkg.Intern("ADD-DIRECT-METHOD").Item1.Function!;
            var removeDirect = (GenericFunction)MopPkg.Intern("REMOVE-DIRECT-METHOD").Item1.Function!;
            Runtime.DirectMethodHook = (specializer, method, adding) =>
            {
                var protocol = adding ? addDirect : removeDirect;
                if (protocol.Methods.Count <= 1) return;
                protocol.Invoke(new LispObject[] { specializer, method });
            };
        }

        // COMPUTE-EFFECTIVE-METHOD-FUNCTION (gf effective-method options): not in
        // AMOP -- closer-mop adds it, and defines it portably for the seven hosts
        // it rebuilds the invocation protocol for. dotcl is not one of those, so
        // the name reached the export list with nothing behind it. What it has to
        // return is a function of the generic function.s arguments that runs the
        // form, which is what dispatch already does with it.
        //
        // Method combination options are not supported: dotcl keeps them on the
        // generic function, not in the form, so an options list here cannot be
        // honoured and is rejected rather than silently dropped.
        RegisterMop("COMPUTE-EFFECTIVE-METHOD-FUNCTION", -1, args =>
        {
            var form = args.Length > 1 ? args[1] : Nil.Instance;
            var options = args.Length > 2 ? args[2] : Nil.Instance;
            if (options is not Nil)
                throw new LispErrorException(new LispError(
                    $"COMPUTE-EFFECTIVE-METHOD-FUNCTION: method combination options are not supported: {options}"));
            return new LispFunction(callArgs => Runtime.ApplyEffectiveMethodForm(form, callArgs),
                "DOTCL-MOP:COMPUTE-EFFECTIVE-METHOD-FUNCTION result", -1);
        });

        // SET-FUNCALLABLE-INSTANCE-FUNCTION (instance function): AMOP sets what a
        // funcallable instance does when called. On dotcl the funcallable instances
        // that exist are generic functions -- GenericFunction is a LispFunction
        // subclass carrying its own class -- so this installs the function the
        // generic function calls, which is what a discriminating function is.
        //
        // An instance of a user class with :metaclass FUNCALLABLE-STANDARD-CLASS is
        // an ordinary instance here and is not callable, so it is rejected
        // rather than accepting the call and doing nothing.
        RegisterMop("SET-FUNCALLABLE-INSTANCE-FUNCTION", 2, args =>
        {
            if (args[0] is not GenericFunction gfInstance)
                throw new LispErrorException(new LispTypeError(
                    "SET-FUNCALLABLE-INSTANCE-FUNCTION: not a funcallable instance; on dotcl "
                    + "the funcallable instances are generic functions", args[0]));
            if (args[1] is not LispFunction newFunction)
                throw new LispErrorException(new LispTypeError(
                    "SET-FUNCALLABLE-INSTANCE-FUNCTION: not a function", args[1]));
            gfInstance.DispatchFunction = newFunction;
            gfInstance.InvalidateCache();
            return args[0];
        });

        // The invocation protocol: which generic functions the dispatcher consults, and
        // the default method each was registered with. A generic function is dispatched
        // through the protocol only when a method on one of these applies to IT.
        {
            var camSym = Startup.Sym("COMPUTE-APPLICABLE-METHODS");
            var camucSym = MopPkg.Intern("COMPUTE-APPLICABLE-METHODS-USING-CLASSES").Item1;
            var cemSym2 = MopPkg.Intern("COMPUTE-EFFECTIVE-METHOD").Item1;
            var protocols = new List<(GenericFunction, LispMethod)>();
            foreach (var sym in new[] { camSym, camucSym, cemSym2 })
                if (sym.Function is GenericFunction pgf && pgf.Methods.Count > 0)
                    protocols.Add((pgf, pgf.Methods[0]));
            Runtime.InvocationProtocolGfs = protocols.ToArray();
        }

        // READER-METHOD-CLASS / WRITER-METHOD-CLASS and COMPUTE-DEFAULT-INITARGS are
        // registered above as ordinary MOP functions; these hooks are what makes class
        // initialization and finalization actually call them, so a metaclass that
        // specialises one is heard. Same gate as the rest: nothing dispatches until
        // someone has added a method.
        {
            var readerGf = MopPkg.Intern("READER-METHOD-CLASS").Item1.Function as GenericFunction;
            var writerGf = MopPkg.Intern("WRITER-METHOD-CLASS").Item1.Function as GenericFunction;
            var defaultInitargsGf =
                MopPkg.Intern("COMPUTE-DEFAULT-INITARGS").Item1.Function as GenericFunction;
            Runtime.AccessorMethodClassHook = (cls, slotd, isReader) =>
            {
                var protocol = isReader ? readerGf : writerGf;
                if (protocol == null || protocol.Methods.Count <= 1) return;
                protocol.Invoke(new LispObject[] { cls, slotd });
            };
            Runtime.DefaultInitargsHook = cls =>
            {
                if (defaultInitargsGf == null || defaultInitargsGf.Methods.Count <= 1) return;
                defaultInitargsGf.Invoke(new LispObject[] { cls });
            };
        }

        // ADD-DIRECT-SUBCLASS / REMOVE-DIRECT-SUBCLASS: the same shape as the
        // direct-method pair above. CLASS-DIRECT-SUBCLASSES is derived by scanning
        // the class registry, so the default methods have nothing to record; the
        // call is what a metaobject class specializes. Reported on class
        // registration, including the superclasses a redefinition drops.
        {
            var tCls2 = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("ADD-DIRECT-SUBCLASS", 2, new LispClass[] { tCls2, tCls2 },
                args => Nil.Instance);
            RegisterMopGF("REMOVE-DIRECT-SUBCLASS", 2, new LispClass[] { tCls2, tCls2 },
                args => Nil.Instance);
            var addSub = (GenericFunction)MopPkg.Intern("ADD-DIRECT-SUBCLASS").Item1.Function!;
            var removeSub = (GenericFunction)MopPkg.Intern("REMOVE-DIRECT-SUBCLASS").Item1.Function!;
            Runtime.DirectSubclassHook = (super, subclass, adding) =>
            {
                var protocol = adding ? addSub : removeSub;
                if (protocol.Methods.Count <= 1) return;
                protocol.Invoke(new LispObject[] { super, subclass });
            };
        }

        // The dependent protocol. ADD- / REMOVE- / MAP-DEPENDENTS keep and walk the
        // list; UPDATE-DEPENDENT is the one a user specialises, so its default method
        // does nothing -- being told is the whole point of registering. The runtime
        // calls it after a metaobject is reinitialized and after a method is added or
        // removed, and only when something has actually registered.
        {
            var tCls = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("ADD-DEPENDENT", 2, new LispClass[] { tCls, tCls },
                args => Runtime.AddDependent(args[0], args[1]));
            RegisterMopGF("REMOVE-DEPENDENT", 2, new LispClass[] { tCls, tCls },
                args => Runtime.RemoveDependent(args[0], args[1]));
            RegisterMopGF("MAP-DEPENDENTS", 2, new LispClass[] { tCls, tCls },
                args => Runtime.MapDependents(args[0], args[1]));
            RegisterMopGF("UPDATE-DEPENDENT", -1, new LispClass[] { tCls, tCls },
                args => Nil.Instance);
        }
        // FIND-METHOD-COMBINATION (gf type-name options): AMOP hands back a
        // method combination metaobject. dotcl decides the combination from the
        // symbol and its arguments, so the object is a record of those two; what
        // callers do with it is test its type and pass it on.
        {
            var gfCls = (LispClass)Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
            var anyCls = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("FIND-METHOD-COMBINATION", 3,
                new LispClass[] { gfCls, anyCls, anyCls },
                args => new MethodCombinationObject(
                    args[1] as Symbol ?? Startup.Sym("STANDARD"), args[2]));

            // DEFGENERIC asks for the metaobject named by its :METHOD-COMBINATION
            // option. dotcl still decides the combination from the symbol; the call
            // is what a generic function class specialises. Quiet until it is.
            var findCombinationGf = (GenericFunction)MopPkg.Intern("FIND-METHOD-COMBINATION").Item1.Function!;
            Runtime.MethodCombinationHook = (gf, name, options) =>
            {
                if (findCombinationGf.Methods.Count <= 1) return;
                findCombinationGf.Invoke(new LispObject[] { gf, name, options });
            };
        }

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

        // Wire the COMPUTE-EFFECTIVE-SLOT-DEFINITION protocol into class finalization.
        // The GF is resolved lazily so user methods (added later) participate.
        LispClass.ComputeEffectiveSlotHook = (cls, name, defs) =>
        {
            if (Startup.Sym("COMPUTE-EFFECTIVE-SLOT-DEFINITION").Function is not LispFunction gf)
                return null;
            var defsList = Runtime.List(defs.Cast<LispObject>().ToArray());
            var result = MultipleValues.Primary(gf.Invoke(new LispObject[] { cls, name, defsList }));
            return result as SlotDefinition;
        };
        // Gap-fill: every DOTCL-MOP symbol still unbound after the RegisterMop/
        // RegisterMopGF calls above was flat-registered via
        // CilAssembler.RegisterFunction BEFORE Mop.Init ran — so the
        // RegisterFunction mirror couldn't fire (MopPkg was null). Adopt the
        // Function from the same-named fbound symbol in CL or DOTCL-INTERNAL
        // (matching Startup.SymForRegistration's lookup precedence) so
        // package-qualified dotcl-mop:<name> calls resolve (GetFunctionBySymbol
        // is authoritative).
        foreach (var sym in MopPkg.ExternalSymbols)
        {
            if (sym.Function != null) continue;
            var (clSym, clSt) = Startup.CL.FindSymbol(sym.Name);
            if (clSt != SymbolStatus.None && clSym.Function is LispFunction clFn)
            {
                sym.Function = clFn;
                continue;
            }
            var (internalSym, internalSt) = Startup.Internal.FindSymbol(sym.Name);
            if (internalSt != SymbolStatus.None && internalSym.Function is LispFunction fn)
                sym.Function = fn;
        }
    }

    // --- helpers -------------------------------------------------------------

    // Create a MOP GF with a single default method specializing on the given classes.
    private static void RegisterMopGF(string name, int arity, LispClass[] specializers,
        Func<LispObject[], LispObject> defaultImpl)
    {
        var (sym, _) = MopPkg.Intern(name);
        MopPkg.Export(sym);
        var gf = (GenericFunction)Runtime.MakeGF(sym, new Fixnum(arity));
        gf.RequiredCount = specializers.Length;
        gf.LambdaListInfoSet = true;
        if (arity < 0) gf.HasRest = true;
        Runtime.RegisterGF(sym, gf);
        sym.Function = gf;
        var specs = Runtime.List(specializers.Cast<LispObject>().ToArray());
        var method = (LispMethod)Runtime.MakeMethod(specs, Nil.Instance,
            new LispFunction(defaultImpl));
        method.RequiredCount = specializers.Length;
        if (arity < 0) method.HasRest = true;
        Runtime.AddMethod(gf, method);
    }

    private static void RegisterMopGFMethod(string name, LispClass[] specializers,
        Func<LispObject[], LispObject> impl)
    {
        var (sym, _) = MopPkg.Intern(name);
        if (sym.Function is not GenericFunction gf)
            throw new InvalidOperationException($"RegisterMopGFMethod: {name} is not a GF");
        var specs = Runtime.List(specializers.Cast<LispObject>().ToArray());
        var method = (LispMethod)Runtime.MakeMethod(specs, Nil.Instance, new LispFunction(impl));
        method.RequiredCount = specializers.Length;
        Runtime.AddMethod(gf, method);
    }

    // True if SPEC appears in SPECIALIZERS. A class specializer matches by identity;
    // an EQL specializer matches another when the objects they specialize on are EQL,
    // whichever representation each side is written in.
    private static bool SpecializerListContains(LispObject[] specializers, LispObject spec)
    {
        var wanted = Runtime.EqlSpecializerValue(spec);
        foreach (var s in specializers)
        {
            if (ReferenceEquals(s, spec)) return true;
            if (wanted != null && Runtime.EqlSpecializerValue(s) is { } have
                && Runtime.Eql(have, wanted) is T)
                return true;
        }
        return false;
    }

    // Slot access by layout index, shared by STANDARD-INSTANCE-ACCESS and
    // FUNCALLABLE-STANDARD-INSTANCE-ACCESS. WHO names the function the caller
    // called, so the error does not point at the wrong one.
    private static LispObject InstanceSlotRead(string who, LispObject obj, LispObject location)
    {
        if (FuncallableSlotName(who, obj, location) is { } name)
        {
            var funcallable = (GenericFunction)obj;
            if (funcallable.ExtraSlots != null
                && funcallable.ExtraSlots.TryGetValue(name, out var stored))
                return stored ?? Nil.Instance;
            return Nil.Instance;
        }
        var (instance, index) = InstanceSlotIndex(who, obj, location);
        // AMOP returns an unbound marker for unbound slots; dotcl has none
        // exposed, so NIL stands in (callers writing before reading are fine).
        return instance.Slots[index] ?? Nil.Instance;
    }

    private static LispObject InstanceSlotWrite(string who, LispObject obj, LispObject location,
        LispObject newValue)
    {
        if (FuncallableSlotName(who, obj, location) is { } name)
        {
            (((GenericFunction)obj).EnsureExtraSlots())[name] = newValue;
            return newValue;
        }
        var (instance, index) = InstanceSlotIndex(who, obj, location);
        instance.Slots[index] = newValue;
        return newValue;
    }

    /// <summary>The slot name a layout index names on a funcallable instance, or null
    /// when OBJ is an ordinary instance. A funcallable instance is a callable object,
    /// so it has no slot vector to index -- its slots live by name -- but AMOP still
    /// addresses them by the location the class layout gives, so the index is resolved
    /// through the class.</summary>
    private static string? FuncallableSlotName(string who, LispObject obj, LispObject location)
    {
        if (obj is not GenericFunction gf || gf.StoredClass is not { } cls) return null;
        if (location is not Fixnum loc)
            throw new LispErrorException(new LispTypeError(
                $"{who}: location must be an integer", location));
        int index = (int)loc.Value;
        var slots = cls.EffectiveSlots;
        if (index < 0 || index >= slots.Length)
            throw new LispErrorException(new LispProgramError(
                $"{who}: location {index} out of range [0,{slots.Length})"));
        return slots[index].Name.Name;
    }

    private static (LispInstance, int) InstanceSlotIndex(string who, LispObject obj,
        LispObject location)
    {
        if (obj is not LispInstance instance)
            throw new LispErrorException(new LispTypeError($"{who}: not an instance", obj));
        if (location is not Fixnum loc)
            throw new LispErrorException(new LispTypeError(
                $"{who}: location must be an integer", location));
        int index = (int)loc.Value;
        if (index < 0 || index >= instance.Slots.Length)
            throw new LispErrorException(new LispProgramError(
                $"{who}: location {index} out of range [0,{instance.Slots.Length})"));
        return (instance, index);
    }

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

    internal static LispObject BuildLambdaListPlaceholder(
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
