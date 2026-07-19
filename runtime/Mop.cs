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
            // Must return the SAME instance every call (memoized on the class) —
            // EQL-method dispatch (e.g. McCLIM define-presentation-method) relies
            // on (eql class-prototype) being stable across definition and call.
            if (args[0] is not LispClass c)
                throw new LispErrorException(new LispTypeError("CLASS-PROTOTYPE: not a class", args[0]));
            return c.Prototype;
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
            // AMOP: a non-negative integer index into the instance layout for
            // instance-allocated slots; NIL for :class allocation.
            if (args[0] is not SlotDefinition s) return Nil.Instance;
            return s.Location >= 0 ? (LispObject)Fixnum.Make(s.Location) : Nil.Instance;
        });

        // STANDARD-INSTANCE-ACCESS (instance location): read the slot at the given
        // integer layout index directly, bypassing slot-value-using-class.
        RegisterMop("STANDARD-INSTANCE-ACCESS", 2, args =>
        {
            if (args[0] is not LispInstance inst)
                throw new LispErrorException(new LispTypeError(
                    "STANDARD-INSTANCE-ACCESS: not an instance", args[0]));
            if (args[1] is not Fixnum loc)
                throw new LispErrorException(new LispTypeError(
                    "STANDARD-INSTANCE-ACCESS: location must be an integer", args[1]));
            int i = (int)loc.Value;
            if (i < 0 || i >= inst.Slots.Length)
                throw new LispErrorException(new LispProgramError(
                    $"STANDARD-INSTANCE-ACCESS: location {i} out of range [0,{inst.Slots.Length})"));
            // AMOP returns an unbound marker for unbound slots; dotcl has none
            // exposed, so NIL stands in (callers writing before reading are fine).
            return inst.Slots[i] ?? Nil.Instance;
        });
        // (setf (standard-instance-access instance location) new-value)
        {
            var (siaSym, _) = MopPkg.Intern("STANDARD-INSTANCE-ACCESS");
            siaSym.SetfFunction = new LispFunction(args =>
            {
                // setf-function arg order: (new-value instance location)
                var newValue = args[0];
                if (args[1] is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError(
                        "(SETF STANDARD-INSTANCE-ACCESS): not an instance", args[1]));
                if (args[2] is not Fixnum loc)
                    throw new LispErrorException(new LispTypeError(
                        "(SETF STANDARD-INSTANCE-ACCESS): location must be an integer", args[2]));
                int i = (int)loc.Value;
                if (i < 0 || i >= inst.Slots.Length)
                    throw new LispErrorException(new LispProgramError(
                        $"(SETF STANDARD-INSTANCE-ACCESS): location {i} out of range [0,{inst.Slots.Length})"));
                inst.Slots[i] = newValue;
                return newValue;
            }, "(SETF DOTCL-MOP:STANDARD-INSTANCE-ACCESS)", 3);
        }

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
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            if (gf.StoredLambdaList != null) return gf.StoredLambdaList;
            // Reconstruct a placeholder lambda list from arity info when no stored lambda-list.
            return BuildLambdaListPlaceholder(gf.RequiredCount, gf.OptionalCount,
                gf.HasRest, gf.HasKey, gf.KeywordNames, gf.HasAllowOtherKeys);
        });

        RegisterMop("GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER", 1, args =>
        {
            if (args[0] is not GenericFunction gf) return Nil.Instance;
            // Collect required-parameter names from stored lambda-list
            var ll = gf.StoredLambdaList ?? BuildLambdaListPlaceholder(gf.RequiredCount,
                gf.OptionalCount, gf.HasRest, gf.HasKey, gf.KeywordNames, gf.HasAllowOtherKeys);
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

        // ACCESSOR-METHOD-SLOT-DEFINITION (method) → the slot-definition the accessor
        // reads/writes. DEFCLASS tags reader/writer/accessor methods with their slot
        // via %REGISTER-ACCESSOR-METHOD; ordinary methods return NIL.
        RegisterMop("ACCESSOR-METHOD-SLOT-DEFINITION", 1, args =>
            args[0] is LispMethod m && m.AccessorSlot is { } sd ? (LispObject)sd : Nil.Instance);

        // METHOD-FUNCTION (method) → the method's function object. AMOP: the function
        // called with (args next-methods); dotcl's method Function already follows the
        // call-method calling convention used by the dispatcher.
        RegisterMop("METHOD-FUNCTION", 1, args =>
            args[0] is LispMethod m ? (LispObject)m.Function : Nil.Instance);

        // READER-METHOD-CLASS / WRITER-METHOD-CLASS (class slotd &rest initargs) →
        // the class of accessor methods. dotcl creates plain standard methods for
        // accessors, so the metaobject class is STANDARD-METHOD.
        RegisterMop("READER-METHOD-CLASS", -1, args =>
            (LispObject?)Runtime.FindClassOrNil(Startup.Sym("STANDARD-METHOD")) ?? Nil.Instance);
        RegisterMop("WRITER-METHOD-CLASS", -1, args =>
            (LispObject?)Runtime.FindClassOrNil(Startup.Sym("STANDARD-METHOD")) ?? Nil.Instance);

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
        RegisterMop("COMPUTE-SLOTS", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.EffectiveSlots.Cast<LispObject>().ToArray()) : Nil.Instance);
        RegisterMop("COMPUTE-CLASS-PRECEDENCE-LIST", 1, args =>
            args[0] is LispClass c ? Runtime.List(c.ClassPrecedenceList.Cast<LispObject>().ToArray()) : Nil.Instance);
        RegisterMop("COMPUTE-DEFAULT-INITARGS", 1, args =>
        {
            if (args[0] is not LispClass c) return Nil.Instance;
            var items = c.DefaultInitargs
                .Select(p => Runtime.List(p.Key, Nil.Instance, p.Thunk))
                .ToArray();
            return Runtime.List(items);
        });

        // COMPUTE-APPLICABLE-METHODS-USING-CLASSES (gf classes) → (values methods
        // definitive-p). definitive-p is NIL when applicability depends on an EQL
        // specializer (undecidable from a class), so the caller falls back to
        // COMPUTE-APPLICABLE-METHODS on the actual arguments.
        RegisterMop("COMPUTE-APPLICABLE-METHODS-USING-CLASSES", 2, args =>
            Runtime.ComputeApplicableMethodsUsingClasses(args[0], args[1]));

        // COMPUTE-DISCRIMINATING-FUNCTION (gf) → a function that performs the GF's
        // dispatch. dotcl's GenericFunction IS its own discriminating function (it is
        // funcallable and runs the dispatcher), so return it directly.
        RegisterMop("COMPUTE-DISCRIMINATING-FUNCTION", 1, args =>
            args[0] is GenericFunction gf ? (LispObject)gf : Nil.Instance);

        // COMPUTE-EFFECTIVE-METHOD (gf method-combination methods) → the effective
        // method form for the STANDARD method combination (CLHS 7.6.6.2). METHODS is
        // the applicable list most-specific-first. Non-standard combinations are not
        // synthesized here (dotcl's own dispatcher handles them internally).
        RegisterMop("COMPUTE-EFFECTIVE-METHOD", 3, args =>
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
                    var effClass = esdcFn.Invoke(new LispObject[] {
                        cls, Startup.Keyword("NAME"), name });
                    if (effClass is LispClass ec && !ReferenceEquals(ec, stdEffective))
                    {
                        eslot.MetaClass = ec;
                        Runtime.InitializeSlotdExtraSlots(eslot, ec, null);
                    }
                }
                return eslot;
            });
        }
        // ADD-DEPENDENT / REMOVE-DEPENDENT / MAP-DEPENDENTS / UPDATE-DEPENDENT (stubs)
        {
            var tCls = (LispClass)Runtime.FindClass(Startup.Sym("T"));
            RegisterMopGF("ADD-DEPENDENT", 2, new LispClass[] { tCls, tCls }, args => Nil.Instance);
            RegisterMopGF("REMOVE-DEPENDENT", 2, new LispClass[] { tCls, tCls }, args => Nil.Instance);
            RegisterMopGF("MAP-DEPENDENTS", 2, new LispClass[] { tCls, tCls }, args => Nil.Instance);
            RegisterMopGF("UPDATE-DEPENDENT", -1, new LispClass[] { tCls, tCls }, args => Nil.Instance);
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
    // an (eql X) specializer matches another (eql Y) when X and Y are EQL.
    private static bool SpecializerListContains(LispObject[] specializers, LispObject spec)
    {
        foreach (var s in specializers)
        {
            if (ReferenceEquals(s, spec)) return true;
            if (s is Cons sc && spec is Cons pc
                && sc.Car is Symbol ss && ss.Name == "EQL"
                && pc.Car is Symbol ps && ps.Name == "EQL"
                && sc.Cdr is Cons scd && pc.Cdr is Cons pcd
                && Runtime.Eql(scd.Car, pcd.Car) is T)
                return true;
        }
        return false;
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
