using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    /// <summary>Resolve the :DIRECT-SUPERCLASSES and :DIRECT-SLOTS initargs AMOP uses
    /// for class creation. Shared by ENSURE-CLASS and by MAKE-INSTANCE on a class
    /// metaobject class, so the two read the same canonical plists the same way.</summary>
    internal static (LispObject Supers, LispObject SlotDefs) ParseClassInitargs(
        LispObject supersSpec, LispObject slotsSpec)
    {
        LispObject supersList = Nil.Instance;
        {
            var resolved = new List<LispObject>();
            for (var c = supersSpec; c is Cons cc; c = cc.Cdr)
            {
                LispObject sup = cc.Car switch
                {
                    LispClass sc => sc,
                    Symbol sn => FindClassOrNil(sn),
                    _ => Nil.Instance
                };
                if (sup is LispClass) resolved.Add(sup);
            }
            for (int i = resolved.Count - 1; i >= 0; i--) supersList = new Cons(resolved[i], supersList);
        }
        LispObject slotDefsList = Nil.Instance;
        var pairs = ParseDirectSlotPlists(slotsSpec);
        for (int i = pairs.Count - 1; i >= 0; i--) slotDefsList = new Cons(pairs[i].Slot, slotDefsList);
        return (supersList, slotDefsList);
    }

    /// <summary>Each canonical direct slot plist of a :DIRECT-SLOTS value, paired with
    /// the slot definition made from it. An element that is not a plist naming a slot
    /// is skipped.</summary>
    internal static List<(LispObject Plist, SlotDefinition Slot)> ParseDirectSlotPlists(LispObject slotsSpec)
    {
        var pairs = new List<(LispObject, SlotDefinition)>();
        for (var c = slotsSpec; c is Cons cc; c = cc.Cdr)
            if (SlotDefFromCanonicalPlist(cc.Car) is { } sd) pairs.Add((cc.Car, sd));
        return pairs;
    }

    /// <summary>A direct slot definition from an AMOP canonical slot plist
    /// (:NAME :INITFORM :INITFUNCTION :INITARGS :READERS :WRITERS :ALLOCATION :TYPE
    /// :DOCUMENTATION and any other option). Options this does not know, and :TYPE,
    /// become the slot's raw options, which is where DEFCLASS puts them for
    /// DIRECT-SLOT-DEFINITION-CLASS. Null when the plist names no slot.</summary>
    internal static SlotDefinition? SlotDefFromCanonicalPlist(LispObject plist)
    {
        if (plist is not Cons) return null;
        LispObject? name = null, initargs = null, initfn = null, initform = null,
            readers = null, writers = null, type = null, doc = null, alloc = null;
        var raw = new List<LispObject>();
        for (var p = plist; p is Cons pc && pc.Cdr is Cons pv; p = pv.Cdr)
        {
            var k = pc.Car;
            var v = pv.Car;
            // The first occurrence of a standard option wins, as for any plist.
            if (k == Startup.Keyword("NAME")) name ??= v;
            else if (k == Startup.Keyword("INITARGS")) initargs ??= v;
            else if (k == Startup.Keyword("INITFUNCTION")) initfn ??= v;
            else if (k == Startup.Keyword("INITFORM")) initform ??= v;
            else if (k == Startup.Keyword("READERS")) readers ??= v;
            else if (k == Startup.Keyword("WRITERS")) writers ??= v;
            else if (k == Startup.Keyword("DOCUMENTATION")) doc ??= v;
            else if (k == Startup.Keyword("TYPE"))
            {
                type ??= v;
                raw.Add(k); raw.Add(v);
            }
            else if (k == Startup.Keyword("ALLOCATION"))
            {
                alloc ??= v;
                if (v is not Symbol av || (av.Name != "INSTANCE" && av.Name != "CLASS"))
                { raw.Add(k); raw.Add(v); }
            }
            else { raw.Add(k); raw.Add(v); }
        }
        if (name is not Symbol || name is Nil) return null;
        var sd = alloc is Symbol a && a.Name == "CLASS"
            ? (SlotDefinition)MakeSlotDefWithAllocation(name, initargs ?? Nil.Instance, initfn ?? Nil.Instance, Startup.Sym("CLASS"))
            : (SlotDefinition)MakeSlotDef(name, initargs ?? Nil.Instance, initfn ?? Nil.Instance);
        if (raw.Count > 0) SetSlotDefRawOptions(sd, List(raw.ToArray()));
        SetSlotDefAttrs(sd, readers ?? Nil.Instance, writers ?? Nil.Instance,
            type ?? Nil.Instance, initform ?? Nil.Instance);
        if (doc != null) sd.Documentation = doc;
        return sd;
    }

    /// <summary>The AMOP canonical plist for a direct slot definition, the shape
    /// DEFCLASS hands a metaclass in :DIRECT-SLOTS: :NAME, :INITFORM and :INITFUNCTION
    /// when there is an initform, :INITARGS, :READERS, :WRITERS, then :ALLOCATION,
    /// :DOCUMENTATION and :TYPE when they say something, then the slot's other
    /// options as written.</summary>
    internal static LispObject CanonicalSlotPlist(SlotDefinition sd)
    {
        var items = new List<LispObject> { Startup.Keyword("NAME"), sd.Name };
        if (sd.InitformThunk != null)
        {
            items.Add(Startup.Keyword("INITFORM")); items.Add(sd.Initform);
            items.Add(Startup.Keyword("INITFUNCTION")); items.Add(sd.InitformThunk);
        }
        items.Add(Startup.Keyword("INITARGS")); items.Add(List(CanonicalInitargs(sd.Initargs)));
        items.Add(Startup.Keyword("READERS")); items.Add(List(sd.Readers));
        items.Add(Startup.Keyword("WRITERS")); items.Add(List(sd.Writers));
        if (sd.IsClassAllocation) { items.Add(Startup.Keyword("ALLOCATION")); items.Add(Startup.Keyword("CLASS")); }
        if (sd.Documentation is not Nil) { items.Add(Startup.Keyword("DOCUMENTATION")); items.Add(sd.Documentation); }
        bool rawHasType = false;
        for (var c = sd.RawOptions; c is Cons rc && rc.Cdr is Cons rv; c = rv.Cdr)
            if (rc.Car == Startup.Keyword("TYPE")) { rawHasType = true; break; }
        if (!rawHasType && sd.SlotType is not T) { items.Add(Startup.Keyword("TYPE")); items.Add(sd.SlotType); }
        for (var c = sd.RawOptions; c is Cons rc; c = rc.Cdr) items.Add(rc.Car);
        return List(items.ToArray());
    }

    /// <summary>AMOP canonical default initargs, ((name form function) ...), as the
    /// class stores them. Elements not of that shape are skipped.</summary>
    internal static (Symbol Key, LispObject Form, LispFunction Thunk)[] ParseCanonicalDefaultInitargs(LispObject spec)
    {
        var result = new List<(Symbol, LispObject, LispFunction)>();
        for (var c = spec; c is Cons cc; c = cc.Cdr)
            if (cc.Car is Cons e && e.Car is Symbol k && e.Cdr is Cons fc
                && fc.Cdr is Cons tc && tc.Car is LispFunction thunk)
                result.Add((k, fc.Car, thunk));
        return result.ToArray();
    }

    /// <summary>MAKE-INSTANCE on a class metaobject class makes a class (AMOP). The
    /// result is not registered under a name: an anonymous class is reachable only
    /// through the object, which is the point of it. A :NAME initarg still names it,
    /// but naming is what ENSURE-CLASS is for.</summary>
    internal static LispObject MakeClassMetaobject(LispClass metaclass, LispObject[] initargs)
    {
        LispObject supersSpec = Nil.Instance;
        bool supersGiven = false;
        Symbol nameSym = Startup.Sym("NIL");
        // Everything else is an initarg for the metaclass's own slots, the same
        // thing DEFCLASS passes for a class option it does not handle itself.
        // :DIRECT-SLOTS stays among them: MakeClassCore makes the slots from it and
        // hands the plists to INITIALIZE-INSTANCE as they were given.
        var extra = new List<LispObject>();
        bool nameSeen = false;
        for (int i = 0; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is not Symbol k) continue;
            if (k == Startup.Keyword("DIRECT-SUPERCLASSES"))
            {
                if (!supersGiven) { supersSpec = initargs[i + 1]; supersGiven = true; }
            }
            else if (!nameSeen && k == Startup.Keyword("NAME") && initargs[i + 1] is Symbol ns)
            {
                nameSym = ns;
                nameSeen = true;
            }
            else { extra.Add(k); extra.Add(initargs[i + 1]); }
        }
        // With no :DIRECT-SUPERCLASSES at all, the initargs get (STANDARD-OBJECT), as
        // a default initarg would (SBCL does the same). An explicit empty list is
        // passed on as given: a metaclass's INITIALIZE-INSTANCE :AROUND that adds a
        // superclass of its own (ContextL's SPECIAL-CLASS) must not find
        // STANDARD-OBJECT ahead of it. The class still gets STANDARD-OBJECT when
        // the list stays empty (MakeClassCore and the SHARED-INITIALIZE primary).
        if (!supersGiven && FindClassOrNil(Startup.Sym("STANDARD-OBJECT")) is LispClass stdObj)
            supersSpec = new Cons(stdObj, Nil.Instance);
        var (supers, _) = ParseClassInitargs(supersSpec, Nil.Instance);
        return MakeClassCore(nameSym, supers, Nil.Instance, metaclass, extra.ToArray());
    }

    /// <summary>MAKE-INSTANCE of a slot-definition class (AMOP 5.4.2). A slot
    /// definition is a SlotDefinition object here, not a LispInstance, so the
    /// standard initargs (:name :initargs :initform :initfunction :allocation :type
    /// :documentation :readers :writers) fill it directly. The slots a subclass
    /// adds live where those of a DIRECT-/EFFECTIVE-SLOT-DEFINITION-CLASS result
    /// do. A metaclass's COMPUTE-SLOTS method uses this to add a slot of its own.</summary>
    internal static LispObject MakeSlotDefinitionMetaobject(LispClass cls, LispObject[] initargs)
    {
        LispObject? Get(string key)
        {
            var k = Startup.Keyword(key);
            for (int i = 0; i + 1 < initargs.Length; i += 2)
                if (ReferenceEquals(initargs[i], k)) return initargs[i + 1];
            return null;
        }
        if (Get("NAME") is not Symbol name)
            throw new LispErrorException(new LispProgramError(
                $"MAKE-INSTANCE {cls.Name.Name}: the :NAME initarg must be a symbol"));
        var initargNames = new List<Symbol>();
        foreach (var ia in ListToArray(Get("INITARGS") ?? Nil.Instance))
            if (ia is Symbol s) initargNames.Add(s);
            else if (ia is Nil) initargNames.Add(Startup.NIL_SYM);
            else if (ia is T) initargNames.Add(Startup.T_SYM);
        var slotd = new SlotDefinition(name, initargNames.ToArray(), Get("INITFUNCTION") as LispFunction);
        if (Get("INITFORM") is { } initform) slotd.Initform = initform;
        if (Get("TYPE") is { } type) slotd.SlotType = type;
        if (Get("DOCUMENTATION") is { } doc) slotd.Documentation = doc;
        if (Get("ALLOCATION") is Symbol alloc && alloc != Startup.Keyword("INSTANCE"))
        {
            if (alloc == Startup.Keyword("CLASS")) slotd.IsClassAllocation = true;
            else slotd.Allocation = alloc;
        }
        slotd.IsEffective = cls.ClassPrecedenceList.Any(c => IsSystemClass(c, "EFFECTIVE-SLOT-DEFINITION"));
        if (!slotd.IsEffective)
        {
            slotd.Readers = ListToArray(Get("READERS") ?? Nil.Instance);
            slotd.Writers = ListToArray(Get("WRITERS") ?? Nil.Instance);
        }
        var standard = FindClassOrNil(Startup.Sym(slotd.IsEffective
            ? "STANDARD-EFFECTIVE-SLOT-DEFINITION" : "STANDARD-DIRECT-SLOT-DEFINITION"));
        if (!ReferenceEquals(cls, standard))
        {
            slotd.MetaClass = cls;
            InitializeSlotDefinitionInstance(slotd, initargs);
        }
        return slotd;
    }

    internal static GenericFunction? EnsureGfGf;
    internal static LispMethod? EnsureGfDefault;
    /// <summary>What the default method of ENSURE-GENERIC-FUNCTION-USING-CLASS runs when
    /// DEFGENERIC reached it: the expansion's own way of finding or making the function.
    /// Taken once by that method.</summary>
    [ThreadStatic] internal static LispFunction? s_egfThunk;

    /// <summary>True when a method of the program on ENSURE-GENERIC-FUNCTION-USING-CLASS
    /// applies: its first specializer covers the class of EXISTING (the function being
    /// redefined), or of NIL when there is none, or is EQL to EXISTING.</summary>
    internal static bool EnsureGfOverridden(LispObject existing)
    {
        var gf = EnsureGfGf;
        if (gf == null || gf.Methods.Count <= 1) return false;
        var argClass = ClassOf(existing) as LispClass;
        if (argClass == null) return false;
        foreach (var m in gf.Methods)
        {
            if (ReferenceEquals(m, EnsureGfDefault) || m.Specializers.Length == 0) continue;
            var spec = m.Specializers[0];
            if (spec is LispClass sc)
            {
                if (Array.IndexOf(argClass.ClassPrecedenceList, sc) >= 0) return true;
            }
            else if (spec is EqlSpecializer es && ReferenceEquals(es.Object, existing))
                return true;
        }
        return false;
    }

    /// <summary>AMOP has DEFGENERIC and ENSURE-GENERIC-FUNCTION go through the generic
    /// function ENSURE-GENERIC-FUNCTION-USING-CLASS. EXISTING is the generic function
    /// of the name or NIL. THUNK, when given, is DEFGENERIC's own way of getting the
    /// function, which the default method runs.</summary>
    public static LispObject EnsureGfViaProtocol(LispObject name, LispObject existing, LispObject initargs, LispObject thunk)
    {
        var args = new List<LispObject> { existing, name };
        for (var cur = initargs; cur is Cons c; cur = c.Cdr) args.Add(c.Car);
        s_egfThunk = thunk as LispFunction;
        try { return MultipleValues.Primary(EnsureGfGf!.Invoke(args.ToArray())); }
        finally { s_egfThunk = null; }
    }

    internal static GenericFunction? EnsureClassGf;
    internal static LispMethod? EnsureClassDefault;
    [ThreadStatic] internal static bool s_ensureClassDefaultRunning;

    /// <summary>True when a method of the program on ENSURE-CLASS-USING-CLASS applies:
    /// its first specializer covers PRIOR's metaclass (the class being redefined), or
    /// NULL for a class not defined yet, or is EQL to PRIOR.</summary>
    internal static bool EnsureClassOverridden(GenericFunction gf, LispClass? prior)
    {
        var argClass = (prior != null ? ClassOf(prior) : ClassOf(Nil.Instance)) as LispClass;
        if (argClass == null) return false;
        foreach (var m in gf.Methods)
        {
            if (ReferenceEquals(m, EnsureClassDefault) || m.Specializers.Length == 0) continue;
            var spec = m.Specializers[0];
            if (spec is LispClass sc)
            {
                if (Array.IndexOf(argClass.ClassPrecedenceList, sc) >= 0) return true;
            }
            else if (spec is EqlSpecializer es && prior != null && ReferenceEquals(es.Object, prior))
                return true;
        }
        return false;
    }

    public static LispObject MakeClassFull(LispObject name, LispObject supersList, LispObject slotDefsList, LispObject metaclassObj)
        => MakeClassCore(name, supersList, slotDefsList, metaclassObj as LispClass, null, defining: true);

    // Variant carrying metaclass-slot initargs (e.g. :type-name) so the class object's
    // single initialize-instance applies them before inherited :after methods run.
    // Distinct name (not an overload) because builtins are reflected by method name and
    // two methods named MakeClassFull would make that lookup ambiguous.
    public static LispObject MakeClassFullWithInitargs(LispObject name, LispObject supersList, LispObject slotDefsList, LispObject metaclassObj, LispObject[] extraInitargs)
        => MakeClassCore(name, supersList, slotDefsList, metaclassObj as LispClass, extraInitargs, defining: true);

    // The same thing for DEFCLASS, which has the initargs as a Lisp list rather than
    // an array: a class option that is not :METACLASS, :DEFAULT-INITARGS or
    // :DOCUMENTATION is an initarg for the metaclass, so DEFCLASS builds a plist of
    // them and calls this. Kept separate from the array version because builtins are
    // reflected by method name, so an overload would make the lookup ambiguous.
    public static LispObject MakeClassFullOptions(LispObject name, LispObject supersList, LispObject slotDefsList, LispObject metaclassObj, LispObject extraInitargs)
    {
        var extra = new List<LispObject>();
        for (var cur = extraInitargs; cur is Cons c; cur = c.Cdr)
            extra.Add(c.Car);
        return MakeClassCore(name, supersList, slotDefsList, metaclassObj as LispClass, extra.ToArray(), defining: true);
    }

    private static LispObject MakeClassCore(LispObject name, LispObject supersList, LispObject slotDefsList, LispClass? metaclass, LispObject[]? extraInitargs = null, bool defining = false)
    {
        if (name is not Symbol sym)
            throw new LispErrorException(new LispTypeError("MAKE-CLASS: name must be a symbol", name));

        // AMOP has DEFCLASS and ENSURE-CLASS go through the generic function
        // ENSURE-CLASS-USING-CLASS. A method of the program on it takes the call;
        // its default method comes back here with the flag set, which is read once.
        if (defining)
        {
            if (s_ensureClassDefaultRunning) s_ensureClassDefaultRunning = false;
            else if (EnsureClassGf is { } ecGf && ecGf.Methods.Count > 1)
            {
                _classRegistry.TryGetValue(sym, out var prior);
                if (prior != null && (prior.IsForwardReferenced || prior.IsBuiltIn)) prior = null;
                if (EnsureClassOverridden(ecGf, prior))
                {
                    var viaGf = new List<LispObject> { (LispObject?)prior ?? Nil.Instance, sym };
                    if (metaclass != null) { viaGf.Add(Startup.Keyword("METACLASS")); viaGf.Add(metaclass); }
                    viaGf.Add(Startup.Keyword("DIRECT-SUPERCLASSES")); viaGf.Add(supersList);
                    bool hasSlotPlists = false;
                    if (extraInitargs != null)
                        for (int i = 0; i + 1 < extraInitargs.Length; i += 2)
                            if (extraInitargs[i] == Startup.Keyword("DIRECT-SLOTS")) hasSlotPlists = true;
                    if (!hasSlotPlists)
                    {
                        var plists = new List<LispObject>();
                        for (var sc = slotDefsList; sc is Cons scc; sc = scc.Cdr)
                            if (scc.Car is SlotDefinition ssd) plists.Add(CanonicalSlotPlist(ssd));
                        viaGf.Add(Startup.Keyword("DIRECT-SLOTS")); viaGf.Add(List(plists.ToArray()));
                    }
                    if (extraInitargs != null) viaGf.AddRange(extraInitargs);
                    return ecGf.Invoke(viaGf.ToArray());
                }
            }
        }

        // Collect superclasses
        var supers = new List<LispClass>();
        var cur = supersList;
        while (cur is Cons c)
        {
            if (c.Car is LispClass sc)
                supers.Add(sc);
            cur = c.Cdr;
        }
        // Default to STANDARD-OBJECT if no supers -- except under
        // FUNCALLABLE-STANDARD-CLASS, where AMOP names FUNCALLABLE-STANDARD-OBJECT.
        // A class whose instances are callable is not a STANDARD-OBJECT.
        // A class whose metaclass is a FORWARD-REFERENCED-CLASS (ENSURE-CLASS with such a
        // :METACLASS, as McCLIM does for presentation types named before they exist) is
        // a placeholder: it gets no default superclass, is not validated or finalized,
        // and a later real definition replaces it in place.
        bool forwardMeta = IsForwardReferencedMetaclass(metaclass);
        if (supers.Count == 0 && !forwardMeta
            && _classRegistry.TryGetValue(DefaultDirectSuperclassName(metaclass), out var stdObj))
            supers.Add(stdObj);

        // Collect slot definitions
        var slots = new List<SlotDefinition>();
        cur = slotDefsList;
        while (cur is Cons c2)
        {
            if (c2.Car is SlotDefinition sd)
                slots.Add(sd);
            cur = c2.Cdr;
        }

        // ENSURE-CLASS and MAKE-INSTANCE of a metaclass give the direct slots as
        // canonical plists (:DIRECT-SLOTS) and may give :DIRECT-DEFAULT-INITARGS;
        // DEFCLASS under a custom metaclass gives the latter the same way. The slots
        // are made from the plists, and the plists themselves are what
        // INITIALIZE-INSTANCE sees.
        List<(LispObject Plist, SlotDefinition Slot)>? slotPlists = null;
        LispObject? givenSlotPlists = null, givenDefaultInitargs = null;
        if (extraInitargs != null)
        {
            var rest = new List<LispObject>(extraInitargs.Length);
            for (int i = 0; i + 1 < extraInitargs.Length; i += 2)
            {
                var k = extraInitargs[i];
                if (k == Startup.Keyword("DIRECT-SLOTS"))
                {
                    if (givenSlotPlists == null)
                    {
                        givenSlotPlists = extraInitargs[i + 1];
                        slotPlists = ParseDirectSlotPlists(givenSlotPlists);
                    }
                    continue;
                }
                if (k == Startup.Keyword("DIRECT-DEFAULT-INITARGS"))
                    givenDefaultInitargs ??= extraInitargs[i + 1];
                rest.Add(k); rest.Add(extraInitargs[i + 1]);
            }
            extraInitargs = rest.ToArray();
            if (slotPlists != null)
            {
                slots.Clear();
                foreach (var (_, sd) in slotPlists) slots.Add(sd);
            }
        }

        // Redefining a class (DEFCLASS / ENSURE-CLASS of a name whose class is
        // already defined, under the same custom metaclass) reinitializes that class
        // (CLHS 4.3.6, AMOP ENSURE-CLASS-USING-CLASS), so the metaclass's
        // REINITIALIZE-INSTANCE methods run, not INITIALIZE-INSTANCE on a fresh
        // object. A metaclass change, or a class that never got finalized, still
        // takes the fresh-object path, which RegisterClass copies into place.
        if (defining && metaclass != null
            && _classRegistry.TryGetValue(sym, out var existing)
            && ReferenceEquals(existing.Name, sym) && ReferenceEquals(existing.Metaclass, metaclass)
            && !existing.IsBuiltIn && !existing.IsStructureClass && !existing.NameCleared
            && !existing.IsForwardReferenced && existing.ClassPrecedenceList.Length > 0)
            return RedefineByReinitialize(existing, supersList, slots, slotPlists, extraInitargs);

        ValidateSuperclasses(sym, metaclass, supers);

        var cls = new LispClass(sym, slots.ToArray(), supers.ToArray());
        cls.Metaclass = metaclass;
        if (forwardMeta) cls.IsForwardReferenced = true;
        // Slots given as plists (ENSURE-CLASS, MAKE-INSTANCE of a metaclass) have no
        // DEFCLASS expansion behind them to define their readers and writers.
        if (slotPlists != null)
            NotePendingAccessors(cls, slots);
        if (givenDefaultInitargs != null)
            cls.DirectDefaultInitargs = ParseCanonicalDefaultInitargs(givenDefaultInitargs);
        var supersBeforeInit = cls.DirectSuperclasses;
        // AMOP: for a custom metaclass, consult DIRECT-SLOT-DEFINITION-CLASS for each
        // direct slot. A non-standard return class becomes the slot's MetaClass and its
        // Lisp-level slots are initialized from the slot's options.
        if (metaclass != null)
        {
            ApplyDirectSlotDefinitionClass(cls);
            // A class is an instance of its metaclass. Run the real CLOS init protocol
            // on the class object so the metaclass's added slots get their initforms
            // AND any inherited initialize-instance / shared-initialize :after
            // methods fire: e.g. a slot computed by an :after method. The
            // shared-initialize primary handles a LispClass's ExtraSlots above.
            // The class is being made, so the metaclass's :DEFAULT-INITARGS are due
            // (CLHS 7.1.3), as for any MAKE-INSTANCE: a metaclass that defaults one of
            // its own initargs used to see NIL for it in INITIALIZE-INSTANCE and keep
            // the slot's initform.
            //
            // AMOP also passes :NAME and :DIRECT-SUPERCLASSES (the superclasses as
            // given, before the STANDARD-OBJECT default). A metaclass method may
            // rewrite :DIRECT-SUPERCLASSES and call the next method with the new
            // list; the SHARED-INITIALIZE primary for class metaobjects is what
            // installs it, so the class is finalized with what arrived there.
            //
            // Likewise :DIRECT-SLOTS, as canonical slot plists, and whatever
            // :DIRECT-DEFAULT-INITARGS the caller gave. The SHARED-INITIALIZE primary
            // installs the values that reach it, so a method that rewrites either
            // one (clsql's view classes read the slot plists) is honored.
            if (slotPlists == null)
            {
                slotPlists = new List<(LispObject, SlotDefinition)>(cls.DirectSlots.Length);
                foreach (var sd in cls.DirectSlots) slotPlists.Add((CanonicalSlotPlist(sd), sd));
            }
            var plistItems = new LispObject[slotPlists.Count];
            for (int i = 0; i < plistItems.Length; i++) plistItems[i] = slotPlists[i].Plist;
            var given = new List<LispObject> {
                Startup.Keyword("NAME"), CanonicalizeSymbol(sym),
                Startup.Keyword("DIRECT-SUPERCLASSES"), supersList,
                Startup.Keyword("DIRECT-SLOTS"), List(plistItems),
            };
            if (extraInitargs != null) given.AddRange(extraInitargs);
            extraInitargs = ApplyDefaultInitargs(metaclass, given.ToArray());
            if (Startup.Sym("INITIALIZE-INSTANCE").Function is LispFunction iiFn)
            {
                // Pass the metaclass-slot initargs (e.g. :type-name from ensure-class) so
                // shared-initialize applies them to the class object's ExtraSlots BEFORE any
                // inherited initialize-instance :after runs: matching the ordinary instance
                // init order. Otherwise an :after that reads an initarg-filled slot sees it
                // UNBOUND.
                var iiArgs = new LispObject[1 + extraInitargs.Length];
                iiArgs[0] = cls;
                Array.Copy(extraInitargs, 0, iiArgs, 1, extraInitargs.Length);
                cls.PendingSlotPlists = PendingSlotPlistsOf(slotPlists);
                if (givenSlotPlists == null) cls.DefclassAccessors = AccessorKeys(slots);
                try { iiFn.Invoke(iiArgs); }
                finally { cls.PendingSlotPlists = null; cls.DefclassAccessors = null; }
            }
            if (!ReferenceEquals(cls.DirectSuperclasses, supersBeforeInit))
            {
                supers = new List<LispClass>(cls.DirectSuperclasses);
                ValidateSuperclasses(sym, metaclass, supers);
            }
        }
        // Skip finalization if a superclass, direct or not, is forward-referenced:
        // the class is finalized when that one is defined (RefinalizeDependents).
        bool hasForwardRef = cls.IsForwardReferenced;
        foreach (var s in supers)
        {
            if (ForwardReferencedAncestor(s) != null) { hasForwardRef = true; break; }
        }
        if (!hasForwardRef && !IsClassFinalized(cls))
        {
            // Under a custom metaclass the class is finalized through the
            // FINALIZE-INHERITANCE generic function, as a redefinition already is,
            // so a metaclass's methods on it run for a new class too (clsql computes
            // its key slots in a FINALIZE-INHERITANCE :AFTER method). A class the
            // metaclass already finalized from its INITIALIZE-INSTANCE is not
            // finalized again.
            if (metaclass != null && Startup.Sym("FINALIZE-INHERITANCE").Function is LispFunction fi)
                fi.Invoke(new LispObject[] { cls });
            if (!IsClassFinalized(cls))
                cls.FinalizeClass();
        }
        return cls;
    }

    /// <summary>The redefinition half of MakeClassCore: REINITIALIZE-INSTANCE on the
    /// existing class with :DIRECT-SUPERCLASSES (as given), :DIRECT-SLOTS (canonical
    /// plists) and the other initargs, the shape SBCL passes. :NAME is not passed
    /// (the name is not changing) and the metaclass's default initargs do not apply,
    /// as for any REINITIALIZE-INSTANCE.</summary>
    private static LispClass RedefineByReinitialize(LispClass existing, LispObject supersList,
        List<SlotDefinition> slots, List<(LispObject Plist, SlotDefinition Slot)>? slotPlists,
        LispObject[]? extraInitargs)
    {
        // No plists means the slots are DEFCLASS's, whose expansion defines their
        // readers and writers after this returns.
        HashSet<(Symbol, LispObject)>? defclassAccessors = slotPlists == null ? AccessorKeys(slots) : null;
        if (slotPlists == null)
        {
            existing.StaleAccessorMethods = existing.AccessorMethods;
            existing.AccessorMethods = new();
        }
        if (slotPlists == null)
        {
            slotPlists = new List<(LispObject, SlotDefinition)>(slots.Count);
            foreach (var sd in slots) slotPlists.Add((CanonicalSlotPlist(sd), sd));
        }
        // DIRECT-SLOT-DEFINITION-CLASS is consulted for each new slot, as on first
        // definition.
        foreach (var (_, sd) in slotPlists) ApplyDirectSlotDefinitionClass(existing, sd);
        var plistItems = new LispObject[slotPlists.Count];
        for (int i = 0; i < plistItems.Length; i++) plistItems[i] = slotPlists[i].Plist;
        var riArgs = new List<LispObject> {
            existing,
            Startup.Keyword("DIRECT-SUPERCLASSES"), supersList,
            Startup.Keyword("DIRECT-SLOTS"), List(plistItems),
        };
        if (extraInitargs != null) riArgs.AddRange(extraInitargs);
        var riFn = Startup.Sym("REINITIALIZE-INSTANCE").Function as LispFunction
            ?? throw new LispErrorException(new LispError("REINITIALIZE-INSTANCE not defined"));
        existing.PendingSlotPlists = PendingSlotPlistsOf(slotPlists);
        existing.DefclassAccessors = defclassAccessors;
        try { riFn.Invoke(riArgs.ToArray()); }
        finally { existing.PendingSlotPlists = null; existing.DefclassAccessors = null; }
        return existing;
    }

    /// <summary>The superclass a class with no direct superclasses gets: STANDARD-OBJECT,
    /// or FUNCALLABLE-STANDARD-OBJECT under FUNCALLABLE-STANDARD-CLASS (AMOP). A class
    /// whose instances are callable is not a STANDARD-OBJECT.</summary>
    private static bool IsForwardReferencedMetaclass(LispClass? metaclass)
        => metaclass != null
           && metaclass.ClassPrecedenceList.Any(m => IsSystemClass(m, "FORWARD-REFERENCED-CLASS"));

    private static Symbol DefaultDirectSuperclassName(LispClass? metaclass)
    {
        bool funcallable = metaclass != null
            && metaclass.ClassPrecedenceList.Any(m => IsSystemClass(m, "FUNCALLABLE-STANDARD-CLASS"));
        return Startup.Sym(funcallable ? "FUNCALLABLE-STANDARD-OBJECT" : "STANDARD-OBJECT");
    }

    /// <summary>Validate each superclass via the VALIDATE-SUPERCLASS generic function
    /// (AMOP), before finalization. The class being defined is represented by a
    /// temporary LispClass with the right metaclass, which is what the GF dispatches on.</summary>
    private static void ValidateSuperclasses(Symbol sym, LispClass? metaclass, IReadOnlyList<LispClass> supers)
    {
        if (Startup.Sym("VALIDATE-SUPERCLASS").Function is not LispFunction validateGF) return;
        // A placeholder class is never finalized, so it has no superclass pairing to check.
        if (IsForwardReferencedMetaclass(metaclass)) return;
        var tempCls = new LispClass(sym, Array.Empty<SlotDefinition>(), supers.ToArray());
        tempCls.Metaclass = metaclass; // needed for validate-superclass dispatch on (c mm)
        foreach (var super in supers)
        {
            // Skip T: always valid
            if (super.Name.Name == "T") continue;
            // A superclass that has not been defined yet is a placeholder, so there
            // is no metaclass pair to validate: what its class will be is exactly
            // what is not known. The class stays unfinalized until the real
            // definition arrives, which is when the pair becomes a real question.
            if (super.IsForwardReferenced) continue;
            var result = validateGF.Invoke(new LispObject[] { tempCls, super });
            if (result is Nil)
                throw new LispErrorException(new LispError(
                    $"DEFCLASS {sym.Name}: validate-superclass rejected superclass {super.Name.Name}"));
        }
    }

    public static LispObject MakeSlotDef(LispObject name, LispObject initargs, LispObject initformThunk)
    {
        if (name is not Symbol sym)
            throw new LispErrorException(new LispTypeError("MAKE-SLOT-DEF: name must be a symbol", name));

        // initargs can be: NIL (no initargs), a single Symbol (backward compat), or a Lisp list of symbols
        var iaList = new List<Symbol>();
        if (initargs is Symbol s)
        {
            iaList.Add(s);
        }
        else
        {
            var cur = initargs;
            while (cur is Cons c)
            {
                if (c.Car is Symbol ia)
                    iaList.Add(ia);
                else if (c.Car is Nil)
                    iaList.Add(Startup.NIL_SYM);
                else if (c.Car is T)
                    iaList.Add(Startup.T_SYM);
                cur = c.Cdr;
            }
        }

        LispFunction? thunk = initformThunk is LispFunction f ? f : null;
        return new SlotDefinition(sym, iaList.ToArray(), thunk);
    }

    public static LispObject MakeSlotDefWithAllocation(LispObject name, LispObject initargs, LispObject initformThunk, LispObject allocation)
    {
        var sd = (SlotDefinition)MakeSlotDef(name, initargs, initformThunk);
        if (allocation is Symbol allSym && allSym.Name == "CLASS")
            sd.IsClassAllocation = true;
        return sd;
    }

    /// <summary>Attach the canonical slot-option plist (a Lisp list :key val ...) captured
    /// from a DEFCLASS slot specifier under a custom metaclass. Returns the slotd so the
    /// DEFCLASS expansion can wrap %make-slot-def transparently.</summary>
    public static LispObject SetSlotDefRawOptions(LispObject slotdObj, LispObject options)
    {
        if (slotdObj is SlotDefinition sd)
        {
            sd.RawOptions = options;
            // DEFCLASS passes a metaclass-defined :allocation through here, since the
            // two standard ones are already on the bool. Reading it now means
            // EFFECTIVE-SLOT-DEFINITION-CLASS and SLOT-DEFINITION-ALLOCATION see the
            // keyword the user wrote.
            for (var c = options; c is Cons rc && rc.Cdr is Cons rv; c = rv.Cdr)
                if (rc.Car is Symbol k && k.Name == "ALLOCATION" && rv.Car is Symbol av
                    && av.Name != "INSTANCE" && av.Name != "CLASS")
                    sd.Allocation = av;
        }
        return slotdObj;
    }

    /// <summary>Attach the introspectable slot attributes DEFCLASS parses but did not
    /// used to pass on: the :reader/:accessor names, the :writer/(setf accessor) names,
    /// and the declared :type. Returns the slotd so the DEFCLASS expansion can wrap
    /// %make-slot-def transparently, like %SLOT-DEF-RAW-OPTIONS.</summary>
    public static LispObject SetSlotDefAttrs(LispObject slotdObj, LispObject readers,
                                             LispObject writers, LispObject slotType,
                                             LispObject initform)
    {
        if (slotdObj is SlotDefinition sd)
        {
            sd.Readers = ListToArray(readers);
            sd.Writers = ListToArray(writers);
            sd.SlotType = slotType is Nil ? T.Instance : slotType;
            sd.Initform = initform;
            sd.ConstantInitValue = sd.InitformThunk != null ? LiteralInitformValue(initform) : null;
        }
        return slotdObj;
    }

    // The value of an initform whose every evaluation yields the same object.
    private static LispObject? LiteralInitformValue(LispObject form) => form switch
    {
        Number or LispChar or Nil or T => form,
        Symbol s when s.HomePackage?.Name == "KEYWORD" => form,
        Cons { Car: Symbol q, Cdr: Cons { Car: Symbol v, Cdr: Nil } }
            when ReferenceEquals(q, Startup.Sym("QUOTE")) => v,
        _ => null,
    };

    /// <summary>The slot :documentation, attached by its own call rather than as a
    /// sixth argument to SetSlotDefAttrs: a compiled FASL names the runtime method it
    /// calls, so widening that signature stops every FASL built before the change
    /// from loading. A slot with no documentation emits no call at all.</summary>
    public static LispObject SetSlotDefDocumentation(LispObject slotdObj, LispObject documentation)
    {
        if (slotdObj is SlotDefinition sd) sd.Documentation = documentation;
        return slotdObj;
    }

    /// <summary>The :documentation of a slot definition, NIL when it has none.</summary>
    public static LispObject SlotDefinitionDocumentation(LispObject arg)
        => RequireSlotDef("SLOT-DEFINITION-DOCUMENTATION", arg).Documentation;

    /// <summary>The slot initargs as Lisp values: the table entries for T and NIL
    /// stand for those symbols internally and must not reach user code.</summary>
    internal static LispObject[] CanonicalInitargs(Symbol[] initargs)
    {
        var items = new LispObject[initargs.Length];
        for (int i = 0; i < initargs.Length; i++) items[i] = CanonicalizeSymbol(initargs[i]);
        return items;
    }

    /// <summary>Convert a Lisp list to an array. Counts the list first and fills one
    /// exact-size array. Growing a List and copying it out allocates the List, its
    /// backing store (rounded up to a power of two) and the copy, all to produce one
    /// array whose length a free extra walk already knew.</summary>
    private static LispObject[] ListToArray(LispObject list)
    {
        int n = 0;
        for (var cur = list; cur is Cons c; cur = c.Cdr) n++;
        if (n == 0) return Array.Empty<LispObject>();
        var items = new LispObject[n];
        int i = 0;
        for (var cur = list; cur is Cons c; cur = c.Cdr) items[i++] = c.Car;
        return items;
    }

    /// <summary>AMOP DIRECT-SLOT-DEFINITION-CLASS protocol: for each direct slot of a class
    /// with a custom metaclass, ask the GF which class the direct slot definition should be.
    /// A non-standard answer becomes the slot's MetaClass and its extra Lisp slots are
    /// initialized from the slot's options.</summary>
    /// <summary>Install the class-structure initargs of a class metaobject
    /// (:DIRECT-SUPERCLASSES, :DIRECT-SLOTS, :DIRECT-DEFAULT-INITARGS) from an initarg
    /// span. Leftmost occurrence of each wins, as for any initarg; an absent one leaves
    /// that part of the class alone. A slot plist that MakeClassCore or DEFCLASS
    /// handed out (PendingSlotPlists) keeps its slot definition, and what
    /// DIRECT-SLOT-DEFINITION-CLASS made of it; any other plist becomes a new slot
    /// definition. The class is not finalized here.</summary>
    private static void InstallClassInitargs(LispClass klass, LispObject[] initargs, int start)
    {
        bool supersSeen = false, slotsSeen = false, dfltSeen = false;
        for (int i = start; i + 1 < initargs.Length; i += 2)
        {
            var key = initargs[i];
            if (!supersSeen && key == Startup.Keyword("DIRECT-SUPERCLASSES"))
            {
                supersSeen = true;
                var newSupers = new List<LispClass>();
                for (var c = initargs[i + 1]; c is Cons cc; c = cc.Cdr)
                {
                    var sup = cc.Car is Symbol sn && cc.Car is not Nil ? FindClassOrNil(sn) : cc.Car;
                    if (sup is LispClass sc) newSupers.Add(sc);
                    else
                        throw new LispErrorException(new LispTypeError(
                            "SHARED-INITIALIZE: :DIRECT-SUPERCLASSES element is not a class", cc.Car));
                }
                if (newSupers.Count == 0 && !klass.IsForwardReferenced
                    && _classRegistry.TryGetValue(DefaultDirectSuperclassName(klass.Metaclass), out var dflt))
                    newSupers.Add(dflt);
                if (!newSupers.SequenceEqual(klass.DirectSuperclasses))
                    klass.DirectSuperclasses = newSupers.ToArray();
            }
            else if (!slotsSeen && key == Startup.Keyword("DIRECT-SLOTS"))
            {
                slotsSeen = true;
                var pending = klass.PendingSlotPlists;
                var newSlots = new List<SlotDefinition>();
                var fresh = new List<SlotDefinition>();
                for (var c = initargs[i + 1]; c is Cons cc; c = cc.Cdr)
                {
                    SlotDefinition? sd = null;
                    if (pending != null)
                        foreach (var (pl, psd, snap) in pending)
                            if (ReferenceEquals(pl, cc.Car))
                            {
                                if (PlistMatchesSnapshot(pl, snap)) sd = psd;
                                break;
                            }
                    if (sd == null)
                    {
                        sd = SlotDefFromCanonicalPlist(cc.Car)
                            ?? throw new LispErrorException(new LispTypeError(
                                "SHARED-INITIALIZE: :DIRECT-SLOTS element is not a slot specification", cc.Car));
                        fresh.Add(sd);
                    }
                    newSlots.Add(sd);
                }
                if (!newSlots.SequenceEqual(klass.DirectSlots))
                    klass.DirectSlots = newSlots.ToArray();
                if (klass.Metaclass != null)
                    foreach (var sd in fresh) ApplyDirectSlotDefinitionClass(klass, sd);
                NotePendingAccessors(klass, newSlots);
            }
            else if (!dfltSeen && key == Startup.Keyword("DIRECT-DEFAULT-INITARGS"))
            {
                dfltSeen = true;
                klass.DirectDefaultInitargs = ParseCanonicalDefaultInitargs(initargs[i + 1]);
            }
        }
    }

    /// <summary>The (slot name, reader or writer) pairs of SLOTS, the key DEFCLASS's
    /// own accessor methods are recognized by. A writer (SETF x) is keyed by the list
    /// itself compared with EQUAL, which AccessorKeyComparer does.</summary>
    private static HashSet<(Symbol Slot, LispObject Fn)> AccessorKeys(IEnumerable<SlotDefinition> slots)
    {
        var keys = new HashSet<(Symbol, LispObject)>(AccessorKeyComparer.Instance);
        foreach (var sd in slots)
        {
            foreach (var r in sd.Readers) keys.Add((sd.Name, r));
            foreach (var w in sd.Writers) keys.Add((sd.Name, w));
        }
        return keys;
    }

    private sealed class AccessorKeyComparer : IEqualityComparer<(Symbol Slot, LispObject Fn)>
    {
        internal static readonly AccessorKeyComparer Instance = new();
        public bool Equals((Symbol Slot, LispObject Fn) a, (Symbol Slot, LispObject Fn) b)
            => ReferenceEquals(a.Slot, b.Slot) && IsTrueEqual(a.Fn, b.Fn);
        public int GetHashCode((Symbol Slot, LispObject Fn) k)
            => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(k.Slot)
               ^ (k.Fn is Cons fc && fc.Cdr is Cons fn2
                    ? System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(fn2.Car) * 31
                    : System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(k.Fn));
    }

    /// <summary>AMOP has class initialization define the reader and writer methods
    /// named in the direct slot definitions. DEFCLASS defines them in its expansion,
    /// so only the ones its expansion does not know about are noted here: those of
    /// slots a metaclass method added or rewrote, and all of them when the slots came
    /// from ENSURE-CLASS or REINITIALIZE-INSTANCE.</summary>
    private static void NotePendingAccessors(LispClass klass, IEnumerable<SlotDefinition> slots)
    {
        var known = klass.DefclassAccessors;
        List<(Symbol, LispObject, LispObject)>? pending = null;
        foreach (var sd in slots)
        {
            var readers = new List<LispObject>();
            var writers = new List<LispObject>();
            foreach (var r in sd.Readers)
                if (known == null || !known.Contains((sd.Name, r))) readers.Add(r);
            foreach (var w in sd.Writers)
                if (known == null || !known.Contains((sd.Name, w))) writers.Add(w);
            if (readers.Count == 0 && writers.Count == 0) continue;
            (pending ??= new()).Add((sd.Name, List(readers.ToArray()), List(writers.ToArray())));
        }
        klass.PendingAccessors = pending;
    }

    /// <summary>Define the methods NotePendingAccessors left for SOURCE on TARGET, the
    /// class registered under the name (the same object, or the existing class a
    /// redefinition was copied into). A class that is not reachable by its name
    /// (anonymous, or not registered) gets none: the methods are defined through
    /// DEFMETHOD on the class name.</summary>
    private static void DefinePendingAccessors(LispClass source, LispClass target)
    {
        var pending = source.PendingAccessors;
        if (pending == null) return;
        source.PendingAccessors = null;
        if (FindClassOrNil(target.Name) != target) return;
        var fn = DotCL.Emitter.CilAssembler.GetFunction("%DEFINE-SLOT-ACCESSORS");
        foreach (var (slot, readers, writers) in pending)
            fn.Invoke(new LispObject[] { target.Name, slot, readers, writers });
    }

    /// <summary>The slot plists handed to INITIALIZE-INSTANCE or REINITIALIZE-INSTANCE,
    /// each with its slot definition and a copy of its elements, so that a plist a
    /// method edits in place (RPLACD into it, as Mito does to mark a slot as a ghost)
    /// is recognized as changed.</summary>
    private static List<(LispObject, SlotDefinition, LispObject[])> PendingSlotPlistsOf(
        List<(LispObject Plist, SlotDefinition Slot)> slotPlists)
    {
        var result = new List<(LispObject, SlotDefinition, LispObject[])>(slotPlists.Count);
        foreach (var (pl, sd) in slotPlists)
        {
            var items = new List<LispObject>();
            for (var c = pl; c is Cons cc; c = cc.Cdr) items.Add(cc.Car);
            result.Add((pl, sd, items.ToArray()));
        }
        return result;
    }

    private static bool PlistMatchesSnapshot(LispObject plist, LispObject[] snapshot)
    {
        int i = 0;
        for (var c = plist; c is Cons cc; c = cc.Cdr, i++)
            if (i >= snapshot.Length || !ReferenceEquals(cc.Car, snapshot[i])) return false;
        return i == snapshot.Length;
    }

    private static void ApplyDirectSlotDefinitionClass(LispClass cls)
    {
        foreach (var slot in cls.DirectSlots)
            ApplyDirectSlotDefinitionClass(cls, slot);
    }

    private static void ApplyDirectSlotDefinitionClass(LispClass cls, SlotDefinition slot)
    {
        if (Startup.Sym("DIRECT-SLOT-DEFINITION-CLASS").Function is not LispFunction gf)
            return;
        var stdDirect = FindClassOrNil(Startup.Sym("STANDARD-DIRECT-SLOT-DEFINITION")) as LispClass;
        {
            // AMOP: the initargs are the canonicalized slot specification, the same
            // plist DEFCLASS passes in :DIRECT-SLOTS (:name, :initform, :initfunction,
            // :initargs, :readers, :writers, :documentation, ... and the slot's
            // non-standard options).
            var initargs = new List<LispObject>();
            for (var c = CanonicalSlotPlist(slot); c is Cons rc; c = rc.Cdr)
                initargs.Add(rc.Car);
            var callArgs = new LispObject[initargs.Count + 1];
            callArgs[0] = cls;
            for (int i = 0; i < initargs.Count; i++) callArgs[i + 1] = initargs[i];
            var result = gf.Invoke(callArgs);
            var dc = result as LispClass
                ?? (result is Symbol name ? FindClassOrNil(name) as LispClass : null);
            if (dc != null) CheckSlotOptionsKnown(cls, slot, dc);
            if (dc != null && !ReferenceEquals(dc, stdDirect))
            {
                slot.MetaClass = dc;
                InitializeSlotDefinitionInstance(slot, initargs);
            }
        }
    }

    /// <summary>A slot option that is not an initarg of the slot definition class the
    /// metaclass chose is an invalid initialization argument for that class
    /// (AMOP: the options become initargs of the slot definition).</summary>
    private static void CheckSlotOptionsKnown(LispClass cls, SlotDefinition slot, LispClass slotdClass)
    {
        for (var o = slot.RawOptions; o is Cons oc && oc.Cdr is Cons ov; o = ov.Cdr)
        {
            if (oc.Car is not Symbol key || key.Name is "TYPE" or "ALLOCATION") continue;
            bool known = false;
            foreach (var es in slotdClass.EffectiveSlots)
            {
                foreach (var ia in es.Initargs)
                    if (ia.Name == key.Name) { known = true; break; }
                if (known) break;
            }
            if (!known)
                throw new LispErrorException(new LispProgramError(
                    $"Invalid initialization argument {key} for slot {slot.Name.Name} of class "
                    + $"{cls.Name.Name}: not an initarg of {slotdClass.Name.Name}"));
        }
    }

    /// <summary>The standard slot-definition initargs that can still change after a
    /// SlotDefinition is made (:type :initform :documentation :allocation, and
    /// :readers :writers for a direct one), each from its leftmost occurrence and
    /// only when given.</summary>
    private static void ApplySlotDefinitionInitargs(SlotDefinition slotd, LispObject[] initargs, int start)
    {
        bool typeSeen = false, formSeen = false, docSeen = false, allocSeen = false,
             readersSeen = false, writersSeen = false;
        for (int i = start; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is not Symbol k || k.HomePackage?.Name != "KEYWORD") continue;
            var v = initargs[i + 1];
            switch (k.Name)
            {
                case "TYPE" when !typeSeen: typeSeen = true; slotd.SlotType = v; break;
                case "INITFORM" when !formSeen: formSeen = true; slotd.Initform = v; break;
                case "DOCUMENTATION" when !docSeen: docSeen = true; slotd.Documentation = v; break;
                case "ALLOCATION" when !allocSeen:
                    allocSeen = true;
                    if (v is Symbol alloc)
                    {
                        slotd.IsClassAllocation = alloc == Startup.Keyword("CLASS");
                        slotd.Allocation = alloc == Startup.Keyword("INSTANCE") || slotd.IsClassAllocation
                            ? null : alloc;
                    }
                    break;
                case "READERS" when !readersSeen && !slotd.IsEffective:
                    readersSeen = true; slotd.Readers = ListToArray(v); break;
                case "WRITERS" when !writersSeen && !slotd.IsEffective:
                    writersSeen = true; slotd.Writers = ListToArray(v); break;
            }
        }
    }

    /// <summary>Run INITIALIZE-INSTANCE (the generic function, so a user class's
    /// :around, :before and :after methods apply) on a slot definition of a
    /// user-defined slot-definition class.</summary>
    private static void InitializeSlotDefinitionInstance(SlotDefinition slotd, IReadOnlyList<LispObject> initargs)
    {
        if (Startup.Sym("INITIALIZE-INSTANCE").Function is not LispFunction ii) return;
        var args = new LispObject[initargs.Count + 1];
        args[0] = slotd;
        for (int i = 0; i < initargs.Count; i++) args[i + 1] = initargs[i];
        ii.Invoke(args);
    }

    /// <summary>Initialize the Lisp-level slots a custom slot-definition class introduces.
    /// For each effective slot of <paramref name="metaClass"/>, take the value from a matching
    /// initarg in <paramref name="optionsPlist"/> if provided, otherwise evaluate its initform.
    /// Used for both direct (options from the slot spec) and effective (options empty, initforms
    /// only: e.g. McCLIM's DYNAMIC slot reads a dynamically-bound special) slot defs.</summary>
    internal static void InitializeSlotdExtraSlots(SlotDefinition slotd, LispClass metaClass, LispObject? optionsPlist)
    {
        foreach (var es in metaClass.EffectiveSlots)
        {
            LispObject? value = null;
            bool found = false;
            // Match a provided option whose key is one of this slot's initargs.
            for (var c = optionsPlist; c is Cons kc && kc.Cdr is Cons vc; c = vc.Cdr)
            {
                if (kc.Car is Symbol key && Array.Exists(es.Initargs, ia => ia.Name == key.Name))
                {
                    value = vc.Car;
                    found = true;
                    break;
                }
            }
            if (!found && es.InitformThunk is { } thunk)
            {
                value = MultipleValues.Primary(thunk.Invoke(Array.Empty<LispObject>()));
                found = true;
            }
            if (found)
                (slotd.EnsureExtraSlots())[es.Name.Name] = value;
        }
    }

    /// <summary>
    /// Set direct default initargs on a class. initargsList is a flat list: (key1 thunk1 key2 thunk2 ...).
    /// After setting, recomputes effective default initargs from CPL.
    /// </summary>
    public static LispObject SetClassDefaultInitargs(LispObject classObj, LispObject initargsList)
    {
        if (classObj is not LispClass cls)
            throw new LispErrorException(new LispTypeError("SET-CLASS-DEFAULT-INITARGS: not a class", classObj));

        // Two shapes are accepted. (key thunk ...) is what a FASL compiled before the
        // initform source was carried emits; (key form thunk ...) is what DEFCLASS
        // emits now. They are told apart by what follows the key, and a form is never
        // a function object -- it is source. Accepting both is what keeps an older
        // FASL loading, which widening this method's signature would not have.
        var result = new List<(Symbol Key, LispObject Form, LispFunction Thunk)>();
        var cur = initargsList;
        while (cur is Cons c1)
        {
            var key = c1.Car as Symbol;
            if (c1.Cdr is Cons c2)
            {
                if (c2.Car is LispFunction oldShapeThunk)
                {
                    if (key != null) result.Add((key, Nil.Instance, oldShapeThunk));
                    cur = c2.Cdr;
                    continue;
                }
                var form = c2.Car;
                var thunk = (c2.Cdr as Cons)?.Car as LispFunction;
                if (key != null && thunk != null)
                    result.Add((key, form, thunk));
                cur = c2.Cdr is Cons c3 ? c3.Cdr : Nil.Instance;
            }
            else break;
        }
        cls.DirectDefaultInitargs = result.ToArray();
        cls.ComputeEffectiveDefaultInitargs();
        cls.SimpleInitChecked = false;
        cls.SharedInitSimpleChecked = false;
        // Subclasses inherit and cache these defaults: rebuild their plans too.
        RefinalizeDependents(cls);
        return classObj;
    }
}
