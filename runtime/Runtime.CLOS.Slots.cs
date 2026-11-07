using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    public static LispObject ClassOf(LispObject obj)
    {
        if (obj is LispInstance inst)
            return inst.Class;
        if (obj is LispInstanceCondition lic)
            return lic.Instance.Class;
        // Native (runtime-signaled) condition objects: e.g. the DIVISION-BY-ZERO
        // from (/ 1 0), the TYPE-ERROR from (car 3): carry their class name in
        // ConditionTypeName, which type-of/typep already use. Resolve the same
        // registered class here so class-of agrees, instead of falling through the
        // switch to T. LispInstanceCondition (CLOS-backed user conditions)
        // is handled above, so this only catches the native LispCondition family.
        if (obj is LispCondition cond)
        {
            if (_classRegistry.TryGetValue(Startup.Sym(cond.ConditionTypeName), out var cc))
                return cc;
            if (_classRegistry.TryGetValue(Startup.Sym("CONDITION"), out var cbase))
                return cbase;
            return Nil.Instance;
        }
        // Method combination metaobjects: one built-in class, no metaclass of
        // their own.
        if (obj is MethodCombinationObject)
        {
            if (_classRegistry.TryGetValue(Startup.Sym("METHOD-COMBINATION"), out var mc))
                return mc;
            return Nil.Instance;
        }
        if (obj is LispRestart)
        {
            if (_classRegistry.TryGetValue(Startup.Sym("RESTART"), out var restartClass))
                return restartClass;
            return Nil.Instance;
        }
        if (obj is LispDotNetObject dn)
            return EnsureDotNetTypeClass(dn.Type);
        // Slot-definition metaobject with a customized class (direct-/effective-
        // slot-definition-class). Falls through to the name-based default below
        // when MetaClass is null.
        if (obj is SlotDefinition sdmc && sdmc.MetaClass != null)
            return sdmc.MetaClass;
        // LispClass objects: return their metaclass
        if (obj is LispClass lc)
        {
            // A class named as a superclass before it was defined is a placeholder, and
            // AMOP gives placeholders their own class. Defining the class for real
            // clears the flag on this same object, which is the CHANGE-CLASS the
            // protocol describes: the identity a dependent class already holds does
            // not move.
            if (lc.IsForwardReferenced && lc.Metaclass == null
                && _classRegistry.TryGetValue(Startup.Sym("FORWARD-REFERENCED-CLASS"),
                                              out var fwdMeta))
                return fwdMeta;
            // Custom metaclass takes priority
            if (!lc.IsBuiltIn && !lc.IsStructureClass && lc.Metaclass != null)
                return lc.Metaclass;
            string metaName = lc.IsBuiltIn && !ReferenceEquals(lc.Name, Startup.Sym("STRUCTURE-OBJECT")) ? "BUILT-IN-CLASS"
                            : ReferenceEquals(lc.Name, Startup.Sym("STRUCTURE-OBJECT")) ? "STRUCTURE-CLASS"
                            : lc.IsStructureClass ? "STRUCTURE-CLASS"
                            : "STANDARD-CLASS";
            if (_classRegistry.TryGetValue(Startup.Sym(metaName), out var meta)) return meta;
            return Nil.Instance;
        }
        if (obj is GenericFunction gf)
        {
            if (gf.StoredClass != null) return gf.StoredClass;
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-GENERIC-FUNCTION"), out var sgfClass)) return sgfClass;
            return Nil.Instance;
        }
        if (obj is EqlSpecializer)
        {
            if (_classRegistry.TryGetValue(Startup.Sym("EQL-SPECIALIZER"), out var eqlCls))
                return eqlCls;
            return Nil.Instance;
        }
        if (obj is LispMethod lmmc && lmmc.MetaClass != null)
            return lmmc.MetaClass;
        if (obj is LispMethod)
        {
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-METHOD"), out var methodClass)) return methodClass;
            return Nil.Instance;
        }
        // Struct instances: return the struct's registered class
        if (LispStruct.TryAs(obj, out var ls))
        {
            if (_classRegistry.TryGetValue(ls.TypeName, out var structClass))
                return structClass;
            // Fallback to STRUCTURE-OBJECT if type not registered
            if (_classRegistry.TryGetValue(Startup.Sym("STRUCTURE-OBJECT"), out var soClass))
                return soClass;
            return Nil.Instance;
        }
        // Built-in types return their class if registered
        string typeName = obj switch
        {
            Fixnum => "FIXNUM",
            Bignum => "BIGNUM",
            Ratio => "RATIO",
            LispComplex => "COMPLEX",
            SingleFloat => "SINGLE-FLOAT",
            DoubleFloat => "DOUBLE-FLOAT",
            LispString => "STRING",
            LispChar => "CHARACTER",
            Nil => "NULL",
            Symbol or T => "SYMBOL",
            Cons => "CONS",
            GenericFunction => "STANDARD-GENERIC-FUNCTION",
            LispFunction => "FUNCTION",
            LispHashTable => "HASH-TABLE",
            LispVector v when v.IsCharVector && v.Rank == 1
                && v.ElementTypeName is "BASE-CHAR" or "STANDARD-CHAR" =>
                v.IsSimple ? "SIMPLE-BASE-STRING" : "BASE-STRING",
            LispVector v when v.IsCharVector && v.Rank == 1 => "STRING",
            // A bit vector is an instance of BIT-VECTOR (and SIMPLE-BIT-VECTOR when
            // simple), so a method specialized on either is applicable to it.
            LispVector v when v.IsBitVector && v.Rank == 1 =>
                v.IsSimple ? "SIMPLE-BIT-VECTOR" : "BIT-VECTOR",
            LispVector v when v.Rank != 1 => "ARRAY",
            LispVector v when v.IsSimple && v.ElementTypeName == "T" => "SIMPLE-VECTOR",
            LispVector => "VECTOR",
            LispLogicalPathname => "LOGICAL-PATHNAME",
            LispPathname => "PATHNAME",
            LispReadtable => "READTABLE",
            Package => "PACKAGE",
            LispRandomState => "RANDOM-STATE",
            SlotDefinition sd when sd.IsEffective => "STANDARD-EFFECTIVE-SLOT-DEFINITION",
            SlotDefinition => "STANDARD-DIRECT-SLOT-DEFINITION",
            LispStream s when s.StreamTypeName != null => s.StreamTypeName,
            LispStream => "STREAM",
            _ => "T"
        };
        if (_classRegistry.TryGetValue(Startup.Sym(typeName), out var cls))
            return cls;
        return Nil.Instance;
    }

    public static LispObject ClassName(LispObject cls)
    {
        if (cls is LispClass lc)
            return lc.NameCleared ? Nil.Instance : CanonicalizeSymbol(lc.Name);
        throw new LispErrorException(new LispTypeError("CLASS-NAME: not a class", cls));
    }

    public static LispObject MakeInstanceRaw(LispObject cls)
    {
        if (cls is not LispClass lc)
            throw new LispErrorException(new LispTypeError("ALLOCATE-INSTANCE: not a class", cls));
        // A built-in class has no instances to make (SBCL signals here too).
        if (lc.IsBuiltIn && !lc.IsStructureClass && !ReferenceEquals(lc.Name, Startup.Sym("STRUCTURE-OBJECT")))
            throw new LispErrorException(new LispError("ALLOCATE-INSTANCE: cannot allocate an instance of the built-in class " + lc.Name.Name));
        // A structure-class instance must be a LispStruct, not a CLOS LispInstance:
        // equalp compares two LispStructs slot-by-slot and returns NIL if either side
        // is a LispInstance, and make-load-form-saving-slots emits allocate-instance as
        // a struct's creation form: so an allocate-instance'd struct that round-trips
        // through a fasl would otherwise never be equalp to a normally-built one (broke
        // Coalton's equalp-on-KIND type checking).
        //
        // Slots are left NIL (the dotcl stand-in for unbound): allocate-instance must
        // NOT run slot initforms (that is initialize-instance's job). Running them broke
        // the common required-slot idiom (id (required 'id) :read-only t), where the
        // initform signals: so allocate-instance, and thus the make-load-form-saving-slots
        // creation form, errored. The INIT form restores real slot values on the round-trip.
        if (lc.IsStructureClass)
        {
            var slots = new LispObject[lc.EffectiveSlots.Length];
            for (int i = 0; i < slots.Length; i++) slots[i] = Nil.Instance;
            return LispStruct.Create(lc.Name, slots);
        }
        // A metaobject class needs its C# representation here too, for the same reason
        // MAKE-INSTANCE has a branch for it: ALLOCATE-INSTANCE dispatches on
        // (class-of cls) = STANDARD-CLASS, so a method specialized on STANDARD-METHOD or
        // STANDARD-GENERIC-FUNCTION never becomes applicable. Without it, allocating a
        // user-defined method class produced an object that ADD-METHOD rejects and whose
        // slots INITIALIZE-INSTANCE leaves untouched. Checked after the structure-class
        // branch so struct allocation keeps its own path.
        if (HasSpecializedAllocator(lc) && AllocateSpecialized(lc) is { } metaobject)
            return metaobject;
        return new LispInstance(lc);
    }

    /// <summary>Allocate an instance through the ALLOCATE-INSTANCE GF so user overrides
    /// for a custom metaclass run (e.g. McCLIM seeds dynamic slots with their dvars).
    /// Falls back to a raw instance if the GF is missing or returns a non-instance.</summary>
    private static LispInstance AllocateViaGF(LispClass cls, LispObject[] initargs)
    {
        if (Startup.Sym("ALLOCATE-INSTANCE").Function is LispFunction aiFn)
        {
            var aArgs = new LispObject[1 + initargs.Length];
            aArgs[0] = cls;
            Array.Copy(initargs, 0, aArgs, 1, initargs.Length);
            if (MultipleValues.Primary(aiFn.Invoke(aArgs)) is LispInstance li)
                return li;
        }
        return new LispInstance(cls);
    }

    // Native (runtime-signaled) conditions: e.g. the DIVISION-BY-ZERO from (/ 1 0),
    // the TYPE-ERROR from (car 3), the SIMPLE-ERROR from (error "x"); are LispCondition
    // objects, not CLOS LispInstances. They still answer (typep c 'standard-object) => T
    // and class-of correctly , so MOP slot access must work too. The standard
    // condition slots are stored in C# fields rather than a Slots array; map them here so
    // slot-value/slot-boundp/slot-makunbound/slot-exists-p agree with the class's slots.
    // Returns true when NAME is a standard-condition slot applicable to COND; sets val to
    // the bound value, or null when the slot is currently unbound.
    internal static bool TryReadNativeConditionSlot(LispCondition cond, string name, out LispObject? val)
    {
        val = null;
        switch (name)
        {
            case "FORMAT-CONTROL":
                val = cond.FormatControl is Nil ? null : cond.FormatControl; return true;
            case "FORMAT-ARGUMENTS":
                val = cond.FormatArguments; return true;
            case "OPERATION":
                val = cond.OperationRef; return true;
            case "OPERANDS":
                val = cond.OperandsRef; return true;
            case "PACKAGE":
                val = cond.PackageRef; return true;
            case "PATHNAME":
                val = cond.FileErrorPathnameRef; return true;
            case "STREAM":
                val = cond.StreamErrorStreamRef; return true;
            case "DATUM":
                if (cond is LispTypeError te) { val = te.Datum; return true; }
                return false;
            case "EXPECTED-TYPE":
                if (cond is LispTypeError te2) { val = te2.ExpectedType; return true; }
                return false;
            case "NAME":
                if (cond is LispCellError ce) { val = ce.Name; return true; }
                return false;
        }
        return false;
    }

    // Write side of TryReadNativeConditionSlot: store val (null = make unbound) into the
    // native field backing standard-condition slot NAME. Returns true when handled.
    internal static bool TryWriteNativeConditionSlot(LispCondition cond, string name, LispObject? val)
    {
        switch (name)
        {
            case "FORMAT-CONTROL":   cond.FormatControl = val ?? Nil.Instance; return true;
            case "FORMAT-ARGUMENTS": cond.FormatArguments = val ?? Nil.Instance; return true;
            case "OPERATION":        cond.OperationRef = val; return true;
            case "OPERANDS":         cond.OperandsRef = val; return true;
            case "PACKAGE":          cond.PackageRef = val; return true;
            case "PATHNAME":         cond.FileErrorPathnameRef = val; return true;
            case "STREAM":           cond.StreamErrorStreamRef = val; return true;
            case "DATUM":
                if (cond is LispTypeError te) { te.Datum = val ?? Nil.Instance; return true; }
                return false;
            case "EXPECTED-TYPE":
                if (cond is LispTypeError te2) { te2.ExpectedType = val ?? Nil.Instance; return true; }
                return false;
            case "NAME":
                if (cond is LispCellError ce) { ce.Name = val ?? Nil.Instance; return true; }
                return false;
        }
        return false;
    }

    public static LispObject SlotValue(LispObject obj, LispObject slotName)
    {
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        string name = slotName switch { Symbol sym => sym.Name, _ => slotName.ToString() };
        if (obj is LispInstance fastInst) return SlotValueOfInstance(fastInst, slotName, name);
        if (LispStruct.TryAs(obj, out var st))
        {
            var stCls = FindClassOrNil(st.TypeName) as LispClass;
            if (stCls != null && stCls.SlotIndex.TryGetValue(name, out int stIdx) && stIdx < st.SlotCount)
                return st.GetSlot(stIdx) ?? Nil.Instance;
            throw new LispErrorException(new LispError($"SLOT-VALUE: no slot named {name} in struct {st.TypeName.Name}"));
        }
        // Slot-definition metaobjects with a custom slot-definition class carry their
        // extra Lisp slots in ExtraSlots (e.g. McCLIM's DYNAMIC slot).
        if (obj is SlotDefinition slotd)
        {
            if (slotd.ExtraSlots != null && slotd.ExtraSlots.TryGetValue(name, out var sv) && sv != null)
                return sv;
            if (Startup.Sym("SLOT-UNBOUND").Function is LispFunction su)
                return MultipleValues.Primary(su.Invoke(new LispObject[] {
                    ClassOf(slotd), slotd, slotName is Symbol ? slotName : Startup.Sym(name) }));
            throw new LispErrorException(new LispError(
                $"SLOT-VALUE: slot {name} is unbound in slot-definition"));
        }
        // A class metaobject under a custom metaclass holds the metaclass-added slots
        // in ExtraSlots, mirroring the SlotDefinition case above.
        if (obj is LispClass klass && klass.Metaclass != null)
        {
            if (klass.ExtraSlots != null && klass.ExtraSlots.TryGetValue(name, out var cv))
                return cv ?? Nil.Instance;
            if (klass.Metaclass.SlotIndex.ContainsKey(name)
                && Startup.Sym("SLOT-UNBOUND").Function is LispFunction csu)
                return MultipleValues.Primary(csu.Invoke(new LispObject[] {
                    klass.Metaclass, klass, slotName is Symbol ? slotName : Startup.Sym(name) }));
        }
        // A generic function under a user-defined generic function class holds that
        // class's slots the same way. The object is callable, so it cannot also be a
        // LispInstance; without this the class reports slots its instances cannot
        // hold, which is what closer-mop's own generic function class ran into.
        if (obj is GenericFunction gfObj && gfObj.StoredClass != null)
        {
            if (gfObj.ExtraSlots != null && gfObj.ExtraSlots.TryGetValue(name, out var gv))
                return gv ?? Nil.Instance;
            if (gfObj.StoredClass.SlotIndex.ContainsKey(name)
                && Startup.Sym("SLOT-UNBOUND").Function is LispFunction gsu)
                return MultipleValues.Primary(gsu.Invoke(new LispObject[] {
                    gfObj.StoredClass, gfObj, slotName is Symbol ? slotName : Startup.Sym(name) }));
        }
        // A method under a user-defined method class, the same way. Reached when
        // GENERIC-FUNCTION-METHOD-CLASS named a class that adds slots, which is how
        // metaobject code hangs its own information on a method.
        if (obj is LispMethod meth && meth.MetaClass != null)
        {
            if (meth.ExtraSlots != null && meth.ExtraSlots.TryGetValue(name, out var mv))
                return mv ?? Nil.Instance;
            if (meth.MetaClass.SlotIndex.ContainsKey(name)
                && Startup.Sym("SLOT-UNBOUND").Function is LispFunction msu)
                return MultipleValues.Primary(msu.Invoke(new LispObject[] {
                    meth.MetaClass, meth, slotName is Symbol ? slotName : Startup.Sym(name) }));
        }
        if (obj is LispCondition cond)
        {
            var ccls = ClassOf(cond) as LispClass;
            if (ccls != null && ccls.SlotIndex.ContainsKey(name))
            {
                TryReadNativeConditionSlot(cond, name, out var cval);
                if (cval != null) return cval;
                if (Startup.Sym("SLOT-UNBOUND").Function is LispFunction su)
                    return MultipleValues.Primary(su.Invoke(new LispObject[] {
                        ccls, cond, slotName is Symbol ? slotName : Startup.Sym(name) }));
                throw new LispErrorException(new LispError(
                    $"SLOT-UNBOUND: slot {name} is unbound in instance of {ccls.Name.Name}"));
            }
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction csm)
                return CallForPrimary(csm, ccls ?? (LispObject)Nil.Instance, cond,
                    slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-VALUE"));
            throw new LispErrorException(new LispError(
                $"SLOT-VALUE: no slot named {name} in condition {cond.ConditionTypeName}"));
        }
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SLOT-VALUE: not a CLOS instance", obj));
        return SlotValueOfInstance(inst, slotName, name);
    }

    /// <summary>The methods dotcl itself defines on SLOT-VALUE-USING-CLASS,
    /// (SETF SLOT-VALUE-USING-CLASS), SLOT-BOUNDP-USING-CLASS and
    /// SLOT-MAKUNBOUND-USING-CLASS: the direct slot access. Any other method on
    /// those generic functions is the program's.</summary>
    internal static readonly HashSet<LispMethod> SlotProtocolDefaults = new HashSet<LispMethod>();

    /// <summary>The four *-USING-CLASS generic functions above.</summary>
    internal static readonly List<GenericFunction> SlotProtocolGFs = new List<GenericFunction>();

    /// <summary>Set once a method other than dotcl's own default is added to
    /// one of <see cref="SlotProtocolGFs"/>; never cleared (after a
    /// REMOVE-METHOD the per-class answer is recomputed instead).</summary>
    private static bool _programSlotProtocolMethods;

    internal static void NoteSlotProtocolMethod(GenericFunction gf, LispMethod method)
    {
        if (!_programSlotProtocolMethods && !SlotProtocolDefaults.Contains(method)
            && SlotProtocolGFs.Contains(gf))
            System.Threading.Volatile.Write(ref _programSlotProtocolMethods, true);
    }

    /// <summary>
    /// True when slot access on an instance of CLS has to go through the
    /// *-USING-CLASS generic functions rather than reading the slot vector: the
    /// program has defined a method on one of those functions that applies to
    /// its instances, under its metaclass, for one of its slot definitions.
    /// On a STANDARD-CLASS class that is ordinary AMOP use -- an :AFTER method
    /// on (SETF SLOT-VALUE-USING-CLASS) specialized on the instance, to observe
    /// slot writes -- and skipping the protocol for it made such a method never
    /// run. A custom metaclass is asked the same question: one that defines no
    /// such method (it adds slots or options to its classes, say) would only
    /// reach dotcl's own defaults, which read and write the slot vector exactly
    /// as the direct path does. The answer is cached on the class against the
    /// method epoch, which every method added or removed anywhere, and every
    /// class finalization, advances, so the common case costs one compare.
    /// </summary>
    // Until the program defines a method on one of these functions, which most
    // never do, the answer is no for every class: a static read, inlined into
    // the slot access, and nothing else.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    internal static bool UsesSlotProtocol(LispClass cls)
        => _programSlotProtocolMethods && UsesSlotProtocolSlow(cls);

    private static bool UsesSlotProtocolSlow(LispClass cls)
    {
        int epoch = GenericFunction.MethodEpoch;
        long cached = cls.SlotProtocolCache;
        if ((cached & 2) != 0 && (int)(cached >> 2) == epoch) return (cached & 1) != 0;
        bool r = HasSlotProtocolMethodFor(cls);
        cls.SlotProtocolCache = ((long)epoch << 2) | 2L | (r ? 1L : 0L);
        return r;
    }

    private static bool HasSlotProtocolMethodFor(LispClass cls)
    {
        var svuc = Startup.Sym("SLOT-VALUE-USING-CLASS");
        return AnyApplicableSlotProtocolMethod(svuc.Function as GenericFunction, 1, cls)
            || AnyApplicableSlotProtocolMethod(svuc.SetfFunction as GenericFunction, 2, cls)
            || AnyApplicableSlotProtocolMethod(Startup.Sym("SLOT-BOUNDP-USING-CLASS").Function as GenericFunction, 1, cls)
            || AnyApplicableSlotProtocolMethod(Startup.Sym("SLOT-MAKUNBOUND-USING-CLASS").Function as GenericFunction, 1, cls);
    }

    /// <summary>Does GF hold a method of the program's that applies to a slot
    /// access on an instance of CLS: its class specializer (argument
    /// OBJECT-INDEX - 1) admits CLS's metaclass, its instance specializer admits
    /// CLS, and its slot specializer admits the class of one of CLS's effective
    /// slot definitions? An EQL specializer is counted as admitting: it can name
    /// one of them, and taking the protocol for a class that did not need it
    /// costs only speed.</summary>
    private static bool AnyApplicableSlotProtocolMethod(GenericFunction? gf, int objectIndex, LispClass cls)
    {
        if (gf == null) return false;
        foreach (var m in gf.Methods)
        {
            if (SlotProtocolDefaults.Contains(m)) continue;
            if (m.Specializers.Length <= objectIndex + 1) continue;
            // A method specialized on some other metaclass or slot class (the
            // usual reason to define one) does not apply, and counting it would
            // send every class's slot access through the protocol.
            if (!SpecializerAdmits(m.Specializers[objectIndex - 1], cls.Metaclass ?? StandardClassClass())) continue;
            if (!SlotSpecializerAdmits(m.Specializers[objectIndex + 1], cls)) continue;
            if (SpecializerAdmits(m.Specializers[objectIndex], cls)) return true;
        }
        return false;
    }

    /// <summary>Could SPEC match one of CLS's effective slot definitions? Each is
    /// of its own slot-definition class when the metaclass chose one
    /// (EFFECTIVE-SLOT-DEFINITION-CLASS), STANDARD-EFFECTIVE-SLOT-DEFINITION
    /// otherwise.</summary>
    private static bool SlotSpecializerAdmits(LispObject spec, LispClass cls)
    {
        bool standard = false;
        foreach (var slot in cls.EffectiveSlots)
        {
            if (slot.MetaClass is { } mc) { if (SpecializerAdmits(spec, mc)) return true; }
            else standard = true;
        }
        return standard && SpecializerAdmits(spec, StandardEffectiveSlotClass());
    }

    /// <summary>Could SPEC match an argument whose class is CLS? An EQL
    /// specializer (or a class not known here) is answered yes.</summary>
    private static bool SpecializerAdmits(LispObject spec, LispClass? cls)
    {
        if (spec is not LispClass sc || cls == null) return true;
        return ReferenceEquals(sc, cls) || Array.IndexOf(cls.ClassPrecedenceList, sc) >= 0;
    }

    private static LispClass? _standardClassClass, _standardEffectiveSlotClass;
    private static LispClass? StandardClassClass()
        => _standardClassClass ??= FindClassOrNil(Startup.Sym("STANDARD-CLASS")) as LispClass;
    private static LispClass? StandardEffectiveSlotClass()
        => _standardEffectiveSlotClass ??= FindClassOrNil(Startup.Sym("STANDARD-EFFECTIVE-SLOT-DEFINITION")) as LispClass;

    // The LispInstance part of SLOT-VALUE, split out so the entry can test for a
    // standard instance (a sealed class, one compare) before the other kinds.
    private static LispObject SlotValueOfInstance(LispInstance inst, LispObject slotName, string name)
    {
        if (!inst.Class.TryGetSlotIndex(slotName, name, out int idx))
        {
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
                return CallForPrimary(slotMissing, inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-VALUE"));
            throw new LispErrorException(new LispError(
                $"SLOT-VALUE: no slot named {name} in class {inst.Class.Name.Name}"));
        }
        // AMOP 5.4: dispatch through slot-value-using-class for custom metaclasses
        if (UsesSlotProtocol(inst.Class) && Startup.Sym("SLOT-VALUE-USING-CLASS").Function is LispFunction svucFn)
            return CallForPrimary(svucFn, inst.Class, inst, inst.Class.EffectiveSlots[idx]);
        return SlotValueDirect(inst, idx, slotName, name);
    }

    /// <summary>Update an obsolete instance to its class's current layout (CLHS 4.3.6.1),
    /// then call UPDATE-INSTANCE-FOR-REDEFINED-CLASS with the added local slots, the
    /// discarded slots, and a property list of the discarded slots' values. Local slots
    /// in both layouts keep their values; a slot shared before and local now takes the
    /// old shared value; a local slot that is gone or became shared is discarded. Slots
    /// are paired by symbol (A::X is not B::X).</summary>
    public static void UpdateObsoleteInstance(LispInstance inst)
    {
        LispObject added = Nil.Instance, discarded = Nil.Instance, plist = Nil.Instance;
        var newShared = new List<SlotDefinition>();
        lock (inst)
        {
            var cls = inst.Class;
            var current = cls.Layout;
            var old = inst.Layout;
            if (ReferenceEquals(old, current)) return;   // another thread got here first
            var shape = old.Superseded;
            var oldSlots = shape?.Slots ?? Array.Empty<SlotDefinition>();
            var oldShared = shape?.Shared ?? Array.Empty<(Symbol Name, LispObject? Value)>();
            var newSlots = cls.EffectiveSlots;
            var values = new LispObject?[newSlots.Length];
            var oldValues = inst.Slots;

            static bool IsLocal(SlotDefinition s) => !s.IsClassAllocation && s.Allocation == null;
            int FindNewLocal(Symbol name)
            {
                for (int j = 0; j < newSlots.Length; j++)
                    if (IsLocal(newSlots[j]) && LispClass.SameSlotName(newSlots[j].Name, name)) return j;
                return -1;
            }

            var discardedList = new List<Symbol>();
            var plistItems = new List<LispObject>();
            // Old local slots: carried over or discarded.
            for (int i = 0; i < oldSlots.Length; i++)
            {
                if (!IsLocal(oldSlots[i])) continue;
                var v = i < oldValues.Length ? oldValues[i] : null;
                int j = FindNewLocal(oldSlots[i].Name);
                if (j >= 0) values[j] = v;
                else
                {
                    discardedList.Add(oldSlots[i].Name);
                    if (v != null) { plistItems.Add(oldSlots[i].Name); plistItems.Add(v); }
                }
            }
            // Old shared slots that are local now take the shared value.
            foreach (var (name, v) in oldShared)
            {
                int j = FindNewLocal(name);
                if (j >= 0) values[j] = v;
            }
            // Added: new local slots that were neither local nor shared before.
            var addedList = new List<Symbol>();
            foreach (var ns in newSlots)
            {
                if (!IsLocal(ns)) continue;
                bool before = false;
                foreach (var os in oldSlots)
                    if (LispClass.SameSlotName(os.Name, ns.Name)
                        && (IsLocal(os) || os.IsClassAllocation)) { before = true; break; }
                if (!before) addedList.Add(ns.Name);
            }
            foreach (var ns in newSlots)
            {
                if (!ns.IsClassAllocation || ns.InitformThunk == null) continue;
                bool wasShared = false;
                foreach (var os in oldSlots)
                    if (os.IsClassAllocation && LispClass.SameSlotName(os.Name, ns.Name)) { wasShared = true; break; }
                if (!wasShared) newShared.Add(ns);
            }
            inst.Slots = values;
            System.Threading.Volatile.Write(ref inst.Layout, current);

            for (int k = addedList.Count - 1; k >= 0; k--) added = new Cons(addedList[k], added);
            for (int k = discardedList.Count - 1; k >= 0; k--) discarded = new Cons(discardedList[k], discarded);
            for (int k = plistItems.Count - 1; k >= 0; k--) plist = new Cons(plistItems[k], plist);
        }
        // A shared slot the class gained has had no MAKE-INSTANCE yet to run its
        // initform (dotcl fills shared slots lazily there); do it now, so the
        // updated instance does not read it as unbound.
        var cl = inst.Class;
        foreach (var s in newShared)
        {
            var owner = FindClassSlotOwner(cl, s.Name.Name);
            if (!owner.ClassSlotValues.TryGetValue(s.Name.Name, out var cv) || cv == null)
                owner.ClassSlotValues[s.Name.Name] = s.InitformThunk!.Invoke();
        }
        if (Startup.Sym("UPDATE-INSTANCE-FOR-REDEFINED-CLASS").Function is LispFunction uirFn)
            uirFn.Invoke(new LispObject[] { inst, added, discarded, plist });
    }

    internal static LispObject SlotValueDirect(LispInstance inst, int idx, LispObject slotName, string name)
    {
        // Obsolete-instance check, against the class read once (an update never
        // changes the class, so IDX stays valid).
        var cls = inst.Class;
        if (!ReferenceEquals(inst.Layout, cls.Layout)) UpdateObsoleteInstance(inst);
        LispObject? val;
        if (cls.EffectiveSlots[idx].IsClassAllocation)
        {
            // Class-allocated slot: stored on the class that defines it
            var ownerClass = FindClassSlotOwner(cls, name);
            ownerClass.ClassSlotValues.TryGetValue(name, out val);
        }
        else
        {
            val = inst.Slots[idx];
        }
        if (val == null)
        {
            if (Startup.Sym("SLOT-UNBOUND").Function is LispFunction slotUnbound)
                return MultipleValues.Primary(slotUnbound.Invoke(new LispObject[] { inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name) }));
            throw new LispErrorException(new LispError(
                $"SLOT-UNBOUND: slot {name} is unbound in instance of {inst.Class.Name.Name}"));
        }
        return val;
    }

    /// <summary>Find the most specific class in CPL that defines a class-allocated slot with the given name.</summary>
    public static LispClass FindClassSlotOwnerPublic(LispClass cls, string slotName) => FindClassSlotOwner(cls, slotName);
    private static LispClass FindClassSlotOwner(LispClass cls, string slotName)
    {
        foreach (var c in cls.ClassPrecedenceList)
        {
            foreach (var ds in c.DirectSlots)
            {
                if (ds.Name.Name == slotName && ds.IsClassAllocation)
                    return c;
            }
        }
        return cls; // fallback
    }

    public static LispObject SetSlotValue(LispObject obj, LispObject slotName, LispObject value)
    {
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        string name = slotName switch { Symbol sym => sym.Name, _ => slotName.ToString() };
        if (obj is LispInstance fastInst) return SetSlotValueOfInstance(fastInst, slotName, name, value);
        if (LispStruct.TryAs(obj, out var st))
        {
            var stCls = FindClassOrNil(st.TypeName) as LispClass;
            if (stCls != null && stCls.SlotIndex.TryGetValue(name, out int stIdx) && stIdx < st.SlotCount)
            {
                st.SetSlot(stIdx, value);
                return value;
            }
            throw new LispErrorException(new LispError($"SET-SLOT-VALUE: no slot named {name} in struct {st.TypeName.Name}"));
        }
        if (obj is SlotDefinition slotd)
        {
            (slotd.EnsureExtraSlots())[name] = value;
            return value;
        }
        // Metaclass-added slot on a class metaobject.
        if (obj is LispClass klass && klass.Metaclass != null && klass.Metaclass.SlotIndex.ContainsKey(name))
        {
            (klass.EnsureExtraSlots())[name] = value;
            return value;
        }
        // The same for a generic function under a user-defined generic function class.
        if (obj is GenericFunction gfObj && gfObj.StoredClass != null
            && gfObj.StoredClass.SlotIndex.ContainsKey(name))
        {
            (gfObj.EnsureExtraSlots())[name] = value;
            return value;
        }
        // And for a method under a user-defined method class.
        if (obj is LispMethod meth && meth.MetaClass != null
            && meth.MetaClass.SlotIndex.ContainsKey(name))
        {
            (meth.EnsureExtraSlots())[name] = value;
            return value;
        }
        if (obj is LispCondition cond)
        {
            var ccls = ClassOf(cond) as LispClass;
            if (ccls != null && ccls.SlotIndex.ContainsKey(name) && TryWriteNativeConditionSlot(cond, name, value))
                return value;
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction csm)
            {
                CallForPrimary(csm, ccls ?? (LispObject)Nil.Instance, cond,
                    slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SETF"), value);
                return value;
            }
            throw new LispErrorException(new LispError(
                $"SET-SLOT-VALUE: no slot named {name} in condition {cond.ConditionTypeName}"));
        }
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SET-SLOT-VALUE: not a CLOS instance", obj));
        return SetSlotValueOfInstance(inst, slotName, name, value);
    }

    // The LispInstance part of SET-SLOT-VALUE, split out so the entry can test for a
    // standard instance (a sealed class, one compare) before the other kinds.
    private static LispObject SetSlotValueOfInstance(LispInstance inst, LispObject slotName, string name, LispObject value)
    {
        if (!inst.Class.TryGetSlotIndex(slotName, name, out int idx))
        {
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
            {
                CallForPrimary(slotMissing, inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SETF"), value);
                return value;
            }
            throw new LispErrorException(new LispError(
                $"SET-SLOT-VALUE: no slot named {name} in class {inst.Class.Name.Name}"));
        }
        // AMOP 5.4: dispatch through (setf slot-value-using-class) for custom metaclasses
        if (UsesSlotProtocol(inst.Class) && Startup.Sym("SLOT-VALUE-USING-CLASS").SetfFunction is LispFunction setfSvucFn)
            return CallForPrimary(setfSvucFn, value, inst.Class, inst, inst.Class.EffectiveSlots[idx]);
        return SetSlotValueDirect(inst, idx, name, value);
    }

    internal static LispObject SetSlotValueDirect(LispInstance inst, int idx, string name, LispObject value)
    {
        var cls = inst.Class;
        if (!ReferenceEquals(inst.Layout, cls.Layout)) UpdateObsoleteInstance(inst);
        if (cls.EffectiveSlots[idx].IsClassAllocation)
        {
            var ownerClass = FindClassSlotOwner(cls, name);
            ownerClass.ClassSlotValues[name] = value;
        }
        else
        {
            inst.Slots[idx] = value;
        }
        return value;
    }

    /// <summary>Item3b: compile-time-inlined fast path for a call to a simple
    /// slot-reader accessor GF, backed by a per-call-site monomorphic inline cache
    /// (<paramref name="cell"/>, baked once by the assembler). On a hit, same class as
    /// last time and the method-system epoch unchanged, the slot is read straight from
    /// the instance vector, with no GF resolution, no dispatch, and no name->index lookup.
    /// A miss re-resolves the accessor and refills the cell; anything that isn't a plain
    /// instance-allocated simple reader (redefined/extended accessor, a class whose slot
    /// access takes the *-USING-CLASS protocol,
    /// class-allocated or unbound slot) falls through to a normal 1-arg invocation, so
    /// semantics are identical to calling the GF directly.</summary>
    public static LispObject ReaderIC(LispObject obj, ReaderCache cell)
    {
        if (obj is LispInstance inst)
        {
            var e = cell.E;
            if (e != null && e.Epoch == GenericFunction.MethodEpoch
                && ReferenceEquals(inst.Layout, e.Layout))
            {
                var v = inst.Slots[e.Idx];
                if (v != null) return v;   // bound slot (bound NIL is non-null); hot path
                // unbound slot: fall to full path for the SLOT-UNBOUND protocol
            }
            else if (Emitter.CilAssembler.GetFunctionBySymbol(cell.Sym) is GenericFunction gf
                     && gf.SimpleReaderSlot is { } s && !UsesSlotProtocol(inst.Class)
                     && inst.Class.TryGetSlotIndex(s, out int idx))
            {
                // Only instance-allocated slots are cacheable as a direct Slots[idx] read;
                // :class-allocation lives on the owner class, so serve it without caching.
                // The entry keys on the layout token, so the instance must be current
                // first: an obsolete one would otherwise hand out the stale token.
                inst.EnsureCurrent();
                if (!inst.Class.EffectiveSlots[idx].IsClassAllocation)
                    cell.E = inst.Layout.SlotEntry(idx, GenericFunction.MethodEpoch);
                return SlotValueDirect(inst, idx, s, s.Name);
            }
        }
        // INVOKE1, not INVOKE with a one-element array: this is the path every call
        // of an accessor takes once its generic function holds anything but reader
        // methods -- one hand-written DEFMETHOD on the same name is enough, and that
        // is ordinary CLOS, not a corner. The array was 32 bytes on each of those
        // calls; the one-argument entry dispatches without one.
        return Emitter.CilAssembler.GetFunctionBySymbol(cell.Sym).Invoke1(obj);
    }

    /// <summary>(slot-value obj 'name) with a per-call-site cache of the slot's
    /// index, keyed and invalidated like <see cref="ReaderIC"/>.</summary>
    public static LispObject SlotValueIC(LispObject obj, ReaderCache cell)
    {
        if (obj is LispInstance inst)
        {
            int epoch = GenericFunction.MethodEpoch;
            var e = cell.E;
            if (e != null && e.Epoch == epoch && ReferenceEquals(inst.Layout, e.Layout))
            {
                var v = inst.Slots[e.Idx];
                if (v != null) return v;
                return SlotValue(obj, cell.Sym);
            }
            var e2 = cell.E2;
            if (e2 != null && e2.Epoch == epoch && ReferenceEquals(inst.Layout, e2.Layout))
            {
                var v = inst.Slots[e2.Idx];
                if (v != null) return v;
                return SlotValue(obj, cell.Sym);
            }
            if (!UsesSlotProtocol(inst.Class) && inst.Class.TryGetSlotIndex(cell.Sym, out int idx))
            {
                inst.EnsureCurrent();
                if (!inst.Class.EffectiveSlots[idx].IsClassAllocation)
                {
                    cell.E2 = e;
                    cell.E = inst.Layout.SlotEntry(idx, epoch);
                }
                return SlotValueDirect(inst, idx, cell.Sym, cell.Sym.Name);
            }
        }
        return SlotValue(obj, cell.Sym);
    }

    /// <summary>Item3c: the writer twin of <see cref="ReaderIC"/>: the compile-time-inlined
    /// fast path for <c>(setf (accessor obj) newval)</c>, backed by a per-call-site
    /// monomorphic inline cache. Arguments keep the (SETF name) generic function's own
    /// order (new value first, object second) so the call site evaluates its argument forms
    /// in exactly the order the normal call would. On a hit the slot is written straight
    /// into the instance vector; a miss re-resolves the writer, and anything that is not a
    /// plain instance-allocated simple writer (extended accessor, a class whose slot
    /// access takes the *-USING-CLASS protocol,
    /// class-allocated slot, newval-specialized method) falls through to a normal 2-arg
    /// invocation, so semantics are identical to calling the GF.</summary>
    public static LispObject WriterIC(LispObject newval, LispObject obj, WriterCache cell)
    {
        if (obj is LispInstance inst)
        {
            var e = cell.E;
            if (e != null && e.Epoch == GenericFunction.MethodEpoch
                && ReferenceEquals(inst.Layout, e.Layout))
            {
                inst.Slots[e.Idx] = newval;   // instance-allocated by construction of the entry
                return newval;
            }
            if (Emitter.CilAssembler.GetSetfFunctionBySymbol(cell.Sym) is GenericFunction gf
                && gf.SimpleWriterSlot is { } s && !UsesSlotProtocol(inst.Class)
                && inst.Class.TryGetSlotIndex(s, out int idx))
            {
                // Only instance-allocated slots are cacheable as a direct Slots[idx] write;
                // :class-allocation lives on the owner class, so serve it without caching.
                inst.EnsureCurrent();
                if (!inst.Class.EffectiveSlots[idx].IsClassAllocation)
                    cell.E = inst.Layout.SlotEntry(idx, GenericFunction.MethodEpoch);
                return SetSlotValueDirect(inst, idx, s.Name, newval);
            }
        }
        // INVOKE2 for the same reason as the reader's INVOKE1 above.
        return Emitter.CilAssembler.GetSetfFunctionBySymbol(cell.Sym)
                                   .Invoke2(newval, obj);
    }

    public static LispObject SlotBoundp(LispObject obj, LispObject slotName)
    {
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        string name = slotName switch { Symbol sym => sym.Name, _ => slotName.ToString() };
        if (obj is LispInstance fastInst) return SlotBoundpOfInstance(fastInst, slotName, name);
        if (LispStruct.TryAs(obj, out var st))
        {
            var stCls = FindClassOrNil(st.TypeName) as LispClass;
            if (stCls != null && stCls.SlotIndex.TryGetValue(name, out int stIdx) && stIdx < st.SlotCount)
                return st.GetSlot(stIdx) != null ? T.Instance : Nil.Instance;
            return Nil.Instance;
        }
        if (obj is SlotDefinition slotd)
            return slotd.ExtraSlots != null && slotd.ExtraSlots.TryGetValue(name, out var bv) && bv != null
                ? T.Instance : Nil.Instance;
        if (obj is LispClass klass && klass.Metaclass != null)
            return klass.ExtraSlots != null && klass.ExtraSlots.TryGetValue(name, out var cbv) && cbv != null
                ? T.Instance : Nil.Instance;
        // A funcallable instance (a generic function under a user-defined
        // funcallable class) keeps its slots in ExtraSlots, as SLOT-VALUE and
        // (SETF SLOT-VALUE) already read and write them.
        if (obj is GenericFunction gfb && gfb.StoredClass != null
            && gfb.StoredClass.SlotIndex.ContainsKey(name))
            return gfb.ExtraSlots != null && gfb.ExtraSlots.TryGetValue(name, out var gbv) && gbv != null
                ? T.Instance : Nil.Instance;
        if (obj is LispMethod meth && meth.MetaClass != null)
            return meth.ExtraSlots != null && meth.ExtraSlots.TryGetValue(name, out var mbv) && mbv != null
                ? T.Instance : Nil.Instance;
        if (obj is LispCondition cond)
        {
            var ccls = ClassOf(cond) as LispClass;
            if (ccls != null && ccls.SlotIndex.ContainsKey(name))
            {
                TryReadNativeConditionSlot(cond, name, out var cval);
                LispObject cb = cval != null ? T.Instance : (LispObject)Nil.Instance;
                MultipleValues.SetOne(cb);
                return cb;
            }
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction csm)
            {
                var r = CallForPrimary(csm, ccls ?? (LispObject)Nil.Instance, cond,
                    slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-BOUNDP"));
                LispObject cb = IsTruthy(r) ? T.Instance : (LispObject)Nil.Instance;
                MultipleValues.SetOne(cb);
                return cb;
            }
            throw new LispErrorException(new LispError(
                $"SLOT-BOUNDP: no slot named {name} in condition {cond.ConditionTypeName}"));
        }
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SLOT-BOUNDP: not a CLOS instance", obj));
        return SlotBoundpOfInstance(inst, slotName, name);
    }

    // The LispInstance part of SLOT-BOUNDP, split out so the entry can test for a
    // standard instance (a sealed class, one compare) before the other kinds.
    private static LispObject SlotBoundpOfInstance(LispInstance inst, LispObject slotName, string name)
    {
        if (!inst.Class.TryGetSlotIndex(slotName, name, out int idx))
        {
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
            {
                var result = CallForPrimary(slotMissing, inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-BOUNDP"));
                // slot-boundp returns a SINGLE generalized boolean: explicitly install
                // exactly one value so any secondary values slot-missing returned (it may
                // legally return (values nil x)) do not leak to the caller. ANSI SLOT-MISSING.8.
                LispObject b = IsTruthy(result) ? T.Instance : (LispObject)Nil.Instance;
                MultipleValues.SetOne(b);
                return b;
            }
            throw new LispErrorException(new LispError(
                $"SLOT-BOUNDP: no slot named {name} in class {inst.Class.Name.Name}"));
        }
        // AMOP 5.4: dispatch through slot-boundp-using-class for custom metaclasses.
        if (UsesSlotProtocol(inst.Class) && Startup.Sym("SLOT-BOUNDP-USING-CLASS").Function is LispFunction sbucFn)
            return IsTruthy(CallForPrimary(sbucFn, inst.Class, inst, inst.Class.EffectiveSlots[idx]))
                ? T.Instance : Nil.Instance;
        return SlotBoundpDirect(inst, idx, name) ? T.Instance : Nil.Instance;
    }

    /// <summary>Raw slot-boundness check against the instance layout / class-allocation
    /// store, without slot-value-using-class dispatch. Shared by SLOT-BOUNDP and the
    /// default SLOT-BOUNDP-USING-CLASS method.</summary>
    internal static bool SlotBoundpDirect(LispInstance inst, int idx, string name)
    {
        var cls = inst.Class;
        if (!ReferenceEquals(inst.Layout, cls.Layout)) UpdateObsoleteInstance(inst);
        if (cls.EffectiveSlots[idx].IsClassAllocation)
        {
            var ownerClass = FindClassSlotOwner(cls, name);
            return ownerClass.ClassSlotValues.TryGetValue(name, out var cv) && cv != null;
        }
        return inst.Slots[idx] != null;
    }

    /// <summary>Raw slot-makunbound against the instance layout / class-allocation store,
    /// without slot-value-using-class dispatch. Shared by SLOT-MAKUNBOUND and the default
    /// SLOT-MAKUNBOUND-USING-CLASS method.</summary>
    internal static void SlotMakunboundDirect(LispInstance inst, int idx, string name)
    {
        inst.EnsureCurrent();
        if (inst.Class.EffectiveSlots[idx].IsClassAllocation)
            FindClassSlotOwnerPublic(inst.Class, name).ClassSlotValues[name] = null;
        else
            inst.Slots[idx] = null!;
    }

    public static LispObject SlotMissingDefault(LispObject[] args)
    {
        // args: class, object, slot-name, operation, [new-value]
        var slotName = args.Length > 2 ? args[2] : Nil.Instance;
        var operation = args.Length > 3 ? args[3] : Nil.Instance;
        throw new LispErrorException(new LispError(
            $"SLOT-MISSING: no slot named {slotName} for operation {operation}"));
    }

    public static LispObject SlotUnboundDefault(LispObject[] args)
    {
        // args: class, object, slot-name
        var obj = args.Length > 1 ? args[1] : Nil.Instance;
        var slotName = args.Length > 2 ? args[2] : Nil.Instance;
        // Signal UNBOUND-SLOT condition per CLHS
        var condition = Runtime.MakeConditionFromType(
            Startup.Sym("UNBOUND-SLOT"),
            new LispObject[] { Startup.Sym("NAME"), slotName, Startup.Sym("INSTANCE"), obj });
        throw new LispErrorException(condition);
    }

    /// <summary>True when SHARED-INITIALIZE has no method of its own applicable to CLS,
    /// so the default method is what a dispatch would land on. Cached per class and
    /// dropped wherever SIMPLEINITCHECKED is (adding or removing a method on either
    /// initialization generic function, and class redefinition).</summary>
    private static bool SharedInitializeIsDefault(LispClass cls)
    {
        if (cls.SharedInitSimpleChecked) return cls.SharedInitSimpleValid;
        cls.SharedInitSimpleChecked = true;
        bool simple = true;
        _sharedInitializeSym ??= Startup.Sym("SHARED-INITIALIZE");
        if (_sharedInitializeSym.Function is GenericFunction siGf)
        {
            foreach (var m in siGf.Methods)
            {
                if (AllTSpecializers(m) && m.Qualifiers.Length == 0) continue;
                if (IsMethodApplicableToClass(m, cls)) { simple = false; break; }
            }
        }
        cls.SharedInitSimpleValid = simple;
        return simple;
    }

    public static LispObject SlotExists(LispObject obj, LispObject slotName)
    {
        string name = slotName switch
        {
            Symbol sym => sym.Name,
            _ => slotName.ToString()
        };

        if (obj is LispInstance inst)
        {
            if (inst.Class.TryGetSlotIndex(slotName, name, out _))
                return T.Instance;
            if (inst.Class.StructSlotNames != null)
            {
                foreach (var sn in inst.Class.StructSlotNames)
                    if (sn.Name == name) return T.Instance;
            }
            return Nil.Instance;
        }

        // A funcallable instance: its class's slots, as SLOT-VALUE sees them.
        if (obj is GenericFunction gfe && gfe.StoredClass != null)
            return gfe.StoredClass.SlotIndex.ContainsKey(name) ? T.Instance : Nil.Instance;

        if (LispStruct.TryAs(obj, out var ls))
        {
            var cls = FindClassOrNil(ls.TypeName) as LispClass;
            if (cls?.StructSlotNames != null)
            {
                foreach (var sn in cls.StructSlotNames)
                    if (sn.Name == name) return T.Instance;
            }
            // Also check SlotIndex (from FinalizeClass)
            if (cls?.SlotIndex?.ContainsKey(name) == true)
                return T.Instance;
            return Nil.Instance;
        }

        // Native (runtime-signaled) condition: consult its registered class's slots.
        if (obj is LispCondition cond)
            return ClassOf(cond) is LispClass ccls && ccls.SlotIndex.ContainsKey(name)
                ? T.Instance : Nil.Instance;

        // Metaobjects that are not LispInstances: a slot definition, a class and a
        // method. Under a custom slot-definition class, metaclass or method class
        // they hold that class's slots (SLOT-VALUE reads them from ExtraSlots), so
        // SLOT-EXISTS-P has to answer from the same class.
        if (obj is SlotDefinition || obj is LispClass || obj is LispMethod)
            return ClassOf(obj) is LispClass mcls && mcls.SlotIndex.ContainsKey(name)
                ? T.Instance : Nil.Instance;

        return Nil.Instance;
    }
}
