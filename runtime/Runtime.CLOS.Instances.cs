using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    public static LispObject InitializeInstance(LispObject[] args)
    {
        // args[0] = instance, args[1..] = initargs
        // Calls shared-initialize with slot-names = T (init all slots)

        // Nothing has a method of its own on SHARED-INITIALIZE for this instance:
        // the default method is what the dispatch would reach, so run it here on the
        // array already in hand. The dispatch path has to build (instance T . initargs)
        // first, which is an array per instance created.
        if (args[0] is LispInstance fastInst && SharedInitializeIsDefault(fastInst.Class))
            return SharedInitializeCore(args[0], T.Instance, args, 1);

        var sharedInitFn = Startup.Sym("SHARED-INITIALIZE").Function as LispFunction
            ?? throw new LispErrorException(new LispError("SHARED-INITIALIZE not defined"));
        var siArgs = new LispObject[args.Length + 1];
        siArgs[0] = args[0];
        siArgs[1] = T.Instance;
        Array.Copy(args, 1, siArgs, 2, args.Length - 1);
        return sharedInitFn.Invoke(siArgs);
    }

    public static LispObject ReinitializeInstance(LispObject[] args)
    {
        // args[0] = instance, args[1..] = initargs
        // Class objects (AMOP): the new :DIRECT-SUPERCLASSES / :DIRECT-SLOTS /
        // :DIRECT-DEFAULT-INITARGS replace the class's, and the class and its
        // subclasses are finalized again. Under a custom metaclass this goes through
        // SHARED-INITIALIZE with the initargs as they reached here, so metaclass
        // methods see (and may rewrite) them and the metaclass's own slots take any
        // initargs given; with no custom metaclass the class-structure initargs are
        // installed directly.
        if (args[0] is LispClass lc)
        {
            bool hasRelevantArgs = false;
            for (int i = 1; i + 1 < args.Length; i += 2)
            {
                if (args[i] is Symbol ks &&
                    (ks.Name == "DIRECT-SUPERCLASSES" || ks.Name == "DIRECT-SLOTS"
                     || ks.Name == "DIRECT-DEFAULT-INITARGS"))
                { hasRelevantArgs = true; break; }
            }
            if (lc.IsBuiltIn || lc.IsStructureClass) return lc;
            var oldSupers = lc.DirectSuperclasses;
            if (lc.Metaclass != null && Startup.Sym("SHARED-INITIALIZE").Function is LispFunction siFn)
            {
                var siArgs = new LispObject[args.Length + 1];
                siArgs[0] = lc;
                siArgs[1] = Nil.Instance;
                Array.Copy(args, 1, siArgs, 2, args.Length - 1);
                bool was = lc.ReinitializingFromInitargs;
                lc.ReinitializingFromInitargs = true;
                try { siFn.Invoke(siArgs); }
                finally { lc.ReinitializingFromInitargs = was; }
            }
            else if (hasRelevantArgs)
                InstallClassInitargs(lc, args, 1);
            if (!ReferenceEquals(oldSupers, lc.DirectSuperclasses))
            {
                ValidateSuperclasses(lc.Name, lc.Metaclass, lc.DirectSuperclasses);
                NotifyDirectSubclasses(oldSupers, lc.DirectSuperclasses, lc);
            }
            if (hasRelevantArgs || !ReferenceEquals(oldSupers, lc.DirectSuperclasses))
            {
                if (!lc.DirectSuperclasses.Any(s => ForwardReferencedAncestor(s) != null))
                    lc.FinalizeClass();
                RefinalizeDependents(lc);
            }
            DefinePendingAccessors(lc, lc);
            return lc;
        }
        // Per CLHS 7.1.2: validate initargs against slots + applicable method &key params.
        // Nothing to validate when no initargs were supplied, which is the common
        // (REINITIALIZE-INSTANCE x) call -- and skipping it also skips the walk over
        // every method of two generic functions.
        if (args.Length > 1 && args[0] is LispInstance li)
        {
            // Collect &key names from applicable reinitialize-instance and shared-initialize
            // methods. If any method has &allow-other-keys, skip validation entirely.
            // The set stays null until a name is actually added: the usual case is no
            // &key method at all, and an empty HashSet allocated per call is the same
            // waste SHARED-INITIALIZE had.
            var mk = ReinitMethodKeysFor(li.Class);
            if (!mk.AllowOtherKeys)
                ValidateInitargs(li.Class, args, 1, mk.Keys);
        }
        // Calls shared-initialize with slot-names = NIL (don't init unbound slots).
        // The small arities pass the arguments positionally: building the array only to
        // insert NIL at index 1 was the other half of this call's cost, and
        // (REINITIALIZE-INSTANCE x) / one initarg pair are what callers actually write.
        var sharedInitFn = Startup.Sym("SHARED-INITIALIZE").Function as LispFunction
            ?? throw new LispErrorException(new LispError("SHARED-INITIALIZE not defined"));
        switch (args.Length)
        {
            case 1:
                sharedInitFn.Invoke2(args[0], Nil.Instance);
                break;
            case 3:
                sharedInitFn.Invoke4(args[0], Nil.Instance, args[1], args[2]);
                break;
            case 5:
                sharedInitFn.Invoke6(args[0], Nil.Instance, args[1], args[2], args[3], args[4]);
                break;
            default:
            {
                var siArgs = new LispObject[args.Length + 1];
                siArgs[0] = args[0];
                siArgs[1] = Nil.Instance;
                Array.Copy(args, 1, siArgs, 2, args.Length - 1);
                sharedInitFn.Invoke(siArgs);
                break;
            }
        }
        return args[0]; // reinitialize-instance returns the instance
    }

    /// <summary>Check if a class has custom initialize-instance or shared-initialize methods
    /// beyond the default T/STANDARD-OBJECT methods. Used for make-instance fast path.</summary>
    private static bool HasCustomInitMethods(LispClass cls)
    {
        if (cls.CachedHasCustomInitMethods is bool cached) return cached;
        bool result = HasCustomInitMethodsUncached(cls);
        cls.CachedHasCustomInitMethods = result;
        return result;
    }

    private static bool HasCustomInitMethodsUncached(LispClass cls)
    {
        // Check initialize-instance GF
        var iiSym = Startup.Sym("INITIALIZE-INSTANCE");
        if (iiSym.Function is GenericFunction iiGf)
        {
            foreach (var method in iiGf.Methods)
            {
                if (method.Specializers.Length > 0 && method.Specializers[0] is LispClass specCls
                    && specCls.Name.Name != "T" && specCls.Name.Name != "STANDARD-OBJECT"
                    && cls.ClassPrecedenceList.Contains(specCls))
                    return true;
            }
        }
        // Check shared-initialize GF
        var siSym = Startup.Sym("SHARED-INITIALIZE");
        if (siSym.Function is GenericFunction siGf)
        {
            foreach (var method in siGf.Methods)
            {
                if (method.Specializers.Length > 0 && method.Specializers[0] is LispClass specCls
                    && specCls.Name.Name != "T" && specCls.Name.Name != "STANDARD-OBJECT"
                    && cls.ClassPrecedenceList.Contains(specCls))
                    return true;
            }
        }
        return false;
    }

    /// <summary>Collect &key names from the applicable methods of GFSYM for INST into KEYS,
    /// creating the set only when there is a name to put in it. Returns true if any
    /// applicable method has &allow-other-keys, in which case the caller skips validation
    /// and the set is not needed at all.</summary>
    private static ReinitMethodKeys ReinitMethodKeysFor(LispClass cls)
    {
        int epoch = GenericFunction.MethodEpoch;
        var c = cls.ReinitKeysCache;
        if (c != null && c.Epoch == epoch) return c;
        HashSet<string>? keys = null;
        bool aok = CollectMethodKeys(cls, Startup.Sym("REINITIALIZE-INSTANCE"), ref keys)
                   || CollectMethodKeys(cls, Startup.Sym("SHARED-INITIALIZE"), ref keys);
        c = new ReinitMethodKeys(epoch, aok, aok ? null : keys);
        cls.ReinitKeysCache = c;
        return c;
    }

    private static bool CollectMethodKeys(LispClass cls, Symbol gfSym, ref HashSet<string>? keys)
    {
        if (gfSym.Function is not GenericFunction gf) return false;
        foreach (var method in gf.MethodsArray)
        {
            if (method.Specializers.Length > 0
                && !(method.Specializers[0] is LispClass sc
                     && (sc.Name.Name == "T" || Array.IndexOf(cls.ClassPrecedenceList, sc) >= 0)))
                continue;
            if (method.HasAllowOtherKeys) return true;
            if (method.HasKey)
                foreach (var kn in method.KeywordNames)
                    (keys ??= new HashSet<string>()).Add(kn);
        }
        return false;
    }

    /// <summary>Collect &key names from applicable methods of the given GF for a class (CPL-based).
    /// Returns true if any applicable method has &allow-other-keys.</summary>
    private static bool AddMethodKeysForClass(LispClass cls, Symbol gfSym, HashSet<string> keys)
    {
        if (gfSym.Function is not GenericFunction gf) return false;
        foreach (var method in gf.MethodsArray)
        {
            if (!IsMethodApplicableToClass(method, cls)) continue;
            if (method.HasAllowOtherKeys) return true;
            if (method.HasKey)
                foreach (var kn in method.KeywordNames)
                    keys.Add(kn);
        }
        return false;
    }

    /// <summary>Check if instance has custom methods on reinitialize-instance or shared-initialize
    /// beyond the default T-specializer methods.</summary>
    private static bool HasCustomApplicableMethods(LispInstance inst)
    {
        // Check reinitialize-instance GF
        var riSym = Startup.Sym("REINITIALIZE-INSTANCE");
        if (riSym.Function is GenericFunction riGf)
        {
            foreach (var method in riGf.Methods)
            {
                if (method.Specializers.Length > 0 && method.Specializers[0] is LispClass cls
                    && cls.Name.Name != "T" && IsTruthy(Typep(inst, cls.Name)))
                    return true;
            }
        }
        // Check shared-initialize GF
        var siSym = Startup.Sym("SHARED-INITIALIZE");
        if (siSym.Function is GenericFunction siGf)
        {
            foreach (var method in siGf.Methods)
            {
                if (method.Specializers.Length > 0 && method.Specializers[0] is LispClass cls
                    && cls.Name.Name != "T" && cls.Name.Name != "STANDARD-OBJECT"
                    && IsTruthy(Typep(inst, cls.Name)))
                    return true;
            }
        }
        return false;
    }

    /// <summary>SHARED-INITIALIZE's default method, in the shape the generic function
    /// calls it: args[0] = instance, args[1] = slot-names, args[2..] = initargs.</summary>
    public static LispObject SharedInitialize(LispObject[] args)
        => SharedInitializeCore(args[0], args.Length > 1 ? args[1] : Nil.Instance, args, 2);

    /// <summary>SHARED-INITIALIZE over a structure: initargs, then DEFSTRUCT defaults.</summary>
    private static LispObject InitializeStructureSlots(LispStruct st, LispObject slotNames0,
                                                       LispObject[] initargs, int start)
    {
        if (!_classRegistry.TryGetValue(st.TypeName, out var cls) || !cls.IsStructureClass)
            throw new LispErrorException(new LispTypeError(
                "SHARED-INITIALIZE: not a CLOS instance", st));
        if ((initargs.Length - start) % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "SHARED-INITIALIZE: odd number of keyword arguments"));
        var plan = StructurePlan(cls);
        if (plan == null) return st;
        bool[]? supplied = null;
        for (int i = start; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is not Symbol key
                || !plan.KeyToSlot.TryGetValue(key, out int idx)) continue;
            supplied ??= new bool[plan.Keys.Length];
            if (supplied[idx]) continue;
            supplied[idx] = true;
            st.SetSlot(idx, initargs[i + 1]);
        }
        bool allSlots = slotNames0 is T;
        for (int i = 0; i < plan.Keys.Length; i++)
        {
            if (st.GetSlot(i) != null) continue;
            if (!allSlots && !StructureSlotNamed(cls, slotNames0, i)) continue;
            st.SetSlot(i, plan.Defaults[i] is { } thunk ? thunk.Invoke() : Nil.Instance);
        }
        return st;
    }

    private static bool StructureSlotNamed(LispClass cls, LispObject slotNames, int index)
    {
        if (slotNames is Nil || index >= cls.EffectiveSlots.Length) return false;
        var name = cls.EffectiveSlots[index].Name;
        for (var cur = slotNames; cur is Cons cell; cur = cell.Cdr)
            if (cell.Car is Symbol s && (ReferenceEquals(s, name) || s.Name == name.Name))
                return true;
        return false;
    }

    /// <summary>The same work over an initarg SPAN, so a caller that already holds the
    /// initargs in an array does not have to build a second one shaped
    /// (instance slot-names . initargs). INITIALIZE-INSTANCE's default method holds
    /// exactly that array minus the slot-names and was copying it for every instance
    /// created.</summary>
    private static LispObject SharedInitializeCore(LispObject instArg, LispObject slotNames0,
                                                   LispObject[] initargs, int start)
    {
        LispObject obj = instArg;
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        // A class metaobject under a custom metaclass: initialize the metaclass-added
        // slots (those beyond standard-class) from initargs, else their initforms;
        // the shared-initialize contract applied to ExtraSlots. Reached via the
        // real initialize-instance protocol so inherited :after methods run.
        if (obj is LispClass klass && klass.Metaclass != null)
        {
            var meta = klass.Metaclass;
            // :DIRECT-SUPERCLASSES, :DIRECT-SLOTS and :DIRECT-DEFAULT-INITARGS as they
            // reached this method (a metaclass INITIALIZE-INSTANCE or
            // REINITIALIZE-INSTANCE method may have rewritten them). Installed only on a
            // class being made or being reinitialized by REINITIALIZE-INSTANCE: the
            // caller finalizes it afterwards.
            if (klass.ClassPrecedenceList.Length == 0 || klass.ReinitializingFromInitargs)
                InstallClassInitargs(klass, initargs, start);
            var stdNames = new HashSet<string>();
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-CLASS"), out var scO) && scO is LispClass scC)
                foreach (var es0 in scC.EffectiveSlots) stdNames.Add(es0.Name.Name);
            foreach (var es in meta.EffectiveSlots)
            {
                string sn = es.Name.Name;
                if (stdNames.Contains(sn)) continue;
                bool fromInitarg = false;
                for (int i = start; i + 1 < initargs.Length; i += 2)
                    if (initargs[i] is Symbol k && Array.Exists(es.Initargs, ia => ia.Name == k.Name))
                    { (klass.EnsureExtraSlots())[sn] = initargs[i + 1]; fromInitarg = true; break; }
                if (fromInitarg) continue;
                bool inNames = slotNames0 is T;
                if (!inNames) for (var c = slotNames0; c is Cons cc; c = cc.Cdr) if (cc.Car is Symbol s2 && s2.Name == sn) { inNames = true; break; }
                bool bound = klass.ExtraSlots != null && klass.ExtraSlots.TryGetValue(sn, out var ev) && ev != null;
                if (inNames && !bound && es.InitformThunk is { } thunk)
                    (klass.EnsureExtraSlots())[sn] = MultipleValues.Primary(thunk.Invoke(Array.Empty<LispObject>()));
            }
            return klass;
        }
        // A generic function under a user-defined generic function class: the same
        // contract applied to its ExtraSlots. Its class's own slots (the ones
        // STANDARD-GENERIC-FUNCTION already has) are the runtime object's business,
        // so only the slots the user class adds are handled here.
        // A generic function with no user-defined class adds no slots, so there is
        // nothing to initialize -- but it is not an error either. REINITIALIZE-INSTANCE
        // reaches here through its primary method, and (SETF GENERIC-FUNCTION-NAME) is
        // defined in terms of that.
        if (obj is GenericFunction plainGf && plainGf.StoredClass == null)
            return plainGf;
        if (obj is GenericFunction gfInit && gfInit.StoredClass is { } gfClass)
        {
            var inheritedNames = new HashSet<string>();
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-GENERIC-FUNCTION"), out var sgfO)
                && sgfO is LispClass sgfC)
                foreach (var es0 in sgfC.EffectiveSlots) inheritedNames.Add(es0.Name.Name);
            foreach (var es in gfClass.EffectiveSlots)
            {
                string sn = es.Name.Name;
                if (inheritedNames.Contains(sn)) continue;
                bool fromInitarg = false;
                for (int i = start; i + 1 < initargs.Length; i += 2)
                    if (initargs[i] is Symbol k && Array.Exists(es.Initargs, ia => ia.Name == k.Name))
                    { (gfInit.EnsureExtraSlots())[sn] = initargs[i + 1]; fromInitarg = true; break; }
                if (fromInitarg) continue;
                bool inNames = slotNames0 is T;
                if (!inNames)
                    for (var c = slotNames0; c is Cons cc; c = cc.Cdr)
                        if (cc.Car is Symbol s2 && s2.Name == sn) { inNames = true; break; }
                bool bound = gfInit.ExtraSlots != null
                    && gfInit.ExtraSlots.TryGetValue(sn, out var gv0) && gv0 != null;
                if (inNames && !bound && es.InitformThunk is { } gfThunk)
                    (gfInit.EnsureExtraSlots())[sn] =
                        MultipleValues.Primary(gfThunk.Invoke(Array.Empty<LispObject>()));
            }
            return gfInit;
        }
        // A method under a user-defined method class: the same contract applied to its
        // ExtraSlots. Only the slots the user class adds are handled here; the ones
        // STANDARD-METHOD already has live on the runtime object and are applied by
        // the INITIALIZE-INSTANCE :after for STANDARD-METHOD.
        if (obj is LispMethod methInit && methInit.MetaClass is { } methClass)
        {
            var inheritedNames = new HashSet<string>();
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-METHOD"), out var smO)
                && smO is LispClass smC)
                foreach (var es0 in smC.EffectiveSlots) inheritedNames.Add(es0.Name.Name);
            foreach (var es in methClass.EffectiveSlots)
            {
                string sn = es.Name.Name;
                if (inheritedNames.Contains(sn)) continue;
                bool fromInitarg = false;
                for (int i = start; i + 1 < initargs.Length; i += 2)
                    if (initargs[i] is Symbol k && Array.Exists(es.Initargs, ia => ia.Name == k.Name))
                    { (methInit.EnsureExtraSlots())[sn] = initargs[i + 1]; fromInitarg = true; break; }
                if (fromInitarg) continue;
                bool inNames = slotNames0 is T;
                if (!inNames)
                    for (var c = slotNames0; c is Cons cc; c = cc.Cdr)
                        if (cc.Car is Symbol s2 && s2.Name == sn) { inNames = true; break; }
                bool bound = methInit.ExtraSlots != null
                    && methInit.ExtraSlots.TryGetValue(sn, out var mv0) && mv0 != null;
                if (inNames && !bound && es.InitformThunk is { } methThunk)
                    (methInit.EnsureExtraSlots())[sn] =
                        MultipleValues.Primary(methThunk.Invoke(Array.Empty<LispObject>()));
            }
            return methInit;
        }
        if (obj is LispStruct structInst)
            return InitializeStructureSlots(structInst, slotNames0, initargs, start);
        if (obj is not LispInstance inst)
        {
            // Checked only once the object is known not to be an ordinary instance,
            // so MAKE-INSTANCE of a standard object pays nothing for it.
            // A slot definition of a user-defined slot-definition class: the standard
            // slot options the initargs carry (as they reached this method, so an
            // INITIALIZE-INSTANCE :around that rewrote them with CALL-NEXT-METHOD is
            // honoured) go onto the SlotDefinition, and the slots the user class adds
            // take their initarg or initform. The name, initargs and initfunction are
            // fixed when the SlotDefinition is made and are not changed here.
            if (obj is SlotDefinition sdInit && sdInit.MetaClass is { } sdClass)
            {
                ApplySlotDefinitionInitargs(sdInit, initargs, start);
                var inheritedNames = new HashSet<string>();
                if (_classRegistry.TryGetValue(Startup.Sym(sdInit.IsEffective
                        ? "STANDARD-EFFECTIVE-SLOT-DEFINITION" : "STANDARD-DIRECT-SLOT-DEFINITION"), out var ssdO)
                    && ssdO is LispClass ssdC)
                    foreach (var es0 in ssdC.EffectiveSlots) inheritedNames.Add(es0.Name.Name);
                foreach (var es in sdClass.EffectiveSlots)
                {
                    string sn = es.Name.Name;
                    if (inheritedNames.Contains(sn)) continue;
                    bool fromInitarg = false;
                    for (int i = start; i + 1 < initargs.Length; i += 2)
                        if (initargs[i] is Symbol k && Array.Exists(es.Initargs, ia => ia.Name == k.Name))
                        { (sdInit.EnsureExtraSlots())[sn] = initargs[i + 1]; fromInitarg = true; break; }
                    if (fromInitarg) continue;
                    bool inNames = slotNames0 is T;
                    if (!inNames)
                        for (var c = slotNames0; c is Cons cc; c = cc.Cdr)
                            if (cc.Car is Symbol s2 && s2.Name == sn) { inNames = true; break; }
                    bool bound = sdInit.ExtraSlots != null
                        && sdInit.ExtraSlots.TryGetValue(sn, out var sv0) && sv0 != null;
                    if (inNames && !bound && es.InitformThunk is { } sdThunk)
                        (sdInit.EnsureExtraSlots())[sn] =
                            MultipleValues.Primary(sdThunk.Invoke(Array.Empty<LispObject>()));
                }
                return sdInit;
            }
            throw new LispErrorException(new LispTypeError("SHARED-INITIALIZE: not a CLOS instance", instArg));
        }
        inst.EnsureCurrent();
        var cls = inst.Class;
        LispObject slotNames = slotNames0;

        // For a custom metaclass, instance-allocated slot writes must go through
        // (setf slot-value-using-class) so overrides (e.g. McCLIM's dynamic slots, which
        // store into a per-instance dynamic variable seeded by allocate-instance) run
        // instead of clobbering the raw slot vector.
        bool customMeta = cls.Metaclass != null;
        LispFunction? setfSvuc = customMeta
            ? Startup.Sym("SLOT-VALUE-USING-CLASS").SetfFunction as LispFunction
            : null;
        void StoreInstanceSlot(SlotDefinition slot, int idx, LispObject val)
        {
            if (setfSvuc != null)
                setfSvuc.Invoke(new LispObject[] { val, cls, inst, slot });
            else
                inst.Slots[idx] = val;
        }

        // Validate initargs: must be even count of key-value pairs with symbol keys
        int initargCount = initargs.Length - start;
        if (initargCount % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "SHARED-INITIALIZE: odd number of keyword arguments"));
        for (int i = start; i < initargs.Length; i += 2)
        {
            // NIL and T are valid symbols in CL but separate types in dotcl
            if (initargs[i] is not Symbol && initargs[i] is not Nil && initargs[i] is not T)
                throw new LispErrorException(new LispProgramError(
                    $"SHARED-INITIALIZE: invalid initarg key {initargs[i]}"));
        }

        // Step 1: Apply initargs (leftmost wins for duplicate keys)
        // CLHS: initargs always override existing slot values (not just unbound slots).
        // Track which slot indices were already set by an earlier initarg in THIS call.
        //
        // A bitmask, not a set: slot layout indices are small and dense, so 64 bits
        // cover every class anyone writes, and the common case then allocates nothing.
        // SHARED-INITIALIZE runs for every instance the initialization protocol
        // touches, so a per-call HashSet is paid by every MAKE-INSTANCE that passes an
        // initarg. The set is still built for a class with more than 64 slots.
        long slotsSetMask = 0;
        HashSet<int>? slotsSetWide = null;
        for (int i = start; i + 1 < initargs.Length; i += 2)
        {
            string initargName = initargs[i] switch
            {
                Symbol s => s.Name,
                _ => initargs[i].ToString()!
            };
            bool strict = cls.HasSymbolCollision;
            for (int si = 0; si < cls.EffectiveSlots.Length; si++)
            {
                var slot = cls.EffectiveSlots[si];
                foreach (var ia in slot.Initargs)
                {
                    // By name, unless the class has a cross-package collision: then
                    // A::X and B::X are different initargs and different slots.
                    if (strict ? cls.InitargMatches(ia, initargs[i]) : ia.Name == initargName)
                    {
                        int idx = si;
                        if (strict || cls.SlotIndex.TryGetValue(slot.Name.Name, out idx))
                        {
                            // CLHS: initargs always override the existing slot value, even
                            // for :allocation :class slots already bound by a prior
                            // make-instance. Only the first initarg for a given slot in
                            // THIS call wins (the mask above guards duplicates).
                            bool alreadySet = idx < 64
                                ? (slotsSetMask & (1L << idx)) != 0
                                : slotsSetWide != null && slotsSetWide.Contains(idx);
                            if (!alreadySet)
                            {
                                if (slot.IsClassAllocation)
                                    FindClassSlotOwner(cls, slot.Name.Name).ClassSlotValues[slot.Name.Name] = initargs[i + 1];
                                else
                                    StoreInstanceSlot(slot, idx, initargs[i + 1]);
                                if (idx < 64) slotsSetMask |= 1L << idx;
                                else (slotsSetWide ??= new HashSet<int>()).Add(idx);
                            }
                        }
                        break; // Found matching initarg for this slot, no need to check more
                    }
                }
            }
        }

        // Step 2: Apply initforms for slots specified by slot-names that are still unbound
        bool allSlots = slotNames is T;
        for (int i = 0; i < cls.EffectiveSlots.Length; i++)
        {
            var slotDef = cls.EffectiveSlots[i];
            bool isBound;
            if (slotDef.IsClassAllocation)
            {
                var ownerClass = FindClassSlotOwner(cls, slotDef.Name.Name);
                isBound = ownerClass.ClassSlotValues.TryGetValue(slotDef.Name.Name, out var cv) && cv != null;
            }
            else if (customMeta && Startup.Sym("SLOT-BOUNDP-USING-CLASS").Function is LispFunction sbucBoundFn)
            {
                // Custom metaclass: an instance slot's boundness is defined by
                // slot-boundp-using-class, not the raw vector (the slot may be seeded with
                // a backing object that is itself still "unbound", e.g. McCLIM's dvar). This
                // lets initforms apply through (setf slot-value-using-class).
                isBound = IsTruthy(Primary(sbucBoundFn.Invoke(new LispObject[] { cls, inst, slotDef })));
            }
            else
            {
                isBound = inst.Slots[i] != null;
            }
            if (isBound) continue;
            if (slotDef.InitformThunk == null) continue; // no initform

            if (allSlots)
            {
                var val = slotDef.InitformThunk!.Invoke();
                if (slotDef.IsClassAllocation)
                    FindClassSlotOwner(cls, slotDef.Name.Name).ClassSlotValues[slotDef.Name.Name] = val;
                else
                    StoreInstanceSlot(slotDef, i, val);
            }
            else if (slotNames is not Nil)
            {
                // slot-names is a list of symbols; check if this slot's name is in it
                string slotName = slotDef.Name.Name;
                LispObject cur = slotNames;
                while (cur is Cons cc)
                {
                    string n = cc.Car switch
                    {
                        Symbol sym => sym.Name,
                        _ => cc.Car.ToString()!
                    };
                    if (n == slotName
                        && (!cls.HasSymbolCollision || cc.Car is not Symbol csym
                            || LispClass.SameSlotName(csym, slotDef.Name)))
                    {
                        var val = slotDef.InitformThunk!.Invoke();
                        if (slotDef.IsClassAllocation)
                            FindClassSlotOwner(cls, slotDef.Name.Name).ClassSlotValues[slotDef.Name.Name] = val;
                        else
                            StoreInstanceSlot(slotDef, i, val);
                        break;
                    }
                    cur = cc.Cdr;
                }
            }
            // if slotNames is NIL, skip initforms
        }

        return instArg; // return original instance (possibly wrapped)
    }

    private static Symbol? _makeInstanceSym;

    /// <summary>MAKE-INSTANCE with a class designator that is not a literal symbol (a
    /// variable holding a symbol or a class object). With no method of the program on
    /// MAKE-INSTANCE the generic function would only reach its default method, so that is
    /// called directly; any other method, a redefinition of the function, or a customised
    /// invocation protocol sends the call through the generic function as before.</summary>
    public static LispObject MakeInstanceDyn(LispObject classSpec, LispObject[] initargs)
    {
        var gf = MakeInstanceGf;
        var sym = _makeInstanceSym ??= Startup.Sym("MAKE-INSTANCE");
        if (gf != null && gf.Methods.Count == 1 && ReferenceEquals(gf.Methods[0], MakeInstanceDefaultMethod)
            && gf.DispatchFunction == null && ReferenceEquals(sym.Function, gf)
            && !(AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf)))
            return MakeInstanceWithInitargs(classSpec, initargs);
        var all = new LispObject[initargs.Length + 1];
        all[0] = classSpec;
        Array.Copy(initargs, 0, all, 1, initargs.Length);
        if (sym.Function is LispFunction lf) return lf.Invoke(all);
        throw new LispErrorException(new LispError("MAKE-INSTANCE is not a function"));
    }

    [System.Runtime.CompilerServices.MethodImpl((System.Runtime.CompilerServices.MethodImplOptions)512)] // AggressiveOptimization (not in netstandard2.0)
    public static LispObject MakeInstanceWithInitargs(LispObject classSpec, params LispObject[] initargs)
    {
        if (classSpec is LispClass lcls && (initargs.Length & 1) == 0
            && LivePlan(lcls) is { } cplan && MakeFromPlan(cplan, initargs) is { } cmade)
            return MultipleValues.Alone(cmade);
        if (classSpec is Symbol sym && (initargs.Length & 1) == 0
            && (sym.ClassMemo is { } memo && memo.Epoch == ClassRegistry.Epoch
                    ? memo.AsClass : FindClassOrNil(sym) as LispClass) is { } cls
            && LivePlan(cls) is { } plan && MakeFromPlan(plan, initargs) is { } made)
            return MultipleValues.Alone(made);
        // A MAKE-INSTANCE method of the program applies to this class (its metaclass
        // is specialised on): the class name goes through the generic function, as
        // AMOP's method on SYMBOL does by calling MAKE-INSTANCE on the class.
        if (classSpec is Symbol csym && MakeInstanceGf != null && MakeInstanceGf.Methods.Count > 1
            && FindClassOrNil(csym) is LispClass ocls && MakeInstanceOverridden(ocls))
        {
            var viaGf = new LispObject[initargs.Length + 1];
            viaGf[0] = ocls;
            Array.Copy(initargs, 0, viaGf, 1, initargs.Length);
            return MakeInstanceGf.Invoke(viaGf);
        }
        return MultipleValues.Alone(MakeInstanceOf(classSpec, initargs));
    }

    // MAKE-INSTANCE of a literal class name with one to three literal keywords: the
    // initargs arrive as arguments. The plan builds the instance from them; anything the
    // plan does not take (no plan yet, :DEFAULT-INITARGS, :AFTER methods, a keyword not
    // seen before, a program method on MAKE-INSTANCE) goes the array way, as before.
    public static LispObject MakeInstanceKV1(LispObject classSpec, LispObject k0, LispObject v0)
        => MakeKV(classSpec, 1, k0, v0, null, null, null, null);

    public static LispObject MakeInstanceKV2(LispObject classSpec, LispObject k0, LispObject v0,
                                             LispObject k1, LispObject v1)
        => MakeKV(classSpec, 2, k0, v0, k1, v1, null, null);

    public static LispObject MakeInstanceKV3(LispObject classSpec, LispObject k0, LispObject v0,
                                             LispObject k1, LispObject v1, LispObject k2, LispObject v2)
        => MakeKV(classSpec, 3, k0, v0, k1, v1, k2, v2);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static LispObject MakeKV(LispObject classSpec, int n, LispObject k0, LispObject v0,
                                     LispObject? k1, LispObject? v1, LispObject? k2, LispObject? v2)
    {
        if (classSpec is Symbol sym
            && (sym.ClassMemo is { } memo && memo.Epoch == ClassRegistry.Epoch
                    ? memo.AsClass : FindClassOrNil(sym) as LispClass) is { } cls
            && LivePlan(cls) is { } plan && plan.TryMakeKV(n, k0, v0, k1, v1, k2, v2) is { } made)
            return MultipleValues.Alone(made);
        return MakeKVSlow(classSpec, n, k0, v0, k1, v1, k2, v2);
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static LispObject MakeKVSlow(LispObject classSpec, int n, LispObject k0, LispObject v0,
                                         LispObject? k1, LispObject? v1, LispObject? k2, LispObject? v2)
    {
        var args = new LispObject[n * 2];
        args[0] = k0; args[1] = v0;
        if (n > 1) { args[2] = k1!; args[3] = v1!; }
        if (n > 2) { args[4] = k2!; args[5] = v2!; }
        return MakeInstanceWithInitargs(classSpec, args);
    }

    /// <summary>The MAKE-INSTANCE generic function and its default method, set when
    /// they are made (Runtime.CLOS.Builtins).</summary>
    internal static GenericFunction? MakeInstanceGf;
    internal static LispMethod? MakeInstanceDefaultMethod;

    /// <summary>True when a method of the program on MAKE-INSTANCE applies to the class
    /// object CLS: one specialised on a class of its metaclass, or EQL to it. Only
    /// asked once such a method exists.</summary>
    internal static bool MakeInstanceOverridden(LispClass cls)
    {
        var gf = MakeInstanceGf;
        if (gf == null || gf.Methods.Count <= 1) return false;
        var meta = ClassOf(cls) as LispClass;
        if (meta == null) return false;
        foreach (var m in gf.Methods)
        {
            if (ReferenceEquals(m, MakeInstanceDefaultMethod) || m.Specializers.Length == 0) continue;
            var spec = m.Specializers[0];
            if (spec is LispClass sc)
            {
                if (!IsSystemClass(sc, "T") && Array.IndexOf(meta.ClassPrecedenceList, sc) >= 0) return true;
            }
            else if (spec is EqlSpecializer es && ReferenceEquals(es.Object, cls))
                return true;
        }
        return false;
    }

    // Method changes and class finalization move MethodEpoch; :DEFAULT-INITARGS set
    // without a finalization clear HasSimpleInitialization.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static CtorPlan? LivePlan(LispClass cls)
        => cls.Ctor is { } plan && plan.Epoch == GenericFunction.MethodEpoch
           && ReferenceEquals(plan.SourceDefaults, cls.DefaultInitargs)
           && cls.HasSimpleInitialization && cls.Metaclass == null ? plan : null;

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static LispInstance? MakeFromPlan(CtorPlan plan, LispObject[] initargs)
    {
        var inst = plan.TryMake(initargs, out var effective);
        if (inst == null || plan.Afters.Length == 0) return inst;
        return RunAfters(plan, inst, effective);
    }

    private static LispInstance RunAfters(CtorPlan plan, LispInstance inst, LispObject[] initargs)
    {
        var args = new LispObject[initargs.Length + 1];
        args[0] = inst;
        Array.Copy(initargs, 0, args, 1, initargs.Length);
        foreach (var m in plan.Afters) InvokeAux(m, args);
        return inst;
    }

    /// <summary>The :AFTER methods of INITIALIZE-INSTANCE applicable to instances of CLS,
    /// least specific first, when they are all it has besides the default method and
    /// SHARED-INITIALIZE has none of its own for CLS; null otherwise.</summary>
    private static LispMethod[]? InitAfterMethodsOnly(LispClass cls)
    {
        _initializeInstanceSym ??= Startup.Sym("INITIALIZE-INSTANCE");
        if (_initializeInstanceSym.Function is not GenericFunction gf || gf.MethodCombination != null
            || gf.DispatchFunction != null
            || (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            || !SharedInitializeIsDefault(cls))
            return null;
        var cpl = cls.ClassPrecedenceList;
        var afters = new List<LispMethod>();
        foreach (var m in gf.Methods)
        {
            if (AllTSpecializers(m) && m.Qualifiers.Length == 0) continue;
            if (m.Specializers.Length != 1 || m.Specializers[0] is not LispClass sc) return null;
            if (Array.IndexOf(cpl, sc) < 0) continue;
            if (m.Qualifiers.Length != 1 || m.Qualifiers[0] is not Symbol q || q.Name != "AFTER"
                || q.HomePackage?.Name != "KEYWORD")
                return null;
            afters.Add(m);
        }
        afters.Sort((a, b) => Array.IndexOf(cpl, (LispClass)b.Specializers[0])
                                  .CompareTo(Array.IndexOf(cpl, (LispClass)a.Specializers[0])));
        return afters.ToArray();
    }

    private static LispObject MakeInstanceOf(LispObject classSpec, LispObject[] initargs)
    {
        // Validate: initargs must be even (key-value pairs)
        if (initargs.Length % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "MAKE-INSTANCE: odd number of keyword arguments"));

        // classSpec is a symbol (quoted class name)
        LispClass cls;
        if (classSpec is Symbol sym)
        {
            if (!_classRegistry.TryGetValue(sym, out cls!))
                throw new LispErrorException(new LispError($"MAKE-INSTANCE: no class named {sym.Name}"));
        }
        else if (classSpec is LispClass lc)
        {
            cls = lc;
        }
        else
            throw new LispErrorException(new LispTypeError("MAKE-INSTANCE: invalid class specifier", classSpec));

        if (LivePlan(cls) is { } plan && MakeFromPlan(plan, initargs) is { } made)
            return made;

        // A class metaobject class makes a class, not an ordinary instance. Checked
        // before the initarg validation below: :DIRECT-SUPERCLASSES and :DIRECT-SLOTS
        // are class-creation initargs, not slots of STANDARD-CLASS.
        foreach (var cplCls in cls.ClassPrecedenceList)
            if (IsSystemClass(cplCls, "CLASS"))
                return MakeClassMetaobject(cls, initargs);

        // A slot-definition class makes a SlotDefinition, which is what a slot
        // definition is here; a LispInstance of it has none of its slots.
        foreach (var cplCls in cls.ClassPrecedenceList)
            if (IsSystemClass(cplCls, "SLOT-DEFINITION"))
                return MakeSlotDefinitionMetaobject(cls, initargs);

        // A structure class builds a structure: the slots take the keywords of
        // their names as initargs (as SBCL's do) and the others their DEFSTRUCT
        // defaults. The DEFSTRUCT record lives on the compiler side, so the work
        // is done by a Lisp helper there.
        if (cls.IsStructureClass && !ReferenceEquals(cls.Name, Startup.Sym("STRUCTURE-OBJECT")))
        {
            if (MakeStructureInstanceFast(cls, initargs) is { } structure)
                return structure;
            if (MakeStructureInstanceSlow(cls, initargs) is { } slowStructure)
                return slowStructure;
            var helper = Package.FindPackage("DOTCL.CIL-COMPILER")
                ?.FindSymbol("%MAKE-STRUCTURE-INSTANCE").symbol?.Function;
            if (helper != null)
            {
                LispObject plist = Nil.Instance;
                for (int i = initargs.Length - 1; i >= 0; i--) plist = new Cons(initargs[i], plist);
                return Funcall(helper, new LispObject[] { cls.Name, plist });
            }
        }

        // Cannot instantiate built-in classes
        if (cls.IsBuiltIn)
            throw new LispErrorException(new LispError(
                $"Cannot create instances of built-in class {cls.Name.Name} with MAKE-INSTANCE"));

        // For classes that need specialized C# allocation (generic-function, method subtypes),
        // create the right C# object directly and then call initialize-instance.
        // (allocate-instance GF dispatch uses class-of(cls)=STANDARD-CLASS so can't specialize on subclass names.)
        if (HasSpecializedAllocator(cls))
        {
            LispObject? allocated2 = AllocateSpecialized(cls);
            if (allocated2 != null)
            {
                var iiSym2 = Startup.Sym("INITIALIZE-INSTANCE");
                if (iiSym2.Function is LispFunction iiFn2)
                {
                    // Defaulted initargs are due here as well (CLHS 7.1.3). This branch
                    // returns before the general path below reaches them, so it makes the
                    // same call: one function, so one set of precedence rules.
                    var effective2 = ApplyDefaultInitargs(cls, initargs);
                    var iiArgs2 = new LispObject[1 + effective2.Length];
                    iiArgs2[0] = allocated2;
                    Array.Copy(effective2, 0, iiArgs2, 1, effective2.Length);
                    iiFn2.Invoke(iiArgs2);
                }
                return allocated2;
            }
        }

        // Custom metaclass: honor the full ALLOCATE-INSTANCE / SLOT-VALUE-USING-CLASS
        // protocol (e.g. McCLIM's class-with-dynamic-slots). Skip every fast path so the
        // instance is allocated via the GF and slot writes go through SHARED-INITIALIZE,
        // which routes through (setf slot-value-using-class) for custom metaclasses.
        bool customMeta = cls.Metaclass != null;

        // Ultra-fast path: bypass GF dispatch for simple classes with no custom methods
        if (!customMeta && cls.HasSimpleInitialization && CanUseSimplePath(cls))
        {
            // Validate initargs even on fast path (CLHS 7.1.2)
            int epoch = GenericFunction.MethodEpoch;
            ValidateInitargs(cls, initargs, 0);
            RecordCtorKeys(cls, initargs, epoch, Array.Empty<LispMethod>());
            initargs = ApplyDefaultInitargs(cls, initargs);

            var fastInst = new LispInstance(cls);
            // Inline shared-initialize: apply initargs then initforms
            if (cls.InitargToSlotIndex != null)
            {
                for (int i = 0; i < initargs.Length - 1; i += 2)
                {
                    string initargName = initargs[i] is Symbol s ? s.Name : initargs[i].ToString()!;
                    if (cls.InitargToSlotIndex.TryGetValue(initargName, out int slotIdx))
                    {
                        if (fastInst.Slots[slotIdx] == null)
                            fastInst.Slots[slotIdx] = initargs[i + 1];
                    }
                }
            }
            // Apply initforms for unset slots
            for (int i = 0; i < cls.EffectiveSlots.Length; i++)
            {
                if (fastInst.Slots[i] == null && cls.EffectiveSlots[i].InitformThunk != null)
                    fastInst.Slots[i] = cls.EffectiveSlots[i].InitformThunk!.Invoke();
            }
            return fastInst;
        }

        // Per CLHS 7.1.2: Validate initargs
        int slowEpoch = GenericFunction.MethodEpoch;
        ValidateInitargs(cls, initargs, 0);
        if (!customMeta && cls.HasSimpleInitialization && !IsConditionClass(cls)
            && InitAfterMethodsOnly(cls) is { Length: > 0 } afters)
            RecordCtorKeys(cls, initargs, slowEpoch, afters);

        // Custom metaclass: allocate through the ALLOCATE-INSTANCE GF so user overrides run.
        var inst = customMeta ? AllocateViaGF(cls, initargs) : new LispInstance(cls);

        // Fast path: no default initargs, no custom init methods, not a condition class,
        // and no class-allocated slots with initargs (which need special handling).
        // Directly set slots from initargs using cached initarg->slot map, then apply initforms.
        // This avoids GF dispatch, array allocation, and redundant initarg validation.
        if (!customMeta && cls.DefaultInitargs.Length == 0 && cls.CanUseFastMakeInstance
            && !IsConditionClass(cls) && !HasCustomInitMethods(cls))
        {
            var map = cls.InitargSlotMap;
            for (int i = 0; i < initargs.Length - 1; i += 2)
            {
                string? keyName = initargs[i] switch
                {
                    Symbol s => s.Name,
                    Nil => "NIL",
                    T => "T",
                    _ => null
                };
                if (keyName != null && map.TryGetValue(keyName, out int slotIdx))
                {
                    if (inst.Slots[slotIdx] == null) // first value wins
                        inst.Slots[slotIdx] = initargs[i + 1];
                }
            }
            // Apply initforms for unset slots
            for (int i = 0; i < cls.EffectiveSlots.Length; i++)
            {
                var slot = cls.EffectiveSlots[i];
                if (slot.InitformThunk != null)
                {
                    if (slot.IsClassAllocation)
                    {
                        var ownerClass = FindClassSlotOwner(cls, slot.Name.Name);
                        if (!ownerClass.ClassSlotValues.TryGetValue(slot.Name.Name, out var cv) || cv == null)
                            ownerClass.ClassSlotValues[slot.Name.Name] = slot.InitformThunk.Invoke();
                    }
                    else if (inst.Slots[i] == null)
                    {
                        inst.Slots[i] = slot.InitformThunk.Invoke();
                    }
                }
            }
            return inst;
        }

        // Slow path: default initargs present or custom methods defined.
        // Per CLHS 7.1.3: Apply default initargs before calling shared-initialize.
        LispObject[] effectiveInitargs = ApplyDefaultInitargs(cls, initargs);


        // Per CLHS 7.1: make-instance calls initialize-instance with (instance . initargs)
        // initialize-instance then calls shared-initialize with slot-names = T
        var iiArgs = new LispObject[1 + effectiveInitargs.Length];
        iiArgs[0] = inst;
        Array.Copy(effectiveInitargs, 0, iiArgs, 1, effectiveInitargs.Length);

        var iiSym = Startup.Sym("INITIALIZE-INSTANCE");
        if (iiSym.Function is LispFunction iiFn)
        {
            iiFn.Invoke(iiArgs);
        }
        else
        {
            // Fallback: call shared-initialize directly
            var siArgs = new LispObject[2 + effectiveInitargs.Length];
            siArgs[0] = inst;
            siArgs[1] = T.Instance;
            Array.Copy(effectiveInitargs, 0, siArgs, 2, effectiveInitargs.Length);
            SharedInitialize(siArgs);
        }

        return inst;
    }

    /// <summary>Fast structure MAKE-INSTANCE: slot initargs, defaults, then :after methods.</summary>
    private static LispObject? MakeStructureInstanceFast(LispClass cls, LispObject[] initargs)
    {
        var plan = StructurePlan(cls);
        if (plan == null) return null;
        int epoch = GenericFunction.MethodEpoch;
        if (plan.Epoch != epoch)
        {
            if (!BuildStructureMethodPlan(plan, cls, epoch)) return null;
        }
        else if (!plan.MethodsSimple)
            return null;

        ValidateStructureInitargs(plan, cls, initargs);

        var values = new LispObject[plan.Keys.Length];
        var supplied = new bool[plan.Keys.Length];
        for (int i = 0; i + 1 < initargs.Length; i += 2)
            if (initargs[i] is Symbol key && plan.KeyToSlot.TryGetValue(key, out int idx)
                && !supplied[idx])
            {
                values[idx] = initargs[i + 1];
                supplied[idx] = true;
            }
        for (int i = 0; i < values.Length; i++)
            if (!supplied[i])
                values[i] = plan.Defaults[i] is { } thunk ? thunk.Invoke() : Nil.Instance;

        var inst = LispStruct.Create(cls.Name, values);
        RunStructureAfters(plan, inst, initargs);
        return inst;
    }

    /// <summary>Structure MAKE-INSTANCE when custom initialization methods exist: run the protocol.</summary>
    private static LispObject? MakeStructureInstanceSlow(LispClass cls, LispObject[] initargs)
    {
        var plan = StructurePlan(cls);
        if (plan == null) return null;
        ValidateStructureInitargs(plan, cls, initargs);
        var inst = new LispStruct(cls.Name, new LispObject[plan.Keys.Length]);
        if (Startup.Sym("INITIALIZE-INSTANCE").Function is LispFunction ii)
        {
            var args = new LispObject[initargs.Length + 1];
            args[0] = inst;
            Array.Copy(initargs, 0, args, 1, initargs.Length);
            ii.Invoke(args);
        }
        else
        {
            InitializeStructureSlots(inst, T.Instance, initargs, 0);
        }
        return inst;
    }

    private static void ValidateStructureInitargs(StructCtorPlan plan, LispClass cls,
                                                  LispObject[] initargs)
    {
        for (int i = 0; i + 1 < initargs.Length; i += 2)
            if (IsKeyword(initargs[i], "ALLOW-OTHER-KEYS"))
            {
                if (initargs[i + 1] is not Nil) return;
                break;
            }
        for (int i = 0; i + 1 < initargs.Length; i += 2)
            if (initargs[i] is not Symbol key
                || (!IsKeyword(key, "ALLOW-OTHER-KEYS") && !plan.KeyToSlot.ContainsKey(key)))
                throw new LispErrorException(new LispError(
                    $"Invalid initarg {initargs[i]} for class {cls.Name}"));
    }

    private static bool IsKeyword(LispObject obj, string name)
        => obj is Symbol s && s.Name == name && s.HomePackage?.Name == "KEYWORD";

    private static StructCtorPlan? StructurePlan(LispClass cls)
    {
        if (cls.StructCtor is { } cached) return cached;
        var helper = Package.FindPackage("DOTCL.CIL-COMPILER")
            ?.FindSymbol("%STRUCTURE-INSTANCE-PLAN").symbol?.Function;
        if (helper == null) return null;
        LispObject planObj;
        try { planObj = Funcall(helper, new LispObject[] { cls.Name }); }
        catch (LispErrorException) { return null; }
        if (planObj is not Cons pair || pair.Car is not LispVector keyVec
            || pair.Cdr is not Cons tail || tail.Car is not LispVector defVec
            || keyVec.Length != defVec.Length)
            return null;
        int n = keyVec.Length;
        var keys = new Symbol[n];
        var defaults = new LispFunction?[n];
        var map = new Dictionary<Symbol, int>(ReferenceEqualityComparer.Instance);
        for (int i = 0; i < n; i++)
        {
            if (keyVec[i] is not Symbol key) return null;
            keys[i] = key;
            map[key] = i;
            defaults[i] = defVec[i] as LispFunction;
        }
        var built = new StructCtorPlan(keys, map, defaults);
        return System.Threading.Interlocked.CompareExchange(ref cls.StructCtor, built, null) ?? built;
    }

    /// <summary>Collect :after methods when only the default primary method also applies.</summary>
    private static bool BuildStructureMethodPlan(StructCtorPlan plan, LispClass cls, int epoch)
    {
        plan.Epoch = epoch;
        var ii = Startup.Sym("INITIALIZE-INSTANCE").Function as GenericFunction;
        var si = Startup.Sym("SHARED-INITIALIZE").Function as GenericFunction;
        if (ii == null || si == null
            || ii.MethodCombination != null || si.MethodCombination != null
            || ii.DispatchFunction != null || si.DispatchFunction != null
            || (AnyInvocationProtocolCustomized
                && (UsesInvocationProtocol(ii) || UsesInvocationProtocol(si))))
        {
            plan.MethodsSimple = false;
            return false;
        }
        if (!CollectStructureAfters(ii, cls, out var initAfters)
            || !CollectStructureAfters(si, cls, out var sharedAfters))
        {
            plan.MethodsSimple = false;
            return false;
        }
        plan.InitAfters = initAfters;
        plan.SharedAfters = sharedAfters;
        plan.MethodsSimple = true;
        return true;
    }

    private static bool CollectStructureAfters(GenericFunction gf, LispClass cls,
                                               out LispMethod[] afters)
    {
        List<LispMethod>? found = null;
        foreach (var m in gf.MethodsArray)
        {
            if (!IsMethodApplicableToClass(m, cls)) continue;
            if (AllTSpecializers(m) && m.Qualifiers.Length == 0) continue;
            if (m.Specializers.Length == 0 || m.Specializers[0] is not LispClass)
            {
                afters = Array.Empty<LispMethod>();
                return false;
            }
            if (m.Qualifiers.Length == 1 && IsKeyword(m.Qualifiers[0], "AFTER"))
            {
                (found ??= new List<LispMethod>()).Add(m);
                continue;
            }
            afters = Array.Empty<LispMethod>();
            return false;
        }
        afters = found?.ToArray() ?? Array.Empty<LispMethod>();
        if (afters.Length > 1)
        {
            var sortArgs = new LispObject[] { cls.Prototype };
            Array.Sort(afters, (a, b) => CompareMethodSpecificity(a, b, sortArgs));
        }
        return true;
    }

    private static void RunStructureAfters(StructCtorPlan plan, LispObject inst,
                                           LispObject[] initargs)
    {
        if (plan.SharedAfters.Length != 0)
        {
            var args = new LispObject[initargs.Length + 2];
            args[0] = inst;
            args[1] = T.Instance;
            Array.Copy(initargs, 0, args, 2, initargs.Length);
            for (int i = plan.SharedAfters.Length - 1; i >= 0; i--)
                InvokeAux(plan.SharedAfters[i], args);
        }
        if (plan.InitAfters.Length != 0)
        {
            var args = new LispObject[initargs.Length + 1];
            args[0] = inst;
            Array.Copy(initargs, 0, args, 1, initargs.Length);
            for (int i = plan.InitAfters.Length - 1; i >= 0; i--)
                InvokeAux(plan.InitAfters[i], args);
        }
    }

    // After ValidateInitargs accepted INITARGS under EPOCH: remember each key's slot.
    // :ALLOW-OTHER-KEYS is never remembered, so a call that passes it keeps validating.
    private static void RecordCtorKeys(LispClass cls, LispObject[] initargs, int epoch, LispMethod[] afters)
    {
        var plan = cls.Ctor;
        if (plan == null || plan.Epoch != epoch)
        {
            if (GenericFunction.MethodEpoch != epoch) return;
            // A MAKE-INSTANCE method of the program applies to the class: no fast plan.
            if (MakeInstanceOverridden(cls)) return;
            cls.Ctor = plan = new CtorPlan(cls, epoch, afters);
        }
        var map = cls.InitargToSlotIndex;
        for (int i = 0; i < initargs.Length; i += 2)
            if (initargs[i] is Symbol s && s.Name == "ALLOW-OTHER-KEYS") return;
        for (int i = 0; i < initargs.Length; i += 2)
        {
            if (initargs[i] is not Symbol s) return;
            plan.AddKey(s, map != null && map.TryGetValue(s.Name, out int idx) ? idx : -1);
        }
    }

    /// <summary>
    /// CLHS 7.1.3: append a defaulted initarg for each default initarg of CLS whose key
    /// the caller did not supply. The supplied initargs stay in front, so they keep
    /// winning over the defaulted ones, which in turn win over slot initforms. The value
    /// form is a function per the MOP, so it runs once per call and is not cached.
    /// Returns INITARGS itself when there is nothing to add.
    /// </summary>
    private static LispObject[] ApplyDefaultInitargs(LispClass cls, LispObject[] initargs)
    {
        if (cls.DefaultInitargs.Length == 0) return initargs;

        // Collect user-supplied keys (every other element starting at 0)
        var suppliedKeys = new HashSet<string>();
        for (int i = 0; i < initargs.Length - 1; i += 2)
        {
            if (initargs[i] is Symbol keySym)
                suppliedKeys.Add(keySym.Name);
        }

        // Check if any defaults need to be added
        var extras = new List<LispObject>();
        foreach (var (key, initformSource, thunk) in cls.DefaultInitargs)
        {
            bool supplied = suppliedKeys.Contains(key.Name);
            if (supplied && cls.HasSymbolCollision)
            {
                // A::X supplied does not supply B::X.
                supplied = false;
                for (int i = 0; i < initargs.Length - 1; i += 2)
                    if (initargs[i] is Symbol ks && LispClass.SameSlotName(ks, key)) { supplied = true; break; }
            }
            if (!supplied)
            {
                extras.Add(key);
                // Unwrap MvReturn: default-initarg thunks may return multiple values
                // (e.g. ensure-gethash returns (values value present-p)); use primary value only.
                extras.Add(UnwrapMv(thunk.Invoke(Array.Empty<LispObject>())));
            }
        }

        if (extras.Count == 0) return initargs;

        var effective = new LispObject[initargs.Length + extras.Count];
        Array.Copy(initargs, effective, initargs.Length);
        for (int i = 0; i < extras.Count; i++)
            effective[initargs.Length + i] = extras[i];
        return effective;
    }

    /// <summary>True when C is the class the system defines under NAME (a
    /// COMMON-LISP or MOP class name), not merely a class whose name is a symbol
    /// of that name in some other package: a library's own CLASS or METHOD type
    /// (Coalton defines both) is an ordinary class.</summary>
    internal static bool IsSystemClass(LispClass c, string name)
        => c.Name.Name == name && ReferenceEquals(c.Name, Startup.Sym(name));

    /// <summary>Check if a class is a condition class (CONDITION in its CPL).</summary>
    private static bool IsConditionClass(LispClass cls)
    {
        if (cls.CachedIsConditionClass is bool cached) return cached;
        bool result = false;
        foreach (var c in cls.ClassPrecedenceList)
            if (IsSystemClass(c, "CONDITION")) { result = true; break; }
        cls.CachedIsConditionClass = result;
        return result;
    }

    /// <summary>
    /// Check (and cache) whether a class can use the fast make-instance path.
    /// Returns true only if no non-default methods on initialize-instance or
    /// shared-initialize are applicable to instances of this class.
    /// </summary>
    private static bool CanUseSimplePath(LispClass cls)
    {
        if (cls.SimpleInitChecked) return cls.SimpleInitValid;
        cls.SimpleInitChecked = true;

        _initializeInstanceSym ??= Startup.Sym("INITIALIZE-INSTANCE");
        if (_initializeInstanceSym.Function is GenericFunction iiGf)
        {
            foreach (var m in iiGf.Methods)
            {
                if (AllTSpecializers(m) && m.Qualifiers.Length == 0)
                    continue;
                if (IsMethodApplicableToClass(m, cls))
                {
                    cls.SimpleInitValid = false;
                    return false;
                }
            }
        }

        _sharedInitializeSym ??= Startup.Sym("SHARED-INITIALIZE");
        if (_sharedInitializeSym.Function is GenericFunction siGf)
        {
            foreach (var m in siGf.Methods)
            {
                if (AllTSpecializers(m) && m.Qualifiers.Length == 0)
                    continue;
                if (IsMethodApplicableToClass(m, cls))
                {
                    cls.SimpleInitValid = false;
                    return false;
                }
            }
        }

        cls.SimpleInitValid = true;
        return true;
    }

    private static bool AllTSpecializers(LispMethod m)
    {
        foreach (var s in m.Specializers)
        {
            if (s is not LispClass cls || cls.Name.Name != "T")
                return false;
        }
        return true;
    }

    private static bool IsMethodApplicableToClass(LispMethod m, LispClass cls)
    {
        if (m.Specializers.Length == 0) return true;
        var spec = m.Specializers[0];
        if (spec is LispClass specCls)
        {
            if (specCls.Name.Name == "T") return true;
            foreach (var c in cls.ClassPrecedenceList)
            {
                if (c == specCls) return true;
            }
            return false;
        }
        return true; // EQL specializer; conservative
    }

    /// <summary>Validate initargs against class slot initargs and default initargs.</summary>
    /// <param name="cls">The class to validate against</param>
    /// <param name="args">The argument array containing initargs</param>
    /// <param name="startIdx">Index where initargs start in the array</param>
    private static void ValidateInitargs(LispClass cls, LispObject[] args, int startIdx,
        HashSet<string>? extraMethodKeys = null)
    {
        int count = args.Length - startIdx;
        if (count <= 0) return;

        // Check if :allow-other-keys t is in the supplied initargs
        bool allowOtherKeys = false;
        for (int i = startIdx; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol aok)
            {
                var aokName = aok.Name.Length > 0 && aok.Name[0] == ':' ? aok.Name[1..] : aok.Name;
                if (aokName == "ALLOW-OTHER-KEYS")
                {
                    allowOtherKeys = !(args[i + 1] is Nil);
                    break;
                }
            }
        }

        // Also check default-initargs for :allow-other-keys t
        if (!allowOtherKeys)
        {
            foreach (var (key, initformSource, thunk) in cls.DefaultInitargs)
            {
                if (key.Name == "ALLOW-OTHER-KEYS")
                {
                    var val = thunk.Invoke(Array.Empty<LispObject>());
                    if (!(val is Nil))
                    {
                        allowOtherKeys = true;
                        break;
                    }
                }
            }
        }

        if (!allowOtherKeys)
        {
            var validKeys = cls.CachedValidInitargKeys;
            if (validKeys == null)
            {
                validKeys = new HashSet<string>();
                validKeys.Add("ALLOW-OTHER-KEYS");
                foreach (var slot in cls.EffectiveSlots)
                    foreach (var ia in slot.Initargs)
                        validKeys.Add(ia.Name);
                foreach (var (key, _, _) in cls.DefaultInitargs)
                    validKeys.Add(key.Name);
                // CLHS 7.1.2: keyword args of applicable initialize-instance and
                // shared-initialize methods are also valid initargs.
                bool methodAOK = AddMethodKeysForClass(cls, Startup.Sym("INITIALIZE-INSTANCE"), validKeys)
                              || AddMethodKeysForClass(cls, Startup.Sym("SHARED-INITIALIZE"), validKeys);
                if (methodAOK) { cls.CachedValidInitargKeys = null; return; } // allow-other-keys from method
                // Condition classes universally accept :format-control and :format-arguments
                // because dotcl's runtime passes them when signaling errors of any type.
                if (IsConditionClass(cls))
                {
                    validKeys.Add("FORMAT-CONTROL");
                    validKeys.Add("FORMAT-ARGUMENTS");
                }
                cls.CachedValidInitargKeys = validKeys;
            }

            for (int i = startIdx; i < args.Length - 1; i += 2)
            {
                string keyName;
                if (args[i] is Symbol keySym)
                {
                    // Normalize: strip leading colon so :PACKAGE and PACKAGE both match
                    var n = keySym.Name;
                    keyName = n.Length > 0 && n[0] == ':' ? n[1..] : n;
                }
                else if (args[i] is Nil)
                    keyName = "NIL";
                else if (args[i] is T)
                    keyName = "T";
                else
                    throw new LispErrorException(new LispProgramError(
                        $"Invalid initarg key {args[i]} for class {cls.Name.Name}: not a symbol"));
                if (!validKeys.Contains(keyName) && (extraMethodKeys == null || !extraMethodKeys.Contains(keyName)))
                    throw new LispErrorException(new LispProgramError(
                        $"Invalid initarg :{keyName} for class {cls.Name.Name}"));
            }
        }
    }

    public static LispObject ChangeClass(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("CHANGE-CLASS: requires at least 2 arguments"));
        // Initargs must be key-value pairs (even count)
        if ((args.Length - 2) % 2 != 0)
            throw new LispErrorException(new LispProgramError("CHANGE-CLASS: odd number of keyword arguments"));

        LispObject instance = args[0];
        LispObject newClassSpec = args[1];

        if (instance is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("CHANGE-CLASS: not a CLOS instance", instance));

        LispClass newClass;
        if (newClassSpec is LispClass lc)
            newClass = lc;
        else if (newClassSpec is Symbol sym)
        {
            if (!_classRegistry.TryGetValue(sym, out newClass!))
                throw new LispErrorException(new LispError($"CHANGE-CLASS: no class named {sym.Name}"));
            // CLHS change-class: the method for a symbol calls CHANGE-CLASS again with the
            // class named, so a method on the class (a :before, say) runs too.
            if (Startup.Sym("CHANGE-CLASS").Function is GenericFunction ccGf)
            {
                var again = (LispObject[])args.Clone();
                again[1] = newClass;
                return ccGf.Invoke(again);
            }
        }
        else
            throw new LispErrorException(new LispTypeError("CHANGE-CLASS: invalid class specifier", newClassSpec));

        // Cannot change-class to a built-in class (CLHS 7.2)
        if (newClass.IsBuiltIn)
            throw new LispErrorException(new LispError(
                $"CHANGE-CLASS: cannot change class to built-in class {newClass.Name.Name}"));

        // Per CLHS 7.2: validate initargs against new class
        ValidateInitargs(newClass, args, 2);

        // An obsolete instance is updated to its (redefined) class first, so the
        // values carried over are read against the layout they were written in.
        inst.EnsureCurrent();
        var oldClass = inst.Class;
        var oldSlots = inst.Slots;

        // Create a "previous" snapshot - a shallow copy of the instance before change
        // Per CLHS, the first arg to UIFDC should be a copy with the OLD class
        var previous = new LispInstance(oldClass);
        previous.Slots = new LispObject?[oldSlots.Length];
        Array.Copy(oldSlots, previous.Slots, oldSlots.Length);

        // Modify the instance to use the new class
        inst.Class = newClass;
        inst.Slots = new LispObject?[newClass.EffectiveSlots.Length];
        inst.Layout = newClass.Layout;

        // Copy slot values for slots with same name in both old and new class (CLHS 7.2).
        // A slot that is :allocation :class in the NEW class is NOT affected by
        // change-class: it keeps its existing shared value and must not be
        // overwritten from the old instance (CLHS 7.2.1; ANSI CHANGE-CLASS.3.2).
        foreach (var newSlot in newClass.EffectiveSlots)
        {
            if (newSlot.IsClassAllocation) continue;
            if (newClass.TryGetSlotIndexExact(newSlot.Name, out int newIdx))
            {
                // By symbol: A::X in the old class is not B::X in the new one.
                if (oldClass.TryGetSlotIndexExact(newSlot.Name, out int oldIdx))
                {
                    // Read value from old class, handling class-allocated (shared) slots
                    LispObject? val;
                    if (oldClass.EffectiveSlots[oldIdx].IsClassAllocation)
                    {
                        var ownerClass = FindClassSlotOwner(oldClass, newSlot.Name.Name);
                        ownerClass.ClassSlotValues.TryGetValue(newSlot.Name.Name, out val);
                    }
                    else
                    {
                        val = oldSlots[oldIdx];
                    }
                    inst.Slots[newIdx] = val;
                }
            }
        }

        // Call update-instance-for-different-class
        var uifdcSym = Startup.Sym("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS");
        if (uifdcSym.Function is LispFunction uifdcFn)
        {
            var uifdcArgs = new LispObject[2 + (args.Length - 2)]; // previous, current, initargs...
            uifdcArgs[0] = previous;
            uifdcArgs[1] = instance;
            Array.Copy(args, 2, uifdcArgs, 2, args.Length - 2);
            uifdcFn.Invoke(uifdcArgs);
        }

        return MultipleValues.Alone(instance);
    }

    /// <summary>Allocate the C# object a metaobject class needs, or null when CLS is an
    /// ordinary class. A method class must produce a LispMethod and a generic function
    /// class a GenericFunction: a LispInstance that merely answers the right CLASS-OF is
    /// neither, so it cannot be given to ADD-METHOD or funcalled, and the
    /// INITIALIZE-INSTANCE primary for METHOD passes non-LispMethod arguments straight
    /// through. Both MAKE-INSTANCE and ALLOCATE-INSTANCE come here for the same reason:
    /// ALLOCATE-INSTANCE dispatches on (class-of cls) = STANDARD-CLASS, so a method
    /// specialized on STANDARD-METHOD is never applicable.</summary>
    private static LispObject? AllocateSpecialized(LispClass cls)
    {
        // Walk CPL to find the most specific recognized type
        foreach (var cplCls in cls.ClassPrecedenceList)
        {
            if (IsSystemClass(cplCls, "STANDARD-GENERIC-FUNCTION") || IsSystemClass(cplCls, "GENERIC-FUNCTION"))
            {
                var newGf = Runtime.NewDispatchingGF(Startup.Sym("UNNAMED"), -1);
                newGf.RequiredCount = 0;
                newGf.LambdaListInfoSet = true;
                newGf.StoredClass = cls;  // track actual Lisp class (may be substandard-generic-function etc.)
                return newGf;
            }
            if (IsSystemClass(cplCls, "METHOD"))
            {
                var newMethod = new LispMethod();
                // Track the actual Lisp class, the way the generic function branch
                // above does with StoredClass: CLASS-OF has to answer the class
                // that was instantiated, and the slots it adds beyond
                // STANDARD-METHOD are initialized against it.
                if (!IsSystemClass(cls, "STANDARD-METHOD")) newMethod.MetaClass = cls;
                return newMethod;
            }
        }
        // A funcallable instance that is not a generic function still has to BE
        // callable, and on dotcl the callable object that carries a class and
        // slots is the generic function. It starts with no methods, so calling
        // one before SET-FUNCALLABLE-INSTANCE-FUNCTION says there is no
        // applicable method -- AMOP leaves that case undefined.
        if (IsFuncallableClass(cls))
        {
            var funcallable = Runtime.NewDispatchingGF(Startup.Sym("UNNAMED"), -1);
            funcallable.RequiredCount = 0;
            funcallable.LambdaListInfoSet = true;
            funcallable.StoredClass = cls;
            return funcallable;
        }
        return null;
    }

    private static bool HasSpecializedAllocator(LispClass cls)
    {
        foreach (var s in cls.ClassPrecedenceList)
            if (IsSystemClass(s, "GENERIC-FUNCTION") || IsSystemClass(s, "METHOD")) return true;
        return IsFuncallableClass(cls);
    }

    /// <summary>True for a class whose instances must be callable: AMOP's
    /// FUNCALLABLE-STANDARD-CLASS as the metaclass, or FUNCALLABLE-STANDARD-OBJECT in
    /// the precedence list.</summary>
    internal static bool IsFuncallableClass(LispClass cls)
    {
        if (cls.Metaclass is { } meta)
            foreach (var m in meta.ClassPrecedenceList)
                if (IsSystemClass(m, "FUNCALLABLE-STANDARD-CLASS")) return true;
        foreach (var s in cls.ClassPrecedenceList)
            if (IsSystemClass(s, "FUNCALLABLE-STANDARD-OBJECT")) return true;
        return false;
    }
}
