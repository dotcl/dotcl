using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- CLOS operations ---

    // ConcurrentDictionary so concurrent DEFCLASS / FIND-CLASS doesn't corrupt the table.
    private static readonly ConcurrentDictionary<Symbol, LispClass> _classRegistry = new(SymbolIdentityComparer.Instance);
    // Maps .NET runtime Type to its CLOS class (for dotnet:define-class and dotnet:new instances).
    private static readonly ConcurrentDictionary<Type, LispClass> _dotNetTypeRegistry = new();
    private static Symbol? _initializeInstanceSym;
    private static Symbol? _sharedInitializeSym;
    /// <summary>Custom method combination registry: name -> (operator name, identity-with-one-argument)</summary>
    private static readonly ConcurrentDictionary<string, (string Operator, bool IdentityWithOneArg)> _methodCombinationRegistry = new();

    /// <summary>Long-form method combination registry: name -> LongFormMC</summary>
    private static readonly ConcurrentDictionary<string, LongFormMethodCombination> _longFormMCRegistry = new();

    public static void RegisterMethodCombination(string name, string operatorName, bool identityWithOneArg = false)
    {
        _methodCombinationRegistry[name] = (operatorName, identityWithOneArg);
    }

    /// <summary>Info for a long-form define-method-combination.</summary>
    internal class LongFormMethodCombination
    {
        /// <summary>Dynamic spec function: (mc-args-list) -> spec-list. Replaces static Groups.</summary>
        public LispFunction? SpecFunction;
        /// <summary>The body function: called with (mc-args... method-groups...) -> effective-method form</summary>
        public LispFunction? BodyFunction;
    }

    internal class MethodGroupSpec
    {
        public string Name = "";
        /// <summary>Qualifier pattern: null means match all (*), empty list means unqualified methods</summary>
        public LispObject? QualifierPattern;
        public bool MatchAll; // true for *
        public bool MatchUnqualified; // true for NIL qualifier pattern
        public string Order = "MOST-SPECIFIC-FIRST";
        public bool Required;
    }

    /// <summary>Resolve a class name to a Symbol for use as registry key.</summary>
    private static Symbol ToClassSymbol(LispObject name)
    {
        if (name is Symbol sym)
        {
            // Fast path: exact match (covers user-defined classes and CL built-ins).
            if (_classRegistry.ContainsKey(sym)) return sym;
            // Fallback by name: handles cross-package aliases (e.g. DOTCL-MOP:SLOT-DEFINITION
            // vs DOTCL-INTERNAL:SLOT-DEFINITION registered by Startup).
            // Do NOT apply for uninterned symbols (gensyms) — they have unique identity
            // and converting them to interned symbols breaks forward-ref resolution.
            if (sym.HomePackage != null)
                return Startup.Sym(sym.Name);
            return sym; // Uninterned gensym: use as-is
        }
        if (name is LispString s) return Startup.Sym(s.Value);
        return Startup.Sym(name.ToString());
    }

    public static LispObject FindClass(LispObject name)
    {
        var sym = ToClassSymbol(name);
        if (_classRegistry.TryGetValue(sym, out var cls))
            return cls;
        throw new LispErrorException(new LispError($"FIND-CLASS: no class named {sym.Name}"));
    }

    public static LispObject FindClassOrNil(LispObject name)
    {
        var sym = ToClassSymbol(name);
        if (_classRegistry.TryGetValue(sym, out var cls))
            return cls;
        return Nil.Instance;
    }

    /// <summary>
    /// Find class or create a forward-referenced placeholder for DEFCLASS superclasses.
    /// </summary>
    public static LispObject FindOrForwardClass(LispObject name)
    {
        var sym = ToClassSymbol(name);
        if (_classRegistry.TryGetValue(sym, out var cls))
            return cls;
        // Create forward-referenced placeholder
        var fwd = new LispClass(sym, Array.Empty<SlotDefinition>(), Array.Empty<LispClass>());
        fwd.IsForwardReferenced = true;
        _classRegistry[sym] = fwd;
        return fwd;
    }

    public static LispObject RegisterClass(LispObject cls)
    {
        if (cls is not LispClass lc)
            throw new LispErrorException(new LispTypeError("REGISTER-CLASS: not a class", cls));
        // Prevent redefining built-in classes (CLHS 4.3.7)
        if (_classRegistry.TryGetValue(lc.Name, out var existing) && existing.IsBuiltIn)
            throw new LispErrorException(new LispError(
                $"Cannot redefine built-in class {lc.Name.Name} with DEFCLASS"));
        // CLHS 4.3.6: re-evaluating DEFCLASS should update existing class in-place,
        // BUT only if the name being defined is still the existing class's PROPER name.
        // After (setf (class-name c) nil) the name is no longer c's proper name
        // (CLHS ensure-class: redefine only when "the name given is the proper name of
        // that class"), so a subsequent defclass under that name must create a fresh,
        // distinct class rather than mutate the orphaned one. ANSI CLASS-0309/0310/0311.
        if (existing != null && !existing.IsBuiltIn && !existing.IsStructureClass
            && !existing.NameCleared && ReferenceEquals(existing.Name, lc.Name))
        {
            existing.DirectSlots = lc.DirectSlots;
            existing.DirectSuperclasses = lc.DirectSuperclasses;
            existing.DirectDefaultInitargs = lc.DirectDefaultInitargs;
            existing.IsForwardReferenced = false;
            // Carry over a (changed) metaclass and its slot values, so re-defining /
            // re-ensure-class'ing a class under a different :metaclass actually switches
            // it — e.g. McCLIM resolving a forward-referenced-class to its real
            // presentation-type-class metaclass.
            existing.Metaclass = lc.Metaclass;
            existing.ExtraSlots = lc.ExtraSlots;
            existing.FinalizeClass();
            // Re-finalize any classes that have this as a superclass
            RefinalizeDependents(existing);
            return existing;
        }
        // Cross-package forward-ref fix: FindOrForwardClass normalizes class names to
        // DOTCL-INTERNAL:: via ToClassSymbol, but MakeClass preserves the original package
        // (e.g. CLIM-INTERNALS::). When the class is first defined, RegisterClass is called
        // with CLIM-INTERNALS::FOO but the forward-ref was stored under DOTCL-INTERNAL::FOO.
        // Without this check, the forward-ref is never updated, leaving dependent classes
        // (which hold a reference to the stale forward-ref placeholder) with empty CPLs.
        if (existing == null)
        {
            var normalizedSym = Startup.Sym(lc.Name.Name);
            if (!ReferenceEquals(normalizedSym, lc.Name) &&
                _classRegistry.TryGetValue(normalizedSym, out var fwdRef) &&
                fwdRef != null && fwdRef.IsForwardReferenced)
            {
                fwdRef.DirectSlots = lc.DirectSlots;
                fwdRef.DirectSuperclasses = lc.DirectSuperclasses;
                fwdRef.DirectDefaultInitargs = lc.DirectDefaultInitargs;
                fwdRef.IsForwardReferenced = false;
                fwdRef.Metaclass = lc.Metaclass;   // metaclass change on forward-ref resolve
                fwdRef.ExtraSlots = lc.ExtraSlots;
                fwdRef.FinalizeClass();
                // Also register under the original package-qualified name for package-aware lookups
                _classRegistry[lc.Name] = fwdRef;
                RefinalizeDependents(fwdRef);
                return fwdRef;
            }
        }
        _classRegistry[lc.Name] = lc;
        // Re-finalize any classes that have this as a forward-referenced superclass
        RefinalizeDependents(lc);
        return cls;
    }

    /// <summary>
    /// Re-finalize all registered classes that have the given class in their superclass chain.
    /// This handles forward-referenced superclasses becoming available.
    /// </summary>
    private static void RefinalizeDependents(LispClass cls)
    {
        foreach (var entry in _classRegistry)
        {
            var c = entry.Value;
            if (ReferenceEquals(c, cls)) continue;
            foreach (var super in c.DirectSuperclasses)
            {
                if (ReferenceEquals(super, cls))
                {
                    // Check if all superclasses are now available (not forward-referenced)
                    bool allReady = true;
                    foreach (var s in c.DirectSuperclasses)
                    {
                        if (s.IsForwardReferenced) { allReady = false; break; }
                    }
                    if (allReady && !c.IsForwardReferenced)
                    {
                        c.FinalizeClass();
                        RefinalizeDependents(c); // propagate through deeper inheritance chains
                    }
                    break;
                }
            }
        }
    }

    /// <summary>Find a class by string name (linear scan). Used for subtypep fallback where
    /// the caller only has a type name string without package context.</summary>
    public static LispClass? FindClassByName(string name)
    {
        foreach (var entry in _classRegistry)
            if (entry.Key.Name == name) return entry.Value;
        return null;
    }

    public static void SetClassByName(string name, LispClass cls) => _classRegistry[Startup.Sym(name)] = cls;

    /// <summary>Register a .NET Type as a CLOS built-in class so that class-of/type-of/find-class work.</summary>
    public static LispClass EnsureDotNetTypeClass(Type type)
    {
        if (_dotNetTypeRegistry.TryGetValue(type, out var existing)) return existing;
        // Prefer the simple type name for the class symbol (friendly, and what
        // unquoted Lisp symbols resolve to). But simple names are ambiguous across
        // namespaces, so guard against collisions below.
        var simpleSym = Startup.Sym(type.Name);
        if (_classRegistry.TryGetValue(simpleSym, out var existingByName))
        {
            // The simple name is already taken. Adopt that class only if it is NOT
            // another .NET type's class — otherwise two same-named types from
            // different namespaces (e.g. System.Collections.ArrayList vs Other.ArrayList)
            // would share one Lisp class and misdirect class-of/typep/dispatch.
            // existingByName cannot be THIS type's class: the _dotNetTypeRegistry
            // lookup above missed.
            bool collidesWithOtherDotNetType =
                _dotNetTypeRegistry.Values.Any(c => ReferenceEquals(c, existingByName));
            if (!collidesWithOtherDotNetType)
            {
                _dotNetTypeRegistry[type] = existingByName;
                return existingByName;
            }
        }
        // Get or create the base class
        LispClass baseCls;
        if (type.BaseType != null && type.BaseType != typeof(object))
            baseCls = EnsureDotNetTypeClass(type.BaseType);
        else if (_classRegistry.TryGetValue(Startup.Sym("T"), out var tCls))
            baseCls = tCls;
        else
            baseCls = _classRegistry.Values.First(); // fallback

        // Name the class by simple name when that slot is free; on a cross-type
        // collision use the unique FullName so the colliding types stay distinct and
        // the first claimant keeps the friendly simple/uppercase names.
        bool simpleFree = !_classRegistry.ContainsKey(simpleSym);
        var classSym = simpleFree
            ? simpleSym
            : Startup.Sym(type.FullName
                ?? (type.Namespace is null ? type.Name : type.Namespace + "." + type.Name));

        var cls = new LispClass(classSym, Array.Empty<SlotDefinition>(), new[] { baseCls });
        var cpl = new List<LispClass> { cls };
        cpl.AddRange(baseCls.ClassPrecedenceList);
        cls.ClassPrecedenceList = cpl.ToArray();
        cls.EffectiveSlots = Array.Empty<SlotDefinition>();
        cls.IsBuiltIn = true;
        _dotNetTypeRegistry[type] = cls;
        if (simpleFree)
        {
            _classRegistry[simpleSym] = cls;
            // Also register under the uppercase name so unquoted Lisp symbols
            // (e.g. (ClsAnimal) read as CLSANIMAL) find the class correctly.
            var upperSym = Startup.Sym(type.Name.ToUpperInvariant());
            if (!ReferenceEquals(upperSym, simpleSym) && !_classRegistry.ContainsKey(upperSym))
                _classRegistry[upperSym] = cls;
        }
        else if (!_classRegistry.ContainsKey(classSym))
        {
            // Make the FullName-qualified class findable by its unique symbol.
            _classRegistry[classSym] = cls;
        }
        return cls;
    }

    /// <summary>Reconstruct a LispInstance from fasl — called by fasl load-time code.</summary>
    public static LispObject MakeFaslInstance(string pkgName, string symName, LispObject[] slots)
    {
        var sym = Startup.SymInPkg(symName, pkgName);
        var cls = FindClassOrNil(sym) as LispClass ?? FindClassByName(symName);
        if (cls == null)
        {
            Console.Error.WriteLine($"[FASL WARNING] MakeFaslInstance: class not found: {pkgName}:{symName}");
            return Nil.Instance;
        }
        var inst = new LispInstance(cls);
        int count = Math.Min(slots.Length, inst.Slots.Length);
        for (int i = 0; i < count; i++)
            inst.Slots[i] = slots[i] is Nil ? null : slots[i];
        return inst;
    }
    public static void RemoveClass(string name) => _classRegistry.TryRemove(Startup.Sym(name), out _);

    /// <summary>Iterate all registered classes. Used by DOTCL-MOP:CLASS-DIRECT-SUBCLASSES
    /// (no back-link is maintained, so we scan).</summary>
    public static IEnumerable<LispClass> AllClasses() => _classRegistry.Values;

    public static void InternClassByName(string name, LispObject cls)
    {
        if (cls is LispClass lc)
            _classRegistry[Startup.Sym(name)] = lc;
        // If not a LispClass (e.g. NIL to remove), ignore for now
    }

    public static LispObject MakeClass(LispObject name, LispObject supersList, LispObject slotDefsList)
        => MakeClassCore(name, supersList, slotDefsList, null);

    public static LispObject MakeClassFull(LispObject name, LispObject supersList, LispObject slotDefsList, LispObject metaclassObj)
        => MakeClassCore(name, supersList, slotDefsList, metaclassObj as LispClass);

    // Variant carrying metaclass-slot initargs (e.g. :type-name) so the class object's
    // single initialize-instance applies them before inherited :after methods run.
    // Distinct name (not an overload) because builtins are reflected by method name and
    // two methods named MakeClassFull would make that lookup ambiguous.
    public static LispObject MakeClassFullWithInitargs(LispObject name, LispObject supersList, LispObject slotDefsList, LispObject metaclassObj, LispObject[] extraInitargs)
        => MakeClassCore(name, supersList, slotDefsList, metaclassObj as LispClass, extraInitargs);

    private static LispObject MakeClassCore(LispObject name, LispObject supersList, LispObject slotDefsList, LispClass? metaclass, LispObject[]? extraInitargs = null)
    {
        if (name is not Symbol sym)
            throw new LispErrorException(new LispTypeError("MAKE-CLASS: name must be a symbol", name));

        // Collect superclasses
        var supers = new List<LispClass>();
        var cur = supersList;
        while (cur is Cons c)
        {
            if (c.Car is LispClass sc)
                supers.Add(sc);
            cur = c.Cdr;
        }
        // Default to STANDARD-OBJECT if no supers
        if (supers.Count == 0 && _classRegistry.TryGetValue(Startup.Sym("STANDARD-OBJECT"), out var stdObj))
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

        // Validate each superclass via validate-superclass GF (AMOP)
        // Must be done before finalization. The new class being defined has a temporary LispClass
        // for dispatch purposes; use a placeholder that has the right metaclass.
        var validateGF = Startup.Sym("VALIDATE-SUPERCLASS").Function as LispFunction;
        if (validateGF != null)
        {
            var tempCls = new LispClass(sym, Array.Empty<SlotDefinition>(), supers.ToArray());
            tempCls.Metaclass = metaclass; // needed for validate-superclass dispatch on (c mm)
            foreach (var super in supers)
            {
                // Skip T — always valid
                if (super.Name.Name == "T") continue;
                var result = validateGF.Invoke(new LispObject[] { tempCls, super });
                if (result is Nil)
                    throw new LispErrorException(new LispError(
                        $"DEFCLASS {sym.Name}: validate-superclass rejected superclass {super.Name.Name}"));
            }
        }

        var cls = new LispClass(sym, slots.ToArray(), supers.ToArray());
        cls.Metaclass = metaclass;
        // AMOP: for a custom metaclass, consult DIRECT-SLOT-DEFINITION-CLASS for each
        // direct slot. A non-standard return class becomes the slot's MetaClass and its
        // Lisp-level slots are initialized from the slot's options.
        if (metaclass != null)
        {
            ApplyDirectSlotDefinitionClass(cls);
            // A class is an instance of its metaclass. Run the real CLOS init protocol
            // on the class object so the metaclass's added slots get their initforms
            // AND any inherited initialize-instance / shared-initialize :after
            // methods fire — e.g. a slot computed by an :after method. The
            // shared-initialize primary handles a LispClass's ExtraSlots above.
            if (Startup.Sym("INITIALIZE-INSTANCE").Function is LispFunction iiFn)
            {
                // Pass the metaclass-slot initargs (e.g. :type-name from ensure-class) so
                // shared-initialize applies them to the class object's ExtraSlots BEFORE any
                // inherited initialize-instance :after runs — matching the ordinary instance
                // init order. Otherwise an :after that reads an initarg-filled slot sees it
                // UNBOUND.
                LispObject[] iiArgs;
                if (extraInitargs is { Length: > 0 })
                {
                    iiArgs = new LispObject[1 + extraInitargs.Length];
                    iiArgs[0] = cls;
                    Array.Copy(extraInitargs, 0, iiArgs, 1, extraInitargs.Length);
                }
                else iiArgs = new LispObject[] { cls };
                iiFn.Invoke(iiArgs);
            }
        }
        // Skip finalization if any superclass is forward-referenced
        bool hasForwardRef = false;
        foreach (var s in supers)
        {
            if (s.IsForwardReferenced) { hasForwardRef = true; break; }
        }
        if (!hasForwardRef)
            cls.FinalizeClass();
        return cls;
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
                    iaList.Add(Startup.Sym("NIL"));
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
            sd.RawOptions = options;
        return slotdObj;
    }

    /// <summary>AMOP DIRECT-SLOT-DEFINITION-CLASS protocol: for each direct slot of a class
    /// with a custom metaclass, ask the GF which class the direct slot definition should be.
    /// A non-standard answer becomes the slot's MetaClass and its extra Lisp slots are
    /// initialized from the slot's options.</summary>
    private static void ApplyDirectSlotDefinitionClass(LispClass cls)
    {
        if (Startup.Sym("DIRECT-SLOT-DEFINITION-CLASS").Function is not LispFunction gf)
            return;
        var stdDirect = FindClassOrNil(Startup.Sym("STANDARD-DIRECT-SLOT-DEFINITION")) as LispClass;
        foreach (var slot in cls.DirectSlots)
        {
            // Build the &rest initargs: :name <name> :initargs (<ia>...) plus the captured
            // slot options. McCLIM only inspects keys (e.g. :dynamic), but pass a faithful set.
            var initargs = new List<LispObject> {
                Startup.Keyword("NAME"), slot.Name,
                Startup.Keyword("INITARGS"), List(slot.Initargs.Cast<LispObject>().ToArray()),
            };
            for (var c = slot.RawOptions; c is Cons rc; c = rc.Cdr)
                initargs.Add(rc.Car);
            var callArgs = new LispObject[initargs.Count + 1];
            callArgs[0] = cls;
            for (int i = 0; i < initargs.Count; i++) callArgs[i + 1] = initargs[i];
            var result = gf.Invoke(callArgs);
            if (result is LispClass dc && !ReferenceEquals(dc, stdDirect))
            {
                slot.MetaClass = dc;
                InitializeSlotdExtraSlots(slot, dc, slot.RawOptions);
            }
        }
    }

    /// <summary>Initialize the Lisp-level slots a custom slot-definition class introduces.
    /// For each effective slot of <paramref name="metaClass"/>, take the value from a matching
    /// initarg in <paramref name="optionsPlist"/> if provided, otherwise evaluate its initform.
    /// Used for both direct (options from the slot spec) and effective (options empty, initforms
    /// only — e.g. McCLIM's DYNAMIC slot reads a dynamically-bound special) slot defs.</summary>
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
                (slotd.ExtraSlots ??= new Dictionary<string, LispObject?>())[es.Name.Name] = value;
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

        var result = new List<(Symbol Key, LispFunction Thunk)>();
        var cur = initargsList;
        while (cur is Cons c1)
        {
            var key = c1.Car as Symbol;
            if (c1.Cdr is Cons c2)
            {
                var thunk = c2.Car as LispFunction;
                if (key != null && thunk != null)
                    result.Add((key, thunk));
                cur = c2.Cdr;
            }
            else break;
        }
        cls.DirectDefaultInitargs = result.ToArray();
        cls.ComputeEffectiveDefaultInitargs();
        // Default initargs disqualify the ultra-fast make-instance path
        if (cls.DefaultInitargs.Length > 0)
            cls.HasSimpleInitialization = false;
        cls.SimpleInitChecked = false;
        return classObj;
    }

    public static LispObject ClassOf(LispObject obj)
    {
        if (obj is LispInstance inst)
            return inst.Class;
        if (obj is LispInstanceCondition lic)
            return lic.Instance.Class;
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
            // Custom metaclass takes priority
            if (!lc.IsBuiltIn && !lc.IsStructureClass && lc.Metaclass != null)
                return lc.Metaclass;
            string metaName = lc.IsBuiltIn ? "BUILT-IN-CLASS"
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
        if (obj is LispMethod)
        {
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-METHOD"), out var methodClass)) return methodClass;
            return Nil.Instance;
        }
        // Struct instances: return the struct's registered class
        if (obj is LispStruct ls)
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
            Fixnum or Bignum => "INTEGER",
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
            LispVector v when v.IsCharVector && v.Rank == 1 => "STRING",
            LispVector v when v.Rank != 1 => "ARRAY",
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
            return lc.NameCleared ? Nil.Instance : lc.Name;
        throw new LispErrorException(new LispTypeError("CLASS-NAME: not a class", cls));
    }

    public static LispObject MakeInstanceRaw(LispObject cls)
    {
        if (cls is not LispClass lc)
            throw new LispErrorException(new LispTypeError("ALLOCATE-INSTANCE: not a class", cls));
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

    public static LispObject SlotValue(LispObject obj, LispObject slotName)
    {
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        string name = slotName switch { Symbol sym => sym.Name, _ => slotName.ToString() };
        if (obj is LispStruct st)
        {
            var stCls = FindClassOrNil(st.TypeName) as LispClass;
            if (stCls != null && stCls.SlotIndex.TryGetValue(name, out int stIdx) && stIdx < st.Slots.Length)
                return st.Slots[stIdx] ?? Nil.Instance;
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
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SLOT-VALUE: not a CLOS instance", obj));
        if (!inst.Class.SlotIndex.TryGetValue(name, out int idx))
        {
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
                return slotMissing.Invoke(new LispObject[] { inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-VALUE") });
            throw new LispErrorException(new LispError(
                $"SLOT-VALUE: no slot named {name} in class {inst.Class.Name.Name}"));
        }
        // AMOP §5.4: dispatch through slot-value-using-class for custom metaclasses
        if (inst.Class.Metaclass != null && Startup.Sym("SLOT-VALUE-USING-CLASS").Function is LispFunction svucFn)
            return svucFn.Invoke(new LispObject[] { inst.Class, inst, inst.Class.EffectiveSlots[idx] });
        return SlotValueDirect(inst, idx, slotName, name);
    }

    internal static LispObject SlotValueDirect(LispInstance inst, int idx, LispObject slotName, string name)
    {
        LispObject? val;
        if (inst.Class.EffectiveSlots[idx].IsClassAllocation)
        {
            // Class-allocated slot: stored on the class that defines it
            var ownerClass = FindClassSlotOwner(inst.Class, name);
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
        if (obj is LispStruct st)
        {
            var stCls = FindClassOrNil(st.TypeName) as LispClass;
            if (stCls != null && stCls.SlotIndex.TryGetValue(name, out int stIdx) && stIdx < st.Slots.Length)
            {
                st.Slots[stIdx] = value;
                return value;
            }
            throw new LispErrorException(new LispError($"SET-SLOT-VALUE: no slot named {name} in struct {st.TypeName.Name}"));
        }
        if (obj is SlotDefinition slotd)
        {
            (slotd.ExtraSlots ??= new Dictionary<string, LispObject?>())[name] = value;
            return value;
        }
        // Metaclass-added slot on a class metaobject.
        if (obj is LispClass klass && klass.Metaclass != null && klass.Metaclass.SlotIndex.ContainsKey(name))
        {
            (klass.ExtraSlots ??= new Dictionary<string, LispObject?>())[name] = value;
            return value;
        }
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SET-SLOT-VALUE: not a CLOS instance", obj));
        if (!inst.Class.SlotIndex.TryGetValue(name, out int idx))
        {
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
            {
                slotMissing.Invoke(new LispObject[] { inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SETF"), value });
                return value;
            }
            throw new LispErrorException(new LispError(
                $"SET-SLOT-VALUE: no slot named {name} in class {inst.Class.Name.Name}"));
        }
        // AMOP §5.4: dispatch through (setf slot-value-using-class) for custom metaclasses
        if (inst.Class.Metaclass != null && Startup.Sym("SLOT-VALUE-USING-CLASS").SetfFunction is LispFunction setfSvucFn)
            return setfSvucFn.Invoke(new LispObject[] { value, inst.Class, inst, inst.Class.EffectiveSlots[idx] });
        return SetSlotValueDirect(inst, idx, name, value);
    }

    internal static LispObject SetSlotValueDirect(LispInstance inst, int idx, string name, LispObject value)
    {
        if (inst.Class.EffectiveSlots[idx].IsClassAllocation)
        {
            var ownerClass = FindClassSlotOwner(inst.Class, name);
            ownerClass.ClassSlotValues[name] = value;
        }
        else
        {
            inst.Slots[idx] = value;
        }
        return value;
    }

    public static LispObject SlotBoundp(LispObject obj, LispObject slotName)
    {
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        string name = slotName switch { Symbol sym => sym.Name, _ => slotName.ToString() };
        if (obj is LispStruct st)
        {
            var stCls = FindClassOrNil(st.TypeName) as LispClass;
            if (stCls != null && stCls.SlotIndex.TryGetValue(name, out int stIdx) && stIdx < st.Slots.Length)
                return st.Slots[stIdx] != null ? T.Instance : Nil.Instance;
            return Nil.Instance;
        }
        if (obj is SlotDefinition slotd)
            return slotd.ExtraSlots != null && slotd.ExtraSlots.TryGetValue(name, out var bv) && bv != null
                ? T.Instance : Nil.Instance;
        if (obj is LispClass klass && klass.Metaclass != null)
            return klass.ExtraSlots != null && klass.ExtraSlots.TryGetValue(name, out var cbv) && cbv != null
                ? T.Instance : Nil.Instance;
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SLOT-BOUNDP: not a CLOS instance", obj));
        if (!inst.Class.SlotIndex.TryGetValue(name, out int idx))
        {
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
            {
                var result = Primary(slotMissing.Invoke(new LispObject[] { inst.Class, inst, slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-BOUNDP") }));
                // slot-boundp returns a SINGLE generalized boolean: explicitly install
                // exactly one value so any secondary values slot-missing returned (it may
                // legally return (values nil x)) do not leak to the caller. ANSI SLOT-MISSING.8.
                LispObject b = IsTruthy(result) ? T.Instance : (LispObject)Nil.Instance;
                MultipleValues.Set(b);
                return b;
            }
            throw new LispErrorException(new LispError(
                $"SLOT-BOUNDP: no slot named {name} in class {inst.Class.Name.Name}"));
        }
        // AMOP §5.4: dispatch through slot-boundp-using-class for custom metaclasses.
        if (inst.Class.Metaclass != null && Startup.Sym("SLOT-BOUNDP-USING-CLASS").Function is LispFunction sbucFn)
            return IsTruthy(Primary(sbucFn.Invoke(new LispObject[] { inst.Class, inst, inst.Class.EffectiveSlots[idx] })))
                ? T.Instance : Nil.Instance;
        return SlotBoundpDirect(inst, idx, name) ? T.Instance : Nil.Instance;
    }

    /// <summary>Raw slot-boundness check against the instance layout / class-allocation
    /// store, without slot-value-using-class dispatch. Shared by SLOT-BOUNDP and the
    /// default SLOT-BOUNDP-USING-CLASS method.</summary>
    internal static bool SlotBoundpDirect(LispInstance inst, int idx, string name)
    {
        if (inst.Class.EffectiveSlots[idx].IsClassAllocation)
        {
            var ownerClass = FindClassSlotOwner(inst.Class, name);
            return ownerClass.ClassSlotValues.TryGetValue(name, out var cv) && cv != null;
        }
        return inst.Slots[idx] != null;
    }

    /// <summary>Raw slot-makunbound against the instance layout / class-allocation store,
    /// without slot-value-using-class dispatch. Shared by SLOT-MAKUNBOUND and the default
    /// SLOT-MAKUNBOUND-USING-CLASS method.</summary>
    internal static void SlotMakunboundDirect(LispInstance inst, int idx, string name)
    {
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

    public static LispObject InitializeInstance(LispObject[] args)
    {
        // args[0] = instance, args[1..] = initargs
        // Calls shared-initialize with slot-names = T (init all slots)
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
        // Class objects: re-finalize if :direct-superclasses or :direct-slots provided, else no-op.
        if (args[0] is LispClass lc)
        {
            bool hasRelevantArgs = false;
            for (int i = 1; i + 1 < args.Length; i += 2)
            {
                if (args[i] is Symbol ks &&
                    (ks.Name == "DIRECT-SUPERCLASSES" || ks.Name == "DIRECT-SLOTS"))
                { hasRelevantArgs = true; break; }
            }
            if (hasRelevantArgs) lc.FinalizeClass();
            return lc;
        }
        // Per CLHS 7.1.2: validate initargs against slots + applicable method &key params.
        if (args[0] is LispInstance li)
        {
            // Collect &key names from applicable reinitialize-instance and shared-initialize methods.
            // If any method has &allow-other-keys, skip validation entirely.
            var methodKeys = new HashSet<string>();
            bool allowOtherKeysFromMethod = CollectMethodKeys(li,
                Startup.Sym("REINITIALIZE-INSTANCE"), methodKeys)
                || CollectMethodKeys(li, Startup.Sym("SHARED-INITIALIZE"), methodKeys);
            if (!allowOtherKeysFromMethod)
                ValidateInitargs(li.Class, args, 1, methodKeys);
        }
        // Calls shared-initialize with slot-names = NIL (don't init unbound slots)
        var sharedInitFn = Startup.Sym("SHARED-INITIALIZE").Function as LispFunction
            ?? throw new LispErrorException(new LispError("SHARED-INITIALIZE not defined"));
        var siArgs = new LispObject[args.Length + 1];
        siArgs[0] = args[0];
        siArgs[1] = Nil.Instance;
        Array.Copy(args, 1, siArgs, 2, args.Length - 1);
        sharedInitFn.Invoke(siArgs);
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

    /// <summary>Collect &key names from applicable methods of the given GF for this instance.
    /// Returns true if any applicable method has &allow-other-keys (meaning validation can be skipped).</summary>
    private static bool CollectMethodKeys(LispInstance inst, Symbol gfSym, HashSet<string> keys)
    {
        if (gfSym.Function is not GenericFunction gf) return false;
        foreach (var method in gf.Methods)
        {
            if (!IsMethodApplicable(method, inst)) continue;
            if (method.HasAllowOtherKeys) return true;
            if (method.HasKey)
                foreach (var kn in method.KeywordNames)
                    keys.Add(kn);
        }
        return false;
    }

    /// <summary>Returns true if the method's first specializer matches the instance's class.</summary>
    private static bool IsMethodApplicable(LispMethod method, LispInstance inst)
    {
        if (method.Specializers.Length == 0) return true;
        var spec = method.Specializers[0];
        return spec is LispClass cls && IsTruthy(Typep(inst, cls.Name));
    }

    /// <summary>Collect &key names from applicable methods of the given GF for a class (CPL-based).
    /// Returns true if any applicable method has &allow-other-keys.</summary>
    private static bool AddMethodKeysForClass(LispClass cls, Symbol gfSym, HashSet<string> keys)
    {
        if (gfSym.Function is not GenericFunction gf) return false;
        foreach (var method in gf.Methods)
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

    public static LispObject SharedInitialize(LispObject[] args)
    {
        // args[0] = instance, args[1] = slot-names (NIL, T, or list of symbols), args[2..] = initargs
        LispObject obj = args[0];
        if (obj is LispInstanceCondition lic) obj = lic.Instance;
        // A class metaobject under a custom metaclass: initialize the metaclass-added
        // slots (those beyond standard-class) from initargs, else their initforms —
        // the shared-initialize contract applied to ExtraSlots. Reached via the
        // real initialize-instance protocol so inherited :after methods run.
        if (obj is LispClass klass && klass.Metaclass != null)
        {
            var meta = klass.Metaclass;
            var stdNames = new HashSet<string>();
            if (_classRegistry.TryGetValue(Startup.Sym("STANDARD-CLASS"), out var scO) && scO is LispClass scC)
                foreach (var es0 in scC.EffectiveSlots) stdNames.Add(es0.Name.Name);
            foreach (var es in meta.EffectiveSlots)
            {
                string sn = es.Name.Name;
                if (stdNames.Contains(sn)) continue;
                bool fromInitarg = false;
                for (int i = 2; i + 1 < args.Length; i += 2)
                    if (args[i] is Symbol k && Array.Exists(es.Initargs, ia => ia.Name == k.Name))
                    { (klass.ExtraSlots ??= new Dictionary<string, LispObject?>())[sn] = args[i + 1]; fromInitarg = true; break; }
                if (fromInitarg) continue;
                bool inNames = args[1] is T;
                if (!inNames) for (var c = args[1]; c is Cons cc; c = cc.Cdr) if (cc.Car is Symbol s2 && s2.Name == sn) { inNames = true; break; }
                bool bound = klass.ExtraSlots != null && klass.ExtraSlots.TryGetValue(sn, out var ev) && ev != null;
                if (inNames && !bound && es.InitformThunk is { } thunk)
                    (klass.ExtraSlots ??= new Dictionary<string, LispObject?>())[sn] = MultipleValues.Primary(thunk.Invoke(Array.Empty<LispObject>()));
            }
            return klass;
        }
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SHARED-INITIALIZE: not a CLOS instance", args[0]));
        var cls = inst.Class;
        LispObject slotNames = args[1];

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
        int initargCount = args.Length - 2;
        if (initargCount % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "SHARED-INITIALIZE: odd number of keyword arguments"));
        for (int i = 2; i < args.Length; i += 2)
        {
            // NIL and T are valid symbols in CL but separate types in dotcl
            if (args[i] is not Symbol && args[i] is not Nil && args[i] is not T)
                throw new LispErrorException(new LispProgramError(
                    $"SHARED-INITIALIZE: invalid initarg key {args[i]}"));
        }

        // Step 1: Apply initargs (leftmost wins for duplicate keys)
        // CLHS: initargs always override existing slot values (not just unbound slots).
        // Track which slot indices were already set by an earlier initarg in THIS call.
        var slotsSetByInitarg = new HashSet<int>();
        for (int i = 2; i + 1 < args.Length; i += 2)
        {
            string initargName = args[i] switch
            {
                Symbol s => s.Name,
                _ => args[i].ToString()!
            };
            foreach (var slot in cls.EffectiveSlots)
            {
                foreach (var ia in slot.Initargs)
                {
                    if (ia.Name == initargName)
                    {
                        if (cls.SlotIndex.TryGetValue(slot.Name.Name, out int idx))
                        {
                            // CLHS: initargs always override the existing slot value, even
                            // for :allocation :class slots already bound by a prior
                            // make-instance. Only the first initarg for a given slot in
                            // THIS call wins (slotsSetByInitarg guards duplicates).
                            if (!slotsSetByInitarg.Contains(idx))
                            {
                                if (slot.IsClassAllocation)
                                    FindClassSlotOwner(cls, slot.Name.Name).ClassSlotValues[slot.Name.Name] = args[i + 1];
                                else
                                    StoreInstanceSlot(slot, idx, args[i + 1]);
                                slotsSetByInitarg.Add(idx);
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
                    if (n == slotName)
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

        return args[0]; // return original instance (possibly wrapped)
    }

    public static LispObject MakeInstanceWithInitargs(LispObject classSpec, params LispObject[] initargs)
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

        // Cannot instantiate built-in classes
        if (cls.IsBuiltIn)
            throw new LispErrorException(new LispError(
                $"Cannot create instances of built-in class {cls.Name.Name} with MAKE-INSTANCE"));

        // For classes that need specialized C# allocation (generic-function, method subtypes),
        // create the right C# object directly and then call initialize-instance.
        // (allocate-instance GF dispatch uses class-of(cls)=STANDARD-CLASS so can't specialize on subclass names.)
        if (HasSpecializedAllocator(cls))
        {
            LispObject? allocated2 = null;
            // Walk CPL to find the most specific recognized type
            foreach (var cplCls in cls.ClassPrecedenceList)
            {
                if (cplCls.Name.Name == "STANDARD-GENERIC-FUNCTION" || cplCls.Name.Name == "GENERIC-FUNCTION")
                {
                    GenericFunction? newGf = null;
                    newGf = new GenericFunction(Startup.Sym("UNNAMED"), -1,
                        callArgs => Runtime.DispatchGF(newGf!, callArgs));
                    newGf.RequiredCount = 0;
                    newGf.LambdaListInfoSet = true;
                    newGf.StoredClass = cls;  // track actual Lisp class (may be substandard-generic-function etc.)
                    allocated2 = newGf;
                    break;
                }
                if (cplCls.Name.Name == "METHOD")
                {
                    allocated2 = new LispMethod();
                    break;
                }
            }
            if (allocated2 != null)
            {
                var iiSym2 = Startup.Sym("INITIALIZE-INSTANCE");
                if (iiSym2.Function is LispFunction iiFn2)
                {
                    var iiArgs2 = new LispObject[1 + initargs.Length];
                    iiArgs2[0] = allocated2;
                    Array.Copy(initargs, 0, iiArgs2, 1, initargs.Length);
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
            ValidateInitargs(cls, initargs, 0);

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
        ValidateInitargs(cls, initargs, 0);

        // Custom metaclass: allocate through the ALLOCATE-INSTANCE GF so user overrides run.
        var inst = customMeta ? AllocateViaGF(cls, initargs) : new LispInstance(cls);

        // Fast path: no default initargs, no custom init methods, not a condition class,
        // and no class-allocated slots with initargs (which need special handling).
        // Directly set slots from initargs using cached initarg→slot map, then apply initforms.
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
        // For each default initarg, if the key is NOT already in user-supplied initargs,
        // evaluate the thunk and append (key, result) to the effective initargs.
        LispObject[] effectiveInitargs = initargs;
        if (cls.DefaultInitargs.Length > 0)
        {
            // Collect user-supplied keys (every other element starting at 0)
            var suppliedKeys = new HashSet<string>();
            for (int i = 0; i < initargs.Length - 1; i += 2)
            {
                if (initargs[i] is Symbol keySym)
                    suppliedKeys.Add(keySym.Name);
            }

            // Check if any defaults need to be added
            var extras = new List<LispObject>();
            foreach (var (key, thunk) in cls.DefaultInitargs)
            {
                if (!suppliedKeys.Contains(key.Name))
                {
                    extras.Add(key);
                    // Unwrap MvReturn: default-initarg thunks may return multiple values
                    // (e.g. ensure-gethash returns (values value present-p)); use primary value only.
                    extras.Add(UnwrapMv(thunk.Invoke(Array.Empty<LispObject>())));
                }
            }

            if (extras.Count > 0)
            {
                effectiveInitargs = new LispObject[initargs.Length + extras.Count];
                Array.Copy(initargs, effectiveInitargs, initargs.Length);
                for (int i = 0; i < extras.Count; i++)
                    effectiveInitargs[initargs.Length + i] = extras[i];
            }
        }


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

    /// <summary>Check if a class is a condition class (CONDITION in its CPL).</summary>
    private static bool IsConditionClass(LispClass cls)
    {
        if (cls.CachedIsConditionClass is bool cached) return cached;
        bool result = false;
        foreach (var c in cls.ClassPrecedenceList)
            if (c.Name.Name == "CONDITION") { result = true; break; }
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
        return true; // EQL specializer — conservative
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
            foreach (var (key, thunk) in cls.DefaultInitargs)
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
                foreach (var (key, _) in cls.DefaultInitargs)
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
                else
                    throw new LispErrorException(new LispProgramError(
                        $"Invalid initarg key {args[i]} for class {cls.Name.Name}: not a symbol"));
                if (!validKeys.Contains(keyName) && (extraMethodKeys == null || !extraMethodKeys.Contains(keyName)))
                    throw new LispErrorException(new LispError(
                        $"Invalid initarg :{keyName} for class {cls.Name.Name}"));
            }
        }
    }

    // --- Macro function registry ---

    private static readonly ConcurrentDictionary<Symbol, LispFunction> _macroFunctions = new();

    public static void RegisterMacroFunction(Symbol sym, LispFunction fn)
    {
        CheckPackageLock(sym, "DEFMACRO");
        _macroFunctions[sym] = fn;
    }

    public static void UnregisterMacroFunction(Symbol sym)
    {
        CheckPackageLock(sym, "FMAKUNBOUND");
        _macroFunctions.TryRemove(sym, out _);
    }

    public static LispObject MacroFunction(LispObject name)
    {
        // Guard against stack overflow from recursive macro expansion
        if (!Compat.TryEnsureSufficientExecutionStack())
            return Nil.Instance;
        var sym = GetSymbol(name, "MACRO-FUNCTION");
        if (_macroFunctions.TryGetValue(sym, out var fn))
            return fn;
        return Nil.Instance;
    }


    // --- Global symbol-macro registry (DEFINE-SYMBOL-MACRO) ---

    private static readonly ConcurrentDictionary<Symbol, LispObject> _globalSymbolMacros = new();

    public static void RegisterGlobalSymbolMacro(Symbol sym, LispObject expansion)
    {
        _globalSymbolMacros[sym] = expansion;
    }

    public static bool TryGetGlobalSymbolMacro(Symbol sym, out LispObject expansion)
    {
        return _globalSymbolMacros.TryGetValue(sym, out expansion!);
    }

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
        // This ensures that the FindGF setf fallback — which searches for bare
        // "(SETF NAME)" keys — can only ever match C#-startup-registered GFs
        // (whose accessor is always a DOTCL-INTERNAL symbol), never user-created
        // GFs like (setf cl:documentation).  Without this, (setf acclimation:doc)
        // bleeds into (setf cl:documentation) via the fallback.
        var pkg = accessor.HomePackage;
        if (pkg == null || pkg.Name == "DOTCL-INTERNAL")
            return $"(SETF {accessor.Name})";
        return $"(SETF {pkg.Name}:{accessor.Name})";
    }

    private static Symbol ToFunctionNameSymbol(LispObject name, string context)
    {
        if (name is Symbol sym) return sym;
        // (setf foo) → intern a stable symbol named "(SETF FOO)" for identity-based registry
        if (name is Cons c && c.Car is Symbol setfSym && setfSym.Name == "SETF"
            && c.Cdr is Cons c2 && c2.Car is Symbol accessor)
        {
            return Startup.Sym(SetfKeyFor(accessor));
        }
        throw new LispErrorException(new LispTypeError($"{context}: invalid function name", name));
    }

    public static LispObject MakeGF(LispObject name, LispObject arity)
    {
        var sym = ToFunctionNameSymbol(name, "MAKE-GF");
        int ar = arity is Fixnum f ? (int)f.Value : -1;
        // Create GF with a dispatch function that calls DispatchGF
        GenericFunction? gf = null;
        gf = new GenericFunction(sym, ar, args => DispatchGF(gf!, args));
        return gf;
    }

    public static LispObject RegisterGF(LispObject name, LispObject gfObj)
    {
        var sym = ToFunctionNameSymbol(name, "REGISTER-GF");
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("REGISTER-GF: not a generic function", gfObj));
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
        return gfObj;
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
        // For setf GFs, the registry key is a cons (setf name), not the symbol itself.
        // We can't easily look up the cons key, so scan for entries whose accessor matches.
        // This is a rare cleanup path so linear scan is acceptable.
        else
        {
            foreach (var kv in _gfRegistry)
            {
                if (kv.Key is Symbol s && s == sym) { _gfRegistry.TryRemove(kv.Key, out _); break; }
            }
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
        // each GF keyword.
        if (gf.HasKey && gf.KeywordNames.Count > 0)
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

    public static LispObject SetMethodCombination(LispObject gfObj, LispObject mcName)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION: not a generic function", gfObj));
        if (mcName is not Symbol mcSym)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION: not a symbol", mcName));
        gf.MethodCombination = mcSym;
        return gfObj;
    }

    public static LispObject SetMethodCombinationOrder(LispObject gfObj, LispObject order)
    {
        if (gfObj is not GenericFunction gf)
            throw new LispErrorException(new LispTypeError("SET-METHOD-COMBINATION-ORDER: not a generic function", gfObj));
        if (order is Symbol sym && sym.Name == "MOST-SPECIFIC-LAST")
            gf.MostSpecificFirst = false;
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
            string symName = sym.Name;
            foreach (var entry in _gfRegistry)
                if (entry.Key.Name == symName
                    && (entry.Key.HomePackage == null || entry.Key.HomePackage.Name == "DOTCL-INTERNAL"))
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
        var quals = new List<Symbol>();
        cur = qualifiers;
        while (cur is Cons c2)
        {
            if (c2.Car is Symbol sym)
                quals.Add(sym);
            cur = c2.Cdr;
        }

        if (fn is not LispFunction func)
            throw new LispErrorException(new LispTypeError("MAKE-METHOD: function required", fn));

        return new LispMethod(specs.ToArray(), quals.ToArray(), func);
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

        // Copy-on-write under MethodsLock so a concurrent dispatch always sees a
        // consistent method array. Replace an existing method with matching
        // specializers+qualifiers, else append.
        lock (gf.MethodsLock)
        {
            var cur = gf.Methods;  // snapshot (current array)
            for (int i = 0; i < cur.Count; i++)
            {
                if (MethodSignatureMatches(cur[i], method))
                {
                    cur[i].Owner = null; // Clear old method's owner
                    var replaced = cur.ToArray();
                    replaced[i] = method;
                    method.Owner = gf;
                    gf.ReplaceMethods(replaced);
                    gf.InvalidateCache();
                    return gf;
                }
            }
            var appended = new LispMethod[cur.Count + 1];
            for (int i = 0; i < cur.Count; i++) appended[i] = cur[i];
            appended[cur.Count] = method;
            method.Owner = gf;
            gf.ReplaceMethods(appended);
            gf.InvalidateCache();
        }
        return gf;
    }

    private static void InvalidateSimpleInitCaches(GenericFunction gf)
    {
        if (gf.Name.Name == "INITIALIZE-INSTANCE" || gf.Name.Name == "SHARED-INITIALIZE")
        {
            foreach (var c in _classRegistry.Values)
            {
                c.SimpleInitChecked = false;
                c.CachedValidInitargKeys = null; // method keys affect valid initargs (CLHS 7.1.2)
            }
        }
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
                gf.InvalidateCache();
            }
        }
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

    private static bool MethodSignatureMatches(LispMethod a, LispMethod b)
    {
        if (a.Specializers.Length != b.Specializers.Length) return false;
        if (a.Qualifiers.Length != b.Qualifiers.Length) return false;
        for (int i = 0; i < a.Specializers.Length; i++)
            if (!ReferenceEquals(a.Specializers[i], b.Specializers[i])) return false;
        for (int i = 0; i < a.Qualifiers.Length; i++)
            if (a.Qualifiers[i].Name != b.Qualifiers[i].Name) return false;
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

    public static LispObject MethodFunction(LispObject methodObj)
    {
        if (methodObj is not LispMethod m)
            throw new LispErrorException(new LispTypeError("METHOD-FUNCTION: not a method", methodObj));
        return m.Function;
    }

    /// <summary>
    /// Dispatch through a GF's methods if any are applicable, otherwise call the default function.
    /// Used for C#-created GFs that have a default behavior but also support defmethod.
    /// </summary>
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
    private static LispClass? ArgDispatchClass(LispObject obj) => obj switch
    {
        LispInstance inst => inst.Class,
        LispStruct st => FindClassOrNil(st.TypeName) as LispClass,
        _ => ClassOf(obj) as LispClass
    };

    /// <summary>Validate keyword arguments against the union of keywords accepted by
    /// the applicable methods plus GF-level keywords (CLHS 7.6.5). Signals program-error
    /// for an unknown keyword unless :allow-other-keys t is passed or some applicable
    /// method allows other keys. Must run on BOTH the cache-hit and cache-miss dispatch
    /// paths — earlier it lived only on the cache-miss path, so a warm monomorphic cache
    /// silently skipped the check (ANSI DEFMETHOD.ERROR.14/15).</summary>
    private static void ValidateGenericKeywords(GenericFunction gf, IReadOnlyList<LispMethod> applicable, LispObject[] args)
    {
        if (!(gf.LambdaListInfoSet && gf.HasKey && !gf.HasAllowOtherKeys)) return;
        int keyStart = gf.RequiredCount + gf.OptionalCount;
        if (args.Length <= keyStart) return;

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

        // :allow-other-keys — only the FIRST occurrence's value is honored (CLHS 3.4.1.4.1).
        for (int i = keyStart; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS"
                && ks.HomePackage?.Name == "KEYWORD")
            {
                if (args[i + 1] is not Nil) return; // suppress unknown-key check
                break;                              // first wins; nil → validate
            }
        }

        // Check if any applicable method has &allow-other-keys or &rest (without &key)
        var allowedKeywords = new HashSet<string> { "ALLOW-OTHER-KEYS" }; // always valid per CLHS 3.4.1.4.1
        foreach (var m in applicable)
        {
            if (m.HasAllowOtherKeys || (m.HasRest && !m.HasKey)) return;
            foreach (var kw in m.KeywordNames)
                allowedKeywords.Add(kw);
        }
        // Also add GF-level keywords
        foreach (var kw in gf.KeywordNames)
            allowedKeywords.Add(kw);

        for (int i = keyStart; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks2 && !allowedKeywords.Contains(ks2.Name))
                throw new LispErrorException(new LispProgramError(
                    $"{gf.Name.Name}: invalid keyword argument :{ks2.Name}"));
        }
    }

    private static LispObject DispatchGF(GenericFunction gf, LispObject[] args)
    {
        // Arity check: signal program-error for too few/too many arguments
        int requiredCount = gf.LambdaListInfoSet ? gf.RequiredCount : (gf.Arity >= 0 ? gf.Arity : 0);
        if (requiredCount > 0 && args.Length < requiredCount)
            throw new LispErrorException(new LispProgramError(
                $"{gf.Name.Name}: too few arguments ({args.Length}), expected at least {requiredCount}"));
        // Check max args: if GF has no &rest and no &key, reject excess arguments
        if (gf.LambdaListInfoSet && !gf.HasRest && !gf.HasKey)
        {
            int maxArgs = gf.RequiredCount + gf.OptionalCount;
            if (args.Length > maxArgs)
                throw new LispErrorException(new LispProgramError(
                    $"{gf.Name.Name}: too many arguments ({args.Length}), expected at most {maxArgs}"));
        }

        // Cache check: try monomorphic inline cache
        var cached = gf.LastDispatch;
        if (cached != null)
        {
            var cachedTypes = cached.ArgTypes;
            bool match = cachedTypes.Length <= args.Length;
            if (match)
            {
                for (int i = 0; i < cachedTypes.Length; i++)
                {
                    if (!ReferenceEquals(ArgDispatchClass(args[i]), cachedTypes[i]))
                    { match = false; break; }
                }
            }
            if (match)
            {
                // Keyword validation must run on the cache-hit path too — a warm
                // monomorphic cache otherwise skips the unknown-keyword check that the
                // cache-miss path performs (ANSI DEFMETHOD.ERROR.14/15).
                if (gf.LambdaListInfoSet && gf.HasKey && !gf.HasAllowOtherKeys)
                {
                    List<LispMethod> cachedApplicable;
                    if (cached.Applicable != null)
                        cachedApplicable = cached.Applicable;
                    else
                    {
                        cachedApplicable = new List<LispMethod>();
                        cachedApplicable.AddRange(cached.Around);
                        cachedApplicable.AddRange(cached.Before);
                        cachedApplicable.AddRange(cached.Primary);
                        cachedApplicable.AddRange(cached.After);
                        if (cached.EqlMethods != null)
                            foreach (var em in cached.EqlMethods)
                                if (IsMethodApplicable(em, args)) cachedApplicable.Add(em);
                    }
                    ValidateGenericKeywords(gf, cachedApplicable, args);
                }
                // For EQL specializers: check if any EQL method matches (takes priority)
                if (cached.HasEqlSpecializers && cached.EqlMethods != null)
                {
                    LispMethod? eqlMatch = null;
                    foreach (var em in cached.EqlMethods)
                    {
                        if (IsMethodApplicable(em, args))
                        { eqlMatch = em; break; } // first = most specific
                    }
                    if (eqlMatch != null)
                    {
                        // Fast path: no around/before/after and no non-EQL primaries
                        // → invoke EQL method directly with minimal overhead
                        if (cached.Around.Count == 0 && cached.Before.Count == 0
                            && cached.After.Count == 0 && cached.Primary.Count == 0)
                        {
                            var savedChain = _nextMethodChain;
                            var savedIndex = _nextMethodIndex;
                            var savedArgs = _currentGFArgs;
                            var savedFallback = _nextMethodFallback;
                            _nextMethodChain = null;
                            _nextMethodIndex = 0;
                            _currentGFArgs = args;
                            _nextMethodFallback = null;
                            try
                            {
                                return eqlMatch.Function.Invoke(args);
                            }
                            finally
                            {
                                _nextMethodChain = savedChain;
                                _nextMethodIndex = savedIndex;
                                _currentGFArgs = savedArgs;
                                _nextMethodFallback = savedFallback;
                            }
                        }
                        // General case: build combined primary list
                        if (cached.Around.Count > 0)
                        {
                            var eqlPrimary = new List<LispMethod> { eqlMatch };
                            eqlPrimary.AddRange(cached.Primary);
                            return InvokeWithNextMethods(cached.Around, 0, args,
                                a => InvokeStandardCombination(cached.Before, eqlPrimary, cached.After, a));
                        }
                        var primaryWithEql = new List<LispMethod> { eqlMatch };
                        primaryWithEql.AddRange(cached.Primary);
                        return InvokeStandardCombination(cached.Before, primaryWithEql, cached.After, args);
                    }
                    // No EQL match — fall through to cached non-EQL result
                }
                // Cache hit: reuse sorted method lists
                if (cached.IsBuiltinCombination && cached.Applicable != null)
                    return DispatchBuiltinCombination(gf, cached.Applicable, args);
                if (cached.Around.Count > 0)
                    return InvokeWithNextMethods(cached.Around, 0, args,
                        a => InvokeStandardCombination(cached.Before, cached.Primary, cached.After, a));
                return InvokeStandardCombination(cached.Before, cached.Primary, cached.After, args);
            }
        }

        // Find applicable methods
        var applicable = new List<LispMethod>();
        bool hasEqlSpec = false;
        foreach (var method in gf.Methods)
        {
            if (IsMethodApplicable(method, args))
                applicable.Add(method);
            if (!hasEqlSpec)
                foreach (var spec in method.Specializers)
                    if (spec is Cons) { hasEqlSpec = true; break; }
        }

        if (applicable.Count == 0)
        {
            // Fall back to the saved original ordinary function (e.g. the C# built-in
            // for CL functions like CLOSE or STREAM-ELEMENT-TYPE when called with a
            // type that has no user-defined Gray-stream method).
            if (gf.FallbackFunction != null)
                return gf.FallbackFunction.Invoke(args);
            throw new LispErrorException(new LispError(
                $"No applicable method for generic function {gf.Name.Name}"));
        }

        // Keyword argument validation (CLHS 7.6.5)
        ValidateGenericKeywords(gf, applicable, args);

        // Built-in operator method combinations (+, NCONC, APPEND, AND, OR, PROGN, MIN, MAX, LIST)
        if (gf.MethodCombination != null)
        {
            // Check for long-form method combination first
            string mcName = gf.MethodCombination.Name;
            if (_longFormMCRegistry.TryGetValue(mcName, out var longFormMC))
            {
                return DispatchLongFormCombination(gf, longFormMC, applicable, args);
            }

            // Cache for built-in combination
            if (!hasEqlSpec)
            {
                int n = Math.Max(1, requiredCount);
                var types = new LispClass?[n];
                for (int i = 0; i < n && i < args.Length; i++)
                    types[i] = ArgDispatchClass(args[i]);
                gf.LastDispatch = new CachedDispatch
                {
                    ArgTypes = types,
                    Applicable = applicable,
                    HasEqlSpecializers = false,
                    IsBuiltinCombination = true,
                    Around = new List<LispMethod>(),
                    Before = new List<LispMethod>(),
                    Primary = new List<LispMethod>(),
                    After = new List<LispMethod>()
                };
            }
            return DispatchBuiltinCombination(gf, applicable, args);
        }

        // STANDARD method combination: partition by qualifier
        var aroundMethods = new List<LispMethod>();
        var beforeMethods = new List<LispMethod>();
        var primaryMethods = new List<LispMethod>();
        var afterMethods = new List<LispMethod>();

        foreach (var m in applicable)
        {
            if (m.Qualifiers.Length == 0)
                primaryMethods.Add(m);
            else if (m.Qualifiers[0].Name == "BEFORE")
                beforeMethods.Add(m);
            else if (m.Qualifiers[0].Name == "AFTER")
                afterMethods.Add(m);
            else if (m.Qualifiers[0].Name == "AROUND")
                aroundMethods.Add(m);
        }

        if (primaryMethods.Count == 0)
            throw new LispErrorException(new LispError(
                $"No primary method for generic function {gf.Name.Name}"));

        // Sort: more specific first
        primaryMethods.Sort((a, b) => CompareMethodSpecificity(a, b, args));
        beforeMethods.Sort((a, b) => CompareMethodSpecificity(a, b, args));
        afterMethods.Sort((a, b) => CompareMethodSpecificity(b, a, args)); // reverse for :after
        aroundMethods.Sort((a, b) => CompareMethodSpecificity(a, b, args));

        // Update monomorphic cache for STANDARD combination
        {
            int n = Math.Max(1, requiredCount);
            var types = new LispClass?[n];
            for (int i = 0; i < n && i < args.Length; i++)
                types[i] = ArgDispatchClass(args[i]);
            if (hasEqlSpec)
            {
                // For EQL GFs: cache the non-EQL result + ALL EQL methods from the GF
                // (not just applicable ones — different calls may match different EQL specializers)
                var nonEqlPrimary = new List<LispMethod>();
                foreach (var m in primaryMethods)
                {
                    bool isEql = false;
                    foreach (var spec in m.Specializers)
                        if (spec is Cons c && c.Car is Symbol s && s.Name == "EQL")
                        { isEql = true; break; }
                    if (!isEql) nonEqlPrimary.Add(m);
                }
                // Collect ALL EQL-specialized primary methods from the GF (sorted by specificity)
                var allEqlMethods = new List<LispMethod>();
                foreach (var m in gf.Methods)
                {
                    if (m.Qualifiers.Length > 0) continue; // skip :before/:after/:around
                    bool isEql = false;
                    foreach (var spec in m.Specializers)
                        if (spec is Cons c && c.Car is Symbol s && s.Name == "EQL")
                        { isEql = true; break; }
                    if (isEql) allEqlMethods.Add(m);
                }
                gf.LastDispatch = new CachedDispatch
                {
                    ArgTypes = types,
                    Around = aroundMethods,
                    Before = beforeMethods,
                    Primary = nonEqlPrimary,
                    After = afterMethods,
                    HasEqlSpecializers = true,
                    EqlMethods = allEqlMethods.ToArray()
                };
            }
            else
            {
                gf.LastDispatch = new CachedDispatch
                {
                    ArgTypes = types,
                    Around = aroundMethods,
                    Before = beforeMethods,
                    Primary = primaryMethods,
                    After = afterMethods,
                    HasEqlSpecializers = false
                };
            }
        }

        // Build effective method chain
        if (aroundMethods.Count > 0)
        {
            // :around wraps everything
            return InvokeWithNextMethods(aroundMethods, 0, args,
                a => {
                    return InvokeStandardCombination(beforeMethods, primaryMethods, afterMethods, a);
                });
        }
        else
        {
            return InvokeStandardCombination(beforeMethods, primaryMethods, afterMethods, args);
        }
    }

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
                bool matches = false;
                if (spec.MatchAll)
                {
                    matches = true;
                }
                else if (spec.MatchUnqualified)
                {
                    matches = m.Qualifiers.Length == 0;
                }
                else if (spec.QualifierPattern is Symbol qs)
                {
                    matches = m.Qualifiers.Length > 0 && m.Qualifiers[0].Name == qs.Name;
                }
                else if (spec.QualifierPattern is Cons qpCons)
                {
                    // Pattern is a list like (:around . *) — match head qualifier
                    if (qpCons.Car is Symbol headSym)
                        matches = m.Qualifiers.Length > 0 && m.Qualifiers[0].Name == headSym.Name;
                }
                if (matches)
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
                var qualStr = m.Qualifiers.Length > 0 ? m.Qualifiers[0].Name : "(unqualified)";
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

        // Build gf-args as a Lisp list for :arguments option
        LispObject gfArgsList = Nil.Instance;
        for (int i = args.Length - 1; i >= 0; i--)
            gfArgsList = MakeCons(args[i], gfArgsList);

        var effectiveMethodForm = mc.BodyFunction!.Invoke(new LispObject[] { mcArgsList, groupsList, gfArgsList });
        return EvalEffectiveMethodForm(effectiveMethodForm, args);
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
                    var qualPat = r2.Car;
                    if (qualPat is Symbol qs && qs.Name == "*")
                        gs.MatchAll = true;
                    else if (qualPat is Nil)
                        gs.MatchUnqualified = true;
                    else
                        gs.QualifierPattern = qualPat;

                    var opts = r2.Cdr;
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
                    // (call-method method next-method-list)
                    var methodObj = (c.Cdr is Cons mc1) ? mc1.Car : Nil.Instance;
                    if (methodObj is LispMethod method)
                        return method.Function.Invoke(args);
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
        string mcName = gf.MethodCombination!.Name;
        // Resolve custom method combination to its operator name
        string operatorName = mcName;
        bool identityWithOneArg = false;
        if (_methodCombinationRegistry.TryGetValue(mcName, out var regEntry))
        {
            operatorName = regEntry.Operator;
            identityWithOneArg = regEntry.IdentityWithOneArg;
        }
        var combinedMethods = new List<LispMethod>();
        var aroundMethods = new List<LispMethod>();

        foreach (var m in applicable)
        {
            // CLHS 7.6.6.2 (short-form / operator method combination): every applicable
            // method must be qualified by exactly the combination name or :around.
            // An unqualified method or any other qualifier (e.g. a stray `nonsense`)
            // is an invalid-method-error, not silently ignored. ANSI
            // DEFGENERIC-METHOD-COMBINATION.APPEND.13.
            bool isAround = m.Qualifiers.Length == 1 && m.Qualifiers[0].Name == "AROUND";
            bool isOperator = m.Qualifiers.Length == 1 && m.Qualifiers[0].Name == mcName;
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
        aroundMethods.Sort((a, b) => CompareMethodSpecificity(a, b, args));

        // Takes nextArgs so an :around method's (call-next-method ...) with explicit
        // arguments forwards them to the combined operator body (CLHS 7.6.6.1).
        Func<LispObject[], LispObject> invokeBody = nextArgs =>
        {
            // :identity-with-one-argument - skip operator when single method
            if (identityWithOneArg && combinedMethods.Count == 1)
                return combinedMethods[0].Function.Invoke(nextArgs);
            switch (operatorName)
            {
                case "+":
                {
                    LispObject result = Fixnum.Make(0);
                    foreach (var m in combinedMethods)
                        result = Arithmetic.Add(AsNumber(result), AsNumber(m.Function.Invoke(nextArgs)));
                    return result;
                }
                case "*":
                {
                    LispObject result = Fixnum.Make(1);
                    foreach (var m in combinedMethods)
                        result = Arithmetic.Multiply(AsNumber(result), AsNumber(m.Function.Invoke(nextArgs)));
                    return result;
                }
                case "MIN":
                {
                    LispObject result = combinedMethods[0].Function.Invoke(nextArgs);
                    for (int i = 1; i < combinedMethods.Count; i++)
                    {
                        var val = combinedMethods[i].Function.Invoke(nextArgs);
                        if (Arithmetic.Compare(AsNumber(val), AsNumber(result)) < 0)
                            result = val;
                    }
                    return result;
                }
                case "MAX":
                {
                    LispObject result = combinedMethods[0].Function.Invoke(nextArgs);
                    for (int i = 1; i < combinedMethods.Count; i++)
                    {
                        var val = combinedMethods[i].Function.Invoke(nextArgs);
                        if (Arithmetic.Compare(AsNumber(val), AsNumber(result)) > 0)
                            result = val;
                    }
                    return result;
                }
                case "AND":
                {
                    LispObject result = T.Instance;
                    foreach (var m in combinedMethods)
                    {
                        result = m.Function.Invoke(nextArgs);
                        if (result is Nil) return Nil.Instance;
                    }
                    return result;
                }
                case "OR":
                {
                    foreach (var m in combinedMethods)
                    {
                        var result = m.Function.Invoke(nextArgs);
                        if (result is not Nil) return result;
                    }
                    return Nil.Instance;
                }
                case "PROGN":
                {
                    LispObject result = Nil.Instance;
                    foreach (var m in combinedMethods)
                        result = m.Function.Invoke(nextArgs);
                    return result;
                }
                case "LIST":
                {
                    var results = new List<LispObject>();
                    foreach (var m in combinedMethods)
                        results.Add(m.Function.Invoke(nextArgs));
                    LispObject result = Nil.Instance;
                    for (int i = results.Count - 1; i >= 0; i--)
                        result = MakeCons(results[i], result);
                    return result;
                }
                case "NCONC":
                {
                    LispObject result = Nil.Instance;
                    foreach (var m in combinedMethods)
                    {
                        var val = m.Function.Invoke(nextArgs);
                        result = NconcTwo(result, val);
                    }
                    return result;
                }
                case "APPEND":
                {
                    var results = new List<LispObject>();
                    foreach (var m in combinedMethods)
                        results.Add(m.Function.Invoke(nextArgs));
                    if (results.Count == 0) return Nil.Instance;
                    LispObject result = results[results.Count - 1];
                    for (int i = results.Count - 2; i >= 0; i--)
                        result = Append(results[i], result);
                    return result;
                }
                default:
                    throw new LispErrorException(new LispError(
                        $"Unknown method combination operator: {operatorName} (combination: {mcName})"));
            }
        };

        if (aroundMethods.Count > 0)
            return InvokeWithNextMethods(aroundMethods, 0, args, invokeBody);
        else
            return invokeBody(args);
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
        List<LispMethod> before, List<LispMethod> primary, List<LispMethod> after,
        LispObject[] args)
    {
        // Fast path: single primary, no before/after → minimal next-method setup
        if (before.Count == 0 && after.Count == 0 && primary.Count == 1)
        {
            var savedChain = _nextMethodChain;
            var savedIndex = _nextMethodIndex;
            var savedArgs = _currentGFArgs;
            var savedFallback = _nextMethodFallback;
            _nextMethodChain = primary;
            _nextMethodIndex = 1;
            _currentGFArgs = args;
            _nextMethodFallback = null;
            // Single primary, no before/after → there is no next method. Publish the
            // default closures thread-locally (the body capture sees "no next method").
            // These were previously installed on the global symbol-functions, which a
            // concurrent dispatch could read mid-window and mis-signal.
            var savedCapturedNmp = _capturedNmp;
            var savedCapturedCnm = _capturedCnm;
            _capturedNmp = _defaultNmp;
            _capturedCnm = _defaultCnm;
            try
            {
                return primary[0].Function.Invoke(args);
            }
            finally
            {
                _nextMethodChain = savedChain;
                _nextMethodIndex = savedIndex;
                _currentGFArgs = savedArgs;
                _nextMethodFallback = savedFallback;
                _capturedNmp = savedCapturedNmp;
                _capturedCnm = savedCapturedCnm;
            }
        }

        // :before methods (most specific first)
        foreach (var m in before)
            m.Function.Invoke(args);

        // Primary methods with call-next-method chain
        var result = InvokeWithNextMethods(primary, 0, args, null);

        // :after methods (least specific first — already sorted that way)
        foreach (var m in after)
            m.Function.Invoke(args);

        return result;
    }

    [ThreadStatic]
    private static List<LispMethod>? _nextMethodChain;
    [ThreadStatic]
    private static int _nextMethodIndex;
    [ThreadStatic]
    private static LispObject[]? _currentGFArgs;
    [ThreadStatic]
    // The fallback runs the "next method" when the next-method chain (e.g. the
    // :around list) is exhausted — typically the before/primary/after combination.
    // It takes the actual arguments so that (call-next-method ...) with EXPLICIT
    // arguments propagates them to that combination (CLHS 7.6.6.1), rather than
    // capturing the original generic-function arguments.
    private static Func<LispObject[], LispObject>? _nextMethodFallback;

    // Per-invocation CALL-NEXT-METHOD / NEXT-METHOD-P closures, captured by a
    // method body at entry via the %CAPTURED-CALL-NEXT-METHOD / %CAPTURED-NEXT-METHOD-P
    // intrinsics (CapturedCnm/CapturedNmp below). These MUST be thread-local: they
    // used to be published by mutating the GLOBAL symbol-functions of
    // CALL-NEXT-METHOD / NEXT-METHOD-P, which a second thread dispatching
    // concurrently would clobber — the victim method then captured the other
    // thread's closure (or the fast-path "no next method" stub) and signalled a
    // spurious "CALL-NEXT-METHOD: no next method" / wrong dispatch (the
    // residual race after the method list became copy-on-write).
    [ThreadStatic]
    private static LispObject? _capturedCnm;
    [ThreadStatic]
    private static LispObject? _capturedNmp;

    // Defaults used outside any method invocation (and for the single-primary fast
    // path, where there is no next method). Stateless and immutable, so a single
    // shared instance is safe across threads.
    private static readonly LispFunction _defaultCnm = new LispFunction(
        _ => throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method")),
        "CALL-NEXT-METHOD", -1);
    private static readonly LispFunction _defaultNmp = new LispFunction(
        _ => Nil.Instance, "NEXT-METHOD-P", 0);

    /// <summary>Intrinsic: the current method's captured CALL-NEXT-METHOD closure
    /// (thread-local). Emitted for the defmethod-body capture instead of
    /// (symbol-function 'call-next-method), which read a process-global field.</summary>
    public static LispObject CapturedCnm() => _capturedCnm ?? _defaultCnm;
    /// <summary>Intrinsic: the current method's captured NEXT-METHOD-P closure (thread-local).</summary>
    public static LispObject CapturedNmp() => _capturedNmp ?? _defaultNmp;

    private static LispObject InvokeWithNextMethods(
        List<LispMethod> methods, int startIdx, LispObject[] args,
        Func<LispObject[], LispObject>? fallback)
    {
        var savedChain = _nextMethodChain;
        var savedIndex = _nextMethodIndex;
        var savedArgs = _currentGFArgs;
        var savedFallback = _nextMethodFallback;

        _nextMethodChain = methods;
        _nextMethodIndex = startIdx + 1;
        _currentGFArgs = args;
        _nextMethodFallback = fallback;

        // Create closure versions of next-method-p and call-next-method
        // with indefinite extent (CLHS 7.6.6.1, 7.6.6.2).
        // These capture the current method chain state so they remain valid
        // even after the method returns.
        var closureChain = methods;
        var closureIdx = startIdx + 1;
        var closureArgs = args;
        var closureFallback = fallback;

        var nmpClosure = new LispFunction(
            nmpArgs => {
                if (nmpArgs.Length != 0)
                    throw new LispErrorException(new LispProgramError(
                        $"NEXT-METHOD-P: wrong number of arguments: {nmpArgs.Length} (expected 0)"));
                return (closureIdx < closureChain.Count || closureFallback != null)
                    ? (LispObject)T.Instance : Nil.Instance;
            },
            "NEXT-METHOD-P", 0);
        var currentMethod = methods[startIdx];
        var cnmClosure = new LispFunction(
            cnmArgs => {
                var actualArgs = cnmArgs.Length > 0 ? cnmArgs : closureArgs;
                if (cnmArgs.Length > 0 && !IsMethodApplicable(currentMethod, actualArgs))
                    throw new LispErrorException(new LispProgramError(
                        "CALL-NEXT-METHOD: changed arguments are not applicable to the current method"));
                return CallNextMethodWithChain(closureChain, closureIdx, actualArgs, closureFallback);
            },
            "CALL-NEXT-METHOD", -1);

        // Publish the per-invocation closures thread-locally so the method body's
        // entry capture (%CAPTURED-CALL-NEXT-METHOD / %CAPTURED-NEXT-METHOD-P) sees
        // ITS OWN chain. Mutating the global symbol-functions here instead would let
        // a concurrent dispatch on another thread clobber them mid-window.
        var savedCapturedNmp = _capturedNmp;
        var savedCapturedCnm = _capturedCnm;
        _capturedNmp = nmpClosure;
        _capturedCnm = cnmClosure;

        try
        {
            return methods[startIdx].Function.Invoke(args);
        }
        finally
        {
            _nextMethodChain = savedChain;
            _nextMethodIndex = savedIndex;
            _currentGFArgs = savedArgs;
            _nextMethodFallback = savedFallback;
            _capturedNmp = savedCapturedNmp;
            _capturedCnm = savedCapturedCnm;
        }
    }

    /// <summary>
    /// Call next method using captured chain state (for indefinite extent closures).
    /// </summary>
    private static LispObject CallNextMethodWithChain(
        List<LispMethod> chain, int idx, LispObject[] args, Func<LispObject[], LispObject>? fallback)
    {
        if (idx < chain.Count)
            return InvokeWithNextMethods(chain, idx, args, fallback);
        if (fallback != null)
            return fallback(args);
        throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method"));
    }

    public static LispObject CallNextMethod(params LispObject[] args)
    {
        if (_nextMethodChain == null)
            throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method"));

        LispObject[] actualArgs = args.Length > 0 ? args : _currentGFArgs!;

        // CLHS 7.6.6.1: When call-next-method is called with arguments,
        // the ordered set of applicable methods must be the same as for the
        // original arguments. Check that the new arguments are applicable to
        // the current method (the one that called call-next-method).
        if (args.Length > 0 && _nextMethodIndex > 0)
        {
            var currentMethod = _nextMethodChain[_nextMethodIndex - 1];
            if (!IsMethodApplicable(currentMethod, actualArgs))
            {
                throw new LispErrorException(new LispError(
                    "CALL-NEXT-METHOD: changed arguments are not applicable to the current method"));
            }
        }

        if (_nextMethodIndex < _nextMethodChain.Count)
        {
            return InvokeWithNextMethods(_nextMethodChain, _nextMethodIndex, actualArgs, _nextMethodFallback);
        }
        else if (_nextMethodFallback != null)
        {
            return _nextMethodFallback(actualArgs);
        }
        else
        {
            throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method"));
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
                    if (!found) return false;
                }
                else if (!IsTruthy(Typep(args[i], cls.Name)))
                    return false;
            }
            // EQL specializer: (eql value)
            else if (spec is Cons eqlSpec && eqlSpec.Car is Symbol sym && sym.Name == "EQL")
            {
                if (!IsTrueEql(args[i], ((Cons)eqlSpec.Cdr).Car))
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
            bool aIsEql = a.Specializers[i] is Cons eqlA && eqlA.Car is Symbol symA && symA.Name == "EQL";
            bool bIsEql = b.Specializers[i] is Cons eqlB && eqlB.Car is Symbol symB && symB.Name == "EQL";
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
                    foreach (var c in argClass.ClassPrecedenceList)
                    {
                        if (ReferenceEquals(c, clsA)) return -1; // a is more specific
                        if (ReferenceEquals(c, clsB)) return 1;  // b is more specific
                    }
                }
                // Fallback: compare by name (arbitrary but deterministic)
                return string.Compare(clsA.Name.Name, clsB.Name.Name, StringComparison.Ordinal);
            }
        }
        return 0;
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
        }
        else
            throw new LispErrorException(new LispTypeError("CHANGE-CLASS: invalid class specifier", newClassSpec));

        // Cannot change-class to a built-in class (CLHS 7.2)
        if (newClass.IsBuiltIn)
            throw new LispErrorException(new LispError(
                $"CHANGE-CLASS: cannot change class to built-in class {newClass.Name.Name}"));

        // Per CLHS 7.2: validate initargs against new class
        ValidateInitargs(newClass, args, 2);

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

        // Copy slot values for slots with same name in both old and new class (CLHS 7.2).
        // A slot that is :allocation :class in the NEW class is NOT affected by
        // change-class — it keeps its existing shared value and must not be
        // overwritten from the old instance (CLHS 7.2.1; ANSI CHANGE-CLASS.3.2).
        foreach (var newSlot in newClass.EffectiveSlots)
        {
            if (newSlot.IsClassAllocation) continue;
            if (newClass.SlotIndex.TryGetValue(newSlot.Name.Name, out int newIdx))
            {
                if (oldClass.SlotIndex.TryGetValue(newSlot.Name.Name, out int oldIdx))
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

        return instance;
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
            if (inst.Class.SlotIndex.ContainsKey(name))
                return T.Instance;
            if (inst.Class.StructSlotNames != null)
            {
                foreach (var sn in inst.Class.StructSlotNames)
                    if (sn.Name == name) return T.Instance;
            }
            return Nil.Instance;
        }

        if (obj is LispStruct ls)
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

        return Nil.Instance;
    }

    private static bool HasSpecializedAllocator(LispClass cls)
    {
        foreach (var s in cls.ClassPrecedenceList)
            if (s.Name.Name == "GENERIC-FUNCTION" || s.Name.Name == "METHOD") return true;
        return false;
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

    private static Symbol[] CollectSymbols(LispObject lst)
    {
        var result = new List<Symbol>();
        var cur = lst;
        while (cur is Cons c) { if (c.Car is Symbol s) result.Add(s); cur = c.Cdr; }
        return result.ToArray();
    }

    internal static void RegisterCLOSBuiltins()
    {
        // CLOS internal primitives
        Startup.RegisterBinary("%MAKE-GF", Runtime.MakeGF);
        Startup.RegisterBinary("%REGISTER-GF", Runtime.RegisterGF);
        Startup.RegisterUnary("%CLEAR-DEFGENERIC-INLINE-METHODS", Runtime.ClearDefgenericInlineMethods);
        Startup.RegisterBinary("%MARK-DEFGENERIC-INLINE-METHOD", Runtime.MarkDefgenericInlineMethod);
        Emitter.CilAssembler.RegisterFunction("%REGISTER-METHOD-COMBINATION",
            new LispFunction(args =>
            {
                var name = ((LispString)args[0]).Value;
                var op = ((LispString)args[1]).Value;
                bool identity = args.Length > 2 && args[2] is not Nil;
                Runtime.RegisterMethodCombination(name, op, identity);
                return Nil.Instance;
            }));
        // Long-form method combination registration:
        // (%register-long-method-combination name group-specs-list body-function)
        // group-specs-list: ((name qualifier-pattern . options) ...)
        //   qualifier-pattern: * (match all) or NIL (unqualified) or a qualifier symbol
        //   options: :order :most-specific-first/:most-specific-last, :required t/nil
        Emitter.CilAssembler.RegisterFunction("%REGISTER-LONG-METHOD-COMBINATION",
            new LispFunction(args =>
            {
                var name = ((LispString)args[0]).Value;
                var specFn = (LispFunction)args[1];
                var bodyFn = (LispFunction)args[2];
                _longFormMCRegistry[name] = new LongFormMethodCombination { SpecFunction = specFn, BodyFunction = bodyFn };
                return Nil.Instance;
            }));
        // CLASS-NAME as a proper GF
        {
            var cnSym = Startup.Sym("CLASS-NAME");
            var cnGF = (GenericFunction)Runtime.MakeGF(cnSym, new Fixnum(1));
            cnGF.RequiredCount = 1;
            cnGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(cnSym, cnGF);
            cnSym.Function = cnGF;
            Emitter.CilAssembler.RegisterFunction("CLASS-NAME", cnGF);

            var tCls3 = Runtime.FindClass(Startup.Sym("T"));
            var cnSpecializers = new Cons(tCls3, Nil.Instance);
            var cnDefaultMethod = Runtime.MakeMethod(cnSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.ClassName(args[0])));
            ((LispMethod)cnDefaultMethod).RequiredCount = 1;
            Runtime.AddMethod(cnGF, cnDefaultMethod);
        }
        // (SETF CLASS-NAME) as a proper GF
        {
            var scnSym = Startup.Sym("(SETF CLASS-NAME)");
            var scnGF = (GenericFunction)Runtime.MakeGF(Startup.Sym("CLASS-NAME"), new Fixnum(2));
            scnGF.RequiredCount = 2;
            scnGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(scnSym, scnGF);
            scnSym.Function = scnGF;
            Emitter.CilAssembler.RegisterFunction("(SETF CLASS-NAME)", scnGF);

            var tCls4 = Runtime.FindClass(Startup.Sym("T"));
            var scnSpecializers = new Cons(tCls4, new Cons(tCls4, Nil.Instance));
            var scnDefaultMethod = Runtime.MakeMethod(scnSpecializers, Nil.Instance,
                new LispFunction(args => {
                    var newName = args[0];
                    var cls = args[1];
                    if (cls is not LispClass lc)
                        throw new LispErrorException(new LispTypeError("(SETF CLASS-NAME): not a class", cls));
                    if (newName is Symbol sym)
                        lc.Name = sym;
                    else if (newName is Nil)
                        lc.NameCleared = true;
                    else
                        lc.Name = Startup.Sym(newName.ToString());
                    return newName;
                }));
            ((LispMethod)scnDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(scnGF, scnDefaultMethod);
        }
        // ALLOCATE-INSTANCE as a proper GF
        {
            var aiSym = Startup.Sym("ALLOCATE-INSTANCE");
            var aiGF = (GenericFunction)Runtime.MakeGF(aiSym, new Fixnum(-1));
            aiGF.RequiredCount = 1;
            aiGF.HasRest = true;
            aiGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(aiSym, aiGF);
            aiSym.Function = aiGF;
            Emitter.CilAssembler.RegisterFunction("ALLOCATE-INSTANCE", aiGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var aiSpecializers = new Cons(tCls, Nil.Instance);
            var aiDefaultMethod = Runtime.MakeMethod(aiSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.MakeInstanceRaw(args[0])));
            ((LispMethod)aiDefaultMethod).RequiredCount = 1;
            ((LispMethod)aiDefaultMethod).HasRest = true;
            Runtime.AddMethod(aiGF, aiDefaultMethod);

            // allocate-instance (standard-generic-function) → real GenericFunction
            var sgfCls = Runtime.FindClass(Startup.Sym("STANDARD-GENERIC-FUNCTION"));
            var sgfAllocM = Runtime.MakeMethod(new Cons(sgfCls, Nil.Instance), Nil.Instance,
                new LispFunction(allocArgs => {
                    GenericFunction? newGf = null;
                    newGf = new GenericFunction(Startup.Sym("UNNAMED"), -1,
                        callArgs => Runtime.DispatchGF(newGf!, callArgs));
                    newGf.RequiredCount = 0;
                    newGf.LambdaListInfoSet = true;
                    return newGf;
                }));
            ((LispMethod)sgfAllocM).RequiredCount = 1;
            ((LispMethod)sgfAllocM).HasRest = true;
            Runtime.AddMethod(aiGF, sgfAllocM);

            // allocate-instance (standard-method) → raw LispMethod
            var smCls = Runtime.FindClass(Startup.Sym("STANDARD-METHOD"));
            var smAllocM = Runtime.MakeMethod(new Cons(smCls, Nil.Instance), Nil.Instance,
                new LispFunction(allocArgs => new LispMethod()));
            ((LispMethod)smAllocM).RequiredCount = 1;
            ((LispMethod)smAllocM).HasRest = true;
            Runtime.AddMethod(aiGF, smAllocM);
        }

        // METHOD-QUALIFIERS as a proper GF
        {
            var mqSym = Startup.Sym("METHOD-QUALIFIERS");
            var mqGF = (GenericFunction)Runtime.MakeGF(mqSym, new Fixnum(1));
            mqGF.RequiredCount = 1;
            mqGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(mqSym, mqGF);
            mqSym.Function = mqGF;
            Emitter.CilAssembler.RegisterFunction("METHOD-QUALIFIERS", mqGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var mqSpecializers = new Cons(tCls, Nil.Instance);
            var mqDefaultMethod = Runtime.MakeMethod(mqSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args[0] is not LispMethod method)
                        throw new LispErrorException(new LispTypeError("METHOD-QUALIFIERS: not a method", args[0]));
                    LispObject result = Nil.Instance;
                    for (int i = method.Qualifiers.Length - 1; i >= 0; i--)
                        result = new Cons(method.Qualifiers[i], result);
                    return result;
                }));
            ((LispMethod)mqDefaultMethod).RequiredCount = 1;
            Runtime.AddMethod(mqGF, mqDefaultMethod);
        }
        Startup.RegisterBinary("SLOT-BOUNDP", Runtime.SlotBoundp);
        Startup.RegisterBinary("SLOT-VALUE", Runtime.SlotValue);
        // SLOT-VALUE-USING-CLASS and (SETF SLOT-VALUE-USING-CLASS) as proper GFs (AMOP §5.4)
        {
            var tCls = Runtime.FindClass(Startup.Sym("T"));
            // SLOT-VALUE-USING-CLASS (class instance slot-def) → value
            var svucSym = Startup.Sym("SLOT-VALUE-USING-CLASS");
            var svucGF = (GenericFunction)Runtime.MakeGF(svucSym, new Fixnum(3));
            svucGF.RequiredCount = 3;
            svucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(svucSym, svucGF);
            svucSym.Function = svucGF;
            Emitter.CilAssembler.RegisterFunction("SLOT-VALUE-USING-CLASS", svucGF);
            var svucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var svucDefault = Runtime.MakeMethod(svucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 3)
                    throw new LispErrorException(new LispProgramError("SLOT-VALUE-USING-CLASS: wrong number of arguments"));
                var obj = args[1] is LispInstanceCondition lic2 ? lic2.Instance : args[1];
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("SLOT-VALUE-USING-CLASS: not a CLOS instance", args[1]));
                if (args[2] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("SLOT-VALUE-USING-CLASS: slot-def is not a slot definition", args[2]));
                if (!inst.Class.SlotIndex.TryGetValue(slotDef.Name.Name, out int idx))
                    throw new LispErrorException(new LispError($"SLOT-VALUE-USING-CLASS: no slot named {slotDef.Name.Name} in {inst.Class.Name.Name}"));
                return Runtime.SlotValueDirect(inst, idx, slotDef.Name, slotDef.Name.Name);
            }));
            ((LispMethod)svucDefault).RequiredCount = 3;
            Runtime.AddMethod(svucGF, svucDefault);
            // (SETF SLOT-VALUE-USING-CLASS) (new-value class instance slot-def) → new-value
            var setfSvucName = new Cons(Startup.Sym("SETF"), new Cons(svucSym, Nil.Instance));
            var setfSvucGF = (GenericFunction)Runtime.MakeGF(setfSvucName, new Fixnum(4));
            setfSvucGF.RequiredCount = 4;
            setfSvucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(setfSvucName, setfSvucGF); // also sets svucSym.SetfFunction
            Emitter.CilAssembler.RegisterFunction("(SETF SLOT-VALUE-USING-CLASS)", setfSvucGF);
            var setfSvucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance))));
            var setfSvucDefault = Runtime.MakeMethod(setfSvucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 4)
                    throw new LispErrorException(new LispProgramError("(SETF SLOT-VALUE-USING-CLASS): wrong number of arguments"));
                var newVal = args[0];
                var obj = args[2] is LispInstanceCondition lic3 ? lic3.Instance : args[2];
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("(SETF SLOT-VALUE-USING-CLASS): not a CLOS instance", args[2]));
                if (args[3] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("(SETF SLOT-VALUE-USING-CLASS): slot-def is not a slot definition", args[3]));
                if (!inst.Class.SlotIndex.TryGetValue(slotDef.Name.Name, out int idx))
                    throw new LispErrorException(new LispError($"(SETF SLOT-VALUE-USING-CLASS): no slot named {slotDef.Name.Name} in {inst.Class.Name.Name}"));
                return Runtime.SetSlotValueDirect(inst, idx, slotDef.Name.Name, newVal);
            }));
            ((LispMethod)setfSvucDefault).RequiredCount = 4;
            Runtime.AddMethod(setfSvucGF, setfSvucDefault);

            // SLOT-BOUNDP-USING-CLASS (class instance slot-def) → boolean (AMOP §5.4).
            // The default consults the raw slot vector; custom metaclasses (e.g. McCLIM's
            // dynamic slots) override to consult their own backing store so initforms and
            // slot-boundp see the right state.
            var sbucSym = Startup.Sym("SLOT-BOUNDP-USING-CLASS");
            var sbucGF = (GenericFunction)Runtime.MakeGF(sbucSym, new Fixnum(3));
            sbucGF.RequiredCount = 3;
            sbucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(sbucSym, sbucGF);
            sbucSym.Function = sbucGF;
            Emitter.CilAssembler.RegisterFunction("SLOT-BOUNDP-USING-CLASS", sbucGF);
            var sbucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var sbucDefault = Runtime.MakeMethod(sbucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 3)
                    throw new LispErrorException(new LispProgramError("SLOT-BOUNDP-USING-CLASS: wrong number of arguments"));
                var obj = args[1] is LispInstanceCondition licb ? licb.Instance : args[1];
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("SLOT-BOUNDP-USING-CLASS: not a CLOS instance", args[1]));
                if (args[2] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("SLOT-BOUNDP-USING-CLASS: slot-def is not a slot definition", args[2]));
                if (!inst.Class.SlotIndex.TryGetValue(slotDef.Name.Name, out int idx))
                    return Nil.Instance;
                return Runtime.SlotBoundpDirect(inst, idx, slotDef.Name.Name) ? T.Instance : Nil.Instance;
            }));
            ((LispMethod)sbucDefault).RequiredCount = 3;
            Runtime.AddMethod(sbucGF, sbucDefault);

            // SLOT-MAKUNBOUND-USING-CLASS (class instance slot-def) → instance (AMOP §5.4).
            var smucSym = Startup.Sym("SLOT-MAKUNBOUND-USING-CLASS");
            var smucGF = (GenericFunction)Runtime.MakeGF(smucSym, new Fixnum(3));
            smucGF.RequiredCount = 3;
            smucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(smucSym, smucGF);
            smucSym.Function = smucGF;
            Emitter.CilAssembler.RegisterFunction("SLOT-MAKUNBOUND-USING-CLASS", smucGF);
            var smucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var smucDefault = Runtime.MakeMethod(smucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 3)
                    throw new LispErrorException(new LispProgramError("SLOT-MAKUNBOUND-USING-CLASS: wrong number of arguments"));
                var obj = args[1] is LispInstanceCondition licm ? licm.Instance : args[1];
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("SLOT-MAKUNBOUND-USING-CLASS: not a CLOS instance", args[1]));
                if (args[2] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("SLOT-MAKUNBOUND-USING-CLASS: slot-def is not a slot definition", args[2]));
                if (inst.Class.SlotIndex.TryGetValue(slotDef.Name.Name, out int idx))
                    Runtime.SlotMakunboundDirect(inst, idx, slotDef.Name.Name);
                return inst;
            }));
            ((LispMethod)smucDefault).RequiredCount = 3;
            Runtime.AddMethod(smucGF, smucDefault);
        }
        Emitter.CilAssembler.RegisterFunction("SLOT-MAKUNBOUND", new LispFunction(args => {
            if (args.Length != 2) throw new LispErrorException(new LispProgramError("SLOT-MAKUNBOUND requires exactly 2 arguments"));
            var obj0 = args[0] is LispInstanceCondition lic0 ? lic0.Instance : args[0];
            string name = args[1] switch {
                Symbol sym => sym.Name,
                _ => args[1].ToString()
            };
            // Metaclass-added slot on a class metaobject.
            if (obj0 is LispClass klass && klass.Metaclass != null)
            {
                klass.ExtraSlots?.Remove(name);
                return klass;
            }
            if (obj0 is not LispInstance inst)
                throw new LispErrorException(new LispTypeError("SLOT-MAKUNBOUND: not a CLOS instance", args[0]));
            if (!inst.Class.SlotIndex.TryGetValue(name, out int idx))
            {
                if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
                {
                    slotMissing.Invoke(new LispObject[] { inst.Class, inst, args[1] is Symbol ? args[1] : Startup.Sym(name), Startup.Sym("SLOT-MAKUNBOUND") });
                    return args[0];
                }
                throw new LispErrorException(new LispError(
                    $"SLOT-MAKUNBOUND: no slot named {name} in class {inst.Class.Name.Name}"));
            }
            // AMOP §5.4: dispatch through slot-makunbound-using-class for custom metaclasses.
            if (inst.Class.Metaclass != null && Startup.Sym("SLOT-MAKUNBOUND-USING-CLASS").Function is LispFunction smucFn)
            {
                smucFn.Invoke(new LispObject[] { inst.Class, inst, inst.Class.EffectiveSlots[idx] });
                return args[0];
            }
            Runtime.SlotMakunboundDirect(inst, idx, name);
            return args[0];
        }));
        Emitter.CilAssembler.RegisterFunction("SLOT-EXISTS-P", new LispFunction(args => {
            if (args.Length != 2) throw new LispErrorException(new LispProgramError("SLOT-EXISTS-P requires exactly 2 arguments"));
            var obj0 = args[0] is LispInstanceCondition lic1 ? lic1.Instance : args[0];
            return Runtime.SlotExists(obj0, args[1]);
        }));
        // SLOT-MISSING generic function
        var slotMissingSym = Startup.Sym("SLOT-MISSING");
        GenericFunction slotMissingGF = null!;
        slotMissingGF = new GenericFunction(slotMissingSym, -1,
            args => Runtime.DispatchGFOrDefault(slotMissingGF, args, Runtime.SlotMissingDefault));
        slotMissingSym.Function = slotMissingGF;
        Runtime.RegisterGF(slotMissingSym, slotMissingGF);
        Emitter.CilAssembler.RegisterFunction("SLOT-MISSING", slotMissingGF);
        // SLOT-UNBOUND generic function
        var slotUnboundSym = Startup.Sym("SLOT-UNBOUND");
        GenericFunction slotUnboundGF = null!;
        slotUnboundGF = new GenericFunction(slotUnboundSym, 3,
            args => Runtime.DispatchGFOrDefault(slotUnboundGF, args, Runtime.SlotUnboundDefault));
        slotUnboundSym.Function = slotUnboundGF;
        Runtime.RegisterGF(slotUnboundSym, slotUnboundGF);
        Emitter.CilAssembler.RegisterFunction("SLOT-UNBOUND", slotUnboundGF);

        // UPDATE-INSTANCE-FOR-DIFFERENT-CLASS as a proper GF
        {
            var uifdcSym = Startup.Sym("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS");
            var uifdcGF = (GenericFunction)Runtime.MakeGF(uifdcSym, new Fixnum(2));
            Runtime.RegisterGF(uifdcSym, uifdcGF);
            uifdcSym.Function = uifdcGF;
            Emitter.CilAssembler.RegisterFunction("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS", uifdcGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var specializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(args => {
                    if (args.Length < 2) return Nil.Instance;
                    var previous = args[0];
                    var current = args[1];
                    LispObject addedSlots = Nil.Instance;
                    if (previous is LispInstance prevInst && current is LispInstance curInst)
                    {
                        foreach (var slot in curInst.Class.EffectiveSlots)
                        {
                            if (!prevInst.Class.SlotIndex.ContainsKey(slot.Name.Name))
                                addedSlots = new Cons(slot.Name, addedSlots);
                        }
                    }
                    var sharedInitSym2 = Startup.Sym("SHARED-INITIALIZE");
                    if (sharedInitSym2.Function is LispFunction sharedInitFn)
                    {
                        var siArgs = new LispObject[2 + (args.Length - 2)];
                        siArgs[0] = current;
                        siArgs[1] = addedSlots;
                        Array.Copy(args, 2, siArgs, 2, args.Length - 2);
                        sharedInitFn.Invoke(siArgs);
                    }
                    return current;
                }));
            Runtime.AddMethod(uifdcGF, defaultMethod);
        }

        // MAKE-INSTANCES-OBSOLETE as a proper GF
        var mioSym = Startup.Sym("MAKE-INSTANCES-OBSOLETE");
        Func<LispObject[], LispObject> mioDefault = args => {
            if (args.Length != 1)
                throw new LispErrorException(new LispProgramError($"MAKE-INSTANCES-OBSOLETE: wrong number of arguments: {args.Length} (expected 1)"));
            if (args[0] is Symbol sym2)
            {
                var cls = Runtime.FindClassOrNil(sym2);
                return cls ?? args[0];
            }
            return args[0];
        };
        GenericFunction mioGF = null!;
        mioGF = new GenericFunction(mioSym, 1,
            args => Runtime.DispatchGFOrDefault(mioGF, args, mioDefault));
        mioSym.Function = mioGF;
        Runtime.RegisterGF(mioSym, mioGF);
        Emitter.CilAssembler.RegisterFunction("MAKE-INSTANCES-OBSOLETE", mioGF);

        // NO-APPLICABLE-METHOD
        var namSym = Startup.Sym("NO-APPLICABLE-METHOD");
        Func<LispObject[], LispObject> namDefault = args => {
            var gfName = args.Length > 0 ? args[0].ToString() : "unknown";
            throw new LispErrorException(new LispError(
                $"No applicable method for generic function {gfName}"));
        };
        GenericFunction namGF = null!;
        namGF = new GenericFunction(namSym, -1,
            args => Runtime.DispatchGFOrDefault(namGF, args, namDefault));
        namSym.Function = namGF;
        Runtime.RegisterGF(namSym, namGF);
        Emitter.CilAssembler.RegisterFunction("NO-APPLICABLE-METHOD", namGF);

        // NO-NEXT-METHOD
        var nnmSym = Startup.Sym("NO-NEXT-METHOD");
        Func<LispObject[], LispObject> nnmDefault = args => {
            throw new LispErrorException(new LispError("No next method"));
        };
        GenericFunction nnmGF = null!;
        nnmGF = new GenericFunction(nnmSym, -1,
            args => Runtime.DispatchGFOrDefault(nnmGF, args, nnmDefault));
        nnmSym.Function = nnmGF;
        Runtime.RegisterGF(nnmSym, nnmGF);
        Emitter.CilAssembler.RegisterFunction("NO-NEXT-METHOD", nnmGF);
        // ADD-METHOD as a proper GF
        {
            var amSym = Startup.Sym("ADD-METHOD");
            var amGF = (GenericFunction)Runtime.MakeGF(amSym, new Fixnum(2));
            amGF.RequiredCount = 2;
            amGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(amSym, amGF);
            amSym.Function = amGF;
            Emitter.CilAssembler.RegisterFunction("ADD-METHOD", amGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var amSpecializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var amDefaultMethod = Runtime.MakeMethod(amSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.AddMethod(args[0], args[1])));
            ((LispMethod)amDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(amGF, amDefaultMethod);
        }
        // REMOVE-METHOD as a proper GF
        {
            var rmSym = Startup.Sym("REMOVE-METHOD");
            var rmGF = (GenericFunction)Runtime.MakeGF(rmSym, new Fixnum(2));
            rmGF.RequiredCount = 2;
            rmGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(rmSym, rmGF);
            rmSym.Function = rmGF;
            Emitter.CilAssembler.RegisterFunction("REMOVE-METHOD", rmGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var rmSpecializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var rmDefaultMethod = Runtime.MakeMethod(rmSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.RemoveMethod(args[0], args[1])));
            ((LispMethod)rmDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(rmGF, rmDefaultMethod);
        }
        // COMPUTE-APPLICABLE-METHODS as a proper GF
        {
            var camSym = Startup.Sym("COMPUTE-APPLICABLE-METHODS");
            var camGF = (GenericFunction)Runtime.MakeGF(camSym, new Fixnum(2));
            camGF.RequiredCount = 2;
            camGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(camSym, camGF);
            camSym.Function = camGF;
            Emitter.CilAssembler.RegisterFunction("COMPUTE-APPLICABLE-METHODS", camGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var camSpecializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var camDefaultMethod = Runtime.MakeMethod(camSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.ComputeApplicableMethods(args[0], args[1])));
            ((LispMethod)camDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(camGF, camDefaultMethod);
        }
        // NEXT-METHOD-P and CALL-NEXT-METHOD
        Emitter.CilAssembler.RegisterFunction("NEXT-METHOD-P",
            new LispFunction(args => Runtime.NextMethodP(), "NEXT-METHOD-P", 0));
        Emitter.CilAssembler.RegisterFunction("CALL-NEXT-METHOD",
            new LispFunction(Runtime.CallNextMethod, "CALL-NEXT-METHOD", -1));

        // CLOS intrinsics callable as functions, for the emit-free tree-walk
        // interpreter (%mini-eval). The compiler emits these as inline
        // (:call "Runtime.X") via the special-form table; the compiled path is
        // unaffected. defclass/defgeneric/defmethod macro-expand into calls to
        // these (DOTCL-INTERNAL package), so the interpreter must be able to
        // apply them. All map to existing Runtime methods — no new emit.
        void RegClos(string name, System.Func<LispObject[], LispObject> fn, int arity = -1)
            => Emitter.CilAssembler.RegisterFunction(name, new LispFunction(fn, name, arity));
        RegClos("%MAKE-CLASS", a => Runtime.MakeClass(a[0], a[1], a[2]), 3);
        RegClos("%REGISTER-CLASS", a => Runtime.RegisterClass(a[0]), 1);
        RegClos("%MAKE-SLOT-DEF", a => Runtime.MakeSlotDef(a[0], a[1], a[2]), 3);
        RegClos("%SLOT-DEF-RAW-OPTIONS", a => Runtime.SetSlotDefRawOptions(a[0], a[1]), 2);
        RegClos("%SET-CLASS-DEFAULT-INITARGS", a => Runtime.SetClassDefaultInitargs(a[0], a[1]), 2);
        RegClos("%FIND-CLASS-OR-NIL", a => Runtime.FindClassOrNil(a[0]), 1);
        RegClos("%SET-SLOT-VALUE", a => Runtime.SetSlotValue(a[0], a[1], a[2]), 3);
        RegClos("%ALLOCATE-INSTANCE", a => Runtime.MakeInstanceRaw(a[0]), 1);
        RegClos("%SLOT-EXISTS-P", a => Runtime.SlotExists(a[0], a[1]), 2);
        RegClos("%CHANGE-CLASS", Runtime.ChangeClass);
        RegClos("%MAKE-GF", a => Runtime.MakeGF(a[0], a[1]), 2);
        RegClos("%REGISTER-GF", a => Runtime.RegisterGF(a[0], a[1]), 2);
        RegClos("%FIND-GF", a => Runtime.FindGF(a[0]), 1);
        RegClos("%SET-GF-LAMBDA-LIST-INFO", Runtime.SetGFLambdaListInfo);
        RegClos("%SET-METHOD-LAMBDA-LIST-INFO", Runtime.SetMethodLambdaListInfo);
        RegClos("%MAKE-METHOD", a => Runtime.MakeMethod(a[0], a[1], a[2]), 3);
        RegClos("%ADD-METHOD", a => Runtime.AddMethod(a[0], a[1]), 2);
        RegClos("%GF-METHODS", a => Runtime.GetGFMethods(a[0]), 1);
        RegClos("%METHOD-SPECIALIZERS", a => Runtime.MethodSpecializers(a[0]), 1);
        RegClos("%METHOD-QUALIFIERS", a => Runtime.MethodQualifiers(a[0]), 1);
        RegClos("%METHOD-FUNCTION", a => Runtime.MethodFunction(a[0]), 1);
        RegClos("%CLEAR-DEFGENERIC-INLINE-METHODS", a => Runtime.ClearDefgenericInlineMethods(a[0]), 1);
        RegClos("%MARK-DEFGENERIC-INLINE-METHOD", a => Runtime.MarkDefgenericInlineMethod(a[0], a[1]), 2);
        RegClos("%SET-METHOD-COMBINATION", a => Runtime.SetMethodCombination(a[0], a[1]), 2);
        RegClos("%SET-METHOD-COMBINATION-ORDER", a => Runtime.SetMethodCombinationOrder(a[0], a[1]), 2);
        RegClos("%SET-METHOD-COMBINATION-ARGS", a => Runtime.SetMethodCombinationArgs(a[0], a[1]), 2);
        RegClos("%CAPTURED-CALL-NEXT-METHOD", a => Runtime.CapturedCnm(), 0);
        RegClos("%CAPTURED-NEXT-METHOD-P", a => Runtime.CapturedNmp(), 0);
        // defstruct intrinsics (same rationale).
        RegClos("%MAKE-STRUCT", a => Runtime.MakeStruct(a[0], a.SubArray(1)));
        RegClos("%STRUCT-REF", a => Runtime.StructRef(a[0], a[1]), 2);
        RegClos("%STRUCT-SET", a => Runtime.StructSet(a[0], a[1], a[2]), 3);
        RegClos("%STRUCT-TYPEP", a => Runtime.StructTypep(a[0], a[1]), 2);
        RegClos("%COPY-STRUCT", a => Runtime.CopyStruct(a[0]), 1);
        // array/string element-set intrinsics (setf aref / setf char).
        RegClos("%AREF-SET", a => a.Length switch
        {
            3 => Runtime.ArefSet(a[0], a[1], a[2]),
            4 => Runtime.ArefSet2D(a[0], a[1], a[2], a[3]),
            5 => Runtime.ArefSet3D(a[0], a[1], a[2], a[3], a[4]),
            _ => Runtime.ArefSetMulti(a),
        });
        RegClos("%CHAR-SET", a => Runtime.CharSet(a[0], a[1], a[2]), 3);

        // MAKE-INSTANCE as a proper GF
        {
            var miSym = Startup.Sym("MAKE-INSTANCE");
            var miGF = (GenericFunction)Runtime.MakeGF(miSym, new Fixnum(-1));
            miGF.RequiredCount = 1;
            miGF.HasRest = true;
            miGF.HasKey = true;
            miGF.HasAllowOtherKeys = true;
            miGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(miSym, miGF);
            miSym.Function = miGF;
            Emitter.CilAssembler.RegisterFunction("MAKE-INSTANCE", miGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var miSpecializers = new Cons(tCls, Nil.Instance);
            var miQualifiers = Nil.Instance;
            var miDefaultMethod = Runtime.MakeMethod(miSpecializers, miQualifiers,
                new LispFunction(args => {
                    if (args.Length == 0)
                        throw new LispErrorException(new LispProgramError("MAKE-INSTANCE: requires at least 1 argument"));
                    return Runtime.MakeInstanceWithInitargs(args[0],
                        args.Length > 1 ? args.SubArray(1) : Array.Empty<LispObject>());
                }));
            ((LispMethod)miDefaultMethod).RequiredCount = 1;
            ((LispMethod)miDefaultMethod).HasRest = true;
            ((LispMethod)miDefaultMethod).HasKey = true;
            ((LispMethod)miDefaultMethod).HasAllowOtherKeys = true;
            Runtime.AddMethod(miGF, miDefaultMethod);
        }

        // FIND-CLASS
        Emitter.CilAssembler.RegisterFunction("FIND-CLASS", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("FIND-CLASS: too few arguments"));
            if (args.Length > 3) throw new LispErrorException(new LispProgramError($"FIND-CLASS: too many arguments: {args.Length} (expected 1-3)"));
            bool errorp = args.Length < 2 || Runtime.IsTruthy(args[1]);
            return errorp ? Runtime.FindClass(args[0]) : Runtime.FindClassOrNil(args[0]);
        }, "FIND-CLASS"));
        Emitter.CilAssembler.RegisterFunction("%FIND-OR-FORWARD-CLASS",
            new LispFunction(args => Runtime.FindOrForwardClass(args[0]),
                "%FIND-OR-FORWARD-CLASS", 1));
        Emitter.CilAssembler.RegisterFunction("(SETF FIND-CLASS)", new LispFunction(args => {
            if (args.Length < 2) throw new Exception("(SETF FIND-CLASS): too few arguments");
            var newVal = args[0];
            var sym = ToClassSymbol(args[1]);
            if (newVal is Nil) {
                _classRegistry.TryRemove(sym, out _);
            } else if (newVal is LispClass lc) {
                _classRegistry[sym] = lc;
            } else {
                throw new LispErrorException(new LispTypeError("(SETF FIND-CLASS): not a class", newVal));
            }
            return newVal;
        }, "(SETF FIND-CLASS)"));
        Startup.RegisterUnary("CLASS-OF", Runtime.ClassOf);

        // %REGISTER-STRUCT-CLASS
        Emitter.CilAssembler.RegisterFunction("%REGISTER-STRUCT-CLASS", new LispFunction(args => {
            var name = (Symbol)args[0];
            LispClass? parentCls = null;
            if (args.Length > 1 && args[1] is Symbol parentSym && parentSym.Name != "NIL") {
                parentCls = Runtime.FindClassOrNil(parentSym) as LispClass;
            }
            if (parentCls == null) {
                parentCls = Runtime.FindClassOrNil(Startup.Sym("STRUCTURE-OBJECT")) as LispClass
                    ?? throw new Exception("%REGISTER-STRUCT-CLASS: STRUCTURE-OBJECT class not found");
            }
            var slotNames = new Symbol[args.Length - 2];
            var directSlots = new SlotDefinition[args.Length - 2];
            for (int i = 2; i < args.Length; i++)
            {
                Symbol slotSym;
                if (args[i] is Symbol sym3)
                    slotSym = sym3;
                else if (args[i] is T)
                    slotSym = Startup.Sym("T");
                else if (args[i] is Nil)
                    slotSym = Startup.Sym("NIL");
                else
                    slotSym = Startup.Sym(args[i].ToString() ?? "");
                slotNames[i - 2] = slotSym;
                directSlots[i - 2] = new SlotDefinition(slotSym);
            }
            var cls = new LispClass(name, directSlots, new[] { parentCls });
            cls.IsStructureClass = true;
            cls.StructSlotNames = slotNames;
            cls.FinalizeClass();
            Runtime.RegisterClass(cls);
            return name;
        }));

        // FIND-METHOD as a proper GF
        {
            var fmSym = Startup.Sym("FIND-METHOD");
            var fmGF = (GenericFunction)Runtime.MakeGF(fmSym, new Fixnum(-1));
            fmGF.RequiredCount = 3;
            fmGF.OptionalCount = 1;
            fmGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(fmSym, fmGF);
            fmSym.Function = fmGF;
            Emitter.CilAssembler.RegisterFunction("FIND-METHOD", fmGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var fmSpecializers = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var fmDefaultMethod = Runtime.MakeMethod(fmSpecializers, Nil.Instance,
                new LispFunction(args => {
                if (args.Length < 3 || args.Length > 4)
                    throw new LispErrorException(new LispProgramError(
                        $"FIND-METHOD: expected 3 or 4 arguments, got {args.Length}"));
                bool errorp = args.Length < 4 || args[3] != Nil.Instance;
                if (args[0] is GenericFunction gf)
                {
                    int specCount = 0;
                    var specCheck = args[2];
                    while (specCheck is Cons sc2) { specCount++; specCheck = sc2.Cdr; }
                    if (gf.LambdaListInfoSet && specCount != gf.RequiredCount)
                        throw new LispErrorException(new LispError(
                            $"FIND-METHOD: specializer list length {specCount} does not match " +
                            $"the number of required parameters {gf.RequiredCount} of {gf.Name.Name}"));

                    var qualList = new System.Collections.Generic.List<Symbol>();
                    var ql = args[1];
                    while (ql is Cons qc) { if (qc.Car is Symbol qs) qualList.Add(qs); ql = qc.Cdr; }

                    foreach (var method in gf.Methods)
                    {
                        if (method.Qualifiers.Length != qualList.Count) continue;
                        bool qualMatch = true;
                        for (int i = 0; i < qualList.Count; i++)
                        {
                            if (!ReferenceEquals(method.Qualifiers[i], qualList[i]))
                            { qualMatch = false; break; }
                        }
                        if (!qualMatch) continue;

                        bool match = true;
                        var specList = args[2];
                        for (int i = 0; i < method.Specializers.Length; i++)
                        {
                            if (specList is not Cons sc) { match = false; break; }
                            if (!ReferenceEquals(method.Specializers[i], sc.Car))
                            {
                                string mName = method.Specializers[i] is LispClass mc ? mc.Name.Name
                                    : method.Specializers[i] is Symbol ms ? ms.Name
                                    : method.Specializers[i].ToString();
                                string sName = sc.Car is LispClass lc2 ? lc2.Name.Name
                                    : sc.Car is Symbol ss ? ss.Name
                                    : sc.Car.ToString();
                                if (!string.Equals(mName, sName, StringComparison.OrdinalIgnoreCase))
                                { match = false; break; }
                            }
                            specList = sc.Cdr;
                        }
                        if (match) return method;
                    }
                }
                if (!errorp) return Nil.Instance;
                throw new LispErrorException(new LispError("FIND-METHOD: method not found"));
            }));
            ((LispMethod)fmDefaultMethod).RequiredCount = 3;
            ((LispMethod)fmDefaultMethod).OptionalCount = 1;
            Runtime.AddMethod(fmGF, fmDefaultMethod);
        }

        // shared-initialize as GF with default method on T
        var tClass = Runtime.FindClass(Startup.Sym("T"));
        {
            var siName = Startup.Sym("SHARED-INITIALIZE");
            var gf = Runtime.MakeGF(siName, new Fixnum(2));
            Runtime.RegisterGF(siName, gf);
            siName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("SHARED-INITIALIZE", (LispFunction)gf);

            var specializers = new Cons(tClass, Nil.Instance);
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(Runtime.SharedInitialize));
            Runtime.AddMethod(gf, defaultMethod);
        }

        // initialize-instance as GF
        {
            var iiName = Startup.Sym("INITIALIZE-INSTANCE");
            var gf = Runtime.MakeGF(iiName, new Fixnum(1));
            Runtime.RegisterGF(iiName, gf);
            iiName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("INITIALIZE-INSTANCE", (LispFunction)gf);

            var specializers = new Cons(tClass, Nil.Instance);
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(Runtime.InitializeInstance));
            Runtime.AddMethod(gf, defaultMethod);

            // initialize-instance primary for GENERIC-FUNCTION — skip shared-initialize (no Lisp slots)
            {
                var gfPrimCls = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var gfPrimM = Runtime.MakeMethod(new Cons(gfPrimCls, Nil.Instance), Nil.Instance,
                    new LispFunction(args => args[0]));
                ((LispMethod)gfPrimM).RequiredCount = 1;
                ((LispMethod)gfPrimM).HasRest = true;
                ((LispMethod)gfPrimM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, gfPrimM);
            }

            // initialize-instance primary for METHOD — skip shared-initialize (no Lisp slots)
            {
                var mPrimCls = Runtime.FindClass(Startup.Sym("METHOD"));
                var mPrimM = Runtime.MakeMethod(new Cons(mPrimCls, Nil.Instance), Nil.Instance,
                    new LispFunction(args => args[0]));
                ((LispMethod)mPrimM).RequiredCount = 1;
                ((LispMethod)mPrimM).HasRest = true;
                ((LispMethod)mPrimM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, mPrimM);
            }

            // initialize-instance :after for GenericFunction — apply initargs
            {
                var afterQuals = new Cons(Startup.Keyword("AFTER"), Nil.Instance);
                var gfCls2 = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var gfAfterM = Runtime.MakeMethod(new Cons(gfCls2, Nil.Instance), afterQuals,
                    new LispFunction(args => {
                        if (args[0] is not GenericFunction ugf) return args[0];
                        for (int i = 1; i + 1 < args.Length; i += 2)
                        {
                            if (args[i] is not Symbol ks) continue;
                            if (ks.Name == "LAMBDA-LIST")
                                ParseLambdaListIntoGF(ugf, args[i + 1]);
                            else if (ks.Name == "NAME" && args[i + 1] is Symbol ns
                                     && ugf.Name.Name == "UNNAMED")
                            {
                                ns.Function = ugf;
                                Runtime.RegisterGF(ns, ugf);
                            }
                        }
                        return ugf;
                    }));
                ((LispMethod)gfAfterM).RequiredCount = 1;
                ((LispMethod)gfAfterM).HasRest = true;
                ((LispMethod)gfAfterM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, gfAfterM);

                // initialize-instance :after for standard-method — set qualifiers/specializers/function
                var smCls2 = Runtime.FindClass(Startup.Sym("STANDARD-METHOD"));
                var smAfterM = Runtime.MakeMethod(new Cons(smCls2, Nil.Instance), afterQuals,
                    new LispFunction(args => {
                        if (args[0] is not LispMethod m) return args[0];
                        for (int i = 1; i + 1 < args.Length; i += 2)
                        {
                            if (args[i] is not Symbol ks) continue;
                            switch (ks.Name)
                            {
                                case "QUALIFIERS":
                                    m.Qualifiers = CollectSymbols(args[i + 1]);
                                    break;
                                case "SPECIALIZERS":
                                    m.Specializers = CollectList(args[i + 1]);
                                    break;
                                case "FUNCTION":
                                    if (args[i + 1] is LispFunction mf) m.Function = mf;
                                    break;
                                case "LAMBDA-LIST":
                                    ParseLambdaListIntoMethod(m, args[i + 1]);
                                    break;
                            }
                        }
                        return m;
                    }));
                ((LispMethod)smAfterM).RequiredCount = 1;
                ((LispMethod)smAfterM).HasRest = true;
                ((LispMethod)smAfterM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, smAfterM);
            }
        }

        // reinitialize-instance as GF
        {
            var riName = Startup.Sym("REINITIALIZE-INSTANCE");
            var gf = Runtime.MakeGF(riName, new Fixnum(1));
            Runtime.RegisterGF(riName, gf);
            riName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("REINITIALIZE-INSTANCE", (LispFunction)gf);

            var specializers = new Cons(tClass, Nil.Instance);
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(Runtime.ReinitializeInstance));
            Runtime.AddMethod(gf, defaultMethod);
        }

        // describe-object as GF with default method on T
        {
            var doName = Startup.Sym("DESCRIBE-OBJECT");
            var gf = Runtime.MakeGF(doName, new Fixnum(2));
            Runtime.RegisterGF(doName, gf);
            doName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("DESCRIBE-OBJECT", (LispFunction)gf);

            var specializers = new Cons(tClass, new Cons(tClass, Nil.Instance));
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(args => {
                    if (args.Length < 2) throw new LispErrorException(new LispProgramError("DESCRIBE-OBJECT: requires 2 arguments"));
                    var obj = args[0];
                    var stream = args[1];
                    var writer = Runtime.GetOutputWriter(stream);
                    var typeObj = Runtime.TypeOf(obj);
                    writer.Write(Runtime.FormatObject(obj, true));
                    writer.WriteLine();
                    writer.Write("  [");
                    writer.Write(Runtime.FormatObject(typeObj, true));
                    writer.WriteLine("]");
                    writer.Flush();
                    return Nil.Instance;
                }, "DESCRIBE-OBJECT-DEFAULT", 2));
            Runtime.AddMethod(gf, defaultMethod);
        }

        // CHANGE-CLASS as a proper GF
        {
            var ccSym = Startup.Sym("CHANGE-CLASS");
            var ccGF = (GenericFunction)Runtime.MakeGF(ccSym, new Fixnum(-1));
            ccGF.RequiredCount = 2;
            ccGF.HasRest = true;
            ccGF.HasKey = true;
            ccGF.HasAllowOtherKeys = true;
            ccGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(ccSym, ccGF);
            ccSym.Function = ccGF;
            Emitter.CilAssembler.RegisterFunction("CHANGE-CLASS", ccGF);

            var tCls2 = Runtime.FindClass(Startup.Sym("T"));
            var ccSpecializers = new Cons(tCls2, new Cons(tCls2, Nil.Instance));
            var ccQualifiers = Nil.Instance;
            var ccDefaultMethod = Runtime.MakeMethod(ccSpecializers, ccQualifiers,
                new LispFunction(Runtime.ChangeClass));
            ((LispMethod)ccDefaultMethod).RequiredCount = 2;
            ((LispMethod)ccDefaultMethod).HasRest = true;
            ((LispMethod)ccDefaultMethod).HasKey = true;
            ((LispMethod)ccDefaultMethod).HasAllowOtherKeys = true;
            Runtime.AddMethod(ccGF, ccDefaultMethod);
        }

        // MAKE-LOAD-FORM: standard GF
        {
            var mlfSym = Startup.Sym("MAKE-LOAD-FORM");
            GenericFunction mlfGf = null!;
            mlfGf = new GenericFunction(mlfSym, -1, args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM: wrong number of arguments: 0 (expected 1-2)"));
                if (args.Length > 2)
                    throw new LispErrorException(new LispProgramError($"MAKE-LOAD-FORM: wrong number of arguments: {args.Length} (expected 1-2)"));
                return Runtime.DispatchGFOrDefault(mlfGf, args, mlfArgs => {
                    var obj = mlfArgs[0];
                    string className = "<unknown>";
                    if (obj is LispInstance inst2) className = inst2.Class.Name.Name;
                    else if (obj is LispInstanceCondition lic2) className = lic2.Instance.Class.Name.Name;
                    else if (obj is LispStruct ls2) className = ls2.TypeName.Name;
                    throw new LispErrorException(new LispError(
                        $"No applicable method for MAKE-LOAD-FORM on object of class {className}"));
                });
            });
            mlfGf.RequiredCount = 1;
            mlfGf.OptionalCount = 1;
            mlfGf.HasRest = false;
            mlfGf.HasKey = false;
            Runtime.RegisterGF(mlfSym, mlfGf);
            mlfSym.Function = mlfGf;
            Emitter.CilAssembler.RegisterFunction("MAKE-LOAD-FORM", mlfGf);

            var mlfSpecializers = new Cons(tClass, Nil.Instance);
            var mlfQualifiers = Nil.Instance;
            var mlfDefaultMethod = Runtime.MakeMethod(mlfSpecializers, mlfQualifiers,
                new LispFunction(args => {
                    var obj = args[0];
                    string className = "<unknown>";
                    if (obj is LispInstance inst) className = inst.Class.Name.Name;
                    else if (obj is LispInstanceCondition lic) className = lic.Instance.Class.Name.Name;
                    else if (obj is LispStruct ls) className = ls.TypeName.Name;
                    throw new LispErrorException(new LispError(
                        $"No applicable method for MAKE-LOAD-FORM on object of class {className}"));
                }, "MAKE-LOAD-FORM-DEFAULT", -1));
            if (mlfDefaultMethod is LispMethod mlfMeth)
            {
                mlfMeth.RequiredCount = 1;
                mlfMeth.OptionalCount = 1;
            }
            Runtime.AddMethod(mlfGf, mlfDefaultMethod);
        }

        // FUNCTION-KEYWORDS as a proper GF
        {
            var fkSym = Startup.Sym("FUNCTION-KEYWORDS");
            var fkGF = (GenericFunction)Runtime.MakeGF(fkSym, new Fixnum(1));
            fkGF.RequiredCount = 1;
            fkGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(fkSym, fkGF);
            fkSym.Function = fkGF;
            Emitter.CilAssembler.RegisterFunction("FUNCTION-KEYWORDS", fkGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var fkSpecializers = new Cons(tCls, Nil.Instance);
            var fkDefaultMethod = Runtime.MakeMethod(fkSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args[0] is LispMethod m)
                    {
                        LispObject kwList = Nil.Instance;
                        for (int i = m.KeywordNames.Count - 1; i >= 0; i--)
                        {
                            var kwSym = Startup.Keyword(m.KeywordNames[i]);
                            kwList = new Cons(kwSym, kwList);
                        }
                        return MultipleValues.Values(kwList, m.HasAllowOtherKeys ? (LispObject)T.Instance : Nil.Instance);
                    }
                    return MultipleValues.Values(Nil.Instance, Nil.Instance);
                }));
            ((LispMethod)fkDefaultMethod).RequiredCount = 1;
            Runtime.AddMethod(fkGF, fkDefaultMethod);
        }
        // PRINT-OBJECT as a proper GF with default method on T
        {
            var poSym = Startup.Sym("PRINT-OBJECT");
            var poGF = (GenericFunction)Runtime.MakeGF(poSym, new Fixnum(2));
            poGF.RequiredCount = 2;
            poGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(poSym, poGF);
            poSym.Function = poGF;
            Emitter.CilAssembler.RegisterFunction("PRINT-OBJECT", poGF);

            var poCls = Runtime.FindClass(Startup.Sym("T"));
            var poSpecializers = new Cons(poCls, new Cons(poCls, Nil.Instance));
            var poDefaultMethod = Runtime.MakeMethod(poSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args.Length < 2) throw new LispErrorException(new LispProgramError("PRINT-OBJECT: requires 2 arguments"));
                    var writer = Runtime.GetOutputWriter(args[1]);
                    writer.Write(Runtime.FormatTop(args[0], true));
                    writer.Flush();
                    return args[0];
                }, "PRINT-OBJECT-DEFAULT", 2));
            ((LispMethod)poDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(poGF, poDefaultMethod);
        }
        // UPDATE-INSTANCE-FOR-REDEFINED-CLASS as a proper GF
        {
            var uirSym = Startup.Sym("UPDATE-INSTANCE-FOR-REDEFINED-CLASS");
            var uirGF = (GenericFunction)Runtime.MakeGF(uirSym, new Fixnum(-1));
            uirGF.RequiredCount = 4;
            uirGF.HasRest = true;
            uirGF.HasKey = true;
            uirGF.HasAllowOtherKeys = true;
            uirGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(uirSym, uirGF);
            uirSym.Function = uirGF;
            Emitter.CilAssembler.RegisterFunction("UPDATE-INSTANCE-FOR-REDEFINED-CLASS", uirGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var uirSpecializers = new Cons(tCls, new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance))));
            var uirDefaultMethod = Runtime.MakeMethod(uirSpecializers, Nil.Instance,
                new LispFunction(args => Nil.Instance));
            ((LispMethod)uirDefaultMethod).RequiredCount = 4;
            ((LispMethod)uirDefaultMethod).HasRest = true;
            ((LispMethod)uirDefaultMethod).HasKey = true;
            ((LispMethod)uirDefaultMethod).HasAllowOtherKeys = true;
            Runtime.AddMethod(uirGF, uirDefaultMethod);
        }

        // MOP accessor functions for slot definitions (AMOP)
        // SLOT-DEFINITION-NAME
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-NAME", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-NAME: wrong arg count"));
            if (args[0] is SlotDefinition sd) return sd.Name;
            throw new LispErrorException(new LispTypeError("SLOT-DEFINITION-NAME: not a slot definition", args[0]));
        }, "SLOT-DEFINITION-NAME", 1));

        // SLOT-DEFINITION-TYPE
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-TYPE", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-TYPE: wrong arg count"));
            if (args[0] is SlotDefinition) return T.Instance;  // T = no type restriction
            throw new LispErrorException(new LispTypeError("SLOT-DEFINITION-TYPE: not a slot definition", args[0]));
        }, "SLOT-DEFINITION-TYPE", 1));

        // SLOT-DEFINITION-INITARGS
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITARGS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITARGS: wrong arg count"));
            if (args[0] is SlotDefinition sd) {
                LispObject result = Nil.Instance;
                for (int i = sd.Initargs.Length - 1; i >= 0; i--)
                    result = new Cons(sd.Initargs[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("SLOT-DEFINITION-INITARGS: not a slot definition", args[0]));
        }, "SLOT-DEFINITION-INITARGS", 1));

        // SLOT-DEFINITION-INITFORM / SLOT-DEFINITION-INITFUNCTION
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITFORM", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITFORM: wrong arg count"));
            // CL spec: returns the initform if one was specified, or signals slot-value-not-available if none.
            // Simplified: we don't store the raw initform, only the compiled thunk. Return NIL if no initform.
            if (args[0] is SlotDefinition) return Nil.Instance;
            throw new LispErrorException(new LispTypeError("SLOT-DEFINITION-INITFORM: not a slot definition", args[0]));
        }, "SLOT-DEFINITION-INITFORM", 1));

        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITFUNCTION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITFUNCTION: wrong arg count"));
            if (args[0] is SlotDefinition sd)
                return sd.InitformThunk != null ? (LispObject)sd.InitformThunk : Nil.Instance;
            throw new LispErrorException(new LispTypeError("SLOT-DEFINITION-INITFUNCTION: not a slot definition", args[0]));
        }, "SLOT-DEFINITION-INITFUNCTION", 1));

        // SLOT-DEFINITION-ALLOCATION → :instance or :class
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-ALLOCATION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-ALLOCATION: wrong arg count"));
            if (args[0] is SlotDefinition sd2)
                return sd2.IsClassAllocation ? (LispObject)Startup.Keyword("CLASS") : Startup.Keyword("INSTANCE");
            throw new LispErrorException(new LispTypeError("SLOT-DEFINITION-ALLOCATION: not a slot definition", args[0]));
        }, "SLOT-DEFINITION-ALLOCATION", 1));

        // SLOT-DEFINITION-READERS / SLOT-DEFINITION-WRITERS → NIL stub
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-READERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-READERS: wrong arg count"));
            return Nil.Instance;
        }, "SLOT-DEFINITION-READERS", 1));
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-WRITERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-WRITERS: wrong arg count"));
            return Nil.Instance;
        }, "SLOT-DEFINITION-WRITERS", 1));

        // CLASS-DIRECT-SLOTS: list of SlotDefinition objects for the class's own slots
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SLOTS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SLOTS: wrong arg count"));
            if (args[0] is LispClass lc) {
                LispObject result = Nil.Instance;
                for (int i = lc.DirectSlots.Length - 1; i >= 0; i--)
                    result = new Cons(lc.DirectSlots[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("CLASS-DIRECT-SLOTS: not a class", args[0]));
        }, "CLASS-DIRECT-SLOTS", 1));

        // CLASS-SLOTS: list of effective SlotDefinition objects (all inherited slots)
        Emitter.CilAssembler.RegisterFunction("CLASS-SLOTS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-SLOTS: wrong arg count"));
            if (args[0] is LispClass lc) {
                LispObject result = Nil.Instance;
                if (lc.EffectiveSlots != null)
                    for (int i = lc.EffectiveSlots.Length - 1; i >= 0; i--)
                        result = new Cons(lc.EffectiveSlots[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("CLASS-SLOTS: not a class", args[0]));
        }, "CLASS-SLOTS", 1));

        // CLASS-DIRECT-SUPERCLASSES
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SUPERCLASSES", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SUPERCLASSES: wrong arg count"));
            if (args[0] is LispClass lc) {
                LispObject result = Nil.Instance;
                for (int i = lc.DirectSuperclasses.Length - 1; i >= 0; i--)
                    result = new Cons(lc.DirectSuperclasses[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("CLASS-DIRECT-SUPERCLASSES: not a class", args[0]));
        }, "CLASS-DIRECT-SUPERCLASSES", 1));

        // CLASS-DIRECT-SUBCLASSES — tracked at registration time would be ideal, but
        // for now return NIL (stub). Real implementation requires tracking all registered subclasses.
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SUBCLASSES", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SUBCLASSES: wrong arg count"));
            if (args[0] is LispClass) return Nil.Instance;
            throw new LispErrorException(new LispTypeError("CLASS-DIRECT-SUBCLASSES: not a class", args[0]));
        }, "CLASS-DIRECT-SUBCLASSES", 1));

        // CLASS-PRECEDENCE-LIST
        Emitter.CilAssembler.RegisterFunction("CLASS-PRECEDENCE-LIST", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-PRECEDENCE-LIST: wrong arg count"));
            if (args[0] is LispClass lc) {
                if (lc.ClassPrecedenceList == null || lc.ClassPrecedenceList.Length == 0)
                    return new Cons(lc, Nil.Instance);
                LispObject result = Nil.Instance;
                for (int i = lc.ClassPrecedenceList.Length - 1; i >= 0; i--)
                    result = new Cons(lc.ClassPrecedenceList[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("CLASS-PRECEDENCE-LIST: not a class", args[0]));
        }, "CLASS-PRECEDENCE-LIST", 1));

        // CLASS-FINALIZED-P — all dotcl classes are considered finalized
        Emitter.CilAssembler.RegisterFunction("CLASS-FINALIZED-P", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-FINALIZED-P: wrong arg count"));
            if (args[0] is LispClass) return T.Instance;
            throw new LispErrorException(new LispTypeError("CLASS-FINALIZED-P: not a class", args[0]));
        }, "CLASS-FINALIZED-P", 1));

        // CLASS-PROTOTYPE — make a prototype instance of a class
        Emitter.CilAssembler.RegisterFunction("CLASS-PROTOTYPE", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-PROTOTYPE: wrong arg count"));
            if (args[0] is LispClass lc && !lc.IsBuiltIn)
                return lc.Prototype; // memoized: stable identity for (eql class-prototype) dispatch
            throw new LispErrorException(new LispError("CLASS-PROTOTYPE: cannot create prototype for built-in class"));
        }, "CLASS-PROTOTYPE", 1));

        // GENERIC-FUNCTION-METHODS
        Emitter.CilAssembler.RegisterFunction("GENERIC-FUNCTION-METHODS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("GENERIC-FUNCTION-METHODS: wrong arg count"));
            if (args[0] is GenericFunction gf) {
                LispObject result = Nil.Instance;
                for (int i = gf.Methods.Count - 1; i >= 0; i--)
                    result = new Cons(gf.Methods[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("GENERIC-FUNCTION-METHODS: not a generic function", args[0]));
        }, "GENERIC-FUNCTION-METHODS", 1));

        // GENERIC-FUNCTION-NAME
        Emitter.CilAssembler.RegisterFunction("GENERIC-FUNCTION-NAME", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("GENERIC-FUNCTION-NAME: wrong arg count"));
            if (args[0] is GenericFunction gf) return gf.Name;
            throw new LispErrorException(new LispTypeError("GENERIC-FUNCTION-NAME: not a generic function", args[0]));
        }, "GENERIC-FUNCTION-NAME", 1));

        // GENERIC-FUNCTION-LAMBDA-LIST is registered by Mop.cs (returns the stored
        // lambda-list / arity placeholder). A leftover stub here returned NIL for any
        // GF and, depending on startup registration order, shadowed the real one on
        // some platforms (Linux) but not others (Windows) — making
        // generic-function-lambda-list return NIL only on Linux. Removed so only
        // Mop.cs registers it.

        // METHOD-QUALIFIERS is already registered as a proper GenericFunction above (lines ~2659-2685)

        // METHOD-SPECIALIZERS
        Emitter.CilAssembler.RegisterFunction("METHOD-SPECIALIZERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-SPECIALIZERS: wrong arg count"));
            if (args[0] is LispMethod m) {
                LispObject result = Nil.Instance;
                for (int i = m.Specializers.Length - 1; i >= 0; i--)
                    result = new Cons(m.Specializers[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("METHOD-SPECIALIZERS: not a method", args[0]));
        }, "METHOD-SPECIALIZERS", 1));

        // METHOD-GENERIC-FUNCTION — stub returning NIL (not tracked in dotcl)
        Emitter.CilAssembler.RegisterFunction("METHOD-GENERIC-FUNCTION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-GENERIC-FUNCTION: wrong arg count"));
            if (args[0] is LispMethod) return Nil.Instance;
            throw new LispErrorException(new LispTypeError("METHOD-GENERIC-FUNCTION: not a method", args[0]));
        }, "METHOD-GENERIC-FUNCTION", 1));

        // METHOD-LAMBDA-LIST — stub returning NIL
        Emitter.CilAssembler.RegisterFunction("METHOD-LAMBDA-LIST", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-LAMBDA-LIST: wrong arg count"));
            if (args[0] is LispMethod) return Nil.Instance;
            throw new LispErrorException(new LispTypeError("METHOD-LAMBDA-LIST: not a method", args[0]));
        }, "METHOD-LAMBDA-LIST", 1));

        // MAKE-METHOD-LAMBDA — stub (needed by some MOP code)
        Emitter.CilAssembler.RegisterFunction("MAKE-METHOD-LAMBDA", new LispFunction(args => {
            // (make-method-lambda gf method lambda-form env)
            if (args.Length < 3) throw new LispErrorException(new LispProgramError("MAKE-METHOD-LAMBDA: wrong arg count"));
            return args[2];  // return the lambda form as-is
        }, "MAKE-METHOD-LAMBDA"));

        // ENSURE-CLASS — create/redefine a class honoring :metaclass,
        // :direct-superclasses, and :direct-slots, then register it. Mirrors what
        // DEFCLASS expands to (%register-class (%make-class-full ...)) so a custom
        // metaclass is preserved (AMOP). Previously a stub that ignored them.
        Emitter.CilAssembler.RegisterFunction("ENSURE-CLASS", new LispFunction(args => {
            // (ensure-class name &key metaclass direct-superclasses direct-slots ...)
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("ENSURE-CLASS: wrong arg count"));
            var name = args[0];
            // Normalize name to a symbol (list names become their print-name symbol)
            Symbol nameSym = name is Symbol s ? s : Startup.Sym(name.ToString() ?? "");
            LispObject metaclassSpec = Nil.Instance, supersSpec = Nil.Instance, slotsSpec = Nil.Instance;
            var extra = new List<LispObject>();   // metaclass-slot initargs, e.g. :type-name
            for (int i = 1; i + 1 < args.Length; i += 2)
            {
                if (args[i] is not Symbol k) continue;
                if (k == Startup.Keyword("METACLASS")) metaclassSpec = args[i + 1];
                else if (k == Startup.Keyword("DIRECT-SUPERCLASSES")) supersSpec = args[i + 1];
                else if (k == Startup.Keyword("DIRECT-SLOTS")) slotsSpec = args[i + 1];
                else { extra.Add(k); extra.Add(args[i + 1]); }
            }
            // Resolve the metaclass (a class object or a class name); null => STANDARD-CLASS.
            LispClass? metaclass = metaclassSpec switch
            {
                LispClass mc => mc,
                Symbol ms => Runtime.FindClassOrNil(ms) as LispClass,
                _ => null
            };
            // Resolve superclasses: each entry is a class object or a class name.
            LispObject supersList = Nil.Instance;
            {
                var resolved = new List<LispObject>();
                for (var c = supersSpec; c is Cons cc; c = cc.Cdr)
                {
                    LispObject sup = cc.Car switch
                    {
                        LispClass sc => sc,
                        Symbol sn => Runtime.FindClassOrNil(sn),
                        _ => Nil.Instance
                    };
                    if (sup is LispClass) resolved.Add(sup);
                }
                for (int i = resolved.Count - 1; i >= 0; i--) supersList = new Cons(resolved[i], supersList);
            }
            // Convert canonical direct-slot plists (:name N :initargs (..) :initfunction fn ..)
            // to SlotDefinitions.
            LispObject slotDefsList = Nil.Instance;
            {
                var sds = new List<LispObject>();
                for (var c = slotsSpec; c is Cons cc; c = cc.Cdr)
                {
                    if (cc.Car is not Cons) continue;
                    LispObject sName = Nil.Instance, sInitargs = Nil.Instance, sInitfn = Nil.Instance;
                    for (var p = cc.Car; p is Cons pc && pc.Cdr is Cons pv; p = pv.Cdr)
                    {
                        if (pc.Car is Symbol pk)
                        {
                            if (pk == Startup.Keyword("NAME")) sName = pv.Car;
                            else if (pk == Startup.Keyword("INITARGS")) sInitargs = pv.Car;
                            else if (pk == Startup.Keyword("INITFUNCTION")) sInitfn = pv.Car;
                        }
                    }
                    if (sName is Symbol) sds.Add(Runtime.MakeSlotDef(sName, sInitargs, sInitfn));
                }
                for (int i = sds.Count - 1; i >= 0; i--) slotDefsList = new Cons(sds[i], slotDefsList);
            }
            // Pass metaclass-slot initargs (e.g. :type-name) into the class object's single
            // init so shared-initialize applies them before inherited initialize-instance
            // :after runs. RegisterClass copies ExtraSlots to the existing/forward-ref class,
            // so re-ensure / forward-ref resolution carry the initialized slots too.
            var clsObj = Runtime.MakeClassFullWithInitargs(nameSym, supersList, slotDefsList,
                                               (LispObject?)metaclass ?? Nil.Instance, extra.ToArray());
            LispObject registered = clsObj is LispClass cls ? Runtime.RegisterClass(cls) : clsObj;
            return registered;
        }, "ENSURE-CLASS"));
    }
}

