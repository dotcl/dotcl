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
            // vs DOTCL-INTERNAL:SLOT-DEFINITION registered by Startup, and dotnet:define-class
            // simple-name lookup). Only reached when there is no package-qualified entry, so
            // distinct same-named user classes — which DO get their own exact entry, see
            // FindOrForwardClass / RegisterClass — never fall through to here.
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

    /// <summary>A DEFMETHOD parameter specializer designator → the class to specialize on.
    /// Accepts everything FIND-CLASS does, plus .NET type designators: a type-name string,
    /// an already-resolved System.Type (e.g. from dotnet:resolve-type or
    /// dotnet:make-generic-type), and a symbol that names a .NET type with no CLOS class
    /// registered yet. In those cases the class is registered on the spot
    /// (EnsureDotNetTypeClass), so specializing on a .NET type no longer requires that an
    /// instance of it has been seen first, and a FullName disambiguates same-simple-name
    /// types. A name that is neither a known class nor a resolvable .NET type still fails
    /// with FIND-CLASS's error.</summary>
    public static LispObject SpecializerClass(LispObject spec)
    {
        switch (spec)
        {
            case LispClass c:
                return c;
            case LispDotNetObject dno when dno.Value is Type t:
                return EnsureDotNetTypeClass(t);
            case LispString s:
                return TryResolveDotNetType(s.Value) is Type st
                    ? EnsureDotNetTypeClass(st)
                    : throw new LispErrorException(new LispError(
                        $"DEFMETHOD: specializer \"{s.Value}\" names neither a class nor a resolvable .NET type"));
        }
        if (FindClassOrNil(spec) is LispClass known) return known;
        if (spec is Symbol sym && TryResolveDotNetType(sym.Name) is Type nt)
            return EnsureDotNetTypeClass(nt);
        return FindClass(spec);   // signals the standard "no class named X"
    }

    /// <summary>
    /// Find class or create a forward-referenced placeholder for DEFCLASS superclasses.
    /// </summary>
    public static LispObject FindOrForwardClass(LispObject name)
    {
        var sym = ToClassSymbol(name);
        if (_classRegistry.TryGetValue(sym, out var cls))
            return cls;
        // Create the forward-ref under the ORIGINAL package-qualified symbol, not the
        // bare-name-normalized one. Otherwise CL-PPCRE::SEQ and FSET::SEQ would both
        // forward-ref to one DOTCL-INTERNAL::SEQ placeholder and the later DEFCLASS
        // would shadow the earlier class. The exact entry also means later
        // same-package references hit ToClassSymbol's exact-match fast path instead of
        // normalizing. Uninterned gensyms (HomePackage == null) keep ToClassSymbol's sym.
        var key = (name is Symbol orig && orig.HomePackage != null) ? orig : sym;
        var fwd = new LispClass(key, Array.Empty<SlotDefinition>(), Array.Empty<LispClass>());
        fwd.IsForwardReferenced = true;
        _classRegistry[key] = fwd;
        return fwd;
    }

    /// <summary>Called when a class gains or loses a direct superclass, so AMOP.s
    /// ADD-DIRECT-SUBCLASS / REMOVE-DIRECT-SUBCLASS run. Installed by Mop.Init;
    /// null until then, so class registration during the bootstrap is unaffected.</summary>
    public static Action<LispObject, LispObject, bool>? DirectSubclassHook;

    /// <summary>Report the difference between a class.s old and new direct
    /// superclasses. A redefinition that drops a superclass has to report the drop,
    /// or a metaobject class keeping its own registry would keep a stale entry.</summary>
    private static void NotifyDirectSubclasses(LispClass[] oldSupers, LispClass[] newSupers,
        LispClass subclass)
    {
        var hook = DirectSubclassHook;
        if (hook == null) return;
        foreach (var super in oldSupers)
            if (System.Array.IndexOf(newSupers, super) < 0)
                hook(super, subclass, false);
        foreach (var super in newSupers)
            if (System.Array.IndexOf(oldSupers, super) < 0)
                hook(super, subclass, true);
    }

    public static LispObject RegisterClass(LispObject cls)
    {
        if (cls is not LispClass lc)
            throw new LispErrorException(new LispTypeError("REGISTER-CLASS: not a class", cls));
        // CLHS TYPE: the class name must not already name a declaration. Every
        // class-defining operator lands here — defclass, defstruct and
        // define-condition all register a class.
        CheckTypeNameAvailable(lc.Name, "DEFCLASS");
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
            NotifyDirectSubclasses(existing.DirectSuperclasses, lc.DirectSuperclasses, existing);
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
                NotifyDirectSubclasses(fwdRef.DirectSuperclasses, lc.DirectSuperclasses, fwdRef);
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
        NotifyDirectSubclasses(System.Array.Empty<LispClass>(), lc.DirectSuperclasses, lc);
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

    /// <summary>Readable display name for a .NET type, used as its CLOS class symbol.
    /// Ordinary types keep their simple name; a generic instantiation gets a closed
    /// form (List&lt;Int32&gt;, Dictionary&lt;String,Int32&gt;, nested recursively), and
    /// an open definition yields List&lt;T&gt;. Bare Type.Name is identical for every
    /// instantiation of a generic (List`1 for every List&lt;T&gt;), so distinct closed
    /// names keep the class symbol from depending on which instantiation is reflected
    /// over first. (dotcl/dotcl#50)</summary>
    internal static string DotNetTypeDisplayName(Type t)
    {
        if (!t.IsGenericType) return t.Name;
        var name = t.Name;
        int tick = name.IndexOf('`');
        if (tick >= 0) name = name.Substring(0, tick);
        var args = string.Join(",", t.GetGenericArguments().Select(DotNetTypeDisplayName));
        return name + "<" + args + ">";
    }

    private static readonly HashSet<string> _reportedTypeNameCollisions = new();

    /// <summary>Warn once when a .NET type is registered under its (ugly) assembly-
    /// qualified name because its display name is already claimed by another type, so
    /// the load-order hazard is visible rather than silent. class-for-type sidesteps
    /// the name entirely. (dotcl/dotcl#50)</summary>
    private static void WarnDisplayNameCollision(Type type, string display)
    {
        var key = type.FullName ?? display;
        lock (_reportedTypeNameCollisions)
            if (!_reportedTypeNameCollisions.Add(key)) return;
        Console.Error.WriteLine(
            $"[DOTNET WARNING] .NET type {type.FullName} shares the class name \"{display}\" " +
            "with an already-registered type; registering it under its assembly-qualified name " +
            "instead. Use (dotnet:class-for-type ...) to obtain its class object directly.");
    }

    /// <summary>Register a .NET Type as a CLOS built-in class so that class-of/type-of/find-class work.</summary>
    /// <summary>Order interfaces so that a derived interface precedes the ones it
    /// extends (IList`1 before ICollection`1 before IEnumerable`1). Sorting by how many
    /// interfaces each one itself implements gives that order, and is stable for
    /// unrelated interfaces so the declaration order survives.</summary>
    private static Type[] OrderInterfacesMostDerivedFirst(Type[] interfaces)
    {
        var ordered = (Type[])interfaces.Clone();
        // OrderByDescending is a stable sort in LINQ, unlike Array.Sort.
        return ordered.OrderByDescending(i => i.GetInterfaces().Length).ToArray();
    }

    /// <summary>Class precedence list for a .NET type's class: the type itself, then the
    /// concrete chain, then interfaces (this type's first, then any the base chain
    /// contributes), then the tail the base chain ended with (T). Keeping interfaces
    /// behind every concrete class is what makes a method on a concrete .NET class win
    /// over one on an interface it implements.</summary>
    private static LispClass[] BuildDotNetCpl(LispClass cls, LispClass baseCls,
                                              List<LispClass> ifaceClasses, LispClass? openCls)
    {
        var tCls = _classRegistry.TryGetValue(Startup.Sym("T"), out var t) ? t : null;
        var concrete = new List<LispClass>();
        var baseIfaces = new List<LispClass>();
        var tail = new List<LispClass>();
        foreach (var c in baseCls.ClassPrecedenceList)
        {
            if (tail.Count > 0 || ReferenceEquals(c, tCls)) tail.Add(c);
            else if (c.IsDotNetInterface) baseIfaces.Add(c);
            else concrete.Add(c);
        }
        var cpl = new List<LispClass> { cls };
        // The open generic definition (List<T>) sits immediately behind its own closed
        // instantiation, so a wildcard method on List<T> loses to one on List<Int32> but
        // still beats anything inherited. Each level of a generic base chain keeps that
        // pairing, because the base's own CPL already carries its open form.
        if (openCls != null && !cpl.Contains(openCls)) cpl.Add(openCls);
        foreach (var c in concrete) if (!cpl.Contains(c)) cpl.Add(c);
        foreach (var c in ifaceClasses) if (!cpl.Contains(c)) cpl.Add(c);
        foreach (var c in baseIfaces) if (!cpl.Contains(c)) cpl.Add(c);
        foreach (var c in tail) if (!cpl.Contains(c)) cpl.Add(c);
        return cpl.ToArray();
    }

    public static LispClass EnsureDotNetTypeClass(Type type)
    {
        // An open-constructed type (List<T>.GetInterfaces() yields IList<T> with the
        // declaring type's own parameter) has no FullName and is not a type any object
        // can be an instance of. Collapse it to its generic definition, which is the
        // wildcard class users specialize on — otherwise it would register a second,
        // nameless class under the same display name and warn about the collision.
        if (type.IsConstructedGenericType && type.ContainsGenericParameters)
            type = type.GetGenericTypeDefinition();
        if (_dotNetTypeRegistry.TryGetValue(type, out var existing)) return existing;
        // Prefer a readable display name for the class symbol (friendly, and what
        // unquoted Lisp symbols resolve to): the simple name for an ordinary type, a
        // closed form (List<Int32>) for a generic instantiation. Names can still be
        // ambiguous across namespaces, so guard against collisions below.
        var display = DotNetTypeDisplayName(type);
        var simpleSym = Startup.Sym(display);
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
        if (!simpleFree)
            WarnDisplayNameCollision(type, display);

        // Interfaces are superclasses too, so a method specialized on IEnumerable
        // applies to every implementor. They rank below the whole concrete chain:
        // the precedence list is concrete classes, then interfaces (most derived
        // first), then the T tail — dotcl's T standing for the object/lisp tail.
        var ifaceClasses = new List<LispClass>();
        foreach (var itf in OrderInterfacesMostDerivedFirst(type.GetInterfaces()))
        {
            ifaceClasses.Add(EnsureDotNetTypeClass(itf));
            // A closed generic interface is followed by its open definition, so
            // (defmethod f ((x (dotnet:resolve-type "...IEnumerable`1")))) applies to
            // every instantiation while a method on IEnumerable<Int32> still wins.
            if (itf.IsConstructedGenericType)
                ifaceClasses.Add(EnsureDotNetTypeClass(itf.GetGenericTypeDefinition()));
        }

        // Same for the type itself: List<Int32> gets List<T> as a wildcard superclass.
        LispClass? openCls = type.IsConstructedGenericType
            ? EnsureDotNetTypeClass(type.GetGenericTypeDefinition())
            : null;

        // Direct superclasses: the base class plus the interfaces this type introduces
        // itself (the ones its base class does not already implement).
        var inheritedIfaces = type.BaseType?.GetInterfaces() ?? Type.EmptyTypes;
        var directSupers = new List<LispClass> { baseCls };
        if (openCls != null) directSupers.Add(openCls);
        foreach (var itf in OrderInterfacesMostDerivedFirst(type.GetInterfaces()))
            if (Array.IndexOf(inheritedIfaces, itf) < 0)
            {
                var ic = EnsureDotNetTypeClass(itf);
                if (!directSupers.Contains(ic)) directSupers.Add(ic);
            }

        var cls = new LispClass(classSym, Array.Empty<SlotDefinition>(), directSupers.ToArray());
        cls.IsDotNetInterface = type.IsInterface;
        cls.DotNetType = type;
        cls.ClassPrecedenceList = BuildDotNetCpl(cls, baseCls, ifaceClasses, openCls);
        cls.EffectiveSlots = Array.Empty<SlotDefinition>();
        cls.IsBuiltIn = true;
        _dotNetTypeRegistry[type] = cls;
        if (simpleFree)
        {
            _classRegistry[simpleSym] = cls;
            // Also register under the uppercase name so unquoted Lisp symbols
            // (e.g. (ClsAnimal) read as CLSANIMAL) find the class correctly.
            var upperSym = Startup.Sym(display.ToUpperInvariant());
            if (!ReferenceEquals(upperSym, simpleSym) && !_classRegistry.ContainsKey(upperSym))
                _classRegistry[upperSym] = cls;
            // Always alias the class under its FullName too, so the FullName is a
            // deterministic, load-order-independent specializer for EVERY type —
            // not just the loser of a same-simple-name collision (whose FullName is
            // registered in the else branch below). A code generator can then emit
            // the FullName specializer for any type and have it resolve, without
            // depending on which same-named type won the simple-name slot.
            // (dotcl/dotcl#50). The simple name stays first-wins as an ambiguous
            // interactive convenience.
            var fullName = type.FullName
                ?? (type.Namespace is null ? type.Name : type.Namespace + "." + type.Name);
            var fullSym = Startup.Sym(fullName);
            if (!ReferenceEquals(fullSym, simpleSym) && !_classRegistry.ContainsKey(fullSym))
                _classRegistry[fullSym] = cls;
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

    /// <summary>The classes that name C as a direct superclass, as a list.
    /// Not maintained as a back-link; scanning the registry is cheap enough for
    /// occasional MOP introspection. Shared by CL:CLASS-DIRECT-SUBCLASSES and
    /// DOTCL-MOP:CLASS-DIRECT-SUBCLASSES so the two cannot drift apart — the
    /// argument check lives here for the same reason: with it at the call sites,
    /// one returned NIL for a non-class and the other signalled, and which one
    /// answered depended on the platform's registration order.</summary>
    /// <summary>The class argument of a MOP class accessor. AMOP (and SBCL) make a
    /// non-class here a type error; the DOTCL-MOP registrations used to return NIL
    /// instead, so the answer depended on which of the two same-named functions the
    /// caller reached.</summary>
    private static LispClass RequireClass(string who, LispObject arg)
        => arg as LispClass ?? throw new LispErrorException(new LispTypeError(
            $"{who}: not a class", arg, Startup.Sym("CLASS")));

    private static LispObject ToLispList(LispObject[]? items)
    {
        LispObject result = Nil.Instance;
        if (items != null)
            for (int i = items.Length - 1; i >= 0; i--)
                result = new Cons(items[i], result);
        return result;
    }

    public static LispObject ClassDirectSlots(LispObject arg)
        => ToLispList(RequireClass("CLASS-DIRECT-SLOTS", arg).DirectSlots);

    public static LispObject ClassSlots(LispObject arg)
        => ToLispList(RequireClass("CLASS-SLOTS", arg).EffectiveSlots);

    public static LispObject ClassDirectSuperclasses(LispObject arg)
        => ToLispList(RequireClass("CLASS-DIRECT-SUPERCLASSES", arg).DirectSuperclasses);

    /// <summary>The CPL, falling back to (C) for a class whose list has not been
    /// computed — without that, DOTCL-MOP's version dereferenced a null array.</summary>
    public static LispObject ClassPrecedenceListOf(LispObject arg)
    {
        var c = RequireClass("CLASS-PRECEDENCE-LIST", arg);
        var cpl = c.ClassPrecedenceList;
        if (cpl == null || cpl.Length == 0) return new Cons(c, Nil.Instance);
        return ToLispList(cpl);
    }

    /// <summary>dotcl finalizes eagerly during DEFCLASS, so every class that exists
    /// is finalized — except a forward-referenced one, which by definition is not.
    /// The CL-side registration answered T even for those.</summary>
    public static LispObject ClassFinalizedP(LispObject arg)
        => RequireClass("CLASS-FINALIZED-P", arg).IsForwardReferenced ? Nil.Instance : T.Instance;

    /// <summary>The memoized prototype instance (stable identity, which EQL-method
    /// dispatch on a prototype depends on). Built-in classes have no instance to
    /// hand out.</summary>
    public static LispObject ClassPrototypeOf(LispObject arg)
    {
        var c = RequireClass("CLASS-PROTOTYPE", arg);
        if (c.IsBuiltIn)
            throw new LispErrorException(new LispError(
                "CLASS-PROTOTYPE: cannot create prototype for built-in class"));
        return c.Prototype;
    }

    private static GenericFunction RequireGf(string who, LispObject arg)
        => arg as GenericFunction ?? throw new LispErrorException(new LispTypeError(
            $"{who}: not a generic function", arg, Startup.Sym("GENERIC-FUNCTION")));

    public static LispObject GenericFunctionMethods(LispObject arg)
        => ToLispList(RequireGf("GENERIC-FUNCTION-METHODS", arg).Methods.ToArray());

    public static LispObject GenericFunctionName(LispObject arg)
        => RequireGf("GENERIC-FUNCTION-NAME", arg).Name;

    private static SlotDefinition RequireSlotDef(string who, LispObject arg)
        => arg as SlotDefinition ?? throw new LispErrorException(new LispTypeError(
            $"{who}: not a slot definition", arg, Startup.Sym("SLOT-DEFINITION")));

    public static LispObject SlotDefinitionName(LispObject arg)
        => RequireSlotDef("SLOT-DEFINITION-NAME", arg).Name;

    public static LispObject SlotDefinitionInitargs(LispObject arg)
        => ToLispList(RequireSlotDef("SLOT-DEFINITION-INITARGS", arg).Initargs);

    public static LispObject SlotDefinitionInitfunction(LispObject arg)
    {
        var sd = RequireSlotDef("SLOT-DEFINITION-INITFUNCTION", arg);
        return sd.InitformThunk is { } f ? f : Nil.Instance;
    }

    public static LispObject SlotDefinitionAllocation(LispObject arg)
        => RequireSlotDef("SLOT-DEFINITION-ALLOCATION", arg).AllocationKeyword;

    /// <summary>The declared :type, T when the slot did not name one.</summary>
    public static LispObject SlotDefinitionType(LispObject arg)
        => RequireSlotDef("SLOT-DEFINITION-TYPE", arg).SlotType;

    /// <summary>The :initform as source, NIL when the slot has none.</summary>
    public static LispObject SlotDefinitionInitform(LispObject arg)
        => RequireSlotDef("SLOT-DEFINITION-INITFORM", arg).Initform;

    /// <summary>AMOP puts readers/writers on DIRECT slot definitions; an effective
    /// slot definition has none, which is what the empty array gives.</summary>
    public static LispObject SlotDefinitionReaders(LispObject arg)
        => ToLispList(RequireSlotDef("SLOT-DEFINITION-READERS", arg).Readers);

    public static LispObject SlotDefinitionWriters(LispObject arg)
        => ToLispList(RequireSlotDef("SLOT-DEFINITION-WRITERS", arg).Writers);

    /// <summary>The generic function a method is attached to, NIL while it is
    /// unattached. Shared by CL:METHOD-GENERIC-FUNCTION and the DOTCL-MOP one.</summary>
    public static LispObject MethodGenericFunction(LispObject arg)
    {
        if (arg is not LispMethod m)
            throw new LispErrorException(new LispTypeError(
                "METHOD-GENERIC-FUNCTION: not a method", arg, Startup.Sym("METHOD")));
        return m.Owner is { } gf ? gf : Nil.Instance;
    }

    /// <summary>A method's lambda list. dotcl does not keep the source list, so
    /// this rebuilds one of the right shape from the recorded arity. Shared by
    /// CL:METHOD-LAMBDA-LIST and the DOTCL-MOP one.</summary>
    public static LispObject MethodLambdaList(LispObject arg)
    {
        if (arg is not LispMethod m)
            throw new LispErrorException(new LispTypeError(
                "METHOD-LAMBDA-LIST: not a method", arg, Startup.Sym("METHOD")));
        if (m.StoredLambdaList is { } given) return given;
        return m.PlaceholderLambdaList ??= Mop.BuildLambdaListPlaceholder(
            m.RequiredCount, m.OptionalCount,
            m.HasRest, m.HasKey, m.KeywordNames, m.HasAllowOtherKeys);
    }

    public static LispObject ClassDirectSubclasses(LispObject arg)
    {
        if (arg is not LispClass c)
            throw new LispErrorException(new LispTypeError(
                "CLASS-DIRECT-SUBCLASSES: not a class", arg, Startup.Sym("CLASS")));
        var subs = new List<LispObject>();
        foreach (var cls in AllClasses())
            if (Array.IndexOf(cls.DirectSuperclasses, c) >= 0)
                subs.Add(cls);
        return List(subs.ToArray());
    }

    /// <summary>Apply the generic function initargs AMOP names. Shared by
    /// initialize-instance and reinitialize-instance so the two cannot drift.
    /// RENAMING says whether :NAME may replace a name that is already there:
    /// initialization only fills in an unnamed one, while (SETF GENERIC-FUNCTION-NAME)
    /// exists precisely to change it.</summary>
    internal static void ApplyGenericFunctionInitargs(GenericFunction gf, LispObject[] args,
        bool renaming)
    {
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is not Symbol ks) continue;
            if (ks.Name == "LAMBDA-LIST")
                ParseLambdaListIntoGF(gf, args[i + 1]);
            else if (ks.Name == "DECLARATIONS")
                gf.Declarations = args[i + 1];
            else if (ks.Name == "NAME" && args[i + 1] is Symbol ns
                     && (renaming || gf.Name.Name == "UNNAMED"))
            {
                gf.Name = ns;
                ns.Function = gf;
                Runtime.RegisterGF(ns, gf);
            }
        }
    }

    /// <summary>The three generic functions the invocation protocol goes through, with
    /// the default method each was registered with. Set by Mop.Init; null before that.
    /// Identity is what decides whether a generic function has been customized -- the
    /// question is not whether a method exists but whether one applies to THIS generic
    /// function. Asking the weaker question makes one user's specialisation change how
    /// every generic function in the image is dispatched.</summary>
    internal static (GenericFunction Gf, LispMethod Default)[]? InvocationProtocolGfs;

    /// <summary>True once any method has been added to one of them. Until then every
    /// generic function keeps the ordinary path, and the arity fast paths never even
    /// build an argument array to ask.</summary>
    internal static volatile bool AnyInvocationProtocolCustomized;

    /// <summary>Called when a method is added to or removed from one of the protocol
    /// generic functions: every generic function has to ask again, and every cached
    /// dispatch built under the old answer is stale.</summary>
    internal static void InvocationProtocolChanged()
    {
        AnyInvocationProtocolCustomized = true;
        foreach (var gf in AllGenericFunctions())
        {
            gf.UsesInvocationProtocol = null;
            gf.InvalidateCache();
        }
    }

    /// <summary>A method was added to or removed from GF: if GF is one of the protocol
    /// generic functions, every other generic function's answer is stale.</summary>
    private static void NoteInvocationProtocolMethodChange(GenericFunction gf)
    {
        if (InvocationProtocolGfs is not { } protocols) return;
        foreach (var (protocolGf, _) in protocols)
            if (ReferenceEquals(gf, protocolGf)) { InvocationProtocolChanged(); return; }
    }

    /// <summary>Whether this generic function's calls go through the protocol. Asked
    /// once per generic function and remembered.</summary>
    internal static bool UsesInvocationProtocol(GenericFunction gf)
    {
        if (!AnyInvocationProtocolCustomized || InvocationProtocolGfs is not { } protocols)
            return false;
        if (gf.UsesInvocationProtocol is bool known) return known;
        bool customized = false;
        foreach (var (protocolGf, defaultMethod) in protocols)
        {
            if (protocolGf.Methods.Count <= 1) continue;
            // The protocol generic functions themselves keep the ordinary path: asking
            // one of them how to dispatch would need it to dispatch first.
            if (ReferenceEquals(gf, protocolGf)) { gf.UsesInvocationProtocol = false; return false; }
            // The argument list has to be as long as the protocol generic function's
            // required parameters, or nothing is applicable and the answer is a
            // silent no: COMPUTE-EFFECTIVE-METHOD takes three.
            int required = Math.Max(1, protocolGf.RequiredCount);
            var probeArgs = new LispObject[required];
            probeArgs[0] = gf;
            for (int i = 1; i < required; i++) probeArgs[i] = Nil.Instance;
            var applicable = ComputeApplicableMethods(protocolGf, List(probeArgs));
            if (applicable is Cons c && !ReferenceEquals(c.Car, defaultMethod))
            { customized = true; break; }
        }
        gf.UsesInvocationProtocol = customized;
        return customized;
    }

    /// <summary>The effective method for these arguments, obtained the AMOP way:
    /// COMPUTE-APPLICABLE-METHODS, then COMPUTE-EFFECTIVE-METHOD, then
    /// COMPUTE-EFFECTIVE-METHOD-FUNCTION. Returns null when the protocol cannot answer,
    /// in which case the caller falls back to dotcl's own dispatch.</summary>
    private static LispFunction? EffectiveMethodThroughProtocol(GenericFunction gf,
                                                                LispObject[] args)
    {
        if (Startup.Sym("COMPUTE-APPLICABLE-METHODS").Function is not LispFunction cam)
            return null;
        var (cemSym, cemStatus) = Mop.MopPkg.FindSymbol("COMPUTE-EFFECTIVE-METHOD");
        var (cemfSym, cemfStatus) = Mop.MopPkg.FindSymbol("COMPUTE-EFFECTIVE-METHOD-FUNCTION");
        if (cemStatus == SymbolStatus.None || cemfStatus == SymbolStatus.None) return null;
        if (cemSym.Function is not LispFunction cem || cemfSym.Function is not LispFunction cemf)
            return null;
        // AMOP asks COMPUTE-APPLICABLE-METHODS-USING-CLASSES first and only falls back
        // when its second value says the answer is not definitive. The cache here is
        // keyed by argument class, so the class-based question is the one that matches
        // what is being stored.
        LispObject methods = Nil.Instance;
        var (camucSym, camucStatus) = Mop.MopPkg.FindSymbol("COMPUTE-APPLICABLE-METHODS-USING-CLASSES");
        if (camucStatus != SymbolStatus.None && camucSym.Function is LispFunction camuc)
        {
            var classes = new LispObject[Math.Max(0, Math.Min(args.Length, gf.RequiredCount))];
            for (int i = 0; i < classes.Length; i++)
                classes[i] = (LispObject?)ArgDispatchClass(args[i]) ?? Nil.Instance;
            var answer = camuc.Invoke(new LispObject[] { gf, List(classes) });
            var primary = MultipleValues.Primary(answer);
            var values = MultipleValues.Get();
            bool definitive = values.Length > 1 && values[1] is not Nil;
            if (definitive) methods = primary;
        }
        if (methods is Nil)
            methods = MultipleValues.Primary(cam.Invoke(new LispObject[] { gf, List(args) }));
        if (methods is Nil) return null;
        var combination = gf.MethodCombination ?? Startup.Sym("STANDARD");
        var form = MultipleValues.Primary(cem.Invoke(new LispObject[] { gf, combination, methods }));
        return MultipleValues.Primary(cemf.Invoke(new LispObject[] { gf, form, Nil.Instance }))
               as LispFunction;
    }

    /// <summary>The dependents AMOP's dependent protocol keeps per metaobject. A weak
    /// table rather than a field on each metaobject class: dependents are rare, the
    /// protocol applies to any metaobject, and a metaobject that is dropped should not
    /// be held alive by a list nobody asked about.</summary>
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispObject,
        List<LispObject>> _dependents = new();

    /// <summary>The initargs of a reinitialization, without the metaobject the method
    /// received as its first argument: UPDATE-DEPENDENT is told what changed, not who.
    /// Copied out by hand rather than sliced: `args[1..]` lowers to a call to
    /// RuntimeHelpers.GetSubArray, which the netstandard2.0 target does not have.</summary>
    private static LispObject[] SkipInstanceArg(LispObject[] args)
    {
        if (args.Length <= 1) return Array.Empty<LispObject>();
        var rest = new LispObject[args.Length - 1];
        Array.Copy(args, 1, rest, 0, rest.Length);
        return rest;
    }

    public static LispObject AddDependent(LispObject metaobject, LispObject dependent)
    {
        var list = _dependents.GetOrCreateValue(metaobject);
        lock (list)
            if (!list.Any(d => ReferenceEquals(d, dependent))) list.Add(dependent);
        return metaobject;
    }

    public static LispObject RemoveDependent(LispObject metaobject, LispObject dependent)
    {
        if (_dependents.TryGetValue(metaobject, out var list))
            lock (list) list.RemoveAll(d => ReferenceEquals(d, dependent));
        return metaobject;
    }

    public static LispObject MapDependents(LispObject metaobject, LispObject function)
    {
        if (function is LispFunction fn && _dependents.TryGetValue(metaobject, out var list))
        {
            LispObject[] snapshot;
            lock (list) snapshot = list.ToArray();
            foreach (var dependent in snapshot) fn.Invoke(new[] { dependent });
        }
        return Nil.Instance;
    }

    /// <summary>Tell a metaobject's dependents that it changed, as AMOP requires after
    /// reinitialization and after adding or removing a method. Nothing happens, and no
    /// generic function is called, until something has been registered as a dependent.</summary>
    internal static void NotifyDependents(LispObject metaobject, params LispObject[] initargs)
    {
        if (!_dependents.TryGetValue(metaobject, out var list)) return;
        LispObject[] snapshot;
        lock (list) snapshot = list.ToArray();
        if (snapshot.Length == 0) return;
        if (Startup.Sym("UPDATE-DEPENDENT").Function is not LispFunction update)
        {
            var (mopSym, status) = Mop.MopPkg.FindSymbol("UPDATE-DEPENDENT");
            if (status == SymbolStatus.None || mopSym.Function is not LispFunction mopUpdate) return;
            update = mopUpdate;
        }
        foreach (var dependent in snapshot)
        {
            var args = new LispObject[2 + initargs.Length];
            args[0] = metaobject;
            args[1] = dependent;
            Array.Copy(initargs, 0, args, 2, initargs.Length);
            update.Invoke(args);
        }
    }

    /// <summary>Called when DEFCLASS creates an accessor method, so AMOP's
    /// READER-METHOD-CLASS / WRITER-METHOD-CLASS run. Installed by Mop.Init; null
    /// until then. The bool says whether the accessor reads.</summary>
    public static Action<LispClass, SlotDefinition, bool>? AccessorMethodClassHook;

    /// <summary>Called while a class is being finalized, so AMOP's
    /// COMPUTE-DEFAULT-INITARGS runs. Installed by Mop.Init; null until then.</summary>
    public static Action<LispClass>? DefaultInitargsHook;

    /// <summary>Called while a class is being finalized, so AMOP's COMPUTE-SLOTS runs
    /// and the list it returns becomes the class's effective slots. Returns null to
    /// leave the class's own answer alone, which is what happens until a metaclass
    /// specialises the protocol. Installed by Mop.Init.</summary>
    public static Func<LispClass, SlotDefinition[]?>? ComputeSlotsHook;

    /// <summary>Called when a generic function.s methods or initialization change,
    /// so AMOP.s COMPUTE-DISCRIMINATING-FUNCTION runs and its answer is installed.
    /// Installed by Mop.Init; null until then.</summary>
    public static Action<GenericFunction>? DiscriminatingFunctionHook;


    /// <summary>Called by DEFMETHOD so AMOP's GENERIC-FUNCTION-METHOD-CLASS runs and
    /// the method it names becomes the method's class. Installed by Mop.Init; null
    /// until then, and it answers null while nobody has specialised the protocol, so
    /// an ordinary DEFMETHOD pays nothing.</summary>
    public static Func<GenericFunction, LispClass?>? MethodClassHook;

    /// <summary>Called by DEFGENERIC so AMOP's FIND-METHOD-COMBINATION runs. The
    /// combination dotcl uses is still decided from the symbol; what this adds is the
    /// call, which is the hook a metaobject class specialises. Installed by Mop.Init.</summary>
    public static Action<GenericFunction, LispObject, LispObject>? MethodCombinationHook;

    /// <summary>Called by DEFMETHOD, at macroexpansion time, so AMOP's
    /// MAKE-METHOD-LAMBDA processes the method lambda and the processed one is what
    /// gets compiled. Returns null to leave the lambda alone, which is the answer
    /// while nobody has specialised the protocol. Installed by Mop.Init.</summary>
    public static Func<GenericFunction, LispObject, LispObject?>? MakeMethodLambdaHook;

    /// <summary>DEFMETHOD: hand the method lambda to MAKE-METHOD-LAMBDA and take back
    /// what it returns. The generic function has to exist already for there to be a
    /// class to ask -- a DEFMETHOD that creates one gets the unprocessed lambda, the
    /// same fallback every implementation makes here.</summary>
    public static LispObject MakeMethodLambdaFor(LispObject name, LispObject lambdaForm)
    {
        if (MakeMethodLambdaHook is not { } hook) return lambdaForm;
        if (FindGF(name) is not GenericFunction gf) return lambdaForm;
        return hook(gf, lambdaForm) ?? lambdaForm;
    }

    /// <summary>DEFMETHOD: ask the generic function what class its methods are, and
    /// record the answer on the method. A generic function whose class does not
    /// specialise the protocol answers nothing and the method stays a STANDARD-METHOD.</summary>
    public static LispObject NoteMethodClass(LispObject gfObj, LispObject methodObj)
    {
        if (gfObj is GenericFunction gf && methodObj is LispMethod method
            && MethodClassHook is { } hook && hook(gf) is { } cls)
        {
            method.MetaClass = cls;
            // A method is an instance of its method class, so the slots that class
            // adds get the initialization protocol run on them -- initforms, and any
            // INITIALIZE-INSTANCE :after the user wrote for the method class. DEFMETHOD
            // passes no initargs for those slots, so the initforms are what fill them.
            // Only reached once a generic function class has specialised
            // GENERIC-FUNCTION-METHOD-CLASS, so an ordinary DEFMETHOD pays nothing.
            if (Startup.Sym("INITIALIZE-INSTANCE").Function is LispFunction iiFn)
                iiFn.Invoke(new LispObject[] { method });
        }
        return methodObj;
    }

    /// <summary>DEFGENERIC: ask the generic function for the method combination
    /// metaobject named by its :METHOD-COMBINATION option.</summary>
    public static LispObject NoteMethodCombination(LispObject gfObj, LispObject name,
                                                   LispObject options)
    {
        if (gfObj is GenericFunction gf && MethodCombinationHook is { } hook)
            hook(gf, name, options);
        return gfObj;
    }

    private static void NotifyDiscriminatingFunction(GenericFunction gf)
        => DiscriminatingFunctionHook?.Invoke(gf);

    /// <summary>dotcl.s standard dispatch as a function object, for
    /// COMPUTE-DISCRIMINATING-FUNCTION to return.</summary>
    public static LispFunction StandardDiscriminatingFunction(GenericFunction gf)
        => new LispFunction(args => DispatchGFCore(gf, args),
            $"discriminating function for {gf.Name.Name}", -1);

    /// <summary>Called for each specializer of a method being added to or removed
    /// from a generic function, so AMOP.s ADD-DIRECT-METHOD / REMOVE-DIRECT-METHOD
    /// run. Installed by Mop.Init; null until then, which is what keeps the
    /// bootstrap (which adds methods before the MOP package exists) working.</summary>
    public static Action<LispObject, LispObject, bool>? DirectMethodHook;

    private static void NotifyDirectMethod(LispMethod method, bool adding)
    {
        var hook = DirectMethodHook;
        if (hook == null) return;
        foreach (var specializer in method.Specializers)
            hook(specializer, method, adding);
    }

    /// <summary>Iterate all registered generic functions. Used by
    /// DOTCL-MOP:SPECIALIZER-DIRECT-METHODS / SPECIALIZER-DIRECT-GENERIC-FUNCTIONS
    /// (no specializer→method back-link is maintained, so we scan).</summary>
    public static IEnumerable<GenericFunction> AllGenericFunctions() => _gfRegistry.Values;

    public static void InternClassByName(string name, LispObject cls)
    {
        if (cls is LispClass lc)
            _classRegistry[Startup.Sym(name)] = lc;
        // If not a LispClass (e.g. NIL to remove), ignore for now
    }

    public static LispObject MakeClass(LispObject name, LispObject supersList, LispObject slotDefsList)
        => MakeClassCore(name, supersList, slotDefsList, null);

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
                if (sName is Symbol) sds.Add(MakeSlotDef(sName, sInitargs, sInitfn));
            }
            for (int i = sds.Count - 1; i >= 0; i--) slotDefsList = new Cons(sds[i], slotDefsList);
        }
        return (supersList, slotDefsList);
    }

    /// <summary>MAKE-INSTANCE on a class metaobject class makes a class (AMOP). The
    /// result is not registered under a name: an anonymous class is reachable only
    /// through the object, which is the point of it. A :NAME initarg still names it,
    /// but naming is what ENSURE-CLASS is for.</summary>
    internal static LispObject MakeClassMetaobject(LispClass metaclass, LispObject[] initargs)
    {
        LispObject supersSpec = Nil.Instance, slotsSpec = Nil.Instance;
        Symbol nameSym = Startup.Sym("NIL");
        for (int i = 0; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is not Symbol k) continue;
            if (k == Startup.Keyword("DIRECT-SUPERCLASSES")) supersSpec = initargs[i + 1];
            else if (k == Startup.Keyword("DIRECT-SLOTS")) slotsSpec = initargs[i + 1];
            else if (k == Startup.Keyword("NAME") && initargs[i + 1] is Symbol ns) nameSym = ns;
        }
        // AMOP: with no direct superclasses, a standard class gets STANDARD-OBJECT.
        if (supersSpec is Nil && FindClassOrNil(Startup.Sym("STANDARD-OBJECT")) is LispClass stdObj)
            supersSpec = new Cons(stdObj, Nil.Instance);
        var (supers, slotDefs) = ParseClassInitargs(supersSpec, slotsSpec);
        return MakeClassFull(nameSym, supers, slotDefs, metaclass);
    }

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
        // Default to STANDARD-OBJECT if no supers -- except under
        // FUNCALLABLE-STANDARD-CLASS, where AMOP names FUNCALLABLE-STANDARD-OBJECT.
        // A class whose instances are callable is not a STANDARD-OBJECT.
        if (supers.Count == 0)
        {
            bool funcallable = metaclass != null
                && metaclass.ClassPrecedenceList.Any(m => m.Name.Name == "FUNCALLABLE-STANDARD-CLASS");
            var defaultSuper = Startup.Sym(funcallable ? "FUNCALLABLE-STANDARD-OBJECT"
                                                       : "STANDARD-OBJECT");
            if (_classRegistry.TryGetValue(defaultSuper, out var stdObj))
                supers.Add(stdObj);
        }

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
        }
        return slotdObj;
    }

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
        // Default initargs disqualify the ultra-fast make-instance path
        if (cls.DefaultInitargs.Length > 0)
            cls.HasSimpleInitialization = false;
        cls.SimpleInitChecked = false;
        cls.SharedInitSimpleChecked = false;
        return classObj;
    }

    public static LispObject ClassOf(LispObject obj)
    {
        if (obj is LispInstance inst)
            return inst.Class;
        if (obj is LispInstanceCondition lic)
            return lic.Instance.Class;
        // Native (runtime-signaled) condition objects — e.g. the DIVISION-BY-ZERO
        // from (/ 1 0), the TYPE-ERROR from (car 3) — carry their class name in
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
            if (lc.IsForwardReferenced
                && _classRegistry.TryGetValue(Startup.Sym("FORWARD-REFERENCED-CLASS"),
                                              out var fwdMeta))
                return fwdMeta;
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
        // A structure-class instance must be a LispStruct, not a CLOS LispInstance:
        // equalp compares two LispStructs slot-by-slot and returns NIL if either side
        // is a LispInstance, and make-load-form-saving-slots emits allocate-instance as
        // a struct's creation form — so an allocate-instance'd struct that round-trips
        // through a fasl would otherwise never be equalp to a normally-built one (broke
        // Coalton's equalp-on-KIND type checking).
        //
        // Slots are left NIL (the dotcl stand-in for unbound): allocate-instance must
        // NOT run slot initforms (that is initialize-instance's job). Running them broke
        // the common required-slot idiom (id (required 'id) :read-only t), where the
        // initform signals — so allocate-instance, and thus the make-load-form-saving-slots
        // creation form, errored. The INIT form restores real slot values on the round-trip.
        if (lc.IsStructureClass)
        {
            var slots = new LispObject[lc.EffectiveSlots.Length];
            for (int i = 0; i < slots.Length; i++) slots[i] = Nil.Instance;
            return new LispStruct(lc.Name, slots);
        }
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

    // Native (runtime-signaled) conditions — e.g. the DIVISION-BY-ZERO from (/ 1 0),
    // the TYPE-ERROR from (car 3), the SIMPLE-ERROR from (error "x") — are LispCondition
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
                return csm.Invoke(new LispObject[] { ccls ?? (LispObject)Nil.Instance, cond,
                    slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-VALUE") });
            throw new LispErrorException(new LispError(
                $"SLOT-VALUE: no slot named {name} in condition {cond.ConditionTypeName}"));
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
                csm.Invoke(new LispObject[] { ccls ?? (LispObject)Nil.Instance, cond,
                    slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SETF"), value });
                return value;
            }
            throw new LispErrorException(new LispError(
                $"SET-SLOT-VALUE: no slot named {name} in condition {cond.ConditionTypeName}"));
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

    /// <summary>Item3b: compile-time-inlined fast path for a call to a simple
    /// slot-reader accessor GF, backed by a per-call-site monomorphic inline cache
    /// (<paramref name="cell"/>, baked once by the assembler). On a hit — same class as
    /// last time and the method-system epoch unchanged — the slot is read straight from
    /// the instance vector, with no GF resolution, no dispatch, and no name→index lookup.
    /// A miss re-resolves the accessor and refills the cell; anything that isn't a plain
    /// instance-allocated simple reader (redefined/extended accessor, custom metaclass,
    /// class-allocated or unbound slot) falls through to a normal 1-arg invocation, so
    /// semantics are identical to calling the GF directly.</summary>
    public static LispObject ReaderIC(LispObject obj, ReaderCache cell)
    {
        if (obj is LispInstance inst)
        {
            var e = cell.E;
            if (e != null && e.Epoch == GenericFunction.MethodEpoch
                && ReferenceEquals(inst.Class, e.Cls))
            {
                var v = inst.Slots[e.Idx];
                if (v != null) return v;   // bound slot (bound NIL is non-null) — hot path
                // unbound slot: fall to full path for the SLOT-UNBOUND protocol
            }
            else if (Emitter.CilAssembler.GetFunctionBySymbol(cell.Sym) is GenericFunction gf
                     && gf.SimpleReaderSlot is { } s && inst.Class.Metaclass == null
                     && inst.Class.SlotIndex.TryGetValue(s.Name, out int idx))
            {
                // Only instance-allocated slots are cacheable as a direct Slots[idx] read;
                // :class-allocation lives on the owner class, so serve it without caching.
                if (!inst.Class.EffectiveSlots[idx].IsClassAllocation)
                    cell.E = new ReaderCache.Entry(inst.Class, idx, GenericFunction.MethodEpoch);
                return SlotValueDirect(inst, idx, s, s.Name);
            }
        }
        return Emitter.CilAssembler.GetFunctionBySymbol(cell.Sym).Invoke(new LispObject[] { obj });
    }

    /// <summary>Item3c: the writer twin of <see cref="ReaderIC"/> — the compile-time-inlined
    /// fast path for <c>(setf (accessor obj) newval)</c>, backed by a per-call-site
    /// monomorphic inline cache. Arguments keep the (SETF name) generic function's own
    /// order (new value first, object second) so the call site evaluates its argument forms
    /// in exactly the order the normal call would. On a hit the slot is written straight
    /// into the instance vector; a miss re-resolves the writer, and anything that is not a
    /// plain instance-allocated simple writer (extended accessor, custom metaclass,
    /// class-allocated slot, newval-specialized method) falls through to a normal 2-arg
    /// invocation, so semantics are identical to calling the GF.</summary>
    public static LispObject WriterIC(LispObject newval, LispObject obj, WriterCache cell)
    {
        if (obj is LispInstance inst)
        {
            var e = cell.E;
            if (e != null && e.Epoch == GenericFunction.MethodEpoch
                && ReferenceEquals(inst.Class, e.Cls))
            {
                inst.Slots[e.Idx] = newval;   // instance-allocated by construction of the entry
                return newval;
            }
            if (Emitter.CilAssembler.GetSetfFunctionBySymbol(cell.Sym) is GenericFunction gf
                && gf.SimpleWriterSlot is { } s && inst.Class.Metaclass == null
                && inst.Class.SlotIndex.TryGetValue(s.Name, out int idx))
            {
                // Only instance-allocated slots are cacheable as a direct Slots[idx] write;
                // :class-allocation lives on the owner class, so serve it without caching.
                if (!inst.Class.EffectiveSlots[idx].IsClassAllocation)
                    cell.E = new WriterCache.Entry(inst.Class, idx, GenericFunction.MethodEpoch);
                return SetSlotValueDirect(inst, idx, s.Name, newval);
            }
        }
        return Emitter.CilAssembler.GetSetfFunctionBySymbol(cell.Sym)
                                   .Invoke(new LispObject[] { newval, obj });
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
                MultipleValues.Set(cb);
                return cb;
            }
            if (Startup.Sym("SLOT-MISSING").Function is LispFunction csm)
            {
                var r = Primary(csm.Invoke(new LispObject[] { ccls ?? (LispObject)Nil.Instance, cond,
                    slotName is Symbol ? slotName : Startup.Sym(name), Startup.Sym("SLOT-BOUNDP") }));
                LispObject cb = IsTruthy(r) ? T.Instance : (LispObject)Nil.Instance;
                MultipleValues.Set(cb);
                return cb;
            }
            throw new LispErrorException(new LispError(
                $"SLOT-BOUNDP: no slot named {name} in condition {cond.ConditionTypeName}"));
        }
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

    /// <summary>SHARED-INITIALIZE's default method, in the shape the generic function
    /// calls it: args[0] = instance, args[1] = slot-names, args[2..] = initargs.</summary>
    public static LispObject SharedInitialize(LispObject[] args)
        => SharedInitializeCore(args[0], args.Length > 1 ? args[1] : Nil.Instance, args, 2);

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
        if (obj is not LispInstance inst)
            throw new LispErrorException(new LispTypeError("SHARED-INITIALIZE: not a CLOS instance", instArg));
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

        return instArg; // return original instance (possibly wrapped)
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

        // A class metaobject class makes a class, not an ordinary instance. Checked
        // before the initarg validation below: :DIRECT-SUPERCLASSES and :DIRECT-SLOTS
        // are class-creation initargs, not slots of STANDARD-CLASS.
        foreach (var cplCls in cls.ClassPrecedenceList)
            if (cplCls.Name.Name == "CLASS")
                return MakeClassMetaobject(cls, initargs);

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
                    var newGf = Runtime.NewDispatchingGF(Startup.Sym("UNNAMED"), -1);
                    newGf.RequiredCount = 0;
                    newGf.LambdaListInfoSet = true;
                    newGf.StoredClass = cls;  // track actual Lisp class (may be substandard-generic-function etc.)
                    allocated2 = newGf;
                    break;
                }
                if (cplCls.Name.Name == "METHOD")
                {
                    var newMethod = new LispMethod();
                    // Track the actual Lisp class, the way the generic function branch
                    // above does with StoredClass: CLASS-OF has to answer the class
                    // that was instantiated, and the slots it adds beyond
                    // STANDARD-METHOD are initialized against it.
                    if (cls.Name.Name != "STANDARD-METHOD") newMethod.MetaClass = cls;
                    allocated2 = newMethod;
                    break;
                }
            }
            // A funcallable instance that is not a generic function still has to BE
            // callable, and on dotcl the callable object that carries a class and
            // slots is the generic function. It starts with no methods, so calling
            // one before SET-FUNCALLABLE-INSTANCE-FUNCTION says there is no
            // applicable method -- AMOP leaves that case undefined.
            if (allocated2 == null && IsFuncallableClass(cls))
            {
                var funcallable = Runtime.NewDispatchingGF(Startup.Sym("UNNAMED"), -1);
                funcallable.RequiredCount = 0;
                funcallable.LambdaListInfoSet = true;
                funcallable.StoredClass = cls;
                allocated2 = funcallable;
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
            foreach (var (key, initformSource, thunk) in cls.DefaultInitargs)
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

    // 1-arg direct entry for MACRO-FUNCTION — (macro-function name). Replicates
    // the args-array registration's single-argument branch exactly, so the
    // compiler's per-form macro check ((macro-function sym), very hot) skips the
    // args array and the InvokeSlow detour. The 2-arg (&optional environment)
    // form stays on the variadic registration wrapper.
    public static LispObject MacroFunction1(LispObject name)
    {
        var result = MacroFunction(name);
        if (result != Nil.Instance) return result;
        var sym = GetSymbol(name, "MACRO-FUNCTION");
        var compilerFn = Startup.LookupCompilerMacro(sym);
        if (compilerFn != null)
            return new LispFunction(wrapArgs =>
                compilerFn.Invoke(new LispObject[] { wrapArgs[0] }),
                $"MACRO-EXPANDER-{sym.Name}", 2);
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
        return NewDispatchingGF(sym, ar);
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
                gf.RecomputeAccessorFlags();
            }
        }
        // Removing an INITIALIZE-INSTANCE / SHARED-INITIALIZE method also changes
        // the make-instance fast-path eligibility — invalidate the per-class caches.
        InvalidateSimpleInitCaches(gf);
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
                switch (m.Qualifiers[0].Name)
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
    /// class alone — the caller must fall back to COMPUTE-APPLICABLE-METHODS.</summary>
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
            foreach (var s in cls.EffectiveSlots)
                if (s.Name.Name == slotName.Name) { slotd = s; break; }
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
                    { m.AccessorSlot = slotd; classPosition = at; break; }
                }
            // The AccessorSlot tag is what RecomputeAccessorFlags keys on; the method
            // was already added (with AccessorSlot null then), so recompute now.
            gf.RecomputeAccessorFlags();
            // AMOP has class initialization ask the metaclass what class an accessor
            // method should be. dotcl makes plain standard methods, so the answer is
            // not used to build anything -- what the protocol buys here is the call.
            // A writer generic function is named (SETF name).
            if (slotd != null && classPosition >= 0)
                AccessorMethodClassHook?.Invoke(cls, slotd, classPosition == 0);
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
    // dispatch (cache compare + applicability checks) — e.g. (gf instance i)
    // with a fixnum i paid the INTEGER lookup per call. Builtin classes are
    // registered once at startup and CL forbids redefining them, so a one-shot
    // memo is safe. Types with special ClassOf handling (conditions, .NET
    // objects, class/GF/method metaobjects, vectors) fall through unchanged.
    private static LispClass? _dcInteger, _dcNull, _dcSymbol, _dcCons,
        _dcString, _dcCharacter, _dcDoubleFloat;

    private static LispClass? ArgDispatchClass(LispObject obj) => obj switch
    {
        LispInstance inst => inst.Class,
        Fixnum or Bignum => _dcInteger ??= FindClassOrNil(Startup.Sym("INTEGER")) as LispClass,
        Nil => _dcNull ??= FindClassOrNil(Startup.Sym("NULL")) as LispClass,
        Symbol or T => _dcSymbol ??= FindClassOrNil(Startup.Sym("SYMBOL")) as LispClass,
        Cons => _dcCons ??= FindClassOrNil(Startup.Sym("CONS")) as LispClass,
        LispString => _dcString ??= FindClassOrNil(Startup.Sym("STRING")) as LispClass,
        LispChar => _dcCharacter ??= FindClassOrNil(Startup.Sym("CHARACTER")) as LispClass,
        DoubleFloat => _dcDoubleFloat ??= FindClassOrNil(Startup.Sym("DOUBLE-FLOAT")) as LispClass,
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

    /// <summary>add ENTRY to GF's N-way dispatch cache. Rebuilds an immutable
    /// array with ENTRY at the front (most-recent), dropping any existing entry that
    /// has the same argument classes (ENTRY replaces it) and capping at
    /// DispatchCacheWidth, then publishes it with one volatile write — a concurrent
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
    /// are declared — on entry to the scope, not where the lambda is written — so
    /// writing this inline made every dispatch, :around or not, allocate a display
    /// class it almost never used. That was 56 B of the ~110 B a GF call allocated,
    /// and it survived returning early from the dispatcher, which is what made it
    /// hard to place.</summary>
    private static LispObject InvokeAroundCombination(
        CachedDispatch cached, List<LispMethod> primary, LispObject[] args)
        => InvokeAroundCombination(cached.Around, cached.Before, primary, cached.After, args);

    /// <summary>As above, for the cache-miss path, which holds the four method lists
    /// in locals rather than in a cache entry.</summary>
    private static LispObject InvokeAroundCombination(
        List<LispMethod> around, List<LispMethod> before, List<LispMethod> primary,
        List<LispMethod> after, LispObject[] args)
        => InvokeWithNextMethods(around, 0, args,
               a => InvokeStandardCombination(before, primary, after, a));

    /// <summary>Sorts methods by specificity for one call's arguments. A class rather
    /// than the lambda it replaces: a lambda over ARGS captures the dispatcher's own
    /// parameter, and C# then allocates a display class on entry to DISPATCHGF — on
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

    /// <summary>A generic function that dispatches through DISPATCHGF, with the
    /// arity-1 entry point installed. Every GF whose dispatch is the standard one is
    /// built here, so the entry points a GF carries are decided in one place.</summary>
    internal static GenericFunction NewDispatchingGF(Symbol name, int arity)
    {
        GenericFunction? gf = null;
        gf = new GenericFunction(name, arity, args => DispatchGF(gf!, args));
        gf.SetDirectDelegate((Func<LispObject, LispObject>)(a => DispatchGF1(gf!, a)));
        gf.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)((a, b) => DispatchGF2(gf!, a, b)));
        gf.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)((a, b, c) => DispatchGF3(gf!, a, b, c)));
        gf.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject, LispObject>)((a, b, c, d) => DispatchGF4(gf!, a, b, c, d)));
        return gf;
    }

    /// <summary>One-argument entry point for generic-function calls, installed as the
    /// GF's arity-1 direct delegate so INVOKE1 reaches dispatch without building an
    /// argument array (32 B a call, which is what a GF call allocates once the closure
    /// in the dispatcher is gone).
    ///
    /// Only the shape that needs nothing but the single argument runs here: a warm
    /// cache entry for this argument's class, one primary method, nothing around it,
    /// and no keyword checking to do. Everything else — a miss, EQL specializers,
    /// :around/:before/:after, built-in combinations, the slot reader/writer
    /// shortcuts, arity errors — builds the array and goes through DISPATCHGF
    /// unchanged, so this adds a fast path rather than a second dispatcher.</summary>
    private static LispObject DispatchGF1(GenericFunction gf, LispObject a)
    {
        if (gf.DispatchFunction is LispFunction df1) return df1.Invoke(new[] { a });
        // The protocol route needs the argument array anyway; the check itself is one
        // static bool read until someone specialises one of the three.
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a });
        var entry = PlainCacheHit(gf, a, null, null, null, 1);
        if (entry != null)
            return entry.PlainPrimaryChain
                ? InvokeChainLoose(entry.Primary, a, null, null, null, 1)
                : InvokeCombinationLoose(entry, a, null, null, null, 1);
        // EQL-specialized generic functions are cached only for one required argument,
        // so this is the only arity that can take them without an array. The array path
        // is what an EQL dispatch used to cost even on a warm cache: 32 B a call, and
        // (defmethod f ((x (eql :k))) ...) is an ordinary way to write a dispatch table.
        var eqlChain = EqlCacheHit(gf, a);
        if (eqlChain != null) return InvokeChainLoose(eqlChain, a, null, null, null, 1);
        return DispatchGF(gf, new[] { a });
    }

    /// <summary>The method chain a warm EQL-specialized cache entry runs for A, or null
    /// when this call is not one the loose-argument path can take (no entry, a shape with
    /// :around/:before/:after or a built-in combination, keyword checking to do, or one of
    /// the slot reader/writer shortcuts). A matching EQL method runs at the head of the
    /// precomputed [eql-method, non-EQL primaries...] chain; with no match the non-EQL
    /// primaries are the whole chain. Mirrors the entry-selection half of DISPATCHGF's
    /// cache-hit path — the invocation half is INVOKECHAINLOOSE.</summary>
    private static List<LispMethod>? EqlCacheHit(GenericFunction gf, LispObject a)
    {
        var dcache = gf.DispatchCache;
        if (dcache == null) return null;
        if (gf.LambdaListInfoSet && gf.HasKey && !gf.HasAllowOtherKeys) return null;
        int required = gf.LambdaListInfoSet ? gf.RequiredCount : (gf.Arity >= 0 ? gf.Arity : 0);
        if (required != 1) return null;
        if (gf.LambdaListInfoSet && !gf.HasRest && !gf.HasKey
            && 1 > gf.RequiredCount + gf.OptionalCount) return null;
        foreach (var entry in dcache)
        {
            var types = entry.ArgTypes;
            if (types.Length != 1) continue;
            if (!ReferenceEquals(types[0], ArgDispatchClass(a))) continue;
            if (!entry.HasEqlSpecializers || entry.EqlValues == null || entry.EqlChains == null)
                return null;
            if (entry.IsBuiltinCombination || entry.Around.Count != 0
                || entry.Before.Count != 0 || entry.After.Count != 0
                || entry.ReaderSlotIndex >= 0 || entry.WriterSlotIndex >= 0)
                return null;
            var eqlValues = entry.EqlValues;
            for (int i = 0; i < eqlValues.Length; i++)
            {
                // Same comparison the array path uses: identity, then fixnum value,
                // then the general EQL for the mixed cases.
                var v = eqlValues[i];
                if (ReferenceEquals(a, v)
                    || (a is Fixnum fa
                        ? v is Fixnum fv && fa.Value == fv.Value
                        : v is not Fixnum && IsTrueEql(a, v)))
                    return entry.EqlChains[i];
            }
            // No EQL value matched: the class-specialized primaries are what applies.
            // An empty set is "no applicable method", which DISPATCHGF reports.
            return entry.Primary.Count > 0 ? entry.Primary : null;
        }
        return null;
    }

    /// <summary>Two-argument twin of DISPATCHGF1.</summary>
    private static LispObject DispatchGF2(GenericFunction gf, LispObject a, LispObject b)
    {
        if (gf.DispatchFunction is LispFunction df2) return df2.Invoke(new[] { a, b });
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a, b });
        var entry = PlainCacheHit(gf, a, b, null, null, 2);
        if (entry != null)
            return entry.PlainPrimaryChain
                ? InvokeChainLoose(entry.Primary, a, b, null, null, 2)
                : InvokeCombinationLoose(entry, a, b, null, null, 2);
        return DispatchGF(gf, new[] { a, b });
    }

    /// <summary>Three-argument twin of DISPATCHGF1.</summary>
    private static LispObject DispatchGF3(GenericFunction gf, LispObject a, LispObject b, LispObject c)
    {
        if (gf.DispatchFunction is LispFunction df3) return df3.Invoke(new[] { a, b, c });
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a, b, c });
        var entry = PlainCacheHit(gf, a, b, c, null, 3);
        if (entry != null)
            return entry.PlainPrimaryChain
                ? InvokeChainLoose(entry.Primary, a, b, c, null, 3)
                : InvokeCombinationLoose(entry, a, b, c, null, 3);
        return DispatchGF(gf, new[] { a, b, c });
    }

    /// <summary>Run a one-primary-with-:before/:after combination on loose arguments.
    ///
    /// The auxiliary methods run purely for effect on the same arguments the primary
    /// gets -- CLHS 7.6.6.2: :before methods most specific first, then the primary,
    /// then :after methods least specific first, and the value is the primary's.
    /// None of them can reach CALL-NEXT-METHOD past the single primary, so the whole
    /// combination needs no arguments array; INVOKECHAINLOOSE handles the primary and
    /// its next-method state, and the auxiliaries invoke directly by arity.</summary>
    private static LispObject InvokeCombinationLoose(
        CachedDispatch entry, LispObject a, LispObject? b, LispObject? c, LispObject? d, int argc)
    {
        var before = entry.Before;
        for (int i = 0; i < before.Count; i++) InvokeLoose(before[i], a, b, c, d, argc);
        var result = InvokeChainLoose(entry.Primary, a, b, c, d, argc);
        var after = entry.After;
        for (int i = 0; i < after.Count; i++) InvokeLoose(after[i], a, b, c, d, argc);
        return result;
    }

    /// <summary>Invoke one method on loose arguments, by arity.</summary>
    private static void InvokeLoose(
        LispMethod m, LispObject a, LispObject? b, LispObject? c, LispObject? d, int argc)
    {
        var fn = m.Function;
        switch (argc)
        {
            case 1: fn.Invoke1(a); break;
            case 2: fn.Invoke2(a, b!); break;
            case 3: fn.Invoke3(a, b!, c!); break;
            default: fn.Invoke4(a, b!, c!, d!); break;
        }
    }

    /// <summary>Four-argument twin of DISPATCHGF1. The arity cliff was real: a
    /// generic function of 1-3 arguments allocated nothing on a warm cache while
    /// one of 4 allocated 72 B a call, purely because the loose path stopped at
    /// three. SHARED-INITIALIZE reaches it on every (instance slot-names . initargs)
    /// call with one initarg pair.</summary>
    private static LispObject DispatchGF4(
        GenericFunction gf, LispObject a, LispObject b, LispObject c, LispObject d)
    {
        if (gf.DispatchFunction is LispFunction df4) return df4.Invoke(new[] { a, b, c, d });
        if (AnyInvocationProtocolCustomized && UsesInvocationProtocol(gf))
            return DispatchGFCore(gf, new[] { a, b, c, d });
        var entry = PlainCacheHit(gf, a, b, c, d, 4);
        if (entry != null)
            return entry.PlainPrimaryChain
                ? InvokeChainLoose(entry.Primary, a, b, c, d, 4)
                : InvokeCombinationLoose(entry, a, b, c, d, 4);
        return DispatchGF(gf, new[] { a, b, c, d });
    }

    /// <summary>The cache entry for these arguments when it is one this path can run
    /// without an argument array, else null (the caller then goes through DISPATCHGF).
    ///
    /// An entry may hold fewer classes than the call has arguments — it records the
    /// ones dispatch looked at — so the same rule DISPATCHGF's own scan uses applies:
    /// compare the classes the entry has, ignore the rest.</summary>
    private static CachedDispatch? PlainCacheHit(
        GenericFunction gf, LispObject a, LispObject? b, LispObject? c, LispObject? d, int argc)
    {
        var dcache = gf.DispatchCache;
        if (dcache == null) return null;
        if (gf.LambdaListInfoSet && gf.HasKey && !gf.HasAllowOtherKeys) return null;
        // Argument count. DISPATCHGF checks this itself and signals a PROGRAM-ERROR,
        // and these entry points are reached before it: INVOKE2 on a one-argument
        // generic function lands in DISPATCHGF2, where a cache entry recorded for the
        // one argument it dispatches on would otherwise match and run the method with
        // an argument too many. Out of range, hand it to DISPATCHGF to signal.
        int required = gf.LambdaListInfoSet ? gf.RequiredCount : (gf.Arity >= 0 ? gf.Arity : 0);
        if (argc < required) return null;
        if (gf.LambdaListInfoSet && !gf.HasRest && !gf.HasKey
            && argc > gf.RequiredCount + gf.OptionalCount) return null;
        foreach (var entry in dcache)
        {
            var types = entry.ArgTypes;
            if (types.Length == 0 || types.Length > argc) continue;
            if (!ReferenceEquals(types[0], ArgDispatchClass(a))) continue;
            if (types.Length > 1 && !ReferenceEquals(types[1], ArgDispatchClass(b!))) continue;
            if (types.Length > 2 && !ReferenceEquals(types[2], ArgDispatchClass(c!))) continue;
            if (types.Length > 3 && !ReferenceEquals(types[3], ArgDispatchClass(d!))) continue;
            // Matching entry: either it is a shape this path runs, or nothing else in
            // the cache can match these classes, so stop either way.
            return (entry.PlainPrimaryChain || entry.PrimaryChainWithBeforeAfter) ? entry : null;
        }
        return null;
    }

    /// <summary>Run CHAIN[0] on ARGC loose arguments with the next-method state a method
    /// body expects. The counterpart of INVOKESTANDARDCOMBINATION for the callers that
    /// hold the arguments rather than an array. A one-method chain has nowhere for
    /// CALL-NEXT-METHOD to go, so the body must see the default closures; a longer one
    /// leaves the captured slots empty, which is how CAPTUREDCNM knows to build from the
    /// chain state (an EQL-specialized method followed by the class-specialized primaries
    /// is the case that has more than one). The arguments of the current invocation are
    /// published lazily — only CALL-NEXT-METHOD with no arguments needs them as a list,
    /// and it materialises them from the loose slots through CURRENTGFARGS.</summary>
    private static LispObject InvokeChainLoose(
        List<LispMethod> primary, LispObject a, LispObject? b, LispObject? c, LispObject? d, int argc)
    {
        var savedChain = _nextMethodChain;
        var savedIndex = _nextMethodIndex;
        var savedArgs = _currentGFArgs;
        var savedFallback = _nextMethodFallback;
        var savedCapturedNmp = _capturedNmp;
        var savedCapturedCnm = _capturedCnm;
        var savedArg0 = _currentGFArg0;
        var savedArg1 = _currentGFArg1;
        var savedArg2 = _currentGFArg2;
        var savedArg3 = _currentGFArg3;
        var savedArgc = _currentGFArgc;
        _nextMethodChain = primary;
        _nextMethodIndex = 1;
        _currentGFArgs = null;
        _currentGFArg0 = a;
        _currentGFArg1 = b;
        _currentGFArg2 = c;
        _currentGFArg3 = d;
        _currentGFArgc = argc;
        _nextMethodFallback = null;
        // One method: nothing for CALL-NEXT-METHOD to reach, so publish the default
        // closures. More than one: leave the slots empty so a body that captures builds
        // from the chain state above.
        bool single = primary.Count == 1;
        _capturedNmp = single ? _defaultNmp : null;
        _capturedCnm = single ? _defaultCnm : null;
        try
        {
            var fn = primary[0].Function;
            return argc switch
            {
                1 => fn.Invoke1(a),
                2 => fn.Invoke2(a, b!),
                3 => fn.Invoke3(a, b!, c!),
                _ => fn.Invoke4(a, b!, c!, d!)
            };
        }
        finally
        {
            _nextMethodChain = savedChain;
            _nextMethodIndex = savedIndex;
            _currentGFArgs = savedArgs;
            _currentGFArg0 = savedArg0;
            _currentGFArg1 = savedArg1;
            _currentGFArg2 = savedArg2;
            _currentGFArg3 = savedArg3;
            _currentGFArgc = savedArgc;
            _nextMethodFallback = savedFallback;
            _capturedNmp = savedCapturedNmp;
            _capturedCnm = savedCapturedCnm;
        }
    }

    private static LispObject DispatchGF(GenericFunction gf, LispObject[] args)
    {
        // A discriminating function installed on this generic function replaces
        // dispatch entirely -- applicable methods, the cache and the combination are
        // all its business now. Checked at every entry point, arity fast paths
        // included, or an installed function would be bypassed by exactly the calls
        // that are most common. Null on every generic function until something
        // installs one, so the cost on the usual path is a field read.
        if (gf.DispatchFunction is LispFunction dfN) return dfN.Invoke(args);
        return DispatchGFCore(gf, args);
    }

    /// <summary>dotcl.s own dispatch with the discriminating-function check skipped.
    /// COMPUTE-DISCRIMINATING-FUNCTION.s default method hands this back, so a user
    /// method that calls CALL-NEXT-METHOD and installs the result gets standard
    /// dispatch instead of looping back through the function it just installed.</summary>
    internal static LispObject DispatchGFCore(GenericFunction gf, LispObject[] args)
    {
        // Arity check: signal program-error for too few/too many arguments
        int requiredCount = gf.LambdaListInfoSet ? gf.RequiredCount : (gf.Arity >= 0 ? gf.Arity : 0);
        // A generic function with no required parameters has nothing to dispatch on:
        // every call sees the same applicable set, so the cache key is the empty class
        // vector and the entry matches unconditionally. Only when the lambda list is
        // actually known -- with LAMBDALISTINFOSET false and no arity, REQUIREDCOUNT is
        // 0 because nothing was recorded, not because the function takes no arguments,
        // and an empty key would then match calls that really do dispatch.
        bool zeroDispatch = gf.LambdaListInfoSet && gf.RequiredCount == 0;
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

        // Cache check: N-way polymorphic inline cache. Scan the recent
        // dispatches for one whose argument classes match this call, then fall into
        // the (unchanged) hit logic below. The old single-entry cache missed on every
        // alternation between 2+ classes at a call site (3.5-4.6x on poly sites).
        CachedDispatch? cached = null;
        {
            var dcache = gf.DispatchCache;
            if (dcache != null)
            {
                foreach (var entry in dcache)
                {
                    var cachedTypes = entry.ArgTypes;
                    if (cachedTypes.Length > args.Length) continue;
                    bool match = true;
                    for (int i = 0; i < cachedTypes.Length; i++)
                    {
                        if (!ReferenceEquals(ArgDispatchClass(args[i]), cachedTypes[i]))
                        { match = false; break; }
                    }
                    if (match) { cached = entry; break; }
                }
            }
        }
        if (cached != null)
        {
                // The AMOP route: this generic function's effective method for these
                // argument classes was built by the protocol on the miss below.
                if (cached.EffectiveMethodFunction is { } cachedEmf)
                    return cachedEmf.Invoke(args);
                // Item2: specialized standard slot reader — read the slot directly,
                // skipping keyword/eql checks and effective-method construction. The cache
                // entry was only stored for this shape (1 dispatch arg, single accessor
                // primary, no aux methods, standard metaclass, instance-allocated slot), so
                // args[0] is a LispInstance of ArgTypes[0] and ReaderSlotIndex is its slot.
                if (cached.ReaderSlotIndex >= 0 && args[0] is LispInstance readerInst)
                    return SlotValueDirect(readerInst, cached.ReaderSlotIndex,
                                           cached.ReaderSlotName!, cached.ReaderSlotName!.Name);
                // Item2b: specialized standard slot writer — write the slot directly.
                // (setf accessor) is arity 2: object = args[1], new value = args[0].
                if (cached.WriterSlotIndex >= 0 && args.Length >= 2 && args[1] is LispInstance writerInst)
                    return SetSlotValueDirect(writerInst, cached.WriterSlotIndex,
                                              cached.WriterSlotName!.Name, args[0]);
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
                // For EQL specializers: check if any EQL method matches (takes
                // priority — the cache is only stored for single-required-arg GFs,
                // where an applicable EQL method is always the most specific and at
                // most one EQL value can match; see the cache-store comment).
                if (cached.HasEqlSpecializers && cached.EqlValues != null)
                {
                    int eqlIdx = -1;
                    var eqlValues = cached.EqlValues;
                    var a0 = args[0];
                    for (int i = 0; i < eqlValues.Length; i++)
                    {
                        // Fast eql: identity (symbols/keywords/interned values) or
                        // fixnum value compare; a fixnum is never eql to a
                        // non-fixnum, so only mixed non-fixnum pairs (floats,
                        // chars, ...) take the generic IsTrueEql.
                        var v = eqlValues[i];
                        if (ReferenceEquals(a0, v)
                            || (a0 is Fixnum fa
                                ? v is Fixnum fv && fa.Value == fv.Value
                                : v is not Fixnum && IsTrueEql(a0, v)))
                        { eqlIdx = i; break; }
                    }
                    if (eqlIdx >= 0)
                    {
                        var eqlMatch = cached.EqlMethods![eqlIdx];
                        // Fast path: no around/before/after and no non-EQL primaries
                        // → invoke EQL method directly with minimal overhead
                        if (cached.Around.Count == 0 && cached.Before.Count == 0
                            && cached.After.Count == 0 && cached.Primary.Count == 0)
                        {
                            var savedChain = _nextMethodChain;
                            var savedIndex = _nextMethodIndex;
                            var savedArgs = _currentGFArgs;
                            var savedFallback = _nextMethodFallback;
                            var savedNmp = _capturedNmp;
                            var savedCnm = _capturedCnm;
                            _nextMethodChain = null;
                            _nextMethodIndex = 0;
                            _currentGFArgs = args;
                            _nextMethodFallback = null;
                            // Reset the captured cnm/nmp slots too — otherwise a body
                            // that captures CALL-NEXT-METHOD would see an enclosing
                            // dispatch's closure instead of "no next method".
                            _capturedNmp = null;
                            _capturedCnm = null;
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
                                _capturedNmp = savedNmp;
                                _capturedCnm = savedCnm;
                            }
                        }
                        // General case: use the precomputed [eql-method, non-EQL
                        // primaries...] chain — no per-hit list building.
                        var eqlPrimary = cached.EqlChains![eqlIdx];
                        if (cached.Around.Count > 0)
                            return InvokeAroundCombination(cached, eqlPrimary, args);
                        return InvokeStandardCombination(cached.Before, eqlPrimary, cached.After, args);
                    }
                    // No EQL match — fall through to cached non-EQL result
                }
                // EQL cache with no matching EQL method and no non-EQL primaries:
                // mirror the miss path (no-applicable → fallback fn or error;
                // applicable-but-no-primary → error). Non-EQL caches always have
                // >= 1 primary (the miss path throws before storing otherwise).
                if (cached.HasEqlSpecializers && cached.Primary.Count == 0)
                {
                    if (cached.Around.Count == 0 && cached.Before.Count == 0
                        && cached.After.Count == 0)
                    {
                        if (gf.FallbackFunction != null)
                            return gf.FallbackFunction.Invoke(args);
                        throw new LispErrorException(new LispError(
                            $"No applicable method for generic function {gf.Name.Name}"));
                    }
                    throw new LispErrorException(new LispError(
                        $"No primary method for generic function {gf.Name.Name}"));
                }
                // Cache hit: reuse sorted method lists
                if (cached.IsBuiltinCombination && cached.Applicable != null)
                    return DispatchBuiltinCombination(gf, cached.Applicable, args);
                if (cached.Around.Count > 0)
                    return InvokeAroundCombination(cached, cached.Primary, args);
                return InvokeStandardCombination(cached.Before, cached.Primary, cached.After, args);
        }

        // A generic function whose invocation protocol someone specialised is dispatched
        // through it. The answer is cached per argument-class vector like everything
        // else here, so the protocol runs on a miss rather than on every call. If it
        // cannot answer (no applicable methods, or the pieces are missing), fall through
        // to dotcl's own dispatch, which reports no-applicable-method the usual way.
        if (UsesInvocationProtocol(gf))
        {
            var protocolEmf = EffectiveMethodThroughProtocol(gf, args);
            if (protocolEmf != null)
            {
                var protocolTypes = new LispClass?[Math.Min(args.Length, Math.Max(1, requiredCount))];
                for (int i = 0; i < protocolTypes.Length; i++)
                    protocolTypes[i] = ArgDispatchClass(args[i]);
                AddDispatchCache(gf, new CachedDispatch
                {
                    ArgTypes = protocolTypes,
                    Around = new List<LispMethod>(),
                    Before = new List<LispMethod>(),
                    Primary = new List<LispMethod>(),
                    After = new List<LispMethod>(),
                    EffectiveMethodFunction = protocolEmf
                });
                return protocolEmf.Invoke(args);
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
                    if (EqlSpecializerValue(spec) != null) { hasEqlSpec = true; break; }
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
                // Width 0 for a no-required-parameter GF: see ZERODISPATCH above.
                int n = zeroDispatch ? 0 : Math.Max(1, requiredCount);
                var types = new LispClass?[n];
                for (int i = 0; i < n && i < args.Length; i++)
                    types[i] = ArgDispatchClass(args[i]);
                AddDispatchCache(gf, new CachedDispatch
                {
                    ArgTypes = types,
                    Applicable = applicable,
                    HasEqlSpecializers = false,
                    IsBuiltinCombination = true,
                    Around = new List<LispMethod>(),
                    Before = new List<LispMethod>(),
                    Primary = new List<LispMethod>(),
                    After = new List<LispMethod>()
                });
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
        var bySpecificity = new SpecificityComparer(args, false);
        primaryMethods.Sort(bySpecificity);
        beforeMethods.Sort(bySpecificity);
        afterMethods.Sort(new SpecificityComparer(args, true)); // reverse for :after
        aroundMethods.Sort(bySpecificity);

        // Update monomorphic cache for STANDARD combination.
        //
        // EQL-specialized GFs need care: the cache key is the argument CLASSES
        // only, so two calls with the same classes but different EQL values
        // (e.g. (cv :seq x) vs (cv :other x)) share the single cache slot. A
        // class-keyed hit therefore must re-check the EQL methods against the
        // actual argument values — that is what the hit path's EqlMethods scan
        // does. Storing such a cache is only correct when the hit path's
        // reconstruction ("matching EQL method is the most specific primary,
        // prepended to the cached non-EQL primaries") provably matches CLHS
        // 7.6.6 ordering:
        //   - exactly 1 required (dispatch) position: with a single position an
        //     applicable EQL method always beats every class-specialized method,
        //     and two distinct EQL methods can never both be applicable — so
        //     "prepend the (unique) EQL match" IS the sorted order. With 2+
        //     positions a class method can out-rank an EQL method (leftmost
        //     comparison), so those GFs stay uncached.
        //   - all EQL methods unqualified: the hit path files the EQL match as a
        //     primary; an EQL :before/:after/:around would be misfiled.
        // GFs outside this shape keep the old behavior (recompute per call).
        if (!hasEqlSpec)
        {
            // Width 0 for a no-required-parameter GF: see ZERODISPATCH above.
            int n = zeroDispatch ? 0 : Math.Max(1, requiredCount);
            var types = new LispClass?[n];
            for (int i = 0; i < n && i < args.Length; i++)
                types[i] = ArgDispatchClass(args[i]);
            // Item2: specialize a standard slot READER — one dispatch arg, a single
            // accessor primary with no before/after/around, a standard-metaclass instance
            // and an instance-allocated slot. The hit path then reads the slot directly
            // (SlotValueDirect), which is exactly what the reader method's body does, minus
            // effective-method construction and the lambda call. requiredCount==1 excludes
            // (setf accessor) writers (arity 2). InvalidateCache on any method add clears it.
            int readerIdx = -1, writerIdx = -1;
            Symbol? readerName = null, writerName = null;
            if (primaryMethods.Count == 1
                && aroundMethods.Count == 0 && beforeMethods.Count == 0 && afterMethods.Count == 0
                && primaryMethods[0].AccessorSlot is { } asd)
            {
                if (requiredCount == 1
                    && args.Length >= 1 && args[0] is LispInstance rinst && rinst.Class.Metaclass == null
                    && rinst.Class.SlotIndex.TryGetValue(asd.Name.Name, out int sidx)
                    && !rinst.Class.EffectiveSlots[sidx].IsClassAllocation)
                {
                    readerIdx = sidx;
                    readerName = asd.Name;
                }
                // Item2b: (setf accessor) writer — object is the last required arg
                // (args[requiredCount-1]), new value is args[0]. Write the slot directly.
                else if (requiredCount == 2
                    && args.Length >= 2 && args[1] is LispInstance winst && winst.Class.Metaclass == null
                    && winst.Class.SlotIndex.TryGetValue(asd.Name.Name, out int widx)
                    && !winst.Class.EffectiveSlots[widx].IsClassAllocation)
                {
                    writerIdx = widx;
                    writerName = asd.Name;
                }
            }
            AddDispatchCache(gf, new CachedDispatch
            {
                ArgTypes = types,
                Around = aroundMethods,
                Before = beforeMethods,
                Primary = primaryMethods,
                After = afterMethods,
                HasEqlSpecializers = false,
                ReaderSlotIndex = readerIdx,
                ReaderSlotName = readerName,
                WriterSlotIndex = writerIdx,
                WriterSlotName = writerName
            });
        }
        else if (requiredCount == 1)
        {
            var eqlMethods = new List<LispMethod>();
            bool cacheable = true;
            foreach (var m in gf.Methods)
            {
                bool hasEql = false;
                foreach (var spec in m.Specializers)
                    if (EqlSpecializerValue(spec) != null) { hasEql = true; break; }
                if (!hasEql) continue;
                if (m.Qualifiers.Length != 0) { cacheable = false; break; }
                eqlMethods.Add(m);
            }
            if (cacheable)
            {
                // Cached partitions hold only the non-EQL methods; the hit path
                // re-checks EqlMethods per call and prepends the match. The
                // current call's own invocation below still uses the full
                // sorted primaryMethods (EQL included), so copy-on-filter.
                var nonEqlPrimary = new List<LispMethod>(primaryMethods.Count);
                foreach (var m in primaryMethods)
                {
                    bool hasEql = false;
                    foreach (var spec in m.Specializers)
                        if (EqlSpecializerValue(spec) != null) { hasEql = true; break; }
                    if (!hasEql) nonEqlPrimary.Add(m);
                }
                // Precompute per-EQL-method dispatch data: the EQL value (the
                // single required arg means the EQL specializer is at position
                // 0) and the effective primary chain [eql-method, non-EQL
                // primaries...], so a warm hit allocates nothing.
                var eqlArr = eqlMethods.ToArray();
                var eqlValues = new LispObject[eqlArr.Length];
                var eqlChains = new List<LispMethod>[eqlArr.Length];
                for (int i = 0; i < eqlArr.Length; i++)
                {
                    eqlValues[i] = EqlSpecializerValue(eqlArr[i].Specializers[0])!;
                    var chain = new List<LispMethod>(1 + nonEqlPrimary.Count) { eqlArr[i] };
                    chain.AddRange(nonEqlPrimary);
                    eqlChains[i] = chain;
                }
                var types = new LispClass?[1];
                if (args.Length > 0) types[0] = ArgDispatchClass(args[0]);
                AddDispatchCache(gf, new CachedDispatch
                {
                    ArgTypes = types,
                    Around = aroundMethods,
                    Before = beforeMethods,
                    Primary = nonEqlPrimary,
                    After = afterMethods,
                    HasEqlSpecializers = true,
                    EqlMethods = eqlArr,
                    EqlValues = eqlValues,
                    EqlChains = eqlChains
                });
            }
        }

        // Build effective method chain
        if (aroundMethods.Count > 0)
        {
            // :around wraps everything
            return InvokeAroundCombination(aroundMethods, beforeMethods, primaryMethods,
                                           afterMethods, args);
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
    /// <summary>The arguments of an invocation that came through one of the loose-argument
    /// entry points, which have no array to publish. Read only through
    /// <see cref="CurrentGFArgs"/>, and only meaningful while _currentGFArgs is null;
    /// _currentGFArgc says how many of them are live.</summary>
    [ThreadStatic]
    private static LispObject? _currentGFArg0;
    [ThreadStatic]
    private static LispObject? _currentGFArg1;
    [ThreadStatic]
    private static LispObject? _currentGFArg2;
    [ThreadStatic]
    private static LispObject? _currentGFArg3;
    [ThreadStatic]
    private static int _currentGFArgc;

    /// <summary>The arguments of the invocation in progress, as a list. CALL-NEXT-METHOD
    /// with no arguments passes these on, and that is the only reader — so the arity-1
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
    /// CALL-NEXT-METHOD / NEXT-METHOD-P — so methods that never use them pay no
    /// per-dispatch closure allocation at all. The lazily built closure snapshots
    /// the same values the previous eager version captured, keeping indefinite
    /// extent (CLHS 7.6.6.1).</summary>
    public static LispObject CapturedCnm()
    {
        var cached = _capturedCnm;
        if (cached != null) return cached;
        var chain = _nextMethodChain;
        if (chain == null) return _defaultCnm;
        var closureChain = chain;
        var closureIdx = _nextMethodIndex;
        var closureArgs = CurrentGFArgs();
        var closureFallback = _nextMethodFallback;
        var currentMethod = (closureIdx > 0 && closureIdx - 1 < closureChain.Count)
            ? closureChain[closureIdx - 1] : null;
        var cnm = new LispFunction(
            cnmArgs => {
                var actualArgs = cnmArgs.Length > 0 ? cnmArgs : closureArgs;
                if (cnmArgs.Length > 0 && currentMethod != null
                    && !IsMethodApplicable(currentMethod, actualArgs))
                    throw new LispErrorException(new LispProgramError(
                        "CALL-NEXT-METHOD: changed arguments are not applicable to the current method"));
                return CallNextMethodWithChain(closureChain, closureIdx, actualArgs, closureFallback);
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

        // Indefinite-extent NEXT-METHOD-P / CALL-NEXT-METHOD closures (CLHS
        // 7.6.6.1, 7.6.6.2) are built LAZILY by CapturedNmp/CapturedCnm from the
        // thread state installed above: clearing the captured slots here marks
        // "this invocation has not built its closures yet". Method bodies that
        // never mention CALL-NEXT-METHOD / NEXT-METHOD-P (the defmethod compiler
        // emits the capture only when the body uses them) thus pay no closure
        // allocation per dispatch.
        var savedCapturedNmp = _capturedNmp;
        var savedCapturedCnm = _capturedCnm;
        _capturedNmp = null;
        _capturedCnm = null;

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
        var chain = _nextMethodChain;
        if (chain == null)
            throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method"));
        int idx = _nextMethodIndex;
        if (_currentGFArgs != null || idx >= chain.Count
            || _currentGFArgc < 1 || _currentGFArgc > 4)
            return CallNextMethod();

        var savedIndex = _nextMethodIndex;
        var savedCapturedNmp = _capturedNmp;
        var savedCapturedCnm = _capturedCnm;
        _nextMethodIndex = idx + 1;
        // The next body builds its own closures from the chain state if it
        // captures, exactly as INVOKEWITHNEXTMETHODS arranges for the array path.
        _capturedNmp = null;
        _capturedCnm = null;
        try
        {
            var fn = chain[idx].Function;
            return _currentGFArgc switch
            {
                1 => fn.Invoke1(_currentGFArg0!),
                2 => fn.Invoke2(_currentGFArg0!, _currentGFArg1!),
                3 => fn.Invoke3(_currentGFArg0!, _currentGFArg1!, _currentGFArg2!),
                _ => fn.Invoke4(_currentGFArg0!, _currentGFArg1!, _currentGFArg2!, _currentGFArg3!)
            };
        }
        finally
        {
            _nextMethodIndex = savedIndex;
            _capturedNmp = savedCapturedNmp;
            _capturedCnm = savedCapturedCnm;
        }
    }

    public static LispObject CallNextMethod(params LispObject[] args)
    {
        if (_nextMethodChain == null)
            throw new LispErrorException(new LispError("CALL-NEXT-METHOD: no next method"));

        LispObject[] actualArgs = args.Length > 0 ? args : CurrentGFArgs();

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

    /// <summary>Is SPECCLS applicable to an argument of ARGCLS through an assignability
    /// the class precedence list does not carry? .NET generic variance is the case that
    /// matters: List&lt;String&gt; implements IEnumerable&lt;String&gt;, and the covariant
    /// IEnumerable&lt;out T&gt; makes that assignable to IEnumerable&lt;Object&gt; — but
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

        // Native (runtime-signaled) condition: consult its registered class's slots.
        if (obj is LispCondition cond)
            return ClassOf(cond) is LispClass ccls && ccls.SlotIndex.ContainsKey(name)
                ? T.Instance : Nil.Instance;

        return Nil.Instance;
    }

    private static bool HasSpecializedAllocator(LispClass cls)
    {
        foreach (var s in cls.ClassPrecedenceList)
            if (s.Name.Name == "GENERIC-FUNCTION" || s.Name.Name == "METHOD") return true;
        return IsFuncallableClass(cls);
    }

    /// <summary>True for a class whose instances must be callable: AMOP's
    /// FUNCALLABLE-STANDARD-CLASS as the metaclass, or FUNCALLABLE-STANDARD-OBJECT in
    /// the precedence list.</summary>
    internal static bool IsFuncallableClass(LispClass cls)
    {
        if (cls.Metaclass is { } meta)
            foreach (var m in meta.ClassPrecedenceList)
                if (m.Name.Name == "FUNCALLABLE-STANDARD-CLASS") return true;
        foreach (var s in cls.ClassPrecedenceList)
            if (s.Name.Name == "FUNCALLABLE-STANDARD-OBJECT") return true;
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
                    if (cls is not LispClass)
                        throw new LispErrorException(new LispTypeError("(SETF CLASS-NAME): not a class", cls));
                    // AMOP defines this as reinitialization, not a field write, so a
                    // metaclass with a REINITIALIZE-INSTANCE method sees the change.
                    // The :after below is what actually applies the name.
                    if (Startup.Sym("REINITIALIZE-INSTANCE").Function is LispFunction reinit)
                        reinit.Invoke(new LispObject[] { cls, Startup.Keyword("NAME"), newName });
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
                    var newGf = Runtime.NewDispatchingGF(Startup.Sym("UNNAMED"), -1);
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
                klass.ExtraSlots?.TryRemove(name, out _);
                return klass;
            }
            if (obj0 is LispCondition cond)
            {
                var ccls = Runtime.ClassOf(cond) as LispClass;
                if (ccls != null && ccls.SlotIndex.ContainsKey(name) && Runtime.TryWriteNativeConditionSlot(cond, name, null))
                    return args[0];
                if (Startup.Sym("SLOT-MISSING").Function is LispFunction csm)
                {
                    csm.Invoke(new LispObject[] { ccls ?? (LispObject)Nil.Instance, cond,
                        args[1] is Symbol ? args[1] : Startup.Sym(name), Startup.Sym("SLOT-MAKUNBOUND") });
                    return args[0];
                }
                throw new LispErrorException(new LispError(
                    $"SLOT-MAKUNBOUND: no slot named {name} in condition {cond.ConditionTypeName}"));
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
        // NEXT-METHOD-P and CALL-NEXT-METHOD.
        // NEXT-METHOD-P accepts exactly 0 arguments (CLHS): passing one is a
        // PROGRAM-ERROR. The compiler rejects it statically (the next-method-p
        // special-form handler) and the per-invocation captured closure checks it
        // at call time, but this globally registered function silently ignored
        // whatever it was handed — so an interpreted method body, where the
        // operator resolves through this binding, returned NIL/T instead of
        // signalling. Same check as CapturedNmp, so all three paths agree.
        Emitter.CilAssembler.RegisterFunction("NEXT-METHOD-P",
            new LispFunction(args => {
                if (args.Length != 0)
                    throw new LispErrorException(new LispProgramError(
                        $"NEXT-METHOD-P: wrong number of arguments: {args.Length} (expected 0)"));
                return Runtime.NextMethodP();
            }, "NEXT-METHOD-P", 0));
        Emitter.CilAssembler.RegisterFunction("CALL-NEXT-METHOD",
            new LispFunction(Runtime.CallNextMethod, "CALL-NEXT-METHOD", -1));

        // CLOS intrinsics callable as functions, for the emit-free tree-walk
        // interpreter (%mini-eval). The compiler emits these as inline
        // (:call "Runtime.X") via the special-form table; the compiled path is
        // unaffected. defclass/defgeneric/defmethod macro-expand into calls to
        // these (DOTCL-INTERNAL package), so the interpreter must be able to
        // apply them. All map to existing Runtime methods — no new emit.
        // A fixed-arity builtin also gets a typed direct delegate, so a call site of
        // that arity reaches it through Invoke0..3 instead of InvokeSlow (which pushes
        // a call-stack frame and re-checks the argument count). The wrapper still packs
        // the small array the shared body expects; what is saved is the slow-path
        // bookkeeping, and these run once per defclass / defmethod / slot access during
        // compilation. RegisterUnary/RegisterBinary in Startup have done this all along;
        // the CLOS builtins were the set that missed out.
        void RegClos(string name, System.Func<LispObject[], LispObject> fn, int arity = -1)
        {
            var lispFn = new LispFunction(fn, name, arity);
            switch (arity)
            {
                case 0: lispFn.SetDirectDelegate((Func<LispObject>)(() => fn(System.Array.Empty<LispObject>()))); break;
                case 1: lispFn.SetDirectDelegate((Func<LispObject, LispObject>)(a => fn(new[] { a }))); break;
                case 2: lispFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)((a, b) => fn(new[] { a, b }))); break;
                case 3: lispFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)((a, b, c) => fn(new[] { a, b, c }))); break;
            }
            Emitter.CilAssembler.RegisterFunction(name, lispFn);
        }
        RegClos("%MAKE-CLASS", a => Runtime.MakeClass(a[0], a[1], a[2]), 3);
        RegClos("%REGISTER-CLASS", a => Runtime.RegisterClass(a[0]), 1);
        RegClos("%MAKE-SLOT-DEF", a => Runtime.MakeSlotDef(a[0], a[1], a[2]), 3);
        // The :ALLOCATION :CLASS variant of the slot-def lowering target. It was the
        // one member of this family with no function binding, so the DEFCLASS
        // expansion — which the tree-walk evaluator runs as real code — died with
        // "Undefined function: %MAKE-SLOT-DEF-WITH-ALLOCATION" as soon as a slot
        // carried :allocation (ansi-test CLASS-REDEFINITION.1/2/3). Compiled code
        // never noticed: the compiler emits Runtime.MakeSlotDefWithAllocation
        // inline from its own table and never looks the name up.
        RegClos("%MAKE-SLOT-DEF-WITH-ALLOCATION",
            a => Runtime.MakeSlotDefWithAllocation(a[0], a[1], a[2], a[3]), 4);
        // The custom-:METACLASS variant of the class lowering target, missing for the
        // same reason %MAKE-SLOT-DEF-WITH-ALLOCATION was: DEFCLASS emits it only when
        // the class names a metaclass, and compiled code never looks the name up
        // (the compiler emits Runtime.MakeClassFull inline). The tree-walk evaluator
        // runs the expansion as real code, so every (defclass ... (:metaclass ...))
        // died with "Undefined function: %MAKE-CLASS-FULL".
        RegClos("%MAKE-CLASS-FULL", a => Runtime.MakeClassFull(a[0], a[1], a[2], a[3]), 4);
        RegClos("%SLOT-DEF-RAW-OPTIONS", a => Runtime.SetSlotDefRawOptions(a[0], a[1]), 2);
        RegClos("%SLOT-DEF-ATTRS", a => Runtime.SetSlotDefAttrs(a[0], a[1], a[2], a[3], a[4]), 5);
        RegClos("%SLOT-DEF-DOC", a => Runtime.SetSlotDefDocumentation(a[0], a[1]), 2);
        RegClos("%SLOT-DEF-DOCUMENTATION", a => Runtime.SlotDefinitionDocumentation(a[0]), 1);
        // Used by DOCUMENTATION's default method, which is read by the host Lisp
        // during the cross compile and so cannot name the MOP package: anything that
        // is not a slot definition answers NIL and falls through to the table.
        RegClos("%SLOT-DEF-DOCUMENTATION-OR-NIL",
            a => a[0] is SlotDefinition sd ? sd.Documentation : Nil.Instance, 1);
        RegClos("%SET-CLASS-DEFAULT-INITARGS", a => Runtime.SetClassDefaultInitargs(a[0], a[1]), 2);
        RegClos("%FIND-CLASS-OR-NIL", a => Runtime.FindClassOrNil(a[0]), 1);
        RegClos("%SPECIALIZER-CLASS", a => Runtime.SpecializerClass(a[0]), 1);
        RegClos("%SET-SLOT-VALUE", a => Runtime.SetSlotValue(a[0], a[1], a[2]), 3);
        RegClos("%ALLOCATE-INSTANCE", a => Runtime.MakeInstanceRaw(a[0]), 1);
        RegClos("%SLOT-EXISTS-P", a => Runtime.SlotExists(a[0], a[1]), 2);
        RegClos("%CHANGE-CLASS", Runtime.ChangeClass);
        RegClos("%REGISTER-ACCESSOR-METHOD", a => Runtime.RegisterAccessorMethod(a[0], a[1], a[2]), 3);
        RegClos("%MAKE-GF", a => Runtime.MakeGF(a[0], a[1]), 2);
        RegClos("%REGISTER-GF", a => Runtime.RegisterGF(a[0], a[1]), 2);
        RegClos("%FIND-GF", a => Runtime.FindGF(a[0]), 1);
        RegClos("%SET-GF-LAMBDA-LIST-INFO", Runtime.SetGFLambdaListInfo);
        RegClos("%SET-METHOD-LAMBDA-LIST-INFO", Runtime.SetMethodLambdaListInfo);
        RegClos("%MAKE-METHOD", a => Runtime.MakeMethod(a[0], a[1], a[2]), 3);
        RegClos("%ADD-METHOD", a => Runtime.AddMethod(a[0], a[1]), 2);
        RegClos("%NOTE-METHOD-CLASS", a => Runtime.NoteMethodClass(a[0], a[1]), 2);
        RegClos("%NOTE-METHOD-COMBINATION", a => Runtime.NoteMethodCombination(a[0], a[1], a[2]), 3);
        RegClos("%MAKE-METHOD-LAMBDA-FOR", a => Runtime.MakeMethodLambdaFor(a[0], a[1]), 2);
        RegClos("%INTERN-EQL-SPECIALIZER", a => Runtime.InternEqlSpecializer(a[0]), 1);
        RegClos("%GF-METHODS", a => Runtime.GetGFMethods(a[0]), 1);
        RegClos("%METHOD-SPECIALIZERS", a => Runtime.MethodSpecializers(a[0]), 1);
        RegClos("%METHOD-QUALIFIERS", a => Runtime.MethodQualifiers(a[0]), 1);
        RegClos("%METHOD-FUNCTION", a => Runtime.MethodFunction(a[0]), 1);
        RegClos("%CLEAR-DEFGENERIC-INLINE-METHODS", a => Runtime.ClearDefgenericInlineMethods(a[0]), 1);
        RegClos("%MARK-DEFGENERIC-INLINE-METHOD", a => Runtime.MarkDefgenericInlineMethod(a[0], a[1]), 2);
        RegClos("%SET-GF-DECLARATIONS", a => Runtime.SetGFDeclarations(a[0], a[1]), 2);
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
        var findOrForward = new LispFunction(args => Runtime.FindOrForwardClass(args[0]),
                                             "%FIND-OR-FORWARD-CLASS", 1);
        // Typed 1-arg entry: DEFCLASS resolves every superclass through this.
        findOrForward.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.FindOrForwardClass);
        Emitter.CilAssembler.RegisterFunction("%FIND-OR-FORWARD-CLASS", findOrForward);
        Emitter.CilAssembler.RegisterFunction("(SETF FIND-CLASS)", new LispFunction(args => {
            if (args.Length < 2) throw new Exception("(SETF FIND-CLASS): too few arguments");
            var newVal = args[0];
            // Key by the ORIGINAL package-qualified symbol, never the bare-name-normalized
            // one — otherwise (setf (find-class 'pa::seq) ...) and (setf (find-class 'pb::seq) ...)
            // both land on DOTCL-INTERNAL::SEQ and the second clobbers the first. fset's
            // post.lisp aliases its classes into the FSET2 package exactly this way.
            var sym = (args[1] is Symbol orig && orig.HomePackage != null) ? orig : ToClassSymbol(args[1]);
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
                                // EQL specializers compare by the object they specialize
                                // on, so the metaobject and the list (EQL object) name
                                // the same method whichever side each is written as.
                                var mEql = EqlSpecializerValue(method.Specializers[i]);
                                var sEql = EqlSpecializerValue(sc.Car);
                                if (mEql != null || sEql != null)
                                {
                                    if (mEql == null || sEql == null || !IsTrueEql(mEql, sEql))
                                    { match = false; break; }
                                    specList = sc.Cdr;
                                    continue;
                                }
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

            // initialize-instance primary for GENERIC-FUNCTION. dotcl's own generic
            // functions have no Lisp slots and skip shared-initialize; one made from a
            // user-defined generic function class does have them (they live in
            // ExtraSlots), so for those the standard protocol runs and initargs and
            // initforms are applied.
            {
                var gfPrimCls = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var gfPrimM = Runtime.MakeMethod(new Cons(gfPrimCls, Nil.Instance), Nil.Instance,
                    new LispFunction(args =>
                    {
                        if (args[0] is GenericFunction userGf && userGf.StoredClass != null
                            && Startup.Sym("SHARED-INITIALIZE").Function is LispFunction siFn)
                        {
                            var siArgs = new LispObject[args.Length + 1];
                            siArgs[0] = args[0];
                            siArgs[1] = T.Instance;
                            Array.Copy(args, 1, siArgs, 2, args.Length - 1);
                            siFn.Invoke(siArgs);
                        }
                        return args[0];
                    }));
                ((LispMethod)gfPrimM).RequiredCount = 1;
                ((LispMethod)gfPrimM).HasRest = true;
                ((LispMethod)gfPrimM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, gfPrimM);
            }

            // initialize-instance primary for METHOD. A STANDARD-METHOD has no Lisp
            // slots and skips shared-initialize; one made from a user-defined method
            // class does have them (they live in ExtraSlots), so for those the standard
            // protocol runs and initargs and initforms are applied. Mirrors the
            // GENERIC-FUNCTION primary above.
            {
                var mPrimCls = Runtime.FindClass(Startup.Sym("METHOD"));
                var mPrimM = Runtime.MakeMethod(new Cons(mPrimCls, Nil.Instance), Nil.Instance,
                    new LispFunction(args =>
                    {
                        if (args[0] is LispMethod userMethod && userMethod.MetaClass != null
                            && Startup.Sym("SHARED-INITIALIZE").Function is LispFunction siFn)
                        {
                            var siArgs = new LispObject[args.Length + 1];
                            siArgs[0] = args[0];
                            siArgs[1] = T.Instance;
                            Array.Copy(args, 1, siArgs, 2, args.Length - 1);
                            siFn.Invoke(siArgs);
                        }
                        return args[0];
                    }));
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
                        ApplyGenericFunctionInitargs(ugf, args, renaming: false);
                        // A generic function with no methods yet never reaches
                        // ADD-METHOD, so this is the only point where a class of
                        // generic function that computes its own dispatch gets to
                        // do so before the first call.
                        NotifyDiscriminatingFunction(ugf);
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
                                    // AMOP: the function passed here takes (args
                                    // next-methods). Dispatch calls method functions
                                    // with the arguments spread, so what is stored is
                                    // an adapter; METHOD-FUNCTION still answers the
                                    // object that was passed in.
                                    if (args[i + 1] is LispFunction mf)
                                    {
                                        m.ProcessedParameterFunction = mf;
                                        m.Function = new LispFunction(callArgs =>
                                            mf.Invoke(new LispObject[] {
                                                Runtime.List(callArgs),
                                                Runtime.CurrentNextMethods() }),
                                            "method function adapter", -1);
                                    }
                                    break;
                                case "LAMBDA-LIST":
                                    ParseLambdaListIntoMethod(m, args[i + 1]);
                                    // Keep the list itself, not just the arity it
                                    // implies: METHOD-LAMBDA-LIST has to hand back
                                    // what was passed, and a rebuilt placeholder
                                    // (#:R0 ...) is not that.
                                    m.StoredLambdaList = args[i + 1];
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

            // reinitialize-instance :after for CLASS — applies :NAME, which is how
            // (SETF CLASS-NAME) changes a name. NIL clears the proper name rather than
            // naming the class NIL (CLHS ensure-class: redefinition only happens under
            // a class's proper name).
            {
                var riClsQuals = new Cons(Startup.Keyword("AFTER"), Nil.Instance);
                var riClsCls = Runtime.FindClass(Startup.Sym("CLASS"));
                var riClsM = Runtime.MakeMethod(new Cons(riClsCls, Nil.Instance), riClsQuals,
                    new LispFunction(args =>
                    {
                        if (args[0] is not LispClass target) return args[0];
                        for (int i = 1; i + 1 < args.Length; i += 2)
                        {
                            if (args[i] is not Symbol k || k.Name != "NAME") continue;
                            if (args[i + 1] is Symbol newSym) target.Name = newSym;
                            else if (args[i + 1] is Nil) target.NameCleared = true;
                            else target.Name = Startup.Sym(args[i + 1].ToString());
                        }
                        // AMOP: reinitializing a class that was already finalized
                        // finalizes it again, through the generic function so a
                        // metaclass method is heard.
                        if (!target.IsForwardReferenced
                            && Startup.Sym("FINALIZE-INHERITANCE").Function is LispFunction fi)
                            fi.Invoke(new LispObject[] { target });
                        NotifyDependents(target, SkipInstanceArg(args));
                        return target;
                    }));
                ((LispMethod)riClsM).RequiredCount = 1;
                ((LispMethod)riClsM).HasRest = true;
                ((LispMethod)riClsM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, riClsM);
            }

            // reinitialize-instance :after for GENERIC-FUNCTION
            // initialization applies, with :NAME now allowed to replace an existing
            // name: AMOP defines (SETF GENERIC-FUNCTION-NAME) as reinitialization.
            // The discriminating function is recomputed afterwards, since the lambda
            // list it was built for may have changed.
            {
                var riAfterQuals = new Cons(Startup.Keyword("AFTER"), Nil.Instance);
                var riGfCls = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var riGfM = Runtime.MakeMethod(new Cons(riGfCls, Nil.Instance), riAfterQuals,
                    new LispFunction(args =>
                    {
                        if (args[0] is not GenericFunction rgf) return args[0];
                        ApplyGenericFunctionInitargs(rgf, args, renaming: true);
                        NotifyDiscriminatingFunction(rgf);
                        NotifyDependents(rgf, SkipInstanceArg(args));
                        return rgf;
                    }));
                ((LispMethod)riGfM).RequiredCount = 1;
                ((LispMethod)riGfM).HasRest = true;
                ((LispMethod)riGfM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, riGfM);
            }
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
                        return MultipleValues.Values2(kwList, m.HasAllowOtherKeys ? (LispObject)T.Instance : Nil.Instance);
                    }
                    return MultipleValues.Values2(Nil.Instance, Nil.Instance);
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
            return SlotDefinitionName(args[0]);
        }, "SLOT-DEFINITION-NAME", 1));

        // SLOT-DEFINITION-TYPE
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-TYPE", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-TYPE: wrong arg count"));
            return SlotDefinitionType(args[0]);
        }, "SLOT-DEFINITION-TYPE", 1));

        // SLOT-DEFINITION-INITARGS
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITARGS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITARGS: wrong arg count"));
            return SlotDefinitionInitargs(args[0]);
        }, "SLOT-DEFINITION-INITARGS", 1));

        // SLOT-DEFINITION-INITFORM / SLOT-DEFINITION-INITFUNCTION
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITFORM", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITFORM: wrong arg count"));
            return SlotDefinitionInitform(args[0]);
        }, "SLOT-DEFINITION-INITFORM", 1));

        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITFUNCTION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITFUNCTION: wrong arg count"));
            return SlotDefinitionInitfunction(args[0]);
        }, "SLOT-DEFINITION-INITFUNCTION", 1));

        // SLOT-DEFINITION-ALLOCATION → :instance or :class
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-ALLOCATION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-ALLOCATION: wrong arg count"));
            return SlotDefinitionAllocation(args[0]);
        }, "SLOT-DEFINITION-ALLOCATION", 1));

        // SLOT-DEFINITION-READERS / SLOT-DEFINITION-WRITERS: the :reader/:accessor
        // and :writer/(setf accessor) names DEFCLASS parsed. Both used to be NIL.
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-READERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-READERS: wrong arg count"));
            return SlotDefinitionReaders(args[0]);
        }, "SLOT-DEFINITION-READERS", 1));
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-WRITERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-WRITERS: wrong arg count"));
            return SlotDefinitionWriters(args[0]);
        }, "SLOT-DEFINITION-WRITERS", 1));

        // CLASS-DIRECT-SLOTS: list of SlotDefinition objects for the class's own slots
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SLOTS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SLOTS: wrong arg count"));
            return ClassDirectSlots(args[0]);
        }, "CLASS-DIRECT-SLOTS", 1));

        // CLASS-SLOTS: list of effective SlotDefinition objects (all inherited slots)
        Emitter.CilAssembler.RegisterFunction("CLASS-SLOTS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-SLOTS: wrong arg count"));
            return ClassSlots(args[0]);
        }, "CLASS-SLOTS", 1));

        // CLASS-DIRECT-SUPERCLASSES
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SUPERCLASSES", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SUPERCLASSES: wrong arg count"));
            return ClassDirectSuperclasses(args[0]);
        }, "CLASS-DIRECT-SUPERCLASSES", 1));

        // CLASS-DIRECT-SUBCLASSES — the same implementation DOTCL-MOP exposes.
        // This used to be a stub returning NIL for every class, so the answer
        // depended on which symbol the caller reached: dotcl-mop:… was right and
        // the CL one silently wrong. GENERIC-FUNCTION-LAMBDA-LIST had the same
        // split and returned NIL only on Linux (see below).
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SUBCLASSES", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SUBCLASSES: wrong arg count"));
            return ClassDirectSubclasses(args[0]);
        }, "CLASS-DIRECT-SUBCLASSES", 1));

        // CLASS-PRECEDENCE-LIST
        Emitter.CilAssembler.RegisterFunction("CLASS-PRECEDENCE-LIST", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-PRECEDENCE-LIST: wrong arg count"));
            return ClassPrecedenceListOf(args[0]);
        }, "CLASS-PRECEDENCE-LIST", 1));

        // CLASS-FINALIZED-P — all dotcl classes are considered finalized
        Emitter.CilAssembler.RegisterFunction("CLASS-FINALIZED-P", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-FINALIZED-P: wrong arg count"));
            return ClassFinalizedP(args[0]);
        }, "CLASS-FINALIZED-P", 1));

        // CLASS-PROTOTYPE — make a prototype instance of a class
        Emitter.CilAssembler.RegisterFunction("CLASS-PROTOTYPE", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-PROTOTYPE: wrong arg count"));
            return ClassPrototypeOf(args[0]);
        }, "CLASS-PROTOTYPE", 1));

        // GENERIC-FUNCTION-METHODS
        Emitter.CilAssembler.RegisterFunction("GENERIC-FUNCTION-METHODS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("GENERIC-FUNCTION-METHODS: wrong arg count"));
            return GenericFunctionMethods(args[0]);
        }, "GENERIC-FUNCTION-METHODS", 1));

        // GENERIC-FUNCTION-NAME
        Emitter.CilAssembler.RegisterFunction("GENERIC-FUNCTION-NAME", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("GENERIC-FUNCTION-NAME: wrong arg count"));
            return GenericFunctionName(args[0]);
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

        // METHOD-GENERIC-FUNCTION — the generic function this method is attached
        // to, or NIL while it is unattached. The comment here used to say "not
        // tracked in dotcl" and the body returned NIL for every method, but
        // LispMethod.Owner has tracked it all along (ADD-METHOD sets it,
        // REMOVE-METHOD clears it), and DOTCL-MOP returned it correctly.
        Emitter.CilAssembler.RegisterFunction("METHOD-GENERIC-FUNCTION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-GENERIC-FUNCTION: wrong arg count"));
            return MethodGenericFunction(args[0]);
        }, "METHOD-GENERIC-FUNCTION", 1));

        // METHOD-LAMBDA-LIST — rebuilt from the recorded arity (dotcl does not
        // keep the source lambda list). Used to return NIL for every method here
        // while DOTCL-MOP returned the real shape.
        Emitter.CilAssembler.RegisterFunction("METHOD-LAMBDA-LIST", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-LAMBDA-LIST: wrong arg count"));
            return MethodLambdaList(args[0]);
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
            var (supersList, slotDefsList) = Runtime.ParseClassInitargs(supersSpec, slotsSpec);
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

