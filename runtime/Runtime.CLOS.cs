using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- CLOS operations ---

    // ConcurrentDictionary so concurrent DEFCLASS / FIND-CLASS doesn't corrupt the table.
    private static readonly ClassRegistry _classRegistry = new();

    /// <summary>The class table. Every store or removal bumps Epoch, which
    /// invalidates the per-symbol FIND-CLASS answers cached on Symbol.ClassMemo.</summary>
    private sealed class ClassRegistry : ConcurrentDictionary<Symbol, LispClass>
    {
        internal static int Epoch;
        public ClassRegistry() : base(SymbolIdentityComparer.Instance) { }
        public new LispClass this[Symbol key]
        {
            get => base[key];
            set { base[key] = value; System.Threading.Interlocked.Increment(ref Epoch); }
        }
        public new bool TryRemove(Symbol key, out LispClass value)
        {
            bool removed = base.TryRemove(key, out value!);
            System.Threading.Interlocked.Increment(ref Epoch);
            return removed;
        }
    }
    // Maps .NET runtime Type to its CLOS class (for dotnet:define-class and dotnet:new instances).
    private static readonly ConcurrentDictionary<Type, LispClass> _dotNetTypeRegistry = new();
    private static Symbol? _initializeInstanceSym;
    private static Symbol? _sharedInitializeSym;
    /// <summary>Custom method combination registry: name -> (operator name, identity-with-one-argument)</summary>
    private static readonly ConcurrentDictionary<string, (string Operator, bool IdentityWithOneArg)> _methodCombinationRegistry = new();

    /// <summary>Short-form combination name -> the operator symbol itself, for an
    /// operator that is not one of the built-in names (a user function or macro).
    /// Filled only when the registering code passed the symbol.</summary>
    private static readonly ConcurrentDictionary<string, Symbol> _methodCombinationOperatorSymbols = new();

    /// <summary>Long-form method combination registry: name -> LongFormMC</summary>
    private static readonly ConcurrentDictionary<string, LongFormMethodCombination> _longFormMCRegistry = new();

    public static void RegisterMethodCombination(string name, string operatorName, bool identityWithOneArg = false)
    {
        _methodCombinationRegistry[name] = (operatorName, identityWithOneArg);
        _methodCombinationOperatorSymbols.TryRemove(name, out _);
    }

    public static void RegisterMethodCombination(string name, string operatorName, bool identityWithOneArg, Symbol? operatorSymbol)
    {
        RegisterMethodCombination(name, operatorName, identityWithOneArg);
        if (operatorSymbol != null) _methodCombinationOperatorSymbols[name] = operatorSymbol;
    }

    // A combination is registered under its symbol's package-qualified name, so
    // same-named combinations of two package units stay apart. Combinations
    // registered under a bare name (built-ins, older fasls) are the fallback.
    private static string MethodCombinationKey(Symbol s) => TypeExpanderKey(s);

    private static bool TryGetMC<T>(ConcurrentDictionary<string, T> table, Symbol s, out T value)
        => table.TryGetValue(MethodCombinationKey(s), out value!) || table.TryGetValue(s.Name, out value!);

    /// <summary>Info for a long-form define-method-combination.</summary>
    internal class LongFormMethodCombination
    {
        /// <summary>Dynamic spec function: (mc-args-list) -> spec-list. Replaces static Groups.</summary>
        public LispFunction? SpecFunction;
        /// <summary>The body function: called with (mc-args... method-groups...) -> effective-method form</summary>
        public LispFunction? BodyFunction;
        /// <summary>(ARGS-VAR BINDINGS FRESH-VARS) for the :ARGUMENTS option, or null for
        /// a combination registered by an older compiler, whose body function takes the
        /// argument values and whose form is run by the private evaluator.</summary>
        public LispObject? ArgumentsInfo;
        /// <summary>Compiled effective methods, keyed by the method groups they were
        /// computed from.</summary>
        public readonly ConcurrentDictionary<EffectiveMethodKey, LispFunction> Cache = new();
    }

    /// <summary>Identity of an effective method: the generic function and the methods
    /// of each group, in order. Compared by reference.</summary>
    internal sealed class EffectiveMethodKey : IEquatable<EffectiveMethodKey>
    {
        private readonly object[] _items;
        private readonly int _hash;
        public EffectiveMethodKey(object[] items)
        {
            _items = items;
            int h = 17;
            foreach (var it in items)
                h = h * 31 + (it == null ? 0 : System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(it));
            _hash = h;
        }
        public bool Equals(EffectiveMethodKey? other)
        {
            if (other == null || other._items.Length != _items.Length) return false;
            for (int i = 0; i < _items.Length; i++)
                if (!ReferenceEquals(_items[i], other._items[i])) return false;
            return true;
        }
        public override bool Equals(object? obj) => Equals(obj as EffectiveMethodKey);
        public override int GetHashCode() => _hash;
    }

    internal class MethodGroupSpec
    {
        public string Name = "";
        /// <summary>The group's qualifier patterns (CLHS DEFINE-METHOD-COMBINATION):
        /// each is *, a list (NIL matches unqualified methods), or a symbol naming a
        /// predicate on the qualifier list. A method belongs to the group when any
        /// pattern matches.</summary>
        public List<LispObject> Patterns = new();
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
            // distinct same-named user classes, which DO get their own exact entry, see
            // FindOrForwardClass / RegisterClass, never fall through to here.
            // Do NOT apply for uninterned symbols (gensyms): they have unique identity
            // and converting them to interned symbols breaks forward-ref resolution.
            // Never into COMMON-LISP, though: a symbol named LIST that is not CL:LIST
            // does not name the class LIST (cl-l10n's LDML:LIST did).
            if (sym.HomePackage != null)
            {
                var byName = Startup.Sym(sym.Name);
                if (byName.HomePackage != null && byName.HomePackage.Name == "COMMON-LISP")
                    return sym;
                return byName;
            }
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
        // TYPEP and slot type checks ask this for the same few symbols over and
        // over; the answer only changes when the class table does.
        if (name is Symbol key)
        {
            int epoch = System.Threading.Volatile.Read(ref ClassRegistry.Epoch);
            var memo = key.ClassMemo;
            if (memo != null && memo.Epoch == epoch) return memo.Class;
            var found = FindClassOrNilUncached(key);
            key.ClassMemo = new Symbol.FindClassMemo(found, epoch);
            return found;
        }
        return FindClassOrNilUncached(name);
    }

    private static LispObject FindClassOrNilUncached(LispObject name)
    {
        var sym = ToClassSymbol(name);
        if (_classRegistry.TryGetValue(sym, out var cls))
            return cls;
        return Nil.Instance;
    }

    // Classes named by the top-level DEFCLASS forms of the files being compiled,
    // innermost COMPILE-FILE last. COMPILE-FILE does not create those classes
    // (they are made when the fasl is loaded), but CLHS DEFCLASS still asks that
    // FIND-CLASS given a macro's environment return the class definition while
    // the rest of the file is compiled. The entry is a bare placeholder carrying
    // only the name: it is never registered, never finalized, and is dropped
    // when that COMPILE-FILE returns.
    [ThreadStatic]
    private static Stack<Dictionary<Symbol, LispClass>>? s_compileTimeClassesTS;

    internal static void PushCompileTimeClasses()
        => (s_compileTimeClassesTS ??= new()).Push(new Dictionary<Symbol, LispClass>(SymbolIdentityComparer.Instance));

    internal static void PopCompileTimeClasses()
    {
        var stack = s_compileTimeClassesTS;
        if (stack != null && stack.Count > 0) stack.Pop();
    }

    /// <summary>%COMPILER-DEFCLASS (name): the compile-time effect of a top-level
    /// DEFCLASS under COMPILE-FILE. Outside COMPILE-FILE it does nothing.</summary>
    public static LispObject CompilerDefclass(LispObject name)
    {
        var stack = s_compileTimeClassesTS;
        if (stack == null || stack.Count == 0 || name is not Symbol sym) return Nil.Instance;
        var table = stack.Peek();
        if (!table.ContainsKey(sym))
            table[sym] = new LispClass(sym, Array.Empty<SlotDefinition>(), Array.Empty<LispClass>());
        return Nil.Instance;
    }

    private static LispObject FindCompileTimeClass(LispObject name)
    {
        var stack = s_compileTimeClassesTS;
        if (stack == null || stack.Count == 0 || name is not Symbol sym) return Nil.Instance;
        foreach (var table in stack)
            if (table.TryGetValue(sym, out var cls)) return cls;
        return Nil.Instance;
    }

    /// <summary>A DEFMETHOD parameter specializer designator -> the class to specialize on.
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
        var registered = RegisterClassCore(cls);
        // Readers and writers of slots that came through the metaobject protocol are
        // defined once the class can be found by its name.
        if (cls is LispClass lc && registered is LispClass target)
            DefinePendingAccessors(lc, target);
        return registered;
    }

    private static void InitializeDirectSharedSlotInitforms(LispClass cls)
    {
        foreach (var slot in cls.DirectSlots)
        {
            if (!slot.IsClassAllocation || slot.InitformThunk == null) continue;
            if (!cls.ClassSlotValues.ContainsKey(slot.Name.Name))
                cls.ClassSlotValues[slot.Name.Name] = slot.InitformThunk.Invoke();
        }
    }

    private static LispObject RegisterClassCore(LispObject cls)
    {
        if (cls is not LispClass lc)
            throw new LispErrorException(new LispTypeError("REGISTER-CLASS: not a class", cls));
        // CLHS TYPE: the class name must not already name a declaration. Every
        // class-defining operator lands here: defclass, defstruct and
        // define-condition all register a class.
        CheckTypeNameAvailable(lc.Name, "DEFCLASS");
        if (!lc.IsStructureClass) lc.Name.StructParent = null;
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
            // it: e.g. McCLIM resolving a forward-referenced-class to its real
            // presentation-type-class metaclass.
            existing.Metaclass = lc.Metaclass;
            existing.ExtraSlots = lc.ExtraSlots;
            existing.FinalizeClass();
            InitializeDirectSharedSlotInitforms(existing);
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
                InitializeDirectSharedSlotInitforms(fwdRef);
                RefinalizeDependents(fwdRef);
                return fwdRef;
            }
        }
        _classRegistry[lc.Name] = lc;
        NotifyDirectSubclasses(System.Array.Empty<LispClass>(), lc.DirectSuperclasses, lc);
        InitializeDirectSharedSlotInitforms(lc);
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

    /// <summary>Find a class by string name (linear scan): the first registered class
    /// whose name matches, from any package. Not used to resolve type specifiers (it
    /// would answer for a symbol that names no class with some other package's class);
    /// see FindSystemClassByName and ClassOfTypeSpecifier.</summary>
    public static LispClass? FindClassByName(string name)
    {
        foreach (var entry in _classRegistry)
            if (entry.Key.Name == name) return entry.Value;
        return null;
    }

    /// <summary>A class named NAME by dotcl's own symbols (CL first, then
    /// DOTCL-INTERNAL), for callers that only hold a type name string. Unlike
    /// FindClassByName this never answers with a user class from some other package
    /// that happens to share the name, and it does not intern anything.</summary>
    public static LispClass? FindSystemClassByName(string name)
    {
        var (clSym, clStatus) = Startup.CL.FindSymbol(name);
        if (clStatus != SymbolStatus.None && _classRegistry.TryGetValue(clSym, out var c1))
            return c1;
        var (inSym, inStatus) = Startup.Internal.FindSymbol(name);
        if (inStatus != SymbolStatus.None && _classRegistry.TryGetValue(inSym, out var c2))
            return c2;
        return null;
    }

    /// <summary>The class a type specifier names as a class: a class object itself, or a
    /// symbol that FIND-CLASS resolves. A symbol naming no class yields null even when a
    /// same-named class exists in another package.</summary>
    public static LispClass? ClassOfTypeSpecifier(LispObject spec) => spec switch
    {
        LispClass c => c,
        Symbol s => FindClassOrNil(s) as LispClass,
        _ => null,
    };

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
        // wildcard class users specialize on: otherwise it would register a second,
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
            // another .NET type's class: otherwise two same-named types from
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
        // first), then the T tail: dotcl's T standing for the object/lisp tail.
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
            // deterministic, load-order-independent specializer for EVERY type;
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

    /// <summary>Reconstruct a LispInstance from fasl: called by fasl load-time code.</summary>
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
    /// DOTCL-MOP:CLASS-DIRECT-SUBCLASSES so the two cannot drift apart; the
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
                result = new Cons(CanonicalValue(items[i]), result);
        return result;
    }

    public static LispObject ClassDirectSlots(LispObject arg)
        => ToLispList(RequireClass("CLASS-DIRECT-SLOTS", arg).DirectSlots);

    public static LispObject ClassSlots(LispObject arg)
        => ToLispList(RequireClass("CLASS-SLOTS", arg).EffectiveSlots);

    public static LispObject ClassDirectSuperclasses(LispObject arg)
        => ToLispList(RequireClass("CLASS-DIRECT-SUPERCLASSES", arg).DirectSuperclasses);

    /// <summary>The CPL, falling back to (C) for a class whose list has not been
    /// computed: without that, DOTCL-MOP's version dereferenced a null array.</summary>
    public static LispObject ClassPrecedenceListOf(LispObject arg)
    {
        var c = RequireClass("CLASS-PRECEDENCE-LIST", arg);
        var cpl = c.ClassPrecedenceList;
        if (cpl == null || cpl.Length == 0) return new Cons(c, Nil.Instance);
        return ToLispList(cpl);
    }

    /// <summary>dotcl finalizes eagerly at the end of DEFCLASS, so a class that exists
    /// is normally finalized. It is not while it is being made (a metaclass's
    /// INITIALIZE-INSTANCE runs before finalization, and CLASS-SLOTS is still empty
    /// there), when it is forward-referenced, or when one of its superclasses is:
    /// in all of those the precedence list has not been computed yet.</summary>
    public static LispObject ClassFinalizedP(LispObject arg)
        => IsClassFinalized(RequireClass("CLASS-FINALIZED-P", arg)) ? T.Instance : Nil.Instance;

    internal static bool IsClassFinalized(LispClass c)
        => !c.IsForwardReferenced && c.ClassPrecedenceList.Length > 0;

    /// <summary>The first forward-referenced class among C and its superclasses
    /// (depth first), or null when every class there is defined. Such a class
    /// cannot be finalized until that one is.</summary>
    internal static LispClass? ForwardReferencedAncestor(LispClass c)
    {
        var seen = new HashSet<LispClass>(ReferenceEqualityComparer.Instance);
        var stack = new Stack<LispClass>();
        stack.Push(c);
        while (stack.Count > 0)
        {
            var k = stack.Pop();
            if (!seen.Add(k)) continue;
            if (k.IsForwardReferenced) return k;
            if (IsClassFinalized(k)) continue;
            foreach (var s in k.DirectSuperclasses) stack.Push(s);
        }
        return null;
    }

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
        => PublicFunctionName(RequireGf("GENERIC-FUNCTION-NAME", arg).Name);

    private static SlotDefinition RequireSlotDef(string who, LispObject arg)
        => arg as SlotDefinition ?? throw new LispErrorException(new LispTypeError(
            $"{who}: not a slot definition", arg, Startup.Sym("SLOT-DEFINITION")));

    public static LispObject SlotDefinitionName(LispObject arg)
        => CanonicalizeSymbol(RequireSlotDef("SLOT-DEFINITION-NAME", arg).Name);

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

    /// <summary>A method's unspecialized lambda list: the one DEFMETHOD or
    /// MAKE-INSTANCE recorded, else (a method from an older FASL) one of the
    /// right shape rebuilt from the recorded arity. Shared by
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
        var subs = new List<LispClass>();
        foreach (var cls in AllClasses())
            if (Array.IndexOf(cls.DirectSuperclasses, c) >= 0)
                subs.Add(cls);
        subs.Sort((a, b) => b.CreationSeq.CompareTo(a.CreationSeq));
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
        bool llSeen = false, declSeen = false, nameSeen = false;
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is not Symbol ks) continue;
            if (!llSeen && ks.Name == "LAMBDA-LIST")
            {
                ParseLambdaListIntoGF(gf, args[i + 1]);
                llSeen = true;
            }
            else if (!declSeen && ks.Name == "DECLARATIONS")
            {
                gf.Declarations = args[i + 1];
                declSeen = true;
            }
            else if (!nameSeen && ks.Name == "NAME" && args[i + 1] is Symbol ns
                     && (renaming || gf.Name.Name == "UNNAMED"))
            {
                gf.Name = ns;
                ns.Function = gf;
                Runtime.RegisterGF(ns, gf);
                nameSeen = true;
            }
            else if (!nameSeen && ks.Name == "NAME" && args[i + 1] is Cons setfName
                     && setfName.Car is Symbol setfKw && setfKw.Name == "SETF"
                     && setfName.Cdr is Cons { Car: Symbol, Cdr: Nil }
                     && (renaming || gf.Name.Name == "UNNAMED"))
            {
                // (SETF accessor): the name is kept the way DEFGENERIC keeps it
                // for a generic function of the default class.
                gf.Name = Runtime.ToFunctionNameSymbol(setfName, "GENERIC-FUNCTION-NAME");
                Runtime.RegisterGF(setfName, gf);
                nameSeen = true;
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
    /// (no specializer->method back-link is maintained, so we scan).</summary>
    public static IEnumerable<GenericFunction> AllGenericFunctions() => _gfRegistry.Values;

    /// <summary>Drop every generic function's cached dispatch, named or not: a class's
    /// precedence list or instance layout changed, so the methods (or slot positions)
    /// cached for it, and for its subclasses, which are re-finalized and come here
    /// too, may no longer be right.</summary>
    internal static void InvalidateAllDispatchCaches() => GenericFunction.InvalidateAllCaches();

    public static void InternClassByName(string name, LispObject cls)
    {
        if (cls is LispClass lc)
            _classRegistry[Startup.Sym(name)] = lc;
        // If not a LispClass (e.g. NIL to remove), ignore for now
    }

    public static LispObject MakeClass(LispObject name, LispObject supersList, LispObject slotDefsList)
        => MakeClassCore(name, supersList, slotDefsList, null);

    // --- Macro function registry ---

    private static readonly ConcurrentDictionary<Symbol, LispFunction> _macroFunctions = new();

    // The macro each expander was first registered for, so FUNCTION-LAMBDA-EXPRESSION
    // can name it (MACRO-FUNCTION name) as SBCL does. Weak: an expander a redefinition
    // replaced goes away with it.
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispFunction, Symbol>
        _macroFunctionNames = new();

    // Bumped whenever a global macro or symbol macro is defined or removed. The
    // tree-walk evaluator keeps the expansions it made (%MINI-MACROEXPAND-1-CACHED)
    // and drops one made under an older epoch, so a redefinition is seen by the
    // next evaluation of a form, as it was when every evaluation expanded afresh.
    internal static int MacroEpoch;

    public static void RegisterMacroFunction(Symbol sym, LispFunction fn)
    {
        CheckPackageLock(sym, "DEFMACRO");
        _macroFunctions[sym] = fn;
        System.Threading.Interlocked.Increment(ref MacroEpoch);
        // GetValue rather than AddOrUpdate: the latter is not in netstandard2.0.
        _macroFunctionNames.GetValue(fn, _ => sym);
    }

    /// <summary>The macro FN was registered as the expander of, or null.</summary>
    public static Symbol? MacroFunctionName(LispFunction fn)
        => _macroFunctionNames.TryGetValue(fn, out var sym) ? sym : null;

    public static void UnregisterMacroFunction(Symbol sym)
    {
        CheckPackageLock(sym, "FMAKUNBOUND");
        _macroFunctions.TryRemove(sym, out _);
        System.Threading.Interlocked.Increment(ref MacroEpoch);
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

    // 1-arg direct entry for MACRO-FUNCTION: (macro-function name). Replicates
    // the args-array registration's single-argument branch exactly, so the
    // compiler's per-form macro check ((macro-function sym), very hot) skips the
    // args array and the InvokeSlow detour. The 2-arg (&optional environment)
    // form stays on the variadic registration wrapper.
    public static LispObject MacroFunction1(LispObject name)
    {
        var result = MacroFunction(name);
        if (result != Nil.Instance) return result;
        var sym = GetSymbol(name, "MACRO-FUNCTION");
        var compilerFn = Startup.LookupCompilerMacroAsMacro(sym);
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
        System.Threading.Interlocked.Increment(ref MacroEpoch);
    }

    public static bool TryGetGlobalSymbolMacro(Symbol sym, out LispObject expansion)
    {
        return _globalSymbolMacros.TryGetValue(sym, out expansion!);
    }
}

