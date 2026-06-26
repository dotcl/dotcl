namespace DotCL;

/// <summary>
/// CLOS slot definition: name, initarg, initform thunk.
/// </summary>
public class SlotDefinition : LispObject
{
    public Symbol Name { get; }
    public Symbol[] Initargs { get; }
    public LispFunction? InitformThunk { get; }
    /// <summary>True when :allocation :class was specified (shared slot stored on class, not instance).</summary>
    public bool IsClassAllocation { get; set; }

    /// <summary>True for effective slot definitions (STANDARD-EFFECTIVE-SLOT-DEFINITION),
    /// false for direct slot definitions (STANDARD-DIRECT-SLOT-DEFINITION).</summary>
    public bool IsEffective { get; set; }

    /// <summary>Index of this slot in the instance layout (LispInstance.Slots),
    /// set during class finalization. -1 for :class-allocation slots and direct
    /// slot definitions (not in any instance layout). Returned by the AMOP
    /// SLOT-DEFINITION-LOCATION accessor and used by STANDARD-INSTANCE-ACCESS.</summary>
    public int Location { get; set; } = -1;

    /// <summary>The CLOS class of this slot-definition metaobject when customized
    /// via direct-/effective-slot-definition-class (a subclass of standard-{direct,
    /// effective}-slot-definition). null = the standard class implied by IsEffective.
    /// CLASS-OF and TYPEP consult this so methods can dispatch on the slotd's class
    /// (e.g. slot-value-using-class specialized on a custom effective-slot).</summary>
    public LispClass? MetaClass { get; set; }

    /// <summary>Storage for the Lisp-level slots introduced by a custom slot-definition
    /// class (e.g. McCLIM's DYNAMIC-DIRECT-SLOT/DYNAMIC-EFFECTIVE-SLOT add a DYNAMIC
    /// slot). Keyed by slot name; null until the slotd gets a custom MetaClass. SLOT-VALUE
    /// / (SETF SLOT-VALUE) / SLOT-BOUNDP route through this for SlotDefinition objects.</summary>
    public Dictionary<string, LispObject?>? ExtraSlots { get; set; }

    /// <summary>The canonical slot-option plist (a Lisp list :key val ...) captured from the
    /// DEFCLASS slot specifier, used as the &rest initargs when DIRECT-SLOT-DEFINITION-CLASS
    /// is consulted for a custom metaclass. Null for slots defined under STANDARD-CLASS.</summary>
    public LispObject? RawOptions { get; set; }

    public SlotDefinition(Symbol name, Symbol[]? initargs = null, LispFunction? initformThunk = null, bool isClassAllocation = false)
    {
        Name = name;
        Initargs = initargs ?? Array.Empty<Symbol>();
        InitformThunk = initformThunk;
        IsClassAllocation = isClassAllocation;
    }

    public override string ToString() => $"#<SLOT-DEFINITION {Name.Name}>";
}

/// <summary>
/// CLOS class metaobject: name, slots, CPL, superclasses.
/// </summary>
public class LispClass : LispObject
{
    public Symbol Name { get; set; }
    /// <summary>True when (setf (class-name ...) nil) was called to clear the proper name.</summary>
    public bool NameCleared { get; set; }
    /// <summary>The metaclass of this class. Null means STANDARD-CLASS (default).</summary>
    public LispClass? Metaclass { get; set; }
    public SlotDefinition[] DirectSlots { get; set; }
    public LispClass[] DirectSuperclasses { get; set; }
    public LispClass[] ClassPrecedenceList { get; set; }
    public SlotDefinition[] EffectiveSlots { get; set; }
    public Dictionary<string, int> SlotIndex { get; }
    /// <summary>True for built-in classes (BUILT-IN-CLASS metaclass). False for user-defined (STANDARD-CLASS).</summary>
    public bool IsBuiltIn { get; set; }
    /// <summary>True for structure classes (STRUCTURE-CLASS metaclass).</summary>
    public bool IsStructureClass { get; set; }
    /// <summary>True for forward-referenced classes (superclass not yet defined).</summary>
    public bool IsForwardReferenced { get; set; }
    /// <summary>Slot names for #S reader macro support.</summary>
    public Symbol[]? StructSlotNames { get; set; }
    /// <summary>Direct default initargs defined by this class (before inheritance merge).</summary>
    public (Symbol Key, LispFunction Thunk)[] DirectDefaultInitargs { get; set; } = Array.Empty<(Symbol, LispFunction)>();
    /// <summary>Effective default initargs (merged from CPL, most specific first).</summary>
    public (Symbol Key, LispFunction Thunk)[] DefaultInitargs { get; set; } = Array.Empty<(Symbol, LispFunction)>();
    /// <summary>Storage for :allocation :class slots (shared across all instances).</summary>
    public Dictionary<string, LispObject?> ClassSlotValues { get; } = new();
    /// <summary>Slot values this class holds as an instance of its (custom) metaclass —
    /// i.e. slots the metaclass adds beyond STANDARD-CLASS. Null until populated.
    /// Lets slot-value on a class metaobject read metaclass-defined slots,
    /// mirroring SlotDefinition.ExtraSlots.</summary>
    public Dictionary<string, LispObject?>? ExtraSlots { get; set; }
    /// <summary>Cached mapping from initarg name to slot index (only valid when each initarg maps to one slot).</summary>
    public Dictionary<string, int>? InitargToSlotIndex { get; set; }
    /// <summary>True if this class can use the fast make-instance path.</summary>
    public bool HasSimpleInitialization { get; set; }
    /// <summary>Whether the GF method check for simple init has been performed.</summary>
    public bool SimpleInitChecked { get; set; }
    /// <summary>Cached result of the GF method check for simple init.</summary>
    public bool SimpleInitValid { get; set; }

    /// <summary>Cached mapping from initarg keyword name to slot index for fast make-instance path.
    /// Only includes instance-allocated slots (not :allocation :class).</summary>
    private Dictionary<string, int>? _initargSlotMap;
    /// <summary>True if all slots are instance-allocated and no initargs use NIL as key.
    /// When false, the fast make-instance path must be skipped.</summary>
    private bool? _canUseFastPath;
    /// <summary>Cached class prototype (AMOP class-prototype): a single per-class
    /// instance reused across calls. Must be stable — define-presentation-method
    /// (McCLIM) and other code dispatch via (eql class-prototype), which only
    /// works if the same object is returned every time. Lazily created.</summary>
    private LispInstance? _prototype;
    public LispInstance Prototype => _prototype ??= new LispInstance(this);
    /// <summary>Cached result of HasCustomInitMethods check. Null = not yet computed.</summary>
    internal bool? CachedHasCustomInitMethods;
    /// <summary>Cached result of IsConditionClass check. Null = not yet computed.</summary>
    internal bool? CachedIsConditionClass;
    /// <summary>Cached set of valid initarg key names for ValidateInitargs.</summary>
    internal HashSet<string>? CachedValidInitargKeys;
    public Dictionary<string, int> InitargSlotMap
    {
        get
        {
            if (_initargSlotMap == null)
                BuildInitargCache();
            return _initargSlotMap!;
        }
    }
    public bool CanUseFastMakeInstance
    {
        get
        {
            if (_canUseFastPath == null)
                BuildInitargCache();
            return _canUseFastPath!.Value;
        }
    }
    private void BuildInitargCache()
    {
        _initargSlotMap = new Dictionary<string, int>();
        _canUseFastPath = true;
        for (int i = 0; i < EffectiveSlots.Length; i++)
        {
            var slot = EffectiveSlots[i];
            if (slot.IsClassAllocation && slot.Initargs.Length > 0)
            {
                // Class-allocated slots with initargs can't use fast path
                _canUseFastPath = false;
            }
            if (!slot.IsClassAllocation)
            {
                foreach (var ia in slot.Initargs)
                {
                    _initargSlotMap.TryAdd(ia.Name, i);
                }
            }
        }
    }

    public LispClass(Symbol name, SlotDefinition[] directSlots, LispClass[] directSuperclasses)
    {
        Name = name;
        DirectSlots = directSlots;
        DirectSuperclasses = directSuperclasses;
        ClassPrecedenceList = Array.Empty<LispClass>();
        EffectiveSlots = Array.Empty<SlotDefinition>();
        SlotIndex = new Dictionary<string, int>();
    }

    /// <summary>
    /// Compute CPL using C3 linearization and build effective slots.
    /// Called after construction once all superclasses are registered.
    /// </summary>
    public void FinalizeClass()
    {
        ClassPrecedenceList = ComputeCPL();
        EffectiveSlots = ComputeEffectiveSlots();
        _initargSlotMap = null; // invalidate cached initarg→slot mapping
        _canUseFastPath = null;
        CachedHasCustomInitMethods = null;
        CachedIsConditionClass = null;
        CachedValidInitargKeys = null;
        SlotIndex.Clear();
        for (int i = 0; i < EffectiveSlots.Length; i++)
        {
            SlotIndex[EffectiveSlots[i].Name.Name] = i;
            // Instance-allocated slots get their layout index as location; :class
            // allocation slots are not in the per-instance vector.
            EffectiveSlots[i].Location = EffectiveSlots[i].IsClassAllocation ? -1 : i;
        }
        ComputeEffectiveDefaultInitargs();

        // Build initarg-to-slot cache for fast make-instance path
        InitargToSlotIndex = new Dictionary<string, int>();
        bool hasSharedInitarg = false;
        for (int i = 0; i < EffectiveSlots.Length; i++)
        {
            foreach (var ia in EffectiveSlots[i].Initargs)
            {
                if (InitargToSlotIndex.ContainsKey(ia.Name))
                    hasSharedInitarg = true;
                else
                    InitargToSlotIndex[ia.Name] = i;
            }
        }

        // Fast path: no default initargs, no shared initargs, no :class allocation slots
        HasSimpleInitialization = DefaultInitargs.Length == 0
            && !hasSharedInitarg
            && !Array.Exists(EffectiveSlots, s => s.IsClassAllocation);
        SimpleInitChecked = false;
    }

    /// <summary>
    /// Merge default-initargs from CPL (most specific first, first wins for same key).
    /// </summary>
    public void ComputeEffectiveDefaultInitargs()
    {
        var seen = new HashSet<string>();
        var result = new List<(Symbol Key, LispFunction Thunk)>();
        foreach (var cls in ClassPrecedenceList)
        {
            foreach (var (key, thunk) in cls.DirectDefaultInitargs)
            {
                if (seen.Add(key.Name))
                    result.Add((key, thunk));
            }
        }
        DefaultInitargs = result.ToArray();
    }

    private LispClass[] ComputeCPL()
    {
        // CLHS 4.3.5 CLOS class precedence list linearization (NOT C3).
        //
        // This is the standard ANSI algorithm, which is non-monotonic: a class
        // can precede one of its direct superclasses' more-specific neighbours
        // when a later branch demands it (see ANSI CLASS-0306). C3 would block
        // that and produce a different order, so we implement 4.3.5 verbatim.
        //
        // Step 1: Sc = transitive set of superclasses (including this class).
        // Step 2: Local precedence order R: for each class C with direct
        //   superclasses (D1 D2 ... Dn), the pairs C<D1, D1<D2, ..., D(n-1)<Dn.
        // Step 3: Topological sort of Sc respecting R. Tie-break among classes
        //   with no remaining predecessor: pick the one that is a direct
        //   superclass of the right-most (most recently placed) class in the
        //   partial CPL that has such a candidate as a direct superclass.

        // Step 1: collect all superclasses (this + transitive direct supers).
        var sc = new List<LispClass>();
        var scSet = new HashSet<LispClass>(ReferenceEqualityComparer.Instance);
        void Collect(LispClass c)
        {
            if (!scSet.Add(c)) return;
            sc.Add(c);
            foreach (var s in c.DirectSuperclasses)
                Collect(s);
        }
        Collect(this);

        // Step 2: build local precedence pairs (predecessor -> set of successors)
        // and a predecessor count per class restricted to Sc.
        var successors = new Dictionary<LispClass, List<LispClass>>(ReferenceEqualityComparer.Instance);
        var predCount = new Dictionary<LispClass, int>(ReferenceEqualityComparer.Instance);
        foreach (var c in sc)
        {
            successors[c] = new List<LispClass>();
            predCount[c] = 0;
        }
        foreach (var c in sc)
        {
            // C < D1 (class precedes its first direct super) and Di < D(i+1).
            LispClass prev = c;
            foreach (var d in c.DirectSuperclasses)
            {
                successors[prev].Add(d);
                predCount[d] = predCount[d] + 1;
                prev = d;
            }
        }

        // Step 3: topological sort with the "most-recently-placed direct super" tie-break.
        var result = new List<LispClass>(sc.Count);
        var placed = new HashSet<LispClass>(ReferenceEqualityComparer.Instance);
        int remaining = sc.Count;
        while (remaining > 0)
        {
            // Candidates: in Sc, not yet placed, with no remaining predecessors.
            var candidates = new List<LispClass>();
            foreach (var c in sc)
                if (!placed.Contains(c) && predCount[c] == 0)
                    candidates.Add(c);

            if (candidates.Count == 0)
                throw new LispErrorException(new LispError(
                    $"Cannot compute CPL for {Name.Name}: inconsistent precedence graph"));

            LispClass chosen;
            if (candidates.Count == 1)
            {
                chosen = candidates[0];
            }
            else
            {
                // Tie-break: scan the partial CPL from most-recently-placed back to
                // the front; pick the first candidate that is a direct superclass of
                // some already-placed class encountered in that scan.
                chosen = null!;
                for (int i = result.Count - 1; i >= 0 && chosen == null; i--)
                {
                    var rp = result[i];
                    foreach (var d in rp.DirectSuperclasses)
                    {
                        if (candidates.Contains(d))
                        {
                            chosen = d;
                            break;
                        }
                    }
                }
                // No placed class has any candidate as a direct super (only happens
                // for the very first pick, which is this class) — take the first.
                if (chosen == null)
                    chosen = candidates[0];
            }

            result.Add(chosen);
            placed.Add(chosen);
            remaining--;
            // Removing `chosen` satisfies the predecessor edges out of it.
            foreach (var succ in successors[chosen])
                predCount[succ] = predCount[succ] - 1;
        }

        return result.ToArray();
    }

    /// <summary>Hook for the COMPUTE-EFFECTIVE-SLOT-DEFINITION metaobject protocol.
    /// Set by Runtime CLOS init. When a class has a custom metaclass, FinalizeClass
    /// routes each slot's effective-definition construction through this delegate
    /// (which calls the Lisp GF) instead of building it directly in C#.</summary>
    public static Func<LispClass, Symbol, SlotDefinition[], SlotDefinition?>? ComputeEffectiveSlotHook;

    /// <summary>Build the standard effective slot definition by merging the per-name
    /// direct slot definitions (most-specific first), per CLHS 7.5.3. Shared by the
    /// default C# path and the default COMPUTE-EFFECTIVE-SLOT-DEFINITION method.</summary>
    public static SlotDefinition BuildEffectiveSlot(Symbol name, IReadOnlyList<SlotDefinition> defs)
    {
        var primary = defs[0]; // most specific

        // Union of all initargs
        var allInitargs = new List<Symbol>();
        var seenInitargs = new HashSet<string>();
        foreach (var d in defs)
            foreach (var ia in d.Initargs)
                if (seenInitargs.Add(ia.Name))
                    allInitargs.Add(ia);

        // Most specific initform (first one that has it)
        LispFunction? initform = null;
        foreach (var d in defs)
        {
            if (d.InitformThunk != null)
            {
                initform = d.InitformThunk;
                break;
            }
        }

        return new SlotDefinition(
            name,
            allInitargs.Count > 0 ? allInitargs.ToArray() : null,
            initform,
            primary.IsClassAllocation) { IsEffective = true };
    }

    private SlotDefinition[] ComputeEffectiveSlots()
    {
        // Per CLHS 7.5.3: merge slot definitions from CPL
        // - Initargs: union of all initargs across CPL
        // - Initform: from the most specific class that provides one
        // - Allocation: from the most specific class (default :instance)
        var slotOrder = new List<string>();
        var slotDefs = new Dictionary<string, List<SlotDefinition>>();
        foreach (var cls in ClassPrecedenceList)
        {
            foreach (var slot in cls.DirectSlots)
            {
                if (!slotDefs.ContainsKey(slot.Name.Name))
                {
                    slotOrder.Add(slot.Name.Name);
                    slotDefs[slot.Name.Name] = new List<SlotDefinition>();
                }
                slotDefs[slot.Name.Name].Add(slot);
            }
        }

        // For a custom metaclass, drive each slot through the COMPUTE-EFFECTIVE-SLOT-DEFINITION
        // protocol (AMOP). Standard classes (Metaclass == null) keep the direct C# path,
        // which also avoids GF calls during bootstrap and for the common case.
        bool useProtocol = Metaclass != null && ComputeEffectiveSlotHook != null;

        var slots = new List<SlotDefinition>();
        foreach (var name in slotOrder)
        {
            var defs = slotDefs[name];
            SlotDefinition? effective = null;
            if (useProtocol)
                effective = ComputeEffectiveSlotHook!(this, defs[0].Name, defs.ToArray());
            effective ??= BuildEffectiveSlot(defs[0].Name, defs);
            slots.Add(effective);
        }
        return slots.ToArray();
    }

    public override string ToString() => $"#<STANDARD-CLASS {Name.Name}>";
}

/// <summary>
/// CLOS instance: class pointer + slot array.
/// </summary>
public class LispInstance : LispObject
{
    public LispClass Class { get; set; }
    public LispObject?[] Slots { get; set; }

    public LispInstance(LispClass cls)
    {
        Class = cls;
        Slots = new LispObject?[cls.EffectiveSlots.Length];
        // null = unbound
        DotCL.Diagnostics.AllocCounter.Inc("LispInstance");
    }

    public override string ToString() => $"#<{Class.Name.Name}>";

    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, System.WeakReference<LispInstance>>
        _internCache = new();

    // NOTE: deliberately does NOT populate _internCache. CLHS 3.2.4.2 requires the
    // make-load-form creation form to be evaluated at LOAD time. Registering the
    // compile-time instance here (the emitter calls this during compile-file, which
    // shares a process with load under --asm / same-image ansi-test) made
    // InternViaEval cache-HIT on the compile-time object and SKIP the load-time eval,
    // so creation-form side effects (e.g. the :creating push in MAKE-LOAD-FORM.ORDER)
    // never ran and the loaded object was the compile-time object, not a load-time
    // reconstruction. EQ-ness across multiple references within one FASL is preserved
    // by InternViaEval itself: the first reference evaluates+caches the load-time
    // instance by key, later references (emitted with a Nil form) look it up.
    public static void PreRegisterIntern(string key, LispInstance inst)
    {
        // intentionally empty — see note above
    }

    /// <summary>FASL load-time: evaluate make-load-form creation form once and cache by key.</summary>
    public static LispObject InternViaEval(string key, LispObject creationForm)
    {
        if (_internCache.TryGetValue(key, out var weakRef) && weakRef.TryGetTarget(out var existing))
            return existing;
        // Nil means "just look up" — creation form already evaluated elsewhere
        if (creationForm is Nil) return Nil.Instance;
        var obj = Runtime.Eval(creationForm);
        if (obj is LispInstance result)
        {
            _internCache[key] = new System.WeakReference<LispInstance>(result);
            return result;
        }
        return obj;
    }
}

/// <summary>
/// CLOS method: specializers + qualifiers + function body.
/// </summary>
public class LispMethod : LispObject
{
    public LispObject[] Specializers { get; set; }  // LispClass or (eql value) cons
    public Symbol[] Qualifiers { get; set; }         // :BEFORE, :AFTER, :AROUND, or empty
    public LispFunction Function { get; set; }
    public int RequiredCount { get; set; }
    public int OptionalCount { get; set; }
    public bool HasRest { get; set; }
    public bool HasKey { get; set; }
    public bool HasAllowOtherKeys { get; set; }
    public List<string> KeywordNames { get; set; } = new();
    public GenericFunction? Owner { get; set; }
    /// <summary>True if this method was defined by an inline :method in defgeneric.</summary>
    public bool IsFromDefgenericInline { get; set; }

    public LispMethod(LispObject[] specializers, Symbol[] qualifiers, LispFunction function)
    {
        Specializers = specializers;
        Qualifiers = qualifiers;
        Function = function;
    }

    public LispMethod() {
        Specializers = Array.Empty<LispObject>();
        Qualifiers = Array.Empty<Symbol>();
        Function = null!;
    }

    public override string ToString() => "#<METHOD>";
}

/// <summary>
/// Generic function: dispatches to methods based on argument classes.
/// The actual dispatch logic is in Lisp (cil-stdlib.lisp).
/// </summary>
/// <summary>Cached result of GF dispatch for a specific argument type signature.</summary>
internal class CachedDispatch
{
    public LispClass?[] ArgTypes;
    public List<LispMethod> Around;
    public List<LispMethod> Before;
    public List<LispMethod> Primary;
    public List<LispMethod> After;
    public List<LispMethod>? Applicable; // for built-in method combination
    public bool HasEqlSpecializers;
    public bool IsBuiltinCombination;
    /// <summary>EQL-specialized methods to check on cache hit (only when HasEqlSpecializers).</summary>
    public LispMethod[]? EqlMethods;
}

public class GenericFunction : LispFunction
{
    public new Symbol Name { get; }

    // Method list is copy-on-write for thread safety: dispatch reads an
    // immutable snapshot (the `Methods` property returns the current array, which
    // foreach/Count/[i] enumerate consistently even if a concurrent defmethod swaps
    // it), while mutations (ADD-METHOD / REMOVE-METHOD / defgeneric-inline clear)
    // build a new array under `MethodsLock` and atomically publish it via the
    // volatile field. A plain List<T> here let concurrent enumerate-vs-Add corrupt
    // the applicable-method set → spurious "CALL-NEXT-METHOD: no next method".
    private readonly object _methodsLock = new();
    private volatile LispMethod[] _methods = System.Array.Empty<LispMethod>();
    /// <summary>Read-only snapshot of the GF's methods. Enumeration is consistent:
    /// the property reads the volatile array reference once, so a concurrent
    /// ReplaceMethods swap cannot tear an in-progress loop.</summary>
    public IReadOnlyList<LispMethod> Methods => _methods;
    /// <summary>Lock held while building+publishing a new method array (write path only).</summary>
    internal object MethodsLock => _methodsLock;
    /// <summary>Publish a new method array (volatile write). Call under MethodsLock.</summary>
    internal void ReplaceMethods(LispMethod[] methods) => _methods = methods;
    public LispFunction? DispatchFunction { get; set; }
    /// <summary>Method combination type: null means STANDARD, otherwise the operator symbol (+, LIST, APPEND, etc.)</summary>
    public Symbol? MethodCombination { get; set; }
    /// <summary>Method combination arguments from defgeneric (:method-combination name arg1 arg2 ...)</summary>
    public LispObject[]? MethodCombinationArgs { get; set; }
    /// <summary>:argument-precedence-order as a permutation of required-parameter
    /// indices (CLHS 7.6.6.1.2). null means natural left-to-right order.</summary>
    public int[]? ArgumentPrecedenceOrder { get; set; }
    /// <summary>Method combination order: true = most-specific-first (default), false = most-specific-last</summary>
    public bool MostSpecificFirst { get; set; } = true;
    /// <summary>Lambda list structure for congruence checking (CLHS 7.6.4)</summary>
    public int RequiredCount { get; set; }
    public int OptionalCount { get; set; }
    public bool HasRest { get; set; }
    public bool HasKey { get; set; }
    public bool HasAllowOtherKeys { get; set; }
    public List<string> KeywordNames { get; set; } = new();
    public bool LambdaListInfoSet { get; set; }
    /// <summary>Stored lambda-list from :lambda-list initarg (MOP accessor).</summary>
    public LispObject? StoredLambdaList { get; set; }
    /// <summary>Actual Lisp class of this GF instance (for subclasses of standard-generic-function).</summary>
    public LispClass? StoredClass { get; set; }

    /// <summary>When a GF auto-created by defmethod replaces an ordinary function, the
    /// original is saved here. The dispatcher uses it as a last-resort fallback when
    /// no applicable method is found, preserving built-in behaviour for CL functions
    /// (e.g. CLOSE, STREAM-ELEMENT-TYPE) that have user-defined Gray-stream methods
    /// without losing the original C# implementation for non-Gray streams.</summary>
    public LispFunction? FallbackFunction { get; set; }

    /// <summary>Single-entry dispatch cache (monomorphic inline cache).
    /// Caches the last successful dispatch result for quick reuse. `volatile` so a
    /// concurrent InvalidateCache (defmethod) / cache-fill is visible across threads
    /// and reads never tear — worst case a reader uses a complete but slightly stale
    /// CachedDispatch, never a corrupt one.</summary>
    internal volatile CachedDispatch? LastDispatch;

    /// <summary>Invalidate dispatch cache when methods are added/removed.</summary>
    internal void InvalidateCache() => LastDispatch = null;

    public GenericFunction(Symbol name, int arity, Func<LispObject[], LispObject> dispatchFn)
        : base(dispatchFn, name.Name, arity)
    {
        Name = name;
    }

    public override string ToString() => $"#<GENERIC-FUNCTION {Name.Name}>";
}
