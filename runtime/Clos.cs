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
            SlotIndex[EffectiveSlots[i].Name.Name] = i;
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
        // C3 linearization
        var result = new List<LispClass>();
        var toMerge = new List<List<LispClass>>();

        // Add each superclass's CPL
        foreach (var super in DirectSuperclasses)
        {
            toMerge.Add(new List<LispClass>(super.ClassPrecedenceList));
        }
        // Add direct superclasses list
        if (DirectSuperclasses.Length > 0)
            toMerge.Add(new List<LispClass>(DirectSuperclasses));

        // Start with this class
        result.Add(this);

        while (toMerge.Count > 0)
        {
            // Remove empty lists
            toMerge.RemoveAll(l => l.Count == 0);
            if (toMerge.Count == 0) break;

            // Find a candidate: head of some list that doesn't appear in the tail of any list
            LispClass? candidate = null;
            foreach (var list in toMerge)
            {
                var head = list[0];
                bool inTail = false;
                foreach (var other in toMerge)
                {
                    for (int i = 1; i < other.Count; i++)
                    {
                        if (ReferenceEquals(other[i], head))
                        {
                            inTail = true;
                            break;
                        }
                    }
                    if (inTail) break;
                }
                if (!inTail)
                {
                    candidate = head;
                    break;
                }
            }

            if (candidate == null)
                throw new LispErrorException(new LispError(
                    $"Cannot compute CPL for {Name.Name}: inconsistent precedence graph"));

            result.Add(candidate);
            // Remove candidate from all lists
            foreach (var list in toMerge)
            {
                if (list.Count > 0 && ReferenceEquals(list[0], candidate))
                    list.RemoveAt(0);
            }
        }

        return result.ToArray();
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

        var slots = new List<SlotDefinition>();
        foreach (var name in slotOrder)
        {
            var defs = slotDefs[name];
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

            var effective = new SlotDefinition(
                primary.Name,
                allInitargs.Count > 0 ? allInitargs.ToArray() : null,
                initform,
                primary.IsClassAllocation);
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
}

/// <summary>
/// CLOS method: specializers + qualifiers + function body.
/// </summary>
public class LispMethod : LispObject
{
    public LispObject[] Specializers { get; }  // LispClass or (eql value) cons
    public Symbol[] Qualifiers { get; }         // :BEFORE, :AFTER, :AROUND, or empty
    public LispFunction Function { get; }
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
    public List<LispMethod> Methods { get; } = new();
    public LispFunction? DispatchFunction { get; set; }
    /// <summary>Method combination type: null means STANDARD, otherwise the operator symbol (+, LIST, APPEND, etc.)</summary>
    public Symbol? MethodCombination { get; set; }
    /// <summary>Method combination arguments from defgeneric (:method-combination name arg1 arg2 ...)</summary>
    public LispObject[]? MethodCombinationArgs { get; set; }
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

    /// <summary>Single-entry dispatch cache (monomorphic inline cache).
    /// Caches the last successful dispatch result for quick reuse.</summary>
    internal CachedDispatch? LastDispatch;

    /// <summary>Invalidate dispatch cache when methods are added/removed.</summary>
    internal void InvalidateCache() => LastDispatch = null;

    public GenericFunction(Symbol name, int arity, Func<LispObject[], LispObject> dispatchFn)
        : base(dispatchFn, name.Name, arity)
    {
        Name = name;
    }

    public override string ToString() => $"#<GENERIC-FUNCTION {Name.Name}>";
}
