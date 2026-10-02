namespace DotCL;

/// <summary>
/// CLOS slot definition: name, initarg, initform thunk.
/// </summary>
public class SlotDefinition : LispObject
{
    public Symbol Name { get; }
    public Symbol[] Initargs { get; }
    public LispFunction? InitformThunk { get; }

    /// <summary>:reader / :accessor names, and :writer / (setf accessor) names.
    /// AMOP puts these on DIRECT slot definitions only; an effective slot keeps
    /// them empty. DEFCLASS has always parsed them (it defines the methods from
    /// them) but did not pass them on, so the accessors answered NIL.</summary>
    public LispObject[] Readers { get; set; } = Array.Empty<LispObject>();
    /// <summary>Writer names: a symbol for :writer, the list (SETF name) for :accessor.</summary>
    public LispObject[] Writers { get; set; } = Array.Empty<LispObject>();

    /// <summary>The slot's declared :type, T when unspecified. Introspection only;
    /// dotcl does not check slot values against it.</summary>
    public LispObject SlotType { get; set; } = T.Instance;

    /// <summary>The :initform as source, NIL when the slot has none. The compiled
    /// thunk (InitformThunk) is what actually runs; this is what AMOP's
    /// SLOT-DEFINITION-INITFORM has to hand back, and a thunk cannot be turned
    /// back into the form it came from.</summary>
    public LispObject Initform { get; set; } = Nil.Instance;
    /// <summary>True when :allocation :class was specified (shared slot stored on class, not instance).</summary>
    public bool IsClassAllocation { get; set; }

    /// <summary>An :allocation other than :INSTANCE or :CLASS. CLHS 7.1.2 allows only
    /// those two, but that rule is about STANDARD-CLASS: under a custom metaclass AMOP
    /// has the metaclass decide what an allocation means, and it needs the keyword to
    /// decide with. Null for the two standard allocations, which stay on the bool.</summary>
    public Symbol? Allocation { get; set; }

    /// <summary>The allocation as AMOP reports it, whichever of the two representations
    /// holds it.</summary>
    public Symbol AllocationKeyword
        => Allocation ?? Startup.Keyword(IsClassAllocation ? "CLASS" : "INSTANCE");

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
    /// / (SETF SLOT-VALUE) / SLOT-BOUNDP route through this for SlotDefinition objects.
    /// ConcurrentDictionary + atomic lazy init (see EnsureExtraSlots): under
    /// (set-parallel-eval t), parallel make-instance / defclass of a custom metaclass
    /// otherwise tore a plain Dictionary.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?>? ExtraSlots;

    /// <summary>Atomically obtain the ExtraSlots table, creating it on first use.
    /// A plain (ExtraSlots ??= new()) lets two threads publish different dictionaries
    /// and lose an update; CompareExchange keeps a single winner.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?> EnsureExtraSlots()
        => ExtraSlots ?? System.Threading.Interlocked.CompareExchange(
               ref ExtraSlots, new(), null) ?? ExtraSlots;

    /// <summary>The canonical slot-option plist (a Lisp list :key val ...) captured from the
    /// DEFCLASS slot specifier, used as the &rest initargs when DIRECT-SLOT-DEFINITION-CLASS
    /// is consulted for a custom metaclass. Null for slots defined under STANDARD-CLASS.</summary>
    public LispObject? RawOptions { get; set; }

    /// <summary>The :documentation the slot was defined with, NIL when it has none.
    /// AMOP passes it to EFFECTIVE-SLOT-DEFINITION-CLASS and DOCUMENTATION reads it
    /// back, so it has to live on the slot definition rather than in the global
    /// documentation table -- an effective slot definition is built, not defined.</summary>
    public LispObject Documentation { get; set; } = Nil.Instance;

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
    /// <summary>Cache for Runtime.UsesSlotProtocol: (method epoch &lt;&lt; 2) | valid | answer.</summary>
    internal long SlotProtocolCache;
    public SlotDefinition[] DirectSlots { get; set; }
    /// <summary>While a class under a custom metaclass is being initialized: the
    /// canonical :DIRECT-SLOTS plists handed to INITIALIZE-INSTANCE, each with the slot
    /// definition it was made from. A plist that comes back unchanged keeps its slot
    /// definition; any other plist becomes a new one. Null outside that window.</summary>
    internal List<(LispObject Plist, SlotDefinition Slot)>? PendingSlotPlists { get; set; }
    /// <summary>While DEFCLASS makes or redefines this class: the (slot name, reader or
    /// writer) pairs its expansion defines methods for itself. Null otherwise.</summary>
    internal HashSet<(Symbol Slot, LispObject Fn)>? DefclassAccessors { get; set; }
    /// <summary>Reader and writer methods still to be defined for direct slots that
    /// arrived through the class metaobject protocol (a metaclass rewriting
    /// :DIRECT-SLOTS, ENSURE-CLASS, REINITIALIZE-INSTANCE) rather than from DEFCLASS,
    /// whose expansion defines its own. Defined once the class is reachable by name.</summary>
    internal List<(Symbol Slot, LispObject Readers, LispObject Writers)>? PendingAccessors { get; set; }
    /// <summary>True while REINITIALIZE-INSTANCE's default method runs SHARED-INITIALIZE
    /// on this class, so the class-metaobject SHARED-INITIALIZE installs the new
    /// superclasses / slots / default initargs on an already finalized class.</summary>
    internal bool ReinitializingFromInitargs { get; set; }
    public LispClass[] DirectSuperclasses { get; set; }
    public LispClass[] ClassPrecedenceList { get; set; }
    public SlotDefinition[] EffectiveSlots { get; set; }
    public Dictionary<string, int> SlotIndex { get; private set; }

    /// <summary>Slot layout index keyed by the slot's symbol. Null unless two
    /// effective slots share a name while being different symbols (CLHS 7.5.3:
    /// slots are named by symbols, so A::X and B::X are two slots). SlotIndex,
    /// keyed by the name string, then answers the first of them; paths that hold
    /// the symbol consult this map first. Classes without such a collision keep
    /// the string-only lookup, so their slot access costs one null check more.</summary>
    public Dictionary<Symbol, int>? SlotIndexBySym { get; private set; }

    /// <summary>True when this class has slots or initargs whose names collide
    /// across packages, so name-string matching would conflate them. Paths that
    /// match slots or initargs by name switch to symbol identity (see
    /// SameSlotName) when this is set.</summary>
    public bool HasSymbolCollision { get; private set; }

    /// <summary>A symbol whose home is DOTCL-INTERNAL stands for any symbol of the
    /// same name: the runtime builds the standard condition classes' slot names and
    /// initargs with Startup.Sym, which lands there, and user code refers to those
    /// slots (and passes their keyword initargs) with its own symbols.</summary>
    internal static bool IsNameWildcard(Symbol s)
        => s.HomePackage != null && ReferenceEquals(s.HomePackage, Startup.Internal);

    /// <summary>Whether two slot names (or two initargs) denote the same slot
    /// (initarg): the same symbol, or the same name where one side is a
    /// runtime-internal wildcard.</summary>
    public static bool SameSlotName(Symbol a, Symbol b)
        => ReferenceEquals(a, b)
           || (a.Name == b.Name && (IsNameWildcard(a) || IsNameWildcard(b)));

    /// <summary>Whether initarg key KEY (as passed to MAKE-INSTANCE etc.) selects
    /// the initarg IA. By name unless this class has a cross-package collision,
    /// where it is by symbol.</summary>
    public bool InitargMatches(Symbol ia, LispObject key)
    {
        if (!HasSymbolCollision)
            return key switch
            {
                Symbol s => ia.Name == s.Name,
                _ => ia.Name == key.ToString()
            };
        return key switch
        {
            Symbol s => SameSlotName(ia, s),
            Nil => ia.Name == "NIL" && (IsNameWildcard(ia) || ia.HomePackage?.Name == "COMMON-LISP"),
            T => ia.Name == "T" && (IsNameWildcard(ia) || ia.HomePackage?.Name == "COMMON-LISP"),
            _ => false
        };
    }

    /// <summary>Layout index of the slot named SLOTNAME (NAME is its name string,
    /// which the caller has at hand). Consults the symbol-keyed index first when
    /// the class has one.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public bool TryGetSlotIndex(LispObject slotName, string name, out int idx)
    {
        var bySym = SlotIndexBySym;
        if (bySym != null && slotName is Symbol s)
            return TryGetSlotIndexBySymbol(bySym, s, name, out idx);
        if (!SlotIndex.TryGetValue(name, out idx)) return false;
        // The name matched; the symbol has to as well. Almost always the very
        // same symbol, so this is one load and one compare.
        if (slotName is Symbol s2)
        {
            var slots = EffectiveSlots;
            if ((uint)idx < (uint)slots.Length && !ReferenceEquals(slots[idx].Name, s2)
                && !SameSlotName(slots[idx].Name, s2))
                return false;
        }
        return true;
    }

    /// <summary>The lookup for a class with a cross-package collision: by symbol,
    /// except that a runtime-internal (wildcard) name on either side still matches
    /// by name.</summary>
    private bool TryGetSlotIndexBySymbol(Dictionary<Symbol, int> bySym, Symbol s, string name, out int idx)
    {
        if (bySym.TryGetValue(s, out idx)) return true;
        if (SlotIndex.TryGetValue(name, out idx)
            && (IsNameWildcard(s) || IsNameWildcard(EffectiveSlots[idx].Name)))
            return true;
        idx = -1;
        return false;
    }

    /// <summary>Layout index of the slot that SLOTNAME names, by symbol
    /// (SameSlotName) whether or not the class has a collision. For paths that
    /// pair slots across two classes (CHANGE-CLASS, redefinition), where a name
    /// match between A::X and B::X would carry a value into the wrong slot.</summary>
    public bool TryGetSlotIndexExact(Symbol slotName, out int idx)
    {
        if (TryGetSlotIndex(slotName, out idx) && idx < EffectiveSlots.Length
            && SameSlotName(EffectiveSlots[idx].Name, slotName))
            return true;
        for (int i = 0; i < EffectiveSlots.Length; i++)
            if (SameSlotName(EffectiveSlots[i].Name, slotName)) { idx = i; return true; }
        idx = -1;
        return false;
    }

    /// <summary>Layout index of the effective slot definition SLOTD (as handed to
    /// SLOT-VALUE-USING-CLASS and friends). Identity first, then its name.</summary>
    public bool TryGetSlotIndex(SlotDefinition slotd, out int idx)
    {
        if (TryGetSlotIndex(slotd.Name, out idx) && idx < EffectiveSlots.Length
            && ReferenceEquals(EffectiveSlots[idx], slotd))
            return true;
        for (int i = 0; i < EffectiveSlots.Length; i++)
            if (ReferenceEquals(EffectiveSlots[i], slotd)) { idx = i; return true; }
        return TryGetSlotIndexExact(slotd.Name, out idx) || TryGetSlotIndex(slotd.Name, out idx);
    }

    /// <summary>TryGetSlotIndex for a slot symbol.</summary>
    public bool TryGetSlotIndex(Symbol slotName, out int idx)
        => TryGetSlotIndex(slotName, slotName.Name, out idx);

    /// <summary>The effective slot definition named SLOTNAME, or null.</summary>
    public SlotDefinition? FindEffectiveSlot(Symbol slotName)
    {
        if (TryGetSlotIndex(slotName, out int idx) && idx < EffectiveSlots.Length
            && EffectiveSlots[idx].Name.Name == slotName.Name)
            return EffectiveSlots[idx];
        foreach (var s in EffectiveSlots)
            if (SameSlotName(s.Name, slotName)) return s;
        foreach (var s in EffectiveSlots)
            if (s.Name.Name == slotName.Name) return s;
        return null;
    }

    /// <summary>Serializes (re)definition of THIS class. FinalizeClass mutates the
    /// class's non-concurrent caches (SlotIndex, InitargToSlotIndex, EffectiveSlots,
    /// CPL); under set-parallel-eval two threads re-defining the same class name
    /// concurrently otherwise corrupt those Dictionaries. Cold path: taken
    /// only during defclass/ensure-class, never during dispatch or make-instance.</summary>
    internal readonly object DefLock = new();
    /// <summary>True for built-in classes (BUILT-IN-CLASS metaclass). False for user-defined (STANDARD-CLASS).</summary>
    public bool IsBuiltIn { get; set; }
    /// <summary>True when this class stands for a .NET interface. Interfaces are
    /// superclasses for dispatch, but rank below every concrete class in a class
    /// precedence list, so EnsureDotNetTypeClass keeps them separable.</summary>
    public bool IsDotNetInterface { get; set; }
    /// <summary>The .NET type this class stands for, or null for an ordinary Lisp
    /// class. Dispatch consults it for the assignabilities a class precedence list
    /// cannot enumerate: a variant generic (List&lt;String&gt; is an
    /// IEnumerable&lt;Object&gt;) would need every instantiation of every supertype
    /// of every type argument spelled out.</summary>
    public System.Type? DotNetType { get; set; }
    /// <summary>True for structure classes (STRUCTURE-CLASS metaclass).</summary>
    public bool IsStructureClass { get; set; }
    /// <summary>True for forward-referenced classes (superclass not yet defined).</summary>
    public bool IsForwardReferenced { get; set; }
    /// <summary>Slot names for #S reader macro support.</summary>
    public Symbol[]? StructSlotNames { get; set; }
    /// <summary>Direct default initargs defined by this class (before inheritance merge).</summary>
    /// <summary>The canonicalized default initargs AMOP asks for: the name, the
    /// initform as source, and the function that evaluates it. The form is what
    /// CLASS-DIRECT-DEFAULT-INITARGS has to show -- a thunk cannot be turned back
    /// into it.</summary>
    public (Symbol Key, LispObject Form, LispFunction Thunk)[] DirectDefaultInitargs { get; set; }
        = Array.Empty<(Symbol, LispObject, LispFunction)>();
    /// <summary>Effective default initargs (merged from CPL, most specific first).</summary>
    public (Symbol Key, LispObject Form, LispFunction Thunk)[] DefaultInitargs { get; set; }
        = Array.Empty<(Symbol, LispObject, LispFunction)>();
    /// <summary>Storage for :allocation :class slots (shared across all instances).
    /// ConcurrentDictionary: a :class slot's initform can fire on the make-instance
    /// hot path, so parallel make-instance / setf slot-value on the same class write
    /// concurrently. Usage is TryGetValue/indexer only (no lock; hot path).</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?> ClassSlotValues { get; } = new();
    /// <summary>Slot values this class holds as an instance of its (custom) metaclass;
    /// i.e. slots the metaclass adds beyond STANDARD-CLASS. Null until populated.
    /// Lets slot-value on a class metaobject read metaclass-defined slots,
    /// mirroring SlotDefinition.ExtraSlots.
    /// ConcurrentDictionary + atomic lazy init (see EnsureExtraSlots) so parallel
    /// eval cannot tear a plain Dictionary.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?>? ExtraSlots;

    /// <summary>Atomically obtain the ExtraSlots table, creating it on first use.
    /// CompareExchange keeps a single winner if two threads race the first write.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?> EnsureExtraSlots()
        => ExtraSlots ?? System.Threading.Interlocked.CompareExchange(
               ref ExtraSlots, new(), null) ?? ExtraSlots;
    /// <summary>Cached mapping from initarg name to slot index (only valid when each initarg maps to one slot).</summary>
    public Dictionary<string, int>? InitargToSlotIndex { get; set; }
    /// <summary>True if this class can use the fast make-instance path.</summary>
    public bool HasSimpleInitialization { get; set; }
    /// <summary>Whether the GF method check for simple init has been performed.</summary>
    public bool SimpleInitChecked { get; set; }
    /// <summary>Cached result of the GF method check for simple init.</summary>
    public bool SimpleInitValid { get; set; }
    /// <summary>Whether the SHARED-INITIALIZE-only method check has been performed.</summary>
    public bool SharedInitSimpleChecked { get; set; }
    /// <summary>Cached result of that check: true when SHARED-INITIALIZE has no method
    /// of its own for this class, so its default method is what a dispatch lands on.</summary>
    public bool SharedInitSimpleValid { get; set; }

    /// <summary>Cached mapping from initarg keyword name to slot index for fast make-instance path.
    /// Only includes instance-allocated slots (not :allocation :class).</summary>
    private Dictionary<string, int>? _initargSlotMap;
    /// <summary>True if all slots are instance-allocated and no initargs use NIL as key.
    /// When false, the fast make-instance path must be skipped.</summary>
    private bool? _canUseFastPath;
    /// <summary>Cached class prototype (AMOP class-prototype): a single per-class
    /// instance reused across calls. Must be stable: define-presentation-method
    /// (McCLIM) and other code dispatch via (eql class-prototype), which only
    /// works if the same object is returned every time. Lazily created.</summary>
    private LispInstance? _prototype;
    // Atomic lazy init: a plain ??= lets two threads publish different instances,
    // which silently breaks (eql class-prototype) dispatch. CompareExchange keeps
    // the first winner as the single stable prototype (a losing extra instance is
    // discarded, not published).
    public LispInstance Prototype
        => _prototype ?? System.Threading.Interlocked.CompareExchange(ref _prototype, new LispInstance(this), null) ?? _prototype;
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
        // Build into locals, then publish the fully-populated map LAST. Under
        // set-parallel-eval, two threads' first make-instance on this class can
        // race here: the old code assigned the empty Dictionary to the shared
        // field first and populated it afterwards, so a concurrent reader (or a
        // second builder) mutated/read a half-built non-concurrent Dictionary and
        // tripped ".NET: concurrent update corrupted its state": the flaky
        // parallel-eval/mop crash. With publish-when-complete a reader sees
        // either null (and harmlessly rebuilds an identical map) or a complete,
        // thereafter-immutable map. Matches the CachedValidInitargKeys builder.
        var map = new Dictionary<string, int>();
        var canFast = true;
        for (int i = 0; i < EffectiveSlots.Length; i++)
        {
            var slot = EffectiveSlots[i];
            if (slot.IsClassAllocation && slot.Initargs.Length > 0)
            {
                // Class-allocated slots with initargs can't use fast path
                canFast = false;
            }
            if (!slot.IsClassAllocation)
            {
                foreach (var ia in slot.Initargs)
                {
                    map.TryAdd(ia.Name, i);
                }
            }
        }
        if (HasSymbolCollision) canFast = false; // the map is keyed by name
        _canUseFastPath = canFast;
        _initargSlotMap = map;
    }

    public LispClass(Symbol name, SlotDefinition[] directSlots, LispClass[] directSuperclasses)
    {
        Name = name;
        DirectSlots = directSlots;
        DirectSuperclasses = directSuperclasses;
        ClassPrecedenceList = Array.Empty<LispClass>();
        EffectiveSlots = Array.Empty<SlotDefinition>();
        SlotIndex = new Dictionary<string, int>();
        Layout = new ClassLayout(this);
    }

    /// <summary>The layout token of this class's current instances. An instance
    /// records the token it was built with; FinalizeClass replaces the token when the
    /// set of local and shared slots changes, and MAKE-INSTANCES-OBSOLETE replaces it
    /// unconditionally. An instance whose token is not the class's current one is
    /// obsolete and is brought up to date before its slots are next touched
    /// (CLHS 4.3.6).</summary>
    public ClassLayout Layout;

    /// <summary>Mark every existing instance of this class obsolete: they keep the
    /// old token and are updated the next time a slot of theirs is read or written.
    /// OLDSLOTS is the effective slot list the old token's instances were laid out by;
    /// SHARED is the old shared slot values, captured now because a redefinition may
    /// drop the slot and with it the only way to read its value.</summary>
    internal void SupersedeLayout(SlotDefinition[] oldSlots,
        (Symbol Name, LispObject? Value)[] shared)
    {
        var old = Layout;
        old.Superseded = new ClassLayout.OldShape(oldSlots, shared);
        System.Threading.Volatile.Write(ref Layout, new ClassLayout(this));
        // Call-site accessor caches hold a layout token: force them to refill with
        // the new one, so an instance with the old token misses.
        GenericFunction.BumpMethodEpoch();
        // A generic function's dispatch cache can hold a reader or writer shortcut
        // with this class's old slot index, which no layout token guards: the slot
        // the index names now may be another one.
        Runtime.InvalidateAllDispatchCaches();
    }

    /// <summary>The values of the shared slots among SLOTS, as seen through CPL.
    /// A slot's value lives on the most specific class that declares it shared; when
    /// that declaration is already gone (the class is being redefined), the first
    /// class in CPL still holding a value under the name answers.</summary>
    internal static (Symbol Name, LispObject? Value)[] CaptureSharedValues(
        SlotDefinition[] slots, LispClass[] cpl)
    {
        var result = new List<(Symbol, LispObject?)>();
        foreach (var s in slots)
        {
            if (!s.IsClassAllocation) continue;
            string name = s.Name.Name;
            LispObject? val = null;
            LispClass? owner = null;
            foreach (var c in cpl)
            {
                foreach (var ds in c.DirectSlots)
                    if (ds.IsClassAllocation && ds.Name.Name == name) { owner = c; break; }
                if (owner != null) break;
            }
            if (owner == null)
                foreach (var c in cpl)
                    if (c.ClassSlotValues.ContainsKey(name)) { owner = c; break; }
            owner?.ClassSlotValues.TryGetValue(name, out val);
            result.Add((s.Name, val));
        }
        return result.ToArray();
    }

    /// <summary>True when OLD and NEW lay out instances the same way: the same slot
    /// names in the same positions with the same allocation. Then existing instances
    /// need no update and keep their token.</summary>
    private static bool SameInstanceShape(SlotDefinition[] old, SlotDefinition[] neu)
    {
        if (old.Length != neu.Length) return false;
        for (int i = 0; i < old.Length; i++)
        {
            if (!ReferenceEquals(old[i].Name, neu[i].Name)) return false;
            if (old[i].IsClassAllocation != neu[i].IsClassAllocation) return false;
            if (!ReferenceEquals(old[i].Allocation, neu[i].Allocation)) return false;
        }
        return true;
    }

    /// <summary>
    /// Compute CPL using C3 linearization and build effective slots.
    /// Called after construction once all superclasses are registered.
    /// </summary>
    public void FinalizeClass()
    {
        // Serialize concurrent (re)finalization of THIS class: two threads
        // re-defining the same class name under set-parallel-eval would otherwise
        // both run this body and corrupt the non-concurrent caches below. Cold
        // path (defclass/ensure-class only). SlotIndex and InitargToSlotIndex are
        // rebuilt into locals and published by a single reference swap, so a
        // concurrent reader on the make-instance/slot-value hot path (which does
        // NOT take DefLock) always sees a complete map, never one mid-rebuild.
        lock (DefLock)
        {
            var oldSlots = EffectiveSlots;
            var oldCpl = ClassPrecedenceList;
            ClassPrecedenceList = ComputeCPL();
            EffectiveSlots = ComputeEffectiveSlots();
            // AMOP has finalization go through COMPUTE-SLOTS, and the list it hands
            // back IS the class's effective slots -- the order included, which is the
            // whole point of a metaclass that reorders them. Runs before the layout
            // indices below are handed out, so slot access follows the answer. Quiet
            // until somebody has specialised it.
            if (Runtime.ComputeSlotsHook?.Invoke(this) is { } requested)
                EffectiveSlots = requested;
            _initargSlotMap = null; // invalidate cached initarg->slot mapping
            _canUseFastPath = null;
            CachedHasCustomInitMethods = null;
            CachedIsConditionClass = null;
            CachedValidInitargKeys = null;
            var slotIndex = new Dictionary<string, int>();
            bool slotCollision = false;
            for (int i = 0; i < EffectiveSlots.Length; i++)
            {
                // First occurrence wins: with a cross-package collision the string
                // key answers the most specific class's slot.
                if (!slotIndex.TryAdd(EffectiveSlots[i].Name.Name, i))
                    slotCollision = true;
                // Instance-allocated slots get their layout index as location; :class
                // allocation slots are not in the per-instance vector, and neither is
                // one whose metaclass defined its own allocation -- that slot is the
                // metaclass's business, reached through SLOT-VALUE-USING-CLASS.
                EffectiveSlots[i].Location =
                    (EffectiveSlots[i].IsClassAllocation || EffectiveSlots[i].Allocation != null)
                        ? -1 : i;
            }
            Dictionary<Symbol, int>? bySym = null;
            if (slotCollision)
            {
                bySym = new Dictionary<Symbol, int>(ReferenceEqualityComparer.Instance);
                for (int i = 0; i < EffectiveSlots.Length; i++)
                    bySym.TryAdd(EffectiveSlots[i].Name, i);
            }
            // Non-keyword initargs of the same name from different packages.
            bool initargCollision = false;
            {
                var byName = new Dictionary<string, Symbol>();
                foreach (var es in EffectiveSlots)
                    foreach (var ia in es.Initargs)
                    {
                        if (!byName.TryGetValue(ia.Name, out var prev)) byName[ia.Name] = ia;
                        else if (!SameSlotName(prev, ia)) initargCollision = true;
                    }
            }
            HasSymbolCollision = slotCollision || initargCollision;
            SlotIndexBySym = bySym;
            SlotIndex = slotIndex;
            ComputeEffectiveDefaultInitargs();
            // AMOP has finalization go through COMPUTE-DEFAULT-INITARGS. The hook runs
            // after the class computed its own, so a metaclass that specialises it sees
            // (and can replace) the answer; nothing happens until one does.
            Runtime.DefaultInitargsHook?.Invoke(this);

            // Build initarg-to-slot cache for fast make-instance path
            var initargMap = new Dictionary<string, int>();
            bool hasSharedInitarg = false;
            for (int i = 0; i < EffectiveSlots.Length; i++)
            {
                foreach (var ia in EffectiveSlots[i].Initargs)
                {
                    if (initargMap.ContainsKey(ia.Name))
                        hasSharedInitarg = true;
                    else
                        initargMap[ia.Name] = i;
                }
            }
            InitargToSlotIndex = initargMap;

            // Fast path: no default initargs, no shared initargs, no :class allocation slots
            // and no cross-package name collision (the fast paths match by name).
            HasSimpleInitialization = DefaultInitargs.Length == 0
                && !hasSharedInitarg
                && !HasSymbolCollision
                && !Array.Exists(EffectiveSlots, s => s.IsClassAllocation);
            SimpleInitChecked = false;
            SharedInitSimpleChecked = false;
            // CLHS 4.3.6: a redefinition that changes the local or shared slots makes
            // the existing instances obsolete. The first finalization has no instances
            // to update (EffectiveSlots was empty); a re-finalization that ends up with
            // the same shape (a superclass redefined without touching our slots, an
            // accessor-only change) keeps the token, so those instances stay current.
            if (oldSlots.Length != 0 && !SameInstanceShape(oldSlots, EffectiveSlots))
                SupersedeLayout(oldSlots, CaptureSharedValues(oldSlots, oldCpl));
            // Which methods apply to an instance of this class follows its precedence
            // list. Generic functions cache their dispatch per argument class, so a
            // changed list (a superclass added, removed or reordered) makes those
            // entries wrong for this class.
            if (oldCpl.Length != 0 && !SameClassList(oldCpl, ClassPrecedenceList))
                Runtime.InvalidateAllDispatchCaches();
        }
        // Slot layout may have changed: invalidate any call-site reader inline caches
        // that snapshotted this class's old (class, index) pair.
        GenericFunction.BumpMethodEpoch();
    }

    private static bool SameClassList(LispClass[] a, LispClass[] b)
    {
        if (a.Length != b.Length) return false;
        for (int i = 0; i < a.Length; i++)
            if (!ReferenceEquals(a[i], b[i])) return false;
        return true;
    }

    /// <summary>
    /// Merge default-initargs from CPL (most specific first, first wins for same key).
    /// </summary>
    public void ComputeEffectiveDefaultInitargs()
    {
        // Keys are initarg symbols: A::X and B::X are two initargs (CLHS 7.1.3).
        var result = new List<(Symbol Key, LispObject Form, LispFunction Thunk)>();
        foreach (var cls in ClassPrecedenceList)
        {
            foreach (var (key, form, thunk) in cls.DirectDefaultInitargs)
            {
                bool dup = false;
                foreach (var r in result)
                    if (SameSlotName(r.Key, key)) { dup = true; break; }
                if (!dup)
                    result.Add((key, form, thunk));
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
                // for the very first pick, which is this class): take the first.
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
        foreach (var d in defs)
            foreach (var ia in d.Initargs)
            {
                bool dup = false;
                foreach (var seen in allInitargs)
                    if (LispClass.SameSlotName(seen, ia)) { dup = true; break; }
                if (!dup) allInitargs.Add(ia);
            }

        // Most specific initform (first one that has it): the thunk that runs and
        // the source form that AMOP reports come from the same slot definition.
        LispFunction? initform = null;
        LispObject initformSource = Nil.Instance;
        foreach (var d in defs)
        {
            if (d.InitformThunk != null)
            {
                initform = d.InitformThunk;
                initformSource = d.Initform;
                break;
            }
        }

        // The effective :type is the conjunction of every declared one (CLHS 7.5.3).
        // A type that is a supertype of another one declared is dropped, so the
        // usual case, a subclass narrowing the type, still reports just the narrow
        // type; types that only overlap give (AND T1 T2 ...), most specific first.
        var declaredTypes = new List<LispObject>();
        foreach (var d in defs)
        {
            // "No :type given" is T.Instance, which is not a Symbol: checking
            // only for the symbol T made every subclass slot look type-specific.
            if (d.SlotType is T || (d.SlotType is Symbol s && s.Name == "T")) continue;
            bool seen = false;
            foreach (var t in declaredTypes)
                if (Runtime.IsTruthy(Runtime.Equal(t, d.SlotType))) { seen = true; break; }
            if (!seen) declaredTypes.Add(d.SlotType);
        }
        var keptTypes = new List<LispObject>();
        for (int i = 0; i < declaredTypes.Count; i++)
        {
            bool redundant = false;
            for (int j = 0; j < declaredTypes.Count && !redundant; j++)
            {
                if (i == j) continue;
                // TYPE[i] adds nothing when some other declared type is a subtype of
                // it. Of two equivalent types the later one goes.
                if (SubtypeCertain(declaredTypes[j], declaredTypes[i])
                    && (j < i || !SubtypeCertain(declaredTypes[i], declaredTypes[j])))
                    redundant = true;
            }
            if (!redundant) keptTypes.Add(declaredTypes[i]);
        }
        LispObject slotType;
        if (keptTypes.Count == 0) slotType = T.Instance;
        else if (keptTypes.Count == 1) slotType = keptTypes[0];
        else
        {
            var andForm = new LispObject[keptTypes.Count + 1];
            andForm[0] = Startup.Sym("AND");
            for (int i = 0; i < keptTypes.Count; i++) andForm[i + 1] = keptTypes[i];
            slotType = Runtime.List(andForm);
        }

        // The most specific :documentation given (CLHS 7.5.3): a subclass that
        // restates the slot without one keeps the inherited string.
        LispObject documentation = Nil.Instance;
        foreach (var d in defs)
        {
            if (d.Documentation is Nil) continue;
            documentation = d.Documentation;
            break;
        }

        return new SlotDefinition(
            name,
            allInitargs.Count > 0 ? allInitargs.ToArray() : null,
            initform,
            primary.IsClassAllocation) { IsEffective = true, SlotType = slotType,
                                         Initform = initformSource,
                                         Allocation = primary.Allocation,
                                         Documentation = documentation };
    }

    /// <summary>True only when SUBTYPEP answers yes for certain. A type that cannot
    /// be decided yet (a class not defined at finalization time) counts as no.</summary>
    private static bool SubtypeCertain(LispObject sub, LispObject super)
    {
        try { return Runtime.IsTruthy(Runtime.Subtypep(sub, super)); }
        catch (LispErrorException) { return false; }
    }

    private SlotDefinition[] ComputeEffectiveSlots()
    {
        // Per CLHS 7.5.3: merge slot definitions from CPL
        // - Initargs: union of all initargs across CPL
        // - Initform: from the most specific class that provides one
        // - Allocation: from the most specific class (default :instance)
        //
        // Slots are named by symbols: A::X and B::X are two slots. Grouping is by
        // symbol (SameSlotName), with the name string only narrowing the search.
        var slotOrder = new List<List<SlotDefinition>>();
        var groupsByName = new Dictionary<string, List<List<SlotDefinition>>>();
        foreach (var cls in ClassPrecedenceList)
        {
            foreach (var slot in cls.DirectSlots)
            {
                if (!groupsByName.TryGetValue(slot.Name.Name, out var groups))
                    groupsByName[slot.Name.Name] = groups = new List<List<SlotDefinition>>(1);
                List<SlotDefinition>? group = null;
                foreach (var g in groups)
                    if (SameSlotName(g[0].Name, slot.Name)) { group = g; break; }
                if (group == null)
                {
                    group = new List<SlotDefinition>();
                    groups.Add(group);
                    slotOrder.Add(group);
                }
                group.Add(slot);
            }
        }

        // For a custom metaclass, drive each slot through the COMPUTE-EFFECTIVE-SLOT-DEFINITION
        // protocol (AMOP). Standard classes (Metaclass == null) keep the direct C# path,
        // which also avoids GF calls during bootstrap and for the common case.
        bool useProtocol = Metaclass != null && ComputeEffectiveSlotHook != null;

        var slots = new List<SlotDefinition>();
        foreach (var defs in slotOrder)
        {
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

/// <summary>A class layout token (see LispClass.Layout). Carries nothing while it is
/// current; when superseded it records the shape its instances were built with, which
/// is what updating one of them needs.</summary>
public sealed class ClassLayout
{
    public readonly LispClass Class;
    public ClassLayout(LispClass cls) { Class = cls; }

    internal sealed class OldShape
    {
        internal readonly SlotDefinition[] Slots;
        internal readonly (Symbol Name, LispObject? Value)[] Shared;
        internal OldShape(SlotDefinition[] slots, (Symbol Name, LispObject? Value)[] shared)
        { Slots = slots; Shared = shared; }
    }
    internal volatile OldShape? Superseded;

    /// <summary>The slot-access cache entries made for this layout, by slot index.
    /// A call site that sees instances of several classes (an accessor in a method on
    /// a superclass) refills its one-entry cache on each change of class; handing it
    /// the entry made last time, rather than a new one, makes that refill allocate
    /// nothing. Entries are immutable, so sharing them between call sites is safe.</summary>
    private ReaderCache.Entry?[]? _slotEntries;

    internal ReaderCache.Entry SlotEntry(int idx, int epoch)
    {
        var entries = _slotEntries;
        if (entries == null || idx >= entries.Length)
            _slotEntries = entries = new ReaderCache.Entry?[Math.Max(idx + 1, Class.EffectiveSlots.Length)];
        var e = entries[idx];
        if (e == null || e.Epoch != epoch)
            entries[idx] = e = new ReaderCache.Entry(this, idx, epoch);
        return e;
    }
}

/// <summary>
/// CLOS instance: class pointer + slot array.
/// </summary>
public sealed class LispInstance : LispObject
{
    public LispClass Class { get; set; }
    public LispObject?[] Slots { get; set; }
    /// <summary>The class layout token this instance's Slots were built for. Not the
    /// class's current token means the class was redefined (or its instances made
    /// obsolete) since: see EnsureCurrent.</summary>
    public ClassLayout Layout;

    public LispInstance(LispClass cls)
    {
        Class = cls;
        Layout = cls.Layout;
        Slots = new LispObject?[cls.EffectiveSlots.Length];
        // null = unbound
        DotCL.Diagnostics.AllocCounter.Inc("LispInstance");
    }

    public override string ToString() => $"#<{Class.Name.Name}>";

    /// <summary>Bring an obsolete instance up to its class's current layout
    /// (CLHS 4.3.6) before its Slots are read or written. One reference compare
    /// when the class was never redefined.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public void EnsureCurrent()
    {
        if (!ReferenceEquals(Layout, Class.Layout))
            Runtime.UpdateObsoleteInstance(this);
    }

    // Strong, deliberately. A reference to a make-load-form literal compiles to a
    // lookup by key -- the creation form runs once, in its own top-level method,
    // and every use of the literal reads the cache. Holding the result weakly meant
    // a GC between load and use turned the literal into NIL, silently: a fasl that
    // worked when freshly compiled started handing out NIL later in the same session
    // (cffi's defcallback embeds a type object this way, so an argument stopped being
    // translated and a pointer reached Lisp code as an integer). Lifetime is now the
    // process; the bound is the number of distinct make-load-form literals loaded.
    // Holds any object a creation form returns, not only instances: a structure
    // with its own MAKE-LOAD-FORM goes through the same registry.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, LispObject>
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
        // intentionally empty: see note above
    }

    /// <summary>FASL load-time: the make-load-form literal interned under KEY, or
    /// null when its creation form has not run yet.</summary>
    public static LispInstance? TryGetInterned(string key) =>
        _internCache.TryGetValue(key, out var existing) ? existing as LispInstance : null;

    /// <summary>FASL load-time: (ALLOCATE-INSTANCE (FIND-CLASS 'CLASS-NAME)), the
    /// start of the creation form MAKE-LOAD-FORM-SAVING-SLOTS returns, interned
    /// under KEY like InternViaEval. The emitter then fills the slots.</summary>
    public static LispObject InternAllocateInstance(string key, LispObject className)
    {
        if (_internCache.TryGetValue(key, out var existing))
            return existing;
        var cls = Runtime.UnwrapMv(Runtime.FindClass(className));
        var fn = Emitter.CilAssembler.GetFunctionBySymbol(Startup.Sym("ALLOCATE-INSTANCE"));
        MultipleValues.Reset();
        var obj = Runtime.UnwrapMv(fn.Invoke1(cls));
        // Whatever ALLOCATE-INSTANCE made is the literal: a structure (its
        // MAKE-LOAD-FORM-SAVING-SLOTS form has this shape too) has to be found
        // by its key afterwards just as an instance is.
        _internCache[key] = obj;
        return obj;
    }

    /// <summary>FASL load-time: whether a make-load-form literal of any kind
    /// has been interned under KEY.</summary>
    public static bool IsInterned(string key) => _internCache.ContainsKey(key);

    /// <summary>FASL load-time: evaluate make-load-form creation form once and cache by key.</summary>
    public static LispObject InternViaEval(string key, LispObject creationForm)
    {
        if (_internCache.TryGetValue(key, out var existing))
            return existing;
        // Nil means "just look up": the creation form ran in its own top-level method
        // earlier in this same fasl, so a miss is not a legitimate state. Reporting it
        // beats the old silent Nil.Instance, which turned a lost literal into a wrong
        // value that surfaced far away from the cause.
        if (creationForm is Nil)
            throw new LispErrorException(new LispError(
                $"FASL: load-form literal {key} was not created before it was referenced"));
        var obj = Runtime.TryCallConstantForm(creationForm, out var called)
            ? Runtime.UnwrapMv(called) : Runtime.Eval(creationForm);
        _internCache[key] = obj;
        return obj;
    }
}

/// <summary>An EQL specializer metaobject. AMOP makes these objects with identity:
/// INTERN-EQL-SPECIALIZER answers the same one for two EQL objects, so specializers
/// can be compared with EQ and a method's specializer list holds metaobjects rather
/// than a list that merely looks like one. The older representation -- the list
/// (EQL object) -- is still accepted everywhere a specializer is read, because that
/// is what a caller writing one by hand produces.</summary>
public sealed class EqlSpecializer : LispObject
{
    public LispObject Object { get; }

    public EqlSpecializer(LispObject obj) { Object = obj; }

    public override string ToString() => $"#<EQL-SPECIALIZER {Object}>";
}

/// <summary>
/// CLOS method: specializers + qualifiers + function body.
/// </summary>
public class LispMethod : LispObject
{
    public LispObject[] Specializers { get; set; }  // LispClass, EqlSpecializer, or (eql value)
    // :BEFORE, :AFTER, :AROUND, or empty. Any non-list atom may be a qualifier
    // (CLHS DEFMETHOD), so T and numbers are kept too; T is not a Symbol here.
    public LispObject[] Qualifiers { get; set; }
    public LispFunction Function { get; set; }

    /// <summary>The method function as AMOP defines it: called with a list of the
    /// generic function's arguments and a list of the next methods. dotcl's own
    /// method functions take the arguments spread, which is what dispatch calls, so
    /// this holds the AMOP-shaped one -- the object a user passed with the :FUNCTION
    /// initarg, or a view built over the internal one on first ask.</summary>
    public LispFunction? ProcessedParameterFunction { get; set; }
    public int RequiredCount { get; set; }
    public int OptionalCount { get; set; }
    public bool HasRest { get; set; }
    public bool HasKey { get; set; }
    public bool HasAllowOtherKeys { get; set; }
    public List<string> KeywordNames { get; set; } = new();
    public GenericFunction? Owner { get; set; }

    /// <summary>The unspecialized lambda list as given: by AMOP MAKE-INSTANCE with
    /// :lambda-list, or by DEFMETHOD through %SET-METHOD-LAMBDA-LIST-INFO. Null for
    /// a method loaded from a FASL older than that, where only the arity is
    /// recorded and METHOD-LAMBDA-LIST rebuilds a list of the right shape.
    /// Mirrors GenericFunction.StoredLambdaList.</summary>
    public LispObject? StoredLambdaList { get; set; }
    /// <summary>The rebuilt placeholder, kept so this method answers the same list
    /// every time it is asked. See GenericFunction.PlaceholderLambdaList.</summary>
    internal LispObject? PlaceholderLambdaList;
    /// <summary>True if this method was defined by an inline :method in defgeneric.</summary>
    public bool IsFromDefgenericInline { get; set; }
    /// <summary>For a reader/writer/accessor method generated by DEFCLASS: the
    /// effective slot-definition it accesses. Backs DOTCL-MOP:ACCESSOR-METHOD-SLOT-DEFINITION.
    /// Null for ordinary methods.</summary>
    public SlotDefinition? AccessorSlot { get; set; }

    /// <summary>The class this method is an instance of, when DEFMETHOD asked the
    /// generic function for one and got something other than STANDARD-METHOD. Null
    /// for the ordinary case, where CLASS-OF answers STANDARD-METHOD. Mirrors
    /// SlotDefinition.MetaClass.</summary>
    public LispClass? MetaClass { get; set; }

    /// <summary>Slots added by a user-defined method class. A method is not a
    /// LispInstance and has no slot vector; this is the same escape hatch LispClass,
    /// GenericFunction and SlotDefinition use. Null until one is written.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?>? ExtraSlots;

    /// <summary>Atomically obtain the ExtraSlots table, creating it on first use.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?> EnsureExtraSlots()
        => ExtraSlots ?? System.Threading.Interlocked.CompareExchange(
               ref ExtraSlots, new(), null) ?? ExtraSlots;

    public LispMethod(LispObject[] specializers, LispObject[] qualifiers, LispFunction function)
    {
        Specializers = specializers;
        Qualifiers = qualifiers;
        Function = function;
    }

    public LispMethod() {
        Specializers = Array.Empty<LispObject>();
        Qualifiers = Array.Empty<LispObject>();
        Function = null!;
    }

    public override string ToString() => "#<METHOD>";

    /// <summary>The name a qualifier is compared by: a symbol's name, "T" for T,
    /// null for any other atom (a number).</summary>
    public static string? QualifierName(LispObject q)
        => q is Symbol s ? s.Name : q is T ? "T" : null;

    /// <summary>Two qualifiers agree: symbols (and T) by name, as method
    /// identity has always compared them here, anything else by EQL.</summary>
    public static bool QualifierSame(LispObject a, LispObject b)
    {
        var na = QualifierName(a);
        var nb = QualifierName(b);
        if (na != null || nb != null) return na == nb;
        return Runtime.IsTrueEql(a, b);
    }
}

/// <summary>
/// Generic function: dispatches to methods based on argument classes.
/// The actual dispatch logic is in Lisp (cil-stdlib.lisp).
/// </summary>
/// <summary>Cached result of GF dispatch for a specific argument type signature.</summary>
internal class CachedDispatch
{
    // null! rather than nullable: every construction site is an object
    // initializer in DispatchGF that fills all five, so the reader can treat
    // them as always present. A site that forgets one is a bug, not a state to
    // handle at each use.
    public LispClass?[] ArgTypes = null!;

    /// <summary>The effective method for these argument classes, as a function, when
    /// this generic function's invocation goes through the AMOP protocol. Null for the
    /// ordinary entries, which carry resolved method lists instead. Built once per
    /// argument-class vector, exactly like the rest of this entry, so the protocol is
    /// consulted on a cache miss rather than on every call.</summary>
    public LispFunction? EffectiveMethodFunction;
    public List<LispMethod> Around = null!;
    public List<LispMethod> Before = null!;
    public List<LispMethod> Primary = null!;
    public List<LispMethod> After = null!;
    public List<LispMethod>? Applicable; // for built-in method combination
    public bool HasEqlSpecializers;
    public bool IsBuiltinCombination;
    /// <summary>EQL-specialized methods to check on cache hit (only when HasEqlSpecializers).
    /// Stored only for single-required-arg GFs whose EQL methods are all unqualified
    /// (see the DispatchGF cache-store comment), so each entry's EQL specializer is
    /// at argument position 0.</summary>
    public LispMethod[]? EqlMethods;
    /// <summary>EqlMethods[i]'s EQL value (precomputed: the hit path compares the
    /// argument against this directly instead of re-walking the specializer cons).</summary>
    public LispObject[]? EqlValues;
    /// <summary>Precomputed effective primary chain for EqlMethods[i]:
    /// [EqlMethods[i], ..non-EQL primaries..]: avoids a per-hit list allocation.</summary>
    public List<LispMethod>[]? EqlChains;

    /// <summary>Specialized standard slot READER. When >= 0, the single
    /// applicable method is an accessor reader for the instance-allocated slot at this
    /// index in ArgTypes[0]'s layout: no before/after/around/eql, standard metaclass.
    /// The hit path reads the slot directly (SlotValueDirect), skipping effective-method
    /// construction and the reader lambda call. -1 = not a specialized reader.</summary>
    public int ReaderSlotIndex = -1;
    /// <summary>Slot-name symbol for ReaderSlotIndex's SlotValueDirect (unbound/missing
    /// reporting). Only meaningful when ReaderSlotIndex >= 0.</summary>
    public Symbol? ReaderSlotName;

    /// <summary>Specialized standard slot WRITER ((setf accessor)). When
    /// >= 0, the single applicable method is an accessor writer for the instance-
    /// allocated slot at this index in the OBJECT's layout: object is the last required
    /// arg (args[1] for a 2-arg setf writer), new value is args[0], no aux/eql, standard
    /// metaclass. The hit path writes the slot directly (SetSlotValueDirect). -1 = not
    /// a specialized writer.</summary>
    public int WriterSlotIndex = -1;
    /// <summary>Slot-name symbol for WriterSlotIndex. Only meaningful when >= 0.</summary>
    public Symbol? WriterSlotName;

    /// <summary>True when this entry runs a plain chain of primary methods with
    /// nothing around it: no :around/:before/:after, no EQL specializers, no built-in
    /// combination, and not one of the slot reader/writer shortcuts. That is the shape
    /// the loose-argument entry points (DISPATCHGF1/2/3/4) can run without building an
    /// argument array, so it is decided once here rather than re-derived from five list
    /// lengths on every call.
    ///
    /// The chain may hold more than one method: INVOKECHAINLOOSE publishes the chain
    /// state a body needs for CALL-NEXT-METHOD either way. Requiring exactly one was a
    /// restriction, not a safety condition -- and it meant a generic function paid 32 B
    /// on every call as soon as a second method became applicable, which is what any
    /// class hierarchy with a specialized method looks like.</summary>
    public bool PlainPrimaryChain;
    /// <summary>The same chain with :before and/or :after around it and no :around --
    /// also runnable without materialising an arguments array. Mutually exclusive with
    /// <see cref="PlainPrimaryChain"/>.</summary>
    public bool PrimaryChainWithBeforeAfter;

    /// <summary>Set the two shape flags from the fields the cache-store path has just
    /// filled. Called there, after the entry is complete.</summary>
    public void ComputePlainPrimaryChain()
    {
        PlainPrimaryChain = Around.Count == 0 && Before.Count == 0 && After.Count == 0
                            && Primary.Count >= 1
                            && !HasEqlSpecializers && !IsBuiltinCombination
                            && ReaderSlotIndex < 0 && WriterSlotIndex < 0;
        // The same shape with :before and/or :after methods around the primary
        // chain. Those run for effect, before and after it, on the same arguments
        // -- no next-method chain reaches them -- so the loose-argument path can
        // run the whole combination without ever building the array.
        // Worth separating because ONE :after method used to cost 32 B on every
        // call to the generic function, and (defmethod initialize-instance :after
        // ...) is an ordinary thing to write.
        PrimaryChainWithBeforeAfter = !PlainPrimaryChain
                            && Around.Count == 0
                            && Primary.Count >= 1
                            && !HasEqlSpecializers && !IsBuiltinCombination
                            && ReaderSlotIndex < 0 && WriterSlotIndex < 0;
    }
}

/// <summary>A per-call-site monomorphic inline cache for a simple slot
/// reader accessor. One instance is baked (as a unit constant) per <c>(reader obj)</c>
/// call site by the assembler; <see cref="Runtime.ReaderIC"/> reads and fills it.
/// The resolved (class, slot-index) is published atomically as an immutable
/// <see cref="Entry"/> via the volatile <see cref="E"/> field, so a concurrent fill can
/// never expose a torn (class, index) pair. Soundness against redefinition rides on the
/// snapshotted <see cref="GenericFunction.MethodEpoch"/>: any defmethod on the accessor or
/// any class re-layout bumps the epoch and forces a miss + re-resolve.</summary>
public sealed class ReaderCache
{
    /// <summary>The accessor name: used to (re)resolve the GF on a miss.</summary>
    internal readonly Symbol Sym;
    internal volatile Entry? E;
    public ReaderCache(Symbol sym) { Sym = sym; }

    internal sealed class Entry
    {
        internal readonly ClassLayout Layout;
        internal readonly int Idx;
        internal readonly int Epoch;
        internal Entry(ClassLayout layout, int idx, int epoch) { Layout = layout; Idx = idx; Epoch = epoch; }
    }
}

/// <summary>The writer twin of <see cref="ReaderCache"/>: a per-call-site monomorphic
/// inline cache for a simple slot writer, baked once per <c>(setf (accessor obj) v)</c>
/// call site and read/filled by <see cref="Runtime.WriterIC"/>. Same publication and
/// soundness rules: an immutable <see cref="Entry"/> published through the volatile
/// <see cref="E"/> field, invalidated wholesale by a <see cref="GenericFunction.MethodEpoch"/>
/// bump (defmethod on the writer, class re-layout).</summary>
public sealed class WriterCache
{
    /// <summary>The accessor name: the (SETF name) function is re-resolved from it on a miss.</summary>
    internal readonly Symbol Sym;
    /// <summary>Same shape as the reader's entry, and taken from the same per-layout
    /// memo (<see cref="ClassLayout.SlotEntry"/>).</summary>
    internal volatile ReaderCache.Entry? E;
    public WriterCache(Symbol sym) { Sym = sym; }
}

/// <summary>A method combination metaobject. AMOP has FIND-METHOD-COMBINATION
/// return one of these and GENERIC-FUNCTION-METHOD-COMBINATION hand it back;
/// dotcl decides the combination from a symbol plus its arguments, so this is a
/// thin record of exactly that, carrying no behaviour of its own. It exists
/// because closer-mop and portable metaobject code test the result with
/// (typep x 'method-combination), which a bare symbol fails.</summary>
public sealed class MethodCombinationObject : LispObject
{
    /// <summary>The method combination type name, e.g. STANDARD, PROGN, +.</summary>
    public Symbol TypeName { get; }
    /// <summary>The options the combination was found with, as a list.</summary>
    public LispObject Options { get; }

    public MethodCombinationObject(Symbol typeName, LispObject options)
    {
        TypeName = typeName;
        Options = options;
    }

    public override string ToString() => $"#<METHOD-COMBINATION {TypeName.Name}>";
}

public class GenericFunction : LispFunction
{
    public new Symbol Name { get; internal set; }

    // Method list is copy-on-write for thread safety: dispatch reads an
    // immutable snapshot (the `Methods` property returns the current array, which
    // foreach/Count/[i] enumerate consistently even if a concurrent defmethod swaps
    // it), while mutations (ADD-METHOD / REMOVE-METHOD / defgeneric-inline clear)
    // build a new array under `MethodsLock` and atomically publish it via the
    // volatile field. A plain List<T> here let concurrent enumerate-vs-Add corrupt
    // the applicable-method set -> spurious "CALL-NEXT-METHOD: no next method".
    private readonly object _methodsLock = new();
    private volatile LispMethod[] _methods = System.Array.Empty<LispMethod>();
    /// <summary>Read-only snapshot of the GF's methods. Enumeration is consistent:
    /// the property reads the volatile array reference once, so a concurrent
    /// ReplaceMethods swap cannot tear an in-progress loop.</summary>
    public IReadOnlyList<LispMethod> Methods => _methods;
    /// <summary>The same snapshot as METHODS, typed as the array it actually is.
    /// Enumerating through the IReadOnlyList interface allocates an enumerator on
    /// every loop; a hot path that walks the methods (initarg validation runs two of
    /// these per REINITIALIZE-INSTANCE) uses this instead. Read the reference once,
    /// exactly as the property does, so the loop cannot tear.</summary>
    internal LispMethod[] MethodsArray => _methods;
    /// <summary>Lock held while building+publishing a new method array (write path only).</summary>
    internal object MethodsLock => _methodsLock;
    /// <summary>Publish a new method array (volatile write). Call under MethodsLock.</summary>
    internal void ReplaceMethods(LispMethod[] methods) => _methods = methods;
    public LispFunction? DispatchFunction { get; set; }

    /// <summary>Whether this generic function's invocation goes through
    /// COMPUTE-APPLICABLE-METHODS and friends, i.e. whether a method on one of those
    /// applies to it. Null until asked; cleared for every generic function when a
    /// method is added to or removed from one of the protocol generic functions.</summary>
    public bool? UsesInvocationProtocol { get; set; }

    /// <summary>Slots added by a user-defined generic function class. A generic
    /// function is callable, so it cannot also be a LispInstance and carry a slot
    /// vector; this is the same escape hatch LispClass and SlotDefinition use for
    /// metaobjects whose class declares slots. Null until one is written.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?>? ExtraSlots;

    /// <summary>Atomically obtain the ExtraSlots table, creating it on first use.</summary>
    public System.Collections.Concurrent.ConcurrentDictionary<string, LispObject?> EnsureExtraSlots()
        => ExtraSlots ?? System.Threading.Interlocked.CompareExchange(
               ref ExtraSlots, new(), null) ?? ExtraSlots;
    /// <summary>Method combination type: null means STANDARD, otherwise the operator symbol (+, LIST, APPEND, etc.)</summary>
    public Symbol? MethodCombination { get; set; }
    /// <summary>Method combination arguments from defgeneric (:method-combination name arg1 arg2 ...)</summary>
    public LispObject[]? MethodCombinationArgs { get; set; }
    /// <summary>Declarations for the generic function, as a list. AMOP passes
    /// them with the :DECLARATIONS initarg and reads them back with
    /// GENERIC-FUNCTION-DECLARATIONS; ANSI DEFGENERIC spells the same thing
    /// (declare ...), and both land here. NIL when none were given.</summary>
    public LispObject Declarations { get; set; } = Nil.Instance;
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
    /// <summary>The placeholder lambda list rebuilt from the arity when no real one
    /// was given, kept so repeated calls answer the same list. The parameter names
    /// in it are fresh uninterned symbols, that is deliberate, since dotcl does not
    /// keep the source names, but rebuilding it per call made
    /// (equal (generic-function-lambda-list g) (generic-function-lambda-list g))
    /// false, which no caller expects.</summary>
    internal LispObject? PlaceholderLambdaList;
    /// <summary>Actual Lisp class of this GF instance (for subclasses of standard-generic-function).</summary>
    public LispClass? StoredClass { get; set; }

    /// <summary>When a GF auto-created by defmethod replaces an ordinary function, the
    /// original is saved here. The dispatcher uses it as a last-resort fallback when
    /// no applicable method is found, preserving built-in behaviour for CL functions
    /// (e.g. CLOSE, STREAM-ELEMENT-TYPE) that have user-defined Gray-stream methods
    /// without losing the original C# implementation for non-Gray streams.</summary>
    public LispFunction? FallbackFunction { get; set; }

    /// <summary>N-way polymorphic dispatch cache. An immutable array of recent
    /// successful dispatches (most-recent first), swapped atomically via this `volatile`
    /// field so a concurrent InvalidateCache (defmethod) / cache-fill publishes a
    /// complete snapshot: a reader never sees a torn array, worst case a complete but
    /// slightly stale one. Bounded to <see cref="DispatchCacheWidth"/> entries: a call
    /// site that cycles through up to that many argument-class combinations stays warm,
    /// where the old single-entry monomorphic cache missed on every alternation.</summary>
    internal volatile CachedDispatch[]? DispatchCache;

    /// <summary>Max entries in the polymorphic dispatch cache.</summary>
    internal const int DispatchCacheWidth = 4;

    /// <summary>Second level behind <see cref="DispatchCache"/>: every entry made for
    /// one dispatch class, by that class. The cache belongs to the generic function,
    /// not to a call site, so INITIALIZE-INSTANCE or PRINT-OBJECT in a program that
    /// uses more classes than the front cache holds would otherwise recompute the
    /// applicable methods on most calls. Valid only while <see cref="MethodEpoch"/>
    /// is the one it was made under: a method change or a class (re)finalization
    /// anywhere starts a new one.</summary>
    internal volatile DispatchTable1? DispatchByClass;

    internal sealed class DispatchTable1
    {
        internal readonly int Epoch;
        internal readonly System.Collections.Concurrent.ConcurrentDictionary<LispClass, CachedDispatch> Map
            = new(ReferenceEqualityComparer.Instance);
        internal DispatchTable1(int epoch) { Epoch = epoch; }
    }

    /// <summary>Invalidate dispatch cache when methods are added/removed.</summary>
    internal void InvalidateCache() { DispatchCache = null; DispatchByClass = null; BumpMethodEpoch(); }

    /// <summary>Global method-system epoch. Bumped on any method add/remove (via
    /// <see cref="InvalidateCache"/>) and on any class finalization (slot-layout change).
    /// Call-site reader inline caches (<see cref="ReaderCache"/>) snapshot this and miss
    /// wholesale when it moves, so a redefined accessor GF or a re-laid-out class can never
    /// serve a stale cached slot index. A plain int compare on the hot path.</summary>
    internal static volatile int MethodEpoch;
    internal static void BumpMethodEpoch() => System.Threading.Interlocked.Increment(ref MethodEpoch);

    /// <summary>Non-null iff this GF is a PURE simple slot reader: every
    /// method is an unqualified defclass-generated accessor reader for a slot of this
    /// (common) name, no user/aux methods. Then a call site can read the slot directly
    /// via Runtime.ReaderFast, skipping the whole dispatch. Cleared (null) the moment any
    /// non-accessor / qualified / differing-slot method is present, so a redefined or
    /// extended accessor safely falls back to full dispatch. Maintained by
    /// RecomputeAccessorFlags on every method add/remove and accessor tagging.</summary>
    public Symbol? SimpleReaderSlot;
    /// <summary>As SimpleReaderSlot, for a pure (setf accessor) writer.</summary>
    public Symbol? SimpleWriterSlot;

    /// <summary>Recompute SimpleReaderSlot / SimpleWriterSlot from the current methods.
    /// Both null unless every method is an unqualified accessor (AccessorSlot set) of one
    /// common slot name and one common arity: arity 1 -> reader, arity 2 -> writer.</summary>
    internal void RecomputeAccessorFlags()
    {
        SimpleReaderSlot = null;
        SimpleWriterSlot = null;
        var methods = Methods;
        if (methods.Count == 0) return;
        Symbol? slot = null;
        int arity = -1;
        foreach (var m in methods)
        {
            if (m.Qualifiers.Length != 0) return;              // :before/:after/:around
            if (m.AccessorSlot is not { } sd) return;          // user / non-accessor method
            if (slot == null) slot = sd.Name;
            else if (!LispClass.SameSlotName(slot, sd.Name)) return; // differing slot
            if (arity == -1) arity = m.Specializers.Length;
            else if (arity != m.Specializers.Length) return;   // mixed reader/writer
        }
        if (slot == null) return;
        if (arity == 1) SimpleReaderSlot = slot;
        else if (arity == 2) SimpleWriterSlot = slot;
    }

    public GenericFunction(Symbol name, int arity, Func<LispObject[], LispObject> dispatchFn)
        : base(dispatchFn, name.Name, arity)
    {
        Name = name;
        Track(this);
    }

    // Every generic function ever made, named or not, held weakly: a class
    // redefinition has to drop the dispatch caches of all of them (see
    // InvalidateAllCaches), and one made with MAKE-INSTANCE is in no name table.
    private static readonly List<WeakReference<GenericFunction>> _all = new();
    private static int _pruneAt = 256;

    private static void Track(GenericFunction gf)
    {
        lock (_all)
        {
            _all.Add(new WeakReference<GenericFunction>(gf));
            if (_all.Count >= _pruneAt)
            {
                _all.RemoveAll(w => !w.TryGetTarget(out _));
                _pruneAt = Math.Max(256, _all.Count * 2);
            }
        }
    }

    /// <summary>Drop the dispatch caches of every live generic function.</summary>
    internal static void InvalidateAllCaches()
    {
        GenericFunction[] live;
        lock (_all)
        {
            var list = new List<GenericFunction>(_all.Count);
            foreach (var w in _all)
                if (w.TryGetTarget(out var gf)) list.Add(gf);
            live = list.ToArray();
        }
        foreach (var gf in live) gf.InvalidateCache();
    }

    public override string ToString() => $"#<GENERIC-FUNCTION {Name.Name}>";
}
