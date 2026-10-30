using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    private int _inlineDepth;
    private const int MaxInlineDepth = 500;
    // Constant graph nodes emitted into the method this assembler is building.
    // One oversized literal is already bounded (MaxInlineDepth for deep ones,
    // MaxInlineListChunk for long lists and vectors), but a method holding MANY
    // medium-sized literals had no bound at all: each one passed every existing
    // check and they simply accumulated. That is how a top level form with ~130
    // moderate literals reached megabytes of IL in a single method, which is
    // what the JIT has to chew through at load time.
    private int _constNodes;
    private const int MaxConstNodesPerMethod = 2000;
    private HashSet<LispStruct>? _inlineVisited;
    private bool _skipStructIntern; // skip intern key for structs inside hash table values
    // Shared across all CilAssemblers in one FASL to deduplicate struct intern keys
    internal FaslStructInternMap? _faslStructMap;

    // DefineInitializedData refuses a blob of 0x3f0000 bytes or more. Split at a
    // round figure below that; the pieces are concatenated at load.
    internal const int MaxLiteralDataChunk = 0x300000;

    private string Track(string s) =>
        _faslStructMap?.TrackString(s) ?? s;

    /// <summary>Set on the assembler that fills a _const_N helper: the call site
    /// that reaches the helper is already behind a literal cache, so caching the
    /// same graph again inside would only add a second field.</summary>
    private bool _suppressLiteralCache;

    /// <summary>Nonzero while emitting the IL that builds a literal whose result
    /// is kept in a literal cache field (EmitLoadConstInline, EmitCachedConst).
    /// That IL runs once per field, so a symbol inside it gains nothing from a
    /// per-site cache field of its own: EmitInternedSymbol then emits the plain
    /// lookup, 3 instructions instead of 9 and no branch. The code is run once
    /// and JIT-compiled at load, where its size is most of the cost.</summary>
    private int _cachedLiteralDepth;

    /// <summary>Set on the assembler of a fasl top level helper (_toplevel_N)
    /// whose instructions have no backward branch. ModuleInit calls such a
    /// helper once and nothing in it runs twice, so each literal and symbol site
    /// in it executes at most once per load: a cache field for it would be
    /// filled and never read again. The sites are then emitted uncached, which
    /// is most of the IL of a typical top level form, and all of that IL is
    /// JIT-compiled at load only to run once.</summary>
    internal bool _runOnce;

    /// <summary>Set on the assembler of an in-memory top level method (EVAL)
    /// whose instructions have no backward branch, so each call in it runs at
    /// most once: calls through LispFunction.InvokeN go to InvokeOnceN, which
    /// the JIT does not inline.</summary>
    private bool _runOnceInMemory;

    /// <summary>Whether a flat SIL instruction list has a branch (or LEAVE /
    /// SWITCH) to a label that appears before it, i.e. whether any of its code
    /// can run more than once in one call.</summary>
    internal static bool HasBackwardBranch(LispObject instrs)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        for (var cur = instrs; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is not Cons ins || ins.Car is not Symbol op) continue;
            switch (op.Name)
            {
                case "LABEL":
                    if (ins.Cdr is Cons l && l.Car is Symbol ls) seen.Add(ls.Name);
                    break;
                case "BR": case "BRFALSE": case "BRTRUE": case "BEQ": case "BGT":
                case "BLT": case "BGE": case "BLE": case "BNE": case "LEAVE":
                case "BGE-UN": case "BGT-UN": case "BLE-UN": case "BLT-UN":
                    if (ins.Cdr is Cons t && t.Car is Symbol ts && seen.Contains(ts.Name))
                        return true;
                    break;
                case "SWITCH":
                    if (ins.Cdr is Cons sw)
                        for (var lc = sw.Car; lc is Cons lcc; lc = lcc.Cdr)
                            if (lcc.Car is Symbol s2 && seen.Contains(s2.Name)) return true;
                    break;
            }
        }
        return false;
    }

    /// <summary>Emit IL to construct a constant value inline (for FASL mode).
    ///
    /// A top level literal is built once and kept in a static field. Without that
    /// the construction IL sits in the function body and runs on every call: the
    /// literal came back as a different object each time (so EQ on it was NIL and a
    /// destructive change to it vanished), and the whole graph was allocated per
    /// call. Both only in a fasl -- loading the same source kept one object -- so it
    /// showed up only in what we ship.
    ///
    /// Filled at first use rather than in the type initializer: a literal can name
    /// a class or a symbol that an earlier top level form in the same file
    /// establishes, and the initializer runs before any of them.</summary>
    internal void EmitLoadConstInline(LispObject val)
    {
        if (_inlineDepth == 0 && _faslMode && !_suppressLiteralCache && !_runOnce
            && _faslStructMap?.UninternedTypeBuilder != null)
        {
            var field = _faslStructMap.DefineLiteralCacheField();
            var done = _il.DefineLabel();
            _il.Emit(OpCodes.Ldsfld, field);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Brtrue, done);
            _il.Emit(OpCodes.Pop);
            _cachedLiteralDepth++;
            try { EmitLoadConstInlineBody(val); }
            finally { _cachedLiteralDepth--; }
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Stsfld, field);
            _il.MarkLabel(done);
            return;
        }
        EmitLoadConstInlineBody(val);
    }

    /// <summary>Objects that occur more than once in the literal being emitted,
    /// and the static field each one is kept in once built. Inline emission
    /// otherwise builds a fresh object for every occurrence, so
    /// '(#1=#P"foo" #1#) came back as two pathnames (CLHS 3.2.4.4 requires one).
    /// The first occurrence builds the object and stores it; later occurrences
    /// load the field. Emission order is execution order (helper methods are
    /// called where they are emitted), so a load never runs before its store.
    /// Shared with the helper-method assemblers of the same literal.</summary>
    private sealed class SharedLiteralMemo
    {
        public readonly HashSet<LispObject> Shared;
        public readonly Dictionary<LispObject, FieldInfo> Fields =
            new(ReferenceEqualityComparer.Instance);
        public readonly HashSet<LispObject> Building =
            new(ReferenceEqualityComparer.Instance);
        public readonly LispObject Root;
        public SharedLiteralMemo(LispObject root, HashSet<LispObject> shared) { Root = root; Shared = shared; }
    }
    private SharedLiteralMemo? _sharedMemo;

    /// <summary>The identity-carrying objects that occur in more than one
    /// literal (LOAD-CONST operand) of one top level form's instructions,
    /// nested function bodies included. CLHS 3.2.4.4 asks for one object per
    /// object in the source, but each literal is built by its own code, so
    /// a macro that puts one subform into two quoted constants
    /// (`(list '(wrap ,x) ',x)`) gave two copies of it once compiled to a
    /// fasl. The literals of a form run in no fixed order (each is built at
    /// first use), so these objects are built by whichever literal gets there
    /// first and kept in a field the others read (see EmitLoadConstInlineBody).
    ///
    /// A walk stops at an object an earlier literal already reached: what is
    /// below it comes with it. <paramref name="roots"/> gets the literals that
    /// contain a shared object, on both sides.</summary>
    internal static Dictionary<LispObject, System.Reflection.Emit.FieldBuilder?> CollectCrossLiteralNodes(
        LispObject instrList, out HashSet<LispObject> roots)
    {
        var shared = new Dictionary<LispObject, System.Reflection.Emit.FieldBuilder?>(
            ReferenceEqualityComparer.Instance);
        roots = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        // The literals, in order. Instruction lists are walked without going
        // into a literal itself.
        var literals = new List<LispObject>();
        var walked = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        var ist = new Stack<LispObject>();
        ist.Push(instrList);
        while (ist.Count > 0)
        {
            if (ist.Pop() is not Cons c || !walked.Add(c)) continue;
            if (c.Car is Symbol op && op.Name == "LOAD-CONST" && c.Cdr is Cons arg)
            {
                if (HasLiteralIdentity(arg.Car)) literals.Add(arg.Car);
                continue;
            }
            ist.Push(c.Cdr);
            ist.Push(c.Car);
        }
        if (literals.Count < 2) return shared;

        var owner = new Dictionary<LispObject, LispObject>(ReferenceEqualityComparer.Instance);
        var st = new Stack<LispObject>();
        foreach (var root in literals)
        {
            var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
            st.Push(root);
            while (st.Count > 0)
            {
                var node = st.Pop();
                if (!HasLiteralIdentity(node) || !seen.Add(node)) continue;
                if (owner.TryGetValue(node, out var first))
                {
                    shared[node] = null;
                    roots.Add(first);
                    roots.Add(root);
                    continue;
                }
                owner[node] = root;
                switch (node)
                {
                    case Cons cc: st.Push(cc.Cdr); st.Push(cc.Car); break;
                    case LispVector v:
                        if (v.ElementTypeName == "T")
                            for (int i = v.Length - 1; i >= 0; i--) st.Push(v.ElementAt(i));
                        break;
                    case LispStruct ls:
                        foreach (var sl in ls.SlotsSnapshot()) if (sl is LispObject so) st.Push(so);
                        break;
                    case LispHashTable ht:
                        foreach (var kv in ht.Entries) { st.Push(kv.Value); st.Push(kv.Key); }
                        break;
                    case LispInstance li:
                        foreach (var sl in li.Slots) if (sl is LispObject so) st.Push(so);
                        break;
                }
            }
        }
        return shared;
    }

    private void EmitLoadConstInlineBody(LispObject val)
    {
        // An object another literal of this form also contains: build it only
        // if no literal has yet, and keep it in its field.
        var map = _faslStructMap;
        var cross = map?.CrossLiteralShared;
        if (cross != null && map!.UninternedTypeBuilder != null
            && cross.ContainsKey(val) && !map.CrossLiteralBuilding.Contains(val))
        {
            // The object is kept in a slot of the fasl's shared array rather
            // than a field of its own, so that a literal built from data (see
            // FaslData) finds it in the same place.
            int slot = map.CrossLiteralSlot(val);
            var built = _il.DefineLabel();
            _il.Emit(OpCodes.Ldsfld, map.SharedSlotsField());
            _il.Emit(OpCodes.Ldc_I4, slot);
            _il.Emit(OpCodes.Ldelem_Ref);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Brtrue, built);
            _il.Emit(OpCodes.Pop);
            map.CrossLiteralBuilding.Add(val);
            try { EmitLoadConstInlineBodyMemo(val); }
            finally { map.CrossLiteralBuilding.Remove(val); }
            _il.Emit(OpCodes.Ldsfld, map.SharedSlotsField());
            _il.Emit(OpCodes.Ldc_I4, slot);
            _il.Emit(OpCodes.Call, typeof(FaslData).GetMethod("Share")!);
            _il.MarkLabel(built);
            return;
        }
        EmitLoadConstInlineBodyMemo(val);
    }

    private void EmitLoadConstInlineBodyMemo(LispObject val)
    {
        bool ownsMemo = false;
        if (_inlineDepth == 0 && _sharedMemo == null && _faslMode
            && _faslStructMap?.UninternedTypeBuilder != null)
        {
            var shared = CollectSharedLiteralNodes(val);
            if (shared.Count > 0) { _sharedMemo = new SharedLiteralMemo(val, shared); ownsMemo = true; }
        }
        try
        {
            var memo = _sharedMemo;
            if (memo != null && memo.Shared.Contains(val) && !memo.Building.Contains(val))
            {
                if (memo.Fields.TryGetValue(val, out var f))
                {
                    _il.Emit(OpCodes.Ldsfld, f);
                    return;
                }
                memo.Building.Add(val);
                try { EmitLoadConstInlineBodyCore(val); }
                finally { memo.Building.Remove(val); }
                var field = _faslStructMap!.DefineLiteralCacheField();
                _il.Emit(OpCodes.Dup);
                _il.Emit(OpCodes.Stsfld, field);
                memo.Fields[val] = field;
                return;
            }
            EmitLoadConstInlineBodyCore(val);
        }
        finally { if (ownsMemo) _sharedMemo = null; }
    }

    /// <summary>Whether an object's identity has to be kept when it occurs
    /// more than once in one literal. Numbers and characters have no identity
    /// that EQ is required to see; symbols, packages, named classes and
    /// functions are looked up by name, and strings are already coalesced by
    /// value into one field per fasl.</summary>
    private static bool HasLiteralIdentity(LispObject o) =>
        o is Cons or LispVector or LispPathname or LispHashTable or LispStruct or LispInstance;

    /// <summary>The identity-carrying nodes reachable more than once from
    /// <paramref name="root"/>. Walks everything the inline emitter walks
    /// (conses, vectors, structure slots, hash table entries, instance slots),
    /// iteratively. Over-approximating is harmless: a node listed here that the
    /// emitter reaches only once is built once and stored in a field nobody
    /// reads.</summary>
    private static HashSet<LispObject> CollectSharedLiteralNodes(LispObject root)
    {
        var shared = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        if (!HasLiteralIdentity(root)) return shared;
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        var st = new Stack<LispObject>();
        st.Push(root);
        while (st.Count > 0)
        {
            var node = st.Pop();
            if (!HasLiteralIdentity(node)) continue;
            if (!seen.Add(node)) { shared.Add(node); continue; }
            switch (node)
            {
                case Cons c: st.Push(c.Cdr); st.Push(c.Car); break;
                case LispVector v:
                    if (v.ElementTypeName == "T")
                        for (int i = v.Length - 1; i >= 0; i--) st.Push(v.ElementAt(i));
                    break;
                case LispStruct ls:
                    foreach (var s in ls.SlotsSnapshot()) if (s is LispObject so) st.Push(so);
                    break;
                case LispHashTable ht:
                    foreach (var kv in ht.Entries) { st.Push(kv.Value); st.Push(kv.Key); }
                    break;
                case LispInstance li:
                    foreach (var s in li.Slots) if (s is LispObject so) st.Push(so);
                    break;
            }
        }
        return shared;
    }

    /// <summary>True when the literal shares an object that the print-and-read
    /// route cannot give back as one object (anything but a cons or a general
    /// vector): such a literal must be built inline, where the memo keeps it.
    /// A part of the literal (a list chunk or a spilled subgraph, emitted by a
    /// helper assembler) must not take that route either: its printed text
    /// could not refer to the objects the rest of the literal shares.
    private bool ReaderRouteLosesSharing(LispObject val) =>
        (_faslStructMap?.CrossLiteralRoots?.Contains(val) ?? false)
        || _sharedMemo != null
        && (!ReferenceEquals(val, _sharedMemo.Root)
            || _sharedMemo.Shared.Any(o => o is not Cons
                                   && !(o is LispVector v && v.ElementTypeName == "T" && v._dimensions == null)));

    // The atom and aggregate cases below leave a Symbol, Cons, LispString and
    // so on on the stack without a CASTCLASS to LispObject: an upcast is a no-op
    // the JIT still has to resolve, and fasl literals are most of the IL a
    // load JIT-compiles.
    private void EmitLoadConstInlineBodyCore(LispObject val)
    {
        _inlineDepth++;
        try
        {
        // Budget exhausted for this method: continue this literal in its own
        // helper method (which starts with a fresh budget). Only aggregates are
        // worth a method: spilling an atom would cost more IL than it saves.
        // No recursion risk: the helper's own budget starts at zero, so the very
        // value that triggered the spill is emitted inline there.
        if (_faslMode && _faslTypeBuilder != null
            && _constNodes > MaxConstNodesPerMethod && IsSpillWorthy(val))
        {
            EmitFaslDeepConstant(val);
            return;
        }
        _constNodes++;
        // A circular constant literal (e.g. '#1=(1 2 3 . #1#)) cannot be rebuilt by
        // the naive cons-by-cons inline emitter: the cdr-flatten loop spins on the
        // cycle and OOMs. Shared (acyclic but #N=-coalesced) structure rebuilds but
        // loses EQ identity, since inline duplicates each occurrence. Both are fixed
        // by emitting a load-time (read-from-string "<*print-circle* repr>"), which
        // reconstructs the graph, sharing and cycles, exactly. (The non-FASL path
        // stores the live object in the constant pool, so it never hit this.)
        // cl-store circ.*/correct.list tests.
        if (_inlineDepth == 1)
        {
            var kind = AnalyzeConstantGraph(val);
            bool readerLosesSharing = ReaderRouteLosesSharing(val);
            if (LiteralCensus)
            {
                // Every literal, whichever route it ends up on -- the question is
                // what fraction of a rejected graph is actually unprintable, and
                // that cannot be seen from inside the reader-safety check, which
                // some routes never reach.
                ConstantIsReaderSafe(val, out _);
                Console.Error.WriteLine($"#CENSUS-KIND {kind}");
            }
            if (kind == ConstGraphKind.Cyclic)
            {
                if (!TryEmitConstantViaReader(val))
                    throw new LispErrorException(new LispProgramError(
                        "compile-file: cannot emit a circular constant that is not printably readable"));
                return;
            }
            // A literal of plain data is rebuilt from a data payload (see
            // FaslData) instead of by IL that is JIT-compiled to run once.
            // Unlike the reader route it takes instances (by their
            // make-load-form key), uninterned symbols, and objects it shares
            // with other literals of the form, which macro expansions that
            // carry a code walker's state hold in quantity. A part of a larger
            // literal is left to the code that builds the whole.
            if (_faslMode && _faslTypeBuilder != null
                && (_sharedMemo == null || ReferenceEquals(_sharedMemo.Root, val))
                && TryEmitConstantAsData(val))
                return;
            if (kind == ConstGraphKind.Shared && !readerLosesSharing && TryEmitConstantViaReader(val))
                return;
            // Otherwise shared structure is built inline, and _sharedMemo keeps
            // each shared object one object.

            // A large ordinary literal goes the same way, for size rather than
            // correctness. Building it inline emits IL proportional to the graph;
            // roughly 25 bytes a node: and that IL is JITted at load to be run
            // exactly once. Measured on a literal-heavy fasl: 97% of load time is
            // JIT, and the memory it commits for that code is several times the
            // data the literal produces. As text it is data, and reading is work
            // the loading process does with code that is already compiled.
            //
            // Only for graphs the reader reconstructs with IDENTICAL semantics
            // (see ConstantIsReaderSafe) and only past a threshold, since a small
            // literal's inline IL is cheaper than the fixed cost of a read.
            if (kind == ConstGraphKind.Simple && !readerLosesSharing
                && _faslMode && _faslTypeBuilder != null
                && ConstantIsReaderSafe(val, out int nodeCount)
                && nodeCount >= ReaderPathMinNodes
                && TryEmitConstantViaReader(val))
                return;
        }
        if (_inlineDepth > MaxInlineDepth)
        {
            if (_faslMode && _faslTypeBuilder != null)
            {
                // FASL: continue in a helper method instead. The constant pool is
                // process-local, so a fasl that reaches into it picks up whatever
                // unrelated object sits at that index in the loading process (or
                // indexes past the pool): silent corruption of the literal.
                EmitFaslDeepConstant(val);
                return;
            }
            // Too deep: fallback to constant pool (same process only)
            EmitConstantAs(AddConstant(val), typeof(LispObject));
            return;
        }
        switch (val)
        {
            case Nil:
                _il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
                break;
            case T:
                _il.Emit(OpCodes.Ldsfld, typeof(T).GetField("Instance")!);
                break;
            case Fixnum f:
                _il.Emit(OpCodes.Ldc_I8, f.Value);
                _il.Emit(OpCodes.Call, _methodCache["Fixnum.Make"]);
                break;
            case LispString s:
                // In a FASL the literal lives in a static field built once by the
                // type initializer; inline construction would run on every
                // evaluation of the expression, not once per site.
                if (_faslMode && _faslStructMap?.UninternedTypeBuilder != null
                    && _faslStructMap.SymFnSiteInitIl != null)
                {
                    _il.Emit(OpCodes.Ldsfld, _faslStructMap.GetOrCreateStringField(s.Value));
                }
                else
                {
                    _il.Emit(OpCodes.Ldstr, Track(s.Value));
                    _il.Emit(OpCodes.Newobj, _ctorCache["LispString"]);
                }
                break;
            case Symbol sym:
                if (sym.HomePackage?.Name == "KEYWORD")
                {
                    EmitKeyword(sym.Name);
                }
                else if (sym.HomePackage != null)
                {
                    // Resolve once per site, like a LOAD-SYM-PKG reference. A quoted
                    // symbol inside a literal used to re-run the string-keyed package
                    // lookup on every evaluation: SBCL's quasiquote expander holds
                    // QUOTE, NCONC, SB-IMPL::|Append| and friends as literals, and a
                    // make-host-1 run resolved symbols by string 7,049,074 times --
                    // over a million of them QUOTE alone.
                    EmitInternedSymbol(sym.Name, sym.HomePackage.Name);
                    // Record it so the fasl also interns it at load time: the site
                    // only interns when the constant is actually built, and a macro
                    // template does that at expansion time, not at load.
                    if (_faslMode) _faslStructMap?.RecordSymbolReference(sym.Name, sym.HomePackage.Name);
                }
                else if (_faslMode && _faslStructMap?.UninternedTypeBuilder != null)
                {
                    // FASL mode: uninterned symbols must be deduplicated so the same
                    // Symbol object is used everywhere in this assembly (preserves EQ-ness
                    // across make-load-form boundaries: e.g. gensym'd ctor names in defcontext).
                    _il.Emit(OpCodes.Ldsfld, _faslStructMap.UninternedTableField());
                    _il.Emit(OpCodes.Ldc_I4, _faslStructMap.GetOrCreateUninternedSymbolIndex(sym));
                    _il.Emit(OpCodes.Ldelem_Ref);
                }
                else
                {
                    // AssembleAndRun (non-FASL): each occurrence creates a new Symbol.
                    _il.Emit(OpCodes.Ldstr, Track(sym.Name));
                    _il.Emit(OpCodes.Ldnull); // null for homePackage
                    _il.Emit(OpCodes.Newobj, typeof(Symbol).GetConstructor(new[] { typeof(string), typeof(Package) })!);
                }
                break;
            case LispChar lc:
                _il.Emit(OpCodes.Ldc_I4, (int)lc.Value);
                _il.Emit(OpCodes.Call, typeof(LispChar).GetMethod("Make")!);
                break;
            case SingleFloat sf:
                _il.Emit(OpCodes.Ldc_R4, sf.Value);
                _il.Emit(OpCodes.Newobj, typeof(SingleFloat).GetConstructor(new[] { typeof(float) })!);
                break;
            case DoubleFloat df:
                _il.Emit(OpCodes.Ldc_R8, df.Value);
                _il.Emit(OpCodes.Newobj, typeof(DoubleFloat).GetConstructor(new[] { typeof(double) })!);
                break;
            case Cons cons:
            {
                // Flatten proper list to avoid deep CDR recursion
                var cars = new List<LispObject>();
                LispObject tail = cons;
                while (tail is Cons c) { cars.Add(c.Car); tail = c.Cdr; }
                if (_faslMode && _faslTypeBuilder != null && cars.Count > MaxInlineListChunk)
                {
                    EmitFaslChunkedList(cars, tail);
                    break;
                }
                var consCtor = typeof(Cons).GetConstructor(new[] { typeof(LispObject), typeof(LispObject) })!;
                // Emit tail (nil for proper list, or dotted cdr)
                EmitLoadConstInline(tail);
                // Build cons cells right-to-left: stack has accumulated cdr
                for (int ci = cars.Count - 1; ci >= 0; ci--)
                {
                    // Stack: cdr; need car on top for new Cons(car, cdr)
                    // Use a temp local to swap
                    var tmpName = $"__cons_tmp_{_inlineDepth}_{ci}";
                    if (!_locals.TryGetValue(tmpName, out var tmpLocal))
                    {
                        tmpLocal = _il.DeclareLocal(typeof(LispObject));
                        _locals[tmpName] = tmpLocal;
                    }
                    _il.EmitLoc(OpCodes.Stloc, tmpLocal); // save cdr
                    EmitLoadConstInline(cars[ci]);       // push car
                    _il.EmitLoc(OpCodes.Ldloc, tmpLocal);   // push cdr
                    _il.Emit(OpCodes.Newobj, consCtor);
                }
                break;
            }
            case LispVector vec:
                // Build vector inline.
                // Use ElementAt()/Length so bit-vectors work: their elements live in
                // _bitData, not _elements (_elements is empty for bit-vectors).
                int vecLen = vec.Length;
                if (vec._dimensions == null && vecLen > 32 && vec.TryPackLiteral(out var packed))
                {
                    _il.Emit(OpCodes.Ldstr, Track(Convert.ToBase64String(packed)));
                    _il.Emit(OpCodes.Ldc_I4, vecLen);
                    _il.Emit(OpCodes.Ldstr, vec.ElementTypeName);
                    _il.Emit(OpCodes.Call, typeof(LispVector).GetMethod(nameof(LispVector.FromPackedLiteral))!);
                    break;
                }
                _il.Emit(OpCodes.Ldc_I4, vecLen);
                _il.Emit(OpCodes.Newarr, typeof(LispObject));
                if (_faslMode && _faslTypeBuilder != null && vecLen > MaxInlineListChunk)
                {
                    // Same JIT-working-set concern as long lists: fill the backing
                    // array through chunk methods instead of one giant body.
                    EmitFaslChunkedVectorFill(vec, vecLen);
                }
                else
                {
                    for (int i = 0; i < vecLen; i++)
                    {
                        _il.Emit(OpCodes.Dup);
                        _il.Emit(OpCodes.Ldc_I4, i);
                        EmitLoadConstInline(vec.ElementAt(i));
                        _il.Emit(OpCodes.Stelem_Ref);
                    }
                }
                if (vec._dimensions != null)
                {
                    // Multi-dimensional array: preserve rank/dimensions via the
                    // (LispObject[], int[], string) ctor. Without this, #2A(...)
                    // literals in compile-file output come back as flat 1D
                    // vectors.
                    var dims = vec._dimensions;
                    _il.Emit(OpCodes.Ldc_I4, dims.Length);
                    _il.Emit(OpCodes.Newarr, typeof(int));
                    for (int di = 0; di < dims.Length; di++)
                    {
                        _il.Emit(OpCodes.Dup);
                        _il.Emit(OpCodes.Ldc_I4, di);
                        _il.Emit(OpCodes.Ldc_I4, dims[di]);
                        _il.Emit(OpCodes.Stelem_I4);
                    }
                    _il.Emit(OpCodes.Ldstr, vec.ElementTypeName);
                    _il.Emit(OpCodes.Newobj,
                        typeof(LispVector).GetConstructor(
                            new[] { typeof(LispObject[]), typeof(int[]), typeof(string) })!);
                }
                else if (vec.ElementTypeName != "T")
                {
                    // 1D vector with custom element-type (SINGLE-FLOAT, BIT, etc.):
                    // preserve via (LispObject[], string) ctor. Keeps _dimensions null
                    // so the vector stays a proper 1D type.
                    _il.Emit(OpCodes.Ldstr, vec.ElementTypeName);
                    _il.Emit(OpCodes.Newobj,
                        typeof(LispVector).GetConstructor(
                            new[] { typeof(LispObject[]), typeof(string) })!);
                }
                else
                {
                    _il.Emit(OpCodes.Newobj, _ctorCache["LispVector"]);
                }
                break;
            case Bignum bn:
                // BigInteger.Parse(string) -> new Bignum(BigInteger)
                _il.Emit(OpCodes.Ldstr, bn.Value.ToString());
                _il.Emit(OpCodes.Call, typeof(System.Numerics.BigInteger).GetMethod("Parse", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Newobj, typeof(Bignum).GetConstructor(new[] { typeof(System.Numerics.BigInteger) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case Ratio rat:
                // Ratio.Make(BigInteger.Parse(num), BigInteger.Parse(den))
                _il.Emit(OpCodes.Ldstr, rat.Numerator.ToString());
                _il.Emit(OpCodes.Call, typeof(System.Numerics.BigInteger).GetMethod("Parse", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Ldstr, rat.Denominator.ToString());
                _il.Emit(OpCodes.Call, typeof(System.Numerics.BigInteger).GetMethod("Parse", new[] { typeof(string) })!);
                _il.Emit(OpCodes.Call, typeof(Ratio).GetMethod("Make")!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispComplex cx:
                // Reconstruct from real/imag parts through LispComplex.Of, which
                // picks the representation. The parts are themselves constants
                // (fixnum/ratio/float/bignum), so emit them inline and cast each
                // to Number for the call.
                EmitLoadConstInline(cx.Real);
                _il.Emit(OpCodes.Castclass, typeof(Number));
                EmitLoadConstInline(cx.Imaginary);
                _il.Emit(OpCodes.Castclass, typeof(Number));
                _il.Emit(OpCodes.Call,
                    typeof(LispComplex).GetMethod("Of", new[] { typeof(Number), typeof(Number) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispPathname pn:
                // A namestring carries no version syntax, so FromString(namestring)
                // drops the version (e.g. :NEWEST). When the literal pathname has a
                // version, reconstruct it with FromStringWithVersion so #. round-trips
                // preserve it (ANSI COMPILE-FILE.16). Otherwise the cheaper FromString.
                _il.Emit(OpCodes.Ldstr, pn.ToNamestring());
                if (pn.Version != null && pn.Version is not Nil)
                {
                    EmitLoadConstInline(pn.Version);
                    _il.Emit(OpCodes.Call, typeof(LispPathname).GetMethod(
                        "FromStringWithVersion", new[] { typeof(string), typeof(LispObject) })!);
                }
                else
                {
                    _il.Emit(OpCodes.Call, typeof(LispPathname).GetMethod("FromString", new[] { typeof(string) })!);
                }
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispStruct ls:
                _inlineVisited ??= new HashSet<LispStruct>(ReferenceEqualityComparer.Instance);
                if (!_inlineVisited.Add(ls))
                {
                    // Circular reference: fall back to constant pool
                    EmitConstantAs(AddConstant(val), typeof(LispObject));
                    break;
                }
                try
                {
                    if (_skipStructIntern && !(_faslMode && _faslStructMap?.UninternedTypeBuilder != null
                                               && TryEmitViaLoadForm(ls, "")))
                    {
                        // Inside hash table values: skip interning to save string space
                        EmitLoadConstInline(ls.TypeName);
                        var slotsArr2 = ls.SlotsSnapshot();
                        _il.Emit(OpCodes.Ldc_I4, slotsArr2.Length);
                        _il.Emit(OpCodes.Newarr, typeof(LispObject));
                        for (int si = 0; si < slotsArr2.Length; si++)
                        {
                            _il.Emit(OpCodes.Dup);
                            _il.Emit(OpCodes.Ldc_I4, si);
                            EmitLoadConstInline(slotsArr2[si]);
                            _il.Emit(OpCodes.Stelem_Ref);
                        }
                        _il.Emit(OpCodes.Newobj, typeof(LispStruct).GetConstructor(
                            new[] { typeof(Symbol), typeof(LispObject[]) })!);
                        _il.Emit(OpCodes.Castclass, typeof(LispObject));
                    }
                    else if (!_skipStructIntern)
                    {
                        // Top-level: use intern cache for EQ preservation
                        // Use short reference-identity-based key if available (avoids huge keys
                        // for deeply nested struct hierarchies that blow up the UserString heap)
                        string internKey = _faslStructMap != null
                            ? _faslStructMap.GetOrCreate(ls)
                            : ComputeStructInternKey(ls);
                        // CLHS 3.2.4.2: if make-load-form is defined for this type, use the
                        // returned creation form rather than directly serializing raw slots.
                        if (_faslMode && TryEmitViaLoadForm(ls, internKey))
                        {
                            // done: IL emitted by TryEmitViaLoadForm
                        }
                        else
                        {
                            if (_faslStructMap == null)
                                LispStruct.PreRegisterIntern(internKey, ls);
                            _il.Emit(OpCodes.Ldstr, Track(internKey));
                            EmitLoadConstInline(ls.TypeName);
                            var slotsArr = ls.SlotsSnapshot();
                            _il.Emit(OpCodes.Ldc_I4, slotsArr.Length);
                            _il.Emit(OpCodes.Newarr, typeof(LispObject));
                            for (int si = 0; si < slotsArr.Length; si++)
                            {
                                _il.Emit(OpCodes.Dup);
                                _il.Emit(OpCodes.Ldc_I4, si);
                                EmitLoadConstInline(slotsArr[si]);
                                _il.Emit(OpCodes.Stelem_Ref);
                            }
                            _il.Emit(OpCodes.Call, typeof(LispStruct).GetMethod("Intern",
                                new[] { typeof(string), typeof(LispObject), typeof(LispObject[]) })!);
                            _il.Emit(OpCodes.Castclass, typeof(LispObject));
                        }
                    }
                }
                finally { _inlineVisited.Remove(ls); }
                break;
            case Package pkg:
                _il.Emit(OpCodes.Ldstr, pkg.Name);
                _il.Emit(OpCodes.Call, typeof(Package).GetMethod("FindPackagePhysical", new[] { typeof(string) })!);
                if (_faslMode) _faslStructMap?.RecordPackageName(pkg.Name);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            case LispHashTable ht:
            {
                // Small hash tables: inline construction (<=20 entries)
                // Large ones: in FASL mode, emit as helper methods (self-contained, no _constants).
                // In non-FASL (AssembleAndRun), fall back to constant pool (same-process, ok).
                var htEntries = ht.Entries.ToList();
                if (htEntries.Count > MaxHtInline)
                {
                    if (_faslMode && _faslTypeBuilder != null)
                    {
                        EmitFaslLargeHashTable(ht, htEntries);
                    }
                    else
                    {
                        EmitConstantAs(AddConstant(val), typeof(LispObject));
                    }
                    break;
                }
                // Inline hash table: create empty, populate entries
                // Skip struct interning inside HT values to save UserString heap
                bool prevSkip = _skipStructIntern;
                _skipStructIntern = true;
                try
                {
                    _il.Emit(OpCodes.Ldstr, ht.TestName);
                    _il.Emit(OpCodes.Newobj, typeof(LispHashTable).GetConstructor(new[] { typeof(string) })!);
                    var setMethod = typeof(LispHashTable).GetMethod("Set",
                        new[] { typeof(LispObject), typeof(LispObject) })!;
                    foreach (var entry in htEntries)
                    {
                        _il.Emit(OpCodes.Dup);
                        EmitLoadConstInline(entry.Key);
                        EmitLoadConstInline(entry.Value);
                        _il.Emit(OpCodes.Callvirt, setMethod);
                    }
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                finally { _skipStructIntern = prevSkip; }
                break;
            }
            case LispFunction fn:
            {
                // Look up named function by symbol at load time
                if (fn.Name != null)
                {
                    _il.Emit(OpCodes.Ldstr, fn.Name);
                    _il.Emit(OpCodes.Call, typeof(CilAssembler).GetMethod("GetFunction", new[] { typeof(string) })!);
                    _il.Emit(OpCodes.Castclass, typeof(LispObject));
                }
                else
                {
                    // Anonymous function: fall back to constant pool
                    EmitConstantAs(AddConstant(val), typeof(LispObject));
                }
                break;
            }
            case LispClass lc when lc.Name != null && !lc.NameCleared:
            {
                // A class object is identity-recoverable from its proper name via
                // FIND-CLASS, so emit a load-form that re-resolves the class at load
                // time instead of a per-process constant-pool reference. This makes
                // a class literal (e.g. #.(find-class 'foo) / #.(class-of x)) survive
                // a cross-process fasl (dotcl.core, distributed contrib fasls). Built-in
                // and user classes both resolve by name; anonymous / name-cleared
                // classes have no such handle and fall through to the pool fallback.
                EmitLoadConstInline(lc.Name);   // pushes the class-name symbol
                _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("FindClass", new[] { typeof(LispObject) })!);
                _il.Emit(OpCodes.Castclass, typeof(LispObject));
                break;
            }
            case LispInstance li:
            {
                if (_faslMode && _faslTypeBuilder != null)
                    EmitFaslInstanceInline(li);
                else
                {
                    // Non-fasl mode: constant pool is fine (same process)
                    EmitConstantAs(AddConstant(val), typeof(LispObject));
                }
                break;
            }
            default:
                // Fallback: constant pool. This resolves correctly in-process
                // (AssembleAndRun, and compile-file+load within the same process,
                // e.g. make-load-form of a LispClass) but NOT across processes;
                // the pool is per-process and not serialized into the fasl. Warn so
                // the cross-process risk is visible. (Types that must round-trip
                // cross-process need a dedicated inline case above, as LispComplex
                // got.)
                if (_faslMode)
                {
                    try { Console.Error.WriteLine($"[FASL WARNING] Unsupported constant type in fasl (constant-pool fallback, not cross-process safe): {val.GetType().Name}"); }
                    catch { /* ignore ToString errors */ }
                }
                EmitConstantAs(AddConstant(val), typeof(LispObject));
                break;
        }
        }
        finally { _inlineDepth--; }
    }

    private enum ConstGraphKind { Simple, Shared, Cyclic }

    /// <summary>Classify the constant graph reachable from <paramref name="root"/>:
    /// Cyclic if a Cons/Vector reaches an ancestor on the DFS path (back-edge),
    /// Shared if some Cons/Vector is reachable by more than one path (a #N=-coalesced
    /// DAG), else Simple. Iterative so a long proper list does not overflow the C#
    /// stack. Only Cons/Vector cells carry identity here; atoms are ignored.</summary>
    private static ConstGraphKind AnalyzeConstantGraph(LispObject root)
    {
        if (root is not Cons && root is not LispVector) return ConstGraphKind.Simple;
        var onPath = new HashSet<object>(ReferenceEqualityComparer.Instance);
        var done = new HashSet<object>(ReferenceEqualityComparer.Instance);
        bool shared = false;
        // Stack of (node, phase): phase 0 = enter, phase 1 = exit (pop from path).
        var st = new Stack<(LispObject node, int phase)>();
        st.Push((root, 0));
        while (st.Count > 0)
        {
            var (node, phase) = st.Pop();
            if (node is not Cons && node is not LispVector) continue;
            if (phase == 1) { onPath.Remove(node); continue; }
            if (onPath.Contains(node)) return ConstGraphKind.Cyclic;  // back-edge -> cycle
            if (!done.Add(node)) { shared = true; continue; }         // reached again (acyclic) -> shared
            onPath.Add(node);
            st.Push((node, 1));                                       // exit marker
            if (node is Cons c) { st.Push((c.Cdr, 0)); st.Push((c.Car, 0)); }
            else if (node is LispVector v)
                for (int i = v.Length - 1; i >= 0; i--) st.Push((v.ElementAt(i), 0));
        }
        return shared ? ConstGraphKind.Shared : ConstGraphKind.Simple;
    }

    // Above this many graph nodes, an ordinary literal is emitted as text plus a
    // load-time read rather than as construction IL. Below it, inline IL is
    // cheaper than the fixed cost of setting up a read. The crossover was
    // measured, not guessed; DOTCL_LITERAL_READER_MIN_NODES re-derives it (set it
    // to a huge value to turn this route off entirely and compare).
    //
    // 64 was read off a synthetic file of uniform literals, where it is the point
    // at which the read pays for itself. On real code it was far too high: a
    // literal below the threshold keeps its structures on the make-load-form
    // route, which builds the creation form as IL and EVALUATES it at load, and
    // Coalton did that 70,019 times for 44 seconds. Those literals are small
    // individually and never reach 64 nodes. The synthetic table already showed
    // the read winning at 8 nodes (0.641 -> 0.455 s); it just could not show what
    // staying on the other route costs, because a synthetic literal has no
    // structures in it.
    private static readonly int ReaderPathMinNodes =
        int.TryParse(Environment.GetEnvironmentVariable("DOTCL_LITERAL_READER_MIN_NODES"),
                     out var v) && v > 0 ? v : 8;

    /// <summary>Whether every node of this constant graph comes back from
    /// PRINT then READ as the same thing, so the reader path is a pure
    /// size-for-time trade with no change in meaning. Also counts the nodes.</summary>
    /// <remarks>What is deliberately NOT on the list, and why:
    /// <list type="bullet">
    /// <item>structures and CLOS instances: they are emitted through the
    /// make-load-form protocol, with creation and initialization forms ordered
    /// per object. Printing bypasses that entirely.</item>
    /// <item>pathnames: a namestring drops the version component.</item>
    /// <item>vectors that are not simple general vectors (bit vectors,
    /// specialized element types, multi-dimensional arrays): the printed form
    /// does not carry the element type, so the array comes back a different
    /// type than it went in.</item>
    /// </list>
    /// Anything unlisted falls through to inline emission, which handles every
    /// case; this predicate only decides which of two correct routes is used.</remarks>
    /// <summary>Set DOTCL_LITERAL_CENSUS to make the reader-safety walk finish
    /// instead of stopping at the first node it cannot print, and report what it
    /// found: one line per literal on stderr, `#CENSUS nodes=N bad=K types=...`.
    /// The question it answers is whether a rejected literal is unprintable
    /// throughout or printable except for a handful of nodes -- i.e. whether
    /// splitting a literal into text plus a few holes would pay.</summary>
    private static readonly bool LiteralCensus =
        Environment.GetEnvironmentVariable("DOTCL_LITERAL_CENSUS") != null;

    private static bool ConstantIsReaderSafe(LispObject val, out int nodes)
    {
        nodes = 0;
        bool safe = true;
        Dictionary<string, int>? bad = LiteralCensus ? new() : null;
        var stack = new Stack<LispObject>();
        stack.Push(val);
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        while (stack.Count > 0)
        {
            var node = stack.Pop();
            nodes++;
            switch (node)
            {
                case Nil:
                case T:
                case Fixnum:
                case Bignum:
                case Ratio:
                case SingleFloat:
                case DoubleFloat:
                case LispChar:
                case LispString:
                    continue;
                case LispComplex cx:
                    stack.Push(cx.Real); stack.Push(cx.Imaginary);
                    continue;
                case Symbol:
                    // Uninterned symbols included: the printer writes them as an
                    // index into the fasl's table and the reader takes the very
                    // object the table holds, so EQ survives the round trip. See
                    // Runtime.UninternedSymbolIndexer.
                    continue;
                case Cons c:
                    if (!seen.Add(c)) continue;
                    stack.Push(c.Car); stack.Push(c.Cdr);
                    continue;
                case LispVector vec:
                    if (vec._dimensions != null || vec.ElementTypeName != "T")
                    {
                        if (bad == null) return false;
                        safe = false;
                        Bump(bad, vec._dimensions != null
                            ? "array:multidim" : "array:" + vec.ElementTypeName);
                        continue;
                    }
                    if (!seen.Add(vec)) continue;
                    for (int i = 0; i < vec.Length; i++) stack.Push(vec.ElementAt(i));
                    continue;
                case LispStruct ls when StructReadsBackAsPrinted(ls):
                    // A struct prints as #S(TYPE :SLOT value ...) and the reader
                    // rebuilds it by allocating and filling slots by name -- the
                    // same thing make-load-form-saving-slots asks for, minus the
                    // cost of carrying its creation form as code and evaluating
                    // it. Only when that IS what the object's make-load-form
                    // says: a custom method has to be honoured, and is (the
                    // check per instance, since a method may branch on the
                    // object). See StructReadsBackAsPrinted.
                    if (!seen.Add(ls)) continue;
                    foreach (var slot in ls.SlotsSnapshot())
                        if (slot is LispObject so) stack.Push(so);
                    continue;
                default:
                    if (bad == null) return false;
                    safe = false;
                    Bump(bad, node.GetType().Name);
                    // Keep walking INTO the node where we can, so the count is of
                    // nodes and not of subtrees: a struct near the root would
                    // otherwise hide everything under it and make the graph look
                    // small and hopeless.
                    foreach (var child in CensusChildren(node)) stack.Push(child);
                    continue;
            }
        }
        if (bad != null)
        {
            var types = string.Join(",", bad.OrderByDescending(kv => kv.Value)
                                            .Select(kv => $"{kv.Key}:{kv.Value}"));
            int badCount = bad.Values.Sum();
            Console.Error.WriteLine(
                $"#CENSUS nodes={nodes} bad={badCount} safe={(safe ? 1 : 0)} types={types}");
        }
        return safe;
    }

    /// <summary>Whether printing this struct and reading it back gives an object
    /// with the same class and the same slots -- i.e. whether the text route is
    /// allowed to replace its make-load-form creation form.
    ///
    /// Two things have to hold. The reader must be able to rebuild it: #S needs
    /// the structure class to be findable with its slot names when the literal
    /// is read, which is the same requirement the creation form's
    /// (find-class 'name) already carries. And make-load-form must be asking for
    /// exactly that and nothing else -- a custom method can do anything, and a
    /// method may branch on the object (Coalton has one that consults a cache by
    /// depth), so this asks per instance rather than per class.
    ///
    /// The shape it accepts is what make-load-form-saving-slots produces:
    ///   (let ((v (allocate-instance (find-class 'TYPE))))
    ///     (setf (slot-value v 'SLOT) 'VALUE) ...
    ///     v)
    /// with a setf for every slot of the object and each value the object's own.
    /// Anything else falls back to the creation form as before.</summary>
    /// <summary>Print under the same bindings the fasl route uses, for the
    /// round-trip self-check. Verification only.</summary>
    private string? PrintFaslRepr(LispObject val, bool namesTable)
    {
        var pcSym = Startup.Sym("*PRINT-CIRCLE*");
        var prSym = Startup.Sym("*PRINT-READABLY*");
        var plSym = Startup.Sym("*PRINT-LEVEL*");
        var pnSym = Startup.Sym("*PRINT-LENGTH*");
        var pkgSym = Startup.Sym("*PACKAGE*");
        var rdffSym = Startup.Sym("*READ-DEFAULT-FLOAT-FORMAT*");
        DynamicBindings.Push(pcSym, T.Instance);
        DynamicBindings.Push(prSym, T.Instance);
        DynamicBindings.Push(plSym, Nil.Instance);
        DynamicBindings.Push(pnSym, Nil.Instance);
        DynamicBindings.Push(pkgSym, Startup.KeywordPkg);
        DynamicBindings.Push(rdffSym, Startup.Sym("SINGLE-FLOAT"));
        Runtime.ForceInternalSymbolSyntax = true;
        Runtime.FaslStructSyntax = _faslMode;
        if (namesTable)
        {
            var map = _faslStructMap!;
            Runtime.UninternedSymbolIndexer = map.GetOrCreateUninternedSymbolIndex;
        }
        try { return ((LispString)Runtime.WriteToString(val)).Value; }
        catch { return null; }
        finally
        {
            Runtime.UninternedSymbolIndexer = null;
            Runtime.FaslStructSyntax = false;
            Runtime.ForceInternalSymbolSyntax = false;
            DynamicBindings.Pop(rdffSym);
            DynamicBindings.Pop(pkgSym);
            DynamicBindings.Pop(pnSym); DynamicBindings.Pop(plSym);
            DynamicBindings.Pop(prSym); DynamicBindings.Pop(pcSym);
        }
    }

    private static string Truncate(string s, int n) =>
        s.Length <= n ? s : s.Substring(0, n) + " ...";

    /// <summary>The uninterned symbols named so far, as the array the reader
    /// wants. Verification only -- the emitted code builds its own from the blob.
    /// </summary>
    private Symbol[] BuildVerifyTable()
    {
        var syms = _faslStructMap!.UninternedSymbolsForVerify;
        var a = new Symbol[syms.Count];
        for (int i = 0; i < syms.Count; i++) a[i] = syms[i];
        return a;
    }
}
