using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    /// <summary>
    /// Shared map from LispStruct reference identity to short intern keys.
    /// Avoids O(N^2) struct intern key blowup for deep inheritance hierarchies.
    /// </summary>
    internal class FaslStructInternMap
    {
        private readonly Dictionary<LispStruct, string> _map =
            new(ReferenceEqualityComparer.Instance);
        private int _counter;
        private readonly string _prefix;
        // Track unique strings emitted via Ldstr for this assembly
        private readonly HashSet<string> _uniqueStrings = new();
        public long UniqueStringBytes { get; private set; }

        // Debug info collection (opt-in, shared across every body assembler for
        // this fasl). When non-null, top-level function bodies and nested
        // closures/lambdas append their DebugMethodInfo here; FaslAssembler creates
        // it under DOTCL_EMIT_PDB and reads it at Save. Null -> no debug info.
        internal List<DebugMethodInfo>? DebugSink;

        // The self-framed DEFUN bodies of this fasl so far, for :CALL-SAME-FILE:
        // (package, name) -> the body, its arity, whether it takes a value mode,
        // and the static field holding the function the DEFUN made.
        internal readonly Dictionary<(string, string), (MethodInfo Body, int Arity, bool Mv, FieldInfo Fn, bool Leaf)> SameFileBodies = new();

        // :CALL-SAME-FILE to a name not DEFUNed yet in this fasl: a stub per
        // (package, name, arity, mode), static (fn, args.., [mode]) -> value. A
        // later DEFUN the compiler marks :FORWARD fills it like the inline direct
        // path; FinishSameFileForwards fills the rest with the ordinary call.
        private readonly Dictionary<(string, string), List<(MethodBuilder Stub, ILGenerator Il, int N, bool Mode)>> _sameFileForwards = new();
        private int _sameFileForwardCount;

        internal bool HasSameFileForwards(string pkg, string name, int arity)
            => _sameFileForwards.TryGetValue((pkg, name), out var stubs) && stubs.Exists(e => e.N == arity);

        internal MethodInfo? SameFileForwardStub(string pkg, string name, int n, bool mode)
        {
            if (UninternedTypeBuilder == null) return null;
            if (!_sameFileForwards.TryGetValue((pkg, name), out var stubs))
                _sameFileForwards[(pkg, name)] = stubs = new();
            foreach (var e in stubs)
                if (e.N == n && e.Mode == mode) return e.Stub;
            var ps = new Type[1 + n + (mode ? 1 : 0)];
            ps[0] = typeof(LispFunction);
            for (int k = 1; k <= n; k++) ps[k] = typeof(LispObject);
            if (mode) ps[n + 1] = typeof(int);
            var m = UninternedTypeBuilder.DefineMethod("sff" + _sameFileForwardCount++,
                MethodAttributes.Public | MethodAttributes.Static, typeof(LispObject), ps);
            m.SetImplementationFlags(MethodImplAttributes.AggressiveInlining);
            stubs.Add((m, m.GetILGenerator().ForLibrary(), n, mode));
            return m;
        }

        /// <summary>The DEFUN of NAME made the body BODY, whose function the
        /// registration stores in FN: fill the forward stubs of NAME.</summary>
        internal void FillSameFileForwards(string pkg, string name, MethodInfo body, int arity, bool mv, FieldInfo fn)
        {
            if (!_sameFileForwards.Remove((pkg, name), out var stubs)) return;
            foreach (var (_, il, n, mode) in stubs)
            {
                if (n != arity) { EmitForwardOrdinary(il, n, mode); continue; }
                var other = il.DefineLabel();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldsfld, fn);
                il.Emit(OpCodes.Bne_Un, other);
                for (int k = 0; k <= n; k++) il.Emit(OpCodes.Ldarg, (short)k);
                if (mode && mv)
                {
                    il.Emit(OpCodes.Ldarg, (short)(n + 1));
                    il.Emit(OpCodes.Call, typeof(MultipleValues).GetMethod("PassMode")!);
                }
                il.Emit(OpCodes.Call, body);
                il.Emit(OpCodes.Ret);
                il.MarkLabel(other);
                EmitForwardOrdinary(il, n, mode);
            }
        }

        internal void FinishSameFileForwards()
        {
            foreach (var stubs in _sameFileForwards.Values)
                foreach (var (_, il, n, mode) in stubs) EmitForwardOrdinary(il, n, mode);
            _sameFileForwards.Clear();
        }

        private static void EmitForwardOrdinary(ILGenerator il, int n, bool mode)
        {
            int count = 1 + n + (mode ? 1 : 0);
            for (int k = 0; k < count; k++) il.Emit(OpCodes.Ldarg, (short)k);
            il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod("Call" + n + (mode ? "M" : ""))!);
            il.Emit(OpCodes.Ret);
        }

        // These are written by FaslAssembler, whose implementation needs .NET 9+.
        // On an older target framework it compiles out, so the fields stay null and
        // the code that reads them checks for that; CS0649 is reporting the shape,
        // not a bug.
#pragma warning disable CS0649
        // Per-FASL uninterned symbol deduplication: same Symbol object -> same static field.
        // Set by FaslAssembler after construction.
        internal System.Reflection.Emit.TypeBuilder? UninternedTypeBuilder;
        internal System.Reflection.Emit.ILGenerator? UninternedInitIl;
#pragma warning restore CS0649

        // --- Literal-holding static fields ---------------------------------
        // Uninterned symbols, call-site caches and string literals all take one
        // static field each, and they all used to land on CompiledModule. .NET
        // caps a type at 64K fields: past that the type cannot be loaded at all,
        // and the CLR says only "Internal limitation: too many fields", which
        // names neither the file nor the cause. The type initializer that fills
        // them is one method too, so its IL grows with every field and the JIT
        // pays for all of it at load. coalton's library/format bakes 114,451
        // uninterned symbols into its literals: 1.8x the field cap.
        //
        // Roll the fields over onto fresh holder types before either limit
        // bites. Each holder fills its own fields from its own type
        // initializer, which the CLR runs before the first access to that
        // holder: the same guarantee the single .cctor gave, now per holder
        // and lazily, so a file only pays for the literals it reaches.
        private const int MaxFieldsPerHolder = 4096;
#pragma warning disable CS0649
        internal System.Reflection.Emit.ModuleBuilder? ModuleBuilder;
#pragma warning restore CS0649
        internal readonly List<(System.Reflection.Emit.TypeBuilder Type,
                               System.Reflection.Emit.ILGenerator Cctor)> OverflowHolders = new();
        private System.Reflection.Emit.TypeBuilder? _holderType;
        private System.Reflection.Emit.ILGenerator? _holderCctor;
        private int _holderFields;
        private int _holderCount;

        /// <summary>The type a new literal field goes on, with the initializer
        /// that fills it. Rolls over to a fresh holder type once the current one
        /// has taken MaxFieldsPerHolder fields.</summary>
        private (System.Reflection.Emit.TypeBuilder Type,
                 System.Reflection.Emit.ILGenerator Cctor) FieldHolder()
        {
            if (_holderType == null)
            {
                _holderType = UninternedTypeBuilder!;
                _holderCctor = SymFnSiteInitIl!;
            }
            else if (_holderFields >= MaxFieldsPerHolder && ModuleBuilder != null)
            {
                var tb = ModuleBuilder.DefineType($"CompiledModuleLiterals{_holderCount++}",
                    System.Reflection.TypeAttributes.Public | System.Reflection.TypeAttributes.Class);
                _holderType = tb;
                _holderCctor = tb.DefineTypeInitializer().GetILGenerator().ForLibrary();
                OverflowHolders.Add((tb, _holderCctor));
                _holderFields = 0;
            }
            _holderFields++;
            return (_holderType!, _holderCctor!);
        }
        private readonly Dictionary<Symbol, int> _uninternedIndex =
            new(ReferenceEqualityComparer.Instance);
        private readonly List<string> _uninternedNames = new();
        private readonly List<Symbol> _uninternedSymbols = new();

        /// <summary>Verification only (DOTCL_LITERAL_VERIFY): the very symbols the
        /// indices name, so a round trip can be compared with EQUAL. The emitted
        /// code rebuilds equivalent ones from the names blob.</summary>
        public IReadOnlyList<Symbol> UninternedSymbolsForVerify => _uninternedSymbols;
        private System.Reflection.Emit.FieldBuilder? _uninternedTableField;

        public FaslStructInternMap(string modulePrefix)
        {
            // A new file: MAKE-LOAD-FORM answers are remembered per file.
            s_mlfMemo = new Dictionary<LispObject, LispObject>(ReferenceEqualityComparer.Instance);
            _prefix = modulePrefix;
        }

        public string GetOrCreate(LispStruct ls)
        {
            if (!_map.TryGetValue(ls, out var key))
            {
                key = _prefix + "." + (_counter++);
                _map[ls] = key;
                LispStruct.PreRegisterIntern(key, ls);
            }
            return key;
        }

        // --- LispInstance make-load-form registry ---
        private record InstanceEntry(string Key, LispObject? InitForm, List<LispInstance> InitDeps);
        private readonly Dictionary<LispObject, InstanceEntry> _instanceMap =
            new(ReferenceEqualityComparer.Instance);
        private readonly List<LispObject> _instanceOrder = new();

        public bool TryGetInstanceKey(LispObject li, out string key)
        {
            if (_instanceMap.TryGetValue(li, out var entry)) { key = entry.Key; return true; }
            key = ""; return false;
        }

        public string RegisterInstance(LispObject li, LispObject? initForm)
        {
            if (_instanceMap.TryGetValue(li, out var existing))
                return existing.Key;
            var key = _prefix + ".inst." + _instanceOrder.Count;
            var deps = new List<LispInstance>();
            if (initForm != null && initForm is not Nil)
                CollectInstanceRefs(initForm, deps, new HashSet<LispInstance>(ReferenceEqualityComparer.Instance));
            _instanceMap[li] = new InstanceEntry(key, initForm, deps);
            _instanceOrder.Add(li);
            if (li is LispInstance inst) LispInstance.PreRegisterIntern(key, inst);
            return key;
        }

        internal static void CollectInstanceRefs(LispObject form, List<LispInstance> result,
            HashSet<LispInstance> seen)
        {
            if (form is LispInstance li) { if (seen.Add(li)) result.Add(li); return; }
            if (form is Cons c) { CollectInstanceRefs(c.Car, result, seen); CollectInstanceRefs(c.Cdr, result, seen); }
        }

        // Track which instances are "created" (creation form emitted) and "initialized" (init form emitted)
        private readonly HashSet<LispObject> _created = new(ReferenceEqualityComparer.Instance);
        private readonly HashSet<LispObject> _initialized = new(ReferenceEqualityComparer.Instance);

        public void MarkCreated(LispObject li) => _created.Add(li);

        public bool IsCreated(LispObject li) => _created.Contains(li);

        /// <summary>Counter for the per-instance creation/init top-level methods
        /// (_mlf_N) emitted so make-load-form creation and init forms interleave
        /// per object at load time (CLHS ordering, matching SBCL).</summary>
        internal int TopLevelMethodCount;

        /// <summary>The open batch of MAKE-LOAD-FORM-SAVING-SLOTS creations, if
        /// any. Consecutive such objects share one method instead of getting one
        /// each: generic-cl's fasls hold about 7,500 of them, and a method that
        /// runs once at load costs more to JIT-compile than its few instructions
        /// take to run. The batch's call goes into the init sequence when it is
        /// opened, and it takes further objects only while nothing else has been
        /// written there since, so the order of effects is the same as one method
        /// per object.</summary>
        internal MlfBatch? OpenMlfBatch;

        /// <summary>Pop all init forms whose external dependencies are now fully initialized.
        /// Repeats until no more unblocked forms. Call after each creation form is emitted.</summary>
        public List<(string key, LispObject initForm)> PopEagerInitForms()
        {
            var result = new List<(string, LispObject)>();
            bool found;
            do
            {
                found = false;
                foreach (var li in _instanceOrder)
                {
                    if (_initialized.Contains(li)) continue;
                    if (!_created.Contains(li)) continue;
                    if (!_instanceMap.TryGetValue(li, out var entry)) continue;
                    if (entry.InitForm == null || entry.InitForm is Nil)
                    {
                        _initialized.Add(li); // no init form needed
                        continue;
                    }
                    bool ready = true;
                    foreach (var dep in entry.InitDeps)
                    {
                        if (ReferenceEquals(dep, li)) continue; // self-ref OK
                        if (!_initialized.Contains(dep)) { ready = false; break; }
                    }
                    if (ready)
                    {
                        _initialized.Add(li);
                        result.Add((entry.Key, entry.InitForm));
                        found = true;
                    }
                }
            } while (found);
            return result;
        }

        /// <summary>Emit remaining init forms (those still pending, e.g. from cyclic deps).</summary>
        public List<(string key, LispObject initForm)> FlushRemainingInitForms()
        {
            var result = new List<(string, LispObject)>();
            foreach (var li in _instanceOrder)
            {
                if (_initialized.Contains(li)) continue;
                _initialized.Add(li);
                if (_instanceMap.TryGetValue(li, out var entry) && entry.InitForm != null && entry.InitForm is not Nil)
                    result.Add((entry.Key, entry.InitForm));
            }
            return result;
        }

        // Per-FASL SymFnSite deduplication: one static cell per (name, package)
        // named in a call position, shared by every call site that names it.
        // Initialized from the type initializer (below), not ModuleInit, so the
        // CLR guarantees the cells exist before any body can read one.
#pragma warning disable CS0649
        internal System.Reflection.Emit.ILGenerator? SymFnSiteInitIl;
#pragma warning restore CS0649
        private readonly Dictionary<string, System.Reflection.Emit.FieldBuilder> _symFnSiteFields = new();
        private static readonly System.Reflection.ConstructorInfo _symFnSiteCtor =
            typeof(Startup.SymFnSite).GetConstructor(new[] { typeof(string), typeof(string) })!;
        private int _symFnSiteCounter;

        /// <summary>
        /// Static field holding the call-site symbol cache for NAME (optionally
        /// qualified by its home PKG at compile time). Without it, FASL code
        /// resolves the callee through Startup.SymFn on every call: a string
        /// concatenation and a dictionary probe per call, which measured as
        /// ~57 ns more per call than the JIT path's constant-pool cell.
        /// </summary>
        public System.Reflection.Emit.FieldBuilder GetOrCreateSymFnSiteField(string name, string? pkg)
        {
            var key = pkg is null ? name : name + "\0" + pkg;
            if (_symFnSiteFields.TryGetValue(key, out var existing)) return existing;
            if (pkg != null) RecordPackageName(pkg);
            var (tb, il) = FieldHolder();
            var field = tb.DefineField($"_symfn_{_symFnSiteCounter++}",
                LibraryAbiILExtensions.FieldType(typeof(Startup.SymFnSite)),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            il.Emit(System.Reflection.Emit.OpCodes.Ldstr, TrackString(name));
            if (pkg is null) il.Emit(System.Reflection.Emit.OpCodes.Ldnull);
            else il.Emit(System.Reflection.Emit.OpCodes.Ldstr, TrackString(pkg));
            il.Emit(System.Reflection.Emit.OpCodes.Newobj, _symFnSiteCtor);
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, field);
            _symFnSiteFields[key] = field;
            return field;
        }

        // Per-FASL SymSite cells for :load-sym, one per name, created by the
        // type initializer like the SymFnSite cells above.
        private readonly Dictionary<string, System.Reflection.Emit.FieldBuilder> _symSiteFields =
            new(StringComparer.Ordinal);
        private static readonly System.Reflection.ConstructorInfo _symSiteCtor =
            typeof(Startup.SymSite).GetConstructor(new[] { typeof(string) })!;
        private int _symSiteCounter;

        public System.Reflection.Emit.FieldBuilder GetOrCreateSymSiteField(string name)
        {
            if (_symSiteFields.TryGetValue(name, out var existing)) return existing;
            var (tb, il) = FieldHolder();
            var field = tb.DefineField($"_sym_{_symSiteCounter++}",
                LibraryAbiILExtensions.FieldType(typeof(Startup.SymSite)),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            il.Emit(System.Reflection.Emit.OpCodes.Ldstr, TrackString(name));
            il.Emit(System.Reflection.Emit.OpCodes.Newobj, _symSiteCtor);
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, field);
            _symSiteFields[name] = field;
            return field;
        }

        private readonly Dictionary<string, System.Reflection.Emit.FieldBuilder> _keywordFields =
            new(StringComparer.Ordinal);
        private static readonly System.Reflection.MethodInfo _keywordMethod =
            typeof(Startup).GetMethod("Keyword")!;
        private int _keywordCounter;

        /// <summary>
        /// Static field holding the keyword NAME, filled by the type initializer
        /// (see LOAD-SYM-KEYWORD). Unlike a symbol of another package (see
        /// GetOrCreateSymPkgField) a keyword cannot be affected by the forms of
        /// the file: the KEYWORD package always exists and its symbols are never
        /// re-homed, so interning it before the top-level forms run is the same
        /// as interning it at the site, and the site is then a single load.
        /// </summary>
        public System.Reflection.Emit.FieldBuilder GetOrCreateKeywordField(string name)
        {
            if (_keywordFields.TryGetValue(name, out var existing)) return existing;
            var (tb, il) = FieldHolder();
            var field = tb.DefineField($"_kw_{_keywordCounter++}",
                typeof(Symbol),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            il.Emit(System.Reflection.Emit.OpCodes.Ldstr, TrackString(name));
            il.Emit(System.Reflection.Emit.OpCodes.Call, _keywordMethod);
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, field);
            _keywordFields[name] = field;
            return field;
        }

        private readonly Dictionary<string, System.Reflection.Emit.FieldBuilder> _symPkgFields =
            new(StringComparer.Ordinal);
        private int _symPkgCounter;
        /// <summary>
        /// Static field holding a package-qualified Symbol, filled by the first
        /// execution of the site that needs it (see LOAD-SYM-PKG). The
        /// instruction emitted `ldstr name; ldstr pkg; call SymInPkg` INLINE, so
        /// every reference to a special variable (or a quoted symbol) paid a
        /// string-keyed package lookup -- 10.6% of a call-heavy profile
        /// (richards, whose hot loop reads defvar globals). One lookup per site
        /// per assembly is all it can ever need.
        ///
        /// The field is NOT initialized by the type initializer. That runs
        /// before the assembly's top-level forms, and a package a form in this
        /// same file defines (or that uiop's DEFINE-PACKAGE recycles by
        /// uninterning and re-homing its symbols) does not hold the same symbol
        /// objects yet.
        /// </summary>
        public System.Reflection.Emit.FieldBuilder GetOrCreateSymPkgField(string name, string pkg)
        {
            var key = name + "\0" + pkg;
            if (_symPkgFields.TryGetValue(key, out var existing)) return existing;
            var (tb, _) = FieldHolder();
            var field = tb.DefineField($"_symp_{_symPkgCounter++}",
                typeof(Symbol),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            _symPkgFields[key] = field;
            return field;
        }

        private int _inlineCacheCounter;
        /// <summary>A static field for one READER-IC / WRITER-IC site's cache cell
        /// (see EmitFaslInlineCacheCell). Never shared between sites: each site
        /// caches the class it sees, as the in-memory constant-pool cell does.
        /// Left empty by the type initializer; the site fills it.</summary>
        public System.Reflection.Emit.FieldBuilder CreateInlineCacheField(Type cellType)
        {
            var (tb, _) = FieldHolder();
            return tb.DefineField($"_ic_{_inlineCacheCounter++}",
                cellType,
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
        }

        private readonly Dictionary<string, System.Reflection.Emit.FieldBuilder> _stringFields =
            new(StringComparer.Ordinal);
        private static readonly System.Reflection.ConstructorInfo _lispStringCtor =
            typeof(LispString).GetConstructor(new[] { typeof(string) })!;
        private int _stringCounter;

        private readonly Dictionary<string, System.Reflection.Emit.FieldBuilder> _strArrayFields =
            new(StringComparer.Ordinal);
        private int _strArrayCounter;

        /// <summary>
        /// Static field holding the string[] a &key prologue passes to
        /// CheckNoUnknownKeys -- the keyword names, or the packages they were written
        /// in. The vector is a constant of the site and the runtime only reads it, so
        /// it is built once by the type initializer instead of once per CALL, which is
        /// what emitting it in the prologue did (every &key call paid an array, the
        /// calls passing no keyword at all included).
        /// </summary>
        public System.Reflection.Emit.FieldBuilder GetOrCreateStringArrayField(string?[] value)
        {
            // NUL separates the entries and an empty entry stands for null, so that
            // {"A", null} and {"A B"} do not share a field.
            var key = string.Join("\0", value.Select(s => s ?? ""));
            if (_strArrayFields.TryGetValue(key, out var existing)) return existing;
            var (tb, il) = FieldHolder();
            var field = tb.DefineField($"_strarr_{_strArrayCounter++}",
                typeof(string[]),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            EmitStringArrayInline(il, value);
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, field);
            _strArrayFields[key] = field;
            return field;
        }

        /// <summary>
        /// Static field holding the LispString for a string literal, built once by
        /// the type initializer. Emitting the literal inline instead constructs a
        /// fresh LispString every time the expression is EVALUATED, not once per
        /// site: an interpreted call through the core paid four of them, and any
        /// string literal in a loop pays one per iteration. Literal objects are
        /// constants (CLHS 3.7.1), so one object per distinct value per assembly
        /// is the same coalescing a file compiler is allowed to do.
        /// </summary>
        public System.Reflection.Emit.FieldBuilder GetOrCreateStringField(string value)
        {
            if (_stringFields.TryGetValue(value, out var existing)) return existing;
            var (tb, il) = FieldHolder();
            var field = tb.DefineField($"_str_{_stringCounter++}",
                typeof(LispString),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
            il.Emit(System.Reflection.Emit.OpCodes.Ldstr, TrackString(value));
            il.Emit(System.Reflection.Emit.OpCodes.Newobj, _lispStringCtor);
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, field);
            _stringFields[value] = field;
            return field;
        }

        /// <summary>
        /// Get or create a static field for an uninterned symbol so that all uses within
        /// this FASL resolve to the SAME Symbol object (preserves EQ-ness across make-load-form).
        /// </summary>
        private int _litCacheCounter;

        /// <summary>A static field holding one top level literal, filled the first
        /// time the code that names it runs. CLHS 3.2.4.4: the literal in compiled
        /// code is ONE object -- calling the function twice has to give the same
        /// one, and a destructive change to it has to stick. Building it at each
        /// use gave a fresh copy every call, which also made every call allocate
        /// the whole graph.</summary>
        /// <summary>Identity-carrying objects that occur in more than one
        /// literal of the top level form being emitted (see
        /// CollectCrossLiteralNodes), each with the static field that holds it
        /// once built; null outside a form or when nothing is shared.</summary>
        internal Dictionary<LispObject, System.Reflection.Emit.FieldBuilder?>? CrossLiteralShared;
        /// <summary>The literals of the current form that contain one of them.</summary>
        internal HashSet<LispObject>? CrossLiteralRoots;
        internal readonly HashSet<LispObject> CrossLiteralBuilding =
            new(ReferenceEqualityComparer.Instance);

        /// <summary>A large hash table literal emitted as a prototype: its test,
        /// the entries it was emitted with, and the method that returns it.</summary>
        internal sealed class HtPrototype
        {
            public readonly string Test;
            public readonly Dictionary<LispObject, LispObject> Entries;
            /// <summary>The method that builds it, or null when it is built from
            /// data part <see cref="Part"/> (FaslData.HtPrototype).</summary>
            public readonly System.Reflection.Emit.MethodBuilder? Method;
            public readonly int Part;
            public HtPrototype(string test, Dictionary<LispObject, LispObject> entries,
                System.Reflection.Emit.MethodBuilder? method, int part = -1)
            { Test = test; Entries = entries; Method = method; Part = part; }
        }
        /// <summary>The hash table prototypes of this file, in emission order.</summary>
        internal readonly List<HtPrototype> HtPrototypes = new();
        /// <summary>The data part of each table that has a data prototype.</summary>
        internal readonly Dictionary<LispHashTable, int> HtDataParts =
            new(ReferenceEqualityComparer.Instance);
        internal readonly HashSet<LispHashTable> HtDataBuilding =
            new(ReferenceEqualityComparer.Instance);

        /// <summary>Set up CrossLiteralShared for one top level form's
        /// instructions. Paired with EndFormLiterals.</summary>
        internal void BeginFormLiterals(LispObject instrList)
        {
            var shared = CollectCrossLiteralNodes(instrList, out var roots);
            CrossLiteralShared = shared.Count > 0 ? shared : null;
            CrossLiteralRoots = shared.Count > 0 ? roots : null;
        }

        internal void EndFormLiterals()
        {
            CrossLiteralShared = null;
            CrossLiteralRoots = null;
            CrossLiteralBuilding.Clear();
            _crossLiteralSlots.Clear();
        }

        // Slots of the shared array (_fshared) for the objects of the current
        // form that more than one of its literals contains. Slots are not
        // reused: each form's objects get their own.
        private readonly Dictionary<LispObject, int> _crossLiteralSlots =
            new(ReferenceEqualityComparer.Instance);
        private int _sharedSlotCount;
        private System.Reflection.Emit.FieldBuilder? _sharedSlotsField;

        public int CrossLiteralSlot(LispObject node)
        {
            if (!_crossLiteralSlots.TryGetValue(node, out int slot))
            {
                slot = _sharedSlotCount++;
                _crossLiteralSlots[node] = slot;
            }
            return slot;
        }

        /// <summary>The slot of NODE when it is an object several literals of
        /// the current form contain and no enclosing code is building it; else -1.</summary>
        public int CrossLiteralSlotFor(LispObject node) =>
            CrossLiteralShared != null && CrossLiteralShared.ContainsKey(node)
                && !CrossLiteralBuilding.Contains(node)
                ? CrossLiteralSlot(node) : -1;

        public System.Reflection.Emit.FieldBuilder SharedSlotsField() =>
            _sharedSlotsField ??= UninternedTypeBuilder!.DefineField(
                "_fshared", typeof(LispObject[]),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);

        public System.Reflection.Emit.FieldBuilder DefineLiteralCacheField()
        {
            var (tb, _) = FieldHolder();
            return tb.DefineField($"_lit_{_litCacheCounter++}", typeof(LispObject),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
        }

        public int GetOrCreateUninternedSymbolIndex(Symbol sym)
        {
            if (_uninternedIndex.TryGetValue(sym, out var i)) return i;
            i = _uninternedNames.Count;
            _uninternedNames.Add(sym.Name);
            _uninternedSymbols.Add(sym);
            _uninternedIndex[sym] = i;
            return i;
        }

        /// <summary>The one Symbol[] every use of an uninterned symbol indexes.
        /// Defined on the primary holder so that reading it runs that type's
        /// initializer, which is where the array is built (see
        /// EmitUninternedTableInit).</summary>
        public System.Reflection.Emit.FieldBuilder UninternedTableField()
        {
            return _uninternedTableField ??= UninternedTypeBuilder!.DefineField(
                "_gsyms", typeof(Symbol[]),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
        }

        // --- Payloads rebuilt at load by FaslData -------------------------------
        private readonly List<byte[]?> _dataParts = new();
        private System.Reflection.Emit.FieldBuilder? _dataPartsField;
        private System.Reflection.Emit.FieldBuilder? _htCacheField;

        /// <summary>The index of a new data part, whose bytes are given later
        /// (<see cref="SetDataPart"/>): the call that reads it is emitted before
        /// its content is complete.</summary>
        public int ReserveDataPart() { _dataParts.Add(null); return _dataParts.Count - 1; }
        public void SetDataPart(int index, byte[] data) => _dataParts[index] = data;
        public int AddDataPart(byte[] data) { _dataParts.Add(data); return _dataParts.Count - 1; }

        /// <summary>The byte[][] of this fasl's data parts, filled by the
        /// primary holder's initializer (<see cref="EmitDataPartsInit"/>).</summary>
        public System.Reflection.Emit.FieldBuilder DataPartsField() =>
            _dataPartsField ??= UninternedTypeBuilder!.DefineField(
                "_fdata", typeof(byte[][]),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);

        /// <summary>The cache of hash table prototypes built from data parts,
        /// indexed like the parts.</summary>
        public System.Reflection.Emit.FieldBuilder HtCacheField() =>
            _htCacheField ??= UninternedTypeBuilder!.DefineField(
                "_fhtp", typeof(LispObject[]),
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);

        /// <summary>Fill the data parts field from one blob (in pieces when it is
        /// larger than one data field may hold), and allocate the prototype
        /// cache.</summary>
        public void EmitDataPartsInit(System.Reflection.Emit.ILGenerator il)
        {
            if (_sharedSlotsField != null)
            {
                il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, _sharedSlotCount);
                il.Emit(System.Reflection.Emit.OpCodes.Newarr, typeof(LispObject));
                il.Emit(System.Reflection.Emit.OpCodes.Stsfld, _sharedSlotsField);
            }
            if (_dataPartsField == null) return;
            var parts = new List<byte[]>(_dataParts.Count);
            foreach (var p in _dataParts) parts.Add(p ?? FaslDataEmptyPart);
            var blob = FaslDataWriter.JoinParts(parts);
            var initArray = typeof(System.Runtime.CompilerServices.RuntimeHelpers)
                .GetMethod("InitializeArray", new[] { typeof(System.Array), typeof(RuntimeFieldHandle) })!;
            void EmitBlob(byte[] b)
            {
                var f = DefineLiteralData(b);
                il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, b.Length);
                il.Emit(System.Reflection.Emit.OpCodes.Newarr, typeof(byte));
                il.Emit(System.Reflection.Emit.OpCodes.Dup);
                il.Emit(System.Reflection.Emit.OpCodes.Ldtoken, f);
                il.Emit(System.Reflection.Emit.OpCodes.Call, initArray);
            }
            if (blob.Length <= MaxLiteralDataChunk)
            {
                EmitBlob(blob);
                il.Emit(System.Reflection.Emit.OpCodes.Call,
                    typeof(FaslData).GetMethod("SplitParts", new[] { typeof(byte[]) })!);
            }
            else
            {
                int pieces = (blob.Length + MaxLiteralDataChunk - 1) / MaxLiteralDataChunk;
                il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, pieces);
                il.Emit(System.Reflection.Emit.OpCodes.Newarr, typeof(byte[]));
                for (int i = 0; i < pieces; i++)
                {
                    int start = i * MaxLiteralDataChunk;
                    int len = System.Math.Min(MaxLiteralDataChunk, blob.Length - start);
                    var chunk = new byte[len];
                    System.Array.Copy(blob, start, chunk, 0, len);
                    il.Emit(System.Reflection.Emit.OpCodes.Dup);
                    il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, i);
                    EmitBlob(chunk);
                    il.Emit(System.Reflection.Emit.OpCodes.Stelem_Ref);
                }
                il.Emit(System.Reflection.Emit.OpCodes.Call,
                    typeof(FaslData).GetMethod("SplitParts", new[] { typeof(byte[][]) })!);
            }
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, _dataPartsField);
            if (_htCacheField != null)
            {
                il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, _dataParts.Count);
                il.Emit(System.Reflection.Emit.OpCodes.Newarr, typeof(LispObject));
                il.Emit(System.Reflection.Emit.OpCodes.Stsfld, _htCacheField);
            }
        }

        private static readonly byte[] FaslDataEmptyPart = { 0 };

        /// <summary>Fill the Symbol[] from a names blob, at the end of the primary
        /// holder's initializer. Nothing earlier in that initializer reads the
        /// array, and everything that does read it (a _const helper, a top-level
        /// body) runs later, when the fasl is loaded.
        ///
        /// The names travel as data, not as construction IL: this used to be one
        /// static field and four instructions per symbol, which for a Coalton fasl
        /// meant 114,451 fields and the holder-splitting that exists to keep them
        /// under the per-type limit.</summary>
        public void EmitUninternedTableInit(System.Reflection.Emit.ILGenerator il)
        {
            if (_uninternedNames.Count == 0 || _uninternedTableField == null) return;
            var sb = new System.Text.StringBuilder();
            foreach (var n in _uninternedNames)
            {
                var bytes = System.Text.Encoding.UTF8.GetByteCount(n);
                sb.Append(bytes).Append(':').Append(n);
            }
            var blob = System.Text.Encoding.UTF8.GetBytes(sb.ToString());
            var initArray = typeof(System.Runtime.CompilerServices.RuntimeHelpers)
                .GetMethod("InitializeArray", new[] { typeof(System.Array), typeof(RuntimeFieldHandle) })!;
            void EmitBlob(byte[] b)
            {
                var f = DefineLiteralData(b);
                il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, b.Length);
                il.Emit(System.Reflection.Emit.OpCodes.Newarr, typeof(byte));
                il.Emit(System.Reflection.Emit.OpCodes.Dup);
                il.Emit(System.Reflection.Emit.OpCodes.Ldtoken, f);
                il.Emit(System.Reflection.Emit.OpCodes.Call, initArray);
            }
            if (blob.Length <= MaxLiteralDataChunk)
            {
                EmitBlob(blob);
                il.Emit(System.Reflection.Emit.OpCodes.Call,
                    typeof(Runtime).GetMethod("BuildUninternedTable", new[] { typeof(byte[]) })!);
            }
            else
            {
                int parts = (blob.Length + MaxLiteralDataChunk - 1) / MaxLiteralDataChunk;
                il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, parts);
                il.Emit(System.Reflection.Emit.OpCodes.Newarr, typeof(byte[]));
                for (int i = 0; i < parts; i++)
                {
                    int start = i * MaxLiteralDataChunk;
                    int len = System.Math.Min(MaxLiteralDataChunk, blob.Length - start);
                    var chunk = new byte[len];
                    System.Array.Copy(blob, start, chunk, 0, len);
                    il.Emit(System.Reflection.Emit.OpCodes.Dup);
                    il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, i);
                    EmitBlob(chunk);
                    il.Emit(System.Reflection.Emit.OpCodes.Stelem_Ref);
                }
                il.Emit(System.Reflection.Emit.OpCodes.Call,
                    typeof(Runtime).GetMethod("BuildUninternedTable", new[] { typeof(byte[][]) })!);
            }
            il.Emit(System.Reflection.Emit.OpCodes.Stsfld, _uninternedTableField);
        }

        // Symbols this compilation unit names in a package other than CL/KEYWORD.
        // FaslAssembler emits pre-interning calls for them at the end of ModuleInit
        // so that a symbol reachable only from a constant that has not run yet
        // (e.g. one appearing solely inside a macro's backquote template) still
        // exists in its package right after the fasl is loaded: SBCL interns the
        // whole constant pool at load time, and defpackage :import-from in a later
        // file relies on that.
        private readonly HashSet<(string Name, string Package)> _preinternSymbols = new();
        public IReadOnlyCollection<(string Name, string Package)> PreinternSymbols => _preinternSymbols;

        /// <summary>Record a symbol reference for load-time pre-interning (FASL mode only).</summary>
        public void RecordSymbolReference(string name, string pkgName)
        {
            if (pkgName == "COMMON-LISP" || pkgName == "KEYWORD") return;
            _preinternSymbols.Add((name, pkgName));
            RecordPackageName(pkgName);
        }

        // The packages this fasl names (Package.FaslPackageEntry), for Runtime.CheckFaslUnit.
        private readonly Dictionary<string, string> _packageEntries = new(StringComparer.Ordinal);
        public IEnumerable<string> PackageEntries => _packageEntries.Values;

        public void RecordPackageName(string pkgName)
        {
            if (pkgName.Length == 0 || _packageEntries.ContainsKey(pkgName)) return;
            _packageEntries[pkgName] = Package.FaslPackageEntry(pkgName);
        }

        /// <summary>Track a string being emitted via Ldstr. Returns the string.</summary>
        public string TrackString(string s)
        {
            if (_uniqueStrings.Add(s))
                UniqueStringBytes += (long)(s.Length * 2 + 4);
            return s;
        }

        // --- Literal payloads held as data rather than as code ---------------
        // A string emitted with Ldstr lives in the module's #US heap, which is
        // capped at 16 MB by the token encoding (the low three bytes of the token
        // are a byte offset into the heap) and is shared by every string literal,
        // symbol name and printed representation in the module. Data written with
        // DefineInitializedData lands in the PE's data section instead, which that
        // cap does not reach, and is copied out with a single memcpy at load
        // instead of being decoded per token.
        //
        // One field per payload, taken from the same holder rollover as the other
        // literal fields so the per-type field cap keeps applying.
        private int _litDataCounter;

        /// <summary>Define a static field holding DATA (a PE data-section blob),
        /// not code that reconstructs it. Its size must stay under
        /// <see cref="MaxLiteralDataChunk"/>; the caller splits.</summary>
        internal System.Reflection.Emit.FieldBuilder DefineLiteralData(byte[] data)
        {
            var (tb, _) = FieldHolder();
            return tb.DefineInitializedData($"_litdata_{_litDataCounter++}", data,
                System.Reflection.FieldAttributes.Public | System.Reflection.FieldAttributes.Static);
        }
    }
}
