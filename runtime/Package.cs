using System;
using System.Collections.Concurrent;
using System.Linq;

namespace DotCL;

public enum SymbolStatus
{
    None,
    Internal,
    External,
    Inherited
}

public class Package : LispObject
{
    public string Name { get; private set; }

    /// <summary>
    /// The LISP string PACKAGE-NAME answers with. Shared per package for the
    /// same reason as Symbol.NameString: the compiler asks on every local
    /// lookup (SAME-VAR-PACKAGE-P compares package names), so a fresh string
    /// per call is millions of allocations in one COMPILE-FILE. Cleared by
    /// Rename, the only thing that changes a package name.
    /// </summary>
    private LispString? _nameString;
    public LispString NameString => _nameString ??= new LispString(Name);
    // ConcurrentDictionary prevents enumeration-during-mutation crashes when
    // multiple threads intern/export/unintern concurrently. Multi-step
    // operations (e.g. Intern, Export) wrap with `lock (this)` for atomicity.
    private readonly ConcurrentDictionary<string, Symbol> _internalSymbols = new();
    private readonly ConcurrentDictionary<string, Symbol> _externalSymbols = new();
    private readonly List<Package> _useList = new();

    /// <summary>Immutable copy of <see cref="_useList"/>, rebuilt inside the lock
    /// whenever the list changes. FIND-SYMBOL walks it to resolve inherited
    /// symbols and is on every symbol reference in compiled code, so it must not
    /// take a lock to do so: that walk was measurable as Monitor.Enter_Slowpath.
    /// A reader may see a use/unuse that is one step stale, which is no weaker
    /// than before: the lock was released before the caller acted on the result
    /// either way.</summary>
    private volatile Package[] _useSnapshot = Array.Empty<Package>();
    private readonly object _pkgLock = new();
    // Local nicknames: maps nickname string -> package (per CDR 5 / SBCL package-local-nicknames)
    private readonly ConcurrentDictionary<string, Package> _localNicknames = new();

    private static readonly ConcurrentDictionary<string, Package> _allPackages = new();

    public Package(string name, params string[] nicknames)
    {
        if (_unitsInUse)
        {
            var u = _currentUnit.Value;
            if (u != null && u.Owns(name)) HomeUnit = u;
        }
        Name = Register(name);
        foreach (var n in nicknames)
            _nicknames.Add(Register(n));
    }

    /// <summary>The unit this package belongs to, or null for the global namespace.</summary>
    public Unit? HomeUnit { get; private set; }

    /// <summary>
    /// A package unit owns a set of package names (exact, or a prefix ending in
    /// <c>*</c>). While it is current, those names mean its own packages. A unit
    /// package is <c>UNIT!NAME</c> globally (that is <see cref="Name"/>) and
    /// <c>NAME</c> in the unit; a global package the unit hides is <c>!NAME</c>.
    /// </summary>
    public sealed class Unit
    {
        public readonly string UnitName;
        public readonly string Prefix;
        private readonly string[] _owned;
        internal readonly ConcurrentDictionary<string, Package> Table = new();
        private readonly List<Symbol> _syms = new();
        private readonly List<LispObject?> _vals = new();

        public Unit(string name, string ownedCsv)
        {
            UnitName = name;
            Prefix = name + UnitSeparator;
            _owned = ownedCsv.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries)
                .Select(s => s.Trim()).Where(s => s.Length > 0).ToArray();
        }

        public bool Owns(string name)
        {
            foreach (var o in _owned)
                if (o.EndsWith('*') ? name.StartsWith(o.AsSpan(0, o.Length - 1), StringComparison.Ordinal)
                                    : name == o)
                    return true;
            return false;
        }

        /// <summary>A module or system name: owned in any case.</summary>
        public bool OwnsName(string name) => Owns(name) || Owns(name.ToUpperInvariant());

        internal bool Binds(Symbol sym)
        {
            lock (_syms) return _syms.Contains(sym);
        }

        /// <summary>Owned names, and any other name a unit package was given.</summary>
        public bool Hides(string name) => Table.ContainsKey(name) || Owns(name);

        public override string ToString() => $"#<PACKAGE-UNIT {UnitName}>";

        public void AddSpecial(Symbol sym, LispObject? value)
        {
            lock (_syms) { _syms.Add(sym); _vals.Add(value); }
        }

        internal void Enter()
        {
            lock (_syms)
                for (int i = 0; i < _syms.Count; i++)
                    DynamicBindings.Push(_syms[i], _vals[i] ?? DynamicBindings.Unbound);
        }

        internal void Leave()
        {
            lock (_syms)
                for (int i = _syms.Count - 1; i >= 0; i--)
                {
                    _vals[i] = DynamicBindings.TryGet(_syms[i], out var v) ? v : null;
                    DynamicBindings.Pop(_syms[i]);
                }
        }
    }

    public const char UnitSeparator = '!';

    private static readonly System.Threading.AsyncLocal<Unit?> _currentUnit = new();
    internal static bool _unitsInUse;

    /// <summary>Set once, when a unit is first made current.</summary>
    public static bool UnitsInUse => _unitsInUse;

    public static Unit? CurrentUnit => _unitsInUse ? _currentUnit.Value : null;

    public static Unit MakeUnit(string name, string ownedCsv)
    {
        if (name.Length == 0 || name.Contains(UnitSeparator))
            throw new LispErrorException(new LispError($"MAKE-UNIT: bad unit name \"{name}\""));
        return new Unit(name, ownedCsv);
    }

    /// <summary>Make UNIT (or null for none) current in this execution context
    /// and return the previous one. Threads started afterwards inherit it.</summary>
    public static object? SwapUnit(object? unit)
    {
        var u = unit as Unit;
        if (u != null && !_unitsInUse) System.Threading.Volatile.Write(ref _unitsInUse, true);
        var old = _currentUnit.Value;
        if (ReferenceEquals(old, u) && ReferenceEquals(t_entered, u)) return old;
        t_entered?.Leave();
        _currentUnit.Value = u;
        t_entered = u;
        u?.Enter();
        return old;
    }

    /// <summary>The unit whose specials this thread has bound. The current unit
    /// flows into threads and thread-pool work with the execution context; the
    /// bindings do not.</summary>
    [ThreadStatic] private static Unit? t_entered;

    /// <summary>Bind the specials of the unit this new thread inherited.</summary>
    public static void EnterInheritedUnit()
    {
        var u = CurrentUnit;
        if (u == null) return;
        t_entered = u;
        u.Enter();
    }

    public static void LeaveThreadUnit()
    {
        t_entered?.Leave();
        t_entered = null;
    }

    /// <summary>True when the current unit keeps its own value of REGISTRY (a
    /// special such as *MODULES*) but does not own NAME: that entry belongs to the
    /// global value, shared by every unit.</summary>
    public static bool SharesRegistryEntry(Symbol registry, string name)
    {
        var u = CurrentUnit;
        return u != null && u.Binds(registry) && !u.OwnsName(name);
    }

    /// <summary>Run BODY in unit U, however the calling thread came to run it.</summary>
    public static T CallInUnit<T>(Unit u, Func<T> body)
    {
        if (ReferenceEquals(t_entered, u) && ReferenceEquals(_currentUnit.Value, u)) return body();
        if (!_unitsInUse) System.Threading.Volatile.Write(ref _unitsInUse, true);
        var oldCurrent = _currentUnit.Value;
        var oldEntered = t_entered;
        _currentUnit.Value = u;
        t_entered = u;
        u.Enter();
        try { return body(); }
        finally
        {
            u.Leave();
            t_entered = oldEntered;
            _currentUnit.Value = oldCurrent;
        }
    }

    /// <summary>FN, or, made inside a unit, a function that runs FN in that unit
    /// wherever it is called from: for Lisp code .NET calls back into.</summary>
    public static LispObject BindToCurrentUnit(LispObject fn)
    {
        var u = CurrentUnit;
        if (u == null) return fn;
        var lf = fn as LispFunction;
        return new LispFunction(args => CallInUnit(u, () => Runtime.Funcall(fn, args)),
            lf?.Name ?? "PACKAGE-UNIT-CALLBACK", lf?.Arity ?? -1);
    }


    private string Register(string name)
    {
        var u = HomeUnit;
        if (u == null)
        {
            _allPackages[name] = this;
            return name;
        }
        u.Table[name] = this;
        var physical = u.Prefix + name;
        _allPackages[physical] = this;
        return physical;
    }

    private void Unregister(string physical)
    {
        _allPackages.TryRemove(physical, out _);
        var u = HomeUnit;
        if (u != null && physical.StartsWith(u.Prefix, StringComparison.Ordinal))
        {
            var bare = physical.Substring(u.Prefix.Length);
            if (u.Table.TryGetValue(bare, out var p) && ReferenceEquals(p, this))
                u.Table.TryRemove(bare, out _);
        }
    }

    public static Package? FindPackage(string name)
    {
        if (_unitsInUse) return FindPackageInUnits(name);
        _allPackages.TryGetValue(name, out var pkg);
        return pkg;
    }

    private static Package? FindPackageInUnits(string name)
    {
        var cur = _currentUnit.Value;
        if (cur != null)
        {
            if (cur.Table.TryGetValue(name, out var own)) return own;
            if (cur.Owns(name)) return null;
        }
        if (_allPackages.TryGetValue(name, out var pkg)) return pkg;
        if (name.Length > 1 && name[0] == UnitSeparator
            && _allPackages.TryGetValue(name.Substring(1), out pkg) && pkg.HomeUnit == null)
            return pkg;
        return null;
    }

    /// <summary>The package whose name or nickname, as <see cref="Name"/> spells it,
    /// is NAME. Compiled code names packages this way, so what it finds does not
    /// depend on the unit current where it runs.</summary>
    public static Package? FindPackagePhysical(string name)
    {
        _allPackages.TryGetValue(name, out var pkg);
        return pkg;
    }

    /// <summary>How a fasl records a package it names: the physical name, and,
    /// when it differs, the name the source used for it where it was compiled.</summary>
    public static string FaslPackageEntry(string physical)
    {
        if (!_unitsInUse) return physical;
        var p = FindPackagePhysical(physical);
        if (p == null) return physical;
        var text = p.Visible(physical, _currentUnit.Value);
        return text == physical ? physical : physical + "\t" + text;
    }

    /// <summary>The first package of a fasl's recorded ENTRIES (see
    /// <see cref="FaslPackageEntry"/>) that its source would name differently from
    /// here, as (source name, physical name in the fasl, physical name here); or
    /// null when every package means the same here as where it was compiled.</summary>
    public static (string Text, string Compiled, string Here)? FaslUnitMismatch(string entries)
    {
        var cur = _unitsInUse ? _currentUnit.Value : null;
        if (cur == null && entries.IndexOf('\t') < 0) return null;
        foreach (var entry in entries.Split('\n'))
        {
            if (entry.Length == 0) continue;
            int tab = entry.IndexOf('\t');
            var physical = tab < 0 ? entry : entry.Substring(0, tab);
            var text = tab < 0 ? entry : entry.Substring(tab + 1);
            var here = PhysicalNameFor(text, cur);
            if (here != physical) return (text, physical, here);
        }
        return null;
    }

    /// <summary>The physical name the current unit's source spelling NAME stands for.</summary>
    public static string PhysicalNameOfSource(string name)
        => _unitsInUse ? PhysicalNameFor(name, _currentUnit.Value) : name;

    private static string PhysicalNameFor(string text, Unit? cur)
    {
        int sep = text.IndexOf(UnitSeparator);
        if (sep == 0) return text.Substring(1);
        if (sep > 0 || cur == null || !cur.Hides(text)) return text;
        return cur.Table.TryGetValue(text, out var own) ? own.Name : cur.Prefix + text;
    }

    /// <summary>The name, from unit CUR, that FIND-PACKAGE maps to this package.</summary>
    private string Visible(string physical, Unit? cur)
    {
        var u = HomeUnit;
        if (u != null)
            return ReferenceEquals(u, cur) ? physical.Substring(u.Prefix.Length) : physical;
        return cur != null && cur.Hides(physical) ? UnitSeparator + physical : physical;
    }

    public string VisibleName => _unitsInUse ? Visible(Name, _currentUnit.Value) : Name;

    public LispString VisibleNameString => _unitsInUse ? VisibleNameStringInUnits() : NameString;

    private LispString VisibleNameStringInUnits()
    {
        var v = Visible(Name, _currentUnit.Value);
        return ReferenceEquals(v, Name) ? NameString : new LispString(v);
    }

    public IReadOnlyList<string> VisibleNicknames
    {
        get
        {
            var nicks = Nicknames;
            if (!_unitsInUse) return nicks;
            var cur = _currentUnit.Value;
            return nicks.Select(n => Visible(n, cur)).ToList();
        }
    }

    /// <summary>Found by its bare name from the current unit.</summary>
    public bool IsVisible
    {
        get
        {
            if (!_unitsInUse) return true;
            var cur = _currentUnit.Value;
            if (HomeUnit != null) return ReferenceEquals(HomeUnit, cur);
            return cur == null || !cur.Hides(Name);
        }
    }

    public static IEnumerable<Package> AllPackages => _allPackages.Values.Distinct();

    /// <summary>
    /// May a same-named symbol in this package be adopted by one of the
    /// name-based cross-package bridges (Startup.Sym / Startup.SymFn /
    /// CilAssembler.FindFunctionAcrossPackages)?
    ///
    /// The bridges exist for one reason: the C# runtime registers its helpers on
    /// CL or DOTCL-INTERNAL symbols, so Lisp code reading those names in one of
    /// dotcl's own packages produces a DIFFERENT symbol object that must still
    /// reach the registered function (dotcl-thread's %make-thread is the case
    /// this was built for). They
    /// are not a general "same name means same function" rule: letting an
    /// arbitrary library's package answer turns an undefined function into a
    /// silent call of an unrelated one (uiop:emptyp for a user's EMPTYP).
    /// So only dotcl's own namespace may serve as a bridge source.
    /// </summary>
    public bool IsBridgeSource =>
        Name == "COMMON-LISP" || Name == "DOTCL" || Name == "DOTNET"
        || Name.StartsWith("DOTCL-", StringComparison.Ordinal)
        || Name.StartsWith("DOTCL.", StringComparison.Ordinal);

    public (Symbol symbol, SymbolStatus status) FindSymbol(string name)
    {
        if (_externalSymbols.TryGetValue(name, out var sym))
            return (sym, SymbolStatus.External);
        if (_internalSymbols.TryGetValue(name, out sym))
            return (sym, SymbolStatus.Internal);
        foreach (var pkg in _useSnapshot)
        {
            if (pkg._externalSymbols.TryGetValue(name, out sym))
                return (sym, SymbolStatus.Inherited);
        }
        return (null!, SymbolStatus.None);
    }

    public (Symbol symbol, bool isNew) Intern(string name)
    {
        // Overwhelmingly the symbol already lives in this package: INTERN is what
        // every symbol reference in compiled code goes through (Startup.SymInPkg),
        // so it runs constantly and almost never creates anything. The dictionaries
        // are concurrent, so answering from them needs no lock: taking one on
        // every reference showed up as Monitor.Enter_Slowpath in profiles.
        // Anything not already here (a fresh symbol, or one inherited through the
        // use-list) still goes the locked route below.
        if (_externalSymbols.TryGetValue(name, out var existing)
            || _internalSymbols.TryGetValue(name, out existing))
            return (existing, false);

        lock (_pkgLock)
        {
            var (sym, status) = FindSymbol(name);
            if (status != SymbolStatus.None)
                return (sym, false);
            // Keyword names are interned strings, so a compiled function's table of valid
            // keys (string literals) can be matched against them by reference.
            sym = new Symbol(ReferenceEquals(this, Startup.KeywordPkg) || Name == "KEYWORD" ? string.Intern(name) : name, this);
            _internalSymbols[name] = sym;
            TouchSymbols();
            // KEYWORD package: auto-export and make self-evaluating
            if (Name == "KEYWORD")
            {
                Export(sym);
                sym.Value = sym;
            }
            return (sym, true);
        }
    }

    public void Export(Symbol sym)
    {
        lock (_pkgLock)
        {
            var (found, status) = FindSymbol(sym.Name);

            if (status == SymbolStatus.None)
            {
                // Symbol is not accessible at all - signal package-error
                var err = new LispError($"EXPORT: symbol {sym.Name} is not accessible in package {Name}");
                err.ConditionTypeName = "PACKAGE-ERROR";
                err.PackageRef = new LispString(Name);
                throw new LispErrorException(err);
            }

            if (status == SymbolStatus.External && ReferenceEquals(found, sym))
            {
                // Already external - no effect
                return;
            }

            // CLHS 11.1.1.2.5: exporting a symbol whose name is already taken in this
            // package by another symbol (its own, or an inherited one) is a name
            // conflict; without a resolution the package's own symbol was silently
            // lost. DOTCL_EXPORT_CONFLICT=warn only reports it and replaces, as it did.
            if (!ReferenceEquals(found, sym) && !BridgeConflictIsSilent(found, sym))
            {
                if (Environment.GetEnvironmentVariable("DOTCL_EXPORT_CONFLICT") == "warn")
                {
                    Console.Error.WriteLine(
                        $";; EXPORT-CONFLICT: {sym.HomePackage?.Name}:{sym.Name} replaces {found.HomePackage?.Name}:{found.Name} ({status}) in package {Name}");
                }
                else
                {
                    var res = Runtime.SignalNameConflict(
                        this, "EXPORT", sym, found, new[] { sym, found },
                        keepOld: true, takeNew: true, importChoices: false,
                        resolve: false, continueRestart: true);
                    if (res.Choice == Runtime.NameConflictChoice.KeepOld) return;
                    // TakeNew / Continue: the exported symbol replaces the other one.
                }
            }

            // Check for name conflicts in packages that use this package
            while (true)
            {
                Package? conflictPkg = null;
                Symbol? conflictOld = null;
                SymbolStatus oldStatus = SymbolStatus.None;
                foreach (var usingPkg in UsedByList())
                {
                    // Skip if the using package has a shadowing symbol for this name
                    if (usingPkg.IsShadowing(sym.Name))
                        continue;

                    var (theirSym, theirStatus) = usingPkg.FindSymbol(sym.Name);
                    if (theirStatus != SymbolStatus.None && !ReferenceEquals(theirSym, sym)
                        && !BridgeConflictIsSilent(theirSym, sym))
                    {
                        conflictPkg = usingPkg;
                        conflictOld = theirSym;
                        oldStatus = theirStatus;
                        break;
                    }
                }
                if (conflictPkg == null) break;

                var old = conflictOld!;
                var result = Runtime.SignalNameConflict(
                    conflictPkg, "EXPORT", sym, old,
                    new[] { sym, old },
                    keepOld: true, takeNew: true, importChoices: false,
                    resolve: true, continueRestart: true);
                switch (result.Choice)
                {
                    case Runtime.NameConflictChoice.KeepOld:
                        conflictPkg.Shadow(sym.Name);
                        break;
                    case Runtime.NameConflictChoice.TakeNew:
                    case Runtime.NameConflictChoice.Continue:
                        if (oldStatus is SymbolStatus.Internal or SymbolStatus.External)
                            conflictPkg.Unintern(old);
                        else
                            conflictPkg.ShadowingImport(sym);
                        break;
                    case Runtime.NameConflictChoice.Resolve:
                        Runtime.ResolveUseExportConflict(
                            conflictPkg, result.Chosen ?? sym);
                        break;
                    default:
                        break;
                }
            }

            if (status == SymbolStatus.Inherited)
            {
                // Import first, then export
                _internalSymbols[sym.Name] = sym;
                TouchSymbols();
            }

            _internalSymbols.TryRemove(sym.Name, out _);
            TouchSymbols();
            _externalSymbols[sym.Name] = sym;
            TouchSymbols();
        }
    }

    public void Import(Symbol sym)
    {
        lock (_pkgLock)
        {
            var (existing, status) = FindSymbol(sym.Name);

            if (status != SymbolStatus.None)
            {
                // Same symbol already accessible - nothing to do
                if (ReferenceEquals(existing, sym))
                {
                    // But ensure it's directly present (not just inherited)
                    if (status == SymbolStatus.Inherited)
                    {
                        _internalSymbols[sym.Name] = sym;
                        TouchSymbols();
                    }
                    return;
                }
                // Different symbol with same name - name conflict!
                var err = new LispError(
                    $"IMPORT: importing {sym} into package {Name} conflicts with existing symbol {existing}");
                err.ConditionTypeName = "PACKAGE-ERROR";
                err.PackageRef = new LispString(Name);
                throw new LispErrorException(err);
            }

            // No existing symbol - import it
            _internalSymbols[sym.Name] = sym;
            TouchSymbols();

            // If the symbol has no home package, set it to this package
            if (sym.HomePackage == null)
                sym.HomePackage = this;
        }
    }

    /// <summary>Import SYM after its name conflict has been resolved.</summary>
    internal void ImportForce(Symbol sym)
    {
        lock (_pkgLock)
        {
            _internalSymbols[sym.Name] = sym;
            TouchSymbols();
            if (sym.HomePackage == null)
                sym.HomePackage = this;
        }
    }

    /// <summary>Dotcl's own namespaces (CL, DOTNET, DOTCL) share names that a
    /// package using two of them has always silently kept the first of.</summary>
    private static bool BridgeConflictIsSilent(Symbol oldSymbol, Symbol newSymbol)
        => oldSymbol.HomePackage?.IsBridgeSource == true
           && newSymbol.HomePackage?.IsBridgeSource == true;

    public void UsePackage(Package pkg)
    {
        lock (_pkgLock)
        {
            if (_useList.Contains(pkg)) return;

            while (true)
            {
                Symbol? conflictNew = null, conflictOld = null;
                SymbolStatus oldStatus = SymbolStatus.None;
                foreach (var sym in pkg.ExternalSymbols)
                {
                    var (existing, status) = FindSymbol(sym.Name);
                    if (status == SymbolStatus.None || ReferenceEquals(existing, sym)
                        || IsShadowing(sym.Name) || BridgeConflictIsSilent(existing, sym))
                        continue;
                    conflictNew = sym;
                    conflictOld = existing;
                    oldStatus = status;
                    break;
                }
                if (conflictNew == null) break;

                var newSym = conflictNew;
                var old = conflictOld!;
                var result = Runtime.SignalNameConflict(
                    this, "USE-PACKAGE", newSym, old,
                    new[] { newSym, old },
                    keepOld: true, takeNew: true, importChoices: false,
                    resolve: true, continueRestart: true);
                switch (result.Choice)
                {
                    case Runtime.NameConflictChoice.KeepOld:
                        Shadow(newSym.Name);
                        break;
                    case Runtime.NameConflictChoice.TakeNew:
                    case Runtime.NameConflictChoice.Continue:
                        if (oldStatus is SymbolStatus.Internal or SymbolStatus.External)
                            Unintern(old);
                        else
                            ShadowingImport(newSym);
                        break;
                    case Runtime.NameConflictChoice.Resolve:
                        Runtime.ResolveUseExportConflict(this, result.Chosen ?? newSym);
                        break;
                    default:
                        break;
                }
            }

            _useList.Add(pkg);
            _useSnapshot = _useList.ToArray();
        }
    }

    /// <summary>Bumped whenever a package stops mapping a name to the symbol
    /// it mapped it to before (UNINTERN, SHADOWING-IMPORT over a present
    /// symbol, SHADOW over an inherited one, UNUSE-PACKAGE, RENAME-PACKAGE,
    /// DELETE-PACKAGE). A cache of (name, package) -> symbol lookups is valid
    /// only while this has not moved. Interning and exporting never change an
    /// existing mapping, so they leave it alone.</summary>
    public static int MappingEpoch => _mappingEpoch;
    private static int _mappingEpoch;
    private static void BumpMappingEpoch() => System.Threading.Interlocked.Increment(ref _mappingEpoch);

    public void Shadow(string name)
    {
        lock (_pkgLock)
        {
            // Shadow creates a new symbol in this package, even if an inherited one exists
            if (!_internalSymbols.ContainsKey(name) && !_externalSymbols.ContainsKey(name))
            {
                var sym = new Symbol(name, this);
                _internalSymbols[name] = sym;
                TouchSymbols();
                BumpMappingEpoch();
            }
            _shadowingSymbols.Add(name);
        }
    }

    public void Unexport(Symbol sym)
    {
        lock (_pkgLock)
        {
            // Check if the symbol is accessible in this package
            var (found, status) = FindSymbol(sym.Name);

            if (status == SymbolStatus.None || (status != SymbolStatus.None && !ReferenceEquals(found, sym)))
            {
                // Symbol is not accessible - signal package-error
                var err = new LispError($"UNEXPORT: symbol {sym.Name} is not accessible in package {Name}");
                err.ConditionTypeName = "PACKAGE-ERROR";
                err.PackageRef = new LispString(Name);
                throw new LispErrorException(err);
            }

            // If currently external, move to internal
            if (status == SymbolStatus.External)
            {
                _externalSymbols.TryRemove(sym.Name, out _);
                TouchSymbols();
                _internalSymbols[sym.Name] = sym;
                TouchSymbols();
            }
            // If internal or inherited, unexport has no effect (not an error)
        }
    }

    public void UnusePackage(Package pkg)
    {
        lock (_pkgLock)
        {
            _useList.Remove(pkg);
            _useSnapshot = _useList.ToArray();
            BumpMappingEpoch();
        }
    }

    private readonly List<string> _nicknames = new();
    public IReadOnlyList<string> Nicknames
    {
        get { lock (_pkgLock) return _nicknames.ToList(); }
    }

    public void AddNickname(string nickname)
    {
        lock (_pkgLock)
        {
            var physical = HomeUnit == null ? nickname : HomeUnit.Prefix + nickname;
            if (!_nicknames.Contains(physical))
                _nicknames.Add(Register(nickname));
        }
    }

    private int _symVersion;
    private sealed class SymSnapshot
    {
        public int Version;
        public Symbol[] External = Array.Empty<Symbol>();
        public Symbol[] Internal = Array.Empty<Symbol>();
    }
    private SymSnapshot? _symSnap;

    private void TouchSymbols() => System.Threading.Interlocked.Increment(ref _symVersion);

    /// <summary>Arrays of this package's own symbols, rebuilt only after a table mutation.</summary>
    private SymSnapshot Snapshot()
    {
        var snap = _symSnap;
        int v = System.Threading.Volatile.Read(ref _symVersion);
        if (snap != null && snap.Version == v) return snap;
        snap = new SymSnapshot { Version = v, External = _externalSymbols.Values.ToArray(), Internal = _internalSymbols.Values.ToArray() };
        _symSnap = snap;
        return snap;
    }

    /// <summary>Prepend each external symbol onto RESULT (canonicalized NIL / T), skipping the ones
    /// whose name is present in SKIPPRESENTIN when that is given. Walks the table in place, where
    /// ExternalSymbols copies it first.</summary>
    internal LispObject ConsExternalOnto(LispObject result, Package? skipPresentIn)
    {
        foreach (var sym in Snapshot().External)
        {
            if (skipPresentIn != null && skipPresentIn.IsPresent(sym.Name)) continue;
            result = new Cons(Runtime.CanonicalizeSymbol(sym), result);
        }
        return result;
    }

    /// <summary>Prepend the external symbols of USED that are inherited here: those whose names are not
    /// present in this package. When this package has fewer symbols of its own than USED exports, the
    /// names that clash are found by looking up this package's names in USED (usually none), instead of
    /// looking up every exported name here.</summary>
    internal LispObject ConsInheritedOnto(LispObject result, Package used)
    {
        if (_externalSymbols.Count + _internalSymbols.Count >= used._externalSymbols.Count)
            return used.ConsExternalOnto(result, this);
        HashSet<string>? blocked = null;
        foreach (var kv in _externalSymbols)
            if (used._externalSymbols.ContainsKey(kv.Key)) (blocked ??= new()).Add(kv.Key);
        foreach (var kv in _internalSymbols)
            if (used._externalSymbols.ContainsKey(kv.Key)) (blocked ??= new()).Add(kv.Key);
        foreach (var sym in used.Snapshot().External)
        {
            if (blocked != null && blocked.Contains(sym.Name)) continue;
            result = new Cons(Runtime.CanonicalizeSymbol(sym), result);
        }
        return result;
    }

    internal LispObject ConsInternalOnto(LispObject result)
    {
        foreach (var sym in Snapshot().Internal)
            result = new Cons(Runtime.CanonicalizeSymbol(sym), result);
        return result;
    }

    /// <summary>Is NAME the name of a symbol present (external or internal) in this package?</summary>
    internal bool IsPresent(string name)
        => _externalSymbols.ContainsKey(name) || _internalSymbols.ContainsKey(name);

    public IEnumerable<Symbol> ExternalSymbols => _externalSymbols.Values;
    public IEnumerable<Symbol> InternalSymbols => _internalSymbols.Values;
    public bool IsExternalSymbol(string name) => _externalSymbols.ContainsKey(name);

    // Local nickname API (CDR 5)
    // ConcurrentDictionary.IsEmpty takes every bucket lock when the dictionary is
    // empty, and FindPackageNamed asks on each lookup, so threads interning in
    // parallel queued on it. The flag is rewritten under one lock by every writer.
    private volatile bool _hasLocalNicknames;
    private void NoteLocalNicknames() => _hasLocalNicknames = !_localNicknames.IsEmpty;

    public void AddLocalNickname(string nickname, Package pkg)
    {
        lock (_localNicknames) { _localNicknames[nickname] = pkg; NoteLocalNicknames(); }
    }
    public bool RemoveLocalNickname(string nickname)
    {
        lock (_localNicknames)
        {
            bool removed = _localNicknames.TryRemove(nickname, out _);
            NoteLocalNicknames();
            return removed;
        }
    }
    public Package? FindLocalNickname(string nickname) =>
        _hasLocalNicknames && _localNicknames.TryGetValue(nickname, out var p) ? p : null;
    public bool HasLocalNicknames => _hasLocalNicknames;
    /// <summary>Drop every local nickname that names PKG; true if any was.</summary>
    public bool RemoveLocalNicknamesFor(Package pkg)
    {
        lock (_localNicknames)
        {
            bool any = false;
            foreach (var kv in _localNicknames)
                if (ReferenceEquals(kv.Value, pkg) && _localNicknames.TryRemove(kv.Key, out _))
                    any = true;
            NoteLocalNicknames();
            return any;
        }
    }
    public void ClearLocalNicknames()
    {
        lock (_localNicknames) { _localNicknames.Clear(); NoteLocalNicknames(); }
    }
    public IEnumerable<(string Nick, Package Pkg)> LocalNicknames =>
        _localNicknames.Select(kv => (kv.Key, kv.Value));

    /// <summary>
    /// The local nickname this package has for PKG, or null when it has none.
    /// The printer uses it so a symbol prints the way this package can read it
    /// back: declaring a nickname is mostly about not writing the long physical
    /// name, and printing the physical name anyway does not round-trip.
    ///
    /// Several nicknames may point at one package; the shortest wins, ties
    /// broken by ordinal, so the choice does not depend on hash order and the
    /// same image prints the same text twice.
    /// </summary>
    public string? LocalNicknameFor(Package pkg)
    {
        string? best = null;
        foreach (var kv in _localNicknames)
        {
            if (!ReferenceEquals(kv.Value, pkg)) continue;
            if (best == null || kv.Key.Length < best.Length
                || (kv.Key.Length == best.Length
                    && string.CompareOrdinal(kv.Key, best) < 0))
                best = kv.Key;
        }
        return best;
    }
    // Return a snapshot under _pkgLock so iteration by another thread does
    // not race with Use / Unuse mutating the underlying List<Package>
    // Same pattern below for ShadowingSymbolNames and Nicknames.
    public IReadOnlyList<Package> UseList
    {
        get { lock (_pkgLock) return _useList.ToList(); }
    }

    private readonly HashSet<string> _shadowingSymbols = new();
    public IEnumerable<string> ShadowingSymbolNames
    {
        get { lock (_pkgLock) return _shadowingSymbols.ToList(); }
    }

    public bool Unintern(string name)
    {
        lock (_pkgLock)
        {
            Symbol? sym = null;
            bool isExternal = _externalSymbols.TryGetValue(name, out sym);
            if (!isExternal && !_internalSymbols.TryGetValue(name, out sym))
                return false;  // symbol not present
            return RemoveSymbol(sym!, isExternal);
        }
    }

    /// <summary>Remove SYM itself from this package; a different symbol of the
    /// same name is left alone (CLHS UNINTERN removes the symbol).</summary>
    public bool Unintern(Symbol sym)
    {
        lock (_pkgLock)
        {
            bool isExternal = _externalSymbols.TryGetValue(sym.Name, out var present)
                              && ReferenceEquals(present, sym);
            if (!isExternal
                && !(_internalSymbols.TryGetValue(sym.Name, out present)
                     && ReferenceEquals(present, sym)))
                return false;
            return RemoveSymbol(sym, isExternal);
        }
    }

    private bool RemoveSymbol(Symbol sym, bool isExternal)
    {
        string name = sym.Name;

        // Check for shadowing conflict before removing
        if (_shadowingSymbols.Contains(name))
        {
            // Count distinct inherited symbols with this name
            var inherited = new List<Symbol>();
            foreach (var pkg in _useList)
            {
                if (pkg._externalSymbols.TryGetValue(name, out var iSym))
                {
                    if (!inherited.Any(s => ReferenceEquals(s, iSym)))
                        inherited.Add(iSym);
                }
            }
            if (inherited.Count > 1)
            {
                var result = Runtime.SignalNameConflict(
                    this, "UNINTERN", sym, inherited[0], inherited,
                    keepOld: false, takeNew: false, importChoices: false,
                    resolve: true, continueRestart: true);
                if (result.Choice == Runtime.NameConflictChoice.Resolve)
                {
                    ResolveUninternConflict(result.Chosen ?? sym);
                    return true;
                }
            }
        }

        // OK to proceed
        if (isExternal)
            _externalSymbols.TryRemove(name, out _);
        else
            _internalSymbols.TryRemove(name, out _);
        TouchSymbols();
        _shadowingSymbols.Remove(name);
        if (ReferenceEquals(sym.HomePackage, this)) sym.HomePackage = null;
        BumpMappingEpoch();
        return true;
    }

    /// <summary>Keep one candidate accessible instead of removing the shadowing
    /// symbol, as SBCL's RESOLVE-CONFLICT restart does.</summary>
    private void ResolveUninternConflict(Symbol chosen)
    {
        var (present, status) = FindSymbol(chosen.Name);
        if (status is SymbolStatus.Internal or SymbolStatus.External)
        {
            if (ReferenceEquals(present, chosen))
                Shadow(chosen.Name);
            else
                ShadowingImport(chosen);
        }
        else
        {
            ShadowingImport(chosen);
        }
    }

    public void ShadowingImport(Symbol sym)
    {
        lock (_pkgLock)
        {
            bool wasExternal = _externalSymbols.TryRemove(sym.Name, out var oldExt);
            TouchSymbols();
            _internalSymbols.TryRemove(sym.Name, out var oldInt);
            TouchSymbols();
            // The symbol it replaces is uninterned (CLHS SHADOWING-IMPORT), so it
            // loses this package as its home, as UNINTERN does.
            var old = oldExt ?? oldInt;
            if (old != null && !ReferenceEquals(old, sym))
            {
                if (old.HomePackage == this) old.HomePackage = null;
                BumpMappingEpoch();
            }
            // A different symbol is inserted as an internal symbol (CLHS SHADOWING-IMPORT);
            // a symbol that is already present keeps the status it has.
            if (wasExternal && ReferenceEquals(old, sym))
                _externalSymbols[sym.Name] = sym;
            else
                _internalSymbols[sym.Name] = sym;
            TouchSymbols();
            _shadowingSymbols.Add(sym.Name);
        }
    }

    public bool IsShadowing(string name)
    {
        lock (_pkgLock) return _shadowingSymbols.Contains(name);
    }

    public IEnumerable<Package> UsedByList()
    {
        // Snapshot of AllPackages; each package's use-list is read through its
        // own immutable snapshot, so no per-package lock is needed.
        var snapshot = _allPackages.Values.Distinct().ToList();
        foreach (var pkg in snapshot)
            if (Array.IndexOf(pkg._useSnapshot, this) >= 0)
                yield return pkg;
    }

    public void Rename(string newName, IEnumerable<string>? newNicknames = null)
    {
        lock (_pkgLock)
        {
            // Remove old name and nicknames from registry
            Unregister(Name);
            foreach (var n in _nicknames)
                Unregister(n);
            _nicknames.Clear();
            Name = Register(newName);
            _nameString = null;
            BumpMappingEpoch();   // PACKAGE-NAME must answer with the new name
            if (newNicknames != null)
                foreach (var n in newNicknames)
                {
                    var physical = HomeUnit == null ? n : HomeUnit.Prefix + n;
                    if (!_nicknames.Contains(physical))
                        _nicknames.Add(Register(n));
                }
        }
    }

    private bool _deleted;
    public bool IsDeleted => _deleted;

    // Package lock: when true, definers that would rebind symbols
    // whose home package is this package must signal a package-lock-violation.
    // Step 1: plumbing only: no enforcement yet.
    public bool IsLocked { get; set; }

    /// <summary>
    /// Perform the actual deletion cleanup (steps 3-8 of CLHS delete-package).
    /// Caller must handle already-deleted check and used-by signaling.
    /// </summary>
    public void PerformDelete()
    {
        lock (_pkgLock)
        {
            BumpMappingEpoch();
            // Unuse all packages that this package uses
            _useList.Clear();
            _useSnapshot = Array.Empty<Package>();

            // Unintern all symbols, setting home package to nil
            foreach (var sym in _internalSymbols.Values)
            {
                if (ReferenceEquals(sym.HomePackage, this))
                    sym.HomePackage = null;
            }
            foreach (var sym in _externalSymbols.Values)
            {
                if (ReferenceEquals(sym.HomePackage, this))
                    sym.HomePackage = null;
            }
            _internalSymbols.Clear();
            TouchSymbols();
            _externalSymbols.Clear();
            TouchSymbols();
            _shadowingSymbols.Clear();

            // Remove from global package list
            Unregister(Name);
            foreach (var n in _nicknames)
                Unregister(n);
            _nicknames.Clear();

            // Mark as deleted
            _deleted = true;
        }
    }

    public override string ToString() => _deleted ? "#<DELETED PACKAGE>" : $"#<PACKAGE \"{VisibleName}\">";
}
