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
    // ConcurrentDictionary prevents enumeration-during-mutation crashes when
    // multiple threads intern/export/unintern concurrently. Multi-step
    // operations (e.g. Intern, Export) wrap with `lock (this)` for atomicity.
    private readonly ConcurrentDictionary<string, Symbol> _internalSymbols = new();
    private readonly ConcurrentDictionary<string, Symbol> _externalSymbols = new();
    private readonly List<Package> _useList = new();
    private readonly object _pkgLock = new();
    // Local nicknames: maps nickname string → package (per CDR 5 / SBCL package-local-nicknames)
    private readonly ConcurrentDictionary<string, Package> _localNicknames = new();

    private static readonly ConcurrentDictionary<string, Package> _allPackages = new();

    public Package(string name, params string[] nicknames)
    {
        Name = name;
        _allPackages[name] = this;
        foreach (var n in nicknames)
        {
            _allPackages[n] = this;
            _nicknames.Add(n);
        }
    }

    public static Package? FindPackage(string name)
    {
        _allPackages.TryGetValue(name, out var pkg);
        return pkg;
    }

    public static IEnumerable<Package> AllPackages => _allPackages.Values.Distinct();

    public (Symbol symbol, SymbolStatus status) FindSymbol(string name)
    {
        if (_externalSymbols.TryGetValue(name, out var sym))
            return (sym, SymbolStatus.External);
        if (_internalSymbols.TryGetValue(name, out sym))
            return (sym, SymbolStatus.Internal);
        lock (_pkgLock)
        {
            foreach (var pkg in _useList)
            {
                if (pkg._externalSymbols.TryGetValue(name, out sym))
                    return (sym, SymbolStatus.Inherited);
            }
        }
        return (null!, SymbolStatus.None);
    }

    public (Symbol symbol, bool isNew) Intern(string name)
    {
        lock (_pkgLock)
        {
            var (sym, status) = FindSymbol(name);
            if (status != SymbolStatus.None)
                return (sym, false);
            sym = new Symbol(name, this);
            _internalSymbols[name] = sym;
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

            // Check for name conflicts in packages that use this package
            foreach (var usingPkg in UsedByList())
            {
                // Skip if the using package has a shadowing symbol for this name
                if (usingPkg.IsShadowing(sym.Name))
                    continue;

                var (theirSym, theirStatus) = usingPkg.FindSymbol(sym.Name);
                if (theirStatus != SymbolStatus.None && !ReferenceEquals(theirSym, sym))
                {
                    // Name conflict - signal package-error
                    var err = new LispError($"EXPORT: exporting symbol {sym.Name} from package {Name} would cause a name conflict in package {usingPkg.Name}");
                    err.ConditionTypeName = "PACKAGE-ERROR";
                    err.PackageRef = new LispString(Name);
                    throw new LispErrorException(err);
                }
            }

            if (status == SymbolStatus.Inherited)
            {
                // Import first, then export
                _internalSymbols[sym.Name] = sym;
            }

            _internalSymbols.TryRemove(sym.Name, out _);
            _externalSymbols[sym.Name] = sym;
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

            // If the symbol has no home package, set it to this package
            if (sym.HomePackage == null)
                sym.HomePackage = this;
        }
    }

    public void UsePackage(Package pkg)
    {
        lock (_pkgLock)
        {
            if (!_useList.Contains(pkg))
                _useList.Add(pkg);
        }
    }

    public void Shadow(string name)
    {
        lock (_pkgLock)
        {
            // Shadow creates a new symbol in this package, even if an inherited one exists
            if (!_internalSymbols.ContainsKey(name) && !_externalSymbols.ContainsKey(name))
            {
                var sym = new Symbol(name, this);
                _internalSymbols[name] = sym;
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
                _internalSymbols[sym.Name] = sym;
            }
            // If internal or inherited, unexport has no effect (not an error)
        }
    }

    public void UnusePackage(Package pkg)
    {
        lock (_pkgLock)
        {
            _useList.Remove(pkg);
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
            if (!_nicknames.Contains(nickname))
            {
                _nicknames.Add(nickname);
                _allPackages[nickname] = this;
            }
        }
    }

    public IEnumerable<Symbol> ExternalSymbols => _externalSymbols.Values;
    public IEnumerable<Symbol> InternalSymbols => _internalSymbols.Values;
    public bool IsExternalSymbol(string name) => _externalSymbols.ContainsKey(name);

    // Local nickname API (CDR 5)
    public void AddLocalNickname(string nickname, Package pkg) =>
        _localNicknames[nickname] = pkg;
    public bool RemoveLocalNickname(string nickname) =>
        _localNicknames.TryRemove(nickname, out _);
    public Package? FindLocalNickname(string nickname) =>
        _localNicknames.TryGetValue(nickname, out var p) ? p : null;
    public IEnumerable<(string Nick, Package Pkg)> LocalNicknames =>
        _localNicknames.Select(kv => (kv.Key, kv.Value));
    // Return a snapshot under _pkgLock so iteration by another thread does
    // not race with Use / Unuse mutating the underlying List<Package>
    // (#171 Step 2). Same pattern below for ShadowingSymbolNames and Nicknames.
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
                    var err = new LispError(
                        $"UNINTERN: removing shadowing symbol {name} from {Name} would cause a name conflict");
                    err.ConditionTypeName = "PACKAGE-ERROR";
                    err.PackageRef = new LispString(Name);
                    throw new LispErrorException(err);
                }
            }

            // OK to proceed
            if (isExternal)
                _externalSymbols.TryRemove(name, out _);
            else
                _internalSymbols.TryRemove(name, out _);
            _shadowingSymbols.Remove(name);
            if (sym!.HomePackage == this) sym.HomePackage = null;
            return true;
        }
    }

    public void ShadowingImport(Symbol sym)
    {
        lock (_pkgLock)
        {
            // Preserve external status: if the replaced symbol was external, keep new one external
            bool wasExternal = _externalSymbols.TryRemove(sym.Name, out _);
            _internalSymbols.TryRemove(sym.Name, out _);
            // Import the symbol and mark it as shadowing
            if (wasExternal)
                _externalSymbols[sym.Name] = sym;
            else
                _internalSymbols[sym.Name] = sym;
            _shadowingSymbols.Add(sym.Name);
        }
    }

    public bool IsShadowing(string name)
    {
        lock (_pkgLock) return _shadowingSymbols.Contains(name);
    }

    public IEnumerable<Package> UsedByList()
    {
        // Snapshot of AllPackages. Per-pkg _useList may mutate concurrently,
        // so each pkg's Contains check takes its own lock.
        var snapshot = _allPackages.Values.Distinct().ToList();
        foreach (var pkg in snapshot)
        {
            bool contained;
            lock (pkg._pkgLock)
                contained = pkg._useList.Contains(this);
            if (contained) yield return pkg;
        }
    }

    public void Rename(string newName, IEnumerable<string>? newNicknames = null)
    {
        lock (_pkgLock)
        {
            // Remove old name and nicknames from registry
            _allPackages.TryRemove(Name, out _);
            foreach (var n in _nicknames)
                _allPackages.TryRemove(n, out _);
            _nicknames.Clear();
            Name = newName;
            _allPackages[newName] = this;
            if (newNicknames != null)
                foreach (var n in newNicknames)
                {
                    // AddNickname would re-acquire _pkgLock (re-entrant lock, but avoid nested pattern)
                    if (!_nicknames.Contains(n))
                    {
                        _nicknames.Add(n);
                        _allPackages[n] = this;
                    }
                }
        }
    }

    private bool _deleted;
    public bool IsDeleted => _deleted;

    // Package lock: when true, definers that would rebind symbols
    // whose home package is this package must signal a package-lock-violation.
    // Step 1: plumbing only — no enforcement yet (see #93).
    public bool IsLocked { get; set; }

    /// <summary>
    /// Perform the actual deletion cleanup (steps 3-8 of CLHS delete-package).
    /// Caller must handle already-deleted check and used-by signaling.
    /// </summary>
    public void PerformDelete()
    {
        lock (_pkgLock)
        {
            // Unuse all packages that this package uses
            _useList.Clear();

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
            _externalSymbols.Clear();
            _shadowingSymbols.Clear();

            // Remove from global package list
            _allPackages.TryRemove(Name, out _);
            foreach (var n in _nicknames)
                _allPackages.TryRemove(n, out _);
            _nicknames.Clear();

            // Mark as deleted
            _deleted = true;
        }
    }

    public override string ToString() => _deleted ? "#<DELETED PACKAGE>" : $"#<PACKAGE \"{Name}\">";
}
