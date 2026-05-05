namespace DotCL;

public static partial class Runtime
{
    // --- Package operations ---

    public static LispObject MakePackage(LispObject name)
    {
        string pkgName = AsStringDesignator(name, "MAKE-PACKAGE");
        var existing = Package.FindPackage(pkgName);
        if (existing != null) return existing;
        return new Package(pkgName);
    }

    public static LispObject PackageUse(LispObject pkg, LispObject usePkg)
    {
        var p = ResolvePackage(pkg, "USE-PACKAGE");
        var up = ResolvePackage(usePkg, "USE-PACKAGE");
        p.UsePackage(up);
        return T.Instance;
    }

    public static LispObject PackageExport(LispObject pkg, LispObject sym)
    {
        var p = ResolvePackage(pkg, "EXPORT");
        var s = ResolveSymbol(sym, "EXPORT");
        p.Export(s);
        return T.Instance;
    }

    public static LispObject PackageExternalSymbolsList(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "DO-EXTERNAL-SYMBOLS");
        LispObject result = Nil.Instance;
        foreach (var s in p.ExternalSymbols)
            result = new Cons(s, result);
        return result;
    }

    public static LispObject PackageAllSymbolsList(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "DO-SYMBOLS");
        LispObject result = Nil.Instance;
        foreach (var s in p.ExternalSymbols)
            result = new Cons(s, result);
        foreach (var s in p.InternalSymbols)
            result = new Cons(s, result);
        foreach (var used in p.UseList)
            foreach (var s in used.ExternalSymbols)
            {
                // Only include if not shadowed by a directly present symbol
                var (_, status) = p.FindSymbol(s.Name);
                if (status == SymbolStatus.Inherited)
                    result = new Cons(s, result);
            }
        return result;
    }

    public static LispObject CollectPackageIteratorEntries(LispObject pkgListForm, LispObject symTypes)
    {
        // Resolve package list: single designator or list of designators
        var packages = new List<Package>();
        if (pkgListForm is Cons || pkgListForm is Nil)
        {
            // It's a list of package designators
            var cur = pkgListForm;
            while (cur is Cons c)
            {
                packages.Add(ResolvePackage(c.Car, "WITH-PACKAGE-ITERATOR"));
                cur = c.Cdr;
            }
        }
        else
        {
            // Single package designator
            packages.Add(ResolvePackage(pkgListForm, "WITH-PACKAGE-ITERATOR"));
        }

        // Parse symbol types into flags
        bool wantInternal = false, wantExternal = false, wantInherited = false;
        var cur2 = symTypes;
        while (cur2 is Cons c2)
        {
            if (c2.Car is Symbol s)
            {
                switch (s.Name)
                {
                    case "INTERNAL": wantInternal = true; break;
                    case "EXTERNAL": wantExternal = true; break;
                    case "INHERITED": wantInherited = true; break;
                }
            }
            cur2 = c2.Cdr;
        }

        // Collect entries: each is (symbol accessibility package)
        var internalKw = Startup.Keyword("INTERNAL");
        var externalKw = Startup.Keyword("EXTERNAL");
        var inheritedKw = Startup.Keyword("INHERITED");

        LispObject result = Nil.Instance;
        foreach (var pkg in packages)
        {
            if (wantExternal)
            {
                foreach (var sym in pkg.ExternalSymbols)
                {
                    var entry = new Cons(sym, new Cons(externalKw, new Cons(pkg, Nil.Instance)));
                    result = new Cons(entry, result);
                }
            }
            if (wantInternal)
            {
                foreach (var sym in pkg.InternalSymbols)
                {
                    var entry = new Cons(sym, new Cons(internalKw, new Cons(pkg, Nil.Instance)));
                    result = new Cons(entry, result);
                }
            }
            if (wantInherited)
            {
                foreach (var used in pkg.UseList)
                {
                    foreach (var sym in used.ExternalSymbols)
                    {
                        // Only include if the symbol is actually inherited (not shadowed)
                        var (_, status) = pkg.FindSymbol(sym.Name);
                        if (status == SymbolStatus.Inherited)
                        {
                            var entry = new Cons(sym, new Cons(inheritedKw, new Cons(pkg, Nil.Instance)));
                            result = new Cons(entry, result);
                        }
                    }
                }
            }
        }
        return result;
    }

    private static Symbol ResolveSymbol(LispObject obj, string context)
    {
        if (obj is Symbol s) return s;
        if (obj is T) return Startup.T_SYM;
        if (obj is Nil) return Startup.Sym("NIL");
        throw new LispErrorException(new LispTypeError($"{context}: not a symbol", obj));
    }

    private static Package ResolvePackage(LispObject pkg, string context)
    {
        if (pkg is Package pp) return pp;
        string name;
        if (pkg is LispString ls) name = ls.Value;
        else if (pkg is Symbol s) name = s.Name;
        else if (pkg is LispChar lc) name = lc.Value.ToString();
        else if (pkg is LispVector v && v.IsCharVector) name = v.ToCharString();
        else name = pkg.ToString()!;
        var found = Package.FindPackage(name);
        if (found != null) return found;
        var err = new LispError($"{context}: no package named {name}");
        err.ConditionTypeName = "PACKAGE-ERROR";
        err.PackageRef = new LispString(name);
        throw new LispErrorException(err);
    }

    public static LispObject PackageImport(LispObject pkg, LispObject sym)
    {
        var p = ResolvePackage(pkg, "IMPORT");
        var s = ResolveSymbol(sym, "IMPORT");

        retry:
        var (existing, status) = p.FindSymbol(s.Name);
        if (status != SymbolStatus.None && !ReferenceEquals(existing, s))
        {
            // Name conflict — signal correctable error with CONTINUE restart
            var restart = new LispRestart("CONTINUE",
                _ => Nil.Instance,
                description: $"Unintern {existing} from {p.Name} and import {s}.");
            RestartClusterStack.PushCluster(new[] { restart });
            try
            {
                var condition = MakeConditionFromType(
                    Startup.Sym("PACKAGE-ERROR"),
                    new LispObject[] {
                        Startup.Sym(":PACKAGE"), p,
                        Startup.Sym(":FORMAT-CONTROL"),
                        new LispString($"IMPORT: importing ~A into package ~A conflicts with existing symbol ~A."),
                        Startup.Sym(":FORMAT-ARGUMENTS"),
                        new Cons(s, new Cons(new LispString(p.Name), new Cons(existing!, Nil.Instance)))
                    });
                ConditionSystem.Error(condition);
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
            {
                // CONTINUE restart — unintern existing and retry
                p.Unintern(existing!.Name);
                goto retry;
            }
            finally
            {
                RestartClusterStack.PopCluster();
            }
        }

        p.Import(s);
        return T.Instance;
    }

    public static LispObject PackageShadow(LispObject pkg, LispObject name)
    {
        var p = ResolvePackage(pkg, "SHADOW");
        string n = ToStringDesignator(name);
        p.Shadow(n);
        return T.Instance;
    }

    public static LispObject PackageNickname(LispObject pkg, LispObject nickname)
    {
        if (pkg is not Package p)
            throw new LispErrorException(new LispTypeError("not a package", pkg));
        string n = ToStringDesignator(nickname);
        p.AddNickname(n);
        return T.Instance;
    }

    public static LispObject FindPackage(LispObject name)
    {
        if (name is Package pp) return pp;
        string pkgName;
        if (name is LispString ls) pkgName = ls.Value;
        else if (name is Symbol s) pkgName = s.Name;
        else if (name is LispChar lc) pkgName = lc.Value.ToString();
        else if (name is LispVector v && v.IsCharVector) pkgName = v.ToCharString();
        else pkgName = name.ToString()!;
        var pkg = Package.FindPackage(pkgName);
        return pkg ?? (LispObject)Nil.Instance;
    }

    public static LispObject PackageErrorPackage(LispObject condition)
    {
        if (condition is LispInstanceCondition lic)
        {
            // Try to get the PACKAGE slot from the CLOS instance
            if (lic.Instance.Class.SlotIndex.TryGetValue("PACKAGE", out int idx))
                return lic.Instance.Slots[idx] ?? Nil.Instance;
            if (lic.PackageRef != null) return lic.PackageRef;
        }
        if (condition is LispCondition lc && lc.PackageRef != null)
            return lc.PackageRef;
        return Nil.Instance;
    }

    public static LispObject FileErrorPathname(LispObject condition)
    {
        if (condition is LispInstanceCondition lic)
        {
            if (lic.Instance.Class.SlotIndex.TryGetValue("PATHNAME", out int idx))
                return lic.Instance.Slots[idx] ?? Nil.Instance;
            if (lic.FileErrorPathnameRef != null) return lic.FileErrorPathnameRef;
        }
        if (condition is LispCondition lc && lc.FileErrorPathnameRef != null)
            return lc.FileErrorPathnameRef;
        return Nil.Instance;
    }

    // Package lock enforcement (#93 step 2).
    // Called from %SET-FDEFINITION and RegisterMacroFunction. Signals an error
    // if the symbol's home package is locked, the symbol is external, and
    // DOTCL:*PACKAGE-LOCKS-DISABLED* is nil.
    // Only external symbols are protected: internal symbols in locked packages
    // (e.g. newly interned symbols like "Lazy CONS" in CL) may be freely defined.
    public static void CheckPackageLock(Symbol sym, string context)
    {
        var home = sym.HomePackage;
        if (home == null || !home.IsLocked) return;
        // Only block redefinition of symbols that are already exported.
        // Internal symbols in a locked package (e.g. freshly interned ones) are allowed.
        if (!home.IsExternalSymbol(sym.Name)) return;
        // Consult DOTCL:*PACKAGE-LOCKS-DISABLED* — without-package-locks binds this to T.
        var disabledSym = Startup.SymInPkg("*PACKAGE-LOCKS-DISABLED*", "DOTCL");
        if (DynamicBindings.TryGet(disabledSym, out var disabled) && disabled is not Nil)
            return;
        var err = new LispError(
            $"{context}: package {home.Name} is locked; cannot redefine {sym.Name}");
        err.ConditionTypeName = "PACKAGE-ERROR";
        err.PackageRef = new LispString(home.Name);
        throw new LispErrorException(err);
    }

    // Package lock API (DOTCL:LOCK-PACKAGE etc.) — step 1 of #93: plumbing, no enforcement.
    public static LispObject LockPackage(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "LOCK-PACKAGE");
        p.IsLocked = true;
        return T.Instance;
    }

    public static LispObject UnlockPackage(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "UNLOCK-PACKAGE");
        p.IsLocked = false;
        return T.Instance;
    }

    public static LispObject PackageLockedP(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "PACKAGE-LOCKED-P");
        return p.IsLocked ? T.Instance : Nil.Instance;
    }

    // Local nickname API (CDR 5 / SBCL package-local-nicknames)
    public static LispObject AddPackageLocalNickname(LispObject nick, LispObject actual, LispObject pkg)
    {
        var nickname = nick is LispString ns ? ns.Value : (nick is Symbol nsym ? nsym.Name : null);
        if (nickname == null) throw new LispErrorException(new LispError("ADD-PACKAGE-LOCAL-NICKNAME: nickname must be a string designator"));
        var actualPkg = ResolvePackage(actual, "ADD-PACKAGE-LOCAL-NICKNAME");
        var targetPkg = ResolvePackage(pkg, "ADD-PACKAGE-LOCAL-NICKNAME");
        targetPkg.AddLocalNickname(nickname, actualPkg);
        return targetPkg;
    }

    public static LispObject RemovePackageLocalNickname(LispObject nick, LispObject pkg)
    {
        var nickname = nick is LispString ns ? ns.Value : (nick is Symbol nsym ? nsym.Name : null);
        if (nickname == null) throw new LispErrorException(new LispError("REMOVE-PACKAGE-LOCAL-NICKNAME: nickname must be a string designator"));
        var targetPkg = ResolvePackage(pkg, "REMOVE-PACKAGE-LOCAL-NICKNAME");
        return targetPkg.RemoveLocalNickname(nickname) ? T.Instance : Nil.Instance;
    }

    public static LispObject PackageLocalNicknames(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "PACKAGE-LOCAL-NICKNAMES");
        LispObject result = Nil.Instance;
        foreach (var (nick, actualPkg) in p.LocalNicknames)
        {
            var pair = new Cons(new LispString(nick), actualPkg);
            result = new Cons(pair, result);
        }
        return result;
    }

    public static LispObject PackageName(LispObject pkg)
    {
        ValidatePackageDesignator(pkg, "PACKAGE-NAME");
        if (pkg is Package pp && pp.IsDeleted) return Nil.Instance;
        var p = ResolvePackage(pkg, "PACKAGE-NAME");
        if (p.IsDeleted) return Nil.Instance;
        return new LispString(p.Name);
    }

    public static LispObject InternSymbolV(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 2) throw MakeProgramError("INTERN", 1, 2, args.Length);
        var pkg = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
        return InternSymbol(args[0], pkg);
    }

    public static LispObject InternSymbol(LispObject name, LispObject pkg)
    {
        string symName = name switch
        {
            LispString s => s.Value,
            Symbol nameSym => nameSym.Name,
            LispChar c => c.Value.ToString(),
            LispVector v when v.IsCharVector => v.ToCharString(),
            _ => name.ToString()
        };
        Package p;
        if (pkg is Package pp) p = pp;
        else if (pkg is LispString ps) p = Package.FindPackage(ps.Value) ?? throw new LispErrorException(new LispError($"Package not found: {ps.Value}"));
        else if (pkg is Symbol psym) p = Package.FindPackage(psym.Name) ?? throw new LispErrorException(new LispError($"Package not found: {psym.Name}"));
        else if (pkg is LispChar pc) { var pn = pc.Value.ToString(); p = Package.FindPackage(pn) ?? throw new LispErrorException(new LispError($"Package not found: {pn}")); }
        else if (pkg is LispVector pv && pv.IsCharVector) { var pn = pv.ToCharString(); p = Package.FindPackage(pn) ?? throw new LispErrorException(new LispError($"Package not found: {pn}")); }
        else if (pkg is Nil) p = (Package)DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
        else throw new LispErrorException(new LispTypeError("INTERN: invalid package designator", pkg));

        var (resultSym, isNew) = p.Intern(symName);
        LispObject status;
        if (isNew)
        {
            status = Nil.Instance;
        }
        else
        {
            var (_, symStatus) = p.FindSymbol(symName);
            status = symStatus switch {
                SymbolStatus.External  => Startup.Keyword("EXTERNAL"),
                SymbolStatus.Inherited => Startup.Keyword("INHERITED"),
                _                      => Startup.Keyword("INTERNAL"),
            };
        }
        MultipleValues.Set(resultSym, status);
        return resultSym;
    }

    // --- Package operations for UIOP ---

    private static string ToStringDesignator(LispObject obj)
    {
        return obj switch
        {
            LispString s => s.Value,
            Symbol sym => sym.Name,
            Package p => p.Name,
            LispChar c => c.Value.ToString(),
            LispVector v when v.IsCharVector => v.ToCharString(),
            _ => obj.ToString()!
        };
    }

    private static string ToPackageName(LispObject obj) => ToStringDesignator(obj);

    public static LispObject MakePackageK(LispObject[] args)
    {
        // (make-package name &key nicknames use)
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("MAKE-PACKAGE: missing name argument"));

        string name = ToStringDesignator(args[0]);

        // Validate keyword arguments
        int kwStart = 1;
        if ((args.Length - kwStart) % 2 != 0)
            throw new LispErrorException(new LispProgramError("MAKE-PACKAGE: odd number of keyword arguments"));

        // Check for :allow-other-keys first
        bool allowOtherKeys = false;
        for (int i = kwStart; i < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS" && ks.HomePackage?.Name == "KEYWORD")
            {
                allowOtherKeys = !(args[i + 1] is Nil);
                break;
            }
        }

        // Validate all keys
        for (int i = kwStart; i < args.Length; i += 2)
        {
            if (args[i] is not Symbol kSym || kSym.HomePackage?.Name != "KEYWORD")
                throw new LispErrorException(new LispProgramError($"MAKE-PACKAGE: invalid keyword argument: {args[i]}"));
            string kName = kSym.Name;
            if (!allowOtherKeys && kName != "NICKNAMES" && kName != "USE" && kName != "ALLOW-OTHER-KEYS")
                throw new LispErrorException(new LispProgramError($"MAKE-PACKAGE: unrecognized keyword argument: :{kName}"));
        }

        // Check if package already exists - signal continuable package-error with CONTINUE restart
        var existing = Package.FindPackage(name);
        if (existing != null)
        {
            var restart = new LispRestart("CONTINUE",
                _ => existing,
                description: $"Return the existing package named \"{name}\".");
            RestartClusterStack.PushCluster(new[] { restart });
            try
            {
                var condition = MakeConditionFromType(
                    Startup.Sym("PACKAGE-ERROR"),
                    new LispObject[] {
                        Startup.Sym(":PACKAGE"), existing,
                        Startup.Sym(":FORMAT-CONTROL"),
                        new LispString($"A package named ~A already exists."),
                        Startup.Sym(":FORMAT-ARGUMENTS"),
                        new Cons(new LispString(name), Nil.Instance)
                    });
                ConditionSystem.Error(condition);
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
            {
                // CONTINUE restart was invoked — return the existing package
                return existing;
            }
            finally
            {
                RestartClusterStack.PopCluster();
            }
        }

        var pkg = new Package(name);

        // Parse keyword args - collect nicknames and use lists
        LispObject? nicknames = null;
        LispObject? use = null;
        for (int i = kwStart; i < args.Length; i += 2)
        {
            var kw = ((Symbol)args[i]).Name;
            if (kw == "NICKNAMES") nicknames = args[i + 1];
            else if (kw == "USE") use = args[i + 1];
        }

        // Process nicknames - check for conflicts
        if (nicknames != null)
        {
            var cur = nicknames;
            while (cur is Cons c)
            {
                string nick = ToStringDesignator(c.Car);
                var existingNick = Package.FindPackage(nick);
                if (existingNick != null && existingNick != pkg)
                {
                    var restart = new LispRestart("CONTINUE",
                        _ => Nil.Instance,
                        description: $"Skip the nickname \"{nick}\".");
                    RestartClusterStack.PushCluster(new[] { restart });
                    try
                    {
                        var condition = MakeConditionFromType(
                            Startup.Sym("PACKAGE-ERROR"),
                            new LispObject[] {
                                Startup.Sym(":PACKAGE"), existingNick,
                                Startup.Sym(":FORMAT-CONTROL"),
                                new LispString($"MAKE-PACKAGE: nickname ~A conflicts with existing package ~A."),
                                Startup.Sym(":FORMAT-ARGUMENTS"),
                                new Cons(new LispString(nick),
                                    new Cons(new LispString(existingNick.Name), Nil.Instance))
                            });
                        ConditionSystem.Error(condition);
                    }
                    catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
                    {
                        // CONTINUE restart was invoked — skip this nickname
                        cur = c.Cdr;
                        continue;
                    }
                    finally
                    {
                        RestartClusterStack.PopCluster();
                    }
                }
                pkg.AddNickname(nick);
                cur = c.Cdr;
            }
        }

        // Process :use
        if (use != null)
        {
            var cur = use;
            while (cur is Cons c)
            {
                var up = ResolvePackage(c.Car, "MAKE-PACKAGE :USE");
                pkg.UsePackage(up);
                cur = c.Cdr;
            }
        }

        return pkg;
    }

    public static LispObject DeletePackage(LispObject name)
    {
        Package? pkg;
        if (name is Package p)
        {
            pkg = p;
        }
        else
        {
            string n = ToPackageName(name);
            pkg = Package.FindPackage(n);
            if (pkg == null)
            {
                var restart = new LispRestart("CONTINUE",
                    _ => Nil.Instance,
                    description: $"Return NIL from DELETE-PACKAGE.");
                RestartClusterStack.PushCluster(new[] { restart });
                try
                {
                    var condition = MakeConditionFromType(
                        Startup.Sym("PACKAGE-ERROR"),
                        new LispObject[] {
                            Startup.Sym(":PACKAGE"), new LispString(n),
                            Startup.Sym(":FORMAT-CONTROL"),
                            new LispString($"DELETE-PACKAGE: there is no package named ~A."),
                            Startup.Sym(":FORMAT-ARGUMENTS"),
                            new Cons(new LispString(n), Nil.Instance)
                        });
                    ConditionSystem.Error(condition);
                }
                catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
                {
                    return Nil.Instance;
                }
                finally
                {
                    RestartClusterStack.PopCluster();
                }
                return Nil.Instance;
            }
        }

        // 1. If already deleted, return NIL
        if (pkg.IsDeleted) return Nil.Instance;

        // 2. Check used-by-list — signal correctable error with CONTINUE restart
        retry:
        var usedBy = pkg.UsedByList().ToList();
        if (usedBy.Count > 0)
        {
            var names = string.Join(", ", usedBy.Select(p2 => p2.Name));
            var restart = new LispRestart("CONTINUE",
                _ => Nil.Instance,
                description: $"Remove package {pkg.Name} from the use-lists of {names} and retry.");
            RestartClusterStack.PushCluster(new[] { restart });
            try
            {
                // Create a proper PACKAGE-ERROR condition via MakeConditionFromType
                var condition = MakeConditionFromType(
                    Startup.Sym("PACKAGE-ERROR"),
                    new LispObject[] {
                        Startup.Sym(":PACKAGE"), pkg,
                        Startup.Sym(":FORMAT-CONTROL"),
                        new LispString($"Package ~A is used by other packages: ~A."),
                        Startup.Sym(":FORMAT-ARGUMENTS"),
                        new Cons(new LispString(pkg.Name),
                            new Cons(new LispString(names), Nil.Instance))
                    });
                ConditionSystem.Error(condition);
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
            {
                // CONTINUE restart was invoked — unuse from all and retry
                foreach (var p2 in usedBy)
                    p2.UnusePackage(pkg);
                goto retry;
            }
            finally
            {
                RestartClusterStack.PopCluster();
            }
        }

        // 3-8. Perform the actual deletion
        pkg.PerformDelete();
        return T.Instance;
    }

    public static LispObject RenamePackage(LispObject[] args)
    {
        // (rename-package package new-name &optional new-nicknames)
        if (args.Length < 2 || args.Length > 3) throw MakeProgramError("RENAME-PACKAGE", 2, 3, args.Length);
        var p = ResolvePackage(args[0], "RENAME-PACKAGE");
        string newName = ToPackageName(args[1]);
        var nicknames = new List<string>();
        if (args.Length > 2)
        {
            var cur = args[2];
            while (cur is Cons c) { nicknames.Add(ToPackageName(c.Car)); cur = c.Cdr; }
        }
        p.Rename(newName, nicknames);
        return p;
    }

    public static LispObject FindSymbolL(LispObject[] args)
    {
        // (find-symbol name &optional package) → symbol, status
        if (args.Length < 1 || args.Length > 2) throw MakeProgramError("FIND-SYMBOL", 1, 2, args.Length);
        var pkg = args.Length > 1 ? ResolvePackage(args[1], "FIND-SYMBOL") :
            (Package)DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
        string symName = args[0] switch
        {
            LispString s => s.Value,
            Symbol sym => sym.Name,
            LispChar c => c.Value.ToString(),
            LispVector v when v.IsCharVector => v.ToCharString(),
            _ => args[0].ToString()!
        };
        var (foundSym, status) = pkg.FindSymbol(symName);
        LispObject statusSym = status switch
        {
            SymbolStatus.Internal => Startup.Keyword("INTERNAL"),
            SymbolStatus.External => Startup.Keyword("EXTERNAL"),
            SymbolStatus.Inherited => Startup.Keyword("INHERITED"),
            _ => Nil.Instance
        };
        MultipleValues.Set(status == SymbolStatus.None ? Nil.Instance : foundSym, statusSym);
        return status == SymbolStatus.None ? Nil.Instance : foundSym;
    }

    public static LispObject UninternSymbol(LispObject[] args)
    {
        // (unintern symbol &optional package)
        if (args.Length < 1 || args.Length > 2) throw MakeProgramError("UNINTERN", 1, 2, args.Length);
        string name = args[0] is Symbol sym ? sym.Name : args[0] is LispString s ? s.Value : args[0] is LispChar uc ? uc.Value.ToString() : args[0] is LispVector uv && uv.IsCharVector ? uv.ToCharString() : args[0].ToString()!;
        var pkg = args.Length > 1 ? ResolvePackage(args[1], "UNINTERN") :
            (Package)DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
        return pkg.Unintern(name) ? T.Instance : Nil.Instance;
    }

    public static LispObject UnexportSymbol(LispObject sym, LispObject pkg)
    {
        var p = ResolvePackage(pkg, "UNEXPORT");
        var s = ResolveSymbol(sym, "UNEXPORT");
        p.Unexport(s);
        return T.Instance;
    }

    public static LispObject UnusePackage(LispObject pkgToRemove, LispObject fromPkg)
    {
        var from = ResolvePackage(fromPkg, "UNUSE-PACKAGE");
        var toRemove = ResolvePackage(pkgToRemove, "UNUSE-PACKAGE");
        from.UnusePackage(toRemove);
        return T.Instance;
    }

    public static LispObject ShadowingImport(LispObject sym, LispObject pkg)
    {
        var p = ResolvePackage(pkg, "SHADOWING-IMPORT");
        var s = ResolveSymbol(sym, "SHADOWING-IMPORT");
        p.ShadowingImport(s);
        return T.Instance;
    }

    public static LispObject PackageUsedByList(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "PACKAGE-USED-BY-LIST");
        LispObject result = Nil.Instance;
        foreach (var used in p.UsedByList())
            result = new Cons(used, result);
        return result;
    }

    public static LispObject PackageUseListL(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "PACKAGE-USE-LIST");
        LispObject result = Nil.Instance;
        foreach (var used in p.UseList)
            result = new Cons(used, result);
        return result;
    }

    public static LispObject PackageShadowingSymbols(LispObject pkg)
    {
        var p = ResolvePackage(pkg, "PACKAGE-SHADOWING-SYMBOLS");
        LispObject result = Nil.Instance;
        foreach (var name in p.ShadowingSymbolNames)
        {
            var (sym, _) = p.FindSymbol(name);
            if (sym != null) result = new Cons(sym, result);
        }
        return result;
    }

    public static LispObject PackageNicknamesList(LispObject pkg)
    {
        ValidatePackageDesignator(pkg, "PACKAGE-NICKNAMES");
        var p = ResolvePackage(pkg, "PACKAGE-NICKNAMES");
        LispObject result = Nil.Instance;
        foreach (var n in p.Nicknames)
            result = new Cons(new LispString(n), result);
        return result;
    }

    public static LispObject ListAllPackages()
    {
        LispObject result = Nil.Instance;
        foreach (var p in Package.AllPackages)
            result = new Cons(p, result);
        return result;
    }

    public static LispObject ListAllPackagesV(LispObject[] args)
    {
        if (args.Length != 0)
            throw new LispErrorException(new LispProgramError(
                $"LIST-ALL-PACKAGES: expected 0 arguments, got {args.Length}"));
        return ListAllPackages();
    }

    private static void ValidatePackageDesignator(LispObject obj, string fname)
    {
        if (obj is Package || obj is LispString || obj is Symbol || obj is LispChar
            || obj is Nil || obj is T
            || (obj is LispVector v && v.IsCharVector))
            return;
        throw new LispErrorException(new LispTypeError(
            $"{fname}: {obj} is not a valid package designator",
            obj,
            new Cons(Startup.Sym("OR"),
                new Cons(Startup.Sym("PACKAGE"),
                new Cons(Startup.Sym("STRING"),
                new Cons(Startup.Sym("SYMBOL"),
                new Cons(Startup.Sym("CHARACTER"), Nil.Instance)))))));
    }

    private static LispErrorException MakeProgramError(string fname, int min, int max, int actual)
    {
        return new LispErrorException(new LispProgramError(
            $"{fname}: expected {min}-{max} arguments, got {actual}"));
    }

    internal static void RegisterPackageBuiltins()
    {
        Startup.RegisterUnary("%PACKAGE-ALL-SYMBOLS", Runtime.PackageAllSymbolsList);
        Startup.RegisterUnary("%PACKAGE-EXTERNAL-SYMBOLS", Runtime.PackageExternalSymbolsList);
        Startup.RegisterBinary("%COLLECT-PACKAGE-ITERATOR-ENTRIES", Runtime.CollectPackageIteratorEntries);
    }

}
