namespace DotCL;

public static partial class Runtime
{
    // --- Pathname operations ---

    public static LispObject MakePathname(LispObject[] args)
    {
        if (args.Length % 2 != 0)
            throw new LispErrorException(new LispProgramError("MAKE-PATHNAME: odd number of keyword arguments"));

        bool allowOtherKeys = false;
        bool caseCommon = false;
        for (int j = 0; j < args.Length; j += 2)
        {
            if (args[j] is Symbol s && s.Name == "ALLOW-OTHER-KEYS" && IsTruthy(args[j + 1]))
                allowOtherKeys = true;
            else if (args[j] is Symbol c && c.Name == "CASE"
                     && args[j + 1] is Symbol cs && cs.Name == "COMMON")
                caseCommon = true;
        }
        if (!allowOtherKeys)
        {
            for (int j = 0; j < args.Length; j += 2)
            {
                if (args[j] is not Symbol key || !IsMakePathnameKeyword(key.Name))
                    throw new LispErrorException(new LispProgramError(
                        $"MAKE-PATHNAME: unknown keyword argument {args[j]}"));
            }
        }

        LispObject? host = null, device = null, name = null, type = null, version = null;
        LispObject? directory = null;
        LispPathname? defaults = null;
        bool hostSet = false, deviceSet = false, nameSet = false, typeSet = false,
             directorySet = false, versionSet = false, defaultsSet = false;

        // Helper: convert a value to the appropriate LispObject for a pathname component.
        // :WILD keyword -> stored as-is; string -> LispString; NIL -> null; otherwise stored as-is.
        static LispObject? ToComponent(LispObject val)
        {
            if (val is Nil) return null;
            if (val is LispVector v && v.IsCharVector && v.Rank == 1) return new LispString(LispPathname.QuoteLiteral(v.ToCharString()));
            // A string given to make-pathname is literal: * ? [ in it are quoted, so
            // (make-pathname :name "a*b") names a file called a*b and is not wild (use :wild).
            if (val is LispString lsv)
            {
                var quoted = LispPathname.QuoteLiteral(lsv.Value);
                return quoted == lsv.Value ? lsv : new LispString(quoted);
            }
            return val;
        }

        LispObject? Component(LispObject val)
        {
            var c = ToComponent(val);
            return caseCommon ? DiddlePathnameCase(c) : c;
        }

        LispObject DirectoryComponents(LispObject dir)
        {
            if (dir is not Cons) return dir;
            var items = new List<LispObject>();
            for (var c = dir; c is Cons cell; c = cell.Cdr)
                items.Add(Component(cell.Car) ?? Nil.Instance);
            return Runtime.List(items.ToArray());
        }

        // A component is a string, :WILD, :UNSPECIFIC, NIL or a pattern; a number or a
        // character is none of those (the version alone may be an integer).
        static void CheckComponent(string what, LispObject val, bool integerOk = false)
        {
            if (val is LispChar || (val is Number && !(integerOk && val is Fixnum or Bignum)))
                throw new LispErrorException(new LispTypeError(
                    $"MAKE-PATHNAME: invalid {what}: {val}", val, Startup.Sym("STRING")));
        }
        for (int j = 0; j < args.Length - 1; j += 2)
        {
            string key = args[j] switch { Symbol s => s.Name, _ => args[j].ToString() };
            switch (key)
            {
                case "HOST": case "DEVICE": case "NAME": case "TYPE": case "DIRECTORY":
                    CheckComponent(key.ToLowerInvariant(), args[j + 1]);
                    break;
                case "VERSION":
                    CheckComponent("version", args[j + 1], integerOk: true);
                    break;
            }
            switch (key)
            {
                case "HOST":
                    if (!hostSet) { host = ToComponent(args[j + 1]); hostSet = true; }
                    break;
                case "DEVICE":
                    if (!deviceSet) { device = Component(args[j + 1]); deviceSet = true; }
                    break;
                case "NAME":
                    if (!nameSet) { name = Component(args[j + 1]); nameSet = true; }
                    break;
                case "TYPE":
                    if (!typeSet) { type = Component(args[j + 1]); typeSet = true; }
                    break;
                case "VERSION":
                    if (!versionSet) { version = Component(args[j + 1]); versionSet = true; }
                    break;
                case "DIRECTORY":
                    if (directorySet) break;
                    directorySet = true;
                    var dirVal = args[j + 1];
                    if (dirVal is Nil) directory = null;
                    else if (dirVal is Symbol dirSym && dirSym.Name == "WILD")
                        directory = Runtime.List(Startup.Keyword("ABSOLUTE"), Startup.Keyword("WILD-INFERIORS"));
                    // CLHS 19.2.2.4.3: a string is (:ABSOLUTE string).
                    else if (dirVal is LispString
                             || (dirVal is LispVector dirVec && dirVec.IsCharVector && dirVec.Rank == 1))
                        directory = Runtime.List(Startup.Keyword("ABSOLUTE"), Component(dirVal)!);
                    else directory = DirectoryComponents(dirVal);
                    break;
                case "DEFAULTS":
                    if (defaultsSet) break;
                    defaultsSet = true;
                    var dval = args[j + 1];
                    if (dval is LispPathname dp) defaults = dp;
                    else if (dval is LispString dstr) defaults = FromNamestring(dstr.Value);
                    else if (dval is LispVector dvec && dvec.IsCharVector && dvec.Rank == 1) defaults = FromNamestring(dvec.ToCharString());
                    break;
            }
        }

        // Fill unspecified components from defaults
        if (defaults != null)
        {
            if (!hostSet) host = defaults.Host;
            if (!deviceSet) device = defaults.Device;
            if (!directorySet) directory = defaults.DirectoryComponent;
            if (!nameSet) name = defaults.NameComponent;
            if (!typeSet) type = defaults.TypeComponent;
            if (!versionSet) version = defaults.Version;
        }

        // No :VERSION and no :DEFAULTS leaves the version NIL. The default
        // :DEFAULTS is a pathname whose components other than the host are all
        // NIL (CLHS MAKE-PATHNAME), so (make-pathname) has version NIL, as
        // #P"" does. :NEWEST is what MERGE-PATHNAMES fills in later, not what
        // MAKE-PATHNAME makes.

        // CLHS: if host is a known logical host, return a logical pathname
        bool isLogical = false;
        if (host is LispString hs && _logicalPathnameTranslations.ContainsKey(hs.Value.ToUpperInvariant()))
            isLogical = true;
        else if (host is Symbol hsym && hsym.Name != null && _logicalPathnameTranslations.ContainsKey(hsym.Name.ToUpperInvariant()))
            isLogical = true;
        else if (defaults is LispLogicalPathname)
            isLogical = true;

        if (isLogical)
            return new LispLogicalPathname(
                UppercaseLogicalComponent(host), UppercaseLogicalComponent(device),
                UppercaseLogicalComponent(directory), UppercaseLogicalComponent(name),
                UppercaseLogicalComponent(type), UppercaseLogicalComponent(version));

        return new LispPathname(host, device, directory, name, type, version);
    }

    private static bool IsMakePathnameKeyword(string name)
        => name is "HOST" or "DEVICE" or "NAME" or "TYPE" or "VERSION"
            or "DIRECTORY" or "DEFAULTS" or "CASE" or "ALLOW-OTHER-KEYS";

    private static LispObject? DiddlePathnameCase(LispObject? component)
    {
        if (component is LispString s)
        {
            bool upper = false, lower = false;
            foreach (var c in s.Value)
            {
                if (char.IsUpper(c)) upper = true;
                else if (char.IsLower(c)) lower = true;
            }
            if (upper && !lower) return new LispString(s.Value.ToLowerInvariant());
            if (lower && !upper) return new LispString(s.Value.ToUpperInvariant());
            return component;
        }
        if (component is Cons cons)
        {
            var items = new List<LispObject>();
            for (LispObject cur = cons; cur is Cons cell; cur = cell.Cdr)
                items.Add(DiddlePathnameCase(cell.Car) ?? Nil.Instance);
            return Runtime.List(items.ToArray());
        }
        return component;
    }

    private static LispObject? UppercaseLogicalComponent(LispObject? component)
    {
        if (component is LispString s) return new LispString(s.Value.ToUpperInvariant());
        if (component is Cons cons)
        {
            var items = new List<LispObject>();
            for (LispObject cur = cons; cur is Cons cell; cur = cell.Cdr)
                items.Add(UppercaseLogicalComponent(cell.Car) ?? Nil.Instance);
            return Runtime.List(items.ToArray());
        }
        return component;
    }

    /// <summary>
    /// Construct a pathname from 6 positional args: host, device, directory, name, type, version.
    /// Used by the compiler to serialize pathname literals with all components preserved.
    /// </summary>
    public static LispObject MakePathnameFromParts(LispObject[] args)
    {
        static LispObject? ToComp(LispObject v) => v is Nil ? null : v;
        return new LispPathname(
            ToComp(args[0]), ToComp(args[1]), ToComp(args[2]),
            ToComp(args[3]), ToComp(args[4]), ToComp(args[5]));
    }

    public static LispObject PathnameMatchP(LispObject path, LispObject wildcard)
    {
        LispPathname p, w;
        if (path is LispPathname lp) p = lp;
        else if (path is LispString ps) p = FromNamestring(ps.Value);
        else if (path is LispVector pv && pv.IsCharVector) p = FromNamestring(pv.ToCharString());
        else if (path is LispFileStream pfs) p = LispPathname.FromString(pfs.FilePath);
        else throw new LispErrorException(new LispTypeError("PATHNAME-MATCH-P: not a pathname designator", path));

        if (wildcard is LispPathname wp) w = wp;
        else if (wildcard is LispString ws) w = FromNamestring(ws.Value);
        else if (wildcard is LispVector wv && wv.IsCharVector) w = FromNamestring(wv.ToCharString());
        else if (wildcard is LispFileStream wfs) w = LispPathname.FromString(wfs.FilePath);
        else throw new LispErrorException(new LispTypeError("PATHNAME-MATCH-P: not a pathname designator", wildcard));

        // Check directory match
        if (!DirectoryMatchP(p.DirectoryComponent, w.DirectoryComponent))
            return Nil.Instance;

        // Check name match
        if (!ComponentMatchP(p.NameComponent, w.NameComponent))
            return Nil.Instance;

        // Check type match
        if (!ComponentMatchP(p.TypeComponent, w.TypeComponent))
            return Nil.Instance;

        return T.Instance;
    }

    // How pathname pieces compare. Windows file names do not differ by case, so
    // there they are compared without regard to it; on Unix-like systems the case
    // is significant, as in SBCL and ECL.
    private static readonly StringComparison PathCase =
        Compat.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;

    private static bool HasGlob(string s) => LispPathname.HasWildChar(s);

    /// <summary>Whether the pattern, which may hold * (any run of characters) and
    /// ? (one character), matches the whole of the text. The text each * matched is
    /// added to stars, in order.</summary>
    private static bool GlobMatch(string pat, string text, List<string>? stars = null)
    {
        var found = new List<string>();
        bool Rec(int pi, int ti, int mark)
        {
            while (pi < pat.Length)
            {
                char c = pat[pi];
                // A quoted * ? [ is the character itself.
                if (c == '\\' && !Compat.IsWindows() && pi + 1 < pat.Length && LispPathname.IsQuotable(pat[pi + 1]))
                {
                    if (ti >= text.Length || string.Compare(pat, pi + 1, text, ti, 1, PathCase) != 0) return false;
                    pi += 2; ti++;
                    continue;
                }
                if (c == '*')
                {
                    for (int end = text.Length; end >= ti; end--)
                    {
                        found.Add(text.Substring(ti, end - ti));
                        if (Rec(pi + 1, end, found.Count)) return true;
                        found.RemoveRange(found.Count - 1, 1);
                    }
                    return false;
                }
                if (ti >= text.Length) return false;
                if (c != '?' && string.Compare(pat, pi, text, ti, 1, PathCase) != 0) return false;
                pi++; ti++;
            }
            return ti >= text.Length;
        }
        if (!Rec(0, 0, 0)) return false;
        stars?.AddRange(found);
        return true;
    }

    /// <summary>A pathname piece against a pattern piece: a plain string must be the
    /// same, a string with * or ? is a pattern.</summary>
    private static bool PieceMatches(string pattern, string value, List<string>? stars = null)
        => HasGlob(pattern) ? GlobMatch(pattern, value, stars)
                            : string.Equals(LispPathname.UnquoteForFileSystem(value),
                                            LispPathname.UnquoteForFileSystem(pattern), PathCase);

    private static bool ComponentMatchP(LispObject? value, LispObject? pattern)
    {
        if (pattern == null) return true; // nil pattern matches anything
        if (pattern is Symbol ps && ps.Name == "WILD") return true; // :wild matches anything
        if (pattern is LispString pstr && pstr.Value == "*") return true; // "*" matches anything
        if (value == null) return pattern == null;
        if (value is LispString vs && pattern is LispString pts)
            return PieceMatches(pts.Value, vs.Value);
        return false;
    }

    private static bool DirectoryMatchP(LispObject? pathDir, LispObject? wildDir)
    {
        if (wildDir == null || wildDir is Nil) return true;
        if (pathDir == null || pathDir is Nil)
        {
            // A nil path directory can match a pattern that only contains ** (zero dirs allowed)
            if (wildDir is Cons wc)
            {
                // Check: (:absolute :wild-inferiors) or (:relative :wild-inferiors); ** matches zero dirs
                var wFirst = wc.Car;
                bool isAbsOrRel = wFirst is Symbol ws0 && (ws0.Name == "ABSOLUTE" || ws0.Name == "RELATIVE");
                if (isAbsOrRel && wc.Cdr is Cons wRest
                    && wRest.Car is Symbol wi0 && wi0.Name == "WILD-INFERIORS"
                    && wRest.Cdr is Nil)
                    return true;
            }
            return wildDir == null || wildDir is Nil;
        }

        var pList = new List<LispObject>();
        var wList = new List<LispObject>();
        var cur = pathDir;
        while (cur is Cons c) { pList.Add(c.Car); cur = c.Cdr; }
        cur = wildDir;
        while (cur is Cons c) { wList.Add(c.Car); cur = c.Cdr; }

        // Both must start with same type (:absolute/:relative)
        if (pList.Count == 0 || wList.Count == 0) return true;
        if (pList[0] is Symbol ps && wList[0] is Symbol ws && ps.Name != ws.Name)
            return false;

        return DirectoryListMatch(pList, 1, wList, 1);
    }

    /// <summary>The directory components P[PI..] against the pattern W[WI..], where :WILD-INFERIORS
    /// (**) matches any number of components, including none, and there may be several of them.</summary>
    private static bool DirectoryListMatch(List<LispObject> pList, int pi, List<LispObject> wList, int wi)
    {
        while (wi < wList.Count)
        {
            if (wList[wi] is Symbol wsym && wsym.Name == "WILD-INFERIORS")
            {
                wi++;
                if (wi >= wList.Count) return true;
                for (int k = pi; k <= pList.Count; k++)
                    if (DirectoryListMatch(pList, k, wList, wi)) return true;
                return false;
            }
            if (pi >= pList.Count) return false;
            if (wList[wi] is Symbol wsym2 && wsym2.Name == "WILD")
            {
                pi++; wi++; continue; // :wild matches any single component
            }
            if (pList[pi] is LispString ps2 && wList[wi] is LispString ws2)
            {
                if (!PieceMatches(ws2.Value, ps2.Value))
                    return false;
                pi++; wi++; continue;
            }
            // :UP and :BACK (the .. component) match themselves.
            if (pList[pi] is Symbol pup && wList[wi] is Symbol wup && pup.Name == wup.Name)
            {
                pi++; wi++; continue;
            }
            return false;
        }
        return pi >= pList.Count;
    }

    private static bool DirectoryComponentMatch(List<LispObject> pList, int pi, List<LispObject> wList, int wi)
    {
        while (wi < wList.Count && pi < pList.Count)
        {
            if (wList[wi] is Symbol ws && ws.Name == "WILD") { pi++; wi++; continue; }
            if (pList[pi] is LispString ps && wList[wi] is LispString ws2)
            {
                if (!PieceMatches(ws2.Value, ps.Value))
                    return false;
                pi++; wi++; continue;
            }
            return false;
        }
        return wi >= wList.Count && pi >= pList.Count;
    }

    public static LispObject TranslatePathname(LispObject source, LispObject fromWild, LispObject toWild)
    {
        LispPathname s, f, t;
        static LispPathname ToPn(LispObject obj, string who) => obj switch {
            LispPathname p => p,
            LispString s => FromNamestring(s.Value),
            LispVector v when v.IsCharVector => FromNamestring(v.ToCharString()),
            LispFileStream fs => LispPathname.FromString(fs.FilePath),
            _ => throw new LispErrorException(new LispTypeError($"{who}: not a pathname designator", obj))
        };
        s = ToPn(source, "TRANSLATE-PATHNAME");
        f = ToPn(fromWild, "TRANSLATE-PATHNAME");
        t = ToPn(toWild, "TRANSLATE-PATHNAME");

        if (PathnameMatchP(s, f) is not T)
            throw new LispErrorException(new LispError(
                $"TRANSLATE-PATHNAME: source {s} does not match from-wildcard {f}")
                { ConditionTypeName = "SIMPLE-ERROR" });

        // Translate directory: replace the from-wild prefix with to-wild prefix,
        // keeping the subdirectories that matched :wild-inferiors
        var newDir = TranslateDirectory(s.DirectoryComponent, f.DirectoryComponent, t.DirectoryComponent);
        // Translate name: if to is :wild, use source's name; otherwise use to's name
        var newName = TranslateComponent(s.NameComponent, f.NameComponent, t.NameComponent);
        var newType = TranslateComponent(s.TypeComponent, f.TypeComponent, t.TypeComponent);

        var newVersion = TranslateComponent(s.Version, f.Version, t.Version);

        if (t is LispLogicalPathname)
            return new LispLogicalPathname(
                UppercaseLogicalComponent(t.Host ?? s.Host),
                UppercaseLogicalComponent(t.Device ?? s.Device),
                UppercaseLogicalComponent(newDir),
                UppercaseLogicalComponent(newName),
                UppercaseLogicalComponent(newType),
                UppercaseLogicalComponent(newVersion));

        // When source is a logical pathname, lowercase string components in physical result
        bool srcIsLogical = s is LispLogicalPathname;
        if (srcIsLogical)
        {
            if (newName is LispString ns && t.NameComponent is Symbol wn && wn.Name == "WILD")
                newName = new LispString(ns.Value.ToLowerInvariant());
            if (newType is LispString nt && t.TypeComponent is Symbol wt && wt.Name == "WILD")
                newType = new LispString(nt.Value.ToLowerInvariant());
            newDir = LowercaseDirectoryStrings(newDir);
        }

        return new LispPathname(
            t.Host ?? (s is LispLogicalPathname ? null : s.Host),
            t.Device ?? s.Device,
            newDir, newName, newType,
            newVersion);
    }

    private static LispObject? TranslateComponent(LispObject? src, LispObject? from, LispObject? to)
    {
        if (to == null) return src; // nil in dest -> use source
        if (to is Symbol ts && ts.Name == "WILD") return src; // :wild in dest -> use source
        if (to is LispString tstr && tstr.Value == "*") return src; // "*" in dest -> use source
        if (to is LispString pat && pat.Value.IndexOf('*') >= 0 && src is LispString srcStr)
        {
            // Each * of the to-piece takes what the next wild part of the from-piece matched.
            var pieces = new List<string>();
            if (from is Symbol fw && fw.Name == "WILD") pieces.Add(srcStr.Value);
            else if (from is LispString fpat && HasGlob(fpat.Value)) GlobMatch(fpat.Value, srcStr.Value, pieces);
            return new LispString(FillStars(pat.Value, pieces));
        }
        return to;
    }

    /// <summary>The pattern with each * replaced by the next of pieces; a * with no
    /// piece left stays.</summary>
    private static string FillStars(string pattern, List<string> pieces)
    {
        var sb = new System.Text.StringBuilder();
        int next = 0;
        foreach (char c in pattern)
        {
            if (c == '*' && next < pieces.Count) sb.Append(pieces[next++]);
            else sb.Append(c);
        }
        return sb.ToString();
    }

    private static LispObject? LowercaseDirectoryStrings(LispObject? dir)
    {
        if (dir is not Cons) return dir;
        var items = new List<LispObject>();
        var cur = (LispObject)dir;
        while (cur is Cons c)
        {
            items.Add(c.Car is LispString ds ? new LispString(ds.Value.ToLowerInvariant()) : c.Car);
            cur = c.Cdr;
        }
        return List(items.ToArray());
    }

    private static LispObject? TranslateDirectory(LispObject? srcDir, LispObject? fromDir, LispObject? toDir)
    {
        if (toDir == null || toDir is Nil) return srcDir;

        var sList = new List<LispObject>();
        var fList = new List<LispObject>();
        var tList = new List<LispObject>();
        var cur = srcDir;
        while (cur is Cons c) { sList.Add(c.Car); cur = c.Cdr; }
        cur = fromDir;
        while (cur is Cons c) { fList.Add(c.Car); cur = c.Cdr; }
        cur = toDir;
        while (cur is Cons c) { tList.Add(c.Car); cur = c.Cdr; }

        // What the wild pieces of the from-directory matched in the source: a name
        // for each :wild, and a list of names for each :wild-inferiors, in order.
        var wildPieces = new List<LispObject>();
        var inferiorPieces = new List<List<LispObject>>();
        if (sList.Count > 0 && fList.Count > 0)
            CaptureDirectory(fList, 1, sList, 1, wildPieces, inferiorPieces);

        // Build result: each wild piece of the to-directory takes the next piece the
        // from-directory captured, as a :wild or :wild-inferiors of the from does.
        var result = new List<LispObject>();
        int nextWild = 0, nextInferior = 0;
        for (int ti = 0; ti < tList.Count; ti++)
        {
            if (tList[ti] is Symbol ts && ts.Name == "WILD-INFERIORS")
            {
                if (nextInferior < inferiorPieces.Count)
                    result.AddRange(inferiorPieces[nextInferior++]);
            }
            else if (tList[ti] is Symbol tw && tw.Name == "WILD" && nextWild < wildPieces.Count)
            {
                result.Add(wildPieces[nextWild++]);
            }
            else if (tList[ti] is LispString tstr && tstr.Value.IndexOf('*') >= 0)
            {
                var pieces = new List<string>();
                while (nextWild < wildPieces.Count && pieces.Count < tstr.Value.Count(ch => ch == '*'))
                    pieces.Add(wildPieces[nextWild++] is LispString w ? w.Value : "");
                result.Add(new LispString(FillStars(tstr.Value, pieces)));
            }
            else
            {
                result.Add(tList[ti]);
            }
        }

        return result.Count > 0 ? List(result.ToArray()) : null;
    }

    /// <summary>Align the from-directory with the source directory from the given
    /// positions and record what each wild piece matched. :wild-inferiors takes the
    /// shortest run that lets the rest match. False when they do not align.</summary>
    private static bool CaptureDirectory(List<LispObject> f, int fi, List<LispObject> s, int si,
        List<LispObject> wild, List<List<LispObject>> inferiors)
    {
        int wildMark = wild.Count, infMark = inferiors.Count;
        while (fi < f.Count)
        {
            if (f[fi] is Symbol fs && fs.Name == "WILD-INFERIORS")
            {
                for (int take = 0; si + take <= s.Count; take++)
                {
                    inferiors.Add(s.GetRange(si, take));
                    if (CaptureDirectory(f, fi + 1, s, si + take, wild, inferiors)) return true;
                    inferiors.RemoveRange(inferiors.Count - 1, 1);
                    // The recursive call restored the lists it added to.
                }
                wild.RemoveRange(wildMark, wild.Count - wildMark);
                inferiors.RemoveRange(infMark, inferiors.Count - infMark);
                return false;
            }
            if (si >= s.Count) break;
            if (f[fi] is Symbol fw && fw.Name == "WILD")
                wild.Add(s[si]);
            else if (f[fi] is LispString fstr && s[si] is LispString sstr)
            {
                var stars = new List<string>();
                if (!PieceMatches(fstr.Value, sstr.Value, stars)) break;
                foreach (var star in stars) wild.Add(new LispString(star));
            }
            else
                break;
            fi++; si++;
        }
        if (fi >= f.Count && si >= s.Count) return true;
        wild.RemoveRange(wildMark, wild.Count - wildMark);
        inferiors.RemoveRange(infMark, inferiors.Count - infMark);
        return false;
    }

    public static LispObject CompileFilePathname(LispObject[] args)
    {
        // (compile-file-pathname input-file &key output-file &allow-other-keys)
        var input = args[0];
        LispPathname p = input switch {
            LispPathname lp => lp,
            LispString s => FromNamestring(s.Value),
            LispFileStream fs => fs.OriginalPathname ?? LispPathname.FromString(fs.FilePath),
            _ => throw new LispErrorException(new LispTypeError("COMPILE-FILE-PATHNAME: not a pathname designator", input))
        };
        // Check for :output-file keyword
        for (int i = 1; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "OUTPUT-FILE" && !(args[i + 1] is Nil))
            {
                // A name with no type gets the fasl type (SBCL merges the default type in).
                var outp = Pathname(args[i + 1]);
                if (outp is LispPathname op && op.NameComponent != null && op.TypeComponent == null)
                    return op is LispLogicalPathname
                        ? new LispLogicalPathname(op.Host, op.Device, op.DirectoryComponent, op.NameComponent, new LispString("fasl"), op.Version)
                        : new LispPathname(op.Host, op.Device, op.DirectoryComponent, op.NameComponent, new LispString("fasl"), op.Version);
                return outp;
            }
        }
        // Default: always .fasl (content may be PE assembly or text SIL depending on :fasl flag).
        // Preserve logical-ness: when the input is a logical pathname, the result must
        // remain a LispLogicalPathname so downstream ResolvePhysicalPath translates it
        // through its host's translations. Returning a plain LispPathname here merged the
        // logical host into the namestring (e.g. "//CLTESTCOMPILE-FILE-TEST-LP.fasl",
        // which on Windows became "\CLTESTCOMPILE-FILE-TEST-LP.fasl"): ANSI COMPILE-FILE.17.
        var fasl = new LispString("fasl");
        return p is LispLogicalPathname
            ? new LispLogicalPathname(p.Host, p.Device, p.DirectoryComponent, p.NameComponent, fasl, p.Version)
            : new LispPathname(p.Host, p.Device, p.DirectoryComponent, p.NameComponent, fasl, p.Version);
    }

    /// <summary>Parse a namestring the way PATHNAME does: a host prefix that names an
    /// already defined logical host yields a logical pathname, anything else a physical
    /// one. A single character before the colon is always a drive letter, never a host.
    /// </summary>
    internal static LispPathname FromNamestring(string str)
    {
        var p = IsLogicalPathnameString(str)
            ? LispLogicalPathname.FromLogicalString(str)
            : LispPathname.FromString(str);
        // A named physical namestring has no version field, so its version is :NEWEST.
        if (p is not LispLogicalPathname && p.Version == null
            && p.NameComponent != null && p.NameComponent is not Nil)
            return new LispPathname(p.Host, p.Device, p.DirectoryComponent,
                                    p.NameComponent, p.TypeComponent,
                                    Startup.Keyword("NEWEST"));
        return p;
    }

    /// <summary>Coerce a pathname designator to a pathname. CLHS specifies the pathname
    /// accessors and MERGE-PATHNAMES in terms of PATHNAME, so a string designator has to
    /// be parsed with the same logical-host rule everywhere: otherwise "HOST:NAME" reads
    /// as a logical pathname through PATHNAME but as a file named "HOST:NAME" through
    /// PATHNAME-NAME or MERGE-PATHNAMES.</summary>
    internal static LispPathname ToPathnameDesignator(LispObject path, string who) => path switch
    {
        LispPathname p => p,
        LispString s => FromNamestring(s.Value),
        LispVector v when v.IsCharVector => FromNamestring(v.ToCharString()),
        // A stream is coerced through its truename, as it was before: the string here is
        // an already resolved physical path, not a namestring the user wrote.
        LispFileStream fs => LispPathname.FromString(fs.FilePath),
        _ => throw new LispErrorException(new LispTypeError($"{who}: not a pathname designator", path))
    };

    public static LispObject Pathname(LispObject thing)
    {
        if (thing is LispPathname) return thing;
        if (thing is LispString s) return FromNamestring(s.Value);
        if (thing is LispFileStream fs) return fs.OriginalPathname ?? LispPathname.FromString(fs.FilePath);
        if (thing is LispVector v && v.IsCharVector) return FromNamestring(v.ToCharString());
        throw new LispErrorException(new LispTypeError("PATHNAME: cannot convert to pathname", thing));
    }

    public static LispObject Namestring(LispObject path)
    {
        if (path is LispPathname p) return new LispString(p.ToNamestring());
        if (path is LispString) return path;
        if (path is LispFileStream fs) return new LispString(fs.FilePath);
        if (path is LispVector v && v.IsCharVector) return new LispString(v.ToCharString());
        throw new LispErrorException(new LispTypeError("NAMESTRING: not a pathname", path));
    }

    /// <summary>A pathname piece as PATHNAME-NAME and friends answer it: the text without the
    /// quoting backslashes ("a\\*b" is the name a*b; the pathname holds the quoted form).</summary>
    private static LispObject UnquotedPiece(LispObject? piece)
        => piece switch
        {
            null => Nil.Instance,
            LispString ls when ls.Value.IndexOf('\\') >= 0 =>
                LispPathname.UnquoteForFileSystem(ls.Value) is var u && u != ls.Value ? new LispString(u) : ls,
            _ => piece
        };

    public static LispObject PathnameDirectory(LispObject path)
    {
        var dir = ToPathnameDesignator(path, "PATHNAME-DIRECTORY").DirectoryComponent;
        if (dir is not Cons) return dir ?? (LispObject)Nil.Instance;
        var parts = new List<LispObject>();
        bool changed = false;
        for (var cur = dir; cur is Cons c; cur = c.Cdr)
        {
            var piece = UnquotedPiece(c.Car);
            if (!ReferenceEquals(piece, c.Car)) changed = true;
            parts.Add(piece);
        }
        return changed ? List(parts.ToArray()) : dir;
    }

    public static LispObject PathnameName(LispObject path)
        => UnquotedPiece(ToPathnameDesignator(path, "PATHNAME-NAME").NameComponent);

    public static LispObject PathnameType(LispObject path)
        => UnquotedPiece(ToPathnameDesignator(path, "PATHNAME-TYPE").TypeComponent);

    public static LispObject PathnameHost(LispObject path)
        => ToPathnameDesignator(path, "PATHNAME-HOST").Host ?? (LispObject)Nil.Instance;

    public static LispObject PathnameDevice(LispObject path)
    {
        var p = ToPathnameDesignator(path, "PATHNAME-DEVICE");
        if (p is LispLogicalPathname && (p.Device == null || p.Device is Nil))
            return Startup.Keyword("UNSPECIFIC");
        return p.Device ?? (LispObject)Nil.Instance;
    }

    public static LispObject PathnameVersion(LispObject path)
        => ToPathnameDesignator(path, "PATHNAME-VERSION").Version ?? (LispObject)Nil.Instance;

    // :NEWEST the keyword, not a symbol of that name in the current package: CLHS
    // gives MERGE-PATHNAMES the keyword as its default, and code that inspects a
    // version compares with EQ against :NEWEST.
    public static LispObject MergePathnames(LispObject path, LispObject defaults)
        => MergePathnames(path, defaults, Startup.Keyword("NEWEST"));

    public static LispObject MergePathnames(LispObject path, LispObject defaults, LispObject defaultVersion)
    {
        var p = ToPathnameDesignator(path, "MERGE-PATHNAMES");
        var d = ToPathnameDesignator(defaults, "MERGE-PATHNAMES");
        var merged = p.MergeWith(d);
        // CLHS MERGE-PATHNAMES: a version PATHNAME does not supply comes from
        // DEFAULT-PATHNAME only when the name came from there too; when
        // PATHNAME has its own name, it is DEFAULT-VERSION. MergeWith takes the
        // default's version unconditionally, so undo that for a named pathname.
        bool ownVersion = p.Version != null && p.Version is not Nil;
        bool ownName = p.NameComponent != null && p.NameComponent is not Nil;
        if (!ownVersion && ownName)
            return merged is LispLogicalPathname
                ? new LispLogicalPathname(merged.Host, merged.Device, merged.DirectoryComponent,
                    merged.NameComponent, merged.TypeComponent, defaultVersion)
                : new LispPathname(merged.Host, merged.Device, merged.DirectoryComponent,
                    merged.NameComponent, merged.TypeComponent, defaultVersion);
        // Otherwise, if the version is still nil after merge, apply default-version
        if (merged.Version == null || merged.Version is Nil)
            return merged is LispLogicalPathname
                ? new LispLogicalPathname(merged.Host, merged.Device, merged.DirectoryComponent,
                    merged.NameComponent, merged.TypeComponent, defaultVersion)
                : new LispPathname(merged.Host, merged.Device, merged.DirectoryComponent,
                    merged.NameComponent, merged.TypeComponent, defaultVersion);
        return merged;
    }

    private static LispObject ParseNamestringJunk(LispObject value, int offset, bool junkAllowed, string substr)
    {
        if (junkAllowed)
            return MultipleValues.Values2(value, Fixnum.Make(offset));
        throw new LispErrorException(new LispError($"PARSE-NAMESTRING: cannot parse \"{substr}\""));
    }

    private static int LogicalNamestringBadOffset(string s)
    {
        for (int i = 0; i < s.Length; i++)
        {
            char ch = s[i];
            bool wordChar = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
                            || (ch >= '0' && ch <= '9') || ch == '-';
            if (!wordChar && ch != '*' && ch != ';' && ch != ':' && ch != '.')
                return i;
        }
        return -1;
    }

    internal static void RegisterPathnameBuiltins()
    {
        // USER-HOMEDIR-PATHNAME
        Emitter.CilAssembler.RegisterFunction("USER-HOMEDIR-PATHNAME",
            new LispFunction(args =>
            {
                if (args.Length > 1) throw new LispErrorException(new LispProgramError("USER-HOMEDIR-PATHNAME: wrong number of arguments: " + args.Length + " (expected 0-1)"));
                string home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
                if (string.IsNullOrEmpty(home))
                    home = Environment.GetEnvironmentVariable("HOME") ?? "/";
                if (!home.EndsWith(Path.DirectorySeparatorChar))
                    home += Path.DirectorySeparatorChar;
                return LispPathname.FromString(home);
            }));

        Runtime.DefineSysLogicalHost();

        // LOGICAL-PATHNAME, LOGICAL-PATHNAME-TRANSLATIONS, TRANSLATE-LOGICAL-PATHNAME
        Emitter.CilAssembler.RegisterFunction("LOGICAL-PATHNAME",
            new LispFunction(args => Runtime.LogicalPathname(args[0])).WithArgCount(1, 1));
        Emitter.CilAssembler.RegisterFunction("LOGICAL-PATHNAME-TRANSLATIONS",
            new LispFunction(args => Runtime.LogicalPathnameTranslations(args[0])).WithArgCount(1, 1));
        Emitter.CilAssembler.RegisterFunction("TRANSLATE-LOGICAL-PATHNAME",
            new LispFunction(args => Runtime.TranslateLogicalPathname(args[0])).WithKeyArgs(1, 1, false));

        // MAKE-PATHNAME as callable function (also compiled inline by cil-compiler)
        Emitter.CilAssembler.RegisterFunction("MAKE-PATHNAME", new LispFunction(Runtime.MakePathname));

        // Pathname accessor functions: accept the :case keyword (ignored)
        Emitter.CilAssembler.RegisterFunction("PATHNAME-HOST",
            new LispFunction(args => Runtime.PathnameHost(args[0]), "PATHNAME-HOST", -1).WithKeyArgs(1, 1, false, "CASE"));
        Emitter.CilAssembler.RegisterFunction("PATHNAME-DEVICE",
            new LispFunction(args => Runtime.PathnameDevice(args[0]), "PATHNAME-DEVICE", -1).WithKeyArgs(1, 1, false, "CASE"));
        Emitter.CilAssembler.RegisterFunction("PATHNAME-NAME",
            new LispFunction(args => Runtime.PathnameName(args[0]), "PATHNAME-NAME", -1).WithKeyArgs(1, 1, false, "CASE"));
        Emitter.CilAssembler.RegisterFunction("PATHNAME-TYPE",
            new LispFunction(args => Runtime.PathnameType(args[0]), "PATHNAME-TYPE", -1).WithKeyArgs(1, 1, false, "CASE"));

        Emitter.CilAssembler.RegisterFunction("PATHNAME-VERSION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("PATHNAME-VERSION: expected exactly 1 argument"));
            return Runtime.PathnameVersion(args[0]);
        }, "PATHNAME-VERSION", -1));

        var pathnameDirFn = new LispFunction(args => Runtime.PathnameDirectory(args[0]), "PATHNAME-DIRECTORY", -1)
            .WithKeyArgs(1, 1, false, "CASE");
        // 1-arg direct delegate: same code path as the wrapper with one argument.
        pathnameDirFn.SetDirectDelegate((Func<LispObject, LispObject>)(a => Runtime.PathnameDirectory(a)));
        Emitter.CilAssembler.RegisterFunction("PATHNAME-DIRECTORY", pathnameDirFn);

        // LOAD-LOGICAL-PATHNAME-TRANSLATIONS
        Emitter.CilAssembler.RegisterFunction("LOAD-LOGICAL-PATHNAME-TRANSLATIONS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("LOAD-LOGICAL-PATHNAME-TRANSLATIONS: expected 1 argument"));
            var hostName = Runtime.StringValueOrNull(args[0]) ?? (args[0] is Symbol s ? s.Name : args[0].ToString());
            if (Runtime._logicalPathnameTranslations.ContainsKey(hostName.ToUpperInvariant()))
                return Nil.Instance;
            throw new LispErrorException(new LispError($"Cannot find logical pathname translations for host \"{hostName}\""));
        }));

        // PATHNAME and NAMESTRING as callable functions
        Emitter.CilAssembler.RegisterFunction("PATHNAME", new LispFunction(args => { Runtime.CheckArityMin("PATHNAME", args, 1); Runtime.CheckArityMax("PATHNAME", args, 1); return Runtime.Pathname(args[0]); }));
        Emitter.CilAssembler.RegisterFunction("NAMESTRING", new LispFunction(args => { Runtime.CheckArityMin("NAMESTRING", args, 1); Runtime.CheckArityMax("NAMESTRING", args, 1); return Runtime.Namestring(args[0]); }));

        // FILE-NAMESTRING: return name.type portion of namestring
        Emitter.CilAssembler.RegisterFunction("FILE-NAMESTRING", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"FILE-NAMESTRING: expected 1 argument, got {args.Length}"));
            var p = Startup.ToPathname(args[0], "FILE-NAMESTRING");
            var fileP = new LispPathname(null, null, null, p.NameComponent, p.TypeComponent, p.Version);
            return new LispString(fileP.ToNamestring());
        }));

        // DIRECTORY-NAMESTRING: return directory portion of namestring
        Emitter.CilAssembler.RegisterFunction("DIRECTORY-NAMESTRING", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"DIRECTORY-NAMESTRING: expected 1 argument, got {args.Length}"));
            var p = Startup.ToPathname(args[0], "DIRECTORY-NAMESTRING");
            var dirP = p is LispLogicalPathname
                ? (LispPathname)new LispLogicalPathname(null, null, p.DirectoryComponent, null, null, null)
                : new LispPathname(null, null, p.DirectoryComponent, null, null, null);
            return new LispString(dirP.ToNamestring());
        }));

        // HOST-NAMESTRING: return host portion of namestring, or NIL
        Emitter.CilAssembler.RegisterFunction("HOST-NAMESTRING", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"HOST-NAMESTRING: expected 1 argument, got {args.Length}"));
            var p = Startup.ToPathname(args[0], "HOST-NAMESTRING");
            return p.Host is LispString hs ? (LispObject)hs : Nil.Instance;
        }));

        // ENOUGH-NAMESTRING: shortest namestring that reproduces pathname when merged with defaults
        Emitter.CilAssembler.RegisterFunction("ENOUGH-NAMESTRING", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"ENOUGH-NAMESTRING: expected 1-2 arguments, got {args.Length}"));
            var p = Startup.ToPathname(args[0], "ENOUGH-NAMESTRING");
            var defaultsObj = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            var defaults = Startup.ToPathname(defaultsObj, "ENOUGH-NAMESTRING");

            bool sameHost = Startup.LispObjectEqual(p.Host, defaults.Host);
            if (!sameHost) return new LispString(p.ToNamestring());

            bool sameDevice = Startup.LispObjectEqual(p.Device, defaults.Device);
            if (!sameDevice) return new LispString(p.ToNamestring());

            bool sameDir = Startup.LispObjectEqual(p.DirectoryComponent, defaults.DirectoryComponent);
            if (sameDir) {
                // A relative directory is prepended again when merged, so it has to stay.
                bool relative = p.DirectoryComponent is Cons sameCons
                    && sameCons.Car is Symbol sameRoot && sameRoot.Name == "RELATIVE";
                if (!relative)
                {
                    var fileP = new LispPathname(null, null, null, p.NameComponent, p.TypeComponent, p.Version);
                    return new LispString(fileP.ToNamestring());
                }
            }

            if (p.DirectoryComponent is Cons pDir && defaults.DirectoryComponent is Cons dDir) {
                var pParts = Startup.ListToArray(pDir);
                var dParts = Startup.ListToArray(dDir);
                if (pParts.Length > 0 && dParts.Length > 0
                    && pParts[0] is Symbol ps && ps.Name == "ABSOLUTE"
                    && dParts[0] is Symbol ds && ds.Name == "ABSOLUTE"
                    && pParts.Length >= dParts.Length) {
                    bool prefixMatch = true;
                    for (int i = 1; i < dParts.Length; i++) {
                        if (!Startup.LispObjectEqual(pParts[i], dParts[i])) { prefixMatch = false; break; }
                    }
                    if (prefixMatch) {
                        var relParts = new List<LispObject>();
                        relParts.Add(Startup.Keyword("RELATIVE"));
                        for (int i = dParts.Length; i < pParts.Length; i++)
                            relParts.Add(pParts[i]);
                        var relDir = relParts.Count > 1 ? Runtime.List(relParts.ToArray()) : null;
                        var relP = new LispPathname(null, null, relDir, p.NameComponent, p.TypeComponent, p.Version);
                        return new LispString(relP.ToNamestring());
                    }
                }
            }

            return new LispString(p.ToNamestring());
        }));

        // MERGE-PATHNAMES as callable function (supports 1-3 args per CLHS)
        Emitter.CilAssembler.RegisterFunction("MERGE-PATHNAMES", new LispFunction(args => {
            var path = args[0];
            var defaults = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            var defaultVersion = args.Length > 2 ? args[2] : (LispObject)Startup.Keyword("NEWEST");
            // Hand the default-version to the merge rather than patching the result
            // afterwards. The two-argument overload fills the version slot itself, so
            // a caller-supplied default could never be applied on top of it: the
            // "still nil after merge" test was already false by the time it ran.
            return Runtime.MergePathnames(path, defaults, defaultVersion);
        }).WithArgCount(1, 3));

        // WILD-PATHNAME-P: check if pathname has wildcard components
        Emitter.CilAssembler.RegisterFunction("WILD-PATHNAME-P", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("WILD-PATHNAME-P: expected at least 1 argument"));
            if (args.Length > 2) throw new LispErrorException(new LispProgramError("WILD-PATHNAME-P: too many arguments"));
            var path = args[0];
            LispPathname p = Runtime.ToPathnameDesignator(path, "WILD-PATHNAME-P");

            Symbol? key = args.Length > 1 && args[1] is Symbol ks ? ks : null;

            bool IsWild(LispObject? component) {
                if (component is Symbol s && s.Name == "WILD") return true;
                if (component is LispString lstr && LispPathname.HasWildChar(lstr.Value)) return true;
                return false;
            }

            bool DirectoryIsWild(LispObject? dir) {
                if (IsWild(dir)) return true;
                if (dir is Cons) {
                    var cur = dir;
                    while (cur is Cons c) {
                        if (c.Car is Symbol s && (s.Name == "WILD-INFERIORS" || s.Name == "WILD")) return true;
                        if (c.Car is LispString lstr && LispPathname.HasWildChar(lstr.Value)) return true;
                        cur = c.Cdr;
                    }
                }
                return false;
            }

            if (key == null) {
                if (IsWild(p.Host) || IsWild(p.Device) || DirectoryIsWild(p.DirectoryComponent)
                    || IsWild(p.NameComponent) || IsWild(p.TypeComponent) || IsWild(p.Version))
                    return (LispObject)T.Instance;
                return Nil.Instance;
            }

            string keyName = key.Name;
            return keyName switch {
                "HOST" => IsWild(p.Host) ? (LispObject)T.Instance : Nil.Instance,
                "DEVICE" => IsWild(p.Device) ? (LispObject)T.Instance : Nil.Instance,
                "DIRECTORY" => DirectoryIsWild(p.DirectoryComponent) ? (LispObject)T.Instance : Nil.Instance,
                "NAME" => IsWild(p.NameComponent) ? (LispObject)T.Instance : Nil.Instance,
                "TYPE" => IsWild(p.TypeComponent) ? (LispObject)T.Instance : Nil.Instance,
                "VERSION" => IsWild(p.Version) ? (LispObject)T.Instance : Nil.Instance,
                _ => Nil.Instance
            };
        }));

        // PARSE-NAMESTRING
        Emitter.CilAssembler.RegisterFunction("PARSE-NAMESTRING",
            new LispFunction(args => {
                var thing = args[0];

                int start = 0;
                int? end = null;
                bool junkAllowed = false;
                bool startSeen = false, endSeen = false, junkSeen = false;
                for (int i = 3; i < args.Length - 1; i += 2)
                {
                    if (args[i] is Symbol kw)
                    {
                        if (!startSeen && kw.Name == "START" && args[i + 1] is Fixnum sf)
                        { start = Runtime.ClampIndex(sf.Value); startSeen = true; }
                        else if (!endSeen && kw.Name == "END" && args[i + 1] is Fixnum ef)
                        { end = Runtime.ClampIndex(ef.Value); endSeen = true; }
                        else if (!endSeen && kw.Name == "END" && args[i + 1] is Nil)
                        { end = null; endSeen = true; }
                        else if (!junkSeen && kw.Name == "JUNK-ALLOWED")
                        { junkAllowed = Runtime.IsTruthy(args[i + 1]); junkSeen = true; }
                    }
                }

                if (thing is LispPathname pn)
                    return MultipleValues.Values2(pn, Fixnum.Make(start));

                if (thing is LispFileStream fs2)
                    return MultipleValues.Values2(LispPathname.FromString(fs2.FilePath), Nil.Instance);

                string str;
                if (thing is LispString ls) str = ls.Value;
                else if (thing is LispVector vec && vec.IsCharVector) str = vec.ToCharString();
                else throw new LispErrorException(new LispTypeError("PARSE-NAMESTRING: not a string or pathname", thing));

                int strEnd = end ?? str.Length;

                string? logicalHost = null;
                bool explicitLogicalHost = false;
                var hostArg = args.Length > 1 ? args[1] : Nil.Instance;
                if (hostArg is LispString hs
                    && Runtime._logicalPathnameTranslations.ContainsKey(hs.Value.ToUpperInvariant()))
                    { logicalHost = hs.Value.ToUpperInvariant(); explicitLogicalHost = true; }
                else if (hostArg is Symbol hsym && hsym.Name != null
                         && Runtime._logicalPathnameTranslations.ContainsKey(hsym.Name.ToUpperInvariant()))
                    { logicalHost = hsym.Name.ToUpperInvariant(); explicitLogicalHost = true; }
                if (logicalHost == null && args.Length > 2)
                {
                    LispPathname? defaultsPn = args[2] switch
                    {
                        LispPathname dp => dp,
                        LispString ds when Runtime.IsLogicalPathnameString(ds.Value)
                            => LispLogicalPathname.FromLogicalString(ds.Value),
                        LispVector dv when dv.IsCharVector
                            && Runtime.IsLogicalPathnameString(dv.ToCharString())
                            => LispLogicalPathname.FromLogicalString(dv.ToCharString()),
                        _ => null
                    };
                    if (defaultsPn is LispLogicalPathname dlp && dlp.Host is LispString dh)
                        logicalHost = dh.Value;
                }

                var substr = str.Substring(start, strEnd - start);
                int position = substr.Length;
                bool knownLogicalHost = Runtime.IsLogicalPathnameString(substr);
                int badOffset = LogicalNamestringBadOffset(substr);
                bool logicalOk = badOffset < 0;
                try
                {
                    LispPathname result;
                    if (explicitLogicalHost)
                    {
                        if (!logicalOk) return ParseNamestringJunk(Nil.Instance, badOffset, junkAllowed, substr);
                        result = substr.Contains(':')
                            ? LispLogicalPathname.FromLogicalString(substr)
                            : LispLogicalPathname.FromLogicalString(logicalHost + ":" + substr);
                    }
                    else if (knownLogicalHost && logicalOk)
                        result = LispLogicalPathname.FromLogicalString(substr);
                    else if (logicalHost != null)
                    {
                        if (!logicalOk) return ParseNamestringJunk(Nil.Instance, badOffset, junkAllowed, substr);
                        result = substr.Contains(':')
                            ? LispLogicalPathname.FromLogicalString(substr)
                            : LispLogicalPathname.FromLogicalString(logicalHost + ":" + substr);
                    }
                    else if (knownLogicalHost)
                        result = LispPathname.FromString(substr);
                    else
                        result = Runtime.FromNamestring(substr);
                    return MultipleValues.Values2(result, Fixnum.Make(position));
                }
                catch
                {
                    return ParseNamestringJunk(Nil.Instance, position, junkAllowed, substr);
                }
            }).WithKeyArgs(1, 3, false, "START", "END", "JUNK-ALLOWED"));

        // PATHNAME-MATCH-P / TRANSLATE-PATHNAME
        Startup.RegisterBinary("PATHNAME-MATCH-P", Runtime.PathnameMatchP);
        Emitter.CilAssembler.RegisterFunction("TRANSLATE-PATHNAME",
            new LispFunction(args => Runtime.TranslatePathname(args[0], args[1], args[2])).WithKeyArgs(3, 3, false));

        // COMPILE-FILE-PATHNAME
        Emitter.CilAssembler.RegisterFunction("COMPILE-FILE-PATHNAME",
            new LispFunction(args => Runtime.CompileFilePathname(args)).WithKeyArgs(1, 1, true, "OUTPUT-FILE"));

        // %SET-LOGICAL-PATHNAME-TRANSLATIONS
        Emitter.CilAssembler.RegisterFunction("%SET-LOGICAL-PATHNAME-TRANSLATIONS", new LispFunction(args => {
            return Runtime.SetLogicalPathnameTranslations(args[0], args[1]);
        }));
    }

}
