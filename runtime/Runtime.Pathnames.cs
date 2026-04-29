namespace DotCL;

public static partial class Runtime
{
    // --- Pathname operations ---

    public static LispObject MakePathname(LispObject[] args)
    {
        LispObject? host = null, device = null, name = null, type = null, version = null;
        LispObject? directory = null;
        LispPathname? defaults = null;
        bool hostSet = false, deviceSet = false, nameSet = false, typeSet = false, directorySet = false, versionSet = false;

        // Helper: convert a value to the appropriate LispObject for a pathname component.
        // :WILD keyword → stored as-is; string → LispString; NIL → null; otherwise stored as-is.
        static LispObject? ToComponent(LispObject val)
        {
            if (val is Nil) return null;
            // Keep Symbol (e.g. :WILD, :NEWEST, :UNSPECIFIC) as-is
            if (val is Symbol) return val;
            // Keep LispString as-is
            if (val is LispString) return val;
            return val;
        }

        for (int j = 0; j < args.Length - 1; j += 2)
        {
            string key = args[j] switch { Symbol s => s.Name, _ => args[j].ToString() };
            switch (key)
            {
                case "HOST": host = ToComponent(args[j + 1]); hostSet = true; break;
                case "DEVICE": device = ToComponent(args[j + 1]); deviceSet = true; break;
                case "NAME":
                    nameSet = true;
                    name = ToComponent(args[j + 1]);
                    break;
                case "TYPE":
                    typeSet = true;
                    type = ToComponent(args[j + 1]);
                    break;
                case "VERSION":
                    versionSet = true;
                    version = ToComponent(args[j + 1]);
                    break;
                case "DIRECTORY":
                    directorySet = true;
                    var dirVal = args[j + 1];
                    if (dirVal is Nil) directory = null;
                    else if (dirVal is Symbol dirSym && dirSym.Name == "WILD")
                        directory = Runtime.List(Startup.Keyword("ABSOLUTE"), Startup.Keyword("WILD-INFERIORS"));
                    else directory = dirVal;
                    break;
                case "DEFAULTS":
                    var dval = args[j + 1];
                    if (dval is LispPathname dp) defaults = dp;
                    else if (dval is LispString dstr) defaults = LispPathname.FromString(dstr.Value);
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

        // CLHS: if version not supplied and host not supplied, default to :newest
        if (!versionSet && version == null && host == null && defaults == null)
            version = Startup.Keyword("NEWEST");

        // CLHS: if host is a known logical host, return a logical pathname
        bool isLogical = false;
        if (host is LispString hs && _logicalPathnameTranslations.ContainsKey(hs.Value.ToUpperInvariant()))
            isLogical = true;
        else if (host is Symbol hsym && hsym.Name != null && _logicalPathnameTranslations.ContainsKey(hsym.Name.ToUpperInvariant()))
            isLogical = true;
        else if (defaults is LispLogicalPathname)
            isLogical = true;

        if (isLogical)
            return new LispLogicalPathname(host, device, directory, name, type, version);

        return new LispPathname(host, device, directory, name, type, version);
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
        else if (path is LispString ps)
            p = IsLogicalPathnameString(ps.Value) ? LispLogicalPathname.FromLogicalString(ps.Value) : LispPathname.FromString(ps.Value);
        else if (path is LispVector pv && pv.IsCharVector)
        {
            var pvs = pv.ToCharString();
            p = IsLogicalPathnameString(pvs) ? LispLogicalPathname.FromLogicalString(pvs) : LispPathname.FromString(pvs);
        }
        else if (path is LispFileStream pfs) p = LispPathname.FromString(pfs.FilePath);
        else throw new LispErrorException(new LispTypeError("PATHNAME-MATCH-P: not a pathname designator", path));

        if (wildcard is LispPathname wp) w = wp;
        else if (wildcard is LispString ws)
            w = IsLogicalPathnameString(ws.Value) ? LispLogicalPathname.FromLogicalString(ws.Value) : LispPathname.FromString(ws.Value);
        else if (wildcard is LispVector wv && wv.IsCharVector)
        {
            var wvs = wv.ToCharString();
            w = IsLogicalPathnameString(wvs) ? LispLogicalPathname.FromLogicalString(wvs) : LispPathname.FromString(wvs);
        }
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

    private static bool ComponentMatchP(LispObject? value, LispObject? pattern)
    {
        if (pattern == null) return true; // nil pattern matches anything
        if (pattern is Symbol ps && ps.Name == "WILD") return true; // :wild matches anything
        if (pattern is LispString pstr && pstr.Value == "*") return true; // "*" matches anything
        if (value == null) return pattern == null;
        if (value is LispString vs && pattern is LispString pts)
            return string.Equals(vs.Value, pts.Value, StringComparison.OrdinalIgnoreCase);
        return false;
    }

    private static bool DirectoryMatchP(LispObject? pathDir, LispObject? wildDir)
    {
        if (wildDir == null || wildDir is Nil) return true;
        if (pathDir == null || pathDir is Nil) return wildDir == null || wildDir is Nil;

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

        int pi = 1, wi = 1;
        while (wi < wList.Count)
        {
            if (wList[wi] is Symbol wsym && wsym.Name == "WILD-INFERIORS")
            {
                // :wild-inferiors matches zero or more directory components
                wi++;
                if (wi >= wList.Count) return true; // rest of path matches
                // Find next wildcard component match
                while (pi < pList.Count)
                {
                    if (DirectoryComponentMatch(pList, pi, wList, wi))
                        return true;
                    pi++;
                }
                return pi >= pList.Count && wi >= wList.Count;
            }
            if (pi >= pList.Count) return false;
            if (wList[wi] is Symbol wsym2 && wsym2.Name == "WILD")
            {
                pi++; wi++; continue; // :wild matches any single component
            }
            if (pList[pi] is LispString ps2 && wList[wi] is LispString ws2)
            {
                if (!string.Equals(ps2.Value, ws2.Value, StringComparison.OrdinalIgnoreCase))
                    return false;
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
                if (!string.Equals(ps.Value, ws2.Value, StringComparison.OrdinalIgnoreCase))
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
        if (source is LispPathname sp) s = sp;
        else if (source is LispString ss) s = LispPathname.FromString(ss.Value);
        else if (source is LispVector sv && sv.IsCharVector) s = LispPathname.FromString(sv.ToCharString());
        else if (source is LispFileStream sfs) s = LispPathname.FromString(sfs.FilePath);
        else throw new LispErrorException(new LispTypeError("TRANSLATE-PATHNAME: not a pathname designator", source));

        if (fromWild is LispPathname fp) f = fp;
        else if (fromWild is LispString fs) f = LispPathname.FromString(fs.Value);
        else if (fromWild is LispVector fv && fv.IsCharVector) f = LispPathname.FromString(fv.ToCharString());
        else if (fromWild is LispFileStream ffs) f = LispPathname.FromString(ffs.FilePath);
        else throw new LispErrorException(new LispTypeError("TRANSLATE-PATHNAME: not a pathname designator", fromWild));

        if (toWild is LispPathname tp) t = tp;
        else if (toWild is LispString ts2) t = LispPathname.FromString(ts2.Value);
        else if (toWild is LispVector tv && tv.IsCharVector) t = LispPathname.FromString(tv.ToCharString());
        else if (toWild is LispFileStream tfs) t = LispPathname.FromString(tfs.FilePath);
        else throw new LispErrorException(new LispTypeError("TRANSLATE-PATHNAME: not a pathname designator", toWild));

        // Translate directory: replace the from-wild prefix with to-wild prefix,
        // keeping the subdirectories that matched :wild-inferiors
        var newDir = TranslateDirectory(s.DirectoryComponent, f.DirectoryComponent, t.DirectoryComponent);
        // Translate name: if to is :wild, use source's name; otherwise use to's name
        var newName = TranslateComponent(s.NameComponent, f.NameComponent, t.NameComponent);
        var newType = TranslateComponent(s.TypeComponent, f.TypeComponent, t.TypeComponent);

        var newVersion = TranslateComponent(s.Version, f.Version, t.Version);

        return new LispPathname(
            t.Host ?? s.Host,
            t.Device ?? s.Device,
            newDir, newName, newType,
            newVersion);
    }

    private static LispObject? TranslateComponent(LispObject? src, LispObject? from, LispObject? to)
    {
        if (to == null) return src; // nil in dest → use source
        if (to is Symbol ts && ts.Name == "WILD") return src; // :wild in dest → use source
        if (to is LispString tstr && tstr.Value == "*") return src; // "*" in dest → use source
        return to;
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

        // Find the parts of srcDir that are "captured" by :wild-inferiors in fromDir
        var captured = new List<LispObject>();
        int si = 1; // skip :absolute/:relative
        int fi = 1;
        while (fi < fList.Count && si < sList.Count)
        {
            if (fList[fi] is Symbol fs && fs.Name == "WILD-INFERIORS")
            {
                // Capture everything until we match the next from component
                fi++;
                if (fi >= fList.Count)
                {
                    // Rest of source is captured
                    while (si < sList.Count) { captured.Add(sList[si++]); }
                }
                else
                {
                    while (si < sList.Count)
                    {
                        if (fList[fi] is LispString fstr && sList[si] is LispString sstr
                            && string.Equals(fstr.Value, sstr.Value, StringComparison.OrdinalIgnoreCase))
                            break;
                        captured.Add(sList[si++]);
                    }
                }
                continue;
            }
            si++; fi++;
        }

        // Build result: to-directory with :wild-inferiors replaced by captured
        var result = new List<LispObject>();
        for (int ti = 0; ti < tList.Count; ti++)
        {
            if (tList[ti] is Symbol ts && ts.Name == "WILD-INFERIORS")
            {
                result.AddRange(captured);
            }
            else
            {
                result.Add(tList[ti]);
            }
        }

        return result.Count > 0 ? List(result.ToArray()) : null;
    }

    public static LispObject CompileFilePathname(LispObject[] args)
    {
        // (compile-file-pathname input-file &key output-file &allow-other-keys)
        var input = args[0];
        LispPathname p = input switch {
            LispPathname lp => lp,
            LispString s => LispPathname.FromString(s.Value),
            LispFileStream fs => fs.OriginalPathname ?? LispPathname.FromString(fs.FilePath),
            _ => throw new LispErrorException(new LispTypeError("COMPILE-FILE-PATHNAME: not a pathname designator", input))
        };
        // Check for :output-file keyword
        for (int i = 1; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "OUTPUT-FILE" && !(args[i + 1] is Nil))
            {
                return Pathname(args[i + 1]);
            }
        }
        // Default: always .fasl (content may be PE assembly or text SIL depending on :fasl flag)
        return new LispPathname(p.Host, p.Device, p.DirectoryComponent, p.NameComponent, new LispString("fasl"), p.Version);
    }

    public static LispObject Pathname(LispObject thing)
    {
        if (thing is LispPathname) return thing;
        if (thing is LispString s)
            return IsLogicalPathnameString(s.Value) ? LispLogicalPathname.FromLogicalString(s.Value) : LispPathname.FromString(s.Value);
        if (thing is LispFileStream fs) return fs.OriginalPathname ?? LispPathname.FromString(fs.FilePath);
        if (thing is LispVector v && v.IsCharVector)
        {
            var str = v.ToCharString();
            return IsLogicalPathnameString(str) ? LispLogicalPathname.FromLogicalString(str) : LispPathname.FromString(str);
        }
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

    public static LispObject PathnameDirectory(LispObject path)
    {
        if (path is LispPathname p) return p.DirectoryComponent ?? (LispObject)Nil.Instance;
        if (path is LispString s) return LispPathname.FromString(s.Value) is LispPathname pp
            ? pp.DirectoryComponent ?? (LispObject)Nil.Instance : Nil.Instance;
        if (path is LispVector v && v.IsCharVector) return LispPathname.FromString(v.ToCharString()).DirectoryComponent ?? (LispObject)Nil.Instance;
        if (path is LispFileStream fs) return LispPathname.FromString(fs.FilePath).DirectoryComponent ?? (LispObject)Nil.Instance;
        throw new LispErrorException(new LispTypeError("PATHNAME-DIRECTORY: not a pathname designator", path));
    }

    public static LispObject PathnameName(LispObject path)
    {
        if (path is LispPathname p) return p.NameComponent ?? (LispObject)Nil.Instance;
        if (path is LispString s)
        {
            var pp = LispPathname.FromString(s.Value);
            return pp.NameComponent ?? (LispObject)Nil.Instance;
        }
        if (path is LispVector v && v.IsCharVector) return LispPathname.FromString(v.ToCharString()).NameComponent ?? (LispObject)Nil.Instance;
        if (path is LispFileStream fs) return LispPathname.FromString(fs.FilePath).NameComponent ?? (LispObject)Nil.Instance;
        throw new LispErrorException(new LispTypeError("PATHNAME-NAME: not a pathname designator", path));
    }

    public static LispObject PathnameType(LispObject path)
    {
        if (path is LispPathname p) return p.TypeComponent ?? (LispObject)Nil.Instance;
        if (path is LispString s)
        {
            var pp = LispPathname.FromString(s.Value);
            return pp.TypeComponent ?? (LispObject)Nil.Instance;
        }
        if (path is LispVector v && v.IsCharVector) return LispPathname.FromString(v.ToCharString()).TypeComponent ?? (LispObject)Nil.Instance;
        if (path is LispFileStream fs) return LispPathname.FromString(fs.FilePath).TypeComponent ?? (LispObject)Nil.Instance;
        throw new LispErrorException(new LispTypeError("PATHNAME-TYPE: not a pathname designator", path));
    }

    public static LispObject PathnameHost(LispObject path)
    {
        var p = path is LispPathname pp ? pp
            : path is LispString s ? LispPathname.FromString(s.Value)
            : path is LispVector v && v.IsCharVector ? LispPathname.FromString(v.ToCharString())
            : path is LispFileStream fs ? LispPathname.FromString(fs.FilePath)
            : throw new LispErrorException(new LispTypeError("PATHNAME-HOST: not a pathname designator", path));
        return p.Host ?? (LispObject)Nil.Instance;
    }

    public static LispObject PathnameDevice(LispObject path)
    {
        var p = path is LispPathname pp ? pp
            : path is LispString s ? LispPathname.FromString(s.Value)
            : path is LispVector v && v.IsCharVector ? LispPathname.FromString(v.ToCharString())
            : path is LispFileStream fs ? LispPathname.FromString(fs.FilePath)
            : throw new LispErrorException(new LispTypeError("PATHNAME-DEVICE: not a pathname designator", path));
        if (p is LispLogicalPathname && (p.Device == null || p.Device is Nil))
            return Startup.Keyword("UNSPECIFIC");
        return p.Device ?? (LispObject)Nil.Instance;
    }

    public static LispObject PathnameVersion(LispObject path)
    {
        var p = path is LispPathname pp ? pp
            : path is LispString s ? LispPathname.FromString(s.Value)
            : path is LispVector v && v.IsCharVector ? LispPathname.FromString(v.ToCharString())
            : path is LispFileStream fs ? LispPathname.FromString(fs.FilePath)
            : throw new LispErrorException(new LispTypeError("PATHNAME-VERSION: not a pathname designator", path));
        return p.Version ?? (LispObject)Nil.Instance;
    }

    public static LispObject MergePathnames(LispObject path, LispObject defaults)
        => MergePathnames(path, defaults, Startup.Sym("NEWEST"));

    public static LispObject MergePathnames(LispObject path, LispObject defaults, LispObject defaultVersion)
    {
        var p = path is LispPathname pp ? pp : path is LispString ps ? LispPathname.FromString(ps.Value) : throw new LispErrorException(new LispTypeError("MERGE-PATHNAMES: invalid", path));
        var d = defaults is LispPathname dp ? dp : defaults is LispString ds ? LispPathname.FromString(ds.Value) : throw new LispErrorException(new LispTypeError("MERGE-PATHNAMES: invalid", defaults));
        var merged = p.MergeWith(d);
        // CLHS: if version is still nil after merge, apply default-version
        if (merged.Version == null || merged.Version is Nil)
            return new LispPathname(merged.Host, merged.Device, merged.DirectoryComponent,
                merged.NameComponent, merged.TypeComponent, defaultVersion);
        return merged;
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

        // LOGICAL-PATHNAME, LOGICAL-PATHNAME-TRANSLATIONS, TRANSLATE-LOGICAL-PATHNAME
        Emitter.CilAssembler.RegisterFunction("LOGICAL-PATHNAME",
            new LispFunction(args => Runtime.LogicalPathname(args[0])));
        Emitter.CilAssembler.RegisterFunction("LOGICAL-PATHNAME-TRANSLATIONS",
            new LispFunction(args => Runtime.LogicalPathnameTranslations(args[0])));
        Emitter.CilAssembler.RegisterFunction("TRANSLATE-LOGICAL-PATHNAME",
            new LispFunction(args => Runtime.TranslateLogicalPathname(args[0])));

        // MAKE-PATHNAME as callable function (also compiled inline by cil-compiler)
        Emitter.CilAssembler.RegisterFunction("MAKE-PATHNAME", new LispFunction(Runtime.MakePathname));

        // Pathname accessor functions: accept optional :case keyword (ignored)
        Emitter.CilAssembler.RegisterFunction("PATHNAME-HOST", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("PATHNAME-HOST: expected at least 1 argument"));
            if (args.Length > 1 && (args.Length - 1) % 2 != 0)
                throw new LispErrorException(new LispProgramError("PATHNAME-HOST: odd number of keyword arguments"));
            if (args.Length > 1) {
                bool allowOtherKeys = false;
                for (int i = 1; i < args.Length - 1; i += 2) {
                    if (args[i] is Symbol s && s.Name == "ALLOW-OTHER-KEYS" && args[i+1] is not Nil)
                        allowOtherKeys = true;
                }
                if (!allowOtherKeys) {
                    for (int i = 1; i < args.Length - 1; i += 2) {
                        if (!(args[i] is Symbol s && (s.Name == "CASE" || s.Name == "ALLOW-OTHER-KEYS")))
                            throw new LispErrorException(new LispProgramError("PATHNAME-HOST: unknown keyword argument"));
                    }
                }
            }
            return Runtime.PathnameHost(args[0]);
        }, "PATHNAME-HOST", -1));

        Emitter.CilAssembler.RegisterFunction("PATHNAME-DEVICE", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("PATHNAME-DEVICE: expected at least 1 argument"));
            if (args.Length > 1 && (args.Length - 1) % 2 != 0)
                throw new LispErrorException(new LispProgramError("PATHNAME-DEVICE: odd number of keyword arguments"));
            if (args.Length > 1) {
                bool allowOtherKeys = false;
                for (int i = 1; i < args.Length - 1; i += 2) {
                    if (args[i] is Symbol s && s.Name == "ALLOW-OTHER-KEYS" && args[i+1] is not Nil)
                        allowOtherKeys = true;
                }
                if (!allowOtherKeys) {
                    for (int i = 1; i < args.Length - 1; i += 2) {
                        if (!(args[i] is Symbol s && (s.Name == "CASE" || s.Name == "ALLOW-OTHER-KEYS")))
                            throw new LispErrorException(new LispProgramError("PATHNAME-DEVICE: unknown keyword argument"));
                    }
                }
            }
            return Runtime.PathnameDevice(args[0]);
        }, "PATHNAME-DEVICE", -1));

        Emitter.CilAssembler.RegisterFunction("PATHNAME-NAME", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("PATHNAME-NAME: expected at least 1 argument"));
            return Runtime.PathnameName(args[0]);
        }, "PATHNAME-NAME", -1));

        Emitter.CilAssembler.RegisterFunction("PATHNAME-TYPE", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("PATHNAME-TYPE: expected at least 1 argument"));
            return Runtime.PathnameType(args[0]);
        }, "PATHNAME-TYPE", -1));

        Emitter.CilAssembler.RegisterFunction("PATHNAME-VERSION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("PATHNAME-VERSION: expected exactly 1 argument"));
            return Runtime.PathnameVersion(args[0]);
        }, "PATHNAME-VERSION", -1));

        Emitter.CilAssembler.RegisterFunction("PATHNAME-DIRECTORY", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("PATHNAME-DIRECTORY: expected at least 1 argument"));
            return Runtime.PathnameDirectory(args[0]);
        }, "PATHNAME-DIRECTORY", -1));

        // LOAD-LOGICAL-PATHNAME-TRANSLATIONS
        Emitter.CilAssembler.RegisterFunction("LOAD-LOGICAL-PATHNAME-TRANSLATIONS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("LOAD-LOGICAL-PATHNAME-TRANSLATIONS: expected 1 argument"));
            var hostName = args[0] is LispString ls ? ls.Value : args[0] is Symbol s ? s.Name : args[0].ToString();
            if (Runtime._logicalPathnameTranslations.ContainsKey(hostName.ToUpperInvariant()))
                return Nil.Instance;
            throw new LispErrorException(new LispError($"Cannot find logical pathname translations for host \"{hostName}\""));
        }));

        // PATHNAME and NAMESTRING as callable functions
        Emitter.CilAssembler.RegisterFunction("PATHNAME", new LispFunction(args => Runtime.Pathname(args[0])));
        Emitter.CilAssembler.RegisterFunction("NAMESTRING", new LispFunction(args => Runtime.Namestring(args[0])));

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
            var dirP = new LispPathname(null, null, p.DirectoryComponent, null, null, null);
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
                var fileP = new LispPathname(null, null, null, p.NameComponent, p.TypeComponent, p.Version);
                return new LispString(fileP.ToNamestring());
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
            var result = Runtime.MergePathnames(path, defaults);
            if (result is LispPathname rp && (rp.Version == null || rp.Version is Nil)) {
                if (defaultVersion != null && defaultVersion is not Nil) {
                    result = new LispPathname(rp.Host, rp.Device, rp.DirectoryComponent,
                                               rp.NameComponent, rp.TypeComponent, defaultVersion);
                }
            }
            return result;
        }));

        // WILD-PATHNAME-P: check if pathname has wildcard components
        Emitter.CilAssembler.RegisterFunction("WILD-PATHNAME-P", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("WILD-PATHNAME-P: expected at least 1 argument"));
            if (args.Length > 2) throw new LispErrorException(new LispProgramError("WILD-PATHNAME-P: too many arguments"));
            var path = args[0];
            LispPathname p;
            if (path is LispPathname lp) p = lp;
            else if (path is LispString ls) p = LispPathname.FromString(ls.Value);
            else if (path is LispVector v && v.IsCharVector) p = LispPathname.FromString(v.ToCharString());
            else if (path is LispFileStream fs) p = LispPathname.FromString(fs.FilePath);
            else throw new LispErrorException(new LispTypeError("WILD-PATHNAME-P: not a pathname designator", path));

            Symbol? key = args.Length > 1 && args[1] is Symbol ks ? ks : null;

            bool IsWild(LispObject? component) {
                if (component is Symbol s && s.Name == "WILD") return true;
                if (component is LispString lstr && lstr.Value.Contains('*')) return true;
                return false;
            }

            bool DirectoryIsWild(LispObject? dir) {
                if (IsWild(dir)) return true;
                if (dir is Cons) {
                    var cur = dir;
                    while (cur is Cons c) {
                        if (c.Car is Symbol s && (s.Name == "WILD-INFERIORS" || s.Name == "WILD")) return true;
                        if (c.Car is LispString lstr && lstr.Value.Contains('*')) return true;
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
                if (args.Length == 0) throw new Exception("PARSE-NAMESTRING: requires at least 1 argument");
                var thing = args[0];

                if (thing is LispPathname pn)
                    return MultipleValues.Values(pn, Fixnum.Make(0));

                if (thing is LispFileStream fs2)
                    return MultipleValues.Values(LispPathname.FromString(fs2.FilePath), Fixnum.Make(0));

                string str;
                if (thing is LispString ls) str = ls.Value;
                else if (thing is LispVector vec && vec.IsCharVector) str = vec.ToCharString();
                else throw new LispErrorException(new LispTypeError("PARSE-NAMESTRING: not a string or pathname", thing));

                int kwCount = args.Length - 3;
                if (kwCount > 0) {
                    if (kwCount % 2 != 0)
                        throw new LispErrorException(new LispProgramError("PARSE-NAMESTRING: odd number of keyword arguments"));
                    for (int i = 3; i < args.Length; i += 2)
                    {
                        if (!(args[i] is Symbol kw2 && (kw2.Name == "START" || kw2.Name == "END" || kw2.Name == "JUNK-ALLOWED")))
                            throw new LispErrorException(new LispProgramError("PARSE-NAMESTRING: invalid keyword argument"));
                    }
                }
                int start = 0;
                int end = str.Length;
                bool junkAllowed = false;
                for (int i = 3; i < args.Length - 1; i += 2)
                {
                    if (args[i] is Symbol kw)
                    {
                        if (kw.Name == "START" && args[i + 1] is Fixnum sf) start = (int)sf.Value;
                        else if (kw.Name == "END" && args[i + 1] is Fixnum ef) end = (int)ef.Value;
                        else if (kw.Name == "END" && args[i + 1] is Nil) end = str.Length;
                        else if (kw.Name == "JUNK-ALLOWED") junkAllowed = Runtime.IsTruthy(args[i + 1]);
                    }
                }

                var substr = str.Substring(start, end - start);
                if (substr.Length == 0)
                {
                    return MultipleValues.Values(LispPathname.FromString(""), Fixnum.Make(end));
                }

                try
                {
                    LispPathname result;
                    if (Runtime.IsLogicalPathnameString(substr))
                        result = LispLogicalPathname.FromLogicalString(substr);
                    else
                        result = LispPathname.FromString(substr);
                    return MultipleValues.Values(result, Fixnum.Make(end));
                }
                catch
                {
                    if (junkAllowed)
                        return MultipleValues.Values(Nil.Instance, Fixnum.Make(start));
                    throw new LispErrorException(new LispError($"PARSE-NAMESTRING: cannot parse \"{substr}\""));
                }
            }));

        // PATHNAME-MATCH-P / TRANSLATE-PATHNAME
        Startup.RegisterBinary("PATHNAME-MATCH-P", Runtime.PathnameMatchP);
        Emitter.CilAssembler.RegisterFunction("TRANSLATE-PATHNAME",
            new LispFunction(args => Runtime.TranslatePathname(args[0], args[1], args[2])));

        // COMPILE-FILE-PATHNAME
        Emitter.CilAssembler.RegisterFunction("COMPILE-FILE-PATHNAME",
            new LispFunction(args => Runtime.CompileFilePathname(args)));

        // %SET-LOGICAL-PATHNAME-TRANSLATIONS
        Emitter.CilAssembler.RegisterFunction("%SET-LOGICAL-PATHNAME-TRANSLATIONS", new LispFunction(args => {
            return Runtime.SetLogicalPathnameTranslations(args[0], args[1]);
        }));
    }

}
