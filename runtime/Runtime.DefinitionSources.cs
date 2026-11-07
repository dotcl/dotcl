using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Definition source tracking (rich backtrace, "medium" tier follow-up) ---
    // Maps a top-level definition name to where it was defined, so swank/micros
    // sldb can implement frame-source-location / find-definitions (M-. / jump to
    // the erroring function). Populated by LOAD and COMPILE-FILE as each top-level
    // form is read, BEFORE macroexpansion (defun etc. are macros, so the form must
    // be inspected raw). Line granularity is the top-level form, matching what a
    // debugger needs to jump to the definition.
    // Seq orders the records: a later definition replaces an earlier one, also
    // when the earlier one is a fasl's table resolved only at lookup (below).
    private static System.Collections.Concurrent.ConcurrentDictionary<Symbol, (string File, int Line, long Seq)>? _lz_s_defSources;
    private static System.Collections.Concurrent.ConcurrentDictionary<Symbol, (string File, int Line, long Seq)> s_defSources => System.Threading.Volatile.Read(ref _lz_s_defSources) ?? System.Threading.Interlocked.CompareExchange(ref _lz_s_defSources, new(), null) ?? _lz_s_defSources!;
    // The same for the function kinds only (FUNCTION, GENERIC-FUNCTION, MACRO,
    // METHOD): what a debugger frame or FUNCTION-SOURCE-LOCATION looks for.
    private static System.Collections.Concurrent.ConcurrentDictionary<Symbol, (string File, int Line, long Seq)>? _lz_s_defFunctionSources;
    private static System.Collections.Concurrent.ConcurrentDictionary<Symbol, (string File, int Line, long Seq)> s_defFunctionSources => System.Threading.Volatile.Read(ref _lz_s_defFunctionSources) ?? System.Threading.Interlocked.CompareExchange(ref _lz_s_defFunctionSources, new(), null) ?? _lz_s_defFunctionSources!;
    private static long s_defSeq;

    // Definition tables of loaded fasls not yet resolved to symbols, and the
    // entries of earlier tables whose package or symbol did not exist yet when
    // they were resolved. A fasl registers its table before its first
    // definition runs; resolving it means one package and symbol lookup per
    // definition, which is left to the first lookup of a location instead of
    // every load.
    private static readonly object s_faslSourcesLock = new();
    private static List<(byte[] Data, long Seq, bool WithKinds)> s_pendingFaslSources = new();
    private static List<FaslDefinitionSource> s_unresolvedFaslSources = new();
    private readonly record struct FaslDefinitionSource(string Pkg, string Name, string File, int Line,
        long Seq, string? Kind, string Detail);

    // One definition of one kind: KIND is FUNCTION, MACRO, METHOD, ... (SETF-
    // in front for a (SETF name) function), DETAIL tells methods apart.
    internal readonly record struct DefinitionEntry(Symbol Sym, string Kind, string Detail);

    // Every kind of definition a name has, each with its own location.
    // s_defSources keeps the most recent of them per name.
    private static System.Collections.Concurrent.ConcurrentDictionary<DefinitionEntry, (string File, int Line, long Seq)>? _lz_s_defKindSources;
    private static System.Collections.Concurrent.ConcurrentDictionary<DefinitionEntry, (string File, int Line, long Seq)> s_defKindSources => System.Threading.Volatile.Read(ref _lz_s_defKindSources) ?? System.Threading.Interlocked.CompareExchange(ref _lz_s_defKindSources, new(), null) ?? _lz_s_defKindSources!;
    // Definitions of uninterned names (a macro's gensym DEFUN) read from a fasl,
    // whose symbols exist only once the code runs: matched by name.
    private static System.Collections.Concurrent.ConcurrentDictionary<(string Name, string Kind), (string File, int Line, long Seq)>? _lz_s_uninternedDefSources;
    private static System.Collections.Concurrent.ConcurrentDictionary<(string Name, string Kind), (string File, int Line, long Seq)> s_uninternedDefSources => System.Threading.Volatile.Read(ref _lz_s_uninternedDefSources) ?? System.Threading.Interlocked.CompareExchange(ref _lz_s_uninternedDefSources, new(), null) ?? _lz_s_uninternedDefSources!;

    private static string? DefinerKind(Symbol head)
    {
        var home = head.HomePackage?.Name;
        if (home == "COMMON-LISP") return DefinerKind(head.Name);
        return head.Name == "DEFINE-PACKAGE" && home != null && home.StartsWith("UIOP", StringComparison.Ordinal)
            ? "PACKAGE" : null;
    }

    private static string? DefinerKind(string head) => head switch
    {
        "DEFUN" => "FUNCTION",
        "DEFMACRO" or "DEFINE-MODIFY-MACRO" => "MACRO",
        "DEFGENERIC" => "GENERIC-FUNCTION",
        "DEFMETHOD" => "METHOD",
        "DEFINE-COMPILER-MACRO" => "COMPILER-MACRO",
        "DEFVAR" or "DEFPARAMETER" => "VARIABLE",
        "DEFCONSTANT" => "CONSTANT",
        "DEFCLASS" => "CLASS",
        "DEFSTRUCT" => "STRUCTURE",
        "DEFINE-CONDITION" => "CONDITION",
        "DEFTYPE" => "TYPE",
        "DEFSETF" or "DEFINE-SETF-EXPANDER" => "SETF-EXPANDER",
        "DEFINE-SYMBOL-MACRO" => "SYMBOL-MACRO",
        "DEFPACKAGE" => "PACKAGE",
        "DEFINE-METHOD-COMBINATION" => "METHOD-COMBINATION",
        _ => null
    };

    // The order of this table is the fasl encoding of a kind.
    private static readonly string[] s_definitionKinds =
    {
        "FUNCTION", "SETF-FUNCTION", "MACRO", "GENERIC-FUNCTION", "SETF-GENERIC-FUNCTION",
        "METHOD", "SETF-METHOD", "COMPILER-MACRO", "SETF-COMPILER-MACRO", "VARIABLE",
        "CONSTANT", "CLASS", "STRUCTURE", "CONDITION", "TYPE", "SETF-EXPANDER", "SYMBOL-MACRO",
        "PACKAGE", "METHOD-COMBINATION"
    };

    // A package's definition is keyed by a symbol standing for its name.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Symbol> s_packageKeys = new();
    // PackageKey takes a physical name; a name as the source spells it goes through PackageSourceKey.
    private static Symbol PackageKey(string name) => s_packageKeys.GetOrAdd(name, n => new Symbol(n));
    private static Symbol PackageSourceKey(string name) => PackageKey(Package.PhysicalNameOfSource(name));

    internal static int DefinitionKindCode(string kind) => Array.IndexOf(s_definitionKinds, kind);

    /// <summary>Walk a raw (unexpanded) top-level form and record each definition
    /// it names, descending the scopes CLHS 3.2.3.1 flattens like FlattenTopLevel.</summary>
    internal static void RecordDefinitionSources(LispObject form, string file, int line)
        => RecordDefinitionSources(form, file, line, useSourceLines: false);

    internal static void RecordDefinitionSources(LispObject form, string file, int line,
        bool useSourceLines)
    {
        var names = new List<(DefinitionEntry Entry, int Line)>();
        DefinitionNames(form, line, names, useSourceLines);
        RecordDefinitionNames(names, file, line);
    }

    /// <summary>The definitions <see cref="RecordDefinitionSources"/> records for FORM,
    /// added to NAMES in the order they appear, each with the line it starts on.</summary>
    internal static void DefinitionNames(LispObject form, int line,
        List<(DefinitionEntry Entry, int Line)> names, bool useSourceLines = true)
    {
        if (form is not Cons c || c.Car is not Symbol head) return;
        switch (head.Name)
        {
            case "PROGN":
            case "LOCALLY":
                for (var b = c.Cdr; b is Cons bc; b = bc.Cdr)
                    DefinitionNames(bc.Car, line, names, useSourceLines);
                return;
            case "EVAL-WHEN":
                if (c.Cdr is Cons ew)
                    for (var b = ew.Cdr; b is Cons bc; b = bc.Cdr)
                        DefinitionNames(bc.Car, line, names, useSourceLines);
                return;
            case "MACROLET":
            case "SYMBOL-MACROLET":
                if (c.Cdr is Cons rest)
                    for (var b = rest.Cdr; b is Cons bc; b = bc.Cdr)
                        DefinitionNames(bc.Car, line, names, useSourceLines);
                return;
        }
        DefinerEntries(c, names, useSourceLines ? SourceLineOf(c, line) : line);
    }

    /// <summary>The source line FORM starts on, or FALLBACK when the reader did
    /// not record it (a macroexpansion, or a form read outside LOAD/COMPILE-FILE).</summary>
    internal static int SourceLineOf(LispObject form, int fallback)
        => SourceLineTable is { } t && t.TryGetValue(form, out var span) ? span.sl : fallback;

    /// <summary>Add what a definer form defines to NAMES: its own entry, and
    /// for a DEFGENERIC the methods of its :METHOD options.</summary>
    private static void DefinerEntries(Cons c, List<(DefinitionEntry Entry, int Line)> names, int line)
    {
        if (DefinerEntry(c) is not { } entry) return;
        names.Add((entry, line));
        if (entry.Kind is not ("GENERIC-FUNCTION" or "SETF-GENERIC-FUNCTION") || c.Cdr is not Cons nameCell
            || nameCell.Cdr is not Cons llCell)
            return;
        var methodKind = entry.Kind == "GENERIC-FUNCTION" ? "METHOD" : "SETF-METHOD";
        for (var o = llCell.Cdr; o is Cons oc; o = oc.Cdr)
            if (oc.Car is Cons opt && opt.Car is Symbol k && k.Name == "METHOD" && k.HomePackage?.Name == "KEYWORD")
                names.Add((new DefinitionEntry(entry.Sym, methodKind, MethodDetailFromSource(opt.Cdr)), line));
    }

    /// <summary>What a definer form (DEFUN, DEFVAR, ...) defines, or null.</summary>
    private static DefinitionEntry? DefinerEntry(Cons c)
    {
        if (c.Car is not Symbol head || DefinerKind(head) is not { } kind || c.Cdr is not Cons nameCell)
            return null;
        if (kind == "PACKAGE")
            return nameCell.Car switch
            {
                Symbol ps => new DefinitionEntry(PackageSourceKey(ps.Name), kind, ""),
                LispString pstr => new DefinitionEntry(PackageSourceKey(pstr.Value), kind, ""),
                _ => null
            };
        Symbol sym;
        switch (nameCell.Car)
        {
            case Symbol s:
                sym = s;
                break;
            case Cons nc when nc.Car is Symbol setf && setf.Name == "SETF"
                              && nc.Cdr is Cons nc2 && nc2.Car is Symbol fn && nc2.Cdr is Nil:
                if (kind is not ("FUNCTION" or "GENERIC-FUNCTION" or "METHOD" or "COMPILER-MACRO"))
                    return null;
                sym = fn;
                kind = "SETF-" + kind;
                break;
            case Cons nc when nc.Car is Symbol s2: // (defstruct (name ...) ...)
                sym = s2;
                break;
            default:
                return null;
        }
        var detail = kind.EndsWith("METHOD", StringComparison.Ordinal) && !kind.EndsWith("GENERIC-FUNCTION", StringComparison.Ordinal)
            ? MethodDetailFromSource(nameCell.Cdr) : "";
        return new DefinitionEntry(sym, kind, detail);
    }

    /// <summary>The qualifiers and specializers of the DEFMETHOD whose form
    /// continues with REST, in the text <see cref="MethodDetail"/> gives a method.</summary>
    private static string MethodDetailFromSource(LispObject rest)
    {
        var quals = new List<LispObject>();
        var r = rest;
        for (; r is Cons rc && rc.Car is not (Cons or Nil); r = rc.Cdr)
            quals.Add(rc.Car);
        var specs = new List<LispObject>();
        if (r is Cons llc)
            for (var p = llc.Car; p is Cons pc; p = pc.Cdr)
            {
                if (pc.Car is Symbol s && s.Name.StartsWith('&')) break;
                if (pc.Car is Cons ac && ac.Cdr is Cons sc)
                {
                    var spec = sc.Car;
                    if (spec is Cons ec && ec.Car is Symbol eql && eql.Name == "EQL" && ec.Cdr is Cons ef)
                    {
                        var v = ef.Car;
                        if (v is Cons q && q.Car is Symbol qs && qs.Name == "QUOTE" && q.Cdr is Cons qv)
                            v = qv.Car;
                        else if (v is Symbol cs && cs.IsConstant && DynamicBindings.TryGet(cs, out var cv))
                            v = cv;
                        spec = List(Startup.Sym("EQL"), v);
                    }
                    specs.Add(spec);
                }
                else specs.Add(T.Instance);
            }
        return DetailText(quals, specs);
    }

    /// <summary>The text that tells METHOD apart from the other methods of its
    /// generic function, as <see cref="MethodDetailFromSource"/> writes it.</summary>
    private static string MethodDetail(LispMethod m)
    {
        var specs = new List<LispObject>();
        foreach (var sp in m.Specializers)
            specs.Add(sp switch
            {
                LispClass lc => lc.Name.Name == "T" && lc.Name.HomePackage?.Name == "COMMON-LISP" ? T.Instance : lc.Name,
                EqlSpecializer es => List(Startup.Sym("EQL"), es.Object),
                _ => sp
            });
        return DetailText(m.Qualifiers, specs);
    }

    private static string DetailText(IEnumerable<LispObject> quals, IEnumerable<LispObject> specs)
    {
        var sb = new System.Text.StringBuilder("((");
        AppendItems(quals);
        sb.Append(") (");
        AppendItems(specs);
        sb.Append("))");
        return sb.ToString();

        void AppendItems(IEnumerable<LispObject> items)
        {
            bool first = true;
            foreach (var x in items)
            {
                if (!first) sb.Append(' ');
                first = false;
                Append(x);
            }
        }
        void Append(LispObject x)
        {
            switch (x)
            {
                case T: sb.Append('T'); break;
                case Nil: sb.Append("NIL"); break;
                case Symbol s:
                    if (s.HomePackage is Package p)
                        sb.Append('|').Append(Escape(p.Name)).Append("|::|").Append(Escape(s.Name)).Append('|');
                    else
                        sb.Append("#:|").Append(Escape(s.Name)).Append('|');
                    break;
                case Cons c:
                    sb.Append('(');
                    bool first = true;
                    for (LispObject e = c; e is Cons ec; e = ec.Cdr)
                    {
                        if (!first) sb.Append(' ');
                        first = false;
                        Append(ec.Car);
                    }
                    sb.Append(')');
                    break;
                default:
                    sb.Append(FormatObjectNoDispatch(x));
                    break;
            }
        }
        static string Escape(string n) => n.Replace("\\", "\\\\").Replace("|", "\\|");
    }

    // While non-null, FlattenTopLevel adds every definer form it meets,
    // including those that only appear after a top level macro expands, and the
    // compiler adds every DEFUN it compiles, at any depth. Each name carries the
    // line of the form FlattenTopLevel yielded it for.
    [ThreadStatic] private static List<(DefinitionEntry Entry, int Line)>? s_flattenedDefinitionNames;
    // The line FlattenTopLevel is currently yielding, i.e. of the subform being
    // compiled; a name the compiler reports during that compile is located there.
    [ThreadStatic] private static int s_flattenedDefinitionLine;
    // True while LOAD runs a compiled top level form: a DEFUN compiled then
    // (by an EVAL in it) does not belong to the form's source.
    [ThreadStatic] private static bool s_runningTopLevelForm;

    /// <summary>Of the definitions collected for FORM while it was processed,
    /// keep those that <see cref="DefinitionNames"/> does not see in the raw
    /// form: the definitions made through macro expansion or inside a body.</summary>
    private static List<(DefinitionEntry Entry, int Line)> ExpandedDefinitionNames(
        LispObject form, List<(DefinitionEntry Entry, int Line)> seen)
    {
        if (seen.Count == 0) return seen;
        var raw = new List<(DefinitionEntry Entry, int Line)>();
        DefinitionNames(form, 0, raw, useSourceLines: false);
        var result = new List<(DefinitionEntry, int)>();
        foreach (var (e, line) in seen)
        {
            bool isRaw = false;
            foreach (var (re, _) in raw)
                if (re.Equals(e)) { isRaw = true; break; }
            bool already = false;
            foreach (var (re, _) in result)
                if (re.Equals(e)) { already = true; break; }
            if (!isRaw && !already) result.Add((e, line));
        }
        return result;
    }

    /// <summary>DOTCL::%NOTE-DEFINITION (kind name) or (form): the compiler reports a
    /// definition it compiles, so one inside a body (a DEFUN a macro writes into
    /// a function, a DEFMETHOD in a LET) is located at the top level form being
    /// loaded or compiled.</summary>
    public static LispObject NoteDefinition(LispObject[] args)
    {
        if (args.Length == 1)
        {
            if (s_flattenedDefinitionNames is { } formNames && !s_runningTopLevelForm && args[0] is Cons form)
                DefinerEntries(form, formNames, s_flattenedDefinitionLine);
            return Nil.Instance;
        }
        if (args.Length >= 2 && s_flattenedDefinitionNames is { } names && !s_runningTopLevelForm
            && args[0] is Symbol kind && DefinitionKindCode(kind.Name) >= 0)
        {
            var name = args[1];
            var k = kind.Name;
            if (name is Cons nc && nc.Car is Symbol setf && setf.Name == "SETF" && nc.Cdr is Cons nc2)
            {
                name = nc2.Car;
                k = "SETF-" + k;
            }
            if (name is Symbol sym && DefinitionKindCode(k) >= 0)
                names.Add((new DefinitionEntry(sym, k, ""), s_flattenedDefinitionLine));
        }
        return Nil.Instance;
    }

    private static void RecordDefinitionNames(List<(DefinitionEntry Entry, int Line)> names,
        string file, int fallbackLine)
    {
        foreach (var (e, line) in names)
            RecordDefinitionSource(e, file, line > 0 ? line : fallbackLine,
                System.Threading.Interlocked.Increment(ref s_defSeq));
    }

    // When a definition was removed (FMAKUNBOUND, REMOVE-METHOD): a record
    // older than that, from a fasl table resolved only now, is not taken.
    // A null detail stands for every method.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(Symbol Sym, string Kind, string? Detail), long>
        s_defRemoved = new();

    // Writers of the tables above take this lock; s_defEntriesByName (the
    // entries of s_defKindSources by symbol name) is read under it too.
    private static readonly object s_defRecordLock = new();
    private static readonly Dictionary<string, List<DefinitionEntry>> s_defEntriesByName = new();

    private static List<DefinitionEntry> EntriesNamed(string name)
    {
        lock (s_defRecordLock)
            return s_defEntriesByName.TryGetValue(name, out var l) ? new List<DefinitionEntry>(l) : new();
    }

    private static void RemoveEntry(DefinitionEntry e)
    {
        s_defKindSources.TryRemove(e, out _);
        if (s_defEntriesByName.TryGetValue(e.Sym.Name, out var l)) l.Remove(e);
    }

    private static bool RemovedSince(DefinitionEntry e, long seq)
        => !s_defRemoved.IsEmpty
           && ((s_defRemoved.TryGetValue((e.Sym, e.Kind, null), out var all) && all > seq)
               || (e.Detail.Length > 0 && s_defRemoved.TryGetValue((e.Sym, e.Kind, e.Detail), out var one) && one > seq));

    /// <summary>Drop the recorded definitions of SYM of the given KINDS (only
    /// the method DETAIL when non-null), and point SYM's most recent location
    /// at what is left.</summary>
    private static void ForgetDefinitionSources(Symbol sym, string[] kinds, string? detail)
    {
        lock (s_defRecordLock)
        {
            var seq = System.Threading.Interlocked.Increment(ref s_defSeq);
            foreach (var k in kinds) s_defRemoved[(sym, k, detail)] = seq;
            if (!s_defEntriesByName.TryGetValue(sym.Name, out var named)) return;
            var removedSeqs = new HashSet<long>();
            foreach (var e in named.ToArray())
                if (ReferenceEquals(e.Sym, sym) && Array.IndexOf(kinds, e.Kind) >= 0
                    && (detail == null || e.Detail == detail)
                    && s_defKindSources.TryGetValue(e, out var gone))
                {
                    RemoveEntry(e);
                    removedSeqs.Add(gone.Seq);
                }
            if (removedSeqs.Count > 0) RepointLatest(sym, removedSeqs);
        }
    }

    private static void RepointLatest(Symbol sym, HashSet<long> removedSeqs)
    {
        Repoint(s_defSources, kind => kind != "PACKAGE");
        Repoint(s_defFunctionSources, IsFunctionKind);

        void Repoint(System.Collections.Concurrent.ConcurrentDictionary<Symbol, (string File, int Line, long Seq)> latest,
            Func<string, bool> counts)
        {
            if (!latest.TryGetValue(sym, out var cur) || !removedSeqs.Contains(cur.Seq)) return;
            (string File, int Line, long Seq)? best = null;
            if (s_defEntriesByName.TryGetValue(sym.Name, out var named))
                foreach (var e in named)
                    if (ReferenceEquals(e.Sym, sym) && !IsSetfKind(e.Kind) && counts(e.Kind)
                        && s_defKindSources.TryGetValue(e, out var loc)
                        && (best == null || loc.Seq > best.Value.Seq))
                        best = loc;
            if (best is { } b) latest[sym] = b;
            else latest.TryRemove(sym, out _);
        }
    }

    private static bool IsFunctionKind(string kind)
        => kind is "FUNCTION" or "GENERIC-FUNCTION" or "MACRO" or "METHOD";

    private static readonly string[] s_fmakunboundKinds =
        { "FUNCTION", "MACRO", "GENERIC-FUNCTION", "METHOD", "COMPILER-MACRO" };
    private static readonly string[] s_setfFmakunboundKinds =
        { "SETF-FUNCTION", "SETF-GENERIC-FUNCTION", "SETF-METHOD" };

    internal static void ForgetFunctionSources(Symbol sym, bool setf)
        => ForgetDefinitionSources(sym, setf ? s_setfFmakunboundKinds : s_fmakunboundKinds, null);

    internal static void ForgetMethodSource(GenericFunction gf, LispMethod m)
    {
        var name = PublicFunctionName(gf.Name);
        if (name is Symbol s)
            ForgetDefinitionSources(s, new[] { "METHOD" }, MethodDetail(m));
        else if (name is Cons c && c.Cdr is Cons c2 && c2.Car is Symbol s2)
            ForgetDefinitionSources(s2, new[] { "SETF-METHOD" }, MethodDetail(m));
    }

    /// <summary>DELETE-PACKAGE: drop the definitions of PKG's symbols, its
    /// DEFPACKAGE, and the fasl rows still waiting for it.</summary>
    internal static void ForgetPackageSources(Package pkg)
    {
        ResolveFaslDefinitionSources();
        var names = new HashSet<string>(pkg.Nicknames) { pkg.Name };
        lock (s_faslSourcesLock)
            s_unresolvedFaslSources.RemoveAll(e => names.Contains(e.Pkg));
        lock (s_defRecordLock)
        {
            foreach (var kv in s_defKindSources)
                if (ReferenceEquals(kv.Key.Sym.HomePackage, pkg)
                    || (kv.Key.Kind == "PACKAGE" && names.Contains(kv.Key.Sym.Name)))
                    RemoveEntry(kv.Key);
            foreach (var kv in s_defSources)
                if (ReferenceEquals(kv.Key.HomePackage, pkg))
                    s_defSources.TryRemove(kv.Key, out _);
            foreach (var kv in s_defFunctionSources)
                if (ReferenceEquals(kv.Key.HomePackage, pkg))
                    s_defFunctionSources.TryRemove(kv.Key, out _);
        }
    }

    private static void RecordDefinitionSource(DefinitionEntry e, string file, int line, long seq)
    {
        lock (s_defRecordLock)
        {
            if (RemovedSince(e, seq)) return;
            if (s_defKindSources.TryGetValue(e, out var had))
            {
                if (had.Seq > seq) return;
            }
            else
            {
                if (!s_defEntriesByName.TryGetValue(e.Sym.Name, out var named))
                    s_defEntriesByName[e.Sym.Name] = named = new List<DefinitionEntry>();
                named.Add(e);
            }
            s_defKindSources[e] = (file, line, seq);
            if (IsSetfKind(e.Kind) || e.Kind == "PACKAGE") return;
            SetLatest(s_defSources, e.Sym, file, line, seq);
            if (IsFunctionKind(e.Kind)) SetLatest(s_defFunctionSources, e.Sym, file, line, seq);
        }
    }

    private static void SetLatest(System.Collections.Concurrent.ConcurrentDictionary<Symbol, (string File, int Line, long Seq)> latest,
        Symbol sym, string file, int line, long seq)
    {
        if (latest.TryGetValue(sym, out var had) && had.Seq > seq) return;
        latest[sym] = (file, line, seq);
    }

    /// <summary>FILE with a leading part replaced as dotcl:*source-path-map* (or, when that is
    /// NIL, the environment variable DOTCL_SOURCE_PATH_MAP, "from=to;from=to") says. The only
    /// place a source path enters a fasl, so a build that has to give the same bytes wherever
    /// the checkout is maps it here. The longest matching FROM wins; a FROM matches whole
    /// directory names only.</summary>
    internal static string MapSourcePath(string file)
    {
        var pairs = new List<(string From, string To)>();
        var sym = Startup.DotclPkg?.FindSymbol("*SOURCE-PATH-MAP*").Item1;
        if (sym != null)
            for (var cur = DynamicBindings.Get(sym); cur is Cons c; cur = c.Cdr)
                if (c.Car is Cons pair && StringValueOrNull(pair.Car) is string from
                    && StringValueOrNull(pair.Cdr) is string to)
                    pairs.Add((from, to));
        if (pairs.Count == 0)
        {
            var env = Environment.GetEnvironmentVariable("DOTCL_SOURCE_PATH_MAP");
            if (!string.IsNullOrEmpty(env))
                foreach (var item in env.Split(';', StringSplitOptions.RemoveEmptyEntries))
                {
                    int eq = item.IndexOf('=');
                    if (eq > 0) pairs.Add((item.Substring(0, eq), item.Substring(eq + 1)));
                }
        }
        // Separators are compared as '/' on every OS, and case is ignored on Windows, whose
        // paths are written with either separator and any case. A mapped path is written with '/'.
        bool win = Compat.IsWindows();   // elsewhere a backslash is an ordinary file name character
        var cmp = win ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
        string Slash(string x) => win ? x.Replace('\\', '/') : x;
        var slashed = Slash(file);
        string? bestFrom = null, bestTo = null;
        foreach (var (from0, to) in pairs)
        {
            var from = Slash(from0).TrimEnd('/');
            if (from.Length == 0 || (bestFrom != null && from.Length <= bestFrom.Length)) continue;
            if (slashed.Length >= from.Length
                && slashed.StartsWith(from, cmp)
                && (slashed.Length == from.Length || slashed[from.Length] == '/'))
            { bestFrom = from; bestTo = to; }
        }
        if (bestFrom == null) return file;
        return Slash(bestTo!).TrimEnd('/') + slashed.Substring(bestFrom.Length);
    }

    /// <summary>The prefix a packed library's source paths are mapped to: /dotcl-src/NAME/rel.</summary>
    internal const string MappedSourceRoot = "/dotcl-src/";

    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, string> s_sourceRoots = new();

    /// <summary>Tells where the source of the library NAME is on this machine, so that the paths
    /// /dotcl-src/NAME/... its fasl records (see MapSourcePath) lead to real files.</summary>
    public static LispObject RegisterSourceRoot(LispObject[] args)
    {
        if (args.Length != 2 || StringValueOrNull(args[0]) is not string name || StringValueOrNull(args[1]) is not string dir)
            throw new LispErrorException(new LispProgramError("REGISTER-SOURCE-ROOT: requires a name and a directory"));
        s_sourceRoots[name] = dir;
        return args[0];
    }

    /// <summary>The reverse of MapSourcePath: a /dotcl-src/NAME/rel path becomes the real path of
    /// that file, when the source directory of NAME is known (REGISTER-SOURCE-ROOT, or, for a
    /// system ASDF has loaded, asdf:system-source-directory) and the file is there. Anything
    /// else comes back as it was.</summary>
    internal static string UnmapSourcePath(string file)
    {
        if (!file.StartsWith(MappedSourceRoot, StringComparison.Ordinal)) return file;
        int slash = file.IndexOf('/', MappedSourceRoot.Length);
        if (slash < 0) return file;
        var name = file.Substring(MappedSourceRoot.Length, slash - MappedSourceRoot.Length);
        var rel = file.Substring(slash + 1);
        if (name.Length == 0 || rel.Length == 0 || rel.Contains("..")) return file;
        string? dir = s_sourceRoots.TryGetValue(name, out var d) ? d : AsdfSourceDirectory(name);
        if (dir == null) return file;
        var real = dir.TrimEnd('/', '\\') + "/" + rel;
        return File.Exists(real) ? real : file;
    }

    private static string? AsdfSourceDirectory(string name)
    {
        try
        {
            var asdf = Package.FindPackage("ASDF");
            var fn = asdf?.FindSymbol("SYSTEM-SOURCE-DIRECTORY").Item1?.Function as LispFunction;
            if (fn == null) return null;
            var r = fn.Invoke(new LispString(name));
            return r is Nil ? null : (r is LispPathname pn ? pn.ToString() : StringValueOrNull(r));
        }
        catch (Exception) { return null; }
    }

#if DOTCL_EMIT
    /// <summary>Write the definitions FORM names into the fasl's definition
    /// table, with the absolute path of the source (or, for a project build that
    /// compiles several sources as one unit, the source the line falls in).</summary>
    private static void AddFaslDefinitionSources(DotCL.Emitter.FaslAssembler fasl,
        LispObject form, string inputPath, int line)
    {
        var names = new List<(DefinitionEntry Entry, int Line)>();
        DefinitionNames(form, line, names);
        AddFaslDefinitionNames(fasl, names, inputPath, line);
    }

    private static void AddFaslDefinitionNames(DotCL.Emitter.FaslAssembler fasl,
        List<(DefinitionEntry Entry, int Line)> names, string inputPath, int fallbackLine)
    {
        if (names.Count == 0) return;
        var map = BuildDebugLineMap;
        foreach (var (e, nameLine) in names)
        {
            int line = nameLine > 0 ? nameLine : fallbackLine;
            string file;
            if (map != null && map.Length > 0)
            {
                int idx = 0;
                for (int i = 0; i < map.Length; i++)
                    if (map[i].startLine <= line) idx = i; else break;
                file = map[idx].path;
                line = System.Math.Max(1, line - map[idx].startLine + 1);
            }
            else
                file = BuildDebugSourceOverride ?? inputPath;
            file = MapSourcePath(Path.GetFullPath(file));
            fasl.AddDefinitionSource(file, e.Kind == "PACKAGE" ? null : (e.Sym.HomePackage as Package)?.Name, e.Sym.Name,
                line, DefinitionKindCode(e.Kind), e.Detail);
        }
    }
#endif

    /// <summary>Called by a fasl's ModuleInit before its first definition runs:
    /// DATA is the table of an older COMPILE-FILE, which named a symbol per
    /// definition without its kind.</summary>
    public static void RegisterFaslDefinitionSources(byte[] data)
    {
        lock (s_faslSourcesLock)
            s_pendingFaslSources.Add((data, System.Threading.Interlocked.Increment(ref s_defSeq), false));
    }

    /// <summary>Called by a fasl's ModuleInit before its first definition runs:
    /// DATA is the table COMPILE-FILE wrote (see FaslAssembler.AddDefinitionSource).
    /// Only queued here; <see cref="ResolveFaslDefinitionSources"/> reads it.</summary>
    public static void RegisterFaslDefinitionKindSources(byte[] data)
    {
        lock (s_faslSourcesLock)
            s_pendingFaslSources.Add((data, System.Threading.Interlocked.Increment(ref s_defSeq), true));
    }

    /// <summary>Move the queued fasl tables into s_defKindSources. An entry keeps
    /// the sequence number of its fasl's load, so a definition loaded later from
    /// source (or another fasl) is not replaced by it. An entry whose package or
    /// symbol does not exist yet is kept for the next call.</summary>
    private static void ResolveFaslDefinitionSources()
    {
        if (s_pendingFaslSources.Count == 0 && s_unresolvedFaslSources.Count == 0) return;
        lock (s_faslSourcesLock)
        {
            var pending = s_pendingFaslSources;
            var retry = s_unresolvedFaslSources;
            if (pending.Count == 0 && retry.Count == 0) return;
            s_pendingFaslSources = new();
            s_unresolvedFaslSources = new();
            foreach (var e in retry)
                ResolveFaslDefinitionSource(e);
            foreach (var (data, seq, withKinds) in pending)
            {
                int pos = 0;
                var files = new string[(int)FaslData.ReadVarint(data, ref pos)];
                for (int i = 0; i < files.Length; i++) files[i] = FaslData.ReadStr(data, ref pos);
                var pkgs = new string[(int)FaslData.ReadVarint(data, ref pos)];
                for (int i = 0; i < pkgs.Length; i++) pkgs[i] = FaslData.ReadStr(data, ref pos);
                int n = (int)FaslData.ReadVarint(data, ref pos);
                for (int i = 0; i < n; i++)
                {
                    var file = files[(int)FaslData.ReadVarint(data, ref pos)];
                    var pkgIdx = (int)FaslData.ReadVarint(data, ref pos);
                    var name = FaslData.ReadStr(data, ref pos);
                    int line = (int)FaslData.ReadVarint(data, ref pos);
                    if (!withKinds)
                    {
                        ResolveFaslDefinitionSource(new(pkgs[pkgIdx], name, file, line, seq, null, ""));
                        continue;
                    }
                    var kind = s_definitionKinds[(int)FaslData.ReadVarint(data, ref pos)];
                    var detail = FaslData.ReadStr(data, ref pos);
                    if (kind == "PACKAGE")
                    {
                        RecordDefinitionSource(new DefinitionEntry(PackageKey(name), kind, ""), file, line, seq);
                        continue;
                    }
                    // Package index 0 is an uninterned name.
                    if (pkgIdx == 0)
                    {
                        var key = (name, kind);
                        if (!s_uninternedDefSources.TryGetValue(key, out var had) || had.Seq < seq)
                            s_uninternedDefSources[key] = (file, line, seq);
                        continue;
                    }
                    ResolveFaslDefinitionSource(new(pkgs[pkgIdx - 1], name, file, line, seq, kind, detail));
                }
            }
        }
    }

    private static void ResolveFaslDefinitionSource(FaslDefinitionSource e)
    {
        var sym = Package.FindPackage(e.Pkg)?.FindSymbol(e.Name).symbol;
        if (sym == null)
        {
            s_unresolvedFaslSources.Add(e);
            return;
        }
        if (e.Kind == null)
        {
            // An older fasl's row does not say its kind.
            lock (s_defRecordLock)
            {
                SetLatest(s_defSources, sym, e.File, e.Line, e.Seq);
                SetLatest(s_defFunctionSources, sym, e.File, e.Line, e.Seq);
            }
            return;
        }
        RecordDefinitionSource(new DefinitionEntry(sym, e.Kind, e.Detail), e.File, e.Line, e.Seq);
    }

    // Active COMPILE-FILE "was fbound before" snapshots, innermost last.
    // A nested LOAD during compile-file re-snapshots into every pair so the
    // post-compile strip keeps definitions that came from loading other files
    // (e.g. a compile-time (require ...)): see CompileFile.
    [ThreadStatic]
    private static Stack<CompileFileDefinitions>? s_compileFilePreSetsTS;
    private static Stack<CompileFileDefinitions> s_compileFilePreSets
        => s_compileFilePreSetsTS ??= new();

    /// <summary>
    /// Which function cells one COMPILE-FILE leaves alone when it finishes: those
    /// that were fbound when it began, plus the definitions it was told to keep
    /// (explicit compile-time evaluation, a nested LOAD). A cell nobody assigned
    /// during the compilation still holds what it held at the start, so only the
    /// cells the definition journal saw assigned need a closer look; the rest are
    /// answered from their current value. This replaces a snapshot of every
    /// symbol in every package at the start of each COMPILE-FILE and after each
    /// nested LOAD, and the same scan again for the strip at the end.
    /// </summary>
    private sealed class CompileFileDefinitions
    {
        private readonly DefinitionJournal.Window _window = DefinitionJournal.Open();
        private readonly HashSet<Symbol> _keptFn = new();
        private readonly HashSet<Symbol> _keptSetf = new();

        internal bool KeepsFn(Symbol s) =>
            _keptFn.Contains(s)
            || (DefinitionJournal.TryGetBefore(_window, s, out var b) ? b.Fn != null : s.Function != null);

        internal bool KeepsSetf(Symbol s) =>
            _keptSetf.Contains(s)
            || (DefinitionJournal.TryGetBefore(_window, s, out var b) ? b.Setf != null : s.SetfFunction != null);

        internal void KeepFn(Symbol s) => _keptFn.Add(s);
        internal void KeepSetf(Symbol s) => _keptSetf.Add(s);

        /// <summary>Keep every definition there is now (after a nested LOAD).</summary>
        internal void KeepAllCurrent()
        {
            foreach (var s in DefinitionJournal.Touched(_window))
            {
                if (s.Function != null) _keptFn.Add(s);
                if (s.SetfFunction != null) _keptSetf.Add(s);
            }
        }

        /// <summary>Stop recording; returns the symbols assigned meanwhile.</summary>
        internal List<Symbol> Close()
        {
            DefinitionJournal.Close(_window);
            return DefinitionJournal.Touched(_window);
        }
    }

    // Per active COMPILE-FILE (innermost last): the module names a REQUIRE
    // added to *MODULES* while that compile was running. The post-compile
    // cleanup clears the ASDF registration of exactly these. Diffing *MODULES*
    // instead would also catch a module the file itself PROVIDEs at compile
    // time -- which is the name of the ASDF system being built when the file
    // ends in (provide :system-name), so its registration vanished mid-build.
    [ThreadStatic]
    private static Stack<HashSet<string>>? s_compileFileRequiredTS;
    private static Stack<HashSet<string>> s_compileFileRequired
        => s_compileFileRequiredTS ??= new();

    /// <summary>True while COMPILE-FILE is running on this thread, so any code
    /// executing now is being evaluated at compile time.</summary>
    internal static bool CompileFileInProgress => s_compileFilePreSetsTS is { Count: > 0 };

    // Start watching for definitions: what the post-COMPILE-FILE strip would
    // remove is what is fbound now but was not before COMPILE-FILE began, and
    // KeepDefinitionsMadeSince exempts whatever changes from here on.
    private static DefinitionJournal.Window SnapshotStrippableDefinitions() => DefinitionJournal.Open();

    // Exempt from the strip every definition that is new or different since
    // BEFORE was opened: the symbols whose cells were assigned meanwhile and
    // now hold something else.
    private static void KeepDefinitionsMadeSince(
        DefinitionJournal.Window before, CompileFileDefinitions defs)
    {
        DefinitionJournal.Close(before);
        foreach (var (s, b) in before.Before)
        {
            var f = s.Function;
            var sf = s.SetfFunction;
            if (f != null && !defs.KeepsFn(s) && !ReferenceEquals(b.Fn, f)) defs.KeepFn(s);
            if (sf != null && !defs.KeepsSetf(s) && !ReferenceEquals(b.Setf, sf)) defs.KeepSetf(s);
        }
    }

    private static void WhitelistLoadedDefinitionsForCompileFile()
    {
        var stack = s_compileFilePreSetsTS;
        if (stack == null || stack.Count == 0) return;
        foreach (var defs in stack)
            defs.KeepAllCurrent();
    }

    /// <summary>
    /// DOTCL:FUNCTION-SOURCE-LOCATION (name): where NAME (a symbol) was most
    /// recently defined as a function, generic function, method or macro under
    /// LOAD or COMPILE-FILE (failing that, by any other definer: defclass,
    /// defvar, ...), as the plist (:FILE "path" :LINE n), or NIL if unknown.
    /// Backs the swank/micros sldb frame-source-location and find-definitions so
    /// M-. and "jump to erroring frame" work (issue: rich backtrace, source tier).
    /// </summary>
    public static LispObject FunctionSourceLocation(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not Symbol name) return Nil.Instance;
        ResolveFaslDefinitionSources();
        if (!s_defFunctionSources.TryGetValue(name, out var loc) && !s_defSources.TryGetValue(name, out loc))
            return Nil.Instance;
        return new Cons(Startup.Keyword("FILE"),
               new Cons(new LispString(UnmapSourcePath(loc.File)),
               new Cons(Startup.Keyword("LINE"),
               new Cons(Fixnum.Make(loc.Line), Nil.Instance))));
    }

    /// <summary>
    /// DOTCL:DEFINITION-SOURCES (name): every recorded definition of NAME (a
    /// symbol or (SETF symbol); a string or package for its DEFPACKAGE), one plist each: (:KIND kind :FILE "path" :LINE n),
    /// with :QUALIFIERS and :SPECIALIZERS for a method. KIND is one of :FUNCTION
    /// :MACRO :COMPILER-MACRO :GENERIC-FUNCTION :METHOD :VARIABLE :CONSTANT :CLASS
    /// :STRUCTURE :CONDITION :TYPE :SETF-EXPANDER :SYMBOL-MACRO :PACKAGE
    /// :METHOD-COMBINATION.
    /// </summary>
    public static LispObject DefinitionSources(LispObject[] args)
    {
        if (args.Length < 1) return Nil.Instance;
        Symbol? sym = null;
        bool setf = false;
        if (args[0] is LispString or Package)
        {
            var pname = args[0] is Package pk ? pk.Name : Package.PhysicalNameOfSource(((LispString)args[0]).Value);
            ResolveFaslDefinitionSources();
            var pe = new DefinitionEntry(PackageKey(pname), "PACKAGE", "");
            return s_defKindSources.TryGetValue(pe, out var ploc)
                ? new Cons(DefinitionPlist(pe, ploc.File, ploc.Line, null), Nil.Instance)
                : Nil.Instance;
        }
        if (args[0] is Symbol s) sym = s;
        else if (args[0] is Cons c && c.Car is Symbol h && h.Name == "SETF"
                 && c.Cdr is Cons c2 && c2.Car is Symbol s2 && c2.Cdr is Nil)
        {
            sym = s2;
            setf = true;
        }
        if (sym == null) return Nil.Instance;
        ResolveFaslDefinitionSources();
        var found = new List<(DefinitionEntry E, (string File, int Line, long Seq) Loc)>();
        foreach (var e in EntriesNamed(sym.Name))
            if (ReferenceEquals(e.Sym, sym) && IsSetfKind(e.Kind) == setf
                && s_defKindSources.TryGetValue(e, out var loc))
                found.Add((e, loc));
        found.Sort((a, b) =>
        {
            int k = DefinitionKindCode(a.E.Kind).CompareTo(DefinitionKindCode(b.E.Kind));
            if (k != 0) return k;
            k = a.Loc.Seq.CompareTo(b.Loc.Seq);
            if (k != 0) return k;
            k = a.Loc.Line.CompareTo(b.Loc.Line);
            return k != 0 ? k : string.CompareOrdinal(a.E.Detail, b.E.Detail);
        });
        LispObject result = Nil.Instance;
        for (int i = found.Count - 1; i >= 0; i--)
            result = new Cons(DefinitionPlist(found[i].E, found[i].Loc.File, found[i].Loc.Line, null), result);
        return result;
    }

    private static bool IsSetfKind(string kind)
        => kind.StartsWith("SETF-", StringComparison.Ordinal) && kind != "SETF-EXPANDER";

    private static LispObject DefinitionPlist(DefinitionEntry e, string file, int line, LispObject? name)
    {
        var kind = IsSetfKind(e.Kind) ? e.Kind.Substring(5) : e.Kind;
        var items = new List<LispObject>();
        if (name != null) { items.Add(Startup.Keyword("NAME")); items.Add(name); }
        items.Add(Startup.Keyword("KIND")); items.Add(Startup.Keyword(kind));
        if (e.Detail.Length > 0)
        {
            int pos = 0;
            if (ParseDetail(e.Detail, ref pos) is Cons dq && dq.Cdr is Cons ds)
            {
                items.Add(Startup.Keyword("QUALIFIERS")); items.Add(dq.Car);
                items.Add(Startup.Keyword("SPECIALIZERS")); items.Add(ds.Car);
            }
        }
        items.Add(Startup.Keyword("FILE")); items.Add(new LispString(UnmapSourcePath(file)));
        items.Add(Startup.Keyword("LINE")); items.Add(Fixnum.Make(line));
        LispObject r = Nil.Instance;
        for (int i = items.Count - 1; i >= 0; i--) r = new Cons(items[i], r);
        return r;
    }

    /// <summary>Read back what <see cref="DetailText"/> wrote, without interning:
    /// a symbol whose package or symbol is missing comes back uninterned.</summary>
    private static LispObject? ParseDetail(string text, ref int pos)
    {
        while (pos < text.Length && text[pos] == ' ') pos++;
        if (pos >= text.Length) return null;
        char ch = text[pos];
        if (ch == '(')
        {
            pos++;
            var elems = new List<LispObject>();
            while (true)
            {
                while (pos < text.Length && text[pos] == ' ') pos++;
                if (pos >= text.Length) return null;
                if (text[pos] == ')') { pos++; break; }
                if (ParseDetail(text, ref pos) is not { } x) return null;
                elems.Add(x);
            }
            LispObject list = Nil.Instance;
            for (int i = elems.Count - 1; i >= 0; i--) list = new Cons(elems[i], list);
            return list;
        }
        if (ch == '|' || text.AsSpan(pos).StartsWith("#:|".AsSpan()))
        {
            if (ch == '#')
            {
                pos += 2;
                return BarName(text, ref pos) is { } un ? new Symbol(un) : null;
            }
            if (BarName(text, ref pos) is not { } pkgName || !text.AsSpan(pos).StartsWith("::".AsSpan())) return null;
            pos += 2;
            if (BarName(text, ref pos) is not { } symName) return null;
            return Package.FindPackage(pkgName)?.FindSymbol(symName).symbol is { } found
                ? CanonicalizeSymbol(found) : new Symbol(symName);
        }
        int start = pos;
        if (ch == '"')
            for (pos++; pos < text.Length && text[pos] != '"'; pos++)
                if (text[pos] == '\\') pos++;
        while (pos < text.Length && text[pos] != ' ' && text[pos] != ')') pos++;
        var token = text.Substring(start, pos - start);
        if (token == "T") return T.Instance;
        if (token == "NIL") return Nil.Instance;
        try { return UnwrapMv(ReadFromString(new LispObject[] { new LispString(token) })); }
        catch (Exception) { return new LispString(token); }

        static string? BarName(string text, ref int pos)
        {
            if (pos >= text.Length || text[pos] != '|') return null;
            var sb = new System.Text.StringBuilder();
            for (pos++; pos < text.Length; pos++)
            {
                if (text[pos] == '|') { pos++; return sb.ToString(); }
                if (text[pos] == '\\' && pos + 1 < text.Length) pos++;
                sb.Append(text[pos]);
            }
            return null;
        }
    }

    /// <summary>
    /// DOTCL:DEFINITION-SOURCE (object): where OBJECT -- a function, macro
    /// function, compiler macro function, generic function, method, class or package --
    /// was defined, as a plist like those of DOTCL:DEFINITION-SOURCES with :NAME
    /// in front, or NIL if unknown.
    /// </summary>
    public static LispObject DefinitionSource(LispObject[] args)
    {
        if (args.Length < 1) return Nil.Instance;
        ResolveFaslDefinitionSources();
        switch (args[0])
        {
            case LispMethod m:
                if (m.Owner is not GenericFunction mgf) return Nil.Instance;
                return FindByName(PublicFunctionName(mgf.Name), "METHOD", MethodDetail(m)) ?? Nil.Instance;
            case GenericFunction gf:
                return FindByName(PublicFunctionName(gf.Name), "GENERIC-FUNCTION", "")
                    ?? FindByName(PublicFunctionName(gf.Name), "FUNCTION", "")
                    ?? Nil.Instance;
            case LispClass lc:
                return FindByName(lc.Name, "CLASS", "")
                    ?? FindByName(lc.Name, "STRUCTURE", "")
                    ?? FindByName(lc.Name, "CONDITION", "")
                    ?? Nil.Instance;
            case LispFunction fn:
                return FunctionObjectSource(fn) ?? Nil.Instance;
            case Package pkg:
                var pe = new DefinitionEntry(PackageKey(pkg.Name), "PACKAGE", "");
                return s_defKindSources.TryGetValue(pe, out var ploc)
                    ? DefinitionPlist(pe, ploc.File, ploc.Line, new LispString(pkg.Name))
                    : Nil.Instance;
        }
        return Nil.Instance;

        static LispObject? FindByName(LispObject name, string kind, string detail)
        {
            Symbol? sym = name as Symbol;
            if (name is Cons c && c.Cdr is Cons c2 && c2.Car is Symbol s2)
            {
                sym = s2;
                kind = "SETF-" + kind;
            }
            if (sym == null) return null;
            var e = new DefinitionEntry(sym, kind, detail);
            return s_defKindSources.TryGetValue(e, out var loc) ? DefinitionPlist(e, loc.File, loc.Line, name) : null;
        }
    }

    private static LispObject? FunctionObjectSource(LispFunction fn)
    {
        LispObject? Plist(DefinitionEntry e, (string File, int Line, long Seq) loc)
            => DefinitionPlist(e, loc.File, loc.Line,
                IsSetfKind(e.Kind) ? List(Startup.Sym("SETF"), e.Sym) : e.Sym);
        LispObject? Find(Symbol sym, string kind)
        {
            var e = new DefinitionEntry(sym, kind, "");
            return s_defKindSources.TryGetValue(e, out var loc) ? Plist(e, loc) : null;
        }
        if (fn.Name is { } fname)
        {
            bool setf = fname.StartsWith("(SETF ", StringComparison.Ordinal) && fname.EndsWith(")", StringComparison.Ordinal);
            var simple = setf ? fname.Substring(6, fname.Length - 7) : fname;
            foreach (var e in EntriesNamed(simple))
                if (e.Kind == (setf ? "SETF-FUNCTION" : "FUNCTION")
                    && ReferenceEquals(setf ? e.Sym.SetfFunction : e.Sym.Function, fn)
                    && s_defKindSources.TryGetValue(e, out var loc))
                    return Plist(e, loc);
        }
        if (MacroFunctionName(fn) is { } macroName && Find(macroName, "MACRO") is { } mp)
            return mp;
        foreach (var kv in _compilerMacroFunctions)
            if (ReferenceEquals(kv.Value, fn) && Find(kv.Key, "COMPILER-MACRO") is { } cp)
                return cp;
        foreach (var kv in _setfCompilerMacros)
            if (ReferenceEquals(kv.Value, fn) && Find(kv.Key, "SETF-COMPILER-MACRO") is { } scp)
                return scp;
        if (fn.Name is { } name && !name.StartsWith("(", StringComparison.Ordinal)
            && s_uninternedDefSources.TryGetValue((name, "FUNCTION"), out var u))
        {
            var sym = new Symbol(name);
            return DefinitionPlist(new DefinitionEntry(sym, "FUNCTION", ""), u.File, u.Line, sym);
        }
        return null;
    }

    /// <summary>
    /// The recorded definitions a debugger frame named NAME may belong to. A
    /// frame keeps only the function's name as a string, without its package,
    /// so this matches on the symbol name and keeps the fbound ones. When
    /// several packages define a function of that name, the one accessible in
    /// the current package comes first and alone if it is among them; the rest
    /// are returned for the caller to list. Empty when nothing was recorded.
    /// </summary>
    internal static List<(Symbol Sym, string File, int Line)> DefinitionSourcesForFrame(string name)
    {
        ResolveFaslDefinitionSources();
        var found = new List<(Symbol, string, int)>();
        foreach (var kv in s_defFunctionSources)
            if (kv.Key.Name == name && kv.Key.IsFBound)
                found.Add((kv.Key, kv.Value.File, kv.Value.Line));
        if (found.Count > 1
            && DynamicBindings.Get(Startup.Sym("*PACKAGE*")) is Package pkg
            && pkg.FindSymbol(name).symbol is Symbol visible)
        {
            foreach (var f in found)
                if (ReferenceEquals(f.Item1, visible))
                    return new List<(Symbol, string, int)> { f };
        }
        found.Sort((a, b) => string.CompareOrdinal(
            a.Item1.HomePackage?.Name ?? "", b.Item1.HomePackage?.Name ?? ""));
        return found;
    }

    /// <summary>
    /// Identity-keyed map from a read form (a cons) to the source line where its
    /// opening paren appeared. Populated by the Reader only while non-null, which
    /// COMPILE-FILE arranges under DOTCL_EMIT_PDB so the compiler can attach
    /// per-expression sequence points. Off (null) -> the Reader does no work.
    /// Thread-static: compile is single-threaded per file and this avoids sharing.
    /// </summary>
    // Reference types, not value tuples: a framework generic over a value type is not
    // precompiled in a ReadyToRun image and is compiled by the JIT at startup.
    internal sealed class SourceSpan
    {
        public readonly int sl, sc, el, ec;
        public SourceSpan(int sl, int sc, int el, int ec) { this.sl = sl; this.sc = sc; this.el = el; this.ec = ec; }
    }
    internal sealed class SourceRange
    {
        public readonly int start, end;
        public SourceRange(int start, int end) { this.start = start; this.end = end; }
    }

    [ThreadStatic] internal static Dictionary<object, SourceSpan>? SourceLineTable;

    /// <summary>Identity-keyed start and end character offsets for the same forms,
    /// recorded by the Reader beside SourceLineTable.</summary>
    [ThreadStatic] internal static Dictionary<object, SourceRange>? SourcePositionTable;

    /// <summary>Record FORM's source span (start line/col .. end line/col) if line
    /// tracking is active. Called by the Reader for each list it builds.</summary>
    internal static void StampSourceLine(LispObject form, int sl, int sc, int el, int ec)
    {
        var t = SourceLineTable;
        if (t != null) t[form] = new SourceSpan(sl, sc, el, ec);
    }

    internal static void StampSourcePosition(LispObject form, int start, int end)
    {
        var t = SourcePositionTable;
        if (t != null) t[form] = new SourceRange(start, end);
    }

    /// <summary>The function COMPILE-FILE installs in the compiler's
    /// *EMIT-SOURCE-LINES* under DOTCL_EMIT_PDB: (fn form) -> the form's source span
    /// as a list (start-line start-col end-line end-col), or NIL. The compiler
    /// funcalls it per form; NIL means "no source position" (e.g. a
    /// macroexpansion-produced form), which correctly yields no sequence point.</summary>
    public static LispObject FormLineLookup(LispObject[] args)
    {
        if (args.Length < 1) return Nil.Instance;
        var t = SourceLineTable;
        if (t != null && t.TryGetValue(args[0], out var span))
            return new Cons(Fixnum.Make(span.sl),
                   new Cons(Fixnum.Make(span.sc),
                   new Cons(Fixnum.Make(span.el),
                   new Cons(Fixnum.Make(span.ec), Nil.Instance))));
        return Nil.Instance;
    }

    /// <summary>
    /// DOTCL:RECORD-DEFINITION-SOURCES (form file line): run the same definition
    /// walker LOAD/COMPILE-FILE use on FORM, attributing any definitions to
    /// FILE:LINE. Lets a tool that compiles a single form outside LOAD/COMPILE-FILE
    /// (e.g. swank-compile-string on C-c C-c, which receives the buffer filename and
    /// line) populate DOTCL:FUNCTION-SOURCE-LOCATION. Returns FORM unchanged.
    /// </summary>
    public static LispObject RecordDefinitionSourcesForm(LispObject[] args)
    {
        if (args.Length < 3) return args.Length >= 1 ? args[0] : Nil.Instance;
        var form = args[0];
        if (args[1] is LispString file && args[2] is Fixnum line)
            RecordDefinitionSources(form, file.Value, (int)line.Value);
        return form;
    }

    /// <summary>Try to macroexpand-1 a form. Returns expanded form or null if not a macro/expansion fails.</summary>
    private static LispObject? TryMacroexpand1(LispObject form)
    {
        if (form is not Cons c || c.Car is not Symbol sym)
            return null;
        try
        {
            // For DEFSTRUCT: let the Lisp compiler's find-macro-expander handle
            // it. The C# macro function table might contain
            // SBCL's broken 12MB expansion from src/code/defstruct.lisp.
            // DEFKNOWN: SBCL macro that calls split-type-info at expansion time.
            // C# expansion may succeed but produce forms that fail in the Lisp compiler.
            // Skip to let Lisp compiler handle macro expansion + eval in one step.
            if (sym.Name == "DEFSTRUCT" || sym.Name == "DEFKNOWN")
                return null;
            // Check runtime macro table
            var runtimeMacroFn = Runtime.MacroFunction(sym);
            if (runtimeMacroFn is LispFunction rmf)
            {
                // This expansion is a trial: when it fails, the form goes to the
                // Lisp compiler, which expands it again and decides what the
                // failure means (inside COMPILE-FILE it is reported and the form
                // compiled to signal at load time). So an ERROR here must not be
                // seen by the handlers around COMPILE-FILE -- the caller's
                // HANDLER-CASE took it at the signal and COMPILE-FILE never
                // returned. Its own handler, innermost, ends the trial instead.
                var tag = new object();
                var handler = new LispFunction(
                    hargs => throw new HandlerCaseInvocationException(
                        tag, 0, hargs.Length > 0 ? hargs[0] : Nil.Instance),
                    "%TRIAL-MACROEXPAND", -1);
                HandlerClusterStack.PushCluster(new[] { new HandlerBinding(Startup.Sym("ERROR"), handler) });
                try
                {
                    var result = rmf.Invoke(new LispObject[] { form, Nil.Instance });
                    // Macro functions in CL return single value; unwrap MvReturn if present
                    return result is MvReturn mv ? (mv.PrimaryValue) : result;
                }
                catch (HandlerCaseInvocationException ex) when (ReferenceEquals(ex.Tag, tag))
                {
                    // The compiler signals this condition again for the same form
                    // instead of running the expander a second time.
                    if (DynamicBindings.TryGet(Startup.Sym("*FILE-EXPANSION-FAILURES*"), out var failures)
                        && failures is LispHashTable failuresTable)
                        failuresTable.Set(form, ex.Condition);
                    return null;
                }
                finally
                {
                    HandlerClusterStack.PopCluster();
                }
            }
            // Check compiler macro table
            var compilerFn = Startup.LookupCompilerMacro(sym);
            if (compilerFn != null)
            {
                var result2 = compilerFn.Invoke(new LispObject[] { form });
                return result2 is MvReturn mv2 ? mv2.PrimaryValue : result2;
            }
        }
        catch
        {
            // Macro expansion failed (e.g. runtime state not yet set up).
            // Return null to let the Lisp compiler handle it instead.
        }
        return null;
    }
}
