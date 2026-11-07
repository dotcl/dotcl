using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Read from stream ---

    /// <summary>
    /// Resolve a LispStream to a TextReader for input operations.
    /// Handles LispInputStream, LispFileStream, LispBidirectionalStream,
    /// LispTwoWayStream, LispEchoStream, LispSynonymStream, and
    /// LispConcatenatedStream (first component).
    /// </summary>
    // Fasls already accepted as compiled by another core: not reported again.
    private static readonly HashSet<string> _staleFaslWarned = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Accept the fasl at PATH as compiled by another core: its caller has
    /// decided that once for many fasls (a library package's shipped fasls).</summary>
    public static LispObject NoteFaslGeneration(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DOTCL::%NOTE-FASL-GENERATION: wrong number of arguments: {args.Length} (expected 1)"));
        if (args[0] is not LispString s)
            throw new LispErrorException(new LispTypeError(
                "DOTCL::%NOTE-FASL-GENERATION: not a string", args[0]));
        var path = s.Value;
        lock (_staleFaslWarned)
            _staleFaslWarned.Add(Path.GetFullPath(path));
        return T.Instance;
    }

    /// <summary>Set by REQUIRE of a bundled contrib for the fasl it loads next: a stale
    /// contrib fasl is a development tree's leftover, so it is warned about and loaded.</summary>
    [ThreadStatic] private static bool s_nextFaslMismatchWarnsOnly;

    /// <summary>Signal DOTCL:FASL-VERSION-MISMATCH when ASM was compiled by another
    /// dotcl: one with another core generation (restartable: LOAD-AS-IS / CONTINUE
    /// return and the load goes on), or one that needs a newer DotCL.Runtime than
    /// this one (not restartable: .NET cannot bind it). A fasl with no stamp is
    /// left alone. Also checks the packages ASM names (CheckFaslUnit): they ride on
    /// the same stamp, after a newline. CHECKGENERATION is false for an assembly
    /// compiled in this process.</summary>
    private static void CheckFaslStamp(System.Reflection.Assembly asm, string faslPath,
        bool warnOnly, bool checkGeneration = true)
    {
        if (checkGeneration)
        {
            var running = typeof(Runtime).Assembly.GetName().Version;
            foreach (var r in asm.GetReferencedAssemblies())
                if (r.Name == "DotCL.Runtime" && r.Version != null && running != null
                    && r.Version > running)
                    SignalNewerRuntimeFasl(faslPath, r.Version.ToString(), running.ToString());
        }

        string? stamped = null;
        string? hostRecord = null;
        try
        {
            foreach (var a in asm.GetCustomAttributes<System.Reflection.AssemblyMetadataAttribute>())
            {
                if (a.Key == Startup.CoreGenerationKey) stamped = a.Value;
                else if (a.Key == HostFeatures.MetadataKey) hostRecord = a.Value;
            }
        }
        catch { return; }
        if (checkGeneration && hostRecord != null) WarnHostFeatureMismatch(faslPath, hostRecord);
        if (stamped == null) return;
        int nl = stamped.IndexOf('\n');
        if (nl >= 0)
        {
            var entries = stamped.Substring(nl + 1);
            stamped = nl == 0 ? null : stamped.Substring(0, nl);
            if (Package._unitsInUse || entries.IndexOf('\t') >= 0) CheckFaslUnit(faslPath, entries);
        }
        if (!checkGeneration || stamped == null) return;
        string? current;
        try { current = Startup.CoreGeneration(); }
        catch { return; }
        if (current == null || stamped == current) return;
        lock (_staleFaslWarned)
            if (_staleFaslWarned.Contains(faslPath)) return;
        // Only another compiler: the old code still binds, so a contrib loads.
        if (warnOnly && Startup.RuntimePart(stamped) == Startup.RuntimePart(current))
        {
            lock (_staleFaslWarned) _staleFaslWarned.Add(faslPath);
            Console.Error.WriteLine(
                $";; warning: {faslPath} was compiled by a different dotcl core " +
                $"(fasl {stamped}, running {current}).");
            return;
        }
        var tag = new object();
        var loadAsIsSym = Startup.DotclPkg.FindSymbol("LOAD-AS-IS").Item1;
        const string report = "Load the fasl as it is.";
        RestartClusterStack.PushCluster(new[] {
            new LispRestart("LOAD-AS-IS", _ => Nil.Instance, report, tag) { NameSymbol = loadAsIsSym },
            new LispRestart("CONTINUE", _ => Nil.Instance, report, tag) });
        try
        {
            SignalFaslVersionMismatch(faslPath, stamped,
                "~A was compiled by a different dotcl core (fasl ~A, running ~A).",
                new LispString(faslPath), new LispString(stamped), new LispString(current));
        }
        catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, tag))
        {
            lock (_staleFaslWarned) _staleFaslWarned.Add(faslPath);
        }
        finally
        {
            RestartClusterStack.PopCluster();
        }
    }

    /// <summary>Warn (DOTCL:FASL-HOST-MISMATCH) when a feature the fasl's source chose
    /// by #+ / #- has another truth on this host than where it was compiled. The load
    /// goes on. Silenced by binding DOTCL:*WARN-HOST-DEPENDENT-FASL* to NIL.</summary>
    private static void WarnHostFeatureMismatch(string faslPath, string record)
    {
        var flag = Startup.DotclPkg.FindSymbol("*WARN-HOST-DEPENDENT-FASL*").Item1;
        if (flag == null || !IsTruthy(DynamicBindings.Get(flag))) return;
        var diff = HostFeatures.Differences(record);
        if (diff.Count == 0) return;
        var text = string.Join(", ", diff.Select(d =>
            $"{d.Name} (compiled: {(d.Compiled ? "true" : "false")}, here: {(d.Compiled ? "false" : "true")})"));
        LispObject names = Nil.Instance;
        for (int i = diff.Count - 1; i >= 0; i--)
            names = new Cons(Startup.Keyword(diff[i].Name), names);
        LispWarnFormat(new LispObject[] {
            Startup.DotclPkg.FindSymbol("FASL-HOST-MISMATCH").Item1!,
            Startup.Keyword("FORMAT-CONTROL"), new LispString(
                "~A was compiled on a host with other features than this one: ~A. The definitions its #+ / #- chose may be wrong here."),
            Startup.Keyword("FORMAT-ARGUMENTS"),
                new Cons(new LispString(faslPath), new Cons(new LispString(text), Nil.Instance)),
            Startup.Keyword("PATHNAME"), LispPathname.FromString(faslPath),
            Startup.Keyword("FEATURES"), names });
    }

    /// <summary>The host-dependent features recorded in the fasl at PATH, as an alist
    /// of (keyword . compile-time truth), read from its metadata without loading it.
    /// NIL when it has no record.</summary>
    internal static LispObject FaslHostFeatures(string path)
    {
#if NETSTANDARD2_0
        return Nil.Instance;
#else
        try
        {
            using var fs = File.OpenRead(path);
            using var pe = new System.Reflection.PortableExecutable.PEReader(fs);
            if (!pe.HasMetadata) return Nil.Instance;
            var md = System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader(pe);
            foreach (var h in md.GetAssemblyDefinition().GetCustomAttributes())
            {
                var blob = md.GetBlobReader(md.GetCustomAttribute(h).Value);
                if (blob.Length < 2 || blob.ReadUInt16() != 1) continue;
                string? key, value;
                try { key = blob.ReadSerializedString(); value = blob.ReadSerializedString(); }
                catch { continue; }
                if (key != HostFeatures.MetadataKey || value == null) continue;
                var items = HostFeatures.Decode(value);
                LispObject result = Nil.Instance;
                for (int i = items.Count - 1; i >= 0; i--)
                    result = new Cons(new Cons(Startup.Keyword(items[i].Name),
                        items[i].Compiled ? T.Instance : Nil.Instance), result);
                return result;
            }
        }
        catch { }
        return Nil.Instance;
#endif
    }

    /// <summary>Signal DOTCL:FASL-VERSION-MISMATCH (restartable as for another core)
    /// when a package ASM names would be another package here than where it was
    /// compiled: a fasl compiled in a package unit and loaded outside it, or the
    /// other way round.</summary>
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static void CheckFaslUnit(string faslPath, string entries)
    {
        if (Package.FaslUnitMismatch(entries) is not { } m) return;
        var tag = new object();
        var loadAsIsSym = Startup.DotclPkg.FindSymbol("LOAD-AS-IS").Item1;
        const string report = "Load the fasl as it is.";
        RestartClusterStack.PushCluster(new[] {
            new LispRestart("LOAD-AS-IS", _ => Nil.Instance, report, tag) { NameSymbol = loadAsIsSym },
            new LispRestart("CONTINUE", _ => Nil.Instance, report, tag) });
        try
        {
            SignalFaslVersionMismatch(faslPath, m.Compiled,
                "~A was compiled for another package unit: its package ~A is ~A there and ~A here.",
                new LispString(faslPath), new LispString(m.Text),
                new LispString(m.Compiled), new LispString(m.Here));
        }
        catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, tag)) { }
        finally
        {
            RestartClusterStack.PopCluster();
        }
    }

    private static void SignalNewerRuntimeFasl(string faslPath, string needed, string running) =>
        SignalFaslVersionMismatch(faslPath, "DotCL.Runtime " + needed,
            "~A was compiled by a newer dotcl: it needs DotCL.Runtime ~A, and this is ~A. " +
            "It cannot be loaded as it is; compile its source again.",
            new LispString(faslPath), new LispString(needed), new LispString(running));

    private static void SignalFaslVersionMismatch(string faslPath, string builtBy,
        string control, params LispObject[] formatArgs)
    {
        LispObject argList = Nil.Instance;
        for (int i = formatArgs.Length - 1; i >= 0; i--) argList = new Cons(formatArgs[i], argList);
        LispErrorFormat(new LispObject[] {
            Startup.DotclPkg.FindSymbol("FASL-VERSION-MISMATCH").Item1!,
            Startup.Keyword("FORMAT-CONTROL"), new LispString(control),
            Startup.Keyword("FORMAT-ARGUMENTS"), argList,
            Startup.Keyword("PATHNAME"), LispPathname.FromString(faslPath),
            Startup.Keyword("BUILT-BY"), new LispString(builtBy) });
    }

    private static string RunningRuntimeVersion() =>
        typeof(Runtime).Assembly.GetName().Version?.ToString() ?? "unknown";

    private static string NeededRuntimeVersion(Exception ex)
    {
        var name = ex switch { FileLoadException f => f.FileName, FileNotFoundException f => f.FileName, _ => null };
        try { return new AssemblyName(name!).Version?.ToString() ?? "?"; }
        catch { return "?"; }
    }

    /// <summary>.NET failing to bind DotCL.Runtime, which the reference check in
    /// CheckFaslStamp did not foresee.</summary>
    private static bool IsNewerRuntimeBindFailure(Exception ex) =>
        (ex switch { FileLoadException f => f.FileName, FileNotFoundException f => f.FileName, _ => null })
            is string name && name.StartsWith("DotCL.Runtime,", StringComparison.Ordinal);

    private static LispError MakeStreamError(LispObject stream, string op) =>
        new LispError($"{op}: stream is closed") { ConditionTypeName = "STREAM-ERROR", StreamErrorStreamRef = stream };

    /// <summary>A TextReader that spans a concatenated-stream's component streams:
    /// when the current component reaches EOF it advances to the next, so a single
    /// reader (e.g. the token reader behind READ) reads continuously across the
    /// whole concatenation instead of stopping at the first component's EOF.
    /// Advances the shared CurrentIndex, consistent with read-char/peek-char.</summary>
    private sealed class ConcatenatedTextReader : TextReader
    {
        private readonly LispConcatenatedStream _cs;
        public ConcatenatedTextReader(LispConcatenatedStream cs) => _cs = cs;
        private TextReader? Current()
        {
            while (_cs.CurrentIndex < _cs.Streams.Length)
            {
                var r = GetTextReader(_cs.Streams[_cs.CurrentIndex]);
                if (r.Peek() != -1) return r;
                _cs.CurrentIndex++;
            }
            return null;
        }
        public override int Peek() => Current()?.Peek() ?? -1;
        public override int Read()
        {
            while (true)
            {
                var r = Current();
                if (r == null) return -1;
                int c = r.Read();
                if (c != -1) return c;
                _cs.CurrentIndex++; // component exhausted between Peek and Read; advance
            }
        }
    }

    public static TextReader GetTextReader(LispObject stream)
    {
        while (true)
        {
            switch (stream)
            {
                case LispInputStream ins:
                    if (ins.IsClosed) throw new LispErrorException(MakeStreamError(stream, "READ"));
                    return ins.Reader;
                case LispFileStream fs when fs.IsInput:
                    if (fs.IsClosed) throw new LispErrorException(MakeStreamError(stream, "READ"));
                    return fs.InputReader!;
                case LispBidirectionalStream bidi:
                    if (bidi.IsClosed) throw new LispErrorException(MakeStreamError(stream, "READ"));
                    return bidi.Reader;
                case LispTwoWayStream tws:
                    stream = tws.InputStream;
                    continue;
                case LispEchoStream echo:
                    stream = echo.InputStream;
                    continue;
                case LispSynonymStream syn:
                    stream = DynamicBindings.Get(syn.Symbol);
                    continue;
                case LispConcatenatedStream concat:
                    // Span all components so the token reader (READ) crosses component
                    // boundaries instead of stopping at the first component's EOF.
                    return new ConcatenatedTextReader(concat);
                case T:
                    stream = DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*"));
                    continue;
                case Nil:
                    stream = DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                    continue;
                // A binary Gray input stream too, as on SBCL: READ-CHAR reaches its
                // STREAM-READ-CHAR. PEEK-CHAR, READ-LINE and LISTEN reach their own
                // generic functions, whose defaults are for character streams only.
                case LispInstance gi when IsGrayStream(gi):
                    return new GrayStreamTextReader(gi);
                case LispStream:
                case LispInstance:
                    return Console.In;
                default:
                    throw new LispErrorException(NotInputStreamDesignator(stream));
            }
        }
    }

    public static TextWriter GetTextWriter(LispObject stream)
    {
        while (true)
        {
            switch (stream)
            {
                case LispOutputStream outs:
                    if (outs.IsClosed) throw new LispErrorException(MakeStreamError(stream, "WRITE"));
                    return outs.Writer;
                case LispFileStream fs when fs.IsOutput:
                    if (fs.IsClosed) throw new LispErrorException(MakeStreamError(stream, "WRITE"));
                    return fs.OutputWriter!;
                case LispBidirectionalStream bidi:
                    if (bidi.IsClosed) throw new LispErrorException(MakeStreamError(stream, "WRITE"));
                    return bidi.Writer;
                case LispTwoWayStream tws: stream = tws.OutputStream; continue;
                case LispEchoStream es: stream = es.OutputStream; continue;
                case LispSynonymStream syn: stream = DynamicBindings.Get(syn.Symbol); continue;
                case LispBroadcastStream bc:
                    if (bc.Streams.Length == 0) return TextWriter.Null;
                    if (bc.Streams.Length == 1) { stream = bc.Streams[0]; continue; }
                    var bcWriters = new TextWriter[bc.Streams.Length];
                    for (int i = 0; i < bc.Streams.Length; i++) bcWriters[i] = GetTextWriter(bc.Streams[i]);
                    return new BroadcastTextWriter(bcWriters);
                // A character gray output stream, or a binary (bivalent) gray output
                // stream that defines stream-write-char: route character output to the
                // gray protocol. CLHS/SBCL dispatch write-char (and write-string/format)
                // to stream-write-char regardless of element-type, so a binary gray
                // stream with a stream-write-char method is writable as characters.
                // (write-byte uses a separate path, so pure-binary streams are unaffected
                // until a character is actually written, where erroring is correct.)
                case LispInstance gi when IsGrayStream(gi):
                    return new GrayStreamTextWriter(gi);
                // Input-only streams and plain data are not output stream designators;
                // writing to them used to land on the console.
                case LispInstance:
                    return Console.Out;
                default:
                    throw new LispErrorException(new LispTypeError(
                        $"{FormatObject(stream, true)} is not an output stream", stream, Startup.Sym("STREAM")));
            }
        }
    }

    // detect Gray streams by class NAME walked over the precedence list,
    // not by FindClassByName + reference compare. FindClassByName does a linear
    // scan of the class registry and returns the FIRST class whose bare name
    // matches: so when a second class of the same name exists (e.g. after
    // (asdf:load-system :trivial-gray-streams), which defines its own
    // FUNDAMENTAL-CHARACTER-OUTPUT-STREAM in another package), it can return a
    // class that is NOT the one in this instance's CPL, making the reference
    // compare wrongly false (order-dependent). Matching by name is robust to
    // duplicate-named gray protocol classes across packages.

    internal const int GrayCharOut = 1, GrayCharIn = 2, GrayBinIn = 4, GrayBinOut = 8, GrayAny = 16, GrayChar = 32;

    internal static int GrayKinds(LispClass cls)
    {
        var cpl = cls.ClassPrecedenceList;
        if (cpl == null) return 0;
        var cache = cls.GrayKindCache;
        if (cache != null && ReferenceEquals(cache.Cpl, cpl)) return cache.Kinds;
        int kinds = 0;
        bool anyIn = false, anyOut = false;
        foreach (var c in cpl)
        {
            switch (c.Name?.Name)
            {
                case "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM": kinds |= GrayCharOut; break;
                case "FUNDAMENTAL-CHARACTER-INPUT-STREAM": kinds |= GrayCharIn; break;
                case "FUNDAMENTAL-BINARY-INPUT-STREAM": kinds |= GrayBinIn; break;
                case "FUNDAMENTAL-BINARY-OUTPUT-STREAM": kinds |= GrayBinOut; break;
                case "FUNDAMENTAL-INPUT-STREAM": anyIn = true; break;
                case "FUNDAMENTAL-OUTPUT-STREAM": anyOut = true; break;
                case "FUNDAMENTAL-STREAM": kinds |= GrayAny; break;
                case "FUNDAMENTAL-CHARACTER-STREAM": kinds |= GrayChar; break;
            }
        }
        // A stream that is only a FUNDAMENTAL-INPUT/OUTPUT-STREAM (fast-io's) names
        // no element kind; byte and sequence I/O still go to the Gray generics, as on SBCL.
        if (anyIn && (kinds & (GrayCharIn | GrayBinIn)) == 0) kinds |= GrayBinIn;
        if (anyOut && (kinds & (GrayCharOut | GrayBinOut)) == 0) kinds |= GrayBinOut;
        cls.GrayKindCache = new LispClass.GrayKindEntry(cpl, kinds);
        return kinds;
    }

    /// <summary>Call FN (a Gray stream generic, PRINT-OBJECT, a pprint-dispatch
    /// function) for its primary value only: an MvReturn is opened, and the values
    /// FN left in the channel are dropped so the caller's caller does not take them
    /// for its own.</summary>
    internal static LispObject CallForPrimary(LispFunction? fn, params LispObject[] args)
    {
        if (fn == null) return Nil.Instance;
        var r = fn.Invoke(args);
        if (r is MvReturn mv) r = mv.PrimaryValue;
        MultipleValues.Reset();
        return r;
    }

    /// <summary>An instance of FUNDAMENTAL-STREAM, whatever its direction and element classes.</summary>
    internal static bool IsGrayStream(LispInstance inst)
        => GrayKinds(inst.Class) != 0;

    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispInstance, object> GrayClosed = new();

    internal static void MarkGrayClosed(LispInstance inst) => GrayClosed.GetValue(inst, static _ => T.Instance);

    internal static bool IsGrayClosed(LispInstance inst) => GrayClosed.TryGetValue(inst, out _);

    /// <summary>Check if a LispInstance is a Gray output stream (subclass of fundamental-character-output-stream).</summary>
    internal static bool IsGrayOutputStream(LispInstance inst)
        => (GrayKinds(inst.Class) & GrayCharOut) != 0;

    /// <summary>Check if a LispInstance is a Gray input stream (subclass of fundamental-character-input-stream).</summary>
    internal static bool IsGrayInputStream(LispInstance inst)
        => (GrayKinds(inst.Class) & GrayCharIn) != 0;

    /// <summary>A Gray input stream of either kind. Character reads (READ-CHAR,
    /// PEEK-CHAR, UNREAD-CHAR, READ-CHAR-NO-HANG) go to the Gray generics for both,
    /// as on SBCL.</summary>
    internal static bool IsGrayAnyInputStream(LispInstance inst)
        => (GrayKinds(inst.Class) & (GrayCharIn | GrayBinIn)) != 0;

    /// <summary>Check if a LispInstance is a Gray binary input stream (subclass of fundamental-binary-input-stream).
    /// read-byte/read-sequence dispatch to the stream-read-byte generic on these.</summary>
    internal static bool IsGrayBinaryInputStream(LispInstance inst)
        => (GrayKinds(inst.Class) & GrayBinIn) != 0;

    /// <summary>Check if a LispInstance is a Gray binary output stream (subclass of fundamental-binary-output-stream).
    /// write-byte/write-sequence dispatch to the stream-write-byte generic on these.</summary>
    internal static bool IsGrayBinaryOutputStream(LispInstance inst)
        => (GrayKinds(inst.Class) & GrayBinOut) != 0;

    /// <summary>Resolve a stream designator to the outermost LispEchoStream, if any, following synonym streams.</summary>
    private static LispEchoStream? FindEchoStream(LispObject streamObj)
    {
        while (true)
        {
            switch (streamObj)
            {
                case LispSynonymStream syn:
                    streamObj = DynamicBindings.Get(syn.Symbol);
                    continue;
                case LispEchoStream es:
                    return es;
                default:
                    return null;
            }
        }
    }

    public static LispObject ReadFromStream(LispObject stream, LispObject eofErrorP, LispObject eofValue)
        => ReadFromStreamRecursive(stream, eofErrorP, eofValue, false);

    public static LispObject ReadFromStreamRecursive(LispObject stream, LispObject eofErrorP, LispObject eofValue,
                                                     bool recursiveP)
    {
        var value = GuardStreamIO(stream, stream, eofErrorP, eofValue, recursiveP,
                                  static (st, ee, ev, rp) => ReadFromStreamUnguarded(st, ee, ev, rp));
        // READ returns exactly one value. Inside, "no value" is how a reader macro
        // that consumed only whitespace or a comment reports itself, and a Lisp-level
        // macro function (what GET-MACRO-CHARACTER hands back, so every readtable
        // merged from the standard one) says that by publishing ZERO values. That is
        // an internal protocol; letting it out made a READ whose input merely started
        // with a comment return no values at all.
        MultipleValues.Reset();
        return value;
    }

    private static LispObject ReadFromStreamUnguarded(LispObject stream, LispObject eofErrorP,
                                                      LispObject eofValue, bool recursiveP)
    {
        Reader lispReader;
        if (stream is LispStream ls2 && ls2.CachedReader != null)
        {
            lispReader = ls2.CachedReader;
        }
        else
        {
            TextReader reader = GetReaderTextReader(stream);
            lispReader = new Reader(reader) { LispStreamRef = stream };
            if (stream is LispStream ls3)
            {
                ls3.CachedReader = lispReader;
                lispReader.AdoptStreamShareTables(ls3);
            }
        }
        // Transfer any unread char from the LispStream to the Reader's pushback,
        // so that (unread-char ch stream) followed by (read stream) works correctly.
        if (stream is LispStream ls && ls.UnreadCharValue != -1)
        {
            lispReader.UnreadChar(ls.UnreadCharValue);
            ls.UnreadCharValue = -1;
        }
        using var shareScope = !recursiveP && lispReader.InRead ? lispReader.PushShareScope() : null;
        if (lispReader.TryRead(out var result))
        {
            // CLHS: read (not read-preserving-whitespace) consumes
            // one trailing whitespace character after a token
            if (lispReader.WhitespaceTerminated)
                lispReader.ConsumeOneWhitespace();
            lispReader.ReturnLookaheadToStream(stream);
            return result;
        }
        // EOF before any form began: this is the one eof-error-p governs. An end
        // of file in the middle of an object signals END-OF-FILE from the reader
        // itself and passes through here regardless (CLHS READ).
        if (eofErrorP is not Nil)
            { var eof = new LispError("READ: end of file"); eof.ConditionTypeName = "END-OF-FILE"; eof.StreamErrorStreamRef = stream; throw new LispErrorException(eof); }
        return eofValue;
    }

    /// <summary>Make #! a line comment in the current readtable (shebang lines of scripts). False when
    /// *READTABLE* does not hold a readtable.</summary>
    public static bool InstallShebangComment()
    {
        if (DynamicBindings.Get(Startup.Sym("*READTABLE*")) is not LispReadtable rt) return false;
        rt.SetDispatchMacroCharacter('#', '!', (r, c, n) => r.ReadLineComment(c));
        return true;
    }

    public static LispObject ReadFromString(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("READ-FROM-STRING: requires at least 1 argument"));
        string str;
        if (args[0] is LispString s)
            str = s.Value;
        else if (args[0] is LispVector vec && vec.IsCharVector)
            str = vec.ToCharString();
        else
            throw new LispErrorException(new LispTypeError("READ-FROM-STRING: not a string", args[0]));

        // Optional positional args: eof-error-p and eof-value
        // CLHS: read-from-string string &optional eof-error-p eof-value &key ...
        LispObject eofErrorP = T.Instance;
        LispObject eofValue = Nil.Instance;
        if (args.Length > 1) eofErrorP = args[1];
        if (args.Length > 2) eofValue = args[2];
        int optIdx = Math.Min(args.Length, 3);

        // Check for odd number of keyword arguments
        int kwCount = args.Length - optIdx;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("READ-FROM-STRING: odd number of keyword arguments"));

        // Keyword args (CLHS 3.4.1.4.1: first occurrence wins for duplicate keywords)
        int start = 0;
        int end = str.Length;
        bool preserveWhitespace = false;
        bool startSet = false, endSet = false, preserveWhitespaceSet = false;
        bool allowOtherKeys = false;
        // First pass: check for :allow-other-keys
        for (int i = optIdx; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol aok && aok.Name == "ALLOW-OTHER-KEYS")
            {
                allowOtherKeys = args[i + 1] is not Nil;
                break;
            }
        }
        for (int i = optIdx; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol kw)
            {
                switch (kw.Name)
                {
                    case "START":
                        if (!startSet) { start = args[i + 1] is Fixnum fs ? Runtime.ClampIndex(fs.Value) : 0; startSet = true; }
                        break;
                    case "END":
                        if (!endSet) { if (args[i + 1] is Fixnum fe) end = Runtime.ClampIndex(fe.Value); endSet = true; }
                        break;
                    case "PRESERVE-WHITESPACE":
                        if (!preserveWhitespaceSet) { preserveWhitespace = args[i + 1] is not Nil; preserveWhitespaceSet = true; }
                        break;
                    case "ALLOW-OTHER-KEYS":
                        break;
                    default:
                        if (!allowOtherKeys)
                            throw new LispErrorException(new LispProgramError($"READ-FROM-STRING: unrecognized keyword argument :{kw.Name}"));
                        break;
                }
            }
            else
            {
                throw new LispErrorException(new LispProgramError($"READ-FROM-STRING: keyword argument must be a symbol, got {args[i]}"));
            }
        }

        // Copying the whole string when :start/:end were not given is a copy of
        // the input on every READ-FROM-STRING -- and callers pass whole files.
        CheckBoundingIndices(start, end, str.Length, "READ-FROM-STRING");
        string substring = (start == 0 && end == str.Length) ? str : str.Substring(start, end - start);
        // Count physical reads from the underlying string so the reported position
        // tracks consumption done via the stream API by reader macros (read-char/read
        // over a make-concatenated-stream wrap, as in babel's #\ reader), which bypass
        // the Reader's own Position counter. Both the Reader and any stream-API access
        // share this one counting reader, so position = consumed - still-buffered chars.
        // A string input stream, as the reader macros see it: FILE-POSITION
        // answers where they are in STRING (Coalton's reader takes its source
        // spans from it), and it counts what the stream API consumes too.
        var stringStream = new LispStringInputStream(new StringReader(substring), start, str, end);
        var reader = new Reader(stringStream.Reader) { LispStreamRef = stringStream };
        // The stream's live reader, so a reader macro that reads from the stream
        // again (READ, READ-DELIMITED-LIST, a copied built-in macro) continues on
        // this reader rather than a second one with its own look-ahead.
        stringStream.CachedReader = reader;
        if (reader.TryRead(out var result))
        {
            // CLHS: read-from-string uses read (consumes the single whitespace
            // character that terminates a token) unless :preserve-whitespace t
            if (!preserveWhitespace)
                reader.ConsumeOneWhitespace();
            int pending = reader.PendingLookahead + (stringStream.UnreadCharValue != -1 ? 1 : 0);
            return MultipleValues.Values2(result, Fixnum.Make(stringStream.Position - pending));
        }

        // EOF
        if (eofErrorP is not Nil)
            throw new LispErrorException(new LispError("READ-FROM-STRING: end of string") { ConditionTypeName = "END-OF-FILE", StreamErrorStreamRef = stringStream });
        return MultipleValues.Values2(eofValue, Fixnum.Make(end));
    }

    /// <summary>Read exactly one object from a string and return only it (no
    /// multiple-values side effect, unlike READ-FROM-STRING). Used by FASL-emitted
    /// IL to reconstruct a circular/shared constant literal from its *print-circle*
    /// representation at load time. Supports #N= / #N# label syntax via the
    /// standard reader.</summary>
    public static LispObject ReadConstantFromString(string s)
    {
        var stringStream = new LispInputStream(new StringReader(s));
        var reader = new Reader(stringStream.Reader) { LispStreamRef = stringStream };
        // The repr was printed with the standard syntax and fully qualified
        // symbol names (see TryEmitConstantViaReader). Read it back under the
        // standard readtable so a custom load-time *readtable* (e.g. SBCL's
        // cold-build xc-readtable, which replaces "(", digits and #+) cannot
        // distort the reconstruction.
        var rtSym = Startup.Sym("*READTABLE*");
        DynamicBindings.Push(rtSym, Startup.FaslReadtable);
        // The representation was printed under the standard default float format
        // (see TryEmitConstantViaReader), which decides which floats carry an
        // exponent marker. Reading it under whatever the loading process happens
        // to have set would turn a double into a single, or the reverse: the
        // right value with the wrong type, and no error anywhere.
        var rdffSym = Startup.Sym("*READ-DEFAULT-FLOAT-FORMAT*");
        DynamicBindings.Push(rdffSym, Startup.Sym("SINGLE-FLOAT"));
        // A literal is reconstructed the first time the code that holds it runs,
        // so the reader variables are whatever that caller has bound. Under
        // *READ-SUPPRESS* T the read returns NIL, which the site then keeps for
        // good (a reader macro function whose first call came from inside #+(or)
        // saw NIL for its literal from then on); under another *READ-BASE* the
        // digits of every integer change value. Pin both to the standard values
        // the representation was printed for.
        var rsSym = Startup.Sym("*READ-SUPPRESS*");
        DynamicBindings.Push(rsSym, Nil.Instance);
        var rbSym = Startup.Sym("*READ-BASE*");
        DynamicBindings.Push(rbSym, Fixnum.Make(10));
        try
        {
            if (reader.TryRead(out var result))
                return result;
        }
        finally
        {
            DynamicBindings.Pop(rbSym); DynamicBindings.Pop(rsSym);
            DynamicBindings.Pop(rdffSym); DynamicBindings.Pop(rtSym);
        }
        throw new LispErrorException(new LispProgramError(
            "fasl: empty representation while reconstructing a constant literal"));
    }

    /// <summary>Reconstruct a constant from its UTF-8 printed representation, held
    /// in the fasl's data section rather than in the #US string heap (see
    /// CilAssembler.EmitReprAsData).</summary>
    public static LispObject ReadConstantFromUtf8(byte[] utf8) =>
        ReadConstantFromString(System.Text.Encoding.UTF8.GetString(utf8));

    /// <summary>The uninterned symbols of the fasl whose literal is being read,
    /// indexed by the `#<n>U` syntax the printer emitted. Bound only around one
    /// read; nested reads cannot happen because reading a literal never loads
    /// another fasl.</summary>
    [ThreadStatic] public static Symbol[]? FaslUninternedTable;

    /// <summary>Read a literal that names uninterned symbols by index.</summary>
    public static LispObject ReadConstantFromUtf8(byte[] utf8, Symbol[] table)
    {
        var saved = FaslUninternedTable;
        FaslUninternedTable = table;
        try { return ReadConstantFromUtf8(utf8); }
        finally { FaslUninternedTable = saved; }
    }

    /// <summary>Same, for a representation too large for one data field.</summary>
    public static LispObject ReadConstantFromUtf8Parts(byte[][] parts, Symbol[] table)
    {
        var saved = FaslUninternedTable;
        FaslUninternedTable = table;
        try { return ReadConstantFromUtf8Parts(parts); }
        finally { FaslUninternedTable = saved; }
    }

    /// <summary>Rebuild a fasl's uninterned symbols from the names blob the
    /// emitter wrote. Netstring framing ("5:GENSY6:FOOBAR") rather than a
    /// separator, because a symbol name may contain any character at all --
    /// including whatever separator we would have picked.
    ///
    /// One array in place of one static field per symbol: a Coalton fasl had
    /// 114,451 of those fields, which is what forced literal fields to be split
    /// across holder types at 4096 apiece to stay under the 65,535-per-type
    /// limit.</summary>
    public static Symbol[] BuildUninternedTable(byte[][] parts)
    {
        int total = 0;
        foreach (var p in parts) total += p.Length;
        var all = new byte[total];
        int at = 0;
        foreach (var p in parts) { System.Array.Copy(p, 0, all, at, p.Length); at += p.Length; }
        return BuildUninternedTable(all);
    }

    // Each entry is `N:name` where N counts the name's UTF-8 BYTES (that is how
    // the compiler writes it), so the table is walked as bytes. Walking the
    // decoded text instead took N characters and misread every entry from the
    // first non-ASCII name on.
    public static Symbol[] BuildUninternedTable(byte[] utf8)
    {
        var syms = new List<Symbol>();
        int at = 0;
        while (at < utf8.Length)
        {
            int colon = Array.IndexOf(utf8, (byte)':', at);
            int len = 0;
            bool ok = colon > at;
            for (int i = at; ok && i < colon; i++)
            {
                if (utf8[i] < (byte)'0' || utf8[i] > (byte)'9') ok = false;
                else len = len * 10 + (utf8[i] - '0');
            }
            if (!ok || colon + 1 + len > utf8.Length)
                throw new LispErrorException(new LispProgramError(
                    "fasl: malformed uninterned symbol table"));
            syms.Add(new Symbol(System.Text.Encoding.UTF8.GetString(utf8, colon + 1, len), null));
            at = colon + 1 + len;
        }
        return syms.ToArray();
    }

    /// <summary>Same, for a representation too large for one data field.</summary>
    public static LispObject ReadConstantFromUtf8Parts(byte[][] parts)
    {
        int total = 0;
        foreach (var p in parts) total += p.Length;
        var all = new byte[total];
        int at = 0;
        foreach (var p in parts) { Array.Copy(p, 0, all, at, p.Length); at += p.Length; }
        return ReadConstantFromUtf8(all);
    }
}
