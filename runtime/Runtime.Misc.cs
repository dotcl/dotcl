using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // Compiler macro functions registered via define-compiler-macro.
    // ConcurrentDictionary for thread-safe concurrent DEFUN/registration.
    private static readonly ConcurrentDictionary<Symbol, LispFunction> _compilerMacroFunctions = new();
    private static readonly ConcurrentDictionary<Symbol, LispFunction> _setfCompilerMacros = new();

    /// <summary>Remove a symbol's user-defined compiler macro (define-compiler-macro),
    /// for both the plain and (setf sym) namespaces. Called when the compiler macro is
    /// cleared via (setf (compiler-macro-function name) nil) and when the function is
    /// FMAKUNBOUND, so a stale compiler macro can't keep rewriting calls to a removed /
    /// re-signatured -IMPL target after the function is killed and redefined.</summary>
    internal static void RemoveUserCompilerMacro(Symbol sym)
    {
        _compilerMacroFunctions.TryRemove(sym, out _);
        _setfCompilerMacros.TryRemove(sym, out _);
    }

    // --- Read from stream ---

    /// <summary>
    /// Resolve a LispStream to a TextReader for input operations.
    /// Handles LispInputStream, LispFileStream, LispBidirectionalStream,
    /// LispTwoWayStream, LispEchoStream, LispSynonymStream, and
    /// LispConcatenatedStream (first component).
    /// </summary>
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
                _cs.CurrentIndex++; // component exhausted between Peek and Read — advance
            }
        }
    }

    /// <summary>Wraps a TextReader and counts characters physically read. READ-FROM-STRING
    /// uses this to report position from the true consumption of the underlying string,
    /// so reader macros that consume via the stream API (e.g. babel's #\ reader delegating
    /// to the built-in over a make-concatenated-stream wrap) advance the reported position
    /// even though they bypass the Reader's own Position counter. Peek does not consume.</summary>
    private sealed class CountingTextReader : TextReader
    {
        private readonly TextReader _inner;
        public int CharsRead { get; private set; }
        public CountingTextReader(TextReader inner) => _inner = inner;
        public override int Peek() => _inner.Peek();
        public override int Read()
        {
            int c = _inner.Read();
            if (c != -1) CharsRead++;
            return c;
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
                case LispInstance gi when IsGrayInputStream(gi):
                    return new GrayStreamTextReader(gi);
                default:
                    return Console.In;
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
                case LispInstance gi when IsGrayOutputStream(gi) || IsGrayBinaryOutputStream(gi):
                    return new GrayStreamTextWriter(gi);
                default: return Console.Out;
            }
        }
    }

    // detect Gray streams by class NAME walked over the precedence list,
    // not by FindClassByName + reference compare. FindClassByName does a linear
    // scan of the class registry and returns the FIRST class whose bare name
    // matches — so when a second class of the same name exists (e.g. after
    // (asdf:load-system :trivial-gray-streams), which defines its own
    // FUNDAMENTAL-CHARACTER-OUTPUT-STREAM in another package), it can return a
    // class that is NOT the one in this instance's CPL, making the reference
    // compare wrongly false (order-dependent). Matching by name is robust to
    // duplicate-named gray protocol classes across packages.

    /// <summary>Check if a LispInstance is a Gray output stream (subclass of fundamental-character-output-stream).</summary>
    internal static bool IsGrayOutputStream(LispInstance inst)
        => inst.Class.ClassPrecedenceList?.Any(
               c => c.Name?.Name == "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM") == true;

    /// <summary>Check if a LispInstance is a Gray input stream (subclass of fundamental-character-input-stream).</summary>
    internal static bool IsGrayInputStream(LispInstance inst)
        => inst.Class.ClassPrecedenceList?.Any(
               c => c.Name?.Name == "FUNDAMENTAL-CHARACTER-INPUT-STREAM") == true;

    /// <summary>Check if a LispInstance is a Gray binary input stream (subclass of fundamental-binary-input-stream).
    /// read-byte/read-sequence dispatch to the stream-read-byte generic on these.</summary>
    internal static bool IsGrayBinaryInputStream(LispInstance inst)
        => inst.Class.ClassPrecedenceList?.Any(
               c => c.Name?.Name == "FUNDAMENTAL-BINARY-INPUT-STREAM") == true;

    /// <summary>Check if a LispInstance is a Gray binary output stream (subclass of fundamental-binary-output-stream).
    /// write-byte/write-sequence dispatch to the stream-write-byte generic on these.</summary>
    internal static bool IsGrayBinaryOutputStream(LispInstance inst)
        => inst.Class.ClassPrecedenceList?.Any(
               c => c.Name?.Name == "FUNDAMENTAL-BINARY-OUTPUT-STREAM") == true;

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
    {
        Reader lispReader;
        if (stream is LispStream ls2 && ls2.CachedReader != null)
        {
            lispReader = ls2.CachedReader;
        }
        else
        {
            TextReader reader = GetTextReader(stream);
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
        try
        {
            if (lispReader.TryRead(out var result))
            {
                // CLHS: read (not read-preserving-whitespace) consumes
                // one trailing whitespace character after a token
                if (lispReader.WhitespaceTerminated)
                    lispReader.ConsumeOneWhitespace();
                return result;
            }
            // EOF
            if (eofErrorP is not Nil)
                { var eof = new LispError("READ: end of file"); eof.ConditionTypeName = "END-OF-FILE"; eof.StreamErrorStreamRef = stream; throw new LispErrorException(eof); }
            return eofValue;
        }
        catch (EndOfStreamException)
        {
            if (eofErrorP is not Nil)
                throw;
            return eofValue;
        }
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
                        if (!startSet) { start = args[i + 1] is Fixnum fs ? (int)fs.Value : 0; startSet = true; }
                        break;
                    case "END":
                        if (!endSet) { if (args[i + 1] is Fixnum fe) end = (int)fe.Value; endSet = true; }
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

        string substring = str.Substring(start, end - start);
        // Count physical reads from the underlying string so the reported position
        // tracks consumption done via the stream API by reader macros (read-char/read
        // over a make-concatenated-stream wrap, as in babel's #\ reader), which bypass
        // the Reader's own Position counter. Both the Reader and any stream-API access
        // share this one counting reader, so position = consumed - still-buffered chars.
        var counting = new CountingTextReader(new StringReader(substring));
        var stringStream = new LispInputStream(counting);
        var reader = new Reader(stringStream.Reader) { LispStreamRef = stringStream };
        if (reader.TryRead(out var result))
        {
            // CLHS: read-from-string uses read (consumes the single whitespace
            // character that terminates a token) unless :preserve-whitespace t
            if (!preserveWhitespace)
            {
                int ch = reader.Peek();
                if (ch != -1 && char.IsWhiteSpace((char)ch))
                    reader.ReadChar();
            }
            int pending = reader.PendingLookahead + (stringStream.UnreadCharValue != -1 ? 1 : 0);
            return MultipleValues.Values(result, Fixnum.Make(start + counting.CharsRead - pending));
        }

        // EOF
        if (eofErrorP is not Nil)
            throw new LispErrorException(new LispError("READ-FROM-STRING: end of string") { ConditionTypeName = "END-OF-FILE", StreamErrorStreamRef = stringStream });
        return MultipleValues.Values(eofValue, Fixnum.Make(end));
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
        DynamicBindings.Push(rtSym, Startup.StandardReadtable);
        try
        {
            if (reader.TryRead(out var result))
                return result;
        }
        finally { DynamicBindings.Pop(rtSym); }
        throw new LispErrorException(new LispProgramError(
            "fasl: empty representation while reconstructing a constant literal"));
    }

    // --- Modules (provide/require) ---

    // Serialize provide/require so concurrent (require "x") calls don't
    // double-load the same module. _modulesLock is process-wide; a thread
    // doing (require "x") may invoke user code (the provider) while
    // holding the lock, so all other concurrent require/provide calls
    // wait. This is heavy but correct; finer-grained per-module locking
    // is reserved for a follow-up if contention shows up.
    private static readonly object _modulesLock = new();

    public static LispObject Provide(LispObject moduleName)
    {
        string name = ToStringDesignator(moduleName, "PROVIDE");
        // Add to *modules* if not already present
        var modulesSym = Startup.Sym("*MODULES*");
        lock (_modulesLock)
        {
            LispObject modules = DynamicBindings.Get(modulesSym);
            // Check if already present
            LispObject cur = modules;
            while (cur is Cons c)
            {
                if (c.Car is LispString s && s.Value == name) return T.Instance;
                cur = c.Cdr;
            }
            DynamicBindings.Set(modulesSym, new Cons(new LispString(name), modules));
            return T.Instance;
        }
    }

    public static LispObject Require(LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("REQUIRE: missing module name"));
        string name = ToStringDesignator(args[0], "REQUIRE");
        var modulesSym = Startup.Sym("*MODULES*");

        // Hold _modulesLock for the entire snapshot/load/republish sequence
        // so concurrent (require "x") calls cannot both fall past the
        // already-loaded check and invoke the provider twice.
        lock (_modulesLock)
        {

        // Snapshot *modules* so we can return the set-difference (SBCL
        // convention: REQUIRE returns the list of modules newly provided).
        var before = new HashSet<string>();
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s) before.Add(s.Value);

        // Already in *modules* — nothing to do, empty diff = NIL.
        if (before.Contains(name)) return Nil.Instance;

        // If pathname-list provided, load them
        if (args.Length > 1 && args[1] is not Nil)
        {
            var pathnames = args[1];
            if (pathnames is LispString ps)
                Load(new LispObject[] { ps });
            else if (pathnames is LispPathname pp)
                Load(new LispObject[] { pp });
            else
            {
                var paths = pathnames;
                while (paths is Cons pc)
                {
                    Load(new LispObject[] { pc.Car });
                    paths = pc.Cdr;
                }
            }
        }
        else
        {
            // No pathnames — call *module-provider-functions* (SBCL-compatible)
            var mpfSym = Startup.Sym("*MODULE-PROVIDER-FUNCTIONS*");
            LispObject providers = DynamicBindings.Get(mpfSym);
            bool found = false;
            while (providers is Cons pc)
            {
                var provider = pc.Car;
                LispObject result;
                if (provider is Symbol sym && sym.Function is LispFunction sfn)
                    result = sfn.Invoke(new LispString(name));
                else if (provider is LispFunction fn)
                    result = fn.Invoke(new LispString(name));
                else
                {
                    providers = pc.Cdr;
                    continue;
                }
                if (result is not Nil)
                {
                    found = true;
                    break;
                }
                providers = pc.Cdr;
            }
            if (!found)
                throw new LispErrorException(new LispError($"REQUIRE: module \"{name}\" not found"));
        }

        // Some contribs' files forget to call (provide ...), which would
        // let a second (require "same") re-load the file. Defensively push
        // NAME onto *modules* if the load/providers didn't. This deviates
        // slightly from strict SBCL (which trusts the module) but makes
        // REQUIRE idempotent regardless of contrib hygiene.
        bool nameInAfter = false;
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s && s.Value == name) { nameInAfter = true; break; }
        if (!nameInAfter)
            DynamicBindings.Set(modulesSym,
                new Cons(new LispString(name), DynamicBindings.Get(modulesSym)));

        // Collect names added to *modules* during this call. *modules* is
        // prepended newest-first; iterating head→tail and prepending to the
        // result reverses to chronological order of the PROVIDE calls.
        LispObject added = Nil.Instance;
        for (LispObject c = DynamicBindings.Get(modulesSym); c is Cons cc; c = cc.Cdr)
            if (cc.Car is LispString s && !before.Contains(s.Value))
                added = new Cons(new LispString(s.Value), added);
        return added;

        }
    }

    /// <summary>
    /// Extra contrib search paths added by host applications (MAUI, etc.)
    /// whose contrib bundle lives somewhere other than
    /// <c>AppContext.BaseDirectory/contrib</c>. Each path is the parent of a
    /// contrib tree — i.e. candidate files are <c>&lt;path&gt;/name/name.ext</c>.
    /// Entries are prepended to the search order so hosts can override.
    /// </summary>
    public static readonly List<string> ContribExtraSearchPaths = new();

    public static LispObject ContribParent(LispObject[] args)
    {
        var baseDir = AppContext.BaseDirectory;
        var searchDirs = new List<string>();
        searchDirs.AddRange(ContribExtraSearchPaths);
        searchDirs.Add(Path.Combine(baseDir, "contrib"));
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "contrib"));
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "..", "contrib"));
        foreach (var dir in searchDirs)
        {
            var full = Path.GetFullPath(dir);
            if (Directory.Exists(full))
            {
                var parent = Directory.GetParent(full)?.FullName;
                if (parent != null)
                    return LispPathname.FromString(parent + Path.DirectorySeparatorChar);
            }
        }
        return Nil.Instance;
    }

    /// <summary>
    /// Default module provider: searches contrib/ directory relative to the binary.
    /// Tries .fasl, .sil, then .lisp extensions (like SBCL's module-provide-contrib).
    /// Hosts can register additional roots via
    /// <see cref="ContribExtraSearchPaths"/>; these are searched before the
    /// default BaseDirectory-relative paths.
    /// </summary>
    public static LispObject ModuleProvideContrib(LispObject[] args)
    {
        if (args.Length == 0) return Nil.Instance;
        string name = ToStringDesignator(args[0], "MODULE-PROVIDE-CONTRIB").ToLowerInvariant();

        var baseDir = AppContext.BaseDirectory;
        var searchDirs = new List<string>();
        searchDirs.AddRange(ContribExtraSearchPaths);
        searchDirs.Add(Path.Combine(baseDir, "contrib"));
        // Dev fallbacks: runtime/bin/Debug/net10.0/ → runtime/contrib/ or
        // project-root contrib/
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "contrib"));
        searchDirs.Add(Path.Combine(baseDir, "..", "..", "..", "..", "contrib"));

        string[] extensions = { ".fasl", ".sil", ".lisp" };

        // OS key for per-OS fasl lookup (runtimes/{os}/name.fasl).
        // dotcl ships a single OS-agnostic asdf.fasl: the .NET IL is portable and all
        // OS-divergent behavior is resolved at run time (os-cond is runtime for dotcl,
        // and dotcl-reachable read-time #+os-* are impl-gated/invariant), so there is no
        // per-OS runtimes/{os}/ variant to prefer.
        foreach (var dir in searchDirs)
        {
            foreach (var ext in extensions)
            {
                // contrib/name/name.ext
                var path = Path.GetFullPath(Path.Combine(dir, name, name + ext));
                if (File.Exists(path))
                {
                    Load(new LispObject[] { new LispString(path) });
                    if (name == "asdf")
                    {
                        RegisterUserAsdSearchPaths();
                        PatchUiopWindowsPath();
                        RegisterContribWithAsdf(searchDirs.ToArray());
                        InstallAsdfModuleProvider();
                    }
                    return T.Instance;
                }
            }
        }
        return Nil.Instance;
    }

    /// <summary>
    /// User-supplied .asd search paths from --asd-search-path. Set by Program.cs
    /// before module load; consumed by RegisterUserAsdSearchPaths after asdf loads.
    /// </summary>
    public static readonly List<string> UserAsdSearchPaths = new();

    /// <summary>
    /// Script user-arguments from a positional `dotcl file.lisp arg1 arg2`
    /// invocation. Null when not running a positional script (REPL, --eval,
    /// --load-only, save-application exe). Set by Program.cs before the script
    /// runs; surfaced via dotcl:command-line-arguments so that
    /// uiop:command-line-arguments returns these args. See COMMAND-LINE-ARGUMENTS
    /// registration in Startup.cs for the `--`-delimited shape.
    /// </summary>
    public static List<string>? ScriptArgs = null;

    /// <summary>
    /// On Windows hosts, wrap uiop:parse-unix-namestring so that strings
    /// containing `\` separators or a drive-letter prefix are normalized
    /// to UNIX form before reaching ASDF's parser. .NET's Path APIs return
    /// native separators, so user code feeding `Path.Combine`-style results
    /// straight into asdf:load-asd / asdf:load-system would otherwise hit
    /// "Expected an absolute pathname" because the UNIX parser sees `\` as
    /// a filename character. Runtime patch (no asdf source change), so the
    /// behavior stays scoped to dotcl-on-Windows.
    /// </summary>
    private static void PatchUiopWindowsPath()
    {
        if (!Compat.IsWindows()) return;
        // ASDF coerces pathname designators through uiop:parse-unix-namestring, which
        // is UNIX-only and cannot represent a drive letter, so a native Windows path
        // (backslashes or a C: drive) fails ASDF's absolute-pathname check.
        // dotcl's own CL parse-namestring already parses native Windows paths
        // correctly (drive -> device), so on Windows we route native paths through it
        // (lossless — the drive is preserved) and let genuine unix-style namestrings
        // fall through to the original. This replaces an earlier lossy version that
        // stripped the drive (wrong on non-C: drives).
        const string patch = @"
(let ((orig (fdefinition 'uiop:parse-unix-namestring)))
  (setf (fdefinition 'uiop:parse-unix-namestring)
        (lambda (name &rest keys)
          (if (and (stringp name)
                   (>= (length name) 2)
                   (or (find #\\ name)
                       (and (alpha-char-p (char name 0))
                            (eql (char name 1) #\:))))
              (let ((pn (parse-namestring name)))
                (if (getf keys :ensure-directory)
                    (uiop:ensure-directory-pathname pn)
                    pn))
              (apply orig name keys)))))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(patch) }));
            Runtime.Eval(read);
        }
        catch { /* ignore if uiop symbols missing */ }
    }

    /// <summary>
    /// Push the user-supplied --asd-search-path values to asdf:*central-registry*.
    /// These take effect after asdf is loaded.
    /// </summary>
    private static void RegisterUserAsdSearchPaths()
    {
        if (UserAsdSearchPaths.Count == 0) return;
        var dirs = new List<string>();
        foreach (var p in UserAsdSearchPaths)
        {
            var full = Path.GetFullPath(p);
            if (Directory.Exists(full)) dirs.Add(full);
        }
        PushDirsToCentralRegistry(dirs);
    }

    private static void PushDirsToCentralRegistry(IList<string> dirs)
    {
        if (dirs.Count == 0) return;
        // Build a single (dolist (p '("..." "..." ...)) (pushnew ...)) form so
        // the eval round-trip is one call regardless of dir count.
        var sb = new System.Text.StringBuilder();
        sb.Append("(dolist (p '(");
        foreach (var d in dirs)
        {
            sb.Append('"');
            sb.Append(d.Replace('\\', '/').TrimEnd('/'));
            sb.Append("/\" ");
        }
        sb.Append(")) (pushnew (pathname p) asdf:*central-registry* :test #'equal))");
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(sb.ToString()) }));
            Runtime.Eval(read);
        }
        catch { /* ignore if asdf symbols missing */ }
    }

    /// <summary>
    /// After ASDF is loaded, push all contrib subdirectories to asdf:*central-registry*
    /// so that dotcl's built-in shims (trivial-gray-streams etc.) take priority over QL.
    /// </summary>
    private static void RegisterContribWithAsdf(IEnumerable<string> searchDirs)
    {
        // Walk every search dir (not just the first one with a hit). asdf's
        // pushnew on *central-registry* deduplicates by equal pathname, so
        // contributions from runtime/bin/...staged copy/ + source contrib/
        // merge cleanly. Without this, a stale staged copy hides newer .asd
        // files added under source contrib/.
        foreach (var baseContrib in searchDirs)
        {
            var contribFull = Path.GetFullPath(baseContrib);
            if (!Directory.Exists(contribFull)) continue;
            foreach (var sub in Directory.GetDirectories(contribFull))
            {
                var dirPath = Path.GetFullPath(sub).Replace('\\', '/') + "/";
                var form = $"(pushnew (pathname \"{dirPath}\") asdf:*central-registry* :test #'equal)";
                try
                {
                    // ReadFromString returns multiple values; unwrap to the
                    // primary form before passing to Eval (Eval doesn't dispatch on MvReturn).
                    var read = MultipleValues.Primary(
                        Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
                    Runtime.Eval(read);
                }
                catch { /* ignore if ASDF symbols not yet available */ }
            }
        }
    }

    /// <summary>
    /// After ASDF loads, push its module provider onto CL:*MODULE-PROVIDER-FUNCTIONS*
    /// so that (require "some-asdf-system") falls back to ASDF — the way SBCL wires
    /// asdf:module-provide-asdf into sb-ext:*module-provider-functions* (asdf's
    /// footer.lisp does this only for known implementations; dotcl is not in that
    /// feature list, and dotcl's hook lives in CL, not sb-ext, so the integration
    /// never happened). asdf:module-provide-asdf returns T after loading the
    /// matching system and NIL when no such system exists, exactly the
    /// *module-provider-functions* contract REQUIRE expects.
    /// </summary>
    private static void InstallAsdfModuleProvider()
    {
        const string form = @"
(let ((p (find-symbol ""MODULE-PROVIDE-ASDF"" ""ASDF""))
      (v (find-symbol ""*MODULE-PROVIDER-FUNCTIONS*"" ""CL"")))
  (when (and p (fboundp p) v (boundp v))
    (set v (adjoin p (symbol-value v)))))";
        try
        {
            var read = MultipleValues.Primary(
                Runtime.ReadFromString(new LispObject[] { new LispString(form) }));
            Runtime.Eval(read);
        }
        catch { /* ignore if ASDF symbols not yet available */ }
    }

    // --- Load ---

    /// <summary>
    /// 3-arg overload used by the compiler's direct (:call "Runtime.Load") CIL instruction.
    /// </summary>
    public static LispObject Load(LispObject filespec, LispObject verbose, LispObject print)
        => Load(new LispObject[] { filespec,
            Startup.Sym("VERBOSE"), verbose,
            Startup.Sym("PRINT"), print });

    /// <summary>
    /// CL LOAD with keyword arguments: (load filespec &amp;key verbose print if-does-not-exist external-format)
    /// </summary>
    public static LispObject Load(LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("LOAD: requires at least 1 argument"));

        var filespec = args[0];

        // Parse keyword arguments from args[1..]
        LispObject? verboseArg = null;
        LispObject? printArg = null;
        LispObject? ifDoesNotExistArg = null;
        bool allowOtherKeys = false;

        // First pass: check for :allow-other-keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS" && args[i + 1] is not Nil)
            {
                allowOtherKeys = true;
                break;
            }
        }

        // Check for odd number of keyword arguments
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError("LOAD: odd number of keyword arguments"));

        // Second pass: parse known keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            string key = args[i] switch
            {
                Symbol s => s.Name,
                _ => args[i].ToString()
            };
            switch (key)
            {
                case "VERBOSE": verboseArg ??= args[i + 1]; break;
                case "PRINT": printArg ??= args[i + 1]; break;
                case "IF-DOES-NOT-EXIST": ifDoesNotExistArg ??= args[i + 1]; break;
                case "EXTERNAL-FORMAT": break; // accepted but ignored
                case "ALLOW-OTHER-KEYS": break; // already handled
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"LOAD: unrecognized keyword argument :{key}"));
                    break;
            }
        }

        // Defaults from *load-verbose* and *load-print*
        var verboseSym = Startup.Sym("*LOAD-VERBOSE*");
        var printSym = Startup.Sym("*LOAD-PRINT*");
        LispObject verbose = verboseArg ?? DynamicBindings.Get(verboseSym);
        LispObject print = printArg ?? DynamicBindings.Get(printSym);

        bool isVerbose = verbose is not Nil;
        bool isPrint = print is not Nil;

        // CLHS: filespec can be a stream — read and eval forms from it directly
        if (filespec is LispStream)
        {
            if (isVerbose)
            {
                var w = GetStandardOutputWriter();
                w.WriteLine("; Loading from stream");
                w.Flush();
            }

            TextReader streamReader = GetTextReader(filespec);
            var reader = new Reader(streamReader);

            // Save and bind *package* and *readtable* per CLHS (but NOT *load-pathname*/*load-truename* for streams)
            var packageSym = Startup.Sym("*PACKAGE*");
            var readtableSym2 = Startup.Sym("*READTABLE*");
            var oldPackage = DynamicBindings.Get(packageSym);
            var oldReadtable2 = DynamicBindings.Get(readtableSym2);

            try
            {
                LispObject result = Nil.Instance;
                while (reader.TryRead(out var form))
                {
                    foreach (var subForm in FlattenTopLevel(form))
                    {
                        var instrList = CompileTopLevel(subForm);
                        result = DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
                    }

                    if (isPrint)
                    {
                        var w = GetStandardOutputWriter();
                        w.WriteLine(FormatTop(result, true));
                        w.Flush();
                    }
                }
                return T.Instance;
            }
            finally
            {
                DynamicBindings.Set(packageSym, oldPackage);
                DynamicBindings.Set(readtableSym2, oldReadtable2);
            }
        }

        string filePath = ResolvePhysicalPath(filespec);

        // Merge with *default-pathname-defaults* if file not found at given path
        if (!File.Exists(filePath))
        {
            var dpd = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            if (dpd is LispPathname dpdPath)
            {
                var merged = LispPathname.FromString(filePath).MergeWith(dpdPath).ToNamestring();
                if (File.Exists(merged))
                    filePath = merged;
            }
        }

        if (!File.Exists(filePath))
        {
            // :if-does-not-exist nil => return NIL instead of signaling
            if (ifDoesNotExistArg != null && ifDoesNotExistArg is Nil)
                return Nil.Instance;
            var err = new LispError($"LOAD: file not found: {filePath}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = filespec;
            throw new LispErrorException(err);
        }

        if (isVerbose)
        {
            var w = GetStandardOutputWriter();
            w.WriteLine($"; Loading {filePath}");
            w.Flush();
        }

        // Handle persisted .NET assembly — detect by PE header ("MZ"), not extension
        if (IsPeAssembly(filePath))
        {
            return LoadFasl(filePath, filespec, isVerbose, isPrint);
        }

        var source = File.ReadAllText(filePath);
        var loadStringReader = new StringReader(source);
        var reader2 = new Reader(loadStringReader);
        // Same live-reader/share-table link as CompileFile: a Lisp reader macro
        // reading elements via (read stream) must reuse THIS reader so per-form
        // share-table clearing doesn't run per element (see CompileFile).
        var loadStream = new LispInputStream(loadStringReader);
        reader2.LispStreamRef = loadStream;
        loadStream.CachedReader = reader2;
        reader2.AdoptStreamShareTables(loadStream);

        // Save and bind *load-pathname*, *load-truename*, *package*, and *readtable* per CLHS
        // CLHS: *load-pathname* = (merge-pathnames filespec *default-pathname-defaults*)
        // CLHS: *load-truename* = (truename *load-pathname*)
        // CLHS: LOAD binds *readtable* and *package* to their pre-load values
        var loadPathSym = Startup.Sym("*LOAD-PATHNAME*");
        var loadTrueSym = Startup.Sym("*LOAD-TRUENAME*");
        {
            var packageSym = Startup.Sym("*PACKAGE*");
            var readtableSym = Startup.Sym("*READTABLE*");
            var oldLoadPath = DynamicBindings.Get(loadPathSym);
            var oldLoadTrue = DynamicBindings.Get(loadTrueSym);
            var oldPackage = DynamicBindings.Get(packageSym);
            var oldReadtable = DynamicBindings.Get(readtableSym);
            var loadInputPath = filespec switch
            {
                LispPathname p => p,
                LispString s => LispPathname.FromString(s.Value),
                _ => LispPathname.FromString(filespec.ToString())
            };
            var dpdForLoad = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            var mergedLoadPath = MergePathnames(loadInputPath, dpdForLoad is LispPathname ? dpdForLoad : LispPathname.FromString(""));
            DynamicBindings.Set(loadPathSym, mergedLoadPath);
            DynamicBindings.Set(loadTrueSym, LispPathname.FromString(Path.GetFullPath(filePath)));

            try
            {
                LispObject result = Nil.Instance;
                bool isCompiled = filePath.EndsWith(".sil", StringComparison.OrdinalIgnoreCase)
                    || filePath.EndsWith(".ufsl", StringComparison.OrdinalIgnoreCase)
                    || source.StartsWith(";; -*- dotcl-compiled -*-");

                while (reader2.TryRead(out var form))
                {
                    int formLine = reader2.LastFormLine;
                    try
                    {
                        if (isCompiled)
                        {
                            // .sil file: form is already a compiled instruction list
                            result = DotCL.Emitter.CilAssembler.AssembleAndRun(form);
                        }
                        else
                        {
                            // .lisp file: compile each top-level form
                            foreach (var subForm in FlattenTopLevel(form))
                            {
                                var instrList = CompileTopLevel(subForm);
                                result = DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        // Don't wrap control flow exceptions — they must reach their
                        // intended catch blocks (handler-case, block, catch, go, restart).
                        if (Runtime.IsLispControlFlowException(ex))
                            throw;
                        throw new LispSourceException(filePath, formLine, ex);
                    }

                    if (isPrint)
                    {
                        var w = GetStandardOutputWriter();
                        w.WriteLine(FormatTop(result, true));
                        w.Flush();
                    }
                }
                return T.Instance;
            }
            finally
            {
                DynamicBindings.Set(loadPathSym, oldLoadPath);
                DynamicBindings.Set(loadTrueSym, oldLoadTrue);
                DynamicBindings.Set(packageSym, oldPackage);
                DynamicBindings.Set(readtableSym, oldReadtable);
            }
        }
    }

    /// <summary>Check if a file is a PE (.NET) assembly by reading the first two bytes.</summary>
    private static bool IsPeAssembly(string filePath)
    {
        try
        {
            using var fs = File.OpenRead(filePath);
            var b1 = fs.ReadByte();
            var b2 = fs.ReadByte();
            return b1 == 'M' && b2 == 'Z'; // PE header
        }
        catch { return false; }
    }

    /// <summary>Load a .fasl (persisted .NET assembly) file.</summary>
    private static LispObject LoadFasl(string filePath, LispObject filespec,
        bool isVerbose, bool isPrint)
    {
        var loadPathSym = Startup.Sym("*LOAD-PATHNAME*");
        var loadTrueSym = Startup.Sym("*LOAD-TRUENAME*");
        var packageSym = Startup.Sym("*PACKAGE*");
        var readtableSym = Startup.Sym("*READTABLE*");
        var oldLoadPath = DynamicBindings.Get(loadPathSym);
        var oldLoadTrue = DynamicBindings.Get(loadTrueSym);
        var oldPackage = DynamicBindings.Get(packageSym);
        var oldReadtable = DynamicBindings.Get(readtableSym);
        var loadInputPath = filespec switch
        {
            LispPathname p => p,
            LispString s => LispPathname.FromString(s.Value),
            _ => LispPathname.FromString(filespec.ToString())
        };
        var dpdForLoad = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
        var mergedLoadPath = MergePathnames(loadInputPath,
            dpdForLoad is LispPathname ? dpdForLoad : LispPathname.FromString(""));
        DynamicBindings.Set(loadPathSym, mergedLoadPath);
        DynamicBindings.Set(loadTrueSym, LispPathname.FromString(Path.GetFullPath(filePath)));

        try
        {
            var asm = System.Reflection.Assembly.Load(File.ReadAllBytes(Path.GetFullPath(filePath)));
            var moduleType = asm.GetType("CompiledModule")
                ?? throw new Exception($"LOAD: .fasl has no CompiledModule type: {filePath}");
            var initMethod = moduleType.GetMethod("ModuleInit")
                ?? throw new Exception($"LOAD: .fasl has no ModuleInit method: {filePath}");
            try
            {
                var result = (LispObject?)initMethod.Invoke(null, null) ?? Nil.Instance;
            }
            catch (System.Reflection.TargetInvocationException tie) when (tie.InnerException != null)
            {
                throw tie.InnerException;
            }
            return T.Instance;
        }
        finally
        {
            DynamicBindings.Set(loadPathSym, oldLoadPath);
            DynamicBindings.Set(loadTrueSym, oldLoadTrue);
            DynamicBindings.Set(packageSym, oldPackage);
            DynamicBindings.Set(readtableSym, oldReadtable);
        }
    }

    /// <summary>
    /// Flatten eval-when and progn bodies into individual top-level forms.
    /// This ensures that defvar/defparameter values are set before subsequent
    /// forms in the same block are compiled (important for macro expansion).
    /// </summary>
    private static IEnumerable<LispObject> FlattenTopLevel(LispObject form)
    {
        // Defensive: if a reader macro or macro expander leaked MvReturn, take primary value.
        if (form is MvReturn mvr)
            form = mvr.Values.Length > 0 ? mvr.Values[0] : Nil.Instance;
        if (form is Cons c && c.Car is Symbol sym)
        {
            if (sym.Name == "EVAL-WHEN" && c.Cdr is Cons rest)
            {
                // (eval-when (situations...) body...)
                // Only flatten if there are multiple body forms
                var situations = rest.Car;
                var body = rest.Cdr;
                // Count body forms
                int count = 0;
                var tmp = body;
                while (tmp is Cons) { count++; tmp = ((Cons)tmp).Cdr; }
                if (count > 1)
                {
                    while (body is Cons bodyCell)
                    {
                        // (eval-when (situations) single-form)
                        var wrappedForm = new Cons(sym,
                            new Cons(situations, new Cons(bodyCell.Car, Nil.Instance)));
                        // Don't recurse into the wrapped form (it's already single)
                        // but do flatten the inner form if it's progn
                        yield return wrappedForm;
                        body = bodyCell.Cdr;
                    }
                    yield break;
                }
            }
            if (sym.Name == "PROGN")
            {
                var body = c.Cdr;
                while (body is Cons bodyCell)
                {
                    foreach (var sub in FlattenTopLevel(bodyCell.Car))
                        yield return sub;
                    body = bodyCell.Cdr;
                }
                yield break;
            }
            // Per CLHS 3.2.3.1: "If a top level form is a macro form,
            // the macro form is expanded and the result is processed as a top level form."
            // Don't expand forms already handled by ShouldExecuteAtCompileTime or
            // IsEvalWhenForCompileFile — they need their original identity preserved.
            if (!IsCompileTimeSideEffectForm(sym.Name))
            {
                var expanded = TryMacroexpand1(form);
                if (expanded != null && !ReferenceEquals(expanded, form))
                {
                    foreach (var sub in FlattenTopLevel(expanded))
                        yield return sub;
                    yield break;
                }
            }
        }
        yield return form;
    }

    /// <summary>Try to macroexpand-1 a form. Returns expanded form or null if not a macro/expansion fails.</summary>
    private static LispObject? TryMacroexpand1(LispObject form)
    {
        if (form is not Cons c || c.Car is not Symbol sym)
            return null;
        try
        {
            // For DEFSTRUCT — let the Lisp compiler's find-macro-expander handle
            // it. The C# macro function table might contain
            // SBCL's broken 12MB expansion from src/code/defstruct.lisp.
            // DEFKNOWN — SBCL macro that calls split-type-info at expansion time.
            // C# expansion may succeed but produce forms that fail in the Lisp compiler.
            // Skip to let Lisp compiler handle macro expansion + eval in one step.
            if (sym.Name == "DEFSTRUCT" || sym.Name == "DEFKNOWN")
                return null;
            // Check runtime macro table
            var runtimeMacroFn = Runtime.MacroFunction(sym);
            if (runtimeMacroFn is LispFunction rmf)
            {
                var result = rmf.Invoke(new LispObject[] { form, Nil.Instance });
                // Macro functions in CL return single value; unwrap MvReturn if present
                return result is MvReturn mv ? (mv.Values.Length > 0 ? mv.Values[0] : Nil.Instance) : result;
            }
            // Check compiler macro table
            var compilerFn = Startup.LookupCompilerMacro(sym);
            if (compilerFn != null)
            {
                var result2 = compilerFn.Invoke(new LispObject[] { form });
                return result2 is MvReturn mv2 ? (mv2.Values.Length > 0 ? mv2.Values[0] : Nil.Instance) : result2;
            }
        }
        catch
        {
            // Macro expansion failed (e.g. runtime state not yet set up).
            // Return null to let the Lisp compiler handle it instead.
        }
        return null;
    }


    // Compile a single top-level form using the Lisp compiler
    // Cached at first use to prevent user code (e.g. SB-C::COMPILE-TOPLEVEL)
    // from overwriting the compiler's function in the flat name table.
    private static LispFunction? _cachedCompileTopLevel;
    private static LispFunction? _cachedCompileTopLevelEval;
    internal static LispObject CompileTopLevel(LispObject form)
    {
        if (!Compat.TryEnsureSufficientExecutionStack())
            throw new LispErrorException(new LispProgramError("Stack overflow in compilation"));
        _cachedCompileTopLevel ??= DotCL.Emitter.CilAssembler.GetFunction("COMPILE-TOPLEVEL");
        return _cachedCompileTopLevel.Invoke1(form);
    }

    // Compile a top-level form for EVAL (preserves MvReturn in tail position).
    internal static LispObject CompileTopLevelEval(LispObject form)
    {
        if (!Compat.TryEnsureSufficientExecutionStack())
            throw new LispErrorException(new LispProgramError("Stack overflow in compilation"));
        _cachedCompileTopLevelEval ??= DotCL.Emitter.CilAssembler.GetFunction("COMPILE-TOPLEVEL-EVAL");
        return _cachedCompileTopLevelEval.Invoke1(form);
    }

    // --- Compile-file ---

    /// <summary>
    /// Returns true if a top-level form should be executed at compile time.
    /// Per CLHS, compile-file should only execute forms that need compile-time
    /// side effects (defmacro, defvar, in-package, etc.), not defun/defmethod.
    /// </summary>
    /// <summary>Check if a symbol name denotes a form with compile-time side effects.</summary>
    private static bool IsCompileTimeSideEffectForm(string name) => name switch
    {
        "IN-PACKAGE" or "DEFPACKAGE" or "DEFMACRO" or "DEFINE-COMPILER-MACRO"
        or "DECLAIM" or "PROCLAIM" or "USE-PACKAGE" or "SHADOW"
        or "SHADOWING-IMPORT" or "EXPORT" or "IMPORT" or "REQUIRE" or "PROVIDE"
        or "DEFTYPE"  // CLHS: deftype has compile-time effects (type name available during compilation)
        or "DEFCLASS" or "DEFINE-CONDITION"  // CLHS: defclass has compile-time effects (class name available for find-class during compilation)
            => true,
        _ => false
    };

    private static bool ShouldExecuteAtCompileTime(LispObject form)
    {
        if (form is Cons c && c.Car is Symbol sym)
        {
            return IsCompileTimeSideEffectForm(sym.Name);
        }
        return false;
    }

    /// <summary>
    /// Check if form is (eval-when (...) body...) and parse the situations.
    /// For compile-file: :compile-toplevel or :load-toplevel without :execute
    /// needs special handling because the compiler only emits code for :execute.
    /// </summary>
    private static bool IsEvalWhenForCompileFile(LispObject form,
        out LispObject body, out bool hasCompileToplevel, out bool hasLoadToplevel)
    {
        body = Nil.Instance;
        hasCompileToplevel = false;
        hasLoadToplevel = false;
        if (form is not Cons c || c.Car is not Symbol sym || sym.Name != "EVAL-WHEN")
            return false;
        if (c.Cdr is not Cons rest || rest.Car is not Cons situations)
            return false;

        for (var sit = situations; sit != null && sit is Cons sc; sit = sc.Cdr as Cons)
        {
            if (sc.Car is Symbol s)
            {
                if (s.Name == "COMPILE-TOPLEVEL") hasCompileToplevel = true;
                else if (s.Name == "LOAD-TOPLEVEL") hasLoadToplevel = true;
                // :EXECUTE is irrelevant at top level in compile-file (Figure 3-7).
            }
        }

        // Per CLHS 3.2.3.1 (Figure 3-7), at top level in compile-file the
        // behavior is determined solely by :compile-toplevel (eval now) and
        // :load-toplevel (place in fasl). :execute is irrelevant here and does
        // NOT promote the body into the fasl — e.g. {:compile-toplevel :execute}
        // is evaluated at compile time and discarded, NOT written to the fasl
        // (promoting it on :execute leaked the body into the load image).
        // A bare {:execute} (no ct/lt) is handled by the Lisp compile-eval-when
        // (returns here false) and discarded.
        if (hasCompileToplevel || hasLoadToplevel)
        {
            body = rest.Cdr ?? Nil.Instance;
            return true;
        }
        return false;
    }

    /// <summary>
    /// Build (progn form1 form2 ...) from a list of forms.
    /// </summary>
    private static LispObject MakeProgn(LispObject formList)
    {
        return new Cons(Startup.Sym("PROGN"), formList);
    }

    /// <summary>
    /// Register proper macro expanders for WHEN, UNLESS, COND, AND, OR so that
    /// macroexpand-1 returns their actual expansions. Code walkers (e.g. iterate)
    /// need this to see inside these forms.
    /// </summary>
    private static void RegisterCompileFormExpanders()
    {
        var IF = Startup.Sym("IF");
        var NOT = Startup.Sym("NOT");
        var PROGN = Startup.Sym("PROGN");
        var AND = Startup.Sym("AND");
        var OR = Startup.Sym("OR");
        var LET = Startup.Sym("LET");

        // WHEN: (when test . body) → (if test (progn . body))
        Runtime.RegisterMacroFunction(Startup.Sym("WHEN"), new LispFunction(args => {
            Runtime.CheckArityExact("WHEN-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons form || form.Cdr is not Cons rest)
                return args[0];
            var compilerFn = Startup.LookupCompilerMacro(Startup.Sym("WHEN"));
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var test = rest.Car;
            var body = rest.Cdr;
            var progn = new Cons(PROGN, body);
            return new Cons(IF, new Cons(test, new Cons(progn, Nil.Instance)));
        }, "WHEN-MACRO-EXPANDER", 2));

        // UNLESS: (unless test . body) → (if (not test) (progn . body))
        Runtime.RegisterMacroFunction(Startup.Sym("UNLESS"), new LispFunction(args => {
            Runtime.CheckArityExact("UNLESS-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons form || form.Cdr is not Cons rest)
                return args[0];
            var compilerFn = Startup.LookupCompilerMacro(Startup.Sym("UNLESS"));
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var test = rest.Car;
            var body = rest.Cdr;
            var notTest = new Cons(NOT, new Cons(test, Nil.Instance));
            var progn = new Cons(PROGN, body);
            return new Cons(IF, new Cons(notTest, new Cons(progn, Nil.Instance)));
        }, "UNLESS-MACRO-EXPANDER", 2));

        // AND: (and) → t  (and x) → x  (and x . rest) → (if x (and . rest) nil)
        Runtime.RegisterMacroFunction(AND, new LispFunction(args => {
            Runtime.CheckArityExact("AND-MACRO-EXPANDER", args, 2);
            var compilerFn = Startup.LookupCompilerMacro(AND);
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var form = args[0] as Cons;
            if (form == null) return args[0];
            var rest = form.Cdr;
            if (rest is Nil) return T.Instance; // (and) → t
            if (rest is Cons c1)
            {
                if (c1.Cdr is Nil) return c1.Car; // (and x) → x
                // (and x . rest) → (if x (and . rest) nil)
                var andRest = new Cons(AND, c1.Cdr);
                return new Cons(IF, new Cons(c1.Car, new Cons(andRest, new Cons(Nil.Instance, Nil.Instance))));
            }
            return args[0];
        }, "AND-MACRO-EXPANDER", 2));

        // OR: (or) → nil  (or x) → x  (or x . rest) → (let ((#:g x)) (if #:g #:g (or . rest)))
        Runtime.RegisterMacroFunction(OR, new LispFunction(args => {
            Runtime.CheckArityExact("OR-MACRO-EXPANDER", args, 2);
            var compilerFn = Startup.LookupCompilerMacro(OR);
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var form = args[0] as Cons;
            if (form == null) return args[0];
            var rest = form.Cdr;
            if (rest is Nil) return Nil.Instance; // (or) → nil
            if (rest is Cons c1)
            {
                if (c1.Cdr is Nil) return c1.Car; // (or x) → x
                var gSym = (Symbol)Runtime.Gensym(new LispString("OR-VAL"));
                var orRest = new Cons(OR, c1.Cdr);
                var binding = new Cons(new Cons(gSym, new Cons(c1.Car, Nil.Instance)), Nil.Instance);
                var ifForm = new Cons(IF, new Cons(gSym, new Cons(gSym, new Cons(orRest, Nil.Instance))));
                return new Cons(LET, new Cons(binding, new Cons(ifForm, Nil.Instance)));
            }
            return args[0];
        }, "OR-MACRO-EXPANDER", 2));

        // COND: (cond) → nil
        // (cond (test) . rest) → (let ((#:g test)) (if #:g #:g (cond . rest)))
        // (cond (test . body) . rest) → (if test (progn . body) (cond . rest))
        Runtime.RegisterMacroFunction(Startup.Sym("COND"), new LispFunction(args => {
            Runtime.CheckArityExact("COND-MACRO-EXPANDER", args, 2);
            var compilerFn = Startup.LookupCompilerMacro(Startup.Sym("COND"));
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var form = args[0] as Cons;
            if (form == null) return args[0];
            var clauses = form.Cdr;
            if (clauses is Nil) return Nil.Instance; // (cond) → nil
            if (clauses is not Cons c1) return args[0];
            var clause = c1.Car as Cons;
            if (clause == null) return args[0];
            var test = clause.Car;
            var body = clause.Cdr;
            var restClauses = new Cons(Startup.Sym("COND"), c1.Cdr);
            if (body is Nil)
            {
                // (cond (test) . rest) → (let ((#:g test)) (if #:g #:g (cond . rest)))
                var gSym = (Symbol)Runtime.Gensym(new LispString("COND-VAL"));
                var binding = new Cons(new Cons(gSym, new Cons(test, Nil.Instance)), Nil.Instance);
                var ifForm = new Cons(IF, new Cons(gSym, new Cons(gSym, new Cons(restClauses, Nil.Instance))));
                return new Cons(LET, new Cons(binding, new Cons(ifForm, Nil.Instance)));
            }
            var thenForm = new Cons(PROGN, body);
            return new Cons(IF, new Cons(test, new Cons(thenForm, new Cons(restClauses, Nil.Instance))));
        }, "COND-MACRO-EXPANDER", 2));

        // handler-bind / handler-case / restart-bind / restart-case are implemented as
        // compile-form handlers (the compiler dispatches them directly, before macro
        // expansion), but MACRO-FUNCTION reported them as macros while MACROEXPAND-1
        // returned them unexpanded — an inconsistency that breaks code walkers.
        // Register real expanders so MACROEXPAND-1 yields a portable, walkable, eval-able
        // equivalent (CLHS: macro-function non-nil ⇒ macroexpand-1 expands). The compiler
        // is unaffected: compile-form checks its handler table first, and the Lisp
        // find-macro-expander only consults runtime macro-functions for DOTCL-package
        // symbols, so these CL-package entries never divert compilation/analysis.
        var QUOTE = Startup.Sym("QUOTE");
        var LIST = Startup.Sym("LIST");
        var CONS = Startup.Sym("CONS");
        var LAMBDA = Startup.Sym("LAMBDA");
        var BLOCK = Startup.Sym("BLOCK");
        var TAGBODY = Startup.Sym("TAGBODY");
        var GO = Startup.Sym("GO");
        var SETQ = Startup.Sym("SETQ");
        var LOCALLY = Startup.Sym("LOCALLY");
        var MVCALL = Startup.Sym("MULTIPLE-VALUE-CALL");
        var RETURN_FROM = Startup.Sym("RETURN-FROM");
        var UNWIND_PROTECT = Startup.Sym("UNWIND-PROTECT");
        var PUSH_HB = Startup.Sym("%PUSH-HANDLER-CLUSTER");
        var POP_HB = Startup.Sym("%POP-HANDLER-CLUSTER");
        var PUSH_RB = Startup.Sym("%PUSH-RESTART-CLUSTER");
        var POP_RB = Startup.Sym("%POP-RESTART-CLUSTER");

        // (handler-bind ((type fn)...) . body)
        //   → (progn (%push-handler-cluster (list (cons 'type fn)...))
        //            (unwind-protect (progn . body) (%pop-handler-cluster)))
        // Body stays INLINE in a progn (not a lambda thunk), so code walkers can walk it
        //. HandlerClusterStack.Signal restores clusters in a finally, so the
        // unwind-protect pop stays balanced even on a non-local exit from a handler.
        Runtime.RegisterMacroFunction(Startup.Sym("HANDLER-BIND"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var pairs = new System.Collections.Generic.List<LispObject>();
            for (var b = rest.Car; b is Cons bc; b = bc.Cdr)
                if (bc.Car is Cons bd && bd.Cdr is Cons bf)
                    pairs.Add(Runtime.List(CONS, Runtime.List(QUOTE, bd.Car), bf.Car));
            var listForm = new Cons(LIST, Runtime.List(pairs.ToArray()));
            var bodyProgn = new Cons(PROGN, rest.Cdr);
            var uw = Runtime.List(UNWIND_PROTECT, bodyProgn, Runtime.List(POP_HB));
            return Runtime.List(PROGN, Runtime.List(PUSH_HB, listForm), uw);
        }, "HANDLER-BIND-MACRO-EXPANDER", 2));

        // (restart-bind ((name fn . opts)...) . body)
        //   → (progn (%push-restart-cluster (list (cons 'name fn)...))
        //            (unwind-protect (progn . body) (%pop-restart-cluster)))
        // Body inline. Restart options are kept by the compile-form handler used
        // for actual compilation; this expansion is for macroexpand-1 / walkers.
        Runtime.RegisterMacroFunction(Startup.Sym("RESTART-BIND"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var pairs = new System.Collections.Generic.List<LispObject>();
            for (var b = rest.Car; b is Cons bc; b = bc.Cdr)
                if (bc.Car is Cons bd && bd.Cdr is Cons bf)
                    pairs.Add(Runtime.List(CONS, Runtime.List(QUOTE, bd.Car), bf.Car));
            var listForm = new Cons(LIST, Runtime.List(pairs.ToArray()));
            var bodyProgn = new Cons(PROGN, rest.Cdr);
            var uw = Runtime.List(UNWIND_PROTECT, bodyProgn, Runtime.List(POP_RB));
            return Runtime.List(PROGN, Runtime.List(PUSH_RB, listForm), uw);
        }, "RESTART-BIND-MACRO-EXPANDER", 2));

        // (handler-case EXPR (type (var) . body)... [(:no-error ll . nbody)])
        //   → (block #:b (let ((#:c nil)) (tagbody
        //        (handler-bind ((type (lambda (#:g) (setq #:c #:g) (go #:t)))...)
        //          (return-from #:b EXPR))   ; EXPR wrapped in mv-call of :no-error fn if present
        //        #:t (return-from #:b (let ((var #:c)) . body)) ...)))
        Runtime.RegisterMacroFunction(Startup.Sym("HANDLER-CASE"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var expr = rest.Car;
            var blk = (Symbol)Runtime.Gensym(new LispString("HC-BLOCK"));
            var cvar = (Symbol)Runtime.Gensym(new LispString("HC-COND"));
            LispObject noErrorClause = Nil.Instance;
            var handlerClauses = new System.Collections.Generic.List<Cons>();
            for (var c = rest.Cdr; c is Cons cc; c = cc.Cdr)
            {
                if (cc.Car is Cons clause)
                {
                    if (clause.Car is Symbol kw && kw.Name == "NO-ERROR") noErrorClause = clause;
                    else handlerClauses.Add(clause);
                }
            }
            var hbBindings = new System.Collections.Generic.List<LispObject>();
            var tagForms = new System.Collections.Generic.List<LispObject>();
            foreach (var clause in handlerClauses)
            {
                var type = clause.Car;
                var ll = (clause.Cdr is Cons cd) ? cd.Car : Nil.Instance;
                var cbody = (clause.Cdr is Cons cd2) ? cd2.Cdr : Nil.Instance;
                var tag = Runtime.Gensym(new LispString("HC-TAG"));
                var g = (Symbol)Runtime.Gensym(new LispString("HC-C"));
                // (lambda (#:g) (setq #:c #:g) (go #:tag))
                var hfn = Runtime.List(LAMBDA, Runtime.List(g),
                    Runtime.List(SETQ, cvar, g), Runtime.List(GO, tag));
                hbBindings.Add(Runtime.List(type, hfn));
                // var binding: (var #:c) or nothing
                LispObject clauseResult;
                if (ll is Cons llc && llc.Car is Symbol v)
                    clauseResult = new Cons(Startup.Sym("LET"),
                        new Cons(Runtime.List(Runtime.List(v, cvar)), cbody));
                else
                    clauseResult = new Cons(LOCALLY, cbody);
                tagForms.Add(tag);
                tagForms.Add(Runtime.List(RETURN_FROM, blk, clauseResult));
            }
            // protected form, wrapping with :no-error mv-call when present
            LispObject protectedReturn;
            if (noErrorClause is Cons ne && ne.Cdr is Cons nec)
            {
                var nell = nec.Car; var nebody = nec.Cdr;
                protectedReturn = Runtime.List(RETURN_FROM, blk,
                    new Cons(MVCALL, new Cons(new Cons(LAMBDA, new Cons(nell, nebody)),
                        new Cons(expr, Nil.Instance))));
            }
            else
                protectedReturn = Runtime.List(RETURN_FROM, blk, expr);
            var hb = new Cons(Startup.Sym("HANDLER-BIND"),
                new Cons(Runtime.List(hbBindings.ToArray()),
                    new Cons(protectedReturn, Nil.Instance)));
            var tagbody = new Cons(TAGBODY, new Cons(hb, Runtime.List(tagForms.ToArray())));
            var letc = Runtime.List(Startup.Sym("LET"),
                Runtime.List(Runtime.List(cvar, Nil.Instance)), tagbody);
            return Runtime.List(BLOCK, blk, letc);
        }, "HANDLER-CASE-MACRO-EXPANDER", 2));

        // (restart-case EXPR (name (args...) [:report r][:interactive i][:test t] . body)...)
        //   → (block #:b (let ((#:av nil)) (tagbody
        //        (return-from #:b
        //          (restart-bind ((name (lambda (&rest #:a) (setq #:av #:a) (go #:tag))
        //                          opts...)...) EXPR))
        //        #:tag (return-from #:b (apply (lambda (args...) . body) #:av)) ...)))
        Runtime.RegisterMacroFunction(Startup.Sym("RESTART-CASE"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var expr = rest.Car;
            var blk = (Symbol)Runtime.Gensym(new LispString("RC-BLOCK"));
            var avar = (Symbol)Runtime.Gensym(new LispString("RC-ARGS"));
            var REST = Startup.Sym("&REST");
            var APPLY = Startup.Sym("APPLY");
            var rbBindings = new System.Collections.Generic.List<LispObject>();
            var tagForms = new System.Collections.Generic.List<LispObject>();
            for (var c = rest.Cdr; c is Cons cc; c = cc.Cdr)
            {
                if (cc.Car is not Cons clause) continue;
                var name = clause.Car;
                var ll = (clause.Cdr is Cons cd) ? cd.Car : Nil.Instance;
                var cbody = (clause.Cdr is Cons cd2) ? cd2.Cdr : Nil.Instance;
                // Split leading :report/:interactive/:test keyword options off the body.
                var opts = new System.Collections.Generic.List<LispObject>();
                while (cbody is Cons bc && bc.Car is Symbol osym && osym.Name.Length > 0
                       && (osym.Name == "REPORT" || osym.Name == "INTERACTIVE" || osym.Name == "TEST")
                       && bc.Cdr is Cons bv)
                {
                    opts.Add(Startup.Keyword(osym.Name + "-FUNCTION"));
                    // :report "str" stays a string; a symbol/lambda is a function designator.
                    opts.Add(bv.Car);
                    cbody = bv.Cdr;
                }
                var tag = Runtime.Gensym(new LispString("RC-TAG"));
                var g = Runtime.Gensym(new LispString("RC-A"));
                var rfn = Runtime.List(LAMBDA, Runtime.List(REST, g),
                    Runtime.List(SETQ, avar, g), Runtime.List(GO, tag));
                var bindItems = new System.Collections.Generic.List<LispObject> { name, rfn };
                bindItems.AddRange(opts);
                rbBindings.Add(Runtime.List(bindItems.ToArray()));
                tagForms.Add(tag);
                tagForms.Add(Runtime.List(RETURN_FROM, blk,
                    Runtime.List(APPLY, new Cons(LAMBDA, new Cons(ll, cbody)), avar)));
            }
            var rb = new Cons(Startup.Sym("RESTART-BIND"),
                new Cons(Runtime.List(rbBindings.ToArray()), new Cons(expr, Nil.Instance)));
            var tagbody = new Cons(TAGBODY,
                new Cons(Runtime.List(RETURN_FROM, blk, rb), Runtime.List(tagForms.ToArray())));
            var letc = Runtime.List(Startup.Sym("LET"),
                Runtime.List(Runtime.List(avar, Nil.Instance)), tagbody);
            return Runtime.List(BLOCK, blk, letc);
        }, "RESTART-CASE-MACRO-EXPANDER", 2));
    }

    /// When true, COMPILE-FILE wraps a compile error on a top-level form in a
    /// LispSourceException carrying the input file + the form's source line, so
    /// the project build driver (CompileProject) can report file:line and remap
    /// concat lines back to the original source (dotcl/dotcl#48). Thread-static and
    /// gated: ordinary interactive/script COMPILE-FILE leaves it false so the raw
    /// LispErrorException (a real condition) still propagates to handler-case.
    [ThreadStatic] internal static bool EmitBuildSourceLocations;

    public static LispObject CompileFile(LispObject[] args)
    {
#if DOTCL_EMIT
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("COMPILE-FILE: requires at least 1 argument"));

        // (compile-file input-file &key output-file verbose print external-format)
        var inputSpec = args[0];

        // Parse keyword arguments
        LispObject? outputFileArg = null;
        LispObject? verboseArg = null;
        LispObject? printArg = null;
        LispObject? targetFeaturesArg = null;
        LispObject? moduleNameArg = null;
        LispObject? corlibNameArg = null;
        bool emitSil = false; // default: PE assembly (.fasl); :sil t → also write text SIL
        bool allowOtherKeys = false;

        // Check for odd number of keyword arguments
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError("COMPILE-FILE: odd number of keyword arguments"));

        // First pass: check for :allow-other-keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS" && args[i + 1] is not Nil)
            {
                allowOtherKeys = true;
                break;
            }
        }

        // Second pass: parse known keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            string key = args[i] switch
            {
                Symbol s => s.Name,
                _ => args[i].ToString()
            };
            switch (key)
            {
                case "OUTPUT-FILE": outputFileArg ??= args[i + 1]; break;
                case "VERBOSE": verboseArg ??= args[i + 1]; break;
                case "PRINT": printArg ??= args[i + 1]; break;
                case "SIL":
                    emitSil = args[i + 1] is not Nil;
                    break;
                case "TARGET-FEATURES": targetFeaturesArg ??= args[i + 1]; break;
                // :module-name — pin the FASL's .NET assembly/module name to a
                // stable string instead of the default basename_<guid8>. Required
                // for build-time-linked (NativeAOT/IL2CPP) deployment, where the
                // assembly is referenced by a fixed name and the file name must
                // match it. The caller owns uniqueness across co-loaded modules.
                case "MODULE-NAME": moduleNameArg ??= args[i + 1]; break;
                // :corlib-name — rewrite the emitted fasl's corlib reference to a
                // portable facade (only "netstandard"). For build-time-linked
                // deployment on BCLs without System.Private.CoreLib (Unity IL2CPP/
                // WebGL). Default (null) keeps System.Private.CoreLib (CoreCLR/AOT).
                case "CORLIB-NAME": corlibNameArg ??= args[i + 1]; break;
                case "EXTERNAL-FORMAT": break; // accepted but ignored
                case "ALLOW-OTHER-KEYS": break; // already handled
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"COMPILE-FILE: unrecognized keyword argument :{key}"));
                    break;
            }
        }

        // Defaults from *compile-verbose* and *compile-print*
        var compileVerboseSym = Startup.Sym("*COMPILE-VERBOSE*");
        var compilePrintSym = Startup.Sym("*COMPILE-PRINT*");
        bool isVerbose = (verboseArg ?? DynamicBindings.Get(compileVerboseSym)) is not Nil;
        bool isPrint = (printArg ?? DynamicBindings.Get(compilePrintSym)) is not Nil;

        // Handle stream as pathname designator (CLHS: a file stream is a pathname designator)
        if (inputSpec is LispFileStream fs)
        {
            inputSpec = fs.OriginalPathname != null
                ? (LispObject)fs.OriginalPathname
                : LispPathname.FromString(fs.FilePath);
        }

        string inputPath = ResolvePhysicalPath(inputSpec);

        // Merge with *default-pathname-defaults* if file not found at given path
        if (!File.Exists(inputPath))
        {
            var dpd = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            if (dpd is LispPathname dpdPath)
            {
                var merged = LispPathname.FromString(inputPath).MergeWith(dpdPath).ToNamestring();
                if (File.Exists(merged))
                    inputPath = merged;
            }
        }

        // Determine output path
        string? outputPath = null;
        if (outputFileArg != null && outputFileArg is not Nil)
        {
            outputPath = ResolvePhysicalPath(outputFileArg);
        }

        if (outputPath == null)
        {
            var cfp = CompileFilePathname(new[] { inputSpec });
            outputPath = ResolvePhysicalPath(cfp);
        }

        if (!File.Exists(inputPath))
        {
            var err = new LispError($"COMPILE-FILE: file not found: {inputPath}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = inputSpec is LispPathname ? inputSpec : (LispObject)LispPathname.FromString(inputPath);
            throw new LispErrorException(err);
        }

        if (isVerbose)
        {
            var w = GetStandardOutputWriter();
            w.WriteLine($"; Compiling file {inputPath}");
            w.Flush();
        }

        // Bind *compile-file-pathname*, *compile-file-truename*, *package*, and *readtable*
        // CLHS: *compile-file-pathname* = (merge-pathnames (pathname input-file) *default-pathname-defaults*)
        // CLHS: *compile-file-truename* = (truename *compile-file-pathname*)
        var cfpSym = Startup.Sym("*COMPILE-FILE-PATHNAME*");
        var cftSym = Startup.Sym("*COMPILE-FILE-TRUENAME*");
        var packageSym = Startup.Sym("*PACKAGE*");
        var readtableSym = Startup.Sym("*READTABLE*");
        var oldCfp = DynamicBindings.Get(cfpSym);
        var oldCft = DynamicBindings.Get(cftSym);
        var oldPackage = DynamicBindings.Get(packageSym);
        var oldReadtable = DynamicBindings.Get(readtableSym);
        var cfpInputPath = inputSpec switch
        {
            LispPathname p => p,
            LispString s => LispPathname.FromString(s.Value),
            _ => LispPathname.FromString(inputSpec.ToString())
        };
        var dpd2 = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
        var mergedCfp = MergePathnames(cfpInputPath, dpd2 is LispPathname ? dpd2 : LispPathname.FromString(""));
        DynamicBindings.Set(cfpSym, mergedCfp);
        var cftPath = LispPathname.FromString(Path.GetFullPath(inputPath));
        DynamicBindings.Set(cftSym, cftPath);

        // Set *compile-file-mode* so the Lisp compiler handles eval-when
        // per CLHS 3.2.3.1 (eval :compile-toplevel, emit :load-toplevel).
        // Use Startup.Sym (not SymInPkg) to match the compiled code's LOAD-SYM resolution.
        var compileFileModeSym = Startup.Sym("*COMPILE-FILE-MODE*");
        var oldCompileFileMode = DynamicBindings.Get(compileFileModeSym);
        DynamicBindings.Set(compileFileModeSym, Startup.Sym("T"));

        // Snapshot *modules*. A compile-time (require "x") (e.g. eval-when
        // :compile-toplevel) loads a contrib whose defuns are reverted by the
        // Function/SetfFunction strip in `finally` below — but the *modules*
        // push is a dynamic-var mutation not covered by that strip, so without
        // this it would persist, leaving the module marked-loaded yet undefined.
        // A later load-time (require "x") would then see it in *modules* and
        // skip re-loading, so the contrib's functions stay unbound. Reverting
        // *modules* keeps it consistent with the stripped defuns: the compile-time
        // require becomes fully transient and the load-time require re-loads for real.
        var modulesSym2 = Startup.Sym("*MODULES*");
        var oldModules = DynamicBindings.Get(modulesSym2);

        // :target-features — rebind *features* so reader conditionals (#+/#-) and
        // os-cond macro expansions see the target platform, not the host.
        // Used for cross-compiling asdf.fasl for Linux/Win/macOS from any host.
        var featuresSym = Startup.Sym("*FEATURES*");
        var oldFeatures = DynamicBindings.Get(featuresSym);
        if (targetFeaturesArg != null && targetFeaturesArg is not Nil)
            DynamicBindings.Set(featuresSym, targetFeaturesArg);

        // Snapshot Function / SetfFunction state for all symbols. compile-file
        // may evaluate defun / (defun (setf x) ...) at compile-time so that
        // sibling macros can reference the function during the same file
        // (try-eval), but per ANSI 3.2.3.1 those definitions must NOT
        // leak into the global environment after compile-file returns
        // (otherwise (compile-file foo.lisp) would side-effect (fboundp 'bar)
        // for any defun in foo.lisp — failing pfdietz COMPILE-FILE.* tests).
        var preFn = new System.Collections.Generic.HashSet<Symbol>();
        var preSetf = new System.Collections.Generic.HashSet<Symbol>();
        foreach (var pkg in Package.AllPackages.ToList())
        {
            foreach (var s in pkg.ExternalSymbols)
            {
                if (s.Function != null) preFn.Add(s);
                if (s.SetfFunction != null) preSetf.Add(s);
            }
            foreach (var s in pkg.InternalSymbols)
            {
                if (s.Function != null) preFn.Add(s);
                if (s.SetfFunction != null) preSetf.Add(s);
            }
        }

        // FASL module name must be computed before the module-ID binding (declared below).
        // :module-name pins it to a stable string (build-time-link / AOT); otherwise
        // a per-compile guid suffix keeps module names (and thus LTV slot-ID
        // namespaces) unique across independently-compiled FASLs.
        var faslModuleName = (moduleNameArg is LispString mns && mns.Value.Length > 0)
            ? mns.Value
            : Path.GetFileNameWithoutExtension(outputPath)
                + "_" + Guid.NewGuid().ToString("N").Substring(0, 8);

        // Bind *current-module-id* to the FASL's unique module name.
        // The load-time-value compiler handler uses this to namespace LTV slot IDs per module,
        // preventing cross-run collisions when FASLs compiled in different sessions share IDs.
        // Use Startup.Sym (not SymInPkg) so we get the same symbol object that cil-out.sil's
        // (:LOAD-SYM "*CURRENT-MODULE-ID*") resolves to — cross-compiled code uses bare LOAD-SYM
        // (because *cross-compiling* suppresses LOAD-SYM-PKG emission), so the symbol lives in
        // DOTCL-INTERNAL, not DOTCL.CIL-COMPILER.
        Symbol? moduleIdSym = null;
        try { moduleIdSym = Startup.Sym("*CURRENT-MODULE-ID*"); }
        catch { /* should not throw, but guard defensively */ }
        if (moduleIdSym != null)
            DynamicBindings.Push(moduleIdSym, new LispString(faslModuleName));

        try
        {
            // Read source, compile each form, and write instrList to .sil
            var source = File.ReadAllText(inputPath);
            var trackingReader = new PositionTrackingReader(new StringReader(source));
            var reader = new Reader(trackingReader);
            var compileStream = new LispStringInputStream(trackingReader, 0);
            reader.LispStreamRef = compileStream;
            // Link this reader as the stream's live reader and share the #n=/#n#
            // tables with the stream. A Lisp reader macro (e.g. SBCL's cold-build
            // read-list on "(") reads elements via (read stream); without this link
            // ReadFromStream spins up a second Reader whose per-form table clearing
            // runs once per ELEMENT, so a #n= registered in one element is gone by
            // the time the sibling #n# is read — the placeholder then leaks into
            // the compiled fasl as an unresolved constant.
            compileStream.CachedReader = reader;
            reader.AdoptStreamShareTables(compileStream);

            var dir = Path.GetDirectoryName(outputPath);
            if (!string.IsNullOrEmpty(dir) && !Directory.Exists(dir))
                Directory.CreateDirectory(dir);

            bool warningsP = false;
            bool failureP = false;

            // Install handler to detect warnings during compilation
            var warningHandler = new LispFunction(args2 =>
            {
                warningsP = true;
                var cond = args2[0];
                // failure-p is T for non-style-warning warnings (e.g., errors, serious warnings)
                if (cond is LispCondition lc && !IsTruthy(Typep(lc, Startup.Sym("STYLE-WARNING"))))
                    failureP = true;
                return Nil.Instance;
            });
            var handlerCluster = new HandlerBinding[] {
                new HandlerBinding(Startup.Sym("WARNING"), warningHandler)
            };
            HandlerClusterStack.PushCluster(handlerCluster);

            // FASL assembler (always — .fasl is the default output)
            var faslAsm = new DotCL.Emitter.FaslAssembler(faslModuleName);

            try
            {
                // SIL text writer (when :sil t or dotcl:*save-sil* is true)
                if (!emitSil)
                {
                    var saveSilSym = Startup.SymInPkg("*SAVE-SIL*", "DOTCL");
                    if (saveSilSym != null && saveSilSym.IsBound && saveSilSym.Value is not Nil)
                        emitSil = true;
                }
                var silPath = emitSil ? Path.ChangeExtension(outputPath, ".sil") : null;
                StreamWriter? writer = silPath != null ? new StreamWriter(silPath) : null;
                try
                {
                writer?.WriteLine(";; -*- dotcl-compiled -*-");
                while (reader.TryRead(out var form))
                {
                    // Line where this top-level form began — used to attribute a
                    // compile error to the source location under a project build.
                    int formLine = reader.LastFormLine;
                    try
                    {
                    foreach (var subForm in FlattenTopLevel(form))
                    {
                        if (IsEvalWhenForCompileFile(subForm, out var ewBody,
                            out bool hasCT, out bool hasLT))
                        {
                            var prognForm = MakeProgn(ewBody);
                            var bodyInstrList = CompileTopLevel(prognForm);

                            if (hasCT)
                                DotCL.Emitter.CilAssembler.AssembleAndRun(bodyInstrList);

                            if (hasLT)
                            {
                                writer?.WriteLine(bodyInstrList.ToString());
                                faslAsm.AddTopLevelForm(bodyInstrList);
                                faslAsm.FlushInitForms();
                            }
                        }
                        else
                        {
                            var instrList = CompileTopLevel(subForm);
                            if (ShouldExecuteAtCompileTime(subForm))
                                DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
                            writer?.WriteLine(instrList.ToString());
                            faslAsm.AddTopLevelForm(instrList);
                            faslAsm.FlushInitForms();
                        }

                        if (isPrint)
                        {
                            var w = GetStandardOutputWriter();
                            w.WriteLine(FormatTop(subForm, true));
                            w.Flush();
                        }
                    }
                    }
                    catch (LispSourceException) { throw; } // already located (nested load)
                    catch (Exception ex) when (EmitBuildSourceLocations
                                               && ex is not System.Threading.ThreadInterruptedException)
                    {
                        throw new LispSourceException(inputPath, formLine, ex);
                    }
                }

                // Emit any remaining init forms (cycle members, etc.)
                faslAsm.FlushRemainingInitForms();

                var corlibName = (corlibNameArg is LispString cln && cln.Value.Length > 0)
                    ? cln.Value
                    : null;
                faslAsm.Save(outputPath, corlibName);
                }
                finally { writer?.Dispose(); }
            }
            finally
            {
                HandlerClusterStack.PopCluster();
            }

            // Return (values output-truename warnings-p failure-p)
            var outPathname = LispPathname.FromString(Path.GetFullPath(outputPath));
            return MultipleValues.Values(outPathname,
                warningsP ? (LispObject)T.Instance : Nil.Instance,
                failureP ? (LispObject)T.Instance : Nil.Instance);
        }
        finally
        {
            // Strip newly-defined Function / SetfFunction values that escaped
            // from compile-time defun/defmethod try-eval. Anything that was already
            // fbound before compile-file is left untouched — only WE clean up
            // the side-effects WE introduced.
            // Also clear the GF registry for stripped symbols so that when the
            // compiled fasl is later loaded, %find-gf returns NIL and the GF is
            // properly re-registered (and sym.Function re-set). Without this,
            // defclass accessors defined at compile-time would be in _gfRegistry
            // but not in sym.Function after the cleanup, causing %find-gf to skip
            // re-registration and leaving the accessor unbound.
            foreach (var pkg in Package.AllPackages.ToList())
            {
                foreach (var s in pkg.ExternalSymbols.Concat(pkg.InternalSymbols).ToList())
                {
                    if (s.Function != null && !preFn.Contains(s))
                    {
                        s.Function = null;
                        Runtime.RemoveGfRegistryEntry(s);
                    }
                    if (s.SetfFunction != null && !preSetf.Contains(s))
                    {
                        s.SetfFunction = null;
                        Runtime.RemoveGfRegistryEntry(s, isSetf: true);
                    }
                }
            }

            // Restore *modules* so a compile-time (require ...) doesn't leak a
            // marked-loaded-but-undefined module (its defuns were stripped above).
            DynamicBindings.Set(modulesSym2, oldModules);

            // Restore *compile-file-mode* and dynamic bindings
            DynamicBindings.Set(compileFileModeSym, oldCompileFileMode);
            if (targetFeaturesArg != null && targetFeaturesArg is not Nil)
                DynamicBindings.Set(featuresSym, oldFeatures);
            DynamicBindings.Set(cfpSym, oldCfp);
            DynamicBindings.Set(cftSym, oldCft);
            DynamicBindings.Set(packageSym, oldPackage);
            DynamicBindings.Set(readtableSym, oldReadtable);
            if (moduleIdSym != null)
                DynamicBindings.Pop(moduleIdSym);
        }
#else
        throw new LispErrorException(new LispProgramError(
            "COMPILE-FILE requires System.Reflection.Emit, which is unavailable in this runtime build"));
#endif
    }

    // --- save-application (MVP) ---

    /// <lispdoc>(dotcl:save-application output-path &amp;key load system toplevel executable r2r no-self-contained target runtime-csproj) -- Bundle Lisp sources into a .fasl or self-contained exe. :system collects ASDF transitive deps; :r2r t enables ReadyToRun ahead-of-time compilation; :no-self-contained t omits the .NET runtime (smaller binary, requires .NET on target); :target :linux-arm64 etc. for cross-platform publish; :runtime-csproj or DOTCL_RUNTIME_CSPROJ env var for installed-tool use.</lispdoc>
    /// <summary>
    /// <c>(dotcl:save-application output-path &amp;key load system toplevel executable target runtime-csproj)</c>
    ///
    /// Bundle Lisp sources into a .fasl or a self-contained single-file exe.
    ///
    /// <list type="bullet">
    /// <item><term>:load</term><description>
    ///   Pathname or list of pathnames. Sources are compiled into the output in order.
    /// </description></item>
    /// <item><term>:system</term><description>
    ///   ASDF system name (symbol or string). Collects the transitive set of
    ///   cl-source-file components via <c>asdf:required-components</c> and prepends
    ///   them before :load files. ASDF must already be loaded in the current session.
    /// </description></item>
    /// <item><term>:toplevel</term><description>
    ///   Symbol or "PKG:NAME" string. Appended as <c>(funcall (symbol-function 'X))</c>
    ///   at the end of the fasl's init code.
    /// </description></item>
    /// <item><term>:executable</term><description>
    ///   When true, invokes <c>dotnet publish --self-contained -p:PublishSingleFile=true</c>
    ///   to produce a standalone exe with the .NET runtime bundled.
    /// </description></item>
    /// <item><term>:target</term><description>
    ///   Cross-platform RID keyword: :win-x64, :win-arm64, :linux-x64, :linux-arm64,
    ///   :osx-x64, :osx-arm64. Defaults to the current host RID.
    ///   Only meaningful with :executable t.
    /// </description></item>
    /// <item><term>:runtime-csproj</term><description>
    ///   Override path to runtime.csproj for :executable t builds.
    ///   Also honoured via the DOTCL_RUNTIME_CSPROJ environment variable.
    /// </description></item>
    /// </list>
    ///
    /// State reconstruction note: defvar/defpackage/reader-macros defined in
    /// the compiled sources are re-evaluated as part of the fasl's ModuleInit, so
    /// they are reconstructed naturally. Runtime-only state mutations are not captured.
    /// </summary>
    [LispDoc("SAVE-APPLICATION")]
    public static LispObject SaveApplication(LispObject[] args)
    {
#if DOTCL_EMIT
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "SAVE-APPLICATION: requires output-path"));

        string outputPath = ResolvePhysicalPath(args[0]);

        LispObject? loadArg = null;
        LispObject? toplevelArg = null;
        bool isExecutable = false;
        bool isR2r = false;
        bool noSelfContained = false;
        string? targetRid = null;
        string? runtimeCsprojOverride = null;
        LispObject? systemArg = null;

        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "SAVE-APPLICATION: odd number of keyword arguments"));
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            string key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            switch (key)
            {
                case "LOAD": loadArg = args[i + 1]; break;
                case "TOPLEVEL": toplevelArg = args[i + 1]; break;
                case "EXECUTABLE": isExecutable = args[i + 1] is not Nil; break;
                case "R2R": isR2r = args[i + 1] is not Nil; break;
                case "NO-SELF-CONTAINED": noSelfContained = args[i + 1] is not Nil; break;
                case "SYSTEM":
                    if (args[i + 1] is not Nil)
                        systemArg = args[i + 1];
                    break;
                case "TARGET":
                    if (args[i + 1] is not Nil)
                        targetRid = MapTargetToRid(args[i + 1]);
                    break;
                case "RUNTIME-CSPROJ":
                    if (args[i + 1] is not Nil)
                        runtimeCsprojOverride = ResolvePhysicalPath(args[i + 1]);
                    break;
                default:
                    throw new LispErrorException(new LispProgramError(
                        $"SAVE-APPLICATION: unknown keyword :{key}"));
            }
        }

        // Collect ASDF system sources (transitive, topologically ordered) if :system given.
        // This requires ASDF to be loaded in the current session.
        var asdfsources = new List<string>();
        if (systemArg != null)
            asdfsources = CollectAsdfSystemSources(systemArg);

        // Normalize :load into a list of source paths.
        var userSources = new List<string>();
        if (loadArg != null && loadArg is not Nil)
        {
            if (loadArg is Cons)
            {
                var cur = loadArg;
                while (cur is Cons lc)
                {
                    userSources.Add(ResolvePhysicalPath(lc.Car));
                    cur = lc.Cdr;
                }
            }
            else
            {
                userSources.Add(ResolvePhysicalPath(loadArg));
            }
        }

        // ASDF sources come first (they're the deps); user :load files come after.
        var sources = new List<string>(asdfsources.Count + userSources.Count);
        sources.AddRange(asdfsources);
        sources.AddRange(userSources);

        // Extract the toplevel entry. Accept Symbol directly, or a string
        // "PKG:NAME" / "NAME" which is resolved at save time (after :load files
        // have been compiled, so any package defined there is available).
        // String form is necessary when the calling build.lisp cannot reference
        // the entry package at read time (it doesn't exist yet).
        LispObject? toplevelDesignator = toplevelArg is Nil ? null : toplevelArg;

        // Core mode: write the .fasl directly to outputPath.
        // Exec mode: write the .fasl to a temp path, then invoke `dotnet publish`
        // to produce a self-contained single-file exe with the .fasl embedded.
        string faslPath = isExecutable
            ? Path.Combine(Path.GetTempPath(), $"dotcl-saveapp-{Guid.NewGuid():N}.fasl")
            : outputPath;

        var outDir = Path.GetDirectoryName(outputPath);
        if (!string.IsNullOrEmpty(outDir) && !Directory.Exists(outDir))
            Directory.CreateDirectory(outDir);

        try
        {
            BuildFaslFromSources(faslPath, sources, toplevelDesignator);
            if (isExecutable)
                BuildExecutableFromFasl(faslPath, outputPath, targetRid, runtimeCsprojOverride, isR2r, noSelfContained);
        }
        finally
        {
            if (isExecutable)
                try { File.Delete(faslPath); } catch { /* best-effort cleanup */ }
        }

        return LispPathname.FromString(Path.GetFullPath(outputPath));
#else
        throw new LispErrorException(new LispProgramError(
            "SAVE-APPLICATION requires System.Reflection.Emit, which is unavailable in this runtime build"));
#endif
    }

#if DOTCL_EMIT
    /// <summary>
    /// Compile :load sources into the FASL at `faslPath`, append an entry call form
    /// if a toplevel designator was given, save. Shared between save-application's
    /// :executable nil and :executable t paths.
    /// </summary>
    private static void BuildFaslFromSources(
        string faslPath, List<string> sources, LispObject? toplevelDesignator)
    {
        var moduleName = Path.GetFileNameWithoutExtension(faslPath)
            + "_" + Guid.NewGuid().ToString("N").Substring(0, 8);
        var faslAsm = new DotCL.Emitter.FaslAssembler(moduleName);

        var compileFileModeSym = Startup.Sym("*COMPILE-FILE-MODE*");
        var oldCompileFileMode = DynamicBindings.Get(compileFileModeSym);
        DynamicBindings.Set(compileFileModeSym, Startup.Sym("T"));

        var packageSym = Startup.Sym("*PACKAGE*");
        var oldPackage = DynamicBindings.Get(packageSym);

        try
        {
            // Prepend (require "asdf") so UIOP is available when sources
            // compiled with ASDF deps load in a standalone exe context.
            if (sources.Count > 0)
            {
                var requireForm = new Cons(Startup.Sym("REQUIRE"),
                    new Cons(new LispString("asdf"), Nil.Instance));
                faslAsm.AddTopLevelForm(CompileTopLevel(requireForm));
            }

            foreach (var src in sources)
                CompileSourceInto(src, faslAsm);

            if (toplevelDesignator != null)
            {
                // Resolve the toplevel designator *now* (after sources have been
                // compiled, so any package defined in them is available).
                Symbol toplevelSym = toplevelDesignator switch
                {
                    Symbol s => s,
                    LispString lstr => ResolveSymbolName(lstr.Value),
                    _ => throw new LispErrorException(new LispProgramError(
                        "SAVE-APPLICATION: :toplevel must be a symbol or string"))
                };

                // Set *image-dumped-p* to :executable so uiop:command-line-arguments
                // returns user args correctly (without looking for -- separator).
                // Uses find-package/find-symbol to avoid compile-time symbol interning
                // issues across fresh runtimes (dotcl serializes symbols by name+pkg).
                // uiop:getcwd is now implemented via dotcl:getcwd in UIOP's os.lisp patch.
                {
                    const string patchUiop =
                        "(let ((uiop-pkg (find-package \"UIOP\")))" +
                        "  (when uiop-pkg" +
                        "    (let ((dumped-sym (find-symbol \"*IMAGE-DUMPED-P*\" uiop-pkg)))" +
                        "      (when dumped-sym (setf (symbol-value dumped-sym) :executable)))))";
                    var patchReader = new Reader(new System.IO.StringReader(patchUiop));
                    if (patchReader.TryRead(out var patchForm) && patchForm != null)
                        faslAsm.AddTopLevelForm(CompileTopLevel(patchForm));
                }

                // Append `(funcall (symbol-function 'TOPLEVEL))` — late-bound
                // so redefinitions (e.g. by asdf) resolve to the current definition
                // at runtime.
                var quoteSym = Startup.Sym("QUOTE");
                var funcallSym = Startup.Sym("FUNCALL");
                var symFunSym = Startup.Sym("SYMBOL-FUNCTION");
                var entryForm = new Cons(funcallSym,
                    new Cons(new Cons(symFunSym,
                        new Cons(new Cons(quoteSym,
                            new Cons(toplevelSym, Nil.Instance)), Nil.Instance)),
                        Nil.Instance));
                var instrList = CompileTopLevel(entryForm);
                faslAsm.AddTopLevelForm(instrList);
            }

            faslAsm.Save(faslPath);
        }
        finally
        {
            DynamicBindings.Set(compileFileModeSym, oldCompileFileMode);
            DynamicBindings.Set(packageSym, oldPackage);
        }
    }

    /// <summary>
    /// Invoke `dotnet publish` on runtime.csproj with the user fasl embedded as a
    /// manifest resource, producing a self-contained single-file exe at `outputExe`.
    ///
    /// MVP scope: requires the dotcl source tree to be accessible (runtime.csproj).
    /// The installed-tool-only case is a follow-up (no source tree).
    /// </summary>
    private static void BuildExecutableFromFasl(
        string userFaslPath, string outputExe,
        string? targetRid = null, string? runtimeCsprojOverride = null,
        bool readyToRun = false, bool noSelfContained = false)
    {
        var runtimeCsproj = runtimeCsprojOverride
            ?? FindRuntimeCsproj()
            ?? throw new LispErrorException(new LispProgramError(
                "SAVE-APPLICATION :EXECUTABLE T: runtime.csproj not found. "
                + "Provide the path via :runtime-csproj or the DOTCL_RUNTIME_CSPROJ "
                + "environment variable, or run from the dotcl source tree."));

        // Make sure dotcl.core is present next to runtime.csproj so the
        // published exe bundles it. If it's missing, copy from compiler/dotcl.core
        // (produced by `make compile-core-fasl`) as a best-effort.
        var runtimeDir = Path.GetDirectoryName(runtimeCsproj)!;
        var coreInRuntimeDir = Path.Combine(runtimeDir, "dotcl.core");
        bool coreCopiedByUs = false;
        if (!File.Exists(coreInRuntimeDir))
        {
            var coreSrc = FindBuiltCore()
                ?? throw new LispErrorException(new LispProgramError(
                    "SAVE-APPLICATION :EXECUTABLE T: dotcl.core not found. "
                    + "Run `make compile-core-fasl` first."));
            File.Copy(coreSrc, coreInRuntimeDir);
            coreCopiedByUs = true;
        }

        var publishOut = Path.Combine(Path.GetTempPath(),
            $"dotcl-publish-{Guid.NewGuid():N}");
        var rid = targetRid
            ?? System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier;

        try
        {
            var psi = new System.Diagnostics.ProcessStartInfo("dotnet")
            {
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };
            psi.ArgumentList.Add("publish");
            psi.ArgumentList.Add(runtimeCsproj);
            psi.ArgumentList.Add("-c"); psi.ArgumentList.Add("Release");
            psi.ArgumentList.Add("-r"); psi.ArgumentList.Add(rid);
            psi.ArgumentList.Add(noSelfContained ? "--no-self-contained" : "--self-contained");
            psi.ArgumentList.Add("-p:PublishSingleFile=true");
            if (!noSelfContained)
                psi.ArgumentList.Add("-p:EnableCompressionInSingleFile=true");
            psi.ArgumentList.Add("-p:PackAsTool=false");
            psi.ArgumentList.Add($"-p:DotclUserFasl={userFaslPath}");
            if (readyToRun)
                psi.ArgumentList.Add("-p:PublishReadyToRun=true");
            psi.ArgumentList.Add("-o"); psi.ArgumentList.Add(publishOut);

            using var proc = System.Diagnostics.Process.Start(psi)
                ?? throw new InvalidOperationException("failed to start dotnet");
            string stdout = proc.StandardOutput.ReadToEnd();
            string stderr = proc.StandardError.ReadToEnd();
            proc.WaitForExit();
            if (proc.ExitCode != 0)
                throw new LispErrorException(new LispError(
                    $"SAVE-APPLICATION: dotnet publish failed (exit {proc.ExitCode})."
                    + $" stderr: {stderr.TrimEnd()} stdout tail: {TailLines(stdout, 10)}"));

            // Default AssemblyName for runtime.csproj is "runtime" — find it and
            // copy to the user's target path.
            var exeName = Compat.IsWindows() ? "runtime.exe" : "runtime";
            var producedExe = Path.Combine(publishOut, exeName);
            if (!File.Exists(producedExe))
                throw new LispErrorException(new LispError(
                    $"SAVE-APPLICATION: expected publish output at {producedExe}"));

            File.Copy(producedExe, outputExe, overwrite: true);

            // Copy dotcl.core and contrib/ alongside the output exe.
            // These are needed at runtime: dotcl.core for the stdlib,
            // contrib/ for (require "asdf") and other modules.
            var outputDir = Path.GetDirectoryName(outputExe)!;

            var publishedCore = Path.Combine(publishOut, "dotcl.core");
            var coreDestPath = Path.Combine(outputDir, "dotcl.core");
            if (File.Exists(publishedCore))
                File.Copy(publishedCore, coreDestPath, overwrite: true);
            else
            {
                var coreSrcFallback = FindBuiltCore();
                if (coreSrcFallback != null)
                    File.Copy(coreSrcFallback, coreDestPath, overwrite: true);
            }

            // Copy contrib/ (module fasls including asdf) so require works at runtime.
            // Prefer the publish output's contrib; fall back to the runtime source tree.
            var publishedContrib = Path.Combine(publishOut, "contrib");
            var contribDest = Path.Combine(outputDir, "contrib");
            if (Directory.Exists(publishedContrib))
                CopyDirectory(publishedContrib, contribDest);

            // The publish output may omit runtime/contrib/asdf (gitignored, not in
            // Content items). Copy it explicitly from the runtime source tree.
            var runtimeContribAsdf = Path.Combine(runtimeDir, "contrib", "asdf");
            var destContribAsdf = Path.Combine(contribDest, "asdf");
            if (Directory.Exists(runtimeContribAsdf) && !Directory.Exists(destContribAsdf))
                CopyDirectory(runtimeContribAsdf, destContribAsdf);
        }
        finally
        {
            try { if (Directory.Exists(publishOut)) Directory.Delete(publishOut, true); } catch { }
            if (coreCopiedByUs)
                try { File.Delete(coreInRuntimeDir); } catch { }
        }
    }
#endif

    private static string? FindRuntimeCsproj()
    {
        var envPath = System.Environment.GetEnvironmentVariable("DOTCL_RUNTIME_CSPROJ");
        if (!string.IsNullOrEmpty(envPath) && File.Exists(envPath))
            return envPath;

        var baseDir = AppContext.BaseDirectory;
        var candidates = new[]
        {
            // dev tree: runtime/bin/{Debug,Release}/netXX → 4 levels up to repo root
            Path.Combine(baseDir, "..", "..", "..", "..", "runtime", "runtime.csproj"),
            // less common: exe living next to the csproj
            Path.Combine(baseDir, "runtime.csproj"),
        };
        return candidates.Select(Path.GetFullPath).FirstOrDefault(File.Exists);
    }

    private static string MapTargetToRid(LispObject target)
    {
        string name = target is Symbol s ? s.Name
                    : target is LispString ls ? ls.Value
                    : target.ToString();
        return name.ToUpperInvariant() switch
        {
            "WIN-X64"    or "WIN-AMD64"   => "win-x64",
            "WIN-X86"                     => "win-x86",
            "WIN-ARM64"                   => "win-arm64",
            "LINUX-X64"  or "LINUX-AMD64" => "linux-x64",
            "LINUX-ARM64"                 => "linux-arm64",
            "LINUX-ARM"  or "LINUX-ARM32" => "linux-arm",
            "OSX-X64"    or "OSX-AMD64"   => "osx-x64",
            "OSX-ARM64"  or "MAC-ARM64"   => "osx-arm64",
            _ => throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION: unknown :target {name}. "
                + "Use :win-x64, :win-arm64, :linux-x64, :linux-arm64, :osx-x64, :osx-arm64"))
        };
    }

    /// <summary>
    /// Use ASDF (must already be loaded) to collect the transitive set of
    /// CL source files for SYSTEM-DESIGNATOR, in topological load order.
    /// Uses asdf:required-components with :component-type 'asdf:cl-source-file.
    /// </summary>
    private static List<string> CollectAsdfSystemSources(LispObject systemDesignator)
    {
        // Convert designator to a lowercase string for ASDF
        string sysName = systemDesignator switch
        {
            Symbol sym    => sym.Name.ToLowerInvariant(),
            LispString ls => ls.Value.ToLowerInvariant(),
            _             => systemDesignator.ToString().ToLowerInvariant()
        };

        // Evaluate: (mapcar (lambda (c) (namestring (asdf:component-pathname c)))
        //              (remove-if-not (lambda (c) (typep c 'asdf:cl-source-file))
        //                (asdf:required-components (asdf:find-system "<name>")
        //                  :other-systems t)))
        // Returns a Lisp list of namestring strings.
        var exprStr = $@"(mapcar (lambda (c) (namestring (asdf:component-pathname c)))
                           (remove-if-not
                             (lambda (c) (typep c 'asdf:cl-source-file))
                             (asdf:required-components
                               (asdf:find-system ""{sysName}"")
                               :other-systems t)))";

        LispObject result;
        try
        {
            var reader = new Reader(new System.IO.StringReader(exprStr));
            if (!reader.TryRead(out var form))
                throw new InvalidOperationException("failed to read ASDF query form");
            result = Runtime.Eval(form!);
        }
        catch (Exception ex) when (ex is not LispErrorException)
        {
            throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION :system {sysName}: error querying ASDF: {ex.Message}. "
                + "Ensure ASDF is loaded before calling save-application with :system."));
        }

        var files = new List<string>();
        var cur = result;
        while (cur is Cons c)
        {
            if (c.Car is LispString ls)
                files.Add(ls.Value);
            else if (c.Car is LispPathname pn)
                files.Add(pn.ToNamestring());
            cur = c.Cdr;
        }
        return files;
    }

    private static string? FindBuiltCore()
    {
        var baseDir = AppContext.BaseDirectory;
        var candidates = new[]
        {
            // dev tree: produced by `make compile-core-fasl`
            Path.Combine(baseDir, "..", "..", "..", "..", "compiler", "dotcl.core"),
            // bundled next to running exe
            Path.Combine(baseDir, "dotcl.core"),
        };
        return candidates.Select(Path.GetFullPath).FirstOrDefault(File.Exists);
    }

    private static string TailLines(string s, int n)
    {
        if (string.IsNullOrEmpty(s)) return "";
        var lines = s.TrimEnd().Split('\n');
        int start = Math.Max(0, lines.Length - n);
        return string.Join("\n", lines.Skip(start));
    }

    private static void CopyDirectory(string src, string dst)
    {
        Directory.CreateDirectory(dst);
        foreach (var file in Directory.GetFiles(src))
            File.Copy(file, Path.Combine(dst, Path.GetFileName(file)), overwrite: true);
        foreach (var dir in Directory.GetDirectories(src))
            CopyDirectory(dir, Path.Combine(dst, Path.GetFileName(dir)));
    }

    /// <summary>
    /// Resolve a "PKG:NAME" / "PKG::NAME" / "NAME" string into a Symbol.
    /// Called at save-application time when :toplevel was given as a string.
    /// </summary>
    private static Symbol ResolveSymbolName(string designator)
    {
        int idx = designator.IndexOf(':');
        if (idx < 0)
            return Startup.Sym(designator.ToUpperInvariant());

        string pkgName = designator.Substring(0, idx).ToUpperInvariant();
        int nameStart = idx + 1;
        if (nameStart < designator.Length && designator[nameStart] == ':')
            nameStart++;
        string symName = designator.Substring(nameStart).ToUpperInvariant();

        var pkg = Package.FindPackage(pkgName);
        if (pkg == null)
            throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION: package {pkgName} not found for :toplevel {designator}"));
        var (sym, _) = pkg.Intern(symName);
        return sym;
    }

#if DOTCL_EMIT
    /// <summary>
    /// Read a .lisp source and emit each top-level form into the given FASL assembler.
    /// Mirrors the core loop of CompileFile but without warnings tracking or SIL output
    /// (MVP scope). Respects eval-when per CLHS 3.2.3.1.
    /// </summary>
    private static void CompileSourceInto(string inputPath, DotCL.Emitter.FaslAssembler faslAsm)
    {
        if (!File.Exists(inputPath))
            throw new LispErrorException(new LispProgramError(
                $"SAVE-APPLICATION: source file not found: {inputPath}"));

        var source = File.ReadAllText(inputPath);
        var reader = new Reader(new StringReader(source));

        while (reader.TryRead(out var form))
        {
            foreach (var subForm in FlattenTopLevel(form))
            {
                if (IsEvalWhenForCompileFile(subForm, out var ewBody,
                    out bool hasCT, out bool hasLT))
                {
                    var prognForm = MakeProgn(ewBody);
                    var bodyInstrList = CompileTopLevel(prognForm);
                    if (hasCT)
                        DotCL.Emitter.CilAssembler.AssembleAndRun(bodyInstrList);
                    if (hasLT)
                        faslAsm.AddTopLevelForm(bodyInstrList);
                }
                else
                {
                    var instrList = CompileTopLevel(subForm);
                    if (ShouldExecuteAtCompileTime(subForm))
                        DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
                    faslAsm.AddTopLevelForm(instrList);
                }
            }
        }
    }
#endif

    // --- Eval ---

    /// <summary>
    /// Process-wide lock that serializes the compile/run path inside Eval.
    /// dotcl's mutable runtime state (*macros* table, CLOS dispatch, cons
    /// internals, hashtable contents, compiler state) is not yet fully
    /// race-free, so we make Eval implicitly single-threaded at the
    /// runtime level. Hosts (DotclHost callers, McpServer, ASP.NET
    /// controllers) do not need to add their own _evalLock.
    /// `lock` is reentrant on the same thread, so Lisp → C# → Lisp
    /// callbacks (e.g. dotnet:funcall, condition handlers) work fine.
    /// Removing this serialization is future work; the preparation to make
    /// individual operations race-free is already in place.
    /// </summary>
    internal static readonly object _evalLock = new();

    /// <summary>
    /// When true (default), Eval serializes the compound-form compile/run path on
    /// <see cref="_evalLock"/>. Opt out (DOTCL_PARALLEL_EVAL=1, or
    /// dotcl:set-parallel-eval) to run eval concurrently: the runtime state the
    /// compile/run path touches is individually race-safe (Symbol/Package/Modules
    /// concurrent-safe; CLOS method lists copy-on-write; constant pool + function
    /// registry locked; *macros* a synchronized table), so many threads can
    /// compile/call without the global lock. Proving full race-freedom is a
    /// non-goal — user-level cons/array mutation stays the caller's responsibility,
    /// as in any CL. Intended for hosts that call compiled functions from many
    /// threads (e.g. a probe/REPL attached to a multi-threaded .NET service).
    /// </summary>
    internal static bool SerializeEval =
        System.Environment.GetEnvironmentVariable("DOTCL_PARALLEL_EVAL")
            is not ("1" or "true" or "TRUE" or "yes");

    public static LispObject Eval(LispObject form)
    {
        if (!Compat.TryEnsureSufficientExecutionStack())
            throw new LispErrorException(new LispProgramError("Stack overflow in eval"));
        // Fast path for self-evaluating forms: skip full compilation AND
        // skip the eval lock. These return paths touch no shared mutable
        // runtime state (Number/LispChar/LispString/T/Nil are immutable;
        // Symbol value read via per-thread DynamicBindings is already
        // thread-safe). Avoiding the lock here keeps tight inner loops
        // (e.g. SBCL's define-type-vop using #'eval as a reduce key) cheap.
        if (form is Number || form is LispChar || form is LispString || form is T || form is Nil)
            return form;
        // Read via DynamicBindings so the per-thread dynamic binding (Phase B)
        // is honored. Reading sym.Value directly would miss a thread-local
        // binding (e.g. (let ((*x* ...)) (eval '*x*))).
        if (form is Symbol sym2 && DynamicBindings.TryGet(sym2, out var sym2val))
            return sym2val;
        // Compound forms hit the compiler + assembler, both of which mutate
        // shared runtime state. Serialize from here unless the host opted into
        // parallel eval (see SerializeEval).
        if (SerializeEval)
            lock (_evalLock)
                return EvalCompound(form);
        return EvalCompound(form);
    }

    private static LispObject EvalCompound(LispObject form)
    {
#if DOTCL_EMIT
        // :interpret mode routes compound forms through the emit-free tree-walk
        // interpreter; :compile (default) uses the CIL compiler below.
        if (UseInterpreter())
            return MiniEval(form);
        // Use eval-specific compile path that preserves MvReturn at tail
        // so callers can observe multiple values from form.
        var instrList = CompileTopLevelEval(form);
        try
        {
            return DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
        }
        catch (CatchThrowException cte)
        {
            // If a matching (catch tag ...) exists outside this eval, let the exception
            // propagate so the outer catch can handle it.
            if (CatchTagStack.HasMatchingCatch(cte.Tag))
                throw;
            // Unmatched THROW at eval boundary: signal control-error per CL spec
            throw new LispErrorException(new LispControlError(
                $"Attempt to THROW to tag {cte.Tag} but no catching CATCH form was found"));
        }
#else
        // No Reflection.Emit in this build: the tree-walk interpreter is the only
        // way to evaluate a compound form.
        return MiniEval(form);
#endif
    }

    /// <summary>True when eval should use the tree-walk interpreter
    /// (dotcl:*evaluator-mode* is :interpret). Only consulted on emit builds;
    /// emit-free builds always interpret.</summary>
    private static bool UseInterpreter()
    {
        var sym = Startup.SymInPkg("*EVALUATOR-MODE*", "DOTCL");
        return DynamicBindings.Get(sym) is Symbol s && s.Name == "INTERPRET";
    }

    /// <summary>Invoke the Lisp tree-walk interpreter %mini-eval on FORM with an
    /// empty lexical environment. %mini-eval is defined in the compiler sources
    /// (package DOTCL.CIL-COMPILER) and is present in the loaded core.</summary>
    private static LispObject MiniEval(LispObject form)
    {
        var fn = Startup.SymInPkg("%MINI-EVAL", "DOTCL.CIL-COMPILER").Function as LispFunction
            ?? throw new LispErrorException(new LispProgramError(
                "tree-walk interpreter (%mini-eval) is unavailable: the core is not loaded"));
        return fn.Invoke(form, Nil.Instance);
    }

    /// <summary>
    /// Best-effort eval: returns T on success, NIL on error.
    /// Used by compile-eval-when to eval defvar/defmacro at compile time
    /// without crashing if init-forms reference not-yet-defined functions.
    /// Bind *compile-file-mode* to NIL during the inner eval so that the
    /// recursive compile-form for FORM does not retrigger the defun
    /// handler's compile-time eval branch — that would recurse on the
    /// same defun until the depth guard fires.
    /// </summary>
    public static LispObject TryEval(LispObject form)
    {
        var cfmSym = Startup.Sym("*COMPILE-FILE-MODE*");
        DynamicBindings.Push(cfmSym, Nil.Instance);
        try
        {
            Eval(form);
            return T.Instance;
        }
        catch
        {
            return Nil.Instance;
        }
        finally
        {
            DynamicBindings.Pop(cfmSym);
        }
    }

    // --- Gensym ---

    private static Symbol _gensymCounterSym = null!;

    public static void InitGensymCounter()
    {
        _gensymCounterSym = Startup.Sym("*GENSYM-COUNTER*");
        _gensymCounterSym.IsSpecial = true;
        if (!_gensymCounterSym.IsBound)
            _gensymCounterSym.Value = Fixnum.Make(0);
    }

    private static System.Numerics.BigInteger GetGensymCounter()
    {
        if (DynamicBindings.TryGet(_gensymCounterSym, out var v))
            return v is Fixnum f ? f.Value : v is Bignum b ? b.Value : 0;
        return 0;
    }

    private static void SetGensymCounter(System.Numerics.BigInteger val)
    {
        DynamicBindings.Set(_gensymCounterSym,
            val <= long.MaxValue ? (LispObject)Fixnum.Make((long)val) : new Bignum(val));
    }

    public static LispObject Gensym(LispObject prefix)
    {
        // prefix must be a string or non-negative integer (unsigned-byte).
        // CL strings include simple-strings AND character vectors of any element-type
        // CHARACTER/BASE-CHAR/STANDARD-CHAR — accept both LispString and IsCharVector.
        if (prefix is Fixnum fi && fi.Value >= 0)
            return new Symbol($"G{fi.Value}");
        if (prefix is Bignum bi && bi.Value >= 0)
            return new Symbol($"G{bi.Value}");
        string? prefixStr = prefix switch
        {
            LispString s => s.Value,
            LispVector v when v.IsCharVector => v.ToCharString(),
            _ => null,
        };
        if (prefixStr != null)
        {
            ValidateGensymCounter();
            var counter = GetGensymCounter();
            var name = $"{prefixStr}{counter}";
            SetGensymCounter(counter + 1);
            return new Symbol(name);
        }
        // Invalid type: signal type-error
        var expected = new Cons(Startup.Sym("OR"),
            new Cons(Startup.Sym("STRING"),
                new Cons(Startup.Sym("UNSIGNED-BYTE"), Nil.Instance)));
        throw new LispErrorException(new LispTypeError(
            $"GENSYM: argument must be a string or unsigned-byte, not {prefix}",
            prefix, expected));
    }

    private static void ValidateGensymCounter()
    {
        LispObject v = Nil.Instance;
        if (DynamicBindings.TryGet(_gensymCounterSym, out var dv))
            v = dv;
        else if (_gensymCounterSym != null && _gensymCounterSym.IsBound)
            v = _gensymCounterSym.Value;
        else
            return; // unbound = OK, GetGensymCounter returns 0
        bool valid = (v is Fixnum fx && fx.Value >= 0) || (v is Bignum bx && bx.Value >= 0);
        if (!valid)
        {
            var expected = new Cons(Startup.Sym("INTEGER"),
                new Cons(Fixnum.Make(0),
                    new Cons(Startup.Sym("*"), Nil.Instance)));
            throw new LispErrorException(new LispTypeError(
                $"The value of *GENSYM-COUNTER*, {v}, is not a non-negative integer",
                v, expected));
        }
    }

    public static LispObject Gensym0()
    {
        ValidateGensymCounter();
        var counter = GetGensymCounter();
        var name = $"G{counter}";
        SetGensymCounter(counter + 1);
        return new Symbol(name);
    }

    // --- Gentemp ---

    private static long _gentempCounter = 0;

    public static LispObject Gentemp(LispObject[] args)
    {
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError($"GENTEMP: too many arguments: {args.Length} (expected 0-2)"));
        LispObject prefixArg = args.Length > 0 ? args[0] : new LispString("T");
        if (prefixArg is not LispString)
            throw new LispErrorException(new LispTypeError("GENTEMP: prefix must be a string", prefixArg));
        string prefix = ((LispString)prefixArg).Value;
        Package? pkg = null;
        if (args.Length > 1 && args[1] is not Nil)
        {
            var pkgArg = args[1];
            if (pkgArg is Package p2) pkg = p2;
            else if (pkgArg is LispString pkgNameStr) pkg = Package.FindPackage(pkgNameStr.Value);
            else if (pkgArg is Symbol pkgSym) pkg = Package.FindPackage(pkgSym.Name);
            else if (pkgArg is LispChar pkgChar) pkg = Package.FindPackage(pkgChar.Value.ToString());
            else if (pkgArg is LispVector pkgVec && pkgVec.IsCharVector) pkg = Package.FindPackage(pkgVec.ToCharString());
            else throw new LispErrorException(new LispTypeError("GENTEMP: invalid package designator", pkgArg));
            if (pkg == null)
                throw new LispErrorException(new LispTypeError("GENTEMP: package not found", args[1]));
        }
        if (pkg == null) pkg = DynamicBindings.TryGet(Startup.Sym("*PACKAGE*"), out var pv) && pv is Package cp ? cp : Startup.CLUser;
        while (true)
        {
            var counter = System.Threading.Interlocked.Increment(ref _gentempCounter) - 1;
            var name = $"{prefix}{counter}";
            var (existing, status) = pkg.FindSymbol(name);
            if (status == SymbolStatus.None)
            {
                var (newSym, _) = pkg.Intern(name);
                return newSym;
            }
        }
    }

    // --- CharName / NameChar ---

    // Standard CL names + semi-standard + C0 control mnemonics (SBCL-compatible).
    // Some Unicode names from UCD that appear in Quicklisp libs are also listed.
    private static readonly (string name, char ch)[] _charNames = new[]
    {
        // CLHS standard names
        ("Newline", '\n'), ("Space", ' '), ("Rubout", '\x7f'),
        ("Page", '\f'), ("Tab", '\t'), ("Backspace", '\b'),
        ("Return", '\r'), ("Linefeed", '\n'),
        // Semi-standard
        ("Altmode", '\x1b'), ("Delete", '\x7f'), ("Null", '\0'), ("Nul", '\0'),
        ("Escape", '\x1b'),
        // C0 control mnemonics (SBCL additional names)
        ("Soh", '\x01'), ("Stx", '\x02'), ("Etx", '\x03'), ("Eot", '\x04'),
        ("Enq", '\x05'), ("Ack", '\x06'), ("Bel", '\x07'), ("Bell", '\x07'),
        ("Bs",  '\x08'), ("Ht",  '\x09'), ("Lf",  '\x0a'), ("Vt",  '\x0b'),
        ("Ff",  '\x0c'), ("Cr",  '\x0d'), ("So",  '\x0e'), ("Si",  '\x0f'),
        ("Dle", '\x10'), ("Dc1", '\x11'), ("Dc2", '\x12'), ("Dc3", '\x13'),
        ("Dc4", '\x14'), ("Nak", '\x15'), ("Syn", '\x16'), ("Etb", '\x17'),
        ("Can", '\x18'), ("Em",  '\x19'), ("Sub", '\x1a'), ("Esc", '\x1b'),
        ("Fs",  '\x1c'), ("Gs",  '\x1d'), ("Rs",  '\x1e'), ("Us",  '\x1f'),
        // Unicode names that appear in real Lisp libraries
        ("No-break_space", ' '),
        ("Ideographic_space", (char)0x3000),        // U+3000 CJK IDEOGRAPHIC SPACE (cl-str)
        ("Zero_width_no-break_space", (char)0xfeff), // U+FEFF BOM/ZWNBSP (id3v2)
        ("Greek_small_letter_lamda", (char)0x03bb),  // U+03BB GREEK SMALL LETTER LAMDA (fn)
        ("Colon", ':'),                             // U+003A COLON
        ("Nel", (char)0x0085),                        // U+0085 NEXT LINE
        ("Next_line", (char)0x0085),                  // U+0085 NEXT LINE alias
        ("Hyphen-minus", '-'),                       // U+002D HYPHEN-MINUS
        ("Tilde", '~'),                             // U+007E TILDE
        ("Full_stop", '.'),                         // U+002E FULL STOP
        ("Semicolon", ';'),                         // U+003B SEMICOLON
        ("Comma", ','),                             // U+002C COMMA
        ("Solidus", '/'),                           // U+002F SOLIDUS (slash)
        ("Reverse_solidus", (char)0x005c),            // U+005C REVERSE SOLIDUS
        ("Vertical_line", '|'),                     // U+007C VERTICAL LINE
        ("Quotation_mark", '"'),                   // U+0022 QUOTATION MARK
    };

    public static string? CharName(char c)
    {
        foreach (var (name, ch) in _charNames)
            if (ch == c && name != "Linefeed" && name != "Delete" && name != "Nul"
                && name != "Altmode") // only canonical names
                return name;
        // UCD names print with underscores (not spaces) so a #\<name> literal reads
        // back as a single token (CLHS #\ reads one token; name-char accepts both
        // underscores and spaces). This keeps char-name / ~:C / ~@C / prin1 mutually
        // consistent and round-trippable without the reader over-scanning.
        if (Ucd.CharToName.TryGetValue(c, out string? ucdName))
            return ucdName.Replace(' ', '_');
        // Non-graphic characters get U+XXXX names
        if (char.IsControl(c) || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.Format
            || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.Surrogate
            || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.PrivateUse
            || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.OtherNotAssigned)
            return $"U+{(int)c:X4}";
        return null;
    }

    public static char? NameChar(string s)
    {
        var upper = s.ToUpper();
        foreach (var (name, ch) in _charNames)
            if (name.ToUpper() == upper) return ch;
        // UCD lookup: normalize underscores to spaces (e.g. "LATIN_SMALL_LETTER_A" → "LATIN SMALL LETTER A")
        if (Ucd.NameToChar.TryGetValue(upper.Replace('_', ' '), out char ucdCh))
            return ucdCh;
        // Handle U+XXXX format names
        if (upper.StartsWith("U+") && upper.Length >= 3)
        {
            if (int.TryParse(upper.Substring(2), System.Globalization.NumberStyles.HexNumber, null, out int code)
                && code >= 0 && code <= char.MaxValue)
                return (char)code;
        }
        // Handle UXXXX or UXXXXXXXX format (no plus: e.g., u000C, u0085, u0001FFFE)
        // Lengths: 5 (4 hex: u0085), 7 (6 hex: u00FFFF), 9 (8 hex: u0001FFFE)
        if (upper.Length >= 5 && upper[0] == 'U' && upper[1] != '+')
        {
            if (int.TryParse(upper.Substring(1), System.Globalization.NumberStyles.HexNumber, null, out int code2)
                && code2 >= 0 && code2 <= 0x10FFFF)
            {
                // BMP characters: return directly
                if (code2 <= char.MaxValue) return (char)code2;
                // Non-BMP: dotcl's char type is 16-bit; substitute U+FFFD (replacement char).
                // cl-html5-parser uses these as noncharacter sentinels; U+FFFD is an acceptable
                // proxy since it never appears as valid parsed content either.
                return '�';
            }
        }
        return null;
    }

    // --- Helper ---

    public static Number AsNumber(LispObject obj)
    {
        obj = Primary(obj);
        if (obj is Number n) return n;
        throw new LispErrorException(new LispTypeError(
            $"Not a number: {obj}", obj, Startup.Sym("NUMBER")));
    }

    /// <summary>Convert a double to exact rational via IEEE 754 decomposition.</summary>
    public static LispObject DoubleToRational(double d)
    {
        if (double.IsNaN(d) || double.IsInfinity(d))
            throw new LispErrorException(
                new LispError($"RATIONAL: {(double.IsNaN(d) ? "NaN" : "infinity")} cannot be converted to a rational")
                { ConditionTypeName = "FLOATING-POINT-INVALID-OPERATION" });
        if (System.Math.Floor(d) == d && d >= long.MinValue && d <= long.MaxValue)
            return Fixnum.Make((long)d);
        // Use the exact IEEE 754 decomposition: d = mantissa * 2^exponent
        long bits = System.BitConverter.DoubleToInt64Bits(d);
        bool neg = (bits < 0);
        int exp = (int)((bits >> 52) & 0x7FF);
        long mant = bits & 0x000FFFFFFFFFFFFFL;
        if (exp == 0) { exp = 1; } else { mant |= 0x0010000000000000L; } // implicit leading 1
        exp -= 1075; // 1023 (bias) + 52 (mantissa bits)
        System.Numerics.BigInteger num = new System.Numerics.BigInteger(mant);
        System.Numerics.BigInteger den = System.Numerics.BigInteger.One;
        if (exp >= 0) { num <<= exp; }
        else { den <<= -exp; }
        if (neg) num = -num;
        var g = System.Numerics.BigInteger.GreatestCommonDivisor(System.Numerics.BigInteger.Abs(num), den);
        num /= g; den /= g;
        if (den == 1) return Bignum.MakeInteger(num);
        return Ratio.Make(num, den);
    }

    // ===== Environment functions =====

    public static LispObject LispImplementationType(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("LISP-IMPLEMENTATION-TYPE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString("dotcl");
    }

    public static LispObject LispImplementationVersion(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("LISP-IMPLEMENTATION-VERSION: wrong number of arguments: " + args.Length + " (expected 0)"));
        var version = typeof(Runtime).Assembly
            .GetCustomAttribute<System.Reflection.AssemblyInformationalVersionAttribute>()
            ?.InformationalVersion ?? "unknown";
        return new LispString(version);
    }

    public static LispObject SoftwareType(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("SOFTWARE-TYPE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(System.Runtime.InteropServices.RuntimeInformation.OSDescription);
    }

    public static LispObject SoftwareVersion(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("SOFTWARE-VERSION: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(Environment.OSVersion.VersionString);
    }

    public static LispObject MachineType(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("MACHINE-TYPE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(System.Runtime.InteropServices.RuntimeInformation.OSArchitecture.ToString());
    }

    public static LispObject MachineVersion(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("MACHINE-VERSION: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture.ToString());
    }

    public static LispObject MachineInstance(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("MACHINE-INSTANCE: wrong number of arguments: " + args.Length + " (expected 0)"));
        return new LispString(Environment.MachineName);
    }

    public static LispObject ShortSiteName(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("SHORT-SITE-NAME: wrong number of arguments: " + args.Length + " (expected 0)"));
        return Nil.Instance;
    }

    public static LispObject LongSiteName(LispObject[] args)
    {
        if (args.Length != 0) throw new LispErrorException(new LispProgramError("LONG-SITE-NAME: wrong number of arguments: " + args.Length + " (expected 0)"));
        return Nil.Instance;
    }

    public static LispObject AproposList(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("APROPOS-LIST: requires at least 1 argument"));
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError("APROPOS-LIST: wrong number of arguments: " + args.Length + " (expected 1-2)"));
        string searchStr;
        if (args[0] is LispString ls) searchStr = ls.Value;
        else if (args[0] is Symbol sym) searchStr = sym.Name;
        else if (args[0] is LispChar lc) searchStr = lc.Value.ToString();
        else if (args[0] is LispVector lv && lv.IsCharVector) searchStr = lv.ToCharString();
        else throw new LispErrorException(new LispTypeError("APROPOS-LIST: argument must be a string designator", args[0]));

        searchStr = searchStr.ToUpperInvariant();

        IEnumerable<Package> packages;
        if (args.Length >= 2 && !(args[1] is Nil))
        {
            var pkg = ResolvePackage(args[1], "APROPOS-LIST");
            packages = new[] { pkg };
        }
        else
        {
            packages = Package.AllPackages;
        }

        LispObject result = Nil.Instance;
        var seen = new HashSet<Symbol>();
        foreach (var pkg in packages)
        {
            foreach (var sym in pkg.ExternalSymbols)
            {
                if (sym.Name.ToUpperInvariant().Contains(searchStr) && seen.Add(sym))
                    result = new Cons(sym, result);
            }
            foreach (var sym in pkg.InternalSymbols)
            {
                if (sym.Name.ToUpperInvariant().Contains(searchStr) && seen.Add(sym))
                    result = new Cons(sym, result);
            }
        }
        return result;
    }

    public static LispObject Apropos(LispObject[] args)
    {
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError("APROPOS: wrong number of arguments: " + args.Length + " (expected 1-2)"));
        var list = AproposList(args);
        var writer = GetStandardOutputWriter();
        var cur = list;
        while (cur is Cons c)
        {
            if (c.Car is Symbol sym)
            {
                writer.Write(sym.Name);
                if (sym.Function != null) writer.Write(" (function)");
                if (sym.Value != null && !sym.IsConstant) writer.Write(" (variable)");
                writer.WriteLine();
            }
            cur = c.Cdr;
        }
        writer.Flush();
        // APROPOS returns no values
        return MultipleValues.Values();
    }

    public static LispObject Describe(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("DESCRIBE: requires at least 1 argument"));
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError("DESCRIBE: wrong number of arguments: " + args.Length + " (expected 1-2)"));
        var obj = args[0];
        var streamArg = args.Length >= 2 ? args[1] : Nil.Instance;

        // Resolve stream designator per CLHS:
        // NIL -> *standard-output*, T -> *terminal-io*, otherwise the stream itself
        LispObject stream;
        if (streamArg is Nil)
        {
            if (!DynamicBindings.TryGet(Startup.Sym("*STANDARD-OUTPUT*"), out stream))
                stream = Startup.Sym("*STANDARD-OUTPUT*").Value ?? Nil.Instance;
        }
        else if (streamArg is T)
        {
            if (!DynamicBindings.TryGet(Startup.Sym("*TERMINAL-IO*"), out stream))
                stream = Startup.Sym("*TERMINAL-IO*").Value ?? Nil.Instance;
        }
        else
        {
            stream = streamArg;
        }

        // Call describe-object GF
        var describeObjectFn = Startup.Sym("DESCRIBE-OBJECT").Function as LispFunction;
        if (describeObjectFn != null)
        {
            describeObjectFn.Invoke(new LispObject[] { obj, stream });
        }
        else
        {
            // Fallback if GF not yet registered
            var writer = GetOutputWriter(streamArg);
            var typeObj = TypeOf(obj);
            writer.Write(FormatObject(obj, true));
            writer.WriteLine();
            writer.Write("  [");
            writer.Write(FormatObject(typeObj, true));
            writer.WriteLine("]");
            writer.Flush();
        }

        // DESCRIBE returns no values
        return MultipleValues.Values();
    }

    public static LispObject Room(LispObject[] args)
    {
        if (args.Length > 1)
            throw new LispErrorException(new LispProgramError("ROOM: wrong number of arguments: " + args.Length + " (expected 0-1)"));
        var writer = GetStandardOutputWriter();
        long totalMem = GC.GetTotalMemory(false);
        long allocated = Compat.GetTotalAllocatedBytes(false);
        writer.WriteLine($"Total memory: {totalMem:N0} bytes");
        writer.WriteLine($"Total allocated: {allocated:N0} bytes");
        writer.WriteLine($"GC generation 0 collections: {GC.CollectionCount(0)}");
        writer.WriteLine($"GC generation 1 collections: {GC.CollectionCount(1)}");
        writer.WriteLine($"GC generation 2 collections: {GC.CollectionCount(2)}");
        writer.Flush();
        return Nil.Instance;
    }

    // Returns (gen0-count gen1-count gen2-count total-memory total-allocated-bytes)
    // Used by the TIME macro to compute GC deltas around a form.
    public static LispObject GcStats(LispObject[] args)
    {
        if (args.Length != 0)
            throw new LispErrorException(new LispProgramError("GC-STATS: takes no arguments"));
        long gen0 = GC.CollectionCount(0);
        long gen1 = GC.CollectionCount(1);
        long gen2 = GC.CollectionCount(2);
        long totalMem = GC.GetTotalMemory(false);
        long allocated = Compat.GetTotalAllocatedBytes(false);
        return new Cons(Fixnum.Make(gen0),
               new Cons(Fixnum.Make(gen1),
               new Cons(Fixnum.Make(gen2),
               new Cons(Fixnum.Make(totalMem),
               new Cons(Fixnum.Make(allocated), Nil.Instance)))));
    }

    // COMPILER-MACRO-FUNCTION body (shared by the args-array wrapper and the
    // 1-arg direct delegate).
    private static LispObject CompilerMacroFunctionImpl(LispObject nameArg)
    {
        // (setf foo) form: a cons (setf . (foo . nil))
        if (nameArg is Cons setfCons && setfCons.Car is Symbol setfSym && setfSym.Name == "SETF") {
            var innerSym = (setfCons.Cdr is Cons c2) ? c2.Car as Symbol : null;
            if (innerSym != null && _setfCompilerMacros.TryGetValue(innerSym, out var sfn))
                return sfn;
            return Nil.Instance;
        }
        var sym = nameArg as Symbol;
        if (sym == null) return Nil.Instance;
        return _compilerMacroFunctions.TryGetValue(sym, out var fn)
            ? (LispObject)fn : Nil.Instance;
    }

    // FBOUNDP body (shared by the args-array wrapper and the 1-arg direct delegate).
    private static LispObject FboundpImpl(LispObject obj)
    {
        if (obj is Cons setfCons && setfCons.Car is Symbol setfKw && setfKw.Name == "SETF")
        {
            if (setfCons.Cdr is not Cons setfRest || setfRest.Cdr is not Nil)
                throw new LispErrorException(new LispTypeError("FBOUNDP: invalid (setf ...) form", obj));
            if (setfRest.Car is not Symbol setfName)
                throw new LispErrorException(new LispTypeError("FBOUNDP: second element of (setf ...) must be a symbol", setfRest.Car));
            // sym.SetfFunction is authoritative.
            return setfName.SetfFunction != null ? T.Instance : Nil.Instance;
        }
        Symbol s;
        if (obj is Symbol sym2) s = sym2;
        else if (obj is Nil) s = Startup.NIL_SYM;
        else if (obj is T) s = Startup.T_SYM;
        else throw new LispErrorException(new LispTypeError("FBOUNDP: not a symbol", obj));
        if (s.Function != null) return T.Instance;
        if (Runtime.MacroFunction(s) != Nil.Instance) return T.Instance;
        if (Startup.LookupCompilerMacro(s) != null) return T.Instance;
        if (Runtime._specialOperators.Contains(s.Name)) return T.Instance;
        return Nil.Instance;
    }

    // dotcl:emit-pool-stats — (total-entries dynamic-methods data-literals committed-bytes)
    // Diagnostic for the CilAssembler constant-pool retention leak: the
    // global pool roots every compiled DynamicMethod forever, so DYNAMIC-METHODS
    // tracks the JIT-backed code that never frees. COMMITTED-BYTES is the GC's
    // committed heap (the off-heap JIT code is the gap between this and RSS).
    public static LispObject EmitPoolStats(LispObject[] args)
    {
        if (args.Length != 0)
            throw new LispErrorException(new LispProgramError("EMIT-POOL-STATS: takes no arguments"));
        var (total, dm, data) = Emitter.CilAssembler.ConstantsPoolStats();
#if DOTCL_EMIT
        long committed = GC.GetGCMemoryInfo().TotalCommittedBytes;
#else
        long committed = 0; // GetGCMemoryInfo is unavailable on netstandard2.0
#endif
        return new Cons(Fixnum.Make(total),
               new Cons(Fixnum.Make(dm),
               new Cons(Fixnum.Make(data),
               new Cons(Fixnum.Make(committed), Nil.Instance))));
    }

    public static LispObject Disassemble(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DISASSEMBLE: wrong number of arguments: {args.Length} (expected 1)"));
        var arg = args[0];

        // Resolve function designator to LispFunction
        LispFunction? fn = null;
        string? fnName = null;
        if (arg is LispFunction lf)
        {
            fn = lf;
            fnName = lf.Name;
        }
        else if (arg is Symbol sym)
        {
            fn = sym.Function as LispFunction;
            fnName = sym.Name;
            if (fn == null)
                throw new LispErrorException(new LispUndefinedFunction(sym));
        }
        else if (arg is Cons c && c.Car is Symbol cs)
        {
            if (cs.Name == "LAMBDA")
            {
                // Compile and disassemble
                var compiled = CompileTopLevel(arg);
                var result = Emitter.CilAssembler.AssembleAndRun(compiled);
                if (result is LispFunction compiledFn)
                {
                    fn = compiledFn;
                    fnName = "(LAMBDA)";
                }
            }
            else if (cs.Name == "SETF" && c.Cdr is Cons c2 && c2.Car is Symbol setfSym && c2.Cdr is Nil)
            {
                // sym.SetfFunction is authoritative.
                fn = setfSym.SetfFunction as LispFunction;
                fnName = $"(SETF {setfSym.Name})";
            }
        }
        if (fn == null)
            throw new LispErrorException(new LispTypeError(
                "DISASSEMBLE: argument is not a valid function designator", arg,
                Startup.Sym("FUNCTION-DESIGNATOR")));

        var writer = GetStandardOutputWriter();
        if (fn.Sil != null)
        {
            writer.WriteLine($"; disassembly for {fnName ?? "#<FUNCTION>"}");
            DisassembleSil(writer, fn.Sil);
        }
        else
        {
            writer.WriteLine($"; No disassembly for {fnName ?? "#<FUNCTION>"}.");
            writer.WriteLine("; Set (SETF DOTCL:*SAVE-SIL* T) then redefine the function to enable.");
        }
        writer.Flush();
        return Nil.Instance;
    }

    // Hook set by dotcl-jitdisasm contrib at (require "dotcl-jitdisasm") time.
    public static Action<LispFunction, TextWriter>? JitDisassembleHook;

    public static LispObject JitDisassemble(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DOTCL:JIT-DISASSEMBLE: wrong number of arguments: {args.Length} (expected 1)"));
        var arg = args[0];

        LispFunction? fn = null;
        if (arg is LispFunction lf)
            fn = lf;
        else if (arg is Symbol sym)
        {
            fn = sym.Function as LispFunction;
            if (fn == null)
                throw new LispErrorException(new LispUndefinedFunction(sym));
        }
        else if (arg is Cons c && c.Car is Symbol cs && cs.Name == "SETF"
                 && c.Cdr is Cons c2 && c2.Car is Symbol setfSym && c2.Cdr is Nil)
            fn = setfSym.SetfFunction as LispFunction;

        if (fn == null)
            throw new LispErrorException(new LispTypeError(
                "DOTCL:JIT-DISASSEMBLE: not a valid function designator", arg,
                Startup.Sym("FUNCTION-DESIGNATOR")));

        if (JitDisassembleHook == null)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:JIT-DISASSEMBLE: not available; load the contrib first: (require \"dotcl-jitdisasm\")"));

        var writer = GetStandardOutputWriter();
        JitDisassembleHook(fn, writer);
        writer.Flush();
        return Nil.Instance;
    }

    /// <summary>
    /// DOTCL:BACKTRACE — return the current Lisp call stack as a list of
    /// function-name strings, innermost (most recent) frame first. Only named
    /// functions are tracked (see LispFunction call-stack push/pop); anonymous
    /// lambdas and native-fixnum self-calls do not appear. This function itself
    /// is registered without a Name, so it never shows up in its own result.
    /// </summary>
    public static LispObject Backtrace(LispObject[] args)
    {
        var frames = LispFunction.GetCallStack();
        LispObject result = Nil.Instance;
        for (int i = frames.Length - 1; i >= 0; i--)
            result = new Cons(new LispString(frames[i]), result);
        return result;
    }

    /// <summary>
    /// DOTCL:BACKTRACE-WITH-ARGS — like DOTCL:BACKTRACE, but each frame is a list
    /// (NAME arg0 arg1 ...) whose args are the actual captured argument objects,
    /// innermost (most recent) frame first. Lets a debugger / error handler inspect
    /// the arguments a frame was called with, not just function names (cf.
    /// sb-debug:list-backtrace). Same tracking caveats as DOTCL:BACKTRACE (only
    /// named functions; anonymous lambdas and native-fixnum self-calls are absent).
    /// </summary>
    public static LispObject BacktraceWithArgs(LispObject[] args)
    {
        var frames = LispFunction.GetCallStackWithArgs();
        LispObject result = Nil.Instance;
        for (int i = frames.Length - 1; i >= 0; i--)
            result = new Cons(frames[i], result);
        return result;
    }

    /// <summary>
    /// DOTCL:PRINT-BACKTRACE (&amp;optional stream) — print the current call stack
    /// as numbered frames. Defaults to *ERROR-OUTPUT* (cf. sb-debug:print-backtrace).
    /// </summary>
    public static LispObject PrintBacktrace(LispObject[] args)
    {
        LispObject streamArg = args.Length >= 1 ? args[0] : Nil.Instance;
        if (streamArg is Nil)
            streamArg = DynamicBindings.Get(Startup.Sym("*ERROR-OUTPUT*"));
        var writer = GetTextWriter(streamArg);
        var frames = LispFunction.GetCallStackForms();
        if (frames.Length == 0)
            writer.Write("; (no Lisp frames)\n");
        else
            for (int i = 0; i < frames.Length; i++)
                writer.Write($"{i,3}: {frames[i]}\n");
        writer.Flush();
        return Nil.Instance;
    }

    private static void DisassembleSil(TextWriter writer, LispObject sil)
    {
        // SIL is a flat list of instructions: ((:INSTR args...) (:INSTR args...) ...)
        var cur = sil;
        int indent = 2;
        while (cur is Cons c)
        {
            var instr = c.Car;
            if (instr is Cons ic)
            {
                var opSym = ic.Car;
                var opName = opSym is Symbol s ? s.Name : opSym?.ToString() ?? "?";

                // Labels get no indent
                if (opName == "LABEL")
                {
                    var labelName = ic.Cdr is Cons lc ? FormatSilArg(lc.Car) : "?";
                    writer.WriteLine($"{labelName}:");
                }
                else
                {
                    // Format: "  (:OP arg1 arg2 ...)"
                    var sb = new System.Text.StringBuilder();
                    sb.Append(' ', indent);
                    sb.Append(opName.ToLower());
                    var argCur = ic.Cdr;
                    while (argCur is Cons ac)
                    {
                        sb.Append(' ');
                        sb.Append(FormatSilArg(ac.Car));
                        argCur = ac.Cdr;
                    }
                    writer.WriteLine(sb.ToString());
                }
            }
            cur = c.Cdr;
        }
    }

    private static string FormatSilArg(LispObject arg)
    {
        if (arg is LispString ls) return $"\"{ls.Value}\"";
        if (arg is Symbol sym)
        {
            // Strip compiler package prefix for readability
            var name = sym.Name;
            if (sym.HomePackage?.Name == "DOTCL.CIL-COMPILER")
            {
                // Strip gensym suffixes like _1234 for readability? No, keep them.
                return name;
            }
            return sym.ToString();
        }
        if (arg is Cons c)
        {
            // Sub-list (e.g., switch targets)
            var sb = new System.Text.StringBuilder("(");
            var cur = (LispObject)c;
            bool first = true;
            while (cur is Cons cc)
            {
                if (!first) sb.Append(' ');
                sb.Append(FormatSilArg(cc.Car));
                cur = cc.Cdr;
                first = false;
            }
            sb.Append(')');
            return sb.ToString();
        }
        return arg?.ToString() ?? "NIL";
    }

    // Trace infrastructure: maps function name key → original (unwrapped) function
    private static readonly ConcurrentDictionary<string, LispFunction> _tracedOriginals = new();
    private static readonly ConcurrentDictionary<string, LispObject> _tracedNames = new();

    private static string TraceNameKey(LispObject name)
    {
        if (name is Symbol sym) return sym.Name;
        if (name is Cons c && c.Car is Symbol s && s.Name == "SETF" && c.Cdr is Cons c2 && c2.Car is Symbol s2)
            return "(SETF " + s2.Name + ")";
        return name.ToString()!;
    }

    public static LispObject Trace(LispObject[] args)
    {
        if (args.Length == 0)
        {
            // Return list of currently traced function names
            LispObject result = Nil.Instance;
            foreach (var kvp in _tracedNames)
                result = new Cons(kvp.Value, result);
            return result;
        }
        foreach (var name in args)
        {
            var key = TraceNameKey(name);
            if (_tracedOriginals.ContainsKey(key))
                continue; // already traced
            // Find the current function
            LispFunction? original = null;
            if (name is Symbol sym && sym.Function is LispFunction sf)
                original = sf;
            else
            {
                try { original = (LispFunction)Fdefinition(name); }
                catch { }
            }
            if (original == null) continue;
            _tracedOriginals[key] = original;
            _tracedNames[key] = name;
            // Create wrapper that prints to *trace-output*
            var capturedOriginal = original;
            var capturedName = name;
            var wrapper = new LispFunction(wrapArgs =>
            {
                var traceWriter = GetTraceOutputWriter();
                // Print entry
                traceWriter.Write("  Calling ");
                traceWriter.Write(FormatObject(capturedName, true));
                traceWriter.Write(" with (");
                for (int i = 0; i < wrapArgs.Length; i++)
                {
                    if (i > 0) traceWriter.Write(" ");
                    traceWriter.Write(FormatObject(wrapArgs[i], true));
                }
                traceWriter.WriteLine(")");
                traceWriter.Flush();
                // Call original
                var result = capturedOriginal.Invoke(wrapArgs);
                // Print exit
                traceWriter.Write("  => ");
                traceWriter.WriteLine(FormatObject(result, true));
                traceWriter.Flush();
                return result;
            }, original.Name, original.Arity);
            // Replace the function
            if (name is Symbol symN)
            {
                symN.Function = wrapper;
                Emitter.CilAssembler.RegisterFunction(key, wrapper);
            }
            else if (name is Cons setfC && setfC.Car is Symbol setfS && setfS.Name == "SETF"
                     && setfC.Cdr is Cons setfC2 && setfC2.Car is Symbol setfTargetSym)
            {
                // (setf SYM): install on the ACTUAL target symbol's SetfFunction, not
                // Startup.Sym(name-string)'s. Compiled call sites resolve (setf sym)
                // via GetSetfFunctionBySymbol on the source-code symbol (which preserves
                // package); installing on a same-named default-package symbol
                // would leave those calls hitting the untraced original (TRACE.8).
                setfTargetSym.SetfFunction = wrapper;
            }
            else
            {
                Emitter.CilAssembler.RegisterFunction(key, wrapper);
            }
        }
        // Return list of traced names
        return Trace(Array.Empty<LispObject>());
    }

    public static LispObject Untrace(LispObject[] args)
    {
        if (args.Length == 0)
        {
            // Untrace all
            foreach (var kvp in _tracedOriginals.ToArray())
            {
                _tracedNames.TryGetValue(kvp.Key, out var name);
                RestoreTracedFunction(kvp.Key, kvp.Value, name);
            }
            _tracedOriginals.Clear();
            _tracedNames.Clear();
            return Nil.Instance;
        }
        foreach (var name in args)
        {
            var key = TraceNameKey(name);
            if (_tracedOriginals.TryGetValue(key, out var original))
            {
                _tracedNames.TryGetValue(key, out var tracedName);
                RestoreTracedFunction(key, original, tracedName);
                _tracedOriginals.TryRemove(key, out _);
                _tracedNames.TryRemove(key, out _);
            }
        }
        return Trace(Array.Empty<LispObject>());
    }

    private static void RestoreTracedFunction(string key, LispFunction original, LispObject? tracedName)
    {
        // (setf SYM): restore on the ACTUAL target symbol's SetfFunction (mirror of
        // the package-correct install in Trace), not Startup.Sym(name-string)'s.
        if (tracedName is Cons setfC && setfC.Car is Symbol setfS && setfS.Name == "SETF"
            && setfC.Cdr is Cons setfC2 && setfC2.Car is Symbol setfTargetSym)
        {
            setfTargetSym.SetfFunction = original;
            return;
        }
        Emitter.CilAssembler.RegisterFunction(key, original);
        // Restore on the original symbol that was traced
        if (tracedName is Symbol origSym)
            origSym.Function = original;
        // Also restore on CL/CL-USER symbols
        if (!key.StartsWith("("))
        {
            var cl = Package.FindPackage("CL");
            if (cl != null)
            {
                var (sym, status) = cl.FindSymbol(key);
                if (status != SymbolStatus.None) sym.Function = original;
            }
            var clUser = Package.FindPackage("CL-USER");
            if (clUser != null)
            {
                var (sym, status) = clUser.FindSymbol(key);
                if (status != SymbolStatus.None) sym.Function = original;
            }
        }
    }

    private static LispObject TracedKeyToName(string key)
    {
        if (key.StartsWith("(SETF "))
        {
            var inner = key.Substring(6, key.Length - 7);
            return new Cons(Startup.Sym("SETF"), new Cons(Startup.Sym(inner), Nil.Instance));
        }
        return Startup.Sym(key);
    }

    private static TextWriter GetTraceOutputWriter()
    {
        var traceOutSym = Startup.Sym("*TRACE-OUTPUT*");
        if (DynamicBindings.TryGet(traceOutSym, out var val) && val is LispObject streamObj)
            return GetTextWriter(streamObj);
        if (traceOutSym.Value is LispObject globalStream)
            return GetTextWriter(globalStream);
        return Console.Error;
    }

    public static LispObject Ed(LispObject[] args)
    {
        if (args.Length > 1)
            throw new LispErrorException(new LispProgramError("ED: wrong number of arguments: " + args.Length + " (expected 0-1)"));
        return Nil.Instance;
    }

    public static LispObject Dribble(LispObject[] args)
    {
        if (args.Length > 1)
            throw new LispErrorException(new LispProgramError("DRIBBLE: wrong number of arguments: " + args.Length + " (expected 0-1)"));
        return Nil.Instance;
    }

    public static LispObject Inspect(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError("INSPECT: wrong number of arguments: " + args.Length + " (expected 1)"));
        // Minimal: print the object then return it (implementation-defined)
        var writer = GetStandardOutputWriter();
        writer.WriteLine(FormatObject(args[0], true));
        writer.Flush();
        return args[0];
    }

    // Documentation storage: keyed by (object identity hash + doc-type name)
    private static readonly ConcurrentDictionary<(int, string), LispObject> _documentationStore = new();

    public static LispObject GetDocumentation(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("DOCUMENTATION: requires 2 arguments"));
        string docType = args[1] is Symbol s ? s.Name : FormatObject(args[1], true);
        var key = (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(args[0]), docType);
        return _documentationStore.TryGetValue(key, out var doc) ? doc : Nil.Instance;
    }

    public static LispObject SetDocumentation(LispObject[] args)
    {
        // args: new-value, object, doc-type
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError("(SETF DOCUMENTATION): requires 3 arguments"));
        var newValue = args[0];
        string docType = args[2] is Symbol s ? s.Name : FormatObject(args[2], true);
        var key = (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(args[1]), docType);
        if (newValue is Nil)
            _documentationStore.TryRemove(key, out _);
        else
            _documentationStore[key] = newValue;
        return newValue;
    }

    // --- Finalizers -------------------------------------------------
    //
    // CL/trivial-garbage finalizers: register a thunk to run "at some point after"
    // an object is GC'd. Running arbitrary Lisp from the .NET finalizer thread is
    // unsafe (Eval serializes on _evalLock and reads per-thread DynamicBindings),
    // so the .NET finalizer only ENQUEUES the thunk; a normal thread drains the
    // queue via RunFinalizers (called by dotcl:gc and dotcl:run-finalizers). CL
    // does not specify when finalizers run, so deferred draining is conforming.

    // Holds the finalizer thunk(s) for one object. When the object becomes
    // unreachable this object does too (it is only reachable via the
    // ConditionalWeakTable keyed on the object), so its .NET finalizer fires and
    // enqueues the pending thunks. Cancellation clears the list so a fired
    // finalizer enqueues nothing.
    private sealed class Finalizable
    {
        public readonly List<LispObject> Thunks = new();
        ~Finalizable()
        {
            lock (Thunks)
            {
                foreach (var fn in Thunks)
                    _finalizerQueue.Enqueue(fn);
            }
        }
    }

    // object -> its Finalizable holder. CWT keeps the holder alive exactly as
    // long as the key object is alive, and does NOT strong-root the key.
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispObject, Finalizable>
        _finalizers = new();
    private static readonly System.Collections.Concurrent.ConcurrentQueue<LispObject>
        _finalizerQueue = new();

    /// <summary>(finalize object function) — run FUNCTION (a thunk) some time
    /// after OBJECT is garbage-collected. Returns OBJECT.</summary>
    public static LispObject Finalize(LispObject obj, LispObject function)
    {
        var holder = _finalizers.GetOrCreateValue(obj);
        lock (holder.Thunks) holder.Thunks.Add(function);
        return obj;
    }

    /// <summary>(cancel-finalization object) — remove OBJECT's finalizers.
    /// Returns T if any were registered.</summary>
    public static LispObject CancelFinalization(LispObject obj)
    {
        if (_finalizers.TryGetValue(obj, out var holder))
        {
            lock (holder.Thunks) holder.Thunks.Clear();
            _finalizers.Remove(obj);
            return T.Instance;
        }
        return Nil.Instance;
    }

    /// <summary>Run all queued finalizer thunks on the current (normal) thread.
    /// Returns the number run. Safe to call from Lisp; each thunk runs under the
    /// usual Eval serialization. A thunk that errors is swallowed (a finalizer
    /// must not abort the others, mirroring SBCL).</summary>
    public static int RunFinalizers()
    {
        int n = 0;
        while (_finalizerQueue.TryDequeue(out var fn))
        {
            try { Funcall(fn); } catch { /* finalizer errors are ignored */ }
            n++;
        }
        return n;
    }

    internal static void RegisterMiscBuiltins()
    {
        // ERROR (format version), SIGNAL, WARN
        Emitter.CilAssembler.RegisterFunction("ERROR",
            new LispFunction(args => Runtime.LispErrorFormat(args)));
        Emitter.CilAssembler.RegisterFunction("SIGNAL",
            new LispFunction(args => Runtime.LispSignalFormat(args)));
        Emitter.CilAssembler.RegisterFunction("WARN",
            new LispFunction(Runtime.LispWarnFormat, "WARN", -1));

        // PROVIDE, REQUIRE
        Emitter.CilAssembler.RegisterFunction("PROVIDE",
            new LispFunction(args => Runtime.Provide(args[0])));
        Emitter.CilAssembler.RegisterFunction("REQUIRE",
            new LispFunction(args => Runtime.Require(args)));

        // EVAL, FORMAT, FORMATTER
        Emitter.CilAssembler.RegisterFunction("EVAL",
            new LispFunction(args => Runtime.Eval(args[0])));
        Emitter.CilAssembler.RegisterFunction("FORMAT",
            new LispFunction(args => {
                var dest = args[0];
                var rest = new LispObject[args.Length - 1];
                Array.Copy(args, 1, rest, 0, rest.Length);
                return Runtime.Format(dest, rest);
            }));
        Emitter.CilAssembler.RegisterFunction("FORMATTER",
            new LispFunction(args => {
                Runtime.CheckArityExact("FORMATTER", args, 1);
                var controlString = args[0] switch
                {
                    LispString s => s.Value,
                    _ => throw new LispErrorException(new LispTypeError("FORMATTER: control-string must be a string", args[0]))
                };
                return new LispFunction(fargs => {
                    if (fargs.Length < 1)
                        throw new LispErrorException(new LispError("FORMATTER function: missing stream argument"));
                    var stream = fargs[0];
                    var formatArgs = new LispObject[fargs.Length - 1];
                    Array.Copy(fargs, 1, formatArgs, 0, formatArgs.Length);
                    int consumed = Runtime.FormatToStreamReturningArgCount(controlString, formatArgs, stream);
                    LispObject tail = Nil.Instance;
                    for (int j = formatArgs.Length - 1; j >= consumed; j--)
                        tail = new Cons(formatArgs[j], tail);
                    return tail;
                });
            }));

        // VALUES (variadic)
        Emitter.CilAssembler.RegisterFunction("VALUES",
            new LispFunction(args => Runtime.Values(args), "VALUES", -1));
        Startup.RegisterUnary("VALUES-LIST", Runtime.ValuesList);

        // GENSYM, GENTEMP
        Emitter.CilAssembler.RegisterFunction("GENSYM",
            new LispFunction(args => args.Length == 0 ? Runtime.Gensym0() : Runtime.Gensym(args[0])));
        Emitter.CilAssembler.RegisterFunction("GENTEMP",
            new LispFunction(Runtime.Gentemp));

        // FUNCALL
        Emitter.CilAssembler.RegisterFunction("FUNCALL",
            new LispFunction(args => {
                if (args[0] is LispFunction f) return f.Invoke(args.SubArray(1));
                if (args[0] is Symbol s) return ((LispFunction)Runtime.Fdefinition(s)).Invoke(args.SubArray(1));
                throw new LispErrorException(new LispTypeError("FUNCALL: not a function designator", args[0]));
            }));

        // SYMBOL-FUNCTION, FDEFINITION, %SET-FDEFINITION, FMAKUNBOUND, MAKUNBOUND
        // SYMBOL-FUNCTION only accepts symbols (unlike FDEFINITION which also takes (setf sym))
        Startup.RegisterUnary("SYMBOL-FUNCTION", Runtime.SymbolFunction);
        Startup.RegisterUnary("FDEFINITION", Runtime.Fdefinition);
        Emitter.CilAssembler.RegisterFunction("%SET-FDEFINITION",
            new LispFunction(args => {
                if (args[1] is not LispFunction fn)
                    throw new LispErrorException(new LispTypeError("%SET-FDEFINITION: not a function", args[1]));
                if (args[0] is Cons)
                {
                    // For (setf foo) style names: nameSym.SetfFunction is the authoritative
                    // storage. Do NOT call CilAssembler.RegisterFunction with the bare
                    // "(SETF FOO)" key — that would set Startup.Sym("FOO").SetfFunction
                    // (i.e., cl:documentation) instead of the actual nameSym's SetfFunction,
                    // causing package-crossing corruption (e.g. acclimation:documentation's
                    // 4-arg setf GF overwriting cl:documentation.SetfFunction).
                    if (args[0] is Cons nc && nc.Cdr is Cons nc2 && nc2.Car is Symbol nameSym)
                    {
                        Runtime.CheckPackageLock(nameSym, "%SET-FDEFINITION");
                        nameSym.SetfFunction = fn;
                        // For uninterned gensyms (no home package) ONLY: ALSO register on the
                        // CL-package canonical symbol so string-based GetFunction("(SETF name)")
                        // can find it (FDEFINITION.5). For interned symbols in other
                        // packages (e.g. acclimation:documentation), DO NOT pollute the CL
                        // canonical symbol — that would overwrite cl:documentation.SetfFunction
                        // and break unrelated callers (cross-package pollution regression).
                        if (nameSym.HomePackage == null)
                        {
                            var canonSym = Startup.Sym(nameSym.Name);
                            if (canonSym != nameSym) canonSym.SetfFunction = fn;
                        }
                    }
                    else
                    {
                        // Unusual (setf ...) form without a symbol accessor: fall back to key-based
                        var key = Runtime.GetFunctionNameKey(args[0], "%SET-FDEFINITION");
                        Emitter.CilAssembler.RegisterFunction(key, fn);
                    }
                }
                else
                {
                    var sym = Runtime.GetSymbol(args[0], "%SET-FDEFINITION");
                    // Use RegisterFunctionOnSymbol which is package-aware:
                    // it updates sym.Function on the correct symbol (e.g. a shadowed
                    // symbol in a user package) without corrupting the CL flat-name table.
                    Emitter.CilAssembler.RegisterFunctionOnSymbol(sym, fn);
                }
                return args[1];
            }, "%SET-FDEFINITION", 2));
        Startup.RegisterUnary("FMAKUNBOUND", Runtime.Fmakunbound);
        Startup.RegisterUnary("MAKUNBOUND", Runtime.Makunbound);

        // PROCLAIM
        Emitter.CilAssembler.RegisterFunction("PROCLAIM",
            new LispFunction(args => {
                if (args.Length != 1) throw new LispErrorException(new LispProgramError($"PROCLAIM: wrong number of arguments: {args.Length} (expected 1)"));
                var decl = args[0];
                if (decl is not Cons declCons)
                    return Nil.Instance;
                var declKind = declCons.Car as Symbol;
                if (declKind == null)
                    return Nil.Instance;
                {
                    var tail = declCons.Cdr;
                    while (tail is Cons c) tail = c.Cdr;
                    if (tail is not Nil)
                        throw new LispErrorException(new LispTypeError(
                            $"PROCLAIM: malformed declaration (not a proper list)",
                            decl));
                }
                if (declKind.Name == "SPECIAL")
                {
                    var rest = declCons.Cdr;
                    while (rest is Cons c)
                    {
                        if (c.Car is Symbol sym)
                            sym.IsSpecial = true;
                        rest = c.Cdr;
                    }
                }
                return Nil.Instance;
            }));

        // MACRO-FUNCTION
        Emitter.CilAssembler.RegisterFunction("MACRO-FUNCTION", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"MACRO-FUNCTION: wrong number of arguments: {args.Length}"));
            var result = Runtime.MacroFunction(args[0]);
            if (result != Nil.Instance) return result;
            var sym = Runtime.GetSymbol(args[0], "MACRO-FUNCTION");
            var compilerFn = Startup.LookupCompilerMacro(sym);
            if (compilerFn != null)
            {
                return new LispFunction(wrapArgs => {
                    return compilerFn.Invoke(new LispObject[] { wrapArgs[0] });
                }, $"MACRO-EXPANDER-{sym.Name}", 2);
            }
            return Nil.Instance;
        }, "MACRO-FUNCTION", -1));

        // Standard macro function registration
        var standardMacros = new[] {
            "AND", "OR", "WHEN", "UNLESS", "COND", "CASE", "ECASE", "TYPECASE", "ETYPECASE",
            "DEFUN", "DEFVAR", "DEFPARAMETER", "DEFCONSTANT", "DEFMACRO", "DEFSTRUCT",
            "DEFCLASS", "DEFGENERIC", "DEFMETHOD", "DEFTYPE", "DEFINE-CONDITION",
            "SETF", "PUSH", "POP", "INCF", "DECF", "PUSHNEW", "REMF",
            "DOTIMES", "DOLIST", "DO", "DO*", "LOOP", "LOOP-FINISH",
            "WITH-OPEN-FILE", "WITH-OPEN-STREAM", "WITH-OUTPUT-TO-STRING",
            "WITH-INPUT-FROM-STRING", "WITH-ACCESSORS", "WITH-SLOTS",
            "MULTIPLE-VALUE-BIND", "MULTIPLE-VALUE-LIST", "MULTIPLE-VALUE-SETQ",
            "DESTRUCTURING-BIND", "HANDLER-CASE", "HANDLER-BIND", "IGNORE-ERRORS",
            "IN-PACKAGE", "DEFPACKAGE", "LAMBDA", "PPRINT-LOGICAL-BLOCK",
            "PRINT-UNREADABLE-OBJECT", "CHECK-TYPE", "ASSERT",
            "PROG1", "PROG2", "PROG", "PROG*", "DEFINE-SETF-EXPANDER", "DEFSETF",
            "NTH-VALUE", "TRACE", "UNTRACE", "TIME", "STEP",
            "PSETQ", "PSETF", "SHIFTF", "ROTATEF", "RETURN",
            "CCASE", "CTYPECASE", "ETYPECASE",
            "DEFINE-COMPILER-MACRO", "DEFINE-SYMBOL-MACRO", "FORMATTER",
            "DEFINE-MODIFY-MACRO", "DEFINE-METHOD-COMBINATION",
            "WITH-STANDARD-IO-SYNTAX", "WITH-COMPILATION-UNIT",
            "RESTART-CASE", "RESTART-BIND", "WITH-CONDITION-RESTARTS",
            "WITH-SIMPLE-RESTART", "WITH-HASH-TABLE-ITERATOR", "WITH-PACKAGE-ITERATOR",
            "DECLAIM", "DO-SYMBOLS", "DO-EXTERNAL-SYMBOLS", "DO-ALL-SYMBOLS",
            "CASE", "TYPECASE", "ECASE", "WHEN", "UNLESS",
            "AND", "OR", "COND"
        };
        foreach (var m in standardMacros)
        {
            var macroSym = Startup.Sym(m);
            var macroFn = new LispFunction(args => {
                Runtime.CheckArityExact("MACRO-EXPANDER", args, 2);
                var compilerFn = Startup.LookupCompilerMacro(macroSym);
                if (compilerFn != null)
                    return compilerFn.Invoke(new LispObject[] { args[0] });
                return args[0];
            }, $"MACRO-EXPANDER-{m}", 2);
            Runtime.RegisterMacroFunction(macroSym, macroFn);
        }

        // Proper standard-CL expanders for compile-form macros (WHEN, UNLESS, COND, AND, OR).
        // These are handled internally by the compiler but must expand via macroexpand-1
        // so that code walkers (e.g. iterate) can see their bodies.
        RegisterCompileFormExpanders();

        // MACROEXPAND-1
        Emitter.CilAssembler.RegisterFunction("MACROEXPAND-1",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"MACROEXPAND-1: wrong number of arguments: {args.Length} (expected 1-2)"));
                if (!Compat.TryEnsureSufficientExecutionStack())
                    return MultipleValues.Values(args[0], Nil.Instance); // bail out: return unexpanded
                var form = args[0];
                var env = args.Length > 1 ? args[1] : Nil.Instance;
                // Extract macro table and symbol-macro table from env
                // env can be: LispHashTable (macros only), Cons (macros-ht . symbol-macros-ht), or Nil
                LispHashTable? macroHt = null;
                LispHashTable? symMacroHt = null;
                if (env is LispHashTable ht) macroHt = ht;
                else if (env is Cons envCons)
                {
                    if (envCons.Car is LispHashTable mht) macroHt = mht;
                    if (envCons.Cdr is LispHashTable sht) symMacroHt = sht;
                }
                // Symbol-macro expansion: form is a symbol
                if (form is Symbol symForm && symForm.Name != "NIL" && symForm.Name != "T")
                {
                    if (symMacroHt != null)
                    {
                        var key = new LispString(symForm.Name);
                        if (symMacroHt.TryGet(key, out var expansion))
                            return MultipleValues.Values(expansion, T.Instance);
                    }
                    // Check global symbol macros (DEFINE-SYMBOL-MACRO)
                    if (Runtime.TryGetGlobalSymbolMacro(symForm, out var gsmExpansion))
                        return MultipleValues.Values(gsmExpansion, T.Instance);
                }
                // Macro expansion: form is a cons
                if (form is Cons cons && cons.Car is Symbol sym)
                {
                    if (macroHt != null)
                    {
                        var key = new LispString(sym.Name);
                        if (macroHt.TryGet(key, out var expander) && expander is LispFunction fn)
                        {
                            var expanded = MultipleValues.Primary(fn.Invoke(new LispObject[] { form }));
                            if (ReferenceEquals(expanded, form))
                                return MultipleValues.Values(form, Nil.Instance);
                            return MultipleValues.Values(expanded, T.Instance);
                        }
                    }
                    var runtimeMacroFn = Runtime.MacroFunction(sym);
                    if (runtimeMacroFn is LispFunction rmf)
                    {
                        var expanded = MultipleValues.Primary(rmf.Invoke(new LispObject[] { form, env }));
                        // If the macro returns the same object, it didn't actually expand
                        // (e.g. AND/OR/WHEN/UNLESS registered as "compile-form macros").
                        // Returning T for expanded-p would cause infinite recursion in
                        // estimate-code-size-1 (loop.lisp). Treat as unexpanded.
                        if (ReferenceEquals(expanded, form))
                            return MultipleValues.Values(form, Nil.Instance);
                        return MultipleValues.Values(expanded, T.Instance);
                    }
                    var compilerFn = Startup.LookupCompilerMacro(sym);
                    if (compilerFn != null)
                    {
                        var expanded = MultipleValues.Primary(compilerFn.Invoke(new LispObject[] { form }));
                        if (ReferenceEquals(expanded, form))
                            return MultipleValues.Values(form, Nil.Instance);
                        return MultipleValues.Values(expanded, T.Instance);
                    }
                }
                return MultipleValues.Values(form, Nil.Instance);
            }, "MACROEXPAND-1", -1));

        // %CALL-WITH-HANDLER-CLUSTER — handler-bind primitive for the emit-free
        // tree-walk interpreter (%mini-eval). (alist thunk) -> establishes the
        // cluster, runs thunk under it. Mirrors compile-handler-bind.
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-HANDLER-CLUSTER",
            new LispFunction(Runtime.CallWithHandlerCluster, "%CALL-WITH-HANDLER-CLUSTER", 2));
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-RESTART-CLUSTER",
            new LispFunction(Runtime.CallWithRestartCluster, "%CALL-WITH-RESTART-CLUSTER", 2));
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-RESTART-BIND",
            new LispFunction(Runtime.CallWithRestartBind, "%CALL-WITH-RESTART-BIND", 2));
        Emitter.CilAssembler.RegisterFunction("%PUSH-HANDLER-CLUSTER",
            new LispFunction(a => Runtime.PushHandlerCluster(a[0]), "%PUSH-HANDLER-CLUSTER", 1));
        Emitter.CilAssembler.RegisterFunction("%POP-HANDLER-CLUSTER",
            new LispFunction(a => Runtime.PopHandlerCluster(), "%POP-HANDLER-CLUSTER", 0));
        Emitter.CilAssembler.RegisterFunction("%PUSH-RESTART-CLUSTER",
            new LispFunction(a => Runtime.PushRestartCluster(a[0]), "%PUSH-RESTART-CLUSTER", 1));
        Emitter.CilAssembler.RegisterFunction("%POP-RESTART-CLUSTER",
            new LispFunction(a => Runtime.PopRestartCluster(), "%POP-RESTART-CLUSTER", 0));

        // MACROEXPAND
        Emitter.CilAssembler.RegisterFunction("MACROEXPAND",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"MACROEXPAND: wrong number of arguments: {args.Length} (expected 1-2)"));
                if (!Compat.TryEnsureSufficientExecutionStack())
                    return MultipleValues.Values(args[0], Nil.Instance); // bail out: return unexpanded
                var form = args[0];
                var env = args.Length > 1 ? args[1] : Nil.Instance;
                // Extract macro table and symbol-macro table from env
                LispHashTable? macroHt = null;
                LispHashTable? symMacroHt = null;
                if (env is LispHashTable ht) macroHt = ht;
                else if (env is Cons envCons)
                {
                    if (envCons.Car is LispHashTable mht) macroHt = mht;
                    if (envCons.Cdr is LispHashTable sht) symMacroHt = sht;
                }
                bool anyExpanded = false;
                for (int i = 0; i < 1000; i++)
                {
                    // Symbol-macro expansion
                    if (form is Symbol symForm && symForm.Name != "NIL" && symForm.Name != "T")
                    {
                        if (symMacroHt != null)
                        {
                            var key = new LispString(symForm.Name);
                            if (symMacroHt.TryGet(key, out var expansion))
                            {
                                form = expansion;
                                anyExpanded = true;
                                continue;
                            }
                        }
                        // Check global symbol macros (DEFINE-SYMBOL-MACRO)
                        if (Runtime.TryGetGlobalSymbolMacro(symForm, out var gsmExpansion))
                        {
                            form = gsmExpansion;
                            anyExpanded = true;
                            continue;
                        }
                        break;
                    }
                    // Macro expansion
                    if (form is Cons cons && cons.Car is Symbol sym)
                    {
                        LispFunction? fn = null;
                        bool is2arg = false;
                        if (macroHt != null)
                        {
                            var key = new LispString(sym.Name);
                            if (macroHt.TryGet(key, out var expander) && expander is LispFunction f)
                                fn = f;
                        }
                        if (fn == null)
                        {
                            var rmf = Runtime.MacroFunction(sym);
                            if (rmf is LispFunction rf) { fn = rf; is2arg = true; }
                        }
                        if (fn == null)
                            fn = Startup.LookupCompilerMacro(sym);
                        if (fn != null)
                        {
                            var prevForm = form;
                            form = MultipleValues.Primary(is2arg
                                ? fn.Invoke(new LispObject[] { form, env })
                                : fn.Invoke(new LispObject[] { form }));
                            // Fix-point check: identity macro (e.g. compile-form macros return form unchanged)
                            if (ReferenceEquals(form, prevForm))
                                break;
                            anyExpanded = true;
                            continue;
                        }
                    }
                    break;
                }
                return MultipleValues.Values(form, anyExpanded ? (LispObject)T.Instance : Nil.Instance);
            }, "MACROEXPAND", -1));

        // FBOUNDP. Body extracted to FboundpImpl so the 1-arg direct delegate and
        // the args-array wrapper share the exact same code path; argc != 1 still
        // falls back to the wrapper's wrong-number-of-arguments error.
        var fboundpFn = new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"FBOUNDP: wrong number of arguments: {args.Length} (expected 1)"));
            return FboundpImpl(args[0]);
        }, "FBOUNDP", -1);
        fboundpFn.SetDirectDelegate((Func<LispObject, LispObject>)FboundpImpl);
        Emitter.CilAssembler.RegisterFunction("FBOUNDP", fboundpFn);

        // COMPILE
        Emitter.CilAssembler.RegisterFunction("COMPILE",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2)
                    throw new LispErrorException(new LispProgramError($"COMPILE: wrong number of arguments: {args.Length} (expected 1-2)"));
                // (compile name lambda-expression): compile the lambda, and when NAME is
                // non-nil install the result as NAME's definition, returning NAME (CLHS
                // — "compile replaces the function definition of name"). fiveam's run-time
                // test compilation relies on this side effect.
                if (args.Length > 1 && args[1] is not Nil)
                {
                    var instrList = Runtime.CompileTopLevel(args[1]);
                    var fn = Emitter.CilAssembler.AssembleAndRun(instrList);
                    if (args[0] is Nil)
                        return MultipleValues.Values(fn, Nil.Instance, Nil.Instance);
                    if (fn is LispFunction lfn)
                    {
                        if (args[0] is Symbol nameSym)
                        {
                            Emitter.CilAssembler.RegisterFunctionOnSymbol(nameSym, lfn);
                            return MultipleValues.Values(nameSym, Nil.Instance, Nil.Instance);
                        }
                        // (setf NAME) function-name
                        if (args[0] is Cons fc && fc.Car is Symbol setfSym && setfSym.Name == "SETF"
                            && fc.Cdr is Cons tc && tc.Car is Symbol target)
                        {
                            Emitter.CilAssembler.RegisterSetfFunctionOnSymbol(target, lfn);
                            return MultipleValues.Values(args[0], Nil.Instance, Nil.Instance);
                        }
                    }
                    return MultipleValues.Values(args[0], Nil.Instance, Nil.Instance);
                }
                // (compile name): NAME's existing definition is already compiled in dotcl;
                // just return the designator.
                if (args[0] is Symbol sym)
                {
                    return MultipleValues.Values(sym, Nil.Instance, Nil.Instance);
                }
                return MultipleValues.Values(args[0], Nil.Instance, Nil.Instance);
            }));
        Emitter.CilAssembler.RegisterFunction("COMPILE-FILE",
            new LispFunction(args => Runtime.CompileFile(args)));

        // LOAD
        Emitter.CilAssembler.RegisterFunction("LOAD",
            new LispFunction(args => Runtime.Load(args)));

        // Stubs: BREAK, Y-OR-N-P, YES-OR-NO-P
        Emitter.CilAssembler.RegisterFunction("BREAK",
            new LispFunction(args => Nil.Instance, "BREAK", -1));
        Emitter.CilAssembler.RegisterFunction("Y-OR-N-P",
            new LispFunction(args => T.Instance, "Y-OR-N-P", -1));
        Emitter.CilAssembler.RegisterFunction("YES-OR-NO-P",
            new LispFunction(args => T.Instance, "YES-OR-NO-P", -1));

        // INVALID-METHOD-ERROR, METHOD-COMBINATION-ERROR
        Emitter.CilAssembler.RegisterFunction("INVALID-METHOD-ERROR",
            new LispFunction(args => {
                throw new LispErrorException(new LispError("INVALID-METHOD-ERROR called"));
            }, "INVALID-METHOD-ERROR", -1));
        Emitter.CilAssembler.RegisterFunction("METHOD-COMBINATION-ERROR",
            new LispFunction(args => {
                throw new LispErrorException(new LispError("METHOD-COMBINATION-ERROR called"));
            }, "METHOD-COMBINATION-ERROR", -1));

        // MAKE-LOAD-FORM-SAVING-SLOTS
        Emitter.CilAssembler.RegisterFunction("MAKE-LOAD-FORM-SAVING-SLOTS",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM-SAVING-SLOTS: wrong number of arguments: 0 (expected at least 1)"));
                var obj = args[0];
                bool slotNamesSupplied = false;
                LispObject slotNamesList = Nil.Instance;
                bool allowOtherKeysFound = false;
                bool allowOtherKeys = false;
                for (int i = 1; i < args.Length; i += 2)
                {
                    if (i + 1 > args.Length)
                        throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM-SAVING-SLOTS: odd number of keyword arguments"));
                    if (args[i] is Symbol keySym)
                    {
                        if (keySym.Name == "SLOT-NAMES")
                        {
                            if (i + 1 >= args.Length)
                                throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM-SAVING-SLOTS: missing value for :SLOT-NAMES"));
                            slotNamesSupplied = true;
                            slotNamesList = args[i + 1];
                        }
                        else if (keySym.Name == "ENVIRONMENT")
                        {
                            // ignored
                        }
                        else if (keySym.Name == "ALLOW-OTHER-KEYS")
                        {
                            allowOtherKeysFound = true;
                            if (i + 1 < args.Length)
                                allowOtherKeys = args[i + 1] != Nil.Instance;
                        }
                        else if (!allowOtherKeys)
                        {
                            throw new LispErrorException(new LispProgramError($"MAKE-LOAD-FORM-SAVING-SLOTS: unknown keyword argument {keySym}"));
                        }
                    }
                    else
                    {
                        throw new LispErrorException(new LispProgramError($"MAKE-LOAD-FORM-SAVING-SLOTS: invalid keyword {args[i]}"));
                    }
                }

                string className;
                LispClass? cls = null;
                if (obj is LispInstance inst)
                {
                    cls = inst.Class;
                    className = cls.Name.Name;
                }
                else if (obj is LispInstanceCondition lic)
                {
                    cls = lic.Instance.Class;
                    className = cls.Name.Name;
                }
                else if (obj is LispStruct ls)
                {
                    className = ls.TypeName.Name;
                    cls = Runtime.FindClassOrNil(ls.TypeName) as LispClass;
                }
                else
                {
                    throw new LispErrorException(new LispTypeError("MAKE-LOAD-FORM-SAVING-SLOTS: not a standard-object, structure-object, or condition", obj));
                }

                var findClassSym = Startup.Sym("FIND-CLASS");
                var allocInstSym = Startup.Sym("ALLOCATE-INSTANCE");
                var quoteSym = Startup.Sym("QUOTE");
                // Use the class's own name symbol to preserve package identity
                var classNameSym = cls!.Name;
                var findClassForm = new Cons(findClassSym, new Cons(new Cons(quoteSym, new Cons(classNameSym, Nil.Instance)), Nil.Instance));
                var creationForm = new Cons(allocInstSym, new Cons(findClassForm, Nil.Instance));

                var setfForms = new System.Collections.Generic.List<LispObject>();
                var slotValueSym = Startup.Sym("SLOT-VALUE");
                var setfSym = Startup.Sym("SETF");

                if (obj is LispStruct structObj)
                {
                    if (cls != null && cls.StructSlotNames != null)
                    {
                        for (int i = 0; i < cls.StructSlotNames.Length && i < structObj.Slots.Length; i++)
                        {
                            var slotSym = cls.StructSlotNames[i];
                            if (slotNamesSupplied && !MlfsListContainsSymbol(slotNamesList, slotSym))
                                continue;
                            var slotVal = structObj.Slots[i];
                            var svForm = new Cons(slotValueSym, new Cons(obj, new Cons(new Cons(quoteSym, new Cons(slotSym, Nil.Instance)), Nil.Instance)));
                            var setfForm = new Cons(setfSym, new Cons(svForm, new Cons(new Cons(quoteSym, new Cons(slotVal, Nil.Instance)), Nil.Instance)));
                            setfForms.Add(setfForm);
                        }
                    }
                }
                else
                {
                    var theInst = obj is LispInstanceCondition lic2 ? lic2.Instance : (LispInstance)obj;
                    var theCls = theInst.Class;
                    for (int i = 0; i < theCls.EffectiveSlots.Length; i++)
                    {
                        var slotDef = theCls.EffectiveSlots[i];
                        if (slotNamesSupplied && !MlfsListContainsSymbol(slotNamesList, slotDef.Name))
                            continue;
                        if (theInst.Slots[i] == null) continue;
                        var slotVal = theInst.Slots[i]!;
                        var svForm = new Cons(slotValueSym, new Cons(obj, new Cons(new Cons(quoteSym, new Cons(slotDef.Name, Nil.Instance)), Nil.Instance)));
                        var setfForm = new Cons(setfSym, new Cons(svForm, new Cons(new Cons(quoteSym, new Cons(slotVal, Nil.Instance)), Nil.Instance)));
                        setfForms.Add(setfForm);
                    }
                }

                LispObject initForm;
                if (setfForms.Count == 0)
                {
                    initForm = Nil.Instance;
                }
                else
                {
                    var prognSym = Startup.Sym("PROGN");
                    LispObject body = Nil.Instance;
                    for (int i = setfForms.Count - 1; i >= 0; i--)
                        body = new Cons(setfForms[i], body);
                    initForm = new Cons(prognSym, body);
                }

                return MultipleValues.Values(creationForm, initForm);
            }, "MAKE-LOAD-FORM-SAVING-SLOTS", -1));

        // COMPILER-MACRO-FUNCTION — look up runtime-registered compiler macros.
        // Body extracted to CompilerMacroFunctionImpl so the 1-arg direct delegate
        // and the args-array wrapper share the same code path (the optional env
        // argument was already ignored by the wrapper).
        var cmfFn = new LispFunction(args => {
            if (args.Length < 1) return Nil.Instance;
            return CompilerMacroFunctionImpl(args[0]);
        }, "COMPILER-MACRO-FUNCTION", -1);
        cmfFn.SetDirectDelegate((Func<LispObject, LispObject>)CompilerMacroFunctionImpl);
        Emitter.CilAssembler.RegisterFunction("COMPILER-MACRO-FUNCTION", cmfFn);

        // %REGISTER-COMPILER-MACRO-RT — store a compiler macro function at runtime.
        Startup.RegisterBinary("%REGISTER-COMPILER-MACRO-RT", (nameObj, fn) => {
            // (setf foo) form: a cons (setf . (foo . nil))
            if (nameObj is Cons setfCons && setfCons.Car is Symbol setfSym && setfSym.Name == "SETF") {
                var innerSym = (setfCons.Cdr is Cons c2) ? c2.Car as Symbol : null;
                if (innerSym != null) {
                    if (fn is LispFunction lf2) _setfCompilerMacros[innerSym] = lf2;
                    else _setfCompilerMacros.TryRemove(innerSym, out _);   // (setf (compiler-macro-function '(setf foo)) nil)
                }
                return fn;
            }
            var sym = Runtime.GetSymbol(nameObj, "%REGISTER-COMPILER-MACRO-RT");
            if (fn is LispFunction lf)
                _compilerMacroFunctions[sym] = lf;
            else
                _compilerMacroFunctions.TryRemove(sym, out _);   // (setf (compiler-macro-function 'foo) nil) removes it (CLHS)
            return fn;
        });

        // %DOTNET-METHOD-RETURN-TYPE — compile-time return-type resolution for the
        // typed-return inference of the DOTNET:INVOKE compiler macro.
        Emitter.CilAssembler.RegisterFunction("%DOTNET-METHOD-RETURN-TYPE",
            new LispFunction(Runtime.DotNetMethodReturnType, "%DOTNET-METHOD-RETURN-TYPE", -1));

        // %REGISTER-MACRO-FUNCTION-RT, %UNREGISTER-MACRO-FUNCTION-RT
        // First arg is a Symbol (package-aware macro registration)
        Startup.RegisterBinary("%REGISTER-MACRO-FUNCTION-RT", (nameObj, fn) => {
            var sym = Runtime.GetSymbol(nameObj, "%REGISTER-MACRO-FUNCTION-RT");
            var name = sym.Name;
            // Use qualified key for non-CL symbols to avoid name collisions
            var pkg = sym.HomePackage;
            var key = (pkg == null || pkg.Name is "COMMON-LISP" or "CL")
                ? name
                : $"{pkg.Name}:{name}";
            // Protect CL macros from foreign package overwrite.
            // If the symbol is a CL symbol and the macro already has an expander registered,
            // skip to preserve host builtins (e.g., DEFSTRUCT) from SBCL cross-compiler overwrite.
            if (pkg?.Name is "COMMON-LISP" or "CL")
            {
                // Check if the Lisp-side *macros* table already has this key
                foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
                {
                    var pkgObj = Package.FindPackage(pkgName);
                    if (pkgObj != null)
                    {
                        var (macrosSym, status) = pkgObj.FindSymbol("*MACROS*");
                        if (macrosSym != null && status != SymbolStatus.None
                            && macrosSym.Value is LispHashTable macrosTable)
                        {
                            if (macrosTable.Get(sym, Nil.Instance) is not Nil)
                                return fn;  // CL macro already registered — don't overwrite
                            break;
                        }
                    }
                }
            }
            if (fn is LispFunction lf)
            {
                Runtime.RegisterMacroFunction(sym, lf);
                var wrapper = new LispFunction(wrapArgs => {
                    return lf.Invoke(new LispObject[] { wrapArgs[0], Nil.Instance });
                }, $"MACRO-EXPANDER-{name}", 1);
                foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
                {
                    var pkgObj = Package.FindPackage(pkgName);
                    if (pkgObj != null)
                    {
                        var (macrosSym, status) = pkgObj.FindSymbol("*MACROS*");
                        if (macrosSym != null && status != SymbolStatus.None
                            && macrosSym.Value is LispHashTable macrosTable)
                        {
                            macrosTable.Set(sym, wrapper);
                            break;
                        }
                    }
                }
            }
            return fn;
        });
        Startup.RegisterUnary("%UNREGISTER-MACRO-FUNCTION-RT", nameObj => {
            var sym = Runtime.GetSymbol(nameObj, "%UNREGISTER-MACRO-FUNCTION-RT");
            var name = sym.Name;
            var pkg = sym.HomePackage;
            var key = (pkg == null || pkg.Name is "COMMON-LISP" or "CL")
                ? name
                : $"{pkg.Name}:{name}";
            Runtime.UnregisterMacroFunction(sym);
            foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
            {
                var pkgObj = Package.FindPackage(pkgName);
                if (pkgObj != null)
                {
                    var (macrosSym, status) = pkgObj.FindSymbol("*MACROS*");
                    if (macrosSym != null && status != SymbolStatus.None
                        && macrosSym.Value is LispHashTable macrosTable)
                    {
                        macrosTable.Remove(sym);
                        break;
                    }
                }
            }
            return Nil.Instance;
        });

        // %REGISTER-SYMBOL-MACRO-RT: register a global symbol macro at load time
        Startup.RegisterBinary("%REGISTER-SYMBOL-MACRO-RT", (nameObj, expansion) => {
            var sym = Runtime.GetSymbol(nameObj, "%REGISTER-SYMBOL-MACRO-RT");
            Runtime.RegisterGlobalSymbolMacro(sym, expansion);
            // Also register in compiler's *global-symbol-macros* if available
            foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
            {
                var pkgObj = Package.FindPackage(pkgName);
                if (pkgObj != null)
                {
                    var (gsmSym, status) = pkgObj.FindSymbol("*GLOBAL-SYMBOL-MACROS*");
                    if (gsmSym != null && status != SymbolStatus.None
                        && gsmSym.Value is LispHashTable gsmTable)
                    {
                        gsmTable.Set(sym, expansion);
                        break;
                    }
                }
            }
            return sym;
        });

        // Load-time-value slot access (legacy global slots)
        Startup.RegisterUnary("%HAS-LTV-SLOT", id => {
            var slotId = (int)((Fixnum)id).Value;
            return Runtime.HasLtvSlot(slotId) ? T.Instance : Nil.Instance;
        });
        Startup.RegisterUnary("%GET-LTV-SLOT", id => {
            var slotId = (int)((Fixnum)id).Value;
            return Runtime.GetLtvSlot(slotId);
        });
        Startup.RegisterBinary("%SET-LTV-SLOT", (id, value) => {
            var slotId = (int)((Fixnum)id).Value;
            Runtime.SetLtvSlot(slotId, value);
            return value;
        });

        // Load-time-value slot access (per-module namespaced — prevents cross-run collisions)
        Emitter.CilAssembler.RegisterFunction("%HAS-LTV-SLOT-IN", new LispFunction(args => {
            var moduleId = ((LispString)args[0]).Value;
            var slotId = (int)((Fixnum)args[1]).Value;
            return Runtime.HasLtvSlotIn(moduleId, slotId) ? T.Instance : Nil.Instance;
        }, "%HAS-LTV-SLOT-IN"));
        Emitter.CilAssembler.RegisterFunction("%GET-LTV-SLOT-IN", new LispFunction(args => {
            var moduleId = ((LispString)args[0]).Value;
            var slotId = (int)((Fixnum)args[1]).Value;
            return Runtime.GetLtvSlotIn(moduleId, slotId);
        }, "%GET-LTV-SLOT-IN"));
        Emitter.CilAssembler.RegisterFunction("%SET-LTV-SLOT-IN", new LispFunction(args => {
            var moduleId = ((LispString)args[0]).Value;
            var slotId = (int)((Fixnum)args[1]).Value;
            Runtime.SetLtvSlotIn(moduleId, slotId, args[2]);
            return args[2];
        }, "%SET-LTV-SLOT-IN"));

        // Time functions
        Emitter.CilAssembler.RegisterFunction("GET-UNIVERSAL-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-UNIVERSAL-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            var epoch = new System.DateTime(1900, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            var now = System.DateTime.UtcNow;
            long secs = (long)(now - epoch).TotalSeconds;
            return Fixnum.Make(secs);
        }));
        Emitter.CilAssembler.RegisterFunction("GET-INTERNAL-RUN-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-INTERNAL-RUN-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            return Fixnum.Make(System.Diagnostics.Process.GetCurrentProcess().TotalProcessorTime.Ticks);
        }));
        Emitter.CilAssembler.RegisterFunction("GET-INTERNAL-REAL-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-INTERNAL-REAL-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            return Fixnum.Make(Compat.TickCount64());
        }));
        Startup.RegisterUnary("SLEEP", obj => {
            double secs = Arithmetic.ToDouble(Runtime.AsNumber(obj));
            if (secs < 0) throw new LispErrorException(new LispTypeError("SLEEP: negative argument", obj));
            System.Threading.Thread.Sleep((int)(secs * 1000));
            return Nil.Instance;
        });
        Emitter.CilAssembler.RegisterFunction("DECODE-UNIVERSAL-TIME", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2)
                throw new LispErrorException(new LispProgramError("DECODE-UNIVERSAL-TIME: expected 1-2 args"));
            long ut = (long)Arithmetic.ToDouble(Runtime.AsNumber(args[0]));
            var epoch = new System.DateTime(1900, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            var dtUtc = epoch.AddSeconds(ut);
            System.DateTime dt;
            LispObject daylightP;
            LispObject zone;
            if (args.Length == 2) {
                var tzNum = Runtime.AsNumber(args[1]);
                long offsetSeconds = (long)System.Math.Round(Arithmetic.ToDouble(Arithmetic.Multiply(tzNum, new Fixnum(3600))));
                dt = epoch.AddSeconds(ut - offsetSeconds);
                daylightP = Nil.Instance;
                zone = args[1];
            } else {
                dt = dtUtc.ToLocalTime();
                var tzInfo = System.TimeZoneInfo.Local;
                bool isDst = tzInfo.IsDaylightSavingTime(dt);
                daylightP = isDst ? T.Instance : Nil.Instance;
                double utcOffsetHours = tzInfo.GetUtcOffset(dt).TotalHours;
                zone = (-utcOffsetHours == (long)(-utcOffsetHours)) ? (LispObject)Fixnum.Make((long)(-utcOffsetHours)) : new DoubleFloat(-utcOffsetHours);
            }
            int dow = ((int)dt.DayOfWeek + 6) % 7;
            return MultipleValues.Values(
                Fixnum.Make(dt.Second),
                Fixnum.Make(dt.Minute),
                Fixnum.Make(dt.Hour),
                Fixnum.Make(dt.Day),
                Fixnum.Make(dt.Month),
                Fixnum.Make(dt.Year),
                Fixnum.Make(dow),
                daylightP,
                zone
            );
        }, "DECODE-UNIVERSAL-TIME", -1));
        Emitter.CilAssembler.RegisterFunction("GET-DECODED-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-DECODED-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            var now = System.DateTime.Now;
            var tzInfo = System.TimeZoneInfo.Local;
            bool isDst = tzInfo.IsDaylightSavingTime(now);
            double utcOffsetHours = tzInfo.GetUtcOffset(now).TotalHours;
            int dow = ((int)now.DayOfWeek + 6) % 7;
            return MultipleValues.Values(
                Fixnum.Make(now.Second),
                Fixnum.Make(now.Minute),
                Fixnum.Make(now.Hour),
                Fixnum.Make(now.Day),
                Fixnum.Make(now.Month),
                Fixnum.Make(now.Year),
                Fixnum.Make(dow),
                isDst ? T.Instance : Nil.Instance,
                (-utcOffsetHours == (long)(-utcOffsetHours)) ? (LispObject)Fixnum.Make((long)(-utcOffsetHours)) : new DoubleFloat(-utcOffsetHours)
            );
        }, "GET-DECODED-TIME", -1));
        Emitter.CilAssembler.RegisterFunction("ENCODE-UNIVERSAL-TIME", new LispFunction(args => {
            if (args.Length < 6 || args.Length > 7)
                throw new LispErrorException(new LispProgramError("ENCODE-UNIVERSAL-TIME: expected 6-7 args"));
            int second = (int)Arithmetic.ToDouble(Runtime.AsNumber(args[0]));
            int minute = (int)Arithmetic.ToDouble(Runtime.AsNumber(args[1]));
            int hour   = (int)Arithmetic.ToDouble(Runtime.AsNumber(args[2]));
            int date   = (int)Arithmetic.ToDouble(Runtime.AsNumber(args[3]));
            int month  = (int)Arithmetic.ToDouble(Runtime.AsNumber(args[4]));
            int year   = (int)Arithmetic.ToDouble(Runtime.AsNumber(args[5]));
            if (year >= 0 && year <= 99) {
                int curYear = System.DateTime.Now.Year;
                int century = curYear - (curYear % 100);
                year += century;
                if (year > curYear + 50) year -= 100;
            }
            var epoch = new System.DateTime(1900, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            System.DateTime dt;
            if (args.Length == 7) {
                var tzNum = Runtime.AsNumber(args[6]);
                long offsetSeconds = (long)System.Math.Round(Arithmetic.ToDouble(Arithmetic.Multiply(tzNum, new Fixnum(3600))));
                dt = new System.DateTime(year, month, date, hour, minute, second, System.DateTimeKind.Utc);
                dt = dt.AddSeconds(offsetSeconds);
            } else {
                dt = new System.DateTime(year, month, date, hour, minute, second, System.DateTimeKind.Local);
                dt = dt.ToUniversalTime();
            }
            long secs = (long)(dt - epoch).TotalSeconds;
            return Fixnum.Make(secs);
        }, "ENCODE-UNIVERSAL-TIME", -1));

        // Environment functions
        Emitter.CilAssembler.RegisterFunction("LISP-IMPLEMENTATION-TYPE",
            new LispFunction(Runtime.LispImplementationType, "LISP-IMPLEMENTATION-TYPE", 0));
        Emitter.CilAssembler.RegisterFunction("LISP-IMPLEMENTATION-VERSION",
            new LispFunction(Runtime.LispImplementationVersion, "LISP-IMPLEMENTATION-VERSION", 0));
        Emitter.CilAssembler.RegisterFunction("SOFTWARE-TYPE",
            new LispFunction(Runtime.SoftwareType, "SOFTWARE-TYPE", 0));
        Emitter.CilAssembler.RegisterFunction("SOFTWARE-VERSION",
            new LispFunction(Runtime.SoftwareVersion, "SOFTWARE-VERSION", 0));
        Emitter.CilAssembler.RegisterFunction("MACHINE-TYPE",
            new LispFunction(Runtime.MachineType, "MACHINE-TYPE", 0));
        Emitter.CilAssembler.RegisterFunction("MACHINE-VERSION",
            new LispFunction(Runtime.MachineVersion, "MACHINE-VERSION", 0));
        Emitter.CilAssembler.RegisterFunction("MACHINE-INSTANCE",
            new LispFunction(Runtime.MachineInstance, "MACHINE-INSTANCE", 0));
        Emitter.CilAssembler.RegisterFunction("SHORT-SITE-NAME",
            new LispFunction(Runtime.ShortSiteName, "SHORT-SITE-NAME", 0));
        Emitter.CilAssembler.RegisterFunction("LONG-SITE-NAME",
            new LispFunction(Runtime.LongSiteName, "LONG-SITE-NAME", 0));
        Emitter.CilAssembler.RegisterFunction("APROPOS",
            new LispFunction(Runtime.Apropos, "APROPOS", -1));
        Emitter.CilAssembler.RegisterFunction("APROPOS-LIST",
            new LispFunction(Runtime.AproposList, "APROPOS-LIST", -1));
        Emitter.CilAssembler.RegisterFunction("DESCRIBE",
            new LispFunction(Runtime.Describe, "DESCRIBE", -1));
        Emitter.CilAssembler.RegisterFunction("ROOM",
            new LispFunction(Runtime.Room, "ROOM", -1));
        Emitter.CilAssembler.RegisterFunction("DISASSEMBLE",
            new LispFunction(Runtime.Disassemble, "DISASSEMBLE", -1));
        Emitter.CilAssembler.RegisterFunction("%TRACE",
            new LispFunction(Runtime.Trace, "%TRACE", -1));
        Emitter.CilAssembler.RegisterFunction("%UNTRACE",
            new LispFunction(Runtime.Untrace, "%UNTRACE", -1));
        Emitter.CilAssembler.RegisterFunction("ED",
            new LispFunction(Runtime.Ed, "ED", -1));
        Emitter.CilAssembler.RegisterFunction("DRIBBLE",
            new LispFunction(Runtime.Dribble, "DRIBBLE", -1));
        Emitter.CilAssembler.RegisterFunction("INSPECT",
            new LispFunction(Runtime.Inspect, "INSPECT", -1));

        // %REGISTER-TYPE-EXPANDER
        Emitter.CilAssembler.RegisterFunction("%REGISTER-TYPE-EXPANDER",
            new LispFunction(args => {
                // Package-aware key: a non-CL deftype (e.g. SBCL's host-side
                // SB-XC:COMPLEX -> COMPLEXNUM) registers under "PKG::NAME" so it
                // shadows a same-named built-in for its own symbol only, and
                // never hijacks CL:COMPLEX for everyone.
                if (args[0] is Symbol s)
                {
                    Runtime.TypeExpanders[Runtime.TypeExpanderKey(s)] = args[1];
                    // Plain-name alias for cross-package references — but never
                    // for a built-in type name (that would hijack e.g. CL:COMPLEX
                    // for every package; such deftypes work via their own symbol).
                    if (!Runtime.IsBuiltinTypeName(s.Name))
                        Runtime.TypeExpanders[s.Name] = args[1];
                    TypeParser.InvalidateCache(s.Name);
                }
                else
                    Runtime.TypeExpanders[args[0].ToString()!] = args[1];
                return args[0];
            }));

        // Stack space check: returns T if sufficient execution stack remains, NIL otherwise.
        // Used by find-free-vars-expr to bail out before .NET StackOverflowException.
        // The args-array wrapper ignores its arguments entirely, so the 0-arg
        // direct delegate is the identical code path (extra-arg calls still fall
        // back to the wrapper, which also ignores them — unchanged).
        var stackSpaceFn = new LispFunction(_ =>
            Compat.TryEnsureSufficientExecutionStack()
                ? (LispObject)T.Instance : Nil.Instance,
            "%STACK-SPACE-AVAILABLE-P", 0);
        stackSpaceFn.SetDirectDelegate((Func<LispObject>)(() =>
            Compat.TryEnsureSufficientExecutionStack()
                ? (LispObject)T.Instance : Nil.Instance));
        Emitter.CilAssembler.RegisterFunction("%STACK-SPACE-AVAILABLE-P", stackSpaceFn);
    }

    /// <summary>Helper: check if a Lisp list contains a symbol (by name)</summary>
    private static bool MlfsListContainsSymbol(LispObject list, Symbol sym)
    {
        var cur = list;
        while (cur is Cons c)
        {
            if (c.Car is Symbol s && s.Name == sym.Name)
                return true;
            cur = c.Cdr;
        }
        return false;
    }


}
