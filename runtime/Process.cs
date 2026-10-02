namespace DotCL;

/// <summary>A TextReader over a child process's redirected stdout/stderr that never
/// truncates. Reading a process pipe directly from a tight CL loop (e.g. READ-LINE)
/// races with the child exiting: a transient end-of-stream is mistaken for true EOF
/// and the final buffered output is lost. To avoid that, a background thread drains
/// the underlying pipe to true EOF in chunks (the canonical .NET pattern, as in
/// ReadToEnd) into an in-memory buffer; the Lisp-facing reader consumes from that
/// buffer, blocking until data is available or the drain has completed.</summary>
public sealed class ProcessStreamReader : System.IO.TextReader
{
    private readonly System.Text.StringBuilder _buf = new();
    private int _pos;
    private bool _eof;
    private readonly object _lock = new();

    private int _pending;

    public ProcessStreamReader(System.IO.TextReader inner, System.Diagnostics.Process proc)
        : this(proc, inner) { }

    /// <summary>Drain one or more child readers into a single shared buffer. Passing
    /// both stdout and stderr merges them into one stream (uiop's
    /// :error-output :output: send stderr to the same place as stdout). True EOF is
    /// reported only once every producer has drained to end.</summary>
    public ProcessStreamReader(System.Diagnostics.Process proc, params System.IO.TextReader[] inners)
    {
        _pending = inners.Length;
        int i = 0;
        foreach (var inner in inners)
        {
            var src = inner;
            var drain = new System.Threading.Thread(() =>
            {
                try
                {
                    var chunk = new char[4096];
                    int n;
                    while ((n = src.Read(chunk, 0, chunk.Length)) > 0)
                    {
                        lock (_lock) { _buf.Append(chunk, 0, n); System.Threading.Monitor.PulseAll(_lock); }
                    }
                }
                catch { /* pipe closed/broken; treat as EOF */ }
                finally
                {
                    lock (_lock) { if (--_pending <= 0) _eof = true; System.Threading.Monitor.PulseAll(_lock); }
                    try { src.Dispose(); } catch { }
                }
            })
            { IsBackground = true, Name = "dotcl-process-drain-" + (i++) };
            drain.Start();
        }
    }

    public override int Read()
    {
        lock (_lock)
        {
            while (_pos >= _buf.Length && !_eof) System.Threading.Monitor.Wait(_lock);
            return _pos < _buf.Length ? _buf[_pos++] : -1;
        }
    }

    // Non-blocking: may report -1 even when more data is still coming
    // (CL READ-CHAR-NO-HANG / LISTEN depend on Peek not blocking).
    public override int Peek()
    {
        lock (_lock) { return _pos < _buf.Length ? _buf[_pos] : -1; }
    }

    public override int Read(char[] buffer, int index, int count)
    {
        if (count <= 0) return 0;
        lock (_lock)
        {
            while (_pos >= _buf.Length && !_eof) System.Threading.Monitor.Wait(_lock);
            int avail = _buf.Length - _pos;
            if (avail <= 0) return 0;
            int n = System.Math.Min(avail, count);
            _buf.CopyTo(_pos, buffer, index, n);
            _pos += n;
            return n;
        }
    }
}

/// <summary>A live external process launched via dotcl:launch-process.
/// Wraps System.Diagnostics.Process and exposes its redirected stdio as Lisp
/// streams, so UIOP's launch-program/process-info protocol (and thus the full
/// run-program contract via slurp-input-stream) can be implemented on top of it
/// instead of the collect-all dotcl:run-process.
///
/// Each of stdin/stdout/stderr is given a redirection spec:
///   :stream    pipe exposed as a live Lisp stream (PROCESS-INPUT/-OUTPUT/-ERROR)
///   a pathname redirect to/from that file (a background helper thread copies
///              file&lt;-&gt;pipe, since .NET's ProcessStartInfo cannot attach a file
///              handle directly)
///   nil        no input (stdin gets EOF) / output discarded (drained so a chatty
///              child never blocks on a full pipe)
///   t/:inherit inherit the parent's handle (no redirection)
/// Keeping the file/null plumbing here lets the UIOP #+dotcl launch-program
/// branch stay a small clause, like the other implementations'.</summary>
public sealed class LispProcess : LispObject
{
    public System.Diagnostics.Process Process { get; }
    /// <summary>Writable stream to the child's stdin when input is :stream, else NIL.</summary>
    public LispObject InputStream { get; }
    /// <summary>Readable stream from the child's stdout when output is :stream, else NIL.</summary>
    public LispObject OutputStream { get; }
    /// <summary>Readable stream from the child's stderr when error is :stream, else NIL.</summary>
    public LispObject ErrorStream { get; }
    private readonly System.Collections.Generic.List<System.Threading.Thread> _helpers;

    private LispProcess(System.Diagnostics.Process process,
                        LispObject input, LispObject output, LispObject error,
                        System.Collections.Generic.List<System.Threading.Thread> helpers)
    {
        Process = process;
        InputStream = input;
        OutputStream = output;
        ErrorStream = error;
        _helpers = helpers;
    }

    /// <summary>Block until the child exits AND every redirection helper thread has
    /// finished copying, so a file output target is fully written before the caller
    /// reads it back. Returns the exit code.</summary>
    public int Wait()
    {
        Process.WaitForExit();
        foreach (var h in _helpers) { try { h.Join(); } catch { } }
        return Process.ExitCode;
    }

    /// <summary>A ProcessStartInfo that runs PROGRAM with ARGUMENTS, each argument
    /// reaching the child as the same string.
    ///
    /// On Windows a .bat/.cmd file is not an executable: CreateProcess runs it as
    /// cmd.exe /c with the whole command line, and cmd gives its own meaning to
    /// % ^ &amp; | &lt; &gt; ( ) in it. The MSVCRT-style quoting .NET applies to
    /// ArgumentList is not enough there: a&amp;b runs b, %PATH% is expanded. So for a
    /// batch file the cmd.exe command line is built here instead, quoting for cmd
    /// (the same scheme Rust's std::process uses):
    ///   - an argument with anything but ASCII letters, digits and #$*+-./:?@\_ is
    ///     put in double quotes, so the batch file sees it quoted in %1 (%~1 drops
    ///     the quotes). Inside quotes cmd takes ^ &amp; | &lt; &gt; ( ) as text.
    ///   - an embedded " is doubled, and backslashes before it or before the closing
    ///     quote are doubled, so a batch file that forwards %* to an .exe hands it
    ///     the original argument.
    ///   - % is written as %%cd:~,% so that cmd never expands a variable reference.
    ///   - delayed expansion (!), AutoRun commands and command extensions are pinned
    ///     with /v:OFF /d /e:ON.
    /// An argument with CR, LF or NUL cannot be passed this way (cmd ends the
    /// command at a line break), so it is refused with an error.</summary>
    internal static System.Diagnostics.ProcessStartInfo MakeStartInfo(
        string program, System.Collections.Generic.IList<string> arguments)
    {
        if (Compat.IsWindows() && IsBatchFile(program))
            return new System.Diagnostics.ProcessStartInfo(
                System.IO.Path.Combine(System.Environment.SystemDirectory, "cmd.exe"),
                BatchCommandLine(ResolveBatchFile(program), arguments));
        var psi = new System.Diagnostics.ProcessStartInfo(program);
        foreach (var a in arguments) Compat.AddArg(psi, a);
        return psi;
    }

    private static bool IsBatchFile(string program)
    {
        // Windows ignores trailing dots and spaces in a file name: "x.bat. " is x.bat.
        var p = program.TrimEnd('.', ' ');
        return p.EndsWith(".bat", System.StringComparison.OrdinalIgnoreCase)
            || p.EndsWith(".cmd", System.StringComparison.OrdinalIgnoreCase);
    }

    /// <summary>The batch file's full path, found as CreateProcess would find it (the
    /// current directory, then PATH); cmd.exe would search from the child's working
    /// directory instead. A name that is not found is returned unchanged and cmd
    /// reports it.</summary>
    private static string ResolveBatchFile(string program)
    {
        if (program.IndexOfAny(new[] { '/', '\\', ':' }) >= 0)
            return System.IO.Path.GetFullPath(program);
        var here = System.IO.Path.Combine(System.Environment.CurrentDirectory, program);
        if (System.IO.File.Exists(here)) return here;
        var path = System.Environment.GetEnvironmentVariable("PATH") ?? "";
        foreach (var dir in path.Split(System.IO.Path.PathSeparator))
        {
            if (dir.Length == 0) continue;
            try
            {
                var cand = System.IO.Path.Combine(dir.Trim('"'), program);
                if (System.IO.File.Exists(cand)) return cand;
            }
            catch (System.ArgumentException) { }
        }
        return program;
    }

    private static string BatchCommandLine(string script, System.Collections.Generic.IList<string> arguments)
    {
        if (script.IndexOf('"') >= 0 || script.EndsWith("\\"))
            throw new LispErrorException(new LispError(
                $"Cannot run batch file {script}: its name contains a double quote or ends with a backslash"));
        // cmd.exe /c "<line>": cmd removes the first and the last quote of <line>.
        var sb = new System.Text.StringBuilder("/e:ON /v:OFF /d /c \"\"");
        foreach (char c in script) AppendForCmd(sb, c);
        sb.Append('"');
        for (int i = 0; i < arguments.Count; i++)
        {
            var arg = arguments[i];
            if (arg.IndexOfAny(new[] { '\r', '\n', '\0' }) >= 0)
                throw new LispErrorException(new LispError(
                    $"Cannot pass argument {i + 1} to batch file {script}: cmd.exe cannot "
                    + "receive a carriage return, line feed or NUL character in an argument"));
            sb.Append(' ');
            bool quote = arg.Length == 0;
            foreach (char c in arg)
                if (!(c < 128 && (char.IsLetterOrDigit(c) || "#$*+-./:?@\\_".IndexOf(c) >= 0)))
                { quote = true; break; }
            if (quote) sb.Append('"');
            int backslashes = 0;
            foreach (char c in arg)
            {
                if (c == '\\') { backslashes++; sb.Append(c); continue; }
                if (c == '"')
                {
                    sb.Append('\\', backslashes);   // n backslashes become 2n before a quote
                    sb.Append('"');                 // and the quote is doubled
                }
                AppendForCmd(sb, c);
                backslashes = 0;
            }
            if (quote)
            {
                sb.Append('\\', backslashes);
                sb.Append('"');
            }
        }
        sb.Append('"');
        return sb.ToString();
    }

    // A % is written as %%cd:~,% and comes out of cmd's expansion as a single %:
    // the first % is kept and %cd:~,% (an empty substring of CD) expands to nothing,
    // so no text between two percent signs is ever taken as a variable name.
    private static void AppendForCmd(System.Text.StringBuilder sb, char c)
    {
        if (c == '%') sb.Append("%%cd:~,");
        sb.Append(c);
    }

    private static bool IsKw(LispObject o, string name) => o is Symbol s && s == Startup.Keyword(name);
    private static bool IsInherit(LispObject o) => o is T || IsKw(o, "INHERIT");

    /// <summary>A file redirection target: a namestring or a pathname. UIOP's
    /// %normalize-io-specifier turns string specs into pathnames, so both arrive here.
    /// Returns null for non-file specs (:stream, nil, t, a stream).</summary>
    private static string? FilePath(LispObject spec) =>
        spec is LispString s ? s.Value :
        spec is LispPathname p ? p.ToNamestring() : null;

    private static System.Threading.Thread Spawn(string name, System.Action body)
    {
        var t = new System.Threading.Thread(() => { try { body(); } catch { /* pipe/file closed */ } })
        { IsBackground = true, Name = name };
        t.Start();
        return t;
    }

    private static void Copy(System.IO.TextReader r, System.IO.TextWriter w)
    {
        var buf = new char[4096];
        int n;
        while ((n = r.Read(buf, 0, buf.Length)) > 0) w.Write(buf, 0, n);
        w.Flush();
    }

    /// <summary>Spawn PROGRAM with ARGUMENTS, wiring stdin/stdout/stderr per the
    /// given redirection specs. File dispositions are validated up front so errors
    /// surface synchronously (before the child runs), as the other implementations rely on.
    /// With BINARY (:element-type (unsigned-byte 8)) a :stream target is a byte stream
    /// on the pipe itself, for READ-BYTE / WRITE-BYTE / WRITE-SEQUENCE of octets.</summary>
    public static LispProcess Launch(
        string program, System.Collections.Generic.List<string> arguments, string? directory,
        LispObject input, LispObject output, LispObject error,
        LispObject ifInputDoesNotExist, LispObject ifOutputExists, LispObject ifErrorOutputExists,
        System.Collections.Generic.List<string>? environment = null,
        System.Text.Encoding? encoding = null, bool binary = false)
    {
        // uiop normalizes :error-output :output to the :output keyword: send the
        // child's stderr to the same destination as its stdout (like shell 2>&1).
        bool mergeErr = IsKw(error, "OUTPUT");
        bool outInherit = IsInherit(output);

        if (FilePath(input) is string inPath0) EnsureInputExists(inPath0, ifInputDoesNotExist);
        if (FilePath(output) is string outPath0) CheckOutputExists(outPath0, ifOutputExists);
        if (!mergeErr && FilePath(error) is string errPath0) CheckOutputExists(errPath0, ifErrorOutputExists);

        // On Unix a file or null target is given to the child as the descriptor
        // itself (see UnixDirectStartInfo); only :stream targets use pipes. On
        // Windows every redirected target goes through a pipe and a copy thread.
        bool unixDirect = !Compat.IsWindows();
        bool inDirect = unixDirect && IsFdTarget(input);
        bool outDirect = unixDirect && IsFdTarget(output);
        bool errDirect = unixDirect && (mergeErr ? outDirect : IsFdTarget(error));

        var psi = (inDirect || outDirect || errDirect)
            ? UnixDirectStartInfo(program, arguments, directory, environment,
                                  input, output, error, mergeErr, ifOutputExists, ifErrorOutputExists)
            : MakeStartInfo(program, arguments);
        psi.UseShellExecute = false;
        psi.CreateNoWindow = true;
        psi.RedirectStandardInput = !IsInherit(input) && !inDirect;
        psi.RedirectStandardOutput = !outInherit && !outDirect;
        psi.RedirectStandardError = !errDirect && (mergeErr ? !outInherit : !IsInherit(error));
        if (!string.IsNullOrEmpty(directory)) psi.WorkingDirectory = directory;
        // The child's pipes use ENCODING when one was given (:external-format), else
        // the .NET default. The setters are only legal on a redirected stream.
        if (encoding != null)
        {
            if (psi.RedirectStandardOutput) psi.StandardOutputEncoding = encoding;
            if (psi.RedirectStandardError) psi.StandardErrorEncoding = encoding;
        }
        if (environment != null)
        {
            // sb-ext:run-program :environment semantics: the "VAR=value" list
            // REPLACES the child's entire environment (psi.Environment starts as a
            // copy of the parent's, so clear it first). A null list means the key
            // was omitted: inherit the parent environment unchanged.
            psi.Environment.Clear();
            foreach (var kv in environment)
            {
                int eq = kv.IndexOf('=');
                if (eq > 0) psi.Environment[kv.Substring(0, eq)] = kv.Substring(eq + 1);
            }
        }

        var proc = System.Diagnostics.Process.Start(psi)!;
        var helpers = new System.Collections.Generic.List<System.Threading.Thread>();
        LispObject inStream = Nil.Instance, outStream = Nil.Instance, errStream = Nil.Instance;

        // --- stdin ---
        if (IsKw(input, "STREAM") && binary)
            inStream = new LispBinaryStream(proc.StandardInput.BaseStream);
        else if (IsKw(input, "STREAM"))
            // ProcessStartInfo.StandardInputEncoding is missing on netstandard2.0,
            // so the encoding is applied by writing through our own writer.
            inStream = new LispOutputStream(encoding == null ? proc.StandardInput
                : new System.IO.StreamWriter(proc.StandardInput.BaseStream, encoding) { AutoFlush = true });
        else if (inDirect) { }
        else if (FilePath(input) is string inPath)
            helpers.Add(Spawn("dotcl-feed-stdin", () => {
                // Copy the file's bytes as they are, as SBCL does when it hands the
                // file to the child. Going through a TextReader/TextWriter pair would
                // drop a leading BOM and mangle bytes that are not valid in the
                // decoding encoding.
                try
                {
                    using var f = new System.IO.FileStream(inPath, System.IO.FileMode.Open,
                        System.IO.FileAccess.Read, System.IO.FileShare.ReadWrite);
                    var sink = proc.StandardInput.BaseStream;
                    f.CopyTo(sink);
                    sink.Flush();
                }
                finally { try { proc.StandardInput.Close(); } catch { } }
            }));
        else if (!IsInherit(input))
            try { proc.StandardInput.Close(); } catch { }   // nil: child sees EOF

        if (mergeErr)
        {
            // stderr shares stdout's destination. When stdout is inherited, stderr is
            // inherited too (neither redirected), so there is nothing to wire.
            if (!outInherit && !outDirect)
                outStream = WireMergedOutput(output, ifOutputExists, encoding, proc, helpers,
                                             () => proc.StandardOutput, () => proc.StandardError);
        }
        else
        {
            // --- stdout ---
            if (!outDirect)
                outStream = binary && IsKw(output, "STREAM")
                    ? new LispBinaryStream(proc.StandardOutput.BaseStream)
                    : WireOutput(output, ifOutputExists, encoding, "dotcl-drain-stdout",
                                 () => proc.StandardOutput, proc, helpers);
            // --- stderr ---
            if (!errDirect)
                errStream = binary && IsKw(error, "STREAM")
                    ? new LispBinaryStream(proc.StandardError.BaseStream)
                    : WireOutput(error, ifErrorOutputExists, encoding, "dotcl-drain-stderr",
                                 () => proc.StandardError, proc, helpers);
        }

        return new LispProcess(proc, inStream, outStream, errStream, helpers);
    }

    /// <summary>A file or null (discard / no input) target, which on Unix the
    /// child gets as the descriptor itself.</summary>
    private static bool IsFdTarget(LispObject spec) => spec is Nil || FilePath(spec) != null;

    /// <summary>Unix: a start info that runs PROGRAM through
    /// <c>/bin/sh -c 'exec REDIRECTIONS; shift N; exec "$@"'</c>, so each file or
    /// null target is opened by the shell and the program inherits that descriptor,
    /// as with SBCL. With a pipe and a copy thread instead, waiting for the child
    /// meant waiting for EOF on the pipe, which a backgrounded grandchild that
    /// inherited it (<c>cmd &amp;</c>) could hold open indefinitely.
    ///
    /// What the caller sees is kept as it was with a direct start:
    /// - the arguments reach the program unchanged: file names and the arguments
    ///   are positional parameters, never parsed by the shell;
    /// - the second exec replaces the shell, so the pid, the exit code and signals
    ///   are the program's own;
    /// - a program that cannot be started is reported here, before anything runs,
    ///   with the error Process.Start gives, from the same resolution .NET uses;
    /// - output files are created (or truncated / opened for append) here first,
    ///   so a file that cannot be opened signals as before.
    /// The program is named to the shell as it was given (so argv[0] is kept)
    /// whenever the shell's PATH search finds the same file, else by full path.</summary>
    private static System.Diagnostics.ProcessStartInfo UnixDirectStartInfo(
        string program, System.Collections.Generic.IList<string> arguments,
        string? directory, System.Collections.Generic.List<string>? environment,
        LispObject input, LispObject output, LispObject error, bool mergeErr,
        LispObject ifOutputExists, LispObject ifErrorOutputExists)
    {
        var execName = ResolveForExec(program, directory, environment);
        var files = new System.Collections.Generic.List<string>();
        var redirs = new System.Text.StringBuilder();
        string Pos(string path) { files.Add(path); return "\"$" + files.Count + "\""; }
        void Out(string fd, LispObject spec, LispObject ifExists)
        {
            if (FilePath(spec) is string path)
            {
                bool append = IsKw(ifExists, "APPEND");
                new System.IO.FileStream(path, append ? System.IO.FileMode.Append : System.IO.FileMode.Create,
                                         System.IO.FileAccess.Write, System.IO.FileShare.ReadWrite).Dispose();
                redirs.Append(' ').Append(fd).Append(append ? ">>" : ">").Append(Pos(path));
            }
            else if (spec is Nil)
                redirs.Append(' ').Append(fd).Append(">/dev/null");
        }
        if (FilePath(input) is string inPath)
        {
            // An input file that cannot be read gave the child EOF before: keep that.
            bool readable;
            try
            {
                new System.IO.FileStream(inPath, System.IO.FileMode.Open, System.IO.FileAccess.Read,
                                         System.IO.FileShare.ReadWrite).Dispose();
                readable = true;
            }
            catch (System.Exception) { readable = false; }
            redirs.Append(readable ? " <" + Pos(inPath) : " </dev/null");
        }
        else if (input is Nil)
            redirs.Append(" </dev/null");
        Out("", output, ifOutputExists);
        if (mergeErr)
        {
            if (IsFdTarget(output)) redirs.Append(" 2>&1");
        }
        else
            Out("2", error, ifErrorOutputExists);

        var script = "exec" + redirs + (files.Count > 0 ? "; shift " + files.Count : "") + "; exec \"$@\"";
        var shArgs = new System.Collections.Generic.List<string> { "-c", script, "sh" };
        shArgs.AddRange(files);
        shArgs.Add(execName);
        shArgs.AddRange(arguments);
        return MakeStartInfo("/bin/sh", shArgs);
    }

    [System.Runtime.InteropServices.DllImport("libc", SetLastError = true, EntryPoint = "access")]
    private static extern int access_(string path, int mode);

    private static bool IsExecutableFile(string path)
    {
        if (!System.IO.File.Exists(path)) return false;
        try { return access_(path, 1 /* X_OK */) == 0; }
        catch (System.Exception) { return true; }
    }

    private static string? FindInPath(string name, string? pathVar)
    {
        if (pathVar == null) return null;
        foreach (var dir in pathVar.Split(':'))
        {
            if (dir.Length == 0) continue;
            string cand;
            try { cand = System.IO.Path.Combine(dir, name); }
            catch (System.ArgumentException) { continue; }
            if (IsExecutableFile(cand)) return cand;
        }
        return null;
    }

    /// <summary>The name to hand the shell's exec for PROGRAM, after resolving it
    /// the way Process.Start does on Unix (as given if rooted; else next to the
    /// running executable, then in the current directory, then on PATH) and
    /// failing the way it fails when execve would.</summary>
    private static string ResolveForExec(string program, string? directory,
                                         System.Collections.Generic.List<string>? environment)
    {
        var cwd = directory ?? System.IO.Directory.GetCurrentDirectory();
        System.Exception Fail(int errno) => new System.ComponentModel.Win32Exception(errno,
            $"An error occurred trying to start process '{program}' with working directory '{cwd}'. "
            + new System.ComponentModel.Win32Exception(errno).Message);
        const int ENOENT = 2, EACCES = 13;
        if (directory != null && !System.IO.Directory.Exists(directory)) throw Fail(ENOENT);

        string? resolved = null;
        if (System.IO.Path.IsPathRooted(program)) resolved = program;
        else
        {
            var exe = Compat.ProcessPath();
            if (exe != null)
            {
                try
                {
                    var p = System.IO.Path.Combine(System.IO.Path.GetDirectoryName(exe)!, program);
                    if (System.IO.File.Exists(p)) resolved = p;
                }
                catch (System.ArgumentException) { }
            }
            if (resolved == null)
            {
                var p = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), program);
                if (System.IO.File.Exists(p)) resolved = p;
            }
            resolved ??= FindInPath(program, System.Environment.GetEnvironmentVariable("PATH"));
        }
        if (resolved == null) throw Fail(ENOENT);
        if (System.IO.Directory.Exists(resolved)) throw Fail(EACCES);
        if (!System.IO.File.Exists(resolved)) throw Fail(ENOENT);
        if (!IsExecutableFile(resolved)) throw Fail(EACCES);

        // Does the shell's own lookup land on the same file?
        string? shFinds;
        if (System.IO.Path.IsPathRooted(program)) shFinds = program;
        else if (program.IndexOf('/') >= 0)
            shFinds = System.IO.Path.GetFullPath(System.IO.Path.Combine(cwd, program));
        else
        {
            string? childPath = System.Environment.GetEnvironmentVariable("PATH");
            if (environment != null)
            {
                childPath = null;
                foreach (var kv in environment)
                    if (kv.StartsWith("PATH=", System.StringComparison.Ordinal)) childPath = kv.Substring(5);
            }
            shFinds = FindInPath(program, childPath);
        }
        // A name starting with '-' would be taken as an option of exec.
        return shFinds != null && !program.StartsWith("-", System.StringComparison.Ordinal)
               && System.IO.Path.GetFullPath(shFinds) == System.IO.Path.GetFullPath(resolved)
            ? program : System.IO.Path.GetFullPath(resolved);
    }

    private static LispObject WireOutput(
        LispObject spec, LispObject ifExists, System.Text.Encoding? encoding, string threadName,
        System.Func<System.IO.TextReader> source, System.Diagnostics.Process proc,
        System.Collections.Generic.List<System.Threading.Thread> helpers)
    {
        if (IsKw(spec, "STREAM"))
            return new LispInputStream(new ProcessStreamReader(source(), proc));
        if (FilePath(spec) is string path)
        {
            var w = OpenRedirectFile(path, IsKw(ifExists, "APPEND"), encoding);
            helpers.Add(Spawn(threadName, () => { using (w) Copy(source(), w); }));
        }
        else if (!IsInherit(spec))   // nil: drain & discard so a full pipe can't block the child
            helpers.Add(Spawn(threadName, () => Copy(source(), System.IO.TextWriter.Null)));
        return Nil.Instance;
    }

    /// <summary>Wire stdout AND stderr into a single destination (uiop's
    /// :error-output :output). :stream yields one merged live stream; a file gets both
    /// pipes appended under a lock; nil drains both to null. Caller guarantees the
    /// destination (stdout spec) is redirected (not inherited).</summary>
    private static LispObject WireMergedOutput(
        LispObject spec, LispObject ifExists, System.Text.Encoding? encoding, System.Diagnostics.Process proc,
        System.Collections.Generic.List<System.Threading.Thread> helpers,
        System.Func<System.IO.TextReader> stdoutSrc, System.Func<System.IO.TextReader> stderrSrc)
    {
        if (IsKw(spec, "STREAM"))
            return new LispInputStream(new ProcessStreamReader(proc, stdoutSrc(), stderrSrc()));
        if (FilePath(spec) is string path)
        {
            var w = OpenRedirectFile(path, IsKw(ifExists, "APPEND"), encoding);
            var wlock = new object();
            int pending = 2;
            void addDrain(System.Func<System.IO.TextReader> src, string nm) =>
                helpers.Add(Spawn(nm, () => {
                    try { CopyLocked(src(), w, wlock); }
                    finally { lock (wlock) { if (--pending == 0) { try { w.Dispose(); } catch { } } } }
                }));
            addDrain(stdoutSrc, "dotcl-drain-stdout");
            addDrain(stderrSrc, "dotcl-drain-stderr-merged");
            return Nil.Instance;
        }
        // nil: drain both & discard so a full pipe can't block the child
        helpers.Add(Spawn("dotcl-drain-stdout", () => Copy(stdoutSrc(), System.IO.TextWriter.Null)));
        helpers.Add(Spawn("dotcl-drain-stderr-merged", () => Copy(stderrSrc(), System.IO.TextWriter.Null)));
        return Nil.Instance;
    }

    /// <summary>The file a child's output is redirected to. The pipe was decoded
    /// with ENCODING, so the file is written in it too and ends up with the bytes
    /// the child wrote. Without one, UTF-8 as before.</summary>
    private static System.IO.StreamWriter OpenRedirectFile(string path, bool append, System.Text.Encoding? encoding)
        => encoding == null ? new System.IO.StreamWriter(path, append)
                            : new System.IO.StreamWriter(path, append, encoding);

    private static void CopyLocked(System.IO.TextReader r, System.IO.TextWriter w, object wlock)
    {
        var buf = new char[4096];
        int n;
        while ((n = r.Read(buf, 0, buf.Length)) > 0)
            lock (wlock) { w.Write(buf, 0, n); w.Flush(); }
    }

    private static void EnsureInputExists(string path, LispObject ifDoesNotExist)
    {
        if (System.IO.File.Exists(path)) return;
        if (ifDoesNotExist is Nil) return;   // treated as no input
        throw new LispErrorException(new LispError($"LAUNCH-PROCESS: input file does not exist: {path}"));
    }

    private static void CheckOutputExists(string path, LispObject ifExists)
    {
        if (IsKw(ifExists, "ERROR") && System.IO.File.Exists(path))
            throw new LispErrorException(new LispError($"LAUNCH-PROCESS: output file already exists: {path}"));
    }

    public override string ToString()
    {
        try { return $"#<PROCESS pid={Process.Id}>"; }
        catch { return "#<PROCESS>"; }
    }
}
