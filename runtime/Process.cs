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

    public ProcessStreamReader(System.IO.TextReader inner, System.Diagnostics.Process proc)
    {
        var drain = new System.Threading.Thread(() =>
        {
            try
            {
                var chunk = new char[4096];
                int n;
                while ((n = inner.Read(chunk, 0, chunk.Length)) > 0)
                {
                    lock (_lock) { _buf.Append(chunk, 0, n); System.Threading.Monitor.PulseAll(_lock); }
                }
            }
            catch { /* pipe closed/broken — treat as EOF */ }
            finally
            {
                lock (_lock) { _eof = true; System.Threading.Monitor.PulseAll(_lock); }
                try { inner.Dispose(); } catch { }
            }
        })
        { IsBackground = true, Name = "dotcl-process-drain" };
        drain.Start();
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
    /// NOTE: character streams only — :element-type (unsigned-byte 8) is not yet handled.</summary>
    public static LispProcess Launch(
        string program, System.Collections.Generic.List<string> arguments, string? directory,
        LispObject input, LispObject output, LispObject error,
        LispObject ifInputDoesNotExist, LispObject ifOutputExists, LispObject ifErrorOutputExists)
    {
        if (FilePath(input) is string inPath0) EnsureInputExists(inPath0, ifInputDoesNotExist);
        if (FilePath(output) is string outPath0) CheckOutputExists(outPath0, ifOutputExists);
        if (FilePath(error) is string errPath0) CheckOutputExists(errPath0, ifErrorOutputExists);

        var psi = new System.Diagnostics.ProcessStartInfo(program)
        {
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardInput = !IsInherit(input),
            RedirectStandardOutput = !IsInherit(output),
            RedirectStandardError = !IsInherit(error),
        };
        foreach (var a in arguments) psi.ArgumentList.Add(a);
        if (!string.IsNullOrEmpty(directory)) psi.WorkingDirectory = directory;

        var proc = System.Diagnostics.Process.Start(psi)!;
        var helpers = new System.Collections.Generic.List<System.Threading.Thread>();
        LispObject inStream = Nil.Instance, outStream = Nil.Instance, errStream = Nil.Instance;

        // --- stdin ---
        if (IsKw(input, "STREAM"))
            inStream = new LispOutputStream(proc.StandardInput);
        else if (FilePath(input) is string inPath)
            helpers.Add(Spawn("dotcl-feed-stdin", () => {
                try { using var f = new System.IO.StreamReader(inPath); Copy(f, proc.StandardInput); }
                finally { try { proc.StandardInput.Close(); } catch { } }
            }));
        else if (!IsInherit(input))
            try { proc.StandardInput.Close(); } catch { }   // nil: child sees EOF

        // --- stdout ---
        outStream = WireOutput(output, ifOutputExists, "dotcl-drain-stdout",
                               () => proc.StandardOutput, proc, helpers);
        // --- stderr ---
        errStream = WireOutput(error, ifErrorOutputExists, "dotcl-drain-stderr",
                               () => proc.StandardError, proc, helpers);

        return new LispProcess(proc, inStream, outStream, errStream, helpers);
    }

    private static LispObject WireOutput(
        LispObject spec, LispObject ifExists, string threadName,
        System.Func<System.IO.TextReader> source, System.Diagnostics.Process proc,
        System.Collections.Generic.List<System.Threading.Thread> helpers)
    {
        if (IsKw(spec, "STREAM"))
            return new LispInputStream(new ProcessStreamReader(source(), proc));
        if (FilePath(spec) is string path)
        {
            var w = new System.IO.StreamWriter(path, append: IsKw(ifExists, "APPEND"));
            helpers.Add(Spawn(threadName, () => { using (w) Copy(source(), w); }));
        }
        else if (!IsInherit(spec))   // nil: drain & discard so a full pipe can't block the child
            helpers.Add(Spawn(threadName, () => Copy(source(), System.IO.TextWriter.Null)));
        return Nil.Instance;
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
