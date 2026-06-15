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
/// instead of the collect-all dotcl:run-process.</summary>
public sealed class LispProcess : LispObject
{
    public System.Diagnostics.Process Process { get; }
    /// <summary>Writable stream to the child's stdin (LispOutputStream over StandardInput).</summary>
    public LispObject InputStream { get; }
    /// <summary>Readable stream from the child's stdout (LispInputStream over StandardOutput).</summary>
    public LispObject OutputStream { get; }
    /// <summary>Readable stream from the child's stderr (LispInputStream over StandardError).</summary>
    public LispObject ErrorStream { get; }

    public LispProcess(System.Diagnostics.Process process,
                       LispObject input, LispObject output, LispObject error)
    {
        Process = process;
        InputStream = input;
        OutputStream = output;
        ErrorStream = error;
    }

    public override string ToString()
    {
        try { return $"#<PROCESS pid={Process.Id}>"; }
        catch { return "#<PROCESS>"; }
    }
}
