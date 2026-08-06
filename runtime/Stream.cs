namespace DotCL;

/// <summary>TextReader wrapper that tracks how many characters have been read.</summary>
public class PositionTrackingReader : TextReader
{
    private readonly TextReader _inner;
    public int Position { get; set; }

    public PositionTrackingReader(TextReader inner) => _inner = inner;

    public override int Read()
    {
        int ch = _inner.Read();
        if (ch != -1) Position++;
        return ch;
    }

    public override int Peek() => _inner.Peek();

    public override int Read(char[] buffer, int index, int count)
    {
        int n = _inner.Read(buffer, index, count);
        if (n > 0) Position += n;
        return n;
    }

    public override string? ReadLine()
    {
        var line = _inner.ReadLine();
        if (line != null) Position += line.Length + 1; // +1 for newline
        return line;
    }
}

public abstract class LispStream : LispObject
{
    public abstract bool IsInput { get; }
    public abstract bool IsOutput { get; }
    /// <summary>Stream type name for ClassOf dispatch (null = "STREAM")</summary>
    public virtual string? StreamTypeName => null;
    /// <summary>True if the stream has been closed.</summary>
    public bool IsClosed { get; set; }
    /// <summary>Pushback buffer for UNREAD-CHAR. -1 means empty.</summary>
    public int UnreadCharValue { get; set; } = -1;
    /// <summary>Element type of the stream. Default is CHARACTER (null means CHARACTER).</summary>
    public LispObject? ElementType { get; set; }
    /// <summary>External format the stream was opened with, as the designator the
    /// caller supplied; null means the implementation default (UTF-8). Reported by
    /// STREAM-EXTERNAL-FORMAT.</summary>
    public LispObject? ExternalFormat { get; set; }
    /// <summary>True if the last character written was a newline (or nothing written yet).</summary>
    public bool AtLineStart { get; set; } = true;
    /// <summary>Cached Reader instance for ReadFromStream, so pushback state is preserved across calls.</summary>
    public Reader? CachedReader { get; set; }
    /// <summary>Shared #n= labels for Reader instances on this stream, so share references work across Reader lifetimes.</summary>
    public Dictionary<int, LispObject>? ShareLabels { get; set; }
    /// <summary>Shared #n# placeholders for Reader instances on this stream.</summary>
    public Dictionary<int, SharePlaceholder>? SharePlaceholders { get; set; }
}

public class LispInputStream : LispStream
{
    public TextReader Reader { get; protected set; }
    public override bool IsInput => true;
    public override bool IsOutput => false;

    public LispInputStream(TextReader reader) => Reader = reader;

    public override string ToString() => "#<INPUT-STREAM>";
}

public class LispOutputStream : LispStream
{
    public TextWriter Writer { get; }
    public override bool IsInput => false;
    public override bool IsOutput => true;

    public LispOutputStream(TextWriter writer) => Writer = writer;

    public override string ToString() => "#<OUTPUT-STREAM>";
}

public class LispBidirectionalStream : LispStream
{
    public TextReader Reader { get; }
    public TextWriter Writer { get; }
    public override bool IsInput => true;
    public override bool IsOutput => true;

    public LispBidirectionalStream(TextReader reader, TextWriter writer)
    {
        Reader = reader;
        Writer = writer;
    }

    public override string ToString() => "#<BIDIRECTIONAL-STREAM>";
}

public class LispFileStream : LispStream
{
    public string FilePath { get; }
    public TextReader? InputReader { get; }
    public TextWriter? OutputWriter { get; }
    public override bool IsInput => InputReader != null;
    public override bool IsOutput => OutputWriter != null;
    public override string? StreamTypeName => "FILE-STREAM";
    /// <summary>Original Lisp pathname object used to open this stream (may be a logical pathname).</summary>
    public LispPathname? OriginalPathname { get; set; }

    // Input file stream
    public LispFileStream(StreamReader reader, string path)
    {
        InputReader = reader;
        FilePath = path;
    }

    // Output file stream
    public LispFileStream(StreamWriter writer, string path)
    {
        OutputWriter = writer;
        FilePath = path;
    }

    // Bidirectional file stream
    public LispFileStream(StreamReader reader, StreamWriter writer, string path)
    {
        InputReader = reader;
        OutputWriter = writer;
        FilePath = path;
    }

    // Probe (no reader or writer, just path)
    public LispFileStream(string path)
    {
        FilePath = path;
    }

    public void Close()
    {
        if (IsClosed) return;
        IsClosed = true;
        try { InputReader?.Close(); } catch (ObjectDisposedException) { }
        try { OutputWriter?.Close(); } catch (ObjectDisposedException) { }
    }

    public override string ToString() => $"#<FILE-STREAM \"{FilePath}\">";
}

public class LispStringOutputStream : LispOutputStream
{
    private readonly StringWriter _sw;
    public string? ElementTypeName { get; set; }

    public LispStringOutputStream(StringWriter sw, string? elementTypeName = null) : base(sw)
    {
        _sw = sw;
        ElementTypeName = elementTypeName;
    }

    public string GetString() => _sw.ToString();

    /// <summary>Get the string and reset the stream (for GET-OUTPUT-STREAM-STRING).</summary>
    public string GetStringAndReset()
    {
        var result = _sw.ToString();
        _sw.GetStringBuilder().Clear();
        return result;
    }

    public override string? StreamTypeName => "STRING-STREAM";
    public override string ToString() => "#<STRING-OUTPUT-STREAM>";
}

/// <summary>TextWriter that appends characters to a LispVector with fill-pointer using VECTOR-PUSH-EXTEND.</summary>
public class FillPointerStringWriter : TextWriter
{
    private readonly LispVector _vector;

    public FillPointerStringWriter(LispVector vector) => _vector = vector;

    public override System.Text.Encoding Encoding => System.Text.Encoding.Unicode;

    public override void Write(char value)
    {
        _vector.VectorPushExtend(LispChar.Make(value), 16);
    }

    public override void Write(char[] buffer, int index, int count)
    {
        for (int i = index; i < index + count; i++)
            _vector.VectorPushExtend(LispChar.Make(buffer[i]), 16);
    }

    public override void Write(string? value)
    {
        if (value == null) return;
        for (int i = 0; i < value.Length; i++)
            _vector.VectorPushExtend(LispChar.Make(value[i]), 16);
    }
}

/// <summary>String output stream that writes to an existing string (LispVector with fill-pointer).</summary>
public class LispFillPointerStringOutputStream : LispOutputStream
{
    public LispFillPointerStringOutputStream(LispVector vector) : base(new FillPointerStringWriter(vector))
    {
    }

    public override string? StreamTypeName => "STRING-STREAM";
    public override string ToString() => "#<STRING-OUTPUT-STREAM>";
}

public class LispStringInputStream : LispInputStream
{
    /// <summary>The starting offset from the original string (for :start parameter).</summary>
    public int StartOffset { get; set; }
    /// <summary>The position-tracking wrapper for this stream's reader.</summary>
    public PositionTrackingReader? TrackingReader { get; private set; }
    /// <summary>The full original string (before any slicing). Used for repositioning.</summary>
    private readonly string? _fullString;
    /// <summary>The exclusive end offset in the full string.</summary>
    private readonly int _endOffset;

    public LispStringInputStream(StringReader reader) : base(new PositionTrackingReader(reader))
    {
        TrackingReader = (PositionTrackingReader)Reader;
    }
    public LispStringInputStream(StringReader reader, int startOffset, string? fullString = null, int endOffset = 0)
        : base(new PositionTrackingReader(reader))
    {
        StartOffset = startOffset;
        TrackingReader = (PositionTrackingReader)Reader;
        _fullString = fullString;
        _endOffset = endOffset;
    }
    public LispStringInputStream(PositionTrackingReader trackingReader, int startOffset = 0, string? fullString = null, int endOffset = 0)
        : base(trackingReader)
    {
        StartOffset = startOffset;
        TrackingReader = trackingReader;
        _fullString = fullString;
        _endOffset = endOffset;
    }

    /// <summary>Current position in the original string.</summary>
    public int Position => StartOffset + (TrackingReader?.Position ?? 0);

    /// <summary>Reposition the stream to an absolute position in the original string.
    /// Recreates the underlying StringReader at the target offset.</summary>
    public bool SeekToPosition(int absolutePosition)
    {
        if (_fullString == null) return false;
        // Allow seeking to the end (== _endOffset), the valid EOF position:
        // (file-position s (length s)) leaves the stream at end-of-input. A
        // position past the end is still rejected.
        if (absolutePosition < 0 || absolutePosition > _endOffset) return false;
        var sub = _fullString.Substring(absolutePosition, _endOffset - absolutePosition);
        var newReader = new PositionTrackingReader(new StringReader(sub));
        Reader = newReader;
        TrackingReader = newReader;
        StartOffset = absolutePosition;
        return true;
    }

    public override string? StreamTypeName => "STRING-STREAM";
    public override string ToString() => "#<STRING-INPUT-STREAM>";
}

/// <summary>TextWriter that multiplexes writes to multiple writers (for broadcast streams).</summary>
public class BroadcastTextWriter : TextWriter
{
    private readonly TextWriter[] _writers;
    public BroadcastTextWriter(TextWriter[] writers) => _writers = writers;
    public override System.Text.Encoding Encoding => _writers.Length > 0 ? _writers[^1].Encoding : System.Text.Encoding.Unicode;
    public override void Write(char value) { foreach (var w in _writers) w.Write(value); }
    public override void Write(string? value) { foreach (var w in _writers) w.Write(value); }
    public override void Write(char[] buffer, int index, int count) { foreach (var w in _writers) w.Write(buffer, index, count); }
    public override void WriteLine(string? value) { foreach (var w in _writers) w.WriteLine(value); }
    public override void Flush() { foreach (var w in _writers) w.Flush(); }
}

/// <summary>Broadcast stream: output goes to all component streams.</summary>
public class LispBroadcastStream : LispStream
{
    public LispStream[] Streams { get; }
    public override bool IsInput => false;
    public override bool IsOutput => true;
    public override string? StreamTypeName => "BROADCAST-STREAM";

    public LispBroadcastStream(LispStream[] streams) => Streams = streams;

    public override string ToString() => "#<BROADCAST-STREAM>";
}

/// <summary>Concatenated stream: reads from component streams in sequence.</summary>
public class LispConcatenatedStream : LispStream
{
    // LispObject[] components — see LispTwoWayStream (Gray stream support).
    public LispObject[] Streams { get; }
    public int CurrentIndex { get; set; } = 0;
    public override bool IsInput => true;
    public override bool IsOutput => false;
    public override string? StreamTypeName => "CONCATENATED-STREAM";

    public LispConcatenatedStream(LispObject[] streams) => Streams = streams;

    public override string ToString() => "#<CONCATENATED-STREAM>";
}

/// <summary>Echo stream: reads from input, echoes to output.</summary>
public class LispEchoStream : LispStream
{
    // LispObject components — see LispTwoWayStream for why (Gray stream support +
    // accessor identity).
    public LispObject InputStream { get; }
    public LispObject OutputStream { get; }
    // See LispTwoWayStream.ResolvedInputCache.
    internal LispStream? ResolvedInputCache;
    public override bool IsInput => true;
    public override bool IsOutput => true;
    public override string? StreamTypeName => "ECHO-STREAM";

    public LispEchoStream(LispObject input, LispObject output)
    {
        InputStream = input;
        OutputStream = output;
    }

    public override string ToString() => "#<ECHO-STREAM>";
}

/// <summary>Synonym stream: delegates to the stream stored in a symbol.</summary>
public class LispSynonymStream : LispStream
{
    public Symbol Symbol { get; }
    public override bool IsInput
    {
        get
        {
            if (DynamicBindings.TryGet(Symbol, out var val) && val is LispStream s) return s.IsInput;
            return true; // default if can't resolve
        }
    }
    public override bool IsOutput
    {
        get
        {
            if (DynamicBindings.TryGet(Symbol, out var val) && val is LispStream s) return s.IsOutput;
            return true; // default if can't resolve
        }
    }
    public override string? StreamTypeName => "SYNONYM-STREAM";

    public LispSynonymStream(Symbol sym) => Symbol = sym;

    public override string ToString() => $"#<SYNONYM-STREAM {Symbol.Name}>";
}

/// <summary>Two-way stream: separate input and output streams.</summary>
public class LispTwoWayStream : LispStream
{
    // LispObject (not LispStream) so a Gray CLOS stream (a LispInstance, not a
    // LispStream subclass) can be a component. GetTextReader/GetTextWriter and the
    // byte helpers already resolve composite components and dispatch Gray at the
    // leaf, so holding the original object also preserves accessor identity
    // (two-way-stream-input-stream returns the exact object given).
    public LispObject InputStream { get; }
    public LispObject OutputStream { get; }
    // Native adapter cached when InputStream is a Gray stream, so the char-level
    // read path (which needs a LispStream with a persistent unread-char slot)
    // reads through the Gray protocol. Null when InputStream is already a LispStream.
    internal LispStream? ResolvedInputCache;
    public override bool IsInput => true;
    public override bool IsOutput => true;
    public override string? StreamTypeName => "TWO-WAY-STREAM";

    public LispTwoWayStream(LispObject input, LispObject output)
    {
        InputStream = input;
        OutputStream = output;
    }

    public override string ToString() => "#<TWO-WAY-STREAM>";
}

/// <summary>Binary stream wrapping a raw System.IO.Stream for byte-level I/O.</summary>
public class LispBinaryStream : LispStream
{
    public System.IO.Stream BaseStream { get; }
    public override bool IsInput => BaseStream.CanRead;
    public override bool IsOutput => BaseStream.CanWrite;
    public override string? StreamTypeName => "BINARY-STREAM";

    public LispBinaryStream(System.IO.Stream stream)
    {
        BaseStream = stream;
        ElementType = new Cons(Startup.Sym("UNSIGNED-BYTE"),
                        new Cons(new Fixnum(8), Nil.Instance));
    }

    public override string ToString() => $"#<BINARY-STREAM>";
}

/// <summary>TextReader over a raw byte Stream that does NOT read ahead, so the same
/// stream can serve both character I/O (read-char/read-line) and raw byte I/O
/// (read-byte) without losing buffered bytes — a "bivalent" stream, as SBCL's socket
/// streams are. Characters are decoded one UTF-8 codepoint at a time, pulling only the
/// bytes that codepoint needs; ReadRawByte / PeekRawByte draw from the same byte source
/// (a tiny pushback ring), so char and byte reads stay coordinated.</summary>
public sealed class BivalentStreamReader : System.IO.TextReader
{
    private readonly System.IO.Stream _s;
    // pushback ring for raw bytes (max lookahead = 4 bytes for one UTF-8 codepoint)
    private readonly int[] _pb = new int[8];
    private int _pbHead, _pbCount;

    public BivalentStreamReader(System.IO.Stream s) => _s = s;
    public System.IO.Stream BaseStream => _s;

    private int NextByte()
    {
        if (_pbCount > 0) { int v = _pb[_pbHead]; _pbHead = (_pbHead + 1) % _pb.Length; _pbCount--; return v; }
        return _s.ReadByte();
    }
    private void PushFront(int b)
    {
        _pbHead = (_pbHead - 1 + _pb.Length) % _pb.Length;
        _pb[_pbHead] = b; _pbCount++;
    }

    /// <summary>Raw byte read (read-byte). -1 on EOF.</summary>
    public int ReadRawByte() => NextByte();
    /// <summary>Raw byte peek without consuming. -1 on EOF.</summary>
    public int PeekRawByte()
    {
        int b = NextByte();
        if (b >= 0) PushFront(b);
        return b;
    }

    // Decode one UTF-8 codepoint. When consume is false, the bytes read are pushed back
    // so a peek does not advance the byte position (keeps byte/char reads coordinated).
    private int ReadCodepoint(bool consume)
    {
        int b0 = NextByte();
        if (b0 < 0) return -1;
        if (b0 < 0x80) { if (!consume) PushFront(b0); return b0; }

        int extra, cp;
        if ((b0 & 0xE0) == 0xC0) { extra = 1; cp = b0 & 0x1F; }
        else if ((b0 & 0xF0) == 0xE0) { extra = 2; cp = b0 & 0x0F; }
        else if ((b0 & 0xF8) == 0xF0) { extra = 3; cp = b0 & 0x07; }
        else { if (!consume) PushFront(b0); return 0xFFFD; } // invalid lead byte

        Span<int> got = stackalloc int[4];
        got[0] = b0; int n = 1;
        for (int i = 0; i < extra; i++)
        {
            int bi = NextByte();
            if (bi < 0 || (bi & 0xC0) != 0x80) // truncated / invalid continuation
            {
                if (bi >= 0) PushFront(bi);
                for (int k = n - 1; k >= 1; k--) PushFront(got[k]); // restore consumed continuation bytes
                if (!consume) PushFront(b0);
                return 0xFFFD;
            }
            got[n++] = bi; cp = (cp << 6) | (bi & 0x3F);
        }
        if (!consume) for (int k = n - 1; k >= 0; k--) PushFront(got[k]);
        return cp;
    }

    public override int Read() => ReadCodepoint(true);
    public override int Peek() => ReadCodepoint(false);
}

/// <summary>TextWriter over a raw byte Stream that writes UTF-8 directly (no buffering,
/// no BOM). Companion to BivalentStreamReader: WriteRawByte emits a raw byte to the same
/// stream, so character and byte output share one sink.</summary>
public sealed class BivalentStreamWriter : System.IO.TextWriter
{
    private readonly System.IO.Stream _s;
    private static readonly System.Text.UTF8Encoding Utf8NoBom = new(false);

    public BivalentStreamWriter(System.IO.Stream s) => _s = s;
    public System.IO.Stream BaseStream => _s;
    public override System.Text.Encoding Encoding => Utf8NoBom;

    public override void Write(char c)
    {
        var bytes = Utf8NoBom.GetBytes(new[] { c });
        _s.Write(bytes, 0, bytes.Length);
    }
    public override void Write(string? value)
    {
        if (string.IsNullOrEmpty(value)) return;
        var bytes = Utf8NoBom.GetBytes(value);
        _s.Write(bytes, 0, bytes.Length);
    }
    /// <summary>Raw byte write (write-byte).</summary>
    public void WriteRawByte(int b) => _s.WriteByte((byte)b);
    public override void Flush() => _s.Flush();
}
