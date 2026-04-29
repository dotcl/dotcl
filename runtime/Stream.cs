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
    public TextReader Reader { get; }
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
    public PositionTrackingReader? TrackingReader { get; }

    public LispStringInputStream(StringReader reader) : base(new PositionTrackingReader(reader))
    {
        TrackingReader = (PositionTrackingReader)Reader;
    }
    public LispStringInputStream(StringReader reader, int startOffset) : base(new PositionTrackingReader(reader))
    {
        StartOffset = startOffset;
        TrackingReader = (PositionTrackingReader)Reader;
    }

    /// <summary>Current position in the original string.</summary>
    public int Position => StartOffset + (TrackingReader?.Position ?? 0);

    public override string? StreamTypeName => "STRING-STREAM";
    public override string ToString() => "#<STRING-INPUT-STREAM>";
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
    public LispStream[] Streams { get; }
    public int CurrentIndex { get; set; } = 0;
    public override bool IsInput => true;
    public override bool IsOutput => false;
    public override string? StreamTypeName => "CONCATENATED-STREAM";

    public LispConcatenatedStream(LispStream[] streams) => Streams = streams;

    public override string ToString() => "#<CONCATENATED-STREAM>";
}

/// <summary>Echo stream: reads from input, echoes to output.</summary>
public class LispEchoStream : LispStream
{
    public LispStream InputStream { get; }
    public LispStream OutputStream { get; }
    public override bool IsInput => true;
    public override bool IsOutput => true;
    public override string? StreamTypeName => "ECHO-STREAM";

    public LispEchoStream(LispStream input, LispStream output)
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
    public LispStream InputStream { get; }
    public LispStream OutputStream { get; }
    public override bool IsInput => true;
    public override bool IsOutput => true;
    public override string? StreamTypeName => "TWO-WAY-STREAM";

    public LispTwoWayStream(LispStream input, LispStream output)
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
