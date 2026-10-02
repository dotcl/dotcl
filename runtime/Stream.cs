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


/// <summary>Character file input that knows its byte offset.
///
/// FILE-POSITION on a character file stream reports where the next character
/// starts, in bytes -- that is what SBCL answers, and what the seek side of
/// FILE-POSITION here has always taken. Asking a StreamReader could not produce
/// it: BaseStream.Position is where the BUFFER was filled to, so one READ-CHAR
/// of a ten-byte file answered 10. It was right only when the buffer happened to
/// be empty (before the first read, right after a seek, at end of file).
///
/// The fix is not to re-encode the characters handed out -- that invents
/// questions about line terminators, byte order marks and surrogate pairs. It is
/// to decode with a Decoder, which reports how many bytes each decode consumed,
/// and to count those. Because the count lives under the decode rather than in
/// the reading API, READLINE and READTOEND need no special case.
///
/// Bytes are read from the file a block at a time; only the decoding is done a
/// character at a time, so this costs no extra system calls.
/// </summary>
public sealed class ByteTrackingReader : TextReader
{
    private readonly System.IO.Stream _stream;
    private readonly System.Text.Decoder _decoder;
    private readonly bool _utf8;
    private readonly byte[] _bytes = new byte[4096];
    private int _byteLen;                 // bytes in _bytes
    private int _bytePos;                 // consumed within _bytes
    private readonly char[] _pending = new char[2];
    private int _pendingLen, _pendingPos; // chars decoded but not yet handed out
    private long _pendingBytes;           // bytes behind the pending characters
    private long _fillEnd;                // where the underlying stream should be
                                          // if only this reader has moved it

    /// <summary>True when a writer shares the underlying stream (an :IO file
    /// stream). A write then moves the stream under this reader, and whatever
    /// the reader holds decoded belongs to the old position.</summary>
    public bool SharesStreamWithWriter { get; set; }

    /// <summary>Byte offset of the next character. Bytes whose characters have been
    /// decoded but not yet handed out are not counted: a surrogate pair reports its
    /// four bytes when its second half is read, so the value only ever lands on a
    /// character boundary.</summary>
    public long BytePosition { get; private set; }

    public System.IO.Stream BaseStream => _stream;
    public System.Text.Encoding CurrentEncoding { get; }

    public ByteTrackingReader(System.IO.Stream stream, System.Text.Encoding encoding,
                              bool skipPreamble)
    {
        _stream = stream;
        CurrentEncoding = encoding;
        _decoder = encoding.GetDecoder();
        _utf8 = encoding.CodePage == 65001;
        // A stream opened for appending starts at its end, not at 0.
        if (_stream.CanSeek) BytePosition = _fillEnd = _stream.Position;
        if (skipPreamble) SkipPreamble();
    }

    /// <summary>For a reader that shares its stream with a writer: when a write
    /// has moved the stream since this reader last read from it, drop what was
    /// decoded and continue from where the stream now is, which is where the
    /// write ended.</summary>
    public void SyncWithWriter()
    {
        if (SharesStreamWithWriter && _stream.CanSeek && _stream.Position != _fillEnd)
            ResetTo(_stream.Position);
    }

    /// <summary>A byte order mark is not part of the text, so the first character
    /// starts after it. StreamReader consumes it invisibly; here it has to be
    /// consumed explicitly or every position would be off by its length.</summary>
    private void SkipPreamble()
    {
        var pre = CurrentEncoding.GetPreamble();
        if (pre.Length == 0 || !_stream.CanSeek) return;
        var head = new byte[pre.Length];
        long at = _stream.Position;
        int got = _stream.Read(head, 0, head.Length);
        bool match = got == pre.Length;
        for (int i = 0; match && i < pre.Length; i++) if (head[i] != pre[i]) match = false;
        if (match) BytePosition = _fillEnd = _stream.Position;
        else _stream.Position = at;
    }

    /// <summary>Called after the owner seeks the underlying stream: everything
    /// decoded so far belongs to the old position.</summary>
    public void ResetTo(long bytePosition)
    {
        _decoder.Reset();
        _byteLen = _bytePos = 0;
        _pendingLen = _pendingPos = 0;
        _pendingBytes = 0;
        BytePosition = _fillEnd = bytePosition;
    }

    /// <summary>Decode exactly one more character sequence into _pending.
    /// Returns false at end of input.</summary>
    private bool FillPending()
    {
        while (true)
        {
            if (_bytePos >= _byteLen)
            {
                _byteLen = _stream.Read(_bytes, 0, _bytes.Length);
                _bytePos = 0;
                if (_byteLen > 0) _fillEnd += _byteLen;
                if (_byteLen <= 0)
                {
                    // Flush whatever the decoder still holds (an incomplete
                    // sequence at end of file becomes the replacement character).
                    _pendingLen = _decoder.GetChars(System.Array.Empty<byte>(), 0, 0,
                                                    _pending, 0, flush: true);
                    _pendingPos = 0;
                    _pendingBytes = 0;
                    return _pendingLen > 0;
                }
            }
            // A UTF-8 byte below 0x80 is one character on its own. Source files are
            // very nearly all such bytes, and taking them without going through the
            // decoder is what keeps this reader close to a StreamReader: the
            // per-byte Convert call cost about 40% on READ-LINE without it.
            if (_utf8 && _bytes[_bytePos] < 0x80)
            {
                _pending[0] = (char)_bytes[_bytePos];
                _bytePos++;
                _pendingBytes += 1;
                _pendingLen = 1;
                _pendingPos = 0;
                return true;
            }
            // One byte at a time so the bytes are attributed to exactly the
            // characters they produced. The bytes are already in memory.
            _decoder.Convert(_bytes, _bytePos, 1, _pending, 0, _pending.Length,
                             flush: false, out int bytesUsed, out int charsUsed,
                             out _);
            _bytePos += bytesUsed;
            _pendingBytes += bytesUsed;
            if (charsUsed > 0)
            {
                _pendingLen = charsUsed;
                _pendingPos = 0;
                return true;
            }
        }
    }

    public override int Read()
    {
        if (SharesStreamWithWriter) SyncWithWriter();
        if (_pendingPos >= _pendingLen && !FillPending()) return -1;
        char c = _pending[_pendingPos++];
        if (_pendingPos >= _pendingLen)
        {
            BytePosition += _pendingBytes;
            _pendingBytes = 0;
        }
        return c;
    }

    public override int Peek()
    {
        if (SharesStreamWithWriter) SyncWithWriter();
        if (_pendingPos >= _pendingLen && !FillPending()) return -1;
        return _pending[_pendingPos];
    }

    public override int Read(char[] buffer, int index, int count)
    {
        int n = 0;
        while (n < count)
        {
            int c = Read();
            if (c == -1) break;
            buffer[index + n++] = (char)c;
        }
        return n;
    }

    public override string? ReadLine()
    {
        int first = Read();
        if (first == -1) return null;
        var sb = new System.Text.StringBuilder();
        int c = first;
        while (c != -1 && c != '\n')
        {
            if (c == '\r')
            {
                if (Peek() == '\n') Read();
                return sb.ToString();
            }
            sb.Append((char)c);
            c = Read();
        }
        return sb.ToString();
    }

    public override string ReadToEnd()
    {
        var sb = new System.Text.StringBuilder();
        int c;
        while ((c = Read()) != -1) sb.Append((char)c);
        return sb.ToString();
    }

    protected override void Dispose(bool disposing)
    {
        if (disposing) _stream.Dispose();
        base.Dispose(disposing);
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
    public LispFileStream(ByteTrackingReader reader, string path)
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
    public LispFileStream(ByteTrackingReader reader, StreamWriter writer, string path)
    {
        InputReader = reader;
        OutputWriter = writer;
        FilePath = path;
        reader.SharesStreamWithWriter = ReferenceEquals(reader.BaseStream, writer.BaseStream);
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

    /// <summary>The column the text of this stream starts at on its first line.
    /// Zero for a stream of its own; the column of the destination for the
    /// buffer a PRINT-OBJECT method is handed while the printer builds the text
    /// it writes there.</summary>
    public int StartColumn { get; set; }

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
    // Where this stream started writing: what the string held before belongs to
    // the caller, and trimming never reaches into it.
    private readonly int _start;

    public FillPointerStringWriter(LispVector vector)
    {
        _vector = vector;
        _start = vector.Length;
    }

    /// <summary>Remove spaces and tabs this stream wrote at the end of the
    /// string, as the pretty printer does before a line break it emits.
    /// Returns how many were removed.</summary>
    public int TrimTrailingBlanks()
    {
        int fp = _vector.Length, n = fp;
        while (n > _start && _vector.ElementAt(n - 1) is LispChar c
               && (c.Value == ' ' || c.Value == '\t'))
            n--;
        if (n != fp) _vector.SetFillPointer(n);
        return fp - n;
    }

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
        // A Lisp reader cached by an earlier READ wraps the old TextReader, which
        // is now detached from the stream (and may sit at end of input). Drop it
        // so the next READ builds one over the new position.
        CachedReader = null;
        return true;
    }

    public override string? StreamTypeName => "STRING-STREAM";
    public override string ToString() => "#<STRING-INPUT-STREAM>";

    // LOAD and COMPILE-FILE read a source file into a string and hand reader macros
    // a stream over it. A reader macro that records where a form starts (eclector,
    // and through it Coalton) asks FILE-POSITION, and expects what a stream opened
    // on the file would answer: a byte offset, as every file stream in dotcl and in
    // SBCL counts. Over a string the answer would be a character offset, which
    // differs as soon as the file holds a character outside ASCII.
    private bool _fileBytes;
    private int _preambleBytes;
    private int[]? _byteAt;

    /// <summary>Make FILE-POSITION count bytes of the UTF-8 file the text came from,
    /// PREAMBLEBYTES (a byte order mark the text no longer holds) included.</summary>
    public void ReportFileBytes(int preambleBytes)
    {
        _fileBytes = true;
        _preambleBytes = preambleBytes;
    }

    public bool ReportsFileBytes => _fileBytes && _fullString != null;

    private int[] ByteTable()
    {
        if (_byteAt != null) return _byteAt;
        var text = _fullString!;
        var table = new int[text.Length + 1];
        int b = _preambleBytes;
        for (int i = 0; i < text.Length; i++)
        {
            table[i] = b;
            char c = text[i];
            if (c < 0x80) b += 1;
            else if (c < 0x800) b += 2;
            else if (char.IsHighSurrogate(c) && i + 1 < text.Length && char.IsLowSurrogate(text[i + 1]))
            {
                b += 4;
                table[++i] = b;   // the low half: no byte boundary of its own
            }
            else b += 3;
        }
        table[text.Length] = b;
        return _byteAt = table;
    }

    /// <summary>The file byte offset of character position CHARPOS.</summary>
    public long FileByteOf(int charPos)
    {
        var t = ByteTable();
        return t[charPos < 0 ? 0 : charPos >= t.Length ? t.Length - 1 : charPos];
    }

    /// <summary>The character position at file byte offset BYTEPOS, or -1 when BYTEPOS
    /// is not the start of a character.</summary>
    public int CharAtFileByte(long bytePos)
    {
        var t = ByteTable();
        int i = Array.BinarySearch(t, (int)bytePos);
        return i >= 0 ? i : -1;
    }

    /// <summary>The length of a UTF-8 byte order mark at the start of PATH, or 0. A
    /// preamble of another encoding answers -1: those files are not counted in
    /// UTF-8 bytes.</summary>
    public static int Utf8Preamble(string path)
    {
        try
        {
            using var f = System.IO.File.OpenRead(path);
            var head = new byte[3];
            int n = f.Read(head, 0, 3);
            if (n >= 3 && head[0] == 0xEF && head[1] == 0xBB && head[2] == 0xBF) return 3;
            if (n >= 2 && ((head[0] == 0xFF && head[1] == 0xFE) || (head[0] == 0xFE && head[1] == 0xFF)))
                return -1;
            return 0;
        }
        catch { return -1; }
    }
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
    // LispObject[] components: see LispTwoWayStream (Gray stream support).
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
    // LispObject components: see LispTwoWayStream for why (Gray stream support +
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
/// (read-byte) without losing buffered bytes: a "bivalent" stream, as SBCL's socket
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
    /// <summary>The writer on the same .NET stream, when there is one. Its
    /// buffered output is sent before this reader reads, so a program that
    /// writes a request and reads the reply without FORCE-OUTPUT is not left
    /// waiting for a reply to a request that was never sent.</summary>
    internal BivalentStreamWriter? Partner { get; set; }

    private int NextByte()
    {
        if (_pbCount > 0) { int v = _pb[_pbHead]; _pbHead = (_pbHead + 1) % _pb.Length; _pbCount--; return v; }
        if (Partner is { HasPending: true } w) w.Flush();
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

    // CLOSE on the Lisp stream closes the stream under it: for a socket that is
    // what ends the connection, and the Lisp stream is all a caller may hold.
    protected override void Dispose(bool disposing)
    {
        if (disposing) _s.Dispose();
        base.Dispose(disposing);
    }

    /// <summary>For LISTEN: false when reading now would wait for the peer. Peek
    /// blocks on a socket until a byte arrives, which is what READ-CHAR does and
    /// what LISTEN must not. Null when the source can not tell without reading.</summary>
    public bool? DataReady()
    {
        if (_pbCount > 0) return true;
        if (Partner is { HasPending: true } w) w.Flush();
        if (_s is System.Net.Sockets.NetworkStream ns)
        {
            if (ns.DataAvailable) return true;
#if !NETSTANDARD2_0
            // Readable with nothing buffered means the peer has closed: Peek then
            // answers end of file at once, so let it.
            try
            {
                if (ns.Socket.Poll(0, System.Net.Sockets.SelectMode.SelectRead)) return null;
            }
            catch (System.ObjectDisposedException) { return null; }
#endif
            return false;
        }
        return null;
    }
}

/// <summary>TextWriter over a raw byte Stream that writes UTF-8 directly (no buffering,
/// no BOM). Companion to BivalentStreamReader: WriteRawByte emits a raw byte to the same
/// stream, so character and byte output share one sink.</summary>
public sealed class BivalentStreamWriter : System.IO.TextWriter
{
    private readonly System.IO.Stream _s;
    private static readonly System.Text.UTF8Encoding Utf8NoBom = new(false);
    // Output is buffered, as SBCL's socket streams are, and goes out on
    // FORCE-OUTPUT / FINISH-OUTPUT / CLOSE, when the buffer fills, or when the
    // paired reader is about to wait for input. Unbuffered, every WRITE-BYTE
    // and every piece of a protocol message was its own send: a reply went out
    // in as many TCP segments as it had writes.
    private readonly byte[] _buf = new byte[8192];
    private int _len;
    // The buffer is shared with the paired reader, which sends it before it
    // reads, usually from another thread than the writer's (a server reading
    // the next request while a worker writes the reply). Every touch of the
    // buffer holds this lock; without it the two copied into and sent the
    // same bytes at once and the output came out garbled.
    private readonly object _lock = new();

    public BivalentStreamWriter(System.IO.Stream s) => _s = s;
    public System.IO.Stream BaseStream => _s;
    public override System.Text.Encoding Encoding => Utf8NoBom;
    internal bool HasPending => System.Threading.Volatile.Read(ref _len) > 0;

    private void Put(byte[] bytes, int offset, int count)
    {
        lock (_lock)
        {
            if (count > _buf.Length - _len)
            {
                FlushBuffer();
                if (count > _buf.Length) { _s.Write(bytes, offset, count); return; }
            }
            System.Buffer.BlockCopy(bytes, offset, _buf, _len, count);
            _len += count;
        }
    }

    // Callers hold _lock.
    private void FlushBuffer()
    {
        if (_len == 0) return;
        int n = _len;
        _len = 0;
        _s.Write(_buf, 0, n);
    }

    public override void Write(char c)
    {
        var bytes = Utf8NoBom.GetBytes(new[] { c });
        Put(bytes, 0, bytes.Length);
    }
    public override void Write(string? value)
    {
        if (string.IsNullOrEmpty(value)) return;
        var bytes = Utf8NoBom.GetBytes(value);
        Put(bytes, 0, bytes.Length);
    }
    /// <summary>Raw byte write (write-byte).</summary>
    public void WriteRawByte(int b)
    {
        lock (_lock)
        {
            if (_len == _buf.Length) FlushBuffer();
            _buf[_len++] = (byte)b;
        }
    }
    /// <summary>Raw octets (write-sequence of a byte vector).</summary>
    public void WriteRawBytes(byte[] buffer, int offset, int count) => Put(buffer, offset, count);
    public override void Flush()
    {
        lock (_lock)
        {
            FlushBuffer();
            _s.Flush();
        }
    }

    // See BivalentStreamReader.Dispose.
    protected override void Dispose(bool disposing)
    {
        if (disposing)
        {
            try { Flush(); } catch (System.IO.IOException) { } catch (System.ObjectDisposedException) { }
            _s.Dispose();
        }
        base.Dispose(disposing);
    }
}
