using System.IO;

namespace DotCL;

/// <summary>
/// A TextReader that copies every character it actually reads to a second
/// writer: what an ECHO-STREAM has to do (CLHS 21.1.3).
///
/// The Lisp reader consumes a stream through a TextReader, and it got that
/// reader from underneath the echo-stream wrapper, so READ and
/// READ-PRESERVING-WHITESPACE echoed nothing at all while READ-CHAR and
/// READ-LINE (which handle the echo themselves) echoed correctly.
///
/// Only Read echoes. Peek does not: peeking is not reading, and the Lisp
/// reader peeks constantly to decide whether a character terminates a token.
/// A character the reader pushes back after reading it stays echoed, which is
/// what SBCL does too: (read-preserving-whitespace) on "abc def" echoes
/// "abc " there, the trailing space included, even though the space is still
/// available to the next read.
/// </summary>
internal sealed class EchoingTextReader : TextReader
{
    private readonly TextReader _inner;
    private TextWriter? _echo;
    private readonly LispObject? _echoStream;

    internal EchoingTextReader(TextReader inner, TextWriter echo)
    {
        _inner = inner;
        _echo = echo;
    }

    /// <summary>The echo stream's output is resolved on the first character echoed, not
    /// when the reader is made: PEEK-CHAR makes a reader and echoes nothing, so a closed
    /// output stream must not make it fail.</summary>
    internal EchoingTextReader(TextReader inner, LispObject echoOutputStream)
    {
        _inner = inner;
        _echoStream = echoOutputStream;
    }

    internal TextReader Inner => _inner;

    private LispStream? _plainOut;   // the output when it is a plain stream, so nothing needs resolving per character

    private void Echo(char c)
    {
        if (_echo != null) { _echo.Write(c); return; }
        var plain = _plainOut;
        if (plain != null && !plain.IsClosed)
        {
            // A string or file output stream: no wrapper to look through, no synonym that
            // could be rebound, and a closed stream goes the slow way to signal.
            _plainEchoWriter!.Write(c);
            plain.AtLineStart = c == '\n';
            return;
        }
        Runtime.GetTextWriter(_echoStream!).Write(c);
        Runtime.UpdateAtLineStart(_echoStream!, c);
        if (_echoStream is LispOutputStream or LispFileStream && _echoStream is LispStream ls)
        {
            _plainOut = ls;
            _plainEchoWriter = Runtime.GetTextWriter(_echoStream);
        }
    }

    private TextWriter? _plainEchoWriter;

    public override int Peek() => _inner.Peek();

    public override int Read()
    {
        int c = _inner.Read();
        if (c != -1) Echo((char)c);
        return c;
    }

    public override int Read(char[] buffer, int index, int count)
    {
        int n = _inner.Read(buffer, index, count);
        for (int i = 0; i < n; i++) Echo(buffer[index + i]);
        return n;
    }
}
