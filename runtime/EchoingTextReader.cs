using System.IO;

namespace DotCL;

/// <summary>
/// A TextReader that copies every character it actually reads to a second
/// writer — what an ECHO-STREAM has to do (CLHS 21.1.3).
///
/// The Lisp reader consumes a stream through a TextReader, and it got that
/// reader from underneath the echo-stream wrapper, so READ and
/// READ-PRESERVING-WHITESPACE echoed nothing at all while READ-CHAR and
/// READ-LINE (which handle the echo themselves) echoed correctly.
///
/// Only Read echoes. Peek does not: peeking is not reading, and the Lisp
/// reader peeks constantly to decide whether a character terminates a token.
/// A character the reader pushes back after reading it stays echoed, which is
/// what SBCL does too — (read-preserving-whitespace) on "abc def" echoes
/// "abc " there, the trailing space included, even though the space is still
/// available to the next read.
/// </summary>
internal sealed class EchoingTextReader : TextReader
{
    private readonly TextReader _inner;
    private readonly TextWriter _echo;

    internal EchoingTextReader(TextReader inner, TextWriter echo)
    {
        _inner = inner;
        _echo = echo;
    }

    public override int Peek() => _inner.Peek();

    public override int Read()
    {
        int c = _inner.Read();
        if (c != -1) _echo.Write((char)c);
        return c;
    }

    public override int Read(char[] buffer, int index, int count)
    {
        int n = _inner.Read(buffer, index, count);
        if (n > 0) _echo.Write(buffer, index, n);
        return n;
    }
}
