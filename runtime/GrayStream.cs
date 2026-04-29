using System.Text;
using DotCL.Emitter;

namespace DotCL;

internal static class GrayStreamLookup
{
    /// <summary>Prefer the DOTCL-GRAY:NAME generic function, fall back to CL:NAME
    /// (or wherever Startup.Sym resolves). Used by the Gray stream bridges.</summary>
    public static LispFunction? GrayOrCl(string name)
    {
        var grayPkg = Package.FindPackage("DOTCL-GRAY");
        if (grayPkg != null)
        {
            var (gsym, gstatus) = grayPkg.FindSymbol(name);
            if (gstatus != SymbolStatus.None && gsym.Function is LispFunction gfn)
                return gfn;
        }
        return Startup.Sym(name).Function as LispFunction;
    }
}

/// <summary>
/// TextWriter bridge for Gray output streams (CLOS instances inheriting
/// from fundamental-character-output-stream). Dispatches Write(char) to
/// the stream-write-char generic function.
/// </summary>
public class GrayStreamTextWriter : TextWriter
{
    private readonly LispInstance _stream;
    private LispFunction? _writeCharFn;
    private LispFunction? _writeStringFn;
    private LispFunction? _forceOutputFn;

    public GrayStreamTextWriter(LispInstance stream) => _stream = stream;

    public override Encoding Encoding => Encoding.UTF8;

    private LispFunction GetWriteCharFn()
    {
        if (_writeCharFn != null) return _writeCharFn;
        _writeCharFn = GrayStreamLookup.GrayOrCl("STREAM-WRITE-CHAR");
        if (_writeCharFn == null)
            throw new LispErrorException(new LispError("Gray stream: STREAM-WRITE-CHAR not defined"));
        return _writeCharFn;
    }

    private LispFunction? GetWriteStringFn()
    {
        if (_writeStringFn != null) return _writeStringFn;
        _writeStringFn = GrayStreamLookup.GrayOrCl("STREAM-WRITE-STRING");
        return _writeStringFn;
    }

    public override void Write(char value)
    {
        GetWriteCharFn().Invoke(new LispObject[] { _stream, LispChar.Make(value) });
    }

    public override void Write(string? value)
    {
        if (value == null) return;
        var fn = GetWriteStringFn();
        if (fn != null)
        {
            fn.Invoke(new LispObject[] { _stream, new LispString(value) });
        }
        else
        {
            foreach (char c in value) Write(c);
        }
    }

    public override void Flush()
    {
        if (_forceOutputFn == null)
        {
            _forceOutputFn = GrayStreamLookup.GrayOrCl("STREAM-FORCE-OUTPUT");
        }
        _forceOutputFn?.Invoke(new LispObject[] { _stream });
    }
}

/// <summary>
/// TextReader bridge for Gray input streams (CLOS instances inheriting
/// from fundamental-character-input-stream). Dispatches Read() to
/// the stream-read-char generic function.
/// </summary>
public class GrayStreamTextReader : TextReader
{
    private readonly LispInstance _stream;
    private LispFunction? _readCharFn;
    private LispFunction? _peekCharFn;

    public GrayStreamTextReader(LispInstance stream) => _stream = stream;

    private LispFunction GetReadCharFn()
    {
        if (_readCharFn != null) return _readCharFn;
        _readCharFn = GrayStreamLookup.GrayOrCl("STREAM-READ-CHAR");
        if (_readCharFn == null)
            throw new LispErrorException(new LispError("Gray stream: STREAM-READ-CHAR not defined"));
        return _readCharFn;
    }

    public override int Read()
    {
        var result = GetReadCharFn().Invoke(new LispObject[] { _stream });
        if (result is LispChar lc) return lc.Value;
        if (result is Symbol s && s.Name == "EOF") return -1;
        return -1;
    }

    public override int Peek()
    {
        if (_peekCharFn == null)
        {
            _peekCharFn = GrayStreamLookup.GrayOrCl("STREAM-PEEK-CHAR");
        }
        if (_peekCharFn != null)
        {
            var result = _peekCharFn.Invoke(new LispObject[] { _stream });
            if (result is LispChar lc) return lc.Value;
            return -1;
        }
        return -1;
    }
}
