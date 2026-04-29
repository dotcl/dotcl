namespace DotCL;

using System.Runtime.InteropServices;

/// <summary>
/// Native memory operations for cffi-sys backend.
/// Exposed as dotnet:alloc-mem, dotnet:free-mem, dotnet:mem-read, dotnet:mem-write, etc.
/// </summary>
public static partial class Runtime
{
    // (dotnet:alloc-mem size) -> integer address
    public static LispObject AllocMem(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:alloc-mem", 1, args.Length);
        var size = (int)((Fixnum)args[0]).Value;
        return Fixnum.Make(Marshal.AllocHGlobal(size).ToInt64());
    }

    // (dotnet:free-mem addr)
    public static LispObject FreeMem(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:free-mem", 1, args.Length);
        Marshal.FreeHGlobal(new IntPtr(((Fixnum)args[0]).Value));
        return Nil.Instance;
    }

    static string CffiTypeName(LispObject kw)
    {
        var raw = kw is Symbol s ? s.Name :
                  kw is LispString ls ? ls.Value :
                  kw.ToString() ?? "";
        return raw.TrimStart(':').ToUpperInvariant().Replace("-", "_");
    }

    // (dotnet:mem-read type addr &optional (offset 0)) -> value
    public static LispObject MemRead(LispObject[] args)
    {
        if (args.Length < 2) throw ArgError("dotnet:mem-read", 2, args.Length);
        var typeName = CffiTypeName(args[0]);
        var addr = ((Fixnum)args[1]).Value;
        var offset = args.Length > 2 ? ((Fixnum)args[2]).Value : 0;
        var ptr = new IntPtr(addr + offset);
        return typeName switch
        {
            "CHAR" or "INT8" =>
                Fixnum.Make((sbyte)Marshal.ReadByte(ptr)),
            "UNSIGNED_CHAR" or "UINT8" =>
                Fixnum.Make(Marshal.ReadByte(ptr)),
            "SHORT" or "INT16" =>
                Fixnum.Make(Marshal.ReadInt16(ptr)),
            "UNSIGNED_SHORT" or "UINT16" =>
                Fixnum.Make((ushort)Marshal.ReadInt16(ptr)),
            "INT" or "INT32" or "LONG" or "BOOL" or "BOOLEAN" =>
                Fixnum.Make(Marshal.ReadInt32(ptr)),
            "UNSIGNED_INT" or "UINT32" or "UNSIGNED_LONG" =>
                Fixnum.Make((uint)Marshal.ReadInt32(ptr)),
            "LONG_LONG" or "INT64" =>
                Fixnum.Make(Marshal.ReadInt64(ptr)),
            "UNSIGNED_LONG_LONG" or "UINT64" =>
                Fixnum.Make(Marshal.ReadInt64(ptr)),
            "FLOAT" =>
                new DoubleFloat(BitConverter.ToSingle(BitConverter.GetBytes(Marshal.ReadInt32(ptr)))),
            "DOUBLE" =>
                new DoubleFloat(BitConverter.ToDouble(BitConverter.GetBytes(Marshal.ReadInt64(ptr)))),
            "POINTER" or "PTR" =>
                Fixnum.Make(Marshal.ReadIntPtr(ptr).ToInt64()),
            _ => throw new LispErrorException(new LispError($"dotnet:mem-read: unknown type {typeName}"))
        };
    }

    // (dotnet:mem-write value type addr &optional (offset 0))
    public static LispObject MemWrite(LispObject[] args)
    {
        if (args.Length < 3) throw ArgError("dotnet:mem-write", 3, args.Length);
        var value = args[0];
        var typeName = CffiTypeName(args[1]);
        var addr = ((Fixnum)args[2]).Value;
        var offset = args.Length > 3 ? ((Fixnum)args[3]).Value : 0;
        var ptr = new IntPtr(addr + offset);
        switch (typeName)
        {
            case "CHAR": case "INT8":
            case "UNSIGNED_CHAR": case "UINT8":
                Marshal.WriteByte(ptr, (byte)((Fixnum)value).Value); break;
            case "SHORT": case "INT16":
            case "UNSIGNED_SHORT": case "UINT16":
                Marshal.WriteInt16(ptr, (short)((Fixnum)value).Value); break;
            case "INT": case "INT32": case "LONG": case "BOOL": case "BOOLEAN":
            case "UNSIGNED_INT": case "UINT32": case "UNSIGNED_LONG":
                Marshal.WriteInt32(ptr, (int)((Fixnum)value).Value); break;
            case "LONG_LONG": case "INT64":
            case "UNSIGNED_LONG_LONG": case "UINT64":
                Marshal.WriteInt64(ptr, ((Fixnum)value).Value); break;
            case "FLOAT":
                var fv = value is DoubleFloat df2 ? (float)df2.Value :
                         value is SingleFloat sf2 ? sf2.Value :
                         (float)((Fixnum)value).Value;
                Marshal.WriteInt32(ptr, BitConverter.ToInt32(BitConverter.GetBytes(fv)));
                break;
            case "DOUBLE":
                var dv = value is DoubleFloat ddf ? ddf.Value :
                         value is SingleFloat sdf ? (double)sdf.Value :
                         (double)((Fixnum)value).Value;
                Marshal.WriteInt64(ptr, BitConverter.ToInt64(BitConverter.GetBytes(dv)));
                break;
            case "POINTER": case "PTR":
                Marshal.WriteIntPtr(ptr, new IntPtr(((Fixnum)value).Value)); break;
            default:
                throw new LispErrorException(new LispError($"dotnet:mem-write: unknown type {typeName}"));
        }
        return value;
    }

    static readonly Dictionary<string, int> _typeSizes = new()
    {
        ["CHAR"] = 1, ["INT8"] = 1,
        ["UNSIGNED_CHAR"] = 1, ["UINT8"] = 1,
        ["SHORT"] = 2, ["INT16"] = 2,
        ["UNSIGNED_SHORT"] = 2, ["UINT16"] = 2,
        ["INT"] = 4, ["INT32"] = 4,
        ["UNSIGNED_INT"] = 4, ["UINT32"] = 4,
        // Windows: long = 32-bit even on x64
        ["LONG"] = 4, ["UNSIGNED_LONG"] = 4,
        ["LONG_LONG"] = 8, ["INT64"] = 8,
        ["UNSIGNED_LONG_LONG"] = 8, ["UINT64"] = 8,
        ["FLOAT"] = 4, ["DOUBLE"] = 8,
        // pointer = 8 on x64
        ["POINTER"] = IntPtr.Size, ["PTR"] = IntPtr.Size,
        ["VOID"] = 0,
        ["BOOL"] = 4, ["BOOLEAN"] = 4,
    };

    // (dotnet:type-size type-keyword) -> integer
    public static LispObject TypeSize(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:type-size", 1, args.Length);
        var name = CffiTypeName(args[0]);
        if (_typeSizes.TryGetValue(name, out int sz)) return Fixnum.Make(sz);
        throw new LispErrorException(new LispError($"dotnet:type-size: unknown type {name}"));
    }

    // (dotnet:type-align type-keyword) -> integer
    // On x64 Windows, alignment equals size for primitive types (max 8).
    public static LispObject TypeAlign(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:type-align", 1, args.Length);
        var name = CffiTypeName(args[0]);
        if (_typeSizes.TryGetValue(name, out int sz))
            return Fixnum.Make(sz == 0 ? 1 : sz);
        throw new LispErrorException(new LispError($"dotnet:type-align: unknown type {name}"));
    }

    static readonly Dictionary<string, IntPtr> _cffiLibHandles = new();
    static readonly Dictionary<IntPtr, string> _cffiHandleToPaths = new();

    // (dotnet:load-library path) -> handle (integer)
    public static LispObject LoadLibrary(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:load-library", 1, args.Length);
        var path = args[0] is LispString ls ? ls.Value :
                   args[0] is LispVector lv && lv.IsCharVector ? lv.ToCharString() :
                   throw new LispErrorException(new LispError("dotnet:load-library: path must be a string"));
        lock (_cffiLibHandles)
        {
            if (_cffiLibHandles.TryGetValue(path, out var h)) return Fixnum.Make(h.ToInt64());
            h = NativeLibrary.Load(path);
            _cffiLibHandles[path] = h;
            _cffiHandleToPaths[h] = path;
            return Fixnum.Make(h.ToInt64());
        }
    }

    // (dotnet:free-library handle)
    public static LispObject FreeLibrary(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:free-library", 1, args.Length);
        var h = new IntPtr(((Fixnum)args[0]).Value);
        lock (_cffiLibHandles)
        {
            if (_cffiHandleToPaths.TryGetValue(h, out var path))
            {
                _cffiLibHandles.Remove(path);
                _cffiHandleToPaths.Remove(h);
            }
        }
        NativeLibrary.Free(h);
        return Nil.Instance;
    }

    // (dotnet:find-symbol func-name handle) -> address or NIL
    public static LispObject FindSymbolInLib(LispObject[] args)
    {
        if (args.Length < 2) throw ArgError("dotnet:find-symbol", 2, args.Length);
        var name = args[0] is LispString ls ? ls.Value :
                   args[0] is LispVector lv && lv.IsCharVector ? lv.ToCharString() :
                   throw new LispErrorException(new LispError("dotnet:find-symbol: name must be a string"));
        var handle = new IntPtr(((Fixnum)args[1]).Value);
        if (NativeLibrary.TryGetExport(handle, name, out var ptr))
            return Fixnum.Make(ptr.ToInt64());
        return Nil.Instance;
    }

    // (dotnet:find-symbol-any func-name) -> address or NIL — searches all loaded libraries
    public static LispObject FindSymbolAny(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:find-symbol-any", 1, args.Length);
        var name = args[0] is LispString ls ? ls.Value :
                   args[0] is LispVector lv && lv.IsCharVector ? lv.ToCharString() :
                   throw new LispErrorException(new LispError("dotnet:find-symbol-any: name must be a string"));
        lock (_cffiLibHandles)
        {
            foreach (var h in _cffiLibHandles.Values)
            {
                if (NativeLibrary.TryGetExport(h, name, out var ptr))
                    return Fixnum.Make(ptr.ToInt64());
            }
        }
        return Nil.Instance;
    }

    // (dotnet:library-path handle) -> string path or NIL
    public static LispObject LibraryPath(LispObject[] args)
    {
        if (args.Length < 1) throw ArgError("dotnet:library-path", 1, args.Length);
        var h = new IntPtr(((Fixnum)args[0]).Value);
        lock (_cffiLibHandles)
        {
            if (_cffiHandleToPaths.TryGetValue(h, out var path))
                return new LispString(path);
        }
        return Nil.Instance;
    }

    // (dotnet:%ffi-call-ptr func-ptr arg-types ret-type &rest args)
    // Like %ffi-call but takes an IntPtr function pointer instead of dll+func name.
    public static LispObject FfiCallPtr(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "dotnet:%ffi-call-ptr: requires at least func-ptr, arg-types, ret-type"));
        var funcPtr = new IntPtr(((Fixnum)args[0]).Value);
        var argTypesList = args[1];
        var retType = args[2];
        var nativeArgs = args.Skip(3).ToArray();
        return NativeFFI.CallPtr(funcPtr, argTypesList, retType, nativeArgs);
    }

    static LispErrorException ArgError(string fn, int expected, int got) =>
        new(new LispProgramError($"{fn}: expected {expected} args, got {got}"));
}
