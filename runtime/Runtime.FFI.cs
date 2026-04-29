namespace DotCL;

using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;

/// <summary>
/// Dynamic native FFI via NativeLibrary + DynamicMethod + calli.
/// Supports (dotcl::%ffi-call dll func arg-types ret-type &rest args).
/// </summary>
static class NativeFFI
{
    // Cache: (dll, func, sigKey) -> DynamicMethod
    static readonly Dictionary<(string, string, string), DynamicMethod> _methodCache = new();
    // Cache: dll name -> library handle
    static readonly Dictionary<string, IntPtr> _libCache = new();

    static IntPtr LoadLib(string dll)
    {
        lock (_libCache)
        {
            if (_libCache.TryGetValue(dll, out var h)) return h;
            h = NativeLibrary.Load(dll);
            _libCache[dll] = h;
            return h;
        }
    }

    /// Map a Lisp keyword/symbol to a .NET type.
    static Type KeyToType(LispObject key)
    {
        var name = (key is Symbol sym ? sym.Name :
                    key is LispString s ? s.Value :
                    key.ToString() ?? "").ToUpperInvariant();
        return name switch
        {
            "POINTER" or "PTR" or ":POINTER" or ":PTR" => typeof(IntPtr),
            "INT" or "INT32" or ":INT" or ":INT32" => typeof(int),
            "UINT" or "UINT32" or ":UINT" or ":UINT32" => typeof(uint),
            "INT8" or ":INT8" => typeof(sbyte),
            "UINT8" or ":UINT8" or "CHAR" or ":CHAR" => typeof(byte),
            "INT16" or "SHORT" or ":INT16" or ":SHORT" => typeof(short),
            "UINT16" or "USHORT" or ":UINT16" or ":USHORT" => typeof(ushort),
            "INT64" or "LONG" or ":INT64" or ":LONG" => typeof(long),
            "UINT64" or "ULONG" or ":UINT64" or ":ULONG" => typeof(ulong),
            "FLOAT" or ":FLOAT" => typeof(float),
            "DOUBLE" or ":DOUBLE" => typeof(double),
            "BOOL" or "BOOLEAN" or ":BOOL" or ":BOOLEAN" => typeof(int), // Win32 BOOL = int
            "VOID" or ":VOID" => typeof(void),
            "STRING" or ":STRING" or "LPCSTR" or ":LPCSTR" => typeof(IntPtr), // manual marshaling
            _ => throw new LispErrorException(new LispError($"dotnet:ffi: unknown type keyword: {name}"))
        };
    }

    static bool IsVoidType(Type t) => t == typeof(void);

    static object? ConvertArg(LispObject arg, Type targetType)
    {
        if (targetType == typeof(IntPtr))
        {
            if (arg is Fixnum fx) return new IntPtr(fx.Value);
            if (arg is LispDotNetObject dno && dno.Value is IntPtr ip) return ip;
            if (arg is Nil) return IntPtr.Zero;
            if (arg is LispString ls)
                return Marshal.StringToHGlobalAnsi(ls.Value);
            if (arg is LispVector v && v.IsCharVector)
                return Marshal.StringToHGlobalAnsi(v.ToCharString());
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :ptr"));
        }
        if (targetType == typeof(bool) || targetType == typeof(int))
        {
            if (arg is Fixnum fx) return (int)fx.Value;
            if (arg is Nil) return 0;
            if (arg is T) return 1;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :int32/:bool"));
        }
        if (targetType == typeof(uint))
        {
            if (arg is Fixnum fx) return (uint)fx.Value;
            if (arg is Nil) return 0u;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :uint32"));
        }
        if (targetType == typeof(long))
        {
            if (arg is Fixnum fx) return fx.Value;
            if (arg is Nil) return 0L;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :int64"));
        }
        if (targetType == typeof(ulong))
        {
            if (arg is Fixnum fx) return (ulong)fx.Value;
            if (arg is Nil) return 0UL;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :uint64"));
        }
        if (targetType == typeof(float))
        {
            if (arg is DoubleFloat df) return (float)df.Value;
            if (arg is SingleFloat sf) return sf.Value;
            if (arg is Fixnum fx) return (float)fx.Value;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :float"));
        }
        if (targetType == typeof(double))
        {
            if (arg is DoubleFloat df) return df.Value;
            if (arg is SingleFloat sf) return (double)sf.Value;
            if (arg is Fixnum fx) return (double)fx.Value;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :double"));
        }
        if (targetType == typeof(short))
        {
            if (arg is Fixnum fx) return (short)fx.Value;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :int16"));
        }
        if (targetType == typeof(ushort))
        {
            if (arg is Fixnum fx) return (ushort)fx.Value;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :uint16"));
        }
        if (targetType == typeof(sbyte))
        {
            if (arg is Fixnum fx) return (sbyte)fx.Value;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :int8"));
        }
        if (targetType == typeof(byte))
        {
            if (arg is Fixnum fx) return (byte)fx.Value;
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :uint8"));
        }
        throw new LispErrorException(new LispError(
            $"dotnet:ffi: unsupported target type {targetType.Name}"));
    }

    static LispObject ConvertReturn(object? result, Type retType)
    {
        if (IsVoidType(retType) || result == null) return Nil.Instance;
        return result switch
        {
            IntPtr ip => Fixnum.Make(ip.ToInt64()),
            int i => Fixnum.Make(i),
            uint u => Fixnum.Make((long)u),
            long l => Fixnum.Make(l),
            ulong ul => Fixnum.Make((long)ul),
            short s => Fixnum.Make(s),
            ushort us => Fixnum.Make(us),
            sbyte sb => Fixnum.Make(sb),
            byte b => Fixnum.Make(b),
            float f => new DoubleFloat(f),
            double d => new DoubleFloat(d),
            bool bv => bv ? T.Instance : Nil.Instance,
            _ => Runtime.DotNetToLisp(result)
        };
    }

    public static LispObject Call(
        string dll, string func,
        LispObject argTypesList, LispObject retTypeKw,
        LispObject[] nativeArgs)
    {
        // Parse arg types
        var argTypes = new List<Type>();
        for (var cur = argTypesList; cur is Cons c; cur = c.Cdr)
            argTypes.Add(KeyToType(c.Car));

        var retType = retTypeKw is Nil ? typeof(void) : KeyToType(retTypeKw);

        if (nativeArgs.Length != argTypes.Count)
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: {func} expects {argTypes.Count} args, got {nativeArgs.Length}"));

        // Build or reuse DynamicMethod
        var sigKey = string.Join(",", argTypes.Select(t => t.Name)) + "→" + retType.Name;
        var cacheKey = (dll, func, sigKey);
        DynamicMethod dm;
        lock (_methodCache)
        {
            if (!_methodCache.TryGetValue(cacheKey, out dm!))
            {
                // DynamicMethod params: (IntPtr funcPtr, arg0Type, arg1Type, ...)
                var allParams = argTypes.Prepend(typeof(IntPtr)).ToArray();
                var actualRet = IsVoidType(retType) ? null : retType;
                dm = new DynamicMethod($"ffi_{func}_{sigKey}", actualRet, allParams,
                                       typeof(NativeFFI), skipVisibility: true);
                var il = dm.GetILGenerator();
                // Push native args (positions 1..n)
                for (int i = 0; i < argTypes.Count; i++)
                    il.Emit(OpCodes.Ldarg, i + 1);
                // Push funcPtr (position 0) — must be last before calli
                il.Emit(OpCodes.Ldarg_0);
                // calli: pops funcPtr last, args before it
                il.EmitCalli(OpCodes.Calli, CallingConvention.StdCall,
                             actualRet, argTypes.ToArray());
                il.Emit(OpCodes.Ret);
                _methodCache[cacheKey] = dm;
            }
        }

        // Get function pointer
        var libHandle = LoadLib(dll);
        var funcPtr = NativeLibrary.GetExport(libHandle, func);

        // Build invoke args: [funcPtr, nativeArg0, nativeArg1, ...]
        var invokeArgs = new object?[nativeArgs.Length + 1];
        invokeArgs[0] = funcPtr;
        for (int i = 0; i < nativeArgs.Length; i++)
            invokeArgs[i + 1] = ConvertArg(nativeArgs[i], argTypes[i]);

        var result = dm.Invoke(null, invokeArgs);
        return ConvertReturn(result, retType);
    }

    /// <summary>
    /// Call a native function by pointer (no DLL/symbol lookup).
    /// Reuses the DynamicMethod infrastructure from Call().
    /// </summary>
    public static LispObject CallPtr(
        IntPtr funcPtr,
        LispObject argTypesList, LispObject retTypeKw,
        LispObject[] nativeArgs)
    {
        var argTypes = new List<Type>();
        for (var cur = argTypesList; cur is Cons c; cur = c.Cdr)
            argTypes.Add(KeyToType(c.Car));

        var retType = retTypeKw is Nil ? typeof(void) : KeyToType(retTypeKw);

        if (nativeArgs.Length != argTypes.Count)
            throw new LispErrorException(new LispError(
                $"dotnet:%ffi-call-ptr: expects {argTypes.Count} args, got {nativeArgs.Length}"));

        var sigKey = string.Join(",", argTypes.Select(t => t.Name)) + "→" + retType.Name;
        var cacheKey = ("*ptr*", "*ptr*", sigKey);
        DynamicMethod dm;
        lock (_methodCache)
        {
            if (!_methodCache.TryGetValue(cacheKey, out dm!))
            {
                var allParams = argTypes.Prepend(typeof(IntPtr)).ToArray();
                var actualRet = IsVoidType(retType) ? null : retType;
                dm = new DynamicMethod($"ffi_ptr_{sigKey}", actualRet, allParams,
                                       typeof(NativeFFI), skipVisibility: true);
                var il = dm.GetILGenerator();
                for (int i = 0; i < argTypes.Count; i++)
                    il.Emit(OpCodes.Ldarg, i + 1);
                il.Emit(OpCodes.Ldarg_0);
                il.EmitCalli(OpCodes.Calli, CallingConvention.StdCall,
                             actualRet, argTypes.ToArray());
                il.Emit(OpCodes.Ret);
                _methodCache[cacheKey] = dm;
            }
        }

        var invokeArgs = new object?[nativeArgs.Length + 1];
        invokeArgs[0] = funcPtr;
        for (int i = 0; i < nativeArgs.Length; i++)
            invokeArgs[i + 1] = ConvertArg(nativeArgs[i], argTypes[i]);

        var result = dm.Invoke(null, invokeArgs);
        return ConvertReturn(result, retType);
    }
}

public static partial class Runtime
{
    /// <summary>
    /// (dotnet:ffi dll func :args '(type ...) :ret type arg1 arg2 ...)
    /// Keyword-arg wrapper. Parses :args and :ret keywords then delegates to FfiCall.
    /// </summary>
    public static LispObject FfiCallKeyword(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "dotnet:ffi: requires at least dll and func arguments"));

        var dll = args[0] is LispString ds ? ds.Value :
                  args[0] is LispVector dv && dv.IsCharVector ? dv.ToCharString() :
                  throw new LispErrorException(new LispTypeError(
                      "dotnet:ffi: dll must be a string", args[0],
                      Startup.Sym("DOTNET:FFI")));

        var func = args[1] is LispString fs ? fs.Value :
                   args[1] is LispVector fv && fv.IsCharVector ? fv.ToCharString() :
                   throw new LispErrorException(new LispTypeError(
                       "dotnet:ffi: func must be a string", args[1],
                       Startup.Sym("DOTNET:FFI")));

        LispObject argTypes = Nil.Instance;
        LispObject retType = Nil.Instance;
        int i = 2;
        while (i + 1 < args.Length && args[i] is Symbol kw && kw.HomePackage == Startup.KeywordPkg)
        {
            switch (kw.Name)
            {
                case "ARGS": argTypes = args[i + 1]; i += 2; break;
                case "RET":  retType  = args[i + 1]; i += 2; break;
                default: i += 2; break;
            }
        }

        var nativeArgs = args.Skip(i).ToArray();
        return NativeFFI.Call(dll, func, argTypes, retType, nativeArgs);
    }

    /// <summary>
    /// (dotcl::%ffi-call dll func arg-types ret-type &rest args)
    /// Low-level native FFI call. dll and func are strings; arg-types is a list
    /// of type keywords; ret-type is a keyword or NIL for void; args are the values.
    /// </summary>
    public static LispObject FfiCall(LispObject[] args)
    {
        if (args.Length < 4)
            throw new LispErrorException(new LispProgramError(
                $"dotcl::%ffi-call: requires at least 4 arguments (dll func arg-types ret-type &rest)"));

        var dll = args[0] is LispString ds ? ds.Value :
                  args[0] is LispVector dv && dv.IsCharVector ? dv.ToCharString() :
                  throw new LispErrorException(new LispTypeError(
                      "dotcl::%ffi-call: dll must be a string", args[0],
                      Startup.Sym("DOTCL::%FFI-CALL")));

        var func = args[1] is LispString fs ? fs.Value :
                   args[1] is LispVector fv && fv.IsCharVector ? fv.ToCharString() :
                   throw new LispErrorException(new LispTypeError(
                       "dotcl::%ffi-call: func must be a string", args[1],
                       Startup.Sym("DOTCL::%FFI-CALL")));

        var argTypes = args[2];
        var retType = args[3];
        var nativeArgs = args.Skip(4).ToArray();

        return NativeFFI.Call(dll, func, argTypes, retType, nativeArgs);
    }
}
