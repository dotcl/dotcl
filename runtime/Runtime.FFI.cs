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
            if (arg is Nil) return 0L;
            if (arg is Fixnum or Bignum) return Runtime.ToLong(arg, "dotnet:ffi");
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :int64"));
        }
        if (targetType == typeof(ulong))
        {
            if (arg is Nil) return 0UL;
            // 2^63..2^64-1 arrive as Bignum
            if (arg is Fixnum or Bignum) return Runtime.ToULong(arg, "dotnet:ffi");
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :uint64"));
        }
        // Any real converts, not just the float types and fixnums: a ratio or a
        // bignum is an ordinary result of Lisp arithmetic ((/ 1 3), (expt 2 70)),
        // and refusing it made the caller coerce by hand for no reason.
        if (targetType == typeof(float))
        {
            if (arg is DoubleFloat df) return (float)df.Value;
            if (arg is SingleFloat sf) return sf.Value;
            if (arg is Fixnum fx) return (float)fx.Value;
            if (arg is Bignum or Ratio) return (float)Arithmetic.ToDouble((Number)arg);
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: cannot convert {arg} to :float"));
        }
        if (targetType == typeof(double))
        {
            if (arg is DoubleFloat df) return df.Value;
            if (arg is SingleFloat sf) return (double)sf.Value;
            if (arg is Fixnum fx) return (double)fx.Value;
            if (arg is Bignum or Ratio) return Arithmetic.ToDouble((Number)arg);
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
            ulong ul => Runtime.MakeUnsigned64(ul),
            short s => Fixnum.Make(s),
            ushort us => Fixnum.Make(us),
            sbyte sb => Fixnum.Make(sb),
            byte b => Fixnum.Make(b),
            float f => new SingleFloat(f),   // binary32 — see DotNetToLisp
            double d => new DoubleFloat(d),
            bool bv => bv ? T.Instance : Nil.Instance,
            _ => Runtime.DotNetToLisp(result)
        };
    }

    /// <summary>
    /// Parse an :args list into CLR types, honouring a :VARARGS marker that says
    /// where the variadic part begins -- what libffi needs ffi_prep_cif_var for.
    /// Without it every argument looked fixed, and a variadic float never
    /// arrived: on ARM64 the fixed and the variadic calling conventions put
    /// floats in different places.
    /// </summary>
    static (List<Type> Types, int FixedCount) ParseArgTypes(LispObject argTypesList)
    {
        var types = new List<Type>();
        int fixedCount = -1;
        for (var cur = argTypesList; cur is Cons c; cur = c.Cdr)
        {
            if (IsVarargsMarker(c.Car))
            {
                if (fixedCount >= 0)
                    throw new LispErrorException(new LispError(
                        "dotnet:ffi: :varargs may appear only once in the argument type list"));
                fixedCount = types.Count;
                continue;
            }
            types.Add(KeyToType(c.Car));
        }
        return (types, fixedCount < 0 ? types.Count : fixedCount);
    }

    static bool IsVarargsMarker(LispObject o)
    {
        var name = o is Symbol s ? s.Name : o is LispString ls ? ls.Value : null;
        if (name == null) return false;
        name = name.TrimStart(':').ToUpperInvariant();
        return name == "VARARGS" || name == "...";
    }

    /// <summary>
    /// The CLR type to give an argument in the variadic part.
    ///
    /// C promotes a variadic float to double everywhere. On ARM64 Windows the
    /// variadic part additionally travels in the general-purpose registers and on
    /// the stack -- never in v0-v7 -- so a double declared as such is written
    /// where the callee never looks, and sprintf("%.0f", x) printed 0. Declaring
    /// it as a 64-bit integer (and passing the double's bit pattern) puts it
    /// exactly where the variadic callee reads from.
    ///
    /// Not applied elsewhere: x64 passes variadic floats in the SSE registers the
    /// ordinary convention already uses, and ARM64 macOS puts the whole variadic
    /// part on the stack, which this trick would not reproduce.
    /// </summary>
    static bool VariadicFloatsAsIntegerBits =>
        Compat.IsWindows()
        && System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture
           == System.Runtime.InteropServices.Architecture.Arm64;

    static Type VariadicCallType(Type t)
    {
        if (t == typeof(float) || t == typeof(double))
            return VariadicFloatsAsIntegerBits ? typeof(long) : typeof(double);
        return t;
    }

    /// <summary>The value for a variadic argument declared as T: a float is
    /// promoted to double, and passed as its bits where the ABI wants it in an
    /// integer register.</summary>
    static object? ConvertVariadicArg(LispObject arg, Type declared)
    {
        if (declared == typeof(float) || declared == typeof(double))
        {
            var d = (double)ConvertArg(arg, typeof(double))!;
            return VariadicFloatsAsIntegerBits ? BitConverter.DoubleToInt64Bits(d) : (object)d;
        }
        return ConvertArg(arg, declared);
    }

    public static LispObject Call(
        string dll, string func,
        LispObject argTypesList, LispObject retTypeKw,
        LispObject[] nativeArgs)
    {
        Emitter.CilAssembler.EnsureEmitAllowed("native FFI call");
        var (argTypes, fixedCount) = ParseArgTypes(argTypesList);

        var retType = retTypeKw is Nil ? typeof(void) : KeyToType(retTypeKw);

        if (nativeArgs.Length != argTypes.Count)
            throw new LispErrorException(new LispError(
                $"dotnet:ffi: {func} expects {argTypes.Count} args, got {nativeArgs.Length}"));

        // The types the call is MADE with: the variadic part may travel
        // differently from how it is declared (see VariadicCallType).
        var callTypes = argTypes.Select((t, i) => i < fixedCount ? t : VariadicCallType(t)).ToList();

        // Build or reuse DynamicMethod
        var sigKey = string.Join(",", callTypes.Select(t => t.Name)) + "→" + retType.Name
                     + "|fixed" + fixedCount;
        var cacheKey = (dll, func, sigKey);
        DynamicMethod dm;
        lock (_methodCache)
        {
            if (!_methodCache.TryGetValue(cacheKey, out dm!))
            {
                // DynamicMethod params: (IntPtr funcPtr, arg0Type, arg1Type, ...)
                var allParams = callTypes.Prepend(typeof(IntPtr)).ToArray();
                var actualRet = IsVoidType(retType) ? null : retType;
                dm = new DynamicMethod($"ffi_{func}_{sigKey}", actualRet, allParams,
                                       typeof(NativeFFI), skipVisibility: true);
                var il = dm.GetILGenerator();
                // Push native args (positions 1..n)
                for (int i = 0; i < callTypes.Count; i++)
                    il.Emit(OpCodes.Ldarg, i + 1);
                // Push funcPtr (position 0) — must be last before calli
                il.Emit(OpCodes.Ldarg_0);
                // calli: pops funcPtr last, args before it
                il.EmitCalli(OpCodes.Calli, CallingConvention.StdCall,
                             actualRet, callTypes.ToArray());
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
            invokeArgs[i + 1] = i < fixedCount
                ? ConvertArg(nativeArgs[i], argTypes[i])
                : ConvertVariadicArg(nativeArgs[i], argTypes[i]);

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
        Emitter.CilAssembler.EnsureEmitAllowed("native FFI call");
        var (argTypes, fixedCount) = ParseArgTypes(argTypesList);

        var retType = retTypeKw is Nil ? typeof(void) : KeyToType(retTypeKw);

        if (nativeArgs.Length != argTypes.Count)
            throw new LispErrorException(new LispError(
                $"dotnet:%ffi-call-ptr: expects {argTypes.Count} args, got {nativeArgs.Length}"));

        // See Call: the variadic part may travel differently from how it is declared.
        var callTypes = argTypes.Select((t, i) => i < fixedCount ? t : VariadicCallType(t)).ToList();
        var sigKey = string.Join(",", callTypes.Select(t => t.Name)) + "→" + retType.Name
                     + "|fixed" + fixedCount;
        var cacheKey = ("*ptr*", "*ptr*", sigKey);
        DynamicMethod dm;
        lock (_methodCache)
        {
            if (!_methodCache.TryGetValue(cacheKey, out dm!))
            {
                var allParams = callTypes.Prepend(typeof(IntPtr)).ToArray();
                var actualRet = IsVoidType(retType) ? null : retType;
                dm = new DynamicMethod($"ffi_ptr_{sigKey}", actualRet, allParams,
                                       typeof(NativeFFI), skipVisibility: true);
                var il = dm.GetILGenerator();
                for (int i = 0; i < callTypes.Count; i++)
                    il.Emit(OpCodes.Ldarg, i + 1);
                il.Emit(OpCodes.Ldarg_0);
                il.EmitCalli(OpCodes.Calli, CallingConvention.StdCall,
                             actualRet, callTypes.ToArray());
                il.Emit(OpCodes.Ret);
                _methodCache[cacheKey] = dm;
            }
        }

        var invokeArgs = new object?[nativeArgs.Length + 1];
        invokeArgs[0] = funcPtr;
        for (int i = 0; i < nativeArgs.Length; i++)
            invokeArgs[i + 1] = i < fixedCount
                ? ConvertArg(nativeArgs[i], argTypes[i])
                : ConvertVariadicArg(nativeArgs[i], argTypes[i]);

        var result = dm.Invoke(null, invokeArgs);
        return ConvertReturn(result, retType);
    }

    // --- Reverse callbacks: expose a Lisp function as a native function pointer. ---

    sealed class CallbackEntry
    {
        public LispFunction Fn = null!;
        public Type[] ArgTypes = null!;
        public Type RetType = null!;
    }

    // Rooted so the GC never collects a delegate whose function pointer is live in
    // native code. Indexed by id (baked into the emitted thunk as a constant).
    static readonly List<CallbackEntry> _callbackEntries = new();
    static readonly List<Delegate> _callbackRoots = new();

    /// <summary>Invoked (via the emitted thunk) when native code calls the callback.
    /// Marshals native args → Lisp, runs the Lisp function, marshals the result back.</summary>
    public static object? CallbackTrampoline(int id, object?[] nativeArgs)
    {
        CallbackEntry e;
        lock (_callbackEntries) e = _callbackEntries[id];
        var lispArgs = new LispObject[nativeArgs.Length];
        for (int i = 0; i < nativeArgs.Length; i++)
            lispArgs[i] = ConvertReturn(nativeArgs[i], e.ArgTypes[i]);
        var lispResult = e.Fn.Invoke(lispArgs);
        if (IsVoidType(e.RetType)) return null;
        // A callback body that ends in a multiple-value form hands back an
        // MvReturn, which has no native representation and used to reach
        // ConvertArg as-is (TargetInvocationException at the call from C). One
        // value crosses the boundary, so take the primary and drop the rest --
        // which is what every other CL implementation's FFI does. cffi hits this
        // for every :string-returning callback, because its conversion calls
        // FOREIGN-STRING-ALLOC and that returns (values pointer size).
        return ConvertArg(MultipleValues.Primary(lispResult), e.RetType);
    }

    static readonly MethodInfo _trampolineMI =
        typeof(NativeFFI).GetMethod(nameof(CallbackTrampoline))!;

    // Non-generic delegate types (Marshal.GetFunctionPointerForDelegate rejects Func<>/
    // Action<>) built once per signature via Reflection.Emit.
    static readonly Dictionary<string, Type> _delegateTypeCache = new();
    static ModuleBuilder? _delegateModule;

    static Type GetNativeDelegateType(Type[] argTypes, Type retType)
    {
        var key = string.Join(",", argTypes.Select(t => t.Name)) + "->" + retType.Name;
        lock (_delegateTypeCache)
        {
            if (_delegateTypeCache.TryGetValue(key, out var cached)) return cached;
            if (_delegateModule == null)
            {
                var ab = AssemblyBuilder.DefineDynamicAssembly(
                    new AssemblyName("DotclFfiCallbacks"), AssemblyBuilderAccess.Run);
                _delegateModule = ab.DefineDynamicModule("m");
            }
            var tb = _delegateModule.DefineType(
                "cb_" + _delegateTypeCache.Count,
                TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.AutoClass,
                typeof(MulticastDelegate));
            // On x64/ARM64 the calling convention is unified; Cdecl matches C callbacks
            // like qsort and is CFFI's default.
            tb.SetCustomAttribute(new CustomAttributeBuilder(
                typeof(UnmanagedFunctionPointerAttribute).GetConstructor(new[] { typeof(CallingConvention) })!,
                new object[] { CallingConvention.Cdecl }));
            var ctor = tb.DefineConstructor(
                MethodAttributes.RTSpecialName | MethodAttributes.HideBySig | MethodAttributes.Public,
                CallingConventions.Standard, new[] { typeof(object), typeof(IntPtr) });
            ctor.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
            var invoke = tb.DefineMethod("Invoke",
                MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.NewSlot | MethodAttributes.Virtual,
                retType, argTypes);
            invoke.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
            var dt = tb.CreateType()!;
            _delegateTypeCache[key] = dt;
            return dt;
        }
    }

    /// <summary>Build a native function pointer that dispatches to LISPFN. ARGTYPES/RET are
    /// dotcl FFI type keywords (as used by %ffi-call-ptr).</summary>
    public static IntPtr MakeCallback(LispObject lispFn, LispObject argTypesList, LispObject retTypeKw)
    {
        Emitter.CilAssembler.EnsureEmitAllowed("native FFI callback");
        if (lispFn is not LispFunction fn)
            throw new LispErrorException(new LispError(
                "dotnet:make-ffi-callback: first argument must be a function"));

        var argTypes = new List<Type>();
        for (var cur = argTypesList; cur is Cons c; cur = c.Cdr)
            argTypes.Add(KeyToType(c.Car));
        var retType = retTypeKw is Nil ? typeof(void) : KeyToType(retTypeKw);

        int id;
        var entry = new CallbackEntry { Fn = fn, ArgTypes = argTypes.ToArray(), RetType = retType };
        lock (_callbackEntries) { id = _callbackEntries.Count; _callbackEntries.Add(entry); }

        // Emit a thunk with the exact native signature that boxes its args into an
        // object[] and calls CallbackTrampoline(id, args), then unboxes the result.
        var actualRet = IsVoidType(retType) ? null : retType;
        var dm = new DynamicMethod($"ffi_cb_{id}", actualRet, argTypes.ToArray(),
                                   typeof(NativeFFI), skipVisibility: true);
        var il = dm.GetILGenerator();
        var arr = il.DeclareLocal(typeof(object[]));
        il.Emit(OpCodes.Ldc_I4, argTypes.Count);
        il.Emit(OpCodes.Newarr, typeof(object));
        il.Emit(OpCodes.Stloc, arr);
        for (int i = 0; i < argTypes.Count; i++)
        {
            il.Emit(OpCodes.Ldloc, arr);
            il.Emit(OpCodes.Ldc_I4, i);
            il.Emit(OpCodes.Ldarg, i);
            il.Emit(OpCodes.Box, argTypes[i]);
            il.Emit(OpCodes.Stelem_Ref);
        }
        il.Emit(OpCodes.Ldc_I4, id);
        il.Emit(OpCodes.Ldloc, arr);
        il.Emit(OpCodes.Call, _trampolineMI);
        if (actualRet == null) il.Emit(OpCodes.Pop);
        else il.Emit(OpCodes.Unbox_Any, retType);
        il.Emit(OpCodes.Ret);

        var delType = GetNativeDelegateType(argTypes.ToArray(), actualRet ?? typeof(void));
        var del = dm.CreateDelegate(delType);
        lock (_callbackRoots) _callbackRoots.Add(del);
        return Marshal.GetFunctionPointerForDelegate(del);
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

    /// <summary>
    /// (dotnet:make-ffi-callback fn arg-types ret-type) => pointer
    /// Expose the Lisp function FN as a native function pointer. ARG-TYPES is a list
    /// of type keywords, RET-TYPE a keyword or NIL for void. The pointer stays valid
    /// for the process lifetime (the delegate is rooted).
    /// </summary>
    public static LispObject MakeFfiCallback(LispObject[] args)
    {
        if (args.Length != 3)
            throw new LispErrorException(new LispProgramError(
                "dotnet:make-ffi-callback: requires (fn arg-types ret-type)"));
        var ptr = NativeFFI.MakeCallback(args[0], args[1], args[2]);
        return Fixnum.Make(ptr.ToInt64());
    }
}
