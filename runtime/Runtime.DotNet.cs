namespace DotCL;

/// <summary>
/// Wraps a .NET object for use in Lisp code.
/// </summary>
public class LispDotNetObject : LispObject
{
    public object Value { get; }
    public Type Type { get; }

    public LispDotNetObject(object value)
    {
        Value = value ?? throw new ArgumentNullException(nameof(value));
        Type = value.GetType();
    }

    public override string ToString()
        => $"#<DOTNET {Type.FullName} {Value}>";
}

/// <summary>
/// A LispObject that carries a type hint for .NET method resolution.
/// </summary>
public class LispDotNetBoxed : LispDotNetObject
{
    public Type HintType { get; }

    public LispDotNetBoxed(object value, Type hintType) : base(value)
    {
        HintType = hintType;
    }

    public override string ToString()
        => $"#<DOTNET-BOXED {HintType.Name} {Value}>";
}

public static partial class Runtime
{
    // Tracks dynamically-defined class names (uppercase simple name → Type) for
    // case-insensitive resolution from Lisp symbols (e.g. symbol Animal → "ANIMAL").
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Type>
        _dotNetDynTypeByUpperName = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Convert a .NET object to an appropriate LispObject.</summary>
    public static LispObject DotNetToLisp(object? value)
    {
        if (value == null) return Nil.Instance;
        return value switch
        {
            int i => Fixnum.Make(i),
            long l => Fixnum.Make(l),
            double d => new DoubleFloat(d),
            float f => new DoubleFloat(f),
            string s => new LispString(s),
            char c => LispChar.Make(c),
            bool b => b ? (LispObject)T.Instance : Nil.Instance,
            LispObject lo => lo,
            _ => new LispDotNetObject(value)
        };
    }

    /// <summary>Convert a LispObject to a .NET type based on target parameter type.</summary>
    public static object? LispToDotNet(LispObject arg, Type targetType)
    {
        // LispDotNetBoxed: use hint type
        if (arg is LispDotNetBoxed boxed)
        {
            if (targetType.IsAssignableFrom(boxed.HintType))
                return boxed.Value;
            return Convert.ChangeType(boxed.Value, targetType);
        }

        // LispDotNetObject: unwrap
        if (arg is LispDotNetObject dno)
        {
            if (targetType.IsAssignableFrom(dno.Type))
                return dno.Value;
            return Convert.ChangeType(dno.Value, targetType);
        }

        // Nil → null or false
        if (arg is Nil)
        {
            if (targetType == typeof(bool)) return false;
            if (!targetType.IsValueType) return null;
            return Activator.CreateInstance(targetType);
        }

        // T → true
        if (arg is T && targetType == typeof(bool))
            return true;

        // Fixnum → numeric types
        if (arg is Fixnum fx)
        {
            if (targetType == typeof(int)) return (int)fx.Value;
            if (targetType == typeof(long)) return fx.Value;
            if (targetType == typeof(double)) return (double)fx.Value;
            if (targetType == typeof(float)) return (float)fx.Value;
            if (targetType == typeof(short)) return (short)fx.Value;
            if (targetType == typeof(byte)) return (byte)fx.Value;
            if (targetType == typeof(decimal)) return (decimal)fx.Value;
            if (targetType == typeof(object)) return fx.Value;
        }

        // DoubleFloat → double/float
        if (arg is DoubleFloat df)
        {
            if (targetType == typeof(double)) return df.Value;
            if (targetType == typeof(float)) return (float)df.Value;
            if (targetType == typeof(decimal)) return (decimal)df.Value;
            if (targetType == typeof(object)) return df.Value;
        }

        // SingleFloat → float/double
        if (arg is SingleFloat sf)
        {
            if (targetType == typeof(float)) return sf.Value;
            if (targetType == typeof(double)) return (double)sf.Value;
            if (targetType == typeof(decimal)) return (decimal)sf.Value;
            if (targetType == typeof(object)) return sf.Value;
        }

        // LispString → string
        if (arg is LispString ls)
        {
            if (targetType == typeof(string)) return ls.Value;
            if (targetType == typeof(object)) return ls.Value;
        }

        // Char-backed LispVector (BASE-STRING / fill-pointered string) → string.
        // CL strings have two runtime representations (LispString and char
        // LispVector); both must marshal to System.String for .NET interop.
        if (arg is LispVector cv && cv.IsCharVector)
        {
            if (targetType == typeof(string)) return cv.ToCharString();
            if (targetType == typeof(object)) return cv.ToCharString();
        }

        // LispFunction → delegate (Func<>, Action<>, EventHandler<>, etc.)
        if (arg is LispFunction fn && typeof(Delegate).IsAssignableFrom(targetType))
            return CreateLispDelegate(fn, targetType);

        // Fallback: pass as object
        if (targetType == typeof(object)) return arg;

        throw new LispErrorException(new LispTypeError(
            $"Cannot convert {arg.GetType().Name} to {targetType.Name}", arg));
    }

    public static LispObject DotNetLoadAssembly(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:LOAD-ASSEMBLY: wrong number of arguments: " + args.Length + " (expected 1)"));

        string path = args[0] switch
        {
            LispString ls => ls.Value,
            _ => args[0].ToString() ?? ""
        };

        // If no path separators and no .dll extension, treat as assembly name.
        // Try Assembly.Load first (base runtime), then search shared framework dirs
        // so e.g. "System.Windows.Forms" finds Microsoft.WindowsDesktop.App.
        if (!path.Contains('/') && !path.Contains('\\')
            && !path.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
        {
            try
            {
                var byName = System.Reflection.Assembly.Load(path);
                return new LispString(byName.FullName ?? path);
            }
            catch { }

            var dllPath = FindSharedFrameworkDll(path);
            if (dllPath != null)
            {
                var byPath = System.Reflection.Assembly.LoadFrom(dllPath);
                return new LispString(byPath.FullName ?? path);
            }

            throw new LispErrorException(new LispProgramError(
                $"DOTNET:LOAD-ASSEMBLY: assembly not found: {path}"));
        }

        // LoadFrom (not Load(bytes)) so transitive references in the same
        // directory resolve automatically. Required for contribs that ship
        // their own lib/ directory with multiple interdependent DLLs (e.g.
        // dotcl-cs loading Roslyn).
        var asm = System.Reflection.Assembly.LoadFrom(System.IO.Path.GetFullPath(path));
        return new LispString(asm.FullName ?? path);
    }

    internal static string? FindSharedFrameworkDll(string assemblyName)
    {
        // RuntimeEnvironment.GetRuntimeDirectory() returns e.g.
        // C:\Program Files\dotnet\shared\Microsoft.NETCore.App\10.0.5\
        // Go up two levels to reach the dotnet root's shared/ directory.
        var runtimeDir = System.Runtime.InteropServices.RuntimeEnvironment
                             .GetRuntimeDirectory();
        var sharedDir = System.IO.Path.GetDirectoryName(
                            System.IO.Path.GetDirectoryName(runtimeDir.TrimEnd('/', '\\')));
        if (sharedDir == null || !System.IO.Directory.Exists(sharedDir)) return null;

        foreach (var fwDir in System.IO.Directory.GetDirectories(sharedDir))
        foreach (var verDir in System.IO.Directory.GetDirectories(fwDir)
                                    .OrderByDescending(d => d))
        {
            var dll = System.IO.Path.Combine(verDir, assemblyName + ".dll");
            if (System.IO.File.Exists(dll)) return dll;
        }
        return null;
    }

    /// <summary>DOTNET:RESOLVE-TYPE — resolve a .NET System.Type by name (searching loaded
    /// assemblies, loading by namespace prefix, and COM ProgIDs on Windows), returning it
    /// wrapped as a .NET object. The result can be inspected or passed anywhere a
    /// System.Type is expected (it unwraps to the Type). Signals an error if not found.
    /// Exposes the previously-internal ResolveDotNetType (dotcl/dotcl#17).</summary>
    public static LispObject DotNetResolveType(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:RESOLVE-TYPE: expected 1 argument, got {args.Length}"));
        if (args[0] is not LispString name)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:RESOLVE-TYPE: type name must be a string", args[0]));
        return new LispDotNetObject(ResolveDotNetType(name.Value));
    }

    /// <summary>Resolve a .NET type by full name, searching loaded assemblies.
    /// Falls back to COM ProgID lookup (Windows only) for names like "Excel.Application".</summary>
    internal static Type ResolveDotNetType(string typeName)
    {
        var type = Type.GetType(typeName);
        if (type != null) return type;

        // Strip ", AssemblyName" suffix before searching loaded assemblies with GetType(),
        // which only accepts unqualified type names.
        string bareTypeName = typeName;
        int commaIdx = typeName.IndexOf(',');
        if (commaIdx > 0) bareTypeName = typeName[..commaIdx].Trim();

        foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
        {
            type = asm.GetType(bareTypeName);
            if (type != null) return type;
        }

        // Try to load the assembly based on namespace prefix
        // e.g., "System.Net.IPAddress" → try loading "System.Net" assembly
        var parts = bareTypeName.Split('.');
        for (int len = parts.Length - 1; len >= 1; len--)
        {
            var asmName = string.Join(".", parts, 0, len);
            try
            {
                var asm = System.Reflection.Assembly.Load(asmName);
                type = asm.GetType(bareTypeName);
                if (type != null) return type;
            }
            catch { }
        }

        // COM ProgID fallback. On non-Windows GetTypeFromProgID returns null
        // (does not throw in modern .NET), so this is safe to call always.
        try
        {
            var comType = Type.GetTypeFromProgID(typeName);
            if (comType != null) return comType;
        }
        catch { }

        // Fallback: case-insensitive lookup in dynamically-defined types (e.g. Lisp symbol
        // 'Animal uppercased to "ANIMAL" by reader, but dynamic type is named "Animal").
        if (_dotNetDynTypeByUpperName.TryGetValue(typeName, out var dynType))
            return dynType;

        throw new LispErrorException(new LispError($"DOTNET: type not found: {typeName}"));
    }

    /// <summary>Best-fit conversion when target parameter type is unknown
    /// (InvokeMember path). Default Binder picks the overload from these
    /// runtime types.</summary>
    internal static object? LispToDotNetGeneric(LispObject arg)
    {
        return arg switch
        {
            LispDotNetBoxed b => b.Value,
            LispDotNetObject d => d.Value,
            Nil => null,
            T => true,
            Fixnum fx => (fx.Value >= int.MinValue && fx.Value <= int.MaxValue)
                            ? (object)(int)fx.Value : fx.Value,
            DoubleFloat df => df.Value,
            SingleFloat sf => sf.Value,
            LispString ls => ls.Value,
            // Char-backed LispVector (BASE-STRING / fill-pointered string): CL
            // strings have two representations; marshal both to System.String so
            // overload resolution (e.g. Graphics.DrawString(string,...)) binds.
            LispVector cv when cv.IsCharVector => cv.ToCharString(),
            _ => arg
        };
    }

    private static object?[] LispArgsToDotNetGeneric(LispObject[] lispArgs)
    {
        var result = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            result[i] = LispToDotNetGeneric(lispArgs[i]);
        return result;
    }

    private const System.Reflection.BindingFlags InstanceReadFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Instance
        | System.Reflection.BindingFlags.InvokeMethod
        | System.Reflection.BindingFlags.GetProperty
        | System.Reflection.BindingFlags.GetField;

    private const System.Reflection.BindingFlags InstanceWriteFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Instance
        | System.Reflection.BindingFlags.SetProperty
        | System.Reflection.BindingFlags.SetField;

    private const System.Reflection.BindingFlags StaticReadFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Static
        | System.Reflection.BindingFlags.InvokeMethod
        | System.Reflection.BindingFlags.GetProperty
        | System.Reflection.BindingFlags.GetField;

    private const System.Reflection.BindingFlags StaticWriteFlags =
        System.Reflection.BindingFlags.Public
        | System.Reflection.BindingFlags.Static
        | System.Reflection.BindingFlags.SetProperty
        | System.Reflection.BindingFlags.SetField;

    // ── dotnet:invoke / dotnet:static method-resolution cache ────────────────────
    // Type.InvokeMember re-runs member-name lookup + default-Binder overload
    // resolution on every call. Cache the resolved MethodInfo keyed by (runtime
    // type OBJECT, member name, arg runtime types) — exactly the inputs the binder
    // uses — so a hot interop loop pays resolution only once and then goes straight
    // to MethodInfo.Invoke (which .NET 8+ backs with a cached fast invoker stub).
    // Pure reflection, no Reflection.Emit, so the fast path is AOT/IL2CPP-safe.
    //
    // Only plain fixed-arity method calls are cached. COM/IDispatch targets (one
    // shared __ComObject type, per-object member set), params / by-ref methods,
    // generic definitions, null args, and property/field access all fall through to
    // the unchanged InvokeMember path — the cache can never alter overload
    // resolution, COM dispatch, or the #24 optional-argument fallback.
    //
    // Keying on the Type OBJECT (not its name) makes class redefinition safe: a
    // redefined type is a new object = a new key, so old entries are never served.
    // A RuntimeType's member set is otherwise immutable, so entries don't go stale
    // (the one runtime-mutation path, Hot Reload via MetadataUpdater.ApplyUpdate, is
    // dev-only; wire a MetadataUpdateHandler to flush this if it ever matters).
    private readonly struct InvokeKey : IEquatable<InvokeKey>
    {
        public readonly Type Owner;
        public readonly string Name;
        public readonly Type[] ArgTypes;
        public InvokeKey(Type owner, string name, Type[] argTypes)
        { Owner = owner; Name = name; ArgTypes = argTypes; }

        public bool Equals(InvokeKey o)
        {
            if (!ReferenceEquals(Owner, o.Owner) || Name != o.Name
                || ArgTypes.Length != o.ArgTypes.Length) return false;
            for (int i = 0; i < ArgTypes.Length; i++)
                if (!ReferenceEquals(ArgTypes[i], o.ArgTypes[i])) return false;
            return true;
        }
        public override bool Equals(object? o) => o is InvokeKey k && Equals(k);
        public override int GetHashCode()
        {
            var h = new HashCode();
            h.Add(Owner);
            h.Add(Name);
            for (int i = 0; i < ArgTypes.Length; i++) h.Add(ArgTypes[i]);
            return h.ToHashCode();
        }
    }

    // null value = "this signature is not cacheable; always use InvokeMember".
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<InvokeKey, System.Reflection.MethodInfo?>
        _invokeMethodCache = new();

    /// <summary>Fast path for dotnet:invoke / dotnet:static plain method calls:
    /// resolve the MethodInfo once (cached) then invoke it directly, skipping
    /// InvokeMember's per-call name lookup and overload resolution. Returns false
    /// (caller falls back to InvokeMember) for anything not safe to cache. A
    /// TargetInvocationException from the invoked method propagates unchanged so the
    /// caller's existing handler wraps it identically.</summary>
    private static bool TryCachedInvoke(
        Type type, string name, object? target, object?[] callArgs, bool isStatic, out object? result)
    {
        result = null;

        // COM IDispatch: every __ComObject shares one Type but resolves members
        // per-object. Caching by Type would serve another object's dispatch.
        if (type.IsCOMObject) return false;

        // A null arg has no runtime type → can't key it or overload-resolve it; let
        // InvokeMember's binder handle null matching.
        var argTypes = new Type[callArgs.Length];
        for (int i = 0; i < callArgs.Length; i++)
        {
            if (callArgs[i] is null) return false;
            argTypes[i] = callArgs[i]!.GetType();
        }

        var key = new InvokeKey(type, name, argTypes);
        if (!_invokeMethodCache.TryGetValue(key, out var method))
        {
            method = ResolveCacheableMethod(type, name, argTypes, isStatic);
            _invokeMethodCache[key] = method;
        }
        if (method is null) return false;

        result = method.Invoke(target, callArgs);
        return true;
    }

    /// <summary>Pick the method InvokeMember's default binder would select, but only
    /// when it is a plain fixed-arity method (no params / by-ref params, not a
    /// generic definition). Returns null otherwise so the caller keeps the
    /// InvokeMember path (which also handles property/field access and #24 optional
    /// defaults).</summary>
    private static System.Reflection.MethodInfo? ResolveCacheableMethod(
        Type type, string name, Type[] argTypes, bool isStatic)
    {
        var flags = System.Reflection.BindingFlags.Public
            | (isStatic ? System.Reflection.BindingFlags.Static : System.Reflection.BindingFlags.Instance);

        System.Collections.Generic.List<System.Reflection.MethodBase>? candidates = null;
        foreach (var m in type.GetMethods(flags))
        {
            if (m.Name != name || m.IsGenericMethodDefinition) continue;
            var ps = m.GetParameters();
            if (ps.Length != argTypes.Length) continue;          // omitted optionals -> InvokeMember
            bool ok = true;
            foreach (var p in ps)
                if (p.ParameterType.IsByRef
                    || System.Attribute.IsDefined(p, typeof(ParamArrayAttribute)))
                { ok = false; break; }                            // ref/out and params -> InvokeMember
            if (!ok) continue;
            (candidates ??= new System.Collections.Generic.List<System.Reflection.MethodBase>()).Add(m);
        }
        if (candidates is null) return null;

        try
        {
            return (System.Reflection.MethodInfo?)Type.DefaultBinder.SelectMethod(
                flags, candidates.ToArray(), argTypes, null);
        }
        catch (System.Reflection.AmbiguousMatchException) { return null; }
    }

    /// <summary>Fallback for dotnet:invoke / dotnet:static when InvokeMember finds no
    /// matching overload: locate a method NAME whose first parameters take the supplied
    /// args and whose remaining (trailing) parameters are all C# optional, then fill those
    /// with their declared default values. Lets callers omit defaulted parameters
    /// (e.g. SpriteBatch.Begin()) (dotcl/dotcl#24). Returns true and sets RESULT on success.</summary>
    private static bool TryInvokeWithOptionalDefaults(
        Type type, string name, object? target, LispObject[] lispArgs, bool isStatic, out object? result)
    {
        result = null;
        var flags = System.Reflection.BindingFlags.Public
            | (isStatic ? System.Reflection.BindingFlags.Static : System.Reflection.BindingFlags.Instance);
        System.Reflection.MethodInfo? best = null;
        int bestLen = int.MaxValue;
        foreach (var m in type.GetMethods(flags))
        {
            if (m.Name != name || m.IsGenericMethodDefinition) continue;
            var ps = m.GetParameters();
            if (ps.Length <= lispArgs.Length) continue;           // need omitted optional params
            bool tailOptional = true;
            for (int i = lispArgs.Length; i < ps.Length; i++)
                if (!ps[i].IsOptional) { tailOptional = false; break; }
            if (!tailOptional) continue;
            if (ps.Length < bestLen) { best = m; bestLen = ps.Length; }   // closest match
        }
        if (best == null) return false;

        var bps = best.GetParameters();
        var full = new object?[bps.Length];
        try
        {
            for (int i = 0; i < lispArgs.Length; i++)
                full[i] = LispToDotNet(lispArgs[i], bps[i].ParameterType);
        }
        catch { return false; }   // supplied args not convertible to this overload
        for (int i = lispArgs.Length; i < bps.Length; i++)
            full[i] = bps[i].HasDefaultValue ? bps[i].DefaultValue : Type.Missing;

        try { result = best.Invoke(target, full); return true; }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:{(isStatic ? "STATIC" : "INVOKE")} {type.Name}.{name}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }

    /// <summary>(dotnet:static "Type" "Member" &rest args)
    /// Read-side entry point for static methods, properties, and fields.
    /// Type.InvokeMember dispatches based on member kind + arg count.</summary>
    public static LispObject DotNetStatic(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:STATIC: requires at least 2 arguments (type-name member-name &rest args)"));

        string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        try
        {
            if (TryCachedInvoke(type, memberName, null, callArgs, true, out var cached))
                return DotNetToLisp(cached);
            var result = type.InvokeMember(memberName, StaticReadFlags, null, null, callArgs);
            return DotNetToLisp(result);
        }
        catch (MissingMethodException)
        {
            // No exact overload — retry allowing omitted C# optional parameters (#24).
            if (TryInvokeWithOptionalDefaults(type, memberName, null, args.Skip(2).ToArray(), true, out var r))
                return DotNetToLisp(r);
            throw;
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:STATIC {typeName}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }

    /// <summary>(dotnet:%set-static "Type" "Member" &rest indexer-args value)
    /// Write-side entry point. Last argument is the value to assign;
    /// preceding args (if any) are property indexer arguments.</summary>
    public static LispObject DotNetSetStatic(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%SET-STATIC: requires at least 3 arguments (type-name member-name [indexers...] value)"));

        string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        try
        {
            type.InvokeMember(memberName, StaticWriteFlags, null, null, callArgs);
            return args[args.Length - 1];
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:%SET-STATIC {typeName}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }

    /// <summary>(dotnet:invoke object "Member" &rest args)
    /// Read-side entry point for instance methods, properties, fields, and
    /// COM IDispatch members. Type.InvokeMember on the runtime type routes
    /// transparently for both managed and __ComObject targets.</summary>
    public static LispObject DotNetInvoke(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:INVOKE: requires at least 2 arguments (object member-name &rest args)"));

        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:INVOKE: first argument must be a .NET object", args[0]));

        var target = dno.Value;
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = target.GetType();
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        // C# arrays don't expose get_Item/set_Item as named methods; route to
        // Array.GetValue / Array.SetValue instead.
        if (target is Array arr)
        {
            if (memberName == "get_Item")
            {
                var indices = callArgs.Select(Convert.ToInt32).ToArray();
                return DotNetToLisp(arr.GetValue(indices));
            }
            if (memberName == "set_Item")
            {
                var indices = callArgs.Take(callArgs.Length - 1).Select(Convert.ToInt32).ToArray();
                arr.SetValue(callArgs[callArgs.Length - 1], indices);
                return DotNetToLisp(callArgs[callArgs.Length - 1]);
            }
        }

        try
        {
            if (TryCachedInvoke(type, memberName, target, callArgs, false, out var cached))
                return DotNetToLisp(cached);
            var result = type.InvokeMember(memberName, InstanceReadFlags, null, target, callArgs);
            return DotNetToLisp(result);
        }
        catch (MissingMethodException)
        {
            // No exact overload — retry allowing omitted C# optional parameters (#24).
            if (TryInvokeWithOptionalDefaults(type, memberName, target, args.Skip(2).ToArray(), false, out var r))
                return DotNetToLisp(r);
            throw;
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:INVOKE {type.Name}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }

    /// <summary>(dotnet:%set-invoke object "Member" &rest indexer-args value)
    /// Last arg is the value; preceding args are indexer arguments.</summary>
    public static LispObject DotNetSetInvoke(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%SET-INVOKE: requires at least 3 arguments (object member-name [indexers...] value)"));

        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:%SET-INVOKE: first argument must be a .NET object", args[0]));

        var target = dno.Value;
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = target.GetType();
        var callArgs = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        try
        {
            type.InvokeMember(memberName, InstanceWriteFlags, null, target, callArgs);
            return args[args.Length - 1];
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:%SET-INVOKE {type.Name}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }

    public static LispObject DotNetNew(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:NEW: requires at least 1 argument (type-name &rest args)"));

        string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);

        if (args.Length == 1)
        {
            // A true parameterless ctor (value types always have one).
            if (type.IsValueType || type.GetConstructor(Type.EmptyTypes) != null)
                return new LispDotNetObject(Activator.CreateInstance(type)!);
            // Otherwise fall back to an all-optional ctor, supplying its defaults —
            // C#'s `new T()` does the same (e.g. FluentTheme(Uri? baseUri = null)).
            var optCtor = type.GetConstructors()
                .Where(c => c.GetParameters().Length > 0 && c.GetParameters().All(p => p.IsOptional))
                .OrderBy(c => c.GetParameters().Length)
                .FirstOrDefault();
            if (optCtor != null)
            {
                var ps = optCtor.GetParameters();
                var defaults = new object?[ps.Length];
                for (int i = 0; i < ps.Length; i++)
                    defaults[i] = ps[i].HasDefaultValue ? ps[i].DefaultValue : Type.Missing;
                return new LispDotNetObject(optCtor.Invoke(defaults)!);
            }
            // No usable ctor — let Activator throw its descriptive error.
            return new LispDotNetObject(Activator.CreateInstance(type)!);
        }

        var lispArgs = args.Skip(1).ToArray();
        var ctors = type.GetConstructors()
            .Where(c => c.GetParameters().Length == lispArgs.Length).ToArray();

        if (ctors.Length == 0)
            throw new LispErrorException(new LispError(
                $"DOTNET:NEW: no constructor for {typeName} with {lispArgs.Length} arguments"));

        // Score constructors like methods for best type match
        var ctor = ctors.OrderByDescending(c => {
            int score = 0;
            var ps = c.GetParameters();
            for (int i = 0; i < ps.Length && i < lispArgs.Length; i++)
            {
                var pt = ps[i].ParameterType;
                if (lispArgs[i] is Fixnum) { if (pt == typeof(int) || pt == typeof(long)) score += 10; }
                else if (lispArgs[i] is DoubleFloat || lispArgs[i] is SingleFloat) { if (pt == typeof(double)) score += 10; }
                else if (lispArgs[i] is LispString) { if (pt == typeof(string)) score += 10; }
            }
            return score;
        }).First();
        var paramTypes = ctor.GetParameters();
        var convertedArgs = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            convertedArgs[i] = LispToDotNet(lispArgs[i], paramTypes[i].ParameterType);

        var instance = ctor.Invoke(convertedArgs);
        return new LispDotNetObject(instance);
    }

    /// <summary>(dotnet:%define-class "Full.Name" &optional "Base.Type" field-specs attr-specs method-specs ctor-body property-specs interface-specs event-specs)
    /// Emit a named public class. Shapes: (fields) (attrs),
    /// (methods) (ctor-body: 1-arg Lisp fn called after base.ctor),
    /// (property-specs: list of ("Name" "TypeName") for auto-properties).
    /// method-spec accepts optional 5th element override-flag; when truthy,
    /// the method is emitted as an override of a matching base virtual method.
    /// 8th arg interface-specs is a list of fully qualified interface
    /// type names; each declared, and any method in method-specs whose
    /// name+signature matches an interface method is emitted as the implicit
    /// implementation of that slot.
    /// 9th arg event-specs is a list of ("Name" "DelegateTypeName"); each
    /// emits a private delegate field + public add_/remove_ accessors +
    /// EventBuilder. If a declared interface carries a matching add_/remove_
    /// slot, the accessors are wired up as implicit implementations.
    /// property-specs accepts optional 3rd element (notify-flag); when
    /// truthy, the setter additionally calls OnPropertyChanged with a
    /// PropertyChangedEventArgs carrying the property name. Requires a
    /// matching PropertyChanged event to be declared via event-specs.
    /// Returns the full name as a LispString on success.</summary>
    public static LispObject DotNetDefineClass(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 12)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:%DEFINE-CLASS: requires 1-12 arguments (full-name &optional base-type-name field-specs attr-specs method-specs ctor-body property-specs interface-specs event-specs ctor-param-types base-ctor-arg-indices ctor-specs-list)"));

        string fullName = args[0] switch
        {
            LispString ls => ls.Value,
            _ => args[0].ToString() ?? ""
        };

        Type? baseType = null;
        if (args.Length >= 2 && args[1] != Nil.Instance)
        {
            string baseName = args[1] switch
            {
                LispString ls => ls.Value,
                _ => args[1].ToString() ?? ""
            };
            baseType = ResolveDotNetType(baseName);
        }

        List<(string, Type)>? fields = null;
        if (args.Length >= 3 && args[2] != Nil.Instance)
        {
            fields = new List<(string, Type)>();
            var cur = args[2];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each field spec must be a (name type-name) list",
                        c.Car));
                var nameObj = spec.Car;
                var typeObj = spec.Cdr is Cons c2 ? c2.Car : Nil.Instance;

                string fname = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                fields.Add((fname, ResolveDotNetType(tname)));
                cur = c.Cdr;
            }
        }

        List<System.Reflection.Emit.CustomAttributeBuilder>? attrs = null;
        if (args.Length >= 4 && args[3] != Nil.Instance)
        {
            attrs = new List<System.Reflection.Emit.CustomAttributeBuilder>();
            var cur = args[3];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each attr spec must be a (type-name ctor-args...) list",
                        c.Car));
                var typeObj = spec.Car;
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                var attrType = ResolveDotNetType(tname);

                // Collect ctor args (rest of spec).
                var ctorLispArgs = new List<LispObject>();
                var acur = spec.Cdr;
                while (acur is Cons ac) { ctorLispArgs.Add(ac.Car); acur = ac.Cdr; }

                // Find a ctor matching argcount.
                var ctors = attrType.GetConstructors()
                    .Where(ci => ci.GetParameters().Length == ctorLispArgs.Count).ToArray();
                if (ctors.Length == 0)
                    throw new LispErrorException(new LispError(
                        $"DOTNET:%DEFINE-CLASS: no constructor on {tname} with {ctorLispArgs.Count} arguments"));
                var ctor = ctors[0];
                var ctorParamTypes = ctor.GetParameters();
                var ctorArgs = new object?[ctorLispArgs.Count];
                for (int i = 0; i < ctorLispArgs.Count; i++)
                    ctorArgs[i] = LispToDotNet(ctorLispArgs[i], ctorParamTypes[i].ParameterType);

                attrs.Add(new System.Reflection.Emit.CustomAttributeBuilder(ctor, ctorArgs));
                cur = c.Cdr;
            }
        }

        List<Emitter.DynamicClassBuilder.MethodSpec>? methods = null;
        if (args.Length >= 5 && args[4] != Nil.Instance)
        {
            methods = new List<Emitter.DynamicClassBuilder.MethodSpec>();
            var cur = args[4];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each method spec must be a (name return-type (param-types) lambda) list",
                        c.Car));
                // Spec shape: (name return-type (param-types) lambda)
                var nameObj = spec.Car;
                var rest = spec.Cdr;
                if (rest is not Cons r1)
                    throw new LispErrorException(new LispProgramError(
                        "DOTNET:%DEFINE-CLASS: method spec missing return type"));
                var retObj = r1.Car;
                var rest2 = r1.Cdr;
                if (rest2 is not Cons r2)
                    throw new LispErrorException(new LispProgramError(
                        "DOTNET:%DEFINE-CLASS: method spec missing param-types list"));
                var paramListObj = r2.Car;
                var rest3 = r2.Cdr;
                if (rest3 is not Cons r3)
                    throw new LispErrorException(new LispProgramError(
                        "DOTNET:%DEFINE-CLASS: method spec missing lambda"));
                var lambdaObj = r3.Car;

                string mname = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string rname = retObj switch
                {
                    LispString ls => ls.Value,
                    _ => retObj.ToString() ?? ""
                };
                Type rtype = rname == "System.Void" ? typeof(void) : ResolveDotNetType(rname);

                var paramTypes = new List<Type>();
                var pcur = paramListObj;
                while (pcur is Cons pc)
                {
                    var ptObj = pc.Car;
                    string ptname = ptObj switch
                    {
                        LispString ls => ls.Value,
                        _ => ptObj.ToString() ?? ""
                    };
                    paramTypes.Add(ResolveDotNetType(ptname));
                    pcur = pc.Cdr;
                }

                if (lambdaObj is not LispFunction)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: method body must be a function",
                        lambdaObj));

                // Optional 5th element: override flag. Nil/absent = false.
                bool isOverride = false;
                LispObject? attrSpecsObj = null;
                if (r3.Cdr is Cons r4)
                {
                    isOverride = r4.Car != Nil.Instance;
                    // Optional 6th element: list of (type-name ctor-args...)
                    // attribute specs, same shape as the class-level attrs list.
                    if (r4.Cdr is Cons r5)
                        attrSpecsObj = r5.Car;
                }

                List<System.Reflection.Emit.CustomAttributeBuilder>? methodAttrs = null;
                if (attrSpecsObj != null && attrSpecsObj != Nil.Instance)
                {
                    methodAttrs = new List<System.Reflection.Emit.CustomAttributeBuilder>();
                    var acur = attrSpecsObj;
                    while (acur is Cons ac)
                    {
                        if (ac.Car is not Cons aspec)
                            throw new LispErrorException(new LispTypeError(
                                "DOTNET:%DEFINE-CLASS: each method attr spec must be a (type-name ctor-args...) list",
                                ac.Car));
                        var atypeObj = aspec.Car;
                        string atname = atypeObj switch
                        {
                            LispString ls => ls.Value,
                            _ => atypeObj.ToString() ?? ""
                        };
                        var attrType = ResolveDotNetType(atname);

                        var actorArgs = new List<LispObject>();
                        var aacur = aspec.Cdr;
                        while (aacur is Cons aac) { actorArgs.Add(aac.Car); aacur = aac.Cdr; }

                        var actors = attrType.GetConstructors()
                            .Where(ci => ci.GetParameters().Length == actorArgs.Count).ToArray();
                        if (actors.Length == 0)
                            throw new LispErrorException(new LispError(
                                $"DOTNET:%DEFINE-CLASS: no constructor on {atname} with {actorArgs.Count} arguments"));
                        var actor = actors[0];
                        var actorParamTypes = actor.GetParameters();
                        var actorArgsArr = new object?[actorArgs.Count];
                        for (int i = 0; i < actorArgs.Count; i++)
                            actorArgsArr[i] = LispToDotNet(actorArgs[i], actorParamTypes[i].ParameterType);

                        methodAttrs.Add(new System.Reflection.Emit.CustomAttributeBuilder(actor, actorArgsArr));
                        acur = ac.Cdr;
                    }
                }

                methods.Add(new Emitter.DynamicClassBuilder.MethodSpec(
                    mname, rtype, paramTypes, lambdaObj, isOverride, methodAttrs));
                cur = c.Cdr;
            }
        }

        LispObject? ctorBody = null;
        if (args.Length >= 6 && args[5] != Nil.Instance)
        {
            if (args[5] is not LispFunction)
                throw new LispErrorException(new LispTypeError(
                    "DOTNET:%DEFINE-CLASS: ctor-body must be a function",
                    args[5]));
            ctorBody = args[5];
        }

        List<(string, Type, bool)>? propertySpecs = null;
        if (args.Length >= 7 && args[6] != Nil.Instance)
        {
            propertySpecs = new List<(string, Type, bool)>();
            var cur = args[6];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each property spec must be a (name type-name &optional notify) list",
                        c.Car));
                var nameObj = spec.Car;
                var rest = spec.Cdr;
                var typeObj = rest is Cons c2 ? c2.Car : Nil.Instance;
                var notifyObj = (rest is Cons c2a && c2a.Cdr is Cons c3)
                    ? c3.Car : Nil.Instance;

                string pname = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                bool notify = notifyObj != Nil.Instance;
                propertySpecs.Add((pname, ResolveDotNetType(tname), notify));
                cur = c.Cdr;
            }
        }

        List<Type>? interfaceSpecs = null;
        if (args.Length >= 8 && args[7] != Nil.Instance)
        {
            interfaceSpecs = new List<Type>();
            var cur = args[7];
            while (cur is Cons c)
            {
                var entry = c.Car;
                string tname = entry switch
                {
                    LispString ls => ls.Value,
                    _ => entry.ToString() ?? ""
                };
                interfaceSpecs.Add(ResolveDotNetType(tname));
                cur = c.Cdr;
            }
        }

        List<(string, Type)>? eventSpecs = null;
        if (args.Length >= 9 && args[8] != Nil.Instance)
        {
            eventSpecs = new List<(string, Type)>();
            var cur = args[8];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each event spec must be a (name delegate-type-name) list",
                        c.Car));
                var nameObj = spec.Car;
                var typeObj = spec.Cdr is Cons c2 ? c2.Car : Nil.Instance;

                string ename = nameObj switch
                {
                    LispString ls => ls.Value,
                    _ => nameObj.ToString() ?? ""
                };
                string tname = typeObj switch
                {
                    LispString ls => ls.Value,
                    _ => typeObj.ToString() ?? ""
                };
                eventSpecs.Add((ename, ResolveDotNetType(tname)));
                cur = c.Cdr;
            }
        }

        List<Type>? userCtorParamTypes = null;
        if (args.Length >= 10 && args[9] != Nil.Instance)
        {
            userCtorParamTypes = new List<Type>();
            var cur = args[9];
            while (cur is Cons c)
            {
                string tname = c.Car switch
                {
                    LispString ls => ls.Value,
                    _ => c.Car.ToString() ?? ""
                };
                userCtorParamTypes.Add(ResolveDotNetType(tname));
                cur = c.Cdr;
            }
        }

        List<int>? baseCtorArgIndices = null;
        if (args.Length >= 11 && args[10] != Nil.Instance)
        {
            baseCtorArgIndices = new List<int>();
            var cur = args[10];
            while (cur is Cons c)
            {
                if (c.Car is not Fixnum fi)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: base-ctor-arg-indices must be a list of integers",
                        c.Car));
                baseCtorArgIndices.Add((int)fi.Value);
                cur = c.Cdr;
            }
        }

        // arg 11: ctor-specs-list: list of (lambda param-types base-arg-indices) triples.
        // When non-nil, overrides the single-ctor path (args 5/9/10).
        List<Emitter.DynamicClassBuilder.CtorSpec>? ctorSpecs = null;
        if (args.Length >= 12 && args[11] != Nil.Instance)
        {
            ctorSpecs = new List<Emitter.DynamicClassBuilder.CtorSpec>();
            var cur = args[11];
            while (cur is Cons c)
            {
                if (c.Car is not Cons spec)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: each ctor-spec must be a (lambda param-types base-arg-indices) list",
                        c.Car));
                var lambdaObj = spec.Car;
                var rest1 = spec.Cdr;
                var paramTypesObj = rest1 is Cons r1 ? r1.Car : Nil.Instance;
                var baseIndicesObj = (rest1 is Cons r1b && r1b.Cdr is Cons r2b) ? r2b.Car : Nil.Instance;

                if (lambdaObj is not LispFunction && lambdaObj != Nil.Instance)
                    throw new LispErrorException(new LispTypeError(
                        "DOTNET:%DEFINE-CLASS: ctor-spec body must be a function or nil", lambdaObj));

                LispObject? ctorLambda = lambdaObj == Nil.Instance ? null : lambdaObj;

                var paramTypes = new List<Type>();
                var pcur = paramTypesObj;
                while (pcur is Cons pc)
                {
                    string ptname = pc.Car switch
                    {
                        LispString ls => ls.Value,
                        _ => pc.Car.ToString() ?? ""
                    };
                    paramTypes.Add(ResolveDotNetType(ptname));
                    pcur = pc.Cdr;
                }

                var baseIndices = new List<int>();
                var bcur = baseIndicesObj;
                while (bcur is Cons bc)
                {
                    if (bc.Car is not Fixnum bfi)
                        throw new LispErrorException(new LispTypeError(
                            "DOTNET:%DEFINE-CLASS: ctor-spec base-arg-indices must be integers", bc.Car));
                    baseIndices.Add((int)bfi.Value);
                    bcur = bc.Cdr;
                }

                ctorSpecs.Add(new Emitter.DynamicClassBuilder.CtorSpec(
                    ctorLambda,
                    paramTypes.Count > 0 ? (IReadOnlyList<Type>)paramTypes : null,
                    baseIndices.Count > 0 ? (IReadOnlyList<int>)baseIndices : null));
                cur = c.Cdr;
            }
        }

        try
        {
            var type = Emitter.DynamicClassBuilder.DefineMinimalClass(
                fullName, baseType, fields, attrs, methods, ctorBody, propertySpecs,
                interfaceSpecs, eventSpecs, userCtorParamTypes, baseCtorArgIndices,
                ctorSpecs);
            // Register as CLOS class so class-of/type-of/find-class work for instances.
            EnsureDotNetTypeClass(type);
            // Register by uppercase name for resolution from Lisp symbols (e.g. Animal → ANIMAL).
            _dotNetDynTypeByUpperName[type.Name] = type;
            if (type.FullName != null && type.FullName != type.Name)
                _dotNetDynTypeByUpperName[type.FullName] = type;
            return new LispString(type.FullName ?? fullName);
        }
        catch (ArgumentException ae)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:%DEFINE-CLASS: {ae.Message}"));
        }
    }

    // Cache: (selfType, methodName, paramTypeSig) → DynamicMethod for non-virtual base call.
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(Type, string, string), System.Reflection.Emit.DynamicMethod>
        _baseCallCache = new();

    /// <summary>(dotnet:call-base self "Method" &rest args)
    /// Call the base class implementation of a virtual method non-virtually
    /// (equivalent to C# base.Method(args)). self must be a dotcl-defined class
    /// instance; base type is self.GetType().BaseType.</summary>
    public static LispObject DotNetCallBase(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:CALL-BASE: requires at least 2 arguments (self method-name &rest args)"));
        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:CALL-BASE: first argument must be a .NET object", args[0]));

        var target    = dno.Value;
        var selfType  = target.GetType();
        var baseType  = selfType.BaseType
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:CALL-BASE: {selfType.FullName} has no base type"));
        string methodName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var callArgs  = LispArgsToDotNetGeneric(args.Skip(2).ToArray());

        // Find best-matching method on base type by name + arg count.
        var candidates = baseType.GetMethods(
            System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)
            .Where(m => m.Name == methodName && m.GetParameters().Length == callArgs.Length)
            .ToArray();
        if (candidates.Length == 0)
            throw new LispErrorException(new LispError(
                $"DOTNET:CALL-BASE: no method {baseType.Name}.{methodName} with {callArgs.Length} args"));
        var baseMethod = candidates[0];
        var paramInfos = baseMethod.GetParameters();

        // Convert args to expected parameter types.
        var convertedArgs = new object?[callArgs.Length];
        for (int i = 0; i < callArgs.Length; i++)
            convertedArgs[i] = callArgs[i] == null ? null
                : Convert.ChangeType(callArgs[i], paramInfos[i].ParameterType,
                    System.Globalization.CultureInfo.InvariantCulture);

        // Get or create a DynamicMethod that emits `call` (non-virtual) to baseMethod.
        var sig = string.Join(",", paramInfos.Select(p => p.ParameterType.FullName));
        var dm = _baseCallCache.GetOrAdd((selfType, methodName, sig), _ =>
        {
            var pTypes   = new[] { selfType }.Concat(paramInfos.Select(p => p.ParameterType)).ToArray();
            var dynMethod = new System.Reflection.Emit.DynamicMethod(
                "__base_" + methodName, baseMethod.ReturnType, pTypes, selfType, skipVisibility: true);
            var il = dynMethod.GetILGenerator();
            il.Emit(System.Reflection.Emit.OpCodes.Ldarg_0);
            for (int i = 0; i < paramInfos.Length; i++)
                il.Emit(System.Reflection.Emit.OpCodes.Ldarg, i + 1);
            il.Emit(System.Reflection.Emit.OpCodes.Call, baseMethod);
            il.Emit(System.Reflection.Emit.OpCodes.Ret);
            return dynMethod;
        });

        var allArgs = new object?[] { target }.Concat(convertedArgs).ToArray();
        var result  = dm.Invoke(null, allArgs);
        return DotNetToLisp(result);
    }

    public static LispObject DotNetBox(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:BOX: requires 2 arguments (value type-name)"));

        string typeName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var type = ResolveDotNetType(typeName);
        var converted = LispToDotNet(args[0], type);
        return new LispDotNetBoxed(converted!, type);
    }

    public static LispObject DotNetToStream(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:TO-STREAM: requires at least 1 argument"));

        System.IO.Stream netStream;
        if (args[0] is LispDotNetObject dno && dno.Value is System.IO.Stream s)
            netStream = s;
        else
            throw new LispErrorException(new LispTypeError(
                "DOTNET:TO-STREAM: argument must be a .NET Stream", args[0]));

        // Check for :binary keyword argument
        bool binary = false;
        for (int i = 1; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol kw && kw.Name == "BINARY" && args[i + 1] != Nil.Instance)
                binary = true;
        }

        if (binary)
            return new LispBinaryStream(netStream);

        var encoding = System.Text.Encoding.UTF8;

        var reader = new System.IO.StreamReader(netStream, encoding, false, 4096, leaveOpen: true);
        var writer = new System.IO.StreamWriter(netStream, encoding, 4096, leaveOpen: true)
        {
            AutoFlush = false
        };

        return new LispBidirectionalStream(reader, writer);
    }

    // --- Delegate marshal ---

    /// <summary>
    /// <lispdoc>(dotnet:make-delegate type-name function) -- Wrap a Lisp function as a .NET delegate. type-name is e.g. "System.Func`2[System.String,System.Boolean]". The delegate can be passed to any .NET method expecting that delegate type. LispFunction arguments are auto-converted via dotnet:call when the target parameter type is a delegate.</lispdoc>
    /// Wrap a Lisp <paramref name="fn"/> as a .NET delegate of <paramref name="delegateType"/>.
    /// Each call to the delegate marshals .NET args → LispObject, calls <paramref name="fn"/>,
    /// then marshals the LispObject result back to the delegate's return type.
    /// </summary>
    [LispDoc("DOTNET:MAKE-DELEGATE")]
    public static LispObject DotNetMakeDelegate(LispObject[] args)
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:MAKE-DELEGATE: requires 2 arguments (type-name function)"));

        string typeName = args[0] switch
        {
            LispString ls => ls.Value,
            Symbol s      => s.Name,
            _             => args[0].ToString() ?? ""
        };

        if (args[1] is not LispFunction fn)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:MAKE-DELEGATE: second argument must be a function", args[1]));

        var delegateType = ResolveDotNetType(typeName)
            ?? throw new LispErrorException(new LispProgramError(
                $"DOTNET:MAKE-DELEGATE: cannot resolve type '{typeName}'"));

        if (!typeof(Delegate).IsAssignableFrom(delegateType))
            throw new LispErrorException(new LispProgramError(
                $"DOTNET:MAKE-DELEGATE: '{typeName}' is not a delegate type"));

        return new LispDotNetObject(CreateLispDelegate(fn, delegateType));
    }

    /// <summary>
    /// Build a .NET delegate of <paramref name="delegateType"/> that, when invoked,
    /// marshals its arguments to LispObject[], calls <paramref name="fn"/>, and
    /// marshals the return value back to the delegate's return type.
    /// Uses Expression.Lambda — no raw IL required.
    /// </summary>
    internal static Delegate CreateLispDelegate(LispFunction fn, Type delegateType)
    {
        var invokeMethod = delegateType.GetMethod("Invoke")
            ?? throw new InvalidOperationException(
                $"CreateLispDelegate: {delegateType} has no Invoke method");

        var paramInfos  = invokeMethod.GetParameters();
        var returnType  = invokeMethod.ReturnType;

        // Expression parameters matching the delegate signature
        var parameters = paramInfos
            .Select(p => System.Linq.Expressions.Expression.Parameter(p.ParameterType, p.Name))
            .ToArray();

        // Box each arg to object then call DotNetToLisp
        var dotNetToLisp = typeof(Runtime).GetMethod(nameof(DotNetToLisp))!;
        var lispArgs = parameters
            .Select(p => (System.Linq.Expressions.Expression)
                System.Linq.Expressions.Expression.Call(
                    dotNetToLisp,
                    System.Linq.Expressions.Expression.Convert(p, typeof(object))))
            .ToArray();

        // fn.Invoke(new LispObject[] { ... })
        var argsArray = System.Linq.Expressions.Expression.NewArrayInit(
            typeof(LispObject), lispArgs);
        var invoke = typeof(LispFunction).GetMethod(nameof(LispFunction.Invoke))!;
        var callFn = System.Linq.Expressions.Expression.Call(
            System.Linq.Expressions.Expression.Constant(fn),
            invoke,
            argsArray);

        System.Linq.Expressions.Expression body;
        if (returnType == typeof(void))
        {
            // Action<…>: discard return value
            body = System.Linq.Expressions.Expression.Block(
                callFn,
                System.Linq.Expressions.Expression.Empty());
        }
        else
        {
            // Func<…,TResult>: marshal LispObject result → TResult
            var lispToDotNet = typeof(Runtime)
                .GetMethod(nameof(LispToDotNet), new[] { typeof(LispObject), typeof(Type) })!;
            var converted = System.Linq.Expressions.Expression.Call(
                lispToDotNet,
                callFn,
                System.Linq.Expressions.Expression.Constant(returnType));
            body = System.Linq.Expressions.Expression.Convert(converted, returnType);
        }

        return System.Linq.Expressions.Expression.Lambda(delegateType, body, parameters)
            .Compile();
    }

    /// <summary>
    /// <lispdoc>(dotnet:call-out type-or-obj "Method" &amp;rest in-args) -- Call a .NET method that has out/ref parameters. type-or-obj is a type-name string for static calls, or a .NET object for instance calls. in-args supplies only the non-out parameters. Returns multiple values: the method's return value (T for void), followed by each out/ref parameter value in declaration order. Example: (multiple-value-bind (ok n) (dotnet:call-out "System.Int32" "TryParse" "42") ...)</lispdoc>
    /// Invoke a .NET static or instance method that has <c>out</c>/<c>ref</c> parameters.
    /// Supply only the in (non-out) arguments from Lisp; out positions are filled automatically.
    /// Returns multiple values: return-value (T for void) followed by each out/ref value.
    /// </summary>
    [LispDoc("DOTNET:CALL-OUT")]
    public static LispObject DotNetCallOut(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:CALL-OUT: requires type-or-obj method-name &rest in-args"));

        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var lispInArgs   = args.Skip(2).ToArray();

        System.Reflection.MethodInfo method;
        object? target;
        Type    type;

        if (args[0] is LispDotNetObject dno)
        {
            // Instance call
            target = dno.Value;
            type   = target.GetType();
            method = FindOutMethod(type, memberName, lispInArgs.Length,
                         System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance)
                     ?? throw new LispErrorException(new LispError(
                         $"DOTNET:CALL-OUT: no instance method {type.Name}.{memberName} " +
                         $"with {lispInArgs.Length} in-parameter(s)"));
        }
        else
        {
            // Static call
            string typeName = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
            type   = ResolveDotNetType(typeName);
            target = null;
            method = FindOutMethod(type, memberName, lispInArgs.Length,
                         System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
                     ?? throw new LispErrorException(new LispError(
                         $"DOTNET:CALL-OUT: no static method {typeName}.{memberName} " +
                         $"with {lispInArgs.Length} in-parameter(s)"));
        }

        var paramInfos = method.GetParameters();
        var callArgs   = new object?[paramInfos.Length];
        int inIdx = 0;
        for (int i = 0; i < paramInfos.Length; i++)
        {
            var p = paramInfos[i];
            if (p.IsOut || (p.ParameterType.IsByRef && !p.IsIn))
            {
                callArgs[i] = null; // placeholder; filled by .NET on return
            }
            else
            {
                var elemType = p.ParameterType.IsByRef ? p.ParameterType.GetElementType()! : p.ParameterType;
                callArgs[i] = LispToDotNet(lispInArgs[inIdx++], elemType);
            }
        }

        object? returnVal;
        try
        {
            returnVal = method.Invoke(target, callArgs);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:CALL-OUT {type.Name}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }

        var mvVals = new System.Collections.Generic.List<LispObject>();
        mvVals.Add(method.ReturnType == typeof(void) ? T.Instance : DotNetToLisp(returnVal));
        for (int i = 0; i < paramInfos.Length; i++)
        {
            if (paramInfos[i].IsOut || paramInfos[i].ParameterType.IsByRef)
                mvVals.Add(DotNetToLisp(callArgs[i]));
        }
        return MultipleValues.Values(mvVals.ToArray());
    }

    private static System.Reflection.MethodInfo? FindOutMethod(
        Type type, string name, int inArgCount, System.Reflection.BindingFlags flags)
    {
        return type.GetMethods(flags)
            .Where(m => m.Name == name)
            .FirstOrDefault(m => {
                var ps = m.GetParameters();
                var inCount = ps.Count(p => !p.IsOut && !(p.ParameterType.IsByRef && !p.IsIn));
                return inCount == inArgCount;
            });
    }

    /// <summary>
    /// <lispdoc>(dotnet:static-generic "TypeName" "MethodName" type-args-list &amp;rest args) -- Call a generic static method with explicit type arguments. type-args-list is a Lisp list of type-name strings. Uses type-guided conversion so Lisp lambdas are auto-marshaled to the concrete delegate type. Example: (dotnet:static-generic "System.Linq.Enumerable" "Where" '("System.Int32") list (lambda (x) (> x 3)))</lispdoc>
    /// Invoke a generic static method with explicit type arguments.
    /// <paramref name="args"/>[0] = type name, [1] = method name,
    /// [2] = Lisp list of type-arg strings, [3..] = method arguments.
    /// Uses type-guided conversion, so LispFunction args are auto-marshaled to
    /// the concrete delegate types inferred from <c>MakeGenericMethod</c>.
    /// </summary>
    [LispDoc("DOTNET:STATIC-GENERIC")]
    public static LispObject DotNetStaticGeneric(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:STATIC-GENERIC: requires type-name method-name type-args-list &rest args"));

        string typeName   = args[0] switch { LispString ls => ls.Value, _ => args[0].ToString() ?? "" };
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var    type       = ResolveDotNetType(typeName);
        var    lispArgs   = args.Skip(3).ToArray();

        // Parse type-args list
        var typeArgNames = new System.Collections.Generic.List<string>();
        var cursor = args[2];
        while (cursor is Cons c)
        {
            typeArgNames.Add(c.Car switch { LispString ls => ls.Value, _ => c.Car.ToString() ?? "" });
            cursor = c.Cdr;
        }

        // Find the generic method definition matching name + arity
        var methodDef = type.GetMethods(
                System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static)
            .Where(m => m.Name == memberName
                     && m.IsGenericMethodDefinition
                     && m.GetGenericArguments().Length == typeArgNames.Count
                     && m.GetParameters().Length == lispArgs.Length)
            .FirstOrDefault()
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:STATIC-GENERIC: no generic static method {typeName}.{memberName} " +
                $"with {typeArgNames.Count} type arg(s) and {lispArgs.Length} parameter(s)"));

        var concreteTypes  = typeArgNames.Select(ResolveDotNetType).ToArray();
        var concreteMethod = methodDef.MakeGenericMethod(concreteTypes);
        var paramInfos     = concreteMethod.GetParameters();

        var callArgs = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            callArgs[i] = LispToDotNet(lispArgs[i], paramInfos[i].ParameterType);

        try
        {
            var result = concreteMethod.Invoke(null, callArgs);
            return DotNetToLisp(result);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:STATIC-GENERIC {typeName}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }

    /// <summary>(dotnet:invoke-generic object "Method" '("TypeArg" ...) &rest args)
    /// Instance counterpart of dotnet:static-generic: invoke a generic instance method on
    /// OBJECT, instantiating it with the given type-argument names (MakeGenericMethod)
    /// before the call (e.g. ContentManager.Load&lt;Texture2D&gt;) (dotcl/dotcl#23).</summary>
    public static LispObject DotNetInvokeGeneric(LispObject[] args)
    {
        if (args.Length < 3)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:INVOKE-GENERIC: requires object method-name type-args-list &rest args"));
        if (args[0] is not LispDotNetObject dno)
            throw new LispErrorException(new LispTypeError(
                "DOTNET:INVOKE-GENERIC: first argument must be a .NET object", args[0]));

        var    target     = dno.Value;
        var    type       = target.GetType();
        string memberName = args[1] switch { LispString ls => ls.Value, _ => args[1].ToString() ?? "" };
        var    lispArgs   = args.Skip(3).ToArray();

        // Parse type-args list
        var typeArgNames = new System.Collections.Generic.List<string>();
        var cursor = args[2];
        while (cursor is Cons c)
        {
            typeArgNames.Add(c.Car switch { LispString ls => ls.Value, _ => c.Car.ToString() ?? "" });
            cursor = c.Cdr;
        }

        // Find the generic instance method matching name + type-arg arity + param count
        var methodDef = type.GetMethods(
                System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance)
            .Where(m => m.Name == memberName
                     && m.IsGenericMethodDefinition
                     && m.GetGenericArguments().Length == typeArgNames.Count
                     && m.GetParameters().Length == lispArgs.Length)
            .FirstOrDefault()
            ?? throw new LispErrorException(new LispError(
                $"DOTNET:INVOKE-GENERIC: no generic instance method {type.Name}.{memberName} " +
                $"with {typeArgNames.Count} type arg(s) and {lispArgs.Length} parameter(s)"));

        var concreteTypes  = typeArgNames.Select(ResolveDotNetType).ToArray();
        var concreteMethod = methodDef.MakeGenericMethod(concreteTypes);
        var paramInfos     = concreteMethod.GetParameters();

        var callArgs = new object?[lispArgs.Length];
        for (int i = 0; i < lispArgs.Length; i++)
            callArgs[i] = LispToDotNet(lispArgs[i], paramInfos[i].ParameterType);

        try
        {
            var result = concreteMethod.Invoke(target, callArgs);
            return DotNetToLisp(result);
        }
        catch (System.Reflection.TargetInvocationException tie)
        {
            throw new LispErrorException(new LispError(
                $"DOTNET:INVOKE-GENERIC {type.Name}.{memberName}: {tie.InnerException?.Message ?? tie.Message}"));
        }
    }
}
