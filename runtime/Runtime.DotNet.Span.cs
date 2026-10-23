using System;
using System.Collections.Generic;
using System.Linq;

namespace DotCL;

// Span<T> / ReadOnlySpan<T> / Memory<T> / ReadOnlyMemory<T> at the dotnet: boundary.
//
// A Lisp vector with numeric or character storage is handed over as a view of that
// storage, not a copy: a Span the method writes into is the vector itself. A Span
// cannot be boxed, so a method taking or returning one is called through an emitted
// stub that receives each Span argument as the matching (ReadOnly)Memory<T> and
// takes its .Span on the stack. A returned Span is copied into a fresh Lisp vector.
public static partial class Runtime
{
    internal static bool IsByRefLikeType(Type t)
    {
#if NETSTANDARD2_0
        foreach (var a in t.GetCustomAttributesData())
            if (a.AttributeType.FullName == "System.Runtime.CompilerServices.IsByRefLikeAttribute")
                return true;
        return false;
#else
        return t.IsByRefLike;
#endif
    }

    private static bool IsSpanType(Type t, out Type elem, out bool readOnly)
    {
        elem = typeof(void); readOnly = false;
        if (!t.IsGenericType) return false;
        var def = t.GetGenericTypeDefinition();
        if (def == typeof(Span<>)) { }
        else if (def == typeof(ReadOnlySpan<>)) readOnly = true;
        else return false;
        elem = t.GetGenericArguments()[0];
        return true;
    }

    private static bool IsMemoryType(Type t, out Type elem, out bool readOnly)
    {
        elem = typeof(void); readOnly = false;
        if (!t.IsGenericType) return false;
        var def = t.GetGenericTypeDefinition();
        if (def == typeof(Memory<>)) { }
        else if (def == typeof(ReadOnlyMemory<>)) readOnly = true;
        else return false;
        elem = t.GetGenericArguments()[0];
        return true;
    }

    /// <summary>True when M takes or returns a ref struct, so reflection cannot call it.</summary>
    internal static bool NeedsSpanStub(System.Reflection.MethodInfo m)
    {
        if (IsByRefLikeType(m.ReturnType)) return true;
        foreach (var p in m.GetParameters())
        {
            var t = p.ParameterType.IsByRef ? p.ParameterType.GetElementType()! : p.ParameterType;
            if (IsByRefLikeType(t)) return true;
        }
        return false;
    }

    /// <summary>Order among same-arity overloads: ones taking neither spans nor
    /// memories first (what was chosen before spans could be passed), then span
    /// ones (their results come back as Lisp data), then Memory ones.</summary>
    internal static int SpanOverloadRank(System.Reflection.MethodInfo m)
    {
        if (NeedsSpanStub(m)) return 1;
        foreach (var p in m.GetParameters())
            if (IsMemoryType(p.ParameterType, out _, out _)) return 2;
        return 0;
    }

    /// <summary>The storage a Lisp vector's elements live in, following displacement:
    /// elements [OFFSET, OFFSET+LENGTH) of ARR.</summary>
    private static bool TryVectorStorage(LispVector v, out Array? arr, out int offset, out int length)
    {
        arr = null; offset = 0; length = v.Length;
        if (v._dimensions != null) return false;
        while (v._displacedTo is { } to)
        {
            offset += v.DisplacedOffset;
            v = to;
        }
        if (v._numKind == 0 || v._numData is not { } a || offset + length > v._numLen) return false;
        arr = a;
        return true;
    }

    /// <summary>ARG's own storage when it is an ARRAYTYPE (a T[], or any for System.Array):
    /// a string, or a vector without a fill pointer or displacement whose storage is
    /// exactly that array, with no narrower element type over it. NONSIMPLE is set
    /// instead for a vector with such storage that is not passable as it.</summary>
    internal static Array? VectorAsArray(LispObject arg, Type arrayType, out bool nonSimple)
    {
        nonSimple = false;
        bool anyArray = arrayType == typeof(Array);
        switch (arg)
        {
            case LispString ls when anyArray || arrayType == typeof(char[]):
                return ls.RawChars;
            case LispVector v when v._dimensions == null:
                if (v._numData is { } a)
                {
                    if ((!anyArray && a.GetType() != arrayType) || v._numNarrow != 0) return null;
                    if (!v.HasFillPointer && v._numLen == v.Length) return a;
                    nonSimple = true;
                    return null;
                }
                nonSimple = v._displacedTo != null
                            && TryVectorStorage(v, out var st, out _, out _)
                            && (anyArray || st!.GetType() == arrayType) && StorageNarrow(v) == 0;
                return null;
        }
        return null;
    }

    internal static LispErrorException NonSimpleArrayArgError(LispObject arg, Type arrayType)
        => new LispErrorException(new LispTypeError(
            $"Cannot pass a vector with a fill pointer or displacement as {arrayType.Name}: "
            + "the method would get a copy and writes into it would be lost. "
            + "Pass a simple vector (COPY-SEQ, then REPLACE the result back).", arg));

    /// <summary>The index in LISPARGS of an argument to an array parameter of M that has
    /// the array's storage but cannot be passed as it (see VectorAsArray), or -1.</summary>
    internal static int NonSimpleArrayArg(System.Reflection.MethodBase m, LispObject[] lispArgs, bool outAware = false)
    {
        int k = 0;
        foreach (var p in m.GetParameters())
        {
            if (outAware && p.IsOut) continue;
            if (k >= lispArgs.Length) break;
            var t = p.ParameterType;
            var arg = lispArgs[k++];
            if ((t.IsArray || t == typeof(Array)) && arg is LispVector)
            {
                VectorAsArray(arg, t, out bool nonSimple);
                if (nonSimple) return k - 1;
            }
        }
        return -1;
    }

    private static sbyte StorageNarrow(LispVector v)
    {
        while (v._displacedTo is { } to) v = to;
        return v._numNarrow;
    }

    private static object MakeMemory(Array arr, int off, int len, bool ro) => arr switch
    {
        byte[] a => ro ? (object)new ReadOnlyMemory<byte>(a, off, len) : new Memory<byte>(a, off, len),
        char[] a => ro ? (object)new ReadOnlyMemory<char>(a, off, len) : new Memory<char>(a, off, len),
        double[] a => ro ? (object)new ReadOnlyMemory<double>(a, off, len) : new Memory<double>(a, off, len),
        float[] a => ro ? (object)new ReadOnlyMemory<float>(a, off, len) : new Memory<float>(a, off, len),
        int[] a => ro ? (object)new ReadOnlyMemory<int>(a, off, len) : new Memory<int>(a, off, len),
        long[] a => ro ? (object)new ReadOnlyMemory<long>(a, off, len) : new Memory<long>(a, off, len),
        _ => Activator.CreateInstance(
                 (ro ? typeof(ReadOnlyMemory<>) : typeof(Memory<>)).MakeGenericType(arr.GetType().GetElementType()!),
                 arr, off, len)!,
    };

    private static bool IsNumericElement(Type t) => t.IsPrimitive && t != typeof(char) && t != typeof(bool);

    /// <summary>Whether ARG can stand for a (ReadOnly)Span or (ReadOnly)Memory of ELEM.
    /// Decided without converting anything: a failed conversion signals, and a
    /// HANDLER-CASE around the call would take that as the call's error while
    /// overloads are still being tried. A writable view has to be ARG's own storage;
    /// a read-only one may be a copy of a list, a general vector or a numeric vector
    /// of another width.</summary>
    private static bool SpanArgFits(LispObject arg, Type elem, bool ro)
    {
        switch (arg)
        {
            case LispString:
                return elem == typeof(char);
            case LispVector v:
                if (TryVectorStorage(v, out var arr, out _, out _))
                {
                    var have = arr!.GetType().GetElementType()!;
                    return (have == elem && (ro || StorageNarrow(v) == 0))
                        || (ro && IsNumericElement(have) && IsNumericElement(elem));
                }
                return ro && v._dimensions == null && v._numKind == 0 && v._bitData == null;
            case Cons:
            case Nil:
                return ro;
            case LispDotNetObject d:
                var t = d.Value.GetType();
                return (t.IsArray && t.GetElementType() == elem)
                    || t == typeof(Memory<>).MakeGenericType(elem)
                    || (ro && t == typeof(ReadOnlyMemory<>).MakeGenericType(elem))
                    || (ro && elem == typeof(char) && d.Value is string);
            default:
                return false;
        }
    }

    /// <summary>A (ReadOnly)Memory&lt;ELEM&gt; for ARG when SpanArgFits: a view of its
    /// own storage, or for a read-only one without such storage, of a copy.</summary>
    private static bool TryLispToMemory(LispObject arg, Type elem, bool ro, out object? memory)
    {
        memory = null;
        if (!SpanArgFits(arg, elem, ro)) return false;
        switch (arg)
        {
            case LispString ls:
                memory = !ro ? (object)new Memory<char>(ls.RawChars)
                       : ls.CharsOrNull is { } chars ? new ReadOnlyMemory<char>(chars)
                       : (object)ls.StrOrNull!.AsMemory();
                return true;
            case LispVector v when TryVectorStorage(v, out var arr, out int off, out int len)
                                   && arr!.GetType().GetElementType() == elem:
                memory = MakeMemory(arr, off, len, ro);
                return true;
            case LispDotNetObject d:
                memory = d.Value switch
                {
                    Array a => MakeMemory(a, 0, a.Length, ro),
                    string str => str.AsMemory(),
                    var m when ro && m.GetType().GetGenericTypeDefinition() == typeof(Memory<>)
                        => LispToDotNet(arg, typeof(ReadOnlyMemory<>).MakeGenericType(elem)),
                    var m => m,
                };
                return true;
        }
        if (LispToDotNet(arg, elem.MakeArrayType()) is not Array copy) return false;
        memory = MakeMemory(copy, 0, copy.Length, true);
        return true;
    }

    /// <summary>Whether every Span / Memory parameter of M can take its argument
    /// (see SpanArgFits). With OUTAWARE, LISPARGS skips M's out parameters.</summary>
    internal static bool SpanParamsFit(System.Reflection.MethodBase m, LispObject[] lispArgs, bool outAware = false)
    {
        if (NonSimpleArrayArg(m, lispArgs, outAware) >= 0) return false;
        int k = 0;
        foreach (var p in m.GetParameters())
        {
            if (outAware && p.IsOut) continue;
            if (k >= lispArgs.Length) return true;
            var t = p.ParameterType.IsByRef ? p.ParameterType.GetElementType()! : p.ParameterType;
            var arg = lispArgs[k++];
            if ((IsSpanType(t, out var elem, out bool ro) || IsMemoryType(t, out elem, out ro))
                && !SpanArgFits(arg, elem, ro))
                return false;
        }
        return true;
    }

    /// <summary>An overload to try first among same-arity ones: its Span / Memory
    /// parameters take their arguments, and a string or array parameter is not
    /// given something of another kind. Only a filter for picking the overload;
    /// the conversion itself still decides.</summary>
    internal static bool PlausibleOverload(System.Reflection.MethodBase m, LispObject[] lispArgs, bool outAware)
    {
        if (!SpanParamsFit(m, lispArgs, outAware)) return false;
        int k = 0;
        foreach (var p in m.GetParameters())
        {
            if (outAware && p.IsOut) continue;
            if (k >= lispArgs.Length) break;
            var t = p.ParameterType.IsByRef ? p.ParameterType.GetElementType()! : p.ParameterType;
            var arg = lispArgs[k++];
            if (arg is Nil || arg is LispDotNetObject) continue;
            if (t == typeof(string) && !(arg is LispString || (arg is LispVector cv && cv.IsCharVector)))
                return false;
            if (t.IsArray && !(arg is Cons || arg is LispVector || arg is LispString))
                return false;
            if ((IsNumericElement(t) || t == typeof(decimal)) && (arg is Cons || arg is LispVector || arg is LispString))
                return false;
        }
        return true;
    }

    /// <summary>LispToDotNet for a Memory&lt;T&gt; / ReadOnlyMemory&lt;T&gt; target; null when
    /// TARGETTYPE is not one or ARG does not convert.</summary>
    private static object? LispToMemoryTarget(LispObject arg, Type targetType)
        => IsMemoryType(targetType, out var elem, out bool ro)
           && TryLispToMemory(arg, elem, ro, out var mem) ? mem : null;

    /// <summary>LispToDotNet for a method parameter. A Span&lt;T&gt; / ReadOnlySpan&lt;T&gt;
    /// parameter gets its (ReadOnly)Memory&lt;T&gt;, which InvokeMethod's stub turns
    /// into the span. An array parameter does not take a copy of a vector that has
    /// the array's storage (the method's writes would be lost).</summary>
    public static object? LispToDotNetParam(LispObject arg, Type paramType)
    {
        if (arg is LispVector && (paramType.IsArray || paramType == typeof(Array)))
        {
            if (VectorAsArray(arg, paramType, out bool nonSimple) is { } own) return own;
            if (nonSimple) throw NonSimpleArrayArgError(arg, paramType);
        }
        if (IsSpanType(paramType, out var elem, out bool ro))
        {
            if (TryLispToMemory(arg, elem, ro, out var mem)) return mem;
            throw new LispErrorException(new LispTypeError(
                $"Cannot convert {arg.GetType().Name} to {(ro ? "ReadOnlySpan" : "Span")}<{elem.Name}>"
                + (ro ? "" : ": a writable span needs a vector whose storage has that element type"), arg));
        }
        return LispToDotNet(arg, paramType);
    }

    /// <summary>MethodInfo.Invoke, or the span stub for a method reflection cannot
    /// call. ARGS holds by-ref results afterwards either way, and an exception the
    /// method throws arrives wrapped in TargetInvocationException either way.</summary>
    internal static object? InvokeMethod(System.Reflection.MethodInfo m, object? target, object?[] args)
    {
        if (!NeedsSpanStub(m)) return m.Invoke(target, args);
#if DOTCL_EMIT
        var stub = _spanStubs.GetOrAdd(m, BuildSpanStub);
        try { return stub(target, args); }
        catch (Exception e) when (!IsLispOriginatedException(e))
        {
            throw new System.Reflection.TargetInvocationException(e);
        }
#else
        throw new LispErrorException(new LispError(
            $"{m.DeclaringType?.Name}.{m.Name} takes or returns a Span, which needs the emitting runtime"));
#endif
    }

    /// <summary>A copy of a returned span's elements as a Lisp vector of the matching
    /// element type.</summary>
    internal static LispObject SpanArrayToLisp(Array a) => a switch
    {
        char[] c => new LispString(c),
        byte[] => LispVector.WithNumStorage("UNSIGNED-BYTE-8", a, false),
        ushort[] => LispVector.WithNumStorage("UNSIGNED-BYTE-16", a, false),
        uint[] => LispVector.WithNumStorage("UNSIGNED-BYTE-32", a, true),
        ulong[] => LispVector.WithNumStorage("UNSIGNED-BYTE-64", a, false),
        sbyte[] => LispVector.WithNumStorage("SIGNED-BYTE-8", a, true),
        short[] => LispVector.WithNumStorage("SIGNED-BYTE-16", a, true),
        int[] => LispVector.WithNumStorage("SIGNED-BYTE-32", a, false),
        long[] => LispVector.WithNumStorage("SIGNED-BYTE-64", a, false),
        float[] => LispVector.WithNumStorage("SINGLE-FLOAT", a, false),
        double[] => LispVector.WithNumStorage("DOUBLE-FLOAT", a, false),
        _ => new LispVector(a.Cast<object?>().Select(DotNetToLisp).ToArray()),
    };

#if DOTCL_EMIT
    private static readonly System.Collections.Concurrent.ConcurrentDictionary<
        System.Reflection.MethodInfo, Func<object?, object?[], object?>> _spanStubs = new();

    private static Func<object?, object?[], object?> BuildSpanStub(System.Reflection.MethodInfo m)
    {
        var dm = new System.Reflection.Emit.DynamicMethod(
            "span_" + m.Name, typeof(object), new[] { typeof(object), typeof(object[]) },
            typeof(Runtime).Module, skipVisibility: true);
        var il = dm.GetILGenerator();
        var ps = m.GetParameters();
        var byRefLocals = new System.Reflection.Emit.LocalBuilder?[ps.Length];

        if (!m.IsStatic)
        {
            var decl = m.DeclaringType!;
            il.Emit(System.Reflection.Emit.OpCodes.Ldarg_0);
            if (decl.IsValueType) il.Emit(System.Reflection.Emit.OpCodes.Unbox, decl);
            else il.Emit(System.Reflection.Emit.OpCodes.Castclass, decl);
        }

        for (int i = 0; i < ps.Length; i++)
        {
            var pt = ps[i].ParameterType;
            if (pt.IsByRef)
            {
                var et = pt.GetElementType()!;
                if (IsByRefLikeType(et)) throw UnsupportedRefStruct(m, et);
                var loc = il.DeclareLocal(et);
                byRefLocals[i] = loc;
                if (!ps[i].IsOut)
                {
                    LoadArg(il, i);
                    il.Emit(et.IsValueType ? System.Reflection.Emit.OpCodes.Unbox_Any
                                           : System.Reflection.Emit.OpCodes.Castclass, et);
                    il.Emit(System.Reflection.Emit.OpCodes.Stloc, loc);
                }
                il.Emit(System.Reflection.Emit.OpCodes.Ldloca, loc);
            }
            else if (IsSpanType(pt, out var elem, out bool ro))
            {
                var memType = (ro ? typeof(ReadOnlyMemory<>) : typeof(Memory<>)).MakeGenericType(elem);
                var mem = il.DeclareLocal(memType);
                LoadArg(il, i);
                il.Emit(System.Reflection.Emit.OpCodes.Unbox_Any, memType);
                il.Emit(System.Reflection.Emit.OpCodes.Stloc, mem);
                il.Emit(System.Reflection.Emit.OpCodes.Ldloca, mem);
                il.Emit(System.Reflection.Emit.OpCodes.Call, memType.GetProperty("Span")!.GetGetMethod()!);
            }
            else if (IsByRefLikeType(pt))
                throw UnsupportedRefStruct(m, pt);
            else
            {
                LoadArg(il, i);
                il.Emit(pt.IsValueType ? System.Reflection.Emit.OpCodes.Unbox_Any
                                       : System.Reflection.Emit.OpCodes.Castclass, pt);
            }
        }

        bool callvirt = !m.IsStatic && !m.DeclaringType!.IsValueType;
        il.Emit(callvirt ? System.Reflection.Emit.OpCodes.Callvirt : System.Reflection.Emit.OpCodes.Call, m);

        var rt = m.ReturnType;
        if (rt == typeof(void))
            il.Emit(System.Reflection.Emit.OpCodes.Ldnull);
        else if (IsSpanType(rt, out _, out _))
        {
            var r = il.DeclareLocal(rt);
            il.Emit(System.Reflection.Emit.OpCodes.Stloc, r);
            il.Emit(System.Reflection.Emit.OpCodes.Ldloca, r);
            il.Emit(System.Reflection.Emit.OpCodes.Call, rt.GetMethod("ToArray", Type.EmptyTypes)!);
            il.Emit(System.Reflection.Emit.OpCodes.Call,
                typeof(Runtime).GetMethod(nameof(SpanArrayToLisp),
                    System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.NonPublic)!);
        }
        else if (IsByRefLikeType(rt) || rt.IsByRef)
            throw UnsupportedRefStruct(m, rt);
        else if (rt.IsValueType)
            il.Emit(System.Reflection.Emit.OpCodes.Box, rt);

        for (int i = 0; i < ps.Length; i++)
        {
            if (byRefLocals[i] is not { } loc) continue;
            il.Emit(System.Reflection.Emit.OpCodes.Ldarg_1);
            il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, i);
            il.Emit(System.Reflection.Emit.OpCodes.Ldloc, loc);
            if (loc.LocalType.IsValueType) il.Emit(System.Reflection.Emit.OpCodes.Box, loc.LocalType);
            il.Emit(System.Reflection.Emit.OpCodes.Stelem_Ref);
        }
        il.Emit(System.Reflection.Emit.OpCodes.Ret);
        return (Func<object?, object?[], object?>)dm.CreateDelegate(typeof(Func<object?, object?[], object?>));
    }

    private static void LoadArg(System.Reflection.Emit.ILGenerator il, int i)
    {
        il.Emit(System.Reflection.Emit.OpCodes.Ldarg_1);
        il.Emit(System.Reflection.Emit.OpCodes.Ldc_I4, i);
        il.Emit(System.Reflection.Emit.OpCodes.Ldelem_Ref);
    }

    private static LispErrorException UnsupportedRefStruct(System.Reflection.MethodInfo m, Type t)
        => new(new LispError(
            $"{m.DeclaringType?.Name}.{m.Name}: {t.Name} is a ref struct other than Span/ReadOnlySpan, which dotnet: cannot pass"));
#endif
}
