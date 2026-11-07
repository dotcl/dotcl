using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    // --- Method/field/type resolution ---

    private void EmitLdsfld(string name)
    {
        if (_fieldCache.TryGetValue(name, out var fi))
        {
            _il.Emit(OpCodes.Ldsfld, fi);
            return;
        }
        throw new Exception($"Unknown field: {name}");
    }

    private static FieldInfo ResolveInstanceField(string name)
    {
        if (_fieldCache.TryGetValue(name, out var fi)) return fi;
        throw new Exception($"Unknown field: {name}");
    }

    private static MethodInfo? _lazy__coverageHit;
    private static MethodInfo? _coverageHit => _lazy__coverageHit ??=
        CallCoverage.Enabled ? typeof(CallCoverage).GetMethod("Hit") : null;
    private bool _afterTailPrefix;

    // Nothing may sit between a tail. prefix and its call, so that call is
    // only counted as emitted.
    private void EmitCoverageHit(string name)
    {
        int id = CallCoverage.Emitted(name);
        if (_afterTailPrefix) { _afterTailPrefix = false; return; }
        if (id < 0) return;
        _il.Emit(OpCodes.Ldc_I4, id);
        _il.Emit(OpCodes.Call, _coverageHit!);
    }

    private static bool SimdMethod(string name, out MethodInfo? mi)
    {
        return Tables.TrySimdMethod(name, out mi);
    }

    private void EmitCall(string name)
    {
        if (_methodCache.TryGetValue(name, out var mi) || SimdMethod(name, out mi))
        {
            _il.Emit(OpCodes.Call, mi!);
            return;
        }
        throw new Exception($"Unknown method: {name}");
    }

    private void EmitCallvirt(string name)
    {
        if (_runOnceInMemory && name.StartsWith("LispFunction.Invoke", StringComparison.Ordinal)
            && name.Length == "LispFunction.Invoke".Length + 1 && char.IsDigit(name[^1]))
        {
            _il.Emit(OpCodes.Callvirt, typeof(LispFunction).GetMethod("InvokeOnce" + name[^1])!);
            return;
        }
        if (_methodCache.TryGetValue(name, out var mi) || SimdMethod(name, out mi))
        {
            _il.Emit(OpCodes.Callvirt, mi!);
            return;
        }
        throw new Exception($"Unknown method for callvirt: {name}");
    }

    // Resolve a type/method for the direct path, returning null (so the caller
    // emits the dynamic fallback) for anything the direct codegen can't handle:
    // an unresolvable type (e.g. a runtime dotnet:define-class not yet defined at
    // assembly time) or no matching overload (e.g. a method reached only via
    // optional-arg defaults). Value-type receivers ARE handled (see EmitDirectReceiver).
    private MethodInfo? ResolveDirectTarget(string typeName, string methodName, Type[] paramTypes, out Type? type)
    {
        type = null;
        // The non-signalling lookup: this is a guess, and a signalling one would
        // be seen by the caller's handlers (and by the REPL debugger) as an error
        // the user made.
        var t = Runtime.TryResolveDotNetType(typeName);
        if (t == null) return null;
        var m = Runtime.ResolveTypedInstanceMember(t, methodName, paramTypes);
        if (m == null || Runtime.NeedsSpanStub(m)) return null;
        type = t;
        return m;
    }

    // Unwrap a receiver local to its static .NET type, leaving the `this` value on
    // the stack: a typed object reference for reference types, or a managed pointer
    // to the unboxed value for value types (via `unbox`). For value types the
    // caller must prefix the call with `constrained. type` so both the value type's
    // own methods and inherited virtuals (ToString, ...) dispatch correctly.
    // The exception is a non-virtual method declared on a reference type
    // (Object.GetType, Enum.HasFlag): its `this` is an object reference, and the
    // receiver's value is already one (the boxed value), so it is passed as is.
    private bool EmitDirectReceiver(LocalBuilder recv, Type type, MethodInfo method)
    {
        _il.EmitLoc(OpCodes.Ldloc, recv);
        _il.Emit(OpCodes.Ldtoken, type);
        _il.Emit(OpCodes.Call, typeof(Type).GetMethod("GetTypeFromHandle", new[] { typeof(RuntimeTypeHandle) })!);
        _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("DirectReceiver", new[] { typeof(LispObject), typeof(Type) })!);
        if (type.IsValueType)
        {
            if (!method.IsVirtual && method.DeclaringType is { IsValueType: false } declaring)
            {
                _il.Emit(OpCodes.Castclass, declaring);
                return false;
            }
            _il.Emit(OpCodes.Unbox, type);
            return true;
        }
        _il.Emit(OpCodes.Castclass, type);
        return false;
    }

    // Emit the instance call given the receiver kind. Reference receiver: plain
    // callvirt. Value-type receiver (this = managed pointer from `unbox`): a
    // virtual method (e.g. ToString) needs `constrained. type; callvirt`, while a
    // non-virtual one (most struct methods / property getters like get_Year) needs
    // a plain `call`: constrained.callvirt on a non-virtual method misdispatches.
    private void EmitTypedInstanceCall(MethodInfo method, bool valueRecv, Type type)
    {
        if (!valueRecv)
            _il.Emit(OpCodes.Callvirt, method);
        else if (method.IsVirtual)
        {
            _il.Emit(OpCodes.Constrained, type);
            _il.Emit(OpCodes.Callvirt, method);
        }
        else
            _il.Emit(OpCodes.Call, method);
    }

    // Dynamic fallback: Runtime.DotNetInvoke(new LispObject[]{ recv, "method", args... }).
    private void EmitDynamicInvokeFallback(LocalBuilder recv, string methodName, IReadOnlyList<LocalBuilder> args)
    {
        _il.Emit(OpCodes.Ldc_I4, 2 + args.Count);
        _il.Emit(OpCodes.Newarr, typeof(LispObject));
        _il.Emit(OpCodes.Dup); _il.Emit(OpCodes.Ldc_I4, 0);
        _il.EmitLoc(OpCodes.Ldloc, recv); _il.Emit(OpCodes.Stelem_Ref);
        _il.Emit(OpCodes.Dup); _il.Emit(OpCodes.Ldc_I4, 1);
        _il.Emit(OpCodes.Ldstr, methodName);
        _il.Emit(OpCodes.Newobj, typeof(LispString).GetConstructor(new[] { typeof(string) })!);
        _il.Emit(OpCodes.Stelem_Ref);
        for (int i = 0; i < args.Count; i++)
        {
            _il.Emit(OpCodes.Dup); _il.Emit(OpCodes.Ldc_I4, 2 + i);
            _il.EmitLoc(OpCodes.Ldloc, args[i]); _il.Emit(OpCodes.Stelem_Ref);
        }
        _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("DotNetInvoke", new[] { typeof(LispObject[]) })!);
    }

    private void EmitDotnetCallDirect(string typeName, string methodName)
    {
        // Stack on entry: [ receiver : LispObject ]. Direct dispatch for a
        // known-typed, zero-argument dotnet:invoke: unwrap the receiver to its
        // static .NET type and callvirt the resolved method: no runtime member
        // lookup / InvokeMember. Falls back to the dynamic path when the target
        // can't be resolved directly (see ResolveDirectTarget).
        var recvTmp = _il.DeclareLocal(typeof(LispObject));
        _il.EmitLoc(OpCodes.Stloc, recvTmp);

        var method = ResolveDirectTarget(typeName, methodName, Type.EmptyTypes, out var type);
        if (method == null || type == null)
        {
            EmitDynamicInvokeFallback(recvTmp, methodName, System.Array.Empty<LocalBuilder>());
            return;
        }

        bool valueRecv = EmitDirectReceiver(recvTmp, type, method);
        EmitTypedInstanceCall(method, valueRecv, type);
        EmitDotnetResultMarshal(method);
    }

    private void EmitDotnetCallDirectLocals(LispObject c)
    {
        // (:dotnet-call-direct-locals "Type" "Method" RECV-LOCAL (ARG-LOCAL...) (PARAM-TYPE...))
        // Typed direct dispatch for a known-typed dotnet:invoke with arguments: the
        // receiver and each argument are already in named locals (set by the
        // compiler with an empty stack between them), so no LispObject[] is
        // allocated. Resolve the exact overload by param types, marshal each arg
        // local to its parameter type, and callvirt directly (no InvokeMember).
        // Reference and value-type receivers (the latter via unbox + constrained.).
        string typeName = GetString(Cadr(c));
        string methodName = GetString(Caddr(c));
        var rest = Cdr(Cdr(Cdr(c)));            // (RECV (ARGS) (PARAMS))
        string recvLocal = GetSymbolName(Car(rest));
        var argLocalNames = new System.Collections.Generic.List<string>();
        for (var cur = Car(Cdr(rest)); cur is Cons cc; cur = cc.Cdr)
            argLocalNames.Add(GetSymbolName(cc.Car));
        var paramTypeNames = new System.Collections.Generic.List<string>();
        for (var cur = Car(Cdr(Cdr(rest))); cur is Cons cc; cur = cc.Cdr)
            paramTypeNames.Add(GetString(cc.Car));

        var argLocals = new System.Collections.Generic.List<LocalBuilder>();
        foreach (var n in argLocalNames) argLocals.Add(GetLocal(n));

        // Resolve param types up front; an unresolvable one (e.g. runtime-defined)
        // means we fall back to the dynamic path with the original args.
        var paramTypes = new Type[paramTypeNames.Count];
        bool paramsResolved = true;
        for (int i = 0; i < paramTypes.Length; i++)
        {
            var pt = Runtime.TryResolveDotNetType(paramTypeNames[i]);
            if (pt == null) { paramsResolved = false; break; }
            paramTypes[i] = pt;
        }
        Type? type = null;
        MethodInfo? method = null;
        if (paramsResolved && argLocalNames.Count == paramTypes.Length)
            method = ResolveDirectTarget(typeName, methodName, paramTypes, out type);
        if (method == null || type == null)
        {
            EmitDynamicInvokeFallback(GetLocal(recvLocal), methodName, argLocals);
            return;
        }

        // Typed receiver (reference: cast; value type: unbox + constrained. call).
        bool valueRecv = EmitDirectReceiver(GetLocal(recvLocal), type, method);

        EmitTypedArgs(argLocals, method);
        EmitTypedInstanceCall(method, valueRecv, type);
        EmitDotnetResultMarshal(method);
    }

    // Each argument straight from its local, marshalled to the type the
    // resolved method declares: Runtime.LispToDotNet(arg, declared). The
    // argument's own static type can differ -- a value passed to an Object /
    // Enum / interface parameter, or an Int16 to an Int64 one -- and the
    // value on the stack has to be what the parameter takes.
    private void EmitTypedArgs(IReadOnlyList<LocalBuilder> argLocals, MethodInfo method)
    {
        var lispToDotNet = typeof(Runtime).GetMethod("LispToDotNet", new[] { typeof(LispObject), typeof(Type) })!;
        var lispToDotNetParam = typeof(Runtime).GetMethod("LispToDotNetParam", new[] { typeof(LispObject), typeof(Type) })!;
        var getTypeFromHandle = typeof(Type).GetMethod("GetTypeFromHandle", new[] { typeof(RuntimeTypeHandle) })!;
        var declaredParams = method.GetParameters();
        for (int i = 0; i < argLocals.Count; i++)
        {
            var declared = declaredParams[i].ParameterType;
            _il.EmitLoc(OpCodes.Ldloc, argLocals[i]);
            _il.Emit(OpCodes.Ldtoken, declared);
            _il.Emit(OpCodes.Call, getTypeFromHandle);
            _il.Emit(OpCodes.Call,
                     declared.IsArray || declared == typeof(Array) ? lispToDotNetParam : lispToDotNet);
            if (declared.IsValueType) _il.Emit(OpCodes.Unbox_Any, declared);
            else _il.Emit(OpCodes.Castclass, declared);
        }
    }

    private void EmitDotnetStaticDirectLocals(LispObject c)
    {
        // (:dotnet-static-direct-locals "Type" "Member" (ARG-LOCAL...) (PARAM-TYPE...))
        // A typed dotnet:static: a direct call to the resolved static method or
        // property getter, or a load of the static field. Otherwise the dynamic
        // Runtime.DotNetStatic with the same arguments.
        string typeName = GetString(Cadr(c));
        string memberName = GetString(Caddr(c));
        var rest = Cdr(Cdr(Cdr(c)));            // ((ARGS) (PARAMS))
        var argLocals = new List<LocalBuilder>();
        for (var cur = Car(rest); cur is Cons cc; cur = cc.Cdr)
            argLocals.Add(GetLocal(GetSymbolName(cc.Car)));
        var paramTypeNames = new List<string>();
        for (var cur = Car(Cdr(rest)); cur is Cons cc; cur = cc.Cdr)
            paramTypeNames.Add(GetString(cc.Car));

        System.Reflection.MemberInfo? member = null;
        var t = Runtime.TryResolveDotNetType(typeName);
        if (t != null && paramTypeNames.Count == argLocals.Count)
        {
            var paramTypes = new Type[paramTypeNames.Count];
            bool resolved = true;
            for (int i = 0; i < paramTypes.Length && resolved; i++)
                resolved = (paramTypes[i] = Runtime.TryResolveDotNetType(paramTypeNames[i])!) != null;
            if (resolved) member = Runtime.ResolveTypedStaticMember(t, memberName, paramTypes);
        }

        switch (member)
        {
            case MethodInfo method when !Runtime.NeedsSpanStub(method):
                EmitTypedArgs(argLocals, method);
                _il.Emit(OpCodes.Call, method);
                EmitDotnetResultMarshal(method);
                return;
            case FieldInfo field when !field.IsLiteral:
                _il.Emit(OpCodes.Ldsfld, field);
                EmitDotnetValueMarshal(field.FieldType);
                return;
            case FieldInfo field when EmitLiteralConstant(field.GetRawConstantValue()):
                EmitDotnetValueMarshal(field.FieldType);
                return;
        }

        _il.Emit(OpCodes.Ldc_I4, 2 + argLocals.Count);
        _il.Emit(OpCodes.Newarr, typeof(LispObject));
        _il.Emit(OpCodes.Dup); _il.Emit(OpCodes.Ldc_I4, 0);
        _il.Emit(OpCodes.Ldstr, typeName);
        _il.Emit(OpCodes.Newobj, typeof(LispString).GetConstructor(new[] { typeof(string) })!);
        _il.Emit(OpCodes.Stelem_Ref);
        _il.Emit(OpCodes.Dup); _il.Emit(OpCodes.Ldc_I4, 1);
        _il.Emit(OpCodes.Ldstr, memberName);
        _il.Emit(OpCodes.Newobj, typeof(LispString).GetConstructor(new[] { typeof(string) })!);
        _il.Emit(OpCodes.Stelem_Ref);
        for (int i = 0; i < argLocals.Count; i++)
        {
            _il.Emit(OpCodes.Dup); _il.Emit(OpCodes.Ldc_I4, 2 + i);
            _il.EmitLoc(OpCodes.Ldloc, argLocals[i]); _il.Emit(OpCodes.Stelem_Ref);
        }
        _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("DotNetStatic", new[] { typeof(LispObject[]) })!);
    }

    // Push a const field's raw value (its underlying primitive for an enum), or
    // answer false for a value that has no single IL constant form.
    private bool EmitLiteralConstant(object? v)
    {
        switch (v)
        {
            case bool b: _il.Emit(OpCodes.Ldc_I4, b ? 1 : 0); return true;
            case char ch: _il.Emit(OpCodes.Ldc_I4, (int)ch); return true;
            case sbyte x: _il.Emit(OpCodes.Ldc_I4, (int)x); return true;
            case byte x: _il.Emit(OpCodes.Ldc_I4, (int)x); return true;
            case short x: _il.Emit(OpCodes.Ldc_I4, (int)x); return true;
            case ushort x: _il.Emit(OpCodes.Ldc_I4, (int)x); return true;
            case int x: _il.Emit(OpCodes.Ldc_I4, x); return true;
            case uint x: _il.Emit(OpCodes.Ldc_I4, unchecked((int)x)); return true;
            case long x: _il.Emit(OpCodes.Ldc_I8, x); return true;
            case ulong x: _il.Emit(OpCodes.Ldc_I8, unchecked((long)x)); return true;
            case float x: _il.Emit(OpCodes.Ldc_R4, x); return true;
            case double x: _il.Emit(OpCodes.Ldc_R8, x); return true;
            case string x: _il.Emit(OpCodes.Ldstr, x); return true;
            default: return false;
        }
    }

    private void EmitDotnetValueMarshal(Type type)
    {
        if (type.IsValueType) _il.Emit(OpCodes.Box, type);
        _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("DotNetToLisp", new[] { typeof(object) })!);
    }

    private void EmitDotnetResultMarshal(MethodInfo method)
    {
        if (method.ReturnType == typeof(void))
        {
            // A void method produces no values, the way a Lisp function that
            // returns nothing does. Used where one value is wanted it still reads
            // as NIL, so callers that ignore the result are unaffected; what
            // changes is that MULTIPLE-VALUE-LIST of it is empty.
            //
            // This is the one place the question can be answered exactly: the
            // overload has already been resolved, so there is no guessing from the
            // result (a void return and a null return arrive identically) or from
            // the name (a few BCL names have both void and value-returning
            // overloads).
            _il.Emit(OpCodes.Call, typeof(MultipleValues).GetMethod("Values0")!);
        }
        else
        {
            if (method.ReturnType.IsValueType)
                _il.Emit(OpCodes.Box, method.ReturnType);
            _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("DotNetToLisp", new[] { typeof(object) })!);
        }
    }

    private void EmitNewobj(string name)
    {
        if (_ctorCache.TryGetValue(name, out var ci))
        {
            _il.Emit(OpCodes.Newobj, ci);
            return;
        }
        throw new Exception($"Unknown constructor: {name}");
    }

    /// <summary>A castclass to a type the value provably already has is dead work:
    /// the JIT still calls the cast helper unless it can prove otherwise, and this
    /// pattern is everywhere: every (:load-sym "X") (:castclass "Symbol") pair.
    /// PREVPUSHED is the exact type the previous instruction left, when known.</summary>
    private void EmitCastclass(string name, Type? prevPushed = null)
    {
        var t = ResolveType(name);
        if (prevPushed != null && t.IsAssignableFrom(prevPushed)) return;
        _il.Emit(OpCodes.Castclass, t);
    }

    private void EmitNewarr(string name)
    {
        var t = ResolveType(name);
        _il.Emit(OpCodes.Newarr, t);
    }

    /// <summary>Push a string[] that is a constant of this site: the keyword names a
    /// &amp;key prologue hands to CheckNoUnknownKeys, or the packages those keywords were
    /// written in (NIL entries become nulls). The runtime only reads the vector, so one
    /// array per site is enough. Built in the prologue it was one array per CALL, paid
    /// even by the calls that pass no keyword at all -- where the check returns on its
    /// first line. FASL fills a static field from the type initializer, like a string
    /// literal; JIT takes a constant-pool slot.</summary>
    private void EmitConstStringArray(LispObject spec)
    {
        var items = new List<string?>();
        for (var p = spec; p is Cons cell; p = cell.Cdr)
            items.Add(cell.Car is Nil ? null : GetString(cell.Car));
        var arr = items.ToArray();
        if (_faslMode && _faslStructMap?.UninternedTypeBuilder != null)
        {
            _il.Emit(OpCodes.Ldsfld, _faslStructMap.GetOrCreateStringArrayField(arr));
            return;
        }
        if (!_faslMode)
        {
            EmitConstantAs(AddStringArrayConstant(arr), typeof(string[]));
            return;
        }
        // FASL without a field holder (the bootstrap shapes that have no uninterned
        // type): build it inline, which is what every site did before.
        EmitStringArrayInline(_il, arr);
    }

    /// <summary>Emit the construction of ARR as string[] onto IL. Shared by the
    /// FASL type initializer and the no-field-holder fallback.</summary>
    private static void EmitStringArrayInline(ILGenerator il, string?[] arr)
    {
        il.Emit(OpCodes.Ldc_I4, arr.Length);
        il.Emit(OpCodes.Newarr, typeof(string));
        for (int i = 0; i < arr.Length; i++)
        {
            if (arr[i] == null) continue;
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Ldc_I4, i);
            il.Emit(OpCodes.Ldstr, arr[i]!);
            il.Emit(OpCodes.Stelem_Ref);
        }
    }

    private static readonly Dictionary<string, int> _stringArrayConstants =
        new(StringComparer.Ordinal);

    /// <summary>Constant-pool slot for ARR, shared between sites with the same
    /// contents. The pool is global and never reset, so a per-site array would be
    /// retained forever; keyword vectors repeat heavily across a build (every
    /// (&amp;key test key) function has the same one).</summary>
    private static int AddStringArrayConstant(string?[] arr)
    {
        // The names are interned so CheckNoUnknownKeys can match them against the
        // (interned) names of keyword symbols by reference.
        for (int i = 0; i < arr.Length; i++)
            if (arr[i] != null) arr[i] = string.Intern(arr[i]!);
        // NUL separates the entries and an empty entry stands for null, so that
        // {"A", null} and {"A B"} do not collide on one slot.
        string key = string.Join("\0", arr.Select(s => s ?? ""));
        lock (_stringArrayConstants)
        {
            if (_stringArrayConstants.TryGetValue(key, out int existing)) return existing;
            int idx = AddConstant(arr);
            _stringArrayConstants[key] = idx;
            return idx;
        }
    }

    private void EmitLdarg(int n)
    {
        n += _argShift;
        switch (n)
        {
            case 0: _il.Emit(OpCodes.Ldarg_0); break;
            case 1: _il.Emit(OpCodes.Ldarg_1); break;
            case 2: _il.Emit(OpCodes.Ldarg_2); break;
            case 3: _il.Emit(OpCodes.Ldarg_3); break;
            default: _il.Emit(OpCodes.Ldarg, n); break;
        }
    }

    private void EmitSwitch(LispObject labelList)
    {
        var labels = new List<Label>();
        var cur = labelList;
        while (cur is Cons c)
        {
            labels.Add(GetOrDefineLabel(GetSymbolName(c.Car)));
            cur = c.Cdr;
        }
        _il.Emit(OpCodes.Switch, labels.ToArray());
    }

    /// <summary>A constant the cross compiler could only give as a recipe: the
    /// instructions build it, and this keeps the result. Same shape as the literal
    /// cache in EmitLoadConstInline, for the same reason -- a literal is one
    /// object, and rebuilding it per call is both wrong and expensive.
    ///
    /// Filled at first use, not in the type initializer: the recipe resolves
    /// symbols by name, and the packages they live in are made by earlier top
    /// level forms of the same file.</summary>
    private void EmitCachedConst(LispObject instrs)
    {
        if (!_faslMode || _faslStructMap?.UninternedTypeBuilder == null || _runOnce)
        {
            Assemble(instrs);   // JIT path: the constant pool already shares it
            return;
        }
        var field = _faslStructMap.DefineLiteralCacheField();
        var done = _il.DefineLabel();
        _il.Emit(OpCodes.Ldsfld, field);
        _il.Emit(OpCodes.Dup);
        _il.Emit(OpCodes.Brtrue, done);
        _il.Emit(OpCodes.Pop);
        _cachedLiteralDepth++;
        try { Assemble(instrs); }
        finally { _cachedLiteralDepth--; }
        _il.Emit(OpCodes.Dup);
        _il.Emit(OpCodes.Stsfld, field);
        _il.MarkLabel(done);
    }

    private void EmitLoadConst(LispObject val)
    {
        if (_faslMode)
        {
            EmitLoadConstInline(val);
            return;
        }
        // Store the LispObject literal in the constant pool
        int idx = AddConstant(val);
        _il.Emit(OpCodes.Ldc_I4, idx);
        // Typed reads without a castclass: the entry is VAL. A castclass per
        // site is JIT work, and a cast to LispObject is a helper call.
        if (val is Symbol)
        {
            _il.Emit(OpCodes.Call, _getConstantSymbol);
            _lastPushedType = typeof(Symbol);
        }
        else
            _il.Emit(OpCodes.Call, _getConstantObject);
    }

    /// <summary>Push constant-pool entry IDX, which the emitter added as a T, typed
    /// as T: the entry never changes, so a castclass would only cost JIT work.</summary>
    private void EmitConstantAs(int idx, Type t)
    {
        _il.Emit(OpCodes.Ldc_I4, idx);
        if (t == typeof(Symbol)) _il.Emit(OpCodes.Call, _getConstantSymbol);
        else if (t == typeof(LispObject)) _il.Emit(OpCodes.Call, _getConstantObject);
        else
        {
            _il.Emit(OpCodes.Call, _getConstant);
            EmitAs(t);
        }
    }

    /// <summary>Type the object on the stack, known to be a T, as T. In memory that
    /// is Unsafe.As, which the JIT drops; a saved assembly keeps the castclass.</summary>
    private void EmitAs(Type t)
    {
        if (_faslMode) _il.Emit(OpCodes.Castclass, t);
        else _il.Emit(OpCodes.Call, _unsafeAs.GetOrAdd(t, x => _unsafeAsDef.MakeGenericMethod(x)));
    }

    private static readonly System.Collections.Concurrent.ConcurrentDictionary<Type, MethodInfo> _unsafeAs = new();
    private static MethodInfo? _lazy_unsafeAsDef;
    private static MethodInfo _unsafeAsDef => _lazy_unsafeAsDef ??= typeof(System.Runtime.CompilerServices.Unsafe)
        .GetMethods().Single(m => m.Name == "As" && m.GetGenericArguments().Length == 1);
}
