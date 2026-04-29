using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Runtime emission of named public .NET classes via AssemblyBuilder +
/// TypeBuilder. Part of the defclass-cil roadmap:
///   D771 — Step 1: named class, default ctor, fixed Greet() method
///   D772 — Step 2: optional base class (SetParent + base ctor call)
///   D773 — Step 3: optional public instance fields (XAML x:Name targets)
///   D774 — Step 4: optional type-level custom attributes
///   D776 — Step 5a: user-supplied instance methods whose bodies dispatch
///                   back to a Lisp lambda (Greet auto-injection removed)
///   D783 — Step 7a: optional ctor body (Lisp lambda invoked after base.ctor)
///   D785 — Step 7b: optional auto-properties (backing field + get_X/set_X)
///   D786 — Step 7c: optional virtual-method override (DefineMethodOverride +
///                   MethodAttributes.Virtual matching a base virtual method)
///   D787 — Step 7d: optional interface implementations. AddInterfaceImplementation
///                   + methods that match an interface method by name+signature
///                   are emitted as implicit interface impls (NewSlot|Final|Virtual)
///   D788 — Step 7e: optional events (private delegate field + public add_/
///                   remove_ accessors + EventBuilder). add_/remove_ automatically
///                   wire to interface slots when the type declares a matching
///                   event-bearing interface (INotifyPropertyChanged etc.)
///   D789 — Step 7f: event raisers (public virtual OnName). For each event,
///                   a method that invokes the delegate if non-null is emitted.
///                   EventHandler-shaped delegates (first param == object) use
///                   this as sender automatically; other delegate shapes pass
///                   all params through.
///   D790 — Step 7g: auto-property setters with :notify flag. When set, the
///                   setter calls OnPropertyChanged(PropertyChangedEventArgs)
///                   after stfld, so `(dotnet:%set-invoke vm "Title" v)` alone
///                   fires the INotifyPropertyChanged notification.
///
/// Each call creates a fresh dynamic assembly holding one type. That assembly
/// becomes visible to Type.GetType lookup through AppDomain.CurrentDomain
/// .GetAssemblies(), which is the path ResolveDotNetType already uses.
/// </summary>
public static class DynamicClassBuilder
{
    private static int _assemblyCounter;

    // Global dispatch table: Lisp lambda bodies keyed by (typeFullName, methodName).
    // Populated at DefineClass time and consulted by DispatchLispMethod on every
    // invocation of a Lisp-backed method. Keeping the lambda alive keeps its
    // lexical closure alive.
    private static readonly Dictionary<(string, string), LispObject> _methodHandlers
        = new();

    public record MethodSpec(string Name, Type ReturnType, IReadOnlyList<Type> ParamTypes,
                             LispObject LispBody, bool IsOverride = false,
                             IReadOnlyList<CustomAttributeBuilder>? Attributes = null);

    /// <summary>
    /// Define a public class. See roadmap in the type doc-comment for what
    /// each parameter maps to. Returns the materialized Type.
    /// </summary>
    // Reserved method-table key for the ctor body dispatch. Chosen so it can
    // never collide with a user-defined method (.ctor isn't a valid CLR method
    // name that MethodBuilder would accept for DefineMethod).
    private const string CtorKey = ".ctor";

    public static Type DefineMinimalClass(string fullName, Type? baseType = null,
        IReadOnlyList<(string Name, Type Type)>? fields = null,
        IReadOnlyList<CustomAttributeBuilder>? attributes = null,
        IReadOnlyList<MethodSpec>? methods = null,
        LispObject? ctorBody = null,
        IReadOnlyList<(string Name, Type Type, bool Notify)>? properties = null,
        IReadOnlyList<Type>? interfaces = null,
        IReadOnlyList<(string Name, Type DelegateType)>? events = null)
    {
        if (string.IsNullOrEmpty(fullName))
            throw new ArgumentException("fullName must be non-empty", nameof(fullName));

        baseType ??= typeof(object);

        if (baseType.IsSealed)
            throw new ArgumentException(
                $"Cannot derive from sealed type {baseType.FullName}", nameof(baseType));
        if (baseType.IsInterface)
            throw new ArgumentException(
                $"Base type must be a class, not interface: {baseType.FullName}", nameof(baseType));

        var baseCtor = baseType.GetConstructor(
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
            binder: null, types: Type.EmptyTypes, modifiers: null);
        if (baseCtor == null)
            throw new ArgumentException(
                $"Base type {baseType.FullName} has no accessible parameterless constructor",
                nameof(baseType));

        int id = System.Threading.Interlocked.Increment(ref _assemblyCounter);
        var asmName = new AssemblyName("DotclDynamic_" + id);
        var ab = AssemblyBuilder.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Run);
        var mb = ab.DefineDynamicModule(asmName.Name!);

        var tb = mb.DefineType(fullName,
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoClass
                | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit,
            baseType);

        // D787 — declared interfaces. Each is a reference type that must be
        // an interface; duplicates are rejected to catch user typos early.
        if (interfaces != null)
        {
            var seenIfaces = new HashSet<Type>();
            foreach (var iface in interfaces)
            {
                if (iface == null)
                    throw new ArgumentException("interface entry must not be null", nameof(interfaces));
                if (!iface.IsInterface)
                    throw new ArgumentException(
                        $"{iface.FullName} is not an interface", nameof(interfaces));
                if (!seenIfaces.Add(iface))
                    throw new ArgumentException(
                        $"duplicate interface: {iface.FullName}", nameof(interfaces));
                tb.AddInterfaceImplementation(iface);
            }
        }

        if (attributes != null)
            foreach (var attr in attributes)
                tb.SetCustomAttribute(attr);

        if (fields != null)
        {
            var seen = new HashSet<string>(StringComparer.Ordinal);
            foreach (var (name, type) in fields)
            {
                if (string.IsNullOrEmpty(name))
                    throw new ArgumentException("field name must be non-empty", nameof(fields));
                if (!seen.Add(name))
                    throw new ArgumentException(
                        $"duplicate field name: {name}", nameof(fields));
                tb.DefineField(name, type, FieldAttributes.Public);
            }
        }

        // Events (D788): private delegate field + public add_/remove_ accessors
        // + EventBuilder. Reserved accessor names go into reservedMethodNames so
        // the user cannot collide them with explicit methods below. We emit
        // events BEFORE properties because D790's :notify flag causes property
        // setters to reference the OnPropertyChanged raiser MethodBuilder.
        var reservedMethodNames = new HashSet<string>(StringComparer.Ordinal);
        var raisersByEvent = new Dictionary<string, (MethodBuilder Raiser, Type[] ParamTypes)>(
            StringComparer.Ordinal);
        if (events != null)
        {
            var seenEvents = new HashSet<string>(StringComparer.Ordinal);
            foreach (var (name, delegateType) in events)
            {
                if (string.IsNullOrEmpty(name))
                    throw new ArgumentException("event name must be non-empty", nameof(events));
                if (!seenEvents.Add(name))
                    throw new ArgumentException($"duplicate event name: {name}", nameof(events));
                if (delegateType == null || !typeof(Delegate).IsAssignableFrom(delegateType))
                    throw new ArgumentException(
                        $"event type must derive from System.Delegate: {delegateType?.FullName ?? "<null>"}",
                        nameof(events));
                var (raiser, raiserParams) = EmitEvent(tb, interfaces, name, delegateType);
                raisersByEvent[name] = (raiser, raiserParams);
                reservedMethodNames.Add("add_" + name);
                reservedMethodNames.Add("remove_" + name);
                reservedMethodNames.Add("On" + name);
            }
        }

        // Auto-properties (D785): private backing field + public get_X/set_X
        // wired to a PropertyBuilder. Reflection-based frameworks (MAUI
        // Binding, JSON serializers, etc.) discover these as regular .NET
        // properties. When `notify` is true, D790 appends an
        // OnPropertyChanged(PropertyChangedEventArgs) call to the setter body.
        if (properties != null)
        {
            var seenProps = new HashSet<string>(StringComparer.Ordinal);
            foreach (var (name, type, notify) in properties)
            {
                if (string.IsNullOrEmpty(name))
                    throw new ArgumentException("property name must be non-empty", nameof(properties));
                if (!seenProps.Add(name))
                    throw new ArgumentException(
                        $"duplicate property name: {name}", nameof(properties));
                (MethodBuilder Raiser, Type[] ParamTypes)? raiserInfo = null;
                if (notify)
                {
                    if (!raisersByEvent.TryGetValue("PropertyChanged", out var found))
                        throw new ArgumentException(
                            $"property {name} has :notify t but no PropertyChanged event " +
                            "declared — add (:events (\"PropertyChanged\" PropertyChangedEventHandler))",
                            nameof(properties));
                    raiserInfo = found;
                }
                EmitAutoProperty(tb, name, type, raiserInfo);
            }
        }

        // public .ctor() : base() { <optional Lisp body> }
        var ctor = tb.DefineConstructor(
            MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName,
            CallingConventions.Standard, Type.EmptyTypes);
        var cil = ctor.GetILGenerator();
        cil.Emit(OpCodes.Ldarg_0);
        cil.Emit(OpCodes.Call, baseCtor);

        if (ctorBody != null)
        {
            // After base.ctor has run, `this` is a valid derived instance and
            // can be passed to the Lisp lambda as self. The lambda takes one
            // arg (self) and returns whatever — we discard the result.
            cil.Emit(OpCodes.Ldstr, fullName);
            cil.Emit(OpCodes.Ldstr, CtorKey);
            cil.Emit(OpCodes.Ldtoken, typeof(void));
            cil.Emit(OpCodes.Call, GetTypeFromHandleMI);
            cil.Emit(OpCodes.Ldarg_0); // self
            cil.Emit(OpCodes.Ldc_I4_0); // empty args
            cil.Emit(OpCodes.Newarr, typeof(object));
            cil.Emit(OpCodes.Call, DispatchMI);
            cil.Emit(OpCodes.Pop); // void discard
        }

        cil.Emit(OpCodes.Ret);

        // User-defined instance methods (D776). Each body dispatches to the
        // corresponding Lisp lambda through DispatchLispMethod.
        var pendingMethods = new List<(string MethodName, LispObject Lambda)>();
        if (methods != null)
        {
            var seenMethods = new HashSet<string>(StringComparer.Ordinal);
            foreach (var m in methods)
            {
                if (string.IsNullOrEmpty(m.Name))
                    throw new ArgumentException("method name must be non-empty", nameof(methods));
                if (!seenMethods.Add(m.Name))
                    throw new ArgumentException(
                        $"duplicate method name: {m.Name}", nameof(methods));
                if (reservedMethodNames.Contains(m.Name))
                    throw new ArgumentException(
                        $"method name {m.Name} collides with an auto-generated event accessor",
                        nameof(methods));
                EmitLispDispatchMethod(tb, fullName, baseType, interfaces, m);
                pendingMethods.Add((m.Name, m.LispBody));
            }
        }

        // Register ctor body BEFORE CreateType so it is available when the
        // first Activator.CreateInstance fires (CreateInstance may JIT-compile
        // the ctor which doesn't itself call it, but defensive ordering).
        if (ctorBody != null)
            _methodHandlers[(fullName, CtorKey)] = ctorBody;

        var createdType = tb.CreateType()!;

        // Register method handlers AFTER CreateType so first call to the method
        // (e.g. from a test's DOTNET:INVOKE) finds its Lisp body. A re-define of
        // the same full name overwrites (matches Step 1 "fresh assembly per call").
        foreach (var (methodName, lambda) in pendingMethods)
            _methodHandlers[(fullName, methodName)] = lambda;

        return createdType;
    }

    /// <summary>
    /// Emit a public auto-property: a private backing field, public
    /// <c>get_Name</c>/<c>set_Name</c> methods, and a PropertyBuilder that
    /// ties them together. The getter/setter are marked
    /// <c>SpecialName | HideBySig</c>, matching what the C# compiler emits
    /// for `public T Name { get; set; }`.
    /// </summary>
    private static void EmitAutoProperty(TypeBuilder tb, string name, Type propType,
        (MethodBuilder Raiser, Type[] ParamTypes)? notifyRaiser = null)
    {
        var backing = tb.DefineField("<" + name + ">k__BackingField",
            propType, FieldAttributes.Private);

        var getter = tb.DefineMethod("get_" + name,
            MethodAttributes.Public | MethodAttributes.SpecialName
                | MethodAttributes.HideBySig,
            propType, Type.EmptyTypes);
        var gil = getter.GetILGenerator();
        gil.Emit(OpCodes.Ldarg_0);
        gil.Emit(OpCodes.Ldfld, backing);
        gil.Emit(OpCodes.Ret);

        var setter = tb.DefineMethod("set_" + name,
            MethodAttributes.Public | MethodAttributes.SpecialName
                | MethodAttributes.HideBySig,
            typeof(void), new[] { propType });
        var sil = setter.GetILGenerator();
        sil.Emit(OpCodes.Ldarg_0);
        sil.Emit(OpCodes.Ldarg_1);
        sil.Emit(OpCodes.Stfld, backing);

        if (notifyRaiser.HasValue)
        {
            var (raiser, raiserParams) = notifyRaiser.Value;
            // raiser shape for EventHandler-pattern is OnPropertyChanged(PCEA).
            // Build an instance with the property name and hand it to the raiser.
            if (raiserParams.Length != 1)
                throw new ArgumentException(
                    $":notify requires OnPropertyChanged to take exactly one " +
                    $"parameter; got {raiserParams.Length}");
            var argType = raiserParams[0];
            var argCtor = argType.GetConstructor(new[] { typeof(string) })
                ?? throw new ArgumentException(
                    $":notify requires {argType.FullName} to have a (String) constructor");
            sil.Emit(OpCodes.Ldarg_0);
            sil.Emit(OpCodes.Ldstr, name);
            sil.Emit(OpCodes.Newobj, argCtor);
            sil.Emit(OpCodes.Callvirt, raiser);
        }

        sil.Emit(OpCodes.Ret);

        var prop = tb.DefineProperty(name, PropertyAttributes.None,
            propType, Type.EmptyTypes);
        prop.SetGetMethod(getter);
        prop.SetSetMethod(setter);
    }

    /// <summary>
    /// Emit the body of a user-defined instance method. The body boxes args
    /// into an <c>object[]</c>, calls <see cref="DispatchLispMethod"/>, then
    /// unboxes/casts the result to the declared return type.
    /// If <c>m.IsOverride</c> is true, the method is emitted as Virtual and
    /// explicitly tied to a matching base virtual method via
    /// <see cref="TypeBuilder.DefineMethodOverride"/> (D786).
    /// Otherwise, if the type declares any interfaces and the method matches
    /// an interface method by name+signature, it is emitted as an implicit
    /// interface implementation (D787): Virtual|NewSlot|Final|HideBySig plus
    /// DefineMethodOverride for each matched interface method.
    /// </summary>
    private static void EmitLispDispatchMethod(TypeBuilder tb, string fullName,
        Type baseType, IReadOnlyList<Type>? interfaces, MethodSpec m)
    {
        var paramArr = m.ParamTypes.ToArray();

        MethodInfo? baseMethod = null;
        List<MethodInfo>? ifaceTargets = null;
        var attrs = MethodAttributes.Public | MethodAttributes.HideBySig;

        if (m.IsOverride)
        {
            baseMethod = FindOverridableBaseMethod(baseType, m.Name, paramArr, m.ReturnType);
            attrs |= MethodAttributes.Virtual;
        }
        else if (interfaces != null)
        {
            ifaceTargets = FindMatchingInterfaceMethods(interfaces, m.Name, paramArr, m.ReturnType);
            if (ifaceTargets.Count > 0)
                attrs |= MethodAttributes.Virtual | MethodAttributes.NewSlot
                       | MethodAttributes.Final;
        }

        var method = tb.DefineMethod(m.Name, attrs, m.ReturnType, paramArr);

        // Apply method-level CustomAttributes (e.g., [HttpGet], [Route])
        // before emitting IL so MVC controller discovery sees them on the
        // built MethodInfo.
        if (m.Attributes != null)
            foreach (var ab2 in m.Attributes)
                method.SetCustomAttribute(ab2);

        var il = method.GetILGenerator();

        // DispatchLispMethod(typeName, methodName, returnType, self, object[] args)
        il.Emit(OpCodes.Ldstr, fullName);
        il.Emit(OpCodes.Ldstr, m.Name);

        // ldtoken + GetTypeFromHandle → Type
        il.Emit(OpCodes.Ldtoken, m.ReturnType);
        il.Emit(OpCodes.Call, GetTypeFromHandleMI);

        // self (object)
        il.Emit(OpCodes.Ldarg_0);

        // new object[paramCount]
        il.Emit(OpCodes.Ldc_I4, paramArr.Length);
        il.Emit(OpCodes.Newarr, typeof(object));

        for (int i = 0; i < paramArr.Length; i++)
        {
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Ldc_I4, i);
            il.Emit(OpCodes.Ldarg, i + 1); // +1: skip `this`
            if (paramArr[i].IsValueType)
                il.Emit(OpCodes.Box, paramArr[i]);
            il.Emit(OpCodes.Stelem_Ref);
        }

        il.Emit(OpCodes.Call, DispatchMI);

        // Convert return value
        if (m.ReturnType == typeof(void))
        {
            il.Emit(OpCodes.Pop);
        }
        else if (m.ReturnType.IsValueType)
        {
            il.Emit(OpCodes.Unbox_Any, m.ReturnType);
        }
        else if (m.ReturnType != typeof(object))
        {
            il.Emit(OpCodes.Castclass, m.ReturnType);
        }
        // else: result is already object; leave on stack

        il.Emit(OpCodes.Ret);

        if (baseMethod != null)
            tb.DefineMethodOverride(method, baseMethod);

        if (ifaceTargets != null)
            foreach (var target in ifaceTargets)
                tb.DefineMethodOverride(method, target);
    }

    /// <summary>
    /// Emit a public event (D788): a private delegate backing field, public
    /// <c>add_Name</c>/<c>remove_Name</c> accessors that combine/remove the
    /// handler, and an <see cref="EventBuilder"/> tying them together. If a
    /// declared interface carries a matching add_/remove_ slot (same name and
    /// delegate type) the accessors are emitted as implicit interface impls
    /// via <see cref="TypeBuilder.DefineMethodOverride"/>. Not thread-safe —
    /// uses plain Delegate.Combine/Remove rather than Interlocked.CompareExchange.
    /// </summary>
    private static (MethodBuilder Raiser, Type[] ParamTypes) EmitEvent(
        TypeBuilder tb, IReadOnlyList<Type>? interfaces,
        string name, Type delegateType)
    {
        var field = tb.DefineField("_" + name, delegateType, FieldAttributes.Private);

        // If either accessor matches an interface slot, the pair must be
        // Virtual|NewSlot|Final for CLR interface binding.
        var ifaceAdd = interfaces != null
            ? FindMatchingInterfaceMethods(interfaces, "add_" + name,
                new[] { delegateType }, typeof(void))
            : new List<MethodInfo>();
        var ifaceRemove = interfaces != null
            ? FindMatchingInterfaceMethods(interfaces, "remove_" + name,
                new[] { delegateType }, typeof(void))
            : new List<MethodInfo>();

        var attrs = MethodAttributes.Public | MethodAttributes.SpecialName
                  | MethodAttributes.HideBySig;
        if (ifaceAdd.Count > 0 || ifaceRemove.Count > 0)
            attrs |= MethodAttributes.Virtual | MethodAttributes.NewSlot
                   | MethodAttributes.Final;

        var add = tb.DefineMethod("add_" + name, attrs, typeof(void),
            new[] { delegateType });
        EmitAddOrRemoveBody(add, field, delegateType, isAdd: true);

        var rem = tb.DefineMethod("remove_" + name, attrs, typeof(void),
            new[] { delegateType });
        EmitAddOrRemoveBody(rem, field, delegateType, isAdd: false);

        foreach (var target in ifaceAdd)
            tb.DefineMethodOverride(add, target);
        foreach (var target in ifaceRemove)
            tb.DefineMethodOverride(rem, target);

        var eb = tb.DefineEvent(name, EventAttributes.None, delegateType);
        eb.SetAddOnMethod(add);
        eb.SetRemoveOnMethod(rem);

        // D789 — raiser: public virtual OnName(args...). Fires the delegate
        // with a null check. For (object sender, TArgs e) shaped delegates
        // (EventHandler etc.) the method is OnName(TArgs e) and passes `this`
        // as the sender. For other shapes it takes the delegate's full param
        // list. C# convention is protected for the raiser, but our Lisp-side
        // dotnet:invoke only reaches Public members so we emit Public.
        return EmitEventRaiser(tb, field, name, delegateType);
    }

    private static (MethodBuilder, Type[]) EmitEventRaiser(TypeBuilder tb, FieldBuilder field,
        string eventName, Type delegateType)
    {
        var invokeMi = delegateType.GetMethod("Invoke")
            ?? throw new ArgumentException(
                $"delegate {delegateType.FullName} has no Invoke method",
                nameof(delegateType));
        if (invokeMi.ReturnType != typeof(void))
            throw new ArgumentException(
                $"event delegate {delegateType.FullName} must return void for auto-raiser",
                nameof(delegateType));

        var invokeParams = invokeMi.GetParameters();
        bool senderPattern = invokeParams.Length >= 1
            && invokeParams[0].ParameterType == typeof(object);

        Type[] raiserParams = senderPattern
            ? invokeParams.Skip(1).Select(p => p.ParameterType).ToArray()
            : invokeParams.Select(p => p.ParameterType).ToArray();

        var mb = tb.DefineMethod("On" + eventName,
            MethodAttributes.Public | MethodAttributes.Virtual
                | MethodAttributes.HideBySig,
            typeof(void), raiserParams);

        var il = mb.GetILGenerator();
        var nullLabel = il.DefineLabel();

        // handler = this._field
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldfld, field);
        il.Emit(OpCodes.Dup);
        il.Emit(OpCodes.Brfalse_S, nullLabel);

        // handler.Invoke(<sender>, arg1, arg2, ...)
        if (senderPattern)
        {
            il.Emit(OpCodes.Ldarg_0);
            for (int i = 0; i < raiserParams.Length; i++)
                il.Emit(OpCodes.Ldarg, i + 1);
        }
        else
        {
            for (int i = 0; i < raiserParams.Length; i++)
                il.Emit(OpCodes.Ldarg, i + 1);
        }
        il.Emit(OpCodes.Callvirt, invokeMi);
        il.Emit(OpCodes.Ret);

        il.MarkLabel(nullLabel);
        il.Emit(OpCodes.Pop); // pop the null dup
        il.Emit(OpCodes.Ret);

        return (mb, raiserParams);
    }

    private static readonly MethodInfo DelegateCombineMI =
        typeof(Delegate).GetMethod("Combine",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: new[] { typeof(Delegate), typeof(Delegate) },
            modifiers: null)!;

    private static readonly MethodInfo DelegateRemoveMI =
        typeof(Delegate).GetMethod("Remove",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: new[] { typeof(Delegate), typeof(Delegate) },
            modifiers: null)!;

    /// <summary>
    /// Emit the body of add_/remove_ accessor:
    ///   this._field = (DelegateType) Delegate.Combine/Remove(this._field, value);
    /// </summary>
    private static void EmitAddOrRemoveBody(MethodBuilder mb, FieldBuilder field,
        Type delegateType, bool isAdd)
    {
        var il = mb.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldfld, field);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Call, isAdd ? DelegateCombineMI : DelegateRemoveMI);
        il.Emit(OpCodes.Castclass, delegateType);
        il.Emit(OpCodes.Stfld, field);
        il.Emit(OpCodes.Ret);
    }

    /// <summary>
    /// Collect interface methods that match the given name, parameter types,
    /// and return type across all declared interfaces. A single user method
    /// may implement the same-named slot on multiple interfaces simultaneously.
    /// </summary>
    private static List<MethodInfo> FindMatchingInterfaceMethods(
        IReadOnlyList<Type> interfaces, string name, Type[] paramTypes, Type returnType)
    {
        var hits = new List<MethodInfo>();
        foreach (var iface in interfaces)
        {
            var mi = iface.GetMethod(name,
                BindingFlags.Public | BindingFlags.Instance,
                binder: null, types: paramTypes, modifiers: null);
            if (mi != null && mi.ReturnType == returnType)
                hits.Add(mi);
        }
        return hits;
    }

    /// <summary>
    /// Locate a virtual method on <paramref name="baseType"/> (or an ancestor)
    /// that can be overridden with the given name, parameter types, and
    /// return type. Throws ArgumentException with a specific message for
    /// missing / non-virtual / sealed / return-type-mismatch cases so the
    /// Lisp-level error is actionable.
    /// </summary>
    private static MethodInfo FindOverridableBaseMethod(
        Type baseType, string name, Type[] paramTypes, Type returnType)
    {
        var candidate = baseType.GetMethod(name,
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
            binder: null, types: paramTypes, modifiers: null);
        if (candidate == null)
            throw new ArgumentException(
                $"Cannot override: no method {name}({string.Join(",", paramTypes.Select(t => t.FullName))}) " +
                $"found on {baseType.FullName} or its ancestors");
        if (!candidate.IsVirtual)
            throw new ArgumentException(
                $"Cannot override: {baseType.FullName}.{name} is not virtual");
        if (candidate.IsFinal)
            throw new ArgumentException(
                $"Cannot override: {baseType.FullName}.{name} is sealed");
        if (candidate.ReturnType != returnType)
            throw new ArgumentException(
                $"Cannot override: {baseType.FullName}.{name} returns {candidate.ReturnType.FullName}, " +
                $"not {returnType.FullName}");
        return candidate;
    }

    private static readonly MethodInfo GetTypeFromHandleMI =
        typeof(Type).GetMethod("GetTypeFromHandle", BindingFlags.Public | BindingFlags.Static)!;

    private static readonly MethodInfo DispatchMI =
        typeof(DynamicClassBuilder).GetMethod(nameof(DispatchLispMethod),
            BindingFlags.Public | BindingFlags.Static)!;

    /// <summary>
    /// Runtime entry point called by the emitted method body. Looks up the
    /// Lisp lambda registered for (typeFullName, methodName), marshals self
    /// and args through DotNetToLisp, funcalls the lambda, and marshals the
    /// result back through LispToDotNet for the declared <paramref name="returnType"/>.
    /// </summary>
    public static object? DispatchLispMethod(
        string typeFullName, string methodName, Type returnType,
        object? self, object?[] args)
    {
        if (!_methodHandlers.TryGetValue((typeFullName, methodName), out var lispFn))
            throw new InvalidOperationException(
                $"DispatchLispMethod: no Lisp handler registered for {typeFullName}.{methodName}");

        var lispArgs = new LispObject[args.Length + 1];
        lispArgs[0] = Runtime.DotNetToLisp(self);
        for (int i = 0; i < args.Length; i++)
            lispArgs[i + 1] = Runtime.DotNetToLisp(args[i]);

        var result = Runtime.Funcall(lispFn, lispArgs);

        if (returnType == typeof(void)) return null;
        return Runtime.LispToDotNet(result, returnType);
    }
}
