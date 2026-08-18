using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Runtime emission of named public .NET classes via AssemblyBuilder +
/// TypeBuilder. Part of the defclass-cil roadmap:
///   Step 1: named class, default ctor, fixed Greet() method
///   Step 2: optional base class (SetParent + base ctor call)
///   Step 3: optional public instance fields (XAML x:Name targets)
///   Step 4: optional type-level custom attributes
///   Step 5a: user-supplied instance methods whose bodies dispatch
///            back to a Lisp lambda (Greet auto-injection removed)
///   Step 7a: optional ctor body (Lisp lambda invoked after base.ctor)
///   Step 7b: optional auto-properties (backing field + get_X/set_X)
///   Step 7c: optional virtual-method override (DefineMethodOverride +
///            MethodAttributes.Virtual matching a base virtual method)
///   Step 7d: optional interface implementations. AddInterfaceImplementation
///            + methods that match an interface method by name+signature
///            are emitted as implicit interface impls (NewSlot|Final|Virtual)
///   Step 7e: optional events (private delegate field + public add_/
///            remove_ accessors + EventBuilder). add_/remove_ automatically
///            wire to interface slots when the type declares a matching
///            event-bearing interface (INotifyPropertyChanged etc.)
///   Step 7f: event raisers (public virtual OnName). For each event,
///            a method that invokes the delegate if non-null is emitted.
///            EventHandler-shaped delegates (first param == object) use
///            this as sender automatically; other delegate shapes pass
///            all params through.
///   Step 7g: auto-property setters with :notify flag. When set, the
///            setter calls OnPropertyChanged(PropertyChangedEventArgs)
///            after stfld, so `(dotnet:%set-invoke vm "Title" v)` alone
///            fires the INotifyPropertyChanged notification.
///
/// DefineMinimalClass (the in-process path) creates a fresh dynamic assembly
/// holding one type; that assembly becomes visible to Type.GetType lookup
/// through AppDomain.CurrentDomain.GetAssemblies(), which is the path
/// ResolveDotNetType already uses. For saved, C#-referenceable libraries,
/// BeginLibrary/LibraryBuilder accumulate MANY types into one persisted
/// assembly (the aggregation unit — a real library is more than one type);
/// PopulateType is the shared per-type emitter both paths call.
/// </summary>
public static class DynamicClassBuilder
{
    private static int _assemblyCounter;

    // Global dispatch table: Lisp lambda bodies keyed by (typeFullName, dispatchKey).
    // dispatchKey = methodName for no-param methods; methodName + "#" + "|"-joined
    // FullNames for parameterized methods. Populated at DefineClass time and
    // consulted by DispatchLispMethod on every invocation. Keeping the lambda
    // alive keeps its lexical closure alive.
    private static readonly Dictionary<(string, string), LispObject> _methodHandlers
        = new();

    // IsStatic maps a Lisp defun to a `public static` method (no `self`) — the
    // shape a function library exports (System.Math-style). A static method
    // cannot be an override or interface impl, so IsStatic and IsOverride are
    // mutually exclusive.
    public record MethodSpec(string Name, Type ReturnType, IReadOnlyList<Type> ParamTypes,
                             LispObject LispBody, bool IsOverride = false,
                             IReadOnlyList<CustomAttributeBuilder>? Attributes = null,
                             bool IsStatic = false);

    // multi-ctor support: one spec per constructor overload.
    public record CtorSpec(
        LispObject? Body,
        IReadOnlyList<Type>? ParamTypes,
        IReadOnlyList<int>? BaseArgIndices);

    // Build the runtime dispatch key for a method / ctor.
    // No-param methods use just the name so existing single-overload code is unaffected.
    internal static string MethodDispatchKey(string methodName, IReadOnlyList<Type> paramTypes)
        => paramTypes.Count == 0
           ? methodName
           : methodName + "#" + string.Join("|", paramTypes.Select(t => t.FullName!));

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
        IReadOnlyList<(string Name, Type DelegateType)>? events = null,
        IReadOnlyList<Type>? ctorParamTypes = null,
        IReadOnlyList<int>? baseCtorArgIndices = null,
        IReadOnlyList<CtorSpec>? ctorSpecs = null,
        string? saveToPath = null)
    {
        CilAssembler.EnsureEmitAllowed("dotnet:define-class");
        if (string.IsNullOrEmpty(fullName))
            throw new ArgumentException("fullName must be non-empty", nameof(fullName));

        // saveToPath != null → the type goes into a saved, C#-referenceable
        // facade assembly (stage 1). A single-type save is just a one-class library,
        // so we route through the aggregation layer (BeginLibrary/AddClass/Save)
        // to keep one code path for "populate a module + retarget corlib".
        if (saveToPath != null)
        {
#if NET9_0_OR_GREATER
            // Assembly simple-name = the type's namespace if any, else its name;
            // this is what a C# consumer sees as the reference.
            var dot = fullName.LastIndexOf('.');
            var asmSimple = dot > 0 ? fullName.Substring(0, dot) : fullName;
            var lib = BeginLibrary(saveToPath, asmSimple);
            var t = lib.AddClass(fullName, baseType, fields, attributes, methods, ctorBody,
                properties, interfaces, events, ctorParamTypes, baseCtorArgIndices, ctorSpecs);
            lib.Save();
            return t;
#else
            throw new LispErrorException(new LispProgramError(
                "saving a class library requires .NET 9+ (PersistedAssemblyBuilder)"));
#endif
        }

        // Run assembly: usable in-process (make-instance, method dispatch). One
        // fresh dynamic assembly per call, historical behavior.
        int id = System.Threading.Interlocked.Increment(ref _assemblyCounter);
        var asmName = new AssemblyName("DotclDynamic_" + id);
        var ab = AssemblyBuilder.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Run);
        var mb = ab.DefineDynamicModule(asmName.Name!);

        var (createdType, handlers) = PopulateType(mb, fullName, baseType, fields, attributes,
            methods, ctorBody, properties, interfaces, events, ctorParamTypes,
            baseCtorArgIndices, ctorSpecs);

        // Register method/ctor handlers AFTER CreateType so the first call to a
        // method (e.g. from a test's DOTNET:INVOKE) finds its Lisp body. A
        // re-define of the same full name overwrites (matches "fresh assembly
        // per call").
        foreach (var (key, body) in handlers)
            _methodHandlers[(fullName, key)] = body;

        return createdType;
    }

    /// <summary>
    /// Populate one public type on the given <paramref name="mb"/> from the
    /// member specs, emit all members, call CreateType, and return the built
    /// Type plus the (dispatchKey, LispBody) handler pairs to register. The
    /// caller decides whether to register them: the in-process Run path does
    /// (so method dispatch works); the saved-facade path does not (the type is
    /// never invoked in the emitting process). Shared by DefineMinimalClass and
    /// LibraryBuilder.AddClass so both single-type and multi-type-per-assembly
    /// paths emit identical member IL.
    /// </summary>
    private static (Type Type, List<(string Key, LispObject Body)> Handlers) PopulateType(
        ModuleBuilder mb, string fullName, Type? baseType,
        IReadOnlyList<(string Name, Type Type)>? fields,
        IReadOnlyList<CustomAttributeBuilder>? attributes,
        IReadOnlyList<MethodSpec>? methods,
        LispObject? ctorBody,
        IReadOnlyList<(string Name, Type Type, bool Notify)>? properties,
        IReadOnlyList<Type>? interfaces,
        IReadOnlyList<(string Name, Type DelegateType)>? events,
        IReadOnlyList<Type>? ctorParamTypes,
        IReadOnlyList<int>? baseCtorArgIndices,
        IReadOnlyList<CtorSpec>? ctorSpecs)
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

        // Handlers collected here and returned; the caller registers them into
        // _methodHandlers only for the in-process Run path.
        var handlers = new List<(string Key, LispObject Body)>();

        // Single-ctor path resolves baseCtor eagerly; multi-ctor path resolves per spec.
        ConstructorInfo? baseCtor = null;
        if (ctorSpecs == null)
        {
            if (baseCtorArgIndices != null && baseCtorArgIndices.Count > 0 && ctorParamTypes != null)
            {
                var baseCtorTypes = baseCtorArgIndices.Select(i => ctorParamTypes[i]).ToArray();
                baseCtor = baseType.GetConstructor(
                    BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
                    binder: null, types: baseCtorTypes, modifiers: null);
                if (baseCtor == null)
                    throw new ArgumentException(
                        $"Base type {baseType.FullName} has no accessible constructor matching types ({string.Join(", ", baseCtorTypes.Select(t => t.Name))})",
                        nameof(baseType));
            }
            else
            {
                baseCtor = baseType.GetConstructor(
                    BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
                    binder: null, types: Type.EmptyTypes, modifiers: null);
                if (baseCtor == null)
                    throw new ArgumentException(
                        $"Base type {baseType.FullName} has no accessible parameterless constructor",
                        nameof(baseType));
            }
        }

        var tb = mb.DefineType(fullName,
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoClass
                | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit,
            baseType);

        // Declared interfaces. Each is a reference type that must be
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

        // Events: private delegate field + public add_/remove_ accessors
        // + EventBuilder. Reserved accessor names go into reservedMethodNames so
        // the user cannot collide them with explicit methods below. We emit
        // events BEFORE properties because the :notify flag causes property
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

        // Auto-properties: private backing field + public get_X/set_X
        // wired to a PropertyBuilder. Reflection-based frameworks (MAUI
        // Binding, JSON serializers, etc.) discover these as regular .NET
        // properties. When `notify` is true, an
        // OnPropertyChanged(PropertyChangedEventArgs) call is appended to the setter body.
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

        // Constructor(s). Two paths:
        //   ctorSpecs != null → multi-ctor: one tb.DefineConstructor per spec.
        //   ctorSpecs == null → single-ctor: backward-compat path using ctorBody /
        //                       ctorParamTypes / baseCtorArgIndices.
        if (ctorSpecs != null && ctorSpecs.Count > 0)
        {
            var pendingCtorHandlers = new List<(string Key, LispObject Body)>();
            foreach (var spec in ctorSpecs)
            {
                var specCtorTypes = spec.ParamTypes?.ToArray() ?? Type.EmptyTypes;

                ConstructorInfo specBaseCtor;
                if (spec.BaseArgIndices != null && spec.BaseArgIndices.Count > 0)
                {
                    var baseCtorTypes = spec.BaseArgIndices.Select(i => specCtorTypes[i]).ToArray();
                    specBaseCtor = baseType.GetConstructor(
                        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
                        binder: null, types: baseCtorTypes, modifiers: null)
                        ?? throw new ArgumentException(
                            $"Base type {baseType.FullName} has no accessible constructor matching types ({string.Join(", ", baseCtorTypes.Select(t => t.Name))})",
                            nameof(ctorSpecs));
                }
                else
                {
                    specBaseCtor = baseType.GetConstructor(
                        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
                        binder: null, types: Type.EmptyTypes, modifiers: null)
                        ?? throw new ArgumentException(
                            $"Base type {baseType.FullName} has no accessible parameterless constructor",
                            nameof(ctorSpecs));
                }

                var ctorDef = tb.DefineConstructor(
                    MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName,
                    CallingConventions.Standard, specCtorTypes);
                var ctorIl = ctorDef.GetILGenerator();
                ctorIl.Emit(OpCodes.Ldarg_0);
                if (spec.BaseArgIndices != null && spec.BaseArgIndices.Count > 0)
                    foreach (var idx in spec.BaseArgIndices)
                        ctorIl.Emit(OpCodes.Ldarg, idx + 1);
                ctorIl.Emit(OpCodes.Call, specBaseCtor);

                if (spec.Body != null)
                {
                    var ctorKey = MethodDispatchKey(CtorKey, spec.ParamTypes ?? Array.Empty<Type>());
                    ctorIl.Emit(OpCodes.Ldstr, fullName);
                    ctorIl.Emit(OpCodes.Ldstr, ctorKey);
                    ctorIl.Emit(OpCodes.Ldtoken, typeof(void));
                    ctorIl.Emit(OpCodes.Call, GetTypeFromHandleMI);
                    ctorIl.Emit(OpCodes.Ldarg_0);
                    ctorIl.Emit(OpCodes.Ldc_I4, specCtorTypes.Length);
                    ctorIl.Emit(OpCodes.Newarr, typeof(object));
                    for (int i = 0; i < specCtorTypes.Length; i++)
                    {
                        ctorIl.Emit(OpCodes.Dup);
                        ctorIl.Emit(OpCodes.Ldc_I4, i);
                        ctorIl.Emit(OpCodes.Ldarg, i + 1);
                        if (specCtorTypes[i].IsValueType)
                            ctorIl.Emit(OpCodes.Box, specCtorTypes[i]);
                        ctorIl.Emit(OpCodes.Stelem_Ref);
                    }
                    ctorIl.Emit(OpCodes.Call, DispatchMI);
                    ctorIl.Emit(OpCodes.Pop);
                    pendingCtorHandlers.Add((ctorKey, spec.Body));
                }
                ctorIl.Emit(OpCodes.Ret);
            }
            handlers.AddRange(pendingCtorHandlers);
        }
        else
        {
            // Single-ctor path (backward compat).
            var ctorTypes = (ctorParamTypes != null && ctorParamTypes.Count > 0)
                ? ctorParamTypes.ToArray() : Type.EmptyTypes;
            var ctor = tb.DefineConstructor(
                MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName,
                CallingConventions.Standard, ctorTypes);
            var cil = ctor.GetILGenerator();
            cil.Emit(OpCodes.Ldarg_0);
            if (baseCtorArgIndices != null && baseCtorArgIndices.Count > 0)
            {
                foreach (var idx in baseCtorArgIndices)
                    cil.Emit(OpCodes.Ldarg, idx + 1); // +1: arg 0 is 'this'
            }
            cil.Emit(OpCodes.Call, baseCtor!);

            if (ctorBody != null)
            {
                cil.Emit(OpCodes.Ldstr, fullName);
                cil.Emit(OpCodes.Ldstr, CtorKey);
                cil.Emit(OpCodes.Ldtoken, typeof(void));
                cil.Emit(OpCodes.Call, GetTypeFromHandleMI);
                cil.Emit(OpCodes.Ldarg_0);
                cil.Emit(OpCodes.Ldc_I4, ctorTypes.Length);
                cil.Emit(OpCodes.Newarr, typeof(object));
                for (int i = 0; i < ctorTypes.Length; i++)
                {
                    cil.Emit(OpCodes.Dup);
                    cil.Emit(OpCodes.Ldc_I4, i);
                    cil.Emit(OpCodes.Ldarg, i + 1); // arg 0 = this
                    if (ctorTypes[i].IsValueType)
                        cil.Emit(OpCodes.Box, ctorTypes[i]);
                    cil.Emit(OpCodes.Stelem_Ref);
                }
                cil.Emit(OpCodes.Call, DispatchMI);
                cil.Emit(OpCodes.Pop);
            }
            cil.Emit(OpCodes.Ret);

            if (ctorBody != null)
                handlers.Add((CtorKey, ctorBody));
        }

        // User-defined instance methods. Each body dispatches to the
        // corresponding Lisp lambda through DispatchLispMethod.
        // dispatch key = method name for no-param; name#Type1|Type2 for parameterized.
        if (methods != null)
        {
            var seenMethods = new HashSet<string>(StringComparer.Ordinal);
            foreach (var m in methods)
            {
                if (string.IsNullOrEmpty(m.Name))
                    throw new ArgumentException("method name must be non-empty", nameof(methods));
                var dispatchKey = MethodDispatchKey(m.Name, m.ParamTypes);
                if (!seenMethods.Add(dispatchKey))
                    throw new ArgumentException(
                        $"duplicate method overload: {dispatchKey}", nameof(methods));
                if (reservedMethodNames.Contains(m.Name))
                    throw new ArgumentException(
                        $"method name {m.Name} collides with an auto-generated event accessor",
                        nameof(methods));
                EmitLispDispatchMethod(tb, fullName, baseType, interfaces, m, dispatchKey);
                handlers.Add((dispatchKey, m.LispBody));
            }
        }

        var createdType = tb.CreateType()!;
        return (createdType, handlers);
    }

#if NET9_0_OR_GREATER
    /// <summary>
    /// Begin a saved class library: a single persisted assembly that many types
    /// are added to (via <see cref="LibraryBuilder.AddClass"/>) before one
    /// <see cref="LibraryBuilder.Save"/>. This is the aggregation unit — the
    /// original "1 define-class → 1 assembly" cannot express a real library.
    /// The saved DLL is a C#-referenceable facade (stage 1 semantics extended to N
    /// types): its method bodies dispatch through <see cref="DispatchLispMethod"/>,
    /// so consuming it at runtime needs DotCL.Runtime + the Lisp loaded (stage 2
    /// bakes bodies into IL). Requires .NET 9+ (PersistedAssemblyBuilder).
    /// </summary>
    /// <param name="savePath">Path the .dll is written to on Save().</param>
    /// <param name="assemblyName">
    /// Library assembly simple-name — what a C# consumer references. Sanitized
    /// to a legal AssemblyName.
    /// </param>
    /// <param name="version">Optional assembly version stamped into metadata.</param>
    /// <param name="corlibProfile">
    /// Reference corlib the saved DLL is retargeted to (default "netstandard"),
    /// so a C# consumer referencing System.Runtime facades does not hit CS0012.
    /// </param>
    public static LibraryBuilder BeginLibrary(string savePath, string assemblyName,
        Version? version = null, string corlibProfile = "netstandard")
    {
        CilAssembler.EnsureEmitAllowed("dotcl:library");
        if (string.IsNullOrEmpty(savePath))
            throw new ArgumentException("savePath must be non-empty", nameof(savePath));
        var simple = SanitizeAssemblyName(assemblyName);
        var an = new AssemblyName(simple);
        if (version != null) an.Version = version;
        var pab = new PersistedAssemblyBuilder(an, typeof(object).Assembly);
        var mb = pab.DefineDynamicModule(simple);
        return new LibraryBuilder(pab, mb, savePath, corlibProfile, simple);
    }

    /// <summary>
    /// Accumulates multiple public types into one persisted (saved) assembly.
    /// Obtained from <see cref="BeginLibrary"/>. Add each type with
    /// <see cref="AddClass"/>, then call <see cref="Save"/> once to write the
    /// .dll and retarget its corlib reference. Not thread-safe; drive from one
    /// thread. Saved types are facades and are never invoked in the emitting
    /// process, so AddClass does not register method handlers.
    /// </summary>
    public sealed class LibraryBuilder
    {
        private readonly PersistedAssemblyBuilder _pab;
        private readonly ModuleBuilder _mb;
        private readonly string _savePath;
        private readonly string _corlibProfile;
        private readonly string _assemblyName;
        // (docCommentId, summaryText) pairs for the sidecar XML doc file.
        private readonly List<(string Id, string Summary)> _docs = new();
        private bool _saved;

        internal LibraryBuilder(PersistedAssemblyBuilder pab, ModuleBuilder mb,
            string savePath, string corlibProfile, string assemblyName)
        {
            _pab = pab;
            _mb = mb;
            _savePath = savePath;
            _corlibProfile = corlibProfile;
            _assemblyName = assemblyName;
        }

        /// <summary>
        /// Record one XML doc-comment entry. <paramref name="id"/> is a doc
        /// member id (e.g. "T:MyLib.Calculator"); <paramref name="summary"/> is
        /// the &lt;summary&gt; text. Written to a sidecar &lt;name&gt;.xml on Save
        /// so a C# consumer's IntelliSense shows the summary.
        /// </summary>
        public void AddDoc(string id, string summary) => _docs.Add((id, summary));

        /// <summary>
        /// Add one public class to the library. Parameters mirror
        /// <see cref="DefineMinimalClass"/> (minus saveToPath). Returns the
        /// built Type. The handler pairs PopulateType produces are discarded:
        /// a saved facade type is never invoked in this process.
        /// </summary>
        public Type AddClass(string fullName, Type? baseType = null,
            IReadOnlyList<(string Name, Type Type)>? fields = null,
            IReadOnlyList<CustomAttributeBuilder>? attributes = null,
            IReadOnlyList<MethodSpec>? methods = null,
            LispObject? ctorBody = null,
            IReadOnlyList<(string Name, Type Type, bool Notify)>? properties = null,
            IReadOnlyList<Type>? interfaces = null,
            IReadOnlyList<(string Name, Type DelegateType)>? events = null,
            IReadOnlyList<Type>? ctorParamTypes = null,
            IReadOnlyList<int>? baseCtorArgIndices = null,
            IReadOnlyList<CtorSpec>? ctorSpecs = null)
        {
            if (_saved)
                throw new InvalidOperationException("cannot AddClass after Save()");
            CilAssembler.EnsureEmitAllowed("dotcl:library");
            var (type, _) = PopulateType(_mb, fullName, baseType, fields, attributes,
                methods, ctorBody, properties, interfaces, events, ctorParamTypes,
                baseCtorArgIndices, ctorSpecs);
            return type;
        }

        /// <summary>
        /// Add one public enum type. Unlike a class facade, an enum is pure
        /// metadata (named constants over an integral underlying type), so the
        /// emitted type is genuinely standalone — a C# consumer uses it with no
        /// DotCL.Runtime dependency and no Lisp loaded. <paramref name="members"/>
        /// pairs each literal name with its value (already the underlying type).
        /// </summary>
        public Type AddEnum(string fullName, Type underlyingType,
            IReadOnlyList<(string Name, object Value)> members)
        {
            if (_saved)
                throw new InvalidOperationException("cannot AddEnum after Save()");
            CilAssembler.EnsureEmitAllowed("dotcl:library");
            var eb = _mb.DefineEnum(fullName, TypeAttributes.Public, underlyingType);
            foreach (var (name, value) in members)
                eb.DefineLiteral(name, value);
            return eb.CreateTypeInfo()!;
        }

        /// <summary>
        /// Add a public interface type with abstract method signatures. Pure
        /// signature metadata (no bodies), so the emitted type is standalone: a
        /// C# consumer references and implements it with no DotCL.Runtime. Each
        /// method is emitted Public|Abstract|Virtual|NewSlot|HideBySig.
        /// </summary>
        public Type AddInterface(string fullName,
            IReadOnlyList<(string Name, Type ReturnType, IReadOnlyList<Type> ParamTypes)> methods)
        {
            if (_saved)
                throw new InvalidOperationException("cannot AddInterface after Save()");
            CilAssembler.EnsureEmitAllowed("dotcl:library");
            var tb = _mb.DefineType(fullName,
                TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);
            foreach (var m in methods)
                tb.DefineMethod(m.Name,
                    MethodAttributes.Public | MethodAttributes.Abstract | MethodAttributes.Virtual
                    | MethodAttributes.NewSlot | MethodAttributes.HideBySig,
                    m.ReturnType, m.ParamTypes.ToArray());
            return tb.CreateType();
        }

        /// <summary>
        /// Add a public delegate type of the given signature. A delegate is a
        /// sealed type over System.MulticastDelegate with a runtime-provided
        /// (object,IntPtr) ctor and Invoke method — pure metadata, so the emitted
        /// type is standalone (a C# consumer references it as a callback type with
        /// no DotCL.Runtime). Both members are MethodImplAttributes.Runtime, i.e.
        /// the CLR supplies their bodies; this is the canonical reflection-emit
        /// delegate shape.
        /// </summary>
        public Type AddDelegate(string fullName, Type returnType,
            IReadOnlyList<Type> paramTypes)
        {
            if (_saved)
                throw new InvalidOperationException("cannot AddDelegate after Save()");
            CilAssembler.EnsureEmitAllowed("dotcl:library");
            var tb = _mb.DefineType(fullName,
                TypeAttributes.Public | TypeAttributes.Sealed
                | TypeAttributes.AnsiClass | TypeAttributes.AutoClass,
                typeof(MulticastDelegate));
            var ctor = tb.DefineConstructor(
                MethodAttributes.Public | MethodAttributes.HideBySig
                | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName,
                CallingConventions.Standard, new[] { typeof(object), typeof(IntPtr) });
            ctor.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
            var invoke = tb.DefineMethod("Invoke",
                MethodAttributes.Public | MethodAttributes.HideBySig
                | MethodAttributes.NewSlot | MethodAttributes.Virtual,
                returnType, paramTypes.ToArray());
            invoke.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
            return tb.CreateType();
        }

        /// <summary>
        /// Add a public value type (C# <c>struct</c>) with public instance
        /// fields. Like enums/consts a fields-only struct is pure data — no Lisp
        /// dispatch — so the emitted type is standalone (a C# consumer reads/
        /// writes its fields with no DotCL.Runtime). Emitted sequential-layout
        /// sealed over System.ValueType; the implicit default ctor zero-inits.
        /// </summary>
        public Type AddStruct(string fullName,
            IReadOnlyList<(string Name, Type Type)> fields)
        {
            if (_saved)
                throw new InvalidOperationException("cannot AddStruct after Save()");
            CilAssembler.EnsureEmitAllowed("dotcl:library");
            var tb = _mb.DefineType(fullName,
                TypeAttributes.Public | TypeAttributes.SequentialLayout
                | TypeAttributes.Sealed | TypeAttributes.BeforeFieldInit,
                typeof(ValueType));
            foreach (var (name, type) in fields)
                tb.DefineField(name, type, FieldAttributes.Public);
            return tb.CreateType();
        }

        /// <summary>
        /// Add a static holder type of <c>public const</c> fields. Each constant
        /// is a compile-time literal (SetConstant) — like an enum it is pure
        /// metadata, so the type is standalone (no DotCL.Runtime, no Lisp) and
        /// the value is inlined into a C# consumer. Only literal-capable field
        /// types are valid (the integral/floating primitives, bool, char, string,
        /// or an enum); a non-literal type throws at DefineField/SetConstant time.
        /// The holder is emitted abstract+sealed (a C# <c>static class</c>).
        /// </summary>
        public Type AddConstants(string fullName,
            IReadOnlyList<(string Name, Type Type, object Value)> constants)
        {
            if (_saved)
                throw new InvalidOperationException("cannot AddConstants after Save()");
            CilAssembler.EnsureEmitAllowed("dotcl:library");
            var tb = _mb.DefineType(fullName,
                TypeAttributes.Public | TypeAttributes.Abstract | TypeAttributes.Sealed);
            foreach (var (name, type, value) in constants)
            {
                var fb = tb.DefineField(name, type,
                    FieldAttributes.Public | FieldAttributes.Static
                    | FieldAttributes.Literal | FieldAttributes.HasDefault);
                fb.SetConstant(value);
            }
            return tb.CreateType();
        }

        /// <summary>
        /// Write the assembly to disk and retarget its corlib reference so the
        /// DLL is C#-referenceable. Idempotent guard: throws if called twice.
        /// </summary>
        public void Save()
        {
            if (_saved) throw new InvalidOperationException("library already saved");
            _saved = true;
            var dir = System.IO.Path.GetDirectoryName(System.IO.Path.GetFullPath(_savePath));
            if (!string.IsNullOrEmpty(dir)) System.IO.Directory.CreateDirectory(dir);
            _pab.Save(_savePath);
            // PersistedAssemblyBuilder references System.Private.CoreLib (the
            // implementation corlib) for Object etc. A C# consumer references
            // System.Runtime / netstandard facades, so without retargeting it
            // gets CS0012 ("Object is defined in an assembly that is not
            // referenced"). Retarget to the netstandard facade, same as
            // FaslAssembler does for its fasls.
            FaslCorlibRetarget.RetargetCorlib(_savePath, _corlibProfile);
            WriteXmlDoc();
        }

        // Write the sidecar <name>.xml doc file next to the .dll (the standard
        // location the C# compiler auto-loads for IntelliSense). No-op if no docs
        // were recorded.
        private void WriteXmlDoc()
        {
            if (_docs.Count == 0) return;
            var xmlPath = System.IO.Path.ChangeExtension(_savePath, ".xml");
            var sb = new System.Text.StringBuilder();
            sb.Append("<?xml version=\"1.0\"?>\n<doc>\n    <assembly>\n        <name>");
            sb.Append(System.Security.SecurityElement.Escape(_assemblyName));
            sb.Append("</name>\n    </assembly>\n    <members>\n");
            foreach (var (id, summary) in _docs)
            {
                sb.Append("        <member name=\"");
                sb.Append(System.Security.SecurityElement.Escape(id));
                sb.Append("\">\n            <summary>");
                sb.Append(System.Security.SecurityElement.Escape(summary));
                sb.Append("</summary>\n        </member>\n");
            }
            sb.Append("    </members>\n</doc>\n");
            System.IO.File.WriteAllText(xmlPath, sb.ToString());
        }
    }
#endif

    /// <summary>
    /// Sanitize a dotted type namespace into a legal assembly simple-name: an
    /// AssemblyName rejects the characters the parser reads as attribute syntax
    /// ('=', ',', etc.). Mirrors FaslAssembler.SanitizeModuleName's intent.
    /// </summary>
    private static string SanitizeAssemblyName(string name)
    {
        var sb = new System.Text.StringBuilder(name.Length);
        foreach (var c in name)
            sb.Append(char.IsLetterOrDigit(c) || c == '.' || c == '_' || c == '-' ? c : '_');
        var s = sb.ToString();
        return string.IsNullOrEmpty(s) ? "DotclLibrary" : s;
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
    /// Emit the body of a user-defined method. The body boxes args into an
    /// <c>object[]</c>, dispatches to the registered Lisp lambda, then
    /// unboxes/casts the result to the declared return type.
    /// If <c>m.IsStatic</c> is true, a <c>public static</c> method is emitted
    /// (no <c>self</c>; dispatch through <see cref="DispatchLispStatic"/>) —
    /// the shape a function library exports.
    /// Otherwise an instance method through <see cref="DispatchLispMethod"/>:
    /// if <c>m.IsOverride</c> is true it is Virtual and tied to a matching base
    /// virtual method via <see cref="TypeBuilder.DefineMethodOverride"/>; else
    /// if the type declares interfaces and the method matches one by
    /// name+signature it is emitted as the implicit interface implementation
    /// (Virtual|NewSlot|Final|HideBySig + DefineMethodOverride per slot).
    /// </summary>
    private static void EmitLispDispatchMethod(TypeBuilder tb, string fullName,
        Type baseType, IReadOnlyList<Type>? interfaces, MethodSpec m, string dispatchKey)
    {
        var paramArr = m.ParamTypes.ToArray();

        if (m.IsStatic)
        {
            if (m.IsOverride)
                throw new ArgumentException(
                    $"static method {m.Name} cannot be an override", nameof(m));
            EmitLispStaticMethod(tb, fullName, m, dispatchKey, paramArr);
            return;
        }

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

        // DispatchLispMethod(typeName, dispatchKey, returnType, self, object[] args)
        il.Emit(OpCodes.Ldstr, fullName);
        il.Emit(OpCodes.Ldstr, dispatchKey);

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

        EmitReturnConversion(il, m.ReturnType);

        if (baseMethod != null)
            tb.DefineMethodOverride(method, baseMethod);

        if (ifaceTargets != null)
            foreach (var target in ifaceTargets)
                tb.DefineMethodOverride(method, target);
    }

    /// <summary>
    /// Emit a <c>public static</c> method whose body boxes its args into an
    /// <c>object[]</c> and dispatches to the registered Lisp function via
    /// <see cref="DispatchLispStatic"/> (no <c>self</c>). This is how a Lisp
    /// <c>defun</c> becomes a callable static member of a library type.
    /// </summary>
    private static void EmitLispStaticMethod(TypeBuilder tb, string fullName,
        MethodSpec m, string dispatchKey, Type[] paramArr)
    {
        var method = tb.DefineMethod(m.Name,
            MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            m.ReturnType, paramArr);

        if (m.Attributes != null)
            foreach (var ab2 in m.Attributes)
                method.SetCustomAttribute(ab2);

        var il = method.GetILGenerator();

        // DispatchLispStatic(typeName, dispatchKey, returnType, object[] args)
        il.Emit(OpCodes.Ldstr, fullName);
        il.Emit(OpCodes.Ldstr, dispatchKey);
        il.Emit(OpCodes.Ldtoken, m.ReturnType);
        il.Emit(OpCodes.Call, GetTypeFromHandleMI);

        il.Emit(OpCodes.Ldc_I4, paramArr.Length);
        il.Emit(OpCodes.Newarr, typeof(object));
        for (int i = 0; i < paramArr.Length; i++)
        {
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Ldc_I4, i);
            il.Emit(OpCodes.Ldarg, i); // static: arg 0 is the first parameter
            if (paramArr[i].IsValueType)
                il.Emit(OpCodes.Box, paramArr[i]);
            il.Emit(OpCodes.Stelem_Ref);
        }

        il.Emit(OpCodes.Call, DispatchStaticMI);

        EmitReturnConversion(il, m.ReturnType);
    }

    /// <summary>
    /// Emit the return-value conversion + Ret shared by instance and static
    /// dispatch bodies: pop for void, unbox for value types, castclass for
    /// non-object reference types, leave-as-is for object.
    /// </summary>
    private static void EmitReturnConversion(ILGenerator il, Type returnType)
    {
        if (returnType == typeof(void))
            il.Emit(OpCodes.Pop);
        else if (returnType.IsValueType)
            il.Emit(OpCodes.Unbox_Any, returnType);
        else if (returnType != typeof(object))
            il.Emit(OpCodes.Castclass, returnType);
        // else: result is already object; leave on stack

        il.Emit(OpCodes.Ret);
    }

    /// <summary>
    /// Emit a public event: a private delegate backing field, public
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

        // Raiser: public virtual OnName(args...). Fires the delegate
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

    private static readonly MethodInfo DispatchStaticMI =
        typeof(DynamicClassBuilder).GetMethod(nameof(DispatchLispStatic),
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

        // Cross the C#→Lisp boundary through InvokeForeignCallback so a Lisp error
        // in the override body is handled (dotcl:*foreign-callback-handler*) rather
        // than escaping as TargetInvocationException and crashing the .NET caller.
        var result = Runtime.InvokeForeignCallback(lispFn, lispArgs);

        if (returnType == typeof(void)) return null;
        return Runtime.LispToDotNet(result, returnType);
    }

    /// <summary>
    /// Runtime entry point for an emitted <c>public static</c> method body.
    /// Like <see cref="DispatchLispMethod"/> but with no <c>self</c>: looks up
    /// the Lisp function registered for (typeFullName, methodName), marshals the
    /// args, funcalls it through InvokeForeignCallback (so a Lisp error is
    /// handled rather than crashing the .NET caller), and marshals the result
    /// back for the declared <paramref name="returnType"/>.
    /// </summary>
    public static object? DispatchLispStatic(
        string typeFullName, string methodName, Type returnType, object?[] args)
    {
        if (!_methodHandlers.TryGetValue((typeFullName, methodName), out var lispFn))
            throw new InvalidOperationException(
                $"DispatchLispStatic: no Lisp handler registered for {typeFullName}.{methodName}");

        var lispArgs = new LispObject[args.Length];
        for (int i = 0; i < args.Length; i++)
            lispArgs[i] = Runtime.DotNetToLisp(args[i]);

        var result = Runtime.InvokeForeignCallback(lispFn, lispArgs);

        if (returnType == typeof(void)) return null;
        return Runtime.LispToDotNet(result, returnType);
    }
}
