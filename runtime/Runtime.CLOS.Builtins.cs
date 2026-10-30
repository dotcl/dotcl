using System.Collections.Concurrent;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    internal static void RegisterCLOSBuiltins()
    {
        // CLOS internal primitives
        Startup.RegisterBinary("%MAKE-GF", Runtime.MakeGF);
        Startup.RegisterBinary("%REGISTER-GF", Runtime.RegisterGF);
        Startup.RegisterUnary("%CLEAR-DEFGENERIC-INLINE-METHODS", Runtime.ClearDefgenericInlineMethods);
        Startup.RegisterBinary("%MARK-DEFGENERIC-INLINE-METHOD", Runtime.MarkDefgenericInlineMethod);
        Emitter.CilAssembler.RegisterFunction("%REGISTER-METHOD-COMBINATION",
            new LispFunction(args =>
            {
                var name = ((LispString)args[0]).Value;
                if (args.Length > 4 && args[4] is Symbol nameSym) name = MethodCombinationKey(nameSym);
                var op = ((LispString)args[1]).Value;
                bool identity = args.Length > 2 && args[2] is not Nil;
                // The fourth argument, the operator symbol itself, is passed by
                // current DEFINE-METHOD-COMBINATION expansions; code compiled
                // before that passes three.
                var opSym = args.Length > 3 ? args[3] as Symbol : null;
                Runtime.RegisterMethodCombination(name, op, identity, opSym);
                return Nil.Instance;
            }));
        // Long-form method combination registration:
        // (%register-long-method-combination name group-specs-list body-function)
        // group-specs-list: ((name qualifier-pattern . options) ...)
        //   qualifier-pattern: * (match all) or NIL (unqualified) or a qualifier symbol
        //   options: :order :most-specific-first/:most-specific-last, :required t/nil
        Emitter.CilAssembler.RegisterFunction("%REGISTER-LONG-METHOD-COMBINATION",
            new LispFunction(args =>
            {
                var name = ((LispString)args[0]).Value;
                if (args.Length > 4 && args[4] is Symbol nameSym) name = MethodCombinationKey(nameSym);
                var specFn = (LispFunction)args[1];
                var bodyFn = (LispFunction)args[2];
                // A fourth argument (possibly NIL) marks the current calling convention;
                // three arguments come from code compiled by an older compiler.
                _longFormMCRegistry[name] = new LongFormMethodCombination
                {
                    SpecFunction = specFn, BodyFunction = bodyFn,
                    ArgumentsInfo = args.Length > 3 ? args[3] : null
                };
                return Nil.Instance;
            }));
        Emitter.CilAssembler.RegisterFunction("%CALL-METHOD-RT",
            new LispFunction(args => Runtime.CallMethodRt(args[0], args[1], args[2]),
                "%CALL-METHOD-RT", 3));
        Emitter.CilAssembler.RegisterFunction("%MAKE-METHOD-RT",
            new LispFunction(args => Runtime.MakeMethod(Nil.Instance, Nil.Instance, args[0]),
                "%MAKE-METHOD-RT", 1));
        // CLASS-NAME as a proper GF
        {
            var cnSym = Startup.Sym("CLASS-NAME");
            var cnGF = (GenericFunction)Runtime.MakeGF(cnSym, new Fixnum(1));
            cnGF.RequiredCount = 1;
            cnGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(cnSym, cnGF);
            cnSym.Function = cnGF;
            Emitter.CilAssembler.RegisterFunction("CLASS-NAME", cnGF);

            var tCls3 = Runtime.FindClass(Startup.Sym("T"));
            var cnSpecializers = new Cons(tCls3, Nil.Instance);
            var cnDefaultMethod = Runtime.MakeMethod(cnSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.ClassName(args[0])));
            ((LispMethod)cnDefaultMethod).RequiredCount = 1;
            Runtime.AddMethod(cnGF, cnDefaultMethod);
        }
        // (SETF CLASS-NAME) as a proper GF
        {
            var scnSym = Startup.Sym("(SETF CLASS-NAME)");
            // Named by its key symbol like every other (SETF accessor) generic
            // function, so GENERIC-FUNCTION-NAME answers (SETF CLASS-NAME), not CLASS-NAME.
            _setfKeyAccessors.TryAdd(scnSym, Startup.Sym("CLASS-NAME"));
            var scnGF = (GenericFunction)Runtime.MakeGF(scnSym, new Fixnum(2));
            scnGF.RequiredCount = 2;
            scnGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(scnSym, scnGF);
            scnSym.Function = scnGF;
            Emitter.CilAssembler.RegisterFunction("(SETF CLASS-NAME)", scnGF);

            var tCls4 = Runtime.FindClass(Startup.Sym("T"));
            var scnSpecializers = new Cons(tCls4, new Cons(tCls4, Nil.Instance));
            var scnDefaultMethod = Runtime.MakeMethod(scnSpecializers, Nil.Instance,
                new LispFunction(args => {
                    var newName = args[0];
                    var cls = args[1];
                    if (cls is not LispClass)
                        throw new LispErrorException(new LispTypeError("(SETF CLASS-NAME): not a class", cls));
                    // AMOP defines this as reinitialization, not a field write, so a
                    // metaclass with a REINITIALIZE-INSTANCE method sees the change.
                    // The :after below is what actually applies the name.
                    if (Startup.Sym("REINITIALIZE-INSTANCE").Function is LispFunction reinit)
                        reinit.Invoke(new LispObject[] { cls, Startup.Keyword("NAME"), newName });
                    return newName;
                }));
            ((LispMethod)scnDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(scnGF, scnDefaultMethod);
        }
        // ALLOCATE-INSTANCE as a proper GF
        {
            var aiSym = Startup.Sym("ALLOCATE-INSTANCE");
            var aiGF = (GenericFunction)Runtime.MakeGF(aiSym, new Fixnum(-1));
            aiGF.RequiredCount = 1;
            aiGF.HasRest = true;
            aiGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(aiSym, aiGF);
            aiSym.Function = aiGF;
            Emitter.CilAssembler.RegisterFunction("ALLOCATE-INSTANCE", aiGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var aiSpecializers = new Cons(tCls, Nil.Instance);
            var aiDefaultMethod = Runtime.MakeMethod(aiSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.MakeInstanceRaw(args[0])));
            ((LispMethod)aiDefaultMethod).RequiredCount = 1;
            ((LispMethod)aiDefaultMethod).HasRest = true;
            Runtime.AddMethod(aiGF, aiDefaultMethod);

            // allocate-instance (standard-generic-function) -> real GenericFunction
            var sgfCls = Runtime.FindClass(Startup.Sym("STANDARD-GENERIC-FUNCTION"));
            var sgfAllocM = Runtime.MakeMethod(new Cons(sgfCls, Nil.Instance), Nil.Instance,
                new LispFunction(allocArgs => {
                    var newGf = Runtime.NewDispatchingGF(Startup.Sym("UNNAMED"), -1);
                    newGf.RequiredCount = 0;
                    newGf.LambdaListInfoSet = true;
                    return newGf;
                }));
            ((LispMethod)sgfAllocM).RequiredCount = 1;
            ((LispMethod)sgfAllocM).HasRest = true;
            Runtime.AddMethod(aiGF, sgfAllocM);

            // allocate-instance (standard-method) -> raw LispMethod
            var smCls = Runtime.FindClass(Startup.Sym("STANDARD-METHOD"));
            var smAllocM = Runtime.MakeMethod(new Cons(smCls, Nil.Instance), Nil.Instance,
                new LispFunction(allocArgs => new LispMethod()));
            ((LispMethod)smAllocM).RequiredCount = 1;
            ((LispMethod)smAllocM).HasRest = true;
            Runtime.AddMethod(aiGF, smAllocM);
        }

        // METHOD-QUALIFIERS as a proper GF
        {
            var mqSym = Startup.Sym("METHOD-QUALIFIERS");
            var mqGF = (GenericFunction)Runtime.MakeGF(mqSym, new Fixnum(1));
            mqGF.RequiredCount = 1;
            mqGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(mqSym, mqGF);
            mqSym.Function = mqGF;
            Emitter.CilAssembler.RegisterFunction("METHOD-QUALIFIERS", mqGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var mqSpecializers = new Cons(tCls, Nil.Instance);
            var mqDefaultMethod = Runtime.MakeMethod(mqSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args[0] is not LispMethod method)
                        throw new LispErrorException(new LispTypeError("METHOD-QUALIFIERS: not a method", args[0]));
                    LispObject result = Nil.Instance;
                    for (int i = method.Qualifiers.Length - 1; i >= 0; i--)
                        result = new Cons(method.Qualifiers[i], result);
                    return result;
                }));
            ((LispMethod)mqDefaultMethod).RequiredCount = 1;
            Runtime.AddMethod(mqGF, mqDefaultMethod);
        }
        Startup.RegisterBinary("SLOT-BOUNDP", Runtime.SlotBoundp);
        Startup.RegisterBinary("SLOT-VALUE", Runtime.SlotValue);
        // SLOT-VALUE-USING-CLASS and (SETF SLOT-VALUE-USING-CLASS) as proper GFs (AMOP 5.4)
        {
            var tCls = Runtime.FindClass(Startup.Sym("T"));
            // SLOT-VALUE-USING-CLASS (class instance slot-def) -> value
            var svucSym = Startup.Sym("SLOT-VALUE-USING-CLASS");
            var svucGF = (GenericFunction)Runtime.MakeGF(svucSym, new Fixnum(3));
            svucGF.RequiredCount = 3;
            svucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(svucSym, svucGF);
            svucSym.Function = svucGF;
            Emitter.CilAssembler.RegisterFunction("SLOT-VALUE-USING-CLASS", svucGF);
            var svucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var svucDefault = Runtime.MakeMethod(svucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 3)
                    throw new LispErrorException(new LispProgramError("SLOT-VALUE-USING-CLASS: wrong number of arguments"));
                var obj = args[1] is LispInstanceCondition lic2 ? lic2.Instance : args[1];
                // A structure instance has slots too (SBCL serves them here as well).
                if (LispStruct.TryAs(obj, out _) && args[2] is SlotDefinition stSlot)
                    return Runtime.SlotValue(obj, stSlot.Name);
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("SLOT-VALUE-USING-CLASS: not a CLOS instance", args[1]));
                if (args[2] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("SLOT-VALUE-USING-CLASS: slot-def is not a slot definition", args[2]));
                if (!inst.Class.TryGetSlotIndex(slotDef, out int idx))
                    throw new LispErrorException(new LispError($"SLOT-VALUE-USING-CLASS: no slot named {slotDef.Name.Name} in {inst.Class.Name.Name}"));
                return Runtime.SlotValueDirect(inst, idx, slotDef.Name, slotDef.Name.Name);
            }));
            ((LispMethod)svucDefault).RequiredCount = 3;
            Runtime.AddMethod(svucGF, svucDefault);
            Runtime.SlotProtocolDefaults.Add((LispMethod)svucDefault);
            Runtime.SlotProtocolGFs.Add(svucGF);
            // (SETF SLOT-VALUE-USING-CLASS) (new-value class instance slot-def) -> new-value
            var setfSvucName = new Cons(Startup.Sym("SETF"), new Cons(svucSym, Nil.Instance));
            var setfSvucGF = (GenericFunction)Runtime.MakeGF(setfSvucName, new Fixnum(4));
            setfSvucGF.RequiredCount = 4;
            setfSvucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(setfSvucName, setfSvucGF); // also sets svucSym.SetfFunction
            Emitter.CilAssembler.RegisterFunction("(SETF SLOT-VALUE-USING-CLASS)", setfSvucGF);
            var setfSvucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance))));
            var setfSvucDefault = Runtime.MakeMethod(setfSvucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 4)
                    throw new LispErrorException(new LispProgramError("(SETF SLOT-VALUE-USING-CLASS): wrong number of arguments"));
                var newVal = args[0];
                var obj = args[2] is LispInstanceCondition lic3 ? lic3.Instance : args[2];
                if (LispStruct.TryAs(obj, out _) && args[3] is SlotDefinition stSlot)
                    return Runtime.SetSlotValue(obj, stSlot.Name, newVal);
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("(SETF SLOT-VALUE-USING-CLASS): not a CLOS instance", args[2]));
                if (args[3] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("(SETF SLOT-VALUE-USING-CLASS): slot-def is not a slot definition", args[3]));
                if (!inst.Class.TryGetSlotIndex(slotDef, out int idx))
                    throw new LispErrorException(new LispError($"(SETF SLOT-VALUE-USING-CLASS): no slot named {slotDef.Name.Name} in {inst.Class.Name.Name}"));
                return Runtime.SetSlotValueDirect(inst, idx, slotDef.Name.Name, newVal);
            }));
            ((LispMethod)setfSvucDefault).RequiredCount = 4;
            Runtime.AddMethod(setfSvucGF, setfSvucDefault);
            Runtime.SlotProtocolDefaults.Add((LispMethod)setfSvucDefault);
            Runtime.SlotProtocolGFs.Add(setfSvucGF);

            // SLOT-BOUNDP-USING-CLASS (class instance slot-def) -> boolean (AMOP 5.4).
            // The default consults the raw slot vector; custom metaclasses (e.g. McCLIM's
            // dynamic slots) override to consult their own backing store so initforms and
            // slot-boundp see the right state.
            var sbucSym = Startup.Sym("SLOT-BOUNDP-USING-CLASS");
            var sbucGF = (GenericFunction)Runtime.MakeGF(sbucSym, new Fixnum(3));
            sbucGF.RequiredCount = 3;
            sbucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(sbucSym, sbucGF);
            sbucSym.Function = sbucGF;
            Emitter.CilAssembler.RegisterFunction("SLOT-BOUNDP-USING-CLASS", sbucGF);
            var sbucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var sbucDefault = Runtime.MakeMethod(sbucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 3)
                    throw new LispErrorException(new LispProgramError("SLOT-BOUNDP-USING-CLASS: wrong number of arguments"));
                var obj = args[1] is LispInstanceCondition licb ? licb.Instance : args[1];
                if (LispStruct.TryAs(obj, out _) && args[2] is SlotDefinition stSlot)
                    return Runtime.SlotBoundp(obj, stSlot.Name);
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("SLOT-BOUNDP-USING-CLASS: not a CLOS instance", args[1]));
                if (args[2] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("SLOT-BOUNDP-USING-CLASS: slot-def is not a slot definition", args[2]));
                if (!inst.Class.TryGetSlotIndex(slotDef, out int idx))
                    return Nil.Instance;
                return Runtime.SlotBoundpDirect(inst, idx, slotDef.Name.Name) ? T.Instance : Nil.Instance;
            }));
            ((LispMethod)sbucDefault).RequiredCount = 3;
            Runtime.AddMethod(sbucGF, sbucDefault);
            Runtime.SlotProtocolDefaults.Add((LispMethod)sbucDefault);
            Runtime.SlotProtocolGFs.Add(sbucGF);

            // SLOT-MAKUNBOUND-USING-CLASS (class instance slot-def) -> instance (AMOP 5.4).
            var smucSym = Startup.Sym("SLOT-MAKUNBOUND-USING-CLASS");
            var smucGF = (GenericFunction)Runtime.MakeGF(smucSym, new Fixnum(3));
            smucGF.RequiredCount = 3;
            smucGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(smucSym, smucGF);
            smucSym.Function = smucGF;
            Emitter.CilAssembler.RegisterFunction("SLOT-MAKUNBOUND-USING-CLASS", smucGF);
            var smucSpec = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var smucDefault = Runtime.MakeMethod(smucSpec, Nil.Instance, new LispFunction(args => {
                if (args.Length < 3)
                    throw new LispErrorException(new LispProgramError("SLOT-MAKUNBOUND-USING-CLASS: wrong number of arguments"));
                var obj = args[1] is LispInstanceCondition licm ? licm.Instance : args[1];
                if (obj is not LispInstance inst)
                    throw new LispErrorException(new LispTypeError("SLOT-MAKUNBOUND-USING-CLASS: not a CLOS instance", args[1]));
                if (args[2] is not SlotDefinition slotDef)
                    throw new LispErrorException(new LispTypeError("SLOT-MAKUNBOUND-USING-CLASS: slot-def is not a slot definition", args[2]));
                if (inst.Class.TryGetSlotIndex(slotDef, out int idx))
                    Runtime.SlotMakunboundDirect(inst, idx, slotDef.Name.Name);
                return inst;
            }));
            ((LispMethod)smucDefault).RequiredCount = 3;
            Runtime.AddMethod(smucGF, smucDefault);
            Runtime.SlotProtocolDefaults.Add((LispMethod)smucDefault);
            Runtime.SlotProtocolGFs.Add(smucGF);
        }
        Emitter.CilAssembler.RegisterFunction("SLOT-MAKUNBOUND", new LispFunction(args => {
            if (args.Length != 2) throw new LispErrorException(new LispProgramError("SLOT-MAKUNBOUND requires exactly 2 arguments"));
            var obj0 = args[0] is LispInstanceCondition lic0 ? lic0.Instance : args[0];
            string name = args[1] switch {
                Symbol sym => sym.Name,
                _ => args[1].ToString()
            };
            // Metaclass-added slot on a class metaobject.
            if (obj0 is LispClass klass && klass.Metaclass != null)
            {
                klass.ExtraSlots?.TryRemove(name, out _);
                return klass;
            }
            // Funcallable instance: its slots live in ExtraSlots (see SlotBoundp).
            if (obj0 is GenericFunction gfm && gfm.StoredClass != null
                && gfm.StoredClass.SlotIndex.ContainsKey(name))
            {
                gfm.ExtraSlots?.TryRemove(name, out _);
                return gfm;
            }
            if (obj0 is LispCondition cond)
            {
                var ccls = Runtime.ClassOf(cond) as LispClass;
                if (ccls != null && ccls.SlotIndex.ContainsKey(name) && Runtime.TryWriteNativeConditionSlot(cond, name, null))
                    return args[0];
                if (Startup.Sym("SLOT-MISSING").Function is LispFunction csm)
                {
                    Runtime.CallForPrimary(csm, ccls ?? (LispObject)Nil.Instance, cond,
                        args[1] is Symbol ? args[1] : Startup.Sym(name), Startup.Sym("SLOT-MAKUNBOUND"));
                    return args[0];
                }
                throw new LispErrorException(new LispError(
                    $"SLOT-MAKUNBOUND: no slot named {name} in condition {cond.ConditionTypeName}"));
            }
            if (obj0 is not LispInstance inst)
                throw new LispErrorException(new LispTypeError("SLOT-MAKUNBOUND: not a CLOS instance", args[0]));
            if (!inst.Class.TryGetSlotIndex(args[1], name, out int idx))
            {
                if (Startup.Sym("SLOT-MISSING").Function is LispFunction slotMissing)
                {
                    Runtime.CallForPrimary(slotMissing, inst.Class, inst, args[1] is Symbol ? args[1] : Startup.Sym(name), Startup.Sym("SLOT-MAKUNBOUND"));
                    return args[0];
                }
                throw new LispErrorException(new LispError(
                    $"SLOT-MAKUNBOUND: no slot named {name} in class {inst.Class.Name.Name}"));
            }
            // AMOP 5.4: dispatch through slot-makunbound-using-class for custom metaclasses.
            if (UsesSlotProtocol(inst.Class) && Startup.Sym("SLOT-MAKUNBOUND-USING-CLASS").Function is LispFunction smucFn)
            {
                Runtime.CallForPrimary(smucFn, inst.Class, inst, inst.Class.EffectiveSlots[idx]);
                return args[0];
            }
            Runtime.SlotMakunboundDirect(inst, idx, name);
            return args[0];
        }));
        Emitter.CilAssembler.RegisterFunction("SLOT-EXISTS-P", new LispFunction(args => {
            if (args.Length != 2) throw new LispErrorException(new LispProgramError("SLOT-EXISTS-P requires exactly 2 arguments"));
            var obj0 = args[0] is LispInstanceCondition lic1 ? lic1.Instance : args[0];
            return Runtime.SlotExists(obj0, args[1]);
        }));
        // SLOT-MISSING generic function
        var slotMissingSym = Startup.Sym("SLOT-MISSING");
        GenericFunction slotMissingGF = null!;
        slotMissingGF = new GenericFunction(slotMissingSym, -1,
            args => { Runtime.CheckArgCount("SLOT-MISSING", args, 4, 5); return Runtime.DispatchGFOrDefault(slotMissingGF, args, Runtime.SlotMissingDefault); });
        slotMissingSym.Function = slotMissingGF;
        Runtime.RegisterGF(slotMissingSym, slotMissingGF);
        Emitter.CilAssembler.RegisterFunction("SLOT-MISSING", slotMissingGF);
        // SLOT-UNBOUND generic function
        var slotUnboundSym = Startup.Sym("SLOT-UNBOUND");
        GenericFunction slotUnboundGF = null!;
        slotUnboundGF = new GenericFunction(slotUnboundSym, 3,
            args => { Runtime.CheckArgCount("SLOT-UNBOUND", args, 3, 3); return Runtime.DispatchGFOrDefault(slotUnboundGF, args, Runtime.SlotUnboundDefault); });
        slotUnboundSym.Function = slotUnboundGF;
        Runtime.RegisterGF(slotUnboundSym, slotUnboundGF);
        Emitter.CilAssembler.RegisterFunction("SLOT-UNBOUND", slotUnboundGF);

        // UPDATE-INSTANCE-FOR-DIFFERENT-CLASS as a proper GF
        {
            var uifdcSym = Startup.Sym("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS");
            var uifdcGF = (GenericFunction)Runtime.MakeGF(uifdcSym, new Fixnum(2));
            Runtime.RegisterGF(uifdcSym, uifdcGF);
            uifdcSym.Function = uifdcGF;
            Emitter.CilAssembler.RegisterFunction("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS", uifdcGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var specializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(args => {
                    if (args.Length < 2) return Nil.Instance;
                    var previous = args[0];
                    var current = args[1];
                    LispObject addedSlots = Nil.Instance;
                    if (previous is LispInstance prevInst && current is LispInstance curInst)
                    {
                        foreach (var slot in curInst.Class.EffectiveSlots)
                        {
                            if (!prevInst.Class.TryGetSlotIndexExact(slot.Name, out _))
                                addedSlots = new Cons(slot.Name, addedSlots);
                        }
                    }
                    var sharedInitSym2 = Startup.Sym("SHARED-INITIALIZE");
                    if (sharedInitSym2.Function is LispFunction sharedInitFn)
                    {
                        var siArgs = new LispObject[2 + (args.Length - 2)];
                        siArgs[0] = current;
                        siArgs[1] = addedSlots;
                        Array.Copy(args, 2, siArgs, 2, args.Length - 2);
                        sharedInitFn.Invoke(siArgs);
                    }
                    return current;
                }));
            Runtime.AddMethod(uifdcGF, defaultMethod);
        }

        // MAKE-INSTANCES-OBSOLETE as a proper GF
        var mioSym = Startup.Sym("MAKE-INSTANCES-OBSOLETE");
        Func<LispObject[], LispObject> mioDefault = args => {
            if (args.Length != 1)
                throw new LispErrorException(new LispProgramError($"MAKE-INSTANCES-OBSOLETE: wrong number of arguments: {args.Length} (expected 1)"));
            var target = args[0] is Symbol sym2 ? Runtime.FindClassOrNil(sym2) : args[0];
            if (target is not LispClass cls)
                throw new LispErrorException(new LispTypeError(
                    "MAKE-INSTANCES-OBSOLETE: not a class", args[0]));
            // Built-in and structure classes have no updatable instances.
            if (!cls.IsBuiltIn && !cls.IsStructureClass)
                lock (cls.DefLock)
                    cls.SupersedeLayout(cls.EffectiveSlots,
                        LispClass.CaptureSharedValues(cls.EffectiveSlots, cls.ClassPrecedenceList));
            return cls;
        };
        GenericFunction mioGF = null!;
        mioGF = new GenericFunction(mioSym, 1,
            args => Runtime.DispatchGFOrDefault(mioGF, args, mioDefault));
        mioSym.Function = mioGF;
        Runtime.RegisterGF(mioSym, mioGF);
        Emitter.CilAssembler.RegisterFunction("MAKE-INSTANCES-OBSOLETE", mioGF);

        // NO-APPLICABLE-METHOD
        var namSym = Startup.Sym("NO-APPLICABLE-METHOD");
        Func<LispObject[], LispObject> namDefault = args =>
            throw Runtime.NoApplicableMethodError(
                args.Length > 0 ? args[0] : Nil.Instance,
                args.Length > 1 ? args.AsSpan(1).ToArray() : Array.Empty<LispObject>());
        GenericFunction namGF = null!;
        namGF = new GenericFunction(namSym, -1,
            args => { Runtime.CheckArgCount("NO-APPLICABLE-METHOD", args, 1, -1); return Runtime.DispatchGFOrDefault(namGF, args, namDefault); });
        namSym.Function = namGF;
        Runtime.RegisterGF(namSym, namGF);
        Emitter.CilAssembler.RegisterFunction("NO-APPLICABLE-METHOD", namGF);

        // NO-NEXT-METHOD
        var nnmSym = Startup.Sym("NO-NEXT-METHOD");
        Func<LispObject[], LispObject> nnmDefault = args => {
            throw new LispErrorException(new LispError("No next method"));
        };
        GenericFunction nnmGF = null!;
        nnmGF = new GenericFunction(nnmSym, -1,
            args => { Runtime.CheckArgCount("NO-NEXT-METHOD", args, 2, -1); return Runtime.DispatchGFOrDefault(nnmGF, args, nnmDefault); });
        nnmGF.RequiredCount = 2;
        nnmGF.HasRest = true;
        nnmGF.LambdaListInfoSet = true;
        nnmSym.Function = nnmGF;
        Runtime.RegisterGF(nnmSym, nnmGF);
        Emitter.CilAssembler.RegisterFunction("NO-NEXT-METHOD", nnmGF);
        // A real primary lets a user method continue into the default error.
        var nnmT = Runtime.FindClass(Startup.Sym("T"));
        var nnmSpecializers = new Cons(nnmT, new Cons(nnmT, Nil.Instance));
        var nnmDefaultMethod = Runtime.MakeMethod(nnmSpecializers, Nil.Instance,
            new LispFunction(nnmDefault));
        ((LispMethod)nnmDefaultMethod).RequiredCount = 2;
        ((LispMethod)nnmDefaultMethod).HasRest = true;
        Runtime.AddMethod(nnmGF, nnmDefaultMethod);
        // ADD-METHOD as a proper GF
        {
            var amSym = Startup.Sym("ADD-METHOD");
            var amGF = (GenericFunction)Runtime.MakeGF(amSym, new Fixnum(2));
            amGF.RequiredCount = 2;
            amGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(amSym, amGF);
            amSym.Function = amGF;
            Emitter.CilAssembler.RegisterFunction("ADD-METHOD", amGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var amSpecializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var amDefaultMethod = Runtime.MakeMethod(amSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.AddMethod(args[0], args[1])));
            ((LispMethod)amDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(amGF, amDefaultMethod);
        }
        // REMOVE-METHOD as a proper GF
        {
            var rmSym = Startup.Sym("REMOVE-METHOD");
            var rmGF = (GenericFunction)Runtime.MakeGF(rmSym, new Fixnum(2));
            rmGF.RequiredCount = 2;
            rmGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(rmSym, rmGF);
            rmSym.Function = rmGF;
            Emitter.CilAssembler.RegisterFunction("REMOVE-METHOD", rmGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var rmSpecializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var rmDefaultMethod = Runtime.MakeMethod(rmSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.RemoveMethod(args[0], args[1])));
            ((LispMethod)rmDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(rmGF, rmDefaultMethod);
        }
        // COMPUTE-APPLICABLE-METHODS as a proper GF
        {
            var camSym = Startup.Sym("COMPUTE-APPLICABLE-METHODS");
            var camGF = (GenericFunction)Runtime.MakeGF(camSym, new Fixnum(2));
            camGF.RequiredCount = 2;
            camGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(camSym, camGF);
            camSym.Function = camGF;
            Emitter.CilAssembler.RegisterFunction("COMPUTE-APPLICABLE-METHODS", camGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var camSpecializers = new Cons(tCls, new Cons(tCls, Nil.Instance));
            var camDefaultMethod = Runtime.MakeMethod(camSpecializers, Nil.Instance,
                new LispFunction(args => Runtime.ComputeApplicableMethods(args[0], args[1])));
            ((LispMethod)camDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(camGF, camDefaultMethod);
        }
        // NEXT-METHOD-P and CALL-NEXT-METHOD.
        // NEXT-METHOD-P accepts exactly 0 arguments (CLHS): passing one is a
        // PROGRAM-ERROR. The compiler rejects it statically (the next-method-p
        // special-form handler) and the per-invocation captured closure checks it
        // at call time, but this globally registered function silently ignored
        // whatever it was handed: so an interpreted method body, where the
        // operator resolves through this binding, returned NIL/T instead of
        // signalling. Same check as CapturedNmp, so all three paths agree.
        Emitter.CilAssembler.RegisterFunction("NEXT-METHOD-P",
            new LispFunction(args => {
                if (args.Length != 0)
                    throw new LispErrorException(new LispProgramError(
                        $"NEXT-METHOD-P: wrong number of arguments: {args.Length} (expected 0)"));
                return Runtime.NextMethodP();
            }, "NEXT-METHOD-P", 0));
        Emitter.CilAssembler.RegisterFunction("CALL-NEXT-METHOD",
            new LispFunction(Runtime.CallNextMethod, "CALL-NEXT-METHOD", -1));

        // CLOS intrinsics callable as functions, for the emit-free tree-walk
        // interpreter (%mini-eval). The compiler emits these as inline
        // (:call "Runtime.X") via the special-form table; the compiled path is
        // unaffected. defclass/defgeneric/defmethod macro-expand into calls to
        // these (DOTCL-INTERNAL package), so the interpreter must be able to
        // apply them. All map to existing Runtime methods: no new emit.
        // A fixed-arity builtin also gets a typed direct delegate, so a call site of
        // that arity reaches it through Invoke0..3 instead of InvokeSlow (which pushes
        // a call-stack frame and re-checks the argument count). The wrapper still packs
        // the small array the shared body expects; what is saved is the slow-path
        // bookkeeping, and these run once per defclass / defmethod / slot access during
        // compilation. RegisterUnary/RegisterBinary in Startup have done this all along;
        // the CLOS builtins were the set that missed out.
        void RegClos(string name, System.Func<LispObject[], LispObject> fn, int arity = -1)
        {
            var lispFn = new LispFunction(fn, name, arity);
            switch (arity)
            {
                case 0: lispFn.SetDirectDelegate((Func<LispObject>)(() => fn(System.Array.Empty<LispObject>()))); break;
                case 1: lispFn.SetDirectDelegate((Func<LispObject, LispObject>)(a => fn(new[] { a }))); break;
                case 2: lispFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)((a, b) => fn(new[] { a, b }))); break;
                case 3: lispFn.SetDirectDelegate((Func<LispObject, LispObject, LispObject, LispObject>)((a, b, c) => fn(new[] { a, b, c }))); break;
            }
            Emitter.CilAssembler.RegisterFunction(name, lispFn);
        }
        RegClos("%MAKE-CLASS", a => Runtime.MakeClass(a[0], a[1], a[2]), 3);
        RegClos("%REGISTER-CLASS", a => Runtime.RegisterClass(a[0]), 1);
        RegClos("%MAKE-SLOT-DEF", a => Runtime.MakeSlotDef(a[0], a[1], a[2]), 3);
        // The :ALLOCATION :CLASS variant of the slot-def lowering target. It was the
        // one member of this family with no function binding, so the DEFCLASS
        // expansion, which the tree-walk evaluator runs as real code, died with
        // "Undefined function: %MAKE-SLOT-DEF-WITH-ALLOCATION" as soon as a slot
        // carried :allocation (ansi-test CLASS-REDEFINITION.1/2/3). Compiled code
        // never noticed: the compiler emits Runtime.MakeSlotDefWithAllocation
        // inline from its own table and never looks the name up.
        RegClos("%MAKE-SLOT-DEF-WITH-ALLOCATION",
            a => Runtime.MakeSlotDefWithAllocation(a[0], a[1], a[2], a[3]), 4);
        // The custom-:METACLASS variant of the class lowering target, missing for the
        // same reason %MAKE-SLOT-DEF-WITH-ALLOCATION was: DEFCLASS emits it only when
        // the class names a metaclass, and compiled code never looks the name up
        // (the compiler emits Runtime.MakeClassFull inline). The tree-walk evaluator
        // runs the expansion as real code, so every (defclass ... (:metaclass ...))
        // died with "Undefined function: %MAKE-CLASS-FULL".
        RegClos("%MAKE-CLASS-FULL", a => Runtime.MakeClassFull(a[0], a[1], a[2], a[3]), 4);
        RegClos("%MAKE-CLASS-FULL-OPTIONS",
            a => Runtime.MakeClassFullOptions(a[0], a[1], a[2], a[3], a[4]), 5);
        RegClos("%SLOT-DEF-RAW-OPTIONS", a => Runtime.SetSlotDefRawOptions(a[0], a[1]), 2);
        RegClos("%SLOT-DEF-ATTRS", a => Runtime.SetSlotDefAttrs(a[0], a[1], a[2], a[3], a[4]), 5);
        RegClos("%SLOT-DEF-DOC", a => Runtime.SetSlotDefDocumentation(a[0], a[1]), 2);
        RegClos("%SLOT-DEF-DOCUMENTATION", a => Runtime.SlotDefinitionDocumentation(a[0]), 1);
        // Used by DOCUMENTATION's default method, which is read by the host Lisp
        // during the cross compile and so cannot name the MOP package: anything that
        // is not a slot definition answers NIL and falls through to the table.
        RegClos("%SLOT-DEF-DOCUMENTATION-OR-NIL",
            a => a[0] is SlotDefinition sd ? sd.Documentation : Nil.Instance, 1);
        RegClos("%SET-CLASS-DEFAULT-INITARGS", a => Runtime.SetClassDefaultInitargs(a[0], a[1]), 2);
        RegClos("%FIND-CLASS-OR-NIL", a => Runtime.FindClassOrNil(a[0]), 1);
        RegClos("%SPECIALIZER-CLASS", a => Runtime.SpecializerClass(a[0]), 1);
        RegClos("%SET-SLOT-VALUE", a => Runtime.SetSlotValue(a[0], a[1], a[2]), 3);
        RegClos("%ALLOCATE-INSTANCE", a => Runtime.MakeInstanceRaw(a[0]), 1);
        RegClos("%SLOT-EXISTS-P", a => Runtime.SlotExists(a[0], a[1]), 2);
        RegClos("%CHANGE-CLASS", Runtime.ChangeClass);
        RegClos("%REGISTER-ACCESSOR-METHOD", a => Runtime.RegisterAccessorMethod(a[0], a[1], a[2]), 3);
        RegClos("%MAKE-GF", a => Runtime.MakeGF(a[0], a[1]), 2);
        RegClos("%REGISTER-GF", a => Runtime.RegisterGF(a[0], a[1]), 2);
        RegClos("%FIND-GF", a => Runtime.FindGF(a[0]), 1);
        RegClos("%REGISTER-GF-IF-ABSENT", a => Runtime.RegisterGFIfAbsent(a[0], a[1]), 2);
        RegClos("%SET-GF-LAMBDA-LIST-INFO", Runtime.SetGFLambdaListInfo);
        RegClos("%SET-METHOD-LAMBDA-LIST-INFO", Runtime.SetMethodLambdaListInfo);
        RegClos("%MAKE-METHOD", a => Runtime.MakeMethod(a[0], a[1], a[2]), 3);
        RegClos("%ADD-METHOD", a => Runtime.AddMethod(a[0], a[1]), 2);
        RegClos("%NOTE-METHOD-CLASS", a => Runtime.NoteMethodClass(a[0], a[1]), 2);
        RegClos("%NOTE-METHOD-COMBINATION", a => Runtime.NoteMethodCombination(a[0], a[1], a[2]), 3);
        RegClos("%MAKE-METHOD-LAMBDA-FOR", a => Runtime.MakeMethodLambdaFor(a[0], a[1]), 2);
        RegClos("%CURRENT-NEXT-METHODS", a => Runtime.CurrentNextMethods(), 0);
        RegClos("%INTERN-EQL-SPECIALIZER", a => Runtime.InternEqlSpecializer(a[0]), 1);
        RegClos("%GF-METHODS", a => Runtime.GetGFMethods(a[0]), 1);
        RegClos("%METHOD-SPECIALIZERS", a => Runtime.MethodSpecializers(a[0]), 1);
        RegClos("%METHOD-QUALIFIERS", a => Runtime.MethodQualifiers(a[0]), 1);
        RegClos("%METHOD-FUNCTION", a => Runtime.MethodFunction(a[0]), 1);
        RegClos("%CLEAR-DEFGENERIC-INLINE-METHODS", a => Runtime.ClearDefgenericInlineMethods(a[0]), 1);
        RegClos("%MARK-DEFGENERIC-INLINE-METHOD", a => Runtime.MarkDefgenericInlineMethod(a[0], a[1]), 2);
        RegClos("%SET-GF-DECLARATIONS", a => Runtime.SetGFDeclarations(a[0], a[1]), 2);
        RegClos("%SET-METHOD-COMBINATION", a => Runtime.SetMethodCombination(a[0], a[1]), 2);
        RegClos("%SET-METHOD-COMBINATION-ORDER", a => Runtime.SetMethodCombinationOrder(a[0], a[1]), 2);
        RegClos("%SET-METHOD-COMBINATION-ARGS", a => Runtime.SetMethodCombinationArgs(a[0], a[1]), 2);
        RegClos("%CAPTURED-CALL-NEXT-METHOD", a => Runtime.CapturedCnm(), 0);
        RegClos("%CAPTURED-NEXT-METHOD-P", a => Runtime.CapturedNmp(), 0);
        // defstruct intrinsics (same rationale).
        RegClos("%MAKE-STRUCT", a => Runtime.MakeStruct(a[0], a.SubArray(1)));
        RegClos("%STRUCT-REF", a => Runtime.StructRef(a.Length > 2 ? Runtime.StructOf(a[0], a[2]) : a[0], a[1]));
        RegClos("%STRUCT-SET", a => Runtime.StructSet(a.Length > 3 ? Runtime.StructOf(a[0], a[3]) : a[0], a[1], a[2]));
        RegClos("%CHECK-SLOT-TYPE",
                a => Runtime.CheckSlotType(a[0], a[1], a[2], a[3]), 4);
        // Two arguments as well as three: a FASL compiled before layouts carried
        // a version calls this with the pair it knew about, and it still means
        // "version 0" -- which is what its call sites were compiled against.
        RegClos("%STRUCT-REGISTER-LAYOUT",
                a => Runtime.StructRegisterLayout(a[0], a[1],
                                                  a.Length > 2 ? a[2] : Fixnum.Make(0)));
        RegClos("%STRUCT-TYPEP", a => Runtime.StructTypep(a[0], a[1]), 2);
        RegClos("%COPY-STRUCT", a => Runtime.CopyStruct(a[0]), 1);
        RegClos("%PRINT-OBJECT-DEPTH", a => Runtime.PrintObjectDepth(), 0);
        // array/string element-set intrinsics (setf aref / setf char).
        RegClos("%AREF-SET", a => a.Length switch
        {
            3 => Runtime.ArefSet(a[0], a[1], a[2]),
            4 => Runtime.ArefSet2D(a[0], a[1], a[2], a[3]),
            5 => Runtime.ArefSet3D(a[0], a[1], a[2], a[3], a[4]),
            _ => Runtime.ArefSetMulti(a),
        });
        RegClos("%CHAR-SET", a => Runtime.CharSet(a[0], a[1], a[2]), 3);

        // MAKE-INSTANCE as a proper GF
        {
            var miSym = Startup.Sym("MAKE-INSTANCE");
            var miGF = (GenericFunction)Runtime.MakeGF(miSym, new Fixnum(-1));
            miGF.RequiredCount = 1;
            miGF.HasRest = true;
            miGF.HasKey = true;
            miGF.HasAllowOtherKeys = true;
            miGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(miSym, miGF);
            miSym.Function = miGF;
            Emitter.CilAssembler.RegisterFunction("MAKE-INSTANCE", miGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var miSpecializers = new Cons(tCls, Nil.Instance);
            var miQualifiers = Nil.Instance;
            var miDefaultMethod = Runtime.MakeMethod(miSpecializers, miQualifiers,
                new LispFunction(args => {
                    if (args.Length == 0)
                        throw new LispErrorException(new LispProgramError("MAKE-INSTANCE: requires at least 1 argument"));
                    return Runtime.MakeInstanceWithInitargs(args[0],
                        args.Length > 1 ? args.SubArray(1) : Array.Empty<LispObject>());
                }));
            ((LispMethod)miDefaultMethod).RequiredCount = 1;
            ((LispMethod)miDefaultMethod).HasRest = true;
            ((LispMethod)miDefaultMethod).HasKey = true;
            ((LispMethod)miDefaultMethod).HasAllowOtherKeys = true;
            Runtime.AddMethod(miGF, miDefaultMethod);
        }

        // FIND-CLASS
        Emitter.CilAssembler.RegisterFunction("FIND-CLASS", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("FIND-CLASS: too few arguments"));
            if (args.Length > 3) throw new LispErrorException(new LispProgramError($"FIND-CLASS: too many arguments: {args.Length} (expected 1-3)"));
            bool errorp = args.Length < 2 || Runtime.IsTruthy(args[1]);
            // Given an environment argument, a class that a DEFCLASS earlier in
            // the file being compiled names (see CompilerDefclass) is found too.
            if (args.Length == 3 && Runtime.FindClassOrNil(args[0]) is Nil
                && Runtime.FindCompileTimeClass(args[0]) is LispClass pending)
                return pending;
            return errorp ? Runtime.FindClass(args[0]) : Runtime.FindClassOrNil(args[0]);
        }, "FIND-CLASS"));
        Emitter.CilAssembler.RegisterFunction("%COMPILER-DEFCLASS",
            new LispFunction(args => Runtime.CompilerDefclass(args[0]), "%COMPILER-DEFCLASS", 1));
        var findOrForward = new LispFunction(args => Runtime.FindOrForwardClass(args[0]),
                                             "%FIND-OR-FORWARD-CLASS", 1);
        // Typed 1-arg entry: DEFCLASS resolves every superclass through this.
        findOrForward.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.FindOrForwardClass);
        Emitter.CilAssembler.RegisterFunction("%FIND-OR-FORWARD-CLASS", findOrForward);
        Emitter.CilAssembler.RegisterFunction("(SETF FIND-CLASS)", new LispFunction(args => {
            if (args.Length < 2) throw new Exception("(SETF FIND-CLASS): too few arguments");
            var newVal = args[0];
            // A class name is a symbol (CLHS FIND-CLASS). ToClassSymbol would turn
            // anything else into one by its printed name, so a class object or a
            // string silently registered under a made-up symbol.
            if (args[1] is not Symbol && args[1] is not Nil)
                throw new LispErrorException(new LispTypeError(
                    $"(SETF FIND-CLASS): {args[1]} is not a legal class name",
                    args[1], Startup.Sym("SYMBOL")));
            // Key by the ORIGINAL package-qualified symbol, never the bare-name-normalized
            // one: otherwise (setf (find-class 'pa::seq) ...) and (setf (find-class 'pb::seq) ...)
            // both land on DOTCL-INTERNAL::SEQ and the second clobbers the first. fset's
            // post.lisp aliases its classes into the FSET2 package exactly this way.
            var sym = (args[1] is Symbol orig && orig.HomePackage != null) ? orig : ToClassSymbol(args[1]);
            if (newVal is Nil) {
                _classRegistry.TryRemove(sym, out _);
            } else if (newVal is LispClass lc) {
                _classRegistry[sym] = lc;
            } else {
                throw new LispErrorException(new LispTypeError("(SETF FIND-CLASS): not a class", newVal));
            }
            return newVal;
        }, "(SETF FIND-CLASS)"));
        Startup.RegisterUnary("CLASS-OF", Runtime.ClassOf);

        // %REGISTER-STRUCT-CLASS
        Emitter.CilAssembler.RegisterFunction("%REGISTER-STRUCT-CLASS", new LispFunction(args => {
            var name = (Symbol)args[0];
            LispClass? parentCls = null;
            var parentSym = args.Length > 1 && args[1] is Symbol ps && ps.Name != "NIL" ? ps : null;
            if (parentSym != null) {
                parentCls = Runtime.FindClassOrNil(parentSym) as LispClass;
            }
            if (parentCls == null) {
                parentCls = Runtime.FindClassOrNil(Startup.Sym("STRUCTURE-OBJECT")) as LispClass
                    ?? throw new Exception("%REGISTER-STRUCT-CLASS: STRUCTURE-OBJECT class not found");
            }
            var slotNames = new Symbol[args.Length - 2];
            var directSlots = new SlotDefinition[args.Length - 2];
            for (int i = 2; i < args.Length; i++)
            {
                Symbol slotSym;
                if (args[i] is Symbol sym3)
                    slotSym = sym3;
                else if (args[i] is T)
                    slotSym = Startup.Sym("T");
                else if (args[i] is Nil)
                    slotSym = Startup.Sym("NIL");
                else
                    slotSym = Startup.Sym(args[i].ToString() ?? "");
                slotNames[i - 2] = slotSym;
                directSlots[i - 2] = new SlotDefinition(slotSym);
            }
            var cls = new LispClass(name, directSlots, new[] { parentCls });
            cls.IsStructureClass = true;
            cls.StructSlotNames = slotNames;
            cls.FinalizeClass();
            Runtime.RegisterClass(cls);
            name.StructParent = parentSym ?? Startup.Sym("STRUCTURE-OBJECT");
            return name;
        }));

        Emitter.CilAssembler.RegisterFunction("%REGISTER-STRUCT-READER", new LispFunction(args => {
            var name = (Symbol)args[0];
            if (Runtime.FindClassOrNil(name) is LispClass cls)
                cls.StructKeywordConstructor = args.Length > 1 && args[1] is Symbol ctor ? ctor : null;
            return T.Instance;
        }));

        // FIND-METHOD as a proper GF
        {
            var fmSym = Startup.Sym("FIND-METHOD");
            var fmGF = (GenericFunction)Runtime.MakeGF(fmSym, new Fixnum(-1));
            fmGF.RequiredCount = 3;
            fmGF.OptionalCount = 1;
            fmGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(fmSym, fmGF);
            fmSym.Function = fmGF;
            Emitter.CilAssembler.RegisterFunction("FIND-METHOD", fmGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var fmSpecializers = new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance)));
            var fmDefaultMethod = Runtime.MakeMethod(fmSpecializers, Nil.Instance,
                new LispFunction(args => {
                if (args.Length < 3 || args.Length > 4)
                    throw new LispErrorException(new LispProgramError(
                        $"FIND-METHOD: expected 3 or 4 arguments, got {args.Length}"));
                bool errorp = args.Length < 4 || args[3] != Nil.Instance;
                if (args[0] is GenericFunction gf)
                {
                    int specCount = 0;
                    var specCheck = args[2];
                    while (specCheck is Cons sc2) { specCount++; specCheck = sc2.Cdr; }
                    if (gf.LambdaListInfoSet && specCount != gf.RequiredCount)
                        throw new LispErrorException(new LispError(
                            $"FIND-METHOD: specializer list length {specCount} does not match " +
                            $"the number of required parameters {gf.RequiredCount} of {gf.Name.Name}"));

                    var qualList = new System.Collections.Generic.List<LispObject>();
                    var ql = args[1];
                    while (ql is Cons qc) { qualList.Add(qc.Car); ql = qc.Cdr; }

                    foreach (var method in gf.Methods)
                    {
                        if (method.Qualifiers.Length != qualList.Count) continue;
                        bool qualMatch = true;
                        for (int i = 0; i < qualList.Count; i++)
                        {
                            if (!ReferenceEquals(method.Qualifiers[i], qualList[i])
                                && !(method.Qualifiers[i] is not Symbol && IsTrueEql(method.Qualifiers[i], qualList[i])))
                            { qualMatch = false; break; }
                        }
                        if (!qualMatch) continue;

                        bool match = true;
                        var specList = args[2];
                        for (int i = 0; i < method.Specializers.Length; i++)
                        {
                            if (specList is not Cons sc) { match = false; break; }
                            if (!ReferenceEquals(method.Specializers[i], sc.Car))
                            {
                                // EQL specializers compare by the object they specialize
                                // on, so the metaobject and the list (EQL object) name
                                // the same method whichever side each is written as.
                                var mEql = EqlSpecializerValue(method.Specializers[i]);
                                var sEql = EqlSpecializerValue(sc.Car);
                                if (mEql != null || sEql != null)
                                {
                                    if (mEql == null || sEql == null || !IsTrueEql(mEql, sEql))
                                    { match = false; break; }
                                    specList = sc.Cdr;
                                    continue;
                                }
                                string mName = method.Specializers[i] is LispClass mc ? mc.Name.Name
                                    : method.Specializers[i] is Symbol ms ? ms.Name
                                    : method.Specializers[i].ToString();
                                string sName = sc.Car is LispClass lc2 ? lc2.Name.Name
                                    : sc.Car is Symbol ss ? ss.Name
                                    : sc.Car.ToString();
                                if (!string.Equals(mName, sName, StringComparison.OrdinalIgnoreCase))
                                { match = false; break; }
                            }
                            specList = sc.Cdr;
                        }
                        if (match) return method;
                    }
                }
                if (!errorp) return Nil.Instance;
                throw new LispErrorException(new LispError("FIND-METHOD: method not found"));
            }));
            ((LispMethod)fmDefaultMethod).RequiredCount = 3;
            ((LispMethod)fmDefaultMethod).OptionalCount = 1;
            Runtime.AddMethod(fmGF, fmDefaultMethod);
        }

        // shared-initialize as GF with default method on T
        var tClass = Runtime.FindClass(Startup.Sym("T"));
        {
            var siName = Startup.Sym("SHARED-INITIALIZE");
            var gf = Runtime.MakeGF(siName, new Fixnum(2));
            Runtime.RegisterGF(siName, gf);
            siName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("SHARED-INITIALIZE", (LispFunction)gf);

            var specializers = new Cons(tClass, Nil.Instance);
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(Runtime.SharedInitialize));
            Runtime.AddMethod(gf, defaultMethod);
        }

        // initialize-instance as GF
        {
            var iiName = Startup.Sym("INITIALIZE-INSTANCE");
            var gf = Runtime.MakeGF(iiName, new Fixnum(1));
            Runtime.RegisterGF(iiName, gf);
            iiName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("INITIALIZE-INSTANCE", (LispFunction)gf);

            var specializers = new Cons(tClass, Nil.Instance);
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(Runtime.InitializeInstance));
            Runtime.AddMethod(gf, defaultMethod);

            // initialize-instance primary for GENERIC-FUNCTION. dotcl's own generic
            // functions have no Lisp slots and skip shared-initialize; one made from a
            // user-defined generic function class does have them (they live in
            // ExtraSlots), so for those the standard protocol runs and initargs and
            // initforms are applied.
            {
                var gfPrimCls = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var gfPrimM = Runtime.MakeMethod(new Cons(gfPrimCls, Nil.Instance), Nil.Instance,
                    new LispFunction(args =>
                    {
                        if (args[0] is GenericFunction userGf && userGf.StoredClass != null
                            && Startup.Sym("SHARED-INITIALIZE").Function is LispFunction siFn)
                        {
                            var siArgs = new LispObject[args.Length + 1];
                            siArgs[0] = args[0];
                            siArgs[1] = T.Instance;
                            Array.Copy(args, 1, siArgs, 2, args.Length - 1);
                            siFn.Invoke(siArgs);
                        }
                        return args[0];
                    }));
                ((LispMethod)gfPrimM).RequiredCount = 1;
                ((LispMethod)gfPrimM).HasRest = true;
                ((LispMethod)gfPrimM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, gfPrimM);
            }

            // initialize-instance primary for METHOD. A STANDARD-METHOD has no Lisp
            // slots and skips shared-initialize; one made from a user-defined method
            // class does have them (they live in ExtraSlots), so for those the standard
            // protocol runs and initargs and initforms are applied. Mirrors the
            // GENERIC-FUNCTION primary above.
            {
                var mPrimCls = Runtime.FindClass(Startup.Sym("METHOD"));
                var mPrimM = Runtime.MakeMethod(new Cons(mPrimCls, Nil.Instance), Nil.Instance,
                    new LispFunction(args =>
                    {
                        if (args[0] is LispMethod userMethod && userMethod.MetaClass != null
                            && Startup.Sym("SHARED-INITIALIZE").Function is LispFunction siFn)
                        {
                            var siArgs = new LispObject[args.Length + 1];
                            siArgs[0] = args[0];
                            siArgs[1] = T.Instance;
                            Array.Copy(args, 1, siArgs, 2, args.Length - 1);
                            siFn.Invoke(siArgs);
                        }
                        return args[0];
                    }));
                ((LispMethod)mPrimM).RequiredCount = 1;
                ((LispMethod)mPrimM).HasRest = true;
                ((LispMethod)mPrimM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, mPrimM);
            }

            // initialize-instance :after for GenericFunction: apply initargs
            {
                var afterQuals = new Cons(Startup.Keyword("AFTER"), Nil.Instance);
                var gfCls2 = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var gfAfterM = Runtime.MakeMethod(new Cons(gfCls2, Nil.Instance), afterQuals,
                    new LispFunction(args => {
                        if (args[0] is not GenericFunction ugf) return args[0];
                        ApplyGenericFunctionInitargs(ugf, args, renaming: false);
                        // A generic function with no methods yet never reaches
                        // ADD-METHOD, so this is the only point where a class of
                        // generic function that computes its own dispatch gets to
                        // do so before the first call.
                        NotifyDiscriminatingFunction(ugf);
                        return ugf;
                    }));
                ((LispMethod)gfAfterM).RequiredCount = 1;
                ((LispMethod)gfAfterM).HasRest = true;
                ((LispMethod)gfAfterM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, gfAfterM);

                // initialize-instance :after for standard-method: set qualifiers/specializers/function
                var smCls2 = Runtime.FindClass(Startup.Sym("STANDARD-METHOD"));
                var smAfterM = Runtime.MakeMethod(new Cons(smCls2, Nil.Instance), afterQuals,
                    new LispFunction(args => {
                        if (args[0] is not LispMethod m) return args[0];
                        bool qualSeen = false, specSeen = false, fnSeen = false, llSeen = false;
                        for (int i = 1; i + 1 < args.Length; i += 2)
                        {
                            if (args[i] is not Symbol ks) continue;
                            switch (ks.Name)
                            {
                                case "QUALIFIERS":
                                    if (!qualSeen) { m.Qualifiers = CollectList(args[i + 1]); qualSeen = true; }
                                    break;
                                case "SPECIALIZERS":
                                    if (!specSeen) { m.Specializers = CollectList(args[i + 1]); specSeen = true; }
                                    break;
                                case "FUNCTION":
                                    // AMOP: the function passed here takes (args
                                    // next-methods). Dispatch calls method functions
                                    // with the arguments spread, so what is stored is
                                    // an adapter; METHOD-FUNCTION still answers the
                                    // object that was passed in.
                                    if (!fnSeen)
                                    {
                                        fnSeen = true;
                                        if (args[i + 1] is LispFunction mf)
                                        {
                                            m.ProcessedParameterFunction = mf;
                                            m.Function = new LispFunction(callArgs =>
                                                mf.Invoke(new LispObject[] {
                                                    Runtime.List(callArgs),
                                                    Runtime.CurrentNextMethods() }),
                                                "method function adapter", -1);
                                        }
                                    }
                                    break;
                                case "LAMBDA-LIST":
                                    if (!llSeen)
                                    {
                                        ParseLambdaListIntoMethod(m, args[i + 1]);
                                        // METHOD-LAMBDA-LIST hands back this list, not a rebuilt shape.
                                        m.StoredLambdaList = args[i + 1];
                                        llSeen = true;
                                    }
                                    break;
                            }
                        }
                        return m;
                    }));
                ((LispMethod)smAfterM).RequiredCount = 1;
                ((LispMethod)smAfterM).HasRest = true;
                ((LispMethod)smAfterM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, smAfterM);
            }
        }

        // reinitialize-instance as GF
        {
            var riName = Startup.Sym("REINITIALIZE-INSTANCE");
            var gf = Runtime.MakeGF(riName, new Fixnum(1));
            Runtime.RegisterGF(riName, gf);
            riName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("REINITIALIZE-INSTANCE", (LispFunction)gf);

            var specializers = new Cons(tClass, Nil.Instance);
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(Runtime.ReinitializeInstance));
            Runtime.AddMethod(gf, defaultMethod);

            // reinitialize-instance :after for CLASS: applies :NAME, which is how
            // (SETF CLASS-NAME) changes a name. NIL clears the proper name rather than
            // naming the class NIL (CLHS ensure-class: redefinition only happens under
            // a class's proper name).
            {
                var riClsQuals = new Cons(Startup.Keyword("AFTER"), Nil.Instance);
                var riClsCls = Runtime.FindClass(Startup.Sym("CLASS"));
                var riClsM = Runtime.MakeMethod(new Cons(riClsCls, Nil.Instance), riClsQuals,
                    new LispFunction(args =>
                    {
                        if (args[0] is not LispClass target) return args[0];
                        bool nameSeen = false;
                        for (int i = 1; i + 1 < args.Length; i += 2)
                        {
                            if (nameSeen || args[i] is not Symbol k || k.Name != "NAME") continue;
                            nameSeen = true;
                            if (args[i + 1] is Symbol newSym) target.Name = newSym;
                            else if (args[i + 1] is Nil) target.NameCleared = true;
                            else target.Name = Startup.Sym(args[i + 1].ToString());
                        }
                        // AMOP: reinitializing a class that was already finalized
                        // finalizes it again, through the generic function so a
                        // metaclass method is heard.
                        if (ForwardReferencedAncestor(target) == null
                            && Startup.Sym("FINALIZE-INHERITANCE").Function is LispFunction fi)
                            fi.Invoke(new LispObject[] { target });
                        NotifyDependents(target, SkipInstanceArg(args));
                        return target;
                    }));
                ((LispMethod)riClsM).RequiredCount = 1;
                ((LispMethod)riClsM).HasRest = true;
                ((LispMethod)riClsM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, riClsM);
            }

            // reinitialize-instance :after for GENERIC-FUNCTION
            // initialization applies, with :NAME now allowed to replace an existing
            // name: AMOP defines (SETF GENERIC-FUNCTION-NAME) as reinitialization.
            // The discriminating function is recomputed afterwards, since the lambda
            // list it was built for may have changed.
            {
                var riAfterQuals = new Cons(Startup.Keyword("AFTER"), Nil.Instance);
                var riGfCls = Runtime.FindClass(Startup.Sym("GENERIC-FUNCTION"));
                var riGfM = Runtime.MakeMethod(new Cons(riGfCls, Nil.Instance), riAfterQuals,
                    new LispFunction(args =>
                    {
                        if (args[0] is not GenericFunction rgf) return args[0];
                        ApplyGenericFunctionInitargs(rgf, args, renaming: true);
                        NotifyDiscriminatingFunction(rgf);
                        NotifyDependents(rgf, SkipInstanceArg(args));
                        return rgf;
                    }));
                ((LispMethod)riGfM).RequiredCount = 1;
                ((LispMethod)riGfM).HasRest = true;
                ((LispMethod)riGfM).HasAllowOtherKeys = true;
                Runtime.AddMethod(gf, riGfM);
            }
        }

        // describe-object as GF with default method on T
        {
            var doName = Startup.Sym("DESCRIBE-OBJECT");
            var gf = Runtime.MakeGF(doName, new Fixnum(2));
            Runtime.RegisterGF(doName, gf);
            doName.Function = (LispFunction)gf;
            Emitter.CilAssembler.RegisterFunction("DESCRIBE-OBJECT", (LispFunction)gf);

            var specializers = new Cons(tClass, new Cons(tClass, Nil.Instance));
            var qualifiers = Nil.Instance;
            var defaultMethod = Runtime.MakeMethod(specializers, qualifiers,
                new LispFunction(args => {
                    if (args.Length < 2) throw new LispErrorException(new LispProgramError("DESCRIBE-OBJECT: requires 2 arguments"));
                    var obj = args[0];
                    var stream = args[1];
                    var writer = Runtime.GetOutputWriter(stream);
                    var typeObj = Runtime.TypeOf(obj);
                    writer.Write(Runtime.FormatObject(obj, true));
                    writer.WriteLine();
                    writer.Write("  [");
                    writer.Write(Runtime.FormatObject(typeObj, true));
                    writer.WriteLine("]");
                    writer.Flush();
                    return Nil.Instance;
                }, "DESCRIBE-OBJECT-DEFAULT", 2));
            Runtime.AddMethod(gf, defaultMethod);
        }

        // CHANGE-CLASS as a proper GF
        {
            var ccSym = Startup.Sym("CHANGE-CLASS");
            var ccGF = (GenericFunction)Runtime.MakeGF(ccSym, new Fixnum(-1));
            ccGF.RequiredCount = 2;
            ccGF.HasRest = true;
            ccGF.HasKey = true;
            ccGF.HasAllowOtherKeys = true;
            ccGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(ccSym, ccGF);
            ccSym.Function = ccGF;
            Emitter.CilAssembler.RegisterFunction("CHANGE-CLASS", ccGF);

            var tCls2 = Runtime.FindClass(Startup.Sym("T"));
            var ccSpecializers = new Cons(tCls2, new Cons(tCls2, Nil.Instance));
            var ccQualifiers = Nil.Instance;
            var ccDefaultMethod = Runtime.MakeMethod(ccSpecializers, ccQualifiers,
                new LispFunction(Runtime.ChangeClass));
            ((LispMethod)ccDefaultMethod).RequiredCount = 2;
            ((LispMethod)ccDefaultMethod).HasRest = true;
            ((LispMethod)ccDefaultMethod).HasKey = true;
            ((LispMethod)ccDefaultMethod).HasAllowOtherKeys = true;
            Runtime.AddMethod(ccGF, ccDefaultMethod);
        }

        // MAKE-LOAD-FORM: standard GF
        {
            var mlfSym = Startup.Sym("MAKE-LOAD-FORM");
            GenericFunction mlfGf = null!;
            mlfGf = new GenericFunction(mlfSym, -1, args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM: wrong number of arguments: 0 (expected 1-2)"));
                if (args.Length > 2)
                    throw new LispErrorException(new LispProgramError($"MAKE-LOAD-FORM: wrong number of arguments: {args.Length} (expected 1-2)"));
                return Runtime.DispatchGFOrDefault(mlfGf, args, mlfArgs => {
                    var obj = mlfArgs[0];
                    string className = "<unknown>";
                    if (obj is LispInstance inst2) className = inst2.Class.Name.Name;
                    else if (obj is LispInstanceCondition lic2) className = lic2.Instance.Class.Name.Name;
                    else if (LispStruct.TryAs(obj, out var ls2)) className = ls2.TypeName.Name;
                    throw new LispErrorException(new LispError(
                        $"No applicable method for MAKE-LOAD-FORM on object of class {className}"));
                });
            });
            mlfGf.RequiredCount = 1;
            mlfGf.OptionalCount = 1;
            mlfGf.HasRest = false;
            mlfGf.HasKey = false;
            Runtime.RegisterGF(mlfSym, mlfGf);
            mlfSym.Function = mlfGf;
            Emitter.CilAssembler.RegisterFunction("MAKE-LOAD-FORM", mlfGf);

            var mlfSpecializers = new Cons(tClass, Nil.Instance);
            var mlfQualifiers = Nil.Instance;
            var mlfDefaultMethod = Runtime.MakeMethod(mlfSpecializers, mlfQualifiers,
                new LispFunction(args => {
                    var obj = args[0];
                    string className = "<unknown>";
                    if (obj is LispInstance inst) className = inst.Class.Name.Name;
                    else if (obj is LispInstanceCondition lic) className = lic.Instance.Class.Name.Name;
                    else if (LispStruct.TryAs(obj, out var ls)) className = ls.TypeName.Name;
                    throw new LispErrorException(new LispError(
                        $"No applicable method for MAKE-LOAD-FORM on object of class {className}"));
                }, "MAKE-LOAD-FORM-DEFAULT", -1));
            if (mlfDefaultMethod is LispMethod mlfMeth)
            {
                mlfMeth.RequiredCount = 1;
                mlfMeth.OptionalCount = 1;
            }
            Runtime.AddMethod(mlfGf, mlfDefaultMethod);
        }

        // FUNCTION-KEYWORDS as a proper GF
        {
            var fkSym = Startup.Sym("FUNCTION-KEYWORDS");
            var fkGF = (GenericFunction)Runtime.MakeGF(fkSym, new Fixnum(1));
            fkGF.RequiredCount = 1;
            fkGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(fkSym, fkGF);
            fkSym.Function = fkGF;
            Emitter.CilAssembler.RegisterFunction("FUNCTION-KEYWORDS", fkGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var fkSpecializers = new Cons(tCls, Nil.Instance);
            var fkDefaultMethod = Runtime.MakeMethod(fkSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args[0] is LispMethod m)
                    {
                        LispObject kwList = Nil.Instance;
                        for (int i = m.KeywordNames.Count - 1; i >= 0; i--)
                        {
                            var kwSym = Startup.Keyword(m.KeywordNames[i]);
                            kwList = new Cons(kwSym, kwList);
                        }
                        return MultipleValues.Values2(kwList, m.HasAllowOtherKeys ? (LispObject)T.Instance : Nil.Instance);
                    }
                    return MultipleValues.Values2(Nil.Instance, Nil.Instance);
                }));
            ((LispMethod)fkDefaultMethod).RequiredCount = 1;
            Runtime.AddMethod(fkGF, fkDefaultMethod);
        }
        // PRINT-OBJECT as a proper GF with default method on T
        {
            var poSym = Startup.Sym("PRINT-OBJECT");
            var poGF = (GenericFunction)Runtime.MakeGF(poSym, new Fixnum(2));
            poGF.RequiredCount = 2;
            poGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(poSym, poGF);
            poSym.Function = poGF;
            Emitter.CilAssembler.RegisterFunction("PRINT-OBJECT", poGF);

            var poCls = Runtime.FindClass(Startup.Sym("T"));
            var poSpecializers = new Cons(poCls, new Cons(poCls, Nil.Instance));
            var poDefaultMethod = Runtime.MakeMethod(poSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args.Length < 2) throw new LispErrorException(new LispProgramError("PRINT-OBJECT: requires 2 arguments"));
                    var writer = Runtime.GetOutputWriter(args[1]);
                    var potext = Runtime.FormatTopBody(args[0], true);
                    writer.Write(potext);
                    writer.Flush();
                    Runtime.NoteWritten(args[1], potext);
                    return args[0];
                }, "PRINT-OBJECT-DEFAULT", 2));
            ((LispMethod)poDefaultMethod).RequiredCount = 2;
            Runtime.AddMethod(poGF, poDefaultMethod);
        }
        // UPDATE-INSTANCE-FOR-REDEFINED-CLASS as a proper GF
        {
            var uirSym = Startup.Sym("UPDATE-INSTANCE-FOR-REDEFINED-CLASS");
            var uirGF = (GenericFunction)Runtime.MakeGF(uirSym, new Fixnum(-1));
            uirGF.RequiredCount = 4;
            uirGF.HasRest = true;
            uirGF.HasKey = true;
            uirGF.HasAllowOtherKeys = true;
            uirGF.LambdaListInfoSet = true;
            Runtime.RegisterGF(uirSym, uirGF);
            uirSym.Function = uirGF;
            Emitter.CilAssembler.RegisterFunction("UPDATE-INSTANCE-FOR-REDEFINED-CLASS", uirGF);

            var tCls = Runtime.FindClass(Startup.Sym("T"));
            var uirSpecializers = new Cons(tCls, new Cons(tCls, new Cons(tCls, new Cons(tCls, Nil.Instance))));
            // The standard method (CLHS UPDATE-INSTANCE-FOR-REDEFINED-CLASS): initialize
            // the added local slots through SHARED-INITIALIZE, passing the initargs on.
            var uirDefaultMethod = Runtime.MakeMethod(uirSpecializers, Nil.Instance,
                new LispFunction(args => {
                    if (args.Length < 4) return Nil.Instance;
                    if (Startup.Sym("SHARED-INITIALIZE").Function is LispFunction siFn)
                    {
                        var siArgs = new LispObject[2 + (args.Length - 4)];
                        siArgs[0] = args[0];
                        siArgs[1] = args[1];
                        Array.Copy(args, 4, siArgs, 2, args.Length - 4);
                        siFn.Invoke(siArgs);
                    }
                    return args[0];
                }));
            ((LispMethod)uirDefaultMethod).RequiredCount = 4;
            ((LispMethod)uirDefaultMethod).HasRest = true;
            ((LispMethod)uirDefaultMethod).HasKey = true;
            ((LispMethod)uirDefaultMethod).HasAllowOtherKeys = true;
            Runtime.AddMethod(uirGF, uirDefaultMethod);
        }

        // MOP accessor functions for slot definitions (AMOP)
        // SLOT-DEFINITION-NAME
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-NAME", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-NAME: wrong arg count"));
            return SlotDefinitionName(args[0]);
        }, "SLOT-DEFINITION-NAME", 1));

        // SLOT-DEFINITION-TYPE
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-TYPE", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-TYPE: wrong arg count"));
            return SlotDefinitionType(args[0]);
        }, "SLOT-DEFINITION-TYPE", 1));

        // SLOT-DEFINITION-INITARGS
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITARGS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITARGS: wrong arg count"));
            return SlotDefinitionInitargs(args[0]);
        }, "SLOT-DEFINITION-INITARGS", 1));

        // SLOT-DEFINITION-INITFORM / SLOT-DEFINITION-INITFUNCTION
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITFORM", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITFORM: wrong arg count"));
            return SlotDefinitionInitform(args[0]);
        }, "SLOT-DEFINITION-INITFORM", 1));

        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-INITFUNCTION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-INITFUNCTION: wrong arg count"));
            return SlotDefinitionInitfunction(args[0]);
        }, "SLOT-DEFINITION-INITFUNCTION", 1));

        // SLOT-DEFINITION-ALLOCATION -> :instance or :class
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-ALLOCATION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-ALLOCATION: wrong arg count"));
            return SlotDefinitionAllocation(args[0]);
        }, "SLOT-DEFINITION-ALLOCATION", 1));

        // SLOT-DEFINITION-READERS / SLOT-DEFINITION-WRITERS: the :reader/:accessor
        // and :writer/(setf accessor) names DEFCLASS parsed. Both used to be NIL.
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-READERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-READERS: wrong arg count"));
            return SlotDefinitionReaders(args[0]);
        }, "SLOT-DEFINITION-READERS", 1));
        Emitter.CilAssembler.RegisterFunction("SLOT-DEFINITION-WRITERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("SLOT-DEFINITION-WRITERS: wrong arg count"));
            return SlotDefinitionWriters(args[0]);
        }, "SLOT-DEFINITION-WRITERS", 1));

        // CLASS-DIRECT-SLOTS: list of SlotDefinition objects for the class's own slots
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SLOTS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SLOTS: wrong arg count"));
            return ClassDirectSlots(args[0]);
        }, "CLASS-DIRECT-SLOTS", 1));

        // CLASS-SLOTS: list of effective SlotDefinition objects (all inherited slots)
        Emitter.CilAssembler.RegisterFunction("CLASS-SLOTS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-SLOTS: wrong arg count"));
            return ClassSlots(args[0]);
        }, "CLASS-SLOTS", 1));

        // CLASS-DIRECT-SUPERCLASSES
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SUPERCLASSES", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SUPERCLASSES: wrong arg count"));
            return ClassDirectSuperclasses(args[0]);
        }, "CLASS-DIRECT-SUPERCLASSES", 1));

        // CLASS-DIRECT-SUBCLASSES: the same implementation DOTCL-MOP exposes.
        // This used to be a stub returning NIL for every class, so the answer
        // depended on which symbol the caller reached: dotcl-mop:... was right and
        // the CL one silently wrong. GENERIC-FUNCTION-LAMBDA-LIST had the same
        // split and returned NIL only on Linux (see below).
        Emitter.CilAssembler.RegisterFunction("CLASS-DIRECT-SUBCLASSES", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-DIRECT-SUBCLASSES: wrong arg count"));
            return ClassDirectSubclasses(args[0]);
        }, "CLASS-DIRECT-SUBCLASSES", 1));

        // CLASS-PRECEDENCE-LIST
        Emitter.CilAssembler.RegisterFunction("CLASS-PRECEDENCE-LIST", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-PRECEDENCE-LIST: wrong arg count"));
            return ClassPrecedenceListOf(args[0]);
        }, "CLASS-PRECEDENCE-LIST", 1));

        // CLASS-FINALIZED-P
        Emitter.CilAssembler.RegisterFunction("CLASS-FINALIZED-P", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-FINALIZED-P: wrong arg count"));
            return ClassFinalizedP(args[0]);
        }, "CLASS-FINALIZED-P", 1));

        // CLASS-PROTOTYPE: make a prototype instance of a class
        Emitter.CilAssembler.RegisterFunction("CLASS-PROTOTYPE", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("CLASS-PROTOTYPE: wrong arg count"));
            return ClassPrototypeOf(args[0]);
        }, "CLASS-PROTOTYPE", 1));

        // GENERIC-FUNCTION-METHODS
        Emitter.CilAssembler.RegisterFunction("GENERIC-FUNCTION-METHODS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("GENERIC-FUNCTION-METHODS: wrong arg count"));
            return GenericFunctionMethods(args[0]);
        }, "GENERIC-FUNCTION-METHODS", 1));

        // GENERIC-FUNCTION-NAME
        Emitter.CilAssembler.RegisterFunction("GENERIC-FUNCTION-NAME", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("GENERIC-FUNCTION-NAME: wrong arg count"));
            return GenericFunctionName(args[0]);
        }, "GENERIC-FUNCTION-NAME", 1));

        // GENERIC-FUNCTION-LAMBDA-LIST is registered by Mop.cs (returns the stored
        // lambda-list / arity placeholder). A leftover stub here returned NIL for any
        // GF and, depending on startup registration order, shadowed the real one on
        // some platforms (Linux) but not others (Windows): making
        // generic-function-lambda-list return NIL only on Linux. Removed so only
        // Mop.cs registers it.

        // METHOD-QUALIFIERS is already registered as a proper GenericFunction above (lines ~2659-2685)

        // METHOD-SPECIALIZERS
        Emitter.CilAssembler.RegisterFunction("METHOD-SPECIALIZERS", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-SPECIALIZERS: wrong arg count"));
            if (args[0] is LispMethod m) {
                LispObject result = Nil.Instance;
                for (int i = m.Specializers.Length - 1; i >= 0; i--)
                    result = new Cons(m.Specializers[i], result);
                return result;
            }
            throw new LispErrorException(new LispTypeError("METHOD-SPECIALIZERS: not a method", args[0]));
        }, "METHOD-SPECIALIZERS", 1));

        // METHOD-GENERIC-FUNCTION: the generic function this method is attached
        // to, or NIL while it is unattached. The comment here used to say "not
        // tracked in dotcl" and the body returned NIL for every method, but
        // LispMethod.Owner has tracked it all along (ADD-METHOD sets it,
        // REMOVE-METHOD clears it), and DOTCL-MOP returned it correctly.
        Emitter.CilAssembler.RegisterFunction("METHOD-GENERIC-FUNCTION", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-GENERIC-FUNCTION: wrong arg count"));
            return MethodGenericFunction(args[0]);
        }, "METHOD-GENERIC-FUNCTION", 1));

        // METHOD-LAMBDA-LIST: the recorded unspecialized lambda list (see
        // MethodLambdaList). Used to return NIL for every method here while
        // DOTCL-MOP returned the real shape.
        Emitter.CilAssembler.RegisterFunction("METHOD-LAMBDA-LIST", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError("METHOD-LAMBDA-LIST: wrong arg count"));
            return MethodLambdaList(args[0]);
        }, "METHOD-LAMBDA-LIST", 1));

        // MAKE-METHOD-LAMBDA: stub (needed by some MOP code)
        Emitter.CilAssembler.RegisterFunction("MAKE-METHOD-LAMBDA", new LispFunction(args => {
            // (make-method-lambda gf method lambda-form env)
            if (args.Length < 3) throw new LispErrorException(new LispProgramError("MAKE-METHOD-LAMBDA: wrong arg count"));
            return args[2];  // return the lambda form as-is
        }, "MAKE-METHOD-LAMBDA"));

        // ENSURE-CLASS: create/redefine a class honoring :metaclass,
        // :direct-superclasses, and :direct-slots, then register it. Mirrors what
        // DEFCLASS expands to (%register-class (%make-class-full ...)) so a custom
        // metaclass is preserved (AMOP). Previously a stub that ignored them.
        Emitter.CilAssembler.RegisterFunction("ENSURE-CLASS", new LispFunction(args => {
            // (ensure-class name &key metaclass direct-superclasses direct-slots ...)
            var name = args[0];
            // Normalize name to a symbol (list names become their print-name symbol)
            Symbol nameSym = name is Symbol s ? s : Startup.Sym(name.ToString() ?? "");
            LispObject metaclassSpec = Nil.Instance, supersSpec = Nil.Instance;
            // Metaclass-slot initargs (e.g. :type-name), plus :DIRECT-SLOTS and
            // :DIRECT-DEFAULT-INITARGS, which MakeClassCore reads from here.
            var extra = new List<LispObject>();
            bool metaSeen = false, supersSeen = false;
            for (int i = 1; i + 1 < args.Length; i += 2)
            {
                if (args[i] is not Symbol k) continue;
                if (k == Startup.Keyword("METACLASS"))
                {
                    if (!metaSeen) { metaclassSpec = args[i + 1]; metaSeen = true; }
                }
                else if (k == Startup.Keyword("DIRECT-SUPERCLASSES"))
                {
                    if (!supersSeen) { supersSpec = args[i + 1]; supersSeen = true; }
                }
                else { extra.Add(k); extra.Add(args[i + 1]); }
            }
            // Resolve the metaclass (a class object or a class name); null => STANDARD-CLASS.
            LispClass? metaclass = metaclassSpec switch
            {
                LispClass mc => mc,
                Symbol ms => Runtime.FindClassOrNil(ms) as LispClass,
                _ => null
            };
            var (supersList, slotDefsList) = Runtime.ParseClassInitargs(supersSpec, Nil.Instance);
            // Pass metaclass-slot initargs (e.g. :type-name) into the class object's single
            // init so shared-initialize applies them before inherited initialize-instance
            // :after runs. RegisterClass copies ExtraSlots to the existing/forward-ref class,
            // so re-ensure / forward-ref resolution carry the initialized slots too.
            var clsObj = Runtime.MakeClassFullWithInitargs(nameSym, supersList, slotDefsList,
                                               (LispObject?)metaclass ?? Nil.Instance, extra.ToArray());
            LispObject registered = clsObj is LispClass cls ? Runtime.RegisterClass(cls) : clsObj;
            return registered;
        }, "ENSURE-CLASS").WithKeyArgs(1, 1, true, "METACLASS", "DIRECT-SUPERCLASSES", "DIRECT-SLOTS", "DIRECT-DEFAULT-INITARGS"));
    }
}
