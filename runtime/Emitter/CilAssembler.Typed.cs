#if DOTCL_EMIT
using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

// DEFMETHOD-TYPED: a global function whose DECLAIM FTYPE gives every argument and
// the result a numeric type. Its body is assembled into a method taking and
// returning raw values (TypedEntrySig), and the ordinary entries of the
// LispFunction are a shell over it: convert each argument (checked from safety 1
// up), call the body, box the result. The body method is also installed as the
// function's typed entry, for callers that know the signature.
public partial class CilAssembler
{
    /// <summary>The typed body method being assembled, the target of
    /// (:call-typed-self). Null outside a typed body.</summary>
    internal MethodInfo? _typedSelfMethod;

    internal sealed class TypedForm
    {
        public string Name = "";
        public string? Pkg;
        public bool NoFrame;
        public LispObject? LambdaList;
        public List<string> Params = new();
        public string Sig = "";
        public bool Checks;
        public long[] Lo = Array.Empty<long>();
        public long[] Hi = Array.Empty<long>();
        public bool[] Ranged = Array.Empty<bool>();
        public LispObject Body = Nil.Instance;
        public int ParamCount => Params.Count;
    }

    // (:defmethod-typed "NAME" [:pkg "PKG"] [:no-frame T] [:lambda-list LL]
    //   :params ("P1" ...) :sig "LD>L" :arg-ranges (NIL (LO HI) ...) :checks T|NIL
    //   :body (...))
    internal static TypedForm ParseTypedForm(Cons instr)
    {
        var f = new TypedForm();
        var plist = instr.Cdr;
        f.Name = GetString(Car(plist));
        plist = Cdr(plist);
        LispObject ranges = Nil.Instance;
        LispObject? body = null;
        while (plist is Cons pc)
        {
            var key = GetSymbolName(pc.Car);
            var val = Cadr(pc);
            switch (key)
            {
                case "NO-FRAME": f.NoFrame = true; break;
                case "PKG": f.Pkg = GetString(val); break;
                case "LAMBDA-LIST": f.LambdaList = val; break;
                case "PARAMS":
                    for (var cur = val; cur is Cons lc; cur = lc.Cdr) f.Params.Add(GetString(lc.Car));
                    break;
                case "SIG": f.Sig = GetString(val); break;
                case "CHECKS": f.Checks = val is not Nil; break;
                case "ARG-RANGES": ranges = val; break;
                case "BODY": body = val; break;
            }
            plist = Cddr(pc);
        }
        if (body == null) throw new Exception("DEFMETHOD-TYPED: missing :body");
        f.Body = body;
        int n = f.Params.Count;
        if (!TypedEntrySig.IsValid(f.Sig) || f.Sig.Length - 2 != n)
            throw new Exception($"DEFMETHOD-TYPED: signature {f.Sig} does not fit {n} parameters");
        f.Lo = new long[n]; f.Hi = new long[n]; f.Ranged = new bool[n];
        int i = 0;
        for (var cur = ranges; cur is Cons rc && i < n; cur = rc.Cdr, i++)
        {
            if (rc.Car is Cons r && r.Car is Fixnum lo && r.Cdr is Cons r2 && r2.Car is Fixnum hi)
            {
                f.Lo[i] = lo.Value; f.Hi[i] = hi.Value; f.Ranged[i] = true;
            }
        }
        return f;
    }

    static readonly MethodInfo TypedArgLMI = typeof(Runtime).GetMethod("TypedArgL")!;
    static readonly MethodInfo TypedArgLRMI = typeof(Runtime).GetMethod("TypedArgLR")!;
    static readonly MethodInfo TypedArgDMI = typeof(Runtime).GetMethod("TypedArgD")!;
    static readonly MethodInfo FixnumMakeTMI = typeof(Fixnum).GetMethod("Make")!;
    static readonly MethodInfo FixnumGetValueTMI = typeof(Fixnum).GetProperty("Value")!.GetGetMethod()!;
    static readonly MethodInfo DoubleGetValueTMI = typeof(DoubleFloat).GetProperty("Value")!.GetGetMethod()!;
    static readonly ConstructorInfo DoubleCtorTMI = typeof(DoubleFloat).GetConstructor(new[] { typeof(double) })!;

    /// <summary>The body of the boxed entry over a typed body method:
    /// LispObject Shell(LispFunction self, LispObject a1..an).</summary>
    internal static void EmitTypedShell(ILGenerator il, TypedForm f, MethodInfo typed)
    {
        int n = f.ParamCount;
        il.Emit(OpCodes.Ldarg_0);
        for (int i = 0; i < n; i++)
        {
            il.Emit(OpCodes.Ldarg, i + 1);
            if (f.Sig[i] == 'D')
            {
                if (f.Checks) il.Emit(OpCodes.Call, TypedArgDMI);
                else { il.Emit(OpCodes.Castclass, typeof(DoubleFloat)); il.Emit(OpCodes.Call, DoubleGetValueTMI); }
            }
            else if (f.Checks && f.Ranged[i])
            {
                il.Emit(OpCodes.Ldc_I8, f.Lo[i]);
                il.Emit(OpCodes.Ldc_I8, f.Hi[i]);
                il.Emit(OpCodes.Call, TypedArgLRMI);
            }
            else if (f.Checks) il.Emit(OpCodes.Call, TypedArgLMI);
            else { il.Emit(OpCodes.Castclass, typeof(Fixnum)); il.Emit(OpCodes.Call, FixnumGetValueTMI); }
        }
        il.Emit(OpCodes.Call, typed);
        if (f.Sig[n + 1] == 'D') il.Emit(OpCodes.Newobj, DoubleCtorTMI);
        else il.Emit(OpCodes.Call, FixnumMakeTMI);
        il.Emit(OpCodes.Ret);
    }

    private void HandleDefmethodTyped(Cons instr)
    {
        var f = ParseTypedForm(instr);
        int n = f.ParamCount;

        if (_faslMode && _faslTypeBuilder != null)
        {
            int faslId = Interlocked.Increment(ref _faslClosureCount);
            FaslAssembler.EmitDefmethodTypedInto(_faslTypeBuilder, _il, _faslStructMap!, f, faslId);
            return;
        }

        var typedDm = new DynamicMethod(f.Name + "_typed", TypedEntrySig.ReturnType(f.Sig),
            TypedEntrySig.ParamTypes(f.Sig), typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = typedDm.GetILGenerator();
        innerAsm._typedSelfMethod = typedDm;
        innerAsm.Assemble(f.Body);

        var shellTypes = new Type[n + 1];
        shellTypes[0] = typeof(LispFunction);
        for (int i = 0; i < n; i++) shellTypes[i + 1] = typeof(LispObject);
        var shellDm = new DynamicMethod(f.Name + "_shell", typeof(LispObject), shellTypes,
            typeof(CilAssembler).Module, true);
        EmitTypedShell(shellDm.GetILGenerator(), f, typedDm);

        var fn = BuildSelfDirectFunction(shellDm, f.Name, n);
        fn.SetTypedEntry(typedDm.CreateDelegate(TypedEntrySig.DelegateType(f.Sig)),
            TypedEntrySig.Code(f.Sig));
        FinishInMemoryDefmethod(fn, f.Name, f.Pkg, f.LambdaList, f.NoFrame, f.Body);
    }
}
#endif
