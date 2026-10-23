#if DOTCL_EMIT
using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

// DEFMETHOD-TYPED: a global function whose DECLAIM FTYPE gives some arguments or
// the result a numeric type with a raw representation. Its body is assembled into a method taking and
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

    /// <summary>The (lo, hi) the shell checks each integer argument against, for
    /// LispFunction.SetTypedArgRanges: four pairs, the full range where it checks none.</summary>
    internal static long[] TypedArgRanges(TypedForm f)
    {
        var r = new long[2 * TypedEntrySig.MaxNarrowArgs];
        for (int i = 0; i < TypedEntrySig.MaxNarrowArgs; i++)
        {
            bool ranged = i < f.ParamCount && f.Sig[i] == 'L' && f.Checks && f.Ranged[i];
            r[2 * i] = ranged ? f.Lo[i] : long.MinValue;
            r[2 * i + 1] = ranged ? f.Hi[i] : long.MaxValue;
        }
        return r;
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
    /// <summary>A shell for SELFFRAMED links the function's own debugger frame
    /// around the body (see Function.SelfFramed.cs), so that callers reach it
    /// through the entry pointer instead of InvokeN's framed path.</summary>
    internal static void EmitTypedShell(ILGenerator il, TypedForm f, MethodInfo typed,
                                        bool selfFramed = false, bool unit = false)
    {
        int n = f.ParamCount;
        LocalBuilder? fr = null;
        if (selfFramed)
        {
            fr = il.DeclareLocal(typeof(LispFunction.Frame));
            il.Emit(OpCodes.Ldloca, fr);
            il.Emit(OpCodes.Ldstr, string.Intern(f.Name));
            if (unit) il.Emit(OpCodes.Ldarg_0);
            for (int i = 1; i <= n; i++) il.Emit(OpCodes.Ldarg, i);
            il.Emit(OpCodes.Call, (unit ? s_enterNU : s_enterN)[n]);
        }
        il.Emit(OpCodes.Ldarg_0);
        for (int i = 0; i < n; i++)
        {
            il.Emit(OpCodes.Ldarg, i + 1);
            if (f.Sig[i] == 'O') continue;
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
        if (fr != null)
        {
            il.Emit(OpCodes.Ldloca, fr);
            il.Emit(OpCodes.Call, s_leaveFrame);
        }
        if (f.Sig[n + 1] == 'D') il.Emit(OpCodes.Newobj, DoubleCtorTMI);
        else if (f.Sig[n + 1] == 'L') il.Emit(OpCodes.Call, FixnumMakeTMI);
        il.Emit(OpCodes.Ret);
    }

    /// <summary>The framed typed entry over a typed body method:
    /// R Entry(LispFunction self, T1..Tn) links the frame InvokeTypedN would push
    /// (raw arguments), calls the body and unlinks it.</summary>
    internal static void EmitTypedFramedEntry(ILGenerator il, TypedForm f, MethodInfo typed, bool unit = false)
    {
        int n = f.ParamCount;
        var fr = il.DeclareLocal(typeof(LispFunction.Frame));
        var ta = il.DeclareLocal(TypedEntrySig.IsWide(f.Sig) ? typeof(TypedFrameArgsW) : typeof(TypedFrameArgs));
        il.Emit(OpCodes.Ldloca, fr);
        il.Emit(OpCodes.Ldloca, ta);
        il.Emit(OpCodes.Ldstr, string.Intern(f.Name));
        il.Emit(OpCodes.Ldc_I4, TypedEntrySig.Code(f.Sig));
        for (int i = 1; i <= n; i++) il.Emit(OpCodes.Ldarg, i);
        il.Emit(OpCodes.Call, LispFunction.TypedEnterMethod(f.Sig));
        if (unit)
        {
            il.Emit(OpCodes.Ldloca, fr);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, s_enterUnit);
        }
        for (int i = 0; i <= n; i++) il.Emit(OpCodes.Ldarg, i);
        il.Emit(OpCodes.Call, typed);
        il.Emit(OpCodes.Ldloca, fr);
        il.Emit(OpCodes.Call, s_leaveFrame);
        il.Emit(OpCodes.Ret);
    }

    // Set while assembling the leaf copy of a typed body as its framed entry:
    // the frame the entry stands for, handed to the LispFunction.LeafX helpers.
    internal TypedForm? _leafEntry;

    static readonly Dictionary<string, MethodInfo> s_leafOps = BuildLeafOps();

    static Dictionary<string, MethodInfo> BuildLeafOps()
    {
        var d = new Dictionary<string, MethodInfo>();
        foreach (var op in new[] { "AddFixnumChecked", "SubtractFixnumChecked", "MultiplyFixnumChecked",
                                   "FloorFixnumL", "TruncateFixnumL", "ModFixnumL", "RemFixnumL",
                                   "CheckLongRange" })
            d["Runtime." + op] = typeof(LispFunction).GetMethod("Leaf" + op)!;
        return d;
    }

    static readonly HashSet<string> s_leafPlainOps = new()
    {
        "LDARG", "LDLOC", "STLOC", "DECLARE-LOCAL", "LDC-I4", "LDC-I8", "LDC-R8", "LDC-R4",
        "POP", "DUP", "RET", "ADD", "SUB", "MUL", "AND", "OR", "XOR", "NOT", "NEG",
        "SHL", "SHR", "SHR-UN", "CEQ", "CGT", "CLT", "CGT-UN", "CLT-UN",
        "CONV-I1", "CONV-I2", "CONV-U1", "CONV-U2", "CONV-U4", "CONV-U8", "CONV-I8", "CONV-I",
        "CONV-I4", "CONV-R4", "CONV-R8", "TYPED-TAIL-SELF",
    };

    // Calls that cannot signal, kept as they are in a leaf.
    static readonly HashSet<string> s_leafPureCalls = new() { "Math.MinLong", "Math.MaxLong" };

    static readonly HashSet<string> s_leafBranchOps = new()
    {
        "BR", "BRFALSE", "BRTRUE", "BEQ", "BNE", "BGT", "BLT", "BGE", "BLE",
        "BGE-UN", "BGT-UN", "BLE-UN", "BLT-UN",
    };

    /// <summary>A typed body that calls nothing but the checked helpers of
    /// s_leafOps and has no loop: nothing in it can look at the frame list
    /// except those helpers' slow paths.</summary>
    internal static bool IsTypedLeaf(LispObject body)
    {
        var seen = new HashSet<string>();
        for (var cur = body; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is not Cons ic || ic.Car is not Symbol s) return false;
            var op = s.Name;
            if (op == "LABEL") seen.Add(GetSymbolName(Cadr(ic)));
            else if (s_leafBranchOps.Contains(op)) { if (seen.Contains(GetSymbolName(Cadr(ic)))) return false; }
            else if (op == "CALL")
            {
                var callee = GetString(Cadr(ic));
                if (!s_leafOps.ContainsKey(callee) && !s_leafPureCalls.Contains(callee)) return false;
            }
            else if (!s_leafPlainOps.Contains(op)) return false;
        }
        return true;
    }

    private bool TryEmitLeafCall(string name)
    {
        if (_leafEntry is not { } f || !s_leafOps.TryGetValue(name, out var leaf)) return false;
        _il.Emit(OpCodes.Ldstr, string.Intern(f.Name));
        _il.Emit(OpCodes.Ldc_I4, TypedEntrySig.Code(f.Sig));
        _il.Emit(OpCodes.Ldc_I4, f.ParamCount);
        for (int i = 1; i <= 4; i++)
        {
            if (i > f.ParamCount) { _il.Emit(OpCodes.Ldc_I8, 0L); continue; }
            _il.Emit(OpCodes.Ldarg, i);
            if (f.Sig[i - 1] == 'D') _il.Emit(OpCodes.Call, s_doubleBits);
        }
        _il.Emit(OpCodes.Call, leaf);
        return true;
    }

    static readonly MethodInfo s_doubleBits = typeof(BitConverter).GetMethod("DoubleToInt64Bits", new[] { typeof(double) })!;

    /// <summary>Assemble the leaf copy of F's typed body into IL: the framed
    /// entry of a leaf.</summary>
    internal static void EmitLeafEntry(ILGenerator il, TypedForm f, Action<CilAssembler>? setup = null)
    {
        var asm = new CilAssembler();
        asm._il = il;
        asm._leafEntry = f;
        setup?.Invoke(asm);
        asm.Assemble(f.Body);
    }

    // The framed entry of an in-memory function is reached only by its address:
    // this keeps its DynamicMethod alive as long as the function.
    static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispFunction, DynamicMethod>
        s_typedEntryOwners = new();

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
        bool selfFramed = CanSelfFrame(f.Name, n, f.NoFrame);
        EmitTypedShell(shellDm.GetILGenerator(), f, typedDm, selfFramed, unit: true);

        var fn = BuildSelfDirectFunction(shellDm, f.Name, n);
        if (selfFramed) fn.SetSelfFramedEntry(DynamicMethodEntry(shellDm), n);
        fn.SetTypedEntry(typedDm.CreateDelegate(TypedEntrySig.DelegateType(f.Sig)),
            TypedEntrySig.Code(f.Sig));
        if (selfFramed)
        {
            var entryDm = new DynamicMethod(f.Name + "_tentry", TypedEntrySig.ReturnType(f.Sig),
                TypedEntrySig.ParamTypes(f.Sig), typeof(CilAssembler).Module, true);
            if (!TypedEntrySig.IsWide(f.Sig) && IsTypedLeaf(f.Body)) EmitLeafEntry(entryDm.GetILGenerator(), f);
            else EmitTypedFramedEntry(entryDm.GetILGenerator(), f, typedDm, unit: true);
            s_typedEntryOwners.Add(fn, entryDm);
            var r = TypedArgRanges(f);
            fn.SetTypedArgRanges(r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7]);
            fn.SetTypedFramedEntry(DynamicMethodEntry(entryDm), TypedEntrySig.Code(f.Sig));
        }
        FinishInMemoryDefmethod(fn, f.Name, f.Pkg, f.LambdaList, f.NoFrame, f.Body);
    }
}
#endif
