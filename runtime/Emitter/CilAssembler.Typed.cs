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
    // The typed body being assembled. A nested self-call grows the stack with no
    // frame and so meets no depth check of an entry: the body makes its own.
    internal TypedForm? _typedForm;

    static MethodInfo? _lazy_s_typedSelfDepth;
    static MethodInfo s_typedSelfDepth => _lazy_s_typedSelfDepth ??= typeof(LispFunction).GetMethod("TypedSelfDepth")!;
    static MethodInfo? _lazy_s_typedSelfCount;
    static MethodInfo s_typedSelfCount => _lazy_s_typedSelfCount ??= typeof(LispFunction).GetMethod("TypedSelfCount")!;

    /// <summary>For a SelfFree body, the receiver of a self-call: one less than
    /// its own count of self-calls left before a stack check.</summary>
    private void EmitTypedSelfDepth(TypedForm f)
    {
        _il.Emit(OpCodes.Ldarg_0);
        _il.Emit(OpCodes.Ldc_I4_1);
        _il.Emit(OpCodes.Sub);
    }

    /// <summary>Assemble the body of F; a SelfFree one first checks the stack
    /// when its count has run out.</summary>
    internal void AssembleTypedBody(TypedForm f)
    {
        _typedForm = f;
        if (f.SelfFree)
        {
            _il.Emit(OpCodes.Ldarg_0);
            _il.Emit(OpCodes.Ldstr, string.Intern(f.Name));
            _il.Emit(OpCodes.Call, s_typedSelfDepth);
            _il.Emit(OpCodes.Starg, 0);
        }
        Assemble(f.Body);
    }

    private void EmitTypedSelfCount(TypedForm f)
    {
        _il.Emit(OpCodes.Ldstr, string.Intern(f.Name));
        _il.Emit(OpCodes.Call, s_typedSelfCount);
    }

    /// <summary>Call the body with self (argument 0 of the caller), or a fresh
    /// count for a SelfFree one, and the raw arguments the caller pushes with PUSHARGS.</summary>
    internal static void EmitTypedBodyCall(ILGenerator il, TypedForm f, MethodInfo body, Action pushArgs)
    {
        if (f.SelfFree) il.Emit(OpCodes.Ldc_I4, LispFunction.TypedSelfInterval);
        else il.Emit(OpCodes.Ldarg_0);
        pushArgs();
        il.Emit(OpCodes.Call, body);
    }

    /// <summary>The method for the typed entry delegate: BODY, or for a SelfFree
    /// one a wrapper of the TypedEntrySig signature.</summary>
    internal static MethodInfo TypedEntryMethod(TypedForm f, MethodInfo body, Func<MethodInfo> defineWrapper,
                                                Func<MethodInfo, ILGenerator> ilOf)
    {
        if (!f.SelfFree) return body;
        var w = defineWrapper();
        var il = ilOf(w);
        EmitTypedBodyCall(il, f, body, () => { for (int i = 1; i <= f.ParamCount; i++) il.Emit(OpCodes.Ldarg, i); });
        il.Emit(OpCodes.Ret);
        return w;
    }

    /// <summary>BODY calls itself, and every (:ldarg 0) of it is the receiver of
    /// a self-call: followed by the N argument loads and (:call-typed-self).</summary>
    static bool SelfOnlyForSelfCalls(LispObject body, int n)
    {
        bool any = false;
        for (var cur = body; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is not Cons ic || ic.Car is not Symbol s) continue;
            if (s.Name is "LDARGA" or "STARG" && Cadr(ic) is Fixnum { Value: 0 }) return false;
            if (s.Name != "LDARG" || Cadr(ic) is not Fixnum { Value: 0 }) continue;
            var k = c.Cdr;
            for (int i = 0; i < n; i++)
            {
                if (k is not Cons kc || kc.Car is not Cons kl || kl.Car is not Symbol ks || ks.Name != "LDLOC") return false;
                k = kc.Cdr;
            }
            if (k is not Cons cc || cc.Car is not Cons cl || cl.Car is not Symbol cs || cs.Name != "CALL-TYPED-SELF") return false;
            any = true;
        }
        return any;
    }

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
        // The body method takes the self-call depth in place of the function.
        public bool SelfFree;
        // A speculative entry: the ordinary body, run by the shell for an
        // argument the typed body does not take. GenericSelf: it takes the
        // function as its leading argument.
        public LispObject? Generic;
        public bool GenericSelf;
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
                case "GENERIC": f.Generic = val; break;
                case "GENERIC-SELF": f.GenericSelf = val is not Nil; break;
            }
            plist = Cddr(pc);
        }
        if (body == null) throw new Exception("DEFMETHOD-TYPED: missing :body");
        f.Body = body;
        f.SelfFree = SelfOnlyForSelfCalls(body, f.Params.Count);
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

    static MethodInfo? _lazy_TypedArgLMI;
    static MethodInfo TypedArgLMI => _lazy_TypedArgLMI ??= typeof(Runtime).GetMethod("TypedArgL")!;
    static MethodInfo? _lazy_TypedArgLRMI;
    static MethodInfo TypedArgLRMI => _lazy_TypedArgLRMI ??= typeof(Runtime).GetMethod("TypedArgLR")!;
    static MethodInfo? _lazy_TypedArgDMI;
    static MethodInfo TypedArgDMI => _lazy_TypedArgDMI ??= typeof(Runtime).GetMethod("TypedArgD")!;
    static MethodInfo? _lazy_FixnumMakeTMI;
    static MethodInfo FixnumMakeTMI => _lazy_FixnumMakeTMI ??= typeof(Fixnum).GetMethod("Make")!;
    static MethodInfo? _lazy_FixnumGetValueTMI;
    static MethodInfo FixnumGetValueTMI => _lazy_FixnumGetValueTMI ??= typeof(Fixnum).GetProperty("Value")!.GetGetMethod()!;
    static MethodInfo? _lazy_DoubleGetValueTMI;
    static MethodInfo DoubleGetValueTMI => _lazy_DoubleGetValueTMI ??= typeof(DoubleFloat).GetProperty("Value")!.GetGetMethod()!;
    static ConstructorInfo? _lazy_DoubleCtorTMI;
    static ConstructorInfo DoubleCtorTMI => _lazy_DoubleCtorTMI ??= typeof(DoubleFloat).GetConstructor(new[] { typeof(double) })!;

    /// <summary>The body of the boxed entry over a typed body method:
    /// LispObject Shell(LispFunction self, LispObject a1..an).</summary>
    /// <summary>A shell for SELFFRAMED links the function's own debugger frame
    /// around the body (see Function.SelfFramed.cs), so that callers reach it
    /// through the entry pointer instead of InvokeN's framed path.</summary>
    internal static void EmitTypedShell(ILGenerator il, TypedForm f, MethodInfo typed,
                                        bool selfFramed = false, bool unit = false, MethodInfo? generic = null,
                                        MethodInfo? entryCheck = null)
    {
        int n = f.ParamCount;
        // The shell of a function without a frame (debug 0) that callers enter by calli: it makes
        // the stack and interrupt check InvokeN makes before a body.
        if (entryCheck != null) il.Emit(OpCodes.Call, entryCheck);
        Label gen = default;
        if (generic != null)
        {
            gen = il.DefineLabel();
            for (int i = 0; i < n; i++)
            {
                if (f.Sig[i] == 'O') continue;
                il.Emit(OpCodes.Ldarg, i + 1);
                il.Emit(OpCodes.Isinst, f.Sig[i] == 'D' ? typeof(DoubleFloat) : typeof(Fixnum));
                il.Emit(OpCodes.Brfalse, gen);
                if (f.Sig[i] == 'L' && f.Ranged[i])
                {
                    il.Emit(OpCodes.Ldarg, i + 1);
                    il.Emit(OpCodes.Castclass, typeof(Fixnum));
                    il.Emit(OpCodes.Call, FixnumGetValueTMI);
                    il.Emit(OpCodes.Ldc_I8, f.Lo[i]);
                    il.Emit(OpCodes.Blt, gen);
                    il.Emit(OpCodes.Ldarg, i + 1);
                    il.Emit(OpCodes.Castclass, typeof(Fixnum));
                    il.Emit(OpCodes.Call, FixnumGetValueTMI);
                    il.Emit(OpCodes.Ldc_I8, f.Hi[i]);
                    il.Emit(OpCodes.Bgt, gen);
                }
            }
        }
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
        EmitTypedBodyCall(il, f, typed, () =>
        {
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
        });
        if (fr != null)
        {
            il.Emit(OpCodes.Ldloca, fr);
            il.Emit(OpCodes.Call, s_leaveFrame);
        }
        if (f.Sig[n + 1] == 'D') il.Emit(OpCodes.Newobj, DoubleCtorTMI);
        else if (f.Sig[n + 1] == 'L') il.Emit(OpCodes.Call, FixnumMakeTMI);
        il.Emit(OpCodes.Ret);
        if (generic != null)
        {
            il.MarkLabel(gen);
            for (int i = 0; i <= n; i++) il.Emit(OpCodes.Ldarg, i);
            il.Emit(OpCodes.Call, generic);
            il.Emit(OpCodes.Ret);
        }
    }

    /// <summary>Set up ASM to assemble the generic body of F into a method
    /// LispObject (LispFunction self, LispObject a1..an), the way a
    /// DEFMETHOD-DIRECT body with a self argument is assembled.</summary>
    internal static void SetupGenericBody(CilAssembler asm, TypedForm f, bool selfFramed, bool unit)
    {
        if (selfFramed)
        {
            asm._selfFrameName = string.Intern(f.Name);
            asm._selfFrameArgc = f.ParamCount;
            asm._selfFrameUnit = unit;
        }
        if (!f.GenericSelf) asm._argShift = 1;
    }

    internal static Type[] GenericBodyParamTypes(int n)
    {
        var t = new Type[n + 1];
        t[0] = typeof(LispFunction);
        for (int i = 0; i < n; i++) t[i + 1] = typeof(LispObject);
        return t;
    }

    /// <summary>The shell of a leaf links no frame while every argument has the
    /// raw type it declares: it unboxes them and calls LEAF, the leaf copy of the
    /// body, which links one only on a path that signals. Any other argument goes
    /// on to FRAMED, the framed shell, which signals the type error. That is a
    /// method of its own: a frame local here would be zeroed on every call.</summary>
    internal static void EmitLeafShell(ILGenerator il, TypedForm f, MethodInfo leaf, MethodInfo framed)
    {
        int n = f.ParamCount;
        var slow = il.DefineLabel();
        var boxed = new LocalBuilder?[n];
        for (int i = 0; i < n; i++)
        {
            char k = f.Sig[i];
            if (k == 'O') continue;
            var t = k == 'D' ? typeof(DoubleFloat) : typeof(Fixnum);
            boxed[i] = il.DeclareLocal(t);
            il.Emit(OpCodes.Ldarg, i + 1);
            il.Emit(OpCodes.Isinst, t);
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Stloc, boxed[i]!);
            il.Emit(OpCodes.Brfalse, slow);
            if (k != 'D' && f.Checks && f.Ranged[i])
            {
                il.Emit(OpCodes.Ldloc, boxed[i]!);
                il.Emit(OpCodes.Call, FixnumGetValueTMI);
                il.Emit(OpCodes.Ldc_I8, f.Lo[i]);
                il.Emit(OpCodes.Blt, slow);
                il.Emit(OpCodes.Ldloc, boxed[i]!);
                il.Emit(OpCodes.Call, FixnumGetValueTMI);
                il.Emit(OpCodes.Ldc_I8, f.Hi[i]);
                il.Emit(OpCodes.Bgt, slow);
            }
        }
        il.Emit(OpCodes.Ldarg_0);
        for (int i = 0; i < n; i++)
        {
            if (boxed[i] == null) { il.Emit(OpCodes.Ldarg, i + 1); continue; }
            il.Emit(OpCodes.Ldloc, boxed[i]!);
            il.Emit(OpCodes.Call, f.Sig[i] == 'D' ? DoubleGetValueTMI : FixnumGetValueTMI);
        }
        il.Emit(OpCodes.Call, leaf);
        if (f.Sig[n + 1] == 'D') il.Emit(OpCodes.Newobj, DoubleCtorTMI);
        else if (f.Sig[n + 1] == 'L') il.Emit(OpCodes.Call, FixnumMakeTMI);
        il.Emit(OpCodes.Ret);
        il.MarkLabel(slow);
        for (int i = 0; i <= n; i++) il.Emit(OpCodes.Ldarg, i);
        il.Emit(OpCodes.Call, framed);
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
        EmitTypedBodyCall(il, f, typed, () => { for (int i = 1; i <= n; i++) il.Emit(OpCodes.Ldarg, i); });
        il.Emit(OpCodes.Ldloca, fr);
        il.Emit(OpCodes.Call, s_leaveFrame);
        il.Emit(OpCodes.Ret);
    }

    // Set while assembling the leaf copy of a typed body as its framed entry:
    // the frame the entry stands for, handed to the LispFunction.LeafX helpers.
    internal TypedForm? _leafEntry;

    static Dictionary<string, MethodInfo>? _lazy_s_leafOps;
    static Dictionary<string, MethodInfo> s_leafOps => _lazy_s_leafOps ??= BuildLeafOps();

    static Dictionary<string, MethodInfo> BuildLeafOps()
    {
        var d = new Dictionary<string, MethodInfo>();
        foreach (var op in new[] { "AddFixnumChecked", "SubtractFixnumChecked", "MultiplyFixnumChecked",
                                   "FloorFixnumL", "TruncateFixnumL", "ModFixnumL", "RemFixnumL",
                                   "CheckLongRange" })
            d["Runtime." + op] = typeof(LispFunction).GetMethod("Leaf" + op)!;
        foreach (var op in new[] { "AddD", "SubtractD", "MultiplyD", "DivideD",
                                   "AddS", "SubtractS", "MultiplyS", "DivideS", "NarrowD" })
            d["FloatLane." + op] = typeof(LispFunction).GetMethod("Leaf" + op)!;
        return d;
    }

    static readonly HashSet<string> s_leafPlainOps = new()
    {
        "LDARG", "LDLOC", "STLOC", "DECLARE-LOCAL", "LDC-I4", "LDC-I8", "LDC-R8", "LDC-R4",
        "POP", "DUP", "RET", "ADD", "SUB", "MUL", "AND", "OR", "XOR", "NOT", "NEG",
        "SHL", "SHR", "SHR-UN", "CEQ", "CGT", "CLT", "CGT-UN", "CLT-UN",
        "CONV-I1", "CONV-I2", "CONV-U1", "CONV-U2", "CONV-U4", "CONV-U8", "CONV-I8", "CONV-I",
        "CONV-I4", "CONV-R4", "CONV-R8", "TYPED-TAIL-SELF", "LDNULL", "LDSFLD", "LOAD-CONST",
        "LOAD-SYM-KEYWORD",
    };

    // Calls that cannot signal, kept as they are in a leaf.
    static readonly HashSet<string> s_leafPureCalls = new()
    {
        "Math.MinLong", "Math.MaxLong", "MultipleValues.Reset", "MultipleValues.PrimaryAlone",
        "FloatLane.NonFiniteD", "FloatLane.NonFiniteS",
    };

    static readonly HashSet<string> s_leafBranchOps = new()
    {
        "BR", "BRFALSE", "BRTRUE", "BEQ", "BNE", "BGT", "BLT", "BGE", "BLE",
        "BGE-UN", "BGT-UN", "BLE-UN", "BLT-UN",
    };

    /// <summary>A typed body that calls nothing but the checked helpers of
    /// s_leafOps and has no loop: nothing in it can look at the frame list
    /// except those helpers' slow paths.</summary>
    /// <summary>A signature a leaf copy can take: at most four raw arguments (the
    /// narrow frame of LispFunction.InLeafFrame holds them), any result.</summary>
    internal static bool IsLeafSig(string sig)
    {
        int n = sig.Length - 2;
        return n <= TypedEntrySig.MaxNarrowArgs && sig.IndexOf('O') is var o && (o < 0 || o > n);
    }

    internal static bool IsTypedLeaf(LispObject body, bool cachedConsts = false)
    {
#if NET9_0_OR_GREATER
        // A leaf's frame is built on demand by runtime helpers a library does not call.
        if (LibraryAbiIL.Active) return false;
#endif
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
                if (!s_leafOps.ContainsKey(callee) && !s_leafPureCalls.Contains(callee) && callee != "Runtime.Car")
                    return false;
            }
            else if (op == "LOAD-CONST-CACHED") { if (!cachedConsts || !IsLtvFetch(Cadr(ic))) return false; }
            else if (!s_leafPlainOps.Contains(op)) return false;
        }
        return true;
    }

    static readonly HashSet<string> s_directLeafOps = new()
    {
        "LDARG", "LDLOC", "STLOC", "DECLARE-LOCAL", "LDC-I4", "LDC-I8", "POP", "DUP", "RET",
        "CEQ", "CGT", "CLT", "LDNULL", "LOAD-CONST", "TAIL-SELF", "LDSFLD", "LOAD-SYM-KEYWORD",
    };

    /// <summary>An untyped DEFUN body that calls nothing but MultipleValues.Reset /
    /// PrimaryAlone, EQ / EQL, CAR and the argument type check, reads constants
    /// and has no loop: its entry links no frame, and the two that can signal link one on
    /// their slow path (LispFunction.LeafCarD / LeafCheckArgType).</summary>
    internal static bool IsDirectLeaf(LispObject body, bool cachedConsts)
    {
#if NET9_0_OR_GREATER
        // A leaf's frame is built on demand by runtime helpers a library does not call.
        if (LibraryAbiIL.Active) return false;
#endif
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
                if (callee is not ("MultipleValues.Reset" or "MultipleValues.PrimaryAlone" or "Runtime.Car"
                    or "MultipleValues.TakeMode" or "Runtime.CheckArgType" or "Runtime.IsTrueEq" or "Runtime.IsTrueEql" or "Runtime.Eq"
                    or "Runtime.Eql" or "Runtime.IsTruthy" or "Runtime.IsNotNil" or "Runtime.UnwrapMv")) return false;
            }
            else if (op == "LOAD-CONST-CACHED") { if (!cachedConsts || !IsLtvFetch(Cadr(ic))) return false; }
            else if (!s_directLeafOps.Contains(op)) return false;
        }
        return true;
    }

    /// <summary>The recipe of a LOAD-TIME-VALUE cell in a fasl, run once by its
    /// cached constant: a call of DOTCL-INTERNAL::%GET-LTV-SLOT-IN on a string and
    /// an integer.</summary>
    static bool IsLtvFetch(LispObject recipe) =>
        MatchesShape(recipe, s_ltvFetch) || MatchesShape(recipe, s_ltvFetchMode)
        || MatchesShape(recipe, s_ltvFetchNoReset);

    // The fetch in a single-value position: no reset in front of the call.
    static readonly string[] s_ltvFetchNoReset =
    {
        "LOAD-SYM-PKG %GET-LTV-SLOT-IN", "CASTCLASS Symbol", "CALL CilAssembler.GetFunctionBySymbol",
        "LOAD-CONST", "LDC-I8", "CALL Fixnum.Make",
        "CALLVIRT LispFunction.Invoke2", "CALL Runtime.UnwrapMv",
    };

    static readonly string[] s_ltvFetch =
    {
        "LOAD-SYM-PKG %GET-LTV-SLOT-IN", "CASTCLASS Symbol", "CALL CilAssembler.GetFunctionBySymbol",
        "CALL MultipleValues.Reset", "LOAD-CONST", "LDC-I8", "CALL Fixnum.Make",
        "CALLVIRT LispFunction.Invoke2", "CALL Runtime.UnwrapMv",
    };

    // In a tail position the fetch passes on the value mode.
    static readonly string[] s_ltvFetchMode =
    {
        "LOAD-SYM-PKG %GET-LTV-SLOT-IN", "CASTCLASS Symbol", "CALL CilAssembler.GetFunctionBySymbol",
        "CALL MultipleValues.Reset", "LOAD-CONST", "LDC-I8", "CALL Fixnum.Make",
        "LDLOC", "CALLVIRT LispFunction.Invoke2M",
    };

    static bool MatchesShape(LispObject recipe, string[] shape)
    {
        int i = 0;
        for (var cur = recipe; cur is Cons c; cur = c.Cdr, i++)
        {
            if (i >= shape.Length || c.Car is not Cons ic || ic.Car is not Symbol s) return false;
            var want = shape[i];
            int sp = want.IndexOf(' ');
            var wantOp = sp < 0 ? want : want.Substring(0, sp);
            if (s.Name != wantOp) return false;
            if (sp >= 0 && !(Cadr(ic) is LispString arg && arg.Value == want.Substring(sp + 1))) return false;
            if (wantOp == "LOAD-CONST" && Cadr(ic) is not LispString) return false;
        }
        return i == shape.Length;
    }

    // Set while assembling an untyped leaf body (IsDirectLeaf): its name and
    // argument count, for the frame LeafCarD / LeafCheckArgType link.
    internal (string Name, int Argc)? _directLeaf;

    static MethodInfo? _lazy_s_leafCar;
    static MethodInfo s_leafCar => _lazy_s_leafCar ??= typeof(LispFunction).GetMethod("LeafCar")!;
    static MethodInfo? _lazy_s_leafCarD;
    static MethodInfo s_leafCarD => _lazy_s_leafCarD ??= typeof(LispFunction).GetMethod("LeafCarD")!;
    static MethodInfo? _lazy_s_leafCheckArgType;
    static MethodInfo s_leafCheckArgType => _lazy_s_leafCheckArgType ??= typeof(LispFunction).GetMethod("LeafCheckArgType")!;

    private bool TryEmitLeafCall(string name)
    {
        if (_directLeaf is { } d)
        {
            var m = name == "Runtime.Car" ? s_leafCarD : name == "Runtime.CheckArgType" ? s_leafCheckArgType : null;
            if (m == null) return false;
            _il.Emit(OpCodes.Ldstr, string.Intern(d.Name));
            _il.Emit(OpCodes.Ldc_I4, d.Argc);
            for (int i = 1; i <= 4; i++)
            {
                if (i > d.Argc) _il.Emit(OpCodes.Ldnull);
                else _il.Emit(OpCodes.Ldarg, i);
            }
            _il.Emit(OpCodes.Call, m);
            return true;
        }
        if (_leafEntry is not { } f) return false;
        MethodInfo? leaf;
        if (name == "Runtime.Car") leaf = s_leafCar;
        else if (!s_leafOps.TryGetValue(name, out leaf)) return false;
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

    static MethodInfo? _lazy_s_doubleBits;
    static MethodInfo s_doubleBits => _lazy_s_doubleBits ??= typeof(BitConverter).GetMethod("DoubleToInt64Bits", new[] { typeof(double) })!;

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
            TypedEntrySig.BodyParamTypes(f.Sig, f.SelfFree), typeof(CilAssembler).Module, true);
        var innerAsm = new CilAssembler();
        innerAsm._il = typedDm.GetILGenerator().ForLibrary();
        innerAsm._typedSelfMethod = typedDm;
        innerAsm.AssembleTypedBody(f);
        var typedEntryDm = TypedEntryMethod(f, typedDm,
            () => new DynamicMethod(f.Name + "_typedw", TypedEntrySig.ReturnType(f.Sig),
                                    TypedEntrySig.ParamTypes(f.Sig), typeof(CilAssembler).Module, true),
            m => ((DynamicMethod)m).GetILGenerator().ForLibrary());

        var shellTypes = new Type[n + 1];
        shellTypes[0] = typeof(LispFunction);
        for (int i = 0; i < n; i++) shellTypes[i + 1] = typeof(LispObject);
        var shellDm = new DynamicMethod(f.Name + "_shell", typeof(LispObject), shellTypes,
            typeof(CilAssembler).Module, true);
        bool selfFramed = CanSelfFrame(f.Name, n, f.NoFrame);
        bool leaf = selfFramed && f.Generic == null && IsLeafSig(f.Sig) && IsTypedLeaf(f.Body);
        DynamicMethod? genericDm = null;
        if (f.Generic != null)
        {
            genericDm = new DynamicMethod(f.Name + "_generic", typeof(LispObject), GenericBodyParamTypes(n),
                typeof(CilAssembler).Module, true);
            var genAsm = new CilAssembler();
            genAsm._il = genericDm.GetILGenerator().ForLibrary();
            SetupGenericBody(genAsm, f, selfFramed, unit: true);
            genAsm.Assemble(f.Generic);
        }
        DynamicMethod? entryDm = null;
        if (CanTypedFrame(f.Name, n, f.NoFrame))
        {
            entryDm = new DynamicMethod(f.Name + "_tentry", TypedEntrySig.ReturnType(f.Sig),
                TypedEntrySig.ParamTypes(f.Sig), typeof(CilAssembler).Module, true);
            if (leaf) EmitLeafEntry(entryDm.GetILGenerator().ForLibrary(), f);
            else EmitTypedFramedEntry(entryDm.GetILGenerator().ForLibrary(), f, typedDm, unit: true);
        }
        if (leaf)
        {
            var framedDm = new DynamicMethod(f.Name + "_shellf", typeof(LispObject), shellTypes,
                typeof(CilAssembler).Module, true);
            EmitTypedShell(framedDm.GetILGenerator().ForLibrary(), f, typedDm, selfFramed, unit: true);
            EmitLeafShell(shellDm.GetILGenerator().ForLibrary(), f, entryDm!, framedDm);
        }
        else EmitTypedShell(shellDm.GetILGenerator().ForLibrary(), f, typedDm, selfFramed, unit: true, generic: genericDm);

        var fn = BuildSelfDirectFunction(shellDm, f.Name, n);
        if (selfFramed) fn.SetSelfFramedEntry(DynamicMethodEntry(shellDm), n);
        if (leaf) fn.MarkLeafEntry(n);
        fn.SetTypedEntry(((DynamicMethod)typedEntryDm).CreateDelegate(TypedEntrySig.DelegateType(f.Sig)),
            TypedEntrySig.Code(f.Sig));
        if (entryDm != null)
        {
            s_typedEntryOwners.Add(fn, entryDm);
            var r = TypedArgRanges(f);
            fn.SetTypedArgRanges(r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7]);
            fn.SetTypedFramedEntry(DynamicMethodEntry(entryDm), TypedEntrySig.Code(f.Sig));
        }
        FinishInMemoryDefmethod(fn, f.Name, f.Pkg, f.LambdaList, f.NoFrame, f.Body);
    }
}
#endif
