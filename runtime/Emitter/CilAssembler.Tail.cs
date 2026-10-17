using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Calls in tail position. A function body the compiler marks with
/// (:tail-self NAME) has every call whose value goes straight to the method's
/// RET made as a tail call: the call site asks LispFunction.TailPrep how to
/// make it, then calls the callee's body delegate with the IL tail. prefix
/// (or InvokeN, when the callee needs a debugger frame the caller cannot give
/// it). See the tail-call section of Function.cs for the frame handling.
///
/// The positions are found here, on the instruction list, rather than in the
/// compiler: a call is in tail position exactly when nothing but labels,
/// branches and a store to a local that is loaded right back lies between it
/// and a RET, outside every protected region. (A LEAVE outside every region is
/// a branch; one that leaves a region crosses its boundary and so ends the
/// search.) That
/// excludes by construction the calls the compiler's notion of tail position
/// does not, such as the argument of THROW or a call under a special binding,
/// UNWIND-PROTECT or HANDLER-CASE, all of which have code or a region boundary
/// after the call.
/// </summary>
public partial class CilAssembler
{
    // The call instructions (the list cells holding them) to emit as tail calls,
    // and the frame name the body runs under (null: one it may not reuse).
    private HashSet<Cons>? _tailSites;
    private string? _tailSelf;
    private LocalBuilder? _tailFn, _tailMode;
    private LocalBuilder?[]? _tailArgs;

    /// <summary>Arity and value-mode flag of a LispFunction.InvokeN / InvokeNM
    /// call, or (-1, false) for anything else.</summary>
    private static (int arity, bool mode) TailInvokeShape(LispObject instr)
    {
        if (instr is not Cons c || c.Car is not Symbol op || op.Name != "CALLVIRT"
            || c.Cdr is not Cons a || a.Car is not LispString s) return (-1, false);
        var name = s.Value;
        const string prefix = "LispFunction.Invoke";
        if (!name.StartsWith(prefix, StringComparison.Ordinal)) return (-1, false);
        int rest = name.Length - prefix.Length;
        if (rest < 1 || rest > 2 || !char.IsDigit(name[prefix.Length])) return (-1, false);
        int n = name[prefix.Length] - '0';
        if (n > 8) return (-1, false);
        if (rest == 2)
            return name[^1] == 'M' && typeof(LispFunction).GetMethod("Invoke" + n + "M") != null
                ? (n, true) : (-1, false);
        return (n, false);
    }

    /// <summary>Find the tail calls of INSTRUCTIONS when it is a function body the
    /// compiler marked with (:tail-self ...).</summary>
    private void FindTailSites(LispObject instructions)
    {
        bool marked = false;
        string? self = null;
        for (var cur = instructions; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is Cons ic && ic.Car is Symbol s && s.Name == "TAIL-SELF")
            {
                marked = true;
                self = Cadr(ic) is LispString ls ? ls.Value : LispFunction.TailAnonName;
                break;
            }
        }
        if (!marked) return;
        _tailSelf = self;
        var cells = new List<Cons>();
        for (var cur = instructions; cur is Cons c; cur = c.Cdr) cells.Add(c);

        var labelAt = new Dictionary<string, int>();
        var depth = new int[cells.Count];
        int d = 0;
        for (int i = 0; i < cells.Count; i++)
        {
            if (cells[i].Car is Cons ic && ic.Car is Symbol s)
            {
                if (s.Name == "LABEL") labelAt[GetSymbolName(Cadr(ic))] = i;
                else if (s.Name == "BEGIN-EXCEPTION-BLOCK") d++;
                else if (s.Name == "END-EXCEPTION-BLOCK") d--;
            }
            depth[i] = d;
        }

        for (int i = 0; i < cells.Count; i++)
        {
            if (depth[i] != 0 || (TailInvokeShape(cells[i].Car).arity < 0
                                  && !(cells[i].Car is Cons sc && sc.Car is Symbol ss && ss.Name == "CALL-SAME-FILE")))
                continue;
            // Follow the value to the RET. It may pass through one local on the
            // way (STLOC v ... LDLOC v), which is how a BLOCK, an inlined call
            // and a LET body hand their value on.
            int j = i + 1, steps = 0;
            string? heldIn = null;
            bool reachesRet = false;
            while (j < cells.Count && steps++ < 64)
            {
                if (cells[j].Car is not Cons jc || jc.Car is not Symbol js) break;
                var n = js.Name;
                if (n == "LABEL" || n == "LINE" || n == "DECLARE-LOCAL") { j++; continue; }
                // LEAVE outside every protected region is a branch that empties
                // the stack, so the value has to be in the local by then.
                if ((n == "BR" || (n == "LEAVE" && heldIn != null && depth[j] == 0))
                    && labelAt.TryGetValue(GetSymbolName(Cadr(jc)), out var to)) { j = to; continue; }
                if (heldIn == null)
                {
                    if (n == "RET") { reachesRet = depth[j] == 0; break; }
                    if (n == "STLOC") { heldIn = GetSymbolName(Cadr(jc)); j++; continue; }
                }
                else if (n == "LDLOC" && GetSymbolName(Cadr(jc)) == heldIn) { heldIn = null; j++; continue; }
                break;
            }
            if (reachesRet) (_tailSites ??= new HashSet<Cons>(ReferenceEqualityComparer.Instance)).Add(cells[i]);
        }
    }

    /// <summary>In the body of a typed entry marked with (:typed-tail-self): the
    /// self-calls (:call-typed-self) whose result is the body's result, to be
    /// made as tail calls to the body method itself. The result may reach the RET
    /// raw, or boxed and converted back on the way (the box, the TypedRet
    /// conversion and a range check of it are then skipped: the callee has
    /// already produced a value of the declared type). The path is followed the
    /// way FindTailSites follows it.</summary>
    private void FindTypedTailSites(LispObject instructions)
    {
        bool marked = false;
        for (var cur = instructions; cur is Cons c; cur = c.Cdr)
            if (c.Car is Cons ic && ic.Car is Symbol s && s.Name == "TYPED-TAIL-SELF")
            {
                marked = true;
                // The frame name the body runs under, for TailPrepTypedN (an
                // older compiler wrote none: no frame is then taken as its own).
                _typedTailSelf = Cdr(ic) is Cons nc && nc.Car is LispString ls ? ls.Value
                    : Cdr(ic) is Cons nn && nn.Car is Nil ? LispFunction.TailAnonName : null;
                break;
            }
        if (!marked) return;
        var cells = new List<Cons>();
        for (var cur = instructions; cur is Cons c; cur = c.Cdr) cells.Add(c);
        var labelAt = new Dictionary<string, int>();
        var depth = new int[cells.Count];
        int d = 0;
        for (int i = 0; i < cells.Count; i++)
        {
            if (cells[i].Car is Cons ic && ic.Car is Symbol s)
            {
                if (s.Name == "LABEL") labelAt[GetSymbolName(Cadr(ic))] = i;
                else if (s.Name == "BEGIN-EXCEPTION-BLOCK") d++;
                else if (s.Name == "END-EXCEPTION-BLOCK") d--;
            }
            depth[i] = d;
        }
        static bool IsCall(Cons ic, string op, string target) =>
            ic.Car is Symbol s && s.Name == op && ic.Cdr is Cons a && a.Car is LispString ls && ls.Value == target;
        for (int i = 0; i < cells.Count; i++)
        {
            if (depth[i] != 0 || cells[i].Car is not Cons ci || ci.Car is not Symbol cs
                || (cs.Name != "CALL-TYPED-SELF" && cs.Name != "CALL-TYPED-ENTRY")) continue;
            // A call to another function's typed entry is a tail call only when
            // its raw result is of the kind this body returns.
            if (cs.Name == "CALL-TYPED-ENTRY")
            {
                var sig = GetString(Cadr(ci));
                if (TypedEntrySig.KindType(sig[^1]) != _typedSelfMethod!.ReturnType) continue;
            }
            int j = i + 1, steps = 0;
            bool boxed = false;
            if (j < cells.Count && cells[j].Car is Cons bc
                && (IsCall(bc, "CALL", "Fixnum.Make") || IsCall(bc, "NEWOBJ", "DoubleFloat")))
            { boxed = true; j++; }
            string? heldIn = null;
            bool reachesRet = false;
            while (j < cells.Count && steps++ < 64)
            {
                if (cells[j].Car is not Cons jc || jc.Car is not Symbol js) break;
                var n = js.Name;
                if (n == "LABEL" || n == "LINE" || n == "DECLARE-LOCAL") { j++; continue; }
                if ((n == "BR" || (n == "LEAVE" && heldIn != null && depth[j] == 0))
                    && labelAt.TryGetValue(GetSymbolName(Cadr(jc)), out var to)) { j = to; continue; }
                if (heldIn == null)
                {
                    if (boxed && (IsCall(jc, "CALL", "Runtime.TypedRetL") || IsCall(jc, "CALL", "Runtime.TypedRetD")))
                    {
                        boxed = false; j++;
                        // A range check of the converted value.
                        if (j + 2 < cells.Count && cells[j].Car is Cons l1 && l1.Car is Symbol s1 && s1.Name == "LDC-I8"
                            && cells[j + 1].Car is Cons l2 && l2.Car is Symbol s2 && s2.Name == "LDC-I8"
                            && cells[j + 2].Car is Cons l3 && IsCall(l3, "CALL", "Runtime.CheckLongRange"))
                            j += 3;
                        continue;
                    }
                    if (n == "RET") { reachesRet = !boxed && depth[j] == 0; break; }
                    if (n == "STLOC") { heldIn = GetSymbolName(Cadr(jc)); j++; continue; }
                }
                else if (n == "LDLOC" && GetSymbolName(Cadr(jc)) == heldIn) { heldIn = null; j++; continue; }
                break;
            }
            if (reachesRet) (_typedTailSites ??= new HashSet<Cons>(ReferenceEqualityComparer.Instance)).Add(cells[i]);
        }
    }

    private HashSet<Cons>? _typedTailSites;

    private static readonly Type[] s_tailDirectTypes =
    {
        typeof(Func<LispObject>),
        typeof(Func<LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
        typeof(Func<LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject, LispObject>),
    };

    /// <summary>Emit the tail call for a site FindTailSites chose. The callee and
    /// its arguments (and the value mode, for InvokeNM) are on the stack.</summary>
    private void EmitTailInvoke(LispObject instr)
    {
        if (instr is Cons sc && sc.Car is Symbol ss && ss.Name == "CALL-SAME-FILE")
        {
            EmitSameFileTail(sc);
            return;
        }
        var (n, mode) = TailInvokeShape(instr);
        var il = _il;
        _tailFn ??= il.DeclareLocal(typeof(LispFunction));
        _tailArgs ??= new LocalBuilder?[8];
        if (mode) { _tailMode ??= il.DeclareLocal(typeof(int)); il.EmitLoc(OpCodes.Stloc, _tailMode); }
        for (int k = n - 1; k >= 0; k--)
            il.EmitLoc(OpCodes.Stloc, _tailArgs[k] ??= il.DeclareLocal(typeof(LispObject)));
        il.EmitLoc(OpCodes.Stloc, _tailFn);

        void LoadArgs() { for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, _tailArgs[k]!); }

        var selfLocal = _selfFrameLocal != null ? EmitSelfTail(n, mode, _tailFn, _tailArgs, _tailMode) : null;

        // int kind = LispFunction.TailPrepN(fn, mode, self, a0, ...), or for more
        // than four arguments TailPrep8(fn, n, mode, self, a0, ..., a7)
        il.EmitLoc(OpCodes.Ldloc, _tailFn);
        if (n > 4) il.Emit(OpCodes.Ldc_I4, n);
        if (mode) il.EmitLoc(OpCodes.Ldloc, _tailMode!); else il.Emit(OpCodes.Ldc_I4_M1);
        if (selfLocal != null) il.EmitLoc(OpCodes.Ldloc, selfLocal);
        else if (_tailSelf == null) il.Emit(OpCodes.Ldnull);
        else il.Emit(OpCodes.Ldstr, Track(_tailSelf));
        int slots = n <= 4 ? n : 8;
        for (int k = 0; k < slots; k++)
        {
            if (k < n) il.EmitLoc(OpCodes.Ldloc, _tailArgs[k]!);
            else il.Emit(OpCodes.Ldnull);
        }
        il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod(n <= 4 ? "TailPrep" + n : "TailPrep8")!);

        var direct = il.DefineLabel();
        var closure = il.DefineLabel();
        // TailKindInvoke = 0 falls through, TailKindDirect = 1, TailKindClosure = 2
        il.Emit(OpCodes.Ldc_I4_1);
        il.Emit(OpCodes.Sub);
        il.Emit(OpCodes.Switch, n <= 6 ? new[] { direct, closure } : new[] { direct });

        // TailKindInvoke: the ordinary entry, still as a tail call.
        il.EmitLoc(OpCodes.Ldloc, _tailFn);
        LoadArgs();
        if (mode) il.EmitLoc(OpCodes.Ldloc, _tailMode!);
        il.Emit(OpCodes.Tailcall);
        il.Emit(OpCodes.Callvirt, typeof(LispFunction).GetMethod("Invoke" + n + (mode ? "M" : ""))!);
        il.Emit(OpCodes.Ret);

        il.MarkLabel(direct);
        il.EmitLoc(OpCodes.Ldloc, _tailFn);
        il.Emit(OpCodes.Call, typeof(LispFunction).GetProperty("TailDirect" + n)!.GetGetMethod()!);
        LoadArgs();
        il.Emit(OpCodes.Tailcall);
        il.Emit(OpCodes.Callvirt, s_tailDirectTypes[n].GetMethod("Invoke")!);
        il.Emit(OpCodes.Ret);

        if (n > 6) return;   // no direct closure takes more than 6 arguments
        il.MarkLabel(closure);
        il.EmitLoc(OpCodes.Ldloc, _tailFn);
        il.Emit(OpCodes.Call, typeof(LispFunction).GetProperty("TailClosure" + n)!.GetGetMethod()!);
        il.EmitLoc(OpCodes.Ldloc, _tailFn);
        il.Emit(OpCodes.Call, typeof(LispFunction).GetProperty("Environment")!.GetGetMethod()!);
        LoadArgs();
        il.Emit(OpCodes.Tailcall);
        il.Emit(OpCodes.Callvirt, DirectClosureDelegateType(n).GetMethod("Invoke")!);
        il.Emit(OpCodes.Ret);
    }

    private string? _typedTailSelf;

    /// <summary>Emit the tail call for a (:call-typed-entry SIG) site
    /// FindTypedTailSites chose. The callee and its raw arguments are on the
    /// stack.</summary>
    private void EmitTypedTailInvoke(string sig)
    {
        var il = _il;
        int n = sig.Length - 2;
        int code = TypedEntrySig.Code(sig);
        var fnLocal = il.DeclareLocal(typeof(LispFunction));
        var args = new LocalBuilder[n];
        for (int k = 0; k < n; k++) args[k] = il.DeclareLocal(TypedEntrySig.KindType(sig[k]));
        for (int k = n - 1; k >= 0; k--) il.EmitLoc(OpCodes.Stloc, args[k]);
        il.EmitLoc(OpCodes.Stloc, fnLocal);
        void LoadArgs() { for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, args[k]); }

        var argTypes = new Type[n];
        for (int k = 0; k < n; k++) argTypes[k] = TypedEntrySig.KindType(sig[k]);
        // kind = TailPrepTypedN(fn, code, self, a0, ...)
        il.EmitLoc(OpCodes.Ldloc, fnLocal);
        il.Emit(OpCodes.Ldc_I4, code);
        if (_typedTailSelf == null) il.Emit(OpCodes.Ldnull);
        else il.Emit(OpCodes.Ldstr, Track(_typedTailSelf));
        LoadArgs();
        il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod("TailPrepTyped" + n)!.MakeGenericMethod(argTypes));
        var invoke = il.DefineLabel();
        il.Emit(OpCodes.Brfalse, invoke);   // TailKindInvoke = 0

        // TailKindDirect: the callee's typed body, as a tail call.
        var delType = TypedEntrySig.DelegateType(sig);
        il.EmitLoc(OpCodes.Ldloc, fnLocal);
        il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod("TailTypedDelegate")!.MakeGenericMethod(delType));
        il.EmitLoc(OpCodes.Ldloc, fnLocal);
        LoadArgs();
        il.Emit(OpCodes.Tailcall);
        il.Emit(OpCodes.Callvirt, delType.GetMethod("Invoke")!);
        il.Emit(OpCodes.Ret);

        // TailKindInvoke: through InvokeTypedN, still as a tail call.
        il.MarkLabel(invoke);
        il.EmitLoc(OpCodes.Ldloc, fnLocal);
        LoadArgs();
        il.Emit(OpCodes.Ldc_I4, code);
        il.Emit(OpCodes.Tailcall);
        il.Emit(OpCodes.Call, LispFunction.TypedInvokeMethod(sig));
        il.Emit(OpCodes.Ret);
    }
}
