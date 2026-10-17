using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

// Self-framed DEFUN bodies and calls through LispFunction.CallN (see
// Function.SelfFramed.cs).
public partial class CilAssembler
{
    internal string? _selfFrameName;
    internal int _selfFrameArgc;
    internal bool _selfFrameUnit;
    // The body method takes the function as argument 0 where the compiler
    // numbered the parameters from 0.
    internal int _argShift;
    private LocalBuilder? _selfFrameLocal;
    private LocalBuilder? _ehMarker;

    private static readonly MethodInfo[] s_enterN =
    {
        typeof(LispFunction).GetMethod("Enter0")!, typeof(LispFunction).GetMethod("Enter1")!,
        typeof(LispFunction).GetMethod("Enter2")!, typeof(LispFunction).GetMethod("Enter3")!,
        typeof(LispFunction).GetMethod("Enter4")!,
    };
    private static readonly MethodInfo s_enterUnit = typeof(LispFunction).GetMethod("EnterUnit")!;
    private static readonly MethodInfo s_leaveFrame = typeof(LispFunction).GetMethod("Leave")!;
    private static readonly MethodInfo s_resumeFrame = typeof(LispFunction).GetMethod("Resume")!;
    private static readonly MethodInfo s_resumeBelow = typeof(LispFunction).GetMethod("ResumeBelow")!;
    private static readonly MethodInfo s_selfTailEntry = typeof(LispFunction).GetMethod("SelfTailEntry")!;
    private static readonly MethodInfo s_selfTailPrepare = typeof(LispFunction).GetMethod("SelfTailPrepare")!;
    private static readonly FieldInfo s_tailLendMarker = typeof(LispFunction).GetField("TailLendMarker")!;

    internal static bool CanSelfFrame(string name, int paramCount, bool noFrame) =>
        !noFrame && paramCount <= 4
        && !name.StartsWith("%MINI-", StringComparison.Ordinal)
        && !name.StartsWith("%CALL-WITH-", StringComparison.Ordinal);

    private static readonly MethodInfo? s_dmDescriptor =
        typeof(DynamicMethod).GetMethod("GetMethodDescriptor", BindingFlags.NonPublic | BindingFlags.Instance);

    /// <summary>Zero when the runtime gives out no address: InvokeN only.</summary>
    internal static IntPtr DynamicMethodEntry(DynamicMethod dm)
    {
        try
        {
            if (s_dmDescriptor?.Invoke(dm, null) is RuntimeMethodHandle h) return h.GetFunctionPointer();
        }
        catch { }
        return IntPtr.Zero;
    }

    private void EmitSelfFrameProlog()
    {
        var il = _il;
        _selfFrameLocal = il.DeclareLocal(typeof(LispFunction.Frame));
        il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal);
        il.Emit(OpCodes.Ldstr, Track(_selfFrameName!));
        for (int i = 1; i <= _selfFrameArgc; i++) EmitRawLdarg(i);
        il.Emit(OpCodes.Call, s_enterN[_selfFrameArgc]);
        if (_selfFrameUnit)
        {
            il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, s_enterUnit);
        }
    }

    private void EmitRawLdarg(int n)
    {
        switch (n)
        {
            case 0: _il.Emit(OpCodes.Ldarg_0); break;
            case 1: _il.Emit(OpCodes.Ldarg_1); break;
            case 2: _il.Emit(OpCodes.Ldarg_2); break;
            case 3: _il.Emit(OpCodes.Ldarg_3); break;
            default: _il.Emit(OpCodes.Ldarg_S, (byte)n); break;
        }
    }

    private void EmitLeaveOwnFrame()
    {
        if (_selfFrameLocal == null) return;
        _il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal);
        _il.Emit(OpCodes.Call, s_leaveFrame);
    }

    /// <summary>At the start of a handler: the frame list as it was in this method.</summary>
    private void EmitResumeFrames()
    {
        if (_selfFrameLocal != null)
        {
            _il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal);
            _il.Emit(OpCodes.Call, s_resumeFrame);
            return;
        }
        _ehMarker ??= _il.DeclareLocal(typeof(byte));
        _il.EmitLoc(OpCodes.Ldloca, _ehMarker);
        _il.Emit(OpCodes.Call, s_resumeBelow);
    }

    private static readonly HashSet<string> s_inertOps = new()
    {
        "LDLOC", "STLOC", "LDARG", "LDC-I4", "LDC-I8", "LDC-R8", "LDC-R4", "LDSFLD", "LDNULL",
        "LOAD-SYM", "LOAD-SYM-PKG", "LOAD-CONST", "CASTCLASS", "POP", "DUP", "LABEL", "LINE",
        "DECLARE-LOCAL", "BR", "BRFALSE", "BRTRUE", "LEAVE", "LOCAL-VAR", "SCOPE-BEGIN", "SCOPE-END",
    };

    private static bool FinallyRunsLisp(LispObject rest)
    {
        int depth = 0;
        for (var cur = rest; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is not Cons ic || ic.Car is not Symbol op) continue;
            var n = op.Name;
            if (n == "BEGIN-EXCEPTION-BLOCK") { depth++; continue; }
            if (n == "END-EXCEPTION-BLOCK") { if (depth-- == 0) return false; continue; }
            if (s_inertOps.Contains(n)) continue;
            if (n == "CALL" && ic.Cdr is Cons a && a.Car is LispString s
                && (s.Value.StartsWith("DynamicBindings.", StringComparison.Ordinal)
                    || s.Value.StartsWith("HandlerClusterStack.", StringComparison.Ordinal)
                    || s.Value.StartsWith("RestartClusterStack.", StringComparison.Ordinal)
                    || s.Value.StartsWith("MultipleValues.", StringComparison.Ordinal)))
                continue;
            return true;
        }
        return false;
    }

    private bool TryEmitEntryCall(string name)
    {
        const string prefix = "LispFunction.Invoke";
        if (_runOnceInMemory || !name.StartsWith(prefix, StringComparison.Ordinal)) return false;
        int rest = name.Length - prefix.Length;
        if (rest < 1 || rest > 2) return false;
        char d = name[prefix.Length];
        if (d < '0' || d > '4') return false;
        bool mode = rest == 2;
        if (mode && name[^1] != 'M') return false;
        _il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod("Call" + d + (mode ? "M" : ""))!);
        return true;
    }

    /// <summary>Tail call from a self-framed body through the callee's entry, or
    /// the lending call. Returns the local holding TailPrepN's frame name for
    /// the rest.</summary>
    private LocalBuilder EmitSelfTail(int n, bool mode, LocalBuilder fn, LocalBuilder?[] args, LocalBuilder? modeLocal)
    {
        var il = _il;
        void LoadArgs() { for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, args[k]!); }
        var noEntry = il.DefineLabel();
        if (n <= 4)
        {
            // Through the delegate rather than calli on the entry, which would not verify.
            il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal!);
            il.EmitLoc(OpCodes.Ldloc, fn);
            il.Emit(OpCodes.Ldc_I4, n);
            if (mode) il.EmitLoc(OpCodes.Ldloc, modeLocal!); else il.Emit(OpCodes.Ldc_I4_M1);
            il.Emit(OpCodes.Call, s_selfTailEntry);
            il.Emit(OpCodes.Brfalse, noEntry);
            il.EmitLoc(OpCodes.Ldloc, fn);
            il.Emit(OpCodes.Call, typeof(LispFunction).GetProperty("TailDirect" + n)!.GetGetMethod()!);
            LoadArgs();
            il.Emit(OpCodes.Tailcall);
            il.Emit(OpCodes.Callvirt, s_tailDirectTypes[n].GetMethod("Invoke")!);
            il.Emit(OpCodes.Ret);
        }
        il.MarkLabel(noEntry);
        var self = il.DeclareLocal(typeof(string));
        var prep = il.DefineLabel();
        il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal!);
        il.EmitLoc(OpCodes.Ldloc, fn);
        il.Emit(OpCodes.Ldstr, Track(_selfFrameName!));
        il.Emit(OpCodes.Call, s_selfTailPrepare);
        il.Emit(OpCodes.Dup);
        il.EmitLoc(OpCodes.Stloc, self);
        il.Emit(OpCodes.Ldsfld, s_tailLendMarker);
        il.Emit(OpCodes.Bne_Un, prep);
        il.EmitLoc(OpCodes.Ldloc, fn);
        LoadArgs();
        if (mode) il.EmitLoc(OpCodes.Ldloc, modeLocal!);
        il.Emit(OpCodes.Callvirt, typeof(LispFunction).GetMethod((n <= 4 ? "InvokeFast" : "Invoke") + n + (mode ? "M" : ""))!);
        EmitLeaveOwnFrame();
        il.Emit(OpCodes.Ret);
        il.MarkLabel(prep);
        return self;
    }

    /// <summary>(:CALL-SAME-FILE PKG NAME N [:MODE]): an InvokeN[M] whose callee
    /// is a DEFUN this fasl compiled earlier. Shape and the function this file
    /// defined, if known.</summary>
    private (int n, bool mode, (MethodInfo Body, int Arity, bool Mv, FieldInfo Fn)? target) SameFileCall(Cons instr)
    {
        var rest = Cdr(Cdr(Cdr(instr)));
        int n = GetInt(Car(rest));
        bool mode = Cdr(rest) is Cons;
        if (_faslStructMap != null
            && _faslStructMap.SameFileBodies.TryGetValue((GetString(Cadr(instr)), GetString(Caddr(instr))), out var b)
            && b.Arity == n)
            return (n, mode, b);
        return (n, mode, null);
    }

    private static LispObject InvokeShape(int n, bool mode) =>
        new Cons(Startup.Keyword("CALLVIRT"),
                 new Cons(new LispString("LispFunction.Invoke" + n + (mode ? "M" : "")), Nil.Instance));

    /// <summary>The function, the arguments and the value mode are on the stack:
    /// enter the body directly while the function is the one this file defined,
    /// else call it through CallN[M].</summary>
    private void EmitCallSameFile(Cons instr)
    {
        var (n, mode, target) = SameFileCall(instr);
        var callN = typeof(LispFunction).GetMethod("Call" + n + (mode ? "M" : ""))!;
        if (target is not { } t)
        {
            _il.Emit(OpCodes.Call, callN);
            return;
        }
        var il = _il;
        LocalBuilder? modeLocal = mode ? il.DeclareLocal(typeof(int)) : null;
        if (mode) il.EmitLoc(OpCodes.Stloc, modeLocal!);
        var args = new LocalBuilder[n];
        for (int k = n - 1; k >= 0; k--) il.EmitLoc(OpCodes.Stloc, args[k] = il.DeclareLocal(typeof(LispObject)));
        var fn = il.DeclareLocal(typeof(LispFunction));
        il.EmitLoc(OpCodes.Stloc, fn);
        var other = il.DefineLabel();
        var done = il.DefineLabel();
        il.EmitLoc(OpCodes.Ldloc, fn);
        il.Emit(OpCodes.Ldsfld, t.Fn);
        il.Emit(OpCodes.Bne_Un, other);
        il.EmitLoc(OpCodes.Ldloc, fn);
        for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, args[k]);
        if (mode && t.Mv)
        {
            il.EmitLoc(OpCodes.Ldloc, modeLocal!);
            il.Emit(OpCodes.Call, s_passMode);
        }
        il.Emit(OpCodes.Call, t.Body);
        il.Emit(OpCodes.Br, done);
        il.MarkLabel(other);
        il.EmitLoc(OpCodes.Ldloc, fn);
        for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, args[k]);
        if (mode) il.EmitLoc(OpCodes.Ldloc, modeLocal!);
        il.Emit(OpCodes.Call, callN);
        il.MarkLabel(done);
    }

    private static readonly MethodInfo s_passMode = typeof(MultipleValues).GetMethod("PassMode")!;
    private static readonly MethodInfo s_sameFileTail = typeof(LispFunction).GetMethod("SameFileTail")!;
    private static readonly MethodInfo s_sameFileTailFrom = typeof(LispFunction).GetMethod("SameFileTailFrom")!;

    /// <summary>A :CALL-SAME-FILE in tail position: the frame this body owns is
    /// handed to the callee, as for a tail call through its entry.</summary>
    private void EmitSameFileTail(Cons instr)
    {
        var (n, mode, target) = SameFileCall(instr);
        if (target is not { } t)
        {
            EmitTailInvoke(InvokeShape(n, mode));
            return;
        }
        var il = _il;
        LocalBuilder? modeLocal = mode ? il.DeclareLocal(typeof(int)) : null;
        if (mode) il.EmitLoc(OpCodes.Stloc, modeLocal!);
        var args = new LocalBuilder[n];
        for (int k = n - 1; k >= 0; k--) il.EmitLoc(OpCodes.Stloc, args[k] = il.DeclareLocal(typeof(LispObject)));
        var fn = il.DeclareLocal(typeof(LispFunction));
        il.EmitLoc(OpCodes.Stloc, fn);
        var other = il.DefineLabel();
        il.EmitLoc(OpCodes.Ldloc, fn);
        il.Emit(OpCodes.Ldsfld, t.Fn);
        il.Emit(OpCodes.Bne_Un, other);
        if (_selfFrameLocal != null)
        {
            il.EmitLoc(OpCodes.Ldloca, _selfFrameLocal);
            il.Emit(OpCodes.Call, s_sameFileTail);
        }
        else
        {
            if (_tailSelf == null) il.Emit(OpCodes.Ldnull);
            else il.Emit(OpCodes.Ldstr, Track(_tailSelf));
            il.Emit(OpCodes.Call, s_sameFileTailFrom);
        }
        if (mode && t.Mv)
        {
            il.EmitLoc(OpCodes.Ldloc, modeLocal!);
            il.Emit(OpCodes.Call, s_passMode);
        }
        il.EmitLoc(OpCodes.Ldloc, fn);
        for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, args[k]);
        il.Emit(OpCodes.Tailcall);
        il.Emit(OpCodes.Call, t.Body);
        il.Emit(OpCodes.Ret);
        il.MarkLabel(other);
        il.EmitLoc(OpCodes.Ldloc, fn);
        for (int k = 0; k < n; k++) il.EmitLoc(OpCodes.Ldloc, args[k]);
        if (mode) il.EmitLoc(OpCodes.Ldloc, modeLocal!);
        EmitTailInvoke(InvokeShape(n, mode));
    }
}
