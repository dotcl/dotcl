namespace DotCL.Emitter;

/// <summary>
/// Keeping a single-use temporary on the evaluation stack instead of in an IL
/// local.
///
/// The compiler binds every operand it has to evaluate ahead of its use to a
/// fresh local: an inlined function's parameters ((LET ((A X) (B Y)) ...) of an
/// INLINE expansion), the arguments of a call, the operands of the 64-bit lane.
/// When the local is stored once and loaded once, and every instruction between
/// the store and the load leaves the stack as it found it without reaching
/// below it, the value can simply stay where the store found it: the load would
/// put it back on top at exactly that point. The pair is dropped, which saves
/// an IL local and two local references (RyuJIT stops optimizing a method past
/// 8,000 of those; ironclad's SM3-HASH had 14,562).
///
/// Only straight-line code between the store and the load qualifies, built from
/// instructions whose stack effect is known here (loads, arithmetic,
/// conversions, element reads). A call ends the search too: keeping a value on
/// the stack across a call made the JIT spill and reload it, and measured slower
/// (STREEBOG-G and WHIRLPOOL 4 to 7 percent) than the local it replaced. A
/// label, a branch, an exception block or any other instruction ends it as
/// well, so the stack shape at every join point is what it was.
/// </summary>
public partial class CilAssembler
{
    // How far past a store the matching load is looked for.
    private const int StackTempMaxSpan = 96;

    // A body with fewer top-level local references than this is left alone.
    // It is nowhere near the JIT's limit of 8,000, and in a small method the
    // JIT's own forward substitution keeps a single-use temporary out of memory
    // anyway. Running the pass on every method was a third of the time the
    // assembler spent (half a second of starting from compiler/cil-out.sil).
    private const int StackTempMinRefs = 2000;

    /// <summary>How many values INSTR takes off the stack and how many it
    /// leaves, or null when it is not one this pass may keep a value beneath.
    /// </summary>
    private (int pops, int pushes)? StackTempEffect(LispObject instr)
    {
        if (instr is not Cons c || c.Car is not Symbol s) return null;
        switch (s.Name)
        {
            case "DECLARE-LOCAL":
            case "LINE":
                return (0, 0);
            case "NOT": case "NEG":
            case "CONV-I": case "CONV-I4": case "CONV-I8": case "CONV-U": case "CONV-U1":
            case "CONV-U2": case "CONV-U4": case "CONV-U8": case "CONV-I1": case "CONV-I2":
            case "CONV-R8": case "CONV-R4": case "CONV-R-UN":
            case "CASTCLASS": case "ISINST":
            case "LDLEN":
                return (1, 1);
            case "LDLOC": case "LDARG":
            case "LDC-I8": case "LDC-I4": case "LDC-R8": case "LDC-R4": case "LDNULL":
            case "LOAD-SYM": case "LOAD-SYM-PKG": case "LOAD-SYM-KEYWORD":
                return (0, 1);
            case "DUP":
                return (1, 2);
            case "LOAD-CONST":
                // A list constant may be built by instructions of its own.
                return Cadr(c) is Cons ? null : (0, 1);
            case "STLOC": case "POP":
                return (1, 0);
            case "ADD": case "SUB": case "MUL": case "DIV": case "REM": case "DIV-UN": case "REM-UN":
            case "AND": case "OR": case "XOR": case "SHL": case "SHR": case "SHR-UN":
            case "CEQ": case "CGT": case "CLT": case "CGT-UN": case "CLT-UN":
            case "LDELEM-I8": case "LDELEM-I4": case "LDELEM-U1": case "LDELEM-REF":
            case "LDELEM-R8": case "LDELEM-R4": case "LDELEM-I1": case "LDELEM-I2":
            case "LDELEM-U2": case "LDELEM-U4": case "LDELEM-I":
                return (2, 1);
            default:
                return null;
        }
    }

    /// <summary>INSTRUCTIONS without the store / load pairs of single-use
    /// temporaries that can stay on the stack. Returns INSTRUCTIONS itself when
    /// there are none.</summary>
    private LispObject ElideStackTemps(LispObject instructions)
    {
        if (_seqPoints != null || _localVars != null) return instructions;
        var ins = new List<LispObject>();
        int refs = 0;
        for (var cur = instructions; cur is Cons c; cur = c.Cdr)
        {
            ins.Add(c.Car);
            if (c.Car is Cons ic && ic.Car is Symbol iop && (iop.Name == "LDLOC" || iop.Name == "STLOC"))
                refs++;
        }
        int n = ins.Count;
        if (refs < StackTempMinRefs) return instructions;

        // Every mention of each declared local: the top-level LDLOC / STLOC
        // positions, and whether it is named anywhere else.
        var declared = new HashSet<string>();
        var declAt = new Dictionary<string, int>();
        for (int i = 0; i < n; i++)
            if (ins[i] is Cons dc && dc.Car is Symbol ds && ds.Name == "DECLARE-LOCAL"
                && NameOf(Cadr(dc)) is string dn)
            {
                // Declared twice: leave the declaration alone.
                if (!declared.Add(dn)) declAt[dn] = -1;
                else declAt[dn] = i;
            }
        if (declared.Count == 0) return instructions;

        var loads = new Dictionary<string, int>();
        var stores = new Dictionary<string, int>();
        var spoiled = new HashSet<string>();
        for (int i = 0; i < n; i++)
        {
            if (ins[i] is not Cons c || c.Car is not Symbol s) continue;
            if (s.Name == "DECLARE-LOCAL") continue;
            if ((s.Name == "LDLOC" || s.Name == "STLOC") && c.Cdr is Cons a && a.Cdr is not Cons
                && NameOf(a.Car) is string nm && declared.Contains(nm))
            {
                var d = s.Name == "LDLOC" ? loads : stores;
                if (d.ContainsKey(nm)) spoiled.Add(nm); else d[nm] = i;
                continue;
            }
            // Any other mention, at any depth, spoils the local.
            var seen = new HashSet<Cons>(ReferenceEqualityComparer.Instance);
            var stack = new Stack<LispObject>();
            stack.Push(c.Cdr);
            while (stack.Count > 0)
            {
                var y = stack.Pop();
                if (y is Cons yc)
                {
                    if (!seen.Add(yc)) continue;
                    stack.Push(yc.Cdr);
                    stack.Push(yc.Car);
                }
                else if (NameOf(y) is string m2 && declared.Contains(m2)) spoiled.Add(m2);
            }
        }

        var effect = new (int pops, int pushes)?[n];
        for (int i = 0; i < n; i++) effect[i] = StackTempEffect(ins[i]);
        var removed = new bool[n];

        // Candidates in order of their store. Removing an inner pair can make an
        // outer one qualify (the loads of a parameter list come in a row), so
        // repeat while something changes.
        var cands = new List<(int st, int ld, int decl)>();
        foreach (var (nm, st) in stores)
            if (!spoiled.Contains(nm) && loads.TryGetValue(nm, out var ld) && st < ld
                && ld - st <= StackTempMaxSpan)
                cands.Add((st, ld, declAt[nm]));
        if (cands.Count == 0) return instructions;
        cands.Sort((p, q) => p.st.CompareTo(q.st));

        bool any = false, changed = true;
        for (int pass = 0; changed && pass < 8; pass++)
        {
            changed = false;
            foreach (var (st, ld, decl) in cands)
            {
                if (removed[st]) continue;
                int depth = 0;
                bool ok = true;
                for (int k = st + 1; k < ld && ok; k++)
                {
                    if (removed[k]) continue;
                    var e = effect[k];
                    if (e == null) { ok = false; break; }
                    // Nothing below the kept value may be taken.
                    if (depth < e.Value.pops) { ok = false; break; }
                    depth += e.Value.pushes - e.Value.pops;
                }
                if (ok && depth == 0)
                {
                    removed[st] = removed[ld] = true;
                    // The local is no longer used at all: drop its declaration.
                    if (decl >= 0) removed[decl] = true;
                    changed = any = true;
                }
            }
        }
        if (!any) return instructions;

        LispObject result = Nil.Instance;
        for (int i = n - 1; i >= 0; i--)
            if (!removed[i]) result = new Cons(ins[i], result);
        return result;
    }
}
