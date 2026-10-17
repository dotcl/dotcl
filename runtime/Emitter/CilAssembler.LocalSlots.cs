using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Sharing IL local slots between locals whose lifetimes do not overlap.
///
/// The compiler declares a fresh local for every binding and temporary, and
/// never reuses one, so an unrolled body (ironclad's digests and ciphers) ends
/// up with thousands of IL locals. RyuJIT stops optimizing a method with 2,000
/// or more locals, tracks only the first few hundred for register allocation,
/// and a local numbered 256 or more costs a 4-byte ldloc / stloc instead of 2.
///
/// Before a method body is assembled, this computes where each local is live
/// (backward dataflow over the body's basic blocks) and gives two locals of the
/// same type one slot when their live ranges, widened to a single interval of
/// instruction positions, do not overlap. A local whose value is read before it
/// is written on some path is live from the entry and so is the first one in its
/// slot: it still sees the zero the method starts with.
///
/// Only locals that the body touches with plain top-level LDLOC / STLOC share.
/// A local named anywhere else (an LDLOCA, a debug marker, an operand of a
/// composite instruction, a nested instruction list) keeps a slot of its own.
/// Bodies with exception blocks are left alone, as are bodies built with
/// debug information.
/// </summary>
public partial class CilAssembler
{
    // Fewer locals than this and sharing buys nothing: every local still has a
    // one-byte LDLOC.S / STLOC.S, and the JIT tracks the first 1,024 for
    // register allocation. Planning the methods of 64 to 255 locals was most of
    // what this pass cost.
    private const int LocalSlotShareMinLocals = 256;

    private int _assembleDepth;
    private bool _localSlotsPlanned;
    // local name -> slot number, for the locals that share; and each slot's
    // LocalBuilder, made at the first DECLARE-LOCAL of any of its locals.
    private Dictionary<string, int>? _localSlotOf;
    private LocalBuilder?[]? _localSlotBuilders;

    private bool TryDeclareSharedLocal(string name, Type type)
    {
        if (_localSlotOf == null || !_localSlotOf.TryGetValue(name, out var slot)) return false;
        var b = _localSlotBuilders![slot];
        if (b == null)
        {
            b = _il.DeclareLocal(type);
            _localSlotBuilders[slot] = b;
        }
        else if (b.LocalType != type) return false;
        _locals[name] = b;
        return true;
    }

    private static bool IsOp(LispObject x, out string name)
    {
        if (x is Cons c && c.Car is Symbol s) { name = s.Name; return true; }
        name = "";
        return false;
    }

    private static string? NameOf(LispObject x) => x switch
    {
        Symbol s => s.Name,
        LispString ls => ls.Value,
        _ => null,
    };

    private static bool IsExceptionOp(string op) =>
        op == "BEGIN-EXCEPTION-BLOCK" || op == "BEGIN-CATCH-BLOCK" || op == "BEGIN-FILTER-BLOCK"
        || op == "BEGIN-FILTER-HANDLER" || op == "BEGIN-FINALLY-BLOCK" || op == "BEGIN-FAULT-BLOCK"
        || op == "END-EXCEPTION-BLOCK";

    private static bool IsBranchOp(string op) =>
        op == "BR" || op == "BRFALSE" || op == "BRTRUE" || op == "BEQ" || op == "BGT" || op == "BLT"
        || op == "BGE" || op == "BLE" || op == "BNE" || op == "BNE-UN" || op == "BGE-UN" || op == "BGT-UN"
        || op == "BLE-UN" || op == "BLT-UN" || op == "LEAVE";

    private static bool EndsFlow(string op) =>
        op == "BR" || op == "LEAVE" || op == "RET" || op == "THROW" || op == "RETHROW";

    private void PlanLocalSlots(LispObject instructions)
    {
        if (_seqPoints != null || _localVars != null) return;

        var ins = new List<LispObject>();
        int topDeclares = 0;
        for (var cur = instructions; cur is Cons c; cur = c.Cdr)
        {
            ins.Add(c.Car);
            if (IsOp(c.Car, out var op0) && op0 == "DECLARE-LOCAL") topDeclares++;
        }
        int n = ins.Count;
        // Too few locals to share, decided before pass 1 walks every operand.
        if (topDeclares < LocalSlotShareMinLocals) return;

        // Pass 1: top-level declarations, label positions, bail-outs.
        var declType = new Dictionary<string, string>();
        var labelPos = new Dictionary<string, int>();
        var deepDeclared = new HashSet<string>();
        bool bail = false;

        void ScanDeep(LispObject x, int pos)
        {
            // Walk an operand tree for nested instructions: labels and locals
            // they define, and exception blocks. Every cons is visited once
            // (constants can be circular or shared) and each one is looked at
            // as the head of a list.
            var seen = new HashSet<Cons>(ReferenceEqualityComparer.Instance);
            var stack = new Stack<LispObject>();
            stack.Push(x);
            while (stack.Count > 0 && !bail)
            {
                if (stack.Pop() is not Cons yc || !seen.Add(yc)) continue;
                if (yc.Car is Symbol s)
                {
                    if (s.Name == "LABEL" && yc.Cdr is Cons lc && NameOf(lc.Car) is string ln)
                        labelPos.TryAdd(ln, pos);
                    else if (s.Name == "DECLARE-LOCAL" && yc.Cdr is Cons dc && NameOf(dc.Car) is string dn)
                        deepDeclared.Add(dn);
                    else if (IsExceptionOp(s.Name)) bail = true;
                }
                stack.Push(yc.Cdr);
                stack.Push(yc.Car);
            }
        }

        for (int i = 0; i < n && !bail; i++)
        {
            if (!IsOp(ins[i], out var op)) continue;
            var c = (Cons)ins[i];
            if (op == "LABEL")
            {
                if (NameOf(Cadr(c)) is string ln) labelPos[ln] = i;
            }
            else if (op == "DECLARE-LOCAL")
            {
                if (NameOf(Cadr(c)) is string dn && Caddr(c) is LispObject tn)
                    declType.TryAdd(dn, GetString(tn));
            }
            else if (IsExceptionOp(op)) bail = true;
            else ScanDeep(c.Cdr, i);
        }
        if (bail || declType.Count < LocalSlotShareMinLocals) return;

        // Pass 2: classify every mention. A local mentioned other than by a
        // top-level LDLOC / STLOC is pinned.
        var pinned = new HashSet<string>(deepDeclared);
        var refs = new List<(int pos, string name, bool def)>();
        var targets = new List<string>?[n];
        var hasNested = new bool[n];

        void ScanMentions(LispObject x, int pos)
        {
            var seen = new HashSet<Cons>(ReferenceEqualityComparer.Instance);
            var stack = new Stack<LispObject>();
            stack.Push(x);
            while (stack.Count > 0)
            {
                var y = stack.Pop();
                if (y is Cons yc)
                {
                    if (!seen.Add(yc)) continue;
                    if (yc.Car is Symbol ks && ks.HomePackage?.Name == "KEYWORD") hasNested[pos] = true;
                    stack.Push(yc.Cdr);
                    stack.Push(yc.Car);
                    continue;
                }
                var nm = NameOf(y);
                if (nm == null) continue;
                if (declType.ContainsKey(nm)) pinned.Add(nm);
                if (labelPos.ContainsKey(nm)) (targets[pos] ??= new List<string>()).Add(nm);
            }
        }

        for (int i = 0; i < n; i++)
        {
            if (!IsOp(ins[i], out var op)) continue;
            var c = (Cons)ins[i];
            if (op == "LABEL" || op == "DECLARE-LOCAL") continue;
            if (IsBranchOp(op) && !(NameOf(Cadr(c)) is string bl && labelPos.ContainsKey(bl)))
                return;   // a jump to a label this pass did not find: no plan
            if (op == "SWITCH")
                for (LispObject z = Cadr(c); z is Cons zc; z = zc.Cdr)
                    if (!(NameOf(zc.Car) is string sl && labelPos.ContainsKey(sl))) return;
            if ((op == "LDLOC" || op == "STLOC") && c.Cdr is Cons a && a.Cdr is not Cons
                && NameOf(a.Car) is string ln && declType.ContainsKey(ln))
            {
                refs.Add((i, ln, op == "STLOC"));
                continue;
            }
            // Operand symbols of the instruction itself (not nested lists) can
            // be a label (a branch) or a local; nested lists count as nested code.
            for (LispObject z = c.Cdr; z is Cons zc; z = zc.Cdr)
            {
                if (zc.Car is Cons inner) ScanMentions(inner, i);
                else
                {
                    var nm = NameOf(zc.Car);
                    if (nm == null) continue;
                    if (declType.ContainsKey(nm)) pinned.Add(nm);
                    if (labelPos.ContainsKey(nm)) (targets[i] ??= new List<string>()).Add(nm);
                }
            }
        }

        // The locals that share, numbered 0..k-1, grouped by type.
        var idx = new Dictionary<string, int>();
        var names = new List<string>();
        var types = new List<Type>();
        foreach (var (nm, tn) in declType)
        {
            if (pinned.Contains(nm)) continue;
            Type t;
            try { t = ResolveType(tn); } catch { continue; }
            idx[nm] = names.Count;
            names.Add(nm);
            types.Add(t);
        }
        int k = names.Count;
        if (k < LocalSlotShareMinLocals) return;

        // Basic blocks. A block starts at 0, at a label, at an instruction that
        // holds nested code, and after any instruction that can transfer control.
        var leader = new bool[n + 1];
        leader[0] = true;
        leader[n] = true;
        foreach (var p in labelPos.Values) leader[p] = true;
        for (int i = 0; i < n; i++)
        {
            bool ends = targets[i] != null || hasNested[i]
                        || (IsOp(ins[i], out var op) && EndsFlow(op));
            if (hasNested[i]) leader[i] = true;
            if (ends) leader[i + 1] = true;
        }
        var blockStart = new List<int>();
        var blockOf = new int[n];
        for (int i = 0; i < n; i++)
        {
            if (leader[i]) blockStart.Add(i);
            blockOf[i] = blockStart.Count - 1;
        }
        int nb = blockStart.Count;
        int BlockEnd(int b) => (b + 1 < nb ? blockStart[b + 1] : n) - 1;

        var succ = new List<int>[nb];
        for (int b = 0; b < nb; b++)
        {
            var s = new List<int>();
            int last = BlockEnd(b);
            bool fall = !(IsOp(ins[last], out var op) && EndsFlow(op));
            if (fall && b + 1 < nb) s.Add(b + 1);
            if (targets[last] != null)
                foreach (var t in targets[last]!) s.Add(blockOf[labelPos[t]]);
            // Nested code jumps only to labels it names (they are in TARGETS);
            // a label defined inside it is placed at the instruction itself.
            succ[b] = s;
        }

        int words = (k + 63) >> 6;
        var use = new ulong[nb, words];
        var def = new ulong[nb, words];
        var start = new int[k];
        var end = new int[k];
        Array.Fill(start, int.MaxValue);
        Array.Fill(end, -1);
        foreach (var (pos, nm, isDef) in refs)
        {
            int v = idx.TryGetValue(nm, out var vi) ? vi : -1;
            if (v < 0) continue;
            int b = blockOf[pos];
            ulong bit = 1UL << (v & 63);
            int w = v >> 6;
            if (isDef) def[b, w] |= bit;
            else if ((def[b, w] & bit) == 0) use[b, w] |= bit;
            if (pos < start[v]) start[v] = pos;
            if (pos > end[v]) end[v] = pos;
        }

        var liveIn = new ulong[nb, words];
        var liveOut = new ulong[nb, words];
        bool changed = true;
        while (changed)
        {
            changed = false;
            for (int b = nb - 1; b >= 0; b--)
            {
                for (int w = 0; w < words; w++)
                {
                    ulong o = 0;
                    foreach (var s in succ[b]) o |= liveIn[s, w];
                    ulong li = use[b, w] | (o & ~def[b, w]);
                    if (o != liveOut[b, w]) { liveOut[b, w] = o; changed = true; }
                    if (li != liveIn[b, w]) { liveIn[b, w] = li; changed = true; }
                }
            }
        }

        for (int b = 0; b < nb; b++)
        {
            int bs = blockStart[b], be = BlockEnd(b);
            for (int w = 0; w < words; w++)
            {
                for (ulong li = liveIn[b, w]; li != 0; li &= li - 1)
                {
                    int v = (w << 6) + System.Numerics.BitOperations.TrailingZeroCount(li);
                    if (bs < start[v]) start[v] = bs;
                    if (bs > end[v]) end[v] = bs;
                }
                for (ulong lo = liveOut[b, w]; lo != 0; lo &= lo - 1)
                {
                    int v = (w << 6) + System.Numerics.BitOperations.TrailingZeroCount(lo);
                    if (be < start[v]) start[v] = be;
                    if (be > end[v]) end[v] = be;
                }
            }
        }

        // Interval colouring per type, in order of start: a slot is free for a
        // local when the slot's last occupant ended before the local starts.
        var order = Enumerable.Range(0, k).OrderBy(v => start[v]).ThenBy(v => v).ToArray();
        var slotOf = new Dictionary<string, int>();
        var free = new Dictionary<Type, PriorityQueue<int, int>>();  // slot by end
        var anySlot = new Dictionary<Type, int>();
        int nslots = 0;
        foreach (var v in order)
        {
            var t = types[v];
            if (start[v] == int.MaxValue)
            {
                // Never touched: any slot of its type will do.
                if (!anySlot.TryGetValue(t, out var a)) { a = nslots++; anySlot[t] = a; }
                slotOf[names[v]] = a;
                continue;
            }
            if (!free.TryGetValue(t, out var q)) { q = new PriorityQueue<int, int>(); free[t] = q; }
            int slot;
            if (q.TryPeek(out var s0, out var e0) && e0 < start[v]) { q.Dequeue(); slot = s0; }
            else slot = nslots++;
            q.Enqueue(slot, end[v]);
            anySlot.TryAdd(t, slot);
            slotOf[names[v]] = slot;
        }
        if (nslots >= k) return;   // nothing shared
        _localSlotOf = slotOf;
        _localSlotBuilders = new LocalBuilder?[nslots];
    }
}
