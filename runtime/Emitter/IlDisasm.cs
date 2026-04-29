using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Disassembles a .NET method's CIL byte stream into a dotcl-SIL-shaped
/// S-expression list. The output uses the same instruction naming as the
/// internal compiler (keyword symbols like :LDARG, :ADD, :RET) so that
/// `(disassemble 'fn)` output and `(dotcl-cs:disassemble-cs ...)` output
/// can be diffed directly.
///
/// Primary use case: comparing C# compiler output against dotcl's own codegen
/// to drive optimizer work (#122 Phase 1).
/// </summary>
public static class IlDisasm
{
    private static readonly Dictionary<ushort, OpCode> _opCodes = BuildOpCodeTable();

    private static Dictionary<ushort, OpCode> BuildOpCodeTable()
    {
        var d = new Dictionary<ushort, OpCode>();
        foreach (var fi in typeof(OpCodes).GetFields(BindingFlags.Public | BindingFlags.Static))
        {
            if (fi.FieldType == typeof(OpCode))
            {
                var oc = (OpCode)fi.GetValue(null)!;
                d[(ushort)oc.Value] = oc;
            }
        }
        return d;
    }

    /// <summary>
    /// Disassemble a method body to a list of S-expressions shaped like
    /// ((:OP args...) ...) with `(:LABEL IL_xxxx)` pseudo-instructions at
    /// branch target offsets.
    /// </summary>
    public static LispObject DisassembleMethod(MethodBase methodInfo)
    {
        var body = methodInfo.GetMethodBody()
            ?? throw new InvalidOperationException(
                $"IlDisasm: method {methodInfo.Name} has no body (abstract or native)");
        byte[]? il = body.GetILAsByteArray();
        if (il == null || il.Length == 0)
            return Nil.Instance;
        var module = methodInfo.Module;

        // Pass 1: scan branch targets to decide which offsets need labels.
        var labelOffsets = CollectBranchTargets(il);

        // Pass 2: walk and emit instructions, interleaving labels.
        var sexps = new List<LispObject>();
        int pc = 0;
        while (pc < il.Length)
        {
            if (labelOffsets.TryGetValue(pc, out var labelName))
                sexps.Add(MakeInstr("LABEL", Startup.Keyword(labelName)));

            int startPc = pc;
            int opVal = il[pc++];
            if (opVal == 0xFE && pc < il.Length)
                opVal = (0xFE << 8) | il[pc++];
            if (!_opCodes.TryGetValue((ushort)opVal, out var op))
            {
                sexps.Add(MakeInstr("UNKNOWN", new Fixnum(opVal)));
                continue;
            }

            var emitted = EmitInstr(op, il, ref pc, module, labelOffsets);
            sexps.Add(emitted);
        }
        return ListOf(sexps);
    }

    private static Dictionary<int, string> CollectBranchTargets(byte[] il)
    {
        var targets = new SortedSet<int>();
        int pc = 0;
        while (pc < il.Length)
        {
            int opVal = il[pc++];
            if (opVal == 0xFE && pc < il.Length)
                opVal = (0xFE << 8) | il[pc++];
            if (!_opCodes.TryGetValue((ushort)opVal, out var op))
                return new Dictionary<int, string>();   // malformed; skip labeling
            switch (op.OperandType)
            {
                case OperandType.ShortInlineBrTarget:
                {
                    sbyte off = (sbyte)il[pc++];
                    targets.Add(pc + off);
                    break;
                }
                case OperandType.InlineBrTarget:
                {
                    int off = BitConverter.ToInt32(il, pc);
                    pc += 4;
                    targets.Add(pc + off);
                    break;
                }
                case OperandType.InlineSwitch:
                {
                    int count = BitConverter.ToInt32(il, pc);
                    pc += 4;
                    int baseAddr = pc + count * 4;
                    for (int i = 0; i < count; i++)
                    {
                        int off = BitConverter.ToInt32(il, pc);
                        pc += 4;
                        targets.Add(baseAddr + off);
                    }
                    break;
                }
                default:
                    pc += OperandSize(op.OperandType);
                    break;
            }
        }
        var result = new Dictionary<int, string>();
        foreach (var t in targets)
            result[t] = $"IL_{t:X4}";
        return result;
    }

    private static int OperandSize(OperandType t) => t switch
    {
        OperandType.InlineNone => 0,
        OperandType.ShortInlineI => 1,
        OperandType.ShortInlineVar => 1,
        OperandType.ShortInlineBrTarget => 1,
        OperandType.InlineVar => 2,
        OperandType.ShortInlineR => 4,
        OperandType.InlineI => 4,
        OperandType.InlineBrTarget => 4,
        OperandType.InlineString => 4,
        OperandType.InlineField => 4,
        OperandType.InlineMethod => 4,
        OperandType.InlineType => 4,
        OperandType.InlineSig => 4,
        OperandType.InlineTok => 4,
        OperandType.InlineI8 => 8,
        OperandType.InlineR => 8,
        _ => 0,   // InlineSwitch handled separately
    };

    private static LispObject EmitInstr(OpCode op, byte[] il, ref int pc,
        Module module, Dictionary<int, string> labels)
    {
        string name = op.Name.Replace('.', '-').ToUpperInvariant();
        switch (op.OperandType)
        {
            case OperandType.InlineNone:
                return MakeInstr(name, null);
            case OperandType.ShortInlineI:
                return MakeInstr(name, new Fixnum((sbyte)il[pc++]));
            case OperandType.ShortInlineVar:
                return MakeInstr(name, new Fixnum(il[pc++]));
            case OperandType.InlineI:
            {
                int v = BitConverter.ToInt32(il, pc); pc += 4;
                return MakeInstr(name, new Fixnum(v));
            }
            case OperandType.InlineI8:
            {
                long v = BitConverter.ToInt64(il, pc); pc += 8;
                return MakeInstr(name, new Fixnum(v));
            }
            case OperandType.InlineVar:
            {
                ushort v = BitConverter.ToUInt16(il, pc); pc += 2;
                return MakeInstr(name, new Fixnum(v));
            }
            case OperandType.ShortInlineR:
            {
                float v = BitConverter.ToSingle(il, pc); pc += 4;
                return MakeInstr(name, new SingleFloat(v));
            }
            case OperandType.InlineR:
            {
                double v = BitConverter.ToDouble(il, pc); pc += 8;
                return MakeInstr(name, new DoubleFloat(v));
            }
            case OperandType.ShortInlineBrTarget:
            {
                sbyte off = (sbyte)il[pc++];
                int target = pc + off;
                var sym = labels.TryGetValue(target, out var lbl) ? Startup.Keyword(lbl)
                    : (LispObject)new Fixnum(target);
                return MakeInstr(name, sym);
            }
            case OperandType.InlineBrTarget:
            {
                int off = BitConverter.ToInt32(il, pc); pc += 4;
                int target = pc + off;
                var sym = labels.TryGetValue(target, out var lbl) ? Startup.Keyword(lbl)
                    : (LispObject)new Fixnum(target);
                return MakeInstr(name, sym);
            }
            case OperandType.InlineString:
            {
                int tok = BitConverter.ToInt32(il, pc); pc += 4;
                string s;
                try { s = module.ResolveString(tok); }
                catch { s = $"<unresolved-string-{tok:X8}>"; }
                return MakeInstr(name, new LispString(s));
            }
            case OperandType.InlineMethod:
            {
                int tok = BitConverter.ToInt32(il, pc); pc += 4;
                string s;
                try
                {
                    var m = module.ResolveMethod(tok);
                    s = m == null ? $"<nil-method-{tok:X8}>"
                        : $"{m.DeclaringType?.FullName ?? "?"}::{m.Name}";
                }
                catch { s = $"<unresolved-method-{tok:X8}>"; }
                return MakeInstr(name, new LispString(s));
            }
            case OperandType.InlineType:
            {
                int tok = BitConverter.ToInt32(il, pc); pc += 4;
                string s;
                try { s = module.ResolveType(tok).FullName ?? "?"; }
                catch { s = $"<unresolved-type-{tok:X8}>"; }
                return MakeInstr(name, new LispString(s));
            }
            case OperandType.InlineField:
            {
                int tok = BitConverter.ToInt32(il, pc); pc += 4;
                string s;
                try
                {
                    var f = module.ResolveField(tok);
                    s = f == null ? $"<nil-field-{tok:X8}>"
                        : $"{f.DeclaringType?.FullName ?? "?"}::{f.Name}";
                }
                catch { s = $"<unresolved-field-{tok:X8}>"; }
                return MakeInstr(name, new LispString(s));
            }
            case OperandType.InlineTok:
            case OperandType.InlineSig:
            {
                int tok = BitConverter.ToInt32(il, pc); pc += 4;
                return MakeInstr(name, new LispString($"<token-{tok:X8}>"));
            }
            case OperandType.InlineSwitch:
            {
                int count = BitConverter.ToInt32(il, pc); pc += 4;
                int baseAddr = pc + count * 4;
                var targets = new List<LispObject>();
                for (int i = 0; i < count; i++)
                {
                    int off = BitConverter.ToInt32(il, pc); pc += 4;
                    int target = baseAddr + off;
                    targets.Add(labels.TryGetValue(target, out var lbl)
                        ? (LispObject)Startup.Keyword(lbl)
                        : new Fixnum(target));
                }
                return MakeInstr(name, ListOf(targets));
            }
            default:
                return MakeInstr(name, new LispString(
                    $"<unhandled-operand-type-{op.OperandType}>"));
        }
    }

    private static LispObject MakeInstr(string name, LispObject? operand)
    {
        var head = Startup.Keyword(name);
        if (operand == null)
            return new Cons(head, Nil.Instance);
        return new Cons(head, new Cons(operand, Nil.Instance));
    }

    private static LispObject ListOf(IList<LispObject> items)
    {
        LispObject res = Nil.Instance;
        for (int i = items.Count - 1; i >= 0; i--)
            res = new Cons(items[i], res);
        return res;
    }
}
