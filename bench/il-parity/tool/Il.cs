using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace IlParity.Tool;

/// <summary>One normalized instruction: the opcode name, and for the opcodes
/// whose operand is the point (a call, a type test, a field) the name of what
/// it refers to. Local slot numbers, branch targets and literal values are
/// dropped -- two compilers number their locals differently and that is not a
/// difference anyone wants reported.</summary>
public readonly record struct Instr(string Op, string Arg)
{
    public override string ToString() => Arg.Length == 0 ? Op : Op + " " + Arg;
}

public static class Il
{
    // The decode table is built by reflecting over System.Reflection.Emit.OpCodes
    // rather than written out. Every OpCode carries its own Value, Size and
    // OperandType, so the table is exact by construction and cannot drift from
    // the runtime's own idea of the instruction set.
    private static readonly Dictionary<ushort, OpCode> Table = BuildTable();

    private static Dictionary<ushort, OpCode> BuildTable()
    {
        var t = new Dictionary<ushort, OpCode>();
        foreach (var f in typeof(OpCodes).GetFields(BindingFlags.Public | BindingFlags.Static))
        {
            if (f.GetValue(null) is OpCode op)
                t[(ushort)op.Value] = op;
        }
        return t;
    }

    /// <summary>Which opcodes keep their operand in the normalized form. These
    /// are the ones a parity report is about: what got called, what got boxed,
    /// what got cast.</summary>
    private static bool KeepsOperand(OpCode op) => op.OperandType switch
    {
        OperandType.InlineMethod => true,
        OperandType.InlineType => true,
        OperandType.InlineField => true,
        OperandType.InlineTok => true,
        _ => false,
    };

    private static int OperandSize(OpCode op, byte[] il, int at) => op.OperandType switch
    {
        OperandType.InlineNone => 0,
        OperandType.ShortInlineBrTarget => 1,
        OperandType.ShortInlineI => 1,
        OperandType.ShortInlineVar => 1,
        OperandType.InlineVar => 2,
        OperandType.InlineBrTarget => 4,
        OperandType.InlineField => 4,
        OperandType.InlineI => 4,
        OperandType.InlineMethod => 4,
        OperandType.InlineSig => 4,
        OperandType.InlineString => 4,
        OperandType.InlineTok => 4,
        OperandType.InlineType => 4,
        OperandType.ShortInlineR => 4,
        OperandType.InlineI8 => 8,
        OperandType.InlineR => 8,
        // switch: a count followed by that many 4-byte targets.
        OperandType.InlineSwitch => 4 + 4 * BitConverter.ToInt32(il, at),
        _ => throw new InvalidOperationException("unknown operand type " + op.OperandType),
    };

    /// <summary>Decode one method body into normalized instructions.</summary>
    public static List<Instr> Decode(MetadataReader md, byte[] il)
    {
        var result = new List<Instr>();
        int i = 0;
        while (i < il.Length)
        {
            ushort code = il[i];
            i++;
            if (code == 0xFE)
            {
                code = (ushort)(0xFE00 | il[i]);
                i++;
            }
            if (!Table.TryGetValue(code, out var op))
                throw new InvalidOperationException($"unknown opcode 0x{code:X}");
            int size = OperandSize(op, il, i);
            string arg = "";
            if (KeepsOperand(op) && size == 4)
                arg = TokenName(md, BitConverter.ToInt32(il, i));
            result.Add(new Instr(op.Name, arg));
            i += size;
        }
        return result;
    }

    /// <summary>The readable name behind a metadata token: Type.Member for a
    /// member reference, Type for a type. Only the last namespace segment is
    /// kept -- what matters in a diff is which member, not where it lives.
    /// </summary>
    private static string TokenName(MetadataReader md, int token)
    {
        try
        {
            var h = MetadataTokens.EntityHandle(token);
            switch (h.Kind)
            {
                case HandleKind.MethodDefinition:
                {
                    var m = md.GetMethodDefinition((MethodDefinitionHandle)h);
                    return Short(md.GetString(md.GetTypeDefinition(m.GetDeclaringType()).Name))
                           + "." + md.GetString(m.Name);
                }
                case HandleKind.MemberReference:
                {
                    var m = md.GetMemberReference((MemberReferenceHandle)h);
                    return Short(ParentName(md, m.Parent)) + "." + md.GetString(m.Name);
                }
                case HandleKind.FieldDefinition:
                {
                    var f = md.GetFieldDefinition((FieldDefinitionHandle)h);
                    return Short(md.GetString(md.GetTypeDefinition(f.GetDeclaringType()).Name))
                           + "." + md.GetString(f.Name);
                }
                case HandleKind.TypeDefinition:
                    return Short(md.GetString(md.GetTypeDefinition((TypeDefinitionHandle)h).Name));
                case HandleKind.TypeReference:
                    return Short(md.GetString(md.GetTypeReference((TypeReferenceHandle)h).Name));
                case HandleKind.TypeSpecification:
                    return "<typespec>";
                case HandleKind.MethodSpecification:
                {
                    var ms = md.GetMethodSpecification((MethodSpecificationHandle)h);
                    return TokenName(md, MetadataTokens.GetToken(ms.Method));
                }
                default:
                    return "<" + h.Kind + ">";
            }
        }
        catch
        {
            return "<token>";
        }
    }

    private static string ParentName(MetadataReader md, EntityHandle parent) => parent.Kind switch
    {
        HandleKind.TypeReference => md.GetString(md.GetTypeReference((TypeReferenceHandle)parent).Name),
        HandleKind.TypeDefinition => md.GetString(md.GetTypeDefinition((TypeDefinitionHandle)parent).Name),
        HandleKind.TypeSpecification => "<typespec>",
        _ => "<parent>",
    };

    private static string Short(string name)
    {
        int dot = name.LastIndexOf('.');
        return dot < 0 ? name : name.Substring(dot + 1);
    }

    /// <summary>Every method body in an assembly, keyed by Type.Method.
    ///
    /// A dotcl fasl and a C# dll are read by the same code because they are the
    /// same kind of file: an IL-only PE. That is the whole reason this tool has
    /// one reader instead of two.</summary>
    public static Dictionary<string, List<Instr>> ReadAssembly(string path)
    {
        var result = new Dictionary<string, List<Instr>>();
        using var fs = File.OpenRead(path);
        using var pe = new PEReader(fs);
        var md = pe.GetMetadataReader();
        foreach (var handle in md.MethodDefinitions)
        {
            var m = md.GetMethodDefinition(handle);
            if (m.RelativeVirtualAddress == 0) continue;
            string type = md.GetString(md.GetTypeDefinition(m.GetDeclaringType()).Name);
            string name = md.GetString(m.Name);
            var body = pe.GetMethodBody(m.RelativeVirtualAddress);
            var instrs = Decode(md, body.GetILBytes());
            // A dotcl fasl can hold several methods with the same short name
            // (a closure and the function that makes it). Keep the longest --
            // the trivial one is the thunk, the body is what is being compared.
            string key = Short(type) + "." + name;
            if (!result.TryGetValue(key, out var have) || have.Count < instrs.Count)
                result[key] = instrs;
        }
        return result;
    }
}
