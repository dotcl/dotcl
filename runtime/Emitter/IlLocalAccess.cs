using System.Reflection.Emit;

namespace DotCL.Emitter;

/// <summary>
/// Loads and stores of IL locals. Every ldloc / stloc / ldloca the emitters
/// write goes through EmitLoc instead of ILGenerator.Emit(OpCode, LocalBuilder).
///
/// The ILGenerator of PersistedAssemblyBuilder (the one that writes fasls)
/// encodes the long form of ldloc / stloc / ldloca, used for a local whose
/// index is 256 or more, with a 4-byte operand (System.Reflection.Metadata's
/// InstructionEncoder writes an int32). ECMA-335 gives these opcodes a 2-byte
/// operand, so the JIT reads the index and then the two zero bytes as two NOPs.
/// The method still runs, but each such access costs 2 more IL bytes and 2 more
/// IL instructions, which pushed large functions past the JIT's size limits
/// (60,000 IL bytes, 20,000 instructions) and so into unoptimized code.
///
/// Emitting the long form ourselves with an int16 operand is correct for both
/// generators; the short forms are left to the generator.
/// </summary>
internal static class IlLocalAccess
{
    internal static void EmitLoc(this ILGenerator il, OpCode op, LocalBuilder local)
    {
        int index = local.LocalIndex;
        if (index > byte.MaxValue)
        {
            OpCode longOp;
            if (op.Equals(OpCodes.Ldloc) || op.Equals(OpCodes.Ldloc_S)) longOp = OpCodes.Ldloc;
            else if (op.Equals(OpCodes.Stloc) || op.Equals(OpCodes.Stloc_S)) longOp = OpCodes.Stloc;
            else if (op.Equals(OpCodes.Ldloca) || op.Equals(OpCodes.Ldloca_S)) longOp = OpCodes.Ldloca;
            else { il.Emit(op, local); return; }
            il.Emit(longOp, unchecked((short)index));
            return;
        }
        il.Emit(op, local);
    }
}
