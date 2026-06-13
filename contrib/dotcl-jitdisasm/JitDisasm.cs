using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using Iced.Intel;

namespace DotCL.Contrib.JitDisasm;

public static class JitDisasmContrib
{
    public static void Initialize()
    {
        DotCL.Runtime.JitDisassembleHook = Disassemble;
    }

    private static readonly MethodInfo? s_getMethodDescriptor =
        typeof(DynamicMethod).GetMethod("GetMethodDescriptor",
            BindingFlags.NonPublic | BindingFlags.Instance);

    private static unsafe void Disassemble(DotCL.LispFunction fn, TextWriter writer)
    {
        var (del, label) = fn.GetJitDelegate();
        var method = del.Method;

        RuntimeMethodHandle handle;
        try
        {
            if (method is DynamicMethod dm)
            {
                if (s_getMethodDescriptor == null)
                {
                    writer.WriteLine("; DynamicMethod: GetMethodDescriptor not found (runtime version mismatch?)");
                    return;
                }
                handle = (RuntimeMethodHandle)s_getMethodDescriptor.Invoke(dm, null)!;
            }
            else
            {
                handle = method.MethodHandle;
            }
            RuntimeHelpers.PrepareMethod(handle);
        }
        catch (Exception ex)
        {
            writer.WriteLine($"; Failed to obtain JIT handle: {ex.Message}");
            return;
        }

        var ptr = (ulong)(nint)handle.GetFunctionPointer();
        var arch = RuntimeInformation.ProcessArchitecture;
        writer.WriteLine($"; JIT disassembly for {fn.Name ?? "#<FUNCTION>"} ({arch.ToString().ToLowerInvariant()}, {label})");

        // Limit read to current page to avoid crossing into unmapped memory.
        const int PageSize = 4096;
        int pageOffset = (int)(ptr % PageSize);
        int bytesToRead = PageSize - pageOffset;
        var bytes = new byte[bytesToRead];
        fixed (byte* buf = bytes)
        {
            Buffer.MemoryCopy((void*)ptr, buf, bytesToRead, bytesToRead);
        }

        if (arch == Architecture.X64)
            DisassembleX86(bytes, ptr, writer, 64);
        else if (arch == Architecture.X86)
            DisassembleX86(bytes, ptr, writer, 32);
        else if (arch == Architecture.Arm64)
            DisassembleArm64Hex(bytes, ptr, writer);
        else
            writer.WriteLine($"; Unsupported architecture: {arch}");
    }

    private static void DisassembleX86(byte[] bytes, ulong ip, TextWriter writer, int bitness)
    {
        var reader = new ByteArrayCodeReader(bytes);
        var decoder = Iced.Intel.Decoder.Create(bitness, reader, ip);

        var formatter = new NasmFormatter();
        formatter.Options.HexPrefix = "0x";
        formatter.Options.HexSuffix = null;
        formatter.Options.UppercaseHex = false;
        formatter.Options.SpaceAfterOperandSeparator = true;

        var output = new StringOutput();

        while (reader.CanReadByte)
        {
            int byteOffset = (int)(decoder.IP - ip);
            decoder.Decode(out var instr);
            if (instr.IsInvalid) break;

            output.Reset();
            formatter.Format(instr, output);
            writer.WriteLine($"  {instr.IP:x16}  {output}");

            // Near return: 0xC3 (RETN) or 0xC2 xx xx (RETN imm16)
            if (byteOffset < bytes.Length &&
                (bytes[byteOffset] == 0xC3 || bytes[byteOffset] == 0xC2))
                break;
        }
    }

    private static void DisassembleArm64Hex(byte[] bytes, ulong ip, TextWriter writer)
    {
        // Partial ARM64 decoder covering the instruction subset the .NET JIT emits
        // for our generated methods (frame setup, loads/stores, moves, integer
        // arithmetic/compare, branches and calls). Unknown encodings fall back to a
        // .word hex dump. Good enough to follow control flow and spot bl/blr calls.
        for (int i = 0; i + 4 <= bytes.Length; i += 4)
        {
            uint w = BitConverter.ToUInt32(bytes, i);
            ulong pc = ip + (ulong)i;
            writer.WriteLine($"  {pc:x16}  {w:x8}  {DecodeArm64(w, pc)}");
            if (w == 0xD65F03C0u) // RET
                break;
        }
    }

    // Register name. wantSp=true → reg 31 prints as "sp" (base/dest in add/ldr/str),
    // otherwise reg 31 is the zero register.
    private static string Areg(uint n, bool is64, bool wantSp = false)
        => n == 31 ? (wantSp ? "sp" : (is64 ? "xzr" : "wzr")) : (is64 ? "x" : "w") + n;

    private static long SignExtend(uint v, int bits)
    {
        long m = 1L << (bits - 1);
        return ((long)v ^ m) - m;
    }

    private static string Hex(long v)
        => v < 0 ? $"-0x{-v:x}" : $"0x{v:x}";

    private static string DecodeArm64(uint w, ulong pc)
    {
        // --- branches / calls ---
        if (w == 0xD503201Fu) return "nop";
        if (w == 0xD65F03C0u) return "ret";
        if ((w & 0xFFFFFC1Fu) == 0xD65F0000u) return $"ret {Areg((w >> 5) & 31, true)}";
        if ((w & 0xFFFFFC1Fu) == 0xD63F0000u) return $"blr {Areg((w >> 5) & 31, true)}";
        if ((w & 0xFFFFFC1Fu) == 0xD61F0000u) return $"br {Areg((w >> 5) & 31, true)}";
        if ((w & 0xFC000000u) == 0x14000000u) return $"b 0x{(ulong)((long)pc + (SignExtend(w & 0x03FFFFFFu, 26) << 2)):x}";
        if ((w & 0xFC000000u) == 0x94000000u) return $"bl 0x{(ulong)((long)pc + (SignExtend(w & 0x03FFFFFFu, 26) << 2)):x}";
        if ((w & 0xFF000010u) == 0x54000000u) // b.cond
        {
            string[] cc = { "eq","ne","cs","cc","mi","pl","vs","vc","hi","ls","ge","lt","gt","le","al","nv" };
            long off = SignExtend((w >> 5) & 0x7FFFF, 19) << 2;
            return $"b.{cc[w & 15]} 0x{(ulong)((long)pc + off):x}";
        }
        if ((w & 0x7E000000u) == 0x34000000u) // cbz/cbnz
        {
            bool is64 = (w & 0x80000000u) != 0;
            bool nz = (w & 0x01000000u) != 0;
            long off = SignExtend((w >> 5) & 0x7FFFF, 19) << 2;
            return $"{(nz ? "cbnz" : "cbz")} {Areg(w & 31, is64)}, 0x{(ulong)((long)pc + off):x}";
        }
        if ((w & 0x7E000000u) == 0x36000000u) // tbz/tbnz
        {
            bool nz = (w & 0x01000000u) != 0;
            uint bit = ((w >> 31) << 5) | ((w >> 19) & 31);
            long off = SignExtend((w >> 5) & 0x3FFF, 14) << 2;
            return $"{(nz ? "tbnz" : "tbz")} {Areg(w & 31, true)}, #{bit}, 0x{(ulong)((long)pc + off):x}";
        }
        // --- moves (wide immediate) ---
        if ((w & 0x7F800000u) == 0x52800000u || (w & 0x7F800000u) == 0x12800000u
            || (w & 0x7F800000u) == 0x72800000u)
        {
            bool is64 = (w & 0x80000000u) != 0;
            string op = (w & 0x60000000u) == 0x40000000u ? "movz"
                       : (w & 0x60000000u) == 0x00000000u ? "movn" : "movk";
            int shift = (int)((w >> 21) & 3) * 16;
            uint imm = (w >> 5) & 0xFFFF;
            return $"{op} {Areg(w & 31, is64)}, #0x{imm:x}{(shift != 0 ? $", lsl #{shift}" : "")}";
        }
        // --- add/sub immediate ---
        if ((w & 0x1F000000u) == 0x11000000u)
        {
            bool is64 = (w & 0x80000000u) != 0;
            bool sub = (w & 0x40000000u) != 0;
            bool s = (w & 0x20000000u) != 0;
            uint imm = (w >> 10) & 0xFFF;
            if ((w & 0x00400000u) != 0) imm <<= 12;
            uint rd = w & 31, rn = (w >> 5) & 31;
            if (s && rd == 31) return $"cmp {Areg(rn, is64, true)}, #0x{imm:x}"; // subs xzr
            string mn = (sub ? "sub" : "add") + (s ? "s" : "");
            return $"{mn} {Areg(rd, is64, true)}, {Areg(rn, is64, true)}, #0x{imm:x}";
        }
        // --- add/sub shifted register ---
        if ((w & 0x1F200000u) == 0x0B000000u)
        {
            bool is64 = (w & 0x80000000u) != 0;
            bool sub = (w & 0x40000000u) != 0;
            bool s = (w & 0x20000000u) != 0;
            uint rd = w & 31, rn = (w >> 5) & 31, rm = (w >> 16) & 31;
            if (s && rd == 31) return $"cmp {Areg(rn, is64)}, {Areg(rm, is64)}"; // subs xzr
            string mn = (sub ? "sub" : "add") + (s ? "s" : "");
            return $"{mn} {Areg(rd, is64)}, {Areg(rn, is64)}, {Areg(rm, is64)}";
        }
        // --- logical shifted register (orr → mov alias) ---
        if ((w & 0x7F200000u) == 0x2A000000u) // orr (shifted reg)
        {
            bool is64 = (w & 0x80000000u) != 0;
            uint rd = w & 31, rn = (w >> 5) & 31, rm = (w >> 16) & 31;
            if (rn == 31) return $"mov {Areg(rd, is64)}, {Areg(rm, is64)}";
            return $"orr {Areg(rd, is64)}, {Areg(rn, is64)}, {Areg(rm, is64)}";
        }
        // --- load/store register, unsigned offset ---
        if ((w & 0x3B000000u) == 0x39000000u)
        {
            uint size = w >> 30;          // 0=b,1=h,2=w,3=x
            bool load = (w & 0x00400000u) != 0;
            bool is64 = size == 3;
            uint imm = ((w >> 10) & 0xFFF) << (int)size;
            uint rt = w & 31, rn = (w >> 5) & 31;
            string mn = (load ? "ldr" : "str") + (size == 0 ? "b" : size == 1 ? "h" : "");
            string rtn = size < 2 ? "w" + rt : Areg(rt, is64);
            return $"{mn} {rtn}, [{Areg(rn, true, true)}{(imm != 0 ? $", #0x{imm:x}" : "")}]";
        }
        // --- load/store pair (signed offset / pre / post), incl SIMD&FP ---
        if ((w & 0x3A000000u) == 0x28000000u)
        {
            bool simd = (w & 0x04000000u) != 0;
            bool load = (w & 0x00400000u) != 0;
            uint opc = w >> 30;
            int scale = simd ? (2 + (int)opc) : (opc == 2 ? 3 : 2);
            long imm = SignExtend((w >> 15) & 0x7F, 7) << scale;
            uint rt = w & 31, rt2 = (w >> 10) & 31, rn = (w >> 5) & 31;
            uint idx = (w >> 23) & 3; // 1=post,2=offset,3=pre
            string rc = simd ? (scale == 4 ? "q" : scale == 3 ? "d" : "s") : (opc == 2 ? "x" : "w");
            string mn = load ? "ldp" : "stp";
            string addr = idx == 1 ? $"[{Areg(rn, true, true)}], #{Hex(imm)}"
                        : idx == 3 ? $"[{Areg(rn, true, true)}, #{Hex(imm)}]!"
                        : $"[{Areg(rn, true, true)}{(imm != 0 ? $", #{Hex(imm)}" : "")}]";
            return $"{mn} {rc}{rt}, {rc}{rt2}, {addr}";
        }
        // --- SIMD movi (common for zeroing vector regs in frame setup) ---
        if ((w & 0xBF000000u) == 0x0F000000u && ((w >> 10) & 0x3F) == 0x39)
            return $"movi v{w & 31}.?, #...";
        return $".word 0x{w:x8}";
    }
}
