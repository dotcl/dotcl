// The ablation harness, holding the investigation it was written for as a
// worked example. Read README.md first: what is meant to survive between
// investigations is the shape, not these particular variants. Edit this file
// down to whatever you are chasing and throw the edit away afterwards.
//
// Here: the BOXED structure slot read, Runtime.StructRefI. It reaches the slot
// through two reads of the layout map where one would do -- SlotIndex calls
// SlotEntry for the version check and throws the entry away, and GetSlot then
// reads _layout[index] again to decide boxed from raw. The question is what
// that second read costs, and the answer depends on something the source does
// not make obvious, so there are two groups.
//
//   Group B  _layout != null -- a structure with at least one raw slot,
//            reading one of its BOXED slots. Both reads index the array.
//   Group C  _layout == null -- a structure with no :TYPE anywhere, never
//            redefined, which is most of them. LayoutEntry answers a constant
//            without touching the array, so the duplicated work is a field
//            load and a null test rather than a bounds-checked element read.
//
// B1 and C1 call the REAL DotCL.Runtime on REAL LispStructs. The rest are
// replicas of that chain with one thing removed at a time, so the difference
// between two adjacent rows is the price of the thing between them. B6 is the
// ceiling: a plain sealed class with an object field.
//
// All variants are alternated inside one process and the minimum of 5 is kept,
// and every one of them is warmed before any is timed. The input is built
// once, outside every timed region. Run it under DOTNET_TieredCompilation=0:
// with tiering on these variants do not all reach the same state and the
// ranking comes out wrong.

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using DotCL;

namespace Ablation
{
    // ---- the ceiling ------------------------------------------------------
    public sealed class Holder
    {
        public object A;
        public object B;
        public Holder(object a, object b) { A = a; B = b; }
    }

    // ---- a replica of LispStruct's storage --------------------------------
    public abstract class RObj { }

    public sealed class RStruct : RObj
    {
        public const int SlotPosMask = 0xFFFF;
        public const int SlotBoxed = 0xFFFF;
        public const int SlotKindDouble = 0x8000;
        public const int VersionShift = 16;

        public readonly object[] Slots;
        public readonly int[] Layout;     // null models the untyped structure
        public readonly long[] Longs;

        public RStruct(object[] slots, int[] layout, long[] longs)
        {
            Slots = slots; Layout = layout; Longs = longs;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int LayoutEntry(int index) => Layout == null ? SlotBoxed : Layout[index];

        // The current shape: the caller has already checked the version and
        // thrown the entry away, so this reads the layout a second time.
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public object GetSlot(int index)
        {
            if (Layout != null)
            {
                int entry = Layout[index];
                int pos = entry & SlotPosMask;
                if (pos != SlotBoxed)
                    return (entry & SlotKindDouble) != 0 ? (object)1.0 : (object)Longs[pos];
            }
            return Slots[index];
        }

        // The proposed shape: the entry the version check already loaded is
        // passed in, so the layout is read once.
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public object GetSlotWithEntry(int index, int entry)
        {
            int pos = entry & SlotPosMask;
            if (pos != SlotBoxed)
                return (entry & SlotKindDouble) != 0 ? (object)1.0 : (object)Longs[pos];
            return Slots[index];
        }
    }

    public static class R
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int SlotEntry(RStruct s, int packed)
        {
            int entry = s.LayoutEntry(packed & RStruct.SlotPosMask);
            if (((entry ^ packed) >> RStruct.VersionShift) != 0) throw new InvalidOperationException();
            return entry;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int SlotIndex(RStruct s, int packed)
        {
            SlotEntry(s, packed);
            return packed & RStruct.SlotPosMask;
        }

        // B2/C2: exactly what StructRefI does today.
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static object RefCurrent(RObj o, int packed)
        {
            if (o is RStruct s) return s.GetSlot(SlotIndex(s, packed));
            throw new InvalidOperationException();
        }

        // B3/C3: the version check keeps the entry it loaded.
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static object RefOneRead(RObj o, int packed)
        {
            if (o is RStruct s)
                return s.GetSlotWithEntry(packed & RStruct.SlotPosMask, SlotEntry(s, packed));
            throw new InvalidOperationException();
        }

        // B4: one read and no version check, to price the check separately.
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static object RefNoVersion(RObj o, int packed)
        {
            if (o is RStruct s)
                return s.GetSlotWithEntry(packed & RStruct.SlotPosMask,
                                          s.LayoutEntry(packed & RStruct.SlotPosMask));
            throw new InvalidOperationException();
        }

        // B5/C5: the floor for a boxed read -- cast and element load.
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static object RefSlotsOnly(RObj o, int index) => ((RStruct)o).Slots[index];

        // B7/C7: the current chain again, but as an out-of-line call. The real
        // StructRefI is 445 bytes of IL, far past anything the inliner will
        // take, because the cold paths -- a stale version, a LispInstance, a
        // Package, and a throw that builds a StackTrace and interpolates a
        // string -- are in the same method as the slot read. If B7 lands on B1
        // then that, and not the duplicated layout read, is what the boxed path
        // is paying for.
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static object RefCurrentOutOfLine(RObj o, int packed)
        {
            if (o is RStruct s) return s.GetSlot(SlotIndex(s, packed));
            throw new InvalidOperationException();
        }
    }

    public static class Program
    {
        const long N = 50000000;
        const int Repeats = 1;

        static LispObject marker;
        static object rmarker;

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long Real(LispObject p, int packed, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(Runtime.StructRefI(p, packed), marker)) acc++;
            return acc;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long Current(RObj p, int packed, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(R.RefCurrent(p, packed), rmarker)) acc++;
            return acc;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long OneRead(RObj p, int packed, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(R.RefOneRead(p, packed), rmarker)) acc++;
            return acc;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long NoVersion(RObj p, int packed, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(R.RefNoVersion(p, packed), rmarker)) acc++;
            return acc;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long SlotsOnly(RObj p, int index, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(R.RefSlotsOnly(p, index), rmarker)) acc++;
            return acc;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long OutOfLine(RObj p, int packed, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(R.RefCurrentOutOfLine(p, packed), rmarker)) acc++;
            return acc;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static long Ceiling(Holder p, long n)
        {
            long acc = 0;
            for (long i = 0; i < n; i++)
                if (ReferenceEquals(p.B, rmarker)) acc++;
            return acc;
        }

        static long sink;

        static double Ms(Func<long> f)
        {
            var sw = Stopwatch.StartNew();
            for (int r = 0; r < Repeats; r++) sink += f();
            sw.Stop();
            return sw.Elapsed.TotalMilliseconds;
        }

        static void Main()
        {
            marker = new Symbol("MARK");
            rmarker = new object();

            // --- real LispStructs -------------------------------------------
            // WITH a layout: slot 0 raw, slot 1 boxed. Reading slot 1 is the
            // case where both layout reads index the array.
            var withName = new Symbol("B-WITH-LAYOUT");
            LispStruct.RegisterLayout(withName, new[] { 0, -1 }, new byte[] { 0, 0 });
            var realWith = new LispStruct(withName, new LispObject[] { Fixnum.Make(1), marker });

            // WITHOUT a layout: no :TYPE anywhere, never redefined, so nothing
            // is registered and _layout stays null.
            var noneName = new Symbol("B-NO-LAYOUT");
            var realNone = new LispStruct(noneName, new LispObject[] { Fixnum.Make(1), marker });

            // --- replicas ----------------------------------------------------
            var replWith = new RStruct(new object[] { null, rmarker },
                                       new[] { 0, RStruct.SlotBoxed },
                                       new long[] { 1 });
            var replNone = new RStruct(new object[] { null, rmarker }, null, null);
            var holder = new Holder(null, rmarker);

            var names = new[] {
                "B1 real StructRefI, layout ", "B2 replica, two reads      ",
                "B3 replica, one read       ", "B4 replica, no version     ",
                "B5 replica, slots only     ",
                "C1 real StructRefI, no lay ", "C2 replica, two reads      ",
                "C3 replica, one read       ", "C5 replica, slots only     ",
                "B7 replica, out-of-line   ", "C7 replica, out-of-line nl ",
                "B6 ceiling, object field   ",
            };
            var fns = new Func<long>[] {
                () => Real(realWith, 1, N),
                () => Current(replWith, 1, N),
                () => OneRead(replWith, 1, N),
                () => NoVersion(replWith, 1, N),
                () => SlotsOnly(replWith, 1, N),
                () => Real(realNone, 1, N),
                () => Current(replNone, 1, N),
                () => OneRead(replNone, 1, N),
                () => SlotsOnly(replNone, 1, N),
                () => OutOfLine(replWith, 1, N),
                () => OutOfLine(replNone, 1, N),
                () => Ceiling(holder, N),
            };

            for (int w = 0; w < 3; w++)
                foreach (var f in fns) Ms(f);

            var best = new double[fns.Length];
            for (int i = 0; i < best.Length; i++) best[i] = double.MaxValue;

            for (int round = 0; round < 5; round++)
                for (int i = 0; i < fns.Length; i++)
                {
                    double v = Ms(fns[i]);
                    if (v < best[i]) best[i] = v;
                }

            double ceil = best[best.Length - 1];
            for (int i = 0; i < fns.Length; i++)
                Console.WriteLine($"{names[i]}\t{best[i]:F3}\t{best[i] / ceil:F2}x");
            Console.WriteLine($"sink: {sink}");
        }
    }
}
