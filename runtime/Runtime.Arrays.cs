using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Array/Vector operations ---

    /// <summary>An array subscript that must be an integer. A bare cast would raise
    /// InvalidCastException, which the CLR-exception mapping turns into a
    /// PROGRAM-ERROR; the spec calls for a TYPE-ERROR naming the bad subscript.</summary>
    private static int IntArg(string what, string role, LispObject o)
    {
        // Range-checked, not a bare cast. Every array and string path narrows
        // the subscript to int, and narrowing wraps: (AREF V 4294967296) became
        // (AREF V 0) and returned an element where it had to signal. No
        // sequence this runtime can build reaches int, so a subscript outside
        // it is out of range for any of them.
        if (o is Fixnum f)
        {
            if (f.Value >= int.MinValue && f.Value <= int.MaxValue) return (int)f.Value;
            throw new LispErrorException(new LispTypeError(
                $"{what}: {role} {f.Value} is out of range", o,
                new Cons(Startup.Sym("INTEGER"),
                    new Cons(Fixnum.Make(0), new Cons(Startup.Sym("*"), Nil.Instance)))));
        }
        throw new LispErrorException(new LispTypeError(
            $"{what}: {role} must be an integer", o, Startup.Sym("INTEGER")));
    }

    /// <summary>An array dimension: a non-negative integer. A negative or bignum
    /// dimension used to reach the allocation and fail there as a raw .NET
    /// exception.</summary>
    private static int DimArg(string what, LispObject o)
    {
        if (o is Fixnum f && f.Value >= 0 && f.Value <= int.MaxValue) return (int)f.Value;
        var expected = new Cons(Startup.Sym("INTEGER"),
            new Cons(Fixnum.Make(0), new Cons(Startup.Sym("*"), Nil.Instance)));
        throw new LispErrorException(new LispTypeError(
            $"{what}: dimension must be a non-negative integer", o, expected));
    }

    /// <summary>(MAKE-ARRAY n) on a plain one-dimensional general vector: build it
    /// straight, with no argument array anywhere. Everything else -- a dimension
    /// list, rank 0, any keyword -- goes to the shared body, which reads its
    /// keywords out of an array.</summary>
    public static LispObject MakeArray1(LispObject dims)
    {
        if (dims is Fixnum f && f.Value >= 0 && f.Value <= int.MaxValue)
            return new LispVector((int)f.Value, Nil.Instance, "T");
        return MakeArray(new[] { dims });
    }

    /// <summary>(MAKE-ARRAY n :INITIAL-ELEMENT x) -- the one keyword shape common
    /// enough to be worth its own path. Any other keyword falls through.</summary>
    public static LispObject MakeArray3(LispObject dims, LispObject k, LispObject v)
    {
        if (dims is Fixnum f && f.Value >= 0 && f.Value <= int.MaxValue
            && k is Symbol ks && ks.Name == "INITIAL-ELEMENT")
            return new LispVector((int)f.Value, v, "T");
        return MakeArray(new[] { dims, k, v });
    }

    public static LispObject MakeArray(LispObject[] args)
    {
        // (make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer)
        var dims = args[0];
        // Null for a one-dimensional array, where every constructor below takes
        // the rank-1 overload and never looks at it. Building the int[1] anyway
        // cost 32 bytes on every (MAKE-ARRAY n), which is most of them.
        int[]? dimArray = null;
        int rank;
        int size;

        if (dims is Fixnum)
        {
            size = DimArg("MAKE-ARRAY", dims);
            rank = 1;
        }
        else if (dims is Nil)
        {
            // rank-0 array: 0 dimensions, but 1 element (the scalar)
            size = 1;
            dimArray = Array.Empty<int>();
            rank = 0;
        }
        else if (dims is Cons)
        {
            var dimList = new List<int>();
            var cur = dims;
            while (cur is Cons c) { dimList.Add(DimArg("MAKE-ARRAY", c.Car)); cur = c.Cdr; }
            dimArray = dimList.ToArray();
            rank = dimArray.Length;
            size = 1;
            foreach (var d in dimArray) size *= d;
        }
        else throw new LispErrorException(new LispTypeError($"MAKE-ARRAY: unsupported dimensions: {dims}", dims));

        LispObject? initialElement = null;
        LispObject? initialContents = null;
        string elementType = "T";
        int? fillPointer = null;
        LispObject? displacedTo = null;
        int displacedOffset = 0;
        bool isAdjustable = false;
        // Check for odd number of keyword args
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError("MAKE-ARRAY: odd number of keyword arguments"));
        // First pass: check for :allow-other-keys (first occurrence wins)
        bool allowOtherKeys = false;
        for (int i = 1; i < args.Length - 1; i += 2)
            if (args[i] is Symbol aks && aks.Name == "ALLOW-OTHER-KEYS")
            { allowOtherKeys = args[i + 1] is not Nil; break; }
        // Second pass: process keywords (first-wins for duplicates)
        bool ieSet = false, icSet = false, etSet = false, fpSet = false, dtSet = false, dioSet = false, adjSet = false;
        for (int i = 1; i < args.Length - 1; i += 2)
        {
            if (args[i] is not Symbol ks)
                throw new LispErrorException(new LispProgramError($"MAKE-ARRAY: expected keyword symbol, got {args[i]}"));
            {
                switch (ks.Name)
                {
                    case "INITIAL-ELEMENT": if (!ieSet) { initialElement = args[i + 1]; ieSet = true; } break;
                    case "INITIAL-CONTENTS": if (!icSet) { initialContents = args[i + 1]; icSet = true; } break;
                    case "ELEMENT-TYPE":
                        if (!etSet) { elementType = ParseElementTypeName(args[i + 1]); etSet = true; }
                        break;
                    case "FILL-POINTER":
                        if (!fpSet) {
                            if (args[i + 1] is Fixnum fp) fillPointer = (int)fp.Value;
                            else if (args[i + 1] is T) fillPointer = size;
                            fpSet = true;
                        }
                        break;
                    case "DISPLACED-TO": if (!dtSet) { displacedTo = args[i + 1]; dtSet = true; } break;
                    case "DISPLACED-INDEX-OFFSET":
                        if (!dioSet && args[i + 1] is Fixnum dio) { displacedOffset = (int)dio.Value; dioSet = true; } break;
                    case "ADJUSTABLE":
                        if (!adjSet) { isAdjustable = args[i + 1] is not Nil; adjSet = true; } break;
                    case "ALLOW-OTHER-KEYS": break;
                    default:
                        if (!allowOtherKeys)
                            throw new LispErrorException(new LispProgramError($"MAKE-ARRAY: unrecognized keyword :{ks.Name}"));
                        break;
                }
            }
        }


        LispVector vec;
        if (displacedTo != null)
        {
            // Displaced array: true sharing via _displacedTo reference
            LispVector srcVec;
            if (displacedTo is LispVector dv)
            {
                srcVec = dv;
                if (elementType == "T") elementType = dv.ElementTypeName;
            }
            else if (displacedTo is LispString srcStr)
            {
                srcVec = StringAsDisplacementTarget(srcStr);
                if (elementType == "T") elementType = "CHARACTER";
            }
            else throw new LispErrorException(new LispTypeError(
                "MAKE-ARRAY: :displaced-to must be an array", displacedTo, Startup.Sym("ARRAY")));
            CheckDisplacement("MAKE-ARRAY", size, srcVec, displacedOffset);
            vec = new LispVector(size, srcVec, displacedOffset, elementType,
                                 dimArray ?? new[] { size });
        }
        else if (initialContents != null)
        {
            var items = new LispObject[size];
            FlattenContents(initialContents, items, 0, rank);
            vec = rank == 1 ? new LispVector(items, elementType)
                            : new LispVector(items, dimArray!, elementType);
        }
        else
        {
            LispObject fill;
            if (initialElement != null)
                fill = initialElement;
            else if (elementType is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR")
                fill = LispChar.Make('\0');
            else if (elementType == "BIT")
                fill = Fixnum.Make(0);
            else if (elementType.StartsWith("UNSIGNED-BYTE") || elementType.StartsWith("SIGNED-BYTE") ||
                     elementType is "INTEGER" or "FIXNUM" or "FLOAT" or "SINGLE-FLOAT" or "SHORT-FLOAT" or "DOUBLE-FLOAT" or "LONG-FLOAT" or "RATIONAL" or "REAL" or "NUMBER")
                fill = Fixnum.Make(0);
            else if (elementType == "NIL")
                fill = Nil.Instance;
            else
                fill = LispVector.DefaultElement(elementType);
            // An element type with packed storage fills that storage directly. Going
            // through a boxed LispObject[SIZE] first, which the constructor then packs
            // and drops, costs 8 bytes an element in garbage, four times the array
            // itself for a (integer 0 1000) one.
            if (elementType == "BIT" || LispVector.NumKindForElementType(elementType) != 0)
            {
                vec = rank == 1 ? new LispVector(size, fill, elementType)
                                : new LispVector(size, fill, elementType, dimArray!);
            }
            else
            {
                var items = new LispObject[size];
                for (int j = 0; j < size; j++) items[j] = fill;
                vec = rank == 1 ? new LispVector(items, elementType)
                            : new LispVector(items, dimArray!, elementType);
            }
        }

        if (fillPointer.HasValue)
            vec.SetFillPointer(fillPointer.Value);
        if (isAdjustable || displacedTo != null)
            vec.IsAdjustable = true;
        return vec;
    }

    /// <summary>A displaced array has to fit in its target: the offset plus the
    /// new array's total size may not exceed the target's total size (CLHS
    /// MAKE-ARRAY). Without this the array is made, and the first access past the
    /// target's end fails with a raw .NET index error.</summary>
    private static void CheckDisplacement(string who, int size, LispVector target, int offset)
    {
        int targetSize = target.Capacity;
        if (offset < 0 || offset > targetSize || size > targetSize - offset)
            throw new LispErrorException(new LispError(
                $"{who}: can't displace an array of total size {size} at offset {offset} into an array of total size {targetSize}"));
    }

    /// <summary>A LispString is not a LispVector, so it cannot be the target of a
    /// displaced LispVector directly. It is given a CHARACTER vector view that
    /// shares the string's char[] backing (the string is materialized to char[]
    /// once, which is permanent), so writes through the string and through the
    /// displaced array see each other. One view per string, created on first
    /// use; the reverse table lets ARRAY-DISPLACEMENT return the string itself.</summary>
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispString, LispVector> s_stringViews = new();
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispVector, LispString> s_viewOwners = new();
    private static readonly object s_stringViewLock = new();

    private static LispVector StringAsDisplacementTarget(LispString str)
    {
        if (s_stringViews.TryGetValue(str, out var view)) return view;
        lock (s_stringViewLock)
        {
            if (s_stringViews.TryGetValue(str, out view)) return view;
            view = LispVector.CharView(str.RawChars);
            s_stringViews.Add(str, view);
            s_viewOwners.Add(view, str);
            return view;
        }
    }

    /// <summary>The object a displaced array reports as its target: the string
    /// when TARGET is a string's view, otherwise TARGET.</summary>
    internal static LispObject DisplacementTargetObject(LispVector target) =>
        s_viewOwners.TryGetValue(target, out var str) ? str : target;

    public static LispObject AdjustArray(LispObject[] args)
    {
        // (adjust-array array new-dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset)
        if (args.Length < 2) throw new LispErrorException(new LispProgramError("ADJUST-ARRAY: too few arguments"));
        if (args[0] is not LispVector vec)
            throw new LispErrorException(new LispTypeError($"ADJUST-ARRAY: not an array", args[0]));

        var dims = args[1];
        int[] dimArray;
        int size;
        if (dims is Fixnum)
        {
            size = DimArg("ADJUST-ARRAY", dims);
            dimArray = new[] { size };
        }
        else if (dims is Nil)
        {
            // rank-0 array
            size = 1;
            dimArray = Array.Empty<int>();
        }
        else if (dims is Cons)
        {
            var dimList = new List<int>();
            var cur = dims;
            while (cur is Cons c) { dimList.Add(DimArg("ADJUST-ARRAY", c.Car)); cur = c.Cdr; }
            dimArray = dimList.ToArray();
            size = 1;
            foreach (var d in dimArray) size *= d;
        }
        else throw new LispErrorException(new LispTypeError($"ADJUST-ARRAY: unsupported dimensions: {dims}", dims));

        LispObject? initialElement = null;
        LispObject? initialContents = null;
        LispObject? displacedTo = null;
        int displacedOffset = 0;
        int? fillPointer = null;
        string? elementType = null;
        // Check for odd number of keyword args
        if ((args.Length - 2) % 2 != 0)
            throw new LispErrorException(new LispProgramError("ADJUST-ARRAY: odd number of keyword arguments"));
        // First-wins: check :allow-other-keys first (first occurrence wins)
        bool adjAllowOtherKeys = false;
        for (int i = 2; i < args.Length - 1; i += 2)
            if (args[i] is Symbol aks2 && aks2.Name == "ALLOW-OTHER-KEYS")
            { adjAllowOtherKeys = args[i + 1] is not Nil; break; }
        for (int i = 2; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol ks)
            {
                switch (ks.Name)
                {
                    case "INITIAL-ELEMENT": initialElement = args[i + 1]; break;
                    case "INITIAL-CONTENTS": initialContents = args[i + 1]; break;
                    case "ELEMENT-TYPE": elementType = ParseElementTypeName(args[i + 1]); break;
                    case "FILL-POINTER":
                        if (args[i + 1] is Fixnum fp) fillPointer = (int)fp.Value;
                        else if (args[i + 1] is T) fillPointer = size;
                        else if (args[i + 1] is Nil) { } // :fill-pointer nil = no fill pointer (keep as-is)
                        break;
                    case "DISPLACED-TO": displacedTo = args[i + 1]; break;
                    case "DISPLACED-INDEX-OFFSET":
                        if (args[i + 1] is Fixnum dio) displacedOffset = (int)dio.Value; break;
                    case "ALLOW-OTHER-KEYS": break;
                    default:
                        if (!adjAllowOtherKeys)
                            throw new LispErrorException(new LispProgramError($"ADJUST-ARRAY: unrecognized keyword :{ks.Name}"));
                        break;
                }
            }
        }

        int[]? newDims = dimArray.Length == 1 ? null : dimArray;
        string et = elementType ?? vec.ElementTypeName;

        // :DISPLACED-TO a string used to fall through every branch below as if
        // it had not been given, leaving the array filled with #\Nul.
        if (displacedTo is LispString dstr)
            displacedTo = StringAsDisplacementTarget(dstr);
        else if (displacedTo != null && displacedTo is not Nil && displacedTo is not LispVector)
            throw new LispErrorException(new LispTypeError(
                "ADJUST-ARRAY: :displaced-to must be an array", displacedTo, Startup.Sym("ARRAY")));

        if (!vec.IsAdjustable)
        {
            // Non-adjustable: create a new array (original is unchanged)
            LispVector newVec;
            if (displacedTo is LispVector dv2)
            {
                CheckDisplacement("ADJUST-ARRAY", size, dv2, displacedOffset);
                newVec = new LispVector(size, dv2, displacedOffset, et, dimArray);
            }
            else
            {
                var newItems = new LispObject[size];
                LispObject fill = initialElement ?? Nil.Instance;
                if (initialContents != null)
                    FlattenContents(initialContents, newItems, 0, dimArray.Length);
                else
                    CopyWithDimResize(vec, newItems, dimArray, fill);
                newVec = newDims == null ? new LispVector(newItems, et) : new LispVector(newItems, newDims, et);
            }
            // Set fill pointer: use explicit value, or preserve original, or none
            if (fillPointer.HasValue)
                newVec.SetFillPointer(fillPointer.Value);
            else if (vec.HasFillPointer)
                newVec.SetFillPointer(Math.Min(vec.Length, size));
            return newVec;
        }

        // Adjustable: modify in-place
        if (displacedTo is LispVector dv)
        {
            CheckDisplacement("ADJUST-ARRAY", size, dv, displacedOffset);
            vec.AdjustToDisplaced(size, dv, displacedOffset, et, newDims, fillPointer);
        }
        else if (initialContents != null)
        {
            var newItems = new LispObject[size];
            FlattenContents(initialContents, newItems, 0, dimArray.Length);
            vec.Adjust(size, null, newDims, fillPointer, newItems);
        }
        else
        {
            // For multi-dimensional resize, use proper index-based copy
            if (dimArray.Length > 1 || vec.Rank > 1)
            {
                var newItems2 = new LispObject[size];
                CopyWithDimResize(vec, newItems2, dimArray, initialElement ?? Nil.Instance);
                vec.Adjust(size, null, newDims, fillPointer, newItems2);
            }
            else
            {
                vec.Adjust(size, initialElement, newDims, fillPointer);
            }
        }
        return vec;
    }

    /// <summary>
    /// Copy elements from old array to new flat array, respecting multi-dimensional index mapping.
    /// For each new flat index, compute multi-dim indices, check if valid in old array, and copy.
    /// </summary>
    private static void CopyWithDimResize(LispVector old, LispObject[] newItems, int[] newDims, LispObject fill)
    {
        int[] oldDims = old.Dimensions;
        int rank = newDims.Length;
        if (rank == 0) { if (old.Capacity > 0) newItems[0] = old.GetElement(0); else newItems[0] = fill; return; }
        if (rank != oldDims.Length) { for (int i = 0; i < newItems.Length; i++) newItems[i] = fill; return; }
        // Compute old strides (row-major)
        var oldStrides = new int[rank];
        oldStrides[rank - 1] = 1;
        for (int d = rank - 2; d >= 0; d--) oldStrides[d] = oldStrides[d + 1] * oldDims[d + 1];
        var newStrides = new int[rank];
        newStrides[rank - 1] = 1;
        for (int d = rank - 2; d >= 0; d--) newStrides[d] = newStrides[d + 1] * newDims[d + 1];
        var indices = new int[rank];
        for (int newFlat = 0; newFlat < newItems.Length; newFlat++)
        {
            // Convert newFlat to multi-dim indices in new dims
            int tmp = newFlat;
            for (int d = rank - 1; d >= 0; d--) { indices[d] = tmp % newDims[d]; tmp /= newDims[d]; }
            // Check if all indices are within old dims
            bool valid = true;
            for (int d = 0; d < rank; d++) if (indices[d] >= oldDims[d]) { valid = false; break; }
            if (valid)
            {
                int oldFlat = 0;
                for (int d = 0; d < rank; d++) oldFlat += indices[d] * oldStrides[d];
                newItems[newFlat] = old.GetElement(oldFlat);
            }
            else
                newItems[newFlat] = fill;
        }
    }

    private static int FlattenContents(LispObject contents, LispObject[] items, int idx, int rank = 1)
    {
        if (rank <= 0)
        {
            // A zero-dimensional array holds exactly one element, and
            // :initial-contents IS that element rather than a sequence holding
            // it (CLHS make-array). Falling through to the leaf case spread the
            // list instead and stored its first element, so #0A(1 2) built the
            // array #0A1.
            if (idx < items.Length) items[idx++] = contents;
            return idx;
        }
        if (rank <= 1)
        {
            // Leaf level: iterate sequence, store each element as-is (no recursion into sub-lists)
            if (contents is Cons)
            {
                var cur = contents;
                while (cur is Cons c) { if (idx < items.Length) items[idx++] = c.Car; cur = c.Cdr; }
            }
            else if (contents is LispString str)
                for (int j = 0; j < str.Length && idx < items.Length; j++) items[idx++] = LispChar.Make(str[j]);
            else if (contents is LispVector vec)
                for (int j = 0; j < vec.Length && idx < items.Length; j++) items[idx++] = vec[j];
            else if (idx < items.Length)
                items[idx++] = contents;
        }
        else
        {
            // Multi-dimensional: recurse one level deeper
            if (contents is Cons)
            {
                var cur = contents;
                while (cur is Cons c) { idx = FlattenContents(c.Car, items, idx, rank - 1); cur = c.Cdr; }
            }
            else if (contents is LispVector vec)
                for (int j = 0; j < vec.Length; j++) idx = FlattenContents(vec[j], items, idx, rank - 1);
        }
        return idx;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    /// <summary>True if OBJ wraps a System.Array (e.g. from dotnet:make-array),
    /// so aref / (setf aref) can index it transparently. (dotcl/dotcl#45)</summary>
    private static bool TryDotNetArray(LispObject obj, out System.Array arr)
    {
        if (obj is LispDotNetObject dno && dno.Value is System.Array a) { arr = a; return true; }
        arr = null!;
        return false;
    }

    /// <summary>Error for an index outside its valid range. The spec makes a bad
    /// array index a TYPE-ERROR whose expected type is the range the index had to
    /// fall in, so report (INTEGER 0 (LIMIT)) with the index as the datum.</summary>
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispErrorException IndexError(string what, long idx, int limit, string of)
    {
        var expected = new Cons(Startup.Sym("INTEGER"),
            new Cons(Fixnum.Make(0),
                new Cons(new Cons(Fixnum.Make(limit), Nil.Instance), Nil.Instance)));
        return new LispErrorException(new LispTypeError(
            $"{what}: index {idx} out of range for {of} of size {limit}",
            Fixnum.Make(idx), expected));
    }

    /// <summary>Validate one subscript per axis and return the row-major index.
    /// Checking only the flattened index is not enough: on a 2x3 array the
    /// subscripts (0 5) flatten to 5, which is in range for the storage but
    /// outside axis 1.</summary>
    private static int FlatIndex(string what, LispVector v, LispObject[] args, int first, int nidx)
    {
        int[] dims = v.Dimensions;
        if (nidx != dims.Length)
            throw new LispErrorException(new LispProgramError(
                $"{what}: {nidx} indices for rank-{dims.Length} array"));
        int idx = 0;
        for (int k = 0; k < nidx; k++)
        {
            if (args[first + k] is not Fixnum fi)
                throw new LispErrorException(new LispTypeError(
                    $"{what}: index must be integer", args[first + k], Startup.Sym("INTEGER")));
            long i = fi.Value;
            if ((ulong)i >= (ulong)dims[k])
                throw IndexError(what, i, dims[k], $"axis {k}");
            idx = idx * dims[k] + (int)i;
        }
        return idx;
    }

    public static LispObject Aref(LispObject array, LispObject index)
    {
        // Tight fast path: plain 1D LispVector with Fixnum index. A null
        // _dimensions is exactly "rank 1", so a multi-dimensional array handed
        // a single subscript drops to the slow path and is rejected there.
        if (array is LispVector v && index is Fixnum f
            && v._displacedTo == null && v._bitData == null && v._dimensions == null)
        {
            if (v._numData != null)
            {
                if ((ulong)f.Value < (ulong)v._numLen)
                    return v.NumBox((int)f.Value);
            }
            else
            {
                // Compared as a long: narrowing first would wrap a subscript
                // past int into range and read the wrong element.
                long li = f.Value;
                if ((ulong)li < (ulong)v._elements.Length)
                    return v._elements[(int)li] ?? Nil.Instance;
            }
        }
        return ArefSlow(array, index);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject ArefSlow(LispObject array, LispObject index)
    {
        int idx = IntArg("AREF", "index", index);
        if (TryDotNetArray(array, out var narr))
            return DotNetToLisp(narr.GetValue(idx));
        if (array is LispVector v)
        {
            if (v._dimensions != null && v._dimensions.Length != 1)
                throw new LispErrorException(new LispProgramError(
                    $"AREF: 1 index for rank-{v._dimensions.Length} array"));
            if (idx < 0 || idx >= v.Capacity)
                throw IndexError("AREF", idx, v.Capacity, "array");
            return v.GetElement(idx);
        }
        if (array is LispString s)
        {
            if (idx < 0 || idx >= s.Length)
                throw IndexError("AREF", idx, s.Length, "string");
            return LispChar.Make(s[idx]);
        }
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    /// <summary>BIT / SBIT with one subscript: what ArefMulti answers for the
    /// two-element argument list, without building it. A packed, undisplaced
    /// bit vector is read directly; anything else takes ArefMulti as before.
    /// Called through the function object, e.g. by code that names SBIT as a
    /// global function: that call used to allocate the argument array and the
    /// dimension list on every bit read.</summary>
    public static LispObject Bit1(LispObject array, LispObject index)
    {
        if (array is LispVector v && index is Fixnum f && v._bitData is { } bits
            && v._displacedTo == null && v._dimensions == null
            && (ulong)f.Value < (ulong)v.Capacity)
            return Fixnum.Make((long)((bits[(int)(f.Value >> 6)] >> (int)(f.Value & 63)) & 1));
        return ArefMulti(new[] { array, index });
    }

    public static LispObject ArefMulti(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("AREF: requires array argument"));
        var array = args[0];
        if (array is LispVector v)
        {
            int idx = FlatIndex("AREF", v, args, 1, args.Length - 1);
            if (idx < 0 || idx >= v.Capacity)
                throw IndexError("AREF", idx, v.Capacity, "array");
            return v.GetElement(idx);
        }
        if (array is LispString s && args.Length == 2)
            return Aref(array, args[1]);
        if (TryDotNetArray(array, out var narr))
        {
            var indices = new int[args.Length - 1];
            for (int k = 0; k < indices.Length; k++)
                indices[k] = IntArg("AREF", "index", args[k + 1]);
            return DotNetToLisp(narr.GetValue(indices));
        }
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    public static LispObject ArefSetMulti(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("(SETF AREF): requires array and value arguments"));
        var array = args[0];
        var value = args[args.Length - 1];
        if (array is LispVector v)
        {
            int idx = FlatIndex("(SETF AREF)", v, args, 1, args.Length - 2);
            if (idx < 0 || idx >= v.Capacity)
                throw IndexError("(SETF AREF)", idx, v.Capacity, "array");
            v.SetElement(idx, value);
            return value;
        }
        if (array is LispString ls)
        {
            int nidx = args.Length - 2;
            if (nidx != 1)
                throw new LispErrorException(new LispProgramError($"(SETF AREF): string requires exactly 1 index, got {nidx}"));
            int i = IntArg("(SETF AREF)", "index", args[1]);
            if (i < 0 || i >= ls.Length)
                throw IndexError("(SETF AREF)", i, ls.Length, "string");
            if (value is not LispChar ch)
                throw new LispErrorException(new LispTypeError("(SETF AREF): value must be a character for string", value, Startup.Sym("CHARACTER")));
            ls[i] = ch.Value;
            return value;
        }
        if (TryDotNetArray(array, out var narr))
        {
            var indices = new int[args.Length - 2];
            for (int k = 0; k < indices.Length; k++)
                indices[k] = IntArg("(SETF AREF)", "index", args[k + 1]);
            narr.SetValue(LispToDotNet(value, narr.GetType().GetElementType()!), indices);
            return value;
        }
        throw new LispErrorException(new LispTypeError("(SETF AREF): not a vector", array));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet(LispObject array, LispObject index, LispObject value)
    {
        if (array is LispVector v && index is Fixnum f
            && v._displacedTo == null && v._bitData == null && v._dimensions == null)
        {
            if (v._numData != null)
            {
                if ((ulong)f.Value < (ulong)v._numLen && v.TryNumStore((int)f.Value, value))
                    return value;
            }
            else
            {
                // Compared as a long, for the reason AREF's read path is.
                long li = f.Value;
                if ((ulong)li < (ulong)v._elements.Length)
                {
                    v._elements[(int)li] = value;
                    return value;
                }
            }
        }
        return ArefSetSlow(array, index, value);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject ArefSetSlow(LispObject array, LispObject index, LispObject value)
    {
        int idx = IntArg("(SETF AREF)", "index", index);
        if (TryDotNetArray(array, out var narr))
        {
            narr.SetValue(LispToDotNet(value, narr.GetType().GetElementType()!), idx);
            return value;
        }
        if (array is LispVector v)
        {
            if (v._dimensions != null && v._dimensions.Length != 1)
                throw new LispErrorException(new LispProgramError(
                    $"(SETF AREF): 1 index for rank-{v._dimensions.Length} array"));
            if (idx < 0 || idx >= v.Capacity)
                throw IndexError("(SETF AREF)", idx, v.Capacity, "array");
            v.SetElement(idx, value);
            return value;
        }
        if (array is LispString ls)
        {
            if (value is not LispChar ch)
                throw new LispErrorException(new LispTypeError("(SETF AREF): value must be a character for string", value, Startup.Sym("CHARACTER")));
            if (idx < 0 || idx >= ls.Length)
                throw IndexError("(SETF AREF)", idx, ls.Length, "string");
            ls[idx] = ch.Value;
            return value;
        }
        throw new LispErrorException(new LispTypeError("(SETF AREF): not a vector", array));
    }

    /// <summary>Specialized 2D aref - avoids args array allocation.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Aref2D(LispObject array, LispObject idx0, LispObject idx1)
    {
        // Tight fast path: the overwhelming majority of 2D aref calls hit a
        // non-displaced, non-bit LispVector with Fixnum indices. Inline this
        // so JIT can hoist dim/array loads across repeated calls in hot loops.
        // Each subscript is range-checked against its own axis (as the raw-long
        // variant below does): a subscript past its axis can still land inside
        // the flat storage, which would silently read a neighbouring element.
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)f0.Value < (ulong)v._dimensions[0]
            && (ulong)f1.Value < (ulong)v._dimensions[1])
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int idx = i0 * v._dimensions[1] + i1;
            if (v._numData != null)
            {
                if ((uint)idx < (uint)v._numLen)
                    return v.NumBox(idx);
            }
            else if ((uint)idx < (uint)v._elements.Length)
                return v._elements[idx] ?? Nil.Instance;
        }
        return Aref2DSlow(array, idx0, idx1);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject Aref2DSlow(LispObject array, LispObject idx0, LispObject idx1)
    {
        if (array is LispVector v)
            return v.GetElement(FlatIndex("AREF", v, new[] { idx0, idx1 }, 0, 2));
        if (TryDotNetArray(array, out var narr))
            return DotNetToLisp(narr.GetValue(IntArg("AREF", "index", idx0),
                                              IntArg("AREF", "index", idx1)));
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    /// <summary>Specialized 2D aref setter - avoids args array allocation.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet2D(LispObject array, LispObject idx0, LispObject idx1, LispObject value)
    {
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)f0.Value < (ulong)v._dimensions[0]
            && (ulong)f1.Value < (ulong)v._dimensions[1])
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int idx = i0 * v._dimensions[1] + i1;
            if (v._numData != null)
            {
                if ((uint)idx < (uint)v._numLen && v.TryNumStore(idx, value))
                    return value;
            }
            else if ((uint)idx < (uint)v._elements.Length)
            {
                v._elements[idx] = value;
                return value;
            }
        }
        return ArefSet2DSlow(array, idx0, idx1, value);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject ArefSet2DSlow(LispObject array, LispObject idx0, LispObject idx1, LispObject value)
    {
        if (array is LispVector v)
        {
            int idx = FlatIndex("(SETF AREF)", v, new[] { idx0, idx1 }, 0, 2);
            // Fast path: direct element access for non-displaced, non-bit,
            // non-numeric-backed arrays
            if (v._displacedTo == null && v._bitData == null && v._numData == null)
            {
                v._elements[idx] = value;
                return value;
            }
            v.SetElement(idx, value);
            return value;
        }
        if (TryDotNetArray(array, out var narr))
        {
            narr.SetValue(LispToDotNet(value, narr.GetType().GetElementType()!),
                          IntArg("(SETF AREF)", "index", idx0),
                          IntArg("(SETF AREF)", "index", idx1));
            return value;
        }
        throw new LispErrorException(new LispTypeError("(SETF AREF): not an array", array));
    }

    /// <summary>Specialized 3D aref - avoids args array allocation.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Aref3D(LispObject array, LispObject idx0, LispObject idx1, LispObject idx2)
    {
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1 && idx2 is Fixnum f2
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)f0.Value < (ulong)v._dimensions[0]
            && (ulong)f1.Value < (ulong)v._dimensions[1]
            && (ulong)f2.Value < (ulong)v._dimensions[2])
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int i2 = (int)f2.Value;
            int idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if (v._numData != null)
            {
                if ((uint)idx < (uint)v._numLen)
                    return v.NumBox(idx);
            }
            else if ((uint)idx < (uint)v._elements.Length)
                return v._elements[idx] ?? Nil.Instance;
        }
        return Aref3DSlow(array, idx0, idx1, idx2);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject Aref3DSlow(LispObject array, LispObject idx0, LispObject idx1, LispObject idx2)
    {
        if (array is LispVector v)
            return v.GetElement(FlatIndex("AREF", v, new[] { idx0, idx1, idx2 }, 0, 3));
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    /// <summary>Specialized 3D aref setter - avoids args array allocation.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet3D(LispObject array, LispObject idx0, LispObject idx1, LispObject idx2, LispObject value)
    {
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1 && idx2 is Fixnum f2
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)f0.Value < (ulong)v._dimensions[0]
            && (ulong)f1.Value < (ulong)v._dimensions[1]
            && (ulong)f2.Value < (ulong)v._dimensions[2])
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int i2 = (int)f2.Value;
            int idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if (v._numData != null)
            {
                if ((uint)idx < (uint)v._numLen && v.TryNumStore(idx, value))
                    return value;
            }
            else if ((uint)idx < (uint)v._elements.Length)
            {
                v._elements[idx] = value;
                return value;
            }
        }
        return ArefSet3DSlow(array, idx0, idx1, idx2, value);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject ArefSet3DSlow(LispObject array, LispObject idx0, LispObject idx1, LispObject idx2, LispObject value)
    {
        if (array is LispVector v)
        {
            int idx = FlatIndex("(SETF AREF)", v, new[] { idx0, idx1, idx2 }, 0, 3);
            if (v._displacedTo == null && v._bitData == null && v._numData == null)
            {
                v._elements[idx] = value;
                return value;
            }
            v.SetElement(idx, value);
            return value;
        }
        throw new LispErrorException(new LispTypeError("(SETF AREF): not an array", array));
    }

    // --- Native (raw long) index variants ---------------------------------
    // Called by compiled code when the index expressions are statically known
    // to be fixnums (e.g. Int64-slot loop counters): the index arrives as a
    // raw long, skipping the Fixnum box on the caller side and the type-check/
    // unbox here. Fast paths mirror the boxed variants; anything unusual
    // (displaced, bit-vector, string, .NET array, out of range) re-boxes the
    // indices and defers to the existing slow paths so behavior is identical.

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefL(LispObject array, long index)
    {
        if (array is LispVector v && v._displacedTo == null && v._dimensions == null)
        {
            if (v._bitData == null && v._numData == null
                && (ulong)index < (ulong)v._elements.Length)
                return v._elements[(int)index] ?? Nil.Instance;
            // Unboxed numeric backing: read the raw value, box once (small
            // integers hit the Fixnum cache; float kinds box to a float).
            if (v._numData != null && (ulong)index < (ulong)v._numLen)
                return v.NumBox((int)index);
        }
        return ArefSlow(array, Fixnum.Make(index));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSetL(LispObject array, long index, LispObject value)
    {
        if (array is LispVector v && v._displacedTo == null && v._dimensions == null)
        {
            if (v._bitData == null && v._numData == null
                && (ulong)index < (ulong)v._elements.Length)
            {
                v._elements[(int)index] = value;
                return value;
            }
            if (v._numData != null && (ulong)index < (ulong)v._numLen
                && v.TryNumStore((int)index, value))
                return value;
        }
        return ArefSetSlow(array, Fixnum.Make(index), value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Aref2DL(LispObject array, long i0, long i1)
    {
        if (array is LispVector v
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1])
        {
            long idx = i0 * v._dimensions[1] + i1;
            if (v._numData != null)
            {
                if ((ulong)idx < (ulong)v._numLen)
                    return v.NumBox((int)idx);
            }
            else if ((ulong)idx < (ulong)v._elements.Length)
                return v._elements[(int)idx] ?? Nil.Instance;
        }
        return Aref2DSlow(array, Fixnum.Make(i0), Fixnum.Make(i1));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet2DL(LispObject array, long i0, long i1, LispObject value)
    {
        if (array is LispVector v
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1])
        {
            long idx = i0 * v._dimensions[1] + i1;
            if (v._numData != null)
            {
                if ((ulong)idx < (ulong)v._numLen && v.TryNumStore((int)idx, value))
                    return value;
            }
            else if ((ulong)idx < (ulong)v._elements.Length)
            {
                v._elements[(int)idx] = value;
                return value;
            }
        }
        return ArefSet2DSlow(array, Fixnum.Make(i0), Fixnum.Make(i1), value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject Aref3DL(LispObject array, long i0, long i1, long i2)
    {
        if (array is LispVector v
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1]
            && (ulong)i2 < (ulong)v._dimensions[2])
        {
            long idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if (v._numData != null)
            {
                if ((ulong)idx < (ulong)v._numLen)
                    return v.NumBox((int)idx);
            }
            else if ((ulong)idx < (ulong)v._elements.Length)
                return v._elements[(int)idx] ?? Nil.Instance;
        }
        return Aref3DSlow(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(i2));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet3DL(LispObject array, long i0, long i1, long i2, LispObject value)
    {
        if (array is LispVector v
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1]
            && (ulong)i2 < (ulong)v._dimensions[2])
        {
            long idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if (v._numData != null)
            {
                if ((ulong)idx < (ulong)v._numLen && v.TryNumStore((int)idx, value))
                    return value;
            }
            else if ((ulong)idx < (ulong)v._elements.Length)
            {
                v._elements[(int)idx] = value;
                return value;
            }
        }
        return ArefSet3DSlow(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(i2), value);
    }

    // --- Raw-long-value variants for numeric-backed arrays -----------------
    // Called by compiled code when the compiler has PROVEN (from the let-init
    // make-array form) that the array local is numeric-backed with a bounded
    // integer element type: the element value crosses the boundary as a raw
    // long, so a hot loop like (setf (aref a i j) (+ (aref b i j) (aref c i j)))
    // runs without any Fixnum boxing at all. The fast path requires the
    // numeric backing; anything else (adjusted to displaced, bit-packed after
    // a [0,1] upgrade, plain boxed) takes the boxed entry and unboxes; the
    // inferred element type guarantees the value is a fixnum, and a violation
    // surfaces as a loud InvalidCast rather than a silent wrong value.

    /// <summary>A subscript as a raw long, with the type error CL requires when it is
    /// not an integer. The compiler uses this where it knows the array's element
    /// storage -- so the element can be read or written unboxed -- but not that the
    /// subscript expression is fixnum-typed, which is the ordinary case for an
    /// undeclared loop variable. A plain castclass would report the violation as a
    /// .NET InvalidCastException instead, and (AREF A 'X) has to say what AREF says.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long IndexL(LispObject index)
        => index is Fixnum f ? f.Value
           : throw new LispErrorException(new LispTypeError("AREF: index must be integer", index));

    // --- Hoisted element storage -----------------------------------------
    //
    // Two different answers for two different situations, and conflating them
    // costs one thing or the other:
    //
    //   the right element storage, but not pinnable (adjustable, fill-pointered,
    //     displaced) -> answer NULL and let each access run the per-element
    //     helper. Such a vector is not a SIMPLE-ARRAY (TYPEP says so too: it
    //     reads LispVector.IsSimple), so the declaration is false. These entries
    //     are what (safety 0) calls, and there the declaration is trusted rather
    //     than checked, so they answer null and the helper reads correct values.
    //     Above safety 0 the compiler calls the *Checked entries below, which
    //     turn that null into a TYPE-ERROR. Measured before the null arm
    //     existed: ADJUST-ARRAY replaced the buffer, the write inside the
    //     function went to the replaced one and was lost, and a read inside
    //     disagreed with a read outside about the same element.
    //     VECTOR-PUSH-EXTEND does the same to a fill-pointered vector and is far
    //     commoner.
    //
    //   declaration FALSE, not that array at all -> THROW, exactly as before.
    //     That check moved here from the per-element path on purpose: it used to
    //     be re-checked per element, which let a false declaration read the right
    //     values off the boxed path and be quietly absorbed. Answering null for
    //     this case too would walk that back, and it would have looked like an
    //     improvement in the diff.
    //
    // CLHS says a simple array is neither displaced nor adjustable nor
    // fill-pointered, so its storage cannot be replaced while the binding lives.
    // BackingPinned is exactly that test.
    private static string Typep2Name(LispObject o) =>
        o is LispVector lv
            ? (lv.IsDisplaced ? $"a displaced array of {lv.ElementTypeName}"
               : $"an array of {lv.ElementTypeName}")
            : o.ToString();

    private static Exception BackingTypeError(LispObject array, string elementType)
    {
        var expected = new Cons(Startup.Sym("SIMPLE-ARRAY"),
            new Cons(Startup.Sym(elementType),
                new Cons(new Cons(Startup.Sym("*"), Nil.Instance), Nil.Instance)));
        return new LispErrorException(new LispTypeError(
            $"declared (SIMPLE-ARRAY {elementType} (*)), got {Typep2Name(array)}",
            array, expected));
    }

    /// <summary>Whether V's element storage stays put for as long as a binding
    /// lives. Not displaced because a displaced vector's elements are not its
    /// own; not fill-pointered and not adjustable because VECTOR-PUSH-EXTEND and
    /// ADJUST-ARRAY both REPLACE the buffer object (they assign _numData), which
    /// leaves a hoisted reference reading storage nothing else can see. This is
    /// the same test as CLHS simplicity, so it is LispVector.IsSimple.</summary>
    [System.Runtime.CompilerServices.MethodImpl(
        System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    private static bool BackingPinned(LispVector v) => v.IsSimple;


    /// <summary>The char[] storage of a string declared SIMPLE-STRING, or null.
    ///
    /// NEVER signals, and that is not the array entries' contract. A declaration
    /// this declines can be perfectly true: MAKE-ARRAY with :element-type
    /// CHARACTER and no fill pointer builds a **LispVector**, not a LispString,
    /// and it is a simple string under CLHS and under SIMPLE-STRING-P. So a
    /// conforming program can pass one to a
    /// SIMPLE-STRING parameter, and throwing here would break it.
    ///
    /// That LispVector's own char[] is taken too when the vector is simple and
    /// rank 1. It is pinned by the same argument the integer entries above rest
    /// on: after construction _numData is reassigned only by ADJUST-ARRAY on an
    /// adjustable array (resizing, or leaving displaced storage) and by the
    /// growth in VECTOR-PUSH / VECTOR-PUSH-EXTEND, which needs a fill pointer,
    /// and IsSimple excludes both. None
    /// of the operations that could make a simple vector non-simple later can
    /// reach it either (SETF FILL-POINTER and VECTOR-POP require an existing
    /// fill pointer). An adjustable, fill-pointered or displaced character
    /// vector answers null, as before, and its reads take the typed call.
    ///
    /// Only the char[] backing is taken. A LispString holding a System.String
    /// answers null rather than materializing: RAWCHARS would convert it
    /// permanently, and from then on every VALUE read -- STRING=, STRING&lt;,
    /// printing -- allocates a fresh System.String for the rest of the image's
    /// life. Paying that at every binding of a declared parameter, to speed up
    /// scans that may not happen, is not a trade this can make on the caller's
    /// behalf.
    ///
    /// What makes the hoist sound once the buffer is in hand: _chars is
    /// write-once. It is assigned in exactly two places, the char[] constructor
    /// and EnsureMutable under a null guard, and ADJUST-ARRAY cannot reach a
    /// LispString at all -- it rejects a non-LispVector outright. So unlike a
    /// LispVector, whose _numData four paths reassign, the array this returns
    /// cannot be swapped underneath the binding, and writes through SCHAR mutate
    /// that same array in place where a hoisted reference sees them.</summary>
    public static char[]? BackingChars(LispObject s)
    {
        if (s is LispString ls) return ls.CharsOrNull;
        return SimpleCharVectorData(s);
    }

    /// <summary>The char[] of a simple rank-1 character LispVector, else null.
    /// Shared by BackingChars and BackingCharsChecked; see BackingChars for why
    /// the buffer cannot be swapped while a binding holds it.</summary>
    private static char[]? SimpleCharVectorData(LispObject s)
        => s is LispVector v && v._dimensions == null && v.IsSimple
           && v._numData is char[] cv ? cv : null;

    /// <summary>The int64 element buffer of a vector declared
    /// (simple-array fixnum (*)) / (simple-array (signed-byte 64) (*)).</summary>
    public static long[]? BackingI64(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v)) return null;
            if (v._numData is long[] d) return d;
            if (v.IsForeignWidthKind) return null;
        }
        throw BackingTypeError(array, "FIXNUM");
    }

    public static int[]? BackingI32(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v)) return null;
            if (v._numData is int[] d) return d;
            if (v.IsForeignWidthKind) return null;
        }
        throw BackingTypeError(array, "SIGNED-BYTE-32");
    }

    public static ushort[]? BackingU16(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v)) return null;
            if (v._numData is ushort[] d) return d;
        }
        throw BackingTypeError(array, "UNSIGNED-BYTE-16");
    }

    public static byte[]? BackingU8(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v)) return null;
            if (v._numData is byte[] d) return d;
        }
        throw BackingTypeError(array, "UNSIGNED-BYTE-8");
    }

    // --- The same fetches above SAFETY 0 ---------------------------------
    //
    // The entries above answer null for an array that is the declared kind but
    // not simple (adjustable, fill-pointered, displaced), and each access then
    // falls back to the per-element helper. (safety 0) keeps that. Above it the
    // declaration is an assertion, so the compiler calls these instead, which
    // turn exactly that null into a TYPE-ERROR. The branch sits on the path the
    // plain fetch already takes when it declines, so a true declaration pays
    // nothing extra.

    private static string NotSimpleDescription(LispObject o)
    {
        if (o is not LispVector v) return o.ToString();
        var what = new System.Collections.Generic.List<string>();
        if (v.IsAdjustable) what.Add("adjustable");
        if (v.HasFillPointer) what.Add("fill-pointered");
        if (v.IsDisplaced) what.Add("displaced");
        return $"a non-simple ({string.Join(", ", what)}) array of {v.ElementTypeName}";
    }

    private static Exception NotSimpleDeclError(LispObject datum, LispObject expected,
                                                string? varName)
    {
        var who = varName == null ? "" : $"{varName} is ";
        return new LispErrorException(new LispTypeError(
            $"{who}declared {expected}, got {NotSimpleDescription(datum)}",
            datum, expected));
    }

    private static LispObject SimpleVectorOf(LispObject elementType) =>
        new Cons(Startup.Sym("SIMPLE-ARRAY"),
            new Cons(elementType,
                new Cons(new Cons(Startup.Sym("*"), Nil.Instance), Nil.Instance)));

    private static LispObject SizedByte(string head, int width) =>
        new Cons(Startup.Sym(head), new Cons(Fixnum.Make(width), Nil.Instance));

    // Written out rather than wrapping the plain entries, so a true declaration
    // runs exactly the tests the plain entry runs, with no extra call layer.

    public static long[]? BackingI64Checked(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v))
                throw NotSimpleDeclError(array, SimpleVectorOf(SizedByte("SIGNED-BYTE", 64)), null);
            if (v._numData is long[] d) return d;
            if (v.IsForeignWidthKind) return null;
        }
        throw BackingTypeError(array, "FIXNUM");
    }

    public static int[]? BackingI32Checked(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v))
                throw NotSimpleDeclError(array, SimpleVectorOf(SizedByte("SIGNED-BYTE", 32)), null);
            if (v._numData is int[] d) return d;
            if (v.IsForeignWidthKind) return null;
        }
        throw BackingTypeError(array, "SIGNED-BYTE-32");
    }

    public static ushort[] BackingU16Checked(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v))
                throw NotSimpleDeclError(array, SimpleVectorOf(SizedByte("UNSIGNED-BYTE", 16)), null);
            if (v._numData is ushort[] d) return d;
        }
        throw BackingTypeError(array, "UNSIGNED-BYTE-16");
    }

    public static byte[] BackingU8Checked(LispObject array)
    {
        if (array is LispVector v && v._dimensions == null)
        {
            if (!BackingPinned(v))
                throw NotSimpleDeclError(array, SimpleVectorOf(SizedByte("UNSIGNED-BYTE", 8)), null);
            if (v._numData is byte[] d) return d;
        }
        throw BackingTypeError(array, "UNSIGNED-BYTE-8");
    }

    /// <summary>BackingChars above SAFETY 0. Still answers null for everything
    /// BackingChars declines that can be a true SIMPLE-STRING (a LispString
    /// holding a System.String), and for a value that is not a string at all,
    /// which BackingChars never diagnosed. Only the case that is certainly a
    /// false declaration, a non-simple array, signals.</summary>
    public static char[]? BackingCharsChecked(LispObject s)
    {
        if (s is LispString ls) return ls.CharsOrNull;
        if (s is LispVector v && !v.IsSimple)
            throw NotSimpleDeclError(s, Startup.Sym("SIMPLE-STRING"), null);
        return SimpleCharVectorData(s);
    }

    /// <summary>True when O is an array that is not simple: the test a binding
    /// declared with a SIMPLE-* array type runs above SAFETY 0 when nothing
    /// hoisted its storage (the fetches above answer the same question on their
    /// own). A LispString is always simple.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsNonSimpleArray(LispObject o) => o is LispVector v && !v.IsSimple;

    /// <summary>The TYPE-ERROR for a binding declared SPEC whose value DATUM is a
    /// non-simple array. Reached only after IsNonSimpleArray answered true, so
    /// the constant and the name it takes cost nothing on a true declaration.
    /// </summary>
    public static void SignalDeclaredNotSimple(LispObject datum, LispObject spec, string varName)
        => throw NotSimpleDeclError(datum, spec, varName);

    /// <summary>The element-type violation an out-of-width store would commit,
    /// as the error NumSet raises for the same value on the boxed path. The
    /// hoisted store path has no LispVector to ask, so the check is here.</summary>
    public static long CheckStoreU8(long v)
        => (ulong)v <= byte.MaxValue ? v
           : throw new LispErrorException(new LispTypeError(
               $"element value {v} does not fit (UNSIGNED-BYTE 8)", Fixnum.Make(v)));

    public static long CheckStoreU16(long v)
        => (ulong)v <= ushort.MaxValue ? v
           : throw new LispErrorException(new LispTypeError(
               $"element value {v} does not fit (UNSIGNED-BYTE 16)", Fixnum.Make(v)));

    public static long CheckStoreI32(long v)
        => v >= int.MinValue && v <= int.MaxValue ? v
           : throw new LispErrorException(new LispTypeError(
               $"element value {v} does not fit (SIGNED-BYTE 32)", Fixnum.Make(v)));

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ArefNumL(LispObject array, long index)
    {
        if (array is LispVector v && v.IsRawNumKind && v._displacedTo == null
            && v._dimensions == null && (ulong)index < (ulong)v._numLen)
            return v.NumGet((int)index);
        return ((Fixnum)Aref(array, Fixnum.Make(index))).Value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ArefSetNumL(LispObject array, long index, long value)
    {
        if (array is LispVector v && v.IsRawNumKind && v._displacedTo == null
            && v._dimensions == null && (ulong)index < (ulong)v._numLen)
        {
            v.NumSet((int)index, value);
            return value;
        }
        ArefSet(array, Fixnum.Make(index), Fixnum.Make(value));
        return value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ArefNum2DL(LispObject array, long i0, long i1)
    {
        if (array is LispVector v && v.IsRawNumKind && v._displacedTo == null
            && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1])
        {
            long idx = i0 * v._dimensions[1] + i1;
            if ((ulong)idx < (ulong)v._numLen)
                return v.NumGet((int)idx);
        }
        return ((Fixnum)Aref2D(array, Fixnum.Make(i0), Fixnum.Make(i1))).Value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ArefSetNum2DL(LispObject array, long i0, long i1, long value)
    {
        if (array is LispVector v && v.IsRawNumKind && v._displacedTo == null
            && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1])
        {
            long idx = i0 * v._dimensions[1] + i1;
            if ((ulong)idx < (ulong)v._numLen)
            {
                v.NumSet((int)idx, value);
                return value;
            }
        }
        ArefSet2D(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(value));
        return value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ArefNum3DL(LispObject array, long i0, long i1, long i2)
    {
        if (array is LispVector v && v.IsRawNumKind && v._displacedTo == null
            && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1]
            && (ulong)i2 < (ulong)v._dimensions[2])
        {
            long idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if ((ulong)idx < (ulong)v._numLen)
                return v.NumGet((int)idx);
        }
        return ((Fixnum)Aref3D(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(i2))).Value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ArefSetNum3DL(LispObject array, long i0, long i1, long i2, long value)
    {
        if (array is LispVector v && v.IsRawNumKind && v._displacedTo == null
            && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1]
            && (ulong)i2 < (ulong)v._dimensions[2])
        {
            long idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if ((ulong)idx < (ulong)v._numLen)
            {
                v.NumSet((int)idx, value);
                return value;
            }
        }
        ArefSet3D(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(i2), Fixnum.Make(value));
        return value;
    }

    // --- Raw-double-value variants for float-backed array locals -----------
    // Mirror the ArefNum*L (raw long) family for float element types: emitted
    // when the compiler has PROVEN (from the make-array :element-type) that the
    // array local is float-backed (float[] / double[]). The element crosses the
    // boundary as a raw double, so a hot loop like
    // (setf (aref c i) (+ (aref a i) (aref b i))) on double-float arrays runs
    // with zero SingleFloat/DoubleFloat boxing. single-float backing widens to
    // double on read and narrows on store (both exact). The fast path requires
    // float numeric backing (_numKind >= 5); anything else (adjusted to
    // displaced, boxed) takes the boxed entry and coerces: a violation of the
    // inferred element type surfaces loudly rather than as a silent wrong value.

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static double ToDoubleElement(LispObject o) => o switch
    {
        DoubleFloat df => df.Value,
        SingleFloat sf => sf.Value,
        Fixnum fx => fx.Value,
        _ => throw new LispErrorException(new LispTypeError("AREF: element is not a real", o)),
    };

    // Box a raw double back to the element box type of a float-backed array for
    // the rare fallback store (e.g. displaced after adjust-array): single-float
    // backing gets a SingleFloat, double gets a DoubleFloat.
    private static LispObject BoxFloatElement(LispObject array, double value) =>
        array is LispVector fv &&
        (fv._numKind == 5 || fv.ElementTypeName is "SINGLE-FLOAT" or "SHORT-FLOAT")
            ? new SingleFloat((float)value)
            : new DoubleFloat(value);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double ArefNumD(LispObject array, long index)
    {
        if (array is LispVector v && v.IsFloatNumKind
            && v._displacedTo == null && v._dimensions == null
            && (ulong)index < (ulong)v._numLen)
            return v.NumGetF((int)index);
        return ToDoubleElement(Aref(array, Fixnum.Make(index)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double ArefSetNumD(LispObject array, long index, double value)
    {
        if (array is LispVector v && v.IsFloatNumKind
            && v._displacedTo == null && v._dimensions == null
            && (ulong)index < (ulong)v._numLen)
        {
            v.NumSetF((int)index, value);
            return value;
        }
        ArefSet(array, Fixnum.Make(index), BoxFloatElement(array, value));
        return value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double ArefNum2DD(LispObject array, long i0, long i1)
    {
        if (array is LispVector v && v.IsFloatNumKind
            && v._displacedTo == null && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1])
        {
            long idx = i0 * v._dimensions[1] + i1;
            if ((ulong)idx < (ulong)v._numLen)
                return v.NumGetF((int)idx);
        }
        return ToDoubleElement(Aref2D(array, Fixnum.Make(i0), Fixnum.Make(i1)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double ArefSetNum2DD(LispObject array, long i0, long i1, double value)
    {
        if (array is LispVector v && v.IsFloatNumKind
            && v._displacedTo == null && v._dimensions != null && v._dimensions.Length == 2
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1])
        {
            long idx = i0 * v._dimensions[1] + i1;
            if ((ulong)idx < (ulong)v._numLen)
            {
                v.NumSetF((int)idx, value);
                return value;
            }
        }
        ArefSet2D(array, Fixnum.Make(i0), Fixnum.Make(i1), BoxFloatElement(array, value));
        return value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double ArefNum3DD(LispObject array, long i0, long i1, long i2)
    {
        if (array is LispVector v && v.IsFloatNumKind
            && v._displacedTo == null && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1]
            && (ulong)i2 < (ulong)v._dimensions[2])
        {
            long idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if ((ulong)idx < (ulong)v._numLen)
                return v.NumGetF((int)idx);
        }
        return ToDoubleElement(Aref3D(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(i2)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double ArefSetNum3DD(LispObject array, long i0, long i1, long i2, double value)
    {
        if (array is LispVector v && v.IsFloatNumKind
            && v._displacedTo == null && v._dimensions != null && v._dimensions.Length == 3
            && (ulong)i0 < (ulong)v._dimensions[0] && (ulong)i1 < (ulong)v._dimensions[1]
            && (ulong)i2 < (ulong)v._dimensions[2])
        {
            long idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if ((ulong)idx < (ulong)v._numLen)
            {
                v.NumSetF((int)idx, value);
                return value;
            }
        }
        ArefSet3D(array, Fixnum.Make(i0), Fixnum.Make(i1), Fixnum.Make(i2), BoxFloatElement(array, value));
        return value;
    }

    // Binary vector-push-extend: avoids LispObject[] allocation for 2-arg case
    public static LispObject VectorPushExtend2(LispObject element, LispObject vector)
    {
        if (vector is not LispVector vec)
            throw new LispErrorException(new LispTypeError("VECTOR-PUSH-EXTEND: not a vector", vector));
        return Fixnum.Make(vec.VectorPushExtend(element, 0));
    }

    // Void variant for when result is discarded (avoids Fixnum.Make allocation)
    public static void VectorPushExtendVoid2(LispObject element, LispObject vector)
    {
        if (vector is not LispVector vec)
            throw new LispErrorException(new LispTypeError("VECTOR-PUSH-EXTEND: not a vector", vector));
        vec.VectorPushExtend(element, 0);
    }

    // Binary vector-push: avoids LispObject[] allocation
    public static LispObject VectorPush2(LispObject element, LispObject vector)
    {
        if (vector is not LispVector vec)
            throw new LispErrorException(new LispTypeError("VECTOR-PUSH: not a vector", vector));
        return vec.VectorPushCL(element);
    }

    // --- Struct operations ---

    /// <summary>
    /// Build a structure instance. SLOTS becomes the instance's slot storage, so the
    /// caller hands it over and must not keep writing to it: every caller builds the
    /// array for this call and drops it (the compiled %MAKE-STRUCT emits a fresh
    /// argument array per call, the dynamic entry passes a SubArray, and a C# call
    /// through the params overload gets a fresh array from the compiler). Copying it
    /// here instead cost a second array per structure created, which for a two-slot
    /// structure was a third of the allocation.
    /// </summary>
    public static LispObject MakeStruct(LispObject typeName, params LispObject[] slots)
    {
        if (typeName is not Symbol sym)
            throw new LispErrorException(new LispTypeError("MAKE-STRUCT: type name must be a symbol", typeName));
        return new LispStruct(sym, slots);
    }

    /// <summary>
    /// Fast struct slot access with raw int index (avoids Fixnum boxing).
    /// Used by compiler for constant-index struct accessors.
    /// </summary>
    /// <summary>Bits of a packed slot constant that hold the index; the rest
    /// hold the layout version the call site was compiled against.</summary>
    internal const int SlotVersionShift = 16;
    internal const int SlotIndexMask = (1 << SlotVersionShift) - 1;

    /// <summary>The layout entry for the slot a packed constant names, once the
    /// instance is confirmed to come from the definition the caller was
    /// compiled against.
    ///
    /// A compiled call site addresses a slot by position, so a structure
    /// redefined with its slots in a different order leaves every caller
    /// compiled against the old definition reading a valid index into a valid
    /// instance -- the wrong slot, silently. The version travels in the same
    /// constant as the index (0 packs to the index itself, so nothing that was
    /// never redefined pays for this) and the two have to agree.
    ///
    /// The entry carries the instance's version in the same bits, so the check
    /// is an XOR of two values the reader needed anyway: no second load, and
    /// the position comes back in the same register the caller goes on to
    /// use.</summary>
    /// <summary>The position of the slot's raw storage, if every field of the
    /// layout entry is what the caller expects: the version the call site was
    /// compiled against, and KIND (0 for a raw integer, SlotKindDouble for a
    /// raw double). Anything else comes back as a number past the end of the
    /// raw array, so the caller's range test rejects it -- no separate
    /// comparison for the version, and none for the kind.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int RawPosOf(LispStruct s, int packed, int kind)
        => s.LayoutEntry(packed & SlotIndexMask)
           ^ (packed & LispStruct.LayoutVersionMask) ^ kind;

    /// <summary>The layout entry for the slot a packed constant names, with the
    /// version checked. The slow paths use this: the fast ones fold the check
    /// into RawPosOf above and only come here to find out what went wrong.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int SlotEntry(LispStruct s, int packed)
    {
        int entry = s.LayoutEntry(packed & SlotIndexMask);
        if (((entry ^ packed) >> SlotVersionShift) != 0)
            throw StaleSlotAccess(s, packed >> SlotVersionShift);
        return entry;
    }

    /// <summary>The slot index inside a packed constant, version checked.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int SlotIndex(LispStruct s, int packed)
    {
        SlotEntry(s, packed);
        return packed & SlotIndexMask;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispErrorException StaleSlotAccess(LispStruct s, int want)
        => new LispErrorException(new LispError(
            $"structure {s.TypeName} was redefined (slot layout version {s.LayoutVersion}, "
            + $"this accessor was compiled against {want}); recompile the caller"));

    public static LispObject StructRefI(LispObject obj, int packed)
    {
        if (obj is LispStruct s)
        {
            return s.GetSlot(SlotIndex(s, packed));
        }
        int idx = packed & SlotIndexMask;
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
            return inst.Slots[idx] ?? Nil.Instance;
        // SBCL treats packages as structs; map slot indices to Package properties
        if (obj is Package pkg)
            return PackageStructRef(pkg, idx);
        {
            var sv = obj?.ToString() ?? "nil";
            var st = new System.Diagnostics.StackTrace(false);
            var frames = new System.Text.StringBuilder();
            for (int i = 1; i < Math.Min(st.FrameCount, 8); i++) {
                var f = st.GetFrame(i);
                var m = f?.GetMethod();
                if (m != null) frames.Append($"|{m.DeclaringType?.Name}.{m.Name}");
            }
            throw new LispErrorException(new LispTypeError($"STRUCT-REF: not a structure (idx={idx}, type={obj?.GetType().Name ?? "null"}, val={(sv.Length > 60 ? sv[..60] : sv)}) stack={frames}", obj));
        }
    }

    /// <summary>
    /// A struct slot holding a fixnum, read as a raw int64 -- the counterpart of
    /// ArefNumL for a structure. The ordinary path hands back a LispObject, which
    /// a fixnum-declared caller then unwraps and unboxes: two more calls per read
    /// for a value that was a long all along.
    ///
    /// Anything that is not a plain LispStruct slot holding a Fixnum goes through
    /// StructRefI, so a non-structure, a wrong index or a non-fixnum slot value
    /// reports exactly what it reported before.
    /// </summary>
    /// <summary>A structure slot declared DOUBLE-FLOAT, read as a raw double.
    /// The counterpart of StructRefL for the other raw kind.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double StructRefD(LispObject obj, int packed)
    {
        if (obj is LispStruct s && (uint)(packed & SlotIndexMask) < (uint)s.SlotCount
            && s.TryRawLong(RawPosOf(s, packed, LispStruct.SlotKindDouble), out long bits))
            return BitConverter.Int64BitsToDouble(bits);
        return ((DoubleFloat)StructRefI(obj, packed)).Value;
    }

    /// <summary>A double stored into a structure slot without boxing it.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double StructSetD(LispObject obj, int packed, double value)
    {
        if (obj is LispStruct s && (uint)(packed & SlotIndexMask) < (uint)s.SlotCount
            && s.TrySetRaw(RawPosOf(s, packed, LispStruct.SlotKindDouble),
                           BitConverter.DoubleToInt64Bits(value)))
            return value;
        StructSetI(obj, packed, new DoubleFloat(value));
        return value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long StructRefL(LispObject obj, int packed)
    {
        if (obj is LispStruct s && (uint)(packed & SlotIndexMask) < (uint)s.SlotCount)
        {
            // A raw slot is already an int64: no Fixnum in between, which is the
            // whole point of the raw storage.
            if (s.TryRawLong(RawPosOf(s, packed, LispStruct.RawLong), out long raw))
                return raw;
            if (s.GetSlot(SlotIndex(s, packed)) is Fixnum f) return f.Value;
        }
        return ((Fixnum)StructRefI(obj, packed)).Value;
    }

    /// <summary>
    /// A fixnum stored into a struct slot without boxing it first, returning the
    /// value so (SETF (accessor x) v) still answers v. The box was the whole
    /// allocation of a struct-writing loop: 23.8 bytes per iteration, one Fixnum
    /// per store above the small-integer cache.
    ///
    /// Only a plain LispStruct with an in-range index takes the fast path, and
    /// even then the value is boxed on the way in -- the slot holds LispObjects.
    /// What is saved is the box on the DISCARDED path: in statement position the
    /// caller emits no temp and no unwrap, and Fixnum.Make's cache covers the
    /// common small values. Everything else falls back to StructSetI, so slot
    /// type checking and error reporting are unchanged.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long StructSetL(LispObject obj, int packed, long value)
    {
        if (obj is LispStruct s && (uint)(packed & SlotIndexMask) < (uint)s.SlotCount)
        {
            // Into a raw slot the value goes as it is. The Fixnum.Make below is
            // what a declared-fixnum slot used to pay on every store even
            // though nothing ever looked at the object.
            if (s.TrySetRaw(RawPosOf(s, packed, LispStruct.RawLong), value))
                return value;
            s.SetSlot(SlotIndex(s, packed), Fixnum.Make(value));
            return value;
        }
        StructSetI(obj, packed, Fixnum.Make(value));
        return value;
    }

    /// <summary>
    /// The raw int64 array behind OBJ's slots, for a call site that is about to
    /// read or write several of them and already knows where they are -- or null
    /// when it cannot have it, which is every case the caller must then handle by
    /// doing what it does today.
    ///
    /// This is fetched ONCE per binding, not per access, and that is the whole
    /// point: StructRefL and StructSetL otherwise re-derive the array, the layout
    /// entry and the position from the object on every single slot touch, about
    /// twenty-five instructions where C# has one load. A caller holding this array
    /// and a constant position emits the load and nothing else.
    ///
    /// Sound because a LispStruct's raw storage cannot move under the caller:
    /// _longs and _layout are readonly and every assignment to either is in the
    /// constructor, so for one instance the array and the position map are fixed
    /// for its lifetime. That is what makes the version check a once-per-binding
    /// question rather than a per-access one -- and the version is exactly what
    /// says the caller's compiled-in positions still describe this instance.
    ///
    /// NAME and VERSION together are what say the caller's positions describe
    /// this object. The name has to be checked and not just the version: a
    /// position is read out of the DECLARED structure's layout, and a different
    /// structure at the same version maps the same slot index to a different
    /// raw position, so a false declaration would otherwise read a neighbouring
    /// slot in silence. One reference compare, once per binding, removes that.
    /// The check is exact, so a binding declared to hold a parent structure that
    /// is handed an :INCLUDE child falls back rather than hoisting -- slower,
    /// never wrong.
    ///
    /// Null is answered for a non-structure, for another structure, for the
    /// wrong layout version, and for an instance with no raw storage --
    /// including one that HAD raw slots but dropped them at construction because
    /// a value contradicted its declared type. That last case is why this must
    /// not signal: such an instance is perfectly usable and reads correctly
    /// through the ordinary path. So a null here is never an error, only an
    /// instruction to the caller to keep doing what it did before, which is also
    /// what reports the errors it reported before.
    /// </summary>
    public static long[]? StructRawBacking(LispObject obj, LispObject name, int version)
        => obj is LispStruct s
           && ReferenceEquals(s.TypeName, name)
           && s.LayoutVersion == version
            ? s._longs
            : null;

    /// <summary>The double a raw slot holds, given the bits read out of the raw
    /// array. A structure's double slots share the int64 array with its integer
    /// ones, so a hoisted backing serves both and only the reinterpretation
    /// differs.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double RawBitsToDouble(long bits) => BitConverter.Int64BitsToDouble(bits);

    /// <summary>The bits to store into a raw slot declared DOUBLE-FLOAT. Twin of
    /// RawBitsToDouble.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long RawDoubleToBits(double value) => BitConverter.DoubleToInt64Bits(value);

    /// <summary>Map SBCL's package struct slot indices to dotcl Package properties.</summary>
    private static LispObject PackageStructRef(Package pkg, int idx)
    {
        return idx switch
        {
            0 => new LispString(pkg.Name),  // %NAME
            1 => Nil.Instance,              // ID
            2 => new LispVector(new LispObject[] { new LispString(pkg.Name) }, "T"), // KEYS
            3 => new LispVector(Array.Empty<LispObject>(), "T"), // TABLES
            4 => Fixnum.Make(0),            // MRU-TABLE-INDEX
            5 => Nil.Instance,              // %USED-BY
            _ => Nil.Instance,              // other slots
        };
    }

    /// <summary>
    /// Fast struct slot set with raw int index (avoids Fixnum boxing).
    /// Used by compiler for constant-index struct setf accessors.
    /// </summary>
    /// <summary>Signal unless VALUE is of the declared type of a structure slot.
    ///
    /// DEFSTRUCT's :TYPE used to be dropped during macroexpansion, so a slot
    /// declared (:type fixnum) accepted a string in silence. CLHS 3.3.1 leaves a
    /// violated declaration undefined, and silence is a legal choice, but it is
    /// the least useful one: the writer said what belongs in the slot, and the
    /// cost of holding them to it is one TYPEP on a store.
    ///
    /// The compiler drops the call entirely under (safety 0), so this is the
    /// safety 1+ behavior only. Returns the value so it can wrap a store.</summary>
    /// <summary>Record which slots of a structure type are stored raw.
    /// POSITIONS is a list with one entry per slot: the index into the raw
    /// array, or -1 for a slot that stays boxed. Emitted by DEFSTRUCT at load
    /// time, and only when at least one slot qualifies.</summary>
    /// <summary>The shape this had before layouts carried a version. Kept
    /// because a shipped FASL calls what it was compiled against.</summary>
    public static LispObject StructRegisterLayout(LispObject typeName, LispObject positions)
        => StructRegisterLayout(typeName, positions, Fixnum.Make(0));

    public static LispObject StructRegisterLayout(LispObject typeName, LispObject positions,
                                                  LispObject version)
    {
        if (typeName is not Symbol sym)
            throw new LispErrorException(new LispTypeError(
                "%STRUCT-REGISTER-LAYOUT: type name must be a symbol", typeName));
        // Each entry is (POSITION . KIND); KIND is 0 for a raw integer and 1 for
        // a raw double. A boxed slot has position -1.
        var pos = new List<int>();
        var kind = new List<byte>();
        for (var cur = positions; cur is Cons c; cur = c.Cdr)
        {
            if (c.Car is Cons pair)
            {
                pos.Add(pair.Car is Fixnum pf ? (int)pf.Value : -1);
                kind.Add(pair.Cdr is Fixnum kf ? (byte)kf.Value : LispStruct.RawLong);
            }
            else { pos.Add(-1); kind.Add(LispStruct.RawLong); }
        }
        LispStruct.RegisterLayout(sym, pos.ToArray(), kind.ToArray(),
                                  version is Fixnum vf ? (int)vf.Value : 0);
        return Nil.Instance;
    }

    public static LispObject CheckSlotType(LispObject value, LispObject type,
                                           LispObject structName, LispObject slotName)
    {
        if (SlotTypeTest(type).Test(value)) return value;
        throw new LispErrorException(new LispTypeError(
            $"{structName}: slot {slotName} is declared {type}, got {value}",
            value, type));
    }

    // Declared slot types, keyed by the (constant) specifier object a DEFSTRUCT
    // expansion passes in, so the rewrite below runs once per slot, not per store.
    private static readonly ConditionalWeakTable<LispObject, LispObject> _slotCheckTypes = new();

    /// <summary>The type a slot store is actually checked against: TYPE itself,
    /// or a supertype of it that TYPEP can decide.
    ///
    /// A declaration may use specifiers TYPEP rejects -- (FUNCTION (STRING)
    /// BOOLEAN) is an ordinary slot :TYPE, and TYPEP of a FUNCTION compound is an
    /// error (CLHS 4.2.3, TYPEP). Checking the store with TYPEP turned every such
    /// slot into a load failure. The check only has to catch values that cannot
    /// be of the declared type, so a supertype is enough: a FUNCTION compound
    /// becomes FUNCTION (what SBCL checks too), and the rewrite is carried
    /// through OR / AND and through DEFTYPE expansions. Under NOT a weakening
    /// would narrow the set instead, so such a NOT is replaced by T.</summary>
    internal static LispObject SlotCheckType(LispObject type)
    {
        if (type is not Cons && (type is not Symbol || type is Nil || type is T)) return type;
        // GetValue rather than AddOrUpdate: the latter is not in netstandard2.0.
        return _slotCheckTypes.GetValue(type, static t =>
        {
            try { return WeakenForTypep(t, 0); }
            catch (LispErrorException) { return t; }
        });
    }

    // The test each declared slot type is checked with, keyed like _slotCheckTypes.
    private static readonly ConditionalWeakTable<LispObject, TypeTest> _slotTypeTests = new();

    // A direct-mapped cache in front of _slotTypeTests: the declared types are a
    // few hundred constants, and the table lookup (a hash and a dependent-handle
    // read per probe) was most of what a slot store's check cost.
    private sealed class SlotTestCacheEntry
    {
        internal readonly LispObject Type;
        internal readonly TypeTest Test;
        internal SlotTestCacheEntry(LispObject type, TypeTest test) { Type = type; Test = test; }
    }
    private static readonly SlotTestCacheEntry?[] _slotTestCache = new SlotTestCacheEntry?[1024];

    private static TypeTest SlotTypeTest(LispObject type)
    {
        if (type is T) return ConstTypeTest.True;
        int h = RuntimeHelpers.GetHashCode(type) & (1024 - 1);
        var e = _slotTestCache[h];
        if (e != null && ReferenceEquals(e.Type, type)) return e.Test;
        var test = SlotTypeTestSlow(type);
        _slotTestCache[h] = new SlotTestCacheEntry(type, test);
        return test;
    }

    private static TypeTest SlotTypeTestSlow(LispObject type)
    {
        return _slotTypeTests.GetValue(type, static t => BuildTypeTest(SlotCheckType(t)));
    }

    private static LispObject WeakenForTypep(LispObject type, int depth)
    {
        if (depth > 32) return T.Instance;
        if (type is Symbol sym && type is not Nil && type is not T)
        {
            if (TryGetQualifiedTypeExpander(sym, out var qexp)
                || TryGetTypeExpander(sym, out qexp))
            {
                var expanded = Funcall(qexp);
                if (!ReferenceEquals(expanded, type))
                {
                    var w = WeakenForTypep(expanded, depth + 1);
                    return ReferenceEquals(w, expanded) ? type : w;
                }
            }
            return type;
        }
        if (type is Cons c && c.Car is Symbol head)
        {
            switch (head.Name)
            {
                case "FUNCTION": return Startup.Sym("FUNCTION");
                case "VALUES": return T.Instance;
                case "OR":
                case "AND":
                {
                    var parts = new List<LispObject>();
                    bool changed = false;
                    for (var cur = c.Cdr; cur is Cons pc; cur = pc.Cdr)
                    {
                        var w = WeakenForTypep(pc.Car, depth + 1);
                        if (!ReferenceEquals(w, pc.Car)) changed = true;
                        parts.Add(w);
                    }
                    if (!changed) return type;
                    LispObject list = Nil.Instance;
                    for (int i = parts.Count - 1; i >= 0; i--) list = new Cons(parts[i], list);
                    return new Cons(head, list);
                }
                case "NOT":
                {
                    var inner = c.Cdr is Cons nc ? nc.Car : Nil.Instance;
                    return ReferenceEquals(WeakenForTypep(inner, depth + 1), inner) ? type : T.Instance;
                }
            }
            if (TryGetQualifiedTypeExpander(head, out var cexp)
                || TryGetTypeExpander(head, out cexp))
            {
                var expanded = Funcall(cexp, ToList(c.Cdr).ToArray());
                if (!ReferenceEquals(expanded, type))
                {
                    var w = WeakenForTypep(expanded, depth + 1);
                    return ReferenceEquals(w, expanded) ? type : w;
                }
            }
        }
        return type;
    }

    public static LispObject StructSetI(LispObject obj, int packed, LispObject value)
    {
        if (obj is LispStruct s)
        {
            s.SetSlot(SlotIndex(s, packed), value);
            return value;
        }
        int idx = packed & SlotIndexMask;
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
        {
            inst.Slots[idx] = value;
            return value;
        }
        throw new LispErrorException(new LispTypeError($"STRUCT-SET: not a structure (idx={idx}, type={obj?.GetType().AssemblyQualifiedName ?? "null"})", obj));
    }

    public static LispObject StructRef(LispObject obj, LispObject index)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("STRUCT-REF: index must be integer", index));
        // The accessor functions DEFSTRUCT writes call this with the same packed
        // constant a compiled call site emits, so the version check is the same
        // one -- an interpreted call must not read a slot a compiled call would
        // refuse.
        int packed = (int)f.Value;
        int idx = packed & SlotIndexMask;
        if (obj is LispStruct s)
        {
            if (idx < 0 || idx >= s.SlotCount)
                throw IndexError("STRUCT-REF", idx, s.SlotCount, "structure");
            return s.GetSlot(SlotIndex(s, packed));
        }
        // Also support LispInstance for structure classes (created by allocate-instance)
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
        {
            if (idx < 0 || idx >= inst.Slots.Length)
                throw IndexError("STRUCT-REF", idx, inst.Slots.Length, "structure");
            return inst.Slots[idx] ?? Nil.Instance;
        }
        { var sv = obj?.ToString() ?? "nil"; throw new LispErrorException(new LispTypeError($"STRUCT-REF: not a structure (idx={idx}, type={obj?.GetType().Name ?? "null"}, val={(sv.Length > 60 ? sv[..60] : sv)})", obj)); }
    }

    public static LispObject StructSet(LispObject obj, LispObject index, LispObject value)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("STRUCT-SET: index must be integer", index));
        int packed = (int)f.Value;
        int idx = packed & SlotIndexMask;
        if (obj is LispStruct s)
        {
            if (idx < 0 || idx >= s.SlotCount)
                throw IndexError("STRUCT-SET", idx, s.SlotCount, "structure");
            s.SetSlot(SlotIndex(s, packed), value);
            return value;
        }
        // Also support LispInstance for structure classes (created by allocate-instance)
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
        {
            if (idx < 0 || idx >= inst.Slots.Length)
                throw IndexError("STRUCT-SET", idx, inst.Slots.Length, "structure");
            inst.Slots[idx] = value;
            return value;
        }
        throw new LispErrorException(new LispTypeError("STRUCT-SET: not a structure", obj));
    }

    public static LispObject StructTypep(LispObject obj, LispObject typeName)
    {
        if (obj is LispStruct s && typeName is Symbol sym)
        {
            // Fast path: exact symbol reference equality
            if (ReferenceEquals(s.TypeName, sym)) return T.Instance;
            // Fallback: name comparison (different symbol objects, same name)
            if (s.TypeName.Name == sym.Name) return T.Instance;
            // Check class hierarchy for :include inheritance
            var cls = FindClassOrNil(s.TypeName) as LispClass;
            var targetCls = FindClassOrNil(sym) as LispClass;
            if (cls != null && targetCls != null)
            {
                // Walk CPL to check if target is an ancestor
                foreach (var ancestor in cls.ClassPrecedenceList)
                {
                    if (ReferenceEquals(ancestor, targetCls)) return T.Instance;
                }
            }
        }
        return Nil.Instance;
    }

    public static LispObject CopyStruct(LispObject obj)
    {
        if (obj is not LispStruct s)
            throw new LispErrorException(new LispTypeError("COPY-STRUCT: not a structure", obj));
        // Array.Clone goes through MemberwiseClone, which is a runtime call that
        // reads the array's type at run time; allocating and copying is the same
        // work with none of that. COPY-STRUCTURE of a four-slot structure spent
        // 0.60 s per 3M copies through Clone and 0.37 s this way.
        var src = s.SlotsSnapshot();
        var dst = new LispObject[src.Length];
        Array.Copy(src, dst, src.Length);
        return new LispStruct(s, dst);
    }


}
