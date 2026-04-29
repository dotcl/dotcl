using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    // --- Array/Vector operations ---

    public static LispObject MakeArray(LispObject[] args)
    {
        // (make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer)
        var dims = args[0];
        int[] dimArray;
        int size;

        if (dims is Fixnum fn)
        {
            size = (int)fn.Value;
            dimArray = new[] { size };
        }
        else if (dims is Nil)
        {
            // rank-0 array: 0 dimensions, but 1 element (the scalar)
            size = 1;
            dimArray = Array.Empty<int>();
        }
        else if (dims is Cons)
        {
            var dimList = new List<int>();
            var cur = dims;
            while (cur is Cons c) { dimList.Add((int)((Fixnum)c.Car).Value); cur = c.Cdr; }
            dimArray = dimList.ToArray();
            size = 1;
            foreach (var d in dimArray) size *= d;
        }
        else throw new LispErrorException(new LispError($"MAKE-ARRAY: unsupported dimensions: {dims}"));

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

        int rank = dimArray.Length;
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
                // Wrap LispString in a temporary LispVector for sharing
                var strItems = new LispObject[srcStr.Length];
                for (int j = 0; j < srcStr.Length; j++) strItems[j] = LispChar.Make(srcStr[j]);
                srcVec = new LispVector(strItems, "CHARACTER");
                if (elementType == "T") elementType = "CHARACTER";
            }
            else throw new LispErrorException(new LispError("MAKE-ARRAY: :displaced-to must be an array"));
            vec = new LispVector(size, srcVec, displacedOffset, elementType, dimArray);
        }
        else if (initialContents != null)
        {
            var items = new LispObject[size];
            FlattenContents(initialContents, items, 0, rank);
            vec = rank == 1 ? new LispVector(items, elementType) : new LispVector(items, dimArray, elementType);
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
                fill = Nil.Instance;
            var items = new LispObject[size];
            for (int j = 0; j < size; j++) items[j] = fill;
            vec = rank == 1 ? new LispVector(items, elementType) : new LispVector(items, dimArray, elementType);
        }

        if (fillPointer.HasValue)
            vec.SetFillPointer(fillPointer.Value);
        if (isAdjustable || displacedTo != null)
            vec.IsAdjustable = true;
        return vec;
    }

    public static LispObject AdjustArray(LispObject[] args)
    {
        // (adjust-array array new-dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset)
        if (args.Length < 2) throw new LispErrorException(new LispProgramError("ADJUST-ARRAY: too few arguments"));
        if (args[0] is not LispVector vec)
            throw new LispErrorException(new LispTypeError($"ADJUST-ARRAY: not an array", args[0]));

        var dims = args[1];
        int[] dimArray;
        int size;
        if (dims is Fixnum fn)
        {
            size = (int)fn.Value;
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
            while (cur is Cons c) { dimList.Add((int)((Fixnum)c.Car).Value); cur = c.Cdr; }
            dimArray = dimList.ToArray();
            size = 1;
            foreach (var d in dimArray) size *= d;
        }
        else throw new LispErrorException(new LispError($"ADJUST-ARRAY: unsupported dimensions: {dims}"));

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

        if (!vec.IsAdjustable)
        {
            // Non-adjustable: create a new array (original is unchanged)
            LispVector newVec;
            if (displacedTo is LispVector dv2)
            {
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
    public static LispObject Aref(LispObject array, LispObject index)
    {
        // Tight fast path: plain 1D LispVector with Fixnum index.
        if (array is LispVector v && index is Fixnum f
            && v._displacedTo == null && v._bitData == null)
        {
            int idx = (int)f.Value;
            if ((uint)idx < (uint)v._elements.Length)
                return v._elements[idx] ?? Nil.Instance;
        }
        return ArefSlow(array, index);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject ArefSlow(LispObject array, LispObject index)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("AREF: index must be integer", index));
        int idx = (int)f.Value;
        if (array is LispVector v)
        {
            if (idx < 0 || idx >= v.Capacity)
                throw new LispErrorException(new LispError($"AREF: index {idx} out of range for array of size {v.Capacity}"));
            return v.GetElement(idx);
        }
        if (array is LispString s)
        {
            if (idx < 0 || idx >= s.Length)
                throw new LispErrorException(new LispError($"AREF: index {idx} out of range"));
            return LispChar.Make(s[idx]);
        }
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    public static LispObject ArefMulti(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("AREF: requires array argument"));
        var array = args[0];
        if (array is LispVector v)
        {
            int[] dims = v.Dimensions;
            int nidx = args.Length - 1;
            if (nidx != dims.Length)
                throw new LispErrorException(new LispProgramError($"AREF: {nidx} indices for rank-{dims.Length} array"));
            int idx = 0;
            for (int k = 0; k < nidx; k++)
            {
                int i = (int)((Fixnum)args[k + 1]).Value;
                idx = idx * dims[k] + i;
            }
            if (idx < 0 || idx >= v.Capacity)
                throw new LispErrorException(new LispError($"AREF: index out of range"));
            return v.GetElement(idx);
        }
        if (array is LispString s && args.Length == 2)
            return Aref(array, args[1]);
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
            int[] dims = v.Dimensions;
            int nidx = args.Length - 2;
            if (nidx != dims.Length)
                throw new LispErrorException(new LispProgramError($"(SETF AREF): {nidx} indices for rank-{dims.Length} array"));
            int idx = 0;
            for (int k = 0; k < nidx; k++)
            {
                int i = (int)((Fixnum)args[k + 1]).Value;
                idx = idx * dims[k] + i;
            }
            v.SetElement(idx, value);
            return value;
        }
        if (array is LispString ls)
        {
            int nidx = args.Length - 2;
            if (nidx != 1)
                throw new LispErrorException(new LispProgramError($"(SETF AREF): string requires exactly 1 index, got {nidx}"));
            int i = (int)((Fixnum)args[1]).Value;
            if (value is not LispChar ch)
                throw new LispErrorException(new LispTypeError("(SETF AREF): value must be a character for string", value, Startup.Sym("CHARACTER")));
            ls[i] = ch.Value;
            return value;
        }
        throw new LispErrorException(new LispTypeError("(SETF AREF): not a vector", array));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet(LispObject array, LispObject index, LispObject value)
    {
        if (array is LispVector v && index is Fixnum f
            && v._displacedTo == null && v._bitData == null)
        {
            int idx = (int)f.Value;
            if ((uint)idx < (uint)v._elements.Length)
            {
                v._elements[idx] = value;
                return value;
            }
        }
        return ArefSetSlow(array, index, value);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject ArefSetSlow(LispObject array, LispObject index, LispObject value)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("(SETF AREF): index must be integer", index));
        int idx = (int)f.Value;
        if (array is LispVector v)
        {
            if (idx < 0 || idx >= v.Capacity)
                throw new LispErrorException(new LispError($"(SETF AREF): index {idx} out of range for array of size {v.Capacity}"));
            v.SetElement(idx, value);
            return value;
        }
        if (array is LispString ls)
        {
            if (value is not LispChar ch)
                throw new LispErrorException(new LispTypeError("(SETF AREF): value must be a character for string", value, Startup.Sym("CHARACTER")));
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
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null)
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int idx = i0 * v._dimensions[1] + i1;
            if ((uint)idx < (uint)v._elements.Length)
                return v._elements[idx] ?? Nil.Instance;
        }
        return Aref2DSlow(array, idx0, idx1);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject Aref2DSlow(LispObject array, LispObject idx0, LispObject idx1)
    {
        if (array is LispVector v)
        {
            int[] dims = v.Dimensions;
            int i0 = (int)((Fixnum)idx0).Value;
            int i1 = (int)((Fixnum)idx1).Value;
            int idx = i0 * dims[1] + i1;
            return v.GetElement(idx);
        }
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    /// <summary>Specialized 2D aref setter - avoids args array allocation.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet2D(LispObject array, LispObject idx0, LispObject idx1, LispObject value)
    {
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null)
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int idx = i0 * v._dimensions[1] + i1;
            if ((uint)idx < (uint)v._elements.Length)
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
            int[] dims = v.Dimensions;
            int i0 = (int)((Fixnum)idx0).Value;
            int i1 = (int)((Fixnum)idx1).Value;
            int idx = i0 * dims[1] + i1;
            // Fast path: direct element access for non-displaced, non-bit arrays
            if (v._displacedTo == null && v._bitData == null)
            {
                v._elements[idx] = value;
                return value;
            }
            v.SetElement(idx, value);
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
            && v._dimensions != null)
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int i2 = (int)f2.Value;
            int idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if ((uint)idx < (uint)v._elements.Length)
                return v._elements[idx] ?? Nil.Instance;
        }
        return Aref3DSlow(array, idx0, idx1, idx2);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static LispObject Aref3DSlow(LispObject array, LispObject idx0, LispObject idx1, LispObject idx2)
    {
        if (array is LispVector v)
        {
            int[] dims = v.Dimensions;
            int i0 = (int)((Fixnum)idx0).Value;
            int i1 = (int)((Fixnum)idx1).Value;
            int i2 = (int)((Fixnum)idx2).Value;
            int idx = (i0 * dims[1] + i1) * dims[2] + i2;
            return v.GetElement(idx);
        }
        throw new LispErrorException(new LispTypeError("AREF: not an array", array));
    }

    /// <summary>Specialized 3D aref setter - avoids args array allocation.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static LispObject ArefSet3D(LispObject array, LispObject idx0, LispObject idx1, LispObject idx2, LispObject value)
    {
        if (array is LispVector v
            && idx0 is Fixnum f0 && idx1 is Fixnum f1 && idx2 is Fixnum f2
            && v._displacedTo == null && v._bitData == null
            && v._dimensions != null)
        {
            int i0 = (int)f0.Value;
            int i1 = (int)f1.Value;
            int i2 = (int)f2.Value;
            int idx = (i0 * v._dimensions[1] + i1) * v._dimensions[2] + i2;
            if ((uint)idx < (uint)v._elements.Length)
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
            int[] dims = v.Dimensions;
            int i0 = (int)((Fixnum)idx0).Value;
            int i1 = (int)((Fixnum)idx1).Value;
            int i2 = (int)((Fixnum)idx2).Value;
            int idx = (i0 * dims[1] + i1) * dims[2] + i2;
            if (v._displacedTo == null && v._bitData == null)
            {
                v._elements[idx] = value;
                return value;
            }
            v.SetElement(idx, value);
            return value;
        }
        throw new LispErrorException(new LispTypeError("(SETF AREF): not an array", array));
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

    public static LispObject MakeStruct(LispObject typeName, params LispObject[] slots)
    {
        if (typeName is not Symbol sym)
            throw new LispErrorException(new LispTypeError("MAKE-STRUCT: type name must be a symbol", typeName));
        var result = new LispStruct(sym, (LispObject[])slots.Clone());
        return result;
    }

    /// <summary>
    /// Fast struct slot access with raw int index (avoids Fixnum boxing).
    /// Used by compiler for constant-index struct accessors.
    /// </summary>
    public static LispObject StructRefI(LispObject obj, int idx)
    {
        if (obj is LispStruct s)
        {
            return s.Slots[idx];
        }
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
    public static LispObject StructSetI(LispObject obj, int idx, LispObject value)
    {
        if (obj is LispStruct s)
        {
            s.Slots[idx] = value;
            return value;
        }
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
        {
            inst.Slots[idx] = value;
            return value;
        }
        throw new LispErrorException(new LispTypeError("STRUCT-SET: not a structure", obj));
    }

    public static LispObject StructRef(LispObject obj, LispObject index)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("STRUCT-REF: index must be integer", index));
        int idx = (int)f.Value;
        if (obj is LispStruct s)
        {
            if (idx < 0 || idx >= s.Slots.Length)
                throw new LispErrorException(new LispError($"STRUCT-REF: index {idx} out of range"));
            return s.Slots[idx];
        }
        // Also support LispInstance for structure classes (created by allocate-instance)
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
        {
            if (idx < 0 || idx >= inst.Slots.Length)
                throw new LispErrorException(new LispError($"STRUCT-REF: index {idx} out of range"));
            return inst.Slots[idx] ?? Nil.Instance;
        }
        { var sv = obj?.ToString() ?? "nil"; throw new LispErrorException(new LispTypeError($"STRUCT-REF: not a structure (idx={idx}, type={obj?.GetType().Name ?? "null"}, val={(sv.Length > 60 ? sv[..60] : sv)})", obj)); }
    }

    public static LispObject StructSet(LispObject obj, LispObject index, LispObject value)
    {
        if (index is not Fixnum f)
            throw new LispErrorException(new LispTypeError("STRUCT-SET: index must be integer", index));
        int idx = (int)f.Value;
        if (obj is LispStruct s)
        {
            if (idx < 0 || idx >= s.Slots.Length)
                throw new LispErrorException(new LispError($"STRUCT-SET: index {idx} out of range"));
            s.Slots[idx] = value;
            return value;
        }
        // Also support LispInstance for structure classes (created by allocate-instance)
        if (obj is LispInstance inst && inst.Class.IsStructureClass)
        {
            if (idx < 0 || idx >= inst.Slots.Length)
                throw new LispErrorException(new LispError($"STRUCT-SET: index {idx} out of range"));
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
        return new LispStruct(s.TypeName, (LispObject[])s.Slots.Clone());
    }


}
