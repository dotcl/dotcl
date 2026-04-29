namespace DotCL;

public static partial class Runtime
{
    // --- Type expanders (registered by deftype) ---
    public static Dictionary<string, LispObject> TypeExpanders = new();

    // --- Typep ---

    public static bool IsTrueTypep(LispObject obj, LispObject typeSpec) => Typep(obj, typeSpec) is not Nil;

    public static LispObject Typep(LispObject obj, LispObject typeSpec)
    {
        if (typeSpec is Symbol || typeSpec is Nil || typeSpec is T)
        {
            // Fast path: struct type check — avoid full switch when positive match
            if (typeSpec is Symbol typeSym && obj is LispStruct st)
            {
                if (ReferenceEquals(st.TypeName, typeSym) || st.TypeName.Name == typeSym.Name)
                    return T.Instance;
                // Check :include hierarchy
                var stCls = FindClassOrNil(st.TypeName) as LispClass;
                var tgtCls = FindClassOrNil(typeSym) as LispClass;
                if (stCls != null && tgtCls != null)
                {
                    foreach (var a in stCls.ClassPrecedenceList)
                        if (ReferenceEquals(a, tgtCls)) return T.Instance;
                    // Target is a known class but struct doesn't inherit from it.
                    // If target is a structure class, struct can't match — skip CheckSimpleType.
                    if (tgtCls.IsStructureClass) return Nil.Instance;
                }
                // Fall through: type might be T, ATOM, STRUCTURE-OBJECT, etc.
            }
            string name = typeSpec switch
            {
                T => "T",
                Nil => "NIL",
                Symbol sym => sym.Name,
                _ => ""
            };
            if (name == "VALUES")
                throw new LispErrorException(new LispTypeError("TYPEP: VALUES is not a valid type specifier", typeSpec));
            if (CheckSimpleType(obj, name)) return T.Instance;
            // Try user-defined type expander
            if (TypeExpanders.TryGetValue(name, out var expSymExpander))
            {
                var expanded = Funcall(expSymExpander);
                return Typep(obj, expanded);
            }
            return Nil.Instance;
        }
        // LispClass as type specifier: check if obj is instance of this class
        if (typeSpec is LispClass lcSpec)
        {
            if (obj is LispInstance inst)
            {
                // Walk the class precedence list to check subtype relationship
                foreach (var c in inst.Class.ClassPrecedenceList)
                {
                    if (ReferenceEquals(c, lcSpec)) return T.Instance;
                }
            }
            return CheckSimpleType(obj, lcSpec.Name.Name) ? T.Instance : Nil.Instance;
        }
        if (typeSpec is Cons compound)
        {
            var head = compound.Car;
            string headName = head switch
            {
                Symbol sym => sym.Name,
                _ => ""
            };
            switch (headName)
            {
                case "OR":
                {
                    var cur = compound.Cdr;
                    while (cur is Cons c)
                    {
                        if (IsTruthy(Typep(obj, c.Car))) return T.Instance;
                        cur = c.Cdr;
                    }
                    return Nil.Instance;
                }
                case "AND":
                {
                    var cur = compound.Cdr;
                    while (cur is Cons c)
                    {
                        if (!IsTruthy(Typep(obj, c.Car))) return Nil.Instance;
                        cur = c.Cdr;
                    }
                    return T.Instance;
                }
                case "VALUES":
                    throw new LispErrorException(new LispTypeError("TYPEP: VALUES is not a valid type specifier", typeSpec));
                case "FUNCTION":
                    throw new LispErrorException(new LispTypeError("TYPEP: FUNCTION compound type specifier not supported in TYPEP", typeSpec));
                case "NOT":
                    return IsTruthy(Typep(obj, Car(Cdr(typeSpec)))) ? Nil.Instance : T.Instance;
                case "EQL":
                    return IsTrueEql(obj, Car(Cdr(typeSpec))) ? T.Instance : Nil.Instance;
                case "MEMBER":
                {
                    var cur = compound.Cdr;
                    while (cur is Cons c)
                    {
                        if (IsTrueEql(obj, c.Car)) return T.Instance;
                        cur = c.Cdr;
                    }
                    return Nil.Instance;
                }
                case "SATISFIES":
                {
                    var pred = Car(Cdr(typeSpec));
                    if (pred is Symbol predSym)
                    {
                        try { return IsTruthy(Funcall(predSym, obj)) ? T.Instance : Nil.Instance; }
                        catch (LispErrorException) { }
                    }
                    return Nil.Instance;
                }
                case "STRING":
                case "SIMPLE-STRING":
                case "BASE-STRING":
                case "SIMPLE-BASE-STRING":
                {
                    // (string) or (string *) or (string N) — check type then optional length
                    if (!CheckSimpleType(obj, headName)) return Nil.Instance;
                    var sizeSpec = compound.Cdr is Cons sc ? sc.Car : null;
                    if (sizeSpec == null || (sizeSpec is Symbol ss && ss.Name == "*"))
                        return T.Instance;
                    int expectedLen = sizeSpec is Fixnum sf ? (int)sf.Value : -1;
                    int actualLen = obj is LispString ls ? ls.Length : (obj is LispVector lv ? lv.Length : -1);
                    return actualLen == expectedLen ? T.Instance : Nil.Instance;
                }
                case "ARRAY":
                case "SIMPLE-ARRAY":
                {
                    // (array element-type dimension-spec)
                    // First check if it's an array at all
                    if (!(obj is LispVector || obj is LispString)) return Nil.Instance;
                    if (headName == "SIMPLE-ARRAY" && obj is LispVector av && av.HasFillPointer) return Nil.Instance;
                    var rest2 = compound.Cdr;
                    // Element type check (skip if *)
                    if (rest2 is Cons etc)
                    {
                        var elemType = etc.Car;
                        if (!(elemType is Symbol es && es.Name == "*"))
                        {
                            if (!ArrayElementTypeMatches(obj, elemType)) return Nil.Instance;
                        }
                        // Dimension spec
                        if (etc.Cdr is Cons dimCons)
                        {
                            var dimSpec = dimCons.Car;
                            if (!(dimSpec is Symbol ds && ds.Name == "*"))
                            {
                                // Could be a list like (*) or (N) or just 1 or NIL (= rank-0)
                                if (dimSpec is Nil)
                                {
                                    // NIL = empty list = rank-0 array
                                    int actualRankNil = obj is LispVector vNil ? vNil.Rank : (obj is LispString ? 1 : 0);
                                    if (actualRankNil != 0) return Nil.Instance;
                                }
                                else if (dimSpec is Fixnum dimF)
                                {
                                    // rank check (integer = rank)
                                    int actualRankF = obj is LispVector vRank ? vRank.Rank : (obj is LispString ? 1 : 0);
                                    if (dimF.Value != actualRankF) return Nil.Instance;
                                }
                                else if (dimSpec is Cons)
                                {
                                    // list of dimension specs, e.g. (*) or (N) or (3 4 5)
                                    // Collect the list
                                    var dims = new List<LispObject>();
                                    var dc2 = dimSpec;
                                    while (dc2 is Cons dcc2) { dims.Add(dcc2.Car); dc2 = dcc2.Cdr; }
                                    int specRank = dims.Count;
                                    // Get actual rank and dimensions
                                    int actualRank = obj is LispVector vArr ? vArr.Rank : (obj is LispString ? 1 : 0);
                                    if (specRank != actualRank) return Nil.Instance;
                                    // Check each dimension
                                    for (int di = 0; di < specRank; di++)
                                    {
                                        if (dims[di] is Symbol dimStar && dimStar.Name == "*") continue;
                                        if (dims[di] is Fixnum dimFi)
                                        {
                                            int actualDim;
                                            if (obj is LispString sls2) actualDim = sls2.Length;
                                            else if (obj is LispVector slv2) actualDim = di < slv2.Dimensions.Length ? slv2.Dimensions[di] : -1;
                                            else return Nil.Instance;
                                            if (actualDim != (int)dimFi.Value) return Nil.Instance;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    return T.Instance;
                }
                case "BIT-VECTOR":
                case "SIMPLE-BIT-VECTOR":
                {
                    // (bit-vector size) or (bit-vector *) — only size arg, no element-type
                    if (!CheckSimpleType(obj, headName)) return Nil.Instance;
                    if (compound.Cdr is Cons bvSizeCons)
                    {
                        var sizeSpec = bvSizeCons.Car;
                        if (!(sizeSpec is Symbol bvSS && bvSS.Name == "*"))
                        {
                            int expectedSize = sizeSpec is Fixnum sf2 ? (int)sf2.Value : -1;
                            int actualSize = obj is LispVector sv3 ? sv3.Length : -1;
                            if (actualSize != expectedSize) return Nil.Instance;
                        }
                    }
                    return T.Instance;
                }
                case "VECTOR":
                case "SIMPLE-VECTOR":
                {
                    // (vector element-type) or (vector element-type size) or (vector *)
                    if (!CheckSimpleType(obj, headName)) return Nil.Instance;
                    var vecRest = compound.Cdr;
                    if (vecRest is Cons vecEtCons)
                    {
                        var elemTypeSpec = vecEtCons.Car;
                        // * means any element type
                        if (!(elemTypeSpec is Symbol es && es.Name == "*"))
                        {
                            // Check element type matches stored ElementTypeName
                            string storedET = obj is LispVector vv ? vv.ElementTypeName : (obj is LispString ? "CHARACTER" : "T");
                            bool etMatch = MatchesElementType(elemTypeSpec, storedET);
                            if (!etMatch) return Nil.Instance;
                        }
                        // Optional size check
                        if (vecEtCons.Cdr is Cons vecSizeCons)
                        {
                            var sizeSpec = vecSizeCons.Car;
                            if (!(sizeSpec is Symbol ss2 && ss2.Name == "*"))
                            {
                                int expectedSize = sizeSpec is Fixnum sf2 ? (int)sf2.Value : -1;
                                int actualSize = obj is LispVector sv3 ? sv3.Length : (obj is LispString ls3 ? ls3.Length : -1);
                                if (actualSize != expectedSize) return Nil.Instance;
                            }
                        }
                    }
                    return T.Instance;
                }
                case "UNSIGNED-BYTE":
                case "SIGNED-BYTE":
                {
                    // (unsigned-byte n) or (signed-byte n) as element type specifier
                    // Used in typep on vectors via (typep vec '(vector (unsigned-byte n)))
                    // When used directly: (typep 3 '(unsigned-byte 8))
                    if (!CheckSimpleType(obj, headName)) return Nil.Instance;
                    if (compound.Cdr is Cons nbCons && nbCons.Car is Fixnum nbF)
                    {
                        int bits = (int)nbF.Value;
                        if (obj is Fixnum objF)
                        {
                            if (headName == "UNSIGNED-BYTE")
                            {
                                if (objF.Value < 0) return Nil.Instance;
                                if (bits < 63 && objF.Value >= (1L << bits)) return Nil.Instance;
                                // bits >= 63: fixnum max is 2^63-1 which fits in (unsigned-byte 63+)
                            }
                            else // SIGNED-BYTE
                            {
                                if (bits < 63)
                                {
                                    long min = -(1L << (bits - 1)), max = (1L << (bits - 1)) - 1;
                                    if (objF.Value < min || objF.Value > max) return Nil.Instance;
                                }
                                // bits >= 63: all fixnums fit in signed-byte 63+
                            }
                        }
                        else if (obj is Bignum objB)
                        {
                            // Bignum values require BigInteger range check
                            if (headName == "UNSIGNED-BYTE")
                            {
                                if (objB.Value < 0) return Nil.Instance;
                                var maxVal = System.Numerics.BigInteger.Pow(2, bits);
                                if (objB.Value >= maxVal) return Nil.Instance;
                            }
                            else // SIGNED-BYTE
                            {
                                var half = System.Numerics.BigInteger.Pow(2, bits - 1);
                                if (objB.Value < -half || objB.Value >= half) return Nil.Instance;
                            }
                        }
                    }
                    return T.Instance;
                }
                case "INTEGER":
                {
                    // Range type: (integer low high) — use Arithmetic.Compare for exact comparison
                    if (!CheckSimpleType(obj, headName)) return Nil.Instance;
                    if (obj is not Number numObj) return Nil.Instance;
                    var rest = compound.Cdr;
                    var lowSpec = rest is Cons c1 ? c1.Car : null;
                    var highSpec = rest is Cons c2 && c2.Cdr is Cons c3 ? c3.Car : null;
                    if (lowSpec != null && !(lowSpec is Symbol s1 && s1.Name == "*") && !(lowSpec is Nil))
                    {
                        if (lowSpec is Number lowNum)
                        {
                            if (Arithmetic.Compare(numObj, lowNum) < 0) return Nil.Instance;
                        }
                        else if (lowSpec is Cons lowExcl && lowExcl.Car is Number lowExclNum)
                        {
                            if (Arithmetic.Compare(numObj, lowExclNum) <= 0) return Nil.Instance;
                        }
                    }
                    if (highSpec != null && !(highSpec is Symbol s2 && s2.Name == "*") && !(highSpec is Nil))
                    {
                        if (highSpec is Number highNum)
                        {
                            if (Arithmetic.Compare(numObj, highNum) > 0) return Nil.Instance;
                        }
                        else if (highSpec is Cons highExcl && highExcl.Car is Number highExclNum)
                        {
                            if (Arithmetic.Compare(numObj, highExclNum) >= 0) return Nil.Instance;
                        }
                    }
                    return T.Instance;
                }
                case "COMPLEX":
                {
                    // (complex type): obj must be complex with both parts of 'type'
                    if (obj is not LispComplex cx) return Nil.Instance;
                    var partType = compound.Cdr is Cons pt ? pt.Car : T.Instance;
                    if (partType is Symbol pts && pts.Name == "*") return T.Instance;
                    return (IsTruthy(Typep(cx.Real, partType)) && IsTruthy(Typep(cx.Imaginary, partType)))
                        ? T.Instance : Nil.Instance;
                }
                case "FLOAT":
                case "SINGLE-FLOAT":
                case "SHORT-FLOAT":
                case "DOUBLE-FLOAT":
                case "LONG-FLOAT":
                case "REAL":
                case "RATIONAL":
                {
                    // Range type: (float low high), etc. — use Arithmetic.Compare for exact comparison
                    if (!CheckSimpleType(obj, headName)) return Nil.Instance;
                    if (obj is not Number numObj) return Nil.Instance;
                    var rest = compound.Cdr;
                    var lowSpec = rest is Cons c1 ? c1.Car : null;
                    var highSpec = rest is Cons c2 && c2.Cdr is Cons c3 ? c3.Car : null;
                    // Check low bound (* means no bound)
                    if (lowSpec != null && !(lowSpec is Symbol s1 && s1.Name == "*") && !(lowSpec is Nil))
                    {
                        if (lowSpec is Number lowNum)
                        {
                            if (Arithmetic.Compare(numObj, lowNum) < 0) return Nil.Instance;
                        }
                        else if (lowSpec is Cons lowExcl && lowExcl.Car is Number lowExclNum)
                        {
                            // Exclusive bound: (low) means > low
                            if (Arithmetic.Compare(numObj, lowExclNum) <= 0) return Nil.Instance;
                        }
                    }
                    // Check high bound
                    if (highSpec != null && !(highSpec is Symbol s2 && s2.Name == "*") && !(highSpec is Nil))
                    {
                        if (highSpec is Number highNum)
                        {
                            if (Arithmetic.Compare(numObj, highNum) > 0) return Nil.Instance;
                        }
                        else if (highSpec is Cons highExcl && highExcl.Car is Number highExclNum)
                        {
                            // Exclusive bound: (high) means < high
                            if (Arithmetic.Compare(numObj, highExclNum) >= 0) return Nil.Instance;
                        }
                    }
                    return T.Instance;
                }
                case "CONS":
                {
                    if (obj is not Cons consObj) return Nil.Instance;
                    var carType = compound.Cdr is Cons carCons ? carCons.Car : T.Instance;
                    var cdrType = compound.Cdr is Cons cdrCons1 && cdrCons1.Cdr is Cons cdrCons2 ? cdrCons2.Car : T.Instance;
                    // * means match anything
                    if (carType is Symbol carStar && carStar.Name == "*") carType = T.Instance;
                    if (cdrType is Symbol cdrStar && cdrStar.Name == "*") cdrType = T.Instance;
                    // Check car and cdr types recursively
                    if (!IsTruthy(Typep(consObj.Car, carType))) return Nil.Instance;
                    if (!IsTruthy(Typep(consObj.Cdr, cdrType))) return Nil.Instance;
                    return T.Instance;
                }
            }
            // Unknown compound type: try user-defined type expander
            if (!string.IsNullOrEmpty(headName) && TypeExpanders.TryGetValue(headName, out var expCompoundExpander))
            {
                var args2 = ToList(compound.Cdr).ToArray();
                var expanded2 = Funcall(expCompoundExpander, args2);
                return Typep(obj, expanded2);
            }
        }
        return Nil.Instance;
    }

    private static bool CheckSimpleType(LispObject obj, string typeName) => typeName switch
    {
        "T" => true,
        "NIL" => false,
        "NULL" => obj is Nil,
        "CONS" => obj is Cons,
        "LIST" => obj is Cons || obj is Nil,
        "SYMBOL" => obj is Symbol || obj is Nil || obj is T,
        "ATOM" => obj is not Cons,
        "BOOLEAN" => obj is Nil || obj is T,
        "UNSIGNED-BYTE" => (obj is Fixnum uf && uf.Value >= 0) || (obj is Bignum ub && ub.Value >= 0),
        "SIGNED-BYTE" => obj is Fixnum || obj is Bignum,
        "BIT" => obj is Fixnum bf && (bf.Value == 0 || bf.Value == 1),
        "KEYWORD" => obj is Symbol sym && sym.HomePackage?.Name == "KEYWORD",
        "NUMBER" => obj is Number || IsStructFloat(obj) || obj is LispComplex,
        "INTEGER" => obj is Fixnum || obj is Bignum,
        "FIXNUM" => obj is Fixnum,
        "BIGNUM" => obj is Bignum,
        "RATIONAL" => obj is Fixnum || obj is Bignum || obj is Ratio,
        "RATIO" => obj is Ratio,
        "REAL" => obj is Fixnum || obj is Bignum || obj is Ratio || obj is SingleFloat || obj is DoubleFloat || IsStructFloat(obj),
        "FLOAT" => obj is SingleFloat || obj is DoubleFloat,
        "SINGLE-FLOAT" or "SHORT-FLOAT" => obj is SingleFloat,
        "DOUBLE-FLOAT" or "LONG-FLOAT" => obj is DoubleFloat,
        "COMPLEX" => obj is LispComplex,
        "STRING" => obj is LispString || (obj is LispVector sv && sv.IsCharVector && sv.Rank == 1),
        "SIMPLE-STRING" => obj is LispString || (obj is LispVector ssv && ssv.IsCharVector && !ssv.HasFillPointer && ssv.Rank == 1),
        "BASE-STRING" => obj is LispString || (obj is LispVector bsv && bsv.IsCharVector && bsv.ElementTypeName != "NIL" && bsv.Rank == 1),
        "SIMPLE-BASE-STRING" => obj is LispString || (obj is LispVector sbsv && sbsv.IsCharVector && !sbsv.HasFillPointer && sbsv.ElementTypeName != "NIL" && sbsv.Rank == 1),
        "CHARACTER" => obj is LispChar,
        "BASE-CHAR" => obj is LispChar,
        "STANDARD-CHAR" => obj is LispChar lc && IsStandardChar(lc.Value),
        "EXTENDED-CHAR" => false, // base-char = character in dotcl, so extended-char is empty
        "FUNCTION" => obj is LispFunction,
        "GENERIC-FUNCTION" => obj is GenericFunction,
        "STANDARD-GENERIC-FUNCTION" => obj is GenericFunction,
        "COMPILED-FUNCTION" => obj is LispFunction && obj is not GenericFunction,
        "VECTOR" => (obj is LispVector vv && vv.Rank == 1) || obj is LispString,
        "BIT-VECTOR" => obj is LispVector bv && bv.IsBitVector && bv.Rank == 1,
        "SIMPLE-BIT-VECTOR" => obj is LispVector sbv && sbv.IsBitVector && sbv.Rank == 1 && !sbv.HasFillPointer,
        "SIMPLE-VECTOR" => obj is LispVector sv2 && sv2.Rank == 1 && !sv2.IsCharVector && !sv2.IsBitVector && !sv2.HasFillPointer && sv2.ElementTypeName == "T",
        "ARRAY" => obj is LispVector || obj is LispString,
        "SIMPLE-ARRAY" => obj is LispString || (obj is LispVector sav && !sav.HasFillPointer),
        "SEQUENCE" => obj is Cons || obj is Nil || (obj is LispVector vsq && vsq.Rank == 1) || obj is LispString,
        "HASH-TABLE" => obj is LispHashTable,
        "PACKAGE" => obj is Package,
        "STREAM" => obj is LispStream,
        "FILE-STREAM" => obj is LispFileStream,
        "STRING-STREAM" => obj is LispStringInputStream || obj is LispStringOutputStream,
        "BROADCAST-STREAM" => obj is LispBroadcastStream,
        "CONCATENATED-STREAM" => obj is LispConcatenatedStream,
        "ECHO-STREAM" => obj is LispEchoStream,
        "SYNONYM-STREAM" => obj is LispSynonymStream,
        "TWO-WAY-STREAM" => obj is LispTwoWayStream,
        "PATHNAME" => obj is LispPathname,
        "LOGICAL-PATHNAME" => obj is LispLogicalPathname,
        "RANDOM-STATE" => obj is LispRandomState,
        "READTABLE" => obj is LispReadtable || (obj is LispInstance ri && ClassMatchesCPL(ri.Class, "READTABLE")),
        "INPUT-STREAM" => obj is LispStream s1 && s1.IsInput,
        "OUTPUT-STREAM" => obj is LispStream s2 && s2.IsOutput,
        "RESTART" => obj is LispRestart,
        "CONDITION" => obj is LispCondition
            || (obj is LispInstance ci && ClassMatchesCPL(ci.Class, "CONDITION"))
            || (obj is LispInstanceCondition lici && ClassMatchesCPL(lici.Instance.Class, "CONDITION")),
        "SIMPLE-CONDITION" => (obj is LispCondition scc && (scc.ConditionTypeName == "SIMPLE-CONDITION" || scc.ConditionTypeName == "SIMPLE-ERROR" || scc.ConditionTypeName == "SIMPLE-WARNING"))
            || (obj is LispInstanceCondition scci && ClassMatchesCPL(scci.Instance.Class, "SIMPLE-CONDITION")),
        "SERIOUS-CONDITION" => obj is LispError
            || (obj is LispInstance sci && ClassMatchesCPL(sci.Class, "SERIOUS-CONDITION"))
            || (obj is LispInstanceCondition sci2 && ClassMatchesCPL(sci2.Instance.Class, "SERIOUS-CONDITION")),
        "ERROR" => obj is LispError
            || (obj is LispInstance ei && ClassMatchesCPL(ei.Class, "ERROR"))
            || (obj is LispInstanceCondition ei2 && ClassMatchesCPL(ei2.Instance.Class, "ERROR")),
        "SIMPLE-ERROR" => (obj is LispError le && le.ConditionTypeName == "SIMPLE-ERROR")
            || (obj is LispInstanceCondition se && ClassMatchesCPL(se.Instance.Class, "SIMPLE-ERROR")),
        "TYPE-ERROR" => obj is LispTypeError
            || (obj is LispInstanceCondition te && ClassMatchesCPL(te.Instance.Class, "TYPE-ERROR")),
        "WARNING" => obj is LispWarning
            || (obj is LispInstanceCondition w && ClassMatchesCPL(w.Instance.Class, "WARNING")),
        "SIMPLE-WARNING" => (obj is LispWarning lw && lw.ConditionTypeName == "SIMPLE-WARNING")
            || (obj is LispInstanceCondition sw && ClassMatchesCPL(sw.Instance.Class, "SIMPLE-WARNING")),
        "PROGRAM-ERROR" => obj is LispProgramError
            || (obj is LispInstanceCondition pe && ClassMatchesCPL(pe.Instance.Class, "PROGRAM-ERROR")),
        "CONTROL-ERROR" => obj is LispControlError
            || (obj is LispInstanceCondition ce && ClassMatchesCPL(ce.Instance.Class, "CONTROL-ERROR")),
        "CELL-ERROR" => obj is LispCellError
            || (obj is LispInstanceCondition celc && ClassMatchesCPL(celc.Instance.Class, "CELL-ERROR")),
        "UNDEFINED-FUNCTION" => obj is LispUndefinedFunction
            || (obj is LispInstanceCondition ufc && ClassMatchesCPL(ufc.Instance.Class, "UNDEFINED-FUNCTION")),
        "UNBOUND-VARIABLE" => obj is LispUnboundVariable
            || (obj is LispInstanceCondition uvc && ClassMatchesCPL(uvc.Instance.Class, "UNBOUND-VARIABLE")),
        "STRUCTURE-OBJECT" => obj is LispStruct,
        "STRUCTURE-CLASS" => obj is LispClass lsc && lsc.IsStructureClass,
        "STANDARD-OBJECT" => (obj is LispInstance li2 && !li2.Class.IsBuiltIn) || obj is LispClass || obj is LispMethod || obj is LispCondition || obj is LispStream || obj is GenericFunction,
        "STANDARD-CLASS" => obj is LispClass lcs && !lcs.IsBuiltIn && !lcs.IsStructureClass,
        "BUILT-IN-CLASS" => obj is LispClass lcb && lcb.IsBuiltIn,
        "CLASS" => obj is LispClass,
        "METHOD" => obj is LispMethod,
        "STANDARD-METHOD" => obj is LispMethod,
        // Fallback: check for struct type name, CLOS class hierarchy, or condition type hierarchy
        _ => (obj is LispStruct s && StructTypeMatches(s, typeName))
          || (obj is LispInstance inst && ClassMatchesCPL(inst.Class, typeName))
          || (obj is LispInstanceCondition lic && ClassMatchesCPL(lic.Instance.Class, typeName))
          || (obj is LispCondition cond && ConditionTypeMatches(cond.ConditionTypeName, typeName))
    };

    /// <summary>Check if obj is a LispStruct that satisfies FLOAT type (for cross-compilation target-float structs).</summary>
    private static bool IsStructFloat(LispObject obj)
        => obj is LispStruct s && (s.TypeName.Name is "SINGLE-FLOAT" or "DOUBLE-FLOAT"
            or "SHORT-FLOAT" or "LONG-FLOAT" or "FLOAT"
            || StructTypeMatches(s, "FLOAT"));

    /// <summary>Check if a struct's type matches a target type, considering :include hierarchy.</summary>
    private static bool StructTypeMatches(LispStruct s, string typeName)
    {
        if (s.TypeName.Name == typeName) return true;
        // Check class hierarchy for :include inheritance
        var cls = FindClassOrNil(s.TypeName) as LispClass;
        if (cls != null)
        {
            foreach (var ancestor in cls.ClassPrecedenceList)
            {
                if (ancestor.Name.Name == typeName) return true;
            }
        }
        return false;
    }

    // Check if an array object's element type matches a type specifier
    /// <summary>Check if a condition's type name matches a target type, considering the type hierarchy.</summary>
    private static bool ConditionTypeMatches(string condTypeName, string targetType)
    {
        if (condTypeName == targetType) return true;
        if (_typeAncestors.TryGetValue(condTypeName, out var ancestors))
            return ancestors.Contains(targetType);
        return false;
    }

    private static bool ArrayElementTypeMatches(LispObject obj, LispObject elemType)
    {
        // Expand deftype aliases in element type before matching
        if (elemType is Symbol etAS && TypeExpanders.TryGetValue(etAS.Name, out var etAE))
            return ArrayElementTypeMatches(obj, Funcall(etAE));
        if (elemType is Cons etAC && etAC.Car is Symbol etAH && TypeExpanders.TryGetValue(etAH.Name, out var etAE2))
        {
            var aa = ToList(etAC.Cdr).ToArray();
            return ArrayElementTypeMatches(obj, Funcall(etAE2, aa));
        }
        // String/char-vector → element type is CHARACTER (not T)
        if (obj is LispString)
            return elemType is Symbol es && es.Name is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR";
        if (obj is LispVector v)
        {
            string et = v.ElementTypeName ?? "T";
            // T singleton means element-type T (not wildcard); wildcard is the Symbol "*"
            if (elemType is T) return et == "T";
            if (elemType is Symbol esym)
            {
                string etName = esym.Name;
                // (array t) only matches arrays with stored element type T
                if (etName == "T") return et == "T";
                return MatchesElementType(elemType, et);
            }
            // compound like (unsigned-byte 8)
            return MatchesElementType(elemType, et);
        }
        return false;
    }

    // Check if a LispVector's ElementTypeName matches a compound element-type specifier like (unsigned-byte 8)
    private static bool MatchesElementType(LispObject elemTypeSpec, string storedET)
    {
        // Expand deftype aliases (e.g., unicode-char → character) before matching
        if (elemTypeSpec is Symbol etAliasSym && TypeExpanders.TryGetValue(etAliasSym.Name, out var etAliasExp))
            return MatchesElementType(Funcall(etAliasExp), storedET);
        if (elemTypeSpec is Cons etAliasCons && etAliasCons.Car is Symbol etAliasHead
            && TypeExpanders.TryGetValue(etAliasHead.Name, out var etAliasExp2))
        {
            var aliasArgs = ToList(etAliasCons.Cdr).ToArray();
            return MatchesElementType(Funcall(etAliasExp2, aliasArgs), storedET);
        }
        if (elemTypeSpec is Symbol esym)
        {
            string etName = esym.Name;
            return etName switch
            {
                "T" => storedET == "T",
                "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR" => storedET is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR",
                "BIT" => storedET == "BIT",
                "NIL" => storedET == "NIL",
                "FIXNUM" or "INTEGER" or "SIGNED-BYTE" or "UNSIGNED-BYTE" => storedET.StartsWith("INTEGER") || storedET.StartsWith("UNSIGNED-BYTE") || storedET.StartsWith("SIGNED-BYTE") || storedET is "FIXNUM" or "INTEGER",
                "SINGLE-FLOAT" or "SHORT-FLOAT" => storedET is "SINGLE-FLOAT" or "SHORT-FLOAT" or "FLOAT",
                "DOUBLE-FLOAT" or "LONG-FLOAT" => storedET is "DOUBLE-FLOAT" or "LONG-FLOAT",
                "FLOAT" => storedET is "FLOAT" or "SINGLE-FLOAT" or "SHORT-FLOAT" or "DOUBLE-FLOAT" or "LONG-FLOAT",
                _ => storedET == etName
            };
        }
        if (elemTypeSpec is Cons etCons && etCons.Car is Symbol etHead)
        {
            string headN = etHead.Name;
            if (headN == "UNSIGNED-BYTE")
            {
                // (unsigned-byte n) — stored as "UNSIGNED-BYTE-n" or "UNSIGNED-BYTE" or "INTEGER"
                if (etCons.Cdr is Cons nbC && nbC.Car is Fixnum nbF)
                    return storedET == $"UNSIGNED-BYTE-{nbF.Value}" || storedET == "UNSIGNED-BYTE" || storedET == "INTEGER" || storedET == "FIXNUM";
                return storedET.StartsWith("UNSIGNED-BYTE") || storedET == "INTEGER" || storedET == "FIXNUM";
            }
            if (headN == "SIGNED-BYTE")
            {
                if (etCons.Cdr is Cons nbC2 && nbC2.Car is Fixnum nbF2)
                    return storedET == $"SIGNED-BYTE-{nbF2.Value}" || storedET == "SIGNED-BYTE" || storedET == "INTEGER" || storedET == "FIXNUM";
                return storedET.StartsWith("SIGNED-BYTE") || storedET == "INTEGER" || storedET == "FIXNUM";
            }
            if (headN == "COMPLEX") return storedET.StartsWith("COMPLEX") || storedET == "T";
            if (headN is "INTEGER" or "FIXNUM" or "BIGNUM" or "RATIONAL")
            {
                // (integer low high) — matches if stored element type is an integer-compatible type
                return storedET is "INTEGER" or "FIXNUM" or "BIGNUM"
                    || storedET.StartsWith("UNSIGNED-BYTE") || storedET.StartsWith("SIGNED-BYTE");
            }
            if (headN is "FLOAT" or "SHORT-FLOAT" or "SINGLE-FLOAT" or "DOUBLE-FLOAT" or "LONG-FLOAT")
            {
                return headN switch {
                    "SHORT-FLOAT" or "SINGLE-FLOAT" => storedET is "FLOAT" or "SINGLE-FLOAT" or "SHORT-FLOAT",
                    "DOUBLE-FLOAT" or "LONG-FLOAT" => storedET is "FLOAT" or "DOUBLE-FLOAT" or "LONG-FLOAT",
                    _ => storedET is "FLOAT" or "SINGLE-FLOAT" or "SHORT-FLOAT" or "DOUBLE-FLOAT" or "LONG-FLOAT"
                };
            }
        }
        return storedET == "T"; // fallback: generic vector accepts anything
    }

    private static bool ClassMatchesCPL(LispClass cls, string typeName)
    {
        foreach (var c in cls.ClassPrecedenceList)
            if (c.Name.Name == typeName) return true;
        return false;
    }

    // --- Subtypep ---

    // Numeric type names that support interval type specifiers
    private static readonly HashSet<string> _numericTypeNames = new()
    {
        "INTEGER", "FIXNUM", "RATIONAL", "RATIO",
        "REAL", "NUMBER", "FLOAT",
        "SINGLE-FLOAT", "DOUBLE-FLOAT", "SHORT-FLOAT", "LONG-FLOAT"
        // Note: BIGNUM intentionally omitted — handled specially via CheckSubtype/BigNum logic
    };

    // Parse interval bound: * → null (unbounded), (n) → exclusive n, n → inclusive n
    // Returns (value, inclusive) where null means unbounded
    private static (double? value, bool inclusive) ParseIntervalBound(LispObject? bound)
    {
        if (bound == null || (bound is Symbol bs && bs.Name == "*"))
            return (null, true);
        if (bound is Fixnum fi) return ((double)fi.Value, true);
        if (bound is Bignum bi) return ((double)bi.Value, true);
        if (bound is SingleFloat sf) return ((double)sf.Value, true);
        if (bound is DoubleFloat df) return (df.Value, true);
        if (bound is Cons bc && bc.Car != null)
        {
            // Exclusive bound: (n)
            if (bc.Car is Fixnum efi) return ((double)efi.Value, false);
            if (bc.Car is Bignum ebi) return ((double)ebi.Value, false);
            if (bc.Car is SingleFloat esf) return ((double)esf.Value, false);
            if (bc.Car is DoubleFloat edf) return (edf.Value, false);
        }
        return (null, true); // unknown → treat as unbounded
    }

    // Extract numeric interval from type spec like (INTEGER 0 10) or (INTEGER * 10) etc.
    // Returns (baseType, low, lowInclusive, high, highInclusive) or null
    private static (string baseType, double? low, bool lowInc, double? high, bool highInc)? ParseNumericInterval(LispObject typeSpec)
    {
        if (typeSpec is Symbol sym)
        {
            // UNSIGNED-BYTE = (INTEGER 0 *), SIGNED-BYTE = (INTEGER * *)
            if (sym.Name == "UNSIGNED-BYTE") return ("INTEGER", 0, true, null, true);
            if (sym.Name == "SIGNED-BYTE") return ("INTEGER", null, true, null, true);
            // BIT = (INTEGER 0 1)
            if (sym.Name == "BIT") return ("INTEGER", 0, true, 1, true);
            // FIXNUM = (INTEGER most-negative-fixnum most-positive-fixnum)
            if (sym.Name == "FIXNUM") return ("INTEGER", (double)long.MinValue, true, (double)long.MaxValue, true);
            if (_numericTypeNames.Contains(sym.Name)) return (sym.Name, null, true, null, true);
            return null;
        }
        if (typeSpec is not Cons c) return null;
        if (c.Car is not Symbol headSym) return null;
        string head = headSym.Name;
        // (UNSIGNED-BYTE n) = (INTEGER 0 (2^n-1)), treat as (INTEGER 0 *)
        if (head == "UNSIGNED-BYTE")
        {
            var nb = c.Cdr is Cons nc ? nc.Car : null;
            if (nb == null || (nb is Symbol ss && ss.Name == "*")) return ("INTEGER", 0, true, null, true);
            if (nb is Fixnum nf) { double hi = Math.Pow(2, nf.Value) - 1; return ("INTEGER", 0, true, hi, true); }
            return ("INTEGER", 0, true, null, true);
        }
        if (head == "SIGNED-BYTE")
        {
            var nb = c.Cdr is Cons nc ? nc.Car : null;
            if (nb == null || (nb is Symbol ss && ss.Name == "*")) return ("INTEGER", null, true, null, true);
            if (nb is Fixnum nf) { double hi = Math.Pow(2, nf.Value - 1) - 1; double lo = -Math.Pow(2, nf.Value - 1); return ("INTEGER", lo, true, hi, true); }
            return ("INTEGER", null, true, null, true);
        }
        if (!_numericTypeNames.Contains(head)) return null;

        LispObject? lowSpec = c.Cdr is Cons lc ? lc.Car : null;
        LispObject? highSpec = c.Cdr is Cons lc2 && lc2.Cdr is Cons hc ? hc.Car : null;

        var (low, lowInc) = ParseIntervalBound(lowSpec);
        var (high, highInc) = ParseIntervalBound(highSpec);
        return (head, low, lowInc, high, highInc);
    }

    // Normalize exclusive bounds for integer types (discrete intervals):
    // (integer (n)) low-exclusive = integer > n = integer >= n+1
    // (integer * (n)) high-exclusive = integer < n = integer <= n-1
    private static (double? val, bool inc) NormalizeBound(double? val, bool inc, bool isLow, string baseType)
    {
        if (!val.HasValue || inc) return (val, inc);
        // exclusive bound for integer type: convert to inclusive
        if (baseType is "INTEGER" or "FIXNUM")
            return (isLow ? val.Value + 1.0 : val.Value - 1.0, true);
        return (val, inc);
    }

    // Check if numeric interval 1 is a subtype of numeric interval 2
    private static bool NumericIntervalSubtype(
        string base1, double? low1, bool lowInc1, double? high1, bool highInc1,
        string base2, double? low2, bool lowInc2, double? high2, bool highInc2)
    {
        // Check base type relationship
        if (!CheckSubtype(base1, base2)) return false;

        // For integer types, normalize exclusive bounds to inclusive
        (low1, lowInc1) = NormalizeBound(low1, lowInc1, true, base1);
        (high1, highInc1) = NormalizeBound(high1, highInc1, false, base1);
        (low2, lowInc2) = NormalizeBound(low2, lowInc2, true, base2);
        (high2, highInc2) = NormalizeBound(high2, highInc2, false, base2);

        // Now check interval containment: [low1,high1] ⊆ [low2,high2]
        // low bound: low1 >= low2 (accounting for inclusive/exclusive)
        if (low2.HasValue)
        {
            if (!low1.HasValue) return false; // [*, ...] not subtype of [n, ...]
            double l1 = low1.Value, l2 = low2.Value;
            if (lowInc2)
            {
                // low2 is inclusive: need l1 >= l2
                if (l1 < l2) return false;
            }
            else
            {
                // low2 is exclusive: need l1 > l2, or l1 == l2 with l1 exclusive too
                if (l1 < l2) return false;
                if (l1 == l2 && lowInc1) return false; // inclusive doesn't fit in exclusive
            }
        }

        // high bound: high1 <= high2
        if (high2.HasValue)
        {
            if (!high1.HasValue) return false; // [..., *] not subtype of [..., n]
            double h1 = high1.Value, h2 = high2.Value;
            if (highInc2)
            {
                if (h1 > h2) return false;
            }
            else
            {
                if (h1 > h2) return false;
                if (h1 == h2 && highInc1) return false; // inclusive doesn't fit in exclusive
            }
        }

        return true;
    }

    // Per-thread set of type names expanded during a subtypep chain.
    // Once a type is expanded, it is NOT expanded again within the same subtypep call.
    // This prevents infinite recursion from circular deftypes like:
    //   (deftype rational (&optional lo hi) `(cl:rational ,lo ,hi))
    //   where cl:rational = rational, so expanding always produces the same compound type.
    // The set is cleared by Subtypep() at the top-level entry/exit.
    [ThreadStatic]
    private static HashSet<string>? s_expandingTypes;
    [ThreadStatic]
    private static int s_subtypepDepth;

    private static LispObject ExpandTypeSpecifier(LispObject typeSpec)
    {
        // Expand user-defined type specifiers (from deftype)
        if (typeSpec is Symbol sym && TypeExpanders.TryGetValue(sym.Name, out var exp1))
        {
            var expanding = s_expandingTypes ??= new HashSet<string>();
            if (!expanding.Add(sym.Name)) return typeSpec; // Already expanded in this chain
            return Funcall(exp1);
        }
        if (typeSpec is Cons cons)
        {
            string hd = cons.Car is Symbol sh ? sh.Name : "";
            if (!string.IsNullOrEmpty(hd) && TypeExpanders.TryGetValue(hd, out var exp2))
            {
                var expanding = s_expandingTypes ??= new HashSet<string>();
                if (!expanding.Add(hd)) return typeSpec; // Already expanded in this chain
                var args2 = ToList(cons.Cdr).ToArray();
                return Funcall(exp2, args2);
            }
        }
        return typeSpec;
    }

    // Public entry point for typexpand-1: one step of deftype expansion.
    // Returns (expanded-type . expanded?) where expanded? is T if expansion happened.
    public static LispObject TypeExpand1(LispObject[] args)
    {
        if (args.Length < 1) throw new LispErrorException(new LispProgramError("TYPEXPAND-1: requires 1 argument"));
        var typeSpec = args[0];
        LispObject expanded;
        if (typeSpec is Symbol sym2 && TypeExpanders.TryGetValue(sym2.Name, out var exp3))
            expanded = Funcall(exp3);
        else if (typeSpec is Cons cons2)
        {
            string hd = cons2.Car is Symbol sh2 ? sh2.Name : "";
            if (!string.IsNullOrEmpty(hd) && TypeExpanders.TryGetValue(hd, out var exp4))
            {
                var args4 = ToList(cons2.Cdr).ToArray();
                expanded = Funcall(exp4, args4);
            }
            else
                expanded = typeSpec;
        }
        else
            expanded = typeSpec;
        bool didExpand = !ReferenceEquals(expanded, typeSpec) && !expanded.Equals(typeSpec);
        return new Cons(expanded, didExpand ? T.Instance : Nil.Instance);
    }

    // Extract (car-type, cdr-type) strings from CONS compound type or null if not applicable
    private static (string car, string cdr)? ConsTypeArgs(LispObject typeSpec)
    {
        if (typeSpec is Cons c && c.Car is Symbol h && h.Name == "CONS")
        {
            var car = c.Cdr is Cons ca ? TypeSpecName(ca.Car) : "*";
            var cdr = c.Cdr is Cons cb && cb.Cdr is Cons cd ? TypeSpecName(cd.Car) : "*";
            return (car ?? "*", cdr ?? "*");
        }
        return null;
    }
    // Extract part type from COMPLEX compound type, or null if not COMPLEX
    // Returns "*" for bare COMPLEX, or the part type name for (COMPLEX type)
    private static string? ExtractComplexPartType(LispObject spec, string? name)
    {
        if (name == "COMPLEX") return "*"; // bare COMPLEX symbol
        if (spec is Cons c && c.Car is Symbol h && h.Name == "COMPLEX")
        {
            var arg = c.Cdr is Cons cc ? cc.Car : null;
            if (arg == null || (arg is Symbol ws && ws.Name == "*"))
                return "*";
            // For compound part types like (SINGLE-FLOAT 0.0 1.0), extract the head name
            return TypeSpecName(arg) ?? (arg is Cons ca && ca.Car is Symbol ch ? ch.Name : "REAL");
        }
        return null;
    }

    /// <summary>Extract the raw LispObject part type from a complex type specifier.</summary>
    private static LispObject? ExtractComplexPartTypeRaw(LispObject spec, string? name)
    {
        if (name == "COMPLEX") return null; // bare COMPLEX symbol → no specific part type
        if (spec is Cons c && c.Car is Symbol h && h.Name == "COMPLEX")
        {
            var arg = c.Cdr is Cons cc ? cc.Car : null;
            if (arg == null || (arg is Symbol ws && ws.Name == "*"))
                return null;
            return arg;
        }
        return null;
    }

    /// <summary>Compute the upgraded complex part type name for a given type name.</summary>
    private static string UpgradeComplexPartTypeName(string typeName)
    {
        return typeName switch
        {
            "NIL" => "NIL",
            "SINGLE-FLOAT" or "SHORT-FLOAT" => "SINGLE-FLOAT",
            "DOUBLE-FLOAT" or "LONG-FLOAT" => "DOUBLE-FLOAT",
            "*" => "REAL", // wildcard means any part type → REAL
            _ => "REAL"
        };
    }

    // Extract raw LispObject car/cdr from CONS compound type
    private static (LispObject car, LispObject cdr)? ConsTypeArgsRaw(LispObject typeSpec)
    {
        if (typeSpec is Cons c && c.Car is Symbol h && h.Name == "CONS")
        {
            var car = c.Cdr is Cons ca ? ca.Car : (LispObject)Startup.Sym("*");
            var cdr = c.Cdr is Cons cb && cb.Cdr is Cons cd ? cd.Car : (LispObject)Startup.Sym("*");
            return (car, cdr);
        }
        return null;
    }
    private static string? TypeSpecName(LispObject obj) => obj switch
    {
        Symbol s => s.Name, T => "T", Nil => "NIL", _ => null
    };
    private static bool IsNilType(string? t) => t == "NIL";
    private static bool IsWildOrTop(string? t) => t == "*" || t == "T";
    private static bool SubtypepBool(string t1, string t2)
    {
        if (t1 == t2) return true;
        if (t2 == "T" || t2 == "*") return true;
        return CheckSubtype(t1, t2);
    }

    // Array type info: (elementType, rank) where rank=-1 means "*" (any rank)
    // Returns null if not an array/vector type
    /// <summary>
    /// Internal marker for STRING/SIMPLE-STRING element type.
    /// STRING accepts CHARACTER, BASE-CHAR, or NIL as element subtypes,
    /// but is not itself a subtype of any specific char type (asymmetric).
    /// </summary>
    private const string CharOrNilMarker = "CHAR-OR-NIL";

    private struct ArrayTypeInfo
    {
        public string ElemType; // "*", type name, or CharOrNilMarker
        public int Rank;        // -1 = any, else specific rank
        public List<int?>? Dims; // null = wildcard rank, else specific dims (null entry = *)
    }
    private static ArrayTypeInfo? ExtractArrayTypeInfo(LispObject spec)
    {
        string typeName;
        LispObject? rest = null;
        if (spec is Symbol sym) typeName = sym.Name;
        else if (spec is T) typeName = "T";
        else if (spec is Nil) typeName = "NIL";
        else if (spec is Cons c && c.Car is Symbol headSym)
        {
            typeName = headSym.Name;
            rest = c.Cdr;
        }
        else return null;

        string et = "*";
        LispObject? dimSpec = null;

        if (typeName is "ARRAY" or "SIMPLE-ARRAY")
        {
            if (rest is Cons r1)
            {
                // For element type: use TypeSpecName for symbols, but compound types
                // (like (INTEGER 0 0), (UNSIGNED-BYTE 8)) need to be upgraded too.
                // * means any; compound types that aren't * map to their upgraded name.
                var elemObj = r1.Car;
                // Compound element types like (INTEGER 0 0), (UNSIGNED-BYTE 8) upgrade to T
                et = TypeSpecName(elemObj) ?? (elemObj is Cons ? "T" : "*");
                if (r1.Cdr is Cons r2) dimSpec = r2.Car;
            }
        }
        else if (typeName == "VECTOR")
        {
            // (vector et) or (vector et size) = rank 1
            if (rest is Cons r1) { et = TypeSpecName(r1.Car) ?? "*"; if (r1.Cdr is Cons r2) dimSpec = r2.Car; }
            return new ArrayTypeInfo { ElemType = et, Rank = 1, Dims = dimSpec == null ? null : ParseVectorDims(dimSpec) };
        }
        else if (typeName == "SIMPLE-VECTOR")
        {
            // SIMPLE-VECTOR = (SIMPLE-ARRAY T (*)); arg is size, not element type
            et = "T";
            if (rest is Cons r1) dimSpec = r1.Car; // (simple-vector size)
            return new ArrayTypeInfo { ElemType = et, Rank = 1, Dims = dimSpec == null ? null : ParseVectorDims(dimSpec) };
        }
        else if (typeName is "BIT-VECTOR" or "SIMPLE-BIT-VECTOR")
        {
            et = "BIT";
            if (rest is Cons r1) dimSpec = r1.Car;
            return new ArrayTypeInfo { ElemType = et, Rank = 1, Dims = dimSpec == null ? null : ParseVectorDims(dimSpec) };
        }
        // STRING/SIMPLE-STRING: element type is CHARACTER or BASE-CHAR or NIL (nil-vectors-are-strings).
        // Use special marker CharOrNilMarker so that CHARACTER/BASE-CHAR/NIL can be confirmed as subtypes
        // but CharOrNilMarker itself is NOT confirmed as subtype of specific char types (asymmetric).
        else if (typeName is "STRING" or "SIMPLE-STRING")
        {
            LispObject? sizeSpec = rest is Cons rs1 ? rs1.Car : null;
            return new ArrayTypeInfo { ElemType = CharOrNilMarker, Rank = 1,
                                       Dims = sizeSpec == null ? null : ParseVectorDims(sizeSpec) };
        }
        else if (typeName is "BASE-STRING" or "SIMPLE-BASE-STRING")
        {
            LispObject? sizeSpec = rest is Cons rs1 ? rs1.Car : null;
            return new ArrayTypeInfo { ElemType = "BASE-CHAR", Rank = 1,
                                       Dims = sizeSpec == null ? null : ParseVectorDims(sizeSpec) };
        }
        else return null;

        // Parse dimension spec for ARRAY/SIMPLE-ARRAY
        if (dimSpec == null || (dimSpec is Symbol ds0 && ds0.Name == "*"))
            return new ArrayTypeInfo { ElemType = et, Rank = -1, Dims = null };
        if (dimSpec is Nil) // NIL = 0-dimensional
            return new ArrayTypeInfo { ElemType = et, Rank = 0, Dims = new List<int?>() };
        if (dimSpec is Fixnum rf)
            return new ArrayTypeInfo { ElemType = et, Rank = (int)rf.Value, Dims = null };
        if (dimSpec is Cons dimList)
        {
            var dims = new List<int?>();
            LispObject cur = dimSpec;
            while (cur is Cons dc) {
                dims.Add(dc.Car is Fixnum df ? (int?)df.Value : null);
                cur = dc.Cdr;
            }
            return new ArrayTypeInfo { ElemType = et, Rank = dims.Count, Dims = dims };
        }
        return new ArrayTypeInfo { ElemType = et, Rank = -1, Dims = null };
    }
    private static List<int?> ParseVectorDims(LispObject dimSpec)
    {
        if (dimSpec is Symbol ds && ds.Name == "*") return new List<int?> { null };
        if (dimSpec is Fixnum df) return new List<int?> { (int)df.Value };
        return new List<int?> { null };
    }
    private static bool ArrayDimsSubtype(ArrayTypeInfo a1, ArrayTypeInfo a2)
    {
        // a2.Rank == -1 means any rank → always ok
        if (a2.Rank == -1) return true;
        // a1.Rank == -1 means any rank → not necessarily subtype of specific rank
        if (a1.Rank == -1) return false;
        if (a1.Rank != a2.Rank) return false;
        // Same rank: check specific dimension sizes
        if (a2.Dims == null) return true; // a2 doesn't constrain sizes (rank-only spec)
        // a2.Dims is all-wildcards → equivalent to no constraint
        if (a2.Dims.All(d => d == null)) return true;
        if (a1.Dims == null) return false; // a1 is unconstrained but a2 has specific sizes
        for (int i = 0; i < a1.Dims.Count && i < a2.Dims.Count; i++)
        {
            if (a2.Dims[i] == null) continue; // a2's dim is * → ok
            if (a1.Dims[i] == null) return false; // a1's dim is * but a2 requires specific
            if (a1.Dims[i] != a2.Dims[i]) return false;
        }
        return true;
    }
    /// <summary>
    /// Upgrade an array element type name to the canonical upgraded type.
    /// Per CLHS 15.1.2.1, (ARRAY type1) is a subtype of (ARRAY type2) iff
    /// upgraded-array-element-type of both are the same type.
    /// </summary>
    private static string UpgradeArrayElemType(string et)
    {
        return et switch {
            "*" => "*",
            "BIT" => "BIT",
            "CHARACTER" or "STANDARD-CHAR" => "CHARACTER",
            "BASE-CHAR" => "BASE-CHAR",
            "NIL" => "NIL",
            CharOrNilMarker => CharOrNilMarker,
            _ => "T"
        };
    }

    private static bool ArrayElemSubtype(string et1, string et2)
    {
        if (et2 == "*") return true;    // et2 is wildcard → always ok
        if (et1 == "*") return false;   // et1 is any but et2 requires specific
        // CHAR-OR-NIL is an internal type for STRING/SIMPLE-STRING element matching
        if (et2 == CharOrNilMarker) return et1 is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR" or "NIL";
        if (et1 == CharOrNilMarker) return false;
        // Per CLHS 15.1.2.1: (ARRAY t1) <: (ARRAY t2) iff upgraded types are the same
        return UpgradeArrayElemType(et1) == UpgradeArrayElemType(et2);
    }

    // CType routing metrics (enable via dotcl:%ctype-stats)
    private static long s_ctypeHits;
    private static long s_ctypeMisses;
    private static long s_ctypeErrors;
    public static string CTypeStats() =>
        $"hits={s_ctypeHits} misses={s_ctypeMisses} errors={s_ctypeErrors} " +
        $"rate={s_ctypeHits * 100 / Math.Max(1, s_ctypeHits + s_ctypeMisses)}%";

    public static LispObject Subtypep(LispObject type1, LispObject type2)
    {
        bool isTopLevel = s_subtypepDepth == 0;
        s_subtypepDepth++;
        try
        {
            return SubtypepImpl(type1, type2);
        }
        finally
        {
            if (--s_subtypepDepth == 0)
                s_expandingTypes?.Clear(); // Reset for next subtypep call
        }
    }

    private static LispObject SubtypepImpl(LispObject type1, LispObject type2)
    {
        // If both arguments are the same symbol, it's trivially a subtype of itself
        if (type1 is Symbol && ReferenceEquals(type1, type2))
        {
            MultipleValues.Set(T.Instance, T.Instance);
            return T.Instance;
        }

        // Expand user-defined types first
        var expanded1 = ExpandTypeSpecifier(type1);
        if (!ReferenceEquals(expanded1, type1)) return Subtypep(expanded1, type2);
        var expanded2 = ExpandTypeSpecifier(type2);
        if (!ReferenceEquals(expanded2, type2)) return Subtypep(type1, expanded2);

        // Normalize trivial compound types: (AND) → T, (OR) → NIL, (NOT T) → NIL, (NOT NIL) → T
        type1 = NormalizeTrivialCompound(type1);
        type2 = NormalizeTrivialCompound(type2);

        // Phase 2: CType-based subtypep routing
        try
        {
            var ct1 = TypeParser.Parse(type1);
            var ct2 = TypeParser.Parse(type2);
            var (result, certain) = CTypeOps.Subtypep(ct1, ct2);
            if (certain)
            {
                System.Threading.Interlocked.Increment(ref s_ctypeHits);
                MultipleValues.Set(result ? T.Instance : Nil.Instance, T.Instance);
                return result ? T.Instance : Nil.Instance;
            }
            System.Threading.Interlocked.Increment(ref s_ctypeMisses);
        }
        catch
        {
            System.Threading.Interlocked.Increment(ref s_ctypeErrors);
        }

        string? name1 = TypeSpecToName(type1);
        string? name2 = TypeSpecToName(type2);

        // NIL is the bottom type — subtype of everything (CLHS 4.2.2)
        if (name1 == "NIL")
        {
            MultipleValues.Set(T.Instance, T.Instance);
            return T.Instance;
        }

        if (name1 != null && name2 != null)
        {
            bool result = CheckSubtype(name1, name2);
            if (result)
            {
                MultipleValues.Set(T.Instance, T.Instance);
                return T.Instance;
            }
            // If name1 is not a known built-in type, not a registered user type (deftype),
            // and not a CLOS class, we cannot be certain it's not a subtype — return uncertain.
            // Per CLHS: subtypep may return (nil nil) when it cannot determine the relationship.
            bool type1Known = _typeAncestors.ContainsKey(name1)
                || TypeExpanders.ContainsKey(name1)
                || FindClassByName(name1) != null;
            if (!type1Known)
            {
                MultipleValues.Set(Nil.Instance, Nil.Instance);
                return Nil.Instance;
            }
            MultipleValues.Set(Nil.Instance, T.Instance);
            return Nil.Instance;
        }

        // Handle numeric interval type specifiers
        var interval1 = ParseNumericInterval(type1);
        var interval2 = ParseNumericInterval(type2);
        if (interval1.HasValue && interval2.HasValue)
        {
            var (base1, low1, lowInc1, high1, highInc1) = interval1.Value;
            var (base2, low2, lowInc2, high2, highInc2) = interval2.Value;
            bool result = NumericIntervalSubtype(base1, low1, lowInc1, high1, highInc1,
                                                  base2, low2, lowInc2, high2, highInc2);
            MultipleValues.Set(result ? T.Instance : Nil.Instance, T.Instance);
            return result ? T.Instance : Nil.Instance;
        }
        // type1 is compound numeric but type2 is a plain name
        if (interval1.HasValue && name2 != null)
        {
            var (base1, _, _, _, _) = interval1.Value;
            // Special: BIGNUM = integers entirely outside FIXNUM range.
            // Must use raw cons bounds to avoid double-precision loss near fixnum boundaries.
            if (name2 == "BIGNUM" && base1 == "INTEGER" && type1 is Cons intCons)
            {
                var rawLow = intCons.Cdr is Cons lbc ? lbc.Car : null;
                var rawHigh = intCons.Cdr is Cons lbc2 && lbc2.Cdr is Cons hbc ? hbc.Car : null;
                bool unboundedLow = rawLow == null || (rawLow is Symbol ws1 && ws1.Name == "*");
                bool unboundedHigh = rawHigh == null || (rawHigh is Symbol ws2 && ws2.Name == "*");
                // (INTEGER n n) where n is Bignum: singleton bignum range
                if (rawLow is Bignum && rawHigh is Bignum)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                // (INTEGER (max-fixnum) *): exclusive bound at MOST-POSITIVE-FIXNUM = positive bignums
                if (unboundedHigh && rawLow is Cons lowExcl && lowExcl.Car is Fixnum lef && lef.Value == long.MaxValue)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                // (INTEGER * (min-fixnum)): exclusive bound at MOST-NEGATIVE-FIXNUM = negative bignums
                if (unboundedLow && rawHigh is Cons highExcl && highExcl.Car is Fixnum hef && hef.Value == long.MinValue)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                // (INTEGER n n) where n is Fixnum: definitely not a bignum
                if (rawLow is Fixnum && rawHigh is Fixnum)
                { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                // Otherwise uncertain
                MultipleValues.Set(Nil.Instance, Nil.Instance);
                return Nil.Instance;
            }
            bool result = CheckSubtype(base1, name2);
            MultipleValues.Set(result ? T.Instance : Nil.Instance, T.Instance);
            return result ? T.Instance : Nil.Instance;
        }

        // type1 is a plain name, type2 is a numeric interval: check disjointness
        if (name1 != null && interval2.HasValue)
        {
            var (base2, low2, _, high2, _) = interval2.Value;
            // If name1's hierarchy doesn't include base2's hierarchy and vice versa, they're disjoint
            bool sub = CheckSubtype(name1, base2);
            bool super = CheckSubtype(base2, name1);
            if (sub)
            {
                // name1 ⊂ base2, but we must also check if interval bounds constrain it.
                // BIGNUM and INTEGER are unbounded, so they're NOT subtypes of any bounded interval.
                // FIXNUM has a fixed range, so check against interval bounds.
                bool interval2Bounded = low2.HasValue || high2.HasValue;
                if (interval2Bounded && name1 is "BIGNUM" or "INTEGER")
                {
                    // BIGNUM/INTEGER can exceed any finite bound
                    MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance;
                }
                if (interval2Bounded && name1 == "FIXNUM")
                {
                    // FIXNUM range: long.MinValue to long.MaxValue
                    bool fitsLow = !low2.HasValue || low2.Value <= (double)long.MinValue;
                    bool fitsHigh = !high2.HasValue || high2.Value >= (double)long.MaxValue;
                    bool fits = fitsLow && fitsHigh;
                    MultipleValues.Set(fits ? T.Instance : Nil.Instance, T.Instance);
                    return fits ? T.Instance : Nil.Instance;
                }
                MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
            }
            if (!super) { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
            // name1 is a supertype of base2: not a subtype of the interval
            MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance;
        }

        // Handle ARRAY/VECTOR compound type specifiers
        var arr1 = ExtractArrayTypeInfo(type1);
        var arr2 = ExtractArrayTypeInfo(type2);
        if (arr1.HasValue && arr2.HasValue)
        {
            bool elemOk = ArrayElemSubtype(arr1.Value.ElemType, arr2.Value.ElemType);
            bool dimsOk = ArrayDimsSubtype(arr1.Value, arr2.Value);
            bool res = elemOk && dimsOk;
            // Also check simple-array constraint
            bool simple1 = type1 is Cons c1h && c1h.Car is Symbol cs1 && cs1.Name.Contains("SIMPLE");
            if (!simple1 && name1 != null) simple1 = name1.Contains("SIMPLE");
            bool simpleRequired = type2 is Cons c2h && c2h.Car is Symbol cs2 && cs2.Name.Contains("SIMPLE");
            if (!simpleRequired && name2 != null) simpleRequired = name2.Contains("SIMPLE");
            if (simpleRequired && !simple1) res = false;
            MultipleValues.Set(res ? T.Instance : Nil.Instance, T.Instance);
            return res ? T.Instance : Nil.Instance;
        }
        // arr1 is array type but arr2 is plain name (non-array)
        if (arr1.HasValue && name2 != null)
        {
            bool isRank1 = arr1.Value.Rank == 1;
            bool isCharElem = arr1.Value.ElemType is "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR" or "NIL";
            bool isSimple = type1 is Cons c1s && c1s.Car is Symbol cs1s && cs1s.Name.Contains("SIMPLE");
            if (!isSimple && name1 != null) isSimple = name1.Contains("SIMPLE");
            bool res = name2 is "ARRAY" or "T" or "ATOM" ? true :
                       (name2 is "SEQUENCE" && isRank1) ? true :
                       (name2 == "VECTOR" && isRank1) ? true :
                       (name2 == "STRING" && isRank1 && isCharElem) ? true :
                       (name2 == "SIMPLE-STRING" && isRank1 && isCharElem && isSimple) ? true :
                       (name2 == "BASE-STRING" && isRank1 && isCharElem) ? true :
                       (name2 == "SIMPLE-BASE-STRING" && isRank1 && isCharElem && isSimple) ? true :
                       (name2 == "SIMPLE-ARRAY" && arr1.Value.ElemType != "*") ? false : // conservative
                       false;
            MultipleValues.Set(res ? T.Instance : Nil.Instance, T.Instance);
            return res ? T.Instance : Nil.Instance;
        }
        // arr2 is array type but arr1 is plain name (non-compound)
        if (arr2.HasValue && name1 != null)
        {
            bool res = name1 is "ARRAY" or "SIMPLE-ARRAY" ?
                (arr2.Value.ElemType == "*" && arr2.Value.Rank == -1) :
                false;
            if (!res)
            {
                // Check if name1 is an array subtype that matches arr2
                var info1 = ExtractArrayTypeInfo(Startup.Sym(name1));
                if (info1.HasValue)
                {
                    bool elemOk = ArrayElemSubtype(info1.Value.ElemType, arr2.Value.ElemType);
                    bool dimsOk = ArrayDimsSubtype(info1.Value, arr2.Value);
                    res = elemOk && dimsOk;
                    // simple constraint: info1 must be simple if arr2 requires it
                    if (res && arr2.Value.ElemType != "*")
                    {
                        bool simpleRequired2 = type2 is Cons c2s && c2s.Car is Symbol cs2s && cs2s.Name.Contains("SIMPLE");
                        bool isSimple1 = name1.Contains("SIMPLE");
                        if (simpleRequired2 && !isSimple1) res = false;
                    }
                }
            }
            MultipleValues.Set(res ? T.Instance : Nil.Instance, T.Instance);
            return res ? T.Instance : Nil.Instance;
        }

        // Handle COMPLEX compound type specifiers
        // (COMPLEX type1) <: (COMPLEX type2) iff upgraded-complex-part-type of both are same
        // Since our implementation upgrades everything to REAL, all (COMPLEX type) are equivalent
        {
            var cx1 = ExtractComplexPartType(type1, name1);
            var cx2 = ExtractComplexPartType(type2, name2);
            if (cx1 != null && cx2 != null)
            {
                // Both are COMPLEX types
                // cx="*" means bare COMPLEX = union of all complex subtypes
                if (cx1 == "*" && cx2 == "*")
                {
                    // Both bare COMPLEX → equivalent
                    MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
                }
                if (cx2 == "*")
                {
                    // (COMPLEX type) <: COMPLEX → always true
                    MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
                }
                if (cx1 == "*")
                {
                    // COMPLEX <: (COMPLEX type) → true only if all ucpt collapse to same
                    // COMPLEX includes SF, DF, and REAL complex numbers, so false unless type→REAL
                    // But actually bare COMPLEX = (COMPLEX *) which is all complex numbers
                    // It's only a subtype of (COMPLEX type) if (COMPLEX type) covers everything
                    // That only happens if all our upgraded types (REAL, SF, DF) equal ucpt(type)
                    // which is impossible since they differ. So: false.
                    MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance;
                }
                // Both are specific compound types — compare upgraded part types.
                // CLHS 12.1.5.3: (complex T1) <: (complex T2) if ucpt(T1) = ucpt(T2).
                // Also: if T1 <: T2, then (complex T1) <: (complex T2) by set inclusion,
                // because (complex T2) includes all specialized representations whose
                // part types are subtypes of T2.
                var up1 = UpgradeComplexPartTypeName(cx1);
                var up2 = UpgradeComplexPartTypeName(cx2);
                if (up1 == up2)
                {
                    MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
                }
                // Fallback: check if part type T1 <: part type T2 via raw subtypep
                var raw1 = ExtractComplexPartTypeRaw(type1, name1);
                var raw2 = ExtractComplexPartTypeRaw(type2, name2);
                if (raw1 != null && raw2 != null)
                {
                    var partSub = Subtypep(raw1, raw2);
                    var mv = MultipleValues.Get();
                    var partCertain = mv.Length > 1 ? mv[1] : Nil.Instance;
                    if (partSub != Nil.Instance && partCertain != Nil.Instance)
                    {
                        MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
                    }
                }
                MultipleValues.Set(Nil.Instance, T.Instance);
                return Nil.Instance;
            }
            if (cx1 != null && name2 != null)
            {
                // (COMPLEX type) <: COMPLEX, NUMBER, T
                bool res = name2 is "COMPLEX" or "NUMBER" or "T" or "ATOM";
                MultipleValues.Set(res ? T.Instance : Nil.Instance, T.Instance);
                return res ? T.Instance : Nil.Instance;
            }
            if (cx2 != null && name1 != null)
            {
                if (cx2 == "*")
                {
                    // name1 <: COMPLEX: true if name1 is COMPLEX
                    bool res2 = name1 == "COMPLEX";
                    MultipleValues.Set(res2 ? T.Instance : Nil.Instance, T.Instance);
                    return res2 ? T.Instance : Nil.Instance;
                }
                // name1 <: (COMPLEX type): COMPLEX <: (COMPLEX type) only if all ucpts
                // are subtypes of ucpt(type), which requires ucpt(type) = REAL
                bool res = name1 == "COMPLEX" && UpgradeComplexPartTypeName(cx2) == "REAL";
                MultipleValues.Set(res ? T.Instance : Nil.Instance, T.Instance);
                return res ? T.Instance : Nil.Instance;
            }
        }

        // Handle CONS compound type specifiers
        // (CONS A B): car is of type A, cdr is of type B. * means any type (same as T).
        // (CONS NIL ...) or (CONS ... NIL) = empty type (subtype of everything)
        // (CONS * *) = (CONS T T) = CONS (all conses)
        var cons1 = ConsTypeArgs(type1); // null if not CONS compound
        var cons2 = ConsTypeArgs(type2);
        // Normalize: CONS symbol → (CONS * *)
        var (car1, cdr1) = cons1 ?? (name1 == "CONS" ? ("*", "*") : (null!, null!));
        var (car2, cdr2) = cons2 ?? (name2 == "CONS" ? ("*", "*") : (null!, null!));
        if (car1 != null)
        {
            // (CONS NIL ...) or (CONS ... NIL) is empty type: subtype of everything
            if (IsNilType(car1) || IsNilType(cdr1))
            { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }

            if (car2 != null)
            {
                // type2 with NIL component: only empty types are subtypes
                if (IsNilType(car2) || IsNilType(cdr2))
                { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }

                // (CONS A B) <: (CONS C D) iff A <: C and B <: D (treating * as T)
                // Use raw LispObjects for compound type specifiers (OR, AND, etc.)
                var raw1 = ConsTypeArgsRaw(type1);
                var raw2 = ConsTypeArgsRaw(type2);
                bool carOk, cdrOk;
                bool certain = true;
                if (raw1.HasValue && raw2.HasValue)
                {
                    // Check wildcardness on the actual raw LispObject, not the string from ConsTypeArgs.
                    // ConsTypeArgs maps compound types (e.g. (satisfies foo)) to "*" which is
                    // indistinguishable from a true wildcard, causing false positives.
                    bool car2Wild = raw2.Value.car is Symbol sc2r && (sc2r.Name == "*" || sc2r.Name == "T");
                    bool cdr2Wild = raw2.Value.cdr is Symbol sd2r && (sd2r.Name == "*" || sd2r.Name == "T");
                    if (car2Wild)
                    {
                        carOk = true;
                    }
                    else
                    {
                        var carSub = Subtypep(raw1.Value.car, raw2.Value.car);
                        var carMv = MultipleValues.Get();
                        carOk = carSub != Nil.Instance;
                        if (!carOk && carMv[1] == Nil.Instance) certain = false;
                    }
                    if (cdr2Wild)
                    {
                        cdrOk = true;
                    }
                    else
                    {
                        var cdrSub = Subtypep(raw1.Value.cdr, raw2.Value.cdr);
                        var cdrMv = MultipleValues.Get();
                        cdrOk = cdrSub != Nil.Instance;
                        if (!cdrOk && cdrMv[1] == Nil.Instance) certain = false;
                    }
                }
                else
                {
                    carOk = IsWildOrTop(car1) || IsWildOrTop(car2) || SubtypepBool(car1, car2);
                    cdrOk = IsWildOrTop(cdr1) || IsWildOrTop(cdr2) || SubtypepBool(cdr1, cdr2);
                }
                bool res = carOk && cdrOk;
                if (!res && !certain)
                { MultipleValues.Set(Nil.Instance, Nil.Instance); return Nil.Instance; }
                MultipleValues.Set(res ? T.Instance : Nil.Instance, T.Instance);
                return res ? T.Instance : Nil.Instance;
            }
        }
        else if (car2 != null && name1 != null)
        {
            // type1 is a plain name (not CONS), type2 is CONS compound: name1 <: (CONS C D)?
            // Only if name1 is CONS (already handled above) or a subtype of CONS
            if (CheckSubtype(name1, "CONS"))
            {
                // Plain CONS <: (CONS * *) etc. handled via cons1 path above
                // Other CONS subtypes: conservatively return (NIL NIL)
            }
        }

        // Handle compound type specifiers
        // type2 is compound like (simple-array * (*)), (simple-array * 1), (array nil (*)), etc.
        if (name1 != null && type2 is Cons comp2)
        {
            string head2 = comp2.Car is Symbol sym2 ? sym2.Name : "";
            // (simple-array ...) or (array ...) as target type
            if (head2 is "SIMPLE-ARRAY" or "ARRAY")
            {
                // Check if name1 is a subtype of the compound array type
                // Extract element type and dimension spec from compound
                var elemSpec2 = comp2.Cdr is Cons ec2 ? ec2.Car : null;
                var dimSpec2 = comp2.Cdr is Cons ec2b && ec2b.Cdr is Cons dc2 ? dc2.Car : null;

                bool isSimpleRequired = head2 == "SIMPLE-ARRAY";
                string etName = elemSpec2 switch { Symbol esym2 => esym2.Name, Nil => "NIL", T => "T", _ => "*" };
                bool dimIsAny = dimSpec2 == null || (dimSpec2 is Symbol ds2 && ds2.Name == "*");
                bool dimIs1 = dimSpec2 is Fixnum df2 && df2.Value == 1;
                bool dimIs1List = dimSpec2 is Cons dc && !(dc.Car is Nil) && dc.Cdr is Nil; // (*)

                bool dimOk = dimIsAny || dimIs1 || dimIs1List;

                if (name1 is "SIMPLE-STRING" or "SIMPLE-BASE-STRING")
                {
                    if (!dimOk) { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                    if (isSimpleRequired)
                    {
                        // simple-string is subtype of (simple-array * (*)) if element type is * or character-compatible
                        // But NOT if element type is specifically "CHARACTER" or "BASE-CHAR" (nil arrays excluded)
                        if (etName is "*")
                        { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                        // CHARACTER/BASE-CHAR: nil element vectors are strings but not character-typed
                        MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance;
                    }
                    else
                    {
                        if (etName is "*" or "CHARACTER" or "BASE-CHAR")
                        { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                        MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
                    }
                }
                if (name1 is "STRING" or "BASE-STRING")
                {
                    // STRING is not necessarily simple
                    if (isSimpleRequired) { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                    if (!dimOk) { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                    if (etName is "*" or "CHARACTER" or "BASE-CHAR")
                    { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                    MultipleValues.Set(T.Instance, T.Instance); return T.Instance;
                }
            }

        }

        // type1 <: (AND t1 t2 ...): all members must be supertypes of type1
        // type1 <: (OR t1 t2 ...): if any member is a supertype of type1, return (T T)
        if (type2 is Cons comp2ao)
        {
            string head2ao = comp2ao.Car is Symbol sym2ao ? sym2ao.Name : "";
            if (head2ao == "AND")
            {
                var cur2 = comp2ao.Cdr;
                bool allSupertype = true;
                bool allCertain = true;
                while (cur2 is Cons ac)
                {
                    var sub2 = Subtypep(type1, ac.Car);
                    var mv2 = MultipleValues.Get();
                    if (sub2 == Nil.Instance)
                    {
                        allSupertype = false;
                        if (mv2[1] != Nil.Instance) { /* definite no */ }
                        else { allCertain = false; }
                    }
                    cur2 = ac.Cdr;
                }
                if (allSupertype)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                if (allCertain)
                { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                MultipleValues.Set(Nil.Instance, Nil.Instance); return Nil.Instance;
            }
            if (head2ao == "OR")
            {
                // Special case: (OR A B) <: (OR C D) iff every member of LHS is <: RHS
                if (type1 is Cons comp1or && comp1or.Car is Symbol sym1or && sym1or.Name == "OR")
                {
                    var cur1 = comp1or.Cdr;
                    bool allSubtype = true;
                    while (cur1 is Cons oc1)
                    {
                        var sub1 = Subtypep(oc1.Car, type2);
                        if (sub1 == Nil.Instance) { allSubtype = false; break; }
                        cur1 = oc1.Cdr;
                    }
                    if (allSubtype)
                    { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                    MultipleValues.Set(Nil.Instance, Nil.Instance); return Nil.Instance;
                }
                var cur2 = comp2ao.Cdr;
                bool anySupertype = false;
                bool allCertain = true;
                while (cur2 is Cons oc)
                {
                    var sub2 = Subtypep(type1, oc.Car);
                    var mv2 = MultipleValues.Get();
                    if (sub2 != Nil.Instance) { anySupertype = true; break; }
                    if (mv2[1] == Nil.Instance) { allCertain = false; }
                    cur2 = oc.Cdr;
                }
                if (anySupertype)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                // Can't conclude: type1 might still be subtype of the union
                // even if it's not a subtype of any individual member
                MultipleValues.Set(Nil.Instance, Nil.Instance); return Nil.Instance;
            }
        }

        // Handle (NOT inner) as type2
        if (type2 is Cons notCons && notCons.Car is Symbol notSym && notSym.Name == "NOT"
            && notCons.Cdr is Cons notBody && notBody.Cdr is Nil)
        {
            var inner = notBody.Car;
            // type1 <: (NOT inner): true iff type1 and inner are disjoint
            // First check: if type1 <: inner, then definitely NOT subtype of (NOT inner)
            var subOfInner = Subtypep(type1, inner);
            if (subOfInner != Nil.Instance)
            {
                MultipleValues.Set(Nil.Instance, T.Instance);
                return Nil.Instance;
            }
            // Use CType disjoint check (no recursive Subtypep — pure CType algebra)
            try
            {
                var ct1 = TypeParser.Parse(type1);
                var ctInner = TypeParser.Parse(inner);
                if (CTypeOps.AreDisjoint(ct1, ctInner))
                {
                    MultipleValues.Set(T.Instance, T.Instance);
                    return T.Instance;
                }
            }
            catch { /* Parse failed — return uncertain */ }
            MultipleValues.Set(Nil.Instance, Nil.Instance);
            return Nil.Instance;
        }

        // type1 is compound like (array nil (*)), (simple-array nil (*)), etc.
        if (type1 is Cons comp1 && name2 != null)
        {
            string head1 = comp1.Car is Symbol sym1 ? sym1.Name : "";
            if (head1 is "ARRAY" or "SIMPLE-ARRAY")
            {
                var elemSpec1 = comp1.Cdr is Cons ec1 ? ec1.Car : null;
                string etName1 = elemSpec1 switch { Symbol esym1 => esym1.Name, Nil => "NIL", T => "T", _ => "*" };
                bool isSimple1 = head1 == "SIMPLE-ARRAY";

                // (array nil (*)) and (simple-array nil (*)) are subtypes of string but NOT base-string
                if (etName1 == "NIL")
                {
                    if (name2 is "STRING" or "VECTOR" or "ARRAY" or "SEQUENCE" or "T")
                    { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                    if (name2 is "SIMPLE-STRING" or "SIMPLE-ARRAY")
                    {
                        if (isSimple1) { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                        MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance;
                    }
                    // nil arrays are NOT base-string or simple-base-string
                    if (name2 is "BASE-STRING" or "SIMPLE-BASE-STRING" or "SIMPLE-VECTOR")
                    { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                }
            }
        }

        // (MEMBER e1 e2 ...) <: type2: true if every member satisfies typep(e, type2)
        if (type1 is Cons memberCons && name2 != null)
        {
            string head1m = memberCons.Car is Symbol sym1m ? sym1m.Name : "";
            if (head1m == "MEMBER")
            {
                var cur = memberCons.Cdr;
                bool allMatch = true;
                while (cur is Cons mc)
                {
                    if (Typep(mc.Car, type2) == Nil.Instance) { allMatch = false; break; }
                    cur = mc.Cdr;
                }
                if (allMatch)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                else
                { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
            }

            // (INTEGER low high) <: type2 handled via name lookup (both are names)
            // (OR ...) compound subtypep: all branches must be subtypes
            string head1o = memberCons.Car is Symbol sym1o ? sym1o.Name : "";
            if (head1o == "OR")
            {
                var cur2 = memberCons.Cdr;
                bool allSubtype = true;
                bool allCertain = true;
                while (cur2 is Cons oc)
                {
                    var sub2 = Subtypep(oc.Car, type2);
                    var mv2 = MultipleValues.Get();
                    if (sub2 == Nil.Instance)
                    {
                        allSubtype = false;
                        if (mv2[1] == Nil.Instance) allCertain = false;
                        break;
                    }
                    cur2 = oc.Cdr;
                }
                if (allSubtype)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                if (allCertain)
                { MultipleValues.Set(Nil.Instance, T.Instance); return Nil.Instance; }
                MultipleValues.Set(Nil.Instance, Nil.Instance); return Nil.Instance;
            }

            // (AND t1 t2 ...) <: type2: if ANY member is subtype, then AND is subtype
            if (head1o == "AND")
            {
                var cur2 = memberCons.Cdr;
                bool anySubtype = false;
                bool allCertain = true;
                while (cur2 is Cons ac)
                {
                    var sub2 = Subtypep(ac.Car, type2);
                    var mv2 = MultipleValues.Get();
                    if (sub2 != Nil.Instance) { anySubtype = true; break; }
                    if (mv2[1] == Nil.Instance) { allCertain = false; }
                    cur2 = ac.Cdr;
                }
                if (anySubtype)
                { MultipleValues.Set(T.Instance, T.Instance); return T.Instance; }
                // Can't conclude: intersection of AND members might be empty
                // (disjoint types), making it a subtype of anything
                MultipleValues.Set(Nil.Instance, Nil.Instance); return Nil.Instance;
            }
        }

        // Can't determine the relationship
        MultipleValues.Set(Nil.Instance, Nil.Instance);
        return Nil.Instance;
    }

    private static string? TypeSpecToName(LispObject typeSpec)
    {
        switch (typeSpec)
        {
            case T: return "T";
            case Nil: return "NIL";
            case Symbol sym: return sym.Name;
            case LispClass cls: return cls.Name.Name;
            case Cons cons when cons.Car is Symbol head:
                // (COMPLEX), (COMPLEX *) → "COMPLEX"
                if (head.Name == "COMPLEX")
                {
                    var arg = cons.Cdr is Cons cc ? cc.Car : null;
                    if (arg == null || (arg is Symbol ws && ws.Name == "*"))
                        return "COMPLEX";
                }
                return null;
            default: return null;
        }
    }

    private static readonly HashSet<string> _compoundTypeHeads = new()
        { "NOT", "OR", "AND", "MEMBER", "EQL", "CONS", "SATISFIES" };

    private static bool HasCompoundTypeSpecifier(LispObject type)
    {
        return type is Cons c && c.Car is Symbol s && _compoundTypeHeads.Contains(s.Name);
    }

    private static LispObject NormalizeTrivialCompound(LispObject type)
    {
        if (type is not Cons c || c.Car is not Symbol s) return type;
        switch (s.Name)
        {
            case "AND" when c.Cdr is Nil:
                return T.Instance;                    // (AND) = T
            case "OR" when c.Cdr is Nil:
                return Nil.Instance;                  // (OR) = NIL
            case "AND" when c.Cdr is Cons ac && ac.Cdr is Nil:
                return NormalizeTrivialCompound(ac.Car);  // (AND x) = x
            case "OR" when c.Cdr is Cons oc && oc.Cdr is Nil:
                return NormalizeTrivialCompound(oc.Car);  // (OR x) = x
            case "NOT" when c.Cdr is Cons nc && nc.Cdr is Nil:
            {
                var inner = NormalizeTrivialCompound(nc.Car);
                if (inner is T) return Nil.Instance;          // (NOT T) = NIL (empty type)
                if (inner is Nil) return T.Instance;          // (NOT NIL) = T (universal type)
                if (!ReferenceEquals(inner, nc.Car))
                    return new Cons(s, new Cons(inner, Nil.Instance));
                return type;
            }
            default:
                return type;
        }
    }

    // Types that include CONSes (not subtypes of ATOM)
    private static readonly HashSet<string> _consContainingTypes = new()
        { "CONS", "LIST", "SEQUENCE" };

    /// <summary>Public accessor for CheckSubtype, used by CTypeOps.</summary>
    public static bool CheckSubtypeByName(string sub, string super) => CheckSubtype(sub, super);

    private static bool CheckSubtype(string sub, string super)
    {
        if (sub == super) return true;
        if (super == "T") return true;
        if (sub == "NIL") return true; // NIL is bottom type
        // ATOM = (not cons): any type that doesn't contain conses subtypep ATOM
        if (super == "ATOM" && !_consContainingTypes.Contains(sub))
            return true;
        if (_typeAncestors.TryGetValue(sub, out var ancestors))
            return ancestors.Contains(super);
        // Fall back to CLOS class precedence list for user-defined classes
        // Search by name across all registered classes (string-based fallback
        // for subtypep which operates on type names, not symbol identity)
        if (FindClassByName(sub) is LispClass subClass)
        {
            foreach (var c in subClass.ClassPrecedenceList)
                if (c.Name.Name == super) return true;
        }
        return false;
    }

    internal static readonly Dictionary<string, HashSet<string>> _typeAncestors = BuildTypeHierarchy();

    /// <summary>Check if a type name is a built-in type known to the hierarchy table.</summary>
    public static bool IsBuiltinTypeName(string name) => _typeAncestors.ContainsKey(name);

    private static Dictionary<string, HashSet<string>> BuildTypeHierarchy()
    {
        var parents = new Dictionary<string, string[]>
        {
            ["T"] = Array.Empty<string>(),
            ["NULL"] = new[] { "SYMBOL", "LIST", "SEQUENCE", "ATOM", "BOOLEAN", "T" },
            ["CONS"] = new[] { "LIST", "SEQUENCE", "T" },
            ["LIST"] = new[] { "SEQUENCE", "T" },
            ["SEQUENCE"] = new[] { "T" },
            ["SYMBOL"] = new[] { "T" },
            ["BOOLEAN"] = new[] { "SYMBOL", "T" },
            ["KEYWORD"] = new[] { "SYMBOL", "T" },
            ["ATOM"] = new[] { "T" },
            ["NUMBER"] = new[] { "T" },
            ["REAL"] = new[] { "NUMBER", "T" },
            ["RATIONAL"] = new[] { "REAL", "NUMBER", "T" },
            ["INTEGER"] = new[] { "SIGNED-BYTE", "RATIONAL", "REAL", "NUMBER", "T" },
            ["FIXNUM"] = new[] { "INTEGER", "SIGNED-BYTE", "RATIONAL", "REAL", "NUMBER", "T" },
            ["BIGNUM"] = new[] { "INTEGER", "SIGNED-BYTE", "RATIONAL", "REAL", "NUMBER", "T" },
            ["BIT"] = new[] { "UNSIGNED-BYTE", "SIGNED-BYTE", "FIXNUM", "INTEGER", "RATIONAL", "REAL", "NUMBER", "T" },
            ["UNSIGNED-BYTE"] = new[] { "SIGNED-BYTE", "INTEGER", "RATIONAL", "REAL", "NUMBER", "T" },
            ["SIGNED-BYTE"] = new[] { "INTEGER", "RATIONAL", "REAL", "NUMBER", "T" },
            ["RATIO"] = new[] { "RATIONAL", "REAL", "NUMBER", "T" },
            ["FLOAT"] = new[] { "REAL", "NUMBER", "T" },
            // SHORT-FLOAT=SINGLE-FLOAT and DOUBLE-FLOAT=LONG-FLOAT in our .NET impl
            ["SINGLE-FLOAT"] = new[] { "SHORT-FLOAT", "FLOAT", "REAL", "NUMBER", "T" },
            ["SHORT-FLOAT"] = new[] { "SINGLE-FLOAT", "FLOAT", "REAL", "NUMBER", "T" },
            ["DOUBLE-FLOAT"] = new[] { "LONG-FLOAT", "FLOAT", "REAL", "NUMBER", "T" },
            ["LONG-FLOAT"] = new[] { "DOUBLE-FLOAT", "FLOAT", "REAL", "NUMBER", "T" },
            ["COMPLEX"] = new[] { "NUMBER", "T" },
            ["CHARACTER"] = new[] { "T" },
            ["BASE-CHAR"] = new[] { "CHARACTER", "T" },
            ["STANDARD-CHAR"] = new[] { "BASE-CHAR", "CHARACTER", "T" },
            ["EXTENDED-CHAR"] = new[] { "CHARACTER", "T" },
            ["STRING"] = new[] { "VECTOR", "ARRAY", "SEQUENCE", "T" },
            ["SIMPLE-STRING"] = new[] { "STRING", "SIMPLE-ARRAY", "VECTOR", "ARRAY", "SEQUENCE", "T" },
            ["BASE-STRING"] = new[] { "STRING", "VECTOR", "ARRAY", "SEQUENCE", "T" },
            ["SIMPLE-BASE-STRING"] = new[] { "SIMPLE-STRING", "BASE-STRING", "STRING", "SIMPLE-ARRAY", "VECTOR", "ARRAY", "SEQUENCE", "T" },
            ["SIMPLE-ARRAY"] = new[] { "ARRAY", "T" },
            ["SIMPLE-VECTOR"] = new[] { "VECTOR", "SIMPLE-ARRAY", "ARRAY", "SEQUENCE", "T" },
            ["VECTOR"] = new[] { "ARRAY", "SEQUENCE", "T" },
            ["BIT-VECTOR"] = new[] { "VECTOR", "ARRAY", "SEQUENCE", "T" },
            ["SIMPLE-BIT-VECTOR"] = new[] { "BIT-VECTOR", "VECTOR", "SIMPLE-ARRAY", "ARRAY", "SEQUENCE", "T" },
            ["ARRAY"] = new[] { "T" },
            ["FUNCTION"] = new[] { "T" },
            ["COMPILED-FUNCTION"] = new[] { "FUNCTION", "T" },
            ["GENERIC-FUNCTION"] = new[] { "FUNCTION", "T" },
            ["STANDARD-GENERIC-FUNCTION"] = new[] { "GENERIC-FUNCTION", "FUNCTION", "T" },
            ["HASH-TABLE"] = new[] { "T" },
            ["RANDOM-STATE"] = new[] { "T" },
            ["PACKAGE"] = new[] { "T" },
            ["STREAM"] = new[] { "STANDARD-OBJECT", "T" },
            ["INPUT-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["OUTPUT-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["PATHNAME"] = new[] { "T" },
            ["LOGICAL-PATHNAME"] = new[] { "PATHNAME", "T" },
            ["RESTART"] = new[] { "T" },
            ["CONDITION"] = new[] { "STANDARD-OBJECT", "T" },
            ["SERIOUS-CONDITION"] = new[] { "CONDITION", "STANDARD-OBJECT", "T" },
            ["SIMPLE-CONDITION"] = new[] { "CONDITION", "STANDARD-OBJECT", "T" },
            ["ERROR"] = new[] { "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["SIMPLE-ERROR"] = new[] { "SIMPLE-CONDITION", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["SIMPLE-TYPE-ERROR"] = new[] { "SIMPLE-CONDITION", "TYPE-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["TYPE-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["PROGRAM-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["CONTROL-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["CELL-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["UNDEFINED-FUNCTION"] = new[] { "CELL-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["UNBOUND-SLOT"] = new[] { "CELL-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["UNBOUND-VARIABLE"] = new[] { "CELL-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["WARNING"] = new[] { "CONDITION", "STANDARD-OBJECT", "T" },
            ["SIMPLE-WARNING"] = new[] { "SIMPLE-CONDITION", "WARNING", "CONDITION", "STANDARD-OBJECT", "T" },
            ["STYLE-WARNING"] = new[] { "WARNING", "CONDITION", "STANDARD-OBJECT", "T" },
            ["ARITHMETIC-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["DIVISION-BY-ZERO"] = new[] { "ARITHMETIC-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["FLOATING-POINT-INEXACT"] = new[] { "ARITHMETIC-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["FLOATING-POINT-INVALID-OPERATION"] = new[] { "ARITHMETIC-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["FLOATING-POINT-OVERFLOW"] = new[] { "ARITHMETIC-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["FLOATING-POINT-UNDERFLOW"] = new[] { "ARITHMETIC-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["PACKAGE-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["PARSE-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["READER-ERROR"] = new[] { "PARSE-ERROR", "STREAM-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["PRINT-NOT-READABLE"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["FILE-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["STREAM-ERROR"] = new[] { "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["END-OF-FILE"] = new[] { "STREAM-ERROR", "ERROR", "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["STORAGE-CONDITION"] = new[] { "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["INTERACTIVE-INTERRUPT"] = new[] { "SERIOUS-CONDITION", "CONDITION", "STANDARD-OBJECT", "T" },
            ["STANDARD-OBJECT"] = new[] { "T" },
            ["STANDARD-CLASS"] = new[] { "STANDARD-OBJECT", "CLASS", "T" },
            ["BUILT-IN-CLASS"] = new[] { "CLASS", "STANDARD-OBJECT", "T" },
            ["STRUCTURE-CLASS"] = new[] { "CLASS", "STANDARD-OBJECT", "T" },
            ["STRUCTURE-OBJECT"] = new[] { "T" },
            ["CLASS"] = new[] { "STANDARD-OBJECT", "T" },
            ["METHOD"] = new[] { "STANDARD-OBJECT", "T" },
            ["STANDARD-METHOD"] = new[] { "METHOD", "STANDARD-OBJECT", "T" },
            ["FILE-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["STRING-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["BROADCAST-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["CONCATENATED-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["ECHO-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["SYNONYM-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
            ["TWO-WAY-STREAM"] = new[] { "STREAM", "STANDARD-OBJECT", "T" },
        };

        var result = new Dictionary<string, HashSet<string>>();
        foreach (var kv in parents)
            result[kv.Key] = new HashSet<string>(kv.Value);
        return result;
    }


}
