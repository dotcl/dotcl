using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    // --- Static resolution tables ---

    // The tables are built by the Tables class initializer, on the first use of an
    // assembler, so that a process that only registers functions (RegisterFunction)
    // does not reflect over the whole runtime at startup.
    private static Dictionary<string, MethodInfo> _methodCache => Tables._methodCache;
    private static Dictionary<string, FieldInfo> _fieldCache => Tables._fieldCache;
    private static Dictionary<string, ConstructorInfo> _ctorCache => Tables._ctorCache;
    private static Dictionary<string, Type> _typeCache => Tables._typeCache;
    private static MethodInfo _getConstant => Tables._getConstant;
    private static MethodInfo _getConstantObject => Tables._getConstantObject;
    private static MethodInfo _getConstantSymbol => Tables._getConstantSymbol;
    private static MethodInfo _symFnSiteResolve => Tables._symFnSiteResolve;
    private static MethodInfo _fnSiteSymbol => Tables._fnSiteSymbol;
    private static MethodInfo _symSiteResolve => Tables._symSiteResolve;
    private static MethodInfo _getUnitConstant => Tables._getUnitConstant;
    private static MethodInfo _makeFaslInstance => Tables._makeFaslInstance;
    private static MethodInfo _internViaEvalInstance => Tables._internViaEvalInstance;
    private static MethodInfo _makeClosure => Tables._makeClosure;
    private static MethodInfo _makeClosureDirect => Tables._makeClosureDirect;
    private static MethodInfo _checkArityExact => Tables._checkArityExact;
    private static MethodInfo _registerFunction => Tables._registerFunction;
    private static MethodInfo _registerFunctionOnSymbol => Tables._registerFunctionOnSymbol;
    private static MethodInfo _registerSetfFunctionOnSymbol => Tables._registerSetfFunctionOnSymbol;

    // One GetMethods() scan per type, indexed by name, instead of one GetMethod
    // call per table entry. Lookup results are the same MethodInfo objects.
    private static readonly Dictionary<Type, Dictionary<string, List<MethodInfo>>> _methodIndex = new();
    private const BindingFlags LookupFlags = BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static;

    private static MethodInfo? Mi(Type t, string name, Type[]? types = null)
    {
        if (!_methodIndex.TryGetValue(t, out var byName))
        {
            byName = new Dictionary<string, List<MethodInfo>>();
            foreach (var m in t.GetMethods(LookupFlags))
            {
                if (!byName.TryGetValue(m.Name, out var l)) byName[m.Name] = l = new List<MethodInfo>(2);
                l.Add(m);
            }
            _methodIndex[t] = byName;
        }
        MethodInfo? r = null;
        if (byName.TryGetValue(name, out var cands))
        {
            if (types == null)
            {
                if (cands.Count == 1) r = cands[0];
                else return t.GetMethod(name);
            }
            else
            {
                foreach (var c in cands)
                {
                    var ps = c.GetParameters();
                    if (ps.Length != types.Length) continue;
                    bool ok = true;
                    for (int i = 0; i < ps.Length; i++)
                        if (ps[i].ParameterType != types[i]) { ok = false; break; }
                    if (!ok) continue;
                    if (r != null) return t.GetMethod(name, types);
                    r = c;
                }
                if (r == null) return t.GetMethod(name, types);
            }
        }
        return r;
    }

    private static class Tables
    {
        internal static readonly Dictionary<string, MethodInfo> _methodCache;
        internal static readonly Dictionary<string, FieldInfo> _fieldCache;
        internal static readonly Dictionary<string, ConstructorInfo> _ctorCache;
        internal static readonly Dictionary<string, Type> _typeCache;
        internal static readonly MethodInfo _getConstant;
        internal static readonly MethodInfo _getConstantObject;
        internal static readonly MethodInfo _getConstantSymbol;
        internal static readonly MethodInfo _symFnSiteResolve;
        internal static readonly MethodInfo _fnSiteSymbol;
        internal static readonly MethodInfo _symSiteResolve;
        internal static readonly MethodInfo _getUnitConstant;
        internal static readonly MethodInfo _makeFaslInstance;
        internal static readonly MethodInfo _internViaEvalInstance;
        internal static readonly MethodInfo _makeClosure;
        internal static readonly MethodInfo _makeClosureDirect;
        internal static readonly MethodInfo _checkArityExact;
        internal static readonly MethodInfo _registerFunction;
        internal static readonly MethodInfo _registerFunctionOnSymbol;
        internal static readonly MethodInfo _registerSetfFunctionOnSymbol;

        static Tables()
        {
        _methodCache = new Dictionary<string, MethodInfo>
        {
            // Fixnum
            ["Fixnum.Make"] = Mi(typeof(Fixnum), "Make")!,
            ["DoubleFloat.Box"] = Mi(typeof(DoubleFloat), "Box")!,
            ["SingleFloat.Box"] = Mi(typeof(SingleFloat), "Box")!,

            // LispChar
            ["LispChar.Make"] = Mi(typeof(LispChar), "Make")!,

            // Runtime - arithmetic
            ["Runtime.Add"] = Mi(typeof(Runtime), "Add")!,
            ["Runtime.Subtract"] = Mi(typeof(Runtime), "Subtract")!,
            ["Runtime.Negate"] = Mi(typeof(Runtime), "Negate", new[] { typeof(LispObject) })!,
            ["Runtime.Increment"] = Mi(typeof(Runtime), "Increment")!,
            ["Runtime.Decrement"] = Mi(typeof(Runtime), "Decrement")!,
            ["Runtime.Multiply"] = Mi(typeof(Runtime), "Multiply")!,
            ["Runtime.MultiplyFixnum"] = Mi(typeof(Runtime), "MultiplyFixnum")!,
            ["Runtime.AddFixnum"] = Mi(typeof(Runtime), "AddFixnum")!,
            ["Runtime.AddFixnumChecked"] = Mi(typeof(Runtime), "AddFixnumChecked")!,
            ["Runtime.AddFixnumCheckedNonNeg"] = Mi(typeof(Runtime), "AddFixnumCheckedNonNeg")!,
            ["Runtime.AddOrMin"] = Mi(typeof(Runtime), "AddOrMin")!,
            ["Runtime.SubtractOrMin"] = Mi(typeof(Runtime), "SubtractOrMin")!,
            ["Runtime.MultiplyOrMin"] = Mi(typeof(Runtime), "MultiplyOrMin")!,
            ["Runtime.TypedArgL"] = Mi(typeof(Runtime), "TypedArgL")!,
            ["Runtime.TypedArgLR"] = Mi(typeof(Runtime), "TypedArgLR")!,
            ["Runtime.TypedArgD"] = Mi(typeof(Runtime), "TypedArgD")!,
            ["Runtime.TypedRetL"] = Mi(typeof(Runtime), "TypedRetL")!,
            ["Runtime.TypedRetD"] = Mi(typeof(Runtime), "TypedRetD")!,
            ["Runtime.CheckLongRange"] = Mi(typeof(Runtime), "CheckLongRange")!,
            ["Runtime.CheckArgFixnum"] = Mi(typeof(Runtime), "CheckArgFixnum")!,
            ["Runtime.CheckArgRange"] = Mi(typeof(Runtime), "CheckArgRange")!,
            ["Runtime.CheckArgU64"] = Mi(typeof(Runtime), "CheckArgU64")!,
            ["Runtime.CheckArgDouble"] = Mi(typeof(Runtime), "CheckArgDouble")!,
            ["Runtime.CheckArgSingle"] = Mi(typeof(Runtime), "CheckArgSingle")!,
            ["Runtime.CheckArgType"] = Mi(typeof(Runtime), "CheckArgType")!,
            ["Runtime.ArgTypeError"] = Mi(typeof(Runtime), "ArgTypeError")!,
            ["Runtime.CheckArgList"] = Mi(typeof(Runtime), "CheckArgList")!,
            ["Runtime.CheckArgCons"] = Mi(typeof(Runtime), "CheckArgCons")!,
            ["Runtime.CheckArgSymbol"] = Mi(typeof(Runtime), "CheckArgSymbol")!,
            ["Runtime.CheckArgFunction"] = Mi(typeof(Runtime), "CheckArgFunction")!,
            ["Runtime.CheckArgCharacter"] = Mi(typeof(Runtime), "CheckArgCharacter")!,
            ["Runtime.CheckArgInteger"] = Mi(typeof(Runtime), "CheckArgInteger")!,
            ["Runtime.CheckArgString"] = Mi(typeof(Runtime), "CheckArgString")!,
            ["Runtime.CheckArgStructSymKnown"] = Mi(typeof(Runtime), "CheckArgStructSymKnown")!,
            ["Runtime.CheckArgStructSym"] = Mi(typeof(Runtime), "CheckArgStructSym")!,
            ["Runtime.CheckArgSimpleVector"] = Mi(typeof(Runtime), "CheckArgSimpleVector")!,
            ["Runtime.CheckArgSimpleVectorN"] = Mi(typeof(Runtime), "CheckArgSimpleVectorN")!,
            ["Runtime.CheckTheArray"] = Mi(typeof(Runtime), "CheckTheArray")!,
            ["Runtime.SubtractFixnumChecked"] = Mi(typeof(Runtime), "SubtractFixnumChecked")!,
            ["Runtime.MultiplyFixnumChecked"] = Mi(typeof(Runtime), "MultiplyFixnumChecked")!,
            ["Runtime.SubtractFixnum"] = Mi(typeof(Runtime), "SubtractFixnum")!,
            ["Runtime.Divide"] = Mi(typeof(Runtime), "Divide")!,
            ["Runtime.AddN"] = Mi(typeof(Runtime), "AddN")!,
            ["Runtime.SubtractN"] = Mi(typeof(Runtime), "SubtractN")!,
            ["Runtime.MultiplyN"] = Mi(typeof(Runtime), "MultiplyN")!,
            ["Runtime.DivideN"] = Mi(typeof(Runtime), "DivideN")!,

            // Runtime - comparison
            ["Runtime.GreaterThan"] = Mi(typeof(Runtime), "GreaterThan")!,
            ["Runtime.LessThan"] = Mi(typeof(Runtime), "LessThan")!,
            ["Runtime.GreaterEqual"] = Mi(typeof(Runtime), "GreaterEqual")!,
            ["Runtime.LessEqual"] = Mi(typeof(Runtime), "LessEqual")!,
            ["Runtime.NumEqual"] = Mi(typeof(Runtime), "NumEqual")!,
            ["Runtime.NumNotEqual"] = Mi(typeof(Runtime), "NumNotEqual")!,
            ["Runtime.NumEqualN"] = Mi(typeof(Runtime), "NumEqualN")!,
            ["Runtime.NumNotEqualN"] = Mi(typeof(Runtime), "NumNotEqualN")!,
            ["Runtime.LessThanN"] = Mi(typeof(Runtime), "LessThanN")!,
            ["Runtime.GreaterThanN"] = Mi(typeof(Runtime), "GreaterThanN")!,
            ["Runtime.LessEqualN"] = Mi(typeof(Runtime), "LessEqualN")!,
            ["Runtime.GreaterEqualN"] = Mi(typeof(Runtime), "GreaterEqualN")!,

            // Runtime - bool-returning comparisons (fused comparison+branch)
            ["Runtime.IsTrueGt"] = Mi(typeof(Runtime), "IsTrueGt")!,
            ["Runtime.IsTrueLt"] = Mi(typeof(Runtime), "IsTrueLt")!,
            ["Runtime.IsTrueGe"] = Mi(typeof(Runtime), "IsTrueGe")!,
            ["Runtime.IsTrueLe"] = Mi(typeof(Runtime), "IsTrueLe")!,
            ["Runtime.IsTrueNumEq"] = Mi(typeof(Runtime), "IsTrueNumEq")!,
            ["Runtime.IsTrueZerop"] = Mi(typeof(Runtime), "IsTrueZerop")!,
            ["Runtime.Zerop"] = Mi(typeof(Runtime), "Zerop")!,
            ["AbsentKey.Is"] = Mi(typeof(AbsentKey), "Is")!,
            ["Runtime.IsTrueMinusp"] = Mi(typeof(Runtime), "IsTrueMinusp")!,
            ["Runtime.IsTruePlusp"] = Mi(typeof(Runtime), "IsTruePlusp")!,
            // Used by the inline symbol comparison the compiler emits in place of
            // an IsTrueEq/IsTrueEql call: the value still has to be normalized
            // before the reference compare.
            ["Runtime.Primary"] = Mi(typeof(Runtime), "Primary")!,
            ["Runtime.IsTrueEq"] = Mi(typeof(Runtime), "IsTrueEq")!,
            ["Runtime.IsNilObj"] = Mi(typeof(Runtime), "IsNilObj")!,
            ["Runtime.IsTrueEql"] = Mi(typeof(Runtime), "IsTrueEql")!,
            ["Runtime.IsTrueEqual"] = Mi(typeof(Runtime), "IsTrueEqual")!,
            ["Runtime.IsTrueEqualp"] = Mi(typeof(Runtime), "IsTrueEqualp")!,
            ["Runtime.IsTrueTypep"] = Mi(typeof(Runtime), "IsTrueTypep")!,
            ["Runtime.IsTrueConsp"] = Mi(typeof(Runtime), "IsTrueConsp")!,
            ["Runtime.IsTrueEndp"] = Mi(typeof(Runtime), "IsTrueEndp")!,
            ["Runtime.IsTrueAtom"] = Mi(typeof(Runtime), "IsTrueAtom")!,
            ["Runtime.IsTrueListp"] = Mi(typeof(Runtime), "IsTrueListp")!,
            ["Runtime.IsTrueNumberp"] = Mi(typeof(Runtime), "IsTrueNumberp")!,
            ["Runtime.IsTrueIntegerp"] = Mi(typeof(Runtime), "IsTrueIntegerp")!,
            ["Runtime.IsFixnumObject"] = Mi(typeof(Runtime), "IsFixnumObject")!,
            ["Runtime.IsConsObject"] = Mi(typeof(Runtime), "IsConsObject")!,
            ["Runtime.IsAtomObject"] = Mi(typeof(Runtime), "IsAtomObject")!,
            ["Runtime.IsListObject"] = Mi(typeof(Runtime), "IsListObject")!,
            ["Runtime.IsTrueSymbolp"] = Mi(typeof(Runtime), "IsTrueSymbolp")!,
            ["Runtime.IsTrueStringp"] = Mi(typeof(Runtime), "IsTrueStringp")!,
            ["Runtime.IsTrueCharacterp"] = Mi(typeof(Runtime), "IsTrueCharacterp")!,
            ["Runtime.IsTrueFunctionp"] = Mi(typeof(Runtime), "IsTrueFunctionp")!,

            // Runtime - equality
            ["Runtime.Eq"] = Mi(typeof(Runtime), "Eq")!,
            ["Runtime.Eql"] = Mi(typeof(Runtime), "Eql")!,
            ["Runtime.Equal"] = Mi(typeof(Runtime), "Equal")!,
            ["Runtime.Equalp"] = Mi(typeof(Runtime), "Equalp")!,

            // Runtime - list ops
            ["Runtime.Car"] = Mi(typeof(Runtime), "Car")!,
            ["Runtime.Cdr"] = Mi(typeof(Runtime), "Cdr")!,
            ["Runtime.MakeCons"] = Mi(typeof(Runtime), "MakeCons")!,
            ["Runtime.CheckUnaryArity"] = Mi(typeof(Runtime), "CheckUnaryArity")!,
            ["Runtime.CheckBinaryArity"] = Mi(typeof(Runtime), "CheckBinaryArity")!,
            ["Runtime.ProgramError"] = Mi(typeof(Runtime), "ProgramError", new[] { typeof(string) })!,
            ["Runtime.CheckArityExact"] = Mi(typeof(Runtime), "CheckArityExact")!,
            ["Runtime.CheckNumber1"] = Mi(typeof(Runtime), "CheckNumber1")!,
            ["Runtime.CheckReal1"] = Mi(typeof(Runtime), "CheckReal1")!,
            ["Runtime.CheckInteger1"] = Mi(typeof(Runtime), "CheckInteger1")!,
            ["Runtime.CheckArityMin"] = Mi(typeof(Runtime), "CheckArityMin")!,
            ["Runtime.CheckArityMax"] = Mi(typeof(Runtime), "CheckArityMax")!,
            ["Runtime.CheckNoUnknownKeys"] = Mi(typeof(Runtime), "CheckNoUnknownKeys")!,
            ["Runtime.CheckNoUnknownKeys2"] = Mi(typeof(Runtime), "CheckNoUnknownKeys2")!,
            ["Runtime.List"] = Mi(typeof(Runtime), "List")!,
            ["Runtime.ListStar"] = Mi(typeof(Runtime), "ListStar")!,
            ["Runtime.Append"] = Mi(typeof(Runtime), "Append")!,
            ["Runtime.Length"] = Mi(typeof(Runtime), "Length")!,
            ["Runtime.Rplaca"] = Mi(typeof(Runtime), "Rplaca")!,
            ["Runtime.Rplacd"] = Mi(typeof(Runtime), "Rplacd")!,
            ["Runtime.Nreverse"] = Mi(typeof(Runtime), "Nreverse")!,
            ["Runtime.Nth"] = Mi(typeof(Runtime), "Nth")!,
            ["Runtime.Nthcdr"] = Mi(typeof(Runtime), "Nthcdr")!,
            ["Runtime.Last"] = Mi(typeof(Runtime), "Last")!,
            ["Runtime.Nconc2"] = Mi(typeof(Runtime), "Nconc2")!,
            ["Runtime.NconcDestructive2"] = Mi(typeof(Runtime), "NconcDestructive2")!,
            ["Runtime.Butlast"] = Mi(typeof(Runtime), "Butlast")!,
            ["Runtime.CopyList"] = Mi(typeof(Runtime), "CopyList")!,
            ["Runtime.Member"] = Mi(typeof(Runtime), "Member")!,
            ["Runtime.MemberEq"] = Mi(typeof(Runtime), "MemberEq")!,
            ["Runtime.FindEql"] = Mi(typeof(Runtime), "FindEql")!,
            ["Runtime.FindEqual"] = Mi(typeof(Runtime), "FindEqual")!,
            ["Runtime.FindEqualp"] = Mi(typeof(Runtime), "FindEqualp")!,
            ["Runtime.FindCharEq"] = Mi(typeof(Runtime), "FindCharEq")!,
            ["Runtime.FindCharEqual"] = Mi(typeof(Runtime), "FindCharEqual")!,
            ["Runtime.PositionEql"] = Mi(typeof(Runtime), "PositionEql")!,
            ["Runtime.PositionEqual"] = Mi(typeof(Runtime), "PositionEqual")!,
            ["Runtime.PositionEqualp"] = Mi(typeof(Runtime), "PositionEqualp")!,
            ["Runtime.PositionCharEqual"] = Mi(typeof(Runtime), "PositionCharEqual")!,
            ["Runtime.PositionCharEq"] = Mi(typeof(Runtime), "PositionCharEq")!,
            ["Runtime.PositionLiteralString"] = Mi(typeof(Runtime), "PositionLiteralString")!,
            ["Runtime.FindLiteralString"] = Mi(typeof(Runtime), "FindLiteralString")!,
            ["Runtime.Assoc"] = Mi(typeof(Runtime), "Assoc")!,
            ["Runtime.AssocEq"] = Mi(typeof(Runtime), "AssocEq")!,
            ["Runtime.Cadr"] = Mi(typeof(Runtime), "Cadr")!,
            ["Runtime.Cddr"] = Mi(typeof(Runtime), "Cddr")!,
            ["Runtime.Caar"] = Mi(typeof(Runtime), "Caar")!,
            ["Runtime.Cdar"] = Mi(typeof(Runtime), "Cdar")!,
            ["Runtime.Caddr"] = Mi(typeof(Runtime), "Caddr")!,

            // Runtime - predicates
            ["Runtime.Not"] = Mi(typeof(Runtime), "Not")!,
            ["Runtime.Atom"] = Mi(typeof(Runtime), "Atom")!,
            ["Runtime.Consp"] = Mi(typeof(Runtime), "Consp")!,
            ["Runtime.Endp"] = Mi(typeof(Runtime), "Endp")!,
            ["Runtime.IsNotNil"] = Mi(typeof(Runtime), "IsNotNil")!,
            ["Runtime.Listp"] = Mi(typeof(Runtime), "Listp")!,
            ["Runtime.Numberp"] = Mi(typeof(Runtime), "Numberp")!,
            ["Runtime.Integerp"] = Mi(typeof(Runtime), "Integerp")!,
            ["Runtime.Symbolp"] = Mi(typeof(Runtime), "Symbolp")!,
            ["Runtime.Stringp"] = Mi(typeof(Runtime), "Stringp")!,
            ["Runtime.Characterp"] = Mi(typeof(Runtime), "Characterp")!,
            ["Runtime.Functionp"] = Mi(typeof(Runtime), "Functionp")!,
            ["Runtime.Rationalp"] = Mi(typeof(Runtime), "Rationalp")!,
            ["Runtime.Floatp"] = Mi(typeof(Runtime), "Floatp")!,
            ["Runtime.Complexp"] = Mi(typeof(Runtime), "Complexp")!,
            ["Runtime.Vectorp"] = Mi(typeof(Runtime), "Vectorp")!,
            ["Runtime.Hash_table_p"] = Mi(typeof(Runtime), "Hash_table_p")!,
            ["Runtime.Packagep"] = Mi(typeof(Runtime), "Packagep")!,
            ["Runtime.Keywordp"] = Mi(typeof(Runtime), "Keywordp")!,
            ["Runtime.TypeOf"] = Mi(typeof(Runtime), "TypeOf")!,
            ["Runtime.Typep"] = Mi(typeof(Runtime), "Typep")!,
            ["Runtime.SetChar"] = Mi(typeof(Runtime), "SetChar")!,
            ["Runtime.SetElt"] = Mi(typeof(Runtime), "SetElt")!,
            ["Runtime.Putf"] = Mi(typeof(Runtime), "Putf")!,
            ["Runtime.IsTruthy"] = Mi(typeof(Runtime), "IsTruthy")!,
            ["Runtime.CharAccess"] = Mi(typeof(Runtime), "CharAccess")!,
            ["Runtime.CharSet"] = Mi(typeof(Runtime), "CharSet")!,
            ["Runtime.CharEqual"] = Mi(typeof(Runtime), "CharEqual")!,
            ["Runtime.Maphash"] = Mi(typeof(Runtime), "Maphash")!,
            ["Runtime.HashWalkBegin"] = Mi(typeof(Runtime), "HashWalkBegin")!,
            ["Runtime.HashWalkNext"] = Mi(typeof(Runtime), "HashWalkNext")!,
            ["Runtime.HashWalkKey"] = Mi(typeof(Runtime), "HashWalkKey")!,
            ["Runtime.HashWalkValue"] = Mi(typeof(Runtime), "HashWalkValue")!,
            ["Runtime.HashWalkEnd"] = Mi(typeof(Runtime), "HashWalkEnd")!,

            // Runtime - math
            ["Runtime.Abs"] = Mi(typeof(Runtime), "Abs")!,
            // Raw magnitude for a value already on the stack as r8/r4. ABS of a
            // float is a float of the same format, so a declared-float ABS needs
            // neither the boxed argument nor the boxed result the general entry
            // requires. Math.Abs and the general entry agree on the edges that
            // matter here: -0.0 gives +0.0 and NaN stays NaN.
            ["Math.AbsDouble"] = Mi(typeof(System.Math), "Abs", new[] { typeof(double) })!,
            ["Math.AbsSingle"] = Mi(typeof(System.Math), "Abs", new[] { typeof(float) })!,
            // SQRT of a declared double. The checked entry is the (safety 1) one:
            // a negative argument means the declared type is wrong.
            ["Math.SqrtDouble"] = Mi(typeof(System.Math), "Sqrt", new[] { typeof(double) })!,
            ["Runtime.SqrtDoubleChecked"] = Mi(typeof(Runtime), "SqrtDoubleChecked")!,
            // LOG of a declared double, under the same rule as SQRT.
            ["Runtime.LogDouble"] = Mi(typeof(Runtime), "LogDouble")!,
            ["Runtime.LogDoubleChecked"] = Mi(typeof(Runtime), "LogDoubleChecked")!,
            // SIN, COS, ... of a declared double: a double for every double
            // argument, the same System.Math call the generic entries make.
            ["Math.SinDouble"] = Mi(typeof(System.Math), "Sin", new[] { typeof(double) })!,
            ["Math.CosDouble"] = Mi(typeof(System.Math), "Cos", new[] { typeof(double) })!,
            ["Math.TanDouble"] = Mi(typeof(System.Math), "Tan", new[] { typeof(double) })!,
            ["Math.AtanDouble"] = Mi(typeof(System.Math), "Atan", new[] { typeof(double) })!,
            ["Math.SinhDouble"] = Mi(typeof(System.Math), "Sinh", new[] { typeof(double) })!,
            ["Math.CoshDouble"] = Mi(typeof(System.Math), "Cosh", new[] { typeof(double) })!,
            ["Math.TanhDouble"] = Mi(typeof(System.Math), "Tanh", new[] { typeof(double) })!,
            ["Runtime.ExpDouble"] = Mi(typeof(Runtime), "ExpDouble")!,
            ["Runtime.AddChecked"] = Mi(typeof(Runtime), "AddChecked")!,
            ["Runtime.SubtractChecked"] = Mi(typeof(Runtime), "SubtractChecked")!,
            ["Runtime.MultiplyChecked"] = Mi(typeof(Runtime), "MultiplyChecked")!,
            ["Runtime.DivideChecked"] = Mi(typeof(Runtime), "DivideChecked")!,
            ["Runtime.CoerceChecked"] = Mi(typeof(Runtime), "CoerceChecked")!,
            ["Math.ExpDouble"] = Mi(typeof(System.Math), "Exp", new[] { typeof(double) })!,
            ["FloatLane.AddD"] = Mi(typeof(FloatLane), "AddD")!,
            ["FloatLane.SubtractD"] = Mi(typeof(FloatLane), "SubtractD")!,
            ["FloatLane.MultiplyD"] = Mi(typeof(FloatLane), "MultiplyD")!,
            ["FloatLane.DivideD"] = Mi(typeof(FloatLane), "DivideD")!,
            ["FloatLane.AddS"] = Mi(typeof(FloatLane), "AddS")!,
            ["FloatLane.SubtractS"] = Mi(typeof(FloatLane), "SubtractS")!,
            ["FloatLane.MultiplyS"] = Mi(typeof(FloatLane), "MultiplyS")!,
            ["FloatLane.DivideS"] = Mi(typeof(FloatLane), "DivideS")!,
            ["FloatLane.NarrowD"] = Mi(typeof(FloatLane), "NarrowD")!,
            ["FloatLane.NonFiniteD"] = Mi(typeof(FloatLane), "NonFiniteD")!,
            ["FloatLane.SinD"] = Mi(typeof(FloatLane), "SinD")!,
            ["FloatLane.LogD"] = Mi(typeof(FloatLane), "LogD")!,
            ["Runtime.ExptChecked"] = Mi(typeof(Runtime), "ExptChecked")!,
            ["FloatLane.CosD"] = Mi(typeof(FloatLane), "CosD")!,
            ["FloatLane.TanD"] = Mi(typeof(FloatLane), "TanD")!,
            ["FloatLane.NonFiniteS"] = Mi(typeof(FloatLane), "NonFiniteS")!,
            ["FloatLane.SinhD"] = Mi(typeof(FloatLane), "SinhD")!,
            ["FloatLane.CoshD"] = Mi(typeof(FloatLane), "CoshD")!,
            ["Runtime.FloatIntegerS"] = Mi(typeof(Runtime), "FloatIntegerS")!,
            ["Runtime.FloatIntegerD"] = Mi(typeof(Runtime), "FloatIntegerD")!,
            ["Math.FloorDouble"] = Mi(typeof(System.Math), "Floor", new[] { typeof(double) })!,
            ["Math.CeilingDouble"] = Mi(typeof(System.Math), "Ceiling", new[] { typeof(double) })!,
            ["Math.TruncateDouble"] = Mi(typeof(System.Math), "Truncate", new[] { typeof(double) })!,
            ["Math.RoundDouble"] = Mi(typeof(System.Math), "Round", new[] { typeof(double) })!,
            ["Runtime.Mod"] = Mi(typeof(Runtime), "Mod")!,
            ["Runtime.Rem"] = Mi(typeof(Runtime), "Rem")!,
            ["Runtime.FloorOp"] = Mi(typeof(Runtime), "FloorOp")!,
            ["Runtime.TruncateOp"] = Mi(typeof(Runtime), "TruncateOp")!,
            ["Runtime.CeilingOp"] = Mi(typeof(Runtime), "CeilingOp")!,
            ["Runtime.RoundOp"] = Mi(typeof(Runtime), "RoundOp")!,
            // Single-value twins: the peephole swaps these in when the call site
            // discards the secondary value (see MultipleValues.Values2Primary).
            ["Runtime.FloorOpPrimary"] = Mi(typeof(Runtime), "FloorOpPrimary")!,
            ["Runtime.TruncateOpPrimary"] = Mi(typeof(Runtime), "TruncateOpPrimary")!,
            ["Runtime.CeilingOpPrimary"] = Mi(typeof(Runtime), "CeilingOpPrimary")!,
            ["Runtime.RoundOpPrimary"] = Mi(typeof(Runtime), "RoundOpPrimary")!,
            ["Runtime.FloorOpPair"] = Mi(typeof(Runtime), "FloorOpPair")!,
            ["Runtime.TruncateOpPair"] = Mi(typeof(Runtime), "TruncateOpPair")!,
            ["Runtime.CeilingOpPair"] = Mi(typeof(Runtime), "CeilingOpPair")!,
            ["Runtime.RoundOpPair"] = Mi(typeof(Runtime), "RoundOpPair")!,
            ["Runtime.Min"] = Mi(typeof(Runtime), "Min")!,
            ["Math.MinLong"] = Mi(typeof(System.Math), "Min", new[] { typeof(long), typeof(long) })!,
            ["Math.MaxLong"] = Mi(typeof(System.Math), "Max", new[] { typeof(long), typeof(long) })!,
            ["Runtime.Max"] = Mi(typeof(Runtime), "Max")!,
            ["Runtime.MakeStructRaw"] = Mi(typeof(Runtime), "MakeStructRaw")!,
            ["Runtime.DoubleBits"] = Mi(typeof(Runtime), "DoubleBits")!,
            ["Runtime.SingleBits"] = Mi(typeof(Runtime), "SingleBits")!,
            ["Runtime.MinDouble"] = Mi(typeof(Runtime), "MinDouble")!,
            ["Runtime.MaxDouble"] = Mi(typeof(Runtime), "MaxDouble")!,
            ["Runtime.Gcd"] = Mi(typeof(Runtime), "Gcd")!,
            ["Runtime.Lcm"] = Mi(typeof(Runtime), "Lcm")!,
            ["Runtime.Expt"] = Mi(typeof(Runtime), "Expt")!,
            ["Runtime.Logior"] = Mi(typeof(Runtime), "Logior")!,
            ["Runtime.Logand"] = Mi(typeof(Runtime), "Logand")!,
            ["Runtime.Logxor"] = Mi(typeof(Runtime), "Logxor")!,
            ["Runtime.Logior2"] = Mi(typeof(Runtime), "Logior2")!,
            ["Runtime.Low64"] = Mi(typeof(Runtime), "Low64")!,
            ["Runtime.Ldb"] = Mi(typeof(Runtime), "Ldb")!,
            ["Runtime.Dpb"] = Mi(typeof(Runtime), "Dpb")!,
            ["Runtime.IsU64"] = Mi(typeof(Runtime), "IsU64")!,
            ["Runtime.CheckU64Arg"] = Mi(typeof(Runtime), "CheckU64Arg")!,
            ["Runtime.BoxU64"] = Mi(typeof(Runtime), "BoxU64")!,
            ["Runtime.ShrU64"] = Mi(typeof(Runtime), "ShrU64")!,
            ["Runtime.RotateLeft32"] = Mi(typeof(Runtime), "RotateLeft32")!,
            ["Runtime.RotateLeft64"] = Mi(typeof(Runtime), "RotateLeft64")!,
            ["Runtime.RotateByte"] = Mi(typeof(Runtime), "RotateByte")!,
            ["Runtime.ReadU16LE"] = Mi(typeof(Runtime), "ReadU16LE")!,
            ["Runtime.ReadU16BE"] = Mi(typeof(Runtime), "ReadU16BE")!,
            ["Runtime.ReadU32LE"] = Mi(typeof(Runtime), "ReadU32LE")!,
            ["Runtime.ReadU32BE"] = Mi(typeof(Runtime), "ReadU32BE")!,
            ["Runtime.ReadU64LE"] = Mi(typeof(Runtime), "ReadU64LE")!,
            ["Runtime.ReadU64BE"] = Mi(typeof(Runtime), "ReadU64BE")!,
            ["Runtime.OctetWordRef"] = Mi(typeof(Runtime), "OctetWordRef")!,
            ["Runtime.OctetWordRefChecked"] = Mi(typeof(Runtime), "OctetWordRefChecked")!,
            ["Runtime.WriteU16LE"] = Mi(typeof(Runtime), "WriteU16LE")!,
            ["Runtime.WriteU16BE"] = Mi(typeof(Runtime), "WriteU16BE")!,
            ["Runtime.WriteU32LE"] = Mi(typeof(Runtime), "WriteU32LE")!,
            ["Runtime.WriteU32BE"] = Mi(typeof(Runtime), "WriteU32BE")!,
            ["Runtime.WriteU64LE"] = Mi(typeof(Runtime), "WriteU64LE")!,
            ["Runtime.WriteU64BE"] = Mi(typeof(Runtime), "WriteU64BE")!,
            ["Runtime.OctetWordSet"] = Mi(typeof(Runtime), "OctetWordSet")!,
            ["Runtime.OctetWordSetChecked"] = Mi(typeof(Runtime), "OctetWordSetChecked")!,
            ["Runtime.ShlU64"] = Mi(typeof(Runtime), "ShlU64")!,
            ["Runtime.Logand2"] = Mi(typeof(Runtime), "Logand2")!,
            ["Runtime.Logxor2"] = Mi(typeof(Runtime), "Logxor2")!,
            ["Runtime.Lognot"] = Mi(typeof(Runtime), "Lognot")!,
            ["Runtime.Logcount"] = Mi(typeof(Runtime), "Logcount")!,
            ["Runtime.Logtest"] = Mi(typeof(Runtime), "Logtest")!,
            ["Runtime.Ash"] = Mi(typeof(Runtime), "Ash")!,
            ["Runtime.AshLeftLong"] = Mi(typeof(Runtime), "AshLeftLong")!,
            ["Runtime.IntegerLength"] = Mi(typeof(Runtime), "IntegerLength")!,
            ["Runtime.Logbitp"] = Mi(typeof(Runtime), "Logbitp")!,

            // Runtime - I/O
            ["Runtime.Print"] = Mi(typeof(Runtime), "Print")!,
            ["Runtime.Print2"] = Mi(typeof(Runtime), "Print2")!,
            ["Runtime.Prin1"] = Mi(typeof(Runtime), "Prin1")!,
            ["Runtime.Prin12"] = Mi(typeof(Runtime), "Prin12")!,
            ["Runtime.Princ"] = Mi(typeof(Runtime), "Princ")!,
            ["Runtime.Princ2"] = Mi(typeof(Runtime), "Princ2")!,
            ["Runtime.Terpri"] = Mi(typeof(Runtime), "Terpri")!,
            ["Runtime.FreshLine"] = Mi(typeof(Runtime), "FreshLine")!,

            // Runtime - string
            ["Runtime.StringUpcase"] = Mi(typeof(Runtime), "StringUpcase")!,
            ["Runtime.StringDowncase"] = Mi(typeof(Runtime), "StringDowncase")!,
            ["Runtime.StringTrim"] = Mi(typeof(Runtime), "StringTrim")!,
            ["Runtime.StringLeftTrim"] = Mi(typeof(Runtime), "StringLeftTrim")!,
            ["Runtime.StringRightTrim"] = Mi(typeof(Runtime), "StringRightTrim")!,
            ["Runtime.CharCode"] = Mi(typeof(Runtime), "CharCode")!,
            ["Runtime.CodeChar"] = Mi(typeof(Runtime), "CodeChar")!,
            ["Runtime.DigitCharP"] = Mi(typeof(Runtime), "DigitCharP")!,
            ["Runtime.AlphaCharP"] = Mi(typeof(Runtime), "AlphaCharP")!,
            ["Runtime.Alphanumericp"] = Mi(typeof(Runtime), "Alphanumericp")!,
            ["Runtime.UpperCaseP"] = Mi(typeof(Runtime), "UpperCaseP")!,
            ["Runtime.LowerCaseP"] = Mi(typeof(Runtime), "LowerCaseP")!,
            ["Runtime.BothCaseP"] = Mi(typeof(Runtime), "BothCaseP")!,
            ["Runtime.GraphicCharP"] = Mi(typeof(Runtime), "GraphicCharP")!,
            ["Runtime.CharUpcase"] = Mi(typeof(Runtime), "CharUpcase")!,
            ["Runtime.CharDowncase"] = Mi(typeof(Runtime), "CharDowncase")!,

            // Runtime - symbol
            ["Runtime.SymbolName"] = Mi(typeof(Runtime), "SymbolName")!,
            ["Runtime.SymbolPackage"] = Mi(typeof(Runtime), "SymbolPackage")!,
            ["Runtime.SetSymbolValue"] = Mi(typeof(Runtime), "SetSymbolValue")!,
            ["Runtime.GetProp"] = Mi(typeof(Runtime), "GetProp")!,
            ["Runtime.PutProp"] = Mi(typeof(Runtime), "PutProp")!,
            ["Runtime.Remprop"] = Mi(typeof(Runtime), "Remprop")!,
            ["Runtime.CopySymbol"] = Mi(typeof(Runtime), "CopySymbol")!,
            ["Runtime.CopySymbolFull"] = Mi(typeof(Runtime), "CopySymbolFull")!,

            // Runtime - string comparison
            ["Runtime.StringEq"] = Mi(typeof(Runtime), "StringEq")!,
            ["Runtime.StringLt"] = Mi(typeof(Runtime), "StringLt")!,
            ["Runtime.StringGt"] = Mi(typeof(Runtime), "StringGt")!,
            ["Runtime.StringLe"] = Mi(typeof(Runtime), "StringLe")!,
            ["Runtime.StringGe"] = Mi(typeof(Runtime), "StringGe")!,
            ["Runtime.StringNotEq"] = Mi(typeof(Runtime), "StringNotEq")!,
            ["Runtime.String"] = Mi(typeof(Runtime), "String")!,

            // Runtime - sequence operations
            ["Runtime.Elt"] = Mi(typeof(Runtime), "Elt")!,
            ["Runtime.Subseq"] = Mi(typeof(Runtime), "Subseq")!,
            ["Runtime.MakeArrayOfType"] = Mi(typeof(Runtime), "MakeArrayOfType")!,
            ["Runtime.MakeArrayOfTypeInit"] = Mi(typeof(Runtime), "MakeArrayOfTypeInit")!,
            ["Runtime.MakeArrayLike"] = Mi(typeof(Runtime), "MakeArrayLike")!,
            ["Runtime.CheckArgSingleRange"] = Mi(typeof(Runtime), "CheckArgSingleRange")!,
            ["Runtime.CheckArgDoubleRange"] = Mi(typeof(Runtime), "CheckArgDoubleRange")!,
            ["Runtime.MakeArrayLikeInit"] = Mi(typeof(Runtime), "MakeArrayLikeInit")!,
            ["Runtime.ReplaceKeys"] = Mi(typeof(Runtime), "ReplaceKeys")!,
            ["Runtime.Concatenate"] = Mi(typeof(Runtime), "Concatenate")!,
            ["Runtime.Sort"] = Mi(typeof(Runtime), "Sort")!,
            ["Runtime.StableSort"] = Mi(typeof(Runtime), "StableSort")!,
            ["Runtime.Reverse"] = Mi(typeof(Runtime), "Reverse")!,
            ["Runtime.Coerce"] = Mi(typeof(Runtime), "Coerce")!,
            ["Runtime.Search"] = Mi(typeof(Runtime), "Search")!,

            // Runtime - higher-order
            ["Runtime.Apply"] = Mi(typeof(Runtime), "Apply")!,
            ["Runtime.ApplySpread1"] = Mi(typeof(Runtime), "ApplySpread1")!,
            ["Runtime.ApplyM"] = Mi(typeof(Runtime), "ApplyM")!,
            ["Runtime.ApplySpread1M"] = Mi(typeof(Runtime), "ApplySpread1M")!,
            ["Runtime.ApplySpread2M"] = Mi(typeof(Runtime), "ApplySpread2M")!,
            ["Runtime.ApplySpread2"] = Mi(typeof(Runtime), "ApplySpread2")!,
            ["Runtime.Mapcar"] = Mi(typeof(Runtime), "Mapcar")!,
            ["Runtime.MapcarN"] = Mi(typeof(Runtime), "MapcarN")!,
            ["Runtime.NthValueOf"] = Mi(typeof(Runtime), "NthValueOf")!,
            ["Runtime.Mapcar2"] = Mi(typeof(Runtime), "Mapcar2")!,

            // Runtime - rest args
            ["Runtime.CollectRestArgs"] = Mi(typeof(Runtime), "CollectRestArgs")!,

            // Runtime - hash table
            ["Runtime.MakeHashTable"] = Mi(typeof(Runtime), "MakeHashTable")!,
            ["Runtime.MakeHashTable0"] = Mi(typeof(Runtime), "MakeHashTable0")!,
            ["Runtime.Gethash"] = Mi(typeof(Runtime), "Gethash")!,
            ["Runtime.GethashPrimary"] = Mi(typeof(Runtime), "GethashPrimary")!,
            ["Runtime.GethashPair"] = Mi(typeof(Runtime), "GethashPair")!,
            ["Runtime.Puthash"] = Mi(typeof(Runtime), "Puthash")!,
            ["Runtime.Remhash"] = Mi(typeof(Runtime), "Remhash")!,

            // Runtime - values
            ["Runtime.Values"] = Mi(typeof(Runtime), "Values")!,
            ["Runtime.Values2"] = Mi(typeof(Runtime), "Values2")!,
            ["Runtime.Values2Primary"] = Mi(typeof(Runtime), "Values2Primary")!,
            ["Runtime.Values2Pair"] = Mi(typeof(Runtime), "Values2Pair")!,
            ["MultipleValues.CapturePair"] = Mi(typeof(MultipleValues), "CapturePair")!,
            ["MultipleValues.PairNth"] = Mi(typeof(MultipleValues), "PairNth")!,
            ["Runtime.MultipleValuesList"] = Mi(typeof(Runtime), "MultipleValuesList")!,
            ["Runtime.MultipleValuesList1"] = Mi(typeof(Runtime), "MultipleValuesList1")!,
            ["Runtime.UnwrapMv"] = Mi(typeof(Runtime), "UnwrapMv")!,
            ["Runtime.XrefNote"] = Mi(typeof(Runtime), "XrefNote")!,

            // DebugFrames: frame-locals mode only (see DebugFrames.cs)
            ["DebugFrames.Enter"] = Mi(typeof(DebugFrames), "Enter")!,
            ["DebugFrame.Set"] = Mi(typeof(DebugFrame), "Set")!,
            ["DebugFrame.SetBox"] = Mi(typeof(DebugFrame), "SetBox")!,

            // ConditionSystem: loop back-edge interrupt safepoint
            ["ConditionSystem.PollInterrupt"] = Mi(typeof(ConditionSystem), "PollInterrupt")!,
            ["ConditionSystem.PollPending"] = Mi(typeof(ConditionSystem), "PollPending")!,
            ["ConditionSystem.PollSavesFloats"] = Mi(typeof(ConditionSystem), "PollSavesFloats")!,
            ["ConditionSystem.PollInterruptSaved"] = Mi(typeof(ConditionSystem), "PollInterruptSaved")!,
            ["ConditionSystem.PollSave1"] = Mi(typeof(ConditionSystem), "PollSave1")!,
            ["ConditionSystem.PollSave2"] = Mi(typeof(ConditionSystem), "PollSave2")!,
            ["ConditionSystem.PollSave3"] = Mi(typeof(ConditionSystem), "PollSave3")!,
            ["ConditionSystem.PollSave4"] = Mi(typeof(ConditionSystem), "PollSave4")!,
            ["ConditionSystem.PollSaveD"] = Mi(typeof(ConditionSystem), "PollSaveD")!,
            ["ConditionSystem.PollLoadD"] = Mi(typeof(ConditionSystem), "PollLoadD")!,

            // MultipleValues
            ["MultipleValues.Reset"] = Mi(typeof(MultipleValues), "Reset")!,
            ["MultipleValues.ResetOnExit"] = Mi(typeof(MultipleValues), "ResetOnExit")!,
            ["MultipleValues.TakeMode"] = Mi(typeof(MultipleValues), "TakeMode")!,
            ["MultipleValues.PassMode"] = Mi(typeof(MultipleValues), "PassMode")!,
            ["MultipleValues.Values2Mode"] = Mi(typeof(MultipleValues), "Values2Mode")!,
            ["MultipleValues.Values3Mode"] = Mi(typeof(MultipleValues), "Values3Mode")!,
            ["MultipleValues.Values4Mode"] = Mi(typeof(MultipleValues), "Values4Mode")!,
            ["MultipleValues.Values5Mode"] = Mi(typeof(MultipleValues), "Values5Mode")!,
            ["MultipleValues.Values6Mode"] = Mi(typeof(MultipleValues), "Values6Mode")!,
            ["MultipleValues.Values7Mode"] = Mi(typeof(MultipleValues), "Values7Mode")!,
            ["MultipleValues.Values8Mode"] = Mi(typeof(MultipleValues), "Values8Mode")!,
            ["MultipleValues.ValuesArrayMode"] = Mi(typeof(MultipleValues), "ValuesArrayMode")!,
            ["MultipleValues.CallBound"] = Mi(typeof(MultipleValues), "CallBound")!,
            ["MultipleValues.Primary"] = Mi(typeof(MultipleValues), "Primary")!,
            ["MultipleValues.PrimaryAlone"] = Mi(typeof(MultipleValues), "PrimaryAlone")!,
            ["MultipleValues.SaveCount"] = Mi(typeof(MultipleValues), "SaveCount")!,
            ["MultipleValues.SaveValues"] = Mi(typeof(MultipleValues), "SaveValues")!,
            ["MultipleValues.HoldValues"] = Mi(typeof(MultipleValues), "HoldValues")!,
            ["MultipleValues.RestoreValues"] = Mi(typeof(MultipleValues), "RestoreValues")!,
            ["MultipleValues.RestoreSaved"] = Mi(typeof(MultipleValues), "RestoreSaved")!,

            // Runtime - typep / error
            ["Runtime.Subtypep"] = Mi(typeof(Runtime), "Subtypep")!,
            ["Runtime.SubtypepMv"] = Mi(typeof(Runtime), "SubtypepMv")!,
            ["Runtime.SubtypepPrimary"] = Mi(typeof(Runtime), "SubtypepPrimary")!,
            ["Runtime.SubtypepPair"] = Mi(typeof(Runtime), "SubtypepPair")!,
            ["Runtime.Aref"] = Mi(typeof(Runtime), "Aref")!,
            ["Runtime.ArefSet"] = Mi(typeof(Runtime), "ArefSet")!,
            ["Runtime.ArefMulti"] = Mi(typeof(Runtime), "ArefMulti")!,
            ["Runtime.ArefSetMulti"] = Mi(typeof(Runtime), "ArefSetMulti")!,
            ["Runtime.Aref2D"] = Mi(typeof(Runtime), "Aref2D")!,
            ["Runtime.ArefSet2D"] = Mi(typeof(Runtime), "ArefSet2D")!,
            ["Runtime.Aref3D"] = Mi(typeof(Runtime), "Aref3D")!,
            ["Runtime.ArefSet3D"] = Mi(typeof(Runtime), "ArefSet3D")!,
            // Raw-long-index variants (no Fixnum box for statically-fixnum indices)
            ["Runtime.ArefL"] = Mi(typeof(Runtime), "ArefL")!,
            ["Runtime.ArefSetL"] = Mi(typeof(Runtime), "ArefSetL")!,
            ["Runtime.Aref2DL"] = Mi(typeof(Runtime), "Aref2DL")!,
            ["Runtime.ArefSet2DL"] = Mi(typeof(Runtime), "ArefSet2DL")!,
            ["Runtime.Aref3DL"] = Mi(typeof(Runtime), "Aref3DL")!,
            ["Runtime.ArefSet3DL"] = Mi(typeof(Runtime), "ArefSet3DL")!,
            ["Runtime.ArefCopy11L"] = Mi(typeof(Runtime), "ArefCopy11L")!,
            ["Runtime.ArefCopy12L"] = Mi(typeof(Runtime), "ArefCopy12L")!,
            ["Runtime.FuncallAref1L"] = Mi(typeof(Runtime), "FuncallAref1L")!,
            ["Runtime.FuncallAref1LM"] = Mi(typeof(Runtime), "FuncallAref1LM")!,
            ["Runtime.FuncallAref2L"] = Mi(typeof(Runtime), "FuncallAref2L")!,
            ["Runtime.FuncallAref2LM"] = Mi(typeof(Runtime), "FuncallAref2LM")!,
            ["Runtime.FuncallAref1LL"] = Mi(typeof(Runtime), "FuncallAref1LL")!,
            ["Runtime.FuncallAref2LL"] = Mi(typeof(Runtime), "FuncallAref2LL")!,
            ["Runtime.FuncallLong1"] = Mi(typeof(Runtime), "FuncallLong1")!,
            ["Runtime.FuncallLong1M"] = Mi(typeof(Runtime), "FuncallLong1M")!,
            ["Runtime.FuncallLong1L"] = Mi(typeof(Runtime), "FuncallLong1L")!,
            ["Runtime.ArefCopy21L"] = Mi(typeof(Runtime), "ArefCopy21L")!,
            ["Runtime.ArefCopy22L"] = Mi(typeof(Runtime), "ArefCopy22L")!,
            ["Runtime.ArefSetLong1L"] = Mi(typeof(Runtime), "ArefSetLong1L")!,
            ["Runtime.ArefSetLong2L"] = Mi(typeof(Runtime), "ArefSetLong2L")!,
            // Raw-long-value variants for inferred numeric-backed array locals
            ["Runtime.IndexL"] = Mi(typeof(Runtime), "IndexL")!,
            ["Runtime.ArefNumL"] = Mi(typeof(Runtime), "ArefNumL")!,
            ["Runtime.BackingChars"] = Mi(typeof(Runtime), "BackingChars")!,
            ["Runtime.BackingI64"] = Mi(typeof(Runtime), "BackingI64")!,
            ["Runtime.BackingI32"] = Mi(typeof(Runtime), "BackingI32")!,
            ["Runtime.BackingU16"] = Mi(typeof(Runtime), "BackingU16")!,
            ["Runtime.BackingU8"] = Mi(typeof(Runtime), "BackingU8")!,
            ["Runtime.BackingF64"] = Mi(typeof(Runtime), "BackingF64")!,
            ["Runtime.BackingF32"] = Mi(typeof(Runtime), "BackingF32")!,
            // Above (safety 0): the same fetches, signalling on a non-simple array
            ["Runtime.BackingCharsChecked"] = Mi(typeof(Runtime), "BackingCharsChecked")!,
            ["Runtime.BackingI64Checked"] = Mi(typeof(Runtime), "BackingI64Checked")!,
            ["Runtime.BackingI32Checked"] = Mi(typeof(Runtime), "BackingI32Checked")!,
            ["Runtime.BackingU16Checked"] = Mi(typeof(Runtime), "BackingU16Checked")!,
            ["Runtime.BackingU8Checked"] = Mi(typeof(Runtime), "BackingU8Checked")!,
            ["Runtime.BackingF64Checked"] = Mi(typeof(Runtime), "BackingF64Checked")!,
            ["Runtime.BackingF32Checked"] = Mi(typeof(Runtime), "BackingF32Checked")!,
            // Element access the compiled code has proven in range
            ["Runtime.DataU8"] = Mi(typeof(Runtime), "DataU8")!,
            ["Runtime.DataU16"] = Mi(typeof(Runtime), "DataU16")!,
            ["Runtime.DataI32"] = Mi(typeof(Runtime), "DataI32")!,
            ["Runtime.DataI64"] = Mi(typeof(Runtime), "DataI64")!,
            ["Runtime.DataF64"] = Mi(typeof(Runtime), "DataF64")!,
            ["Runtime.DataF32"] = Mi(typeof(Runtime), "DataF32")!,
            ["Runtime.ConstantArrayChanged"] = Mi(typeof(Runtime), "ConstantArrayChanged")!,
            ["Runtime.AtU8"] = Mi(typeof(Runtime), "AtU8")!,
            ["Runtime.AtU16"] = Mi(typeof(Runtime), "AtU16")!,
            ["Runtime.AtI32"] = Mi(typeof(Runtime), "AtI32")!,
            ["Runtime.AtI64"] = Mi(typeof(Runtime), "AtI64")!,
            ["Runtime.AtF64"] = Mi(typeof(Runtime), "AtF64")!,
            ["Runtime.AtF32"] = Mi(typeof(Runtime), "AtF32")!,
            ["Runtime.SetAtU8"] = Mi(typeof(Runtime), "SetAtU8")!,
            ["Runtime.SetAtU16"] = Mi(typeof(Runtime), "SetAtU16")!,
            ["Runtime.SetAtI32"] = Mi(typeof(Runtime), "SetAtI32")!,
            ["Runtime.SetAtI64"] = Mi(typeof(Runtime), "SetAtI64")!,
            ["Runtime.SetAtF64"] = Mi(typeof(Runtime), "SetAtF64")!,
            ["Runtime.SetAtF32"] = Mi(typeof(Runtime), "SetAtF32")!,
            // Above (safety 0), matching the element type's width as well
            ["Runtime.BackingI64Narrow"] = Mi(typeof(Runtime), "BackingI64Narrow")!,
            ["Runtime.BackingI32Narrow"] = Mi(typeof(Runtime), "BackingI32Narrow")!,
            ["Runtime.BackingU16Narrow"] = Mi(typeof(Runtime), "BackingU16Narrow")!,
            ["Runtime.BackingU8Narrow"] = Mi(typeof(Runtime), "BackingU8Narrow")!,
            ["Runtime.IsNonSimpleArray"] = Mi(typeof(Runtime), "IsNonSimpleArray")!,
            ["Runtime.SignalDeclaredNotSimple"] = Mi(typeof(Runtime), "SignalDeclaredNotSimple")!,
            ["Runtime.CheckStoreI32"] = Mi(typeof(Runtime), "CheckStoreI32")!,
            ["Runtime.CheckStoreU16"] = Mi(typeof(Runtime), "CheckStoreU16")!,
            ["Runtime.CheckStoreU8"] = Mi(typeof(Runtime), "CheckStoreU8")!,
            ["Runtime.CheckStoreUnsigned"] = Mi(typeof(Runtime), "CheckStoreUnsigned")!,
            ["Runtime.CheckStoreSigned"] = Mi(typeof(Runtime), "CheckStoreSigned")!,
            // (unsigned-byte 64) elements as their bits (kind 11)
            ["Runtime.BackingU64"] = Mi(typeof(Runtime), "BackingU64")!,
            ["Runtime.BackingU64Checked"] = Mi(typeof(Runtime), "BackingU64Checked")!,
            ["Runtime.ArefU64Bits"] = Mi(typeof(Runtime), "ArefU64Bits")!,
            ["Runtime.ArefSetU64Bits"] = Mi(typeof(Runtime), "ArefSetU64Bits")!,
            ["Runtime.ArefU64Bits2D"] = Mi(typeof(Runtime), "ArefU64Bits2D")!,
            ["Runtime.ArefU64Bits3D"] = Mi(typeof(Runtime), "ArefU64Bits3D")!,
            ["Runtime.ArefSetU64Bits2D"] = Mi(typeof(Runtime), "ArefSetU64Bits2D")!,
            ["Runtime.ArefSetU64Bits3D"] = Mi(typeof(Runtime), "ArefSetU64Bits3D")!,
            ["Runtime.BackingNDU8"] = Mi(typeof(Runtime), "BackingNDU8")!,
            ["Runtime.BackingNDU16"] = Mi(typeof(Runtime), "BackingNDU16")!,
            ["Runtime.BackingNDI32"] = Mi(typeof(Runtime), "BackingNDI32")!,
            ["Runtime.BackingNDI64"] = Mi(typeof(Runtime), "BackingNDI64")!,
            ["Runtime.BackingNDU64"] = Mi(typeof(Runtime), "BackingNDU64")!,
            ["Runtime.BackingNDF32"] = Mi(typeof(Runtime), "BackingNDF32")!,
            ["Runtime.BackingNDF64"] = Mi(typeof(Runtime), "BackingNDF64")!,
            ["Runtime.BackingIfU8"] = Mi(typeof(Runtime), "BackingIfU8")!,
            ["Runtime.BackingIfU16"] = Mi(typeof(Runtime), "BackingIfU16")!,
            ["Runtime.BackingIfI32"] = Mi(typeof(Runtime), "BackingIfI32")!,
            ["Runtime.BackingIfI64"] = Mi(typeof(Runtime), "BackingIfI64")!,
            ["Runtime.BackingIfU64"] = Mi(typeof(Runtime), "BackingIfU64")!,
            ["Runtime.BackingIfF32"] = Mi(typeof(Runtime), "BackingIfF32")!,
            ["Runtime.BackingIfF64"] = Mi(typeof(Runtime), "BackingIfF64")!,
            ["Runtime.ArrayAxisOrZero"] = Mi(typeof(Runtime), "ArrayAxisOrZero")!,
            ["Runtime.AxisIndexL"] = Mi(typeof(Runtime), "AxisIndexL")!,
            ["Runtime.U64Bits"] = Mi(typeof(Runtime), "U64Bits")!,
            ["Runtime.CheckStoreU64"] = Mi(typeof(Runtime), "CheckStoreU64")!,
            ["Runtime.ArefSetNumL"] = Mi(typeof(Runtime), "ArefSetNumL")!,
            ["Runtime.ArefNum2DL"] = Mi(typeof(Runtime), "ArefNum2DL")!,
            ["Runtime.ArefSetNum2DL"] = Mi(typeof(Runtime), "ArefSetNum2DL")!,
            ["Runtime.ArefNum3DL"] = Mi(typeof(Runtime), "ArefNum3DL")!,
            ["Runtime.ArefSetNum3DL"] = Mi(typeof(Runtime), "ArefSetNum3DL")!,
            // Raw-long character code out of a string, for (char-code (schar s i))
            ["Runtime.CharCodeAtL"] = Mi(typeof(Runtime), "CharCodeAtL")!,
            ["Runtime.CharCodeOfL"] = Mi(typeof(Runtime), "CharCodeOfL")!,
            // The character itself out of a string, for a bare (schar s i)
            ["Runtime.CharAtL"] = Mi(typeof(Runtime), "CharAtL")!,
            // Raw-double-value variants for inferred float-backed array locals
            ["Runtime.ArefNumD"] = Mi(typeof(Runtime), "ArefNumD")!,
            ["Runtime.ArefSetNumD"] = Mi(typeof(Runtime), "ArefSetNumD")!,
            ["Runtime.ArefNum2DD"] = Mi(typeof(Runtime), "ArefNum2DD")!,
            ["Runtime.ArefSetNum2DD"] = Mi(typeof(Runtime), "ArefSetNum2DD")!,
            ["Runtime.ArefNum3DD"] = Mi(typeof(Runtime), "ArefNum3DD")!,
            ["Runtime.ArefSetNum3DD"] = Mi(typeof(Runtime), "ArefSetNum3DD")!,
            ["Runtime.LispError"] = Mi(typeof(Runtime), "LispError")!,

            // Runtime - vector push
            ["Runtime.VectorPushExtend2"] = Mi(typeof(Runtime), "VectorPushExtend2")!,
            ["Runtime.VectorPushExtendVoid2"] = Mi(typeof(Runtime), "VectorPushExtendVoid2")!,
            ["Runtime.VectorPush2"] = Mi(typeof(Runtime), "VectorPush2")!,

            // Runtime - struct operations
            ["Runtime.MakeStruct"] = Mi(typeof(Runtime), "MakeStruct")!,
            ["Runtime.MakeStruct1"] = Mi(typeof(Runtime), "MakeStruct1")!,
            ["Runtime.MakeStruct2"] = Mi(typeof(Runtime), "MakeStruct2")!,
            ["Runtime.MakeStruct3"] = Mi(typeof(Runtime), "MakeStruct3")!,
            ["Runtime.MakeStruct4"] = Mi(typeof(Runtime), "MakeStruct4")!,
            ["Runtime.StructRef"] = Mi(typeof(Runtime), "StructRef")!,
            ["Runtime.StructRefI"] = Mi(typeof(Runtime), "StructRefI")!,
            ["Runtime.StructRefIT"] = Mi(typeof(Runtime), "StructRefIT")!,
            ["Runtime.StructRefITMasked"] = Mi(typeof(Runtime), "StructRefITMasked")!,
            ["Runtime.StructSetIT"] = Mi(typeof(Runtime), "StructSetIT")!,
            ["Runtime.StructSetITMasked"] = Mi(typeof(Runtime), "StructSetITMasked")!,
            ["Runtime.StructOf"] = Mi(typeof(Runtime), "StructOf")!,
            ["Runtime.StructOfMasked"] = Mi(typeof(Runtime), "StructOfMasked")!,
            // Raw-long slot read/write for fixnum-typed struct slots
            ["Runtime.StructRefL"] = Mi(typeof(Runtime), "StructRefL")!,
            ["Runtime.StructSetL"] = Mi(typeof(Runtime), "StructSetL")!,
            ["Runtime.StructRefU64"] = Mi(typeof(Runtime), "StructRefU64")!,
            ["Runtime.StructSetU64"] = Mi(typeof(Runtime), "StructSetU64")!,
            // The raw slot array fetched once per binding, and the two
            // reinterpretations a double slot needs on top of it
            ["Runtime.StructRawBacking"] = Mi(typeof(Runtime), "StructRawBacking")!,
            ["Runtime.RawBitsToDouble"] = Mi(typeof(Runtime), "RawBitsToDouble")!,
            ["Runtime.RawDoubleToBits"] = Mi(typeof(Runtime), "RawDoubleToBits")!,
            ["Runtime.RawBitsToSingle"] = Mi(typeof(Runtime), "RawBitsToSingle")!,
            ["Runtime.RawSingleToBits"] = Mi(typeof(Runtime), "RawSingleToBits")!,
            ["Runtime.StructRefF"] = Mi(typeof(Runtime), "StructRefF")!,
            ["Runtime.StructSetF"] = Mi(typeof(Runtime), "StructSetF")!,
            ["Runtime.StructSet"] = Mi(typeof(Runtime), "StructSet")!,
            ["Runtime.StructSetI"] = Mi(typeof(Runtime), "StructSetI")!,
            ["Runtime.CheckSlotType"] = Mi(typeof(Runtime), "CheckSlotType")!,
            ["Runtime.ApplyArgs"] = Mi(typeof(Runtime), "ApplyArgs")!,
            ["Runtime.ApplyArgsM"] = Mi(typeof(Runtime), "ApplyArgsM")!,
            ["Runtime.IsFixnumInRange"] = Mi(typeof(Runtime), "IsFixnumInRange")!,
            ["Runtime.IsTrueHashTable"] = Mi(typeof(Runtime), "IsTrueHashTable")!,
            ["Runtime.CmpSum"] = Mi(typeof(Runtime), "CmpSum")!,
            ["Runtime.SetCharCode"] = Mi(typeof(Runtime), "SetCharCode")!,
            ["Runtime.StructRefD"] = Mi(typeof(Runtime), "StructRefD")!,
            ["Runtime.StructRefDK"] = Mi(typeof(Runtime), "StructRefDK")!,
            ["Runtime.StructSetDK"] = Mi(typeof(Runtime), "StructSetDK")!,
            ["Runtime.StructRefFK"] = Mi(typeof(Runtime), "StructRefFK")!,
            ["Runtime.StructSetFK"] = Mi(typeof(Runtime), "StructSetFK")!,
            ["Runtime.StructRefU64K"] = Mi(typeof(Runtime), "StructRefU64K")!,
            ["Runtime.StructSetU64K"] = Mi(typeof(Runtime), "StructSetU64K")!,
            ["Runtime.StructRefLK"] = Mi(typeof(Runtime), "StructRefLK")!,
            ["Runtime.StructSetLK"] = Mi(typeof(Runtime), "StructSetLK")!,
            ["Runtime.StructRawInline"] = Mi(typeof(Runtime), "StructRawInline")!,
            ["Runtime.RawInlineAt"] = Mi(typeof(Runtime), "RawInlineAt")!,
            ["Runtime.RawInlineSet"] = Mi(typeof(Runtime), "RawInlineSet")!,
            ["Runtime.MakeStructRawInline"] = Mi(typeof(Runtime), "MakeStructRawInline")!,
            ["Runtime.StructSetD"] = Mi(typeof(Runtime), "StructSetD")!,
            ["Runtime.StructTypep"] = Mi(typeof(Runtime), "StructTypep")!,
            ["Runtime.CopyStruct"] = Mi(typeof(Runtime), "CopyStruct")!,

            // Runtime - &key helper
            ["Runtime.FindKeyArg"] = Mi(typeof(Runtime), "FindKeyArg")!,
            ["Runtime.FindKeyArgSym"] = Mi(typeof(Runtime), "FindKeyArgSym")!,
            ["Runtime.FindKeyArgByName"] = Mi(typeof(Runtime), "FindKeyArgByName")!,

            // Startup
            ["Startup.Sym"] = Mi(typeof(Startup), "Sym")!,
            ["Startup.SymInPkg"] = Mi(typeof(Startup), "SymInPkg")!,
            ["Startup.SymFn"] = Mi(typeof(Startup), "SymFn", new[] { typeof(string) })!,
            ["Startup.SymFn(string, string)"] = Mi(typeof(Startup), "SymFn", new[] { typeof(string), typeof(string) })!,
            ["Startup.Keyword"] = Mi(typeof(Startup), "Keyword")!,

            // DynamicBindings
            ["DynamicBindings.Get"] = Mi(typeof(DynamicBindings), "Get")!,
            ["DynamicBindings.GetConstant"] = Mi(typeof(DynamicBindings), "GetConstant")!,
            ["DynamicBindings.Set"] = Mi(typeof(DynamicBindings), "Set")!,
            ["DynamicBindings.SetChecked"] = Mi(typeof(DynamicBindings), "SetChecked")!,
            ["Runtime.DefineConstant"] = Mi(typeof(Runtime), "DefineConstant")!,
            ["DynamicBindings.Push"] = Mi(typeof(DynamicBindings), "Push")!,
            ["DynamicBindings.Pop"] = Mi(typeof(DynamicBindings), "Pop")!,
            ["DynamicBindings.BindDepth"] = Mi(typeof(DynamicBindings), "BindDepth")!,
            ["DynamicBindings.UnbindTo"] = Mi(typeof(DynamicBindings), "UnbindTo")!,
            ["DynamicBindings.Here"] = Mi(typeof(DynamicBindings), "Here")!,
            ["DynamicBindings.GetOn"] = Mi(typeof(DynamicBindings), "GetOn")!,
            ["DynamicBindings.SetCheckedOn"] = Mi(typeof(DynamicBindings), "SetCheckedOn")!,
            ["DynamicBindings.GetLazy"] = Mi(typeof(DynamicBindings), "GetLazy")!,
            ["DynamicBindings.SetCheckedLazy"] = Mi(typeof(DynamicBindings), "SetCheckedLazy")!,
            ["DynamicBindings.PushOn"] = Mi(typeof(DynamicBindings), "PushOn")!,
            ["DynamicBindings.PopOn"] = Mi(typeof(DynamicBindings), "PopOn")!,
            ["DynamicBindings.BindDepthOn"] = Mi(typeof(DynamicBindings), "BindDepthOn")!,
            ["DynamicBindings.BindGroupDepth"] = Mi(typeof(DynamicBindings), "BindGroupDepth")!,
            ["DynamicBindings.BindGroupDepthOn"] = Mi(typeof(DynamicBindings), "BindGroupDepthOn")!,
            ["DynamicBindings.UnbindToOn"] = Mi(typeof(DynamicBindings), "UnbindToOn")!,
            ["DynamicBindings.SetIfUnbound"] = Mi(typeof(DynamicBindings), "SetIfUnbound")!,
            ["DynamicBindings.PushGroup2"] = Mi(typeof(DynamicBindings), "PushGroup2")!,
            ["DynamicBindings.PushGroup2On"] = Mi(typeof(DynamicBindings), "PushGroup2On")!,
            ["DynamicBindings.PushGroup3"] = Mi(typeof(DynamicBindings), "PushGroup3")!,
            ["DynamicBindings.PushGroup3On"] = Mi(typeof(DynamicBindings), "PushGroup3On")!,
            ["DynamicBindings.PushGroup4"] = Mi(typeof(DynamicBindings), "PushGroup4")!,
            ["DynamicBindings.PushGroup4On"] = Mi(typeof(DynamicBindings), "PushGroup4On")!,
            ["DynamicBindings.PushGroup5"] = Mi(typeof(DynamicBindings), "PushGroup5")!,
            ["DynamicBindings.PushGroup5On"] = Mi(typeof(DynamicBindings), "PushGroup5On")!,
            ["DynamicBindings.PushGroup6"] = Mi(typeof(DynamicBindings), "PushGroup6")!,
            ["DynamicBindings.PushGroup6On"] = Mi(typeof(DynamicBindings), "PushGroup6On")!,
            ["DynamicBindings.PushGroup7"] = Mi(typeof(DynamicBindings), "PushGroup7")!,
            ["DynamicBindings.PushGroup7On"] = Mi(typeof(DynamicBindings), "PushGroup7On")!,
            ["DynamicBindings.PushGroup8"] = Mi(typeof(DynamicBindings), "PushGroup8")!,
            ["DynamicBindings.PushGroup8On"] = Mi(typeof(DynamicBindings), "PushGroup8On")!,

            // LispFunction
            ["LispFunction.Invoke"] = Mi(typeof(LispFunction), "Invoke")!,
            ["LispFunction.Invoke0"] = Mi(typeof(LispFunction), "Invoke0")!,
            ["LispFunction.Invoke1"] = Mi(typeof(LispFunction), "Invoke1")!,
            ["LispFunction.Invoke2"] = Mi(typeof(LispFunction), "Invoke2")!,
            ["LispFunction.Invoke3"] = Mi(typeof(LispFunction), "Invoke3")!,
            ["LispFunction.Invoke4"] = Mi(typeof(LispFunction), "Invoke4")!,
            ["LispFunction.Invoke5"] = Mi(typeof(LispFunction), "Invoke5")!,
            ["LispFunction.Invoke6"] = Mi(typeof(LispFunction), "Invoke6")!,
            ["LispFunction.Invoke7"] = Mi(typeof(LispFunction), "Invoke7")!,
            ["LispFunction.Invoke8"] = Mi(typeof(LispFunction), "Invoke8")!,
            ["LispFunction.Invoke0M"] = Mi(typeof(LispFunction), "Invoke0M")!,
            ["LispFunction.Invoke1M"] = Mi(typeof(LispFunction), "Invoke1M")!,
            ["LispFunction.Invoke2M"] = Mi(typeof(LispFunction), "Invoke2M")!,
            ["LispFunction.Invoke3M"] = Mi(typeof(LispFunction), "Invoke3M")!,
            ["LispFunction.Invoke4M"] = Mi(typeof(LispFunction), "Invoke4M")!,
            ["LispFunction.Invoke5M"] = Mi(typeof(LispFunction), "Invoke5M")!,
            ["LispFunction.Invoke6M"] = Mi(typeof(LispFunction), "Invoke6M")!,
            ["LispFunction.InvokeNative1"] = Mi(typeof(LispFunction), "InvokeNative1")!,
            ["LispFunction.InvokeNative2"] = Mi(typeof(LispFunction), "InvokeNative2")!,
            ["LispFunction.InvokeNative3"] = Mi(typeof(LispFunction), "InvokeNative3")!,
            ["LispFunction.InvokeNative4"] = Mi(typeof(LispFunction), "InvokeNative4")!,
            ["LispFunction.get_RawFunction"] =
                typeof(LispFunction).GetProperty("RawFunction")!.GetGetMethod()!,

            // CilAssembler statics
            ["CilAssembler.GetFunction"] = Mi(typeof(CilAssembler), "GetFunction")!,
            ["CilAssembler.GetFunctionBySymbol"] = Mi(typeof(CilAssembler), "GetFunctionBySymbol")!,
            ["CilAssembler.GetSetfFunctionBySymbol"] = Mi(typeof(CilAssembler), "GetSetfFunctionBySymbol")!,
            ["CilAssembler.GetConstant"] = Mi(typeof(CilAssembler), "GetConstant")!,
            ["CilAssembler.MakeClosure"] = Mi(typeof(CilAssembler), "MakeClosure")!,
            ["CilAssembler.RegisterFunctionOnSymbol"] = Mi(typeof(CilAssembler), "RegisterFunctionOnSymbol")!,
            ["CilAssembler.RegisterSetfFunctionOnSymbol"] = Mi(typeof(CilAssembler), "RegisterSetfFunctionOnSymbol")!,
            ["CilAssembler.RegisterFunction"] = Mi(typeof(CilAssembler), "RegisterFunction")!,

            // Runtime - symbol utilities
            ["Runtime.SetSymbolConstant"] = Mi(typeof(Runtime), "SetSymbolConstant")!,
            ["Runtime.MarkSpecial"] = Mi(typeof(Runtime), "MarkSpecial")!,
            ["Runtime.SetVariableDocumentation"] = Mi(typeof(Runtime), "SetVariableDocumentation")!,

            // Runtime - error/signal/warn
            ["Runtime.LispErrorFormat"] = Mi(typeof(Runtime), "LispErrorFormat")!,
            ["Runtime.LispSignal"] = Mi(typeof(Runtime), "LispSignal")!,
            ["Runtime.LispSignalFormat"] = Mi(typeof(Runtime), "LispSignalFormat")!,
            ["Runtime.LispWarn"] = Mi(typeof(Runtime), "LispWarn")!,
            ["Runtime.LispWarnFormat"] = Mi(typeof(Runtime), "LispWarnFormat")!,

            // HandlerClusterStack
            ["HandlerClusterStack.PushCluster"] = Mi(typeof(HandlerClusterStack), "PushCluster")!,
            ["HandlerClusterStack.PopCluster"] = Mi(typeof(HandlerClusterStack), "PopCluster")!,

            // RestartClusterStack
            ["RestartClusterStack.PushCluster"] = Mi(typeof(RestartClusterStack), "PushCluster")!,
            ["RestartClusterStack.PopCluster"] = Mi(typeof(RestartClusterStack), "PopCluster")!,
            ["RestartClusterStack.FindRestart"] = Mi(typeof(RestartClusterStack), "FindRestartByName")!,
            ["RestartClusterStack.ComputeRestarts"] = Mi(typeof(RestartClusterStack), "ComputeRestarts")!,

            // RestartInvocationException properties
            ["RestartInvocationException.get_Tag"] =
                typeof(RestartInvocationException).GetProperty("Tag")!.GetGetMethod()!,
            ["RestartInvocationException.get_Arguments"] =
                typeof(RestartInvocationException).GetProperty("Arguments")!.GetGetMethod()!,

            // LispRestart property setters
            ["LispRestart.set_ReportFunction"] =
                typeof(LispRestart).GetProperty("ReportFunction")!.GetSetMethod()!,
            ["LispRestart.set_InteractiveFunction"] =
                typeof(LispRestart).GetProperty("InteractiveFunction")!.GetSetMethod()!,
            ["LispRestart.set_NameSymbol"] =
                typeof(LispRestart).GetProperty("NameSymbol")!.GetSetMethod()!,
            ["LispRestart.set_TestFunction"] =
                typeof(LispRestart).GetProperty("TestFunction")!.GetSetMethod()!,

            // Runtime - restart
            ["Runtime.RestartArg"] = Mi(typeof(Runtime), "RestartArg")!,
            ["Runtime.RestartArgsToList"] = Mi(typeof(Runtime), "RestartArgsToList")!,
            ["Runtime.RestartArgsAsList"] = Mi(typeof(Runtime), "RestartArgsAsList")!,
            ["Runtime.RestartKeyArg"] = Mi(typeof(Runtime), "RestartKeyArg")!,
            ["Runtime.InvokeRestart"] = Mi(typeof(Runtime), "InvokeRestart")!,
            ["Runtime.FindRestart"] = Mi(typeof(Runtime), "FindRestart", new[] { typeof(LispObject) })!,
            ["Runtime.FindRestartN"] = Mi(typeof(Runtime), "FindRestartN")!,
            ["Runtime.ComputeRestarts"] = Mi(typeof(Runtime), "ComputeRestarts", Type.EmptyTypes)!,
            ["Runtime.ComputeRestartsN"] = Mi(typeof(Runtime), "ComputeRestartsN")!,

            // CLOS operations
            ["Runtime.MakeFaslInstance"] = Mi(typeof(Runtime), "MakeFaslInstance",
                new[] { typeof(string), typeof(string), typeof(LispObject[]) })!,
            ["Runtime.FindClass"] = Mi(typeof(Runtime), "FindClass")!,
            ["Runtime.FindClassOrNil"] = Mi(typeof(Runtime), "FindClassOrNil")!,
            ["Runtime.RegisterClass"] = Mi(typeof(Runtime), "RegisterClass")!,
            ["Runtime.MakeClass"] = Mi(typeof(Runtime), "MakeClass")!,
            ["Runtime.MakeClassFull"] = Mi(typeof(Runtime), "MakeClassFull")!,
            ["Runtime.MakeClassFullOptions"] = Mi(typeof(Runtime), "MakeClassFullOptions")!,
            ["Runtime.MakeSlotDef"] = Mi(typeof(Runtime), "MakeSlotDef")!,
            ["Runtime.MakeSlotDefWithAllocation"] = Mi(typeof(Runtime), "MakeSlotDefWithAllocation")!,
            ["Runtime.SetSlotDefRawOptions"] = Mi(typeof(Runtime), "SetSlotDefRawOptions")!,
            ["Runtime.SetSlotDefDocumentation"] = Mi(typeof(Runtime), "SetSlotDefDocumentation")!,
            ["Runtime.SetSlotDefAttrs"] = Mi(typeof(Runtime), "SetSlotDefAttrs")!,
            ["Runtime.SetClassDefaultInitargs"] = Mi(typeof(Runtime), "SetClassDefaultInitargs")!,
            ["Runtime.ClassOf"] = Mi(typeof(Runtime), "ClassOf")!,
            ["Runtime.ClassName"] = Mi(typeof(Runtime), "ClassName")!,
            ["Runtime.MakeInstanceRaw"] = Mi(typeof(Runtime), "MakeInstanceRaw")!,
            ["Runtime.SlotValue"] = Mi(typeof(Runtime), "SlotValue")!,
            ["Runtime.SetSlotValue"] = Mi(typeof(Runtime), "SetSlotValue")!,
            ["Runtime.ReaderIC"] = Mi(typeof(Runtime), "ReaderIC")!,
            ["Runtime.SlotValueIC"] = Mi(typeof(Runtime), "SlotValueIC")!,
            ["Runtime.WriterIC"] = Mi(typeof(Runtime), "WriterIC")!,
            ["Runtime.Boundp"] = Mi(typeof(Runtime), "Boundp")!,
            ["Runtime.IntegerEvenp"] = Mi(typeof(Runtime), "IntegerEvenp")!,
            ["Runtime.ParseInteger1"] = Mi(typeof(Runtime), "ParseInteger1")!,
            ["Runtime.Map1Fast"] = Mi(typeof(Runtime), "Map1Fast")!,
            ["Runtime.FboundpDirect"] = Mi(typeof(Runtime), "FboundpDirect")!,
            ["Runtime.SymbolPlistDirect"] = Mi(typeof(Runtime), "SymbolPlistDirect")!,
            ["Runtime.SymbolFunction"] = Mi(typeof(Runtime), "SymbolFunction")!,
            ["Runtime.SymbolValue"] = Mi(typeof(Runtime), "SymbolValue")!,
            ["Runtime.Fdefinition"] = Mi(typeof(Runtime), "Fdefinition")!,
            ["Runtime.Getenv"] = Mi(typeof(Runtime), "Getenv")!,
            ["Runtime.SlotBoundp"] = Mi(typeof(Runtime), "SlotBoundp")!,
            ["Runtime.SlotExists"] = Mi(typeof(Runtime), "SlotExists")!,
            ["Runtime.MakeInstanceWithInitargs"] = Mi(typeof(Runtime), "MakeInstanceWithInitargs")!,
            ["Runtime.MakeInstanceKV1"] = Mi(typeof(Runtime), "MakeInstanceKV1")!,
            ["Runtime.MakeInstanceKV2"] = Mi(typeof(Runtime), "MakeInstanceKV2")!,
            ["Runtime.MakeInstanceKV3"] = Mi(typeof(Runtime), "MakeInstanceKV3")!,
            ["Runtime.MakeInstanceDyn"] = Mi(typeof(Runtime), "MakeInstanceDyn")!,
            ["Runtime.ChangeClass"] = Mi(typeof(Runtime), "ChangeClass")!,

            // CLOS generic function operations
            ["Runtime.MakeGF"] = Mi(typeof(Runtime), "MakeGF")!,
            ["Runtime.RegisterGF"] = Mi(typeof(Runtime), "RegisterGF")!,
            ["Runtime.ClearDefgenericInlineMethods"] = Mi(typeof(Runtime), "ClearDefgenericInlineMethods")!,
            ["Runtime.MarkDefgenericInlineMethod"] = Mi(typeof(Runtime), "MarkDefgenericInlineMethod")!,
            ["Runtime.SetMethodCombination"] = Mi(typeof(Runtime), "SetMethodCombination")!,
            ["Runtime.SetMethodCombinationOrder"] = Mi(typeof(Runtime), "SetMethodCombinationOrder")!,
            ["Runtime.SetMethodCombinationArgs"] = Mi(typeof(Runtime), "SetMethodCombinationArgs")!,
            ["Runtime.SetGFLambdaListInfo"] = Mi(typeof(Runtime), "SetGFLambdaListInfo")!,
            ["Runtime.SetMethodLambdaListInfo"] = Mi(typeof(Runtime), "SetMethodLambdaListInfo")!,
            ["Runtime.FindGF"] = Mi(typeof(Runtime), "FindGF")!,
            ["Runtime.MakeMethod"] = Mi(typeof(Runtime), "MakeMethod")!,
            ["Runtime.AddMethod"] = Mi(typeof(Runtime), "AddMethod")!,
            ["Runtime.AddMethodProtocol"] = Mi(typeof(Runtime), "AddMethodProtocol")!,
            ["Runtime.RemoveMethod"] = Mi(typeof(Runtime), "RemoveMethod")!,
            ["Runtime.ComputeApplicableMethods"] = Mi(typeof(Runtime), "ComputeApplicableMethods")!,
            ["Runtime.GetGFMethods"] = Mi(typeof(Runtime), "GetGFMethods")!,
            ["Runtime.MethodSpecializers"] = Mi(typeof(Runtime), "MethodSpecializers")!,
            ["Runtime.MethodQualifiers"] = Mi(typeof(Runtime), "MethodQualifiers")!,
            ["Runtime.MethodFunction"] = Mi(typeof(Runtime), "MethodFunction")!,
            ["Runtime.CallNextMethod"] = Mi(typeof(Runtime), "CallNextMethod")!,
            ["Runtime.CallNextMethodLoose"] = Mi(typeof(Runtime), "CallNextMethodLoose")!,
            ["Runtime.NextMethodP"] = Mi(typeof(Runtime), "NextMethodP")!,
            ["Runtime.CapturedCnm"] = Mi(typeof(Runtime), "CapturedCnm")!,
            ["Runtime.CapturedNmp"] = Mi(typeof(Runtime), "CapturedNmp")!,

            // Runtime - package operations
            ["Runtime.MakePackage"] = Mi(typeof(Runtime), "MakePackage")!,
            ["Runtime.PackageUse"] = Mi(typeof(Runtime), "PackageUse")!,
            ["Runtime.PackageExport"] = Mi(typeof(Runtime), "PackageExport")!,
            ["Runtime.PackageExternalSymbolsList"] = Mi(typeof(Runtime), "PackageExternalSymbolsList")!,
            ["Runtime.PackageAllSymbolsList"] = Mi(typeof(Runtime), "PackageAllSymbolsList")!,
            ["Runtime.HashTablePairs"] = Mi(typeof(Runtime), "HashTablePairs")!,
            ["Runtime.HashTableFlat"] = Mi(typeof(Runtime), "HashTableFlat")!,
            ["Runtime.CoerceToFunction"] = Mi(typeof(Runtime), "CoerceToFunction")!,
            ["Runtime.PackageImport"] = Mi(typeof(Runtime), "PackageImport")!,
            ["Runtime.PackageShadow"] = Mi(typeof(Runtime), "PackageShadow")!,
            ["Runtime.PackageNickname"] = Mi(typeof(Runtime), "PackageNickname")!,
            ["Runtime.FindPackage"] = Mi(typeof(Runtime), "FindPackage")!,
            ["Runtime.PackageName"] = Mi(typeof(Runtime), "PackageName")!,
            ["Runtime.PackageErrorPackage"] = Mi(typeof(Runtime), "PackageErrorPackage")!,
            ["Runtime.InternSymbol"] = Mi(typeof(Runtime), "InternSymbol")!,
            ["Runtime.InternSymbolV"] = Mi(typeof(Runtime), "InternSymbolV")!,
            ["Runtime.InternMv"] = Mi(typeof(Runtime), "InternMv")!,
            ["Runtime.InternPrimary"] = Mi(typeof(Runtime), "InternPrimary")!,
            ["Runtime.MakePackageK"] = Mi(typeof(Runtime), "MakePackageK")!,
            ["Runtime.DeletePackage"] = Mi(typeof(Runtime), "DeletePackage")!,
            ["Runtime.RenamePackage"] = Mi(typeof(Runtime), "RenamePackage")!,
            ["Runtime.FindSymbolL"] = Mi(typeof(Runtime), "FindSymbolL")!,
            ["Runtime.FindSymbolMv"] = Mi(typeof(Runtime), "FindSymbolMv")!,
            ["Runtime.FindSymbolPrimary"] = Mi(typeof(Runtime), "FindSymbolPrimary")!,
            ["Runtime.UninternSymbol"] = Mi(typeof(Runtime), "UninternSymbol")!,
            ["Runtime.UnexportSymbol"] = Mi(typeof(Runtime), "UnexportSymbol")!,
            ["Runtime.UnusePackage"] = Mi(typeof(Runtime), "UnusePackage")!,
            ["Runtime.ShadowingImport"] = Mi(typeof(Runtime), "ShadowingImport")!,
            ["Runtime.PackageUsedByList"] = Mi(typeof(Runtime), "PackageUsedByList")!,
            ["Runtime.PackageUseListL"] = Mi(typeof(Runtime), "PackageUseListL")!,
            ["Runtime.PackageShadowingSymbols"] = Mi(typeof(Runtime), "PackageShadowingSymbols")!,
            ["Runtime.PackageNicknamesList"] = Mi(typeof(Runtime), "PackageNicknamesList")!,
            ["Runtime.ListAllPackages"] = Mi(typeof(Runtime), "ListAllPackages", Type.EmptyTypes)!,
            ["Runtime.ListAllPackagesV"] = Mi(typeof(Runtime), "ListAllPackagesV")!,
            ["Runtime.Random"] = Mi(typeof(Runtime), "Random", new[] { typeof(LispObject) })!,
            ["Runtime.Random2"] = Mi(typeof(Runtime), "Random2")!,
            ["Runtime.RandomDoubleRaw"] = Mi(typeof(Runtime), "RandomDoubleRaw")!,
            ["Runtime.RandomDoubleRaw2"] = Mi(typeof(Runtime), "RandomDoubleRaw2")!,

            // Runtime - format
            ["Runtime.Format"] = Mi(typeof(Runtime), "Format")!,

            // Runtime - file I/O
            ["Runtime.OpenFile"] = Mi(typeof(Runtime), "OpenFile")!,
            ["Runtime.CloseStream"] = Mi(typeof(Runtime), "CloseStream")!,
            ["Runtime.OpenStreamP"] = Mi(typeof(Runtime), "OpenStreamP")!,
            ["Runtime.ReadCharNoHang"] = Mi(typeof(Runtime), "ReadCharNoHang")!,
            ["Runtime.Listen"] = Mi(typeof(Runtime), "Listen")!,
            ["Runtime.ClearInput"] = Mi(typeof(Runtime), "ClearInput")!,
            ["Runtime.WriteByte"] = Mi(typeof(Runtime), "WriteByte")!,
            ["Runtime.ReadPreservingWhitespace"] = Mi(typeof(Runtime), "ReadPreservingWhitespace")!,
            ["Runtime.ReadLine"] = Mi(typeof(Runtime), "ReadLine", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.ReadLineMv"] = Mi(typeof(Runtime), "ReadLineMv")!,
            ["Runtime.ReadLinePrimary"] = Mi(typeof(Runtime), "ReadLinePrimary")!,
            ["Runtime.ReadChar"] = Mi(typeof(Runtime), "ReadChar", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.PeekChar"] = Mi(typeof(Runtime), "PeekChar", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.UnreadChar"] = Mi(typeof(Runtime), "UnreadChar")!,
            ["Runtime.WriteChar"] = Mi(typeof(Runtime), "WriteChar")!,
            ["Runtime.WriteString"] = Mi(typeof(Runtime), "WriteString", new[] { typeof(LispObject[]) })!,
            ["Runtime.WriteLine"] = Mi(typeof(Runtime), "WriteLine", new[] { typeof(LispObject[]) })!,
            ["Runtime.FixnumGeObject"] = Mi(typeof(Runtime), "FixnumGeObject")!,
            ["Runtime.DotimesLimit"] = Mi(typeof(Runtime), "DotimesLimit")!,
            ["Runtime.LoopLimitFloor"] = Mi(typeof(Runtime), "LoopLimitFloor")!,
            ["Runtime.LoopBy"] = Mi(typeof(Runtime), "LoopBy")!,
            ["Runtime.LoopByFixnum"] = Mi(typeof(Runtime), "LoopByFixnum")!,
            ["MultipleValues.Values0"] = Mi(typeof(MultipleValues), "Values0")!,
            ["MultipleValues.CaptureForBind"] = Mi(typeof(MultipleValues), "CaptureForBind")!,
            ["MultipleValues.BindNth"] = Mi(typeof(MultipleValues), "BindNth")!,
            ["MultipleValues.BindNthI"] = Mi(typeof(MultipleValues), "BindNthI")!,
            ["Runtime.ModFixnumL"] = Mi(typeof(Runtime), "ModFixnumL")!,
            ["Runtime.AbsFixnumL"] = Mi(typeof(Runtime), "AbsFixnumL")!,
            ["Runtime.IsqrtFixnumL"] = Mi(typeof(Runtime), "IsqrtFixnumL")!,
            ["Runtime.AshLong"] = Mi(typeof(Runtime), "AshLong")!,
            ["Runtime.FloorFixnumL"] = Mi(typeof(Runtime), "FloorFixnumL")!,
            ["Runtime.TruncateFixnumL"] = Mi(typeof(Runtime), "TruncateFixnumL")!,
            ["Runtime.FloorFixnumBoxed"] = Mi(typeof(Runtime), "FloorFixnumBoxed")!,
            ["Runtime.TruncateFixnumBoxed"] = Mi(typeof(Runtime), "TruncateFixnumBoxed")!,
            ["Runtime.RemFixnumL"] = Mi(typeof(Runtime), "RemFixnumL")!,
            ["Runtime.WriteString1"] = Mi(typeof(Runtime), "WriteString1")!,
            ["Runtime.WriteString2"] = Mi(typeof(Runtime), "WriteString2")!,
            ["Runtime.ReadByteDirect"] = Mi(typeof(Runtime), "ReadByteDirect")!,
            ["Runtime.FinishOutput0"] = Mi(typeof(Runtime), "FinishOutput0")!,
            ["Runtime.FinishOutput1"] = Mi(typeof(Runtime), "FinishOutput1")!,
            ["Runtime.ForceOutput0"] = Mi(typeof(Runtime), "ForceOutput0")!,
            ["Runtime.ForceOutput1"] = Mi(typeof(Runtime), "ForceOutput1")!,
            ["Runtime.Terpri0"] = Mi(typeof(Runtime), "Terpri0")!,
            ["Runtime.Terpri1"] = Mi(typeof(Runtime), "Terpri1")!,
            ["Runtime.FreshLine0"] = Mi(typeof(Runtime), "FreshLine0")!,
            ["Runtime.FreshLine1"] = Mi(typeof(Runtime), "FreshLine1")!,
            ["Runtime.WriteLine1"] = Mi(typeof(Runtime), "WriteLine1")!,
            ["Runtime.WriteLine2"] = Mi(typeof(Runtime), "WriteLine2")!,
            ["Runtime.Directory"] = Mi(typeof(Runtime), "DirectoryFunc")!,
            ["Runtime.ProbeFile"] = Mi(typeof(Runtime), "ProbeFile")!,
            ["Runtime.Truename"] = Mi(typeof(Runtime), "Truename")!,

            ["Runtime.DeleteFile"] = Mi(typeof(Runtime), "DeleteFile")!,
            ["Runtime.FileAuthor"] = Mi(typeof(Runtime), "FileAuthor")!,
            ["Runtime.FileErrorPathname"] = Mi(typeof(Runtime), "FileErrorPathname")!,
            ["Runtime.RenameFile"] = Mi(typeof(Runtime), "RenameFile")!,
            ["Runtime.FileWriteDate"] = Mi(typeof(Runtime), "FileWriteDate")!,

            // Runtime - string streams
            ["Runtime.MakeStringOutputStream"] = Mi(typeof(Runtime), "MakeStringOutputStream")!,
            ["Runtime.MakeStringOutputStreamToString"] = Mi(typeof(Runtime), "MakeStringOutputStreamToString")!,
            ["Runtime.GetOutputStreamString"] = Mi(typeof(Runtime), "GetOutputStreamString")!,
            ["Runtime.MakeStringInputStream"] = Mi(typeof(Runtime), "MakeStringInputStream", new[] { typeof(LispObject[]) })!,

            // Runtime - pathname
            ["Runtime.MakePathname"] = Mi(typeof(Runtime), "MakePathname")!,
            ["Runtime.MakePathnameFromParts"] = Mi(typeof(Runtime), "MakePathnameFromParts")!,
            ["Runtime.Pathname"] = Mi(typeof(Runtime), "Pathname")!,
            ["Runtime.Namestring"] = Mi(typeof(Runtime), "Namestring")!,
            ["Runtime.PathnameDirectory"] = Mi(typeof(Runtime), "PathnameDirectory")!,
            ["Runtime.PathnameName"] = Mi(typeof(Runtime), "PathnameName")!,
            ["Runtime.PathnameType"] = Mi(typeof(Runtime), "PathnameType")!,
            ["Runtime.PathnameHost"] = Mi(typeof(Runtime), "PathnameHost")!,
            ["Runtime.PathnameDevice"] = Mi(typeof(Runtime), "PathnameDevice")!,
            ["Runtime.PathnameVersion"] = Mi(typeof(Runtime), "PathnameVersion")!,
            ["Runtime.MergePathnames"] = Mi(typeof(Runtime), "MergePathnames", new[] { typeof(LispObject), typeof(LispObject) })!,

            // Runtime - read
            ["Runtime.ReadFromStream"] = Mi(typeof(Runtime), "ReadFromStream")!,
            ["Runtime.ReadFromString"] = Mi(typeof(Runtime), "ReadFromString")!,

            // Runtime - load / eval
            ["Runtime.Load"] = Mi(typeof(Runtime), "Load", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.Eval"] = Mi(typeof(Runtime), "Eval")!,
            ["Runtime.TryEval"] = Mi(typeof(Runtime), "TryEval")!,
            ["Runtime.Gensym"] = Mi(typeof(Runtime), "Gensym")!,
            ["Runtime.Gensym0"] = Mi(typeof(Runtime), "Gensym0")!,

            // LispErrorException property
            ["LispErrorException.get_Condition"] =
                typeof(LispErrorException).GetProperty("Condition")!.GetGetMethod()!,

            // HandlerCaseInvocationException properties
            ["HandlerCaseInvocationException.get_Tag"] =
                typeof(HandlerCaseInvocationException).GetProperty("Tag")!.GetGetMethod()!,
            ["HandlerCaseInvocationException.get_ClauseIndex"] =
                typeof(HandlerCaseInvocationException).GetProperty("ClauseIndex")!.GetGetMethod()!,
            ["HandlerCaseInvocationException.get_Condition"] =
                typeof(HandlerCaseInvocationException).GetProperty("Condition")!.GetGetMethod()!,

            // Startup.MakeHandlerCaseFunction
            ["Startup.MakeHandlerCaseFunction"] =
                Mi(typeof(Startup), "MakeHandlerCaseFunction")!,

            // System.Exception property
            ["System.Exception.get_Message"] =
                typeof(System.Exception).GetProperty("Message")!.GetGetMethod()!,

            // Wrap .NET exceptions as Lisp conditions
            ["Runtime.WrapDotNetException"] =
                Mi(typeof(Runtime), "WrapDotNetException")!,
            ["Runtime.WrapDotNetExceptionObj"] =
                Mi(typeof(Runtime), "WrapDotNetExceptionObj")!,
            ["Runtime.RewrapNonLispException"] =
                Mi(typeof(Runtime), "RewrapNonLispException")!,
            ["Runtime.HandlerBindFilter"] =
                Mi(typeof(Runtime), "HandlerBindFilter")!,
            ["Runtime.HandlerBindRethrow"] =
                Mi(typeof(Runtime), "HandlerBindRethrow")!,
            ["Runtime.IsLispControlFlowException"] =
                Mi(typeof(Runtime), "IsLispControlFlowException")!,
            ["Runtime.ThrowControlError"] =
                Mi(typeof(Runtime), "ThrowControlError")!,
            // Builds the exception a THROW raises, or signals CONTROL-ERROR when
            // the tag has no outstanding CATCH.
            ["Runtime.MakeCatchThrow"] =
                Mi(typeof(Runtime), "MakeCatchThrow")!,

            // Fixnum.Value getter: used by compile-as-long for fast-path
            // fixnum arithmetic on (the fixnum ...) expressions.
            ["Fixnum.get_Value"] =
                typeof(Fixnum).GetProperty("Value")!.GetGetMethod()!,
            ["DoubleFloat.get_Value"] =
                typeof(DoubleFloat).GetProperty("Value")!.GetGetMethod()!,
            ["SingleFloat.get_Value"] =
                typeof(SingleFloat).GetProperty("Value")!.GetGetMethod()!,
            // Native System.Decimal arithmetic for (declare (type decimal x))
            // scopes: scale-preserving decimal ops, unlike the undeclared rational path.
            ["LispDecimal.get_Value"] =
                typeof(LispDecimal).GetProperty("Value")!.GetGetMethod()!,
            ["LispStruct.get_TypeName"] =
                typeof(LispStruct).GetProperty("TypeName")!.GetGetMethod()!,
            ["LispStruct.TypeNameOf"] = Mi(typeof(LispStruct), "TypeNameOf")!,
            ["LispStruct.TypeNameOfStruct"] = Mi(typeof(LispStruct), "TypeNameOfStruct")!,
            ["LispStruct.TypeNameOfKnown"] = Mi(typeof(LispStruct), "TypeNameOfKnown")!,
            ["LispStruct.TypeNameOfRawInline"] = Mi(typeof(LispStruct), "TypeNameOfRawInline")!,
            ["Symbol.get_StructParent"] =
                typeof(Symbol).GetProperty("StructParent")!.GetGetMethod()!,
            ["Decimal.op_Addition"] =
                Mi(typeof(decimal), "op_Addition", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_Subtraction"] =
                Mi(typeof(decimal), "op_Subtraction", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_Multiply"] =
                Mi(typeof(decimal), "op_Multiply", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_Division"] =
                Mi(typeof(decimal), "op_Division", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_UnaryNegation"] =
                Mi(typeof(decimal), "op_UnaryNegation", new[] { typeof(decimal) })!,

            // BlockReturnException properties
            ["BlockReturnException.get_Tag"] =
                typeof(BlockReturnException).GetProperty("Tag")!.GetGetMethod()!,
            ["BlockReturnException.get_Value"] =
                typeof(BlockReturnException).GetProperty("Value")!.GetGetMethod()!,

            // CatchThrowException properties and factory
            ["CatchThrowException.get_Tag"] =
                typeof(CatchThrowException).GetProperty("Tag")!.GetGetMethod()!,
            ["CatchThrowException.get_Value"] =
                typeof(CatchThrowException).GetProperty("Value")!.GetGetMethod()!,
            ["CatchThrowException.Get"] =
                Mi(typeof(CatchThrowException), "Get")!,

            // Exception-filter predicates
            ["ControlFlowFilters.CatchTagMatches"] =
                Mi(typeof(ControlFlowFilters), "CatchTagMatches")!,
            // Two shapes, two names. A .sil compiled by an older dotcl names
            // HandlerCaseClause and pushes (tag, specs); rebinding that name to the
            // cluster form makes such a .sil assemble into an invalid program.
            ["ControlFlowFilters.HandlerCaseClause"] =
                Mi(typeof(ControlFlowFilters), "HandlerCaseClause",
                    new[] { typeof(object), typeof(object), typeof(LispObject[]) })!,
            ["ControlFlowFilters.HandlerCaseClauseCluster"] =
                Mi(typeof(ControlFlowFilters), "HandlerCaseClause",
                    new[] { typeof(object), typeof(HandlerBinding[]) })!,
            ["ControlFlowFilters.HandlerCaseCondition"] =
                Mi(typeof(ControlFlowFilters), "HandlerCaseCondition")!,
            ["ControlFlowFilters.Transfer"] =
                Mi(typeof(ControlFlowFilters), "Transfer")!,
            ["ControlFlowFilters.TakeTransfer"] =
                Mi(typeof(ControlFlowFilters), "TakeTransfer")!,
            ["ControlFlowFilters.RestartCaseTag"] =
                Mi(typeof(ControlFlowFilters), "RestartCaseTag")!,
            ["ControlFlowFilters.BlockTagMatches"] =
                Mi(typeof(ControlFlowFilters), "BlockTagMatches")!,
            ["ControlFlowFilters.GoTagbodyMatches"] =
                Mi(typeof(ControlFlowFilters), "GoTagbodyMatches")!,
            ["ControlFlowFilters.MarkExited"] =
                Mi(typeof(ControlFlowFilters), "MarkExited")!,
            ["ControlFlowFilters.IsExitToken"] =
                Mi(typeof(ControlFlowFilters), "IsExitToken")!,
            ["ControlFlowFilters.MakeExitToken"] =
                Mi(typeof(ControlFlowFilters), "MakeExitToken")!,
            ["ControlFlowFilters.TokenTagIs"] =
                Mi(typeof(ControlFlowFilters), "TokenTagIs")!,
            ["ControlFlowFilters.TokenValue"] =
                Mi(typeof(ControlFlowFilters), "TokenValue")!,
            ["ControlFlowFilters.TokenException"] =
                Mi(typeof(ControlFlowFilters), "TokenException")!,
            ["ControlFlowFilters.CheckBlockLive"] =
                Mi(typeof(ControlFlowFilters), "CheckBlockLive")!,
            ["ControlFlowFilters.CheckTagbodyLive"] =
                Mi(typeof(ControlFlowFilters), "CheckTagbodyLive")!,

            // CatchTagStack methods
            ["CatchTagStack.Push"] =
                Mi(typeof(CatchTagStack), "Push")!,
            ["CatchTagStack.Pop"] =
                Mi(typeof(CatchTagStack), "Pop")!,
            ["CatchTagStack.HasMatchingCatch"] =
                Mi(typeof(CatchTagStack), "HasMatchingCatch")!,
            ["CatchTagStack.PushIndex"] =
                Mi(typeof(CatchTagStack), "PushIndex")!,
            ["CatchTagStack.Enter"] = Mi(typeof(CatchTagStack), "Enter")!,
            ["CatchTagStack.Leave"] = Mi(typeof(CatchTagStack), "Leave")!,
            ["ThrowReturn.Claim"] = Mi(typeof(ThrowReturn), "Claim")!,
            ["ThrowReturn.CanReturn"] = Mi(typeof(ThrowReturn), "CanReturn")!,
            ["ThrowReturn.Return"] = Mi(typeof(ThrowReturn), "Return")!,
            ["ThrowReturn.ClaimS"] = Mi(typeof(ThrowReturn), "ClaimS")!,
            ["ThrowReturn.ClaimedIndex"] = Mi(typeof(ThrowReturn), "ClaimedIndex")!,
            ["ThrowReturn.CanReturnS"] = Mi(typeof(ThrowReturn), "CanReturnS")!,
            ["ThrowReturn.ReturnS"] = Mi(typeof(ThrowReturn), "ReturnS")!,
            ["ThrowReturn.IsReturned"] = Mi(typeof(ThrowReturn), "IsReturned")!,
            ["ThrowReturn.TakePending"] = Mi(typeof(ThrowReturn), "TakePending")!,
            ["ThrowReturn.Forward0M"] = Mi(typeof(ThrowReturn), "Forward0M")!,
            ["ThrowReturn.Forward1M"] = Mi(typeof(ThrowReturn), "Forward1M")!,
            ["ThrowReturn.Forward2M"] = Mi(typeof(ThrowReturn), "Forward2M")!,
            ["ThrowReturn.Forward3M"] = Mi(typeof(ThrowReturn), "Forward3M")!,
            ["ThrowReturn.Forward4M"] = Mi(typeof(ThrowReturn), "Forward4M")!,
            ["ThrowReturn.Forward0"] = Mi(typeof(ThrowReturn), "Forward0")!,
            ["ThrowReturn.Forward1"] = Mi(typeof(ThrowReturn), "Forward1")!,
            ["ThrowReturn.Forward2"] = Mi(typeof(ThrowReturn), "Forward2")!,
            ["ThrowReturn.Forward3"] = Mi(typeof(ThrowReturn), "Forward3")!,
            ["ThrowReturn.Forward4"] = Mi(typeof(ThrowReturn), "Forward4")!,
            ["ThrowReturn.Call0"] = Mi(typeof(ThrowReturn), "Call0")!,
            ["ThrowReturn.Call1"] = Mi(typeof(ThrowReturn), "Call1")!,
            ["ThrowReturn.Call2"] = Mi(typeof(ThrowReturn), "Call2")!,
            ["ThrowReturn.Call3"] = Mi(typeof(ThrowReturn), "Call3")!,
            ["ThrowReturn.Call4"] = Mi(typeof(ThrowReturn), "Call4")!,
            ["ThrowReturn.Call0S"] = Mi(typeof(ThrowReturn), "Call0S")!,
            ["ThrowReturn.Call1S"] = Mi(typeof(ThrowReturn), "Call1S")!,
            ["ThrowReturn.Call2S"] = Mi(typeof(ThrowReturn), "Call2S")!,
            ["ThrowReturn.Call3S"] = Mi(typeof(ThrowReturn), "Call3S")!,
            ["ThrowReturn.Call4S"] = Mi(typeof(ThrowReturn), "Call4S")!,

            // GoException properties
            ["GoException.get_TagbodyId"] =
                typeof(GoException).GetProperty("TagbodyId")!.GetGetMethod()!,
            ["GoException.get_TargetLabel"] =
                typeof(GoException).GetProperty("TargetLabel")!.GetGetMethod()!,
        };

        _fieldCache = new Dictionary<string, FieldInfo>
        {
            ["Nil.Instance"] = typeof(Nil).GetField("Instance")!,
            ["T.Instance"] = typeof(T).GetField("Instance")!,
            ["AbsentKey.Instance"] = typeof(AbsentKey).GetField("Instance")!,
            // Debug-only boxed-variable cell (see LispBox); read/written via
            // ldfld/stfld in the debug codegen path.
            ["LispBox.Value"] = typeof(LispBox).GetField("Value")!,
        };

        _ctorCache = new Dictionary<string, ConstructorInfo>
        {
            ["LispString"] = typeof(LispString).GetConstructor(new[] { typeof(string) })!,
            ["DoubleFloat"] = typeof(DoubleFloat).GetConstructor(new[] { typeof(double) })!,
            ["SingleFloat"] = typeof(SingleFloat).GetConstructor(new[] { typeof(float) })!,
            ["LispDecimal"] = typeof(LispDecimal).GetConstructor(new[] { typeof(decimal) })!,
            ["LispProgramError"] = typeof(LispProgramError).GetConstructor(new[] { typeof(string) })!,
            ["LispErrorException"] = typeof(LispErrorException)
                .GetConstructor(new[] { typeof(LispCondition) })!,
            ["LispVector"] = typeof(LispVector).GetConstructor(new[] { typeof(LispObject[]) })!,
            ["LispBox"] = typeof(LispBox).GetConstructor(new[] { typeof(LispObject) })!,
            ["Object"] = typeof(object).GetConstructor(Type.EmptyTypes)!,
            ["BlockReturnException"] = typeof(BlockReturnException)
                .GetConstructor(new[] { typeof(object), typeof(LispObject) })!,
            ["CatchThrowException"] = typeof(CatchThrowException)
                .GetConstructor(new[] { typeof(LispObject), typeof(LispObject) })!,
            ["GoException"] = typeof(GoException)
                .GetConstructor(new[] { typeof(object), typeof(int) })!,
            ["HandlerBinding"] = typeof(HandlerBinding)
                .GetConstructor(new[] { typeof(LispObject), typeof(LispFunction) })!,
            // handler-case clause: no handler function object per clause per entry.
            // The older name takes (type-spec, tag, clause-index) and stays bound to
            // that shape for .sil compiled before the cluster carried the identity.
            ["HandlerBindingHc"] = typeof(HandlerBinding)
                .GetConstructor(new[] { typeof(LispObject), typeof(object), typeof(int) })!,
            ["HandlerBindingHcCluster"] = typeof(HandlerBinding)
                .GetConstructor(new[] { typeof(LispObject), typeof(int) })!,
            ["LispRestart"] = typeof(LispRestart)
                .GetConstructor(new[] { typeof(string), typeof(Func<LispObject[], LispObject>),
                                        typeof(string), typeof(object), typeof(bool) })!,
        };

        _typeCache = new Dictionary<string, Type>
        {
            ["LispObject"] = typeof(LispObject),
            ["LispObject[]"] = typeof(LispObject[]),
            ["LispBox"] = typeof(LispBox),
            ["LispFunction"] = typeof(LispFunction),
            ["LispString"] = typeof(LispString),
            ["LispStruct"] = typeof(LispStruct),
            ["Symbol"] = typeof(Symbol),
            ["Object"] = typeof(object),
            ["Object[]"] = typeof(object[]),
            ["Int32"] = typeof(int),
            ["Int64"] = typeof(long),
            // Concrete element buffers for the hoisted simple-array path: a
            // local of this type holds the vector's own storage, so an AREF in
            // the body is a bare ldelem instead of a helper call that re-derives
            // the same buffer on every element.
            ["Int64[]"] = typeof(long[]),
            ["Int32[]"] = typeof(int[]),
            ["UInt16[]"] = typeof(ushort[]),
            ["Char[]"] = typeof(char[]),
            ["Byte[]"] = typeof(byte[]),
            ["Double[]"] = typeof(double[]),
            ["Single[]"] = typeof(float[]),
            ["BindingStack"] = typeof(DynamicBindings.BindingStack),
            ["Byte"] = typeof(byte),
            ["UInt16"] = typeof(ushort),
            ["Double"] = typeof(double),
            ["Single"] = typeof(float),
            ["Byte&"] = typeof(byte).MakeByRefType(),
            ["UInt16&"] = typeof(ushort).MakeByRefType(),
            ["Int32&"] = typeof(int).MakeByRefType(),
            ["Int64&"] = typeof(long).MakeByRefType(),
            ["Double&"] = typeof(double).MakeByRefType(),
            ["Single&"] = typeof(float).MakeByRefType(),
            // A local slot holding a raw System.Decimal, for the declared-decimal
            // native path: the boxed LispDecimal only reappears when the value
            // crosses back into generic code.
            ["Decimal"] = typeof(decimal),
            ["Boolean"] = typeof(bool),
            ["String"] = typeof(string),
            ["BlockReturnException"] = typeof(BlockReturnException),
            ["CatchThrowException"] = typeof(CatchThrowException),
            ["GoException"] = typeof(GoException),
            ["LispErrorException"] = typeof(LispErrorException),
            ["HandlerCaseInvocationException"] = typeof(HandlerCaseInvocationException),
            ["LispCondition"] = typeof(LispCondition),
            ["HandlerBinding"] = typeof(HandlerBinding),
            ["HandlerBinding[]"] = typeof(HandlerBinding[]),
            ["MvHeld"] = typeof(MvHeld),
            ["RestartInvocationException"] = typeof(RestartInvocationException),
            ["LispRestart"] = typeof(LispRestart),
            ["LispRestart[]"] = typeof(LispRestart[]),
            ["Func<LispObject[],LispObject>"] = typeof(Func<LispObject[], LispObject>),
            ["Cons"] = typeof(Cons),
            ["LispInstance"] = typeof(LispInstance),
            ["LispClass"] = typeof(LispClass),
            ["LispPathname"] = typeof(LispPathname),
            ["LispFileStream"] = typeof(LispFileStream),
            ["LispStringOutputStream"] = typeof(LispStringOutputStream),
            ["System.Exception"] = typeof(System.Exception),
        };

        _getConstant = Mi(typeof(CilAssembler), "GetConstant")!;
        _getConstantObject = Mi(typeof(CilAssembler), "GetConstantObject")!;
        _getConstantSymbol = Mi(typeof(CilAssembler), "GetConstantSymbol")!;
        _symFnSiteResolve = Mi(typeof(Startup.SymFnSite), "Resolve")!;
        _fnSiteSymbol = Mi(typeof(CilAssembler), "FnSiteSymbol")!;
        _symSiteResolve = Mi(typeof(Startup.SymSite), "Resolve")!;
        _getUnitConstant = Mi(typeof(CilAssembler), "GetUnitConstant")!;
        _makeFaslInstance = Mi(typeof(Runtime), "MakeFaslInstance",
            new[] { typeof(string), typeof(string), typeof(LispObject[]) })!;
        _internViaEvalInstance = Mi(typeof(LispInstance), "InternViaEval",
            new[] { typeof(string), typeof(LispObject) })!;
        _makeClosure = Mi(typeof(CilAssembler), "MakeClosure")!;
        _makeClosureDirect = Mi(typeof(CilAssembler), "MakeClosureDirect")!;
        _checkArityExact = Mi(typeof(Runtime), "CheckArityExact",
            new[] { typeof(string), typeof(LispObject[]), typeof(int) })!;
        _registerFunction = Mi(typeof(CilAssembler), "RegisterFunction")!;
        _registerFunctionOnSymbol = Mi(typeof(CilAssembler), "RegisterFunctionOnSymbol")!;
        _registerSetfFunctionOnSymbol = Mi(typeof(CilAssembler), "RegisterSetfFunctionOnSymbol")!;
        if (CallCoverage.Enabled) { EnsureSimd(); CallCoverage.Init(_methodCache.Keys.Concat(_simdMethods!.Keys).ToList()); }
    }

        private static readonly object _simdLock = new();
        private static volatile Dictionary<string, MethodInfo>? _simdMethods;
        private static volatile Dictionary<string, Type>? _simdTypes;

        /// <summary>The SIMD entries (generic instantiations, slow to build), built once
        /// on the first lookup that misses the main tables. They live in their own tables,
        /// published whole, so a thread reading the main tables never sees them change.</summary>
        internal static void EnsureSimd()
        {
            if (_simdTypes != null) return;
            lock (_simdLock)
            {
                if (_simdTypes != null) return;
                var methods = new Dictionary<string, MethodInfo>();
                foreach (var (name, m) in SimdLower.Methods()) methods[name] = m;
                var types = new Dictionary<string, Type>();
                foreach (var t in SimdLower.LaneTypes)
                    types["V128." + t.Name] = typeof(System.Runtime.Intrinsics.Vector128<>).MakeGenericType(t);
                _simdMethods = methods;
                _simdTypes = types;   // published last: a non-null _simdTypes means both are complete
            }
        }

        internal static bool TrySimdMethod(string name, out MethodInfo? mi)
        {
            EnsureSimd();
            return _simdMethods!.TryGetValue(name, out mi);
        }

        internal static bool TrySimdType(string name, out Type? t)
        {
            EnsureSimd();
            return _simdTypes!.TryGetValue(name, out t);
        }
    }
}
