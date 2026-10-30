using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    // --- Static resolution tables ---

    private static readonly Dictionary<string, MethodInfo> _methodCache;
    private static readonly Dictionary<string, FieldInfo> _fieldCache;
    private static readonly Dictionary<string, ConstructorInfo> _ctorCache;
    private static readonly Dictionary<string, Type> _typeCache;
    private static readonly MethodInfo _getConstant;
    private static readonly MethodInfo _getConstantObject;
    private static readonly MethodInfo _getConstantSymbol;
    private static readonly MethodInfo _symFnSiteResolve;
    private static readonly MethodInfo _fnSiteSymbol;
    private static readonly MethodInfo _symSiteResolve;
    private static readonly MethodInfo _getUnitConstant;
    private static readonly MethodInfo _makeFaslInstance;
    private static readonly MethodInfo _internViaEvalInstance;
    private static readonly MethodInfo _makeClosure;
    private static readonly MethodInfo _makeClosureDirect;
    private static readonly MethodInfo _checkArityExact;
    private static readonly MethodInfo _registerFunction;
    private static readonly MethodInfo _registerFunctionOnSymbol;
    private static readonly MethodInfo _registerSetfFunctionOnSymbol;

    static CilAssembler()
    {
        _methodCache = new Dictionary<string, MethodInfo>
        {
            // Fixnum
            ["Fixnum.Make"] = typeof(Fixnum).GetMethod("Make")!,
            ["DoubleFloat.Box"] = typeof(DoubleFloat).GetMethod("Box")!,
            ["SingleFloat.Box"] = typeof(SingleFloat).GetMethod("Box")!,

            // LispChar
            ["LispChar.Make"] = typeof(LispChar).GetMethod("Make")!,

            // Runtime - arithmetic
            ["Runtime.Add"] = typeof(Runtime).GetMethod("Add")!,
            ["Runtime.Subtract"] = typeof(Runtime).GetMethod("Subtract")!,
            ["Runtime.Negate"] = typeof(Runtime).GetMethod("Negate", new[] { typeof(LispObject) })!,
            ["Runtime.Increment"] = typeof(Runtime).GetMethod("Increment")!,
            ["Runtime.Decrement"] = typeof(Runtime).GetMethod("Decrement")!,
            ["Runtime.Multiply"] = typeof(Runtime).GetMethod("Multiply")!,
            ["Runtime.MultiplyFixnum"] = typeof(Runtime).GetMethod("MultiplyFixnum")!,
            ["Runtime.AddFixnum"] = typeof(Runtime).GetMethod("AddFixnum")!,
            ["Runtime.AddFixnumChecked"] = typeof(Runtime).GetMethod("AddFixnumChecked")!,
            ["Runtime.AddFixnumCheckedNonNeg"] = typeof(Runtime).GetMethod("AddFixnumCheckedNonNeg")!,
            ["Runtime.AddOrMin"] = typeof(Runtime).GetMethod("AddOrMin")!,
            ["Runtime.SubtractOrMin"] = typeof(Runtime).GetMethod("SubtractOrMin")!,
            ["Runtime.MultiplyOrMin"] = typeof(Runtime).GetMethod("MultiplyOrMin")!,
            ["Runtime.TypedArgL"] = typeof(Runtime).GetMethod("TypedArgL")!,
            ["Runtime.TypedArgLR"] = typeof(Runtime).GetMethod("TypedArgLR")!,
            ["Runtime.TypedArgD"] = typeof(Runtime).GetMethod("TypedArgD")!,
            ["Runtime.TypedRetL"] = typeof(Runtime).GetMethod("TypedRetL")!,
            ["Runtime.TypedRetD"] = typeof(Runtime).GetMethod("TypedRetD")!,
            ["Runtime.CheckLongRange"] = typeof(Runtime).GetMethod("CheckLongRange")!,
            ["Runtime.CheckArgFixnum"] = typeof(Runtime).GetMethod("CheckArgFixnum")!,
            ["Runtime.CheckArgRange"] = typeof(Runtime).GetMethod("CheckArgRange")!,
            ["Runtime.CheckArgU64"] = typeof(Runtime).GetMethod("CheckArgU64")!,
            ["Runtime.CheckArgDouble"] = typeof(Runtime).GetMethod("CheckArgDouble")!,
            ["Runtime.CheckArgSingle"] = typeof(Runtime).GetMethod("CheckArgSingle")!,
            ["Runtime.CheckArgType"] = typeof(Runtime).GetMethod("CheckArgType")!,
            ["Runtime.ArgTypeError"] = typeof(Runtime).GetMethod("ArgTypeError")!,
            ["Runtime.CheckArgList"] = typeof(Runtime).GetMethod("CheckArgList")!,
            ["Runtime.CheckArgCons"] = typeof(Runtime).GetMethod("CheckArgCons")!,
            ["Runtime.CheckArgSymbol"] = typeof(Runtime).GetMethod("CheckArgSymbol")!,
            ["Runtime.CheckArgFunction"] = typeof(Runtime).GetMethod("CheckArgFunction")!,
            ["Runtime.CheckArgCharacter"] = typeof(Runtime).GetMethod("CheckArgCharacter")!,
            ["Runtime.CheckArgInteger"] = typeof(Runtime).GetMethod("CheckArgInteger")!,
            ["Runtime.CheckArgString"] = typeof(Runtime).GetMethod("CheckArgString")!,
            ["Runtime.CheckArgStructSymKnown"] = typeof(Runtime).GetMethod("CheckArgStructSymKnown")!,
            ["Runtime.CheckArgStructSym"] = typeof(Runtime).GetMethod("CheckArgStructSym")!,
            ["Runtime.CheckArgSimpleVector"] = typeof(Runtime).GetMethod("CheckArgSimpleVector")!,
            ["Runtime.CheckArgSimpleVectorN"] = typeof(Runtime).GetMethod("CheckArgSimpleVectorN")!,
            ["Runtime.CheckTheArray"] = typeof(Runtime).GetMethod("CheckTheArray")!,
            ["Runtime.SubtractFixnumChecked"] = typeof(Runtime).GetMethod("SubtractFixnumChecked")!,
            ["Runtime.MultiplyFixnumChecked"] = typeof(Runtime).GetMethod("MultiplyFixnumChecked")!,
            ["Runtime.SubtractFixnum"] = typeof(Runtime).GetMethod("SubtractFixnum")!,
            ["Runtime.Divide"] = typeof(Runtime).GetMethod("Divide")!,
            ["Runtime.AddN"] = typeof(Runtime).GetMethod("AddN")!,
            ["Runtime.SubtractN"] = typeof(Runtime).GetMethod("SubtractN")!,
            ["Runtime.MultiplyN"] = typeof(Runtime).GetMethod("MultiplyN")!,
            ["Runtime.DivideN"] = typeof(Runtime).GetMethod("DivideN")!,

            // Runtime - comparison
            ["Runtime.GreaterThan"] = typeof(Runtime).GetMethod("GreaterThan")!,
            ["Runtime.LessThan"] = typeof(Runtime).GetMethod("LessThan")!,
            ["Runtime.GreaterEqual"] = typeof(Runtime).GetMethod("GreaterEqual")!,
            ["Runtime.LessEqual"] = typeof(Runtime).GetMethod("LessEqual")!,
            ["Runtime.NumEqual"] = typeof(Runtime).GetMethod("NumEqual")!,
            ["Runtime.NumNotEqual"] = typeof(Runtime).GetMethod("NumNotEqual")!,
            ["Runtime.NumEqualN"] = typeof(Runtime).GetMethod("NumEqualN")!,
            ["Runtime.NumNotEqualN"] = typeof(Runtime).GetMethod("NumNotEqualN")!,
            ["Runtime.LessThanN"] = typeof(Runtime).GetMethod("LessThanN")!,
            ["Runtime.GreaterThanN"] = typeof(Runtime).GetMethod("GreaterThanN")!,
            ["Runtime.LessEqualN"] = typeof(Runtime).GetMethod("LessEqualN")!,
            ["Runtime.GreaterEqualN"] = typeof(Runtime).GetMethod("GreaterEqualN")!,

            // Runtime - bool-returning comparisons (fused comparison+branch)
            ["Runtime.IsTrueGt"] = typeof(Runtime).GetMethod("IsTrueGt")!,
            ["Runtime.IsTrueLt"] = typeof(Runtime).GetMethod("IsTrueLt")!,
            ["Runtime.IsTrueGe"] = typeof(Runtime).GetMethod("IsTrueGe")!,
            ["Runtime.IsTrueLe"] = typeof(Runtime).GetMethod("IsTrueLe")!,
            ["Runtime.IsTrueNumEq"] = typeof(Runtime).GetMethod("IsTrueNumEq")!,
            ["Runtime.IsTrueZerop"] = typeof(Runtime).GetMethod("IsTrueZerop")!,
            ["Runtime.Zerop"] = typeof(Runtime).GetMethod("Zerop")!,
            ["AbsentKey.Is"] = typeof(AbsentKey).GetMethod("Is")!,
            ["Runtime.IsTrueMinusp"] = typeof(Runtime).GetMethod("IsTrueMinusp")!,
            ["Runtime.IsTruePlusp"] = typeof(Runtime).GetMethod("IsTruePlusp")!,
            // Used by the inline symbol comparison the compiler emits in place of
            // an IsTrueEq/IsTrueEql call: the value still has to be normalized
            // before the reference compare.
            ["Runtime.Primary"] = typeof(Runtime).GetMethod("Primary")!,
            ["Runtime.IsTrueEq"] = typeof(Runtime).GetMethod("IsTrueEq")!,
            ["Runtime.IsNilObj"] = typeof(Runtime).GetMethod("IsNilObj")!,
            ["Runtime.IsTrueEql"] = typeof(Runtime).GetMethod("IsTrueEql")!,
            ["Runtime.IsTrueEqual"] = typeof(Runtime).GetMethod("IsTrueEqual")!,
            ["Runtime.IsTrueEqualp"] = typeof(Runtime).GetMethod("IsTrueEqualp")!,
            ["Runtime.IsTrueTypep"] = typeof(Runtime).GetMethod("IsTrueTypep")!,
            ["Runtime.IsTrueConsp"] = typeof(Runtime).GetMethod("IsTrueConsp")!,
            ["Runtime.IsTrueEndp"] = typeof(Runtime).GetMethod("IsTrueEndp")!,
            ["Runtime.IsTrueAtom"] = typeof(Runtime).GetMethod("IsTrueAtom")!,
            ["Runtime.IsTrueListp"] = typeof(Runtime).GetMethod("IsTrueListp")!,
            ["Runtime.IsTrueNumberp"] = typeof(Runtime).GetMethod("IsTrueNumberp")!,
            ["Runtime.IsTrueIntegerp"] = typeof(Runtime).GetMethod("IsTrueIntegerp")!,
            ["Runtime.IsFixnumObject"] = typeof(Runtime).GetMethod("IsFixnumObject")!,
            ["Runtime.IsConsObject"] = typeof(Runtime).GetMethod("IsConsObject")!,
            ["Runtime.IsAtomObject"] = typeof(Runtime).GetMethod("IsAtomObject")!,
            ["Runtime.IsListObject"] = typeof(Runtime).GetMethod("IsListObject")!,
            ["Runtime.IsTrueSymbolp"] = typeof(Runtime).GetMethod("IsTrueSymbolp")!,
            ["Runtime.IsTrueStringp"] = typeof(Runtime).GetMethod("IsTrueStringp")!,
            ["Runtime.IsTrueCharacterp"] = typeof(Runtime).GetMethod("IsTrueCharacterp")!,
            ["Runtime.IsTrueFunctionp"] = typeof(Runtime).GetMethod("IsTrueFunctionp")!,

            // Runtime - equality
            ["Runtime.Eq"] = typeof(Runtime).GetMethod("Eq")!,
            ["Runtime.Eql"] = typeof(Runtime).GetMethod("Eql")!,
            ["Runtime.Equal"] = typeof(Runtime).GetMethod("Equal")!,
            ["Runtime.Equalp"] = typeof(Runtime).GetMethod("Equalp")!,

            // Runtime - list ops
            ["Runtime.Car"] = typeof(Runtime).GetMethod("Car")!,
            ["Runtime.Cdr"] = typeof(Runtime).GetMethod("Cdr")!,
            ["Runtime.MakeCons"] = typeof(Runtime).GetMethod("MakeCons")!,
            ["Runtime.CheckUnaryArity"] = typeof(Runtime).GetMethod("CheckUnaryArity")!,
            ["Runtime.CheckBinaryArity"] = typeof(Runtime).GetMethod("CheckBinaryArity")!,
            ["Runtime.ProgramError"] = typeof(Runtime).GetMethod("ProgramError", new[] { typeof(string) })!,
            ["Runtime.CheckArityExact"] = typeof(Runtime).GetMethod("CheckArityExact")!,
            ["Runtime.CheckArityMin"] = typeof(Runtime).GetMethod("CheckArityMin")!,
            ["Runtime.CheckArityMax"] = typeof(Runtime).GetMethod("CheckArityMax")!,
            ["Runtime.CheckNoUnknownKeys"] = typeof(Runtime).GetMethod("CheckNoUnknownKeys")!,
            ["Runtime.CheckNoUnknownKeys2"] = typeof(Runtime).GetMethod("CheckNoUnknownKeys2")!,
            ["Runtime.List"] = typeof(Runtime).GetMethod("List")!,
            ["Runtime.ListStar"] = typeof(Runtime).GetMethod("ListStar")!,
            ["Runtime.Append"] = typeof(Runtime).GetMethod("Append")!,
            ["Runtime.Length"] = typeof(Runtime).GetMethod("Length")!,
            ["Runtime.Rplaca"] = typeof(Runtime).GetMethod("Rplaca")!,
            ["Runtime.Rplacd"] = typeof(Runtime).GetMethod("Rplacd")!,
            ["Runtime.Nreverse"] = typeof(Runtime).GetMethod("Nreverse")!,
            ["Runtime.Nth"] = typeof(Runtime).GetMethod("Nth")!,
            ["Runtime.Nthcdr"] = typeof(Runtime).GetMethod("Nthcdr")!,
            ["Runtime.Last"] = typeof(Runtime).GetMethod("Last")!,
            ["Runtime.Nconc2"] = typeof(Runtime).GetMethod("Nconc2")!,
            ["Runtime.NconcDestructive2"] = typeof(Runtime).GetMethod("NconcDestructive2")!,
            ["Runtime.Butlast"] = typeof(Runtime).GetMethod("Butlast")!,
            ["Runtime.CopyList"] = typeof(Runtime).GetMethod("CopyList")!,
            ["Runtime.Member"] = typeof(Runtime).GetMethod("Member")!,
            ["Runtime.MemberEq"] = typeof(Runtime).GetMethod("MemberEq")!,
            ["Runtime.FindEql"] = typeof(Runtime).GetMethod("FindEql")!,
            ["Runtime.FindEqual"] = typeof(Runtime).GetMethod("FindEqual")!,
            ["Runtime.FindEqualp"] = typeof(Runtime).GetMethod("FindEqualp")!,
            ["Runtime.FindCharEq"] = typeof(Runtime).GetMethod("FindCharEq")!,
            ["Runtime.FindCharEqual"] = typeof(Runtime).GetMethod("FindCharEqual")!,
            ["Runtime.PositionEql"] = typeof(Runtime).GetMethod("PositionEql")!,
            ["Runtime.PositionEqual"] = typeof(Runtime).GetMethod("PositionEqual")!,
            ["Runtime.PositionEqualp"] = typeof(Runtime).GetMethod("PositionEqualp")!,
            ["Runtime.PositionCharEqual"] = typeof(Runtime).GetMethod("PositionCharEqual")!,
            ["Runtime.PositionCharEq"] = typeof(Runtime).GetMethod("PositionCharEq")!,
            ["Runtime.PositionLiteralString"] = typeof(Runtime).GetMethod("PositionLiteralString")!,
            ["Runtime.FindLiteralString"] = typeof(Runtime).GetMethod("FindLiteralString")!,
            ["Runtime.Assoc"] = typeof(Runtime).GetMethod("Assoc")!,
            ["Runtime.AssocEq"] = typeof(Runtime).GetMethod("AssocEq")!,
            ["Runtime.Cadr"] = typeof(Runtime).GetMethod("Cadr")!,
            ["Runtime.Cddr"] = typeof(Runtime).GetMethod("Cddr")!,
            ["Runtime.Caar"] = typeof(Runtime).GetMethod("Caar")!,
            ["Runtime.Cdar"] = typeof(Runtime).GetMethod("Cdar")!,
            ["Runtime.Caddr"] = typeof(Runtime).GetMethod("Caddr")!,

            // Runtime - predicates
            ["Runtime.Not"] = typeof(Runtime).GetMethod("Not")!,
            ["Runtime.Atom"] = typeof(Runtime).GetMethod("Atom")!,
            ["Runtime.Consp"] = typeof(Runtime).GetMethod("Consp")!,
            ["Runtime.Endp"] = typeof(Runtime).GetMethod("Endp")!,
            ["Runtime.IsNotNil"] = typeof(Runtime).GetMethod("IsNotNil")!,
            ["Runtime.Listp"] = typeof(Runtime).GetMethod("Listp")!,
            ["Runtime.Numberp"] = typeof(Runtime).GetMethod("Numberp")!,
            ["Runtime.Integerp"] = typeof(Runtime).GetMethod("Integerp")!,
            ["Runtime.Symbolp"] = typeof(Runtime).GetMethod("Symbolp")!,
            ["Runtime.Stringp"] = typeof(Runtime).GetMethod("Stringp")!,
            ["Runtime.Characterp"] = typeof(Runtime).GetMethod("Characterp")!,
            ["Runtime.Functionp"] = typeof(Runtime).GetMethod("Functionp")!,
            ["Runtime.Rationalp"] = typeof(Runtime).GetMethod("Rationalp")!,
            ["Runtime.Floatp"] = typeof(Runtime).GetMethod("Floatp")!,
            ["Runtime.Complexp"] = typeof(Runtime).GetMethod("Complexp")!,
            ["Runtime.Vectorp"] = typeof(Runtime).GetMethod("Vectorp")!,
            ["Runtime.Hash_table_p"] = typeof(Runtime).GetMethod("Hash_table_p")!,
            ["Runtime.Packagep"] = typeof(Runtime).GetMethod("Packagep")!,
            ["Runtime.Keywordp"] = typeof(Runtime).GetMethod("Keywordp")!,
            ["Runtime.TypeOf"] = typeof(Runtime).GetMethod("TypeOf")!,
            ["Runtime.Typep"] = typeof(Runtime).GetMethod("Typep")!,
            ["Runtime.SetChar"] = typeof(Runtime).GetMethod("SetChar")!,
            ["Runtime.SetElt"] = typeof(Runtime).GetMethod("SetElt")!,
            ["Runtime.Putf"] = typeof(Runtime).GetMethod("Putf")!,
            ["Runtime.IsTruthy"] = typeof(Runtime).GetMethod("IsTruthy")!,
            ["Runtime.CharAccess"] = typeof(Runtime).GetMethod("CharAccess")!,
            ["Runtime.CharSet"] = typeof(Runtime).GetMethod("CharSet")!,
            ["Runtime.CharEqual"] = typeof(Runtime).GetMethod("CharEqual")!,
            ["Runtime.Maphash"] = typeof(Runtime).GetMethod("Maphash")!,
            ["Runtime.HashWalkBegin"] = typeof(Runtime).GetMethod("HashWalkBegin")!,
            ["Runtime.HashWalkNext"] = typeof(Runtime).GetMethod("HashWalkNext")!,
            ["Runtime.HashWalkKey"] = typeof(Runtime).GetMethod("HashWalkKey")!,
            ["Runtime.HashWalkValue"] = typeof(Runtime).GetMethod("HashWalkValue")!,
            ["Runtime.HashWalkEnd"] = typeof(Runtime).GetMethod("HashWalkEnd")!,

            // Runtime - math
            ["Runtime.Abs"] = typeof(Runtime).GetMethod("Abs")!,
            // Raw magnitude for a value already on the stack as r8/r4. ABS of a
            // float is a float of the same format, so a declared-float ABS needs
            // neither the boxed argument nor the boxed result the general entry
            // requires. Math.Abs and the general entry agree on the edges that
            // matter here: -0.0 gives +0.0 and NaN stays NaN.
            ["Math.AbsDouble"] = typeof(System.Math).GetMethod("Abs", new[] { typeof(double) })!,
            ["Math.AbsSingle"] = typeof(System.Math).GetMethod("Abs", new[] { typeof(float) })!,
            // SQRT of a declared double. The checked entry is the (safety 1) one:
            // a negative argument means the declared type is wrong.
            ["Math.SqrtDouble"] = typeof(System.Math).GetMethod("Sqrt", new[] { typeof(double) })!,
            ["Runtime.SqrtDoubleChecked"] = typeof(Runtime).GetMethod("SqrtDoubleChecked")!,
            // LOG of a declared double, under the same rule as SQRT.
            ["Runtime.LogDouble"] = typeof(Runtime).GetMethod("LogDouble")!,
            ["Runtime.LogDoubleChecked"] = typeof(Runtime).GetMethod("LogDoubleChecked")!,
            // SIN, COS, ... of a declared double: a double for every double
            // argument, the same System.Math call the generic entries make.
            ["Math.SinDouble"] = typeof(System.Math).GetMethod("Sin", new[] { typeof(double) })!,
            ["Math.CosDouble"] = typeof(System.Math).GetMethod("Cos", new[] { typeof(double) })!,
            ["Math.TanDouble"] = typeof(System.Math).GetMethod("Tan", new[] { typeof(double) })!,
            ["Math.AtanDouble"] = typeof(System.Math).GetMethod("Atan", new[] { typeof(double) })!,
            ["Math.SinhDouble"] = typeof(System.Math).GetMethod("Sinh", new[] { typeof(double) })!,
            ["Math.CoshDouble"] = typeof(System.Math).GetMethod("Cosh", new[] { typeof(double) })!,
            ["Math.TanhDouble"] = typeof(System.Math).GetMethod("Tanh", new[] { typeof(double) })!,
            ["Runtime.ExpDouble"] = typeof(Runtime).GetMethod("ExpDouble")!,
            ["Runtime.AddChecked"] = typeof(Runtime).GetMethod("AddChecked")!,
            ["Runtime.SubtractChecked"] = typeof(Runtime).GetMethod("SubtractChecked")!,
            ["Runtime.MultiplyChecked"] = typeof(Runtime).GetMethod("MultiplyChecked")!,
            ["Runtime.DivideChecked"] = typeof(Runtime).GetMethod("DivideChecked")!,
            ["Runtime.CoerceChecked"] = typeof(Runtime).GetMethod("CoerceChecked")!,
            ["Math.ExpDouble"] = typeof(System.Math).GetMethod("Exp", new[] { typeof(double) })!,
            ["FloatLane.AddD"] = typeof(FloatLane).GetMethod("AddD")!,
            ["FloatLane.SubtractD"] = typeof(FloatLane).GetMethod("SubtractD")!,
            ["FloatLane.MultiplyD"] = typeof(FloatLane).GetMethod("MultiplyD")!,
            ["FloatLane.DivideD"] = typeof(FloatLane).GetMethod("DivideD")!,
            ["FloatLane.AddS"] = typeof(FloatLane).GetMethod("AddS")!,
            ["FloatLane.SubtractS"] = typeof(FloatLane).GetMethod("SubtractS")!,
            ["FloatLane.MultiplyS"] = typeof(FloatLane).GetMethod("MultiplyS")!,
            ["FloatLane.DivideS"] = typeof(FloatLane).GetMethod("DivideS")!,
            ["FloatLane.NarrowD"] = typeof(FloatLane).GetMethod("NarrowD")!,
            ["FloatLane.NonFiniteD"] = typeof(FloatLane).GetMethod("NonFiniteD")!,
            ["FloatLane.SinD"] = typeof(FloatLane).GetMethod("SinD")!,
            ["FloatLane.LogD"] = typeof(FloatLane).GetMethod("LogD")!,
            ["Runtime.ExptChecked"] = typeof(Runtime).GetMethod("ExptChecked")!,
            ["FloatLane.CosD"] = typeof(FloatLane).GetMethod("CosD")!,
            ["FloatLane.TanD"] = typeof(FloatLane).GetMethod("TanD")!,
            ["FloatLane.NonFiniteS"] = typeof(FloatLane).GetMethod("NonFiniteS")!,
            ["FloatLane.SinhD"] = typeof(FloatLane).GetMethod("SinhD")!,
            ["FloatLane.CoshD"] = typeof(FloatLane).GetMethod("CoshD")!,
            ["Runtime.FloatIntegerS"] = typeof(Runtime).GetMethod("FloatIntegerS")!,
            ["Runtime.FloatIntegerD"] = typeof(Runtime).GetMethod("FloatIntegerD")!,
            ["Math.FloorDouble"] = typeof(System.Math).GetMethod("Floor", new[] { typeof(double) })!,
            ["Math.CeilingDouble"] = typeof(System.Math).GetMethod("Ceiling", new[] { typeof(double) })!,
            ["Math.TruncateDouble"] = typeof(System.Math).GetMethod("Truncate", new[] { typeof(double) })!,
            ["Math.RoundDouble"] = typeof(System.Math).GetMethod("Round", new[] { typeof(double) })!,
            ["Runtime.Mod"] = typeof(Runtime).GetMethod("Mod")!,
            ["Runtime.Rem"] = typeof(Runtime).GetMethod("Rem")!,
            ["Runtime.FloorOp"] = typeof(Runtime).GetMethod("FloorOp")!,
            ["Runtime.TruncateOp"] = typeof(Runtime).GetMethod("TruncateOp")!,
            ["Runtime.CeilingOp"] = typeof(Runtime).GetMethod("CeilingOp")!,
            ["Runtime.RoundOp"] = typeof(Runtime).GetMethod("RoundOp")!,
            // Single-value twins: the peephole swaps these in when the call site
            // discards the secondary value (see MultipleValues.Values2Primary).
            ["Runtime.FloorOpPrimary"] = typeof(Runtime).GetMethod("FloorOpPrimary")!,
            ["Runtime.TruncateOpPrimary"] = typeof(Runtime).GetMethod("TruncateOpPrimary")!,
            ["Runtime.CeilingOpPrimary"] = typeof(Runtime).GetMethod("CeilingOpPrimary")!,
            ["Runtime.RoundOpPrimary"] = typeof(Runtime).GetMethod("RoundOpPrimary")!,
            ["Runtime.FloorOpPair"] = typeof(Runtime).GetMethod("FloorOpPair")!,
            ["Runtime.TruncateOpPair"] = typeof(Runtime).GetMethod("TruncateOpPair")!,
            ["Runtime.CeilingOpPair"] = typeof(Runtime).GetMethod("CeilingOpPair")!,
            ["Runtime.RoundOpPair"] = typeof(Runtime).GetMethod("RoundOpPair")!,
            ["Runtime.Min"] = typeof(Runtime).GetMethod("Min")!,
            ["Math.MinLong"] = typeof(System.Math).GetMethod("Min", new[] { typeof(long), typeof(long) })!,
            ["Math.MaxLong"] = typeof(System.Math).GetMethod("Max", new[] { typeof(long), typeof(long) })!,
            ["Runtime.Max"] = typeof(Runtime).GetMethod("Max")!,
            ["Runtime.MakeStructRaw"] = typeof(Runtime).GetMethod("MakeStructRaw")!,
            ["Runtime.DoubleBits"] = typeof(Runtime).GetMethod("DoubleBits")!,
            ["Runtime.SingleBits"] = typeof(Runtime).GetMethod("SingleBits")!,
            ["Runtime.MinDouble"] = typeof(Runtime).GetMethod("MinDouble")!,
            ["Runtime.MaxDouble"] = typeof(Runtime).GetMethod("MaxDouble")!,
            ["Runtime.Gcd"] = typeof(Runtime).GetMethod("Gcd")!,
            ["Runtime.Lcm"] = typeof(Runtime).GetMethod("Lcm")!,
            ["Runtime.Expt"] = typeof(Runtime).GetMethod("Expt")!,
            ["Runtime.Logior"] = typeof(Runtime).GetMethod("Logior")!,
            ["Runtime.Logand"] = typeof(Runtime).GetMethod("Logand")!,
            ["Runtime.Logxor"] = typeof(Runtime).GetMethod("Logxor")!,
            ["Runtime.Logior2"] = typeof(Runtime).GetMethod("Logior2")!,
            ["Runtime.Low64"] = typeof(Runtime).GetMethod("Low64")!,
            ["Runtime.Ldb"] = typeof(Runtime).GetMethod("Ldb")!,
            ["Runtime.Dpb"] = typeof(Runtime).GetMethod("Dpb")!,
            ["Runtime.IsU64"] = typeof(Runtime).GetMethod("IsU64")!,
            ["Runtime.CheckU64Arg"] = typeof(Runtime).GetMethod("CheckU64Arg")!,
            ["Runtime.BoxU64"] = typeof(Runtime).GetMethod("BoxU64")!,
            ["Runtime.ShrU64"] = typeof(Runtime).GetMethod("ShrU64")!,
            ["Runtime.RotateLeft32"] = typeof(Runtime).GetMethod("RotateLeft32")!,
            ["Runtime.RotateLeft64"] = typeof(Runtime).GetMethod("RotateLeft64")!,
            ["Runtime.RotateByte"] = typeof(Runtime).GetMethod("RotateByte")!,
            ["Runtime.ReadU16LE"] = typeof(Runtime).GetMethod("ReadU16LE")!,
            ["Runtime.ReadU16BE"] = typeof(Runtime).GetMethod("ReadU16BE")!,
            ["Runtime.ReadU32LE"] = typeof(Runtime).GetMethod("ReadU32LE")!,
            ["Runtime.ReadU32BE"] = typeof(Runtime).GetMethod("ReadU32BE")!,
            ["Runtime.ReadU64LE"] = typeof(Runtime).GetMethod("ReadU64LE")!,
            ["Runtime.ReadU64BE"] = typeof(Runtime).GetMethod("ReadU64BE")!,
            ["Runtime.OctetWordRef"] = typeof(Runtime).GetMethod("OctetWordRef")!,
            ["Runtime.OctetWordRefChecked"] = typeof(Runtime).GetMethod("OctetWordRefChecked")!,
            ["Runtime.WriteU16LE"] = typeof(Runtime).GetMethod("WriteU16LE")!,
            ["Runtime.WriteU16BE"] = typeof(Runtime).GetMethod("WriteU16BE")!,
            ["Runtime.WriteU32LE"] = typeof(Runtime).GetMethod("WriteU32LE")!,
            ["Runtime.WriteU32BE"] = typeof(Runtime).GetMethod("WriteU32BE")!,
            ["Runtime.WriteU64LE"] = typeof(Runtime).GetMethod("WriteU64LE")!,
            ["Runtime.WriteU64BE"] = typeof(Runtime).GetMethod("WriteU64BE")!,
            ["Runtime.OctetWordSet"] = typeof(Runtime).GetMethod("OctetWordSet")!,
            ["Runtime.OctetWordSetChecked"] = typeof(Runtime).GetMethod("OctetWordSetChecked")!,
            ["Runtime.ShlU64"] = typeof(Runtime).GetMethod("ShlU64")!,
            ["Runtime.Logand2"] = typeof(Runtime).GetMethod("Logand2")!,
            ["Runtime.Logxor2"] = typeof(Runtime).GetMethod("Logxor2")!,
            ["Runtime.Lognot"] = typeof(Runtime).GetMethod("Lognot")!,
            ["Runtime.Logcount"] = typeof(Runtime).GetMethod("Logcount")!,
            ["Runtime.Logtest"] = typeof(Runtime).GetMethod("Logtest")!,
            ["Runtime.Ash"] = typeof(Runtime).GetMethod("Ash")!,
            ["Runtime.AshLeftLong"] = typeof(Runtime).GetMethod("AshLeftLong")!,
            ["Runtime.IntegerLength"] = typeof(Runtime).GetMethod("IntegerLength")!,
            ["Runtime.Logbitp"] = typeof(Runtime).GetMethod("Logbitp")!,

            // Runtime - I/O
            ["Runtime.Print"] = typeof(Runtime).GetMethod("Print")!,
            ["Runtime.Print2"] = typeof(Runtime).GetMethod("Print2")!,
            ["Runtime.Prin1"] = typeof(Runtime).GetMethod("Prin1")!,
            ["Runtime.Prin12"] = typeof(Runtime).GetMethod("Prin12")!,
            ["Runtime.Princ"] = typeof(Runtime).GetMethod("Princ")!,
            ["Runtime.Princ2"] = typeof(Runtime).GetMethod("Princ2")!,
            ["Runtime.Terpri"] = typeof(Runtime).GetMethod("Terpri")!,
            ["Runtime.FreshLine"] = typeof(Runtime).GetMethod("FreshLine")!,

            // Runtime - string
            ["Runtime.StringUpcase"] = typeof(Runtime).GetMethod("StringUpcase")!,
            ["Runtime.StringDowncase"] = typeof(Runtime).GetMethod("StringDowncase")!,
            ["Runtime.StringTrim"] = typeof(Runtime).GetMethod("StringTrim")!,
            ["Runtime.StringLeftTrim"] = typeof(Runtime).GetMethod("StringLeftTrim")!,
            ["Runtime.StringRightTrim"] = typeof(Runtime).GetMethod("StringRightTrim")!,
            ["Runtime.CharCode"] = typeof(Runtime).GetMethod("CharCode")!,
            ["Runtime.CodeChar"] = typeof(Runtime).GetMethod("CodeChar")!,
            ["Runtime.DigitCharP"] = typeof(Runtime).GetMethod("DigitCharP")!,

            // Runtime - symbol
            ["Runtime.SymbolName"] = typeof(Runtime).GetMethod("SymbolName")!,
            ["Runtime.SymbolPackage"] = typeof(Runtime).GetMethod("SymbolPackage")!,
            ["Runtime.SetSymbolValue"] = typeof(Runtime).GetMethod("SetSymbolValue")!,
            ["Runtime.GetProp"] = typeof(Runtime).GetMethod("GetProp")!,
            ["Runtime.PutProp"] = typeof(Runtime).GetMethod("PutProp")!,
            ["Runtime.Remprop"] = typeof(Runtime).GetMethod("Remprop")!,
            ["Runtime.CopySymbol"] = typeof(Runtime).GetMethod("CopySymbol")!,
            ["Runtime.CopySymbolFull"] = typeof(Runtime).GetMethod("CopySymbolFull")!,

            // Runtime - string comparison
            ["Runtime.StringEq"] = typeof(Runtime).GetMethod("StringEq")!,
            ["Runtime.StringLt"] = typeof(Runtime).GetMethod("StringLt")!,
            ["Runtime.StringGt"] = typeof(Runtime).GetMethod("StringGt")!,
            ["Runtime.StringLe"] = typeof(Runtime).GetMethod("StringLe")!,
            ["Runtime.StringGe"] = typeof(Runtime).GetMethod("StringGe")!,
            ["Runtime.StringNotEq"] = typeof(Runtime).GetMethod("StringNotEq")!,
            ["Runtime.String"] = typeof(Runtime).GetMethod("String")!,

            // Runtime - sequence operations
            ["Runtime.Elt"] = typeof(Runtime).GetMethod("Elt")!,
            ["Runtime.Subseq"] = typeof(Runtime).GetMethod("Subseq")!,
            ["Runtime.MakeArrayOfType"] = typeof(Runtime).GetMethod("MakeArrayOfType")!,
            ["Runtime.MakeArrayOfTypeInit"] = typeof(Runtime).GetMethod("MakeArrayOfTypeInit")!,
            ["Runtime.MakeArrayLike"] = typeof(Runtime).GetMethod("MakeArrayLike")!,
            ["Runtime.CheckArgSingleRange"] = typeof(Runtime).GetMethod("CheckArgSingleRange")!,
            ["Runtime.CheckArgDoubleRange"] = typeof(Runtime).GetMethod("CheckArgDoubleRange")!,
            ["Runtime.MakeArrayLikeInit"] = typeof(Runtime).GetMethod("MakeArrayLikeInit")!,
            ["Runtime.ReplaceKeys"] = typeof(Runtime).GetMethod("ReplaceKeys")!,
            ["Runtime.Concatenate"] = typeof(Runtime).GetMethod("Concatenate")!,
            ["Runtime.Sort"] = typeof(Runtime).GetMethod("Sort")!,
            ["Runtime.StableSort"] = typeof(Runtime).GetMethod("StableSort")!,
            ["Runtime.Reverse"] = typeof(Runtime).GetMethod("Reverse")!,
            ["Runtime.Coerce"] = typeof(Runtime).GetMethod("Coerce")!,
            ["Runtime.Search"] = typeof(Runtime).GetMethod("Search")!,

            // Runtime - higher-order
            ["Runtime.Apply"] = typeof(Runtime).GetMethod("Apply")!,
            ["Runtime.ApplySpread1"] = typeof(Runtime).GetMethod("ApplySpread1")!,
            ["Runtime.ApplyM"] = typeof(Runtime).GetMethod("ApplyM")!,
            ["Runtime.ApplySpread1M"] = typeof(Runtime).GetMethod("ApplySpread1M")!,
            ["Runtime.ApplySpread2M"] = typeof(Runtime).GetMethod("ApplySpread2M")!,
            ["Runtime.ApplySpread2"] = typeof(Runtime).GetMethod("ApplySpread2")!,
            ["Runtime.Mapcar"] = typeof(Runtime).GetMethod("Mapcar")!,
            ["Runtime.MapcarN"] = typeof(Runtime).GetMethod("MapcarN")!,
            ["Runtime.NthValueOf"] = typeof(Runtime).GetMethod("NthValueOf")!,
            ["Runtime.Mapcar2"] = typeof(Runtime).GetMethod("Mapcar2")!,

            // Runtime - rest args
            ["Runtime.CollectRestArgs"] = typeof(Runtime).GetMethod("CollectRestArgs")!,

            // Runtime - hash table
            ["Runtime.MakeHashTable"] = typeof(Runtime).GetMethod("MakeHashTable")!,
            ["Runtime.MakeHashTable0"] = typeof(Runtime).GetMethod("MakeHashTable0")!,
            ["Runtime.Gethash"] = typeof(Runtime).GetMethod("Gethash")!,
            ["Runtime.GethashPrimary"] = typeof(Runtime).GetMethod("GethashPrimary")!,
            ["Runtime.GethashPair"] = typeof(Runtime).GetMethod("GethashPair")!,
            ["Runtime.Puthash"] = typeof(Runtime).GetMethod("Puthash")!,
            ["Runtime.Remhash"] = typeof(Runtime).GetMethod("Remhash")!,

            // Runtime - values
            ["Runtime.Values"] = typeof(Runtime).GetMethod("Values")!,
            ["Runtime.Values2"] = typeof(Runtime).GetMethod("Values2")!,
            ["Runtime.Values2Primary"] = typeof(Runtime).GetMethod("Values2Primary")!,
            ["Runtime.Values2Pair"] = typeof(Runtime).GetMethod("Values2Pair")!,
            ["MultipleValues.CapturePair"] = typeof(MultipleValues).GetMethod("CapturePair")!,
            ["MultipleValues.PairNth"] = typeof(MultipleValues).GetMethod("PairNth")!,
            ["Runtime.MultipleValuesList"] = typeof(Runtime).GetMethod("MultipleValuesList")!,
            ["Runtime.MultipleValuesList1"] = typeof(Runtime).GetMethod("MultipleValuesList1")!,
            ["Runtime.UnwrapMv"] = typeof(Runtime).GetMethod("UnwrapMv")!,
            ["Runtime.XrefNote"] = typeof(Runtime).GetMethod("XrefNote")!,

            // DebugFrames: frame-locals mode only (see DebugFrames.cs)
            ["DebugFrames.Enter"] = typeof(DebugFrames).GetMethod("Enter")!,
            ["DebugFrame.Set"] = typeof(DebugFrame).GetMethod("Set")!,
            ["DebugFrame.SetBox"] = typeof(DebugFrame).GetMethod("SetBox")!,

            // ConditionSystem: loop back-edge interrupt safepoint
            ["ConditionSystem.PollInterrupt"] = typeof(ConditionSystem).GetMethod("PollInterrupt")!,
            ["ConditionSystem.PollPending"] = typeof(ConditionSystem).GetMethod("PollPending")!,
            ["ConditionSystem.PollSavesFloats"] = typeof(ConditionSystem).GetMethod("PollSavesFloats")!,
            ["ConditionSystem.PollInterruptSaved"] = typeof(ConditionSystem).GetMethod("PollInterruptSaved")!,
            ["ConditionSystem.PollSave1"] = typeof(ConditionSystem).GetMethod("PollSave1")!,
            ["ConditionSystem.PollSave2"] = typeof(ConditionSystem).GetMethod("PollSave2")!,
            ["ConditionSystem.PollSave3"] = typeof(ConditionSystem).GetMethod("PollSave3")!,
            ["ConditionSystem.PollSave4"] = typeof(ConditionSystem).GetMethod("PollSave4")!,
            ["ConditionSystem.PollSaveD"] = typeof(ConditionSystem).GetMethod("PollSaveD")!,
            ["ConditionSystem.PollLoadD"] = typeof(ConditionSystem).GetMethod("PollLoadD")!,

            // MultipleValues
            ["MultipleValues.Reset"] = typeof(MultipleValues).GetMethod("Reset")!,
            ["MultipleValues.ResetOnExit"] = typeof(MultipleValues).GetMethod("ResetOnExit")!,
            ["MultipleValues.TakeMode"] = typeof(MultipleValues).GetMethod("TakeMode")!,
            ["MultipleValues.PassMode"] = typeof(MultipleValues).GetMethod("PassMode")!,
            ["MultipleValues.Values2Mode"] = typeof(MultipleValues).GetMethod("Values2Mode")!,
            ["MultipleValues.Values3Mode"] = typeof(MultipleValues).GetMethod("Values3Mode")!,
            ["MultipleValues.Values4Mode"] = typeof(MultipleValues).GetMethod("Values4Mode")!,
            ["MultipleValues.Values5Mode"] = typeof(MultipleValues).GetMethod("Values5Mode")!,
            ["MultipleValues.Values6Mode"] = typeof(MultipleValues).GetMethod("Values6Mode")!,
            ["MultipleValues.Values7Mode"] = typeof(MultipleValues).GetMethod("Values7Mode")!,
            ["MultipleValues.Values8Mode"] = typeof(MultipleValues).GetMethod("Values8Mode")!,
            ["MultipleValues.ValuesArrayMode"] = typeof(MultipleValues).GetMethod("ValuesArrayMode")!,
            ["MultipleValues.CallBound"] = typeof(MultipleValues).GetMethod("CallBound")!,
            ["MultipleValues.Primary"] = typeof(MultipleValues).GetMethod("Primary")!,
            ["MultipleValues.PrimaryAlone"] = typeof(MultipleValues).GetMethod("PrimaryAlone")!,
            ["MultipleValues.SaveCount"] = typeof(MultipleValues).GetMethod("SaveCount")!,
            ["MultipleValues.SaveValues"] = typeof(MultipleValues).GetMethod("SaveValues")!,
            ["MultipleValues.HoldValues"] = typeof(MultipleValues).GetMethod("HoldValues")!,
            ["MultipleValues.RestoreValues"] = typeof(MultipleValues).GetMethod("RestoreValues")!,
            ["MultipleValues.RestoreSaved"] = typeof(MultipleValues).GetMethod("RestoreSaved")!,

            // Runtime - typep / error
            ["Runtime.Subtypep"] = typeof(Runtime).GetMethod("Subtypep")!,
            ["Runtime.SubtypepMv"] = typeof(Runtime).GetMethod("SubtypepMv")!,
            ["Runtime.SubtypepPrimary"] = typeof(Runtime).GetMethod("SubtypepPrimary")!,
            ["Runtime.SubtypepPair"] = typeof(Runtime).GetMethod("SubtypepPair")!,
            ["Runtime.Aref"] = typeof(Runtime).GetMethod("Aref")!,
            ["Runtime.ArefSet"] = typeof(Runtime).GetMethod("ArefSet")!,
            ["Runtime.ArefMulti"] = typeof(Runtime).GetMethod("ArefMulti")!,
            ["Runtime.ArefSetMulti"] = typeof(Runtime).GetMethod("ArefSetMulti")!,
            ["Runtime.Aref2D"] = typeof(Runtime).GetMethod("Aref2D")!,
            ["Runtime.ArefSet2D"] = typeof(Runtime).GetMethod("ArefSet2D")!,
            ["Runtime.Aref3D"] = typeof(Runtime).GetMethod("Aref3D")!,
            ["Runtime.ArefSet3D"] = typeof(Runtime).GetMethod("ArefSet3D")!,
            // Raw-long-index variants (no Fixnum box for statically-fixnum indices)
            ["Runtime.ArefL"] = typeof(Runtime).GetMethod("ArefL")!,
            ["Runtime.ArefSetL"] = typeof(Runtime).GetMethod("ArefSetL")!,
            ["Runtime.Aref2DL"] = typeof(Runtime).GetMethod("Aref2DL")!,
            ["Runtime.ArefSet2DL"] = typeof(Runtime).GetMethod("ArefSet2DL")!,
            ["Runtime.Aref3DL"] = typeof(Runtime).GetMethod("Aref3DL")!,
            ["Runtime.ArefSet3DL"] = typeof(Runtime).GetMethod("ArefSet3DL")!,
            ["Runtime.ArefCopy11L"] = typeof(Runtime).GetMethod("ArefCopy11L")!,
            ["Runtime.ArefCopy12L"] = typeof(Runtime).GetMethod("ArefCopy12L")!,
            ["Runtime.FuncallAref1L"] = typeof(Runtime).GetMethod("FuncallAref1L")!,
            ["Runtime.FuncallAref1LM"] = typeof(Runtime).GetMethod("FuncallAref1LM")!,
            ["Runtime.FuncallAref2L"] = typeof(Runtime).GetMethod("FuncallAref2L")!,
            ["Runtime.FuncallAref2LM"] = typeof(Runtime).GetMethod("FuncallAref2LM")!,
            ["Runtime.FuncallAref1LL"] = typeof(Runtime).GetMethod("FuncallAref1LL")!,
            ["Runtime.FuncallAref2LL"] = typeof(Runtime).GetMethod("FuncallAref2LL")!,
            ["Runtime.FuncallLong1"] = typeof(Runtime).GetMethod("FuncallLong1")!,
            ["Runtime.FuncallLong1M"] = typeof(Runtime).GetMethod("FuncallLong1M")!,
            ["Runtime.FuncallLong1L"] = typeof(Runtime).GetMethod("FuncallLong1L")!,
            ["Runtime.ArefCopy21L"] = typeof(Runtime).GetMethod("ArefCopy21L")!,
            ["Runtime.ArefCopy22L"] = typeof(Runtime).GetMethod("ArefCopy22L")!,
            ["Runtime.ArefSetLong1L"] = typeof(Runtime).GetMethod("ArefSetLong1L")!,
            ["Runtime.ArefSetLong2L"] = typeof(Runtime).GetMethod("ArefSetLong2L")!,
            // Raw-long-value variants for inferred numeric-backed array locals
            ["Runtime.IndexL"] = typeof(Runtime).GetMethod("IndexL")!,
            ["Runtime.ArefNumL"] = typeof(Runtime).GetMethod("ArefNumL")!,
            ["Runtime.BackingChars"] = typeof(Runtime).GetMethod("BackingChars")!,
            ["Runtime.BackingI64"] = typeof(Runtime).GetMethod("BackingI64")!,
            ["Runtime.BackingI32"] = typeof(Runtime).GetMethod("BackingI32")!,
            ["Runtime.BackingU16"] = typeof(Runtime).GetMethod("BackingU16")!,
            ["Runtime.BackingU8"] = typeof(Runtime).GetMethod("BackingU8")!,
            ["Runtime.BackingF64"] = typeof(Runtime).GetMethod("BackingF64")!,
            ["Runtime.BackingF32"] = typeof(Runtime).GetMethod("BackingF32")!,
            // Above (safety 0): the same fetches, signalling on a non-simple array
            ["Runtime.BackingCharsChecked"] = typeof(Runtime).GetMethod("BackingCharsChecked")!,
            ["Runtime.BackingI64Checked"] = typeof(Runtime).GetMethod("BackingI64Checked")!,
            ["Runtime.BackingI32Checked"] = typeof(Runtime).GetMethod("BackingI32Checked")!,
            ["Runtime.BackingU16Checked"] = typeof(Runtime).GetMethod("BackingU16Checked")!,
            ["Runtime.BackingU8Checked"] = typeof(Runtime).GetMethod("BackingU8Checked")!,
            ["Runtime.BackingF64Checked"] = typeof(Runtime).GetMethod("BackingF64Checked")!,
            ["Runtime.BackingF32Checked"] = typeof(Runtime).GetMethod("BackingF32Checked")!,
            // Element access the compiled code has proven in range
            ["Runtime.DataU8"] = typeof(Runtime).GetMethod("DataU8")!,
            ["Runtime.DataU16"] = typeof(Runtime).GetMethod("DataU16")!,
            ["Runtime.DataI32"] = typeof(Runtime).GetMethod("DataI32")!,
            ["Runtime.DataI64"] = typeof(Runtime).GetMethod("DataI64")!,
            ["Runtime.DataF64"] = typeof(Runtime).GetMethod("DataF64")!,
            ["Runtime.DataF32"] = typeof(Runtime).GetMethod("DataF32")!,
            ["Runtime.ConstantArrayChanged"] = typeof(Runtime).GetMethod("ConstantArrayChanged")!,
            ["Runtime.AtU8"] = typeof(Runtime).GetMethod("AtU8")!,
            ["Runtime.AtU16"] = typeof(Runtime).GetMethod("AtU16")!,
            ["Runtime.AtI32"] = typeof(Runtime).GetMethod("AtI32")!,
            ["Runtime.AtI64"] = typeof(Runtime).GetMethod("AtI64")!,
            ["Runtime.AtF64"] = typeof(Runtime).GetMethod("AtF64")!,
            ["Runtime.AtF32"] = typeof(Runtime).GetMethod("AtF32")!,
            ["Runtime.SetAtU8"] = typeof(Runtime).GetMethod("SetAtU8")!,
            ["Runtime.SetAtU16"] = typeof(Runtime).GetMethod("SetAtU16")!,
            ["Runtime.SetAtI32"] = typeof(Runtime).GetMethod("SetAtI32")!,
            ["Runtime.SetAtI64"] = typeof(Runtime).GetMethod("SetAtI64")!,
            ["Runtime.SetAtF64"] = typeof(Runtime).GetMethod("SetAtF64")!,
            ["Runtime.SetAtF32"] = typeof(Runtime).GetMethod("SetAtF32")!,
            // Above (safety 0), matching the element type's width as well
            ["Runtime.BackingI64Narrow"] = typeof(Runtime).GetMethod("BackingI64Narrow")!,
            ["Runtime.BackingI32Narrow"] = typeof(Runtime).GetMethod("BackingI32Narrow")!,
            ["Runtime.BackingU16Narrow"] = typeof(Runtime).GetMethod("BackingU16Narrow")!,
            ["Runtime.BackingU8Narrow"] = typeof(Runtime).GetMethod("BackingU8Narrow")!,
            ["Runtime.IsNonSimpleArray"] = typeof(Runtime).GetMethod("IsNonSimpleArray")!,
            ["Runtime.SignalDeclaredNotSimple"] = typeof(Runtime).GetMethod("SignalDeclaredNotSimple")!,
            ["Runtime.CheckStoreI32"] = typeof(Runtime).GetMethod("CheckStoreI32")!,
            ["Runtime.CheckStoreU16"] = typeof(Runtime).GetMethod("CheckStoreU16")!,
            ["Runtime.CheckStoreU8"] = typeof(Runtime).GetMethod("CheckStoreU8")!,
            ["Runtime.CheckStoreUnsigned"] = typeof(Runtime).GetMethod("CheckStoreUnsigned")!,
            ["Runtime.CheckStoreSigned"] = typeof(Runtime).GetMethod("CheckStoreSigned")!,
            // (unsigned-byte 64) elements as their bits (kind 11)
            ["Runtime.BackingU64"] = typeof(Runtime).GetMethod("BackingU64")!,
            ["Runtime.BackingU64Checked"] = typeof(Runtime).GetMethod("BackingU64Checked")!,
            ["Runtime.ArefU64Bits"] = typeof(Runtime).GetMethod("ArefU64Bits")!,
            ["Runtime.ArefSetU64Bits"] = typeof(Runtime).GetMethod("ArefSetU64Bits")!,
            ["Runtime.ArefU64Bits2D"] = typeof(Runtime).GetMethod("ArefU64Bits2D")!,
            ["Runtime.ArefU64Bits3D"] = typeof(Runtime).GetMethod("ArefU64Bits3D")!,
            ["Runtime.ArefSetU64Bits2D"] = typeof(Runtime).GetMethod("ArefSetU64Bits2D")!,
            ["Runtime.ArefSetU64Bits3D"] = typeof(Runtime).GetMethod("ArefSetU64Bits3D")!,
            ["Runtime.BackingNDU8"] = typeof(Runtime).GetMethod("BackingNDU8")!,
            ["Runtime.BackingNDU16"] = typeof(Runtime).GetMethod("BackingNDU16")!,
            ["Runtime.BackingNDI32"] = typeof(Runtime).GetMethod("BackingNDI32")!,
            ["Runtime.BackingNDI64"] = typeof(Runtime).GetMethod("BackingNDI64")!,
            ["Runtime.BackingNDU64"] = typeof(Runtime).GetMethod("BackingNDU64")!,
            ["Runtime.BackingNDF32"] = typeof(Runtime).GetMethod("BackingNDF32")!,
            ["Runtime.BackingNDF64"] = typeof(Runtime).GetMethod("BackingNDF64")!,
            ["Runtime.BackingIfU8"] = typeof(Runtime).GetMethod("BackingIfU8")!,
            ["Runtime.BackingIfU16"] = typeof(Runtime).GetMethod("BackingIfU16")!,
            ["Runtime.BackingIfI32"] = typeof(Runtime).GetMethod("BackingIfI32")!,
            ["Runtime.BackingIfI64"] = typeof(Runtime).GetMethod("BackingIfI64")!,
            ["Runtime.BackingIfU64"] = typeof(Runtime).GetMethod("BackingIfU64")!,
            ["Runtime.BackingIfF32"] = typeof(Runtime).GetMethod("BackingIfF32")!,
            ["Runtime.BackingIfF64"] = typeof(Runtime).GetMethod("BackingIfF64")!,
            ["Runtime.ArrayAxisOrZero"] = typeof(Runtime).GetMethod("ArrayAxisOrZero")!,
            ["Runtime.AxisIndexL"] = typeof(Runtime).GetMethod("AxisIndexL")!,
            ["Runtime.U64Bits"] = typeof(Runtime).GetMethod("U64Bits")!,
            ["Runtime.CheckStoreU64"] = typeof(Runtime).GetMethod("CheckStoreU64")!,
            ["Runtime.ArefSetNumL"] = typeof(Runtime).GetMethod("ArefSetNumL")!,
            ["Runtime.ArefNum2DL"] = typeof(Runtime).GetMethod("ArefNum2DL")!,
            ["Runtime.ArefSetNum2DL"] = typeof(Runtime).GetMethod("ArefSetNum2DL")!,
            ["Runtime.ArefNum3DL"] = typeof(Runtime).GetMethod("ArefNum3DL")!,
            ["Runtime.ArefSetNum3DL"] = typeof(Runtime).GetMethod("ArefSetNum3DL")!,
            // Raw-long character code out of a string, for (char-code (schar s i))
            ["Runtime.CharCodeAtL"] = typeof(Runtime).GetMethod("CharCodeAtL")!,
            ["Runtime.CharCodeOfL"] = typeof(Runtime).GetMethod("CharCodeOfL")!,
            // The character itself out of a string, for a bare (schar s i)
            ["Runtime.CharAtL"] = typeof(Runtime).GetMethod("CharAtL")!,
            // Raw-double-value variants for inferred float-backed array locals
            ["Runtime.ArefNumD"] = typeof(Runtime).GetMethod("ArefNumD")!,
            ["Runtime.ArefSetNumD"] = typeof(Runtime).GetMethod("ArefSetNumD")!,
            ["Runtime.ArefNum2DD"] = typeof(Runtime).GetMethod("ArefNum2DD")!,
            ["Runtime.ArefSetNum2DD"] = typeof(Runtime).GetMethod("ArefSetNum2DD")!,
            ["Runtime.ArefNum3DD"] = typeof(Runtime).GetMethod("ArefNum3DD")!,
            ["Runtime.ArefSetNum3DD"] = typeof(Runtime).GetMethod("ArefSetNum3DD")!,
            ["Runtime.LispError"] = typeof(Runtime).GetMethod("LispError")!,

            // Runtime - vector push
            ["Runtime.VectorPushExtend2"] = typeof(Runtime).GetMethod("VectorPushExtend2")!,
            ["Runtime.VectorPushExtendVoid2"] = typeof(Runtime).GetMethod("VectorPushExtendVoid2")!,
            ["Runtime.VectorPush2"] = typeof(Runtime).GetMethod("VectorPush2")!,

            // Runtime - struct operations
            ["Runtime.MakeStruct"] = typeof(Runtime).GetMethod("MakeStruct")!,
            ["Runtime.MakeStruct1"] = typeof(Runtime).GetMethod("MakeStruct1")!,
            ["Runtime.MakeStruct2"] = typeof(Runtime).GetMethod("MakeStruct2")!,
            ["Runtime.MakeStruct3"] = typeof(Runtime).GetMethod("MakeStruct3")!,
            ["Runtime.MakeStruct4"] = typeof(Runtime).GetMethod("MakeStruct4")!,
            ["Runtime.StructRef"] = typeof(Runtime).GetMethod("StructRef")!,
            ["Runtime.StructRefI"] = typeof(Runtime).GetMethod("StructRefI")!,
            ["Runtime.StructRefIT"] = typeof(Runtime).GetMethod("StructRefIT")!,
            ["Runtime.StructRefITMasked"] = typeof(Runtime).GetMethod("StructRefITMasked")!,
            ["Runtime.StructSetIT"] = typeof(Runtime).GetMethod("StructSetIT")!,
            ["Runtime.StructSetITMasked"] = typeof(Runtime).GetMethod("StructSetITMasked")!,
            ["Runtime.StructOf"] = typeof(Runtime).GetMethod("StructOf")!,
            ["Runtime.StructOfMasked"] = typeof(Runtime).GetMethod("StructOfMasked")!,
            // Raw-long slot read/write for fixnum-typed struct slots
            ["Runtime.StructRefL"] = typeof(Runtime).GetMethod("StructRefL")!,
            ["Runtime.StructSetL"] = typeof(Runtime).GetMethod("StructSetL")!,
            ["Runtime.StructRefU64"] = typeof(Runtime).GetMethod("StructRefU64")!,
            ["Runtime.StructSetU64"] = typeof(Runtime).GetMethod("StructSetU64")!,
            // The raw slot array fetched once per binding, and the two
            // reinterpretations a double slot needs on top of it
            ["Runtime.StructRawBacking"] = typeof(Runtime).GetMethod("StructRawBacking")!,
            ["Runtime.RawBitsToDouble"] = typeof(Runtime).GetMethod("RawBitsToDouble")!,
            ["Runtime.RawDoubleToBits"] = typeof(Runtime).GetMethod("RawDoubleToBits")!,
            ["Runtime.RawBitsToSingle"] = typeof(Runtime).GetMethod("RawBitsToSingle")!,
            ["Runtime.RawSingleToBits"] = typeof(Runtime).GetMethod("RawSingleToBits")!,
            ["Runtime.StructRefF"] = typeof(Runtime).GetMethod("StructRefF")!,
            ["Runtime.StructSetF"] = typeof(Runtime).GetMethod("StructSetF")!,
            ["Runtime.StructSet"] = typeof(Runtime).GetMethod("StructSet")!,
            ["Runtime.StructSetI"] = typeof(Runtime).GetMethod("StructSetI")!,
            ["Runtime.CheckSlotType"] = typeof(Runtime).GetMethod("CheckSlotType")!,
            ["Runtime.StructRefD"] = typeof(Runtime).GetMethod("StructRefD")!,
            ["Runtime.StructRefDK"] = typeof(Runtime).GetMethod("StructRefDK")!,
            ["Runtime.StructSetDK"] = typeof(Runtime).GetMethod("StructSetDK")!,
            ["Runtime.StructRefFK"] = typeof(Runtime).GetMethod("StructRefFK")!,
            ["Runtime.StructSetFK"] = typeof(Runtime).GetMethod("StructSetFK")!,
            ["Runtime.StructRefU64K"] = typeof(Runtime).GetMethod("StructRefU64K")!,
            ["Runtime.StructSetU64K"] = typeof(Runtime).GetMethod("StructSetU64K")!,
            ["Runtime.StructRefLK"] = typeof(Runtime).GetMethod("StructRefLK")!,
            ["Runtime.StructSetLK"] = typeof(Runtime).GetMethod("StructSetLK")!,
            ["Runtime.StructRawInline"] = typeof(Runtime).GetMethod("StructRawInline")!,
            ["Runtime.RawInlineAt"] = typeof(Runtime).GetMethod("RawInlineAt")!,
            ["Runtime.RawInlineSet"] = typeof(Runtime).GetMethod("RawInlineSet")!,
            ["Runtime.MakeStructRawInline"] = typeof(Runtime).GetMethod("MakeStructRawInline")!,
            ["Runtime.StructSetD"] = typeof(Runtime).GetMethod("StructSetD")!,
            ["Runtime.StructTypep"] = typeof(Runtime).GetMethod("StructTypep")!,
            ["Runtime.CopyStruct"] = typeof(Runtime).GetMethod("CopyStruct")!,

            // Runtime - &key helper
            ["Runtime.FindKeyArg"] = typeof(Runtime).GetMethod("FindKeyArg")!,
            ["Runtime.FindKeyArgByName"] = typeof(Runtime).GetMethod("FindKeyArgByName")!,

            // Startup
            ["Startup.Sym"] = typeof(Startup).GetMethod("Sym")!,
            ["Startup.SymInPkg"] = typeof(Startup).GetMethod("SymInPkg")!,
            ["Startup.SymFn"] = typeof(Startup).GetMethod("SymFn", new[] { typeof(string) })!,
            ["Startup.SymFn(string, string)"] = typeof(Startup).GetMethod("SymFn", new[] { typeof(string), typeof(string) })!,
            ["Startup.Keyword"] = typeof(Startup).GetMethod("Keyword")!,

            // DynamicBindings
            ["DynamicBindings.Get"] = typeof(DynamicBindings).GetMethod("Get")!,
            ["DynamicBindings.GetConstant"] = typeof(DynamicBindings).GetMethod("GetConstant")!,
            ["DynamicBindings.Set"] = typeof(DynamicBindings).GetMethod("Set")!,
            ["DynamicBindings.SetChecked"] = typeof(DynamicBindings).GetMethod("SetChecked")!,
            ["Runtime.DefineConstant"] = typeof(Runtime).GetMethod("DefineConstant")!,
            ["DynamicBindings.Push"] = typeof(DynamicBindings).GetMethod("Push")!,
            ["DynamicBindings.Pop"] = typeof(DynamicBindings).GetMethod("Pop")!,
            ["DynamicBindings.BindDepth"] = typeof(DynamicBindings).GetMethod("BindDepth")!,
            ["DynamicBindings.UnbindTo"] = typeof(DynamicBindings).GetMethod("UnbindTo")!,
            ["DynamicBindings.Here"] = typeof(DynamicBindings).GetMethod("Here")!,
            ["DynamicBindings.GetOn"] = typeof(DynamicBindings).GetMethod("GetOn")!,
            ["DynamicBindings.SetCheckedOn"] = typeof(DynamicBindings).GetMethod("SetCheckedOn")!,
            ["DynamicBindings.GetLazy"] = typeof(DynamicBindings).GetMethod("GetLazy")!,
            ["DynamicBindings.SetCheckedLazy"] = typeof(DynamicBindings).GetMethod("SetCheckedLazy")!,
            ["DynamicBindings.PushOn"] = typeof(DynamicBindings).GetMethod("PushOn")!,
            ["DynamicBindings.PopOn"] = typeof(DynamicBindings).GetMethod("PopOn")!,
            ["DynamicBindings.BindDepthOn"] = typeof(DynamicBindings).GetMethod("BindDepthOn")!,
            ["DynamicBindings.BindGroupDepth"] = typeof(DynamicBindings).GetMethod("BindGroupDepth")!,
            ["DynamicBindings.BindGroupDepthOn"] = typeof(DynamicBindings).GetMethod("BindGroupDepthOn")!,
            ["DynamicBindings.UnbindToOn"] = typeof(DynamicBindings).GetMethod("UnbindToOn")!,
            ["DynamicBindings.SetIfUnbound"] = typeof(DynamicBindings).GetMethod("SetIfUnbound")!,
            ["DynamicBindings.PushGroup2"] = typeof(DynamicBindings).GetMethod("PushGroup2")!,
            ["DynamicBindings.PushGroup2On"] = typeof(DynamicBindings).GetMethod("PushGroup2On")!,
            ["DynamicBindings.PushGroup3"] = typeof(DynamicBindings).GetMethod("PushGroup3")!,
            ["DynamicBindings.PushGroup3On"] = typeof(DynamicBindings).GetMethod("PushGroup3On")!,
            ["DynamicBindings.PushGroup4"] = typeof(DynamicBindings).GetMethod("PushGroup4")!,
            ["DynamicBindings.PushGroup4On"] = typeof(DynamicBindings).GetMethod("PushGroup4On")!,
            ["DynamicBindings.PushGroup5"] = typeof(DynamicBindings).GetMethod("PushGroup5")!,
            ["DynamicBindings.PushGroup5On"] = typeof(DynamicBindings).GetMethod("PushGroup5On")!,
            ["DynamicBindings.PushGroup6"] = typeof(DynamicBindings).GetMethod("PushGroup6")!,
            ["DynamicBindings.PushGroup6On"] = typeof(DynamicBindings).GetMethod("PushGroup6On")!,
            ["DynamicBindings.PushGroup7"] = typeof(DynamicBindings).GetMethod("PushGroup7")!,
            ["DynamicBindings.PushGroup7On"] = typeof(DynamicBindings).GetMethod("PushGroup7On")!,
            ["DynamicBindings.PushGroup8"] = typeof(DynamicBindings).GetMethod("PushGroup8")!,
            ["DynamicBindings.PushGroup8On"] = typeof(DynamicBindings).GetMethod("PushGroup8On")!,

            // LispFunction
            ["LispFunction.Invoke"] = typeof(LispFunction).GetMethod("Invoke")!,
            ["LispFunction.Invoke0"] = typeof(LispFunction).GetMethod("Invoke0")!,
            ["LispFunction.Invoke1"] = typeof(LispFunction).GetMethod("Invoke1")!,
            ["LispFunction.Invoke2"] = typeof(LispFunction).GetMethod("Invoke2")!,
            ["LispFunction.Invoke3"] = typeof(LispFunction).GetMethod("Invoke3")!,
            ["LispFunction.Invoke4"] = typeof(LispFunction).GetMethod("Invoke4")!,
            ["LispFunction.Invoke5"] = typeof(LispFunction).GetMethod("Invoke5")!,
            ["LispFunction.Invoke6"] = typeof(LispFunction).GetMethod("Invoke6")!,
            ["LispFunction.Invoke7"] = typeof(LispFunction).GetMethod("Invoke7")!,
            ["LispFunction.Invoke8"] = typeof(LispFunction).GetMethod("Invoke8")!,
            ["LispFunction.Invoke0M"] = typeof(LispFunction).GetMethod("Invoke0M")!,
            ["LispFunction.Invoke1M"] = typeof(LispFunction).GetMethod("Invoke1M")!,
            ["LispFunction.Invoke2M"] = typeof(LispFunction).GetMethod("Invoke2M")!,
            ["LispFunction.Invoke3M"] = typeof(LispFunction).GetMethod("Invoke3M")!,
            ["LispFunction.Invoke4M"] = typeof(LispFunction).GetMethod("Invoke4M")!,
            ["LispFunction.Invoke5M"] = typeof(LispFunction).GetMethod("Invoke5M")!,
            ["LispFunction.Invoke6M"] = typeof(LispFunction).GetMethod("Invoke6M")!,
            ["LispFunction.InvokeNative1"] = typeof(LispFunction).GetMethod("InvokeNative1")!,
            ["LispFunction.InvokeNative2"] = typeof(LispFunction).GetMethod("InvokeNative2")!,
            ["LispFunction.InvokeNative3"] = typeof(LispFunction).GetMethod("InvokeNative3")!,
            ["LispFunction.InvokeNative4"] = typeof(LispFunction).GetMethod("InvokeNative4")!,
            ["LispFunction.get_RawFunction"] =
                typeof(LispFunction).GetProperty("RawFunction")!.GetGetMethod()!,

            // CilAssembler statics
            ["CilAssembler.GetFunction"] = typeof(CilAssembler).GetMethod("GetFunction")!,
            ["CilAssembler.GetFunctionBySymbol"] = typeof(CilAssembler).GetMethod("GetFunctionBySymbol")!,
            ["CilAssembler.GetSetfFunctionBySymbol"] = typeof(CilAssembler).GetMethod("GetSetfFunctionBySymbol")!,
            ["CilAssembler.GetConstant"] = typeof(CilAssembler).GetMethod("GetConstant")!,
            ["CilAssembler.MakeClosure"] = typeof(CilAssembler).GetMethod("MakeClosure")!,
            ["CilAssembler.RegisterFunctionOnSymbol"] = typeof(CilAssembler).GetMethod("RegisterFunctionOnSymbol")!,
            ["CilAssembler.RegisterSetfFunctionOnSymbol"] = typeof(CilAssembler).GetMethod("RegisterSetfFunctionOnSymbol")!,
            ["CilAssembler.RegisterFunction"] = typeof(CilAssembler).GetMethod("RegisterFunction")!,

            // Runtime - symbol utilities
            ["Runtime.SetSymbolConstant"] = typeof(Runtime).GetMethod("SetSymbolConstant")!,
            ["Runtime.MarkSpecial"] = typeof(Runtime).GetMethod("MarkSpecial")!,
            ["Runtime.SetVariableDocumentation"] = typeof(Runtime).GetMethod("SetVariableDocumentation")!,

            // Runtime - error/signal/warn
            ["Runtime.LispErrorFormat"] = typeof(Runtime).GetMethod("LispErrorFormat")!,
            ["Runtime.LispSignal"] = typeof(Runtime).GetMethod("LispSignal")!,
            ["Runtime.LispSignalFormat"] = typeof(Runtime).GetMethod("LispSignalFormat")!,
            ["Runtime.LispWarn"] = typeof(Runtime).GetMethod("LispWarn")!,
            ["Runtime.LispWarnFormat"] = typeof(Runtime).GetMethod("LispWarnFormat")!,

            // HandlerClusterStack
            ["HandlerClusterStack.PushCluster"] = typeof(HandlerClusterStack).GetMethod("PushCluster")!,
            ["HandlerClusterStack.PopCluster"] = typeof(HandlerClusterStack).GetMethod("PopCluster")!,

            // RestartClusterStack
            ["RestartClusterStack.PushCluster"] = typeof(RestartClusterStack).GetMethod("PushCluster")!,
            ["RestartClusterStack.PopCluster"] = typeof(RestartClusterStack).GetMethod("PopCluster")!,
            ["RestartClusterStack.FindRestart"] = typeof(RestartClusterStack).GetMethod("FindRestartByName")!,
            ["RestartClusterStack.ComputeRestarts"] = typeof(RestartClusterStack).GetMethod("ComputeRestarts")!,

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
            ["Runtime.RestartArg"] = typeof(Runtime).GetMethod("RestartArg")!,
            ["Runtime.RestartArgsToList"] = typeof(Runtime).GetMethod("RestartArgsToList")!,
            ["Runtime.RestartArgsAsList"] = typeof(Runtime).GetMethod("RestartArgsAsList")!,
            ["Runtime.RestartKeyArg"] = typeof(Runtime).GetMethod("RestartKeyArg")!,
            ["Runtime.InvokeRestart"] = typeof(Runtime).GetMethod("InvokeRestart")!,
            ["Runtime.FindRestart"] = typeof(Runtime).GetMethod("FindRestart", new[] { typeof(LispObject) })!,
            ["Runtime.FindRestartN"] = typeof(Runtime).GetMethod("FindRestartN")!,
            ["Runtime.ComputeRestarts"] = typeof(Runtime).GetMethod("ComputeRestarts", Type.EmptyTypes)!,
            ["Runtime.ComputeRestartsN"] = typeof(Runtime).GetMethod("ComputeRestartsN")!,

            // CLOS operations
            ["Runtime.MakeFaslInstance"] = typeof(Runtime).GetMethod("MakeFaslInstance",
                new[] { typeof(string), typeof(string), typeof(LispObject[]) })!,
            ["Runtime.FindClass"] = typeof(Runtime).GetMethod("FindClass")!,
            ["Runtime.FindClassOrNil"] = typeof(Runtime).GetMethod("FindClassOrNil")!,
            ["Runtime.RegisterClass"] = typeof(Runtime).GetMethod("RegisterClass")!,
            ["Runtime.MakeClass"] = typeof(Runtime).GetMethod("MakeClass")!,
            ["Runtime.MakeClassFull"] = typeof(Runtime).GetMethod("MakeClassFull")!,
            ["Runtime.MakeClassFullOptions"] = typeof(Runtime).GetMethod("MakeClassFullOptions")!,
            ["Runtime.MakeSlotDef"] = typeof(Runtime).GetMethod("MakeSlotDef")!,
            ["Runtime.MakeSlotDefWithAllocation"] = typeof(Runtime).GetMethod("MakeSlotDefWithAllocation")!,
            ["Runtime.SetSlotDefRawOptions"] = typeof(Runtime).GetMethod("SetSlotDefRawOptions")!,
            ["Runtime.SetSlotDefDocumentation"] = typeof(Runtime).GetMethod("SetSlotDefDocumentation")!,
            ["Runtime.SetSlotDefAttrs"] = typeof(Runtime).GetMethod("SetSlotDefAttrs")!,
            ["Runtime.SetClassDefaultInitargs"] = typeof(Runtime).GetMethod("SetClassDefaultInitargs")!,
            ["Runtime.ClassOf"] = typeof(Runtime).GetMethod("ClassOf")!,
            ["Runtime.ClassName"] = typeof(Runtime).GetMethod("ClassName")!,
            ["Runtime.MakeInstanceRaw"] = typeof(Runtime).GetMethod("MakeInstanceRaw")!,
            ["Runtime.SlotValue"] = typeof(Runtime).GetMethod("SlotValue")!,
            ["Runtime.SetSlotValue"] = typeof(Runtime).GetMethod("SetSlotValue")!,
            ["Runtime.ReaderIC"] = typeof(Runtime).GetMethod("ReaderIC")!,
            ["Runtime.SlotValueIC"] = typeof(Runtime).GetMethod("SlotValueIC")!,
            ["Runtime.WriterIC"] = typeof(Runtime).GetMethod("WriterIC")!,
            ["Runtime.Boundp"] = typeof(Runtime).GetMethod("Boundp")!,
            ["Runtime.SymbolValue"] = typeof(Runtime).GetMethod("SymbolValue")!,
            ["Runtime.Fdefinition"] = typeof(Runtime).GetMethod("Fdefinition")!,
            ["Runtime.Getenv"] = typeof(Runtime).GetMethod("Getenv")!,
            ["Runtime.SlotBoundp"] = typeof(Runtime).GetMethod("SlotBoundp")!,
            ["Runtime.SlotExists"] = typeof(Runtime).GetMethod("SlotExists")!,
            ["Runtime.MakeInstanceWithInitargs"] = typeof(Runtime).GetMethod("MakeInstanceWithInitargs")!,
            ["Runtime.ChangeClass"] = typeof(Runtime).GetMethod("ChangeClass")!,

            // CLOS generic function operations
            ["Runtime.MakeGF"] = typeof(Runtime).GetMethod("MakeGF")!,
            ["Runtime.RegisterGF"] = typeof(Runtime).GetMethod("RegisterGF")!,
            ["Runtime.ClearDefgenericInlineMethods"] = typeof(Runtime).GetMethod("ClearDefgenericInlineMethods")!,
            ["Runtime.MarkDefgenericInlineMethod"] = typeof(Runtime).GetMethod("MarkDefgenericInlineMethod")!,
            ["Runtime.SetMethodCombination"] = typeof(Runtime).GetMethod("SetMethodCombination")!,
            ["Runtime.SetMethodCombinationOrder"] = typeof(Runtime).GetMethod("SetMethodCombinationOrder")!,
            ["Runtime.SetMethodCombinationArgs"] = typeof(Runtime).GetMethod("SetMethodCombinationArgs")!,
            ["Runtime.SetGFLambdaListInfo"] = typeof(Runtime).GetMethod("SetGFLambdaListInfo")!,
            ["Runtime.SetMethodLambdaListInfo"] = typeof(Runtime).GetMethod("SetMethodLambdaListInfo")!,
            ["Runtime.FindGF"] = typeof(Runtime).GetMethod("FindGF")!,
            ["Runtime.MakeMethod"] = typeof(Runtime).GetMethod("MakeMethod")!,
            ["Runtime.AddMethod"] = typeof(Runtime).GetMethod("AddMethod")!,
            ["Runtime.RemoveMethod"] = typeof(Runtime).GetMethod("RemoveMethod")!,
            ["Runtime.ComputeApplicableMethods"] = typeof(Runtime).GetMethod("ComputeApplicableMethods")!,
            ["Runtime.GetGFMethods"] = typeof(Runtime).GetMethod("GetGFMethods")!,
            ["Runtime.MethodSpecializers"] = typeof(Runtime).GetMethod("MethodSpecializers")!,
            ["Runtime.MethodQualifiers"] = typeof(Runtime).GetMethod("MethodQualifiers")!,
            ["Runtime.MethodFunction"] = typeof(Runtime).GetMethod("MethodFunction")!,
            ["Runtime.CallNextMethod"] = typeof(Runtime).GetMethod("CallNextMethod")!,
            ["Runtime.CallNextMethodLoose"] = typeof(Runtime).GetMethod("CallNextMethodLoose")!,
            ["Runtime.NextMethodP"] = typeof(Runtime).GetMethod("NextMethodP")!,
            ["Runtime.CapturedCnm"] = typeof(Runtime).GetMethod("CapturedCnm")!,
            ["Runtime.CapturedNmp"] = typeof(Runtime).GetMethod("CapturedNmp")!,

            // Runtime - package operations
            ["Runtime.MakePackage"] = typeof(Runtime).GetMethod("MakePackage")!,
            ["Runtime.PackageUse"] = typeof(Runtime).GetMethod("PackageUse")!,
            ["Runtime.PackageExport"] = typeof(Runtime).GetMethod("PackageExport")!,
            ["Runtime.PackageExternalSymbolsList"] = typeof(Runtime).GetMethod("PackageExternalSymbolsList")!,
            ["Runtime.PackageAllSymbolsList"] = typeof(Runtime).GetMethod("PackageAllSymbolsList")!,
            ["Runtime.HashTablePairs"] = typeof(Runtime).GetMethod("HashTablePairs")!,
            ["Runtime.CoerceToFunction"] = typeof(Runtime).GetMethod("CoerceToFunction")!,
            ["Runtime.PackageImport"] = typeof(Runtime).GetMethod("PackageImport")!,
            ["Runtime.PackageShadow"] = typeof(Runtime).GetMethod("PackageShadow")!,
            ["Runtime.PackageNickname"] = typeof(Runtime).GetMethod("PackageNickname")!,
            ["Runtime.FindPackage"] = typeof(Runtime).GetMethod("FindPackage")!,
            ["Runtime.PackageName"] = typeof(Runtime).GetMethod("PackageName")!,
            ["Runtime.PackageErrorPackage"] = typeof(Runtime).GetMethod("PackageErrorPackage")!,
            ["Runtime.InternSymbol"] = typeof(Runtime).GetMethod("InternSymbol")!,
            ["Runtime.InternSymbolV"] = typeof(Runtime).GetMethod("InternSymbolV")!,
            ["Runtime.InternMv"] = typeof(Runtime).GetMethod("InternMv")!,
            ["Runtime.InternPrimary"] = typeof(Runtime).GetMethod("InternPrimary")!,
            ["Runtime.MakePackageK"] = typeof(Runtime).GetMethod("MakePackageK")!,
            ["Runtime.DeletePackage"] = typeof(Runtime).GetMethod("DeletePackage")!,
            ["Runtime.RenamePackage"] = typeof(Runtime).GetMethod("RenamePackage")!,
            ["Runtime.FindSymbolL"] = typeof(Runtime).GetMethod("FindSymbolL")!,
            ["Runtime.FindSymbolMv"] = typeof(Runtime).GetMethod("FindSymbolMv")!,
            ["Runtime.FindSymbolPrimary"] = typeof(Runtime).GetMethod("FindSymbolPrimary")!,
            ["Runtime.UninternSymbol"] = typeof(Runtime).GetMethod("UninternSymbol")!,
            ["Runtime.UnexportSymbol"] = typeof(Runtime).GetMethod("UnexportSymbol")!,
            ["Runtime.UnusePackage"] = typeof(Runtime).GetMethod("UnusePackage")!,
            ["Runtime.ShadowingImport"] = typeof(Runtime).GetMethod("ShadowingImport")!,
            ["Runtime.PackageUsedByList"] = typeof(Runtime).GetMethod("PackageUsedByList")!,
            ["Runtime.PackageUseListL"] = typeof(Runtime).GetMethod("PackageUseListL")!,
            ["Runtime.PackageShadowingSymbols"] = typeof(Runtime).GetMethod("PackageShadowingSymbols")!,
            ["Runtime.PackageNicknamesList"] = typeof(Runtime).GetMethod("PackageNicknamesList")!,
            ["Runtime.ListAllPackages"] = typeof(Runtime).GetMethod("ListAllPackages", Type.EmptyTypes)!,
            ["Runtime.ListAllPackagesV"] = typeof(Runtime).GetMethod("ListAllPackagesV")!,
            ["Runtime.Random"] = typeof(Runtime).GetMethod("Random", new[] { typeof(LispObject) })!,
            ["Runtime.Random2"] = typeof(Runtime).GetMethod("Random2")!,
            ["Runtime.RandomDoubleRaw"] = typeof(Runtime).GetMethod("RandomDoubleRaw")!,
            ["Runtime.RandomDoubleRaw2"] = typeof(Runtime).GetMethod("RandomDoubleRaw2")!,

            // Runtime - format
            ["Runtime.Format"] = typeof(Runtime).GetMethod("Format")!,

            // Runtime - file I/O
            ["Runtime.OpenFile"] = typeof(Runtime).GetMethod("OpenFile")!,
            ["Runtime.CloseStream"] = typeof(Runtime).GetMethod("CloseStream")!,
            ["Runtime.OpenStreamP"] = typeof(Runtime).GetMethod("OpenStreamP")!,
            ["Runtime.ReadCharNoHang"] = typeof(Runtime).GetMethod("ReadCharNoHang")!,
            ["Runtime.Listen"] = typeof(Runtime).GetMethod("Listen")!,
            ["Runtime.ClearInput"] = typeof(Runtime).GetMethod("ClearInput")!,
            ["Runtime.WriteByte"] = typeof(Runtime).GetMethod("WriteByte")!,
            ["Runtime.ReadPreservingWhitespace"] = typeof(Runtime).GetMethod("ReadPreservingWhitespace")!,
            ["Runtime.ReadLine"] = typeof(Runtime).GetMethod("ReadLine", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.ReadLineMv"] = typeof(Runtime).GetMethod("ReadLineMv")!,
            ["Runtime.ReadLinePrimary"] = typeof(Runtime).GetMethod("ReadLinePrimary")!,
            ["Runtime.ReadChar"] = typeof(Runtime).GetMethod("ReadChar", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.PeekChar"] = typeof(Runtime).GetMethod("PeekChar", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.UnreadChar"] = typeof(Runtime).GetMethod("UnreadChar")!,
            ["Runtime.WriteChar"] = typeof(Runtime).GetMethod("WriteChar")!,
            ["Runtime.WriteString"] = typeof(Runtime).GetMethod("WriteString", new[] { typeof(LispObject[]) })!,
            ["Runtime.WriteLine"] = typeof(Runtime).GetMethod("WriteLine", new[] { typeof(LispObject[]) })!,
            ["Runtime.FixnumGeObject"] = typeof(Runtime).GetMethod("FixnumGeObject")!,
            ["Runtime.DotimesLimit"] = typeof(Runtime).GetMethod("DotimesLimit")!,
            ["Runtime.LoopLimitFloor"] = typeof(Runtime).GetMethod("LoopLimitFloor")!,
            ["Runtime.LoopBy"] = typeof(Runtime).GetMethod("LoopBy")!,
            ["Runtime.LoopByFixnum"] = typeof(Runtime).GetMethod("LoopByFixnum")!,
            ["MultipleValues.Values0"] = typeof(MultipleValues).GetMethod("Values0")!,
            ["MultipleValues.CaptureForBind"] = typeof(MultipleValues).GetMethod("CaptureForBind")!,
            ["MultipleValues.BindNth"] = typeof(MultipleValues).GetMethod("BindNth")!,
            ["MultipleValues.BindNthI"] = typeof(MultipleValues).GetMethod("BindNthI")!,
            ["Runtime.ModFixnumL"] = typeof(Runtime).GetMethod("ModFixnumL")!,
            ["Runtime.AshLong"] = typeof(Runtime).GetMethod("AshLong")!,
            ["Runtime.FloorFixnumL"] = typeof(Runtime).GetMethod("FloorFixnumL")!,
            ["Runtime.TruncateFixnumL"] = typeof(Runtime).GetMethod("TruncateFixnumL")!,
            ["Runtime.FloorFixnumBoxed"] = typeof(Runtime).GetMethod("FloorFixnumBoxed")!,
            ["Runtime.TruncateFixnumBoxed"] = typeof(Runtime).GetMethod("TruncateFixnumBoxed")!,
            ["Runtime.RemFixnumL"] = typeof(Runtime).GetMethod("RemFixnumL")!,
            ["Runtime.WriteString1"] = typeof(Runtime).GetMethod("WriteString1")!,
            ["Runtime.WriteString2"] = typeof(Runtime).GetMethod("WriteString2")!,
            ["Runtime.WriteLine1"] = typeof(Runtime).GetMethod("WriteLine1")!,
            ["Runtime.WriteLine2"] = typeof(Runtime).GetMethod("WriteLine2")!,
            ["Runtime.Directory"] = typeof(Runtime).GetMethod("DirectoryFunc")!,
            ["Runtime.ProbeFile"] = typeof(Runtime).GetMethod("ProbeFile")!,
            ["Runtime.Truename"] = typeof(Runtime).GetMethod("Truename")!,

            ["Runtime.DeleteFile"] = typeof(Runtime).GetMethod("DeleteFile")!,
            ["Runtime.FileAuthor"] = typeof(Runtime).GetMethod("FileAuthor")!,
            ["Runtime.FileErrorPathname"] = typeof(Runtime).GetMethod("FileErrorPathname")!,
            ["Runtime.RenameFile"] = typeof(Runtime).GetMethod("RenameFile")!,
            ["Runtime.FileWriteDate"] = typeof(Runtime).GetMethod("FileWriteDate")!,

            // Runtime - string streams
            ["Runtime.MakeStringOutputStream"] = typeof(Runtime).GetMethod("MakeStringOutputStream")!,
            ["Runtime.MakeStringOutputStreamToString"] = typeof(Runtime).GetMethod("MakeStringOutputStreamToString")!,
            ["Runtime.GetOutputStreamString"] = typeof(Runtime).GetMethod("GetOutputStreamString")!,
            ["Runtime.MakeStringInputStream"] = typeof(Runtime).GetMethod("MakeStringInputStream", new[] { typeof(LispObject[]) })!,

            // Runtime - pathname
            ["Runtime.MakePathname"] = typeof(Runtime).GetMethod("MakePathname")!,
            ["Runtime.MakePathnameFromParts"] = typeof(Runtime).GetMethod("MakePathnameFromParts")!,
            ["Runtime.Pathname"] = typeof(Runtime).GetMethod("Pathname")!,
            ["Runtime.Namestring"] = typeof(Runtime).GetMethod("Namestring")!,
            ["Runtime.PathnameDirectory"] = typeof(Runtime).GetMethod("PathnameDirectory")!,
            ["Runtime.PathnameName"] = typeof(Runtime).GetMethod("PathnameName")!,
            ["Runtime.PathnameType"] = typeof(Runtime).GetMethod("PathnameType")!,
            ["Runtime.PathnameHost"] = typeof(Runtime).GetMethod("PathnameHost")!,
            ["Runtime.PathnameDevice"] = typeof(Runtime).GetMethod("PathnameDevice")!,
            ["Runtime.PathnameVersion"] = typeof(Runtime).GetMethod("PathnameVersion")!,
            ["Runtime.MergePathnames"] = typeof(Runtime).GetMethod("MergePathnames", new[] { typeof(LispObject), typeof(LispObject) })!,

            // Runtime - read
            ["Runtime.ReadFromStream"] = typeof(Runtime).GetMethod("ReadFromStream")!,
            ["Runtime.ReadFromString"] = typeof(Runtime).GetMethod("ReadFromString")!,

            // Runtime - load / eval
            ["Runtime.Load"] = typeof(Runtime).GetMethod("Load", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!,
            ["Runtime.Eval"] = typeof(Runtime).GetMethod("Eval")!,
            ["Runtime.TryEval"] = typeof(Runtime).GetMethod("TryEval")!,
            ["Runtime.Gensym"] = typeof(Runtime).GetMethod("Gensym")!,
            ["Runtime.Gensym0"] = typeof(Runtime).GetMethod("Gensym0")!,

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
                typeof(Startup).GetMethod("MakeHandlerCaseFunction")!,

            // System.Exception property
            ["System.Exception.get_Message"] =
                typeof(System.Exception).GetProperty("Message")!.GetGetMethod()!,

            // Wrap .NET exceptions as Lisp conditions
            ["Runtime.WrapDotNetException"] =
                typeof(Runtime).GetMethod("WrapDotNetException")!,
            ["Runtime.WrapDotNetExceptionObj"] =
                typeof(Runtime).GetMethod("WrapDotNetExceptionObj")!,
            ["Runtime.RewrapNonLispException"] =
                typeof(Runtime).GetMethod("RewrapNonLispException")!,
            ["Runtime.HandlerBindFilter"] =
                typeof(Runtime).GetMethod("HandlerBindFilter")!,
            ["Runtime.HandlerBindRethrow"] =
                typeof(Runtime).GetMethod("HandlerBindRethrow")!,
            ["Runtime.IsLispControlFlowException"] =
                typeof(Runtime).GetMethod("IsLispControlFlowException")!,
            ["Runtime.ThrowControlError"] =
                typeof(Runtime).GetMethod("ThrowControlError")!,
            // Builds the exception a THROW raises, or signals CONTROL-ERROR when
            // the tag has no outstanding CATCH.
            ["Runtime.MakeCatchThrow"] =
                typeof(Runtime).GetMethod("MakeCatchThrow")!,

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
            ["LispStruct.TypeNameOf"] = typeof(LispStruct).GetMethod("TypeNameOf")!,
            ["LispStruct.TypeNameOfStruct"] = typeof(LispStruct).GetMethod("TypeNameOfStruct")!,
            ["LispStruct.TypeNameOfKnown"] = typeof(LispStruct).GetMethod("TypeNameOfKnown")!,
            ["LispStruct.TypeNameOfRawInline"] = typeof(LispStruct).GetMethod("TypeNameOfRawInline")!,
            ["Symbol.get_StructParent"] =
                typeof(Symbol).GetProperty("StructParent")!.GetGetMethod()!,
            ["Decimal.op_Addition"] =
                typeof(decimal).GetMethod("op_Addition", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_Subtraction"] =
                typeof(decimal).GetMethod("op_Subtraction", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_Multiply"] =
                typeof(decimal).GetMethod("op_Multiply", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_Division"] =
                typeof(decimal).GetMethod("op_Division", new[] { typeof(decimal), typeof(decimal) })!,
            ["Decimal.op_UnaryNegation"] =
                typeof(decimal).GetMethod("op_UnaryNegation", new[] { typeof(decimal) })!,

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
                typeof(CatchThrowException).GetMethod("Get")!,

            // Exception-filter predicates
            ["ControlFlowFilters.CatchTagMatches"] =
                typeof(ControlFlowFilters).GetMethod("CatchTagMatches")!,
            // Two shapes, two names. A .sil compiled by an older dotcl names
            // HandlerCaseClause and pushes (tag, specs); rebinding that name to the
            // cluster form makes such a .sil assemble into an invalid program.
            ["ControlFlowFilters.HandlerCaseClause"] =
                typeof(ControlFlowFilters).GetMethod("HandlerCaseClause",
                    new[] { typeof(object), typeof(object), typeof(LispObject[]) })!,
            ["ControlFlowFilters.HandlerCaseClauseCluster"] =
                typeof(ControlFlowFilters).GetMethod("HandlerCaseClause",
                    new[] { typeof(object), typeof(HandlerBinding[]) })!,
            ["ControlFlowFilters.HandlerCaseCondition"] =
                typeof(ControlFlowFilters).GetMethod("HandlerCaseCondition")!,
            ["ControlFlowFilters.Transfer"] =
                typeof(ControlFlowFilters).GetMethod("Transfer")!,
            ["ControlFlowFilters.TakeTransfer"] =
                typeof(ControlFlowFilters).GetMethod("TakeTransfer")!,
            ["ControlFlowFilters.RestartCaseTag"] =
                typeof(ControlFlowFilters).GetMethod("RestartCaseTag")!,
            ["ControlFlowFilters.BlockTagMatches"] =
                typeof(ControlFlowFilters).GetMethod("BlockTagMatches")!,
            ["ControlFlowFilters.GoTagbodyMatches"] =
                typeof(ControlFlowFilters).GetMethod("GoTagbodyMatches")!,

            // CatchTagStack methods
            ["CatchTagStack.Push"] =
                typeof(CatchTagStack).GetMethod("Push")!,
            ["CatchTagStack.Pop"] =
                typeof(CatchTagStack).GetMethod("Pop")!,
            ["CatchTagStack.HasMatchingCatch"] =
                typeof(CatchTagStack).GetMethod("HasMatchingCatch")!,
            ["CatchTagStack.PushIndex"] =
                typeof(CatchTagStack).GetMethod("PushIndex")!,
            ["CatchTagStack.Enter"] = typeof(CatchTagStack).GetMethod("Enter")!,
            ["CatchTagStack.Leave"] = typeof(CatchTagStack).GetMethod("Leave")!,
            ["ThrowReturn.Claim"] = typeof(ThrowReturn).GetMethod("Claim")!,
            ["ThrowReturn.CanReturn"] = typeof(ThrowReturn).GetMethod("CanReturn")!,
            ["ThrowReturn.Return"] = typeof(ThrowReturn).GetMethod("Return")!,
            ["ThrowReturn.ClaimS"] = typeof(ThrowReturn).GetMethod("ClaimS")!,
            ["ThrowReturn.ClaimedIndex"] = typeof(ThrowReturn).GetMethod("ClaimedIndex")!,
            ["ThrowReturn.CanReturnS"] = typeof(ThrowReturn).GetMethod("CanReturnS")!,
            ["ThrowReturn.ReturnS"] = typeof(ThrowReturn).GetMethod("ReturnS")!,
            ["ThrowReturn.IsReturned"] = typeof(ThrowReturn).GetMethod("IsReturned")!,
            ["ThrowReturn.TakePending"] = typeof(ThrowReturn).GetMethod("TakePending")!,
            ["ThrowReturn.Forward0M"] = typeof(ThrowReturn).GetMethod("Forward0M")!,
            ["ThrowReturn.Forward1M"] = typeof(ThrowReturn).GetMethod("Forward1M")!,
            ["ThrowReturn.Forward2M"] = typeof(ThrowReturn).GetMethod("Forward2M")!,
            ["ThrowReturn.Forward3M"] = typeof(ThrowReturn).GetMethod("Forward3M")!,
            ["ThrowReturn.Forward4M"] = typeof(ThrowReturn).GetMethod("Forward4M")!,
            ["ThrowReturn.Forward0"] = typeof(ThrowReturn).GetMethod("Forward0")!,
            ["ThrowReturn.Forward1"] = typeof(ThrowReturn).GetMethod("Forward1")!,
            ["ThrowReturn.Forward2"] = typeof(ThrowReturn).GetMethod("Forward2")!,
            ["ThrowReturn.Forward3"] = typeof(ThrowReturn).GetMethod("Forward3")!,
            ["ThrowReturn.Forward4"] = typeof(ThrowReturn).GetMethod("Forward4")!,
            ["ThrowReturn.Call0"] = typeof(ThrowReturn).GetMethod("Call0")!,
            ["ThrowReturn.Call1"] = typeof(ThrowReturn).GetMethod("Call1")!,
            ["ThrowReturn.Call2"] = typeof(ThrowReturn).GetMethod("Call2")!,
            ["ThrowReturn.Call3"] = typeof(ThrowReturn).GetMethod("Call3")!,
            ["ThrowReturn.Call4"] = typeof(ThrowReturn).GetMethod("Call4")!,
            ["ThrowReturn.Call0S"] = typeof(ThrowReturn).GetMethod("Call0S")!,
            ["ThrowReturn.Call1S"] = typeof(ThrowReturn).GetMethod("Call1S")!,
            ["ThrowReturn.Call2S"] = typeof(ThrowReturn).GetMethod("Call2S")!,
            ["ThrowReturn.Call3S"] = typeof(ThrowReturn).GetMethod("Call3S")!,
            ["ThrowReturn.Call4S"] = typeof(ThrowReturn).GetMethod("Call4S")!,

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

        _getConstant = typeof(CilAssembler).GetMethod("GetConstant")!;
        _getConstantObject = typeof(CilAssembler).GetMethod("GetConstantObject")!;
        _getConstantSymbol = typeof(CilAssembler).GetMethod("GetConstantSymbol")!;
        _symFnSiteResolve = typeof(Startup.SymFnSite).GetMethod("Resolve")!;
        _fnSiteSymbol = typeof(CilAssembler).GetMethod("FnSiteSymbol")!;
        _symSiteResolve = typeof(Startup.SymSite).GetMethod("Resolve")!;
        _getUnitConstant = typeof(CilAssembler).GetMethod("GetUnitConstant")!;
        _makeFaslInstance = typeof(Runtime).GetMethod("MakeFaslInstance",
            new[] { typeof(string), typeof(string), typeof(LispObject[]) })!;
        _internViaEvalInstance = typeof(LispInstance).GetMethod("InternViaEval",
            new[] { typeof(string), typeof(LispObject) })!;
        _makeClosure = typeof(CilAssembler).GetMethod("MakeClosure")!;
        _makeClosureDirect = typeof(CilAssembler).GetMethod("MakeClosureDirect")!;
        _checkArityExact = typeof(Runtime).GetMethod("CheckArityExact",
            new[] { typeof(string), typeof(LispObject[]), typeof(int) })!;
        _registerFunction = typeof(CilAssembler).GetMethod("RegisterFunction")!;
        _registerFunctionOnSymbol = typeof(CilAssembler).GetMethod("RegisterFunctionOnSymbol")!;
        _registerSetfFunctionOnSymbol = typeof(CilAssembler).GetMethod("RegisterSetfFunctionOnSymbol")!;
        foreach (var (name, m) in SimdLower.Methods()) _methodCache[name] = m;
        foreach (var t in SimdLower.LaneTypes)
            _typeCache["V128." + t.Name] = typeof(System.Runtime.Intrinsics.Vector128<>).MakeGenericType(t);
        if (CallCoverage.Enabled) CallCoverage.Init(_methodCache.Keys);
    }
}
