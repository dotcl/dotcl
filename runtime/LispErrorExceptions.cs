namespace DotCL;

// The exceptions a library built with dotnet:library throws for the common
// kinds of Lisp error. Each one is a LispErrorException, so an existing
// catch (LispErrorException) still catches it, and each keeps the condition
// (Condition) the way the base type does. They exist so that C# can catch by
// type: catch (LispDivisionByZeroException) instead of reading the type of
// Condition. The condition-to-exception table is in LibraryAbi.ExceptionFor.

/// <summary>A Lisp TYPE-ERROR.</summary>
public class LispTypeErrorException : LispErrorException
{
    public LispTypeErrorException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp ARITHMETIC-ERROR that is not one of the more specific kinds below.</summary>
public class LispArithmeticErrorException : LispErrorException
{
    public LispArithmeticErrorException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp DIVISION-BY-ZERO.</summary>
public class LispDivisionByZeroException : LispArithmeticErrorException
{
    public LispDivisionByZeroException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp CELL-ERROR that is not one of the more specific kinds below.</summary>
public class LispCellErrorException : LispErrorException
{
    public LispCellErrorException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp UNBOUND-VARIABLE.</summary>
public class LispUnboundVariableException : LispCellErrorException
{
    public LispUnboundVariableException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp UNDEFINED-FUNCTION.</summary>
public class LispUndefinedFunctionException : LispCellErrorException
{
    public LispUndefinedFunctionException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp CONTROL-ERROR (for example a THROW to a tag nobody catches).</summary>
public class LispControlErrorException : LispErrorException
{
    public LispControlErrorException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp PROGRAM-ERROR (for example a call with the wrong number of arguments).</summary>
public class LispProgramErrorException : LispErrorException
{
    public LispProgramErrorException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp STREAM-ERROR that is not an END-OF-FILE.</summary>
public class LispStreamErrorException : LispErrorException
{
    public LispStreamErrorException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp END-OF-FILE.</summary>
public class LispEndOfFileException : LispStreamErrorException
{
    public LispEndOfFileException(LispCondition condition) : base(condition) { }
}

/// <summary>A Lisp FILE-ERROR.</summary>
public class LispFileErrorException : LispErrorException
{
    public LispFileErrorException(LispCondition condition) : base(condition) { }
}
