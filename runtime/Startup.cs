using System.Linq;
using System.Numerics;

namespace DotCL;

public static class Startup
{
    /// <summary>Look up a macro expander in the compiler's *macros* hash table by symbol.
    /// Returns the LispFunction expander if found, null otherwise.</summary>
    internal static LispFunction? LookupCompilerMacro(Symbol sym)
    {
        // *macros* ends up in DOTCL-INTERNAL (via Startup.Sym during cross-compile)
        foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
        {
            var pkg = Package.FindPackage(pkgName);
            if (pkg != null)
            {
                var (macrosSym, status) = pkg.FindSymbol("*MACROS*");
                if (macrosSym != null && status != SymbolStatus.None
                    && macrosSym.Value is LispHashTable macrosTable)
                {
                    // *macros* uses symbol keys (eq test) — look up directly by symbol identity
                    if (macrosTable.TryGet(sym, out var expander) && expander is LispFunction fn)
                        return fn;
                }
            }
        }
        return null;
    }

    /// <summary>Look up a macro expander by name string (convenience overload).
    /// Searches CL package first, then current package.</summary>
    internal static LispFunction? LookupCompilerMacroByName(string name)
    {
        // Try CL symbol first (most macros are CL)
        var clPkg = Package.FindPackage("COMMON-LISP") ?? Package.FindPackage("CL");
        if (clPkg != null)
        {
            var (sym, symStatus) = clPkg.FindSymbol(name);
            if (sym != null && symStatus != SymbolStatus.None)
            {
                var result = LookupCompilerMacro(sym);
                if (result != null) return result;
            }
        }
        // Try current package
        var curPkg = DynamicBindings.Get(Startup.Sym("*PACKAGE*")) as Package;
        if (curPkg != null && curPkg != clPkg)
        {
            var (sym, symStatus) = curPkg.FindSymbol(name);
            if (sym != null && symStatus != SymbolStatus.None)
            {
                var result = LookupCompilerMacro(sym);
                if (result != null) return result;
            }
        }
        return null;
    }

    /// <summary>Return SingleFloat when original is not DoubleFloat, DoubleFloat otherwise.
    /// CL spec: trig/math functions preserve float type; integer/ratio/single-float → single-float result.</summary>
    internal static LispObject MakeFloat(double value, LispObject original)
    {
        if (original is DoubleFloat) return new DoubleFloat(value);
        return new SingleFloat((float)value);
    }

    public static Package CL = null!;
    public static Package CLUser = null!;
    public static Package KeywordPkg = null!;
    public static Package Internal = null!;
    public static Package DotclPkg = null!;
    public static Package DotNetPkg = null!;

    /// <summary>True when dotcl:*debug-stacktrace* is non-NIL.</summary>
    public static bool DebugStacktrace =>
        DotclPkg?.FindSymbol("*DEBUG-STACKTRACE*") is var (sym, _) && sym?.Value is not Nil and not null;

    // Well-known symbols
    public static Symbol NIL_SYM = null!;
    public static Symbol T_SYM = null!;
    public static Symbol QUOTE = null!;
    public static Symbol FUNCTION = null!;
    public static Symbol LAMBDA = null!;
    public static Symbol IF = null!;
    public static Symbol LET = null!;
    public static Symbol PROGN = null!;
    public static Symbol SETQ = null!;
    public static Symbol BLOCK = null!;
    public static Symbol RETURN_FROM = null!;
    public static Symbol TAGBODY = null!;
    public static Symbol GO = null!;
    public static Symbol UNWIND_PROTECT = null!;
    public static Symbol DEFUN = null!;
    public static Symbol DEFVAR = null!;
    public static Symbol DEFPARAMETER = null!;
    public static Symbol DEFCONSTANT = null!;
    public static Symbol DECLARE = null!;
    public static Symbol SPECIAL = null!;

    // Backquote
    public static Symbol QUASIQUOTE = null!;
    public static Symbol UNQUOTE = null!;
    public static Symbol UNQUOTE_SPLICING = null!;
    public static Symbol UNQUOTE_NSPLICING = null!;
    public static LispReadtable StandardReadtable = null!;

    // Standard streams
    public static LispInputStream StandardInput = null!;
    public static LispOutputStream StandardOutput = null!;
    public static LispOutputStream ErrorOutput = null!;

    // Features list
    private static readonly HashSet<string> _features = new()
    {
        "DOTCL", "COMMON-LISP", "NET", "UNICODE", "PACKAGE-LOCAL-NICKNAMES"
    };

    private static bool _initialized;

    public static void Initialize()
    {
        if (_initialized) return;
        _initialized = true;

        // Create packages
        CL = new Package("COMMON-LISP", "CL");
        CLUser = new Package("COMMON-LISP-USER", "CL-USER");
        KeywordPkg = new Package("KEYWORD");

        Internal = new Package("DOTCL-INTERNAL");
        new Package("DOTCL.CIL-COMPILER");  // for compiler-generated labels/locals
        DotclPkg = new Package("DOTCL");
        DotNetPkg = new Package("DOTNET");
        var dotnetPkg = DotNetPkg;
        CLUser.UsePackage(CL);
        // DOTNET package is not use-package'd into CL-USER so dotnet: prefix
        // is always required. This avoids conflicts with CL symbols (e.g. REQUIRE).

        // Intern and export core symbols
        NIL_SYM = InternExport("NIL");
        NIL_SYM.Value = Nil.Instance;
        NIL_SYM.IsConstant = true;

        T_SYM = InternExport("T");
        T_SYM.Value = T.Instance;
        T_SYM.IsConstant = true;

        QUOTE = InternExport("QUOTE");
        FUNCTION = InternExport("FUNCTION");
        LAMBDA = InternExport("LAMBDA");
        IF = InternExport("IF");
        LET = InternExport("LET");
        PROGN = InternExport("PROGN");
        SETQ = InternExport("SETQ");
        BLOCK = InternExport("BLOCK");
        RETURN_FROM = InternExport("RETURN-FROM");
        TAGBODY = InternExport("TAGBODY");
        GO = InternExport("GO");
        UNWIND_PROTECT = InternExport("UNWIND-PROTECT");
        DEFUN = InternExport("DEFUN");
        DEFVAR = InternExport("DEFVAR");
        DEFPARAMETER = InternExport("DEFPARAMETER");
        DEFCONSTANT = InternExport("DEFCONSTANT");
        DECLARE = InternExport("DECLARE");
        SPECIAL = InternExport("SPECIAL");

        // Backquote symbols (internal to DOTCL, not exported from CL)
        QUASIQUOTE = CL.Intern("QUASIQUOTE").symbol;
        UNQUOTE = CL.Intern("UNQUOTE").symbol;
        UNQUOTE_SPLICING = CL.Intern("UNQUOTE-SPLICING").symbol;
        UNQUOTE_NSPLICING = CL.Intern("UNQUOTE-NSPLICING").symbol;

        // Type names
        foreach (var name in new[] {
            "NUMBER", "INTEGER", "FIXNUM", "BIGNUM", "RATIO",
            "FLOAT", "SINGLE-FLOAT", "DOUBLE-FLOAT", "COMPLEX",
            "CHARACTER", "BASE-CHAR", "STRING", "SYMBOL", "CONS", "LIST",
            "VECTOR", "ARRAY", "HASH-TABLE", "FUNCTION",
            "PACKAGE", "STREAM", "CONDITION", "NULL"
        })
        {
            InternExport(name);
        }

        // Core function names
        foreach (var name in new[] {
            "+", "-", "*", "/", "=", "/=", "<", ">", "<=", ">=",
            "CAR", "CDR", "CONS", "LIST", "LIST*", "APPEND",
            "EQ", "EQL", "EQUAL", "EQUALP",
            "NOT", "NULL", "ATOM", "CONSP", "LISTP", "NUMBERP",
            "SYMBOLP", "STRINGP", "CHARACTERP", "FUNCTIONP",
            "INTEGERP", "RATIONALP", "FLOATP", "COMPLEXP", "VECTORP",
            "TYPEP", "SUBTYPEP", "TYPE-OF", "AREF",
            "PRINT", "PRIN1", "PRINC", "TERPRI", "FORMAT",
            "WRITE-TO-STRING", "PRIN1-TO-STRING", "PRINC-TO-STRING",
            "READ", "READ-FROM-STRING",
            "INTERN", "FIND-SYMBOL", "MAKE-PACKAGE",
            "SYMBOL-NAME", "SYMBOL-VALUE", "SYMBOL-FUNCTION", "SYMBOL-PACKAGE",
            "MAKE-HASH-TABLE", "GETHASH", "REMHASH", "MAPHASH", "GETF",
            "MAKE-ARRAY", "AREF", "LENGTH", "ARRAY-ELEMENT-TYPE", "FMAKUNBOUND", "MAKUNBOUND",
            "VALUES", "MULTIPLE-VALUE-BIND", "MULTIPLE-VALUE-LIST",
            "ERROR", "WARN", "SIGNAL", "HANDLER-CASE", "HANDLER-BIND",
            "APPLY", "FUNCALL", "MAPCAR",
            "ABS", "MOD", "REM", "FLOOR", "CEILING", "TRUNCATE", "ROUND",
            "MIN", "MAX",
            "LOGIOR", "LOGAND", "LOGXOR", "LOGNOT", "ASH", "LOGBITP",
            "CHAR-CODE", "CODE-CHAR", "CHAR-UPCASE", "CHAR-DOWNCASE", "DIGIT-CHAR-P",
            "STRING-UPCASE", "STRING-DOWNCASE", "STRING=", "STRING<",
            "CONCATENATE", "SUBSEQ", "PARSE-INTEGER", "PROVIDE", "MAKE-STRING", "REPLACE",
            "WRITE-CHAR", "WRITE-STRING", "WRITE-LINE", "READ-CHAR", "PEEK-CHAR",
            "UNREAD-CHAR", "READ-LINE",
            "OPEN", "CLOSE", "WITH-OPEN-FILE",
            "WITH-OUTPUT-TO-STRING", "WITH-INPUT-FROM-STRING",
            "MAKE-STRING-OUTPUT-STREAM", "MAKE-STRING-INPUT-STREAM",
            "GET-OUTPUT-STREAM-STRING",
            // Special forms
            "QUOTE", "IF", "LET", "LET*", "PROGN", "SETQ",
            "LAMBDA", "FUNCTION", "BLOCK", "RETURN-FROM",
            "TAGBODY", "GO", "UNWIND-PROTECT", "EVAL-WHEN",
            "FLET", "LABELS", "MACROLET", "LOCALLY",
            "THE", "MULTIPLE-VALUE-CALL", "MULTIPLE-VALUE-PROG1",
            "CATCH", "THROW", "LOAD-TIME-VALUE",
            // Standard macros
            "DEFMACRO", "DEFSTRUCT", "DEFCLASS", "DEFGENERIC", "DEFMETHOD",
            "DEFTYPE", "DEFINE-CONDITION",
            "COND", "WHEN", "UNLESS", "AND", "OR", "CASE", "ECASE", "OTHERWISE",
            "DO", "DO*", "DOLIST", "DOTIMES", "LOOP",
            "PUSH", "POP", "PUSHNEW", "INCF", "DECF",
            "WITH-OPEN-FILE", "WITH-OUTPUT-TO-STRING", "WITH-INPUT-FROM-STRING",
            "MULTIPLE-VALUE-SETQ", "NTH-VALUE",
            "RETURN", "PROG", "PROG1", "PROG2",
            "SETF", "DEFSETF", "ROTATEF", "SHIFTF",
            "DESTRUCTURING-BIND", "IGNORE-ERRORS",
            "IN-PACKAGE", "DEFPACKAGE", "DOCUMENTATION",
            // Core functions
            "EVAL", "LOAD", "COMPILE", "COMPILE-FILE", "COMPILE-FILE-PATHNAME", "FDEFINITION",
            "FIND-PACKAGE", "PACKAGE-NAME",
            "EXPORT", "IMPORT", "SHADOW", "USE-PACKAGE",
            "MAKE-PATHNAME", "PATHNAME", "NAMESTRING",
            "PATHNAME-DIRECTORY", "PATHNAME-NAME", "PATHNAME-TYPE",
            "PATHNAME-HOST", "PATHNAME-DEVICE", "PATHNAME-VERSION",
            "MERGE-PATHNAMES", "PARSE-NAMESTRING", "PATHNAME-MATCH-P", "TRANSLATE-PATHNAME",
            "PROBE-FILE", "TRUENAME", "FILE-WRITE-DATE",
            "ENSURE-DIRECTORIES-EXIST", "DELETE-FILE", "RENAME-FILE",
            "USER-HOMEDIR-PATHNAME",
            "STREAMP", "INPUT-STREAM-P", "OUTPUT-STREAM-P",
            "FRESH-LINE", "FORCE-OUTPUT", "FINISH-OUTPUT",
            "STRING-TRIM", "STRING-LEFT-TRIM", "STRING-RIGHT-TRIM",
            "CHAR", "CHAR=", "CHAR<", "CHAR>", "CHAR<=", "CHAR>=",
            "UPPER-CASE-P", "LOWER-CASE-P", "ALPHA-CHAR-P", "DIGIT-CHAR-P",
            "ALPHANUMERICP", "GRAPHIC-CHAR-P",
            "CHAR-UPCASE", "CHAR-DOWNCASE",
            // List accessors
            "CADR", "CDDR", "CAAR", "CDAR", "CADDR", "CDDDR", "CADDDR",
            "FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH",
            "SIXTH", "SEVENTH", "EIGHTH", "NINTH", "TENTH",
            "REST", "ENDP", "LAST", "BUTLAST", "COPY-LIST", "COPY-SEQ", "NTH", "NTHCDR",
            "NREVERSE", "REVERSE", "RPLACA", "RPLACD",
            "NCONC", "COPY-TREE", "SUBST", "ACONS", "PAIRLIS",
            // Sequence operations
            "FIND", "FIND-IF", "FIND-IF-NOT",
            "REMOVE", "REMOVE-IF", "REMOVE-IF-NOT",
            "COUNT", "COUNT-IF", "COUNT-IF-NOT",
            "POSITION", "POSITION-IF",
            "MEMBER", "ASSOC", "RASSOC",
            "UNION", "INTERSECTION", "SET-DIFFERENCE", "SUBSETP",
            "EVERY", "SOME", "NOTEVERY", "NOTANY",
            "REDUCE", "MAP", "MAPC", "MAPCAN",
            "SORT", "STABLE-SORT", "COERCE", "ELT", "SUBSEQ", "CONCATENATE", "SEARCH",
            // Numeric extras
            "ZEROP", "PLUSP", "MINUSP", "EVENP", "ODDP",
            "1+", "1-", "ABS", "SIGNUM", "SQRT",
            "EXPT", "LOG", "EXP", "SIN", "COS", "TAN",
            "FLOAT", "RATIONAL", "NUMERATOR", "DENOMINATOR",
            "RANDOM", "MAKE-RANDOM-STATE",
            // Higher-order / misc
            "FUNCALL", "APPLY", "IDENTITY", "COMPLEMENT", "CONSTANTLY",
            "MAPCAR", "MAPHASH",
            "NOT", "ATOM",
            "GENSYM", "GENTEMP",
            "MAKE-HASH-TABLE", "GETHASH", "REMHASH",
            "HASH-TABLE-P", "PACKAGEP", "PATHNAMEP", "WILD-PATHNAME-P",
            "LISP-IMPLEMENTATION-TYPE", "LISP-IMPLEMENTATION-VERSION",
            "SUBSTITUTE", "SUBSTITUTE-IF",
            "MAKE-STRING-OUTPUT-STREAM", "MAKE-STRING-INPUT-STREAM",
            "GET-OUTPUT-STREAM-STRING",
            "SIGNAL", "WARN",
            "HANDLER-CASE", "HANDLER-BIND", "RESTART-CASE",
            "INVOKE-RESTART", "FIND-RESTART", "COMPUTE-RESTARTS",
            "STORE-VALUE", "USE-VALUE", "CONTINUE", "ABORT", "MUFFLE-WARNING",
            "FIND-CLASS", "CLASS-OF", "CLASS-NAME",
            "SLOT-VALUE", "SLOT-BOUNDP", "MAKE-INSTANCE", "REINITIALIZE-INSTANCE",
            "CALL-NEXT-METHOD", "NEXT-METHOD-P",
            "STRING", "COPY-SEQ",
            // Property list
            "GET", "REMPROP", "SYMBOL-PLIST", "COPY-SYMBOL", "GETF",
            "DO-SYMBOLS", "DO-EXTERNAL-SYMBOLS", "DO-ALL-SYMBOLS",
            "UNEXPORT", "UNINTERN", "SHADOW", "SHADOWING-IMPORT",
            "RENAME-PACKAGE", "DELETE-PACKAGE",
            "PACKAGE-USE-LIST", "PACKAGE-USED-BY-LIST",
            "PACKAGE-SHADOWING-SYMBOLS", "PACKAGE-NICKNAMES",
            "CHECK-TYPE", "ASSERT", "CERROR",
            "WITH-STANDARD-IO-SYNTAX", "WITH-COMPILATION-UNIT",
            "SATISFIES", "BOUNDP", "FBOUNDP", "FMAKUNBOUND", "MAKUNBOUND",
            "MULTIPLE-VALUE-PROG1", "SYMBOL-MACROLET",
            // Lambda-list keywords
            "&OPTIONAL", "&REST", "&BODY", "&KEY",
            "&ALLOW-OTHER-KEYS", "&AUX", "&WHOLE", "&ENVIRONMENT"
        })
        {
            InternExport(name);
        }

        // Standard special variables
        var starPackage = InternExport("*PACKAGE*");
        starPackage.IsSpecial = true;
        starPackage.Value = CLUser;

        // Console.In は Android (no-console host) では PlatformNotSupportedException
        // を投げるので、fallback として StringReader/StringWriter で代用する。
        // Console.Out / Console.Error も同様に守って一貫させる (両方一緒に
        // 失敗する想定のホストもあるため)。
        TextReader hostStdin;
        TextWriter hostStdout;
        TextWriter hostStderr;
        try { hostStdin = Console.In; }
        catch (PlatformNotSupportedException) { hostStdin = TextReader.Null; }
        try { hostStdout = Console.Out; }
        catch (PlatformNotSupportedException) { hostStdout = TextWriter.Null; }
        try { hostStderr = Console.Error; }
        catch (PlatformNotSupportedException) { hostStderr = TextWriter.Null; }

        var starStdin = InternExport("*STANDARD-INPUT*");
        starStdin.IsSpecial = true;
        StandardInput = new LispInputStream(hostStdin);
        starStdin.Value = StandardInput;

        var starStdout = InternExport("*STANDARD-OUTPUT*");
        starStdout.IsSpecial = true;
        StandardOutput = new LispOutputStream(hostStdout);
        starStdout.Value = StandardOutput;

        var starStderr = InternExport("*ERROR-OUTPUT*");
        starStderr.IsSpecial = true;
        ErrorOutput = new LispOutputStream(hostStderr);
        starStderr.Value = ErrorOutput;

        // Bidirectional I/O streams
        var termIO = new LispBidirectionalStream(hostStdin, hostStdout);
        foreach (var name in new[] { "*DEBUG-IO*", "*QUERY-IO*", "*TERMINAL-IO*" })
        {
            var sym = InternExport(name);
            sym.IsSpecial = true;
            sym.Value = termIO;
        }
        var traceOut = InternExport("*TRACE-OUTPUT*");
        traceOut.IsSpecial = true;
        traceOut.Value = StandardOutput;

        // Platform-specific features (must be added before building *features* list)
        if (OperatingSystem.IsWindows())
        {
            _features.Add("WINDOWS");
            _features.Add("WIN32");   // SBCL/CMUCL compat alias used by many libraries
        }
        if (OperatingSystem.IsLinux())
        {
            _features.Add("LINUX");
            _features.Add("UNIX");
        }
        if (OperatingSystem.IsMacOS())
        {
            _features.Add("MACOS");
            _features.Add("DARWIN"); // macOS kernel name, used by cffi/babel etc.
            _features.Add("UNIX");
            _features.Add("BSD");    // macOS derives from BSD
        }

        if (System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture ==
            System.Runtime.InteropServices.Architecture.Arm64)
            _features.Add("ARM64");
        else
            _features.Add("X86-64");

        if (System.BitConverter.IsLittleEndian)
            _features.Add("LITTLE-ENDIAN");
        else
            _features.Add("BIG-ENDIAN");

        // Pointer size features used by cffi and other libraries
        if (System.IntPtr.Size == 8)
            _features.Add("64-BIT");
        else
            _features.Add("32-BIT");

        var starFeatures = InternExport("*FEATURES*");
        starFeatures.IsSpecial = true;
        LispObject featuresList = Nil.Instance;
        foreach (var f in _features)
            featuresList = new Cons(Keyword(f), featuresList);
        starFeatures.Value = featuresList;

        // Load-related special variables
        var loadPathname = InternExport("*LOAD-PATHNAME*");
        loadPathname.IsSpecial = true;
        loadPathname.Value = Nil.Instance;

        var loadTruename = InternExport("*LOAD-TRUENAME*");
        loadTruename.IsSpecial = true;
        loadTruename.Value = Nil.Instance;

        var loadVerbose = InternExport("*LOAD-VERBOSE*");
        loadVerbose.IsSpecial = true;
        loadVerbose.Value = Nil.Instance;

        var compileFilePathname = InternExport("*COMPILE-FILE-PATHNAME*");
        compileFilePathname.IsSpecial = true;
        compileFilePathname.Value = Nil.Instance;

        var compileFileTruename = InternExport("*COMPILE-FILE-TRUENAME*");
        compileFileTruename.IsSpecial = true;
        compileFileTruename.Value = Nil.Instance;

        var loadPrint = InternExport("*LOAD-PRINT*");
        loadPrint.IsSpecial = true;
        loadPrint.Value = Nil.Instance;

        // Print pprint dispatch — use a proper table object
        var printPprintDispatch = InternExport("*PRINT-PPRINT-DISPATCH*");
        printPprintDispatch.IsSpecial = true;
        printPprintDispatch.Value = new LispPprintDispatchTable();

        // Default pathname defaults
        var defaultPathDefaults = InternExport("*DEFAULT-PATHNAME-DEFAULTS*");
        defaultPathDefaults.IsSpecial = true;
        defaultPathDefaults.Value = LispPathname.FromString(Directory.GetCurrentDirectory() + "/");

        // Readtable and read-related
        var readtable = InternExport("*READTABLE*");
        readtable.IsSpecial = true;
        StandardReadtable = LispReadtable.CreateStandard();
        Reader.RegisterStandardMacros(StandardReadtable);
        readtable.Value = StandardReadtable.Clone(); // current readtable is a copy of standard

        var readBase = InternExport("*READ-BASE*");
        readBase.IsSpecial = true;
        readBase.Value = Fixnum.Make(10);

        var readDefaultFloat = InternExport("*READ-DEFAULT-FLOAT-FORMAT*");
        readDefaultFloat.IsSpecial = true;
        readDefaultFloat.Value = Sym("SINGLE-FLOAT");

        var readSuppress = InternExport("*READ-SUPPRESS*");
        readSuppress.IsSpecial = true;
        readSuppress.Value = Nil.Instance;

        // Print variables
        var printEscape = InternExport("*PRINT-ESCAPE*");
        printEscape.IsSpecial = true;
        printEscape.Value = T.Instance;

        // Modules
        var starModules = InternExport("*MODULES*");
        starModules.IsSpecial = true;
        starModules.Value = Nil.Instance;

        // *module-provider-functions* — SBCL-compatible hook list for REQUIRE (not standard CL)
        var (mpfSym, _) = CL.Intern("*MODULE-PROVIDER-FUNCTIONS*");
        mpfSym.IsSpecial = true;
        var (contribProviderSym, _) = CL.Intern("MODULE-PROVIDE-CONTRIB");
        Emitter.CilAssembler.RegisterFunction("MODULE-PROVIDE-CONTRIB",
            new LispFunction(Runtime.ModuleProvideContrib, "MODULE-PROVIDE-CONTRIB"));
        contribProviderSym.Function = (LispFunction)Emitter.CilAssembler.GetFunction("MODULE-PROVIDE-CONTRIB");
        mpfSym.Value = new Cons(contribProviderSym, Nil.Instance);

        // CL constants
        var mpf = InternExport("MOST-POSITIVE-FIXNUM");
        mpf.Value = Fixnum.Make(long.MaxValue);
        mpf.IsConstant = true;
        var mnf = InternExport("MOST-NEGATIVE-FIXNUM");
        mnf.Value = Fixnum.Make(long.MinValue);
        mnf.IsConstant = true;

        var cal = InternExport("CALL-ARGUMENTS-LIMIT");
        cal.Value = Fixnum.Make(int.MaxValue);
        cal.IsConstant = true;
        var lpl = InternExport("LAMBDA-PARAMETERS-LIMIT");
        lpl.Value = Fixnum.Make(int.MaxValue);
        lpl.IsConstant = true;
        var mvl = InternExport("MULTIPLE-VALUES-LIMIT");
        mvl.Value = Fixnum.Make(1024);
        mvl.IsConstant = true;
        var ccl = InternExport("CHAR-CODE-LIMIT");
        ccl.Value = Fixnum.Make(65536);
        ccl.IsConstant = true;
        var arl = InternExport("ARRAY-RANK-LIMIT");
        arl.Value = Fixnum.Make(32);  // .NET System.Array supports up to 32 dimensions
        arl.IsConstant = true;
        var adl = InternExport("ARRAY-DIMENSION-LIMIT");
        adl.Value = Fixnum.Make(int.MaxValue);
        adl.IsConstant = true;
        var ats = InternExport("ARRAY-TOTAL-SIZE-LIMIT");
        ats.Value = Fixnum.Make(int.MaxValue);
        ats.IsConstant = true;
        // LAMBDA-LIST-KEYWORDS: standard CL variable constant
        var llk = InternExport("LAMBDA-LIST-KEYWORDS");
        llk.Value = Runtime.List(new LispObject[] {
            Sym("&ALLOW-OTHER-KEYS"), Sym("&AUX"), Sym("&BODY"), Sym("&ENVIRONMENT"),
            Sym("&KEY"), Sym("&OPTIONAL"), Sym("&REST"), Sym("&WHOLE")});
        llk.IsConstant = true;

        // BOOLE operation constants
        void SetBooleConst(string name, int val) {
            var s = InternExport(name); s.Value = Fixnum.Make(val); s.IsConstant = true; }
        SetBooleConst("BOOLE-CLR", 0); SetBooleConst("BOOLE-SET", 1);
        SetBooleConst("BOOLE-1", 2); SetBooleConst("BOOLE-2", 3);
        SetBooleConst("BOOLE-C1", 4); SetBooleConst("BOOLE-C2", 5);
        SetBooleConst("BOOLE-AND", 6); SetBooleConst("BOOLE-IOR", 7);
        SetBooleConst("BOOLE-XOR", 8); SetBooleConst("BOOLE-EQV", 9);
        SetBooleConst("BOOLE-NAND", 10); SetBooleConst("BOOLE-NOR", 11);
        SetBooleConst("BOOLE-ANDC1", 12); SetBooleConst("BOOLE-ANDC2", 13);
        SetBooleConst("BOOLE-ORC1", 14); SetBooleConst("BOOLE-ORC2", 15);

        // PI constant
        var piSym = InternExport("PI"); piSym.Value = new DoubleFloat(Math.PI); piSym.IsConstant = true;

        // Float constants (single-float/short-float use SingleFloat, double-float/long-float use DoubleFloat)
        void SetSingleFloatConst(string name, float val) {
            var s = InternExport(name); s.Value = new SingleFloat(val); s.IsConstant = true; }
        void SetDoubleFloatConst(string name, double val) {
            var s = InternExport(name); s.Value = new DoubleFloat(val); s.IsConstant = true; }
        // CL spec: smallest positive float eps such that (+ 1.0 eps) /= 1.0
        // Use BitIncrement to find the exact next float above the rounding midpoint.
        float singleEps = MathF.BitIncrement(MathF.ScaleB(1.0f, -24));     // next float after 2^-24
        float singleNegEps = MathF.BitIncrement(MathF.ScaleB(1.0f, -25));  // next float after 2^-25
        double doubleEps = Math.BitIncrement(Math.ScaleB(1.0, -53));       // next float after 2^-53
        double doubleNegEps = Math.BitIncrement(Math.ScaleB(1.0, -54));    // next float after 2^-54
        SetSingleFloatConst("SINGLE-FLOAT-EPSILON", singleEps);
        SetSingleFloatConst("SINGLE-FLOAT-NEGATIVE-EPSILON", singleNegEps);
        SetSingleFloatConst("SHORT-FLOAT-EPSILON", singleEps);
        SetSingleFloatConst("SHORT-FLOAT-NEGATIVE-EPSILON", singleNegEps);
        SetDoubleFloatConst("DOUBLE-FLOAT-EPSILON", doubleEps);
        SetDoubleFloatConst("DOUBLE-FLOAT-NEGATIVE-EPSILON", doubleNegEps);
        SetDoubleFloatConst("LONG-FLOAT-EPSILON", doubleEps);
        SetDoubleFloatConst("LONG-FLOAT-NEGATIVE-EPSILON", doubleNegEps);
        SetSingleFloatConst("MOST-POSITIVE-SINGLE-FLOAT", float.MaxValue);
        SetSingleFloatConst("MOST-NEGATIVE-SINGLE-FLOAT", -float.MaxValue);
        SetSingleFloatConst("MOST-POSITIVE-SHORT-FLOAT", float.MaxValue);
        SetSingleFloatConst("MOST-NEGATIVE-SHORT-FLOAT", -float.MaxValue);
        SetDoubleFloatConst("MOST-POSITIVE-DOUBLE-FLOAT", double.MaxValue);
        SetDoubleFloatConst("MOST-NEGATIVE-DOUBLE-FLOAT", -double.MaxValue);
        SetDoubleFloatConst("MOST-POSITIVE-LONG-FLOAT", double.MaxValue);
        SetDoubleFloatConst("MOST-NEGATIVE-LONG-FLOAT", -double.MaxValue);
        SetSingleFloatConst("LEAST-POSITIVE-SINGLE-FLOAT", 1.4012985e-45f);  // smallest positive single
        SetSingleFloatConst("LEAST-NEGATIVE-SINGLE-FLOAT", -1.4012985e-45f);
        SetSingleFloatConst("LEAST-POSITIVE-SHORT-FLOAT", 1.4012985e-45f);
        SetSingleFloatConst("LEAST-NEGATIVE-SHORT-FLOAT", -1.4012985e-45f);
        SetDoubleFloatConst("LEAST-POSITIVE-DOUBLE-FLOAT", 5e-324);  // smallest positive double
        SetDoubleFloatConst("LEAST-NEGATIVE-DOUBLE-FLOAT", -5e-324);
        SetDoubleFloatConst("LEAST-POSITIVE-LONG-FLOAT", 5e-324);
        SetDoubleFloatConst("LEAST-NEGATIVE-LONG-FLOAT", -5e-324);
        SetSingleFloatConst("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", 1.1754944e-38f);  // 2^-126
        SetSingleFloatConst("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", -1.1754944e-38f);
        SetSingleFloatConst("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT", 1.1754944e-38f);
        SetSingleFloatConst("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT", -1.1754944e-38f);
        SetDoubleFloatConst("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", 2.2250738585072014e-308);  // 2^-1022
        SetDoubleFloatConst("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", -2.2250738585072014e-308);
        SetDoubleFloatConst("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", 2.2250738585072014e-308);
        SetDoubleFloatConst("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", -2.2250738585072014e-308);

        // Float bit-level access (needed by SBCL cross-compilation and nibbles library).
        // Intern in CL (so find-symbol "..." "COMMON-LISP" works) but do NOT export
        // (they are non-standard, exporting would fail NO-EXTRA-SYMBOLS ANSI test).
        // Import into CL-USER so unqualified access works.
        foreach (var floatBitSym in new[] {
            "SINGLE-FLOAT-BITS", "DOUBLE-FLOAT-BITS", "DOUBLE-FLOAT-HIGH-BITS",
            "DOUBLE-FLOAT-LOW-BITS", "MAKE-SINGLE-FLOAT", "MAKE-DOUBLE-FLOAT" })
        {
            var (fbSym, _) = CL.Intern(floatBitSym);
            CLUser.Import(fbSym);
        }
        RegisterUnary("SINGLE-FLOAT-BITS", a => {
            if (a is SingleFloat sf)
                return Fixnum.Make(BitConverter.SingleToInt32Bits(sf.Value));
            throw new LispErrorException(new LispTypeError("SINGLE-FLOAT-BITS: not a single-float", a));
        });
        RegisterUnary("DOUBLE-FLOAT-BITS", a => {
            if (a is DoubleFloat df)
                return Fixnum.Make(BitConverter.DoubleToInt64Bits(df.Value));
            throw new LispErrorException(new LispTypeError("DOUBLE-FLOAT-BITS: not a double-float", a));
        });
        RegisterUnary("DOUBLE-FLOAT-HIGH-BITS", a => {
            if (a is DoubleFloat df)
                return Fixnum.Make((int)(BitConverter.DoubleToInt64Bits(df.Value) >> 32));
            throw new LispErrorException(new LispTypeError("DOUBLE-FLOAT-HIGH-BITS: not a double-float", a));
        });
        RegisterUnary("DOUBLE-FLOAT-LOW-BITS", a => {
            if (a is DoubleFloat df)
                return Fixnum.Make((int)(BitConverter.DoubleToInt64Bits(df.Value) & 0xFFFFFFFFL));
            throw new LispErrorException(new LispTypeError("DOUBLE-FLOAT-LOW-BITS: not a double-float", a));
        });
        RegisterUnary("MAKE-SINGLE-FLOAT", a => {
            long bits = a is Fixnum f ? f.Value : throw new LispErrorException(new LispTypeError("MAKE-SINGLE-FLOAT: not an integer", a));
            return new SingleFloat(BitConverter.Int32BitsToSingle((int)bits));
        });
        RegisterUnary("MAKE-DOUBLE-FLOAT", a => {
            long bits = a is Fixnum f ? f.Value : throw new LispErrorException(new LispTypeError("MAKE-DOUBLE-FLOAT: not an integer", a));
            return new DoubleFloat(BitConverter.Int64BitsToDouble(bits));
        });

        // REPL variables: *, **, ***, +, ++, +++, /, //, ///
        foreach (var name in new[] { "*", "**", "***", "+", "++", "+++", "/", "//", "///" })
        {
            var vs = CL.FindSymbol(name).symbol ?? InternExport(name);
            if (!vs.IsSpecial) { vs.IsSpecial = true; vs.Value = Nil.Instance; }
        }
        // - is the current expression (treated as a variable)
        var minusSym = CL.FindSymbol("-").symbol ?? InternExport("-");
        if (!minusSym.IsSpecial) { minusSym.IsSpecial = true; minusSym.Value = Nil.Instance; }

        // Export all 978 standard CL symbols (so TEST-IF-NOT-IN-CL-PACKAGE passes)
        foreach (var name in new[] {
            "&ALLOW-OTHER-KEYS", "&AUX", "&BODY", "&ENVIRONMENT", "&KEY", "&OPTIONAL", "&REST", "&WHOLE",
            "*", "**", "***", "*BREAK-ON-SIGNALS*", "*COMPILE-FILE-PATHNAME*", "*COMPILE-FILE-TRUENAME*", "*COMPILE-PRINT*", "*COMPILE-VERBOSE*",
            "*DEBUG-IO*", "*DEBUGGER-HOOK*", "*DEFAULT-PATHNAME-DEFAULTS*", "*ERROR-OUTPUT*", "*FEATURES*", "*GENSYM-COUNTER*", "*LOAD-PATHNAME*", "*LOAD-PRINT*",
            "*LOAD-TRUENAME*", "*LOAD-VERBOSE*", "*MACROEXPAND-HOOK*", "*MODULES*", "*PACKAGE*", "*PRINT-ARRAY*", "*PRINT-BASE*", "*PRINT-CASE*",
            "*PRINT-CIRCLE*", "*PRINT-ESCAPE*", "*PRINT-GENSYM*", "*PRINT-LENGTH*", "*PRINT-LEVEL*", "*PRINT-LINES*", "*PRINT-MISER-WIDTH*", "*PRINT-PPRINT-DISPATCH*",
            "*PRINT-PRETTY*", "*PRINT-RADIX*", "*PRINT-READABLY*", "*PRINT-RIGHT-MARGIN*", "*QUERY-IO*", "*RANDOM-STATE*", "*READ-BASE*", "*READ-DEFAULT-FLOAT-FORMAT*",
            "*READ-EVAL*", "*READ-SUPPRESS*", "*READTABLE*", "*STANDARD-INPUT*", "*STANDARD-OUTPUT*", "*TERMINAL-IO*", "*TRACE-OUTPUT*", "+",
            "++", "+++", "-", "/", "//", "///", "/=", "1+",
            "1-", "<", "<=", "=", ">", ">=", "ABORT", "ABS",
            "ACONS", "ACOS", "ACOSH", "ADD-METHOD", "ADJOIN", "ADJUST-ARRAY", "ADJUSTABLE-ARRAY-P", "ALLOCATE-INSTANCE",
            "ALPHA-CHAR-P", "ALPHANUMERICP", "AND", "APPEND", "APPLY", "APROPOS", "APROPOS-LIST", "AREF",
            "ARITHMETIC-ERROR", "ARITHMETIC-ERROR-OPERANDS", "ARITHMETIC-ERROR-OPERATION", "ARRAY", "ARRAY-DIMENSION", "ARRAY-DIMENSION-LIMIT", "ARRAY-DIMENSIONS", "ARRAY-DISPLACEMENT",
            "ARRAY-ELEMENT-TYPE", "ARRAY-HAS-FILL-POINTER-P", "ARRAY-IN-BOUNDS-P", "ARRAY-RANK", "ARRAY-RANK-LIMIT", "ARRAY-ROW-MAJOR-INDEX", "ARRAY-TOTAL-SIZE", "ARRAY-TOTAL-SIZE-LIMIT",
            "ARRAYP", "ASH", "ASIN", "ASINH", "ASSERT", "ASSOC", "ASSOC-IF", "ASSOC-IF-NOT",
            "ATAN", "ATANH", "ATOM", "BASE-CHAR", "BASE-STRING", "BIGNUM", "BIT", "BIT-AND",
            "BIT-ANDC1", "BIT-ANDC2", "BIT-EQV", "BIT-IOR", "BIT-NAND", "BIT-NOR", "BIT-NOT", "BIT-ORC1",
            "BIT-ORC2", "BIT-VECTOR", "BIT-VECTOR-P", "BIT-XOR", "BLOCK", "BOOLE", "BOOLE-1", "BOOLE-2",
            "BOOLE-AND", "BOOLE-ANDC1", "BOOLE-ANDC2", "BOOLE-C1", "BOOLE-C2", "BOOLE-CLR", "BOOLE-EQV", "BOOLE-IOR",
            "BOOLE-NAND", "BOOLE-NOR", "BOOLE-ORC1", "BOOLE-ORC2", "BOOLE-SET", "BOOLE-XOR", "BOOLEAN", "BOTH-CASE-P",
            "BOUNDP", "BREAK", "BROADCAST-STREAM", "BROADCAST-STREAM-STREAMS", "BUILT-IN-CLASS", "BUTLAST", "BYTE", "BYTE-POSITION",
            "BYTE-SIZE", "CAAAAR", "CAAADR", "CAAAR", "CAADAR", "CAADDR", "CAADR", "CAAR",
            "CADAAR", "CADADR", "CADAR", "CADDAR", "CADDDR", "CADDR", "CADR", "CALL-ARGUMENTS-LIMIT",
            "CALL-METHOD", "CALL-NEXT-METHOD", "CAR", "CASE", "CATCH", "CCASE", "CDAAAR", "CDAADR",
            "CDAAR", "CDADAR", "CDADDR", "CDADR", "CDAR", "CDDAAR", "CDDADR", "CDDAR",
            "CDDDAR", "CDDDDR", "CDDDR", "CDDR", "CDR", "CEILING", "CELL-ERROR", "CELL-ERROR-NAME",
            "CERROR", "CHANGE-CLASS", "CHAR", "CHAR-CODE", "CHAR-CODE-LIMIT", "CHAR-DOWNCASE", "CHAR-EQUAL", "CHAR-GREATERP",
            "CHAR-INT", "CHAR-LESSP", "CHAR-NAME", "CHAR-NOT-EQUAL", "CHAR-NOT-GREATERP", "CHAR-NOT-LESSP", "CHAR-UPCASE", "CHAR/=",
            "CHAR<", "CHAR<=", "CHAR=", "CHAR>", "CHAR>=", "CHARACTER", "CHARACTERP", "CHECK-TYPE",
            "CIS", "CLASS", "CLASS-NAME", "CLASS-OF", "CLEAR-INPUT", "CLEAR-OUTPUT", "CLOSE", "CLRHASH",
            "CODE-CHAR", "COERCE", "COMPILATION-SPEED", "COMPILE", "COMPILE-FILE", "COMPILE-FILE-PATHNAME", "COMPILED-FUNCTION", "COMPILED-FUNCTION-P",
            "COMPILER-MACRO", "COMPILER-MACRO-FUNCTION", "COMPLEMENT", "COMPLEX", "COMPLEXP", "COMPUTE-APPLICABLE-METHODS", "COMPUTE-RESTARTS", "CONCATENATE",
            "CONCATENATED-STREAM", "CONCATENATED-STREAM-STREAMS", "COND", "CONDITION", "CONJUGATE", "CONS", "CONSP", "CONSTANTLY",
            "CONSTANTP", "CONTINUE", "CONTROL-ERROR", "COPY-ALIST", "COPY-LIST", "COPY-PPRINT-DISPATCH", "COPY-READTABLE", "COPY-SEQ",
            "COPY-STRUCTURE", "COPY-SYMBOL", "COPY-TREE", "COS", "COSH", "COUNT", "COUNT-IF", "COUNT-IF-NOT",
            "CTYPECASE", "DEBUG", "DECF", "DECLAIM", "DECLARATION", "DECLARE", "DECODE-FLOAT", "DECODE-UNIVERSAL-TIME",
            "DEFCLASS", "DEFCONSTANT", "DEFGENERIC", "DEFINE-COMPILER-MACRO", "DEFINE-CONDITION", "DEFINE-METHOD-COMBINATION", "DEFINE-MODIFY-MACRO", "DEFINE-SETF-EXPANDER",
            "DEFINE-SYMBOL-MACRO", "DEFMACRO", "DEFMETHOD", "DEFPACKAGE", "DEFPARAMETER", "DEFSETF", "DEFSTRUCT", "DEFTYPE",
            "DEFUN", "DEFVAR", "DELETE", "DELETE-DUPLICATES", "DELETE-FILE", "DELETE-IF", "DELETE-IF-NOT", "DELETE-PACKAGE",
            "DENOMINATOR", "DEPOSIT-FIELD", "DESCRIBE", "DESCRIBE-OBJECT", "DESTRUCTURING-BIND", "DIGIT-CHAR", "DIGIT-CHAR-P", "DIRECTORY",
            "DIRECTORY-NAMESTRING", "DISASSEMBLE", "DIVISION-BY-ZERO", "DO", "DO*", "DO-ALL-SYMBOLS", "DO-EXTERNAL-SYMBOLS", "DO-SYMBOLS",
            "DOCUMENTATION", "DOLIST", "DOTIMES", "DOUBLE-FLOAT", "DOUBLE-FLOAT-EPSILON", "DOUBLE-FLOAT-NEGATIVE-EPSILON", "DPB", "DRIBBLE",
            "DYNAMIC-EXTENT", "ECASE", "ECHO-STREAM", "ECHO-STREAM-INPUT-STREAM", "ECHO-STREAM-OUTPUT-STREAM", "ED", "EIGHTH", "ELT",
            "ENCODE-UNIVERSAL-TIME", "END-OF-FILE", "ENDP", "ENOUGH-NAMESTRING", "ENSURE-DIRECTORIES-EXIST", "ENSURE-GENERIC-FUNCTION", "EQ", "EQL",
            "EQUAL", "EQUALP", "ERROR", "ETYPECASE", "EVAL", "EVAL-WHEN", "EVENP", "EVERY",
            "EXP", "EXPORT", "EXPT", "EXTENDED-CHAR", "FBOUNDP", "FCEILING", "FDEFINITION", "FFLOOR",
            "FIFTH", "FILE-AUTHOR", "FILE-ERROR", "FILE-ERROR-PATHNAME", "FILE-LENGTH", "FILE-NAMESTRING", "FILE-POSITION", "FILE-STREAM",
            "FILE-STRING-LENGTH", "FILE-WRITE-DATE", "FILL", "FILL-POINTER", "FIND", "FIND-ALL-SYMBOLS", "FIND-CLASS", "FIND-IF",
            "FIND-IF-NOT", "FIND-METHOD", "FIND-PACKAGE", "FIND-RESTART", "FIND-SYMBOL", "FINISH-OUTPUT", "FIRST", "FIXNUM",
            "FLET", "FLOAT", "FLOAT-DIGITS", "FLOAT-PRECISION", "FLOAT-RADIX", "FLOAT-SIGN", "FLOATING-POINT-INEXACT", "FLOATING-POINT-INVALID-OPERATION",
            "FLOATING-POINT-OVERFLOW", "FLOATING-POINT-UNDERFLOW", "FLOATP", "FLOOR", "FMAKUNBOUND", "FORCE-OUTPUT", "FORMAT", "FORMATTER",
            "FOURTH", "FRESH-LINE", "FROUND", "FTRUNCATE", "FTYPE", "FUNCALL", "FUNCTION", "FUNCTION-KEYWORDS",
            "FUNCTION-LAMBDA-EXPRESSION", "FUNCTIONP", "GCD", "GENERIC-FUNCTION", "GENSYM", "GENTEMP", "GET", "GET-DECODED-TIME",
            "GET-DISPATCH-MACRO-CHARACTER", "GET-INTERNAL-REAL-TIME", "GET-INTERNAL-RUN-TIME", "GET-MACRO-CHARACTER", "GET-OUTPUT-STREAM-STRING", "GET-PROPERTIES", "GET-SETF-EXPANSION", "GET-UNIVERSAL-TIME",
            "GETF", "GETHASH", "GO", "GRAPHIC-CHAR-P", "HANDLER-BIND", "HANDLER-CASE", "HASH-TABLE", "HASH-TABLE-COUNT",
            "HASH-TABLE-P", "HASH-TABLE-REHASH-SIZE", "HASH-TABLE-REHASH-THRESHOLD", "HASH-TABLE-SIZE", "HASH-TABLE-TEST", "HOST-NAMESTRING", "IDENTITY", "IF",
            "IGNORABLE", "IGNORE", "IGNORE-ERRORS", "IMAGPART", "IMPORT", "IN-PACKAGE", "INCF", "INITIALIZE-INSTANCE",
            "INLINE", "INPUT-STREAM-P", "INSPECT", "INTEGER", "INTEGER-DECODE-FLOAT", "INTEGER-LENGTH", "INTEGERP", "INTERACTIVE-STREAM-P",
            "INTERN", "INTERNAL-TIME-UNITS-PER-SECOND", "INTERSECTION", "INVALID-METHOD-ERROR", "INVOKE-DEBUGGER", "INVOKE-RESTART", "INVOKE-RESTART-INTERACTIVELY", "ISQRT",
            "KEYWORD", "KEYWORDP", "LABELS", "LAMBDA", "LAMBDA-LIST-KEYWORDS", "LAMBDA-PARAMETERS-LIMIT", "LAST", "LCM",
            "LDB", "LDB-TEST", "LDIFF", "LEAST-NEGATIVE-DOUBLE-FLOAT", "LEAST-NEGATIVE-LONG-FLOAT", "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT", "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT", "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT",
            "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT", "LEAST-NEGATIVE-SHORT-FLOAT", "LEAST-NEGATIVE-SINGLE-FLOAT", "LEAST-POSITIVE-DOUBLE-FLOAT", "LEAST-POSITIVE-LONG-FLOAT", "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT", "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT", "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT",
            "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT", "LEAST-POSITIVE-SHORT-FLOAT", "LEAST-POSITIVE-SINGLE-FLOAT", "LENGTH", "LET", "LET*", "LISP-IMPLEMENTATION-TYPE", "LISP-IMPLEMENTATION-VERSION",
            "LIST", "LIST*", "LIST-ALL-PACKAGES", "LIST-LENGTH", "LISTEN", "LISTP", "LOAD", "LOAD-LOGICAL-PATHNAME-TRANSLATIONS",
            "LOAD-TIME-VALUE", "LOCALLY", "LOG", "LOGAND", "LOGANDC1", "LOGANDC2", "LOGBITP", "LOGCOUNT",
            "LOGEQV", "LOGICAL-PATHNAME", "LOGICAL-PATHNAME-TRANSLATIONS", "LOGIOR", "LOGNAND", "LOGNOR", "LOGNOT", "LOGORC1",
            "LOGORC2", "LOGTEST", "LOGXOR", "LONG-FLOAT", "LONG-FLOAT-EPSILON", "LONG-FLOAT-NEGATIVE-EPSILON", "LONG-SITE-NAME", "LOOP",
            "LOOP-FINISH", "LOWER-CASE-P", "MACHINE-INSTANCE", "MACHINE-TYPE", "MACHINE-VERSION", "MACRO-FUNCTION", "MACROEXPAND", "MACROEXPAND-1",
            "MACROLET", "MAKE-ARRAY", "MAKE-BROADCAST-STREAM", "MAKE-CONCATENATED-STREAM", "MAKE-CONDITION", "MAKE-DISPATCH-MACRO-CHARACTER", "MAKE-ECHO-STREAM", "MAKE-HASH-TABLE",
            "MAKE-INSTANCE", "MAKE-INSTANCES-OBSOLETE", "MAKE-LIST", "MAKE-LOAD-FORM", "MAKE-LOAD-FORM-SAVING-SLOTS", "MAKE-METHOD", "MAKE-PACKAGE", "MAKE-PATHNAME",
            "MAKE-RANDOM-STATE", "MAKE-SEQUENCE", "MAKE-STRING", "MAKE-STRING-INPUT-STREAM", "MAKE-STRING-OUTPUT-STREAM", "MAKE-SYMBOL", "MAKE-SYNONYM-STREAM", "MAKE-TWO-WAY-STREAM",
            "MAKUNBOUND", "MAP", "MAP-INTO", "MAPC", "MAPCAN", "MAPCAR", "MAPCON", "MAPHASH",
            "MAPL", "MAPLIST", "MASK-FIELD", "MAX", "MEMBER", "MEMBER-IF", "MEMBER-IF-NOT", "MERGE",
            "MERGE-PATHNAMES", "METHOD", "METHOD-COMBINATION", "METHOD-COMBINATION-ERROR", "METHOD-QUALIFIERS", "MIN", "MINUSP", "MISMATCH",
            "MOD", "MOST-NEGATIVE-DOUBLE-FLOAT", "MOST-NEGATIVE-FIXNUM", "MOST-NEGATIVE-LONG-FLOAT", "MOST-NEGATIVE-SHORT-FLOAT", "MOST-NEGATIVE-SINGLE-FLOAT", "MOST-POSITIVE-DOUBLE-FLOAT", "MOST-POSITIVE-FIXNUM",
            "MOST-POSITIVE-LONG-FLOAT", "MOST-POSITIVE-SHORT-FLOAT", "MOST-POSITIVE-SINGLE-FLOAT", "MUFFLE-WARNING", "MULTIPLE-VALUE-BIND", "MULTIPLE-VALUE-CALL", "MULTIPLE-VALUE-LIST", "MULTIPLE-VALUE-PROG1",
            "MULTIPLE-VALUE-SETQ", "MULTIPLE-VALUES-LIMIT", "NAME-CHAR", "NAMESTRING", "NBUTLAST", "NCONC", "NEXT-METHOD-P", "NIL",
            "NINTERSECTION", "NINTH", "NO-APPLICABLE-METHOD", "NO-NEXT-METHOD", "NOT", "NOTANY", "NOTEVERY", "NOTINLINE",
            "NRECONC", "NREVERSE", "NSET-DIFFERENCE", "NSET-EXCLUSIVE-OR", "NSTRING-CAPITALIZE", "NSTRING-DOWNCASE", "NSTRING-UPCASE", "NSUBLIS",
            "NSUBST", "NSUBST-IF", "NSUBST-IF-NOT", "NSUBSTITUTE", "NSUBSTITUTE-IF", "NSUBSTITUTE-IF-NOT", "NTH", "NTH-VALUE",
            "NTHCDR", "NULL", "NUMBER", "NUMBERP", "NUMERATOR", "NUNION", "ODDP", "OPEN",
            "OPEN-STREAM-P", "OPTIMIZE", "OR", "OTHERWISE", "OUTPUT-STREAM-P", "PACKAGE", "PACKAGE-ERROR", "PACKAGE-ERROR-PACKAGE",
            "PACKAGE-NAME", "PACKAGE-NICKNAMES", "PACKAGE-SHADOWING-SYMBOLS", "PACKAGE-USE-LIST", "PACKAGE-USED-BY-LIST", "PACKAGEP", "PAIRLIS", "PARSE-ERROR",
            "PARSE-INTEGER", "PARSE-NAMESTRING", "PATHNAME", "PATHNAME-DEVICE", "PATHNAME-DIRECTORY", "PATHNAME-HOST", "PATHNAME-MATCH-P", "PATHNAME-NAME",
            "PATHNAME-TYPE", "PATHNAME-VERSION", "PATHNAMEP", "PEEK-CHAR", "PHASE", "PI", "PLUSP", "POP",
            "POSITION", "POSITION-IF", "POSITION-IF-NOT", "PPRINT", "PPRINT-DISPATCH", "PPRINT-EXIT-IF-LIST-EXHAUSTED", "PPRINT-FILL", "PPRINT-INDENT",
            "PPRINT-LINEAR", "PPRINT-LOGICAL-BLOCK", "PPRINT-NEWLINE", "PPRINT-POP", "PPRINT-TAB", "PPRINT-TABULAR", "PRIN1", "PRIN1-TO-STRING",
            "PRINC", "PRINC-TO-STRING", "PRINT", "PRINT-NOT-READABLE", "PRINT-NOT-READABLE-OBJECT", "PRINT-OBJECT", "PRINT-UNREADABLE-OBJECT", "PROBE-FILE",
            "PROCLAIM", "PROG", "PROG*", "PROG1", "PROG2", "PROGN", "PROGRAM-ERROR", "PROGV",
            "PROVIDE", "PSETF", "PSETQ", "PUSH", "PUSHNEW", "QUOTE", "RANDOM", "RANDOM-STATE",
            "RANDOM-STATE-P", "RASSOC", "RASSOC-IF", "RASSOC-IF-NOT", "RATIO", "RATIONAL", "RATIONALIZE", "RATIONALP",
            "READ", "READ-BYTE", "READ-CHAR", "READ-CHAR-NO-HANG", "READ-DELIMITED-LIST", "READ-FROM-STRING", "READ-LINE", "READ-PRESERVING-WHITESPACE",
            "READ-SEQUENCE", "READER-ERROR", "READTABLE", "READTABLE-CASE", "READTABLEP", "REAL", "REALP", "REALPART",
            "REDUCE", "REINITIALIZE-INSTANCE", "REM", "REMF", "REMHASH", "REMOVE", "REMOVE-DUPLICATES", "REMOVE-IF",
            "REMOVE-IF-NOT", "REMOVE-METHOD", "REMPROP", "RENAME-FILE", "RENAME-PACKAGE", "REPLACE", "REQUIRE", "REST",
            "RESTART", "RESTART-BIND", "RESTART-CASE", "RESTART-NAME", "RETURN", "RETURN-FROM", "REVAPPEND", "REVERSE",
            "ROOM", "ROTATEF", "ROUND", "ROW-MAJOR-AREF", "RPLACA", "RPLACD", "SAFETY", "SATISFIES",
            "SBIT", "SCALE-FLOAT", "SCHAR", "SEARCH", "SECOND", "SEQUENCE", "SERIOUS-CONDITION", "SET",
            "SET-DIFFERENCE", "SET-DISPATCH-MACRO-CHARACTER", "SET-EXCLUSIVE-OR", "SET-MACRO-CHARACTER", "SET-PPRINT-DISPATCH", "SET-SYNTAX-FROM-CHAR", "SETF", "SETQ",
            "SEVENTH", "SHADOW", "SHADOWING-IMPORT", "SHARED-INITIALIZE", "SHIFTF", "SHORT-FLOAT", "SHORT-FLOAT-EPSILON", "SHORT-FLOAT-NEGATIVE-EPSILON",
            "SHORT-SITE-NAME", "SIGNAL", "SIGNED-BYTE", "SIGNUM", "SIMPLE-ARRAY", "SIMPLE-BASE-STRING", "SIMPLE-BIT-VECTOR", "SIMPLE-BIT-VECTOR-P",
            "SIMPLE-CONDITION", "SIMPLE-CONDITION-FORMAT-ARGUMENTS", "SIMPLE-CONDITION-FORMAT-CONTROL", "SIMPLE-ERROR", "SIMPLE-STRING", "SIMPLE-STRING-P", "SIMPLE-TYPE-ERROR", "SIMPLE-VECTOR",
            "SIMPLE-VECTOR-P", "SIMPLE-WARNING", "SIN", "SINGLE-FLOAT", "SINGLE-FLOAT-EPSILON", "SINGLE-FLOAT-NEGATIVE-EPSILON", "SINH", "SIXTH",
            "SLEEP", "SLOT-BOUNDP", "SLOT-EXISTS-P", "SLOT-MAKUNBOUND", "SLOT-MISSING", "SLOT-UNBOUND", "SLOT-VALUE", "SOFTWARE-TYPE",
            "SOFTWARE-VERSION", "SOME", "SORT", "SPACE", "SPECIAL", "SPECIAL-OPERATOR-P", "SPEED", "SQRT",
            "STABLE-SORT", "STANDARD", "STANDARD-CHAR", "STANDARD-CHAR-P", "STANDARD-CLASS", "STANDARD-GENERIC-FUNCTION", "STANDARD-METHOD", "STANDARD-OBJECT",
            "STEP", "STORAGE-CONDITION", "STORE-VALUE", "STREAM", "STREAM-ELEMENT-TYPE", "STREAM-ERROR", "STREAM-ERROR-STREAM", "STREAM-EXTERNAL-FORMAT",
            "STREAMP", "STRING", "STRING-CAPITALIZE", "STRING-DOWNCASE", "STRING-EQUAL", "STRING-GREATERP", "STRING-LEFT-TRIM", "STRING-LESSP",
            "STRING-NOT-EQUAL", "STRING-NOT-GREATERP", "STRING-NOT-LESSP", "STRING-RIGHT-TRIM", "STRING-STREAM", "STRING-TRIM", "STRING-UPCASE", "STRING/=",
            "STRING<", "STRING<=", "STRING=", "STRING>", "STRING>=", "STRINGP", "STRUCTURE", "STRUCTURE-CLASS",
            "STRUCTURE-OBJECT", "STYLE-WARNING", "SUBLIS", "SUBSEQ", "SUBSETP", "SUBST", "SUBST-IF", "SUBST-IF-NOT",
            "SUBSTITUTE", "SUBSTITUTE-IF", "SUBSTITUTE-IF-NOT", "SUBTYPEP", "SVREF", "SXHASH", "SYMBOL", "SYMBOL-FUNCTION",
            "SYMBOL-MACROLET", "SYMBOL-NAME", "SYMBOL-PACKAGE", "SYMBOL-PLIST", "SYMBOL-VALUE", "SYMBOLP", "SYNONYM-STREAM", "SYNONYM-STREAM-SYMBOL",
            "T", "TAGBODY", "TAILP", "TAN", "TANH", "TENTH", "TERPRI", "THE",
            "THIRD", "THROW", "TIME", "TRACE", "TRANSLATE-LOGICAL-PATHNAME", "TRANSLATE-PATHNAME", "TREE-EQUAL", "TRUENAME",
            "TRUNCATE", "TWO-WAY-STREAM", "TWO-WAY-STREAM-INPUT-STREAM", "TWO-WAY-STREAM-OUTPUT-STREAM", "TYPE", "TYPE-ERROR", "TYPE-ERROR-DATUM", "TYPE-ERROR-EXPECTED-TYPE",
            "TYPE-OF", "TYPECASE", "TYPEP", "UNBOUND-SLOT", "UNBOUND-SLOT-INSTANCE", "UNBOUND-VARIABLE", "UNDEFINED-FUNCTION", "UNEXPORT",
            "UNINTERN", "UNION", "UNLESS", "UNREAD-CHAR", "UNSIGNED-BYTE", "UNTRACE", "UNUSE-PACKAGE", "UNWIND-PROTECT",
            "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS", "UPDATE-INSTANCE-FOR-REDEFINED-CLASS", "UPGRADED-ARRAY-ELEMENT-TYPE", "UPGRADED-COMPLEX-PART-TYPE", "UPPER-CASE-P", "USE-PACKAGE", "USE-VALUE", "USER-HOMEDIR-PATHNAME",
            "VALUES", "VALUES-LIST", "VARIABLE", "VECTOR", "VECTOR-POP", "VECTOR-PUSH", "VECTOR-PUSH-EXTEND", "VECTORP",
            "WARN", "WARNING", "WHEN", "WILD-PATHNAME-P", "WITH-ACCESSORS", "WITH-COMPILATION-UNIT", "WITH-CONDITION-RESTARTS", "WITH-HASH-TABLE-ITERATOR",
            "WITH-INPUT-FROM-STRING", "WITH-OPEN-FILE", "WITH-OPEN-STREAM", "WITH-OUTPUT-TO-STRING", "WITH-PACKAGE-ITERATOR", "WITH-SIMPLE-RESTART", "WITH-SLOTS", "WITH-STANDARD-IO-SYNTAX",
            "WRITE", "WRITE-BYTE", "WRITE-CHAR", "WRITE-LINE", "WRITE-SEQUENCE", "WRITE-STRING", "WRITE-TO-STRING", "Y-OR-N-P",
            "YES-OR-NO-P", "ZEROP",
        }) { InternExport(name); }

        // Set up missing standard CL special variables
        foreach (var (vname, vval) in new (string, LispObject)[] {
            ("**", Nil.Instance), ("***", Nil.Instance),
            ("++", Nil.Instance), ("+++", Nil.Instance),
            ("//", Nil.Instance), ("///", Nil.Instance),
            ("*BREAK-ON-SIGNALS*", Nil.Instance),
            ("*COMPILE-PRINT*", Nil.Instance),
            ("*COMPILE-VERBOSE*", Nil.Instance),
            ("*DEBUGGER-HOOK*", Nil.Instance),
            ("*MACROEXPAND-HOOK*", Nil.Instance),
            ("*PRINT-ARRAY*", T.Instance),
            ("*PRINT-BASE*", Fixnum.Make(10)),
            ("*PRINT-CASE*", Keyword("UPCASE")),
            ("*PRINT-CIRCLE*", Nil.Instance),
            ("*PRINT-GENSYM*", T.Instance),
            ("*PRINT-LENGTH*", Nil.Instance),
            ("*PRINT-LEVEL*", Nil.Instance),
            ("*PRINT-LINES*", Nil.Instance),
            ("*PRINT-MISER-WIDTH*", Nil.Instance),
            ("*PRINT-PRETTY*", Nil.Instance),
            ("*PRINT-RADIX*", Nil.Instance),
            ("*PRINT-READABLY*", Nil.Instance),
            ("*PRINT-RIGHT-MARGIN*", Nil.Instance),
            ("*READ-EVAL*", T.Instance),
            ("*GENSYM-COUNTER*", Fixnum.Make(0)),
        })
        {
            var vsym = CL.FindSymbol(vname).symbol;
            if (vsym != null && !vsym.IsSpecial)
            {
                vsym.IsSpecial = true;
                vsym.Value = vval;
            }
        }

        // Initialize *gensym-counter*
        Runtime.InitGensymCounter();

        // Initialize built-in CLOS classes
        InitializeBuiltinClasses();

        // ClassOf(*readtable*) now returns READTABLE class via Runtime.ClassOf mapping

        // Remaining function registrations moved to Register*Builtins methods:
        // Runtime.Misc.cs: RegisterMiscBuiltins()
        // Runtime.Core.cs: RegisterCoreBuiltins()
        // Runtime.Packages.cs: RegisterPackageBuiltins()

        // INTERNAL-TIME-UNITS-PER-SECOND is a constant (matches TickCount64 = milliseconds)
        var ituSym = InternExport("INTERNAL-TIME-UNITS-PER-SECOND");
        ituSym.Value = Fixnum.Make(1000);
        ituSym.IsConstant = true;
    }

    /// <summary>Convert a pathname designator to LispPathname.</summary>
    internal static LispPathname ToPathname(LispObject obj, string caller)
    {
        if (obj is LispPathname p) return p;
        if (obj is LispString s) return LispPathname.FromString(s.Value);
        if (obj is LispFileStream fs) return LispPathname.FromString(fs.FilePath);
        if (obj is LispVector v && v.IsCharVector) return LispPathname.FromString(v.ToCharString());
        throw new LispErrorException(new LispTypeError($"{caller}: not a pathname designator", obj));
    }

    /// <summary>Compare two LispObjects for structural equality (for pathname components).</summary>
    internal static bool LispObjectEqual(LispObject? a, LispObject? b)
    {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        if (a is Nil && b is Nil) return true;
        if (a is LispString sa && b is LispString sb) return sa.Value == sb.Value;
        if (a is Symbol syma && b is Symbol symb) return syma.Name == symb.Name && syma.HomePackage?.Name == symb.HomePackage?.Name;
        if (a is Cons ca && b is Cons cb) return LispObjectEqual(ca.Car, cb.Car) && LispObjectEqual(ca.Cdr, cb.Cdr);
        return ReferenceEquals(a, b);
    }

    /// <summary>Convert a Lisp list to an array.</summary>
    internal static LispObject[] ListToArray(Cons list)
    {
        var result = new List<LispObject>();
        LispObject cur = list;
        while (cur is Cons c) { result.Add(c.Car); cur = c.Cdr; }
        return result.ToArray();
    }

    // FlushStream: moved to Runtime.IO.cs

    internal static void RegisterUnary(string name, Func<LispObject, LispObject> fn)
    {
        Emitter.CilAssembler.RegisterFunction(name,
            new LispFunction(args => {
                if (args.Length != 1)
                {
                    throw new LispErrorException(new LispProgramError($"{name}: wrong number of arguments: {args.Length} (expected 1)"));
                }
                return fn(args[0]);
            }, name));
    }

    internal static void RegisterBinary(string name, Func<LispObject, LispObject, LispObject> fn)
    {
        Emitter.CilAssembler.RegisterFunction(name,
            new LispFunction(args => {
                if (args.Length != 2)
                    throw new LispErrorException(new LispProgramError($"{name}: wrong number of arguments: {args.Length} (expected 2)"));
                return fn(args[0], args[1]);
            }, name));
    }

    /// <summary>
    /// Creates a handler function for handler-case that throws HandlerCaseInvocationException
    /// to perform the non-local exit when the condition matches.
    /// </summary>
    public static LispFunction MakeHandlerCaseFunction(object tag, int clauseIndex) =>
        new LispFunction(args =>
            throw new HandlerCaseInvocationException(
                tag, clauseIndex, args.Length > 0 ? args[0] : Nil.Instance));

    private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Symbol> _symCache = new();

    public static Symbol Sym(string name)
    {
        if (_symCache.TryGetValue(name, out var cached)) return cached;
        var (sym, status) = CL.FindSymbol(name);
        if (status != SymbolStatus.None) { _symCache[name] = sym; return sym; }
        // Symbols not in CL go to DOTCL-INTERNAL.
        // Avoids polluting CL or CL-USER. Will be resolved by self-hosting.
        var (sym2, status2) = Internal.FindSymbol(name);
        if (status2 != SymbolStatus.None) { _symCache[name] = sym2; return sym2; }
        // Cross-package bridge (replaces the old flat _functions table, D683 / #113):
        // cross-compiled code emits LOAD-SYM with the bare function name even when the
        // defun's home package is e.g. DOTCL.CIL-COMPILER. Check if any package has a
        // symbol by that name with a Function bound, and adopt it. Without this bridge
        // functions defined in non-CL packages are unreachable through flat-name
        // lookup (GetFunctionBySymbol would see newSym with null Function).
        foreach (var pkg in Package.AllPackages)
        {
            var (existingSym, existingStatus) = pkg.FindSymbol(name);
            if (existingStatus != SymbolStatus.None && existingSym.Function != null)
            {
                _symCache[name] = existingSym;
                return existingSym;
            }
        }
        // Intern a fresh placeholder in DOTCL-INTERNAL, but DO NOT cache — a
        // later defun/defmethod-direct in another package may register the
        // function on a different symbol, and a subsequent Sym() call needs
        // to re-search the packages to find it. Caching here would pin a
        // Function-less bogus symbol forever (D683, cache-pollution fix).
        var (newSym, _) = Internal.Intern(name);
        return newSym;
    }

    /// <summary>
    /// Bridge-free symbol lookup for C# function registration (write path).
    /// Only looks in CL and DOTCL-INTERNAL; never adopts symbols from other packages.
    /// Prevents RegisterFunction from silently overwriting other packages' Function slots. (#158/D918)
    /// </summary>
    internal static Symbol SymForRegistration(string name)
    {
        var (sym, status) = CL.FindSymbol(name);
        if (status != SymbolStatus.None) return sym;
        var (sym2, status2) = Internal.FindSymbol(name);
        if (status2 != SymbolStatus.None) return sym2;
        var (newSym, _) = Internal.Intern(name);
        return newSym;
    }

    private static readonly System.Collections.Concurrent.ConcurrentDictionary<(string, string), Symbol> _symInPkgCache = new();

    public static Symbol SymInPkg(string name, string pkgName)
    {
        var key = (name, pkgName);
        if (_symInPkgCache.TryGetValue(key, out var cached)) return cached;
        var pkg = Package.FindPackage(pkgName);
        if (pkg != null)
        {
            var (sym, status) = pkg.FindSymbol(name);
            if (status != SymbolStatus.None) { _symInPkgCache[key] = sym; return sym; }
            var (newSym, _) = pkg.Intern(name);
            // Cross-package Function bridge (replaces the old _functions flat
            // table, D683 / #113 Phase 3): when newly interning into a user
            // package (e.g., DOTCL-THREAD), inherit the Function slot from any
            // existing same-named symbol that has one (e.g. DOTCL-INTERNAL's
            // runtime-registered helper). Copy-on-intern only.
            if (newSym.Function == null)
            {
                foreach (var otherPkg in Package.AllPackages)
                {
                    if (otherPkg == pkg) continue;
                    var (existingSym, existingStatus) = otherPkg.FindSymbol(name);
                    if (existingStatus != SymbolStatus.None && existingSym.Function != null)
                    {
                        newSym.Function = existingSym.Function;
                        break;
                    }
                }
            }
            _symInPkgCache[key] = newSym;
            return newSym;
        }
        // Fallback to CL
        return Sym(name);
    }

    public static Symbol Keyword(string name)
    {
        var (sym, isNew) = KeywordPkg.Intern(name);
        if (isNew)
        {
            KeywordPkg.Export(sym);
            sym.Value = sym; // keywords are self-evaluating
        }
        return sym;
    }

    private static void InitializeBuiltinClasses()
    {
        // Create T class (root of hierarchy)
        var tClass = new LispClass(Sym("T"), Array.Empty<SlotDefinition>(), Array.Empty<LispClass>());
        tClass.ClassPrecedenceList = new[] { tClass };
        tClass.EffectiveSlots = Array.Empty<SlotDefinition>();
        tClass.IsBuiltIn = true;
        Runtime.RegisterClass(tClass);

        // Create STANDARD-OBJECT class (default superclass for user classes)
        var stdObj = new LispClass(Sym("STANDARD-OBJECT"), Array.Empty<SlotDefinition>(), new[] { tClass });
        stdObj.ClassPrecedenceList = new[] { stdObj, tClass };
        stdObj.EffectiveSlots = Array.Empty<SlotDefinition>();
        stdObj.IsBuiltIn = false;  // standard-object is the base of user classes
        Runtime.RegisterClass(stdObj);

        // Helper to create and register a class with given supers
        // Uses "keep-last-occurrence" deduplication to produce correct CPL for multiple inheritance
        LispClass MakeClass(string name, params LispClass[] supers)
        {
            var cls = new LispClass(Sym(name), Array.Empty<SlotDefinition>(), supers);
            var rawCpl = new List<LispClass> { cls };
            foreach (var s in supers) rawCpl.AddRange(s.ClassPrecedenceList);
            // Deduplicate keeping last occurrence (ensures T is last, no gaps)
            var seen = new HashSet<LispClass>(ReferenceEqualityComparer.Instance);
            var cpl = new List<LispClass>();
            for (int i = rawCpl.Count - 1; i >= 0; i--)
                if (seen.Add(rawCpl[i])) cpl.Insert(0, rawCpl[i]);
            cls.ClassPrecedenceList = cpl.ToArray();
            // Inherit effective slots from superclasses
            var inheritedSlots = new List<SlotDefinition>();
            var seenSlotNames = new HashSet<string>();
            foreach (var s in supers)
            {
                if (s.EffectiveSlots != null)
                    foreach (var es in s.EffectiveSlots)
                        if (seenSlotNames.Add(es.Name.Name))
                            inheritedSlots.Add(es);
            }
            cls.EffectiveSlots = inheritedSlots.ToArray();
            for (int i = 0; i < cls.EffectiveSlots.Length; i++)
                cls.SlotIndex[cls.EffectiveSlots[i].Name.Name] = i;
            cls.IsBuiltIn = true;
            Runtime.RegisterClass(cls);
            return cls;
        }

        // Helper to create a class with slots (initarg = lowercased slot name)
        LispClass MakeClassWithSlots(string name, LispClass super, params string[] slotNames)
        {
            var slots = slotNames.Select(sn =>
                new SlotDefinition(Sym(sn), new[] { Sym(sn) })).ToArray();
            var cls = new LispClass(Sym(name), slots, new[] { super });
            var cpl = new List<LispClass> { cls };
            cpl.AddRange(super.ClassPrecedenceList);
            cls.ClassPrecedenceList = cpl.ToArray();
            // Effective slots: own slots + inherited from parent
            var allSlots = new List<SlotDefinition>(slots);
            if (super.EffectiveSlots != null)
            {
                var ownNames = new HashSet<string>(slots.Select(s => s.Name.Name));
                foreach (var es in super.EffectiveSlots)
                    if (!ownNames.Contains(es.Name.Name))
                        allSlots.Add(es);
            }
            cls.EffectiveSlots = allSlots.ToArray();
            for (int i = 0; i < cls.EffectiveSlots.Length; i++)
                cls.SlotIndex[cls.EffectiveSlots[i].Name.Name] = i;
            Runtime.RegisterClass(cls);
            return cls;
        }

        // Helper for condition classes (not built-in, subclassable via DEFCLASS/DEFINE-CONDITION)
        LispClass MakeCondClass(string name, params LispClass[] supers)
        {
            var cls = MakeClass(name, supers);
            cls.IsBuiltIn = false;
            return cls;
        }

        LispClass MakeCondClassWithSlots(string name, LispClass super, params string[] slotNames)
        {
            var cls = MakeClassWithSlots(name, super, slotNames);
            cls.IsBuiltIn = false;
            return cls;
        }

        // Standard condition hierarchy
        var condition = MakeCondClass("CONDITION", stdObj);
        var seriousCondition = MakeCondClass("SERIOUS-CONDITION", condition);
        var error = MakeCondClass("ERROR", seriousCondition);
        var typeError = MakeCondClassWithSlots("TYPE-ERROR", error, "DATUM", "EXPECTED-TYPE");
        var cellError = MakeCondClassWithSlots("CELL-ERROR", error, "NAME");
        MakeCondClass("UNBOUND-VARIABLE", cellError);
        MakeCondClass("UNDEFINED-FUNCTION", cellError);
        MakeCondClassWithSlots("PACKAGE-ERROR", error, "PACKAGE");
        var arithmeticError = MakeCondClassWithSlots("ARITHMETIC-ERROR", error, "OPERATION", "OPERANDS");
        MakeCondClass("DIVISION-BY-ZERO", arithmeticError);
        MakeCondClass("CONTROL-ERROR", error);
        MakeCondClass("PROGRAM-ERROR", error);
        MakeCondClassWithSlots("FILE-ERROR", error, "PATHNAME");
        var streamError = MakeCondClassWithSlots("STREAM-ERROR", error, "STREAM");
        MakeCondClass("END-OF-FILE", streamError);
        var parseError = MakeCondClass("PARSE-ERROR", error);
        var warning = MakeCondClass("WARNING", condition);
        var simpleCondition = MakeCondClassWithSlots("SIMPLE-CONDITION", condition, "FORMAT-CONTROL", "FORMAT-ARGUMENTS");
        MakeCondClass("STYLE-WARNING", warning);
        MakeCondClass("STORAGE-CONDITION", seriousCondition);
        var iiCls = MakeCondClass("INTERACTIVE-INTERRUPT", seriousCondition); // Ctrl-C interrupt (SBCL-like)
        // Import into CL-USER so (interactive-interrupt) is accessible without CL: prefix,
        // but keep NOT-exported from CL to avoid failing NO-EXTRA-SYMBOLS ANSI test.
        CLUser.Import(iiCls.Name);
        // Multiple-inheritance condition classes (CL standard hierarchy)
        MakeCondClass("SIMPLE-ERROR", simpleCondition, error);       // [SIMPLE-ERROR, SIMPLE-CONDITION, ERROR, ...]
        MakeCondClass("SIMPLE-WARNING", simpleCondition, warning);   // [SIMPLE-WARNING, SIMPLE-CONDITION, WARNING, ...]
        MakeCondClass("SIMPLE-TYPE-ERROR", simpleCondition, typeError); // [SIMPLE-TYPE-ERROR, SIMPLE-CONDITION, TYPE-ERROR, ...]
        MakeCondClass("READER-ERROR", parseError, streamError);      // [READER-ERROR, PARSE-ERROR, STREAM-ERROR, ...]

        // Built-in type classes (for defmethod specializers like (function function))
        var function_ = MakeClass("FUNCTION", tClass);
        var symbol_ = MakeClass("SYMBOL", tClass);
        var sequence = MakeClass("SEQUENCE", tClass);
        var list_ = MakeClass("LIST", sequence);
        MakeClass("CONS", list_);   // CONS CPL: [CONS, LIST, SEQUENCE, T]
        MakeClass("NULL", symbol_, list_);  // NULL CPL: [NULL, SYMBOL, LIST, SEQUENCE, T]
        var number = MakeClass("NUMBER", tClass);
        var real_ = MakeClass("REAL", number);
        var rational = MakeClass("RATIONAL", real_);
        var integer_ = MakeClass("INTEGER", rational);
        MakeClass("FIXNUM", integer_);
        MakeClass("BIGNUM", integer_);
        MakeClass("RATIO", rational);
        var float_ = MakeClass("FLOAT", real_);
        MakeClass("SHORT-FLOAT", float_);
        MakeClass("SINGLE-FLOAT", float_);
        MakeClass("DOUBLE-FLOAT", float_);
        MakeClass("LONG-FLOAT", float_);
        MakeClass("COMPLEX", number);
        MakeClass("CHARACTER", tClass);
        var array_ = MakeClass("ARRAY", tClass);    // ARRAY CPL: [ARRAY, T]
        var simpleArray = MakeClass("SIMPLE-ARRAY", array_);  // SIMPLE-ARRAY ⊂ ARRAY
        var vector_ = MakeClass("VECTOR", array_, sequence);  // VECTOR CPL: [VECTOR, ARRAY, SEQUENCE, T]
        MakeClass("SIMPLE-VECTOR", vector_, simpleArray);
        var string_ = MakeClass("STRING", vector_);    // STRING CPL: [STRING, VECTOR, ARRAY, SEQUENCE, T]
        MakeClass("SIMPLE-STRING", string_, simpleArray);
        MakeClass("HASH-TABLE", tClass);
        var stream_ = MakeClass("STREAM", stdObj);
        stream_.IsBuiltIn = false; // Allow subclassing for Gray Streams (de-facto standard)
        var pathname_ = MakeClass("PATHNAME", tClass);
        MakeClass("RANDOM-STATE", tClass);
        MakeClass("READTABLE", tClass);
        MakeClass("RESTART", tClass);
        MakeCondClassWithSlots("PRINT-NOT-READABLE", error, "OBJECT");
        MakeCondClassWithSlots("UNBOUND-SLOT", cellError, "INSTANCE");
        MakeCondClass("FLOATING-POINT-INEXACT", arithmeticError);
        MakeCondClass("FLOATING-POINT-INVALID-OPERATION", arithmeticError);
        MakeCondClass("FLOATING-POINT-UNDERFLOW", arithmeticError);
        MakeCondClass("FLOATING-POINT-OVERFLOW", arithmeticError);

        // Additional standard CL classes needed for find-class (ANSI tests)
        var bitVector = MakeClass("BIT-VECTOR", vector_);
        MakeClass("SIMPLE-BIT-VECTOR", bitVector, simpleArray);
        var stream_class = stream_;  // alias for readability
        MakeClass("BROADCAST-STREAM", stream_class);
        MakeClass("CONCATENATED-STREAM", stream_class);
        MakeClass("ECHO-STREAM", stream_class);
        MakeClass("FILE-STREAM", stream_class);
        MakeClass("STRING-STREAM", stream_class);
        MakeClass("SYNONYM-STREAM", stream_class);
        MakeClass("TWO-WAY-STREAM", stream_class);
        MakeClass("LOGICAL-PATHNAME", pathname_);
        // GENERIC-FUNCTION inherits from both FUNCTION and STANDARD-OBJECT per CLHS
        // (funcallable standard objects are standard-objects)
        var genericFunction = MakeClass("GENERIC-FUNCTION", function_, stdObj);
        genericFunction.IsBuiltIn = false;
        var standardGenericFunction = MakeClass("STANDARD-GENERIC-FUNCTION", genericFunction);
        standardGenericFunction.IsBuiltIn = false;
        var class_ = MakeClass("CLASS", stdObj);
        class_.IsBuiltIn = false;
        MakeClass("BUILT-IN-CLASS", class_);
        var standardClass = MakeClass("STANDARD-CLASS", class_);
        standardClass.IsBuiltIn = false;
        MakeClass("STRUCTURE-CLASS", class_);
        var method_ = MakeClass("METHOD", stdObj);
        method_.IsBuiltIn = false;
        var standardMethod = MakeClass("STANDARD-METHOD", method_);
        standardMethod.IsBuiltIn = false;
        var standardAccessorMethod = MakeClass("STANDARD-ACCESSOR-METHOD", standardMethod);
        standardAccessorMethod.IsBuiltIn = false;
        var standardReaderMethod = MakeClass("STANDARD-READER-METHOD", standardAccessorMethod);
        standardReaderMethod.IsBuiltIn = false;
        var standardWriterMethod = MakeClass("STANDARD-WRITER-METHOD", standardAccessorMethod);
        standardWriterMethod.IsBuiltIn = false;
        MakeClass("METHOD-COMBINATION", tClass);
        MakeClass("PACKAGE", tClass);
        var structureObject = MakeClass("STRUCTURE-OBJECT", tClass);
        structureObject.IsStructureClass = true;

        // MOP: slot definition classes (AMOP hierarchy)
        var slotDefinition = MakeClass("SLOT-DEFINITION", stdObj);
        slotDefinition.IsBuiltIn = false;  // allow Lisp code to subclass
        var standardSlotDefinition = MakeClass("STANDARD-SLOT-DEFINITION", slotDefinition);
        standardSlotDefinition.IsBuiltIn = false;
        var directSlotDefinition = MakeClass("DIRECT-SLOT-DEFINITION", slotDefinition);
        directSlotDefinition.IsBuiltIn = false;
        var effectiveSlotDefinition = MakeClass("EFFECTIVE-SLOT-DEFINITION", slotDefinition);
        effectiveSlotDefinition.IsBuiltIn = false;
        var stdDirectSlot = MakeClass("STANDARD-DIRECT-SLOT-DEFINITION", standardSlotDefinition, directSlotDefinition);
        stdDirectSlot.IsBuiltIn = false;
        var stdEffectiveSlot = MakeClass("STANDARD-EFFECTIVE-SLOT-DEFINITION", standardSlotDefinition, effectiveSlotDefinition);
        stdEffectiveSlot.IsBuiltIn = false;
        // MOP: specializer hierarchy
        var specializer = MakeClass("SPECIALIZER", stdObj);
        specializer.IsBuiltIn = false;
        var eqlSpecializer = MakeClass("EQL-SPECIALIZER", specializer);
        eqlSpecializer.IsBuiltIn = false;

        // ===== Environment functions =====
        Emitter.CilAssembler.RegisterFunction("LISP-IMPLEMENTATION-TYPE",
            new LispFunction(Runtime.LispImplementationType, "LISP-IMPLEMENTATION-TYPE", 0));
        Emitter.CilAssembler.RegisterFunction("LISP-IMPLEMENTATION-VERSION",
            new LispFunction(Runtime.LispImplementationVersion, "LISP-IMPLEMENTATION-VERSION", 0));
        Emitter.CilAssembler.RegisterFunction("SOFTWARE-TYPE",
            new LispFunction(Runtime.SoftwareType, "SOFTWARE-TYPE", 0));
        Emitter.CilAssembler.RegisterFunction("SOFTWARE-VERSION",
            new LispFunction(Runtime.SoftwareVersion, "SOFTWARE-VERSION", 0));
        Emitter.CilAssembler.RegisterFunction("MACHINE-TYPE",
            new LispFunction(Runtime.MachineType, "MACHINE-TYPE", 0));
        Emitter.CilAssembler.RegisterFunction("MACHINE-VERSION",
            new LispFunction(Runtime.MachineVersion, "MACHINE-VERSION", 0));
        Emitter.CilAssembler.RegisterFunction("MACHINE-INSTANCE",
            new LispFunction(Runtime.MachineInstance, "MACHINE-INSTANCE", 0));
        Emitter.CilAssembler.RegisterFunction("SHORT-SITE-NAME",
            new LispFunction(Runtime.ShortSiteName, "SHORT-SITE-NAME", 0));
        Emitter.CilAssembler.RegisterFunction("LONG-SITE-NAME",
            new LispFunction(Runtime.LongSiteName, "LONG-SITE-NAME", 0));
        Emitter.CilAssembler.RegisterFunction("APROPOS",
            new LispFunction(Runtime.Apropos, "APROPOS", -1));
        Emitter.CilAssembler.RegisterFunction("APROPOS-LIST",
            new LispFunction(Runtime.AproposList, "APROPOS-LIST", -1));
        Emitter.CilAssembler.RegisterFunction("DESCRIBE",
            new LispFunction(Runtime.Describe, "DESCRIBE", -1));
        Emitter.CilAssembler.RegisterFunction("ROOM",
            new LispFunction(Runtime.Room, "ROOM", -1));
        // SB-EXT:GC — trigger .NET GC (useful to reduce memory during SBCL XC build)
        {
            var gcFn = new LispFunction(_ => {
                GC.Collect(2, GCCollectionMode.Aggressive, true, true);
                GC.WaitForPendingFinalizers();
                GC.Collect(2, GCCollectionMode.Aggressive, true, true);
                return Nil.Instance;
            }, "SB-EXT:GC", -1);
            var gcSym = SymInPkg("GC", "SB-EXT");
            gcSym.Function = gcFn;
            Emitter.CilAssembler.RegisterFunction("SB-EXT:GC", gcFn);
        }
        // RUN-WITH-TIMEOUT: (run-with-timeout seconds thunk) — run THUNK with per-stem timeout
        // If THUNK does not complete within SECONDS, throws a Lisp error and moves on.
        // The background thread is a daemon thread and will be collected when the process exits.
        {
            var rwtFn = new LispFunction(args => {
                if (args.Length < 2)
                    throw new LispErrorException(new LispProgramError("RUN-WITH-TIMEOUT: requires 2 args"));
                int timeoutMs = (int)(((Fixnum)args[0]).Value * 1000);
                var thunk = args[1];
                Exception? taskEx = null;
                LispObject? taskResult = null;
                var done = new System.Threading.ManualResetEventSlim(false);
                var t = new System.Threading.Thread(() => {
                    try { taskResult = Runtime.Funcall(thunk); }
                    catch (Exception e) { taskEx = e; }
                    finally { done.Set(); }
                });
                t.IsBackground = true;
                t.Start();
                if (!done.Wait(timeoutMs))
                    throw new LispErrorException(new LispProgramError($"RUN-WITH-TIMEOUT: timed out after {((Fixnum)args[0]).Value}s"));
                if (taskEx != null)
                    System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(taskEx).Throw();
                return taskResult!;
            }, "RUN-WITH-TIMEOUT", 2);
            Startup.Sym("RUN-WITH-TIMEOUT")!.Function = rwtFn;
            Emitter.CilAssembler.RegisterFunction("RUN-WITH-TIMEOUT", rwtFn);
        }
        Emitter.CilAssembler.RegisterFunction("DISASSEMBLE",
            new LispFunction(Runtime.Disassemble, "DISASSEMBLE", -1));
        Emitter.CilAssembler.RegisterFunction("%TRACE",
            new LispFunction(Runtime.Trace, "%TRACE", -1));
        Emitter.CilAssembler.RegisterFunction("%UNTRACE",
            new LispFunction(Runtime.Untrace, "%UNTRACE", -1));
        Emitter.CilAssembler.RegisterFunction("ED",
            new LispFunction(Runtime.Ed, "ED", -1));
        Emitter.CilAssembler.RegisterFunction("DRIBBLE",
            new LispFunction(Runtime.Dribble, "DRIBBLE", -1));
        Emitter.CilAssembler.RegisterFunction("INSPECT",
            new LispFunction(Runtime.Inspect, "INSPECT", -1));
        // DOCUMENTATION and (SETF DOCUMENTATION) are defined as generic functions
        // in cil-stdlib.lisp. Don't register as plain functions here to avoid
        // defgeneric validation errors.

        // PPRINT-*, COPY-PPRINT-DISPATCH, SET-PPRINT-DISPATCH, PPRINT-EXIT-IF-LIST-EXHAUSTED, PPRINT-POP: moved to Runtime.Printer.cs

        // SHARED-INITIALIZE, INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, DESCRIBE-OBJECT,
        // CHANGE-CLASS, MAKE-LOAD-FORM, FUNCTION-KEYWORDS, UPDATE-INSTANCE-FOR-REDEFINED-CLASS: moved to Runtime.CLOS.cs

        RegisterDotclFunctions();
        Runtime.RegisterMiscBuiltins();
        Runtime.RegisterCoreBuiltins();
        Runtime.RegisterPackageBuiltins();
        Runtime.RegisterArithmeticBuiltins();
        Runtime.RegisterIOBuiltins();
        Runtime.RegisterCLOSBuiltins();
        // DOTCL-MOP package (#144 phase 1: AMOP introspection on top of dotcl CLOS).
        // MUST run AFTER RegisterCLOSBuiltins — that registers bare-name functions
        // (e.g. GENERIC-FUNCTION-NAME) via Sym() whose cross-package bridge would
        // otherwise find any existing DOTCL-MOP::GENERIC-FUNCTION-NAME with a
        // Function set and clobber it.
        Mop.Init();
        Runtime.RegisterConditionsBuiltins();
        Runtime.RegisterPrinterBuiltins();
        Runtime.RegisterThreadBuiltins();
        Runtime.RegisterSequenceBuiltins();
        Runtime.RegisterCollectionBuiltins();
        Runtime.RegisterPredicateBuiltins();
        Runtime.RegisterPathnameBuiltins();
        GeneratedDocs.Register();
    }

    public static bool HasFeature(string name) => _features.Contains(name);

    /// <summary>Register a function in the DOTCL package (exported).</summary>
    private static void RegisterDotcl(string name, LispFunction fn)
    {
        var (sym, _) = DotclPkg.Intern(name);
        DotclPkg.Export(sym);
        sym.Function = fn;
    }

    /// <summary>Register a function in the DOTCL package WITHOUT exporting.
    /// Used for internal helpers that contrib or dev tooling wraps.</summary>
    private static void RegisterDotclInternal(string name, LispFunction fn)
    {
        var (sym, _) = DotclPkg.Intern(name);
        sym.Function = fn;
    }

    private static void RegisterDotclFunctions()
    {
        // dotcl:*save-sil* — when true, defun stores SIL on symbol plist as %SIL
        var saveSilSym = SymInPkg("*SAVE-SIL*", "DOTCL");
        DotclPkg.Export(saveSilSym);
        saveSilSym.IsSpecial = true;
        saveSilSym.Value = Nil.Instance;

        // dotcl:function-sil — get SIL stored on a function (returns NIL if none)
        RegisterDotcl("FUNCTION-SIL", new LispFunction(args => {
            if (args[0] is LispFunction f && f.Sil != null) return f.Sil;
            return Nil.Instance;
        }));

        // dotcl:getenv — get environment variable (like sb-posix:getenv on SBCL)
        RegisterDotcl("GETENV", new LispFunction(args => {
            var name = args[0] is LispString s ? s.Value : args[0].ToString();
            var val = System.Environment.GetEnvironmentVariable(name);
            return val != null ? (LispObject)new LispString(val) : Nil.Instance;
        }));

        // dotcl:*debug-stacktrace* — when true, .NET stack traces are printed on unhandled errors
        var debugStacktraceSym = SymInPkg("*DEBUG-STACKTRACE*", "DOTCL");
        DotclPkg.Export(debugStacktraceSym);
        debugStacktraceSym.IsSpecial = true;
        debugStacktraceSym.Value = Nil.Instance;

        // dotcl:%ctype-stats — return CType routing statistics (temporary diagnostic)
        RegisterDotcl("%CTYPE-STATS", new LispFunction(args => {
            return new LispString(Runtime.CTypeStats());
        }));

        // Package lock API (#93). *PACKAGE-LOCKS-DISABLED*: when bound to T,
        // CheckPackageLock becomes a no-op (used by WITHOUT-PACKAGE-LOCKS).
        var disabledSym = SymInPkg("*PACKAGE-LOCKS-DISABLED*", "DOTCL");
        DotclPkg.Export(disabledSym);
        disabledSym.IsSpecial = true;
        disabledSym.Value = Nil.Instance;
        RegisterDotcl("LOCK-PACKAGE", new LispFunction(args => Runtime.LockPackage(args[0])));
        RegisterDotcl("UNLOCK-PACKAGE", new LispFunction(args => Runtime.UnlockPackage(args[0])));
        RegisterDotcl("PACKAGE-LOCKED-P", new LispFunction(args => Runtime.PackageLockedP(args[0])));

        // Local package nicknames (CDR 5 / compatible with trivial-package-local-nicknames)
        Emitter.CilAssembler.RegisterFunction("%ADD-LOCAL-NICKNAME",
            new LispFunction(args => Runtime.AddPackageLocalNickname(args[0], args[1], args[2]),
                             "%ADD-LOCAL-NICKNAME", -1));
        RegisterDotcl("ADD-PACKAGE-LOCAL-NICKNAME",
            new LispFunction(args => Runtime.AddPackageLocalNickname(
                args[0], args[1], args.Length > 2 ? args[2] : DynamicBindings.Get(Sym("*PACKAGE*")))));
        RegisterDotcl("REMOVE-PACKAGE-LOCAL-NICKNAME",
            new LispFunction(args => Runtime.RemovePackageLocalNickname(
                args[0], args.Length > 1 ? args[1] : DynamicBindings.Get(Sym("*PACKAGE*")))));
        RegisterDotcl("PACKAGE-LOCAL-NICKNAMES",
            new LispFunction(args => Runtime.PackageLocalNicknames(
                args.Length > 0 ? args[0] : DynamicBindings.Get(Sym("*PACKAGE*")))));

        // Compiler-callable package lock check, used by compile-defmacro before
        // it mutates the Lisp-side *macros* table (which bypasses RegisterMacroFunction).
        Emitter.CilAssembler.RegisterFunction("%CHECK-PACKAGE-LOCK",
            new LispFunction(args => {
                if (args.Length < 1 || args[0] is not Symbol s) return Nil.Instance;
                var ctx = args.Length > 1 && args[1] is LispString ls ? ls.Value : "DEFINE";
                Runtime.CheckPackageLock(s, ctx);
                return Nil.Instance;
            }, "%CHECK-PACKAGE-LOCK", -1));

        // dotcl:without-package-locks — binds *package-locks-disabled* to T
        // Expansion: (let ((dotcl:*package-locks-disabled* t)) . body)
        // Implemented in C# to avoid cross-compile package-resolution issues.
        var withoutLocksSym = SymInPkg("WITHOUT-PACKAGE-LOCKS", "DOTCL");
        DotclPkg.Export(withoutLocksSym);
        var letSym = Sym("LET");
        Runtime.RegisterMacroFunction(withoutLocksSym, new LispFunction(args => {
            var form = args[0];
            var body = form is Cons fc ? fc.Cdr : (LispObject)Nil.Instance;
            // (dotcl:*package-locks-disabled* t)
            var binding = new Cons(disabledSym, new Cons(T.Instance, Nil.Instance));
            // ((binding))
            var bindings = new Cons(binding, Nil.Instance);
            // (let ((...)) . body)
            return new Cons(letSym, new Cons(bindings, body));
        }, "WITHOUT-PACKAGE-LOCKS"));

        // dotcl:quit — exit the process (like sb-ext:exit on SBCL)
        RegisterDotcl("QUIT", new LispFunction(args => {
            int code = args.Length > 0 && args[0] is Fixnum n ? (int)n.Value : 0;
            System.Environment.Exit(code);
            return Nil.Instance; // unreachable
        }));

        // dotcl:save-application — SBCL-style save-lisp-and-die for dotcl.
        // MVP: :executable nil only. See docs/plans/2026-04-21-save-application-design.md.
        RegisterDotcl("SAVE-APPLICATION", new LispFunction(
            Runtime.SaveApplication, "SAVE-APPLICATION"));

        // (dotcl-cs lives entirely in contrib/dotcl-cs/ as of D686 / D903 —
        // no runtime registration here. IlDisasm.cs stays in runtime as a
        // reusable primitive.)

        // dotcl:sil-to-fasl — convert a monolithic .sil instruction list to a
        // .fasl (PE assembly). Input .sil is one single top-level form whose
        // locals/labels may span the whole body (e.g. the cross-compiled
        // cil-out.sil). Emits it into ModuleInit via FaslAssembler.AddMonolithicForm.
        RegisterDotcl("SIL-TO-FASL", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError(
                    "SIL-TO-FASL: requires input-path and output-path"));
            var inPath = args[0] is LispString ls ? ls.Value : args[0].ToString();
            var outPath = args[1] is LispString ls2 ? ls2.Value : args[1].ToString();
            var source = System.IO.File.ReadAllText(inPath);
            var reader = new Reader(new System.IO.StringReader(source));
            if (!reader.TryRead(out var instrList))
                throw new LispErrorException(new LispProgramError(
                    $"SIL-TO-FASL: empty input: {inPath}"));
            var moduleName = System.IO.Path.GetFileNameWithoutExtension(outPath)
                + "_" + System.Guid.NewGuid().ToString("N").Substring(0, 8);
            var fasl = new DotCL.Emitter.FaslAssembler(moduleName);
            fasl.AddMonolithicForm(instrList);
            fasl.Save(outPath);
            return new LispString(outPath);
        }, "SIL-TO-FASL", 2));

        // dotcl:gc-stats — (gen0-count gen1-count gen2-count total-memory total-allocated-bytes)
        // Used by TIME to compute before/after deltas.
        RegisterDotcl("GC-STATS", new LispFunction(Runtime.GcStats, "GC-STATS", 0));

        // dotcl:alloc-report — print per-type allocation counters. Only non-zero
        // when the runtime was started with DOTCL_ALLOC_PROF=1.
        RegisterDotcl("ALLOC-REPORT", new LispFunction(args => {
            var w = Runtime.GetStandardOutputWriter();
            if (!Diagnostics.AllocCounter.Enabled)
            {
                w.WriteLine(";; DOTCL_ALLOC_PROF=1 not set at startup; counters are off");
                w.Flush();
                return Nil.Instance;
            }
            var snap = Diagnostics.AllocCounter.Snapshot();
            long total = 0;
            foreach (var (_, c) in snap) total += c;
            w.WriteLine("| type                  |       count |  % |");
            w.WriteLine("|-----------------------|-------------|----|");
            foreach (var (t, c) in snap)
            {
                var pct = total > 0 ? (c * 100.0 / total) : 0.0;
                w.WriteLine($"| {t,-21} | {c,11:N0} | {pct,3:F0} |");
            }
            w.WriteLine($"| {"TOTAL",-21} | {total,11:N0} |    |");
            w.Flush();
            return Nil.Instance;
        }, "ALLOC-REPORT", 0));

        // dotcl:alloc-reset — zero all per-type counters.
        RegisterDotcl("ALLOC-RESET", new LispFunction(args => {
            Diagnostics.AllocCounter.Reset();
            return Nil.Instance;
        }, "ALLOC-RESET", 0));

        // dotcl:%run-process — run external process, return (list exit-code stdout-string stderr-string)
        // Used by uiop:run-program in contrib/asdf/asdf.lisp
        RegisterDotcl("%RUN-PROCESS", new LispFunction(args => {
            var exe = args[0] is LispString es ? es.Value : args[0].ToString();
            var argList = args.Length > 1 ? args[1] : Nil.Instance;
            var argStrings = new System.Collections.Generic.List<string>();
            var cur = argList;
            while (cur is Cons c) {
                argStrings.Add(c.Car is LispString ls2 ? ls2.Value : c.Car.ToString());
                cur = c.Cdr;
            }
            var psi = new System.Diagnostics.ProcessStartInfo(exe) {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            };
            foreach (var a in argStrings) psi.ArgumentList.Add(a);
            try {
                using var proc = System.Diagnostics.Process.Start(psi)!;
                // Read stdout and stderr concurrently to avoid pipe buffer deadlock
                var stdoutTask = proc.StandardOutput.ReadToEndAsync();
                var stderrTask = proc.StandardError.ReadToEndAsync();
                proc.WaitForExit();
                var stdout = stdoutTask.Result;
                var stderr = stderrTask.Result;
                return Runtime.List(new Fixnum(proc.ExitCode), new LispString(stdout), new LispString(stderr));
            } catch (System.Exception ex) {
                return Runtime.List(new Fixnum(1), new LispString(ex.Message), new LispString(""));
            }
        }));

        // DOTNET package functions
        RegisterDotNet(DotNetPkg, "LOAD-ASSEMBLY", new LispFunction(Runtime.DotNetLoadAssembly, "DOTNET:LOAD-ASSEMBLY", -1));
        RegisterDotNet(DotNetPkg, "MAKE-DELEGATE", new LispFunction(Runtime.DotNetMakeDelegate, "DOTNET:MAKE-DELEGATE", 2));
        RegisterDotNet(DotNetPkg, "CALL-OUT", new LispFunction(Runtime.DotNetCallOut, "DOTNET:CALL-OUT", -1));
        RegisterDotNet(DotNetPkg, "STATIC-GENERIC", new LispFunction(Runtime.DotNetStaticGeneric, "DOTNET:STATIC-GENERIC", -1));
        RegisterDotNet(DotNetPkg, "REQUIRE", new LispFunction(DotNetNuGet.Require, "DOTNET:REQUIRE", -1));
#if !DOTCL_NO_WINFORMS
        RegisterDotNet(DotNetPkg, "UI-INVOKE", new LispFunction(DotNetWinForms.UiInvoke, "DOTNET:UI-INVOKE", -1));
        RegisterDotNet(DotNetPkg, "UI-POST", new LispFunction(DotNetWinForms.UiPost, "DOTNET:UI-POST", -1));
#endif
        RegisterDotNet(DotNetPkg, "ADD-EVENT", new LispFunction(DotNetEvents.AddEvent, "DOTNET:ADD-EVENT", -1));
        RegisterDotNet(DotNetPkg, "REMOVE-EVENT", new LispFunction(DotNetEvents.RemoveEvent, "DOTNET:REMOVE-EVENT", -1));
        RegisterDotNet(DotNetPkg, "STATIC", new LispFunction(Runtime.DotNetStatic, "DOTNET:STATIC", -1));
        RegisterDotNet(DotNetPkg, "%SET-STATIC", new LispFunction(Runtime.DotNetSetStatic, "DOTNET:%SET-STATIC", -1));
        RegisterDotNet(DotNetPkg, "INVOKE", new LispFunction(Runtime.DotNetInvoke, "DOTNET:INVOKE", -1));
        RegisterDotNet(DotNetPkg, "%SET-INVOKE", new LispFunction(Runtime.DotNetSetInvoke, "DOTNET:%SET-INVOKE", -1));
        RegisterDotNet(DotNetPkg, "NEW", new LispFunction(Runtime.DotNetNew, "DOTNET:NEW", -1));
        RegisterDotNet(DotNetPkg, "%DEFINE-CLASS", new LispFunction(Runtime.DotNetDefineClass, "DOTNET:%DEFINE-CLASS", -1));
        RegisterDotNet(DotNetPkg, "BOX", new LispFunction(Runtime.DotNetBox, "DOTNET:BOX", -1));
        RegisterDotNet(DotNetPkg, "TO-STREAM", new LispFunction(Runtime.DotNetToStream, "DOTNET:TO-STREAM", -1));
        RegisterDotNet(DotNetPkg, "%FFI-CALL", new LispFunction(Runtime.FfiCall, "DOTNET:%FFI-CALL", -1));
        RegisterDotNet(DotNetPkg, "FFI", new LispFunction(Runtime.FfiCallKeyword, "DOTNET:FFI", -1));
        RegisterDotNet(DotNetPkg, "%FFI-CALL-PTR", new LispFunction(Runtime.FfiCallPtr, "DOTNET:%FFI-CALL-PTR", -1));
        RegisterDotNet(DotNetPkg, "ALLOC-MEM", new LispFunction(Runtime.AllocMem, "DOTNET:ALLOC-MEM", -1));
        RegisterDotNet(DotNetPkg, "FREE-MEM", new LispFunction(Runtime.FreeMem, "DOTNET:FREE-MEM", -1));
        RegisterDotNet(DotNetPkg, "MEM-READ", new LispFunction(Runtime.MemRead, "DOTNET:MEM-READ", -1));
        RegisterDotNet(DotNetPkg, "MEM-WRITE", new LispFunction(Runtime.MemWrite, "DOTNET:MEM-WRITE", -1));
        RegisterDotNet(DotNetPkg, "TYPE-SIZE", new LispFunction(Runtime.TypeSize, "DOTNET:TYPE-SIZE", -1));
        RegisterDotNet(DotNetPkg, "TYPE-ALIGN", new LispFunction(Runtime.TypeAlign, "DOTNET:TYPE-ALIGN", -1));
        RegisterDotNet(DotNetPkg, "LOAD-LIBRARY", new LispFunction(Runtime.LoadLibrary, "DOTNET:LOAD-LIBRARY", -1));
        RegisterDotNet(DotNetPkg, "FREE-LIBRARY", new LispFunction(Runtime.FreeLibrary, "DOTNET:FREE-LIBRARY", -1));
        RegisterDotNet(DotNetPkg, "FIND-SYMBOL", new LispFunction(Runtime.FindSymbolInLib, "DOTNET:FIND-SYMBOL", -1));
        RegisterDotNet(DotNetPkg, "FIND-SYMBOL-ANY", new LispFunction(Runtime.FindSymbolAny, "DOTNET:FIND-SYMBOL-ANY", -1));
        RegisterDotNet(DotNetPkg, "LIBRARY-PATH", new LispFunction(Runtime.LibraryPath, "DOTNET:LIBRARY-PATH", -1));
    }

    private static void RegisterDotNet(Package pkg, string name, LispFunction fn)
    {
        var (sym, _) = pkg.Intern(name);
        pkg.Export(sym);
        Emitter.CilAssembler.RegisterFunction($"DOTNET:{name}", fn);
        sym.Function = fn;
    }

    internal static Symbol InternExport(string name)
    {
        var (sym, _) = CL.Intern(name);
        CL.Export(sym);
        return sym;
    }
}
