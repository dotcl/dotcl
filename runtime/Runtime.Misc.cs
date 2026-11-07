using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    /// <summary>Resource-name prefix marking a part inside a FASL bundle. Lives
    /// here rather than with the writer because the loader must recognise a
    /// bundle on every target, including the emit-free build that cannot write
    /// one.</summary>
    internal const string FaslBundlePartPrefix = "dotcl-bundle/";
    // Compiler macro functions registered via define-compiler-macro.
    // ConcurrentDictionary for thread-safe concurrent DEFUN/registration.
    private static readonly ConcurrentDictionary<Symbol, LispFunction> _compilerMacroFunctions = new();
    private static readonly ConcurrentDictionary<Symbol, LispFunction> _setfCompilerMacros = new();

    /// <summary>Remove a symbol's user-defined compiler macro (define-compiler-macro),
    /// for both the plain and (setf sym) namespaces. Called when the compiler macro is
    /// cleared via (setf (compiler-macro-function name) nil) and when the function is
    /// FMAKUNBOUND, so a stale compiler macro can't keep rewriting calls to a removed /
    /// re-signatured -IMPL target after the function is killed and redefined.</summary>
    internal static void RemoveUserCompilerMacro(Symbol sym)
    {
        _compilerMacroFunctions.TryRemove(sym, out _);
        _setfCompilerMacros.TryRemove(sym, out _);
    }

    // --- Gensym ---

    private static Symbol _gensymCounterSym = null!;

    public static void InitGensymCounter()
    {
        _gensymCounterSym = Startup.Sym("*GENSYM-COUNTER*");
        _gensymCounterSym.IsSpecial = true;
        if (!_gensymCounterSym.IsBound)
            _gensymCounterSym.Value = Fixnum.Make(0);
    }

    private static System.Numerics.BigInteger GetGensymCounter()
    {
        if (DynamicBindings.TryGet(_gensymCounterSym, out var v))
            return v is Fixnum f ? f.Value : v is Bignum b ? b.Value : 0;
        return 0;
    }

    private static void SetGensymCounter(System.Numerics.BigInteger val)
    {
        DynamicBindings.Set(_gensymCounterSym,
            val <= long.MaxValue ? (LispObject)Fixnum.Make((long)val) : new Bignum(val));
    }

    private static readonly object s_gensymLock = new();

    /// <summary>The value of *GENSYM-COUNTER*, which is then incremented. Two threads
    /// sharing the global value get different numbers.</summary>
    private static System.Numerics.BigInteger NextGensymCounter()
    {
        lock (s_gensymLock)
        {
            var counter = GetGensymCounter();
            SetGensymCounter(counter + 1);
            return counter;
        }
    }

    public static LispObject Gensym(LispObject prefix)
    {
        // prefix must be a string or non-negative integer (unsigned-byte).
        // CL strings include simple-strings AND character vectors of any element-type
        // CHARACTER/BASE-CHAR/STANDARD-CHAR: accept both LispString and IsCharVector.
        if (prefix is Fixnum fi && fi.Value >= 0)
            return new Symbol($"G{fi.Value}");
        if (prefix is Bignum bi && bi.Value >= 0)
            return new Symbol($"G{bi.Value}");
        string? prefixStr = prefix switch
        {
            LispString s => s.Value,
            LispVector v when v.IsCharVector => v.ToCharString(),
            _ => null,
        };
        if (prefixStr != null)
        {
            ValidateGensymCounter();
            return new Symbol($"{prefixStr}{NextGensymCounter()}");
        }
        // Invalid type: signal type-error
        var expected = new Cons(Startup.Sym("OR"),
            new Cons(Startup.Sym("STRING"),
                new Cons(Startup.Sym("UNSIGNED-BYTE"), Nil.Instance)));
        throw new LispErrorException(new LispTypeError(
            $"GENSYM: argument must be a string or unsigned-byte, not {prefix}",
            prefix, expected));
    }

    private static void ValidateGensymCounter()
    {
        LispObject v = Nil.Instance;
        if (DynamicBindings.TryGet(_gensymCounterSym, out var dv))
            v = dv;
        else if (_gensymCounterSym != null && _gensymCounterSym.IsBound)
            v = _gensymCounterSym.Value!;   // IsBound is exactly Value != null
        else
            return; // unbound = OK, GetGensymCounter returns 0
        bool valid = (v is Fixnum fx && fx.Value >= 0) || (v is Bignum bx && bx.Value >= 0);
        if (!valid)
        {
            var expected = new Cons(Startup.Sym("INTEGER"),
                new Cons(Fixnum.Make(0),
                    new Cons(Startup.Sym("*"), Nil.Instance)));
            throw new LispErrorException(new LispTypeError(
                $"The value of *GENSYM-COUNTER*, {v}, is not a non-negative integer",
                v, expected));
        }
    }

    public static LispObject Gensym0()
    {
        ValidateGensymCounter();
        return new Symbol($"G{NextGensymCounter()}");
    }

    // --- Gentemp ---

    private static long _gentempCounter = 0;

    public static LispObject Gentemp(LispObject[] args)
    {
        if (args.Length > 2)
            throw new LispErrorException(new LispProgramError($"GENTEMP: too many arguments: {args.Length} (expected 0-2)"));
        LispObject prefixArg = args.Length > 0 ? args[0] : new LispString("T");
        if (prefixArg is not LispString)
            throw new LispErrorException(new LispTypeError("GENTEMP: prefix must be a string", prefixArg));
        string prefix = ((LispString)prefixArg).Value;
        Package? pkg = null;
        if (args.Length > 1 && args[1] is not Nil)
        {
            var pkgArg = args[1];
            if (pkgArg is Package p2) pkg = p2;
            else if (StringValueOrNull(pkgArg) is string pkgNameStr) pkg = Package.FindPackage(pkgNameStr);
            else if (pkgArg is Symbol pkgSym) pkg = Package.FindPackage(pkgSym.Name);
            else if (pkgArg is LispChar pkgChar) pkg = Package.FindPackage(pkgChar.Value.ToString());
            else if (pkgArg is LispVector pkgVec && pkgVec.IsCharVector) pkg = Package.FindPackage(pkgVec.ToCharString());
            else throw new LispErrorException(new LispTypeError("GENTEMP: invalid package designator", pkgArg));
            if (pkg == null)
                throw new LispErrorException(new LispTypeError("GENTEMP: package not found", args[1]));
        }
        if (pkg == null) pkg = DynamicBindings.TryGet(Startup.Sym("*PACKAGE*"), out var pv) && pv is Package cp ? cp : Startup.CLUser;
        while (true)
        {
            var counter = System.Threading.Interlocked.Increment(ref _gentempCounter) - 1;
            var name = $"{prefix}{counter}";
            var (existing, status) = pkg.FindSymbol(name);
            if (status == SymbolStatus.None)
            {
                var (newSym, _) = pkg.Intern(name);
                return newSym;
            }
        }
    }

    // --- CharName / NameChar ---

    // Standard CL names + semi-standard + C0 control mnemonics (SBCL-compatible).
    // Some Unicode names from UCD that appear in Quicklisp libs are also listed.
    private static readonly (string name, char ch)[] _charNames = new[]
    {
        // CLHS standard names
        ("Newline", '\n'), ("Space", ' '), ("Rubout", '\x7f'),
        ("Page", '\f'), ("Tab", '\t'), ("Backspace", '\b'),
        ("Return", '\r'), ("Linefeed", '\n'),
        // Semi-standard
        ("Altmode", '\x1b'), ("Delete", '\x7f'), ("Null", '\0'), ("Nul", '\0'),
        ("Escape", '\x1b'),
        // C0 control mnemonics (SBCL additional names)
        ("Soh", '\x01'), ("Stx", '\x02'), ("Etx", '\x03'), ("Eot", '\x04'),
        ("Enq", '\x05'), ("Ack", '\x06'), ("Bel", '\x07'), ("Bell", '\x07'),
        ("Bs",  '\x08'), ("Ht",  '\x09'), ("Lf",  '\x0a'), ("Vt",  '\x0b'),
        ("Ff",  '\x0c'), ("Cr",  '\x0d'), ("So",  '\x0e'), ("Si",  '\x0f'),
        ("Dle", '\x10'), ("Dc1", '\x11'), ("Dc2", '\x12'), ("Dc3", '\x13'),
        ("Dc4", '\x14'), ("Nak", '\x15'), ("Syn", '\x16'), ("Etb", '\x17'),
        ("Can", '\x18'), ("Em",  '\x19'), ("Sub", '\x1a'), ("Esc", '\x1b'),
        ("Fs",  '\x1c'), ("Gs",  '\x1d'), ("Rs",  '\x1e'), ("Us",  '\x1f'),
        // Unicode names that appear in real Lisp libraries
        ("No-break_space", (char)0x00A0),
        ("Ideographic_space", (char)0x3000),        // U+3000 CJK IDEOGRAPHIC SPACE (cl-str)
        ("Zero_width_no-break_space", (char)0xfeff), // U+FEFF BOM/ZWNBSP (id3v2)
        ("Greek_small_letter_lamda", (char)0x03bb),  // U+03BB GREEK SMALL LETTER LAMDA (fn)
        ("Colon", ':'),                             // U+003A COLON
        ("Nel", (char)0x0085),                        // U+0085 NEXT LINE
        ("Next_line", (char)0x0085),                  // U+0085 NEXT LINE alias
        ("Hyphen-minus", '-'),                       // U+002D HYPHEN-MINUS
        ("Tilde", '~'),                             // U+007E TILDE
        ("Full_stop", '.'),                         // U+002E FULL STOP
        ("Semicolon", ';'),                         // U+003B SEMICOLON
        ("Comma", ','),                             // U+002C COMMA
        ("Solidus", '/'),                           // U+002F SOLIDUS (slash)
        ("Reverse_solidus", (char)0x005c),            // U+005C REVERSE SOLIDUS
        ("Vertical_line", '|'),                     // U+007C VERTICAL LINE
        ("Quotation_mark", '"'),                   // U+0022 QUOTATION MARK
        // Read-only aliases SBCL also accepts (its older names for these three).
        // They come after the canonical entries, so CHAR-NAME never returns them.
        ("Slash", '/'), ("Backslash", (char)0x005c), ("Period", '.'),
    };

    public static string? CharName(char c)
    {
        foreach (var (name, ch) in _charNames)
            if (ch == c && name != "Linefeed" && name != "Delete" && name != "Nul"
                && name != "Altmode") // only canonical names
                return name;
        // UCD names print with underscores (not spaces) so a #\<name> literal reads
        // back as a single token (CLHS #\ reads one token; name-char accepts both
        // underscores and spaces). This keeps char-name / ~:C / ~@C / prin1 mutually
        // consistent and round-trippable without the reader over-scanning.
        if (Ucd.CharToName.TryGetValue(c, out string? ucdName))
            return ucdName.Replace(' ', '_');
        // Non-graphic characters get U+XXXX names
        if (char.IsControl(c) || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.Format
            || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.Surrogate
            || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.PrivateUse
            || char.GetUnicodeCategory(c) == System.Globalization.UnicodeCategory.OtherNotAssigned)
            return $"U+{(int)c:X4}";
        return null;
    }

    public static char? NameChar(string s)
    {
        // ^X names a control character, as SBCL reads them: ^@ is code 0, ^A 1, ... ^Z 26,
        // ^[ 27, ^] 29, ^^ 30, ^_ 31 (letters in either case; ^\ is not a token).
        if (s.Length == 2 && s[0] == '^')
        {
            char c = char.ToUpperInvariant(s[1]);
            if (c >= '@' && c <= '_' && c != '\\') return (char)(c - '@');
        }
        var upper = s.ToUpperInvariant();
        foreach (var (name, ch) in _charNames)
            if (name.ToUpperInvariant() == upper) return ch;
        // UCD lookup: normalize underscores to spaces (e.g. "LATIN_SMALL_LETTER_A" -> "LATIN SMALL LETTER A")
        if (Ucd.NameToChar.TryGetValue(upper.Replace('_', ' '), out char ucdCh))
            return ucdCh;
        // Handle U+XXXX format names
        if (upper.StartsWith("U+") && upper.Length >= 3)
        {
            if (int.TryParse(upper.Substring(2), System.Globalization.NumberStyles.HexNumber, null, out int code)
                && code >= 0 && code <= char.MaxValue)
                return (char)code;
        }
        // Handle U followed by hex digits, any width (no plus: e.g., U20, u000C,
        // u0085, u0001FFFE), as SBCL reads them. Named characters were tried above,
        // so a name made only of U and hex digits is a code point here.
        if (upper.Length >= 2 && upper.Length <= 9 && upper[0] == 'U' && upper[1] != '+'
            && AllHexDigits(upper, 1))
        {
            if (int.TryParse(upper.Substring(1), System.Globalization.NumberStyles.HexNumber, null, out int code2)
                && code2 >= 0 && code2 <= 0x10FFFF)
            {
                // BMP characters: return directly
                if (code2 <= char.MaxValue) return (char)code2;
                // Non-BMP: dotcl's char type is 16-bit; substitute U+FFFD (replacement char).
                // cl-html5-parser uses these as noncharacter sentinels; U+FFFD is an acceptable
                // proxy since it never appears as valid parsed content either.
                return (char)0xFFFD;
            }
        }
        return null;
    }

    static bool AllHexDigits(string s, int start)
    {
        for (int i = start; i < s.Length; i++)
            if (!System.Uri.IsHexDigit(s[i])) return false;
        return true;
    }

    // --- Helper ---

    public static Number AsNumber(LispObject obj)
    {
        obj = Primary(obj);
        if (obj is Number n) return n;
        throw new LispErrorException(new LispTypeError(
            $"Not a number: {obj}", obj, Startup.Sym("NUMBER")));
    }

    /// <summary>Convert a double to exact rational via IEEE 754 decomposition.</summary>
    public static LispObject DoubleToRational(double d)
    {
        if (double.IsNaN(d) || double.IsInfinity(d))
            throw new LispErrorException(
                new LispError($"RATIONAL: {(double.IsNaN(d) ? "NaN" : "infinity")} cannot be converted to a rational")
                { ConditionTypeName = "FLOATING-POINT-INVALID-OPERATION" });
        if (System.Math.Floor(d) == d && d >= -9223372036854775808.0 && d < 9223372036854775808.0)
            return Fixnum.Make((long)d);
        // Use the exact IEEE 754 decomposition: d = mantissa * 2^exponent
        long bits = System.BitConverter.DoubleToInt64Bits(d);
        bool neg = (bits < 0);
        int exp = (int)((bits >> 52) & 0x7FF);
        long mant = bits & 0x000FFFFFFFFFFFFFL;
        if (exp == 0) { exp = 1; } else { mant |= 0x0010000000000000L; } // implicit leading 1
        exp -= 1075; // 1023 (bias) + 52 (mantissa bits)
        System.Numerics.BigInteger num = new System.Numerics.BigInteger(mant);
        System.Numerics.BigInteger den = System.Numerics.BigInteger.One;
        if (exp >= 0) { num <<= exp; }
        else { den <<= -exp; }
        if (neg) num = -num;
        var g = System.Numerics.BigInteger.GreatestCommonDivisor(System.Numerics.BigInteger.Abs(num), den);
        num /= g; den /= g;
        if (den == 1) return Bignum.MakeInteger(num);
        return Ratio.Make(num, den);
    }

    // --- Finalizers -------------------------------------------------
    //
    // CL/trivial-garbage finalizers: register a thunk to run "at some point after"
    // an object is GC'd. Running arbitrary Lisp from the .NET finalizer thread is
    // unsafe (Eval serializes on _evalLock and reads per-thread DynamicBindings),
    // so the .NET finalizer only ENQUEUES the thunk; a normal thread drains the
    // queue via RunFinalizers (called by dotcl:gc and dotcl:run-finalizers). CL
    // does not specify when finalizers run, so deferred draining is conforming.

    // Holds the finalizer thunk(s) for one object. When the object becomes
    // unreachable this object does too (it is only reachable via the
    // ConditionalWeakTable keyed on the object), so its .NET finalizer fires and
    // enqueues the pending thunks. Cancellation clears the list so a fired
    // finalizer enqueues nothing.
    private sealed class Finalizable
    {
        public readonly List<LispObject> Thunks = new();
        ~Finalizable()
        {
            lock (Thunks)
            {
                foreach (var fn in Thunks)
                    _finalizerQueue.Enqueue(fn);
            }
        }
    }

    // object -> its Finalizable holder. CWT keeps the holder alive exactly as
    // long as the key object is alive, and does NOT strong-root the key.
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<LispObject, Finalizable>
        _finalizers = new();
    private static readonly System.Collections.Concurrent.ConcurrentQueue<LispObject>
        _finalizerQueue = new();

    /// <summary>(finalize object function): run FUNCTION (a thunk) some time
    /// after OBJECT is garbage-collected. Returns OBJECT.</summary>
    public static LispObject Finalize(LispObject obj, LispObject function)
    {
        var holder = _finalizers.GetOrCreateValue(obj);
        lock (holder.Thunks) holder.Thunks.Add(function);
        return obj;
    }

    /// <summary>(cancel-finalization object): remove OBJECT's finalizers.
    /// Returns T if any were registered.</summary>
    public static LispObject CancelFinalization(LispObject obj)
    {
        if (_finalizers.TryGetValue(obj, out var holder))
        {
            lock (holder.Thunks) holder.Thunks.Clear();
            _finalizers.Remove(obj);
            return T.Instance;
        }
        return Nil.Instance;
    }

    /// <summary>Run all queued finalizer thunks on the current (normal) thread.
    /// Returns the number run. Safe to call from Lisp; each thunk runs under the
    /// usual Eval serialization. A thunk that errors is swallowed (a finalizer
    /// must not abort the others, mirroring SBCL).</summary>
    public static int RunFinalizers()
    {
        int n = 0;
        while (_finalizerQueue.TryDequeue(out var fn))
        {
            try { Funcall(fn); } catch { /* finalizer errors are ignored */ }
            n++;
        }
        return n;
    }

    internal static void RegisterMiscBuiltins()
    {
        // ERROR (format version), SIGNAL, WARN
        Emitter.CilAssembler.RegisterFunction("ERROR",
            new LispFunction(args => Runtime.LispErrorFormat(args)).WithArgCount(1, -1));
        Emitter.CilAssembler.RegisterFunction("SIGNAL",
            new LispFunction(args => Runtime.LispSignalFormat(args)).WithArgCount(1, -1));
        Emitter.CilAssembler.RegisterFunction("WARN",
            new LispFunction(Runtime.LispWarnFormat, "WARN", -1));

        // PROVIDE, REQUIRE
        Emitter.CilAssembler.RegisterFunction("PROVIDE",
            new LispFunction(args => Runtime.Provide(args[0])).WithArgCount(1, 1));
        Emitter.CilAssembler.RegisterFunction("REQUIRE",
            new LispFunction(args => Runtime.Require(args)).WithArgCount(1, 2));

        // EVAL, FORMAT, FORMATTER
        Emitter.CilAssembler.RegisterFunction("EVAL",
            new LispFunction(args => { Runtime.CheckArityMin("EVAL", args, 1); Runtime.CheckArityMax("EVAL", args, 1); return Runtime.Eval(args[0]); }));
        Emitter.CilAssembler.RegisterFunction("FORMAT",
            new LispFunction(args => {
                var dest = args[0];
                var rest = new LispObject[args.Length - 1];
                Array.Copy(args, 1, rest, 0, rest.Length);
                return Runtime.Format(dest, rest);
            }).WithArgCount(2, -1));
        Emitter.CilAssembler.RegisterFunction("FORMATTER",
            new LispFunction(args => {
                Runtime.CheckArityExact("FORMATTER", args, 1);
                var controlString = args[0] switch
                {
                    LispString s => s.Value,
                    _ => throw new LispErrorException(new LispTypeError("FORMATTER: control-string must be a string", args[0]))
                };
                return new LispFunction(fargs => {
                    if (fargs.Length < 1)
                        throw new LispErrorException(new LispError("FORMATTER function: missing stream argument"));
                    var stream = fargs[0];
                    var formatArgs = new LispObject[fargs.Length - 1];
                    Array.Copy(fargs, 1, formatArgs, 0, formatArgs.Length);
                    int consumed = Runtime.FormatToStreamReturningArgCount(controlString, formatArgs, stream);
                    LispObject tail = Nil.Instance;
                    for (int j = formatArgs.Length - 1; j >= consumed; j--)
                        tail = new Cons(formatArgs[j], tail);
                    return tail;
                }) { StoredLambdaList = FormatterLambdaList ??= List(Startup.Sym("STREAM"),
                        Startup.Sym("&REST"), Startup.SymInPkg("ARGS", "DOTCL-INTERNAL")) };
            }));

        // VALUES (variadic)
        Emitter.CilAssembler.RegisterFunction("VALUES",
            new LispFunction(args => Runtime.Values(args), "VALUES", -1));
        Startup.RegisterUnary("VALUES-LIST", Runtime.ValuesListTakingMode);
        Emitter.CilAssembler.GetFunction("VALUES-LIST").MarkMvModeEntry(1);

        // GENSYM, GENTEMP
        Emitter.CilAssembler.RegisterFunction("GENSYM",
            new LispFunction(args => args.Length == 0 ? Runtime.Gensym0() : Runtime.Gensym(args[0])));
        Emitter.CilAssembler.RegisterFunction("GENTEMP",
            new LispFunction(Runtime.Gentemp));

        // FUNCALL
        Emitter.CilAssembler.RegisterFunction("FUNCALL",
            new LispFunction(args => {
                Runtime.CheckArityMin("FUNCALL", args, 1);
                if (args[0] is LispFunction f) return f.Invoke(args.SubArray(1));
                // A symbol designator is resolved by CoerceToFunction: the same
                // coercion the compiled general FUNCALL path emits. It used to go
                // through Fdefinition, which is deliberately more permissive: it
                // answers for macros and hands back a stub for special operators,
                // both of which are FBOUNDP but have no function to call. So
                // (funcall 'progn 1) reported "Cannot call special operator PROGN"
                // as a plain ERROR and (funcall 'defconstant ...) reported the
                // MACRO's arity mismatch, where the compiled path reports
                // UNDEFINED-FUNCTION naming the symbol (ansi FUNCALL.ERROR.1/2/3).
                // CoerceToFunction also keeps the cross-package bridge that
                // Fdefinition lacks.
                return Runtime.CoerceToFunction(args[0]).Invoke(args.SubArray(1));
            }));

        // The same coercion, callable from Lisp: %MINI-EVAL uses it to resolve a
        // symbol in OPERATOR position. It used to call SYMBOL-FUNCTION directly,
        // which has no cross-package bare-name bridge: so a function registered
        // on a DOTCL-MOP / DOTCL-INTERNAL symbol was callable from compiled code
        // (CilAssembler.GetFunctionBySymbol bridges) and through FUNCALL, but not
        // as a plain (name args...) form under the interpreter:
        //   (class-precedence-list c)          => Undefined function   (interpreted)
        //   (funcall 'class-precedence-list c) => works
        //   compiled (class-precedence-list c) => works
        // Routing operator position through the same coercion removes that whole
        // class of divergence rather than giving each name its own binding.
        Emitter.CilAssembler.RegisterFunction("%COERCE-TO-FUNCTION",
            new LispFunction(args => {
                Runtime.CheckArityExact("%COERCE-TO-FUNCTION", args, 1);
                return Runtime.CoerceToFunction(args[0]);
            }, "%COERCE-TO-FUNCTION", 1));

        // %SET-FUNCTION-NAME fn name-string -> fn
        // Names a function after the fact. The tree-walk evaluator needs it: a
        // compiled DEFUN gets its name at emit time, but the interpreter builds
        // an ordinary anonymous closure, and an anonymous callee pushes no
        // debugger frame: so BACKTRACE saw nothing of the user's own calls on
        // an emit-free build. Naming the closure makes the two evaluators agree.
        Emitter.CilAssembler.RegisterFunction("%SET-FUNCTION-NAME",
            new LispFunction(args => {
                Runtime.CheckArityExact("%SET-FUNCTION-NAME", args, 2);
                if (args[0] is LispFunction fn && args[1] is LispString s) fn.Name = s.Value;
                return args[0];
            }, "%SET-FUNCTION-NAME", 2));

        // %SET-FN-INTERP-INFO fn info -> fn / %FN-INTERP-INFO fn -> info or NIL
        // The tree-walk evaluator's trampoline: see LispFunction.InterpInfo.
        Emitter.CilAssembler.RegisterFunction("%SET-FN-INTERP-INFO",
            new LispFunction(args => {
                Runtime.CheckArityExact("%SET-FN-INTERP-INFO", args, 2);
                if (args[0] is LispFunction fn) fn.InterpInfo = args[1];
                return args[0];
            }, "%SET-FN-INTERP-INFO", 2));
        Emitter.CilAssembler.RegisterFunction("%FN-INTERP-INFO",
            new LispFunction(args => {
                Runtime.CheckArityExact("%FN-INTERP-INFO", args, 1);
                return args[0] is LispFunction fn ? fn.InterpInfo ?? Nil.Instance : Nil.Instance;
            }, "%FN-INTERP-INFO", 1));
        Emitter.CilAssembler.RegisterFunction("%MINI-TAIL-FRAME",
            new LispFunction(args => {
                Runtime.CheckArityExact("%MINI-TAIL-FRAME", args, 3);
                return args[0] is LispFunction owner && args[1] is LispFunction callee
                    ? (LispObject?)LispFunction.InterpTailFrame(owner, callee, args[2]) ?? Nil.Instance
                    : Nil.Instance;
            }, "%MINI-TAIL-FRAME", 3));

        // %SET-FUNCTION-LAMBDA-LIST fn ll -> fn
        // Development information, not part of calling: see
        // LispFunction.StoredLambdaList. Compiled DEFUN records it at emit time,
        // the interpreter right after it builds the closure.
        Emitter.CilAssembler.RegisterFunction("%SET-FUNCTION-LAMBDA-LIST",
            new LispFunction(args => {
                Runtime.CheckArityExact("%SET-FUNCTION-LAMBDA-LIST", args, 2);
                if (args[0] is LispFunction fn) fn.StoredLambdaList = args[1];
                return args[0];
            }, "%SET-FUNCTION-LAMBDA-LIST", 2));
        Emitter.CilAssembler.RegisterFunction("%FUNCTION-STORED-LAMBDA-LIST",
            new LispFunction(args => {
                Runtime.CheckArityExact("%FUNCTION-STORED-LAMBDA-LIST", args, 1);
                return args[0] is LispFunction fn ? fn.StoredLambdaList ?? Nil.Instance : Nil.Instance;
            }, "%FUNCTION-STORED-LAMBDA-LIST", 1));
        Emitter.CilAssembler.RegisterFunction("%FUNCTION-ARITY",
            new LispFunction(args => {
                Runtime.CheckArityExact("%FUNCTION-ARITY", args, 1);
                return args[0] is LispFunction fn ? Fixnum.Make(fn.Arity) : Nil.Instance;
            }, "%FUNCTION-ARITY", 1));

        // %FUNCTION-ARG-COUNT fn -> (min max), max NIL when unbounded; NIL when the
        // function was registered without WithArgCount.
        Emitter.CilAssembler.RegisterFunction("%FUNCTION-ARG-COUNT",
            new LispFunction(args => {
                Runtime.CheckArityExact("%FUNCTION-ARG-COUNT", args, 1);
                if (args[0] is LispFunction { ArgCount: { } g })
                    return new Cons(Fixnum.Make(g.Min),
                        new Cons(g.Max < 0 ? Nil.Instance : Fixnum.Make(g.Max), Nil.Instance));
                return Nil.Instance;
            }, "%FUNCTION-ARG-COUNT", 1));

        // %FUNCTION-KEY-ARGS fn -> (positional (key-name ...) allow-other-keys); NIL when
        // the function was registered without WithKeyArgs.
        Emitter.CilAssembler.RegisterFunction("%FUNCTION-KEY-ARGS",
            new LispFunction(args => {
                if (args[0] is LispFunction { ArgCount: { Keys: { } keys } g })
                    return Runtime.List(Fixnum.Make(g.KeyStart),
                        Runtime.List(Array.ConvertAll(keys, k => (LispObject)new LispString(k))),
                        g.AllowOtherKeys ? T.Instance : Nil.Instance);
                return Nil.Instance;
            }, "%FUNCTION-KEY-ARGS", 1).WithArgCount(1, 1));

        // %SET-FUNCTION-ARITY fn n -> fn
        // Records how many required parameters the user's lambda list has, which
        // the interpreter's uniformly variadic closures otherwise lose. Compiled
        // code passes the same count (:param-count = number of required
        // parameters) when it emits the function, so the two evaluators agree.
        Emitter.CilAssembler.RegisterFunction("%SET-FUNCTION-ARITY",
            new LispFunction(args => {
                Runtime.CheckArityExact("%SET-FUNCTION-ARITY", args, 2);
                if (args[0] is LispFunction fn && args[1] is Fixnum n) fn.Arity = (int)n.Value;
                return args[0];
            }, "%SET-FUNCTION-ARITY", 2));

        // SYMBOL-FUNCTION, FDEFINITION, %SET-FDEFINITION, FMAKUNBOUND, MAKUNBOUND
        // SYMBOL-FUNCTION only accepts symbols (unlike FDEFINITION which also takes (setf sym))
        Startup.RegisterUnary("SYMBOL-FUNCTION", Runtime.SymbolFunction);
        Startup.RegisterUnary("FDEFINITION", Runtime.Fdefinition);
        Emitter.CilAssembler.RegisterFunction("%SET-FDEFINITION",
            new LispFunction(args => {
                if (args[1] is not LispFunction fn)
                    throw new LispErrorException(new LispTypeError("%SET-FDEFINITION: not a function", args[1]));
                if (args[0] is Cons)
                {
                    // For (setf foo) style names: nameSym.SetfFunction is the authoritative
                    // storage. Do NOT call CilAssembler.RegisterFunction with the bare
                    // "(SETF FOO)" key: that would set Startup.Sym("FOO").SetfFunction
                    // (i.e., cl:documentation) instead of the actual nameSym's SetfFunction,
                    // causing package-crossing corruption (e.g. acclimation:documentation's
                    // 4-arg setf GF overwriting cl:documentation.SetfFunction).
                    if (args[0] is Cons nc && nc.Cdr is Cons nc2 && nc2.Car is Symbol nameSym)
                    {
                        Runtime.CheckPackageLock(nameSym, "%SET-FDEFINITION");
                        nameSym.SetfFunction = fn;
                        // For uninterned gensyms (no home package) ONLY: ALSO register on the
                        // CL-package canonical symbol so string-based GetFunction("(SETF name)")
                        // can find it (FDEFINITION.5). For interned symbols in other
                        // packages (e.g. acclimation:documentation), DO NOT pollute the CL
                        // canonical symbol: that would overwrite cl:documentation.SetfFunction
                        // and break unrelated callers (cross-package pollution regression).
                        if (nameSym.HomePackage == null)
                        {
                            var canonSym = Startup.Sym(nameSym.Name);
                            if (canonSym != nameSym) canonSym.SetfFunction = fn;
                        }
                    }
                    else
                    {
                        // Unusual (setf ...) form without a symbol accessor: fall back to key-based
                        var key = Runtime.GetFunctionNameKey(args[0], "%SET-FDEFINITION");
                        Emitter.CilAssembler.RegisterFunction(key, fn);
                    }
                }
                else
                {
                    var sym = Runtime.GetSymbol(args[0], "%SET-FDEFINITION");
                    // Use RegisterFunctionOnSymbol which is package-aware:
                    // it updates sym.Function on the correct symbol (e.g. a shadowed
                    // symbol in a user package) without corrupting the CL flat-name table.
                    Emitter.CilAssembler.RegisterFunctionOnSymbol(sym, fn);
                }
                return args[1];
            }, "%SET-FDEFINITION", 2));
        Startup.RegisterUnary("FMAKUNBOUND", Runtime.Fmakunbound);
        Startup.RegisterUnary("MAKUNBOUND", Runtime.Makunbound);

        // PROCLAIM
        Emitter.CilAssembler.RegisterFunction("PROCLAIM",
            new LispFunction(args => {
                if (args.Length != 1) throw new LispErrorException(new LispProgramError($"PROCLAIM: wrong number of arguments: {args.Length} (expected 1)"));
                var decl = args[0];
                if (decl is not Cons declCons)
                    return Nil.Instance;
                var declKind = declCons.Car as Symbol;
                if (declKind == null)
                    return Nil.Instance;
                {
                    var tail = declCons.Cdr;
                    while (tail is Cons c) tail = c.Cdr;
                    if (tail is not Nil)
                        throw new LispErrorException(new LispTypeError(
                            $"PROCLAIM: malformed declaration (not a proper list)",
                            decl));
                }
                if (declKind.Name == "SPECIAL")
                {
                    var rest = declCons.Cdr;
                    while (rest is Cons c)
                    {
                        if (c.Car is Symbol sym)
                            sym.IsSpecial = true;
                        rest = c.Cdr;
                    }
                }
                else if (declKind.Name == "NOTINLINE" || declKind.Name == "INLINE")
                {
                    // CLHS 3.2.2.1.1: NOTINLINE in scope suppresses the
                    // compiler macro, and a proclamation is in scope
                    // everywhere. INLINE is the way back off.
                    var notinline = declKind.Name == "NOTINLINE";
                    var rest = declCons.Cdr;
                    while (rest is Cons c)
                    {
                        if (c.Car is Symbol sym)
                        {
                            sym.IsNotinlineProclaimed = notinline;
                            sym.IsInlineProclaimed = !notinline;
                        }
                        else if (SetfNameTarget(c.Car) is Symbol target)
                        {
                            target.IsSetfNotinlineProclaimed = notinline;
                            target.IsSetfInlineProclaimed = !notinline;
                        }
                        rest = c.Cdr;
                    }
                }
                else if (declKind.Name == "DECLARATION")
                {
                    // CLHS TYPE: a symbol cannot name both a type and a
                    // declaration. This is the "or vice versa" direction: the
                    // type definers check the flag set here.
                    var rest = declCons.Cdr;
                    while (rest is Cons c)
                    {
                        if (c.Car is Symbol sym)
                        {
                            if (Runtime.NamesAType(sym))
                                throw new LispErrorException(new LispProgramError(
                                    $"PROCLAIM: {sym.Name} already names a type, "
                                    + "so it cannot also name a declaration"));
                            sym.IsDeclarationName = true;
                        }
                        rest = c.Cdr;
                    }
                }
                else
                {
                    // Declarations the compiler keeps tables for (FTYPE, and what
                    // a compiled DEFUN or DEFSTRUCT leaves in its fasl for later
                    // compilations) go to the compiler, when one is loaded. Anything it does not know
                    // is ignored there, as it is here.
                    var compilerPkg = Package.FindPackage("DOTCL.CIL-COMPILER");
                    if (compilerPkg != null)
                    {
                        var (infoSym, status) = compilerPkg.FindSymbol("%PROCLAIM-COMPILER-INFO");
                        if (status != SymbolStatus.None && infoSym.Function is LispFunction compilerInfo)
                            compilerInfo.Invoke(new LispObject[] { decl });
                    }
                }
                return Nil.Instance;
            }));

        // MACRO-FUNCTION. The 1-arg form is compiler-hot (per-form macro check),
        // so it also carries a direct 1-arg delegate (Runtime.MacroFunction1).
        var macroFunctionFn = new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"MACRO-FUNCTION: wrong number of arguments: {args.Length}"));
            // A lexical binding in the environment decides first: a MACROLET's
            // expander, or NIL under a local function of the same name.
            if (args.Length == 2 && args[1] is Cons menv && menv.Car is Cons mops
                && args[0] is Symbol msym && LexicalOperator(mops, msym) is Cons mb)
                return mb.Cdr is LispFunction ? mb.Cdr : Nil.Instance;
            var result = Runtime.MacroFunction(args[0]);
            if (result != Nil.Instance) return result;
            var sym = Runtime.GetSymbol(args[0], "MACRO-FUNCTION");
            var compilerFn = Startup.LookupCompilerMacroAsMacro(sym);
            if (compilerFn != null)
            {
                return new LispFunction(wrapArgs => {
                    return compilerFn.Invoke(new LispObject[] { wrapArgs[0] });
                }, $"MACRO-EXPANDER-{sym.Name}", 2);
            }
            return Nil.Instance;
        }, "MACRO-FUNCTION", -1);
        macroFunctionFn.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.MacroFunction1);
        Emitter.CilAssembler.RegisterFunction("MACRO-FUNCTION", macroFunctionFn);

        // Standard macro function registration
        var standardMacros = new[] {
            "AND", "OR", "WHEN", "UNLESS", "COND", "CASE", "ECASE", "TYPECASE", "ETYPECASE",
            "DEFUN", "DEFVAR", "DEFPARAMETER", "DEFCONSTANT", "DEFMACRO", "DEFSTRUCT",
            "DEFCLASS", "DEFGENERIC", "DEFMETHOD", "DEFTYPE", "DEFINE-CONDITION",
            "SETF", "PUSH", "POP", "INCF", "DECF", "PUSHNEW", "REMF",
            "DOTIMES", "DOLIST", "DO", "DO*", "LOOP", "LOOP-FINISH",
            "WITH-OPEN-FILE", "WITH-OPEN-STREAM", "WITH-OUTPUT-TO-STRING",
            "WITH-INPUT-FROM-STRING", "WITH-ACCESSORS", "WITH-SLOTS",
            "MULTIPLE-VALUE-BIND", "MULTIPLE-VALUE-LIST", "MULTIPLE-VALUE-SETQ",
            "DESTRUCTURING-BIND", "HANDLER-CASE", "HANDLER-BIND", "IGNORE-ERRORS",
            "IN-PACKAGE", "DEFPACKAGE", "LAMBDA", "PPRINT-LOGICAL-BLOCK",
            "PRINT-UNREADABLE-OBJECT", "CHECK-TYPE", "ASSERT",
            "PROG1", "PROG2", "PROG", "PROG*", "DEFINE-SETF-EXPANDER", "DEFSETF",
            "NTH-VALUE", "TRACE", "UNTRACE", "TIME", "STEP",
            "PSETQ", "PSETF", "SHIFTF", "ROTATEF", "RETURN",
            "CCASE", "CTYPECASE", "ETYPECASE",
            "DEFINE-COMPILER-MACRO", "DEFINE-SYMBOL-MACRO", "FORMATTER",
            "DEFINE-MODIFY-MACRO", "DEFINE-METHOD-COMBINATION",
            "WITH-STANDARD-IO-SYNTAX", "WITH-COMPILATION-UNIT",
            "RESTART-CASE", "RESTART-BIND", "WITH-CONDITION-RESTARTS",
            "WITH-SIMPLE-RESTART", "WITH-HASH-TABLE-ITERATOR", "WITH-PACKAGE-ITERATOR",
            "DECLAIM", "DO-SYMBOLS", "DO-EXTERNAL-SYMBOLS", "DO-ALL-SYMBOLS",
            "CASE", "TYPECASE", "ECASE", "WHEN", "UNLESS",
            "AND", "OR", "COND"
        };
        foreach (var m in standardMacros)
        {
            var macroSym = Startup.Sym(m);
            var macroFn = new LispFunction(args => {
                Runtime.CheckArityExact("MACRO-EXPANDER", args, 2);
                var compilerFn = Startup.LookupCompilerMacro(macroSym);
                if (compilerFn != null)
                    return compilerFn.Invoke(new LispObject[] { args[0] });
                return args[0];
            }, $"MACRO-EXPANDER-{m}", 2);
            Runtime.RegisterMacroFunction(macroSym, macroFn);
        }

        // An equivalent macro definition (CLHS 3.1.2.1.2.2), evaluated via the quoted form.
        var definingMacros = new[] {
            "DEFUN", "DEFMACRO", "DEFVAR", "DEFPARAMETER", "DEFCONSTANT"
        };
        var evalWhenSym = Startup.Sym("EVAL-WHEN");
        var evalSym = Startup.Sym("EVAL");
        var quoteSym = Startup.Sym("QUOTE");
        var situations = new Cons(Startup.Keyword("COMPILE-TOPLEVEL"),
                          new Cons(Startup.Keyword("LOAD-TOPLEVEL"),
                          new Cons(Startup.Keyword("EXECUTE"), Nil.Instance)));
        foreach (var dm in definingMacros)
        {
            var dmSym = Startup.Sym(dm);
            var dmFn = new LispFunction(args => {
                Runtime.CheckArityExact("MACRO-EXPANDER", args, 2);
                if (dm is "DEFPARAMETER" or "DEFCONSTANT"
                    && !(args[0] is Cons f0 && f0.Cdr is Cons f1 && f1.Cdr is Cons))
                    throw new LispErrorException(new LispProgramError(
                        $"{dm}: missing the initial value form in {args[0]}"));
                var quoted = new Cons(quoteSym, new Cons(args[0], Nil.Instance));
                var apply = new Cons(evalSym, new Cons(quoted, Nil.Instance));
                return new Cons(evalWhenSym, new Cons(situations, new Cons(apply, Nil.Instance)));
            }, $"MACRO-EXPANDER-{dm}", 2);
            Runtime.RegisterMacroFunction(dmSym, dmFn);
        }

        // Proper standard-CL expanders for compile-form macros (WHEN, UNLESS, COND, AND, OR).
        // These are handled internally by the compiler but must expand via macroexpand-1
        // so that code walkers (e.g. iterate) can see their bodies.
        RegisterCompileFormExpanders();

        // CLHS: MACROEXPAND-1 invokes the expansion function through
        // *MACROEXPAND-HOOK*. The default hook FUNCALL is the fast path (call the
        // expander directly, no symbol-value read cost beyond the null check); a
        // rebound hook, walker guards, tracing, memoizing expanders, gets
        // (funcall hook expander form env). One-argument expanders (local
        // macrolet tables, compiler macros) are wrapped so the hook always sees
        // the standard two-argument calling convention.
        static LispFunction InvalidMacroexpandHook(LispObject value)
            => new LispFunction(_ => throw new LispErrorException(new LispTypeError(
                   "*MACROEXPAND-HOOK*: value is not a function designator", value)),
               "%INVALID-MACROEXPAND-HOOK", -1);
        static LispFunction? NonDefaultMacroexpandHook()
        {
            var hook = DynamicBindings.Get(Startup.Sym("*MACROEXPAND-HOOK*"));
            if (hook == null || hook is Nil)
                return InvalidMacroexpandHook(Nil.Instance);
            if (hook is Symbol hs)
                return hs.Name == "FUNCALL" ? null
                     : hs.Function as LispFunction ?? InvalidMacroexpandHook(hook);
            return hook as LispFunction ?? InvalidMacroexpandHook(hook);
        }
        static LispObject CallExpander(LispFunction expander, bool is2arg, LispObject form, LispObject env,
                                       bool honourHook = true)
        {
            var hook = !honourHook || _inMacroexpandHook ? null : NonDefaultMacroexpandHook();
            if (hook == null)
                return MultipleValues.Primary(is2arg
                    ? expander.Invoke(new LispObject[] { form, env })
                    : expander.Invoke(new LispObject[] { form }));
            var hookee = is2arg
                ? expander
                : new LispFunction(a => expander.Invoke(new[] { a[0] }), expander.Name, 2);
            var savedInHook = _inMacroexpandHook;
            _inMacroexpandHook = true;
            try
            {
                return MultipleValues.Primary(hook.Invoke(new LispObject[] { hookee, form, env }));
            }
            finally { _inMacroexpandHook = savedInHook; }
        }

        // MACROEXPAND-1
        Emitter.CilAssembler.RegisterFunction("MACROEXPAND-1",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"MACROEXPAND-1: wrong number of arguments: {args.Length} (expected 1-2)"));
                var form0 = args[0];
                var expanded0 = MacroexpandOnce(form0, args.Length > 1 ? args[1] : Nil.Instance,
                                                out bool wasMacro);
                return wasMacro
                    ? MultipleValues.Values2(expanded0, T.Instance)
                    : MultipleValues.Values(form0, Nil.Instance);
            }, "MACROEXPAND-1", -1));

        // Single-value MACROEXPAND-1 for the evaluator; it ignores *MACROEXPAND-HOOK*.
        // It is called on every compound form, so it returns only one value.
        Emitter.CilAssembler.RegisterFunction("%MACROEXPAND-1-OR-SELF",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"%MACROEXPAND-1-OR-SELF: wrong number of arguments: {args.Length} (expected 1-2)"));
                return MacroexpandOnceOrSelfWithoutHook(args[0], args.Length > 1 ? args[1] : Nil.Instance);
            }, "%MACROEXPAND-1-OR-SELF", -1));

        // The name compiled code finds a package by: the same in every package
        // unit, unlike PACKAGE-NAME.
        Emitter.CilAssembler.RegisterFunction("%PACKAGE-PHYSICAL-NAME",
            new LispFunction(args => {
                Runtime.CheckArityExact("%PACKAGE-PHYSICAL-NAME", args, 1);
                return ((Package)args[0]).NameString;
            }, "%PACKAGE-PHYSICAL-NAME", 1));

        Emitter.CilAssembler.RegisterFunction("%NEXT-LTV-ID",
            new LispFunction(args => {
                Runtime.CheckArityExact("%NEXT-LTV-ID", args, 0);
                return Fixnum.Make(System.Threading.Interlocked.Increment(ref _compilerLtvCounter));
            }, "%NEXT-LTV-ID", 0));

        // The tree-walk evaluator's memo of what it works out once per list: a
        // lambda list's shape, a HANDLER-CASE form's rewrite (see %MINI-SHAPE in
        // cil-forms-interp.lisp). Keyed weakly by the list itself. Read on every
        // interpreted call, from any thread, without a lock. NIL when absent.
        Emitter.CilAssembler.RegisterFunction("%MINI-MEMO",
            new LispFunction(args => {
                Runtime.CheckArityExact("%MINI-MEMO", args, 1);
                return _miniMemo.TryGetValue(args[0], out var box) && box.Value != null
                    ? box.Value : Nil.Instance;
            }, "%MINI-MEMO", 1));
        Emitter.CilAssembler.RegisterFunction("%SET-MINI-MEMO",
            new LispFunction(args => {
                Runtime.CheckArityExact("%SET-MINI-MEMO", args, 2);
                _miniMemo.GetValue(args[0], _ => new StrongBox<LispObject?>()).Value = args[1];
                return args[1];
            }, "%SET-MINI-MEMO", 2));

        // One macroexpansion step of FORM for the tree-walk evaluator, remembered
        // per form. The evaluator expanded a macro form every time it evaluated
        // it, so a macro inside a loop or a function body was expanded again on
        // every iteration and every call: a LOOP inside a function called 40,000
        // times was expanded 40,000 times. The evaluator calls this only where no
        // MACROLET or SYMBOL-MACROLET is in scope, so the expansion depends on the
        // global definitions alone, and, once a global symbol macro exists, on
        // whether the lexical variables in scope shadow one: an expansion is only
        // kept for every scope when neither the form nor its expansion mentions a
        // global symbol macro's name (%MINI-STORE-EXPANSION).
        //
        // A form that is not a macro form is remembered as such too: finding that
        // out reads the compiler's macro table under its lock, which every thread
        // evaluating a form waited on. A remembered answer is used while nothing
        // it was made from has changed: no global macro or symbol macro defined
        // or removed (MacroEpoch, and the compiler's symbol-macro table), no
        // change to the compiler's macro tables (MacroTablesVersion), and the
        // operator's function binding the same object (a function of that name
        // hides a compiler-table macro). *MACROEXPAND-HOOK* is not consulted:
        // the evaluator's expansion is not a call to MACROEXPAND-1.
        //
        // Returns the expansion, FORM itself when it is not a macro form, or
        // MiniExpansionMiss when a global symbol macro exists and the expansion
        // has to be made in the caller's environment; the caller then makes it
        // and hands it to %MINI-STORE-EXPANSION.
        Emitter.CilAssembler.RegisterFunction("%MINI-MACROEXPAND-1-CACHED",
            new LispFunction(args => {
                Runtime.CheckArityExact("%MINI-MACROEXPAND-1-CACHED", args, 1);
                var form = args[0];
                if (form is not Cons cons || cons.Car is not Symbol sym
                    || !Compat.TryEnsureSufficientExecutionStack())
                    return MiniExpandUncached(form);
                var tokens = MiniExpansionTokens.Current(sym);
                if (tokens.Usable && _miniExpansions.TryGetValue(cons, out var box))
                {
                    var e = box.Entry;
                    if (e != null && e.MadeFrom(tokens) && e.EnvFree)
                        return e.Expansion;
                }
                var expander = Runtime.MacroFunction(sym) as LispFunction
                               ?? Startup.LookupCompilerMacroAsMacro(sym);
                if (expander != null && tokens.GlobalSymbolMacros)
                    return MiniExpansionMiss;
                var expanded = expander == null ? form : MacroexpandOnceOrSelfWithoutHook(form, new Cons(Nil.Instance, Nil.Instance));
                if (tokens.Usable)
                    _miniExpansions.GetValue(cons, _ => new MiniExpansionBox()).Entry
                        = new MiniExpansion(tokens, true, expanded);
                return expanded;
            }, "%MINI-MACROEXPAND-1-CACHED", 1));

        // (%MINI-STORE-EXPANSION form expansion): remember EXPANSION, made in the
        // caller's environment after %MINI-MACROEXPAND-1-CACHED answered
        // MiniExpansionMiss, and return it. It serves every later evaluation of
        // FORM only when neither FORM nor EXPANSION mentions a global symbol
        // macro; otherwise the next evaluation expands again, as every one did.
        Emitter.CilAssembler.RegisterFunction("%MINI-STORE-EXPANSION",
            new LispFunction(args => {
                Runtime.CheckArityExact("%MINI-STORE-EXPANSION", args, 2);
                var expanded = args[1];
                if (args[0] is Cons cons && cons.Car is Symbol sym)
                {
                    var tokens = MiniExpansionTokens.Current(sym);
                    if (tokens.Usable)
                    {
                        bool envFree = !MentionsGlobalSymbolMacro(cons, tokens.GsmTable)
                                       && !MentionsGlobalSymbolMacro(expanded, tokens.GsmTable);
                        _miniExpansions.GetValue(cons, _ => new MiniExpansionBox()).Entry
                            = new MiniExpansion(tokens, envFree, expanded);
                    }
                }
                return expanded;
            }, "%MINI-STORE-EXPANSION", 2));

        Emitter.CilAssembler.RegisterFunction("%MINI-EXPANSION-MISS",
            new LispFunction(args => {
                Runtime.CheckArityExact("%MINI-EXPANSION-MISS", args, 0);
                return MiniExpansionMiss;
            }, "%MINI-EXPANSION-MISS", 0));

        static LispObject MiniExpandUncached(LispObject form) => MacroexpandOnceOrSelfWithoutHook(form, new Cons(Nil.Instance, Nil.Instance));

        // The symbol-macro table of an environment object: SYMBOL-MACROLET
        // bindings keyed by the symbol itself (a same-named symbol of another
        // package, or a keyword, is a different name). A lexical variable that
        // shadows a symbol macro (a global one, or a SYMBOL-MACROLET further
        // out) is entered as (MARKER . VARIABLE), MARKER being the value of the
        // compiler's *SYMBOL-MACRO-SHADOW-MARKER*: keyed by the symbol, or by
        // its name string when the compiler's analysis walk only kept the name.
        // Returns 1 with the expansion, 2 when SYM is a variable there (no
        // expansion, and no global symbol macro either), 0 when not in the table.
        static int LookupEnvSymbolMacro(LispHashTable ht, Symbol sym, out LispObject expansion)
        {
            if (ht.TryGet(sym, out expansion!))
                return IsShadowEntry(expansion) ? 2 : 1;
            if (ht.TryGet(new LispString(sym.Name), out var byName) && IsShadowEntry(byName))
                return 2;
            return 0;
        }

        static bool IsShadowEntry(LispObject entry)
        {
            if (entry is not Cons c) return false;
            var marker = _symbolMacroShadowMarker;
            if (marker == null)
            {
                foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
                {
                    var pkg = Package.FindPackage(pkgName);
                    if (pkg == null) continue;
                    var (markerSym, status) = pkg.FindSymbol("*SYMBOL-MACRO-SHADOW-MARKER*");
                    if (markerSym != null && status != SymbolStatus.None && markerSym.IsBound)
                    {
                        marker = markerSym.Value;
                        _symbolMacroShadowMarker = marker;
                        break;
                    }
                }
                if (marker == null) return false;
            }
            return ReferenceEquals(c.Car, marker);
        }

        // The evaluator's own expansion must not be affected by *MACROEXPAND-HOOK*.
        static LispObject MacroexpandOnceOrSelfWithoutHook(LispObject form, LispObject env)
            => MacroexpandOnce(form, env, out _, false);

        // WAS-MACRO answers MACROEXPAND-1 even when the expander returns FORM unchanged.
        static LispObject MacroexpandOnce(LispObject form, LispObject env, out bool wasMacro,
                                          bool honourHook = true)
        {
                wasMacro = false;
                if (!Compat.TryEnsureSufficientExecutionStack())
                    return form; // bail out: return unexpanded
                // Extract macro table and symbol-macro table from env
                // env can be: LispHashTable (macros only), Cons (macros . symbol-macros-ht), or Nil.
                // The macros part is a hash table keyed by name, or the alist of
                // lexical operator bindings (see LexicalOperator).
                LispHashTable? macroHt = null;
                LispHashTable? symMacroHt = null;
                Cons? lexOps = null;
                if (env is LispHashTable ht) macroHt = ht;
                else if (env is Cons envCons)
                {
                    if (envCons.Car is LispHashTable mht) macroHt = mht;
                    else if (envCons.Car is Cons ops) lexOps = ops;
                    if (envCons.Cdr is LispHashTable sht) symMacroHt = sht;
                }
                // Symbol-macro expansion: form is a symbol
                if (form is Symbol symForm && symForm.Name != "NIL" && symForm.Name != "T")
                {
                    if (symMacroHt != null)
                    {
                        int found = LookupEnvSymbolMacro(symMacroHt, symForm, out var expansion);
                        if (found == 2) return form;
                        if (found == 1)
                        {
                            wasMacro = true;
                            return expansion;
                        }
                    }
                    // Check global symbol macros (DEFINE-SYMBOL-MACRO)
                    if (Runtime.TryGetGlobalSymbolMacro(symForm, out var gsmExpansion))
                    {
                        wasMacro = true;
                        return gsmExpansion;
                    }
                }
                // Macro expansion: form is a cons
                if (form is Cons cons && cons.Car is Symbol sym)
                {
                    if (lexOps != null && LexicalOperator(lexOps, sym) is Cons binding)
                    {
                        // A local function hides every macro of its name.
                        if (binding.Cdr is not LispFunction lfn)
                            return form;
                        wasMacro = true;
                        return CallExpander(lfn, true, form, env, honourHook);
                    }
                    if (macroHt != null)
                    {
                        var key = new LispString(sym.Name);
                        if (macroHt.TryGet(key, out var expander) && expander is LispFunction fn)
                        {
                            wasMacro = true;
                            return CallExpander(fn, false, form, env, honourHook);
                        }
                    }
                    var runtimeMacroFn = Runtime.MacroFunction(sym);
                    if (runtimeMacroFn is LispFunction rmf)
                    {
                        wasMacro = true;
                        return CallExpander(rmf, true, form, env, honourHook);
                    }
                    var compilerFn = Startup.LookupCompilerMacroAsMacro(sym);
                    if (compilerFn != null)
                    {
                        wasMacro = true;
                        return CallExpander(compilerFn, false, form, env, honourHook);
                    }
                }
                return form;
        }

        // The binding of SYM in the lexical operator alist of an environment
        // (innermost first): (NAME . EXPANDER) for a MACROLET, (NAME) for an
        // FLET / LABELS function that hides a macro of that name. Null when the
        // alist does not bind SYM, so the global tables decide.
        static Cons? LexicalOperator(Cons alist, Symbol sym)
        {
            for (LispObject o = alist; o is Cons c; o = c.Cdr)
                if (c.Car is Cons e && ReferenceEquals(e.Car, sym))
                    return e;
            return null;
        }

        // %CALL-WITH-HANDLER-CLUSTER: handler-bind primitive for the emit-free
        // tree-walk interpreter (%mini-eval). (alist thunk) -> establishes the
        // cluster, runs thunk under it. Mirrors compile-handler-bind.
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-HANDLER-CLUSTER",
            new LispFunction(Runtime.CallWithHandlerCluster, "%CALL-WITH-HANDLER-CLUSTER", 2));
        // %CALL-WITH-HANDLER-CASE: the same for handler-case, which also takes an
        // unsignalled LispErrorException by type, as compile-handler-case does.
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-HANDLER-CASE",
            new LispFunction(Runtime.CallWithHandlerCase, "%CALL-WITH-HANDLER-CASE", 2));
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-RESTART-CLUSTER",
            new LispFunction(Runtime.CallWithRestartCluster, "%CALL-WITH-RESTART-CLUSTER", 2));
        Emitter.CilAssembler.RegisterFunction("%CALL-WITH-RESTART-BIND",
            new LispFunction(Runtime.CallWithRestartBind, "%CALL-WITH-RESTART-BIND", 2));
        Emitter.CilAssembler.RegisterFunction("%PUSH-HANDLER-CLUSTER",
            new LispFunction(a => Runtime.PushHandlerCluster(a[0]), "%PUSH-HANDLER-CLUSTER", 1));
        Emitter.CilAssembler.RegisterFunction("%POP-HANDLER-CLUSTER",
            new LispFunction(a => Runtime.PopHandlerCluster(), "%POP-HANDLER-CLUSTER", 0));
        Emitter.CilAssembler.RegisterFunction("%PUSH-RESTART-CLUSTER",
            new LispFunction(a => Runtime.PushRestartCluster(a[0]), "%PUSH-RESTART-CLUSTER", 1));
        Emitter.CilAssembler.RegisterFunction("%POP-RESTART-CLUSTER",
            new LispFunction(a => Runtime.PopRestartCluster(), "%POP-RESTART-CLUSTER", 0));

        // MACROEXPAND
        Emitter.CilAssembler.RegisterFunction("MACROEXPAND",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2) throw new LispErrorException(new LispProgramError($"MACROEXPAND: wrong number of arguments: {args.Length} (expected 1-2)"));
                if (!Compat.TryEnsureSufficientExecutionStack())
                    return MultipleValues.Values2(args[0], Nil.Instance); // bail out: return unexpanded
                var form = args[0];
                var env = args.Length > 1 ? args[1] : Nil.Instance;
                // Extract macro table and symbol-macro table from env
                LispHashTable? macroHt = null;
                LispHashTable? symMacroHt = null;
                Cons? lexOps = null;
                if (env is LispHashTable ht) macroHt = ht;
                else if (env is Cons envCons)
                {
                    if (envCons.Car is LispHashTable mht) macroHt = mht;
                    else if (envCons.Car is Cons ops) lexOps = ops;
                    if (envCons.Cdr is LispHashTable sht) symMacroHt = sht;
                }
                bool anyExpanded = false;
                for (int i = 0; i < 1000; i++)
                {
                    // Symbol-macro expansion
                    if (form is Symbol symForm && symForm.Name != "NIL" && symForm.Name != "T")
                    {
                        if (symMacroHt != null)
                        {
                            int found = LookupEnvSymbolMacro(symMacroHt, symForm, out var expansion);
                            if (found == 2) break;
                            if (found == 1)
                            {
                                form = expansion;
                                anyExpanded = true;
                                continue;
                            }
                        }
                        // Check global symbol macros (DEFINE-SYMBOL-MACRO)
                        if (Runtime.TryGetGlobalSymbolMacro(symForm, out var gsmExpansion))
                        {
                            form = gsmExpansion;
                            anyExpanded = true;
                            continue;
                        }
                        break;
                    }
                    // Macro expansion
                    if (form is Cons cons && cons.Car is Symbol sym)
                    {
                        LispFunction? fn = null;
                        bool is2arg = false;
                        if (lexOps != null && LexicalOperator(lexOps, sym) is Cons binding)
                        {
                            // A local function hides every macro of its name.
                            if (binding.Cdr is not LispFunction lfn)
                                break;
                            fn = lfn;
                            is2arg = true;
                        }
                        if (fn == null && macroHt != null)
                        {
                            var key = new LispString(sym.Name);
                            if (macroHt.TryGet(key, out var expander) && expander is LispFunction f)
                                fn = f;
                        }
                        if (fn == null)
                        {
                            var rmf = Runtime.MacroFunction(sym);
                            if (rmf is LispFunction rf) { fn = rf; is2arg = true; }
                        }
                        if (fn == null)
                            fn = Startup.LookupCompilerMacroAsMacro(sym);
                        if (fn != null)
                        {
                            var prevForm = form;
                            form = CallExpander(fn, is2arg, form, env);
                            anyExpanded = true;
                            // Fix-point check: a macro may return its argument unchanged.
                            if (ReferenceEquals(form, prevForm) || Runtime.Equal(form, prevForm) is not Nil)
                                break;
                            continue;
                        }
                    }
                    break;
                }
                return MultipleValues.Values2(form, anyExpanded ? (LispObject)T.Instance : Nil.Instance);
            }, "MACROEXPAND", -1));

        // FBOUNDP. Body extracted to FboundpImpl so the 1-arg direct delegate and
        // the args-array wrapper share the exact same code path; argc != 1 still
        // falls back to the wrapper's wrong-number-of-arguments error.
        var fboundpFn = new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"FBOUNDP: wrong number of arguments: {args.Length} (expected 1)"));
            return FboundpImpl(args[0]);
        }, "FBOUNDP", -1);
        fboundpFn.SetDirectDelegate((Func<LispObject, LispObject>)FboundpImpl);
        Emitter.CilAssembler.RegisterFunction("FBOUNDP", fboundpFn);

        // COMPILE
        Emitter.CilAssembler.RegisterFunction("COMPILE",
            new LispFunction(args => {
                if (args.Length < 1 || args.Length > 2)
                    throw new LispErrorException(new LispProgramError($"COMPILE: wrong number of arguments: {args.Length} (expected 1-2)"));
                // (compile name lambda-expression): compile the lambda, and when NAME is
                // non-nil install the result as NAME's definition, returning NAME (CLHS;
                // "compile replaces the function definition of name"). fiveam's run-time
                // test compilation relies on this side effect.
                var name = args[0];
                if (!(name is Nil || name is Symbol
                      || (name is Cons nc && nc.Car is Symbol ns && ns.Name == "SETF"
                          && nc.Cdr is Cons nt && nt.Car is Symbol && nt.Cdr is Nil)))
                    throw new LispErrorException(new LispTypeError(
                        "COMPILE: not a function name", name,
                        Runtime.List(Startup.Sym("OR"), Startup.Sym("SYMBOL"), Startup.Sym("CONS"))));
                if (args.Length > 1 && args[1] is not LispFunction
                    && !(args[1] is Cons dc && dc.Car == Startup.Sym("LAMBDA")))
                    throw new LispErrorException(new LispTypeError(
                        "COMPILE: definition is not a lambda expression or a function", args[1],
                        Runtime.List(Startup.Sym("OR"), Startup.Sym("FUNCTION"), Startup.Sym("CONS"))));
                if (args.Length > 1 && args[1] is not Nil)
                {
                    // Warnings-p / failure-p (CLHS COMPILE): a WARNING signaled while
                    // compiling -- e.g. by a macro's expander -- makes warnings-p true,
                    // and one that is not a STYLE-WARNING makes failure-p true too. The
                    // handler only records it and declines, so the warning is still
                    // reported as before.
                    bool warningsP = false, failureP = false;
                    var warningHandler = CompilerWarningHandler(full =>
                    {
                        warningsP = true;
                        if (full) failureP = true;
                    });
                    LispObject fn;
                    var errorsSym = Startup.Sym("*COMPILE-ERRORS-DETECTED*");
                    var ltvEvalNowSym = Startup.Sym("*LTV-EVAL-NOW*");
                    DynamicBindings.Push(errorsSym, Nil.Instance);
                    // COMPILE evaluates LOAD-TIME-VALUE forms while compiling.
                    DynamicBindings.Push(ltvEvalNowSym, T.Instance);
                    HandlerClusterStack.PushCluster(new HandlerBinding[] {
                        new HandlerBinding(Startup.Sym("WARNING"), warningHandler) });
                    BeginCompilerUnit(null);
                    SetCompilerTopLevel(args[1], 0);
                    try
                    {
                        var instrList = Runtime.CompileTopLevel(args[1],
                            new Cons(Startup.Keyword("COMPILE"), new Cons(args[0], Nil.Instance)));
                        fn = Emitter.CilAssembler.AssembleAndRun(instrList);
                        FlushCompilerWarnings();
                        if (IsTruthy(DynamicBindings.Get(errorsSym))) warningsP = failureP = true;
                    }
                    finally
                    {
                        EndCompilerUnit();
                        HandlerClusterStack.PopCluster();
                        DynamicBindings.Pop(ltvEvalNowSym);
                        DynamicBindings.Pop(errorsSym);
                    }
                    LispObject wP = warningsP ? T.Instance : Nil.Instance;
                    LispObject fP = failureP ? T.Instance : Nil.Instance;
                    if (args[0] is Nil)
                        return MultipleValues.Values(fn, wP, fP);
                    if (fn is LispFunction lfn)
                    {
                        if (args[0] is Symbol nameSym)
                        {
                            if (MacroFunction(nameSym) is not Nil)
                            {
                                RegisterMacroFunctionRt(nameSym, lfn);
                                return MultipleValues.Values(nameSym, wP, fP);
                            }
                            Emitter.CilAssembler.RegisterFunctionOnSymbol(nameSym, lfn);
                            return MultipleValues.Values(nameSym, wP, fP);
                        }
                        // (setf NAME) function-name
                        if (args[0] is Cons fc && fc.Car is Symbol setfSym && setfSym.Name == "SETF"
                            && fc.Cdr is Cons tc && tc.Car is Symbol target)
                        {
                            Emitter.CilAssembler.RegisterSetfFunctionOnSymbol(target, lfn);
                            return MultipleValues.Values(args[0], wP, fP);
                        }
                    }
                    return MultipleValues.Values(args[0], wP, fP);
                }
                // (compile name): NAME's existing definition is already compiled in dotcl;
                // just return the designator.
                if (args[0] is Symbol sym)
                {
                    return MultipleValues.Values(sym, Nil.Instance, Nil.Instance);
                }
                return MultipleValues.Values(args[0], Nil.Instance, Nil.Instance);
            }));
        Emitter.CilAssembler.RegisterFunction("COMPILE-FILE",
            new LispFunction(args => Runtime.CompileFile(args)));

        // LOAD
        Emitter.CilAssembler.RegisterFunction("LOAD",
            new LispFunction(args => Runtime.Load(args)));

        // BREAK lives in Runtime.RegisterConditionsBuiltins, which runs later and
        // owns it. Y-OR-N-P and YES-OR-NO-P are defined in Lisp (cil-stdlib),
        // where they read the answer from *QUERY-IO*.

        // INVALID-METHOD-ERROR, METHOD-COMBINATION-ERROR
        // Both signal a SIMPLE-ERROR whose format control and arguments are the
        // caller's, so the reason given by the method combination is reported.
        Emitter.CilAssembler.RegisterFunction("INVALID-METHOD-ERROR",
            new LispFunction(args => {
                if (args.Length < 2)
                    throw new LispErrorException(new LispProgramError(
                        "INVALID-METHOD-ERROR: expected a method and a format control"));
                return MethodCombinationSimpleError(
                    "invalid method error for " + FormatObject(args[0], true) + ": ",
                    args.SubArray(1));
            }, "INVALID-METHOD-ERROR", -1));
        Emitter.CilAssembler.RegisterFunction("METHOD-COMBINATION-ERROR",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError(
                        "METHOD-COMBINATION-ERROR: expected a format control"));
                return MethodCombinationSimpleError("method combination error: ", args);
            }, "METHOD-COMBINATION-ERROR", -1));

        // MAKE-LOAD-FORM-SAVING-SLOTS
        Emitter.CilAssembler.RegisterFunction("MAKE-LOAD-FORM-SAVING-SLOTS",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM-SAVING-SLOTS: wrong number of arguments: 0 (expected at least 1)"));
                var obj = args[0];
                bool slotNamesSupplied = false;
                LispObject slotNamesList = Nil.Instance;
                bool allowOtherKeys = false, allowOtherKeysSeen = false;
                for (int i = 1; i < args.Length; i += 2)
                {
                    if (i + 1 > args.Length)
                        throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM-SAVING-SLOTS: odd number of keyword arguments"));
                    if (args[i] is Symbol keySym)
                    {
                        if (keySym.Name == "SLOT-NAMES")
                        {
                            if (i + 1 >= args.Length)
                                throw new LispErrorException(new LispProgramError("MAKE-LOAD-FORM-SAVING-SLOTS: missing value for :SLOT-NAMES"));
                            if (!slotNamesSupplied)
                            {
                                slotNamesSupplied = true;
                                slotNamesList = args[i + 1];
                            }
                        }
                        else if (keySym.Name == "ENVIRONMENT")
                        {
                            // ignored
                        }
                        else if (keySym.Name == "ALLOW-OTHER-KEYS")
                        {
                            if (!allowOtherKeysSeen && i + 1 < args.Length)
                            {
                                allowOtherKeys = args[i + 1] != Nil.Instance;
                                allowOtherKeysSeen = true;
                            }
                        }
                        else if (!allowOtherKeys)
                        {
                            throw new LispErrorException(new LispProgramError($"MAKE-LOAD-FORM-SAVING-SLOTS: unknown keyword argument {keySym}"));
                        }
                    }
                    else
                    {
                        throw new LispErrorException(new LispProgramError($"MAKE-LOAD-FORM-SAVING-SLOTS: invalid keyword {args[i]}"));
                    }
                }

                string className;
                LispClass? cls = null;
                if (obj is LispInstance inst)
                {
                    cls = inst.Class;
                    className = cls.Name.Name;
                }
                else if (obj is LispInstanceCondition lic)
                {
                    cls = lic.Instance.Class;
                    className = cls.Name.Name;
                }
                else if (LispStruct.TryAs(obj, out var ls))
                {
                    className = ls.TypeName.Name;
                    cls = Runtime.FindClassOrNil(ls.TypeName) as LispClass;
                }
                else
                {
                    throw new LispErrorException(new LispTypeError("MAKE-LOAD-FORM-SAVING-SLOTS: not a standard-object, structure-object, or condition", obj));
                }

                var findClassSym = Startup.Sym("FIND-CLASS");
                var allocInstSym = Startup.Sym("ALLOCATE-INSTANCE");
                var quoteSym = Startup.Sym("QUOTE");
                // Use the class's own name symbol to preserve package identity
                var classNameSym = cls!.Name;
                var findClassForm = new Cons(findClassSym, new Cons(new Cons(quoteSym, new Cons(classNameSym, Nil.Instance)), Nil.Instance));
                var creationForm = new Cons(allocInstSym, new Cons(findClassForm, Nil.Instance));

                var setfForms = new System.Collections.Generic.List<LispObject>();
                var slotValueSym = Startup.Sym("SLOT-VALUE");
                var setfSym = Startup.Sym("SETF");

                if (LispStruct.TryAs(obj, out var structObj))
                {
                    if (cls != null && cls.StructSlotNames != null)
                    {
                        for (int i = 0; i < cls.StructSlotNames.Length && i < structObj.SlotCount; i++)
                        {
                            var slotSym = cls.StructSlotNames[i];
                            if (slotNamesSupplied && !MlfsListContainsSymbol(slotNamesList, slotSym))
                                continue;
                            var slotVal = structObj.GetSlot(i);
                            var svForm = new Cons(slotValueSym, new Cons(obj, new Cons(new Cons(quoteSym, new Cons(slotSym, Nil.Instance)), Nil.Instance)));
                            var setfForm = new Cons(setfSym, new Cons(svForm, new Cons(new Cons(quoteSym, new Cons(slotVal, Nil.Instance)), Nil.Instance)));
                            setfForms.Add(setfForm);
                        }
                    }
                }
                else
                {
                    var theInst = obj is LispInstanceCondition lic2 ? lic2.Instance : (LispInstance)obj;
                    theInst.EnsureCurrent();
                    var theCls = theInst.Class;
                    for (int i = 0; i < theCls.EffectiveSlots.Length; i++)
                    {
                        var slotDef = theCls.EffectiveSlots[i];
                        if (slotNamesSupplied && !MlfsListContainsSymbol(slotNamesList, slotDef.Name))
                            continue;
                        if (theInst.Slots[i] == null) continue;
                        var slotVal = theInst.Slots[i]!;
                        var svForm = new Cons(slotValueSym, new Cons(obj, new Cons(new Cons(quoteSym, new Cons(slotDef.Name, Nil.Instance)), Nil.Instance)));
                        var setfForm = new Cons(setfSym, new Cons(svForm, new Cons(new Cons(quoteSym, new Cons(slotVal, Nil.Instance)), Nil.Instance)));
                        setfForms.Add(setfForm);
                    }
                }

                LispObject initForm;
                if (setfForms.Count == 0)
                {
                    initForm = Nil.Instance;
                }
                else
                {
                    var prognSym = Startup.Sym("PROGN");
                    LispObject body = Nil.Instance;
                    for (int i = setfForms.Count - 1; i >= 0; i--)
                        body = new Cons(setfForms[i], body);
                    initForm = new Cons(prognSym, body);
                }

                return MultipleValues.Values2(creationForm, initForm);
            }, "MAKE-LOAD-FORM-SAVING-SLOTS", -1));

        // COMPILER-MACRO-FUNCTION: look up runtime-registered compiler macros.
        // Body extracted to CompilerMacroFunctionImpl so the 1-arg direct delegate
        // and the args-array wrapper share the same code path (the optional env
        // argument was already ignored by the wrapper).
        var cmfFn = new LispFunction(args => {
            if (args.Length < 1) return Nil.Instance;
            // A local function or macro of the same name in the environment
            // shadows the global compiler macro (CLHS 3.2.2.1): answer NIL, as
            // MACRO-FUNCTION does for a name under a local function.
            if (args.Length == 2 && args[1] is Cons cenv && cenv.Car is Cons cops
                && args[0] is Symbol csym)
            {
                for (LispObject o = cops; o is Cons oc; o = oc.Cdr)
                    if (oc.Car is Cons ce && ReferenceEquals(ce.Car, csym))
                        return Nil.Instance;
            }
            return CompilerMacroFunctionImpl(args[0]);
        }, "COMPILER-MACRO-FUNCTION", -1).WithArgCount(1, 2);
        cmfFn.SetDirectDelegate((Func<LispObject, LispObject>)CompilerMacroFunctionImpl);
        Emitter.CilAssembler.RegisterFunction("COMPILER-MACRO-FUNCTION", cmfFn);

        // %REGISTER-COMPILER-MACRO-RT: store a compiler macro function at runtime.
        Startup.RegisterBinary("%REGISTER-COMPILER-MACRO-RT", (nameObj, fn) => {
            if (fn is not Nil && fn is not LispFunction)
                throw new LispErrorException(new LispTypeError(
                    "COMPILER-MACRO-FUNCTION: new value is not a compiler macro function or NIL", fn));
            // (setf foo) form: a cons (setf . (foo . nil))
            if (nameObj is Cons setfCons && setfCons.Car is Symbol setfSym && setfSym.Name == "SETF") {
                var innerSym = (setfCons.Cdr is Cons c2) ? c2.Car as Symbol : null;
                if (innerSym != null) {
                    if (fn is LispFunction lf2) _setfCompilerMacros[innerSym] = lf2;
                    else _setfCompilerMacros.TryRemove(innerSym, out _);   // (setf (compiler-macro-function '(setf foo)) nil)
                }
                return fn;
            }
            var sym = Runtime.GetSymbol(nameObj, "%REGISTER-COMPILER-MACRO-RT");
            if (fn is LispFunction lf)
                _compilerMacroFunctions[sym] = lf;
            else
                _compilerMacroFunctions.TryRemove(sym, out _);   // (setf (compiler-macro-function 'foo) nil) removes it (CLHS)
            return fn;
        });

        // %DOTNET-METHOD-RETURN-TYPE: compile-time return-type resolution for the
        // typed-return inference of the DOTNET:INVOKE compiler macro.
        Emitter.CilAssembler.RegisterFunction("%DOTNET-METHOD-RETURN-TYPE",
            new LispFunction(Runtime.DotNetMethodReturnType, "%DOTNET-METHOD-RETURN-TYPE", -1));

        // %REGISTER-MACRO-FUNCTION-RT, %UNREGISTER-MACRO-FUNCTION-RT
        // First arg is a Symbol (package-aware macro registration)
        Startup.RegisterBinary("%REGISTER-MACRO-FUNCTION-RT", (nameObj, fn) =>
            RegisterMacroFunctionRt(Runtime.GetSymbol(nameObj, "%REGISTER-MACRO-FUNCTION-RT"), fn));
        // COMPILE-FILE is about to define the macro NAME again: evaluate the
        // DEFUN stand-ins that mention it first (DeferredDefun.ForcePendingMentioning).
        // A DEFMACRO's expander made at compile time (see Runtime.EvalAtCompileTime).
        Startup.RegisterUnary("%EVAL-AT-COMPILE-TIME", Runtime.EvalAtCompileTime);
        Startup.RegisterUnary("%FORCE-DEFERRED-DEFUNS-MENTIONING", nameObj => {
            if (nameObj is Symbol fsym) DeferredDefun.ForcePendingMentioning(fsym);
            return Nil.Instance;
        });
        Startup.RegisterUnary("%UNREGISTER-MACRO-FUNCTION-RT", nameObj => {
            var sym = Runtime.GetSymbol(nameObj, "%UNREGISTER-MACRO-FUNCTION-RT");
            var name = sym.Name;
            var pkg = sym.HomePackage;
            var key = (pkg == null || pkg.Name is "COMMON-LISP" or "CL")
                ? name
                : $"{pkg.Name}:{name}";
            Runtime.UnregisterMacroFunction(sym);
            foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
            {
                var pkgObj = Package.FindPackage(pkgName);
                if (pkgObj != null)
                {
                    var (macrosSym, status) = pkgObj.FindSymbol("*MACROS*");
                    if (macrosSym != null && status != SymbolStatus.None
                        && macrosSym.Value is LispHashTable macrosTable)
                    {
                        macrosTable.Remove(sym);
                        break;
                    }
                }
            }
            return Nil.Instance;
        });

        // %REGISTER-SYMBOL-MACRO-RT: register a global symbol macro at load time
        Startup.RegisterBinary("%REGISTER-SYMBOL-MACRO-RT", (nameObj, expansion) => {
            var sym = Runtime.GetSymbol(nameObj, "%REGISTER-SYMBOL-MACRO-RT");
            Runtime.RegisterGlobalSymbolMacro(sym, expansion);
            // Also register in compiler's *global-symbol-macros* if available
            foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
            {
                var pkgObj = Package.FindPackage(pkgName);
                if (pkgObj != null)
                {
                    var (gsmSym, status) = pkgObj.FindSymbol("*GLOBAL-SYMBOL-MACROS*");
                    if (gsmSym != null && status != SymbolStatus.None
                        && gsmSym.Value is LispHashTable gsmTable)
                    {
                        gsmTable.Set(sym, expansion);
                        break;
                    }
                }
            }
            return sym;
        });

        // Load-time-value slot access (legacy global slots)
        Startup.RegisterUnary("%HAS-LTV-SLOT", id => {
            var slotId = (int)((Fixnum)id).Value;
            return Runtime.HasLtvSlot(slotId) ? T.Instance : Nil.Instance;
        });
        Startup.RegisterUnary("%GET-LTV-SLOT", id => {
            var slotId = (int)((Fixnum)id).Value;
            return Runtime.GetLtvSlot(slotId);
        });
        Startup.RegisterBinary("%SET-LTV-SLOT", (id, value) => {
            var slotId = (int)((Fixnum)id).Value;
            Runtime.SetLtvSlot(slotId, value);
            return value;
        });

        // Load-time-value slot access (per-module namespaced: prevents cross-run collisions)
        var hasLtvIn = new LispFunction(args => {
            var moduleId = ((LispString)args[0]).Value;
            var slotId = (int)((Fixnum)args[1]).Value;
            return Runtime.HasLtvSlotIn(moduleId, slotId) ? T.Instance : Nil.Instance;
        }, "%HAS-LTV-SLOT-IN");
        // Every LOAD-TIME-VALUE evaluation makes these two calls; without the
        // two-argument entries each one built an argument array.
        hasLtvIn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)((m, i) =>
            Runtime.HasLtvSlotIn(((LispString)m).Value, (int)((Fixnum)i).Value) ? T.Instance : Nil.Instance));
        Emitter.CilAssembler.RegisterFunction("%HAS-LTV-SLOT-IN", hasLtvIn);
        var getLtvIn = new LispFunction(args => {
            var moduleId = ((LispString)args[0]).Value;
            var slotId = (int)((Fixnum)args[1]).Value;
            return Runtime.GetLtvSlotIn(moduleId, slotId);
        }, "%GET-LTV-SLOT-IN");
        getLtvIn.SetDirectDelegate((Func<LispObject, LispObject, LispObject>)((m, i) =>
            Runtime.GetLtvSlotIn(((LispString)m).Value, (int)((Fixnum)i).Value)));
        Emitter.CilAssembler.RegisterFunction("%GET-LTV-SLOT-IN", getLtvIn);
        Emitter.CilAssembler.RegisterFunction("%SET-LTV-SLOT-IN", new LispFunction(args => {
            var moduleId = ((LispString)args[0]).Value;
            var slotId = (int)((Fixnum)args[1]).Value;
            Runtime.SetLtvSlotIn(moduleId, slotId, args[2]);
            return args[2];
        }, "%SET-LTV-SLOT-IN"));

        // Time functions
        Emitter.CilAssembler.RegisterFunction("GET-UNIVERSAL-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-UNIVERSAL-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            var epoch = new System.DateTime(1900, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            var now = System.DateTime.UtcNow;
            long secs = (long)(now - epoch).TotalSeconds;
            return Fixnum.Make(secs);
        }));
        Emitter.CilAssembler.RegisterFunction("GET-INTERNAL-RUN-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-INTERNAL-RUN-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            // TimeSpan ticks are 100 ns; internal time units are microseconds.
            return Fixnum.Make(System.Diagnostics.Process.GetCurrentProcess().TotalProcessorTime.Ticks / 10L);
        }));
        Emitter.CilAssembler.RegisterFunction("GET-INTERNAL-REAL-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-INTERNAL-REAL-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            return Fixnum.Make(Compat.MonotonicMicroseconds());
        }));
        Startup.RegisterUnary("SLEEP", obj => {
            double secs = Arithmetic.ToDouble(Runtime.AsNumber(obj));
            if (secs < 0) throw new LispErrorException(new LispTypeError("SLEEP: negative argument", obj));
            bool interrupted = false;
            var self = Runtime.EnterInterruptibleWait();
            try
            {
                System.Threading.Thread.Sleep((int)(secs * 1000));
            }
            catch (System.Threading.ThreadInterruptedException) { interrupted = true; }
            finally { Runtime.LeaveInterruptibleWait(self); }
            // INTERRUPT-THREAD reached us: run what it queued and cut the sleep
            // short. Nothing queued means DESTROY-THREAD: end the thread.
            if (interrupted && !Runtime.RunPendingInterrupts())
                throw new System.Threading.ThreadInterruptedException();
            return Nil.Instance;
        });
        Emitter.CilAssembler.RegisterFunction("DECODE-UNIVERSAL-TIME", new LispFunction(args => {
            if (args.Length < 1 || args.Length > 2)
                throw new LispErrorException(new LispProgramError("DECODE-UNIVERSAL-TIME: expected 1-2 args"));
            long ut = UniversalTimeInteger("DECODE-UNIVERSAL-TIME", args[0], 0);
            if (args.Length == 2 && args[1] is not Nil) CheckTimeZone("DECODE-UNIVERSAL-TIME", args[1]);
            var epoch = new System.DateTime(1900, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            // The calendar here covers years 1 to 9999; a time outside it has no decoding.
            if (ut < -59_000_000_000L || ut > 255_000_000_000L)
                throw new LispErrorException(new LispTypeError(
                    "DECODE-UNIVERSAL-TIME: the time is outside the years 1 to 9999", args[0], Startup.Sym("INTEGER")));
            var dtUtc = epoch.AddSeconds(ut);
            System.DateTime dt;
            LispObject daylightP;
            LispObject zone;
            // A NIL time zone means none was given (the current one), as in SBCL:
            // libraries pass their own optional TIME-ZONE argument straight through.
            if (args.Length == 2 && args[1] is not Nil) {
                var tzNum = Runtime.AsNumber(args[1]);
                long offsetSeconds = (long)System.Math.Round(Arithmetic.ToDouble(Arithmetic.Multiply(tzNum, new Fixnum(3600))));
                dt = epoch.AddSeconds(ut - offsetSeconds);
                daylightP = Nil.Instance;
                zone = args[1];
            } else {
                dt = dtUtc.ToLocalTime();
                var tzInfo = System.TimeZoneInfo.Local;
                bool isDst = tzInfo.IsDaylightSavingTime(dt);
                daylightP = isDst ? T.Instance : Nil.Instance;
                double utcOffsetHours = StandardUtcOffsetHours(tzInfo, dt);
                zone = (-utcOffsetHours == (long)(-utcOffsetHours)) ? (LispObject)Fixnum.Make((long)(-utcOffsetHours)) : new DoubleFloat(-utcOffsetHours);
            }
            int dow = ((int)dt.DayOfWeek + 6) % 7;
            return MultipleValues.Values(
                Fixnum.Make(dt.Second),
                Fixnum.Make(dt.Minute),
                Fixnum.Make(dt.Hour),
                Fixnum.Make(dt.Day),
                Fixnum.Make(dt.Month),
                Fixnum.Make(dt.Year),
                Fixnum.Make(dow),
                daylightP,
                zone
            );
        }, "DECODE-UNIVERSAL-TIME", -1));
        Emitter.CilAssembler.RegisterFunction("GET-DECODED-TIME", new LispFunction(_ => {
            if (_.Length != 0) throw new LispErrorException(new LispProgramError("GET-DECODED-TIME: wrong number of arguments: " + _.Length + " (expected 0)"));
            var now = System.DateTime.Now;
            var tzInfo = System.TimeZoneInfo.Local;
            bool isDst = tzInfo.IsDaylightSavingTime(now);
            double utcOffsetHours = StandardUtcOffsetHours(tzInfo, now);
            int dow = ((int)now.DayOfWeek + 6) % 7;
            return MultipleValues.Values(
                Fixnum.Make(now.Second),
                Fixnum.Make(now.Minute),
                Fixnum.Make(now.Hour),
                Fixnum.Make(now.Day),
                Fixnum.Make(now.Month),
                Fixnum.Make(now.Year),
                Fixnum.Make(dow),
                isDst ? T.Instance : Nil.Instance,
                (-utcOffsetHours == (long)(-utcOffsetHours)) ? (LispObject)Fixnum.Make((long)(-utcOffsetHours)) : new DoubleFloat(-utcOffsetHours)
            );
        }, "GET-DECODED-TIME", -1));
        Emitter.CilAssembler.RegisterFunction("ENCODE-UNIVERSAL-TIME", new LispFunction(args => {
            if (args.Length < 6 || args.Length > 7)
                throw new LispErrorException(new LispProgramError("ENCODE-UNIVERSAL-TIME: expected 6-7 args"));
            int second = (int)UniversalTimeInteger("ENCODE-UNIVERSAL-TIME", args[0]);
            int minute = (int)UniversalTimeInteger("ENCODE-UNIVERSAL-TIME", args[1]);
            int hour   = (int)UniversalTimeInteger("ENCODE-UNIVERSAL-TIME", args[2]);
            int date   = (int)UniversalTimeInteger("ENCODE-UNIVERSAL-TIME", args[3]);
            int month  = (int)UniversalTimeInteger("ENCODE-UNIVERSAL-TIME", args[4]);
            int year   = (int)UniversalTimeInteger("ENCODE-UNIVERSAL-TIME", args[5]);
            if (args.Length == 7 && args[6] is not Nil) CheckTimeZone("ENCODE-UNIVERSAL-TIME", args[6]);
            if (year >= 0 && year <= 99) {
                int curYear = System.DateTime.Now.Year;
                int century = curYear - (curYear % 100);
                year += century;
                // CLHS: within 50 years of the current year; a year exactly 50 ahead is
                // taken as 50 behind, as SBCL does (the window is cur-50 .. cur+49).
                if (year > curYear + 49) year -= 100;
                else if (year < curYear - 50) year += 100;
            }
            var epoch = new System.DateTime(1900, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
            System.DateTime dt;
            if (args.Length == 7 && args[6] is not Nil) {
                var tzNum = Runtime.AsNumber(args[6]);
                long offsetSeconds = (long)System.Math.Round(Arithmetic.ToDouble(Arithmetic.Multiply(tzNum, new Fixnum(3600))));
                dt = new System.DateTime(year, month, date, hour, minute, second, System.DateTimeKind.Utc);
                dt = dt.AddSeconds(offsetSeconds);
            } else {
                dt = new System.DateTime(year, month, date, hour, minute, second, System.DateTimeKind.Local);
                dt = dt.ToUniversalTime();
            }
            long secs = (long)(dt - epoch).TotalSeconds;
            if (secs < 0)
                throw new LispErrorException(new LispTypeError(
                    "ENCODE-UNIVERSAL-TIME: date is before 1900, outside the range of universal time",
                    Fixnum.Make(secs), Startup.Sym("UNSIGNED-BYTE")));
            return Fixnum.Make(secs);
        }, "ENCODE-UNIVERSAL-TIME", -1));

        // Environment functions
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
        Emitter.CilAssembler.RegisterFunction("DISASSEMBLE",
            new LispFunction(Runtime.Disassemble, "DISASSEMBLE", -1));
        Emitter.CilAssembler.RegisterFunction("%TRACE",
            new LispFunction(Runtime.Trace, "%TRACE", -1));
        Emitter.CilAssembler.RegisterFunction("%UNTRACE",
            new LispFunction(Runtime.Untrace, "%UNTRACE", -1));
        Emitter.CilAssembler.RegisterFunction("%UNTRACED-FUNCTION",
            new LispFunction(args => Runtime.UntracedFunction(args[0]), "%UNTRACED-FUNCTION", 1));
        Emitter.CilAssembler.RegisterFunction("ED",
            new LispFunction(Runtime.Ed, "ED", -1));
        Emitter.CilAssembler.RegisterFunction("INSPECT",
            new LispFunction(Runtime.Inspect, "INSPECT", -1));

        // %REGISTER-TYPE-EXPANDER
        Emitter.CilAssembler.RegisterFunction("%REGISTER-TYPE-EXPANDER",
            new LispFunction(args => {
                // Keyed by the symbol (see Runtime.TypeExpanderKey): a deftype
                // answers for its own symbol only. It must neither hijack a
                // built-in (SB-XC:COMPLEX vs CL:COMPLEX) nor a same-named type
                // in another package (UIOP::TIMESTAMP vs a user TIMESTAMP class).
                if (args[0] is Symbol s)
                {
                    Runtime.CheckTypeNameAvailable(s, "DEFTYPE");
                    Runtime.TypeExpanders[Runtime.TypeExpanderKey(s)] = args[1];
                    System.Threading.Interlocked.Increment(ref Runtime.TypeExpanderEpoch);
                    TypeParser.InvalidateCache(s.Name);
                }
                else
                {
                    Runtime.TypeExpanders[args[0].ToString()!] = args[1];
                    System.Threading.Interlocked.Increment(ref Runtime.TypeExpanderEpoch);
                }
                return args[0];
            }));

        // Stack space check: returns T if sufficient execution stack remains, NIL otherwise.
        // Used by the analysis walks to bail out before .NET StackOverflowException,
        // and by COMPILE-FORM to signal a Lisp error instead. The padded probe is
        // used so that a caller that sees NIL still has room to signal: the
        // runtime's own probe leaves only enough stack to throw.
        // The args-array wrapper ignores its arguments entirely, so the 0-arg
        // direct delegate is the identical code path (extra-arg calls still fall
        // back to the wrapper, which also ignores them: unchanged).
        var stackSpaceFn = new LispFunction(_ =>
            Compat.TryEnsureSufficientExecutionStackWithMargin()
                ? (LispObject)T.Instance : Nil.Instance,
            "%STACK-SPACE-AVAILABLE-P", 0);
        stackSpaceFn.SetDirectDelegate((Func<LispObject>)(() =>
            Compat.TryEnsureSufficientExecutionStackWithMargin()
                ? (LispObject)T.Instance : Nil.Instance));
        Emitter.CilAssembler.RegisterFunction("%STACK-SPACE-AVAILABLE-P", stackSpaceFn);
    }

    /// <summary>Helper: check if a Lisp list contains a symbol (by name)</summary>
    private static bool MlfsListContainsSymbol(LispObject list, Symbol sym)
    {
        var cur = list;
        while (cur is Cons c)
        {
            if (c.Car is Symbol s && s.Name == sym.Name)
                return true;
            cur = c.Cdr;
        }
        return false;
    }


    /// <summary>
    /// (dotcl:function-lambda-list fn-or-name) -> (values lambda-list foundp)
    ///
    /// What a development tool needs and CL does not provide: SLIME/SLY autodoc,
    /// DESCRIBE and completion all want the parameters a function was written
    /// with. FUNCTION-LAMBDA-EXPRESSION is allowed to return NIL and does; ARITY
    /// counts required parameters only. Other implementations expose exactly this
    /// (sb-introspect:function-lambda-list, ext:function-lambda-list on ECL/Clasp,
    /// ext:arglist on CLISP), and a swank/slynk backend can only call whatever
    /// the implementation offers.
    ///
    /// Given a symbol, a macro binding wins over a function binding: a macro's
    /// documented parameters are its own lambda list, never the (form env) pair
    /// its expander takes.
    ///
    /// The second value distinguishes "no parameters" from "unknown", which is
    /// the distinction a caller needs -- swank's ARGLIST falls back to
    /// :not-available rather than showing a function as taking nothing.
    /// </summary>
    /// <summary>Record a function's lambda list as the text it was written as.
    /// Called from the IL a FASL emits, once per function it defines.</summary>
    public static void SetLambdaListSource(LispFunction fn, string text)
        => fn.StoredLambdaList = new LispString(text);
    /// <summary>Read a FASL-carried lambda list, once, replacing the text with what
    /// it read. The text is printed relative to the package of the function's own
    /// name, so nothing here depends on which package is current. A text that will not
    /// read is dropped rather than retried: it is display information, and the
    /// caller is a tool drawing one line of it.</summary>
    private static LispObject? ParseStoredLambdaListText(LispFunction fn, LispString text)
    {
        fn.StoredLambdaList = null;
        // Read in the package of the function's own name, which the text was
        // printed relative to. Names an older FASL wrote uninterned are matched
        // there too.
        var nameObj = FunctionNameObject(fn);
        var home = ((nameObj is Cons sc && sc.Cdr is Cons sn ? sn.Car : nameObj) as Symbol)?.HomePackage;
        var pkgSym = Startup.Sym("*PACKAGE*");
        if (home != null) DynamicBindings.Push(pkgSym, home);
        try
        {
            // READ-FROM-STRING answers with its position as a second value; keeping
            // the pair would hand that on as this function's own second value.
            var parsed = UnwrapMv(ReadFromString(new LispObject[] { text }));
            if (home != null) parsed = ReinternLambdaListNames(parsed, home);
            fn.StoredLambdaList = parsed;
            return parsed;
        }
        catch { return null; }
        finally { if (home != null) DynamicBindings.Pop(pkgSym); }
    }

    /// <summary>The lambda list recorded for the code an anonymous function runs:
    /// an in-memory closure site's, or the one a FASL put on the method. Kept on
    /// FN once found, so the metadata is read once per function.</summary>
    private static LispObject? SiteLambdaList(LispFunction fn, Delegate body)
    {
#if DOTCL_EMIT
        var ll = Emitter.CilAssembler.ClosureSiteLambdaList(body);
#else
        LispObject? ll = null;
#endif
        if (ll == null)
        {
            LispLambdaListAttribute? attr = null;
            try
            {
                var found = body.Method.GetCustomAttributes(typeof(LispLambdaListAttribute), false);
                if (found.Length > 0) attr = (LispLambdaListAttribute)found[0];
            }
            catch { }
            if (attr == null) return null;
            var home = attr.Package != null ? Package.FindPackage(attr.Package) : null;
            var pkgSym = Startup.Sym("*PACKAGE*");
            if (home != null) DynamicBindings.Push(pkgSym, home);
            try
            {
                ll = UnwrapMv(ReadFromString(new LispObject[] { new LispString(attr.Text) }));
                if (home != null) ll = ReinternLambdaListNames(ll, home);
            }
            catch { return null; }
            finally { if (home != null) DynamicBindings.Pop(pkgSym); }
        }
        fn.StoredLambdaList = ll;
        return ll;
    }

    // What a FORMATTER function takes, as its CLHS entry describes it: a stream and
    // the format arguments.
    private static LispObject? FormatterLambdaList;

    /// <summary>The symbol (or (SETF symbol)) a named function was defined
    /// under, found by looking for the symbol whose function cell holds FN: the
    /// current package first, then every package. A LispFunction keeps only the
    /// bare name string, so the name alone would have to be interned somewhere
    /// to become a symbol, and interning it in an internal package handed out a
    /// symbol the caller never wrote. Null when no symbol names FN.</summary>
    internal static LispObject? FunctionNameObject(LispFunction fn)
    {
        var name = fn.Name;
        if (string.IsNullOrEmpty(name)) return null;
        bool setf = name!.StartsWith("(SETF ", StringComparison.Ordinal) && name.EndsWith(")", StringComparison.Ordinal);
        var bare = setf ? name.Substring(6, name.Length - 7) : name;
        Symbol? Match(Package pkg)
        {
            var (sym, status) = pkg.FindSymbol(bare);
            if (sym == null || status == SymbolStatus.None) return null;
            var cell = setf ? sym.SetfFunction : sym.Function;
            return ReferenceEquals(cell, fn) ? sym : null;
        }
        Symbol? found = null;
        if (DynamicBindings.Get(Startup.Sym("*PACKAGE*")) is Package cur) found = Match(cur);
        if (found == null)
            foreach (var pkg in Package.AllPackages)
                if ((found = Match(pkg)) != null) break;
        if (found == null) return null;
        return setf ? List(Startup.Sym("SETF"), CanonicalizeSymbol(found)) : CanonicalizeSymbol(found);
    }

    /// <summary>An older FASL wrote a lambda list's variable names as uninterned
    /// symbols, and a name the compiling process could not resolve is still
    /// written that way. Give them back the symbols the source named: each uninterned variable
    /// name that is accessible in the package of the function's own name is
    /// replaced by that symbol. Nothing is interned; a name that is not found
    /// stays uninterned.</summary>
    private static LispObject ReinternLambdaListNames(LispObject ll, Package pkg)
    {
        LispObject Var(LispObject x)
        {
            if (x is Symbol s && s.HomePackage == null)
            {
                var (found, status) = pkg.FindSymbol(s.Name);
                if (found != null && status != SymbolStatus.None) return CanonicalizeSymbol(found);
            }
            return x;
        }
        LispObject Spec(LispObject x)
        {
            if (x is not Cons c) return Var(x);
            LispObject head = c.Car is Cons kc && kc.Cdr is Cons kv
                ? new Cons(kc.Car, new Cons(Var(kv.Car), kv.Cdr))
                : Var(c.Car);
            LispObject rest = c.Cdr;
            // (name default supplied-p): the supplied-p variable is a name too.
            if (rest is Cons d && d.Cdr is Cons sp)
                rest = new Cons(d.Car, new Cons(Var(sp.Car), sp.Cdr));
            return new Cons(head, rest);
        }
        var items = new List<LispObject>();
        LispObject cur = ll;
        while (cur is Cons cell)
        {
            var e = cell.Car;
            items.Add(e is Symbol ks && ks.Name.StartsWith("&", StringComparison.Ordinal) && ks.HomePackage != null
                      ? e : Spec(e));
            cur = cell.Cdr;
        }
        LispObject result = Var(cur);
        for (int i = items.Count - 1; i >= 0; i--) result = new Cons(items[i], result);
        return result;
    }

    public static LispObject FunctionLambdaList(LispObject arg)
    {
        var ll = StoredLambdaListOf(arg);
        return ll == null
            ? MultipleValues.Values2(Nil.Instance, Nil.Instance)
            : MultipleValues.Values2(ll, T.Instance);
    }

    /// <summary>FUNCTION-LAMBDA-LIST's primary value, without publishing values:
    /// a runtime caller returns something else, and left values could pass for its own.</summary>
    internal static LispObject? StoredLambdaListOf(LispObject arg)
    {
        LispObject target = arg;
        if (arg is Symbol sym)
        {
            var mf = Runtime.MacroFunction(sym);
            target = mf is not Nil ? mf : (sym.Function ?? (LispObject)Nil.Instance);
        }
        // A (SETF name) function name denotes the setf function, as for FDEFINITION.
        else if (arg is Cons c && c.Car is Symbol setfSym && setfSym.Name == "SETF"
                 && setfSym.HomePackage?.Name == "COMMON-LISP"
                 && c.Cdr is Cons rest && rest.Car is Symbol accessor && rest.Cdr is Nil)
        {
            target = accessor.SetfFunction ?? (LispObject)Nil.Instance;
        }
        target = UntracedFunction(target);
        // A standard operator answers the lambda list of its dictionary entry
        // (compiler/cl-lambda-lists.lisp), and an operator of dotcl's own
        // packages that of compiler/ext-lambda-lists.lisp, except a generic
        // function that recorded one of its own.
        if (!(target is GenericFunction { StoredLambdaList: not null })
            && Emitter.CilAssembler.TryGetFunction("%CL-BUILTIN-LAMBDA-LIST") is { } builtin
            && builtin.Invoke(new LispObject[] { arg is Symbol ? arg : target }) is Cons box)
            return box.Car;
        LispObject? ll = null;
        if (target is GenericFunction gf)
        {
            // The MOP accessor already reconstructs a placeholder from the arity
            // counts when no lambda list was stored, and a placeholder is still a
            // truthful answer about the shape.
            ll = gf.StoredLambdaList ?? (gf.PlaceholderLambdaList ??= Mop.BuildLambdaListPlaceholder(
                gf.RequiredCount, gf.OptionalCount, gf.HasRest, gf.HasKey,
                gf.KeywordNames, gf.HasAllowOtherKeys));
        }
        else if (target is LispFunction lf)
        {
            ll = lf.StoredLambdaList;
            // A FASL carries the list as the text it was written as (see
            // LispFunction.StoredLambdaList); this is the reader of it.
            if (ll is LispString text) ll = ParseStoredLambdaListText(lf, text);
            // An interpreted closure carries (params env specials k); PARAMS is
            // the lambda list the user wrote.
            if (ll == null && lf.InterpInfo is Cons info) ll = info.Car;
            // An anonymous function keeps its lambda list with its compiled body
            // rather than on itself (see LispLambdaListAttribute).
            if (ll == null && lf.BodyDelegate is { } body) ll = SiteLambdaList(lf, body);
        }
        return ll;
    }

    /// <summary>Installs FN as the macro function of SYM in both the runtime table
    /// and the compiler's *MACROS* table.</summary>
    internal static LispObject RegisterMacroFunctionRt(Symbol sym, LispObject fn)
    {
        var name = sym.Name;
        // Use qualified key for non-CL symbols to avoid name collisions
        var pkg = sym.HomePackage;
        var key = (pkg == null || pkg.Name is "COMMON-LISP" or "CL")
            ? name
            : $"{pkg.Name}:{name}";
        // Protect CL macros from foreign package overwrite.
        // If the symbol is a CL symbol and the macro already has an expander registered,
        // skip to preserve host builtins (e.g., DEFSTRUCT) from SBCL cross-compiler overwrite.
        if (pkg?.Name is "COMMON-LISP" or "CL")
        {
            // Check if the Lisp-side *macros* table already has this key
            foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
            {
                var pkgObj = Package.FindPackage(pkgName);
                if (pkgObj != null)
                {
                    var (macrosSym, status) = pkgObj.FindSymbol("*MACROS*");
                    if (macrosSym != null && status != SymbolStatus.None
                        && macrosSym.Value is LispHashTable macrosTable)
                    {
                        if (macrosTable.Get(sym, Nil.Instance) is not Nil)
                            return fn;  // CL macro already registered; don't overwrite
                        break;
                    }
                }
            }
        }
        if (fn is LispFunction lf)
        {
            Runtime.RegisterMacroFunction(sym, lf);
            var wrapper = new LispFunction(wrapArgs => {
                return lf.Invoke(new LispObject[] { wrapArgs[0], Nil.Instance });
            }, $"MACRO-EXPANDER-{name}", 1);
            foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
            {
                var pkgObj = Package.FindPackage(pkgName);
                if (pkgObj != null)
                {
                    var (macrosSym, status) = pkgObj.FindSymbol("*MACROS*");
                    if (macrosSym != null && status != SymbolStatus.None
                        && macrosSym.Value is LispHashTable macrosTable)
                    {
                        macrosTable.Set(sym, wrapper);
                        break;
                    }
                }
            }
        }
        return fn;
    }
}
