namespace DotCL;

public static partial class Runtime
{
    // --- I/O ---

    /// <summary>
    /// Applies *PRINT-CASE* to a symbol name string, respecting readtable-case
    /// per CLHS 22.1.3.3.2.
    /// </summary>
    public static string ApplyPrintCase(string name)
    {
        // Get readtable-case
        var rtSym = Startup.CL.FindSymbol("*READTABLE*").symbol;
        LispObject rtVal;
        if (!DynamicBindings.TryGet(rtSym, out rtVal))
            rtVal = rtSym?.Value;
        ReadtableCase rtCase = ReadtableCase.Upcase;
        if (rtVal is LispReadtable rt) rtCase = rt.Case;

        // Get *print-case*
        var pcSym = Startup.CL.FindSymbol("*PRINT-CASE*").symbol;
        if (pcSym == null) return name;
        LispObject pcVal;
        if (!DynamicBindings.TryGet(pcSym, out pcVal))
        {
            if (pcSym.IsBound) pcVal = pcSym.Value!;
            else return name;
        }
        string printCase = (pcVal is Symbol kw) ? kw.Name : "UPCASE";

        switch (rtCase)
        {
            case ReadtableCase.Preserve:
                return name; // No conversion

            case ReadtableCase.Invert:
            {
                // All uppercase → all lowercase; all lowercase → all uppercase; mixed → as-is
                // *print-case* is ignored for :invert
                bool hasUpper = false, hasLower = false;
                foreach (char c in name)
                {
                    if (char.IsUpper(c)) hasUpper = true;
                    if (char.IsLower(c)) hasLower = true;
                }
                if (hasUpper && !hasLower)
                    return name.ToLowerInvariant();
                if (hasLower && !hasUpper)
                    return name.ToUpperInvariant();
                return name; // mixed
            }

            case ReadtableCase.Upcase:
                // Apply *print-case* to uppercase chars, leave lowercase as-is
                return ApplyPrintCaseToChars(name, printCase, char.IsUpper);

            case ReadtableCase.Downcase:
                // Apply *print-case* to lowercase chars, leave uppercase as-is
                return ApplyPrintCaseToChars(name, printCase, char.IsLower);

            default:
                return name;
        }
    }

    /// <summary>
    /// Apply *print-case* conversion to characters matching the predicate.
    /// Non-matching characters are left as-is.
    /// </summary>
    private static string ApplyPrintCaseToChars(string name, string printCase, Func<char, bool> isAffected)
    {
        switch (printCase)
        {
            case "DOWNCASE":
            {
                var sb = new System.Text.StringBuilder(name.Length);
                foreach (char c in name)
                    sb.Append(isAffected(c) ? char.ToLowerInvariant(c) : c);
                return sb.ToString();
            }
            case "UPCASE":
            {
                var sb = new System.Text.StringBuilder(name.Length);
                foreach (char c in name)
                    sb.Append(isAffected(c) ? char.ToUpperInvariant(c) : c);
                return sb.ToString();
            }
            case "CAPITALIZE":
            {
                var sb = new System.Text.StringBuilder(name.Length);
                bool newWord = true;
                foreach (char c in name)
                {
                    if (!char.IsLetterOrDigit(c))
                    {
                        newWord = true;
                        sb.Append(isAffected(c) ? char.ToLowerInvariant(c) : c);
                    }
                    else if (newWord && char.IsLetter(c))
                    {
                        sb.Append(isAffected(c) ? char.ToUpperInvariant(c) : c);
                        newWord = false;
                    }
                    else
                    {
                        sb.Append(isAffected(c) ? char.ToLowerInvariant(c) : c);
                        newWord = false;
                    }
                }
                return sb.ToString();
            }
            default:
                return name;
        }
    }

    /// <summary>
    /// Format a symbol for printing with *PRINT-CASE* applied.
    /// escape=true: prin1-like (with package prefix and escape chars as needed)
    /// escape=false: princ-like (no escape chars)
    /// </summary>
    public static string FormatSymbol(Symbol sym, bool escape)
    {
        string name = ApplyPrintCase(sym.Name);
        // *print-readably* overrides *print-escape*: must produce readable output
        bool effectiveEscape = escape || GetPrintReadably();

        if (!effectiveEscape)
        {
            if (sym.HomePackage == null) return name;
            // Keyword colon prefix is part of the printed name, not an escape:
            // it must appear even when *print-escape* is nil (CLHS 22.1.3.3.1).
            if (sym.HomePackage.Name == "KEYWORD") return ":" + name;
            return name;
        }

        // escape=true (or *print-readably*): check if name needs escaping for round-trip readability
        string escapedName = SymbolNeedsEscaping(sym.Name) ? EscapeSymbolName(sym.Name) : name;

        if (sym.HomePackage == null)
        {
            // Per CLHS 22.1.3.3.1: #: prefix only when *print-gensym* is true
            // *print-readably* overrides: always show prefix
            if (GetPrintGensym() || GetPrintReadably())
                return $"#:{escapedName}";
            return escapedName;
        }
        if (sym.HomePackage.Name == "KEYWORD")
            return $":{escapedName}";

        // Check if symbol is accessible in *package*
        var currentPkg = (Package)DynamicBindings.Get(Startup.Sym("*PACKAGE*"));
        var (found, status) = currentPkg.FindSymbol(sym.Name);
        if (status != SymbolStatus.None && ReferenceEquals(found, sym))
            return escapedName;  // Accessible in current package — no prefix needed

        // Need package prefix
        string pkgName = SymbolNeedsEscaping(sym.HomePackage.Name)
            ? EscapeSymbolName(sym.HomePackage.Name)
            : ApplyPrintCase(sym.HomePackage.Name);
        var (_, homeStatus) = sym.HomePackage.FindSymbol(sym.Name);
        if (homeStatus == SymbolStatus.External)
            return $"{pkgName}:{escapedName}";
        else
            return $"{pkgName}::{escapedName}";
    }

    /// <summary>
    /// Check if a symbol name needs escaping for round-trip readability.
    /// Per CLHS 22.1.3.3.2.
    /// </summary>
    private static bool SymbolNeedsEscaping(string name)
    {
        if (name.Length == 0) return true; // empty name → ||

        // Get current readtable and its case
        var rtSym = Startup.CL.FindSymbol("*READTABLE*").symbol;
        LispObject rtVal;
        if (!DynamicBindings.TryGet(rtSym, out rtVal))
            rtVal = rtSym?.Value;
        LispReadtable? rt = rtVal as LispReadtable;
        ReadtableCase rtCase = rt?.Case ?? ReadtableCase.Upcase;

        // When *print-readably* is T, output must be readable by the standard
        // readtable which uses :upcase.  Force :upcase rules for escaping.
        if (GetPrintReadably())
            rtCase = ReadtableCase.Upcase;

        // Check if name is all dots (would be read as dotted-pair notation)
        bool allDots = true;
        foreach (char c in name)
            if (c != '.') { allDots = false; break; }
        if (allDots) return true;

        // Check each character
        foreach (char c in name)
        {
            // Get syntax type from current readtable
            var st = rt?.GetSyntaxType(c) ?? (c >= ' ' && c <= '~' ? SyntaxType.Constituent : SyntaxType.Invalid);

            // Non-constituent chars need escaping
            if (st != SyntaxType.Constituent) return true;

            // Package marker
            if (c == ':') return true;

            // Check if readtable-case would change this character
            switch (rtCase)
            {
                case ReadtableCase.Upcase:
                    if (char.IsLower(c)) return true;
                    break;
                case ReadtableCase.Downcase:
                    if (char.IsUpper(c)) return true;
                    break;
                case ReadtableCase.Invert:
                    // Invert: all-upper → all-lower, all-lower → all-upper, mixed → preserve
                    // The printer also inverts in ApplyPrintCase, so it should round-trip.
                    break;
                case ReadtableCase.Preserve:
                    break;
            }
        }

        // Check if name looks like a number (would be read as number instead of symbol)
        // Simple check: try to see if it starts with digit, +digit, -digit, or .digit
        if (CouldBeNumber(name)) return true;

        return false;
    }

    /// <summary>
    /// Check if a character is a digit in the given radix (2-36).
    /// </summary>
    private static bool IsDigitInBase(char c, int radix)
    {
        char upper = char.ToUpperInvariant(c);
        if (upper >= '0' && upper <= '9')
            return (upper - '0') < radix;
        if (upper >= 'A' && upper <= 'Z')
            return (upper - 'A' + 10) < radix;
        return false;
    }

    /// <summary>
    /// Check if a string could be read as a number (requiring symbol escaping).
    /// Per CLHS 2.3.1.1 (Potential Numbers) and 22.1.3.3.1 (Printing Symbols).
    /// Must consider the current *print-base* since letters A-Z are digits in bases > 10.
    /// </summary>
    private static bool CouldBeNumber(string name)
    {
        if (name.Length == 0) return false;

        int radix = GetPrintBase();

        // Check if name could be an integer in the current base:
        // Optional sign followed by one or more digits, or digits/digits (ratio)
        string body = name;
        if ((body[0] == '+' || body[0] == '-') && body.Length > 1)
            body = body.Substring(1);

        // Check for ratio: digits/digits
        int slashPos = body.IndexOf('/');
        if (slashPos > 0 && slashPos < body.Length - 1)
        {
            string num = body.Substring(0, slashPos);
            string den = body.Substring(slashPos + 1);
            bool numOk = num.Length > 0;
            bool denOk = den.Length > 0;
            foreach (char c in num) if (!IsDigitInBase(c, radix)) { numOk = false; break; }
            foreach (char c in den) if (!IsDigitInBase(c, radix)) { denOk = false; break; }
            if (numOk && denOk) return true;
        }

        // Check for plain integer in current base: all chars are valid digits
        if (body.Length > 0)
        {
            bool allDigits = true;
            foreach (char c in body)
            {
                if (!IsDigitInBase(c, radix)) { allDigits = false; break; }
            }
            if (allDigits) return true;
        }

        // Check for float patterns (base 10 only): digits with decimal point and/or exponent
        char first = name[0];
        // Starts with digit (base 10)
        if (first >= '0' && first <= '9') return true;
        // Starts with +/- followed by digit or dot
        if ((first == '+' || first == '-') && name.Length > 1)
        {
            char second = name[1];
            if ((second >= '0' && second <= '9') || second == '.') return true;
        }
        // Starts with dot followed by digit
        if (first == '.' && name.Length > 1 && name[1] >= '0' && name[1] <= '9') return true;

        // Check for potential number per CLHS 2.3.1.1:
        // A token is a potential number if it contains at least one digit (in current base),
        // consists only of digits, signs, ratio markers, decimal points, extension chars,
        // and number markers, starts with digit/sign/dot/extension, and doesn't end with sign.
        if (radix > 10)
        {
            // Check if the name could be a potential number with mixed digits and number markers
            // Number markers for floats: D, E, F, L, S (only if not ALL are digits in the base)
            bool hasDigit = false;
            bool allPotentialNumberChars = true;
            bool hasNonDigitLetter = false;
            foreach (char c in name)
            {
                char upper = char.ToUpperInvariant(c);
                if (IsDigitInBase(c, radix)) { hasDigit = true; continue; }
                if (c == '+' || c == '-' || c == '/' || c == '.' || c == '^' || c == '_') continue;
                // Number markers: D E F L S — only count as number markers if they're NOT valid digits
                if (!IsDigitInBase(c, radix) && "DEFLS".IndexOf(upper) >= 0)
                {
                    hasNonDigitLetter = true;
                    continue;
                }
                allPotentialNumberChars = false;
                break;
            }
            if (allPotentialNumberChars && hasDigit && hasNonDigitLetter)
            {
                // Starts with digit, sign, dot, or extension char
                char f = name[0];
                if (IsDigitInBase(f, radix) || f == '+' || f == '-' || f == '.' || f == '^' || f == '_')
                {
                    // Doesn't end with sign
                    char last = name[name.Length - 1];
                    if (last != '+' && last != '-')
                        return true;
                }
            }
        }

        return false;
    }

    /// <summary>
    /// Escape a symbol name using |...| notation.
    /// Chars | and \ inside are escaped with \.
    /// </summary>
    private static string EscapeSymbolName(string name)
    {
        var sb = new System.Text.StringBuilder("|");
        foreach (char c in name)
        {
            if (c == '|' || c == '\\') sb.Append('\\');
            sb.Append(c);
        }
        sb.Append('|');
        return sb.ToString();
    }

    /// <summary>
    /// Format any LispObject for printing with *PRINT-CASE* applied to symbol names.
    /// escape=true: prin1-like; escape=false: princ-like
    /// </summary>
    /// <summary>Format object with dispatch disabled (for consistent key generation).</summary>
    public static string FormatObjectNoDispatch(LispObject obj)
    {
        bool saved = _inPprintDispatch;
        _inPprintDispatch = true;
        try { return FormatObject(obj, true); }
        finally { _inPprintDispatch = saved; }
    }

    public static string FormatObject(LispObject obj, bool escape)
    {
        // *print-circle* label check for compound objects
        if (_circleTable != null && (obj is Cons || (obj is LispVector lv && !lv.IsCharVector) || obj is LispStruct))
        {
            if (_circleTable.TryGetValue(obj, out int circState))
            {
                if (circState < 0) return $"#{-circState}#"; // back-reference
                if (circState == 0) // first occurrence — assign label
                {
                    int label = ++_circleLabelCounter;
                    _circleTable[obj] = -label;
                    var dispatchResult0 = TryPprintDispatch(obj);
                    string content;
                    if (dispatchResult0 != null)
                        content = dispatchResult0;
                    else if (obj is Cons c0)
                        content = FormatCons(c0, escape);
                    else
                        content = FormatCompound(obj, escape);
                    return $"#{label}={content}";
                }
            }
        }

        // Check pprint dispatch table when *print-pretty* is true
        var dispatchResult = TryPprintDispatch(obj);
        if (dispatchResult != null) return dispatchResult;

        // For conditions when escape is false (princ/~a), print the condition report
        if (!escape && obj is LispCondition condReport)
            return GetConditionReport(condReport);

        switch (obj)
        {
            case LispString s:
            {
                bool effectiveEscape = escape || GetPrintReadably();
                return effectiveEscape ? s.ToString() : s.Value;
            }
            case LispVector vec when vec.IsCharVector && vec.Rank == 1:
            {
                // Character vectors are strings
                var str = vec.ToCharString();
                bool effectiveEscape = escape || GetPrintReadably();
                return effectiveEscape ? $"\"{str.Replace("\\", "\\\\").Replace("\"", "\\\"")}\"" : str;
            }
            case LispChar c:
                // When *print-readably* is T, characters must be printed readably
                // even when escape is false (CLHS 22.1.3.2)
                if (!escape && !GetPrintReadably()) return c.Value.ToString();
                // CLHS 22.1.3.2: When *print-readably* is false and *print-escape* is true,
                // graphic standard characters print as #\ followed by the character itself.
                // Space is a graphic standard character.
                if (!GetPrintReadably())
                {
                    char ch = c.Value;
                    // Standard characters that are graphic (including Space) but not Newline
                    if (ch == ' ') return "#\\ ";
                    // Other graphic characters
                    if (ch > ' ' && ch < 127) return $"#\\{ch}";
                }
                return c.ToString();
            case T:
                // When escape/readably and T could be read as a number
                // (e.g. base >= 30 where T is a digit), escape it
                if ((escape || GetPrintReadably()) && CouldBeNumber("T"))
                    return "|T|";
                return ApplyPrintCase("T");
            case Nil:
                // When escape/readably and NIL could be read as a number
                // (e.g. base >= 24 where N,I,L are all digits), print as ()
                if ((escape || GetPrintReadably()) && CouldBeNumber("NIL"))
                    return "()";
                return ApplyPrintCase("NIL");
            case Symbol sym:
                // *print-circle* check for uninterned symbols
                if (_circleTable != null && sym.HomePackage == null
                    && _circleTable.TryGetValue(sym, out int symState))
                {
                    if (symState < 0) return $"#{-symState}#"; // back-reference
                    if (symState == 0) // shared, first print — assign label
                    {
                        int label = ++_circleLabelCounter;
                        _circleTable[sym] = -label;
                        return $"#{label}={FormatSymbol(sym, escape)}";
                    }
                }
                return FormatSymbol(sym, escape);
            case Cons cons:
            {
                // *print-circle* check
                if (_circleTable != null && _circleTable.TryGetValue(cons, out int cState))
                {
                    if (cState < 0) return $"#{-cState}#"; // back-reference
                    if (cState == 0) // shared, first print — assign label
                    {
                        int label = ++_circleLabelCounter;
                        _circleTable[cons] = -label;
                        var lvl = GetPrintLevel();
                        if (lvl.HasValue && _formatConsDepth >= lvl.Value) return $"#{label}= #";
                        return $"#{label}={FormatCons(cons, escape)}";
                    }
                }
                var level = GetPrintLevel();
                if (level.HasValue && _formatConsDepth >= level.Value) return "#";
                return FormatCons(cons, escape);
            }
            case Fixnum fix:
                return FormatInteger(fix.Value, escape);
            case Bignum big:
                return FormatBigInteger(big.Value, escape);
            case Ratio rat:
                return FormatRatio(rat, escape);
            case LispComplex cx:
                return "#C(" + FormatObject(cx.Real, escape) + " " + FormatObject(cx.Imaginary, escape) + ")";
            case SingleFloat sf:
                return FormatSingleFloat(sf.Value);
            case DoubleFloat df:
                return FormatDoubleFloat(df.Value);
            case LispPathname pn:
            {
                var ns = pn.ToNamestring();
                bool effectiveEscape = escape || GetPrintReadably();
                // When *print-readably* is true, check if the pathname can roundtrip
                if (GetPrintReadably())
                {
                    var roundtripped = LispPathname.FromString(ns);
                    if (!IsTruthy(Equal(pn, roundtripped)))
                    {
                        var err = new LispError($"Cannot print pathname readably: {pn}");
                        err.ConditionTypeName = "PRINT-NOT-READABLE";
                        throw new LispErrorException(err);
                    }
                }
                if (effectiveEscape)
                {
                    // CLHS 22.1.3.11: #P followed by escaped namestring
                    var escapedNs = ns.Replace("\\", "\\\\").Replace("\"", "\\\"");
                    return $"#P\"{escapedNs}\"";
                }
                return ns;
            }
            case LispRestart restart:
                if (!escape)
                {
                    if (restart.ReportFunction != null)
                    {
                        var sw = new System.IO.StringWriter();
                        var stream = new LispStringOutputStream(sw);
                        Runtime.Funcall(restart.ReportFunction, new LispObject[] { stream });
                        return sw.ToString();
                    }
                    if (restart.Description != null)
                        return restart.Description;
                }
                return restart.ToString();
            case LispRandomState rs:
            {
                if (GetPrintReadably() || escape)
                    return rs.ToReadableString();
                return "#<RANDOM-STATE>";
            }
            default:
                if (obj is LispVector || obj is LispStruct)
                {
                    // Bit-vectors and strings are atomic for *print-level* purposes (CLHS 22.1.3.6)
                    bool isAtomicPrint = obj is LispVector av && (av.IsCharVector || av.IsBitVector);
                    // *print-circle* check for vectors/structs
                    if (_circleTable != null && !(obj is LispVector cv && cv.IsCharVector)
                        && _circleTable.TryGetValue(obj, out int vState))
                    {
                        if (vState < 0) return $"#{-vState}#";
                        if (vState == 0)
                        {
                            int label = ++_circleLabelCounter;
                            _circleTable[obj] = -label;
                            if (!isAtomicPrint)
                            {
                                var lvl = GetPrintLevel();
                                if (lvl.HasValue && _formatConsDepth >= lvl.Value) return $"#{label}= #";
                            }
                            return $"#{label}={FormatCompound(obj, escape)}";
                        }
                    }
                    if (!isAtomicPrint)
                    {
                        var level = GetPrintLevel();
                        if (level.HasValue && _formatConsDepth >= level.Value) return "#";
                    }
                    return FormatCompound(obj, escape);
                }
                return obj.ToString();
        }
    }

    /// <summary>
    /// Get the condition report string for use with ~a / princ.
    /// Uses :format-control/:format-arguments if available, otherwise the Message field.
    /// </summary>
    private static string GetConditionReport(LispCondition cond)
    {
        if (cond.FormatControl is LispString fcs)
        {
            var fmtArgs = cond.FormatArguments is Cons fac
                ? Startup.ListToArray(fac)
                : Array.Empty<LispObject>();
            try { return FormatString(fcs.Value, fmtArgs); }
            catch { }
        }
        return cond.Message;
    }

    /// <summary>
    /// Get the current value of *READ-DEFAULT-FLOAT-FORMAT*.
    /// Returns "SINGLE-FLOAT" by default.
    /// </summary>
    private static string GetReadDefaultFloatFormat()
    {
        LispObject val;
        var sym = Startup.Sym("*READ-DEFAULT-FLOAT-FORMAT*");
        if (DynamicBindings.TryGet(sym, out val) && val is Symbol s)
            return s.Name;
        if (sym.IsBound && sym.Value is Symbol s2)
            return s2.Name;
        return "SINGLE-FLOAT";
    }

    /// <summary>
    /// Format a float respecting *READ-DEFAULT-FLOAT-FORMAT* per CLHS 22.1.3.1.3.2.
    /// When the float type matches the default format, print without exponent marker.
    /// When it differs, include the appropriate marker (f for single, d for double).
    /// </summary>
    private static string FormatFloat(Number num, bool escape)
    {
        var fmt = GetReadDefaultFloatFormat();
        // short-float = single-float, long-float = double-float in this impl
        bool defaultIsSingle = (fmt == "SINGLE-FLOAT" || fmt == "SHORT-FLOAT");
        bool defaultIsDouble = (fmt == "DOUBLE-FLOAT" || fmt == "LONG-FLOAT");

        if (num is SingleFloat sf)
        {
            float v = sf.Value;
            if (float.IsPositiveInfinity(v)) return "#.SINGLE-FLOAT-POSITIVE-INFINITY";
            if (float.IsNegativeInfinity(v)) return "#.SINGLE-FLOAT-NEGATIVE-INFINITY";
            if (float.IsNaN(v)) return "#.SINGLE-FLOAT-NAN";

            if (defaultIsSingle)
            {
                // Matches default: no marker needed
                var s = NormalizePrinterFloat(v.ToString("R"), Math.Abs((double)v));
                if (s.Contains('E') || s.Contains('e'))
                    return s;
                if (!s.Contains('.'))
                    return s + ".0";
                return s;
            }
            else
            {
                // Doesn't match default: need "f" marker
                var s = NormalizePrinterFloat(v.ToString("R"), Math.Abs((double)v));
                if (s.Contains('E') || s.Contains('e'))
                    return s.Replace("E", "f").Replace("e", "f");
                if (!s.Contains('.'))
                    return s + ".0f0";
                return s + "f0";
            }
        }
        else if (num is DoubleFloat df)
        {
            double v = df.Value;
            if (double.IsPositiveInfinity(v)) return "#.DOUBLE-FLOAT-POSITIVE-INFINITY";
            if (double.IsNegativeInfinity(v)) return "#.DOUBLE-FLOAT-NEGATIVE-INFINITY";
            if (double.IsNaN(v)) return "#.DOUBLE-FLOAT-NAN";

            if (defaultIsDouble)
            {
                // Matches default: no marker needed (like single-float formatting)
                var s = NormalizePrinterFloat(v.ToString("R"), Math.Abs(v));
                if (s.Contains('E') || s.Contains('e'))
                    return s;
                if (!s.Contains('.'))
                    return s + ".0";
                return s;
            }
            else
            {
                // Doesn't match default: need "d" marker (current behavior)
                var s = NormalizePrinterFloat(v.ToString("R"), Math.Abs(v));
                if (s.Contains('E') || s.Contains('e'))
                    return s.Replace("E", "d").Replace("e", "d");
                if (!s.Contains('.'))
                    return s + ".0d0";
                return s + "d0";
            }
        }

        // Fallback (shouldn't reach here)
        return num.ToString();
    }

    private static int? GetPrintLength()
    {
        // Per CLHS 22.1.3.10: *print-readably* overrides *print-length*
        if (GetPrintReadably()) return null;
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-LENGTH*"), out val))
        {
            if (val is Nil) return null;
            if (val is Fixnum f) return (int)f.Value;
        }
        var sym = Startup.Sym("*PRINT-LENGTH*");
        if (sym.IsBound && sym.Value is Fixnum f2) return (int)f2.Value;
        return null; // default: no limit
    }

    private static int? GetPrintLevel()
    {
        // Per CLHS 22.1.3.10: *print-readably* overrides *print-level*
        if (GetPrintReadably()) return null;
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-LEVEL*"), out val))
        {
            if (val is Nil) return null;
            if (val is Fixnum f) return (int)f.Value;
        }
        var sym = Startup.Sym("*PRINT-LEVEL*");
        if (sym.IsBound && sym.Value is Fixnum f2) return (int)f2.Value;
        return null; // default: no limit
    }

    public static bool GetPrintEscapePublic() => GetPrintEscape();

    private static bool GetPrintEscape()
    {
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-ESCAPE*"), out val))
            return !(val is Nil);
        var sym = Startup.Sym("*PRINT-ESCAPE*");
        if (sym.IsBound && sym.Value != null) return !(sym.Value is Nil);
        return true; // default: escape on
    }

    private static bool GetPrintReadably()
    {
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-READABLY*"), out val))
            return !(val is Nil);
        var sym = Startup.Sym("*PRINT-READABLY*");
        if (sym.IsBound && sym.Value != null) return !(sym.Value is Nil);
        return false; // default: not readably
    }

    private static bool GetPrintGensym()
    {
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-GENSYM*"), out val))
            return !(val is Nil);
        var sym = Startup.Sym("*PRINT-GENSYM*");
        if (sym.IsBound && sym.Value != null) return !(sym.Value is Nil);
        return true; // default: print gensym prefix
    }

    private static int GetPrintBase()
    {
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-BASE*"), out val) && val is Fixnum f)
            return (int)f.Value;
        var sym = Startup.Sym("*PRINT-BASE*");
        if (sym.Value is Fixnum f2) return (int)f2.Value;
        return 10;
    }

    private static bool GetPrintRadix()
    {
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-RADIX*"), out val))
            return !(val is Nil);
        var sym = Startup.Sym("*PRINT-RADIX*");
        if (sym.Value != null) return !(sym.Value is Nil);
        return false;
    }

    private static string AddRadixPrefix(string digits, int radix)
    {
        switch (radix)
        {
            case 2: return "#b" + digits;
            case 8: return "#o" + digits;
            case 10: return digits + ".";
            case 16: return "#x" + digits;
            default: return $"#{radix}R" + digits;
        }
    }

    private static string FormatInteger(long value, bool escape)
    {
        int radix = GetPrintBase();
        string digits = (radix == 10) ? value.ToString() : ToRadixString(value, radix);
        // *print-readably* T with non-10 base requires radix prefix for correct readback
        if (GetPrintRadix() || (GetPrintReadably() && radix != 10))
            digits = AddRadixPrefix(digits, radix);
        return digits;
    }

    private static string FormatBigInteger(System.Numerics.BigInteger value, bool escape)
    {
        int radix = GetPrintBase();
        string digits = (radix == 10) ? value.ToString() : BigIntToRadixString(value, radix);
        if (GetPrintRadix() || (GetPrintReadably() && radix != 10))
            digits = AddRadixPrefix(digits, radix);
        return digits;
    }

    private static string FormatRatio(Ratio rat, bool escape)
    {
        int radix = GetPrintBase();
        string num = (radix == 10) ? rat.Numerator.ToString() : BigIntToRadixString(rat.Numerator, radix);
        string den = (radix == 10) ? rat.Denominator.ToString() : BigIntToRadixString(rat.Denominator, radix);
        string result = num + "/" + den;
        // For non-10 bases, add radix prefix when *print-radix* or *print-readably*
        // For base 10, the "/" already disambiguates from potential floats, so no trailing dot needed
        if (radix != 10 && (GetPrintRadix() || GetPrintReadably()))
            result = AddRadixPrefix(result, radix);
        return result;
    }

    /// <summary>
    /// Convert a float string from .NET "R" format to CL convention:
    /// - Strip "+" from exponent (CL uses E20 not E+20)
    /// - If the value is outside (10^-3, 10^7), force scientific notation
    ///   (CLHS 22.1.3.1.3: fixed notation only for magnitudes in (10^-3, 10^7))
    /// </summary>
    private static string NormalizePrinterFloat(string s, double absVal)
    {
        // Strip positive exponent sign: E+7 → E7; normalize to uppercase E
        if (s.Contains("E+") || s.Contains("e+"))
            s = s.Replace("E+", "E").Replace("e+", "E");
        if (s.Contains("e") && !s.Contains("E"))
            s = s.Replace("e", "E");

        // If already in scientific notation, normalize it (add decimal point, strip exp leading zeros)
        bool hasExponent = s.IndexOf('E') >= 0;
        if (hasExponent)
        {
            int eIdx = s.IndexOf('E');
            string sigPart = s.Substring(0, eIdx);
            string expStr = s.Substring(eIdx + 1); // e.g. "8" or "-4" or "08"
            // Add decimal point to significand if missing
            if (!sigPart.Contains('.'))
                sigPart += ".0";
            // Strip leading zeros from exponent magnitude (keep sign)
            bool expNeg = expStr.StartsWith("-");
            string expMag = expNeg ? expStr.Substring(1) : expStr;
            expMag = expMag.TrimStart('0');
            if (expMag.Length == 0) expMag = "0";
            expStr = (expNeg ? "-" : "") + expMag;
            s = sigPart + "E" + expStr;
            return s; // already scientific, done
        }

        // Check if .NET gave fixed notation (no E) but we need scientific
        if (absVal != 0.0 && (absVal >= 1e7 || absVal < 1e-3))
        {
            // Force scientific notation via pure string manipulation to avoid FP rounding errors.
            bool negative = s.StartsWith("-");
            string mag = negative ? s.Substring(1) : s;
            // Split at decimal point
            int dotPos = mag.IndexOf('.');
            string intPart = dotPos < 0 ? mag : mag.Substring(0, dotPos);
            string fracPart = dotPos < 0 ? "" : mag.Substring(dotPos + 1);

            int exp;
            string allDigits; // significant digits, decimal-point-free
            if (intPart == "0" || intPart == "")
            {
                // Small number (e.g. "0.00012345"): exponent is negative
                int firstNZ = -1;
                for (int i = 0; i < fracPart.Length; i++)
                    if (fracPart[i] != '0') { firstNZ = i; break; }
                if (firstNZ < 0) return s; // all zeros, leave unchanged
                exp = -(firstNZ + 1);
                allDigits = fracPart.Substring(firstNZ).TrimEnd('0');
            }
            else
            {
                // Large number (e.g. "12345678.9"): exponent is positive
                exp = intPart.Length - 1;
                allDigits = (intPart + fracPart).TrimEnd('0');
            }
            if (allDigits.Length == 0) allDigits = "0";

            // Build mantissa: one leading digit, dot, rest of significant digits
            string mantissa;
            if (allDigits.Length <= 1)
                mantissa = allDigits + ".0";
            else
            {
                string rest = allDigits.Substring(1).TrimEnd('0');
                mantissa = allDigits[0] + "." + (rest.Length > 0 ? rest : "0");
            }
            s = (negative ? "-" : "") + mantissa + "E" + exp.ToString();
        }
        return s;
    }

    /// <summary>
    /// Format a single-float for prin1, respecting *read-default-float-format*.
    /// Per CLHS 22.1.3.1.3.1: if the float type matches *read-default-float-format*,
    /// use exponent marker E (or omit for non-exponent). Otherwise use 'f' marker.
    /// </summary>
    private static string FormatSingleFloat(float value)
    {
        if (float.IsPositiveInfinity(value)) return "#.SINGLE-FLOAT-POSITIVE-INFINITY";
        if (float.IsNegativeInfinity(value)) return "#.SINGLE-FLOAT-NEGATIVE-INFINITY";
        if (float.IsNaN(value)) return "#.SINGLE-FLOAT-NAN";
        var s = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture);
        s = NormalizePrinterFloat(s, Math.Abs((double)value));
        var defaultFormat = GetReadDefaultFloatFormat();
        bool isDefault = (defaultFormat == "SINGLE-FLOAT" || defaultFormat == "SHORT-FLOAT");
        if (s.Contains('E') || s.Contains('e'))
        {
            if (isDefault)
                return s;
            else
                return s.Replace("E", "f").Replace("e", "f");
        }
        if (!s.Contains('.'))
            s += ".0";
        if (!isDefault)
            s += "f0";
        return s;
    }

    /// <summary>
    /// Format a double-float for prin1, respecting *read-default-float-format*.
    /// Per CLHS 22.1.3.1.3.1: if the float type matches *read-default-float-format*,
    /// use exponent marker E (or omit for non-exponent). Otherwise use 'd' marker.
    /// </summary>
    private static string FormatDoubleFloat(double value)
    {
        if (double.IsPositiveInfinity(value)) return "#.DOUBLE-FLOAT-POSITIVE-INFINITY";
        if (double.IsNegativeInfinity(value)) return "#.DOUBLE-FLOAT-NEGATIVE-INFINITY";
        if (double.IsNaN(value)) return "#.DOUBLE-FLOAT-NAN";
        var s = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture);
        s = NormalizePrinterFloat(s, Math.Abs(value));
        var defaultFormat = GetReadDefaultFloatFormat();
        bool isDefault = (defaultFormat == "DOUBLE-FLOAT" || defaultFormat == "LONG-FLOAT");
        if (s.Contains('E') || s.Contains('e'))
        {
            if (isDefault)
                return s;
            else
                return s.Replace("E", "d").Replace("e", "d");
        }
        if (!s.Contains('.'))
            s += ".0";
        if (!isDefault)
            s += "d0";
        return s;
    }

    private static int _formatConsDepth;
    private const int MaxFormatConsDepth = 256;

    // *print-circle* support: two-pass algorithm (scan then format)
    // Table states: 1=seen once, 0=seen twice+ (shared), negative=assigned label
    [ThreadStatic] private static Dictionary<object, int>? _circleTable;
    [ThreadStatic] private static int _circleLabelCounter;

    private static bool GetPrintCircle()
    {
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-CIRCLE*"), out val))
            return !(val is Nil);
        var sym = Startup.Sym("*PRINT-CIRCLE*");
        if (sym.IsBound && sym.Value != null) return !(sym.Value is Nil);
        return false;
    }

    [ThreadStatic] private static bool _inPprintDispatch;

    /// <summary>Try pprint dispatch table. Returns formatted string or null.</summary>
    private static string? TryPprintDispatch(LispObject obj)
    {
        if (_inPprintDispatch) return null; // prevent recursion
        // Check *print-pretty*
        LispObject prettyVal;
        bool pretty = false;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out prettyVal))
            pretty = !(prettyVal is Nil);
        else
        {
            var sym = Startup.Sym("*PRINT-PRETTY*");
            if (sym.IsBound && sym.Value != null) pretty = !(sym.Value is Nil);
        }
        if (!pretty) return null;

        // Get dispatch table
        LispObject tableVal;
        LispPprintDispatchTable? table = null;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-PPRINT-DISPATCH*"), out tableVal))
            table = tableVal as LispPprintDispatchTable;
        if (table == null || table.Entries.Count == 0) return null;

        // Find highest priority matching entry
        (LispObject TypeSpec, LispObject Function, double Priority)? best = null;
        foreach (var entry in table.Entries.Values)
        {
            if (MatchesTypeSpec(obj, entry.TypeSpec))
            {
                if (best == null || entry.Priority > best.Value.Priority)
                    best = entry;
            }
        }
        if (best == null) return null;

        // Call the dispatch function with a string-output-stream and the object
        _inPprintDispatch = true;
        try
        {
            var sw = new System.IO.StringWriter();
            var stream = new LispOutputStream(sw);
            if (best.Value.Function is LispFunction fn)
                fn.Invoke(new LispObject[] { stream, obj });
            return sw.ToString();
        }
        finally { _inPprintDispatch = false; }
    }

    /// <summary>Check if obj matches a type specifier (basic support for EQL and symbol types).</summary>
    private static bool MatchesTypeSpec(LispObject obj, LispObject typeSpec)
    {
        // (EQL value) type specifier
        if (typeSpec is Cons c && c.Car is Symbol sym && sym.Name == "EQL" && c.Cdr is Cons c2)
        {
            return IsTrueEql(obj, c2.Car);
        }
        // Symbol type specifier - use typep
        if (typeSpec is Symbol typeSym)
        {
            return IsTruthy(Typep(obj, typeSym));
        }
        return false;
    }

    /// <summary>
    /// Pass 1: Walk the object graph and mark shared objects.
    /// After this, table entries with value 0 are shared (seen 2+ times).
    /// </summary>
    private static void ScanCircle(LispObject obj, Dictionary<object, int> table)
    {
        if (obj is Cons cons)
        {
            if (table.TryGetValue(cons, out int state))
            {
                if (state == 1) table[cons] = 0; // mark as shared
                return; // don't recurse again
            }
            table[cons] = 1;
            ScanCircle(cons.Car, table);
            ScanCircle(cons.Cdr, table);
        }
        else if (obj is Symbol sym && sym.HomePackage == null)
        {
            // Track uninterned symbols for *print-circle*
            if (table.TryGetValue(sym, out int state))
            {
                if (state == 1) table[sym] = 0;
                return;
            }
            table[sym] = 1;
        }
        else if (obj is LispVector vec && !vec.IsCharVector)
        {
            if (table.TryGetValue(vec, out int state))
            {
                if (state == 1) table[vec] = 0;
                return;
            }
            table[vec] = 1;
            int total = vec.Capacity;
            for (int i = 0; i < total; i++)
                ScanCircle(vec.ElementAt(i), table);
        }
    }

    /// <summary>
    /// Start circle detection for a pprint-logical-block's list.
    /// Returns true if this call set up the circle table (caller must call PprintCircleEnd).
    /// Returns false if circle detection was already active.
    /// </summary>
    public static bool PprintCircleScan(LispObject list)
    {
        if (!GetPrintCircle()) return false;
        if (_circleTable != null) return false; // already scanning
        if (!(list is Cons)) return false;

        var table = new Dictionary<object, int>(ReferenceEqualityComparer.Instance);

        // Walk list structure respecting *print-length* to avoid labeling objects
        // that won't actually appear in truncated output
        int? printLength = GetPrintLength();
        int count = 0;
        LispObject cur = list;
        while (cur is Cons c)
        {
            if (printLength.HasValue && count >= printLength.Value) break;
            ScanCircle(c.Car, table); // scan each element's subtree
            // Track the cons cell itself
            if (table.TryGetValue(c, out int st))
            {
                if (st == 1) table[c] = 0; // circular list
                break;
            }
            table[c] = 1;
            cur = c.Cdr;
            count++;
        }
        // If not truncated by *print-length*, scan the final CDR (for dotted/circular tails)
        if (!(printLength.HasValue && count >= printLength.Value) && cur is not Nil)
        {
            ScanCircle(cur, table);
        }

        bool hasShared = false;
        foreach (var kv in table)
            if (kv.Value == 0) { hasShared = true; break; }

        if (!hasShared) return false;

        _circleTable = table;
        _circleLabelCounter = 0;
        return true;
    }

    /// <summary>End circle detection started by PprintCircleScan.</summary>
    public static void PprintCircleEnd()
    {
        _circleTable = null;
    }

    /// <summary>Whether circle detection is currently active.</summary>
    public static bool HasCircleTable => _circleTable != null;

    /// <summary>Check if an object is marked as shared or already labeled in the circle table.</summary>
    public static bool IsCircleShared(object obj)
    {
        return _circleTable != null && _circleTable.TryGetValue(obj, out int st) && (st == 0 || st < 0);
    }

    /// <summary>
    /// Check if a list object has a circle label. Returns:
    /// - null if no circle detection or object not shared
    /// - "#n=" string if this is the first print of the object (caller should print prefix + content)
    /// - "#n#" string if this is a back-reference (caller should print this and skip content)
    /// </summary>
    public static string? PprintCircleCheckList(LispObject list)
    {
        if (_circleTable == null || !(list is Cons cons)) return null;
        if (!_circleTable.TryGetValue(cons, out int state)) return null;

        if (state == 0)
        {
            // First print of shared object: assign label
            _circleLabelCounter++;
            int label = _circleLabelCounter;
            _circleTable[cons] = -label;
            return $"#{label}=";
        }
        else if (state < 0)
        {
            // Back-reference: already printed
            return $"#{-state}#";
        }
        return null;
    }

    /// <summary>
    /// Top-level format entry point that handles *print-circle*.
    /// All public print functions should call this instead of FormatObject.
    /// </summary>
    public static string FormatTop(LispObject obj, bool escape)
    {
        // If circle detection already active (recursive call) or disabled, delegate
        if (!GetPrintCircle() || _circleTable != null)
            return FormatObject(obj, escape);

        // Only compound objects need circle detection
        if (!(obj is Cons || (obj is LispVector v && !v.IsCharVector) || obj is LispStruct))
            return FormatObject(obj, escape);

        // Pass 1: scan for shared objects
        var table = new Dictionary<object, int>(ReferenceEqualityComparer.Instance);
        ScanCircle(obj, table);

        // Check if any shared objects found (state == 0)
        bool hasShared = false;
        foreach (var kv in table)
            if (kv.Value == 0) { hasShared = true; break; }

        if (!hasShared) return FormatObject(obj, escape);

        // Pass 2: format with circle labels
        _circleTable = table;
        _circleLabelCounter = 0;
        try { return FormatObject(obj, escape); }
        finally { _circleTable = null; }
    }

    private static string FormatCons(Cons cons, bool escape)
    {
        if (_formatConsDepth >= MaxFormatConsDepth) return "(...)";
        _formatConsDepth++;
        try
        {
            var printLength = GetPrintLength();
            var parts = new List<string>();
            LispObject current = cons;
            var visited = new HashSet<Cons>(ReferenceEqualityComparer.Instance);
            int count = 0;
            while (current is Cons c)
            {
                // *print-circle*: check CDR for circularity (skip first element — handled by caller)
                if (_circleTable != null && count > 0 && _circleTable.TryGetValue(c, out int cdrState))
                {
                    if (cdrState < 0) // back-reference
                        return $"({string.Join(" ", parts)} . #{-cdrState}#)";
                    if (cdrState == 0) // shared CDR, first print — assign label
                    {
                        int label = ++_circleLabelCounter;
                        _circleTable[c] = -label;
                        string rest = FormatCons(c, escape);
                        return $"({string.Join(" ", parts)} . #{label}={rest})";
                    }
                }
                if (!visited.Add(c)) { parts.Add("..."); break; }
                if (printLength.HasValue && count >= printLength.Value) { parts.Add("..."); break; }
                parts.Add(FormatObject(c.Car, escape));
                current = c.Cdr;
                count++;
            }
            if (current is Nil || (current is Cons))
                return $"({string.Join(" ", parts)})";
            else
                return $"({string.Join(" ", parts)} . {FormatObject(current, escape)})";
        }
        finally { _formatConsDepth--; }
    }

    [System.ThreadStatic] private static bool _inStructPrintObjectDispatch;

    private static string FormatCompound(LispObject obj, bool escape)
    {
        if (_formatConsDepth >= MaxFormatConsDepth) return "#";
        _formatConsDepth++;
        try
        {
            if (obj is LispVector vec)
            {
                return FormatVector(vec, escape);
            }
            // LispStruct: check for specialized print-object method first
            if (obj is LispStruct st)
            {
                if (!_inStructPrintObjectDispatch)
                {
                    var poSym = Startup.Sym("PRINT-OBJECT");
                    if (poSym?.Function is GenericFunction gf && HasSpecializedPrintObjectMethod(gf, st))
                    {
                        _inStructPrintObjectDispatch = true;
                        try
                        {
                            var sw = new System.IO.StringWriter();
                            var stream = new LispStringOutputStream(sw);
                            gf.Invoke(new LispObject[] { st, stream });
                            return sw.ToString();
                        }
                        catch { }
                        finally { _inStructPrintObjectDispatch = false; }
                    }
                }
                return FormatStruct(st, escape);
            }
            return obj.ToString();
        }
        finally { _formatConsDepth--; }
    }

    private static bool HasSpecializedPrintObjectMethod(GenericFunction gf, LispStruct st)
    {
        var tClass = FindClass(Startup.Sym("T")) as LispClass;
        var structClass = FindClassOrNil(st.TypeName) as LispClass;
        if (structClass == null) return false;
        foreach (var method in gf.Methods)
        {
            if (method.Qualifiers.Length == 0
                && method.Specializers.Length >= 1
                && method.Specializers[0] is LispClass cls
                && cls != tClass
                && cls == structClass)
                return true;
        }
        return false;
    }

    private static string FormatStruct(LispStruct st, bool escape)
    {
        var printLength = GetPrintLength();
        // Look up struct class to get slot names
        var cls = Runtime.FindClassOrNil(st.TypeName) as LispClass;
        Symbol[]? slotNames = cls?.StructSlotNames;

        var sb = new System.Text.StringBuilder("#S(");
        sb.Append(st.TypeName.Name);

        int slotCount = st.Slots.Length;
        int limit = printLength.HasValue ? Math.Min(printLength.Value, slotCount) : slotCount;

        for (int i = 0; i < limit; i++)
        {
            sb.Append(' ');
            // Print slot keyword name if available
            if (slotNames != null && i < slotNames.Length)
            {
                sb.Append(':');
                sb.Append(slotNames[i].Name);
                sb.Append(' ');
            }
            sb.Append(FormatObject(st.Slots[i], escape));
        }

        if (printLength.HasValue && slotCount > printLength.Value)
            sb.Append(" ...");

        sb.Append(')');
        return sb.ToString();
    }

    private static bool GetPrintArray()
    {
        // Per CLHS: *print-readably* overrides *print-array*
        if (GetPrintReadably()) return true;
        LispObject val;
        if (DynamicBindings.TryGet(Startup.Sym("*PRINT-ARRAY*"), out val))
            return !(val is Nil);
        var sym = Startup.Sym("*PRINT-ARRAY*");
        if (sym.IsBound && sym.Value != null) return !(sym.Value is Nil);
        return true; // default: print array contents
    }

    private static string FormatVector(LispVector vec, bool escape)
    {
        int rank = vec.Rank;

        // When *print-array* is NIL, print non-string vectors as unreadable objects
        if (!GetPrintArray() && !vec.IsCharVector)
        {
            if (rank == 1)
                return $"#<(SIMPLE-VECTOR {vec.Length})>";
            return $"#<(ARRAY T ...)>";
        }
        // Rank-0 arrays
        if (rank == 0)
        {
            if (GetPrintReadably() && vec.ElementTypeName != "T")
            {
                var err = new LispError($"Cannot print array readably: element-type {vec.ElementTypeName} not representable");
                err.ConditionTypeName = "PRINT-NOT-READABLE";
                throw new LispErrorException(err);
            }
            var elem = vec.Length > 0 ? vec.ElementAt(0) : Nil.Instance;
            return $"#0A{FormatObject(elem, escape)}";
        }
        if (vec.IsBitVector)
        {
            if (rank == 1)
            {
                // Bit-vectors are not affected by *print-length* (CLHS 22.1.3.4)
                var sb = new System.Text.StringBuilder("#*");
                int len = vec.Length;
                for (int i = 0; i < len; i++)
                {
                    var elem = vec.ElementAt(i);
                    sb.Append(elem is Fixnum f ? f.Value.ToString() : "0");
                }
                // Bit vectors don't use "..." truncation per CLHS
                return sb.ToString();
            }
            // Multi-dimensional bit array
            return FormatMultiDimArray(vec, escape);
        }
        if (rank == 1)
        {
            if (GetPrintReadably() && vec.ElementTypeName != "T")
            {
                var err = new LispError($"Cannot print array readably: element-type {vec.ElementTypeName} not representable");
                err.ConditionTypeName = "PRINT-NOT-READABLE";
                throw new LispErrorException(err);
            }
            var printLength = GetPrintLength();
            // Per CLHS 22.1.3.10: *print-readably* implies *print-escape* is true
            bool vecEscape = escape || GetPrintReadably();
            int declaredSize = vec.Length;
            var parts = new List<string>();
            int limit = printLength.HasValue ? Math.Min(printLength.Value, declaredSize) : declaredSize;
            for (int i = 0; i < limit; i++)
                parts.Add(FormatObject(vec.ElementAt(i), vecEscape));
            if (printLength.HasValue && declaredSize > printLength.Value)
                parts.Add("...");
            return $"#({string.Join(" ", parts)})";
        }
        // Multi-dimensional array
        return FormatMultiDimArray(vec, escape);
    }

    private static string FormatMultiDimArray(LispVector vec, bool escape)
    {
        int rank = vec.Rank;
        var sb = new System.Text.StringBuilder();
        sb.Append($"#{rank}A");

        if (rank == 0)
        {
            // Already handled above, but just in case
            sb.Append(FormatObject(vec.Length > 0 ? vec.ElementAt(0) : Nil.Instance, escape));
            return sb.ToString();
        }

        // Get dimensions
        var dims = vec.Dimensions;

        // Specialized element-type can't be encoded in #nA(...) syntax
        if (GetPrintReadably() && vec.ElementTypeName != "T")
        {
            var err = new LispError($"Cannot print array readably: element-type {vec.ElementTypeName} not representable");
            err.ConditionTypeName = "PRINT-NOT-READABLE";
            throw new LispErrorException(err);
        }

        // Check if array has zero-size dimensions that make it unreadable
        // e.g., (0 3) prints as #2A() but reads back as (0 0) — not similar
        if (GetPrintReadably() && rank >= 2)
        {
            bool hasZero = false;
            bool hasNonZeroAfterZero = false;
            for (int d = 0; d < rank; d++)
            {
                if (dims[d] == 0) hasZero = true;
                else if (hasZero) { hasNonZeroAfterZero = true; break; }
            }
            if (hasNonZeroAfterZero)
            {
                var err = new LispError($"Cannot print array readably: dimensions ({string.Join(" ", dims)}) lose information");
                err.ConditionTypeName = "PRINT-NOT-READABLE";
                throw new LispErrorException(err);
            }
        }

        // Per CLHS 22.1.3.10: *print-readably* implies *print-escape* is true
        bool effectiveEscape = escape || GetPrintReadably();

        // Recursively build nested list structure
        // Pass current _formatConsDepth as base so leaf elements respect *print-level*
        // Subtract 1 because FormatCompound already incremented for the array object itself,
        // and depth=0 in FormatArrayDim represents the first paren level (which is 1 level deep).
        FormatArrayDim(vec, dims, 0, new int[rank], sb, effectiveEscape, 0, _formatConsDepth - 1);
        return sb.ToString();
    }

    private static void FormatArrayDim(LispVector vec, int[] dims, int dimIdx, int[] indices, System.Text.StringBuilder sb, bool escape, int depth, int baseConsDepth)
    {
        var printLevel = GetPrintLevel();
        // Check absolute depth (base context + array nesting) against *print-level*
        int absDepth = baseConsDepth + depth + 1;
        if (printLevel.HasValue && absDepth > printLevel.Value)
        {
            sb.Append('#');
            return;
        }
        var printLength = GetPrintLength();
        sb.Append('(');
        for (int i = 0; i < dims[dimIdx]; i++)
        {
            if (printLength.HasValue && i >= printLength.Value)
            {
                if (i > 0) sb.Append(' ');
                sb.Append("...");
                break;
            }
            if (i > 0) sb.Append(' ');
            indices[dimIdx] = i;
            if (dimIdx == dims.Length - 1)
            {
                // Leaf dimension - get actual element
                int rowMajor = 0;
                int multiplier = 1;
                for (int d = dims.Length - 1; d >= 0; d--)
                {
                    rowMajor += indices[d] * multiplier;
                    multiplier *= dims[d];
                }
                var elem = vec.ElementAt(rowMajor);
                // Set _formatConsDepth to reflect array nesting so nested objects
                // (like lists inside the array) respect *print-level*
                int saved = _formatConsDepth;
                _formatConsDepth = baseConsDepth + depth + 1;
                try { sb.Append(FormatObject(elem, escape)); }
                finally { _formatConsDepth = saved; }
            }
            else
            {
                FormatArrayDim(vec, dims, dimIdx + 1, indices, sb, escape, depth + 1, baseConsDepth);
            }
        }
        sb.Append(')');
    }

    /// <summary>
    /// Resolve *standard-output* to a TextWriter, handling all stream types.
    /// </summary>
    public static TextWriter GetStandardOutputWriter()
    {
        LispObject stdoutVal;
        if (!DynamicBindings.TryGet(Startup.Sym("*STANDARD-OUTPUT*"), out stdoutVal))
            stdoutVal = Startup.Sym("*STANDARD-OUTPUT*").Value!;
        if (stdoutVal is LispOutputStream os) return os.Writer;
        if (stdoutVal is LispBidirectionalStream bidi) return bidi.Writer;
        if (stdoutVal is LispFileStream fs && fs.IsOutput) return fs.OutputWriter!;
        if (stdoutVal is LispSynonymStream syn)
        {
            LispObject resolved;
            if (!DynamicBindings.TryGet(syn.Symbol, out resolved))
                resolved = syn.Symbol.Value!;
            if (resolved is LispOutputStream so) return so.Writer;
            if (resolved is LispBidirectionalStream sb) return sb.Writer;
            if (resolved is LispFileStream sf && sf.IsOutput) return sf.OutputWriter!;
            return Console.Out;
        }
        return Console.Out;
    }

    /// <summary>
    /// Resolve a stream designator to a TextWriter for output.
    /// NIL → *standard-output*, T → *terminal-io*, otherwise extract writer from stream object.
    /// </summary>
    public static TextWriter GetOutputWriter(LispObject stream)
    {
        if (stream is Nil) return GetStandardOutputWriter();
        if (stream is T)
        {
            LispObject tio;
            if (!DynamicBindings.TryGet(Startup.Sym("*TERMINAL-IO*"), out tio))
                tio = Startup.Sym("*TERMINAL-IO*").Value!;
            if (tio is LispBidirectionalStream bidi) return bidi.Writer;
            if (tio is LispTwoWayStream tws2) return GetOutputWriter(tws2.OutputStream);
            if (tio is LispOutputStream os) return os.Writer;
            return Console.Out;
        }
        if (stream is LispOutputStream los) return los.Writer;
        if (stream is LispBidirectionalStream bs) return bs.Writer;
        if (stream is LispTwoWayStream tws) return GetOutputWriter(tws.OutputStream);
        if (stream is LispFileStream fs && fs.IsOutput) return fs.OutputWriter!;
        if (stream is LispSynonymStream syn)
        {
            LispObject resolved;
            if (!DynamicBindings.TryGet(syn.Symbol, out resolved))
                resolved = syn.Symbol.Value!;
            if (resolved is LispOutputStream so) return so.Writer;
            if (resolved is LispBidirectionalStream sb) return sb.Writer;
            if (resolved is LispTwoWayStream stws) return GetOutputWriter(stws.OutputStream);
            if (resolved is LispFileStream sf && sf.IsOutput) return sf.OutputWriter!;
            return Console.Out;
        }
        return Console.Out;
    }

    public static LispObject Print(LispObject obj)
    {
        var w = GetStandardOutputWriter();
        w.Write('\n');
        PprintTrackWriteChar('\n');
        w.Write(FormatTop(obj, true));
        w.Write(' ');
        w.Flush();
        return obj;
    }

    public static LispObject Print2(LispObject obj, LispObject stream)
    {
        var w = GetOutputWriter(stream);
        w.Write('\n');
        w.Write(FormatTop(obj, true));
        w.Write(' ');
        w.Flush();
        return obj;
    }

    public static LispObject Prin1(LispObject obj)
    {
        var w = GetStandardOutputWriter();
        w.Write(FormatTop(obj, true));
        w.Flush();
        return obj;
    }

    public static LispObject Prin12(LispObject obj, LispObject stream)
    {
        var w = GetOutputWriter(stream);
        var text = FormatTop(obj, true);
        w.Write(text);
        PprintTrackWrite(text);
        w.Flush();
        return obj;
    }

    public static LispObject Princ(LispObject obj)
    {
        // CLHS: princ outputs as if by write with *print-escape* false, *print-readably* false
        var readablySym = Startup.Sym("*PRINT-READABLY*");
        DynamicBindings.Push(readablySym, Nil.Instance);
        try
        {
            var w = GetStandardOutputWriter();
            w.Write(FormatTop(obj, false));
            w.Flush();
            return obj;
        }
        finally
        {
            DynamicBindings.Pop(readablySym);
        }
    }

    public static LispObject Princ2(LispObject obj, LispObject stream)
    {
        // CLHS: princ outputs as if by write with *print-escape* false, *print-readably* false
        var readablySym = Startup.Sym("*PRINT-READABLY*");
        DynamicBindings.Push(readablySym, Nil.Instance);
        try
        {
            var w = GetOutputWriter(stream);
            var text = FormatTop(obj, false);
            w.Write(text);
            PprintTrackWrite(text);
            w.Flush();
            return obj;
        }
        finally
        {
            DynamicBindings.Pop(readablySym);
        }
    }

    public static LispObject WriteToString(LispObject obj)
    {
        // write-to-string: respects *print-escape* dynamic binding
        return new LispString(FormatTop(obj, GetPrintEscape()));
    }

    public static LispObject Prin1ToString(LispObject obj)
    {
        return new LispString(FormatTop(obj, true));
    }

    public static LispObject PrincToString(LispObject obj)
    {
        // CLHS: princ-to-string ≡ (write-to-string obj :escape nil :readably nil)
        var readablySym = Startup.Sym("*PRINT-READABLY*");
        DynamicBindings.Push(readablySym, Nil.Instance);
        try
        {
            return new LispString(FormatTop(obj, false));
        }
        finally
        {
            DynamicBindings.Pop(readablySym);
        }
    }

    /// <summary>
    /// Print list elements with pprint-logical-block and newlines between elements.
    /// Used by pprint-fill (FILL), pprint-linear (LINEAR), and pprint-tabular (FILL + tab).
    /// </summary>
    public static void PprintListWithNewlines(TextWriter w, LispObject list, bool colonP, string newlineKind, int tabSize = 0)
    {
        bool escape = GetPrintEscapePublic();
        if (list is not Cons)
        {
            var s = FormatTop(list, escape);
            w.Write(s);
            PprintTrackWrite(s);
            w.Flush();
            return;
        }
        // When *print-circle* is true, use FormatTop for proper shared structure detection
        if (GetPrintCircle())
        {
            var formatted = FormatTop(list, escape);
            if (!colonP && formatted.StartsWith("(") && formatted.EndsWith(")"))
                formatted = formatted.Substring(1, formatted.Length - 2);
            w.Write(formatted);
            PprintTrackWrite(formatted);
            w.Flush();
            return;
        }
        // Pre-format all elements to determine total width
        int? printLength = GetPrintLength();
        var elements = new List<string>();
        var cur2 = (LispObject)list;
        string? dottedTail = null;
        while (cur2 is Cons c2)
        {
            if (printLength.HasValue && elements.Count >= printLength.Value) break;
            elements.Add(FormatTop(c2.Car, escape));
            cur2 = c2.Cdr;
        }
        bool truncated = printLength.HasValue && elements.Count >= printLength.Value && cur2 is Cons;
        if (cur2 is not Nil && cur2 is not Cons)
            dottedTail = FormatTop(cur2, escape);

        // For LINEAR mode: check if entire output fits on one line
        bool linearBreak = false;
        if (newlineKind == "LINEAR")
        {
            int rightMargin = GetPrintRightMargin();
            int totalWidth = _pprintColumn; // current column position
            totalWidth += colonP ? 1 : 0; // prefix
            for (int ei = 0; ei < elements.Count; ei++)
            {
                if (ei > 0) totalWidth++; // space
                totalWidth += elements[ei].Length;
            }
            if (truncated) totalWidth += 4; // " ..."
            if (dottedTail != null) totalWidth += 3 + dottedTail.Length; // " . tail"
            totalWidth += colonP ? 1 : 0; // suffix
            linearBreak = totalWidth > rightMargin;
        }

        string prefix = colonP ? "(" : "";
        string suffix = colonP ? ")" : "";
        w.Write(prefix);
        PprintStartBlock(w, prefix.Length);
        if (suffix.Length > 0) PprintSetBlockSuffix(suffix.Length);
        try
        {
            for (int ei = 0; ei < elements.Count; ei++)
            {
                if (ei > 0)
                {
                    if (newlineKind == "LINEAR" && linearBreak)
                    {
                        PprintMandatoryNewline(w);
                    }
                    else
                    {
                        w.Write(' ');
                        PprintTrackWriteChar(' ');
                        PprintConditionalNewline(w, newlineKind);
                    }
                    if (tabSize > 0)
                        PprintTab(w, "SECTION-RELATIVE", 0, tabSize);
                }
                PprintFlushPendingBreak(w);
                w.Write(elements[ei]);
                PprintTrackWrite(elements[ei]);
            }
            if (truncated)
            {
                var dots = " ...";
                w.Write(dots);
                PprintTrackWrite(dots);
            }
            if (dottedTail != null)
            {
                var dot = " . " + dottedTail;
                w.Write(dot);
                PprintTrackWrite(dot);
            }
        }
        finally
        {
            PprintEndBlock();
        }
        w.Write(suffix);
        PprintTrackWrite(suffix);
        w.Flush();
    }

    public static LispObject Terpri(LispObject[] args)
    {
        if (args.Length > 1) throw new LispErrorException(new LispProgramError($"TERPRI: wrong number of arguments: {args.Length} (expected 0-1)"));
        LispObject stream;
        if (args.Length > 0 && args[0] is not Nil)
            stream = args[0] is T ? DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*")) : args[0];
        else
            stream = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
        var writer = GetTextWriter(stream);
        writer.Write('\n');
        PprintTrackWriteChar('\n');
        PprintAfterNewline(writer);
        return Nil.Instance;
    }

    public static LispObject FreshLine(LispObject[] args)
    {
        if (args.Length > 1) throw new LispErrorException(new LispProgramError($"FRESH-LINE: wrong number of arguments: {args.Length} (expected 0-1)"));
        LispObject stream;
        if (args.Length > 0 && args[0] is not Nil)
            stream = args[0] is T ? DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*")) : args[0];
        else
            stream = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));

        // Resolve to the actual output stream for AtLineStart check
        LispObject resolved = stream;
        while (resolved is LispEchoStream es2) resolved = es2.OutputStream;
        while (resolved is LispTwoWayStream tw2) resolved = tw2.OutputStream;
        while (resolved is LispSynonymStream syn2) resolved = DynamicBindings.Get(syn2.Symbol);

        // Check if at start of line
        bool atLineStart = false;
        if (resolved is LispStringOutputStream sout)
        {
            var str = sout.GetString();
            atLineStart = str.Length == 0 || str[str.Length - 1] == '\n';
        }
        else if (resolved is LispStream ls)
        {
            atLineStart = ls.AtLineStart;
        }

        if (atLineStart) return Nil.Instance;

        var writer = GetTextWriter(stream);
        writer.Write('\n');
        return T.Instance;
    }

    internal static void RegisterPrinterBuiltins()
    {
        // WRITE-TO-STRING: variadic with printer keyword args
        Emitter.CilAssembler.RegisterFunction("WRITE-TO-STRING", new LispFunction(args => {
            if (args.Length < 1) throw new Exception("WRITE-TO-STRING: requires at least 1 argument");
            var obj = args[0];
            if ((args.Length - 1) % 2 != 0)
                throw new LispErrorException(new LispProgramError("WRITE-TO-STRING: odd number of keyword arguments"));
            // Check :allow-other-keys (first occurrence wins)
            bool? wtsAllowOtherKeys = null;
            for (int i = 1; i < args.Length - 1; i += 2) {
                if (args[i] is Symbol ak && ak.Name == "ALLOW-OTHER-KEYS" && wtsAllowOtherKeys == null)
                    wtsAllowOtherKeys = !(args[i + 1] is Nil);
            }
            // Bind printer variables from keyword args
            var bindings = new System.Collections.Generic.List<(Symbol, LispObject)>();
            var wtsSeenKeys = new System.Collections.Generic.HashSet<string>();
            for (int i = 1; i < args.Length - 1; i += 2) {
                if (args[i] is not Symbol kw)
                    throw new LispErrorException(new LispProgramError("WRITE-TO-STRING: invalid keyword argument"));
                if (kw.Name == "ALLOW-OTHER-KEYS") continue;
                if (!wtsSeenKeys.Add(kw.Name)) continue; // duplicate: skip
                var printVar = kw.Name switch {
                    "BASE" => Startup.Sym("*PRINT-BASE*"),
                    "RADIX" => Startup.Sym("*PRINT-RADIX*"),
                    "ESCAPE" => Startup.Sym("*PRINT-ESCAPE*"),
                    "PRETTY" => Startup.Sym("*PRINT-PRETTY*"),
                    "CIRCLE" => Startup.Sym("*PRINT-CIRCLE*"),
                    "CASE" => Startup.Sym("*PRINT-CASE*"),
                    "GENSYM" => Startup.Sym("*PRINT-GENSYM*"),
                    "LEVEL" => Startup.Sym("*PRINT-LEVEL*"),
                    "LENGTH" => Startup.Sym("*PRINT-LENGTH*"),
                    "ARRAY" => Startup.Sym("*PRINT-ARRAY*"),
                    "READABLY" => Startup.Sym("*PRINT-READABLY*"),
                    "RIGHT-MARGIN" => Startup.Sym("*PRINT-RIGHT-MARGIN*"),
                    "LINES" => Startup.Sym("*PRINT-LINES*"),
                    "MISER-WIDTH" => Startup.Sym("*PRINT-MISER-WIDTH*"),
                    "PPRINT-DISPATCH" => Startup.Sym("*PRINT-PPRINT-DISPATCH*"),
                    _ => null
                };
                if (printVar != null)
                    bindings.Add((printVar, args[i + 1]));
                else if (wtsAllowOtherKeys != true)
                    throw new LispErrorException(new LispProgramError($"WRITE-TO-STRING: unknown keyword argument :{kw.Name}"));
            }
            // Push dynamic bindings
            foreach (var (sym, val) in bindings) DynamicBindings.Push(sym, val);
            try {
                return Runtime.WriteToString(obj);
            } finally {
                for (int i = bindings.Count - 1; i >= 0; i--) DynamicBindings.Pop(bindings[i].Item1);
            }
        }, "WRITE-TO-STRING", -1));
        // WRITE: variadic with :stream and printer keyword args
        Emitter.CilAssembler.RegisterFunction("WRITE", new LispFunction(args => {
            if (args.Length < 1) throw new Exception("WRITE: requires at least 1 argument");
            var obj = args[0];
            if ((args.Length - 1) % 2 != 0)
                throw new LispErrorException(new LispProgramError("WRITE: odd number of keyword arguments"));
            // Parse :stream keyword and printer variable bindings
            // Per CLHS 3.4.1.4: first value for duplicate keywords takes precedence
            LispObject? streamArg = null;
            bool streamSeen = false;
            var seenKeys = new System.Collections.Generic.HashSet<string>();
            var bindings = new System.Collections.Generic.List<(Symbol, LispObject)>();
            bool? allowOtherKeys = null;
            // First pass: check for :allow-other-keys (first occurrence wins per CLHS 3.4.1.4)
            for (int i = 1; i < args.Length - 1; i += 2) {
                if (args[i] is Symbol ak && ak.Name == "ALLOW-OTHER-KEYS" && allowOtherKeys == null)
                    allowOtherKeys = !(args[i + 1] is Nil);
            }
            for (int i = 1; i < args.Length - 1; i += 2) {
                if (args[i] is not Symbol kw)
                    throw new LispErrorException(new LispProgramError("WRITE: invalid keyword argument"));
                if (kw.Name == "ALLOW-OTHER-KEYS") continue;
                if (!seenKeys.Add(kw.Name)) continue; // duplicate keyword: skip
                if (kw.Name == "STREAM") {
                    streamArg = args[i + 1];
                    streamSeen = true;
                    continue;
                }
                var printVar = kw.Name switch {
                    "BASE" => Startup.Sym("*PRINT-BASE*"),
                    "RADIX" => Startup.Sym("*PRINT-RADIX*"),
                    "ESCAPE" => Startup.Sym("*PRINT-ESCAPE*"),
                    "PRETTY" => Startup.Sym("*PRINT-PRETTY*"),
                    "CIRCLE" => Startup.Sym("*PRINT-CIRCLE*"),
                    "CASE" => Startup.Sym("*PRINT-CASE*"),
                    "GENSYM" => Startup.Sym("*PRINT-GENSYM*"),
                    "LEVEL" => Startup.Sym("*PRINT-LEVEL*"),
                    "LENGTH" => Startup.Sym("*PRINT-LENGTH*"),
                    "ARRAY" => Startup.Sym("*PRINT-ARRAY*"),
                    "READABLY" => Startup.Sym("*PRINT-READABLY*"),
                    "RIGHT-MARGIN" => Startup.Sym("*PRINT-RIGHT-MARGIN*"),
                    "LINES" => Startup.Sym("*PRINT-LINES*"),
                    "MISER-WIDTH" => Startup.Sym("*PRINT-MISER-WIDTH*"),
                    "PPRINT-DISPATCH" => Startup.Sym("*PRINT-PPRINT-DISPATCH*"),
                    _ => null
                };
                if (printVar != null)
                    bindings.Add((printVar, args[i + 1]));
                else if (allowOtherKeys != true)
                    throw new LispErrorException(new LispProgramError($"WRITE: unknown keyword argument :{kw.Name}"));
            }
            // Resolve output stream
            TextWriter writer;
            if (streamArg == null || streamArg is Nil) {
                // Default: *standard-output*
                LispObject stdoutVal;
                if (!DynamicBindings.TryGet(Startup.Sym("*STANDARD-OUTPUT*"), out stdoutVal))
                    stdoutVal = Startup.Sym("*STANDARD-OUTPUT*").Value!;
                if (stdoutVal is LispOutputStream outs2) writer = outs2.Writer;
                else if (stdoutVal is LispBidirectionalStream bidi2) writer = bidi2.Writer;
                else if (stdoutVal is LispFileStream fs2 && fs2.IsOutput) writer = fs2.OutputWriter!;
                else if (stdoutVal is LispSynonymStream syn) {
                    LispObject resolved;
                    if (!DynamicBindings.TryGet(syn.Symbol, out resolved))
                        resolved = syn.Symbol.Value!;
                    if (resolved is LispOutputStream so) writer = so.Writer;
                    else if (resolved is LispBidirectionalStream sb) writer = sb.Writer;
                    else if (resolved is LispFileStream sf && sf.IsOutput) writer = sf.OutputWriter!;
                    else writer = Console.Out;
                }
                else writer = Console.Out;
            } else if (streamArg is LispOutputStream outs) {
                writer = outs.Writer;
            } else if (streamArg is LispBidirectionalStream bidi) {
                writer = bidi.Writer;
            } else if (streamArg is LispFileStream fs && fs.IsOutput) {
                writer = fs.OutputWriter!;
            } else if (streamArg is LispSynonymStream syn) {
                LispObject resolved;
                if (!DynamicBindings.TryGet(syn.Symbol, out resolved))
                    resolved = syn.Symbol.Value!;
                if (resolved is LispOutputStream so) writer = so.Writer;
                else if (resolved is LispBidirectionalStream sb) writer = sb.Writer;
                else if (resolved is LispFileStream sf && sf.IsOutput) writer = sf.OutputWriter!;
                else writer = Console.Out;
            } else if (streamArg == T.Instance) {
                // T means *terminal-io*
                LispObject tio;
                if (!DynamicBindings.TryGet(Startup.Sym("*TERMINAL-IO*"), out tio))
                    tio = Startup.Sym("*TERMINAL-IO*").Value!;
                if (tio is LispBidirectionalStream tbidi) writer = tbidi.Writer;
                else if (tio is LispTwoWayStream ttw) {
                    var outS = ttw.OutputStream;
                    if (outS is LispOutputStream to2) writer = to2.Writer;
                    else if (outS is LispStringOutputStream tso) writer = tso.Writer;
                    else if (outS is LispBidirectionalStream tb2) writer = tb2.Writer;
                    else if (outS is LispFileStream tf2 && tf2.IsOutput) writer = tf2.OutputWriter!;
                    else writer = Console.Out;
                }
                else if (tio is LispOutputStream touts) writer = touts.Writer;
                else if (tio is LispFileStream tfs && tfs.IsOutput) writer = tfs.OutputWriter!;
                else writer = Console.Out;
            } else {
                writer = Console.Out;
            }
            // Push dynamic bindings
            foreach (var (sym, val) in bindings) DynamicBindings.Push(sym, val);
            try {
                // Use FormatTop which respects print variables including *print-circle*
                bool escape = Runtime.GetPrintEscapePublic();
                var text = Runtime.FormatTop(obj, escape);
                Runtime.PprintFlushPendingBreak(writer);
                writer.Write(text);
                Runtime.PprintTrackWrite(text);
                writer.Flush();
                return obj;
            } finally {
                for (int i = bindings.Count - 1; i >= 0; i--) DynamicBindings.Pop(bindings[i].Item1);
            }
        }, "WRITE", -1));
        Startup.RegisterUnary("PRIN1-TO-STRING", Runtime.Prin1ToString);
        Startup.RegisterUnary("PRINC-TO-STRING", Runtime.PrincToString);

        // Print: PRINT, PRIN1, PRINC (1 or 2 args)
        Emitter.CilAssembler.RegisterFunction("PRINT",
            new LispFunction(args => {
                if (args.Length == 1) return Runtime.Print(args[0]);
                if (args.Length == 2) return Runtime.Print2(args[0], args[1]);
                throw new LispErrorException(new LispProgramError($"PRINT: wrong number of arguments: {args.Length} (expected 1-2)"));
            }, "PRINT", -1));
        Emitter.CilAssembler.RegisterFunction("PRIN1",
            new LispFunction(args => {
                if (args.Length == 1) return Runtime.Prin1(args[0]);
                if (args.Length == 2) return Runtime.Prin12(args[0], args[1]);
                throw new LispErrorException(new LispProgramError($"PRIN1: wrong number of arguments: {args.Length} (expected 1-2)"));
            }, "PRIN1", -1));
        Emitter.CilAssembler.RegisterFunction("PRINC",
            new LispFunction(args => {
                if (args.Length == 1) return Runtime.Princ(args[0]);
                if (args.Length == 2) return Runtime.Princ2(args[0], args[1]);
                throw new LispErrorException(new LispProgramError($"PRINC: wrong number of arguments: {args.Length} (expected 1-2)"));
            }, "PRINC", -1));

        // terpri: funcallable wrapper
        Emitter.CilAssembler.RegisterFunction("TERPRI",
            new LispFunction(args => Runtime.Terpri(args)));
        // fresh-line: funcallable wrapper
        Emitter.CilAssembler.RegisterFunction("FRESH-LINE",
            new LispFunction(args => Runtime.FreshLine(args)));

        // ===== Pretty-printer stubs =====
        // PPRINT: pretty-print object (stub: just prin1 + newline)
        Emitter.CilAssembler.RegisterFunction("PPRINT", new LispFunction(args => {
            if (args.Length < 1)
                throw new LispErrorException(new LispProgramError("PPRINT: requires at least 1 argument"));
            if (args.Length > 2)
                throw new LispErrorException(new LispProgramError($"PPRINT: too many arguments: {args.Length} (expected 1-2)"));
            var obj = args[0];
            var stream = args.Length >= 2 && args[1] is not Nil ? args[1] : (LispObject)Nil.Instance;
            var w = Runtime.GetOutputWriter(stream);
            // CLHS: pprint is like print but with *print-pretty* T and no trailing space
            DynamicBindings.Push(Startup.Sym("*PRINT-PRETTY*"), T.Instance);
            try {
                w.Write('\n');
                w.Write(Runtime.FormatTop(obj, true));
                w.Flush();
            } finally {
                DynamicBindings.Pop(Startup.Sym("*PRINT-PRETTY*"));
            }
            return MultipleValues.Values(); // pprint returns no values
        }, "PPRINT", -1));
        // PPRINT-NEWLINE: requires 1-2 args (kind &optional stream), kind must be valid
        Emitter.CilAssembler.RegisterFunction("PPRINT-NEWLINE", new LispFunction(args => {
            if (args.Length < 1)
                throw new LispErrorException(new LispProgramError("PPRINT-NEWLINE: requires at least 1 argument"));
            if (args.Length > 2)
                throw new LispErrorException(new LispProgramError("PPRINT-NEWLINE: too many arguments"));
            var kind = args[0];
            if (kind is not Symbol ks)
                throw new LispErrorException(new LispTypeError(
                    $"PPRINT-NEWLINE: invalid kind {kind}", kind, Startup.Sym("MEMBER")));
            string kn = ks.Name;
            if (kn != "LINEAR" && kn != "MISER" && kn != "FILL" && kn != "MANDATORY")
                throw new LispErrorException(new LispTypeError(
                    $"PPRINT-NEWLINE: invalid kind {kind}", kind, Startup.Sym("MEMBER")));
            // When *print-pretty* is T, handle newline kinds
            var prettyVal = DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out var pv) ? pv : Startup.Sym("*PRINT-PRETTY*").Value;
            if (prettyVal is not Nil)
            {
                var stream = args.Length >= 2 && args[1] is not Nil ? args[1] : (LispObject)Nil.Instance;
                var w = Runtime.GetOutputWriter(stream);
                if (kn == "MANDATORY")
                {
                    Runtime.PprintMandatoryNewline(w);
                    w.Flush();
                }
                else if (kn == "FILL" || kn == "LINEAR" || kn == "MISER")
                {
                    Runtime.PprintConditionalNewline(w, kn);
                    w.Flush();
                }
            }
            return Nil.Instance;
        }, "PPRINT-NEWLINE", -1));
        // PPRINT-INDENT: requires 2-3 args (relative-to n &optional stream)
        Emitter.CilAssembler.RegisterFunction("PPRINT-INDENT", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("PPRINT-INDENT: requires at least 2 arguments"));
            if (args.Length > 3)
                throw new LispErrorException(new LispProgramError("PPRINT-INDENT: too many arguments"));
            // Validate relative-to argument
            if (args[0] is not Symbol rel || (rel.Name != "BLOCK" && rel.Name != "CURRENT"))
                throw new LispErrorException(new LispError($"PPRINT-INDENT: invalid relative-to: {args[0]}, expected :BLOCK or :CURRENT"));
            // When *print-pretty* is T, update indentation
            var prettyVal = DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out var pv) ? pv : Startup.Sym("*PRINT-PRETTY*").Value;
            if (prettyVal is not Nil)
            {
                int n = args[1] switch
                {
                    Fixnum fx => (int)fx.Value,
                    SingleFloat sf => (int)sf.Value,
                    DoubleFloat df => (int)df.Value,
                    _ => 0
                };
                Runtime.PprintSetIndent(rel.Name, n);
            }
            return Nil.Instance;
        }, "PPRINT-INDENT", -1));
        // %PPRINT-START-BLOCK: internal helper (stream prefix-length &optional per-line-prefix) -> NIL
        Emitter.CilAssembler.RegisterFunction("%PPRINT-START-BLOCK", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("%PPRINT-START-BLOCK: requires 2 arguments"));
            var stream = args[0];
            var w = Runtime.GetOutputWriter(stream);
            int prefixLen = args[1] is Fixnum fx ? (int)fx.Value : 0;
            string? perLinePrefix = args.Length > 2 && args[2] is LispString plp ? plp.Value : null;
            Runtime.PprintStartBlock(w, prefixLen, perLinePrefix);
            return Nil.Instance;
        }, "%PPRINT-START-BLOCK", -1));
        // %PPRINT-END-BLOCK: internal helper () -> NIL
        Emitter.CilAssembler.RegisterFunction("%PPRINT-END-BLOCK", new LispFunction(args => {
            Runtime.PprintEndBlock();
            return Nil.Instance;
        }, "%PPRINT-END-BLOCK", -1));
        // %PPRINT-CIRCLE-SCAN: scan list for shared structure, returns T if scan started
        Emitter.CilAssembler.RegisterFunction("%PPRINT-CIRCLE-SCAN", new LispFunction(args => {
            if (args.Length < 1) return Nil.Instance;
            return Runtime.PprintCircleScan(args[0]) ? (LispObject)T.Instance : Nil.Instance;
        }, "%PPRINT-CIRCLE-SCAN", -1));
        // %PPRINT-CIRCLE-END: end circle detection
        Emitter.CilAssembler.RegisterFunction("%PPRINT-CIRCLE-END", new LispFunction(args => {
            Runtime.PprintCircleEnd();
            return Nil.Instance;
        }, "%PPRINT-CIRCLE-END", -1));
        // %PPRINT-CIRCLE-CHECK: check if list has circle label, returns label string or NIL
        Emitter.CilAssembler.RegisterFunction("%PPRINT-CIRCLE-CHECK", new LispFunction(args => {
            if (args.Length < 1) return Nil.Instance;
            var label = Runtime.PprintCircleCheckList(args[0]);
            return label != null ? (LispObject)new LispString(label) : Nil.Instance;
        }, "%PPRINT-CIRCLE-CHECK", -1));
        // PPRINT-TAB: requires 3-4 args (kind colnum colinc &optional stream)
        Emitter.CilAssembler.RegisterFunction("PPRINT-TAB", new LispFunction(args => {
            if (args.Length < 3)
                throw new LispErrorException(new LispProgramError("PPRINT-TAB: requires at least 3 arguments"));
            if (args.Length > 4)
                throw new LispErrorException(new LispProgramError("PPRINT-TAB: too many arguments"));
            // Validate kind argument
            var validKinds = new[] { "LINE", "SECTION", "LINE-RELATIVE", "SECTION-RELATIVE" };
            if (args[0] is not Symbol kindSym || !Array.Exists(validKinds, k => k == kindSym.Name))
                throw new LispErrorException(new LispError($"PPRINT-TAB: invalid kind: {args[0]}, expected :LINE, :SECTION, :LINE-RELATIVE, or :SECTION-RELATIVE"));
            // Check *print-pretty*
            var ppSym = Startup.Sym("*PRINT-PRETTY*");
            var ppVal = DynamicBindings.TryGet(ppSym, out var ppv) ? ppv : ppSym.Value;
            if (ppVal is Nil) return Nil.Instance;
            int colnum = args[1] is Fixnum cn ? (int)cn.Value : 0;
            int colinc = args[2] is Fixnum ci ? (int)ci.Value : 0;
            var streamArg2 = args.Length > 3 && args[3] is not Nil ? args[3] : (LispObject)Nil.Instance;
            var w = Runtime.GetOutputWriter(streamArg2);
            Runtime.PprintTab(w, kindSym.Name, colnum, colinc);
            return Nil.Instance;
        }, "PPRINT-TAB", -1));
        // PPRINT-FILL: prints list elements separated by fill-style newlines
        Emitter.CilAssembler.RegisterFunction("PPRINT-FILL", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("PPRINT-FILL: requires at least 2 arguments"));
            if (args.Length > 4)
                throw new LispErrorException(new LispProgramError($"PPRINT-FILL: too many arguments: {args.Length} (expected 2-4)"));
            var stream = args[0] is not Nil ? args[0] : (LispObject)Nil.Instance;
            var list = args[1];
            bool colonP = args.Length < 3 || args[2] is not Nil; // default T
            var w = Runtime.GetOutputWriter(stream);
            Runtime.PprintListWithNewlines(w, list, colonP, "FILL");
            return Nil.Instance;
        }, "PPRINT-FILL", -1));
        // PPRINT-LINEAR: prints list elements separated by linear-style newlines
        Emitter.CilAssembler.RegisterFunction("PPRINT-LINEAR", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("PPRINT-LINEAR: requires at least 2 arguments"));
            if (args.Length > 4)
                throw new LispErrorException(new LispProgramError($"PPRINT-LINEAR: too many arguments: {args.Length} (expected 2-4)"));
            var stream = args[0] is not Nil ? args[0] : (LispObject)Nil.Instance;
            var list = args[1];
            bool colonP = args.Length < 3 || args[2] is not Nil; // default T
            var w = Runtime.GetOutputWriter(stream);
            Runtime.PprintListWithNewlines(w, list, colonP, "LINEAR");
            return Nil.Instance;
        }, "PPRINT-LINEAR", -1));
        // PPRINT-TABULAR: prints list in tabular form with fill-style newlines and tab stops
        Emitter.CilAssembler.RegisterFunction("PPRINT-TABULAR", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("PPRINT-TABULAR: requires at least 2 arguments"));
            if (args.Length > 5)
                throw new LispErrorException(new LispProgramError($"PPRINT-TABULAR: too many arguments: {args.Length} (expected 2-5)"));
            var stream = args[0] is not Nil ? args[0] : (LispObject)Nil.Instance;
            var list = args[1];
            bool colonP = args.Length < 3 || args[2] is not Nil; // default T
            int tabSize = 16; // CLHS default
            if (args.Length >= 5 && args[4] is Fixnum ts) tabSize = (int)ts.Value;
            else if (args.Length >= 5 && args[4] is LispChar tc) tabSize = (int)tc.Value;
            if (tabSize <= 0) tabSize = 1;
            var w = Runtime.GetOutputWriter(stream);
            Runtime.PprintListWithNewlines(w, list, colonP, "FILL", tabSize);
            return Nil.Instance;
        }, "PPRINT-TABULAR", -1));
        // COPY-PPRINT-DISPATCH: (&optional table) — 0-1 args
        Emitter.CilAssembler.RegisterFunction("COPY-PPRINT-DISPATCH", new LispFunction(args => {
            if (args.Length > 1)
                throw new LispErrorException(new LispProgramError("COPY-PPRINT-DISPATCH: too many arguments"));
            LispObject source;
            if (args.Length == 0)
                source = Startup.Sym("*PRINT-PPRINT-DISPATCH*").Value ?? Nil.Instance;
            else
                source = args[0];

            if (source is Nil)
                return new LispPprintDispatchTable(); // fresh standard table
            if (source is LispPprintDispatchTable table)
                return new LispPprintDispatchTable(table);
            throw new LispErrorException(new LispTypeError(
                $"COPY-PPRINT-DISPATCH: argument must be a pprint dispatch table or NIL, got {source}",
                source, Startup.Sym("NULL")));
        }, "COPY-PPRINT-DISPATCH", -1));
        // PPRINT-DISPATCH: (object &optional table) — 1-2 args
        Emitter.CilAssembler.RegisterFunction("PPRINT-DISPATCH", new LispFunction(args => {
            if (args.Length < 1)
                throw new LispErrorException(new LispProgramError("PPRINT-DISPATCH: requires at least 1 argument"));
            if (args.Length > 2)
                throw new LispErrorException(new LispProgramError("PPRINT-DISPATCH: too many arguments"));
            return MultipleValues.Values(Nil.Instance, Nil.Instance);
        }, "PPRINT-DISPATCH", -1));
        // SET-PPRINT-DISPATCH: (type-specifier function &optional priority table)
        Emitter.CilAssembler.RegisterFunction("SET-PPRINT-DISPATCH", new LispFunction(args => {
            if (args.Length < 2)
                throw new LispErrorException(new LispProgramError("SET-PPRINT-DISPATCH: requires at least 2 arguments"));
            if (args.Length > 4)
                throw new LispErrorException(new LispProgramError("SET-PPRINT-DISPATCH: too many arguments"));
            var typeSpec = args[0];
            var func = args[1];
            double priority = 0;
            if (args.Length >= 3)
            {
                var prio = args[2];
                priority = prio switch {
                    Fixnum f => (double)f.Value,
                    SingleFloat sf => (double)sf.Value,
                    DoubleFloat df => df.Value,
                    Ratio r => (double)r.Numerator / (double)r.Denominator,
                    Bignum b => (double)b.Value,
                    _ => throw new LispErrorException(new LispTypeError(
                        $"SET-PPRINT-DISPATCH: priority must be a real number, got {prio}", prio, Startup.Sym("REAL")))
                };
            }
            var table = (args.Length >= 4 && args[3] is not Nil)
                ? args[3] as LispPprintDispatchTable
                    ?? throw new LispErrorException(new LispTypeError("SET-PPRINT-DISPATCH: invalid table", args[3]))
                : DynamicBindings.Get(Startup.Sym("*PRINT-PPRINT-DISPATCH*")) as LispPprintDispatchTable;
            if (table != null)
            {
                var key = Runtime.FormatObjectNoDispatch(typeSpec);
                if (func is Nil)
                    table.Entries.Remove(key);
                else
                    table.Entries[key] = (typeSpec, func, priority);
            }
            return Nil.Instance;
        }, "SET-PPRINT-DISPATCH", -1));
        // PPRINT-EXIT-IF-LIST-EXHAUSTED: must be called inside pprint-logical-block
        Emitter.CilAssembler.RegisterFunction("PPRINT-EXIT-IF-LIST-EXHAUSTED", new LispFunction(args => {
            throw new LispErrorException(new LispError("PPRINT-EXIT-IF-LIST-EXHAUSTED: must be called inside PPRINT-LOGICAL-BLOCK"));
        }, "PPRINT-EXIT-IF-LIST-EXHAUSTED", -1));
        // PPRINT-POP: must be called inside pprint-logical-block
        Emitter.CilAssembler.RegisterFunction("PPRINT-POP", new LispFunction(args => {
            throw new LispErrorException(new LispError("PPRINT-POP: must be called inside PPRINT-LOGICAL-BLOCK"));
        }, "PPRINT-POP", -1));

        // PRINT-OBJECT: the standard GF for printing
        if (Startup.Sym("PRINT-OBJECT").Function == null) {
            var fn = new LispFunction(args => {
                if (args.Length < 2) throw new LispErrorException(new LispProgramError("PRINT-OBJECT: requires 2 arguments"));
                var writer = Runtime.GetOutputWriter(args[1]);
                writer.Write(Runtime.FormatTop(args[0], true));
                writer.Flush();
                return args[0];
            }, "PRINT-OBJECT", -1);
            Emitter.CilAssembler.RegisterFunction("PRINT-OBJECT", fn);
            Startup.Sym("PRINT-OBJECT").Function = fn;
        }
    }

}
