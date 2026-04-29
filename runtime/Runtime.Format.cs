namespace DotCL;

public static partial class Runtime
{
    // --- Format ---

    public static LispObject Format(LispObject dest, LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispError("FORMAT: missing format string"));

        // Format control can be a string or a function (e.g. from formatter)
        if (args[0] is LispFunction fn)
        {
            // Function format control: call with stream and remaining args
            var formatArgs = new LispObject[args.Length]; // fn(stream, arg1, arg2, ...)
            Array.Copy(args, 1, formatArgs, 1, args.Length - 1);

            if (dest is Nil)
            {
                var sw = new System.IO.StringWriter();
                var stream = new LispStringOutputStream(sw);
                formatArgs[0] = stream;
                fn.Invoke(formatArgs);
                return new LispString(stream.GetString());
            }
            else if (dest is T)
            {
                var stdout = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
                formatArgs[0] = stdout;
                fn.Invoke(formatArgs);
                return Nil.Instance;
            }
            else
            {
                formatArgs[0] = dest;
                fn.Invoke(formatArgs);
                return Nil.Instance;
            }
        }

        var formatString = args[0] switch
        {
            LispString s => s.Value,
            _ => throw new LispErrorException(new LispTypeError("FORMAT: format string must be a string", args[0]))
        };

        var formatArgs2 = new LispObject[args.Length - 1];
        Array.Copy(args, 1, formatArgs2, 0, formatArgs2.Length);

        if (dest is Nil)
        {
            var result = FormatString(formatString, formatArgs2);
            return new LispString(result);
        }

        // Resolve the output stream to check AtLineStart
        LispObject resolvedStream = dest;
        if (dest is T)
            resolvedStream = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
        LispObject resolved = resolvedStream;
        while (resolved is LispEchoStream es2) resolved = es2.OutputStream;
        while (resolved is LispTwoWayStream tw2) resolved = tw2.OutputStream;
        while (resolved is LispSynonymStream syn2) resolved = DynamicBindings.Get(syn2.Symbol);
        bool atLineStart = resolved is LispStream ls2 ? ls2.AtLineStart : true;

        var result2 = FormatString(formatString, formatArgs2, atLineStart);

        // Write result and update AtLineStart
        if (dest is T)
        {
            if (resolvedStream is LispOutputStream os)
            {
                os.Writer.Write(result2);
                if (_pprintActive) PprintTrackWrite(result2);
            }
            else
                Console.Write(result2);
        }
        else if (dest is LispBroadcastStream bs)
        {
            foreach (var s in bs.Streams)
                GetTextWriter(s).Write(result2);
            if (_pprintActive) PprintTrackWrite(result2);
        }
        else if (dest is LispStream)
        {
            try
            {
                GetTextWriter(dest).Write(result2);
                if (_pprintActive) PprintTrackWrite(result2);
            }
            catch { throw new LispErrorException(new LispTypeError("FORMAT: invalid destination", dest)); }
        }
        else
        {
            throw new LispErrorException(new LispTypeError("FORMAT: invalid destination", dest));
        }

        // Update AtLineStart on the resolved stream
        if (resolved is LispStream ls3 && result2.Length > 0)
            ls3.AtLineStart = result2[result2.Length - 1] == '\n';

        return dest is Nil ? new LispString(result2) : Nil.Instance;
    }

    private static string ToRadixString(long value, int radix)
    {
        const string digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        if (radix < 2 || radix > 36) throw new ArgumentException($"Invalid radix: {radix}");
        if (value == 0) return "0";
        bool neg = value < 0;
        ulong v = neg ? (ulong)(-value) : (ulong)value;
        var chars = new List<char>();
        while (v > 0) { chars.Add(digits[(int)(v % (ulong)radix)]); v /= (ulong)radix; }
        if (neg) chars.Add('-');
        chars.Reverse();
        return new string(chars.ToArray());
    }

    private static string BigIntToRadixString(System.Numerics.BigInteger value, int radix)
    {
        const string digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        if (radix < 2 || radix > 36) throw new ArgumentException($"Invalid radix: {radix}");
        if (value == 0) return "0";
        bool neg = value < 0;
        var v = neg ? -value : value;
        var chars = new List<char>();
        var bigRadix = new System.Numerics.BigInteger(radix);
        while (v > 0) { chars.Add(digits[(int)(v % bigRadix)]); v /= bigRadix; }
        if (neg) chars.Add('-');
        chars.Reverse();
        return new string(chars.ToArray());
    }

    private static string ToCardinal(long value)
    {
        if (value == 0) return "zero";
        if (value < 0) return "negative " + ToCardinal(-value);
        if (value >= 1000000000000L) // trillions
        {
            string s = ToCardinal(value / 1000000000000L) + " trillion";
            long rem = value % 1000000000000L;
            return rem > 0 ? s + " " + ToCardinal(rem) : s;
        }
        if (value >= 1000000000)
        {
            string s = ToCardinal(value / 1000000000) + " billion";
            long rem = value % 1000000000;
            return rem > 0 ? s + " " + ToCardinal(rem) : s;
        }
        if (value >= 1000000)
        {
            string s = ToCardinal(value / 1000000) + " million";
            long rem = value % 1000000;
            return rem > 0 ? s + " " + ToCardinal(rem) : s;
        }
        if (value >= 1000)
        {
            string s = ToCardinal(value / 1000) + " thousand";
            long rem = value % 1000;
            return rem > 0 ? s + " " + ToCardinal(rem) : s;
        }
        if (value >= 100)
        {
            string s = ToCardinal(value / 100) + " hundred";
            long rem = value % 100;
            return rem > 0 ? s + " " + ToCardinal(rem) : s;
        }
        string[] ones = { "", "one", "two", "three", "four", "five", "six", "seven",
                          "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                          "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" };
        if (value < 20) return ones[value];
        string[] tens = { "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" };
        string t = tens[value / 10];
        long o = value % 10;
        return o > 0 ? t + "-" + ones[o] : t;
    }

    private static string ToOrdinal(long value)
    {
        if (value == 0) return "zeroth";
        if (value < 0) return "negative " + ToOrdinal(-value);

        // Special cases for 1-20 and tens
        string[] ordOnes = { "", "first", "second", "third", "fourth", "fifth", "sixth", "seventh",
                             "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth",
                             "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth" };
        if (value < 20) return ordOnes[value];

        string[] ordTens = { "", "", "twentieth", "thirtieth", "fortieth", "fiftieth",
                             "sixtieth", "seventieth", "eightieth", "ninetieth" };

        // If the last part is a round ten
        if (value >= 20 && value < 100 && value % 10 == 0)
            return ordTens[value / 10];

        // If last part is 1-19 in the ones place
        if (value >= 20 && value < 100)
        {
            string[] tens = { "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" };
            return tens[value / 10] + "-" + ordOnes[value % 10];
        }

        // For larger numbers: cardinal for everything except the last word
        // Split into "prefix" (everything above the last group) and "suffix" (last group)
        // e.g., 142 => "one hundred " + ordinal(42)
        if (value >= 100)
        {
            // Find the appropriate split point
            long divisor = 100;
            string unitName = "hundred";
            if (value >= 1000000000000L) { divisor = 1000000000000L; unitName = "trillion"; }
            else if (value >= 1000000000) { divisor = 1000000000; unitName = "billion"; }
            else if (value >= 1000000) { divisor = 1000000; unitName = "million"; }
            else if (value >= 1000) { divisor = 1000; unitName = "thousand"; }

            long rem = value % divisor;
            if (rem == 0)
            {
                // e.g., "one hundredth", "two thousandth"
                return ToCardinal(value / divisor) + " " + unitName + "th";
            }
            else
            {
                return ToCardinal(value / divisor * divisor) + " " + ToOrdinal(rem);
            }
        }

        return ToCardinal(value) + "th"; // fallback
    }

    private static string ToRoman(long value)
    {
        if (value <= 0 || value >= 4000) return value.ToString(); // out of range
        string[] thousands = { "", "M", "MM", "MMM" };
        string[] hundreds = { "", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM" };
        string[] tens = { "", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC" };
        string[] ones = { "", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" };
        return thousands[value / 1000] + hundreds[(value % 1000) / 100] + tens[(value % 100) / 10] + ones[value % 10];
    }

    private static string ToOldRoman(long value)
    {
        if (value <= 0) return value.ToString(); // out of range
        // Old Roman: additive notation, no limit on M's
        var sb = new System.Text.StringBuilder();
        long ms = value / 1000;
        for (long i = 0; i < ms; i++) sb.Append('M');
        string[] hundreds = { "", "C", "CC", "CCC", "CCCC", "D", "DC", "DCC", "DCCC", "DCCCC" };
        string[] tens = { "", "X", "XX", "XXX", "XXXX", "L", "LX", "LXX", "LXXX", "LXXXX" };
        string[] ones = { "", "I", "II", "III", "IIII", "V", "VI", "VII", "VIII", "VIIII" };
        sb.Append(hundreds[(value % 1000) / 100]);
        sb.Append(tens[(value % 100) / 10]);
        sb.Append(ones[value % 10]);
        return sb.ToString();
    }

    /// <summary>Exception used as control flow for ~^ (up-and-out) in FORMAT.</summary>
    private class FormatUpAndOutException : Exception
    {
        public string PartialOutput { get; set; } = "";
        [ThreadStatic] private static FormatUpAndOutException? _instance;
        public static FormatUpAndOutException GetInstance(string partial)
        {
            _instance ??= new FormatUpAndOutException();
            _instance.PartialOutput = partial;
            return _instance;
        }
        private FormatUpAndOutException() : base() { }
    }

    /// <summary>Exception used as control flow for ~:^ (colon up-and-out) in FORMAT.
    /// Terminates the enclosing ~:{...~} or ~:@{...~} iteration.</summary>
    private class FormatColonUpAndOutException : Exception
    {
        public string PartialOutput { get; set; } = "";
        [ThreadStatic] private static FormatColonUpAndOutException? _instance;
        public static FormatColonUpAndOutException GetInstance(string partial)
        {
            _instance ??= new FormatColonUpAndOutException();
            _instance.PartialOutput = partial;
            return _instance;
        }
        private FormatColonUpAndOutException() : base() { }
    }

    /// <summary>Thread-static: true when the current sublist iteration is the last one.
    /// Used by ~:^ to decide whether to terminate the enclosing ~:{ or ~:@{ iteration.</summary>
    [ThreadStatic] private static bool _isLastSublist;

    private static string? GetCharacterName(char c)
    {
        // Delegate to Runtime.CharName for consistency with (char-name c)
        return Runtime.CharName(c);
    }

    /// <summary>
    /// Format a floating-point number per CLHS 22.3.3.1 (~F directive).
    /// Parameters: w,d,k,overflowchar,padchar
    /// </summary>
    private static string FormatFixedFloat(double value, int? w, int? d, int k, char? overflowChar, char padChar, bool atSign, bool isSingle, bool isDouble)
    {
        // Apply scale factor: the printed value is value * 10^k
        double scaled = value * Math.Pow(10.0, k);
        bool negative = double.IsNegative(scaled); // handles -0.0
        double absVal = Math.Abs(scaled);

        string result;

        if (d == null && w == null)
        {
            // Free-format: like prin1 but without exponent marker, in fixed-point notation
            // Use "R" (roundtrip) format then strip exponent notation
            string raw;
            if (isSingle)
                raw = ((float)absVal).ToString("R", System.Globalization.CultureInfo.InvariantCulture);
            else
                raw = absVal.ToString("R", System.Globalization.CultureInfo.InvariantCulture);

            // If the roundtrip format uses exponent notation, convert to fixed
            if (raw.Contains('E') || raw.Contains('e'))
            {
                // Parse and re-format in fixed notation
                if (isSingle)
                {
                    float fv = float.Parse(raw, System.Globalization.CultureInfo.InvariantCulture);
                    // Determine enough decimal digits
                    raw = fv.ToString("G9", System.Globalization.CultureInfo.InvariantCulture);
                    if (raw.Contains('E') || raw.Contains('e'))
                    {
                        // Use decimal to avoid losing precision
                        int digits = Math.Max(0, -(int)Math.Floor(Math.Log10(fv)) + 8);
                        raw = fv.ToString($"F{digits}", System.Globalization.CultureInfo.InvariantCulture);
                        // Trim trailing zeros but keep at least one digit after dot
                        raw = TrimTrailingZerosFixed(raw);
                    }
                }
                else
                {
                    int digits = Math.Max(0, -(int)Math.Floor(Math.Log10(absVal)) + 16);
                    raw = absVal.ToString($"F{digits}", System.Globalization.CultureInfo.InvariantCulture);
                    raw = TrimTrailingZerosFixed(raw);
                }
            }

            if (!raw.Contains('.'))
                raw += ".0";

            string sign = negative ? "-" : (atSign ? "+" : "");
            result = sign + raw;
        }
        else if (d == null)
        {
            // w specified but d not: choose d to fill the width per CLHS 22.3.3.1
            // "A value is chosen for d in such a way that as many digits as possible
            //  may be printed subject to the width constraint imposed by w, and the
            //  constraint that no trailing zero digits may appear in the fraction,
            //  except that if the fraction to be printed is zero, then a single zero
            //  digit should appear after the decimal point if the width constraint allows it."
            string sign = negative ? "-" : (atSign ? "+" : "");

            // Determine the integer part string
            string intStr;
            if (absVal >= 1e15)
                intStr = ((long)absVal).ToString();
            else
                intStr = ((long)Math.Truncate(absVal)).ToString();

            // Compute available decimal digits within width w
            // Result is: sign + intStr + "." + decimals
            int minChars = sign.Length + intStr.Length + 1; // sign + int + dot
            bool canDropLeadingZero = (intStr == "0");

            int computedD;
            if (w.HasValue)
            {
                computedD = Math.Max(0, w.Value - minChars);
                // If no room and leading zero can be dropped, try that
                if (computedD == 0 && canDropLeadingZero)
                {
                    int minCharsDropped = sign.Length + 1; // sign + dot (no "0")
                    computedD = Math.Max(0, w.Value - minCharsDropped);
                }
            }
            else
            {
                computedD = 6; // fallback
            }

            // Round to computedD digits
            string formatted = absVal.ToString($"F{computedD}", System.Globalization.CultureInfo.InvariantCulture);
            if (!formatted.Contains('.'))
                formatted += ".";

            // Remove trailing zeros per CLHS, but keep one "0" if fraction is zero
            int dotPos = formatted.IndexOf('.');
            string fracPart = formatted.Substring(dotPos + 1);
            string trimmedFrac = fracPart.TrimEnd('0');

            if (trimmedFrac.Length == 0)
            {
                // Fraction is zero: keep one "0" if width allows
                formatted = formatted.Substring(0, dotPos + 1) + "0";
            }
            else
            {
                formatted = formatted.Substring(0, dotPos + 1) + trimmedFrac;
            }

            // Optionally drop leading zero before decimal for narrow width
            if (canDropLeadingZero && w.HasValue && (sign.Length + formatted.Length) > w.Value)
            {
                // Drop the leading "0": "0.xxx" -> ".xxx"
                formatted = formatted.Substring(1); // remove "0", keep ".xxx"
            }

            result = sign + formatted;

            if (w.HasValue && result.Length < w.Value)
            {
                result = new string(padChar, w.Value - result.Length) + result;
            }
        }
        else
        {
            // d is specified: exactly d digits after decimal point
            string formatted = absVal.ToString($"F{d.Value}", System.Globalization.CultureInfo.InvariantCulture);
            // CLHS requires a decimal point even when d=0
            if (d.Value == 0 && !formatted.Contains('.'))
                formatted += ".";

            string sign = negative ? "-" : (atSign ? "+" : "");
            result = sign + formatted;

            if (w.HasValue)
            {
                if (result.Length > w.Value)
                {
                    // Try to squeeze: if there's a leading zero before the decimal point
                    // and the integer part is just "0", we can drop it
                    // e.g., "0.50" with w=3 -> ".50"
                    int dotPos = result.IndexOf('.');
                    if (dotPos >= 0)
                    {
                        string intPart = result.Substring(0, dotPos);
                        string fracPart = result.Substring(dotPos); // includes '.'
                        string signPart = "";
                        string digits3 = intPart;
                        if (intPart.StartsWith("-") || intPart.StartsWith("+"))
                        {
                            signPart = intPart.Substring(0, 1);
                            digits3 = intPart.Substring(1);
                        }
                        if (digits3 == "0" && fracPart.Length > 1)
                        {
                            // Drop leading zero, but only if there are digits after the dot
                            // "0." alone cannot become "." (no digit at all)
                            result = signPart + fracPart;
                        }
                    }
                }

                if (result.Length > w.Value && overflowChar.HasValue)
                {
                    result = new string(overflowChar.Value, w.Value);
                }
                else if (result.Length < w.Value)
                {
                    result = new string(padChar, w.Value - result.Length) + result;
                }
                // If result.Length > w.Value and no overflowchar, output as-is (CLHS allows this)
            }
        }

        return result;
    }

    /// <summary>
    /// Trim trailing zeros from a fixed-format number string, keeping at least one digit after the decimal point.
    /// </summary>
    private static string TrimTrailingZerosFixed(string s)
    {
        int dotPos = s.IndexOf('.');
        if (dotPos < 0) return s;
        int lastNonZero = s.Length - 1;
        while (lastNonZero > dotPos + 1 && s[lastNonZero] == '0')
            lastNonZero--;
        return s.Substring(0, lastNonZero + 1);
    }

    /// <summary>
    /// Format a floating-point number in exponential notation (~E directive).
    /// Parameters: w,d,e,k,overflowchar,padchar,exponentchar
    /// </summary>
    // Parse a .NET-formatted scientific/fixed string into raw digits + decimal exponent.
    // Result: allDigits contains significant digits (leading digit nonzero unless value==0),
    // msdExp = base-10 exponent of the most-significant digit,
    // so the value = digits * 10^(msdExp - allDigits.Length + 1).
    // E.g., "9.6342800939043076E-322" -> allDigits="96342800939043076", msdExp=-322.
    private static void ExtractScientificDigits(string s, out string allDigits, out int msdExp)
    {
        if (s.Length > 0 && (s[0] == '-' || s[0] == '+')) s = s.Substring(1);
        int eIdx = s.IndexOf('E');
        if (eIdx < 0) eIdx = s.IndexOf('e');
        int exp = 0;
        string mant = s;
        if (eIdx >= 0)
        {
            exp = int.Parse(s.Substring(eIdx + 1), System.Globalization.CultureInfo.InvariantCulture);
            mant = s.Substring(0, eIdx);
        }
        int dotIdx = mant.IndexOf('.');
        string digits;
        int msdPosFromFirst; // exponent offset of the first digit
        if (dotIdx < 0)
        {
            // "12345" with possible exponent
            digits = mant;
            msdPosFromFirst = digits.Length - 1; // first digit is at 10^(len-1)
        }
        else
        {
            string intPart = mant.Substring(0, dotIdx);
            string fracPart = mant.Substring(dotIdx + 1);
            if (intPart.Length == 0 || (intPart == "0"))
            {
                // "0.00012345" → leading zeros in frac define exponent
                int lz = 0;
                while (lz < fracPart.Length && fracPart[lz] == '0') lz++;
                digits = fracPart.Substring(lz);
                msdPosFromFirst = -(lz + 1);
            }
            else
            {
                digits = intPart + fracPart;
                msdPosFromFirst = intPart.Length - 1;
            }
        }
        // Strip trailing zeros (they are not significant), but keep at least one digit
        int lastNZ = digits.Length - 1;
        while (lastNZ > 0 && digits[lastNZ] == '0') lastNZ--;
        digits = digits.Substring(0, lastNZ + 1);
        if (digits.Length == 0) digits = "0";
        allDigits = digits;
        msdExp = exp + msdPosFromFirst;
    }

    private static bool HasNonZeroAfter(string s, int start)
    {
        for (int i = start; i < s.Length; i++)
            if (s[i] != '0') return true;
        return false;
    }

    private static string FormatExponentialFloat(double value, int? w, int? d, int? e, int k,
        char? overflowChar, char padChar, char? exponentChar, bool atSign, bool isSingle, bool isDouble)
    {
        bool negative = double.IsNegative(value);
        double absVal = Math.Abs(value);

        // Determine exponent character (CLHS default: E per prin1 convention)
        char expChar = exponentChar ?? 'E';

        // Handle zero
        if (absVal == 0.0)
        {
            int dVal = d ?? 1;
            int eVal = e ?? 1;
            string zSign = negative ? "-" : (atSign ? "+" : "");
            // k digits before decimal (when k>=1), rest after
            string digits;
            if (k <= 0)
            {
                // 0 digits before decimal, then |k| zeros, then dVal+k zeros for fraction
                digits = "0." + new string('0', Math.Max(dVal, 1));
            }
            else
            {
                // k-1 zeros before decimal, then dVal-k+1 zeros after
                digits = new string('0', k) + "." + new string('0', Math.Max(dVal - k + 1, 0));
                // But if fraction is empty, keep at least the dot
            }
            // Trim to match d digits after decimal if d specified
            if (d.HasValue)
            {
                int dotIdx = digits.IndexOf('.');
                string intPart = digits.Substring(0, dotIdx);
                int fracLen = k <= 0 ? dVal : dVal - k + 1;
                if (fracLen < 0) fracLen = 0;
                digits = intPart + "." + new string('0', fracLen);
            }
            string expStr = expChar + "+";
            string expDigits = "0";
            if (e.HasValue) expDigits = expDigits.PadLeft(eVal, '0');
            string result = zSign + digits + expStr + expDigits;
            if (w.HasValue)
            {
                if (result.Length > w.Value && overflowChar.HasValue)
                    return new string(overflowChar.Value, w.Value);
                if (result.Length < w.Value)
                    result = new string(padChar, w.Value - result.Length) + result;
            }
            return result;
        }

        // Compute the exponent via high-precision string roundtrip.
        // Using Math.Log10 + Math.Pow causes rounding errors for values
        // outside normal range (subnormals, near-infinity); ToString("G17"+)
        // uses .NET's correctly-rounded algorithm that works for subnormals.
        int exponent;
        {
            // "G17" gives exact 17-digit round-trip for all doubles including subnormals
            string gs = absVal.ToString("G17", System.Globalization.CultureInfo.InvariantCulture);
            int eIdxG = gs.IndexOf('E');
            if (eIdxG < 0) eIdxG = gs.IndexOf('e');
            if (eIdxG >= 0)
            {
                exponent = int.Parse(gs.Substring(eIdxG + 1), System.Globalization.CultureInfo.InvariantCulture) + 1;
            }
            else
            {
                // Fixed notation (no exponent in string): compute manually
                int dotIdxG = gs.IndexOf('.');
                string ipart = dotIdxG < 0 ? gs : gs.Substring(0, dotIdxG);
                if (ipart == "0")
                {
                    // "0.00012345" form: count leading zeros after dot
                    string frac = dotIdxG < 0 ? "" : gs.Substring(dotIdxG + 1);
                    int lz = 0;
                    while (lz < frac.Length && frac[lz] == '0') lz++;
                    exponent = lz < frac.Length ? -lz : 0;
                }
                else
                {
                    exponent = ipart.Length;
                }
            }
        }
        // Adjust for scale factor k: the number printed is absVal / 10^(exponent-k)
        // meaning k digits appear before the decimal point
        int adjustedExp = exponent - k;

        // Determine d (number of digits after the decimal point)
        int dActual;
        bool dOmitted = !d.HasValue;
        bool trimTrailingZeros = false;
        if (d.HasValue)
        {
            dActual = d.Value;
        }
        else if (!w.HasValue)
        {
            // d omitted, no width constraint: use round-trip accurate representation (same as prin1).
            // Compute via NormalizePrinterFloat to avoid floating-point errors in significand.
            string rStr = isSingle
                ? ((float)value).ToString("R", System.Globalization.CultureInfo.InvariantCulture)
                : value.ToString("R", System.Globalization.CultureInfo.InvariantCulture);
            rStr = NormalizePrinterFloat(rStr, absVal);
            // rStr is now scientific like "1.23456789E7" or "-1.0E-4"
            int rEIdx = rStr.IndexOf('E');
            if (rEIdx >= 0)
            {
                string rSigFull = rStr.Substring(0, rEIdx).TrimStart('-'); // e.g. "1.23456789"
                string rExpStr = rStr.Substring(rEIdx + 1);                // e.g. "7" or "-4"
                bool rExpNeg = rExpStr.StartsWith("-");
                int rExp = (rExpNeg ? -1 : 1) * int.Parse(rExpStr.TrimStart('-')); // exponent in "1-digit-before-dot" form

                // Apply k shift: move decimal point (k-1) places right, adjust exponent
                // For default k=1: no shift needed, exponent stays at rExp
                int kShift = k - 1; // positive = move right
                int finalExp = rExp - kShift;

                // Shift the decimal in rSigFull by kShift positions
                int dotPos = rSigFull.IndexOf('.');
                string allDigitsR = dotPos < 0 ? rSigFull : rSigFull.Replace(".", "");
                // dotPos from left in allDigitsR: was dotPos (0-indexed after leading digit)
                // After k-shift: new dot position in allDigitsR = dotPos + kShift
                int newDotPos = (dotPos < 0 ? allDigitsR.Length : dotPos) + kShift;
                // Clamp and reconstruct sigStr
                string sigStr2;
                if (newDotPos <= 0)
                {
                    // All digits are after the decimal: "0.00..." prefix
                    sigStr2 = "0." + new string('0', -newDotPos) + allDigitsR;
                    finalExp -= kShift; // recalculate
                    sigStr2 = rSigFull; // fallback: just use as-is for k<=0
                }
                else if (newDotPos >= allDigitsR.Length)
                {
                    // All digits before decimal
                    sigStr2 = allDigitsR + new string('0', newDotPos - allDigitsR.Length) + ".0";
                }
                else
                {
                    sigStr2 = allDigitsR.Substring(0, newDotPos) + "." + allDigitsR.Substring(newDotPos);
                    // Trim trailing zeros but keep at least one
                    int dp = sigStr2.IndexOf('.');
                    if (dp >= 0) {
                        string frac = sigStr2.Substring(dp + 1).TrimEnd('0');
                        if (frac.Length == 0) frac = "0";
                        sigStr2 = sigStr2.Substring(0, dp + 1) + frac;
                    }
                }

                // Build the result
                string sign2 = negative ? "-" : (atSign ? "+" : "");
                string expSign2 = finalExp >= 0 ? "+" : "-";
                string expDigits2 = Math.Abs(finalExp).ToString();
                if (e.HasValue && expDigits2.Length < e.Value)
                    expDigits2 = expDigits2.PadLeft(e.Value, '0');
                return sign2 + sigStr2 + expChar + expSign2 + expDigits2;
            }
            else
            {
                // Fixed notation (value in normal printer range): fallback to regular path
                if (isSingle) dActual = 6;
                else dActual = 15;
                trimTrailingZeros = true; // d was defaulted, not width-derived
            }
        }
        else
        {
            // d omitted, width constraint given
            int eVal = e ?? 1;
            int signLen = (negative || atSign) ? 1 : 0;
            int expPartLen = 1 + 1 + eVal; // expChar + sign + eDigits
            dActual = w.Value - signLen - (k >= 1 ? k : 1) - 1 - expPartLen;
            if (dActual < 0) dActual = 0;
        }

        // Total significant digits = dActual + (k >= 1 ? k : 0) when k > 0
        // When k=1 (default): 1 digit before dot, dActual after
        // When k=0: 0 before dot, dActual after, but there's a leading "0."
        // When k=-1: 0 before dot, but we shift right so "0.0d1d2..."
        // General: totalSigDigits = dActual + k (when k >= 1), or dActual + k (adjusted)

        // The printed significand has totalDigits = d + (k > 0 ? k : 1) significant digits
        // But when k <= 0, there are |k| leading zeros in the fraction
        int totalSigDigits;
        if (k >= 1)
            totalSigDigits = dActual + k;
        else
            totalSigDigits = dActual + k; // k<=0: fewer sig digits because |k| zeros pad the front

        // Compute how many significant digits to emit and how to place them.
        // CLHS 22.3.3.2: For k>=1, k digits before dot and (d-k+1) after. For k<=0,
        // 0 before dot, d after dot including |k| leading zeros.
        int digitsBeforeDot, digitsAfterDot, leadingZeros, totalDigitsToEmit;
        if (k >= 1)
        {
            digitsBeforeDot = k;
            digitsAfterDot = Math.Max(0, dActual - k + 1);
            leadingZeros = 0;
            totalDigitsToEmit = digitsBeforeDot + digitsAfterDot;
        }
        else
        {
            digitsBeforeDot = 0;
            digitsAfterDot = dActual;
            leadingZeros = -k;
            totalDigitsToEmit = Math.Max(0, digitsAfterDot - leadingZeros);
        }
        if (totalDigitsToEmit < 1) totalDigitsToEmit = 1; // need at least one digit

        // Request extra digits so we can round correctly.
        int gPrecision = Math.Max(17, totalDigitsToEmit + 2);
        string gStr = absVal.ToString($"G{gPrecision}", System.Globalization.CultureInfo.InvariantCulture);
        ExtractScientificDigits(gStr, out string allDigits, out int msdExp);

        // Round allDigits to totalDigitsToEmit significant digits (round-half-up, then normalize).
        bool carry = false;
        if (totalDigitsToEmit < allDigits.Length)
        {
            int firstDropped = allDigits[totalDigitsToEmit] - '0';
            string truncated = allDigits.Substring(0, totalDigitsToEmit);
            bool roundUp = firstDropped > 5
                || (firstDropped == 5 &&
                    (HasNonZeroAfter(allDigits, totalDigitsToEmit + 1)
                     || (totalDigitsToEmit > 0 && ((truncated[totalDigitsToEmit - 1] - '0') & 1) != 0)));
            if (roundUp)
            {
                var arr = truncated.ToCharArray();
                int i = arr.Length - 1;
                while (i >= 0)
                {
                    if (arr[i] == '9') { arr[i] = '0'; i--; } else { arr[i]++; break; }
                }
                if (i < 0) { carry = true; truncated = "1" + new string(arr); }
                else truncated = new string(arr);
            }
            allDigits = truncated;
        }
        else if (totalDigitsToEmit > allDigits.Length)
        {
            allDigits = allDigits + new string('0', totalDigitsToEmit - allDigits.Length);
        }

        // Handle carry (e.g. 9.99 → 10.00): exponent bumps up
        if (carry)
        {
            exponent++;
            adjustedExp++;
            // Drop the last digit since we added a leading "1"
            if (allDigits.Length > totalDigitsToEmit)
                allDigits = allDigits.Substring(0, totalDigitsToEmit);
        }

        // Assemble sigStr: digitsBeforeDot before dot, leadingZeros + digitsAfterDot-leadingZeros after
        string sigStr;
        if (digitsBeforeDot > 0)
        {
            // k digits before dot, (d-k+1) digits after
            if (allDigits.Length <= digitsBeforeDot)
            {
                sigStr = allDigits.PadRight(digitsBeforeDot, '0') + ".";
                if (digitsAfterDot > 0)
                    sigStr += new string('0', digitsAfterDot);
            }
            else
            {
                string before = allDigits.Substring(0, digitsBeforeDot);
                string after = allDigits.Substring(digitsBeforeDot);
                // Pad/truncate 'after' to exactly digitsAfterDot
                if (after.Length < digitsAfterDot) after = after.PadRight(digitsAfterDot, '0');
                else if (after.Length > digitsAfterDot) after = after.Substring(0, digitsAfterDot);
                sigStr = before + "." + after;
            }
        }
        else
        {
            // 0 before dot: "0." + leadingZeros + remaining sig digits
            string significantAfter = allDigits;
            int remainingAfter = digitsAfterDot - leadingZeros;
            if (remainingAfter <= 0)
            {
                sigStr = "0." + new string('0', digitsAfterDot);
            }
            else
            {
                if (significantAfter.Length < remainingAfter)
                    significantAfter = significantAfter.PadRight(remainingAfter, '0');
                else if (significantAfter.Length > remainingAfter)
                    significantAfter = significantAfter.Substring(0, remainingAfter);
                sigStr = "0." + new string('0', leadingZeros) + significantAfter;
            }
        }

        // Trim trailing zeros when dActual was a default (not width-derived)
        if (trimTrailingZeros)
        {
            int dotPos = sigStr.IndexOf('.');
            if (dotPos >= 0)
            {
                string fracPart = sigStr.Substring(dotPos + 1);
                string trimmed = fracPart.TrimEnd('0');
                if (trimmed.Length == 0) trimmed = "0"; // keep at least one zero per CL
                sigStr = sigStr.Substring(0, dotPos + 1) + trimmed;
            }
        }
        // Ensure there's a decimal point (defensive)
        if (!sigStr.Contains('.'))
            sigStr += ".";

        // Build exponent part
        int eVal2 = e ?? 1;
        string expSign = adjustedExp >= 0 ? "+" : "-";
        string expDigitsStr = Math.Abs(adjustedExp).ToString();
        if (e.HasValue && expDigitsStr.Length < eVal2)
            expDigitsStr = expDigitsStr.PadLeft(eVal2, '0');
        // If exponent doesn't fit in e digits and overflowChar specified, overflow
        if (e.HasValue && expDigitsStr.Length > eVal2)
        {
            if (overflowChar.HasValue && w.HasValue)
                return new string(overflowChar.Value, w.Value);
            // Otherwise just print the wider exponent
        }

        string sign = negative ? "-" : (atSign ? "+" : "");
        string full = sign + sigStr + expChar + expSign + expDigitsStr;

        if (w.HasValue)
        {
            if (full.Length > w.Value && overflowChar.HasValue)
                return new string(overflowChar.Value, w.Value);
            if (full.Length < w.Value)
                full = new string(padChar, w.Value - full.Length) + full;
        }

        return full;
    }

    private static string FormatString(string template, LispObject[] args, bool streamAtLineStart = true)
    {
        // CLHS 22.3.6.1: ~I, ~_, ~W, ~:T cannot coexist with ~<...~:;...~> in same format string
        ValidateJustifyPrettyPrintConflict(template);
        int argIdx = 0;
        return FormatString(template, args, ref argIdx, streamAtLineStart);
    }

    /// <summary>
    /// Format to a stream and return the number of args consumed.
    /// Used by FORMATTER to return the unconsumed arg tail.
    /// </summary>
    public static int FormatToStreamReturningArgCount(string template, LispObject[] args, LispObject dest)
    {
        // Resolve stream and get AtLineStart
        LispObject resolvedStream = dest;
        if (dest is T)
            resolvedStream = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
        LispObject resolved = resolvedStream;
        while (resolved is LispEchoStream es2) resolved = es2.OutputStream;
        while (resolved is LispTwoWayStream tw2) resolved = tw2.OutputStream;
        while (resolved is LispSynonymStream syn2) resolved = DynamicBindings.Get(syn2.Symbol);
        bool atLineStart = resolved is LispStream ls2 ? ls2.AtLineStart : true;

        int argIdx = 0;
        var result = FormatString(template, args, ref argIdx, atLineStart);

        if (dest is T)
        {
            if (resolvedStream is LispOutputStream os)
            {
                os.Writer.Write(result);
                if (_pprintActive) PprintTrackWrite(result);
            }
            else
                Console.Write(result);
        }
        else if (dest is LispBroadcastStream bs2)
        {
            foreach (var s in bs2.Streams)
                GetTextWriter(s).Write(result);
            if (_pprintActive) PprintTrackWrite(result);
        }
        else if (dest is LispStream)
        {
            try
            {
                GetTextWriter(dest).Write(result);
                if (_pprintActive) PprintTrackWrite(result);
            }
            catch { throw new LispErrorException(new LispTypeError("FORMAT: invalid destination", dest)); }
        }
        else
            throw new LispErrorException(new LispTypeError("FORMAT: invalid destination", dest));

        // Update AtLineStart
        if (resolved is LispStream ls3 && result.Length > 0)
            ls3.AtLineStart = result[result.Length - 1] == '\n';

        return argIdx;
    }

    private static string FormatString(string template, LispObject[] args, ref int argIdx, bool streamAtLineStart = true)
    {
        var sb = new System.Text.StringBuilder();
        int i = 0;
        while (i < template.Length)
        {
            if (template[i] == '~' && i + 1 < template.Length)
            {
                i++;
                // Parse prefix parameters: comma-separated list of number, 'char, v/V, or #
                var prefixParams = new System.Collections.Generic.List<object?>(); // int, char, "V", or null
                bool colonMod = false;
                bool atMod = false;

                // Parse prefix parameters
                bool firstParam = true;
                while (i < template.Length)
                {
                    if (!firstParam && template[i] == ',')
                    {
                        i++; // skip comma
                    }
                    else if (!firstParam)
                    {
                        break; // no comma, end of params
                    }
                    firstParam = false;

                    if (i < template.Length && (template[i] == 'v' || template[i] == 'V'))
                    {
                        prefixParams.Add("V"); // marker: use next format arg
                        i++;
                    }
                    else if (i < template.Length && template[i] == '#')
                    {
                        prefixParams.Add("#"); // marker: remaining arg count (resolved after V params)
                        i++;
                    }
                    else if (i < template.Length && template[i] == '\'' && i + 1 < template.Length)
                    {
                        prefixParams.Add(template[i + 1]); // character param
                        i += 2;
                    }
                    else if (i < template.Length && (char.IsDigit(template[i]) || ((template[i] == '-' || template[i] == '+') && i + 1 < template.Length && char.IsDigit(template[i + 1]))))
                    {
                        int numStart = i;
                        if (template[i] == '-' || template[i] == '+') i++;
                        while (i < template.Length && char.IsDigit(template[i]))
                            i++;
                        if (long.TryParse(template[numStart..i], out long lv))
                            prefixParams.Add((int)Math.Clamp(lv, int.MinValue, int.MaxValue));
                        else
                            prefixParams.Add(template[numStart] == '-' ? int.MinValue : int.MaxValue);
                    }
                    else if (i < template.Length && template[i] == ',')
                    {
                        // empty parameter (just comma) — add null placeholder
                        prefixParams.Add(null);
                    }
                    else
                    {
                        // No parameter found — if this was the first position, no params at all
                        if (prefixParams.Count == 0)
                            break;
                        // Otherwise this was after a comma with no value — empty param
                        prefixParams.Add(null);
                        break;
                    }
                }

                // Resolve prefix params: V consumes an arg, # is remaining-arg-count (after V resolution)
                var resolvedParams = new object?[prefixParams.Count];
                // First pass: resolve V params (which consume args)
                for (int pi = 0; pi < prefixParams.Count; pi++)
                {
                    var p = prefixParams[pi];
                    if (p is string s && s == "V")
                    {
                        if (argIdx < args.Length)
                        {
                            var a = args[argIdx++];
                            if (a is Fixnum fv) resolvedParams[pi] = (int)Math.Clamp(fv.Value, int.MinValue, int.MaxValue);
                            else if (a is Bignum) resolvedParams[pi] = a; // keep Bignum for ~^ comparison
                            else if (a is LispChar cv) resolvedParams[pi] = cv.Value;
                            else if (a is Nil) resolvedParams[pi] = null;
                            else resolvedParams[pi] = null;
                        }
                    }
                    else if (p is string sh && sh == "#")
                    {
                        resolvedParams[pi] = args.Length - argIdx; // remaining args after V resolution
                    }
                    else
                    {
                        resolvedParams[pi] = p;
                    }
                }

                int? GetIntParam(int idx, int? defaultVal = null)
                {
                    if (idx >= resolvedParams.Length) return defaultVal;
                    if (resolvedParams[idx] is int iv) return iv;
                    return defaultVal;
                }
                char GetCharParam(int idx, char defaultVal)
                {
                    if (idx >= resolvedParams.Length) return defaultVal;
                    if (resolvedParams[idx] is char cv) return cv;
                    return defaultVal;
                }

                int? prefixParam = GetIntParam(0);

                // Parse modifiers : and @
                while (i < template.Length && (template[i] == ':' || template[i] == '@'))
                {
                    if (template[i] == ':') colonMod = true;
                    if (template[i] == '@') atMod = true;
                    i++;
                }

                if (i >= template.Length) break;
                char directive = char.ToUpper(template[i]);
                i++;

                switch (directive)
                {
                    case 'A': // aesthetic (princ-like)
                        if (argIdx < args.Length)
                        {
                            string s = (colonMod && args[argIdx] is Nil)
                                ? "()"
                                : FormatObject(args[argIdx], false);
                            // ~mincol,colinc,minpad,padcharA
                            int aMincol = GetIntParam(0, 0)!.Value;
                            int aColinc = GetIntParam(1, 1)!.Value;
                            int aMinpad = GetIntParam(2, 0)!.Value;
                            char aPadchar = GetCharParam(3, ' ');
                            if (aColinc < 1) aColinc = 1;
                            // Add minpad padding first
                            int padNeeded = aMinpad;
                            // Then pad to mincol in increments of colinc
                            int totalLen = s.Length + padNeeded;
                            if (totalLen < aMincol)
                            {
                                int extra = aMincol - totalLen;
                                int colRound = (extra + aColinc - 1) / aColinc * aColinc;
                                padNeeded += colRound;
                            }
                            if (padNeeded > 0)
                            {
                                string pad = new string(aPadchar, padNeeded);
                                if (atMod)
                                    s = pad + s; // @ means pad on left
                                else
                                    s = s + pad; // default: pad on right
                            }
                            sb.Append(s);
                            argIdx++;
                        }
                        break;
                    case 'S': // standard (prin1-like)
                        if (argIdx < args.Length)
                        {
                            string s = (colonMod && args[argIdx] is Nil)
                                ? "()"
                                : FormatObject(args[argIdx], true);
                            // ~mincol,colinc,minpad,padcharS
                            int sMincol = GetIntParam(0, 0)!.Value;
                            int sColinc = GetIntParam(1, 1)!.Value;
                            int sMinpad = GetIntParam(2, 0)!.Value;
                            char sPadchar = GetCharParam(3, ' ');
                            if (sColinc < 1) sColinc = 1;
                            int sPadNeeded = sMinpad;
                            int sTotalLen = s.Length + sPadNeeded;
                            if (sTotalLen < sMincol)
                            {
                                int extra = sMincol - sTotalLen;
                                int colRound = (extra + sColinc - 1) / sColinc * sColinc;
                                sPadNeeded += colRound;
                            }
                            if (sPadNeeded > 0)
                            {
                                string pad = new string(sPadchar, sPadNeeded);
                                if (atMod)
                                    s = pad + s;
                                else
                                    s = s + pad;
                            }
                            sb.Append(s);
                            argIdx++;
                        }
                        break;
                    case 'D': // decimal
                    case 'B': // binary
                    case 'O': // octal
                    case 'X': // hex
                        if (argIdx < args.Length)
                        {
                            var arg = args[argIdx];
                            // Per CLHS 22.3.2.2: if arg is not an integer, print as ~A
                            // with *print-base* bound to the appropriate radix
                            if (arg is not Fixnum && arg is not Bignum)
                            {
                                int nonIntRadix = directive == 'D' ? 10 : directive == 'B' ? 2 : directive == 'O' ? 8 : 16;
                                var baseSym = Startup.Sym("*PRINT-BASE*");
                                DynamicBindings.Push(baseSym, Fixnum.Make(nonIntRadix));
                                try
                                {
                                    sb.Append(FormatObject(arg, false));
                                }
                                finally
                                {
                                    DynamicBindings.Pop(baseSym);
                                }
                                argIdx++;
                                break;
                            }

                            // ~mincol,padchar,commachar,comma-intervalD
                            int radixMincol = GetIntParam(0, 0)!.Value;
                            char radixPadchar = GetCharParam(1, ' ');
                            char radixCommachar = GetCharParam(2, ',');
                            int radixCommaInterval = GetIntParam(3, 3)!.Value;
                            if (radixCommaInterval < 1) radixCommaInterval = 3;

                            long numVal = 0;
                            bool isNum = false;
                            if (arg is Fixnum fn) { numVal = fn.Value; isNum = true; }
                            else if (arg is Bignum bi) { /* ToString handles it */ }

                            string numStr;
                            if (directive == 'D')
                            {
                                numStr = arg.ToString();
                                // For BigInteger, ToString already gives decimal
                                isNum = true; // all number types give valid ToString
                            }
                            else
                            {
                                int radix = directive == 'B' ? 2 : directive == 'O' ? 8 : 16;
                                if (arg is Fixnum fnx)
                                {
                                    long val = fnx.Value;
                                    if (val < 0)
                                        numStr = "-" + Convert.ToString(-val, radix);
                                    else
                                        numStr = Convert.ToString(val, radix);
                                    if (directive == 'X') numStr = numStr.ToUpperInvariant();
                                    isNum = true;
                                }
                                else if (arg is Bignum bix)
                                {
                                    // BigInteger radix conversion
                                    var bv = bix.Value;
                                    bool neg = bv < 0;
                                    if (neg) bv = -bv;
                                    if (bv == 0)
                                    {
                                        numStr = "0";
                                    }
                                    else
                                    {
                                        var digits = new System.Collections.Generic.List<char>();
                                        var bigRadix = new System.Numerics.BigInteger(radix);
                                        while (bv > 0)
                                        {
                                            var rem = (int)(bv % bigRadix);
                                            digits.Add("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[rem]);
                                            bv /= bigRadix;
                                        }
                                        digits.Reverse();
                                        numStr = new string(digits.ToArray());
                                    }
                                    if (neg) numStr = "-" + numStr;
                                    isNum = true;
                                }
                                else
                                {
                                    // Should not reach here — non-integers handled above
                                    numStr = FormatObject(arg, false);
                                    isNum = false;
                                }
                            }

                            // : modifier — insert commachar between groups of comma-interval digits
                            if (colonMod && isNum)
                            {
                                bool neg = numStr.StartsWith("-");
                                string digits = neg ? numStr.Substring(1) : numStr;
                                if (digits.Length > radixCommaInterval)
                                {
                                    var commaResult = new System.Text.StringBuilder();
                                    int pos = 0;
                                    int firstGroup = digits.Length % radixCommaInterval;
                                    if (firstGroup == 0) firstGroup = radixCommaInterval;
                                    commaResult.Append(digits, 0, firstGroup);
                                    pos = firstGroup;
                                    while (pos < digits.Length)
                                    {
                                        commaResult.Append(radixCommachar);
                                        commaResult.Append(digits, pos, radixCommaInterval);
                                        pos += radixCommaInterval;
                                    }
                                    numStr = (neg ? "-" : "") + commaResult.ToString();
                                }
                            }

                            // @ modifier — prepend + for non-negative
                            if (atMod && isNum && !numStr.StartsWith("-"))
                            {
                                numStr = "+" + numStr;
                            }

                            // mincol — pad with padchar on the left
                            if (radixMincol > numStr.Length)
                            {
                                numStr = new string(radixPadchar, radixMincol - numStr.Length) + numStr;
                            }

                            sb.Append(numStr);
                            argIdx++;
                        }
                        break;
                    case 'R': // radix
                        if (argIdx < args.Length)
                        {
                            var rArg = args[argIdx];
                            // Per CLHS: if arg is not an integer, print as ~A
                            if (rArg is not Fixnum && rArg is not Bignum)
                            {
                                sb.Append(FormatObject(rArg, false));
                                argIdx++;
                                break;
                            }
                            if (prefixParam.HasValue)
                            {
                                // ~radix,mincol,padchar,commachar,comma-intervalR
                                int rRadix = prefixParam.Value;
                                int rMincol = GetIntParam(1, 0)!.Value;
                                char rPadchar = GetCharParam(2, ' ');
                                char rCommachar = GetCharParam(3, ',');
                                int rCommaInterval = GetIntParam(4, 3)!.Value;
                                if (rCommaInterval < 1) rCommaInterval = 3;

                                string rNumStr;
                                bool rIsNum = false;
                                if (rArg is Fixnum rfn)
                                {
                                    rNumStr = ToRadixString(rfn.Value, rRadix);
                                    rIsNum = true;
                                }
                                else if (rArg is Bignum rbi)
                                {
                                    rNumStr = BigIntToRadixString(rbi.Value, rRadix);
                                    rIsNum = true;
                                }
                                else
                                {
                                    // Should not reach here — non-integers handled above
                                    rNumStr = FormatObject(rArg, false);
                                    rIsNum = false;
                                }

                                // : modifier — insert commachar between groups
                                if (colonMod && rIsNum)
                                {
                                    bool neg = rNumStr.StartsWith("-");
                                    string digs = neg ? rNumStr.Substring(1) : rNumStr;
                                    if (digs.Length > rCommaInterval)
                                    {
                                        var crb = new System.Text.StringBuilder();
                                        int firstGrp = digs.Length % rCommaInterval;
                                        if (firstGrp == 0) firstGrp = rCommaInterval;
                                        crb.Append(digs, 0, firstGrp);
                                        int cpos = firstGrp;
                                        while (cpos < digs.Length)
                                        {
                                            crb.Append(rCommachar);
                                            crb.Append(digs, cpos, rCommaInterval);
                                            cpos += rCommaInterval;
                                        }
                                        rNumStr = (neg ? "-" : "") + crb.ToString();
                                    }
                                }

                                // @ modifier — prepend + for non-negative
                                if (atMod && rIsNum && !rNumStr.StartsWith("-"))
                                    rNumStr = "+" + rNumStr;

                                // mincol — pad with padchar on the left
                                if (rMincol > rNumStr.Length)
                                    rNumStr = new string(rPadchar, rMincol - rNumStr.Length) + rNumStr;

                                sb.Append(rNumStr);
                            }
                            else
                            {
                                // No radix prefix: English words / ordinal / Roman
                                long rVal = 0;
                                bool rHasVal = false;
                                if (rArg is Fixnum rfn2) { rVal = rfn2.Value; rHasVal = true; }
                                else if (rArg is Bignum rbi2) { rVal = (long)rbi2.Value; rHasVal = true; }

                                if (rHasVal)
                                {
                                    if (atMod && colonMod)
                                        sb.Append(ToOldRoman(rVal));    // ~:@R old Roman
                                    else if (atMod)
                                        sb.Append(ToRoman(rVal));       // ~@R Roman
                                    else if (colonMod)
                                        sb.Append(ToOrdinal(rVal));     // ~:R ordinal
                                    else
                                        sb.Append(ToCardinal(rVal));    // ~R cardinal
                                }
                                else
                                {
                                    // Should not reach here — non-integers handled above
                                    sb.Append(FormatObject(rArg, false));
                                }
                            }
                            argIdx++;
                        }
                        break;
                    case 'F': // fixed-format floating point ~w,d,k,overflowchar,padcharF
                        if (argIdx < args.Length)
                        {
                            var fArg = args[argIdx];
                            bool fIsSingle = fArg is SingleFloat;
                            bool fIsDouble = fArg is DoubleFloat;
                            double dv = fArg switch
                            {
                                SingleFloat sf2 => sf2.Value,
                                DoubleFloat df2 => df2.Value,
                                Fixnum fi2 => fi2.Value,
                                Bignum bg => (double)bg.Value,
                                Ratio r => (double)r.Numerator / (double)r.Denominator,
                                _ => 0.0
                            };
                            int? fW = GetIntParam(0);
                            int? fD = GetIntParam(1);
                            int fK = GetIntParam(2) ?? 0;
                            char? fOverflowChar = null;
                            if (3 < resolvedParams.Length && resolvedParams[3] is char oc) fOverflowChar = oc;
                            char fPadChar = GetCharParam(4, ' ');

                            sb.Append(FormatFixedFloat(dv, fW, fD, fK, fOverflowChar, fPadChar, atMod, fIsSingle, fIsDouble));
                            argIdx++;
                        }
                        break;
                    case 'E': // exponential floating point ~w,d,e,k,overflowchar,padchar,exponentcharE
                        if (argIdx < args.Length)
                        {
                            var eArg = args[argIdx];
                            bool eIsSingle = eArg is SingleFloat;
                            bool eIsDouble = eArg is DoubleFloat;
                            double edv = eArg switch
                            {
                                SingleFloat sf3 => sf3.Value,
                                DoubleFloat df3 => df3.Value,
                                Fixnum fi3 => fi3.Value,
                                Bignum bg3 => (double)bg3.Value,
                                Ratio r3 => (double)r3.Numerator / (double)r3.Denominator,
                                _ => 0.0
                            };
                            int? eW = GetIntParam(0);
                            int? eD = GetIntParam(1);
                            int? eE = GetIntParam(2);
                            int eK = GetIntParam(3) ?? 1;
                            char? eOverflowChar = null;
                            if (4 < resolvedParams.Length && resolvedParams[4] is char eoc) eOverflowChar = eoc;
                            char ePadChar = GetCharParam(5, ' ');
                            char? eExpChar = null;
                            if (6 < resolvedParams.Length && resolvedParams[6] is char exc) eExpChar = exc;
                            sb.Append(FormatExponentialFloat(edv, eW, eD, eE, eK, eOverflowChar, ePadChar, eExpChar, atMod, eIsSingle, eIsDouble));
                            argIdx++;
                        }
                        break;
                    case 'C': // character
                        if (argIdx < args.Length)
                        {
                            if (args[argIdx] is LispChar lc)
                            {
                                bool isGraphic = Runtime.IsGraphicChar(lc.Value);
                                if (atMod && colonMod)
                                {
                                    // ~@:C - like ~:C but may include info about how to type
                                    string? cname = GetCharacterName(lc.Value);
                                    if ((!isGraphic || lc.Value == ' ') && cname != null)
                                        sb.Append(cname);
                                    else
                                        sb.Append(lc.Value);
                                }
                                else if (atMod)
                                {
                                    // ~@C - output in #\name syntax
                                    string? cname = GetCharacterName(lc.Value);
                                    if (cname != null)
                                        sb.Append("#\\" + cname);
                                    else
                                        sb.Append("#\\" + lc.Value);
                                }
                                else if (colonMod)
                                {
                                    // ~:C - output character name for non-graphic chars and Space
                                    // CLHS 22.3.1.2: "spells out names of characters that are not textual"
                                    string? cname = GetCharacterName(lc.Value);
                                    if ((!isGraphic || lc.Value == ' ') && cname != null)
                                        sb.Append(cname);
                                    else
                                        sb.Append(lc.Value);
                                }
                                else
                                {
                                    sb.Append(lc.Value);
                                }
                            }
                            argIdx++;
                        }
                        break;
                    case '%': // newline
                    {
                        int count = prefixParam ?? 1;
                        for (int j = 0; j < count; j++)
                            sb.Append('\n');
                        break;
                    }
                    case '&': // fresh-line
                    {
                        int count = prefixParam ?? 1;
                        if (count > 0)
                        {
                            // Check if at beginning of line: use sb content if available,
                            // otherwise fall back to the stream's AtLineStart state
                            bool atLineStart = sb.Length > 0
                                ? sb[sb.Length - 1] == '\n'
                                : streamAtLineStart;
                            if (!atLineStart)
                                sb.Append('\n');
                            // Then output (count-1) additional newlines
                            for (int j = 1; j < count; j++)
                                sb.Append('\n');
                        }
                        break;
                    }
                    case '~': // literal tilde
                    {
                        int count = prefixParam ?? 1;
                        for (int j = 0; j < count; j++)
                            sb.Append('~');
                        break;
                    }
                    case '[': // conditional
                    {
                        // Find matching ~] and ~; separators
                        var (clauses, hasDefault) = ParseConditionalClauses(template, ref i);
                        if (atMod)
                        {
                            // ~@[...~] — if arg is non-nil, process body without consuming arg
                            if (argIdx < args.Length && args[argIdx] is not Nil)
                            {
                                // Arg is non-nil: don't consume it, process body with remaining args
                                int condSubIdx = argIdx;
                                sb.Append(FormatString(clauses[0], args, ref condSubIdx));
                                argIdx = condSubIdx;
                            }
                            else
                            {
                                // Arg is nil: consume it, skip the body
                                argIdx++;
                            }
                        }
                        else if (colonMod)
                        {
                            // ~:[false~;true~] — boolean
                            if (argIdx < args.Length)
                            {
                                int ci = args[argIdx] is Nil ? 0 : 1;
                                if (ci < clauses.Count)
                                    sb.Append(FormatString(clauses[ci], args[(argIdx + 1)..]));
                                argIdx++;
                            }
                        }
                        else
                        {
                            // ~[c0~;c1~;...~] or ~n[c0~;c1~;...~] — numeric selection
                            int ci;
                            bool consumed = false;
                            if (prefixParam.HasValue)
                            {
                                // ~n[...~] — prefix parameter provides the index
                                ci = prefixParam.Value;
                            }
                            else if (argIdx < args.Length && args[argIdx] is Fixnum fi2)
                            {
                                ci = (int)fi2.Value;
                                consumed = true;
                            }
                            else if (argIdx < args.Length && args[argIdx] is Bignum)
                            {
                                // Bignum or other integer type: out of range, but still consume the arg
                                ci = -1;
                                consumed = true;
                            }
                            else
                            {
                                ci = -1; // no valid index
                            }

                            if (ci >= 0 && ci < clauses.Count)
                                sb.Append(FormatString(clauses[ci], consumed ? args[(argIdx + 1)..] : args[argIdx..]));
                            else if (hasDefault && clauses.Count > 0)
                                sb.Append(FormatString(clauses[clauses.Count - 1], consumed ? args[(argIdx + 1)..] : args[argIdx..]));

                            if (consumed)
                                argIdx++;
                        }
                        break;
                    }
                    case '{': // iteration
                    {
                        var body = ParseIterationBody(template, ref i, out bool colonClose);
                        int maxIter = prefixParam ?? -1; // ~n{ limits iterations
                        int iterCount = 0;
                        bool forceOnce = colonClose; // ~:} forces at least one iteration

                        // Handle empty body: next arg is format string
                        if (body.Length == 0 && argIdx < args.Length && !atMod)
                        {
                            // ~{~} or ~:{~}: first arg = format string/function, second arg = data list (or list of sublists)
                            var fmtArg = args[argIdx++];
                            if (argIdx < args.Length)
                            {
                                var listArg = args[argIdx++];
                                if (colonMod)
                                {
                                    // ~:{~}: listArg is list of sublists
                                    if (!(listArg is Cons || listArg is Nil))
                                        throw new LispErrorException(new LispTypeError("FORMAT ~:{: argument is not a list", listArg, Startup.Sym("LIST")));
                                    var sublists = new List<LispObject>();
                                    while (listArg is Cons lc2) { sublists.Add(lc2.Car); listArg = lc2.Cdr; }
                                    try
                                    {
                                        FormatIterateOverSublists(fmtArg, sublists, maxIter, forceOnce, sb, ref iterCount);
                                    }
                                    catch (FormatColonUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                                }
                                else
                                {
                                    // ~{~}: listArg is flat list of elements
                                    var elems = new List<LispObject>();
                                    while (listArg is Cons lc) { elems.Add(lc.Car); listArg = lc.Cdr; }
                                    var elemArr = elems.ToArray();
                                    int elemIdx = 0;

                                    FormatIterateFlat(fmtArg, elemArr, ref elemIdx, maxIter, forceOnce, sb, ref iterCount);
                                }
                            }
                        }
                        else if (body.Length == 0 && argIdx < args.Length && atMod && !colonMod)
                        {
                            // ~@{~}: first arg = format string/function, remaining args used directly
                            var fmtArg = args[argIdx++];
                            var remainArr = args[argIdx..];
                            int elemIdx = 0;
                            FormatIterateFlat(fmtArg, remainArr, ref elemIdx, maxIter, forceOnce, sb, ref iterCount);
                            argIdx += elemIdx;
                        }
                        else if (body.Length == 0 && argIdx < args.Length && atMod && colonMod)
                        {
                            // ~:@{~}: first arg = format string/function, remaining args are sublists
                            var fmtArg = args[argIdx++];
                            var sublists = new List<LispObject>();
                            while (argIdx < args.Length) sublists.Add(args[argIdx++]);
                            try
                            {
                                FormatIterateOverSublists(fmtArg, sublists, maxIter, forceOnce, sb, ref iterCount);
                            }
                            catch (FormatColonUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                        }
                        else if (colonMod && atMod)
                        {
                            // ~:@{body~}: each remaining arg is a sublist
                            try
                            {
                                while ((argIdx < args.Length || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
                                {
                                    LispObject[] subArr;
                                    if (argIdx < args.Length)
                                    {
                                        var subList = args[argIdx++];
                                        if (!(subList is Cons || subList is Nil))
                                            throw new LispErrorException(new LispTypeError("FORMAT ~:@{: argument is not a list", subList, Startup.Sym("LIST")));
                                        var subElems = new List<LispObject>();
                                        while (subList is Cons sc) { subElems.Add(sc.Car); subList = sc.Cdr; }
                                        if (!(subList is Nil))
                                            throw new LispErrorException(new LispTypeError("FORMAT ~:@{: argument is not a proper list", subList, Startup.Sym("LIST")));
                                        subArr = subElems.ToArray();
                                    }
                                    else subArr = Array.Empty<LispObject>();
                                    int subIdx = 0;
                                    var savedLastSublist = _isLastSublist;
                                    _isLastSublist = argIdx >= args.Length;
                                    try
                                    {
                                        sb.Append(FormatString(body, subArr, ref subIdx));
                                    }
                                    catch (FormatUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                                    finally { _isLastSublist = savedLastSublist; }
                                    iterCount++;
                                }
                            }
                            catch (FormatColonUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                        }
                        else if (atMod)
                        {
                            // ~@{body~}: use remaining args directly, consuming multiple per iteration
                            while ((argIdx < args.Length || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
                            {
                                var iterArgs = args[argIdx..];
                                int subIdx = 0;
                                try
                                {
                                    sb.Append(FormatString(body, iterArgs, ref subIdx));
                                }
                                catch (FormatUpAndOutException ex)
                                {
                                    sb.Append(ex.PartialOutput);
                                    argIdx += subIdx;
                                    break;
                                }
                                argIdx += subIdx;
                                iterCount++;
                                // If no args consumed, avoid infinite loop
                                if (subIdx == 0) break;
                            }
                        }
                        else if (colonMod)
                        {
                            // ~:{body~}: take one list arg; each element is a sublist
                            if (argIdx < args.Length)
                            {
                                var listArg = args[argIdx++];
                                if (!(listArg is Cons || listArg is Nil))
                                    throw new LispErrorException(new LispTypeError("FORMAT ~:{: argument is not a list", listArg, Startup.Sym("LIST")));
                                var sublists = new List<LispObject>();
                                while (listArg is Cons lc2) { sublists.Add(lc2.Car); listArg = lc2.Cdr; }
                                if (!(listArg is Nil))
                                    throw new LispErrorException(new LispTypeError("FORMAT ~:{: argument is not a proper list", listArg, Startup.Sym("LIST")));
                                try
                                {
                                    int slIdx = 0;
                                    while ((slIdx < sublists.Count || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
                                    {
                                        LispObject[] subArr;
                                        if (slIdx < sublists.Count)
                                        {
                                            var subList = sublists[slIdx++];
                                            if (!(subList is Cons || subList is Nil))
                                                throw new LispErrorException(new LispTypeError("FORMAT ~:{: sublist is not a list", subList, Startup.Sym("LIST")));
                                            var subElems = new List<LispObject>();
                                            while (subList is Cons sc2) { subElems.Add(sc2.Car); subList = sc2.Cdr; }
                                            subArr = subElems.ToArray();
                                        }
                                        else { subArr = Array.Empty<LispObject>(); slIdx++; }
                                        int subIdx = 0;
                                        var savedLastSublist = _isLastSublist;
                                        _isLastSublist = slIdx >= sublists.Count;
                                        try
                                        {
                                            sb.Append(FormatString(body, subArr, ref subIdx));
                                        }
                                        catch (FormatUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                                        finally { _isLastSublist = savedLastSublist; }
                                        iterCount++;
                                    }
                                }
                                catch (FormatColonUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                            }
                        }
                        else
                        {
                            // ~{body~}: take one list arg, iterate over its elements
                            if (argIdx < args.Length)
                            {
                                var listArg = args[argIdx++];
                                if (!(listArg is Cons || listArg is Nil))
                                    throw new LispErrorException(new LispTypeError("FORMAT ~{: argument is not a list", listArg, Startup.Sym("LIST")));
                                var elems = new List<LispObject>();
                                while (listArg is Cons lc) { elems.Add(lc.Car); listArg = lc.Cdr; }
                                if (!(listArg is Nil))
                                    throw new LispErrorException(new LispTypeError("FORMAT ~{: argument is not a proper list", listArg, Startup.Sym("LIST")));
                                var elemArr = elems.ToArray();
                                int elemIdx = 0;
                                while ((elemIdx < elemArr.Length || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
                                {
                                    var iterArgs = elemArr[elemIdx..];
                                    int subIdx = 0;
                                    try
                                    {
                                        sb.Append(FormatString(body, iterArgs, ref subIdx));
                                    }
                                    catch (FormatUpAndOutException ex)
                                    {
                                        sb.Append(ex.PartialOutput);
                                        elemIdx += subIdx;
                                        break;
                                    }
                                    elemIdx += subIdx;
                                    iterCount++;
                                    // If no args consumed, avoid infinite loop
                                    if (subIdx == 0) break;
                                }
                            }
                        }
                        break;
                    }
                    case '*': // skip/goto args
                        if (atMod)
                            argIdx = prefixParam ?? 0; // goto absolute position
                        else if (colonMod)
                            argIdx = Math.Max(0, argIdx - (prefixParam ?? 1)); // back up
                        else
                            argIdx += prefixParam ?? 1; // skip forward
                        break;
                    case '?': // recursive processing
                        if (argIdx < args.Length && args[argIdx] is LispString fmtStr)
                        {
                            argIdx++;
                            if (atMod)
                            {
                                // ~@? - use remaining args, advance argIdx by consumed count
                                // ~^ inside ~@? terminates the recursive processing only
                                try
                                {
                                    sb.Append(FormatString(fmtStr.Value, args, ref argIdx));
                                }
                                catch (FormatUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                            }
                            else
                            {
                                // ~? - next arg is a list of sub-args
                                // ~^ inside ~? terminates the recursive processing only
                                var subArgs = new List<LispObject>();
                                if (argIdx < args.Length)
                                {
                                    var cur = args[argIdx];
                                    while (cur is Cons c) { subArgs.Add(c.Car); cur = c.Cdr; }
                                    argIdx++;
                                }
                                try
                                {
                                    sb.Append(FormatString(fmtStr.Value, subArgs.ToArray()));
                                }
                                catch (FormatUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                            }
                        }
                        else if (argIdx < args.Length && args[argIdx] is LispFunction fmtFn)
                        {
                            // Format control is a function (e.g. from formatter macro)
                            argIdx++;
                            if (atMod)
                            {
                                // ~@? with function: call function with string-stream + remaining args
                                var remaining = new LispObject[args.Length - argIdx];
                                Array.Copy(args, argIdx, remaining, 0, remaining.Length);
                                var strStream = new LispStringOutputStream(new System.IO.StringWriter());
                                var fnArgs = new LispObject[1 + remaining.Length];
                                fnArgs[0] = strStream;
                                Array.Copy(remaining, 0, fnArgs, 1, remaining.Length);
                                var result = fmtFn.Invoke(fnArgs);
                                sb.Append(strStream.GetString());
                                // Determine how many args were consumed: result is the unconsumed tail
                                int consumed = remaining.Length;
                                if (result is Cons)
                                {
                                    int tailLen = 0;
                                    var cur = result;
                                    while (cur is Cons cc) { tailLen++; cur = cc.Cdr; }
                                    consumed = remaining.Length - tailLen;
                                }
                                else if (result is Nil)
                                {
                                    consumed = remaining.Length;
                                }
                                argIdx += consumed;
                            }
                            else
                            {
                                // ~? with function: next arg is a list of sub-args
                                var subArgs = new List<LispObject>();
                                if (argIdx < args.Length)
                                {
                                    var cur = args[argIdx];
                                    while (cur is Cons c) { subArgs.Add(c.Car); cur = c.Cdr; }
                                    argIdx++;
                                }
                                var strStream2 = new LispStringOutputStream(new System.IO.StringWriter());
                                var fnArgs2 = new LispObject[1 + subArgs.Count];
                                fnArgs2[0] = strStream2;
                                for (int fi = 0; fi < subArgs.Count; fi++)
                                    fnArgs2[1 + fi] = subArgs[fi];
                                fmtFn.Invoke(fnArgs2);
                                sb.Append(strStream2.GetString());
                            }
                        }
                        break;
                    case 'T': // tabulate
                    {
                        int colnum = prefixParam ?? 1;
                        int colinc = GetIntParam(1) ?? 1;
                        if (colonMod)
                        {
                            // ~:T = pprint-tab :section, ~:@T = pprint-tab :section-relative
                            // Only effective inside logical blocks with pretty printing
                            var ppValT = DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out var ppvT) ? ppvT : Startup.Sym("*PRINT-PRETTY*").Value;
                            if (ppValT is not Nil && _pprintActive && _pprintStream != null)
                            {
                                if (sb.Length > 0)
                                {
                                    var flushed = sb.ToString();
                                    _pprintStream.Write(flushed);
                                    PprintTrackWrite(flushed);
                                    sb.Clear();
                                }
                                string tabKind = atMod ? "SECTION-RELATIVE" : "SECTION";
                                PprintTab(_pprintStream, tabKind, colnum, colinc);
                            }
                            break;
                        }
                        // Find current column (approximate: count from last newline)
                        int lastNl = sb.ToString().LastIndexOf('\n');
                        int col = lastNl < 0 ? sb.Length : sb.Length - lastNl - 1;
                        if (atMod)
                        {
                            // ~colnum,colinc@T: relative tabulation
                            // Output colnum spaces, then enough to reach next multiple of colinc
                            int spaces = colnum;
                            if (colinc > 0)
                            {
                                int newcol = col + spaces;
                                int remainder = newcol % colinc;
                                if (remainder != 0) spaces += colinc - remainder;
                            }
                            if (spaces > 0) sb.Append(' ', spaces);
                        }
                        else
                        {
                            // ~colnum,colincT: absolute tabulation
                            if (col < colnum)
                            {
                                sb.Append(' ', colnum - col);
                            }
                            else if (colinc > 0)
                            {
                                // Move to next position >= col that is colnum + k*colinc
                                int target = colnum;
                                while (target <= col) target += colinc;
                                sb.Append(' ', target - col);
                            }
                            // If colinc == 0 and col >= colnum, output nothing
                        }
                        break;
                    }
                    case 'P': // plural
                    {
                        if (colonMod)
                            argIdx = Math.Max(0, argIdx - 1); // back up one arg
                        if (argIdx < args.Length)
                        {
                            bool isOne = args[argIdx] is Fixnum fp && fp.Value == 1;
                            if (atMod)
                                sb.Append(isOne ? "y" : "ies");
                            else
                                sb.Append(isOne ? "" : "s");
                            argIdx++;
                        }
                        break;
                    }
                    case '(': // case conversion ~(...~)
                    {
                        var body2 = ParseCaseBody(template, ref i);
                        bool upAndOut = false;
                        string inner;
                        int caseSubIdx = 0;
                        try
                        {
                            inner = FormatString(body2, args[argIdx..], ref caseSubIdx);
                        }
                        catch (FormatUpAndOutException ex)
                        {
                            inner = ex.PartialOutput;
                            upAndOut = true;
                        }
                        argIdx += caseSubIdx;
                        string converted = ApplyCaseConversion(inner, colonMod, atMod);
                        sb.Append(converted);
                        if (upAndOut)
                            throw FormatUpAndOutException.GetInstance(sb.ToString());
                        break;
                    }
                    case '<': // justification ~<...~;...~>
                    {
                        var sections = ParseJustificationSections(template, ref i,
                            out bool firstSepColon, out string? firstSepParams, out bool closedColon, out bool closedAt,
                            out bool firstSepAt);

                        // ~<...~:> is logical block (pretty printing)
                        // ~; separates prefix, body, suffix: ~<prefix~;body~;suffix~:>
                        // With only one ~;: ~<prefix~;body~:> (suffix defaults to "")
                        // With no ~;: ~<body~:> (prefix and suffix default to "")
                        if (closedColon)
                        {
                            if (sections.Count > 0)
                            {
                                string prefix, body, suffix;
                                if (sections.Count >= 3)
                                {
                                    prefix = sections[0];
                                    // Rejoin middle sections in case there are extra ~; in body
                                    var mid = new System.Text.StringBuilder();
                                    for (int si = 1; si < sections.Count - 1; si++) mid.Append(sections[si]);
                                    body = mid.ToString();
                                    suffix = sections[^1];
                                }
                                else if (sections.Count == 2)
                                {
                                    prefix = sections[0];
                                    body = sections[1];
                                    suffix = "";
                                }
                                else
                                {
                                    prefix = "";
                                    body = sections[0];
                                    suffix = "";
                                }
                                // CLHS: prefix and suffix must not contain format directives
                                if (ContainsFormatDirective(prefix) || ContainsFormatDirective(suffix))
                                    throw new LispErrorException(new LispError("FORMAT ~<...~:>: prefix and suffix must not contain format directives"));
                                // CLHS 22.3.6.2: ~:< means prefix defaults to "(" and suffix to ")"
                                // Only apply defaults for parts not explicitly specified via ~;
                                if (colonMod)
                                {
                                    if (sections.Count == 1)
                                    {
                                        // No ~; at all: both prefix and suffix default
                                        prefix = "(";
                                        suffix = ")";
                                    }
                                    else if (sections.Count == 2)
                                    {
                                        // One ~;: prefix is explicit, suffix defaults
                                        if (suffix == "") suffix = ")";
                                    }
                                    // sections.Count >= 3: both explicitly given, no defaults
                                }
                                bool circleSetup = false;
                                try
                                {
                                    LispObject[] bodyArgs;
                                    LispObject? rawArg = null; // non-list arg for direct output
                                    LispObject? dottedTail = null;
                                    if (atMod)
                                    {
                                        // ~@<...~:>: use remaining format args directly
                                        bodyArgs = args[argIdx..];
                                        argIdx = args.Length;
                                    }
                                    else
                                    {
                                        // ~<...~:>: consume one list arg, use its elements as body args
                                        var listArg = argIdx < args.Length ? args[argIdx++] : Nil.Instance;
                                        if (listArg is not Cons && listArg is not Nil)
                                        {
                                            // CLHS pprint-logical-block: non-list arg is output via write
                                            rawArg = listArg;
                                            bodyArgs = Array.Empty<LispObject>();
                                        }
                                        else
                                        {
                                            // Set up *print-circle* detection for the list
                                            if (Runtime.PprintCircleScan(listArg))
                                                circleSetup = true;
                                            // Check if the top-level list has a circle label
                                            string? circlePrefix = null;
                                            if (circleSetup && listArg is Cons)
                                            {
                                                var labelStr = Runtime.PprintCircleCheckList(listArg);
                                                if (labelStr != null && labelStr.EndsWith("#"))
                                                {
                                                    // Entire list is a back-reference
                                                    rawArg = new LispString(labelStr);
                                                    bodyArgs = Array.Empty<LispObject>();
                                                }
                                                else
                                                {
                                                    circlePrefix = labelStr; // e.g. "#1="
                                                    bodyArgs = Array.Empty<LispObject>(); // will be set below
                                                }
                                            }
                                            else
                                            {
                                                bodyArgs = Array.Empty<LispObject>(); // will be set below
                                            }
                                            if (rawArg == null)
                                            {
                                                var elems = new List<LispObject>();
                                                var cur = listArg;
                                                var visited = new HashSet<object>(ReferenceEqualityComparer.Instance);
                                                while (cur is Cons c)
                                                {
                                                    if (visited.Contains(c))
                                                    {
                                                        dottedTail = cur;
                                                        break;
                                                    }
                                                    visited.Add(c);
                                                    elems.Add(c.Car);
                                                    cur = c.Cdr;
                                                    if (circleSetup && cur is Cons && Runtime.IsCircleShared(cur))
                                                    {
                                                        dottedTail = cur;
                                                        break;
                                                    }
                                                }
                                                if (dottedTail == null && cur is not Nil)
                                                    dottedTail = cur;
                                                bodyArgs = elems.ToArray();
                                                if (circlePrefix != null)
                                                    sb.Append(circlePrefix);
                                            }
                                        }
                                    }
                                    // CLHS pprint-logical-block: non-list arg bypasses body, output via write
                                    if (rawArg != null)
                                    {
                                        sb.Append(FormatObject(rawArg, false));
                                    }
                                    else
                                    {
                                    // Format prefix literally, body with args, suffix literally
                                    var prefixStr = FormatString(prefix, Array.Empty<LispObject>());
                                    var suffixStr = FormatString(suffix, Array.Empty<LispObject>());
                                    // Set up pprint state for logical block
                                    var prettyVal = DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out var ppv) ? ppv : Startup.Sym("*PRINT-PRETTY*").Value;
                                    bool isPretty = prettyVal is not Nil;
                                    if (isPretty)
                                    {
                                        // Determine per-line-prefix from ~:; or ~@; first separator
                                        string? perLinePrefix = null;
                                        if ((firstSepColon || firstSepAt) && sections.Count >= 2)
                                        {
                                            // ~:; or ~@; means first section is per-line-prefix
                                            perLinePrefix = prefixStr;
                                        }
                                        // Use a real StringWriter so XP buffering works
                                        var blockWriter = new System.IO.StringWriter();
                                        blockWriter.Write(prefixStr);
                                        // Compute column offset from outer sb context
                                        int outerCol = 0;
                                        for (int k = sb.Length - 1; k >= 0; k--)
                                        {
                                            if (sb[k] == '\n') break;
                                            outerCol++;
                                        }
                                        Runtime.PprintStartBlock(blockWriter, prefixStr.Length, perLinePrefix, outerCol);
                                        if (suffixStr.Length > 0)
                                            Runtime.PprintSetBlockSuffix(suffixStr.Length);
                                        try
                                        {
                                            int bodyArgIdx = 0;
                                            string effectiveBody = (closedAt && closedColon)
                                                ? InsertFillNewlinesInBody(body) : body;
                                            string formatted = FormatString(effectiveBody, bodyArgs, ref bodyArgIdx);
                                            blockWriter.Write(formatted);
                                            PprintTrackWrite(formatted);
                                            if (dottedTail != null)
                                            {
                                                string dt = " . " + FormatObject(dottedTail, false);
                                                blockWriter.Write(dt);
                                                PprintTrackWrite(dt);
                                            }
                                        }
                                        finally
                                        {
                                            Runtime.PprintEndBlock();
                                        }
                                        sb.Append(blockWriter.ToString());
                                        sb.Append(suffixStr);
                                    }
                                    else
                                    {
                                        sb.Append(prefixStr);
                                        int bodyArgIdx = 0;
                                        string formatted = FormatString(body, bodyArgs, ref bodyArgIdx);
                                        sb.Append(formatted);
                                        if (dottedTail != null)
                                            sb.Append(" . " + FormatObject(dottedTail, false));
                                        sb.Append(suffixStr);
                                    }
                                    }
                                }
                                catch (FormatUpAndOutException ex)
                                {
                                    sb.Append(ex.PartialOutput);
                                }
                                finally
                                {
                                    if (circleSetup)
                                        Runtime.PprintCircleEnd();
                                }
                            }
                            break;
                        }

                        // CLHS 22.3.6.1: ~I, ~_, ~W, ~:T inside justify body is an error
                        foreach (var sec in sections)
                        {
                            if (ContainsPrettyPrintDirective(sec))
                                throw new LispErrorException(new LispError(
                                    "FORMAT: ~I, ~_, ~W, and ~:T are not allowed inside ~<...~> justification"));
                        }

                        // Justification directive ~mincol,colinc,minpad,padchar<...~>
                        int jMincol = GetIntParam(0) ?? 0;
                        int jColinc = GetIntParam(1) ?? 1;
                        if (jColinc <= 0) jColinc = 1;
                        int jMinpad = GetIntParam(2) ?? 0;
                        char jPadchar = GetCharParam(3, ' ');

                        // Determine if first section is an overflow prefix (first ~; was ~:;)
                        bool hasOverflowPrefix = firstSepColon && sections.Count > 1;
                        string? overflowPrefix = null;
                        int overflowSpare = 0;      // spare chars parameter (default 0)
                        int overflowLineWidth = 72;  // line width parameter (default 72)
                        List<string> contentSections;

                        if (hasOverflowPrefix)
                        {
                            overflowPrefix = sections[0];
                            contentSections = sections.GetRange(1, sections.Count - 1);
                            // Parse ~spare,linewidth:; parameters
                            if (firstSepParams != null)
                            {
                                var parts = firstSepParams.Split(',');
                                if (parts.Length > 0 && parts[0].Length > 0 && int.TryParse(parts[0], out int sp))
                                    overflowSpare = sp;
                                if (parts.Length > 1 && parts[1].Length > 0 && int.TryParse(parts[1], out int lw))
                                    overflowLineWidth = lw;
                            }
                        }
                        else
                        {
                            contentSections = new List<string>(sections);
                        }

                        // Format each section, handling ~^ (up-and-out)
                        var formattedSections = new List<string>();
                        int localArgIdx = argIdx;
                        foreach (var sec in contentSections)
                        {
                            try
                            {
                                int secArgIdx = 0;
                                string formatted = FormatString(sec, args[localArgIdx..], ref secArgIdx);
                                formattedSections.Add(formatted);
                                localArgIdx += secArgIdx;
                            }
                            catch (FormatUpAndOutException)
                            {
                                // ~^ causes up-and-out: discard this section and stop processing further sections
                                break;
                            }
                        }
                        argIdx = localArgIdx;

                        // Calculate justification
                        int numSeg = formattedSections.Count;
                        int contentLen = 0;
                        foreach (var fs in formattedSections) contentLen += fs.Length;

                        // Number of padding slots where minpad applies
                        // minpad only applies to inter-segment gaps (and colon/at-sign extra slots)
                        int numSlots;
                        int minpadSlots; // slots where minpad applies
                        if (numSeg == 0)
                        {
                            numSlots = 1;
                            minpadSlots = 0;
                        }
                        else if (numSeg == 1 && !colonMod && !atMod)
                        {
                            // Single segment, no modifiers: pad on the left, no minpad
                            numSlots = 1;
                            minpadSlots = 0;
                        }
                        else
                        {
                            numSlots = numSeg - 1;
                            if (colonMod) numSlots++;
                            if (atMod) numSlots++;
                            if (numSlots == 0) numSlots = 1;
                            minpadSlots = numSlots;
                        }

                        // Total minimum padding from minpad
                        int totalMinPad = jMinpad * minpadSlots;

                        // Calculate total width: mincol + k*colinc such that width >= contentLen + totalMinPad
                        int totalWidth = jMincol;
                        int needed = contentLen + totalMinPad;
                        while (totalWidth < needed) totalWidth += jColinc;

                        int totalPadding = totalWidth - contentLen;

                        // Distribute padding across slots
                        int padPerSlot = numSlots > 0 ? totalPadding / numSlots : 0;
                        int extraPad = numSlots > 0 ? totalPadding % numSlots : 0;

                        // Build per-slot padding array. Extra goes to rightmost slots.
                        int[] pads = new int[numSlots];
                        for (int si = 0; si < numSlots; si++)
                            pads[si] = padPerSlot;
                        // Distribute extra from the right
                        for (int si = numSlots - 1; si >= 0 && extraPad > 0; si--)
                        {
                            pads[si]++;
                            extraPad--;
                        }

                        // Build the justified string
                        var justSb = new System.Text.StringBuilder();

                        if (numSeg == 0)
                        {
                            // No content segments, just padding
                            justSb.Append(jPadchar, totalPadding);
                        }
                        else if (numSeg == 1 && !colonMod && !atMod)
                        {
                            // Single segment, no modifiers: left-pad then content
                            justSb.Append(jPadchar, pads[0]);
                            justSb.Append(formattedSections[0]);
                        }
                        else
                        {
                            int slotIdx = 0;
                            if (colonMod)
                            {
                                justSb.Append(jPadchar, pads[slotIdx++]);
                            }
                            for (int si = 0; si < numSeg; si++)
                            {
                                justSb.Append(formattedSections[si]);
                                if (si < numSeg - 1 && slotIdx < numSlots)
                                {
                                    justSb.Append(jPadchar, pads[slotIdx++]);
                                }
                            }
                            if (atMod && slotIdx < numSlots)
                            {
                                justSb.Append(jPadchar, pads[slotIdx++]);
                            }
                        }

                        // Handle overflow prefix: if first ~; was ~:;, check whether
                        // the justified text fits on the current line
                        if (hasOverflowPrefix && overflowPrefix != null)
                        {
                            // Compute current column position: count chars since last newline in sb
                            int curCol = 0;
                            for (int ci = sb.Length - 1; ci >= 0; ci--)
                            {
                                if (sb[ci] == '\n') break;
                                curCol++;
                            }
                            int resultWidth = justSb.Length;
                            // Overflow if: curCol + resultWidth + spare > lineWidth
                            if (curCol + resultWidth + overflowSpare > overflowLineWidth)
                            {
                                string fmtOverflow = FormatString(overflowPrefix, Array.Empty<LispObject>());
                                sb.Append(fmtOverflow);
                            }
                        }

                        sb.Append(justSb);
                        break;
                    }
                    case '^': // up-and-out
                    {
                        // Resolve parameters for ~^ condition check
                        // Parameters were already resolved above into resolvedParams
                        // When V consumes nil, the parameter is null and should be
                        // treated as omitted, reducing the effective parameter count.
                        bool shouldEscape = false;

                        // Collect non-null parameters (null = V consumed nil = omitted)
                        var effectiveParams = new System.Collections.Generic.List<object>();
                        for (int pi = 0; pi < resolvedParams.Length; pi++)
                        {
                            if (resolvedParams[pi] is int iv2) effectiveParams.Add(iv2);
                            else if (resolvedParams[pi] is char cv2) effectiveParams.Add((int)cv2);
                            else if (resolvedParams[pi] is Bignum bv2) effectiveParams.Add(bv2);
                            else if (resolvedParams[pi] != null) effectiveParams.Add(0);
                            // null (from V consuming nil) is skipped → omitted
                        }
                        int paramCount = effectiveParams.Count;

                        if (paramCount == 0)
                        {
                            // No params: ~^ escapes if no more args; ~:^ escapes if last sublist
                            shouldEscape = colonMod ? _isLastSublist : (argIdx >= args.Length);
                        }
                        else if (paramCount == 1)
                        {
                            // 1 param: escape if n == 0
                            shouldEscape = EscapeParamIsZero(effectiveParams[0]);
                        }
                        else if (paramCount == 2)
                        {
                            // 2 params: escape if n == m
                            shouldEscape = EscapeParamsEqual(effectiveParams[0], effectiveParams[1]);
                        }
                        else
                        {
                            // 3 params: escape if p1 <= p2 && p2 <= p3
                            shouldEscape = EscapeParamLessOrEqual(effectiveParams[0], effectiveParams[1])
                                        && EscapeParamLessOrEqual(effectiveParams[1], effectiveParams[2]);
                        }

                        if (shouldEscape)
                        {
                            if (colonMod)
                                throw FormatColonUpAndOutException.GetInstance(sb.ToString());
                            else
                                throw FormatUpAndOutException.GetInstance(sb.ToString());
                        }
                        break;
                    }
                    case '/': // call function ~/name/
                    {
                        // Parse function name: everything up to the next '/'
                        int slashStart = i;
                        while (i < template.Length && template[i] != '/') i++;
                        string funcName = template[slashStart..i];
                        if (i < template.Length) i++; // skip closing '/'

                        // Get the argument
                        var slashArg = argIdx < args.Length ? args[argIdx++] : Nil.Instance;

                        // Look up the function - handle package prefix
                        Symbol? slashSym = null;
                        string upperName = funcName.ToUpperInvariant();
                        int colonPos = upperName.IndexOf(':');
                        if (colonPos >= 0)
                        {
                            string pkgName = upperName[..colonPos];
                            string symName = upperName[(colonPos + 1)..].TrimStart(':');
                            var pkg = Package.FindPackage(pkgName);
                            if (pkg != null)
                                slashSym = pkg.FindSymbol(symName).symbol;
                        }
                        else
                        {
                            // Try CL package first, then CL-USER
                            slashSym = Startup.CL.FindSymbol(upperName).symbol;
                            if (slashSym == null)
                            {
                                var clUser = Package.FindPackage("CL-USER") ?? Package.FindPackage("COMMON-LISP-USER");
                                if (clUser != null)
                                    slashSym = clUser.FindSymbol(upperName).symbol;
                            }
                        }

                        var slashFunc = slashSym?.Function as LispFunction;
                        if (slashFunc != null)
                        {
                            // Create a stream for output
                            var sw = new System.IO.StringWriter();
                            var stream = new LispOutputStream(sw);

                            // Build args: (stream arg colonp atp &rest params)
                            var callArgs = new List<LispObject>();
                            callArgs.Add(stream);
                            callArgs.Add(slashArg);
                            callArgs.Add(colonMod ? (LispObject)T.Instance : Nil.Instance);
                            callArgs.Add(atMod ? (LispObject)T.Instance : Nil.Instance);
                            for (int pi = 0; pi < resolvedParams.Length; pi++)
                            {
                                var p = resolvedParams[pi];
                                if (p is int iv) callArgs.Add(Fixnum.Make(iv));
                                else if (p is char cv) callArgs.Add(LispChar.Make(cv));
                                else callArgs.Add(Nil.Instance);
                            }

                            slashFunc.Invoke(callArgs.ToArray());
                            sb.Append(sw.ToString());
                        }
                        break;
                    }
                    case '|': // page (form-feed)
                    {
                        int count = prefixParam ?? 1;
                        for (int j = 0; j < count; j++)
                            sb.Append('\f');
                        break;
                    }
                    case '\n': // ~<newline>: CLHS 22.3.9.2
                    {
                        if (atMod)
                        {
                            // ~@<newline>: output newline, skip following whitespace
                            sb.Append('\n');
                            while (i < template.Length && (template[i] == ' ' || template[i] == '\t'))
                                i++;
                        }
                        else if (colonMod)
                        {
                            // ~:<newline>: skip newline only, keep following whitespace
                        }
                        else
                        {
                            // ~<newline>: skip newline and following whitespace
                            while (i < template.Length && (template[i] == ' ' || template[i] == '\t'))
                                i++;
                        }
                        break;
                    }
                    case '_': // pprint-newline
                    {
                        var ppVal = DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out var ppv3) ? ppv3 : Startup.Sym("*PRINT-PRETTY*").Value;
                        if (ppVal is not Nil && _pprintActive)
                        {
                            // Flush sb to the pprint stream so _pprintColumn is accurate
                            if (_pprintStream != null && sb.Length > 0)
                            {
                                var flushed = sb.ToString();
                                _pprintStream.Write(flushed);
                                PprintTrackWrite(flushed);
                                sb.Clear();
                            }

                            if (colonMod && atMod)
                            {
                                // ~:@_ = mandatory newline
                                if (_pprintStream != null)
                                    PprintMandatoryNewline(_pprintStream);
                                else
                                {
                                    sb.Append('\n');
                                    if (_pprintIndent > 0)
                                        sb.Append(' ', _pprintIndent);
                                    _pprintColumn = _pprintIndent;
                                }
                            }
                            else if (atMod)
                            {
                                // ~@_ = miser newline
                                if (_pprintStream != null)
                                    PprintConditionalNewline(_pprintStream, "MISER");
                            }
                            else
                            {
                                // ~:_ = fill, ~_ = linear
                                if (_pprintStream != null)
                                {
                                    string kind = colonMod ? "FILL" : "LINEAR";
                                    PprintConditionalNewline(_pprintStream, kind);
                                }
                            }
                        }
                        break;
                    }
                    case 'I': // pprint-indent
                    {
                        var ppVal2 = DynamicBindings.TryGet(Startup.Sym("*PRINT-PRETTY*"), out var ppv4) ? ppv4 : Startup.Sym("*PRINT-PRETTY*").Value;
                        if (ppVal2 is not Nil)
                        {
                            // Flush sb to the pprint stream so _pprintColumn is accurate
                            if (_pprintActive && _pprintStream != null && sb.Length > 0)
                            {
                                var flushed = sb.ToString();
                                _pprintStream.Write(flushed);
                                PprintTrackWrite(flushed);
                                sb.Clear();
                            }
                            else if (_pprintActive && _pprintStream == null)
                            {
                                // Format-only logical block: sync column from sb content
                                int lastNl = -1;
                                for (int j = sb.Length - 1; j >= 0; j--)
                                    if (sb[j] == '\n') { lastNl = j; break; }
                                int curCol = sb.Length - lastNl - 1;
                                _pprintColumn = curCol;
                            }

                            int indN = GetIntParam(0) ?? 0;
                            if (colonMod)
                            {
                                // ~n:I = (pprint-indent :current n)
                                Runtime.PprintSetIndent("CURRENT", indN);
                            }
                            else
                            {
                                // ~nI = (pprint-indent :block n)
                                Runtime.PprintSetIndent("BLOCK", indN);
                            }
                        }
                        break;
                    }
                    case 'W': // write: print arg using current print settings
                    {
                        if (argIdx >= args.Length)
                            break;
                        var obj = args[argIdx++];
                        sb.Append(FormatTop(obj, GetPrintEscapePublic()));
                        break;
                    }
                    default:
                        sb.Append('~');
                        sb.Append(directive);
                        break;
                }
            }
            else
            {
                sb.Append(template[i]);
                i++;
            }
        }
        return sb.ToString();
    }

    private static List<string> ParseJustificationSections(string template, ref int pos)
    {
        return ParseJustificationSections(template, ref pos, out _, out _, out _, out _, out _);
    }

    /// <summary>
    /// Insert ~:_ (FILL conditional newline) after each group of literal blanks
    /// in a format body string. Used by ~:@> to implement paragraph filling.
    /// Blanks inside ~...~ directives are not affected.
    /// </summary>
    private static string InsertFillNewlinesInBody(string body)
    {
        var result = new System.Text.StringBuilder(body.Length + 20);
        int i = 0;
        while (i < body.Length)
        {
            if (body[i] == '~')
            {
                // Copy the entire format directive (skip blanks inside directives)
                result.Append('~');
                i++;
                // Handle ~<newline>: skip newline and following whitespace
                if (i < body.Length && body[i] == '\n')
                {
                    result.Append(body[i++]);
                    // Skip following whitespace (part of the ~<newline> directive)
                    while (i < body.Length && (body[i] == ' ' || body[i] == '\t'))
                    {
                        result.Append(body[i++]);
                    }
                    continue;
                }
                // Skip params, colon, at
                while (i < body.Length)
                {
                    char dc = body[i];
                    if (dc == ',' || dc == '#' || dc == 'v' || dc == 'V' || dc == '\'' ||
                        (dc >= '0' && dc <= '9') || dc == '+' || dc == '-')
                    {
                        result.Append(dc);
                        i++;
                        if (dc == '\'' && i < body.Length)
                        {
                            result.Append(body[i++]);
                        }
                    }
                    else if (dc == ':' || dc == '@')
                    {
                        result.Append(dc);
                        i++;
                    }
                    else
                    {
                        // This is the directive character
                        result.Append(dc);
                        i++;
                        break;
                    }
                }
            }
            else if (body[i] == ' ' || body[i] == '\t')
            {
                // Literal blank group: copy blanks, then insert ~:_
                while (i < body.Length && (body[i] == ' ' || body[i] == '\t'))
                {
                    result.Append(body[i++]);
                }
                result.Append("~:_");
            }
            else
            {
                result.Append(body[i++]);
            }
        }
        return result.ToString();
    }

    /// <summary>
    /// Parse sections within ~&lt;...~&gt;. Sections are separated by ~; or ~:;.
    /// Returns section strings, plus metadata about the first separator and closing directive.
    /// </summary>
    private static List<string> ParseJustificationSections(string template, ref int pos,
        out bool firstSepIsColon, out string? firstSepParams, out bool closedWithColon, out bool closedWithAt,
        out bool firstSepIsAt)
    {
        var sections = new List<string>();
        var current = new System.Text.StringBuilder();
        firstSepIsColon = false;
        firstSepParams = null;
        closedWithColon = false;
        closedWithAt = false;
        firstSepIsAt = false;
        int depth = 1;
        bool isFirstSep = true;
        while (pos < template.Length && depth > 0)
        {
            if (template[pos] == '~' && pos + 1 < template.Length)
            {
                int start = pos;
                pos++;
                // skip numeric prefix and comma-separated params (including 'char params)
                int paramStart = pos;
                while (pos < template.Length && (char.IsDigit(template[pos]) || template[pos] == ',' || template[pos] == '-'))
                    pos++;
                // Also handle 'char params embedded in the prefix
                // Re-scan properly: go back and scan prefix params
                pos = paramStart;
                while (pos < template.Length)
                {
                    if (char.IsDigit(template[pos]) || template[pos] == '-')
                    {
                        if (template[pos] == '-') pos++; // consume minus sign
                        while (pos < template.Length && char.IsDigit(template[pos])) pos++;
                    }
                    else if (template[pos] == '\'' && pos + 1 < template.Length)
                    {
                        pos += 2; // skip 'char
                    }
                    else if (template[pos] == ',')
                    {
                        pos++; // skip comma separator
                    }
                    else if (template[pos] == 'V' || template[pos] == 'v' || template[pos] == '#')
                    {
                        pos++;
                    }
                    else
                    {
                        break;
                    }
                }
                string paramStr = template[paramStart..pos];
                // skip modifiers
                bool hasColon = false;
                bool hasAt = false;
                while (pos < template.Length && (template[pos] == ':' || template[pos] == '@'))
                {
                    if (template[pos] == ':') hasColon = true;
                    if (template[pos] == '@') hasAt = true;
                    pos++;
                }
                if (pos >= template.Length) break;
                char d = char.ToUpper(template[pos]);
                pos++;
                if (d == '>' && depth == 1)
                {
                    depth--;
                    closedWithColon = hasColon;
                    closedWithAt = hasAt;
                    sections.Add(current.ToString());
                    break;
                }
                else if (d == '>')
                {
                    depth--;
                    current.Append(template[start..pos]);
                }
                else if (d == '<')
                {
                    depth++;
                    current.Append(template[start..pos]);
                }
                else if (d == ';' && depth == 1)
                {
                    // ~; or ~:; separator at top level
                    if (isFirstSep)
                    {
                        firstSepIsColon = hasColon;
                        firstSepIsAt = hasAt;
                        firstSepParams = paramStr.Length > 0 ? paramStr : null;
                        isFirstSep = false;
                    }
                    sections.Add(current.ToString());
                    current.Clear();
                }
                else
                {
                    current.Append(template[start..pos]);
                }
            }
            else
            {
                current.Append(template[pos]);
                pos++;
            }
        }
        return sections;
    }

    private static int CountFormatArgs(string fmt)
    {
        int count = 0;
        for (int j = 0; j < fmt.Length - 1; j++)
        {
            if (fmt[j] == '~')
            {
                j++;
                while (j < fmt.Length && (char.IsDigit(fmt[j]) || fmt[j] == ',' || fmt[j] == '\'' || fmt[j] == ':' || fmt[j] == '@')) j++;
                if (j < fmt.Length)
                {
                    char d = char.ToUpper(fmt[j]);
                    if (d is 'A' or 'S' or 'D' or 'B' or 'O' or 'X' or 'R' or 'F' or 'C' or 'W')
                        count++;
                }
            }
        }
        return count;
    }

    private static string ParseCaseBody(string template, ref int pos)
    {
        var body = new System.Text.StringBuilder();
        int depth = 1;
        while (pos < template.Length && depth > 0)
        {
            if (template[pos] == '~' && pos + 1 < template.Length)
            {
                int start = pos;
                pos++;
                while (pos < template.Length && (template[pos] == ':' || template[pos] == '@'))
                    pos++;
                if (pos >= template.Length) break;
                char d = char.ToUpper(template[pos]);
                pos++;
                if (d == ')')
                {
                    depth--;
                    if (depth == 0) break;
                    body.Append(template[start..pos]);
                }
                else if (d == '(')
                {
                    depth++;
                    body.Append(template[start..pos]);
                }
                else
                {
                    body.Append(template[start..pos]);
                }
            }
            else
            {
                body.Append(template[pos]);
                pos++;
            }
        }
        return body.ToString();
    }

    private static (List<string> clauses, bool hasDefault) ParseConditionalClauses(string template, ref int pos)
    {
        var clauses = new List<string>();
        var current = new System.Text.StringBuilder();
        int depth = 1;
        int otherDepth = 0; // track ~< ~> and ~( ~) nesting
        bool lastSepWasColon = false; // tracks if last ~; was ~:;
        while (pos < template.Length && depth > 0)
        {
            if (template[pos] == '~' && pos + 1 < template.Length)
            {
                int start = pos;
                pos++;
                // skip numeric prefix
                while (pos < template.Length && char.IsDigit(template[pos])) pos++;
                // skip modifiers
                string mods = "";
                while (pos < template.Length && (template[pos] == ':' || template[pos] == '@'))
                {
                    mods += template[pos];
                    pos++;
                }
                if (pos >= template.Length) break;
                char d = char.ToUpper(template[pos]);
                pos++;
                if (d == ']' && otherDepth == 0)
                {
                    depth--;
                    if (depth == 0) { clauses.Add(current.ToString()); break; }
                    current.Append(template[start..pos]);
                }
                else if (d == '[' && otherDepth == 0)
                {
                    depth++;
                    current.Append(template[start..pos]);
                }
                else if (d == '<' || d == '(')
                {
                    otherDepth++;
                    current.Append(template[start..pos]);
                }
                else if (d == '>' || d == ')')
                {
                    otherDepth = Math.Max(0, otherDepth - 1);
                    current.Append(template[start..pos]);
                }
                else if (d == ';' && depth == 1 && otherDepth == 0)
                {
                    clauses.Add(current.ToString());
                    current.Clear();
                    lastSepWasColon = mods.Contains(':');
                }
                else
                {
                    current.Append(template[start..pos]);
                }
            }
            else
            {
                current.Append(template[pos]);
                pos++;
            }
        }
        return (clauses, lastSepWasColon);
    }



    /// <summary>
    /// CLHS 22.3.6.1: ~I, ~_, ~W, ~:T, and ~<...~:> cannot coexist with ~<...~:;...~>
    /// (justify with overflow) in the same format string. Check for this conflict and signal an error.
    /// </summary>
    private static void ValidateJustifyPrettyPrintConflict(string template)
    {
        bool hasJustifyWithOverflow = false;
        bool hasPPDirective = false;
        bool hasLogicalBlock = false;
        int depth = 0; // nesting depth of ~< ~>
        for (int ci = 0; ci < template.Length; ci++)
        {
            if (template[ci] != '~' || ci + 1 >= template.Length) continue;
            int di = ci + 1;
            // Skip params
            while (di < template.Length && (char.IsDigit(template[di]) || template[di] == ','
                || template[di] == 'V' || template[di] == 'v' || template[di] == '#'
                || template[di] == '\'' || template[di] == '+' || template[di] == '-'))
            {
                if (template[di] == '\'' && di + 1 < template.Length) di += 2; else di++;
            }
            bool hasColon = false;
            while (di < template.Length && (template[di] == ':' || template[di] == '@'))
            {
                if (template[di] == ':') hasColon = true;
                di++;
            }
            if (di >= template.Length) break;
            char dc = char.ToUpper(template[di]);
            ci = di; // advance past this directive
            if (dc == '<') depth++;
            else if (dc == '>')
            {
                depth = Math.Max(0, depth - 1);
                // ~:> at depth 0 means a logical block was closed
                if (hasColon) hasLogicalBlock = true;
            }
            else if (dc == ';' && depth == 1 && hasColon)
            {
                // ~:; inside a top-level ~<...~> means justify with overflow
                hasJustifyWithOverflow = true;
            }
            else if (depth == 0 && (dc == 'I' || dc == '_' || dc == 'W' || (dc == 'T' && hasColon)))
            {
                hasPPDirective = true;
            }
        }
        if (hasJustifyWithOverflow && (hasPPDirective || hasLogicalBlock))
            throw new LispErrorException(new LispError(
                "FORMAT: ~I, ~_, ~W, ~:T, and ~<...~:> cannot be used in the same format string as ~<...~:;...~>"));
    }

    /// <summary>
    /// Check if a string contains format directives (any ~ followed by a directive char).
    /// Used to validate that ~<...~:> prefix/suffix segments are plain strings.
    /// </summary>
    private static bool ContainsFormatDirective(string s)
    {
        for (int ci = 0; ci < s.Length - 1; ci++)
        {
            if (s[ci] == '~')
            {
                // Skip past parameters, modifiers to find directive char
                int di = ci + 1;
                // Skip whitespace/newline (which would be ~\n directive)
                if (di < s.Length && (s[di] == '\n' || s[di] == '\r'))
                    return true;
                // Skip numeric params, commas, V, #, '
                while (di < s.Length && (char.IsDigit(s[di]) || s[di] == ',' || s[di] == 'V' || s[di] == 'v'
                    || s[di] == '#' || s[di] == '\'' || s[di] == '+' || s[di] == '-'))
                {
                    if (s[di] == '\'' && di + 1 < s.Length) di += 2; else di++;
                }
                // Skip modifiers : @
                while (di < s.Length && (s[di] == ':' || s[di] == '@')) di++;
                // Now at directive char — any valid directive means this has format content
                if (di < s.Length)
                    return true;
            }
        }
        return false;
    }

    /// <summary>
    /// Check if a format string contains any of the specified directives.
    /// Used to validate ~I, ~_ are not in justify body per CLHS 22.3.6.1.
    /// </summary>
    private static bool ContainsPrettyPrintDirective(string s)
    {
        int depth = 0;
        for (int ci = 0; ci < s.Length - 1; ci++)
        {
            if (s[ci] == '~')
            {
                int di = ci + 1;
                // Skip numeric params, commas, V, #, '
                while (di < s.Length && (char.IsDigit(s[di]) || s[di] == ',' || s[di] == 'V' || s[di] == 'v'
                    || s[di] == '#' || s[di] == '\'' || s[di] == '+' || s[di] == '-'))
                {
                    if (s[di] == '\'' && di + 1 < s.Length) di += 2; else di++;
                }
                // Check for modifiers : @
                bool hasColon = false;
                while (di < s.Length && (s[di] == ':' || s[di] == '@'))
                {
                    if (s[di] == ':') hasColon = true;
                    di++;
                }
                if (di < s.Length)
                {
                    char dc = char.ToUpper(s[di]);
                    if (dc == '<') depth++;
                    else if (dc == '>')
                    {
                        depth = Math.Max(0, depth - 1);
                        // ~:> means logical block — error in justify
                        if (hasColon) return true;
                    }
                    else if (depth == 0)
                    {
                        // ~I (pprint-indent), ~_ (pprint-newline) are errors in justify
                        if (dc == 'I' || dc == '_') return true;
                        // ~W is also an error in justify body
                        if (dc == 'W') return true;
                        // ~:T is an error in justify (but ~T without colon is ok)
                        if (dc == 'T' && hasColon) return true;
                    }
                    ci = di;
                }
            }
        }
        return false;
    }

    private static bool EscapeParamIsZero(object p)
    {
        if (p is int iv) return iv == 0;
        if (p is Bignum bv) return bv.Value.IsZero;
        return false;
    }

    private static bool EscapeParamsEqual(object a, object b)
    {
        if (a is int ia && b is int ib) return ia == ib;
        // Convert to BigInteger for mixed/bignum comparison
        var ba = a is int ia2 ? new System.Numerics.BigInteger(ia2) : (a is Bignum bna ? bna.Value : System.Numerics.BigInteger.Zero);
        var bb = b is int ib2 ? new System.Numerics.BigInteger(ib2) : (b is Bignum bnb ? bnb.Value : System.Numerics.BigInteger.Zero);
        return ba == bb;
    }

    private static bool EscapeParamLessOrEqual(object a, object b)
    {
        if (a is int ia && b is int ib) return ia <= ib;
        var ba = a is int ia2 ? new System.Numerics.BigInteger(ia2) : (a is Bignum bna ? bna.Value : System.Numerics.BigInteger.Zero);
        var bb = b is int ib2 ? new System.Numerics.BigInteger(ib2) : (b is Bignum bnb ? bnb.Value : System.Numerics.BigInteger.Zero);
        return ba <= bb;
    }

    private static string ApplyCaseConversion(string inner, bool colonMod, bool atMod)
    {
        if (colonMod && atMod)
            return inner.ToUpperInvariant();
        if (colonMod)
            return System.Globalization.CultureInfo.InvariantCulture.TextInfo.ToTitleCase(inner.ToLowerInvariant());
        if (atMod)
        {
            string lowered = inner.ToLowerInvariant();
            var csb = new System.Text.StringBuilder(lowered.Length);
            bool found = false;
            for (int ci = 0; ci < lowered.Length; ci++)
            {
                if (!found && char.IsLetter(lowered[ci]))
                {
                    csb.Append(char.ToUpperInvariant(lowered[ci]));
                    found = true;
                }
                else csb.Append(lowered[ci]);
            }
            return csb.ToString();
        }
        return inner.ToLowerInvariant();
    }

    /// <summary>Iterate with a flat element array (for ~{~} and ~@{~} empty body).</summary>
    private static void FormatIterateFlat(LispObject fmtArg, LispObject[] elemArr, ref int elemIdx,
        int maxIter, bool forceOnce, System.Text.StringBuilder sb, ref int iterCount)
    {
        if (fmtArg is LispFunction fmtFn)
        {
            while ((elemIdx < elemArr.Length || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
            {
                var iterArgs = elemArr[elemIdx..];
                var callArgs = new LispObject[iterArgs.Length + 1];
                var strStream = new System.IO.StringWriter();
                callArgs[0] = new LispOutputStream(strStream);
                Array.Copy(iterArgs, 0, callArgs, 1, iterArgs.Length);
                var tail = fmtFn.Invoke(callArgs);
                sb.Append(strStream.ToString());
                int tailLen = 0;
                while (tail is Cons tc) { tailLen++; tail = tc.Cdr; }
                int consumed = iterArgs.Length - tailLen;
                elemIdx += consumed;
                iterCount++;
                if (consumed == 0) break;
            }
        }
        else
        {
            string indirectFmt = fmtArg is LispString ls ? ls.Value : FormatTop(fmtArg, false);
            while ((elemIdx < elemArr.Length || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
            {
                var iterArgs = elemArr[elemIdx..];
                int subIdx = 0;
                try
                {
                    sb.Append(FormatString(indirectFmt, iterArgs, ref subIdx));
                }
                catch (FormatUpAndOutException ex)
                {
                    sb.Append(ex.PartialOutput);
                    elemIdx += subIdx;
                    break;
                }
                elemIdx += subIdx;
                iterCount++;
                if (subIdx == 0) break;
            }
        }
    }

    /// <summary>Iterate over sublists (for ~:{~} and ~:@{~} empty body).</summary>
    private static void FormatIterateOverSublists(LispObject fmtArg, List<LispObject> sublists,
        int maxIter, bool forceOnce, System.Text.StringBuilder sb, ref int iterCount)
    {
        int slIdx = 0;
        if (fmtArg is LispFunction fmtFn)
        {
            while ((slIdx < sublists.Count || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
            {
                LispObject[] subArr;
                if (slIdx < sublists.Count)
                {
                    var subList = sublists[slIdx++];
                    var subElems = new List<LispObject>();
                    while (subList is Cons sc) { subElems.Add(sc.Car); subList = sc.Cdr; }
                    subArr = subElems.ToArray();
                }
                else { subArr = Array.Empty<LispObject>(); slIdx++; }
                var savedLastSublist = _isLastSublist;
                _isLastSublist = slIdx >= sublists.Count;
                var callArgs = new LispObject[subArr.Length + 1];
                var strStream = new System.IO.StringWriter();
                callArgs[0] = new LispOutputStream(strStream);
                Array.Copy(subArr, 0, callArgs, 1, subArr.Length);
                try
                {
                    var tail = fmtFn.Invoke(callArgs);
                    sb.Append(strStream.ToString());
                }
                finally { _isLastSublist = savedLastSublist; }
                iterCount++;
            }
        }
        else
        {
            string indirectFmt = fmtArg is LispString ls ? ls.Value : FormatTop(fmtArg, false);
            while ((slIdx < sublists.Count || (forceOnce && iterCount == 0)) && (maxIter < 0 || iterCount < maxIter))
            {
                LispObject[] subArr;
                if (slIdx < sublists.Count)
                {
                    var subList = sublists[slIdx++];
                    var subElems = new List<LispObject>();
                    while (subList is Cons sc) { subElems.Add(sc.Car); subList = sc.Cdr; }
                    subArr = subElems.ToArray();
                }
                else { subArr = Array.Empty<LispObject>(); slIdx++; }
                int subIdx = 0;
                var savedLastSublist = _isLastSublist;
                _isLastSublist = slIdx >= sublists.Count;
                try
                {
                    sb.Append(FormatString(indirectFmt, subArr, ref subIdx));
                }
                catch (FormatUpAndOutException ex) { sb.Append(ex.PartialOutput); }
                finally { _isLastSublist = savedLastSublist; }
                iterCount++;
            }
        }
    }

    private static string ParseIterationBody(string template, ref int pos, out bool colonClose)
    {
        var body = new System.Text.StringBuilder();
        int depth = 1;
        colonClose = false;
        while (pos < template.Length && depth > 0)
        {
            if (template[pos] == '~' && pos + 1 < template.Length)
            {
                int tildeStart = pos;
                pos++; // skip ~
                // Collect everything between ~ and the directive char (params, modifiers)
                int afterTilde = pos;
                bool hasColon = false;
                // Skip prefix params and modifiers to find the directive char
                while (pos < template.Length)
                {
                    char ch = template[pos];
                    if (ch == ':') { hasColon = true; pos++; }
                    else if (char.IsDigit(ch) || ch == ',' || ch == '-' || ch == '+' ||
                        ch == 'v' || ch == 'V' || ch == '#' || ch == '@')
                    {
                        pos++;
                    }
                    else if (ch == '\'' && pos + 1 < template.Length)
                    {
                        pos += 2; // skip 'c
                    }
                    else
                    {
                        break;
                    }
                }
                if (pos >= template.Length) break;
                char d = template[pos];
                char du = char.ToUpper(d);
                pos++;
                if (du == '}')
                {
                    depth--;
                    if (depth == 0) { colonClose = hasColon; break; }
                    // Include the full directive text for nested ~}
                    body.Append(template[tildeStart..pos]);
                }
                else if (du == '{')
                {
                    depth++;
                    body.Append(template[tildeStart..pos]);
                }
                else
                {
                    // Preserve full directive text including params and modifiers
                    body.Append(template[tildeStart..pos]);
                }
            }
            else
            {
                body.Append(template[pos]);
                pos++;
            }
        }
        return body.ToString();
    }


}
