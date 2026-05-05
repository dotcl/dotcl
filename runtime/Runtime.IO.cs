using System.Numerics;

namespace DotCL;

public static partial class Runtime
{
    // --- Pretty Printing State ---
    [ThreadStatic] private static int _pprintColumn;        // current column position
    [ThreadStatic] private static int _pprintBlockColumn;   // column where current logical block started
    public static int _pprintBlockColumnPublic => _pprintBlockColumn;
    [ThreadStatic] private static int _pprintIndent;        // indentation level (absolute column)
    [ThreadStatic] private static bool _pprintActive;       // are we inside a logical block?
    [ThreadStatic] private static TextWriter? _pprintStream; // stream being tracked
    [ThreadStatic] private static string? _pprintPerLinePrefix; // per-line prefix string (output at start of each line)
    // Pending fill break: deferred newline that fires before the next write
    [ThreadStatic] private static bool _pprintPendingFillBreak;
    [ThreadStatic] private static int _pprintColumnAtLastFillCheck;
    // XP-style buffering: defer conditional newlines and process at outermost block end
    // These fields are NOT saved/restored per inner block — they persist across the full outermost block.
    [ThreadStatic] private static bool _pprintBuffering;        // are we buffering for XP processing?
    [ThreadStatic] private static int _pprintBufferNestLevel;   // nesting level (0 = outermost)
    [ThreadStatic] private static int _pprintBufferStart;       // StringWriter position at outermost block start
    [ThreadStatic] private static List<(int pos, string kind, int blockCol, int indent, string? plp, int suffixLen)>? _pprintDeferredNewlines;
    // Saved state stack for nested logical blocks
    [ThreadStatic] private static Stack<(int col, int blockCol, int indent, bool active, TextWriter? stream, string? perLinePrefix,
        bool pendingFill, int colAtFillCheck)>? _pprintStack;

    // Safety threshold multiplier for XP-style buffering.
    // When the buffered output exceeds (right-margin * PprintBufferMarginMultiplier) characters,
    // we abandon deferred-newline buffering to prevent unbounded memory use.
    // A multiplier of 100 allows up to 100 lines' worth of characters before the safety valve
    // triggers; legitimate pretty-printed output rarely needs more than a few lines of buffer.
    private const int PprintBufferMarginMultiplier = 100;

    /// <summary>Start a pretty-printing logical block. Records block start column and activates tracking.</summary>
    public static void PprintStartBlock(TextWriter writer, int prefixLength)
    {
        PprintStartBlock(writer, prefixLength, null);
    }

    /// <summary>Start a pretty-printing logical block with optional per-line-prefix and column offset.</summary>
    public static void PprintStartBlock(TextWriter writer, int prefixLength, string? perLinePrefix, int columnOffset = 0)
    {
        // Save current state for nesting
        _pprintStack ??= new();
        _pprintStack.Push((_pprintColumn, _pprintBlockColumn, _pprintIndent, _pprintActive, _pprintStream, _pprintPerLinePrefix,
            _pprintPendingFillBreak, _pprintColumnAtLastFillCheck));

        bool wasActive = _pprintActive;
        // If not already in a logical block, compute column from stream content
        if (!wasActive)
        {
            if (writer is StringWriter sw0)
            {
                // Compute column from existing content (prefix already written to stream)
                _pprintColumn = 0;
                var sb0 = sw0.GetStringBuilder();
                for (int i = sb0.Length - 1; i >= 0; i--)
                {
                    if (sb0[i] == '\n') break;
                    _pprintColumn++;
                }
                // Add column offset from outer context (e.g. format sb position)
                _pprintColumn += columnOffset;
            }
            else
            {
                _pprintColumn = prefixLength + columnOffset;
            }
        }
        else if (writer == null)
        {
            // format's null writer: prefix not tracked, add explicitly
            _pprintColumn += prefixLength;
        }

        _pprintActive = true;
        _pprintStream = writer;
        _pprintPerLinePrefix = perLinePrefix;
        _pprintBlockColumn = _pprintColumn;
        // Default indent is right after the prefix
        _pprintIndent = _pprintBlockColumn;
        _pprintPendingFillBreak = false;
        _pprintColumnAtLastFillCheck = _pprintColumn;

        // XP-style buffering: only the outermost block starts a new buffer
        if (!_pprintBuffering && writer is StringWriter sw)
        {
            _pprintBuffering = true;
            _pprintBufferNestLevel = 0;
            _pprintBufferStart = sw.GetStringBuilder().Length;
            _pprintDeferredNewlines = new();
            // Record block-start for the outermost block
            _pprintDeferredNewlines.Add((sw.GetStringBuilder().Length, "BLOCK_START", _pprintBlockColumn, _pprintIndent, _pprintPerLinePrefix, 0));
        }
        else if (_pprintBuffering && writer is StringWriter sw2)
        {
            _pprintBufferNestLevel++;
            // Record inner block start
            _pprintDeferredNewlines?.Add((sw2.GetStringBuilder().Length, "BLOCK_START", _pprintBlockColumn, _pprintIndent, perLinePrefix, 0));
        }
    }

    /// <summary>Set the suffix length for the current block (used for fit/break calculations).</summary>
    public static void PprintSetBlockSuffix(int suffixLen)
    {
        if (_pprintDeferredNewlines == null) return;
        // Find the most recent BLOCK_START at the current nesting level
        for (int i = _pprintDeferredNewlines.Count - 1; i >= 0; i--)
        {
            if (_pprintDeferredNewlines[i].kind == "BLOCK_START")
            {
                var tok = _pprintDeferredNewlines[i];
                _pprintDeferredNewlines[i] = (tok.pos, tok.kind, tok.blockCol, tok.indent, tok.plp, suffixLen);
                break;
            }
        }
    }

    /// <summary>End a pretty-printing logical block. Processes XP buffer if outermost.</summary>
    public static void PprintEndBlock()
    {
        // Record block-end token
        if (_pprintBuffering && _pprintStream is StringWriter sw)
        {
            _pprintDeferredNewlines?.Add((sw.GetStringBuilder().Length, "BLOCK_END", 0, 0, null, 0));

            if (_pprintBufferNestLevel == 0)
            {
                // Outermost block: process the full XP buffer
                ProcessXpBuffer(sw);
                _pprintBuffering = false;
                _pprintDeferredNewlines = null;
            }
            else
            {
                _pprintBufferNestLevel--;
            }
        }

        _pprintPendingFillBreak = false;
        if (_pprintStack != null && _pprintStack.Count > 0)
        {
            var (col, blockCol, indent, active, stream, perLinePrefix,
                pendingFill, colAtFillCheck) = _pprintStack.Pop();
            // Restore saved state but keep current column for the outer block
            _pprintBlockColumn = blockCol;
            _pprintIndent = indent;
            _pprintActive = active;
            _pprintStream = stream;
            _pprintPerLinePrefix = perLinePrefix;
            _pprintPendingFillBreak = pendingFill;
            _pprintColumnAtLastFillCheck = colAtFillCheck;
            // Don't restore column - the outer block needs to track where we are now
        }
        else
        {
            _pprintActive = false;
            _pprintStream = null;
            _pprintPerLinePrefix = null;
        }
    }

    /// <summary>Update column position after writing text. Handles newlines.</summary>
    public static void PprintTrackWrite(string text)
    {
        if (!_pprintActive) return;
        for (int i = 0; i < text.Length; i++)
        {
            if (text[i] == '\n')
                _pprintColumn = 0;
            else
                _pprintColumn++;
        }
    }

    /// <summary>Update column position after writing a single character.</summary>
    public static void PprintTrackWriteChar(char c)
    {
        if (!_pprintActive) return;
        if (c == '\n')
            _pprintColumn = 0;
        else
            _pprintColumn++;
    }

    /// <summary>After a newline is written to writer inside a pprint block,
    /// emit the per-line-prefix and reset column tracking.</summary>
    public static void PprintAfterNewline(TextWriter writer)
    {
        if (!_pprintActive || _pprintPerLinePrefix == null) return;
        writer.Write(_pprintPerLinePrefix);
        _pprintColumn = _pprintPerLinePrefix.Length;
    }

    /// <summary>Trim trailing whitespace (spaces/tabs) from a TextWriter's buffer.
    /// Only works for StringWriter; other writers are left unchanged.
    /// Also adjusts _pprintColumn to account for removed characters.</summary>
    private static void PprintTrimTrailingWhitespace(TextWriter writer)
    {
        if (writer is StringWriter sw)
        {
            var sb = sw.GetStringBuilder();
            int removed = 0;
            while (sb.Length > 0 && (sb[sb.Length - 1] == ' ' || sb[sb.Length - 1] == '\t'))
            {
                sb.Length--;
                removed++;
            }
            _pprintColumn = Math.Max(0, _pprintColumn - removed);
        }
    }

    /// <summary>Emit a mandatory pprint newline: newline + per-line-prefix + indent spaces.</summary>
    public static void PprintMandatoryNewline(TextWriter writer)
    {
        if (!_pprintActive) return;
        PprintTrimTrailingWhitespace(writer);
        writer.Write('\n');
        int col = 0;
        if (_pprintPerLinePrefix != null)
        {
            writer.Write(_pprintPerLinePrefix);
            col = _pprintPerLinePrefix.Length;
        }
        if (_pprintIndent > col)
            writer.Write(new string(' ', _pprintIndent - col));
        _pprintColumn = Math.Max(col, _pprintIndent);
    }

    /// <summary>Get *print-right-margin* value, defaulting to 72.</summary>
    internal static int GetPrintRightMargin()
    {
        var rmSym = Startup.Sym("*PRINT-RIGHT-MARGIN*");
        var val = DynamicBindings.TryGet(rmSym, out var rmVal) ? rmVal : rmSym.Value;
        if (val is Fixnum f) return (int)f.Value;
        return 72; // default
    }

    /// <summary>Execute a pending fill break if one is set. Call before writing new content.</summary>
    public static void PprintFlushPendingBreak(TextWriter writer)
    {
        if (_pprintBuffering) return; // suppressed during XP buffering
        if (!_pprintPendingFillBreak || !_pprintActive || writer == null) return;
        _pprintPendingFillBreak = false;
        PprintTrimTrailingWhitespace(writer);
        writer.Write('\n');
        int col = 0;
        if (_pprintPerLinePrefix != null)
        {
            writer.Write(_pprintPerLinePrefix);
            col = _pprintPerLinePrefix.Length;
        }
        if (_pprintIndent > col)
            writer.Write(new string(' ', _pprintIndent - col));
        _pprintColumn = Math.Max(col, _pprintIndent);
        _pprintColumnAtLastFillCheck = _pprintColumn;
    }

    /// <summary>Emit a fill/linear pprint newline. When XP buffering is active, defers the decision.</summary>
    public static void PprintConditionalNewline(TextWriter writer, string kind)
    {
        if (!_pprintActive) return;

        // XP buffering: defer ALL conditional newlines (LINEAR, FILL, MISER)
        // Must check buffering BEFORE miser mode, because miser mode uses
        // pre-break column which may change after outer block breaks resolve.
        if (_pprintBuffering && writer is StringWriter sw)
        {
            // Safety: if buffer is too large, abandon XP buffering to prevent OOM.
            // The threshold is margin * PprintBufferMarginMultiplier. A multiplier of 100
            // means we allow up to 100 lines worth of buffering before giving up.
            // This is an arbitrary but generous limit; legitimate pretty-printed output
            // rarely exceeds a few lines, so this catches runaway buffering (e.g. circular
            // structures or deeply nested output) before it causes memory exhaustion.
            int bufSize = sw.GetStringBuilder().Length - _pprintBufferStart;
            if (bufSize > GetPrintRightMargin() * PprintBufferMarginMultiplier)
            {
                _pprintBuffering = false;
                _pprintDeferredNewlines = null;
                // Fall through to non-buffered handling below
            }
            else
            {
                _pprintDeferredNewlines?.Add((sw.GetStringBuilder().Length, kind, _pprintBlockColumn, _pprintIndent, _pprintPerLinePrefix, 0));
                return;
            }
        }

        // Non-buffered: In miser mode, FILL and MISER become mandatory
        if ((kind == "FILL" || kind == "MISER") && IsMiserMode())
        {
            PprintMandatoryNewline(writer);
            return;
        }

        // Non-buffered fallback (for non-StringWriter streams)
        if (kind == "FILL")
        {
            int sectionWidth = _pprintColumn - _pprintColumnAtLastFillCheck;
            if (sectionWidth < 1) sectionWidth = 1;
            if (_pprintColumn + sectionWidth > GetPrintRightMargin())
            {
                _pprintPendingFillBreak = true;
            }
            _pprintColumnAtLastFillCheck = _pprintColumn;
        }
        else if (kind == "LINEAR")
        {
            if (_pprintColumn >= GetPrintRightMargin())
            {
                PprintTrimTrailingWhitespace(writer);
                writer.Write('\n');
                int col = 0;
                if (_pprintPerLinePrefix != null)
                {
                    writer.Write(_pprintPerLinePrefix);
                    col = _pprintPerLinePrefix.Length;
                }
                if (_pprintIndent > col)
                    writer.Write(new string(' ', _pprintIndent - col));
                _pprintColumn = Math.Max(col, _pprintIndent);
            }
        }
    }

    /// <summary>Process the XP buffer at outermost block end. Handles nested blocks.</summary>
    private static void ProcessXpBuffer(StringWriter sw)
    {
        var sb = sw.GetStringBuilder();
        var tokens = _pprintDeferredNewlines;
        if (tokens == null || tokens.Count == 0) return;

        int margin = GetPrintRightMargin();
        string content = sb.ToString(_pprintBufferStart, sb.Length - _pprintBufferStart);

        // Process the token stream, handling nested blocks recursively
        var result = new System.Text.StringBuilder();
        int col = ProcessXpBlock(content, tokens, 0, tokens.Count, _pprintBufferStart, margin, result);

        // Replace content in StringWriter
        sb.Length = _pprintBufferStart;
        sb.Append(result);

        _pprintColumn = col;
    }

    /// <summary>Process a single block within the XP buffer. Returns final column.</summary>
    /// <param name="content">Full buffer content</param>
    /// <param name="tokens">All tokens</param>
    /// <param name="tokenStart">Index of BLOCK_START token for this block</param>
    /// <param name="tokenEnd">Index past BLOCK_END token for this block</param>
    /// <param name="bufStart">Buffer offset (to convert token positions)</param>
    /// <param name="margin">Right margin</param>
    /// <param name="result">Output StringBuilder</param>
    /// <returns>Column position after output</returns>
    private static int ProcessXpBlock(string content, List<(int pos, string kind, int blockCol, int indent, string? plp, int suffixLen)> tokens,
        int tokenStart, int tokenEnd, int bufStart, int margin, System.Text.StringBuilder result, int colOverride = -1)
    {
        // Find the BLOCK_START token to get block parameters
        if (tokenStart >= tokenEnd) return 0;
        var startToken = tokens[tokenStart];
        if (startToken.kind != "BLOCK_START") return 0;

        int blockCol = colOverride >= 0 ? colOverride : startToken.blockCol;
        int indent = colOverride >= 0 ? colOverride : startToken.indent;
        // When colOverride shifts the block, compute offset to adjust token indents
        int colShift = colOverride >= 0 ? colOverride - startToken.blockCol : 0;
        string? plp = startToken.plp;

        // Collect this block's own tokens (skip inner blocks)
        // Own tokens are those at nesting level 0 relative to this block
        // (unused, replaced by ownTokens2 below)
        int nestLevel = 0;
        int contentStart = startToken.pos - bufStart; // content position where block starts

        // Find content end (BLOCK_END position)
        int contentEnd = content.Length;
        int blockEndTokenIdx = -1;
        {
            int depth = 0;
            for (int i = tokenStart; i < tokenEnd; i++)
            {
                if (tokens[i].kind == "BLOCK_START") depth++;
                else if (tokens[i].kind == "BLOCK_END")
                {
                    depth--;
                    if (depth == 0)
                    {
                        contentEnd = tokens[i].pos - bufStart;
                        blockEndTokenIdx = i;
                        break;
                    }
                }
            }
        }

        string blockContent = content[contentStart..contentEnd];

        // Get suffix length from BLOCK_START token (set via PprintSetBlockSuffix)
        int suffixLen = startToken.suffixLen;
        // For inner blocks, also check content after BLOCK_END for suffix chars
        if (suffixLen == 0 && contentEnd < content.Length)
        {
            for (int si = contentEnd; si < content.Length; si++)
            {
                char c = content[si];
                if (c == ' ' || c == '\n') break;
                suffixLen++;
            }
        }

        // Check if this block fits on one line (including suffix)
        bool hasHardNewline = blockContent.Contains('\n');
        bool blockFits = !hasHardNewline && (blockCol + blockContent.Length + suffixLen <= margin);

        if (blockFits)
        {
            // Block fits: output as-is, no conditional newlines fire
            result.Append(blockContent);
            return blockCol + blockContent.Length;
        }

        // Check if this block is in miser mode (using resolved blockCol)
        bool blockMiserMode = false;
        {
            var miserSym = Startup.Sym("*PRINT-MISER-WIDTH*");
            var miserVal = DynamicBindings.TryGet(miserSym, out var mv) ? mv : miserSym.Value;
            if (miserVal is Fixnum miserWidth)
            {
                int availWidth = margin - blockCol;
                blockMiserMode = (availWidth <= (int)miserWidth.Value);
            }
        }

        // Block doesn't fit: process tokens
        // Collect own-level tokens (excluding inner block contents)
        // Store (adjustedPos, kind, indent, plp, tokenIndex) where tokenIndex is for inner blocks
        var ownTokens2 = new List<(int adjustedPos, string kind, int tokIndent, string? tokPlp, int tokenIdx, int tokenEndIdx)>();
        nestLevel = 0;
        for (int i = tokenStart + 1; i < tokenEnd; i++)
        {
            var tok = tokens[i];
            if (tok.kind == "BLOCK_START")
            {
                if (nestLevel == 0)
                {
                    // Find matching BLOCK_END
                    int innerDepth = 1;
                    int j = i + 1;
                    while (j < tokenEnd && innerDepth > 0)
                    {
                        if (tokens[j].kind == "BLOCK_START") innerDepth++;
                        else if (tokens[j].kind == "BLOCK_END") innerDepth--;
                        j++;
                    }
                    // Record inner block with its token range
                    ownTokens2.Add((tok.pos - bufStart, "INNER_BLOCK", 0, null, i, j));
                    i = j - 1; // skip past inner block (loop will increment i)
                }
                else
                {
                    nestLevel++;
                }
            }
            else if (tok.kind == "BLOCK_END")
            {
                if (nestLevel == 0) break; // end of our block
                nestLevel--;
            }
            else if (nestLevel == 0)
            {
                // Own-level conditional newline — adjust indent by colShift
                ownTokens2.Add((tok.pos - bufStart, tok.kind, Math.Max(0, tok.indent + colShift), tok.plp, -1, -1));
            }
        }

        // Output content with breaks at own-level tokens
        int col = blockCol;
        int lastPos = contentStart;
        bool innerBlockBroke = false; // Set when an inner block produces multi-line output

        for (int ti = 0; ti < ownTokens2.Count; ti++)
        {
            var (adjPos, kind, tokIndent, tokPlp, tokenIdx, tokenEndIdx) = ownTokens2[ti];
            if (adjPos < lastPos || adjPos > contentEnd) continue;

            string segment = content[lastPos..adjPos];
            bool doBreak = false;

            if (kind == "LINEAR")
            {
                doBreak = true;
            }
            else if (kind == "MISER")
            {
                // MISER: break only if this block is in miser mode
                doBreak = blockMiserMode;
            }
            else if (kind == "FILL")
            {
                // In miser mode, FILL becomes mandatory (like LINEAR)
                if (blockMiserMode)
                {
                    doBreak = true;
                }
                else
                {
                    // FILL: break if segment + next section would exceed margin.
                    // Next section = everything from here to the next newline token
                    // (FILL/LINEAR/MISER), skipping INNER_BLOCK tokens since their
                    // content is part of the section.
                    int segWidth = MeasureSegmentWidth(segment);
                    int tempCol = col + segWidth;
                    // If a preceding inner block produced multi-line output,
                    // break here to maintain visual grouping.
                    if (innerBlockBroke)
                    {
                        doBreak = true;
                    }
                    else
                    {
                        int nextEnd = contentEnd;
                        for (int ni = ti + 1; ni < ownTokens2.Count; ni++)
                        {
                            if (ownTokens2[ni].kind != "INNER_BLOCK")
                            {
                                nextEnd = ownTokens2[ni].adjustedPos;
                                break;
                            }
                        }
                        int nextWidth = MeasureSegmentWidth(content[adjPos..nextEnd]);
                        // If this is the last section (extends to block end), include suffix
                        if (nextEnd == contentEnd) nextWidth += suffixLen;
                        doBreak = (tempCol + nextWidth > margin);
                    }
                }
            }
            else if (kind == "INNER_BLOCK")
            {
                // Inner block: output preceding segment, then process block recursively
                result.Append(segment);
                col = CountColumnAfter(segment, col);

                // Process inner block recursively with current actual column
                var innerResult = new System.Text.StringBuilder();
                col = ProcessXpBlock(content, tokens, tokenIdx, tokenEndIdx, bufStart, margin, innerResult, col);
                // Track if inner block produced multi-line output
                for (int ri = 0; ri < innerResult.Length; ri++)
                {
                    if (innerResult[ri] == '\n') { innerBlockBroke = true; break; }
                }
                result.Append(innerResult);
                // Skip past inner block content
                int innerContentEnd = tokens[tokenEndIdx - 1].pos - bufStart;
                lastPos = innerContentEnd;
                continue;
            }

            if (doBreak)
            {
                innerBlockBroke = false; // Reset after breaking
                // Trim trailing whitespace from segment
                int trimEnd = segment.Length;
                while (trimEnd > 0 && segment[trimEnd - 1] == ' ') trimEnd--;
                result.Append(segment[..trimEnd]);
                // Also trim trailing spaces already in result (e.g. from inner blocks)
                while (result.Length > 0 && result[result.Length - 1] == ' ')
                    result.Length--;
                result.Append('\n');
                // If remaining content starts with \n (mandatory newline), skip indent—
                // the mandatory newline will immediately override it, and the indent
                // would produce orphaned whitespace on an otherwise empty line.
                bool remainingStartsNewline = (adjPos < contentEnd && content[adjPos] == '\n');
                if (!remainingStartsNewline)
                {
                    string? breakPlp = tokPlp;
                    int breakIndent = tokIndent;
                    int breakPlpLen = breakPlp?.Length ?? 0;
                    if (breakPlp != null) result.Append(breakPlp);
                    int indentSpaces = Math.Max(0, breakIndent - breakPlpLen);
                    if (indentSpaces > 0) result.Append(new string(' ', indentSpaces));
                    col = Math.Max(breakPlpLen, breakIndent);
                }
                else
                {
                    col = 0; // will be updated by the mandatory newline
                }
            }
            else
            {
                result.Append(segment);
                col = CountColumnAfter(segment, col);
            }

            lastPos = adjPos;
        }

        // Append remaining content
        if (lastPos < contentEnd)
        {
            result.Append(content[lastPos..contentEnd]);
            col = CountColumnAfter(content[lastPos..contentEnd], col);
        }

        return col;
    }

    /// <summary>Count column after writing text, accounting for newlines.</summary>
    private static int CountColumnAfter(string text, int startCol)
    {
        int col = startCol;
        for (int i = 0; i < text.Length; i++)
        {
            if (text[i] == '\n') col = 0;
            else col++;
        }
        return col;
    }

    /// <summary>Measure the width of a segment (characters, no newlines counted).</summary>
    private static int MeasureSegmentWidth(string segment)
    {
        int width = 0;
        for (int i = 0; i < segment.Length; i++)
        {
            if (segment[i] == '\n') width = 0;
            else width++;
        }
        return width;
    }

    /// <summary>Check if current section is in miser mode.</summary>
    public static bool IsMiserMode()
    {
        var miserSym = Startup.Sym("*PRINT-MISER-WIDTH*");
        var miserVal = DynamicBindings.TryGet(miserSym, out var mv) ? mv : miserSym.Value;
        if (miserVal is Fixnum miserWidth)
        {
            int rightMargin = GetPrintRightMargin();
            int availWidth = rightMargin - _pprintBlockColumn;
            return availWidth <= (int)miserWidth.Value;
        }
        return false;
    }

    /// <summary>Set pprint indentation level.</summary>
    public static void PprintSetIndent(string relativeTo, int n)
    {
        // Per CLHS 22.2.1.1: in miser mode, all indentations revert to body indent (blockColumn)
        if (IsMiserMode())
        {
            _pprintIndent = _pprintBlockColumn;
            return;
        }
        if (relativeTo == "BLOCK")
            _pprintIndent = Math.Max(0, _pprintBlockColumn + n);
        else if (relativeTo == "CURRENT")
            _pprintIndent = Math.Max(0, _pprintColumn + n);
    }

    /// <summary>Implement pprint-tab: emit spaces to reach a tab stop.</summary>
    public static void PprintTab(TextWriter writer, string kind, int colnum, int colinc)
    {
        if (!_pprintActive) return;
        int col = _pprintColumn;
        int target;
        if (kind == "LINE")
        {
            if (col < colnum)
                target = colnum;
            else if (colinc <= 0)
                target = col;
            else
            {
                // Next position >= col that is colnum + k*colinc
                int k = (col - colnum + colinc) / colinc;
                target = colnum + k * colinc;
            }
        }
        else if (kind == "SECTION")
        {
            int sectionCol = col - _pprintBlockColumn;
            if (sectionCol < colnum)
                target = _pprintBlockColumn + colnum;
            else if (colinc <= 0)
                target = col;
            else
            {
                int k = (sectionCol - colnum + colinc) / colinc;
                target = _pprintBlockColumn + colnum + k * colinc;
            }
        }
        else if (kind == "LINE-RELATIVE")
        {
            int newcol = col + colnum;
            if (colinc <= 0)
                target = newcol;
            else
            {
                int remainder = newcol % colinc;
                target = remainder == 0 ? newcol : newcol + colinc - remainder;
            }
        }
        else // SECTION-RELATIVE
        {
            int sectionCol = col - _pprintBlockColumn;
            int newSectionCol = sectionCol + colnum;
            if (colinc <= 0)
                target = _pprintBlockColumn + newSectionCol;
            else
            {
                int remainder = newSectionCol % colinc;
                target = _pprintBlockColumn + (remainder == 0 ? newSectionCol : newSectionCol + colinc - remainder);
            }
        }
        int spaces = target - col;
        if (spaces > 0)
        {
            writer.Write(new string(' ', spaces));
            _pprintColumn = target;
        }
    }

    // --- File I/O ---

    /// Get number of bytes per element for binary streams. Returns 1 for standard byte streams.
    private static int GetBinaryByteWidth(LispObject stream)
    {
        // Resolve through composite streams to find the underlying file stream's element-type
        if (stream is LispTwoWayStream tw) return GetBinaryByteWidth(tw.InputStream);
        if (stream is LispSynonymStream ss) return GetBinaryByteWidth(ss.Symbol.Value!);
        if (stream is LispEchoStream es) return GetBinaryByteWidth(es.InputStream);
        if (stream is LispConcatenatedStream cs && cs.CurrentIndex < cs.Streams.Length)
            return GetBinaryByteWidth(cs.Streams[cs.CurrentIndex]);
        if (stream is LispBroadcastStream bs && bs.Streams.Length > 0)
            return GetBinaryByteWidth(bs.Streams[^1]);
        LispObject? et = null;
        if (stream is LispFileStream fs) et = fs.ElementType;
        else if (stream is LispStream ls) et = ls.ElementType;
        if (et is Cons c && c.Car is Symbol sym && sym.Name == "UNSIGNED-BYTE" && c.Cdr is Cons c2 && c2.Car is Fixnum bits)
            return Math.Max(1, ((int)bits.Value + 7) / 8);
        return 1;
    }

    public static LispObject OpenFile(LispObject path, LispObject[] options)
    {
        string filePath = ResolvePhysicalPath(path);

        // Resolve the original pathname object for storing on the stream
        LispPathname? originalPathname = null;
        if (path is LispPathname pn) originalPathname = pn;
        else if (path is LispString ps)
            originalPathname = IsLogicalPathnameString(ps.Value) ? LispLogicalPathname.FromLogicalString(ps.Value) : LispPathname.FromString(ps.Value);
        else if (path is LispVector pv && pv.IsCharVector)
        {
            var pvs = pv.ToCharString();
            originalPathname = IsLogicalPathnameString(pvs) ? LispLogicalPathname.FromLogicalString(pvs) : LispPathname.FromString(pvs);
        }
        else if (path is LispFileStream pfs)
            originalPathname = pfs.OriginalPathname ?? LispPathname.FromString(pfs.FilePath);

        string direction = "INPUT";
        string ifExists = "DEFAULT";
        string ifDoesNotExist = "DEFAULT";
        LispObject? elementType = null; // null means CHARACTER

        for (int j = 0; j < options.Length - 1; j += 2)
        {
            string key = options[j] switch
            {
                Symbol s => s.Name,
                _ => options[j].ToString()
            };
            if (key == "ELEMENT-TYPE")
            {
                var etVal = options[j + 1];
                // :element-type DEFAULT means CHARACTER per CLHS
                if (etVal is Symbol etSym && etSym.Name == "DEFAULT")
                    elementType = null;
                else
                    elementType = etVal;
                continue;
            }
            string val = options[j + 1] switch
            {
                Symbol s => s.Name,
                Nil => "NIL",
                _ => options[j + 1].ToString()
            };
            switch (key)
            {
                case "DIRECTION": direction = val; break;
                case "IF-EXISTS": ifExists = val; break;
                case "IF-DOES-NOT-EXIST": ifDoesNotExist = val; break;
                case "EXTERNAL-FORMAT": break; // accepted but ignored (implementation uses UTF-8)
                default:
                    throw new LispErrorException(new LispProgramError($"OPEN: unknown keyword argument :{key}"));
            }
        }

        // Resolve default if-exists based on direction (CL spec: implementation-defined for output/io)
        if (ifExists == "DEFAULT")
        {
            if (direction == "OUTPUT" || direction == "IO")
                ifExists = "SUPERSEDE"; // implementation choice: supersede is safest default
            else
                ifExists = "ERROR"; // not used for input/probe, but set a safe value
        }

        // Resolve default if-does-not-exist based on direction and if-exists
        if (ifDoesNotExist == "DEFAULT")
        {
            if (direction == "INPUT")
                ifDoesNotExist = "ERROR";
            else if (direction == "OUTPUT" || direction == "IO")
            {
                if (ifExists == "OVERWRITE" || ifExists == "APPEND")
                    ifDoesNotExist = "ERROR";
                else
                    ifDoesNotExist = "CREATE";
            }
            else // PROBE
                ifDoesNotExist = "NIL";
        }

        switch (direction)
        {
            case "INPUT":
            {
                if (!File.Exists(filePath))
                {
                    if (ifDoesNotExist == "NIL") return Nil.Instance;
                    var err = new LispError($"File not found: {filePath}");
                    err.ConditionTypeName = "FILE-ERROR";
                    err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
                    throw new LispErrorException(err);
                }
                var netFsIn = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                var reader = new StreamReader(netFsIn);
                var fs = new LispFileStream(reader, filePath);
                fs.ElementType = elementType;
                fs.OriginalPathname = originalPathname;
                return fs;
            }
            case "OUTPUT":
            {
                if (File.Exists(filePath))
                {
                    switch (ifExists)
                    {
                        case "SUPERSEDE":
                        case "NEW-VERSION":
                        case "RENAME":
                        case "RENAME-AND-DELETE":
                            break; // overwrite (truncate below)
                        case "OVERWRITE":
                        {
                            var netFs = new FileStream(filePath, FileMode.Open, FileAccess.Write, FileShare.ReadWrite);
                            var writer = new StreamWriter(netFs);
                            var ofs = new LispFileStream(writer, filePath);
                            ofs.ElementType = elementType;
                            ofs.OriginalPathname = originalPathname;
                            return ofs;
                        }
                        case "APPEND":
                        {
                            var netFsApp = new FileStream(filePath, FileMode.Open, FileAccess.Write, FileShare.ReadWrite);
                            netFsApp.Seek(0, SeekOrigin.End);
                            var writer = new StreamWriter(netFsApp);
                            var ofs = new LispFileStream(writer, filePath);
                            ofs.ElementType = elementType;
                            ofs.OriginalPathname = originalPathname;
                            return ofs;
                        }
                        case "NIL": return Nil.Instance;
                        case "ERROR":
                        {
                            var err = new LispError($"File already exists: {filePath}");
                            err.ConditionTypeName = "FILE-ERROR";
                            err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
                            throw new LispErrorException(err);
                        }
                        default: break;
                    }
                }
                else
                {
                    if (ifDoesNotExist == "NIL")
                        return Nil.Instance;
                    else if (ifDoesNotExist == "ERROR")
                    {
                        var err = new LispError($"File does not exist: {filePath}");
                        err.ConditionTypeName = "FILE-ERROR";
                        err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
                        throw new LispErrorException(err);
                    }
                }
                {
                    var netFsOut = new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.ReadWrite);
                    var writer = new StreamWriter(netFsOut);
                    var fs = new LispFileStream(writer, filePath);
                    fs.ElementType = elementType;
                    fs.OriginalPathname = originalPathname;
                    return fs;
                }
            }
            case "IO":
            {
                bool fileExists = File.Exists(filePath);
                if (fileExists)
                {
                    switch (ifExists)
                    {
                        case "NIL": return Nil.Instance;
                        case "ERROR":
                        {
                            var err = new LispError($"File already exists: {filePath}");
                            err.ConditionTypeName = "FILE-ERROR";
                            err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
                            throw new LispErrorException(err);
                        }
                        case "SUPERSEDE":
                        case "NEW-VERSION":
                        case "RENAME":
                        case "RENAME-AND-DELETE":
                        {
                            // Truncate instead of delete+recreate to avoid file lock issues on Windows
                            var netFsTrunc = new FileStream(filePath, FileMode.Truncate, FileAccess.ReadWrite, FileShare.ReadWrite);
                            var readerTrunc = new StreamReader(netFsTrunc);
                            var writerTrunc = new StreamWriter(netFsTrunc) { AutoFlush = true };
                            var fsTrunc = new LispFileStream(readerTrunc, writerTrunc, filePath);
                            fsTrunc.ElementType = elementType;
                            fsTrunc.OriginalPathname = originalPathname;
                            return fsTrunc;
                        }
                        case "OVERWRITE":
                            break; // Open existing, don't truncate
                        case "APPEND":
                        {
                            // Open for append + read
                            var netFs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite);
                            netFs.Seek(0, SeekOrigin.End); // Position at end for append
                            var reader = new StreamReader(netFs);
                            var writer = new StreamWriter(netFs) { AutoFlush = true };
                            var afs = new LispFileStream(reader, writer, filePath);
                            afs.ElementType = elementType;
                            afs.OriginalPathname = originalPathname;
                            return afs;
                        }
                        default: break;
                    }
                }
                else
                {
                    switch (ifDoesNotExist)
                    {
                        case "NIL": return Nil.Instance;
                        case "ERROR":
                        {
                            var err = new LispError($"File does not exist: {filePath}");
                            err.ConditionTypeName = "FILE-ERROR";
                            err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
                            throw new LispErrorException(err);
                        }
                        default: break; // CREATE - fall through
                    }
                }
                {
                    var netFs = new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.ReadWrite);
                    var reader = new StreamReader(netFs);
                    var writer = new StreamWriter(netFs) { AutoFlush = true };
                    var fs = new LispFileStream(reader, writer, filePath);
                    fs.ElementType = elementType;
                    fs.OriginalPathname = originalPathname;
                    return fs;
                }
            }
            case "PROBE":
            {
                if (!File.Exists(filePath))
                {
                    switch (ifDoesNotExist)
                    {
                        case "NIL": return Nil.Instance;
                        case "ERROR":
                        {
                            var err = new LispError($"File does not exist: {filePath}");
                            err.ConditionTypeName = "FILE-ERROR";
                            err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
                            throw new LispErrorException(err);
                        }
                        case "CREATE":
                        {
                            // Create the file
                            File.Create(filePath).Close();
                            break;
                        }
                        default: return Nil.Instance;
                    }
                }
                var probeStream = new LispFileStream(filePath);
                probeStream.IsClosed = true;
                probeStream.ElementType = elementType;
                probeStream.OriginalPathname = originalPathname;
                return probeStream;
            }
            default:
                throw new LispErrorException(new LispError($"OPEN: invalid direction {direction}"));
        }
    }

    public static LispObject CloseStream(LispObject stream)
    {
        if (stream is LispStream ls && ls.IsClosed) return T.Instance;

        if (stream is LispFileStream fs) { fs.Close(); return T.Instance; }
        if (stream is LispInputStream ins) { ins.IsClosed = true; try { ins.Reader.Close(); } catch (ObjectDisposedException) { } return T.Instance; }
        if (stream is LispOutputStream outs) { outs.IsClosed = true; try { outs.Writer.Close(); } catch (ObjectDisposedException) { } return T.Instance; }
        if (stream is LispBidirectionalStream bidi)
        {
            bidi.IsClosed = true;
            try { bidi.Reader.Close(); } catch (ObjectDisposedException) { }
            try { bidi.Writer.Close(); } catch (ObjectDisposedException) { }
            return T.Instance;
        }
        if (stream is LispStream ls2) ls2.IsClosed = true;
        return T.Instance;
    }

    public static LispObject OpenStreamP(LispObject stream)
    {
        if (stream is not LispStream ls)
            throw new LispErrorException(new LispTypeError("OPEN-STREAM-P: not a stream", stream, Startup.Sym("STREAM")));
        return ls.IsClosed ? Nil.Instance : T.Instance;
    }

    public static LispObject ReadCharNoHang(LispObject streamObj, LispObject eofErrorP, LispObject eofValue)
    {
        var lispStream = ResolveLispStream(streamObj);
        if (lispStream.UnreadCharValue != -1)
        {
            char ch = (char)lispStream.UnreadCharValue;
            lispStream.UnreadCharValue = -1;
            return LispChar.Make(ch);
        }
        TextReader reader = GetTextReader(lispStream);
        // For string streams and most .NET streams, Peek tells us if data is available
        int p = reader.Peek();
        if (p == -1)
        {
            if (eofErrorP is not Nil)
                throw new LispErrorException(MakeEndOfFileError(streamObj));
            return eofValue;
        }
        int c = reader.Read();
        // Echo if reading from echo stream
        var echoStream = FindEchoStream(streamObj);
        if (echoStream != null && c != -1)
        {
            TextWriter echoWriter = GetTextWriter(echoStream.OutputStream);
            echoWriter.Write((char)c);
        }
        return LispChar.Make((char)c);
    }

    public static LispObject Listen(LispObject stream)
    {
        // Resolve stream designators
        var resolved = stream;
        if (resolved is LispSynonymStream syn)
            resolved = DynamicBindings.TryGet(syn.Symbol, out var val) ? val : syn.Symbol.Value;
        if (resolved is LispTwoWayStream tw)
            resolved = tw.InputStream;
        if (resolved is LispEchoStream echo)
            resolved = echo.InputStream;
        // Check unread-char buffer first
        if (resolved is LispStream ls && ls.UnreadCharValue != -1)
            return T.Instance;
        // Check concatenated stream: advance past exhausted component streams
        if (resolved is LispConcatenatedStream cs)
        {
            while (cs.CurrentIndex < cs.Streams.Length)
            {
                var result = Listen(cs.Streams[cs.CurrentIndex]);
                if (result is not Nil) return result;
                cs.CurrentIndex++;
            }
            return Nil.Instance;
        }
        TextReader reader = GetTextReader(resolved);
        int p = reader.Peek();
        return p == -1 ? Nil.Instance : T.Instance;
    }

    public static LispObject ClearInput(LispObject stream)
    {
        // Validate: must be a stream designator (stream, nil, or t)
        if (stream is not (LispStream or Nil or T))
            throw new LispErrorException(new LispTypeError("CLEAR-INPUT: not a stream designator", stream, Startup.Sym("STREAM")));
        // For most streams, this is a no-op in our implementation
        return Nil.Instance;
    }

    private static bool IsIntegerElementType(LispObject? et)
    {
        if (et == null) return false;
        // Quick path: standard binary element types
        if (et is Symbol s2 && (s2.Name is "UNSIGNED-BYTE" or "SIGNED-BYTE" or "BIT" or "INTEGER")) return true;
        if (et is Cons c && c.Car is Symbol sym &&
            (sym.Name is "UNSIGNED-BYTE" or "SIGNED-BYTE" or "INTEGER" or "MOD")) return true;
        // General: subtype of INTEGER but not CHARACTER
        if (et is Nil || et is T) return false;
        try { return Runtime.IsTruthy(Runtime.Subtypep(et, Startup.Sym("INTEGER"))); } catch { return false; }
    }

    private static bool IsBinaryOutputStream(LispObject s) => s switch {
        LispBinaryStream => true,
        LispFileStream fs => fs.IsOutput && IsIntegerElementType(fs.ElementType),
        LispBroadcastStream bs => bs.Streams.Length > 0 && IsBinaryOutputStream(bs.Streams[^1]),
        LispTwoWayStream tw => IsBinaryOutputStream(tw.OutputStream),
        LispEchoStream es => IsBinaryOutputStream(es.OutputStream),
        LispSynonymStream ss => IsBinaryOutputStream(DynamicBindings.Get(ss.Symbol)),
        _ => false
    };

    public static LispObject WriteByte(LispObject byteObj, LispObject stream)
    {
        if (stream is not LispStream)
            throw new LispErrorException(new LispTypeError("WRITE-BYTE: not a stream", stream, Startup.Sym("STREAM")));
        if (!IsBinaryOutputStream(stream))
            throw new LispErrorException(new LispTypeError("WRITE-BYTE: not a binary output stream", stream, Startup.Sym("STREAM")));
        if (byteObj is not (Fixnum or Bignum))
            throw new LispErrorException(new LispTypeError("WRITE-BYTE: not an integer", byteObj));
        int byteWidth = GetBinaryByteWidth(stream);
        if (byteWidth == 1)
        {
            int b = byteObj is Fixnum fi ? (int)fi.Value & 0xFF : 0;
            WriteStreamByte(stream, b);
        }
        else
        {
            byte[] bytes = new byte[byteWidth];
            if (byteObj is Fixnum fi2)
            {
                long val = fi2.Value;
                for (int j = 0; j < byteWidth; j++) { bytes[j] = (byte)(val & 0xFF); val >>= 8; }
            }
            else if (byteObj is Bignum bi)
            {
                var bigBytes = bi.Value.ToByteArray(isUnsigned: true, isBigEndian: false);
                Array.Copy(bigBytes, bytes, Math.Min(bigBytes.Length, byteWidth));
            }
            for (int j = 0; j < byteWidth; j++) WriteStreamByte(stream, bytes[j]);
        }
        return byteObj;
    }

    public static LispObject ReadByte(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("READ-BYTE: requires at least 1 argument"));
        if (args.Length > 3) throw new LispErrorException(new LispProgramError($"READ-BYTE: wrong number of arguments: {args.Length} (expected 1-3)"));
        var stream = args[0];
        if (stream is not LispStream ls)
            throw new LispErrorException(new LispTypeError("READ-BYTE: not a stream", stream, Startup.Sym("STREAM")));
        if (!ls.IsInput)
            throw new LispErrorException(new LispTypeError(
                $"READ-BYTE: {ls} is not an input stream", stream, Startup.Sym("STREAM")));
        bool eofErrorP = args.Length < 2 || IsTruthy(args[1]);
        var eofValue = args.Length >= 3 ? args[2] : Nil.Instance;

        int byteWidth = GetBinaryByteWidth(stream);
        if (byteWidth == 1)
        {
            int b = ReadStreamByte(stream);
            if (b == -1)
            {
                if (eofErrorP)
                    throw new LispErrorException(MakeEndOfFileError(stream));
                return eofValue;
            }
            return Fixnum.Make(b);
        }
        else
        {
            byte[] bytes = new byte[byteWidth];
            int firstByte = ReadStreamByte(stream);
            if (firstByte == -1)
            {
                if (eofErrorP)
                    throw new LispErrorException(MakeEndOfFileError(stream));
                return eofValue;
            }
            bytes[0] = (byte)firstByte;
            for (int j = 1; j < byteWidth; j++)
            {
                int b = ReadStreamByte(stream);
                if (b == -1) break;
                bytes[j] = (byte)b;
            }
            if (byteWidth <= 8)
            {
                long val = 0;
                for (int j = byteWidth - 1; j >= 0; j--)
                    val = (val << 8) | bytes[j];
                return Fixnum.Make(val);
            }
            else
            {
                var bigVal = new BigInteger(bytes, isUnsigned: true, isBigEndian: false);
                return Bignum.MakeInteger(bigVal);
            }
        }
    }

    public static LispObject FindAllSymbols(LispObject name)
    {
        string symName;
        if (name is LispString s) symName = s.Value;
        else if (name is Symbol sym) symName = sym.Name;
        else if (name is LispChar ch) symName = ch.Value.ToString();
        else if (name is LispVector v && v.IsCharVector) symName = v.ToCharString();
        else symName = AsStringDesignator(name, "FIND-ALL-SYMBOLS");

        LispObject result = Nil.Instance;
        var seen = new HashSet<Symbol>();
        foreach (var pkg in Package.AllPackages)
        {
            var (found, status) = pkg.FindSymbol(symName);
            if (status == SymbolStatus.Internal || status == SymbolStatus.External)
            {
                if (seen.Add(found))
                {
                    LispObject canonical = ReferenceEquals(found, Startup.NIL_SYM) ? Nil.Instance
                                         : ReferenceEquals(found, Startup.T_SYM) ? (LispObject)T.Instance
                                         : found;
                    result = new Cons(canonical, result);
                }
            }
        }
        return result;
    }

    public static LispObject UpgradedComplexPartType(LispObject[] args)
    {
        // (upgraded-complex-part-type typespec &optional environment)
        // Per CLHS, returns the part type of the most specialized complex number type
        // that can represent components of the given type.
        // dotcl supports SINGLE-FLOAT and DOUBLE-FLOAT complex numbers.
        CheckArityMin("UPGRADED-COMPLEX-PART-TYPE", args, 1);
        CheckArityMax("UPGRADED-COMPLEX-PART-TYPE", args, 2);
        // We ignore the environment argument (args[1] if present)
        var typeName = TypeSpecName(args[0]);
        // For compound type specifiers like (SINGLE-FLOAT 0.0 1.0), extract the head name
        if (typeName == null && args[0] is Cons c && c.Car is Symbol head)
            typeName = head.Name;
        var result = typeName switch
        {
            "NIL" => "NIL", // bottom type: subtype of all types
            "SINGLE-FLOAT" or "SHORT-FLOAT" => "SINGLE-FLOAT",
            "DOUBLE-FLOAT" or "LONG-FLOAT" => "DOUBLE-FLOAT",
            _ => "REAL" // INTEGER, RATIONAL, FIXNUM, REAL, T, etc. upgrade to REAL
        };
        return Startup.Sym(result);
    }

    /// <summary>Check if a stream has binary element type (not character).</summary>
    private static bool IsBinaryStream(LispObject stream)
    {
        // Resolve through composite streams
        if (stream is LispTwoWayStream tw) return IsBinaryStream(tw.InputStream);
        if (stream is LispEchoStream es) return IsBinaryStream(es.InputStream);
        if (stream is LispSynonymStream ss) return IsBinaryStream(ss.Symbol.Value!);
        if (stream is LispConcatenatedStream cs && cs.CurrentIndex < cs.Streams.Length)
            return IsBinaryStream(cs.Streams[cs.CurrentIndex]);
        if (stream is LispStream ls && ls.ElementType != null)
        {
            // ElementType is a LispObject; CHARACTER/BASE-CHAR/STANDARD-CHAR are character streams
            if (ls.ElementType is Symbol sym)
                return sym.Name is not ("CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR");
            // Compound type like (UNSIGNED-BYTE 8) is binary
            return true;
        }
        return false;
    }

    /// <summary>Read a byte from a stream's underlying byte stream. Returns -1 on EOF.</summary>
    private static int ReadStreamByte(LispObject stream)
    {
        if (stream is LispBinaryStream bs)
            return bs.BaseStream.ReadByte();
        if (stream is LispFileStream fs && fs.InputReader is StreamReader sr)
            return sr.BaseStream.ReadByte();
        if (stream is LispConcatenatedStream cs)
        {
            while (cs.CurrentIndex < cs.Streams.Length)
            {
                int b = ReadStreamByte(cs.Streams[cs.CurrentIndex]);
                if (b >= 0) return b;
                cs.CurrentIndex++;
            }
            return -1;
        }
        if (stream is LispEchoStream es)
        {
            int b = ReadStreamByte(es.InputStream);
            if (b >= 0) WriteStreamByte(es.OutputStream, b);
            return b;
        }
        if (stream is LispSynonymStream ss)
            return ReadStreamByte(ss.Symbol.Value!);
        if (stream is LispTwoWayStream tw)
            return ReadStreamByte(tw.InputStream);
        return -1;
    }

    /// <summary>Write a byte to a stream's underlying byte stream.</summary>
    private static void WriteStreamByte(LispObject stream, int b)
    {
        if (stream is LispBinaryStream bin)
        {
            bin.BaseStream.WriteByte((byte)b);
            return;
        }
        if (stream is LispFileStream fs && fs.OutputWriter is StreamWriter sw)
        {
            sw.BaseStream.WriteByte((byte)b);
            sw.BaseStream.Flush();
        }
        else if (stream is LispBroadcastStream bs)
        {
            foreach (var s in bs.Streams) WriteStreamByte(s, b);
        }
        else if (stream is LispEchoStream es)
            WriteStreamByte(es.OutputStream, b);
        else if (stream is LispSynonymStream ss)
            WriteStreamByte(ss.Symbol.Value!, b);
        else if (stream is LispTwoWayStream tw)
            WriteStreamByte(tw.OutputStream, b);
    }

    private static void ValidateSequenceKeywords(string funcName, LispObject[] args, int kwStart, ref int start, ref int end)
    {
        int kwCount = args.Length - kwStart;
        if (kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError($"{funcName}: odd number of keyword arguments"));
        bool allowOtherKeys = false;
        bool hasUnknownKeys = false;
        // First pass: check for :allow-other-keys
        for (int i = kwStart; i < args.Length; i += 2)
        {
            string key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            if (key == "ALLOW-OTHER-KEYS" && args[i + 1] is not Nil)
                allowOtherKeys = true;
        }
        // Second pass: validate all keywords (first occurrence wins per CL spec)
        bool startSeen = false, endSeen = false;
        for (int i = kwStart; i < args.Length; i += 2)
        {
            string key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            if (key == "START")
            {
                if (!startSeen)
                {
                    var val = args[i + 1];
                    if (val is not Fixnum fi || fi.Value < 0)
                        throw new LispErrorException(new LispTypeError($"{funcName}: invalid :START value", val, Startup.Sym("UNSIGNED-BYTE")));
                    start = (int)fi.Value;
                    startSeen = true;
                }
            }
            else if (key == "END")
            {
                if (!endSeen)
                {
                    var val = args[i + 1];
                    if (val is Nil) { /* nil is ok, keep end = -1 */ }
                    else if (val is Fixnum fi && fi.Value >= 0) end = (int)fi.Value;
                    else throw new LispErrorException(new LispTypeError($"{funcName}: invalid :END value", val, Startup.Sym("UNSIGNED-BYTE")));
                    endSeen = true;
                }
            }
            else if (key == "ALLOW-OTHER-KEYS") { /* already handled */ }
            else hasUnknownKeys = true;
        }
        if (hasUnknownKeys && !allowOtherKeys)
            throw new LispErrorException(new LispProgramError($"{funcName}: unrecognized keyword argument"));
    }

    public static LispObject ReadSequence(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("READ-SEQUENCE: requires at least 2 arguments"));
        var seq = args[0];
        var stream = args[1];
        if (stream is LispStream rsls && !rsls.IsInput)
            throw new LispErrorException(new LispTypeError(
                $"READ-SEQUENCE: {rsls} is not an input stream", stream, Startup.Sym("STREAM")));
        // Validate sequence type
        if (seq is not (LispString or LispVector or Cons or Nil))
            throw new LispErrorException(new LispTypeError("READ-SEQUENCE: not a proper sequence", seq, Startup.Sym("VECTOR")));
        if (seq is Cons seqConsR)
        {
            // Check for improper list (dotted pair)
            var tail = seqConsR;
            while (tail.Cdr is Cons next) tail = next;
            if (tail.Cdr is not Nil)
                throw new LispErrorException(new LispTypeError("READ-SEQUENCE: not a proper sequence", seq, Startup.Sym("VECTOR")));
        }
        int start = 0;
        int end = -1;
        // Validate keyword args
        ValidateSequenceKeywords("READ-SEQUENCE", args, 2, ref start, ref end);

        bool binary = IsBinaryStream(stream);
        var echoStream = FindEchoStream(stream);
        TextWriter? echoWriter = echoStream != null ? GetTextWriter(echoStream.OutputStream) : null;

        if (seq is LispString ls)
        {
            if (end < 0) end = ls.Length;
            TextReader reader = GetTextReader(stream);
            int pos = start;
            while (pos < end)
            {
                int ch = reader.Read();
                if (ch == -1) break;
                ls[pos++] = (char)ch;
                echoWriter?.Write((char)ch);
            }
            return Fixnum.Make(pos);
        }
        if (seq is LispVector vec)
        {
            if (end < 0) end = vec.Length;
            int pos = start;
            if (binary)
            {
                int byteWidth = GetBinaryByteWidth(stream);
                while (pos < end)
                {
                    if (byteWidth == 1)
                    {
                        int b = ReadStreamByte(stream);
                        if (b == -1) break;
                        vec.SetElement(pos++, Fixnum.Make(b));
                    }
                    else
                    {
                        byte[] bytes = new byte[byteWidth];
                        int firstByte = ReadStreamByte(stream);
                        if (firstByte == -1) break;
                        bytes[0] = (byte)firstByte;
                        for (int j = 1; j < byteWidth; j++)
                        {
                            int b = ReadStreamByte(stream);
                            if (b == -1) break;
                            bytes[j] = (byte)b;
                        }
                        if (byteWidth <= 8)
                        {
                            long val = 0;
                            for (int j = byteWidth - 1; j >= 0; j--)
                                val = (val << 8) | bytes[j];
                            vec.SetElement(pos++, Fixnum.Make(val));
                        }
                        else
                        {
                            var bigVal = new BigInteger(bytes, isUnsigned: true, isBigEndian: false);
                            vec.SetElement(pos++, Bignum.MakeInteger(bigVal));
                        }
                    }
                }
            }
            else
            {
                TextReader reader = GetTextReader(stream);
                while (pos < end)
                {
                    int ch = reader.Read();
                    if (ch == -1) break;
                    vec.SetElement(pos++, LispChar.Make((char)ch));
                    echoWriter?.Write((char)ch);
                }
            }
            return Fixnum.Make(pos);
        }
        // For cons lists
        if (seq is Cons)
        {
            if (end < 0)
            {
                end = 0;
                var tmp = seq;
                while (tmp is Cons) { end++; tmp = ((Cons)tmp).Cdr; }
            }
            var cell = seq;
            for (int i = 0; i < start && cell is Cons c; i++) cell = c.Cdr;
            int pos = start;
            TextReader consReader = GetTextReader(stream);
            while (pos < end && cell is Cons cc)
            {
                int ch = consReader.Read();
                if (ch == -1) break;
                cc.Car = LispChar.Make((char)ch);
                cell = cc.Cdr;
                pos++;
            }
            return Fixnum.Make(pos);
        }
        return Fixnum.Make(start);
    }

    public static LispObject WriteSequence(LispObject[] args)
    {
        if (args.Length < 2)
            throw new LispErrorException(new LispProgramError("WRITE-SEQUENCE: requires at least 2 arguments"));
        var seq = args[0];
        var stream = args[1];
        // Validate sequence type
        if (seq is not (LispString or LispVector or Cons or Nil))
            throw new LispErrorException(new LispTypeError("WRITE-SEQUENCE: not a proper sequence", seq, Startup.Sym("VECTOR")));
        if (seq is Cons seqConsW)
        {
            var tail = seqConsW;
            while (tail.Cdr is Cons next) tail = next;
            if (tail.Cdr is not Nil)
                throw new LispErrorException(new LispTypeError("WRITE-SEQUENCE: not a proper sequence", seq, Startup.Sym("VECTOR")));
        }
        int start = 0;
        int end = -1;
        // Validate keyword args
        ValidateSequenceKeywords("WRITE-SEQUENCE", args, 2, ref start, ref end);

        // Resolve composite streams
        if (stream is LispTwoWayStream tws) { args[1] = tws.OutputStream; return WriteSequence(args); }
        if (stream is LispSynonymStream syn) { args[1] = DynamicBindings.Get(syn.Symbol); return WriteSequence(args); }
        if (stream is LispEchoStream echoWs) { args[1] = echoWs.OutputStream; return WriteSequence(args); }
        if (stream is LispBroadcastStream bs)
        {
            var origStream = args[1];
            foreach (var s in bs.Streams) { args[1] = s; WriteSequence(args); }
            args[1] = origStream;
            return seq;
        }

        bool binary = IsBinaryStream(stream);

        if (binary)
        {
            int byteWidth = GetBinaryByteWidth(stream);
            // Binary write via BaseStream
            if (seq is LispVector vec)
            {
                if (end < 0) end = vec.Length;
                for (int i = start; i < end; i++)
                {
                    var elem = vec.GetElement(i);
                    if (byteWidth == 1)
                    {
                        int b = elem is Fixnum fi ? (int)fi.Value & 0xFF : 0;
                        WriteStreamByte(stream, b);
                    }
                    else
                    {
                        byte[] bytes = new byte[byteWidth];
                        if (elem is Fixnum fi2)
                        {
                            long val = fi2.Value;
                            for (int j = 0; j < byteWidth; j++) { bytes[j] = (byte)(val & 0xFF); val >>= 8; }
                        }
                        else if (elem is Bignum bi)
                        {
                            var bigBytes = bi.Value.ToByteArray(isUnsigned: true, isBigEndian: false);
                            Array.Copy(bigBytes, bytes, Math.Min(bigBytes.Length, byteWidth));
                        }
                        for (int j = 0; j < byteWidth; j++) WriteStreamByte(stream, bytes[j]);
                    }
                }
            }
            else if (seq is Cons)
            {
                if (end < 0) { end = 0; var tmp = seq; while (tmp is Cons) { end++; tmp = ((Cons)tmp).Cdr; } }
                var cell = seq;
                for (int i = 0; i < start && cell is Cons c; i++) cell = c.Cdr;
                for (int i = start; i < end && cell is Cons cc; i++)
                {
                    if (byteWidth == 1)
                    {
                        int b = cc.Car is Fixnum fi ? (int)fi.Value & 0xFF : 0;
                        WriteStreamByte(stream, b);
                    }
                    else
                    {
                        byte[] bytes = new byte[byteWidth];
                        if (cc.Car is Fixnum fi2)
                        {
                            long val = fi2.Value;
                            for (int j = 0; j < byteWidth; j++) { bytes[j] = (byte)(val & 0xFF); val >>= 8; }
                        }
                        for (int j = 0; j < byteWidth; j++) WriteStreamByte(stream, bytes[j]);
                    }
                    cell = cc.Cdr;
                }
            }
            return seq;
        }

        TextWriter writer;
        if (stream is LispOutputStream outs) writer = outs.Writer;
        else if (stream is LispFileStream fs && fs.IsOutput) writer = fs.OutputWriter!;
        else if (stream is LispBidirectionalStream bidi) writer = bidi.Writer;
        else writer = Console.Out;

        if (seq is LispString ls)
        {
            if (end < 0) end = ls.Value.Length;
            writer.Write(ls.Value.Substring(start, end - start));
        }
        else if (seq is LispVector vecCh)
        {
            if (end < 0) end = vecCh.Length;
            for (int i = start; i < end; i++)
            {
                var elem = vecCh.GetElement(i);
                if (elem is LispChar lc) writer.Write(lc.Value);
                else if (elem is Fixnum li) writer.Write((char)li.Value);
                else writer.Write(elem.ToString());
            }
        }
        else if (seq is Cons)
        {
            if (end < 0)
            {
                end = 0;
                var tmp = seq;
                while (tmp is Cons) { end++; tmp = ((Cons)tmp).Cdr; }
            }
            var cell = seq;
            for (int i = 0; i < start && cell is Cons c; i++) cell = c.Cdr;
            for (int i = start; i < end && cell is Cons cc; i++)
            {
                if (cc.Car is LispChar lc) writer.Write(lc.Value);
                else writer.Write(cc.Car.ToString());
                cell = cc.Cdr;
            }
        }
        return seq;
    }

    public static LispObject ReadPreservingWhitespace(LispObject stream, LispObject eofErrorP, LispObject eofValue)
    {
        Reader lispReader;
        if (stream is LispStream ls2 && ls2.CachedReader != null)
        {
            lispReader = ls2.CachedReader;
        }
        else
        {
            TextReader reader = GetTextReader(stream);
            lispReader = new Reader(reader) { LispStreamRef = stream };
            if (stream is LispStream ls3)
            {
                ls3.CachedReader = lispReader;
                lispReader.AdoptStreamShareTables(ls3);
            }
        }
        // Transfer any unread char from the LispStream to the Reader's pushback
        if (stream is LispStream ls && ls.UnreadCharValue != -1)
        {
            lispReader.UnreadChar(ls.UnreadCharValue);
            ls.UnreadCharValue = -1;
        }
        try
        {
            if (lispReader.TryRead(out var result))
            {
                // After reading, check if the next char is whitespace; if so, "unread" it
                // Since TryRead already consumes whitespace as delimiter, we need to
                // push it back. The Reader class may have already consumed it though.
                // For now, this is best-effort: works for string streams where Peek works.
                return result;
            }
            if (eofErrorP is not Nil)
                { var eof = new LispError("READ-PRESERVING-WHITESPACE: end of file"); eof.ConditionTypeName = "END-OF-FILE"; eof.StreamErrorStreamRef = stream; throw new LispErrorException(eof); }
            return eofValue;
        }
        catch (EndOfStreamException)
        {
            if (eofErrorP is not Nil)
                throw;
            return eofValue;
        }
    }

    public static LispObject ReadLine(LispObject stream, LispObject eofErrorP, LispObject eofValue)
    {
        var echoStream = FindEchoStream(stream);
        TextWriter? echoWriter = echoStream != null ? GetTextWriter(echoStream.OutputStream) : null;

        // Read character by character to handle concatenated streams crossing boundaries
        var sb = new System.Text.StringBuilder();
        bool foundNewline = false;
        bool gotAny = false;
        while (true)
        {
            TextReader reader = GetTextReader(stream); // Re-resolve each iteration for concatenated streams
            int ch = reader.Read();
            if (ch == -1)
            {
                // For concatenated streams, advance to next component
                if (stream is LispConcatenatedStream cs && cs.CurrentIndex < cs.Streams.Length - 1)
                {
                    cs.CurrentIndex++;
                    continue;
                }
                // Also check if the original stream wraps a concatenated stream
                var resolved = stream;
                while (resolved is LispSynonymStream syn) resolved = DynamicBindings.Get(syn.Symbol);
                while (resolved is LispTwoWayStream tw) resolved = tw.InputStream;
                while (resolved is LispEchoStream es) resolved = es.InputStream;
                if (resolved is LispConcatenatedStream cs2 && cs2.CurrentIndex < cs2.Streams.Length - 1)
                {
                    cs2.CurrentIndex++;
                    continue;
                }
                break; // True EOF
            }
            gotAny = true;
            if (ch == '\n')
            {
                foundNewline = true;
                echoWriter?.Write('\n');
                break;
            }
            sb.Append((char)ch);
            echoWriter?.Write((char)ch);
        }

        if (!gotAny)
        {
            if (IsTruthy(eofErrorP))
                throw new LispErrorException(MakeEndOfFileError(stream));
            MultipleValues.Set(eofValue, T.Instance);
            return eofValue;
        }

        var result = new LispString(sb.ToString());
        var missingNewlineP = foundNewline ? (LispObject)Nil.Instance : T.Instance;
        MultipleValues.Set(result, missingNewlineP);
        return result;
    }

    /// <summary>Resolve a stream designator to its underlying LispStream, following synonym/two-way/echo chains.</summary>
    private static LispStream ResolveLispStream(LispObject streamObj)
    {
        while (true)
        {
            switch (streamObj)
            {
                case LispSynonymStream syn:
                    streamObj = DynamicBindings.Get(syn.Symbol);
                    continue;
                case LispTwoWayStream tw:
                    return tw.InputStream;
                case LispEchoStream es:
                    return es.InputStream;
                case LispStream ls:
                    return ls;
                case T:
                    streamObj = DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*"));
                    continue;
                case Nil:
                    streamObj = DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                    continue;
                default:
                    return Startup.StandardInput;
            }
        }
    }

    private static LispError MakeEndOfFileError(LispObject stream)
    {
        var err = new LispError($"unexpected end of file on {stream}");
        err.ConditionTypeName = "END-OF-FILE";
        err.StreamErrorStreamRef = stream;
        return err;
    }

    public static LispObject ReadChar(LispObject streamObj, LispObject eofErrorP, LispObject eofValue)
    {
        // Check if this is an echo stream BEFORE resolving
        var echoStream = FindEchoStream(streamObj);
        var stream = ResolveLispStream(streamObj);
        if (stream.UnreadCharValue != -1)
        {
            char ch = (char)stream.UnreadCharValue;
            stream.UnreadCharValue = -1;
            // Don't echo unread characters (CLHS: unread-char reverses the echo)
            return LispChar.Make(ch);
        }
        TextReader reader = GetTextReader(stream);
        int c = reader.Read();
        if (c == -1)
        {
            if (IsTruthy(eofErrorP))
                throw new LispErrorException(MakeEndOfFileError(streamObj));
            return eofValue;
        }
        // Echo if reading from echo stream
        if (echoStream != null)
        {
            var outWriter = GetTextWriter(echoStream.OutputStream);
            outWriter.Write((char)c);
        }
        return LispChar.Make((char)c);
    }

    /// <summary>Read the next character from a stream, advancing through concatenated streams if needed. Returns -1 on true EOF.</summary>
    private static int ReadCharFromStream(LispObject stream)
    {
        TextReader reader = GetTextReader(stream);
        int ch = reader.Read();
        if (ch != -1) return ch;
        // For concatenated streams, GetTextReader already advances CurrentIndex on Peek/Read,
        // but if the reader returned -1, try re-resolving
        reader = GetTextReader(stream);
        return reader.Read();
    }

    /// <summary>Peek at the next character from a stream, advancing through concatenated streams if needed. Returns -1 on true EOF.</summary>
    private static int PeekCharFromStream(LispObject stream)
    {
        TextReader reader = GetTextReader(stream);
        int ch = reader.Peek();
        if (ch != -1) return ch;
        // Re-resolve in case GetTextReader advanced the concatenated stream
        reader = GetTextReader(stream);
        return reader.Peek();
    }

    public static LispObject PeekChar(LispObject peekType, LispObject streamObj, LispObject eofErrorP, LispObject eofValue)
    {
        var echoStream = FindEchoStream(streamObj);
        var stream = ResolveLispStream(streamObj);
        TextWriter? echoWriter = echoStream != null ? GetTextWriter(echoStream.OutputStream) : null;

        if (peekType is LispChar targetChar)
        {
            // peek-type is a character: skip until that character is found
            // But first check unread char
            if (stream.UnreadCharValue != -1)
            {
                if ((char)stream.UnreadCharValue == targetChar.Value)
                    return LispChar.Make((char)stream.UnreadCharValue);
                // Consume unread char (doesn't match). Per CLHS, unread chars are assumed
                // already echoed — don't echo again.
                stream.UnreadCharValue = -1;
            }
            int ch;
            while ((ch = PeekCharFromStream(streamObj)) != -1 && (char)ch != targetChar.Value)
            {
                ReadCharFromStream(streamObj);
                echoWriter?.Write((char)ch);
            }
            if (ch == -1)
            {
                if (IsTruthy(eofErrorP))
                    throw new LispErrorException(MakeEndOfFileError(streamObj));
                return eofValue;
            }
            return LispChar.Make((char)ch);
        }
        else if (peekType is T)
        {
            // Skip characters with whitespace[2] syntax type in the current readtable
            var rtObj = DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            var readtable = rtObj is LispReadtable lrt ? lrt : Startup.StandardReadtable;
            // Check unread char first
            if (stream.UnreadCharValue != -1)
            {
                if (readtable.GetSyntaxType((char)stream.UnreadCharValue) != SyntaxType.Whitespace)
                    return LispChar.Make((char)stream.UnreadCharValue);
                // Consume whitespace unread char. Per CLHS, unread chars are assumed
                // already echoed — don't echo again.
                stream.UnreadCharValue = -1;
            }
            int ch;
            while ((ch = PeekCharFromStream(streamObj)) != -1 && readtable.GetSyntaxType((char)ch) == SyntaxType.Whitespace)
            {
                ReadCharFromStream(streamObj);
                echoWriter?.Write((char)ch);
            }
            if (ch == -1)
            {
                if (IsTruthy(eofErrorP))
                    throw new LispErrorException(MakeEndOfFileError(streamObj));
                return eofValue;
            }
            return LispChar.Make((char)ch);
        }
        else
        {
            // NIL peek-type: just peek at next character (no echo for peek)
            if (stream.UnreadCharValue != -1)
                return LispChar.Make((char)stream.UnreadCharValue);
            int c = PeekCharFromStream(streamObj);
            if (c == -1)
            {
                if (IsTruthy(eofErrorP))
                    throw new LispErrorException(MakeEndOfFileError(streamObj));
                return eofValue;
            }
            return LispChar.Make((char)c);
        }
    }

    public static LispObject UnreadChar(LispObject ch, LispObject streamObj)
    {
        if (ch is not LispChar lc)
            throw new LispErrorException(new LispTypeError("UNREAD-CHAR: not a character", ch));
        var stream = ResolveLispStream(streamObj);
        stream.UnreadCharValue = lc.Value;
        return Nil.Instance;
    }

    public static LispObject FilePosition(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("FILE-POSITION: wrong number of arguments"));
        if (args.Length > 2) throw new LispErrorException(new LispProgramError($"FILE-POSITION: wrong number of arguments: {args.Length} (expected 1-2)"));
        // Broadcast stream: return file-position of last component, or 0 if empty
        if (args[0] is LispBroadcastStream bs)
        {
            if (bs.Streams.Length == 0)
                return args.Length == 1 ? Fixnum.Make(0) : T.Instance;
            args[0] = bs.Streams[^1];
            return FilePosition(args);
        }
        // String input streams track position via PositionTrackingReader
        if (args[0] is LispStringInputStream sis)
        {
            if (args.Length == 1)
                return Fixnum.Make(sis.Position);
            return Nil.Instance; // Can't set position on string streams
        }
        // String output streams track position
        if (args[0] is LispStringOutputStream sos)
        {
            if (args.Length == 1)
                return Fixnum.Make(sos.GetString().Length);
            return Nil.Instance;
        }
        if (args[0] is not LispFileStream fs)
            return Nil.Instance;

        if (args.Length == 1)
        {
            // Query position
            try
            {
                int byteWidth = GetBinaryByteWidth(fs);
                if (fs.InputReader is StreamReader sr)
                {
                    long pos = sr.BaseStream.Position;
                    return Fixnum.Make(byteWidth > 1 ? pos / byteWidth : pos);
                }
                if (fs.OutputWriter is StreamWriter sw)
                {
                    sw.Flush();
                    long pos = sw.BaseStream.Position;
                    return Fixnum.Make(byteWidth > 1 ? pos / byteWidth : pos);
                }
            }
            catch { }
            return Nil.Instance;
        }
        else
        {
            // Set position
            var posSpec = args[1];
            long newPos;
            bool isEnd = false;
            if (posSpec is Symbol sym && sym.Name == "START")
                newPos = 0;
            else if (posSpec is Symbol sym2 && sym2.Name == "END")
            {
                newPos = -1;
                isEnd = true;
            }
            else if (posSpec is Fixnum fix)
                newPos = fix.Value;
            else if (posSpec is Bignum big)
                newPos = (long)big.Value;
            else
                return Nil.Instance;

            int byteWidth = GetBinaryByteWidth(fs);
            if (byteWidth > 1 && !isEnd && newPos > 0)
                newPos *= byteWidth;

            try
            {
                if (fs.InputReader is StreamReader sr)
                {
                    if (isEnd)
                        sr.BaseStream.Seek(0, SeekOrigin.End);
                    else
                        sr.BaseStream.Seek(newPos, SeekOrigin.Begin);
                    sr.DiscardBufferedData();
                    return T.Instance;
                }
                if (fs.OutputWriter is StreamWriter sw)
                {
                    sw.Flush();
                    if (isEnd)
                        sw.BaseStream.Seek(0, SeekOrigin.End);
                    else
                        sw.BaseStream.Seek(newPos, SeekOrigin.Begin);
                    return T.Instance;
                }
            }
            catch { }
            return Nil.Instance;
        }
    }

    public static LispObject FileLength(LispObject stream)
    {
        // Broadcast stream: return file-length of last component, or 0 if empty
        if (stream is LispBroadcastStream bs)
        {
            if (bs.Streams.Length == 0) return Fixnum.Make(0);
            return FileLength(bs.Streams[^1]);
        }
        // Synonym stream: delegate to underlying stream
        if (stream is LispSynonymStream syn)
        {
            var resolved = DynamicBindings.TryGet(syn.Symbol, out var val) ? val : syn.Symbol.Value;
            if (resolved is LispStream)
                return FileLength(resolved);
        }
        if (stream is not LispFileStream fs)
            throw new LispErrorException(new LispTypeError("FILE-LENGTH: not a file stream", stream, Startup.Sym("FILE-STREAM")));
        try
        {
            int byteWidth = GetBinaryByteWidth(fs);
            if (fs.InputReader is StreamReader sr)
                return Fixnum.Make(sr.BaseStream.Length / byteWidth);
            if (fs.OutputWriter is StreamWriter sw)
            {
                sw.Flush();
                return Fixnum.Make(sw.BaseStream.Length / byteWidth);
            }
        }
        catch { }
        return Nil.Instance;
    }

    /// <summary>Resolve a CL stream designator to a TextWriter.
    /// NIL → *standard-output*, T → *terminal-io* (output side), stream → stream's writer.</summary>
    private static TextWriter ResolveOutputStreamDesignator(LispObject stream)
    {
        if (stream is Nil)
            stream = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
        else if (stream is T)
            stream = DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*"));

        return GetTextWriter(stream);
    }

    /// <summary>Update AtLineStart on the underlying LispStream after writing lastChar.</summary>
    internal static void UpdateAtLineStart(LispObject stream, char lastChar)
    {
        LispObject resolved = stream;
        if (resolved is Nil) resolved = DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
        else if (resolved is T) resolved = DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*"));
        while (resolved is LispEchoStream es) resolved = es.OutputStream;
        while (resolved is LispTwoWayStream tw) resolved = tw.OutputStream;
        while (resolved is LispSynonymStream syn) resolved = DynamicBindings.Get(syn.Symbol);
        if (resolved is LispStream ls) ls.AtLineStart = (lastChar == '\n');
    }

    public static LispObject WriteChar(LispObject ch, LispObject stream)
    {
        if (ch is not LispChar lc)
            throw new LispErrorException(new LispTypeError("WRITE-CHAR: not a character", ch));

        var writer = ResolveOutputStreamDesignator(stream);
        PprintFlushPendingBreak(writer);
        writer.Write(lc.Value);
        PprintTrackWriteChar(lc.Value);
        UpdateAtLineStart(stream, lc.Value);
        if (lc.Value == '\n')
            PprintAfterNewline(writer);
        return ch;
    }

    public static LispObject WriteString(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("WRITE-STRING: wrong number of arguments"));
        var str = args[0];
        string s = str switch
        {
            LispString ls => ls.Value,
            LispVector vec when vec.IsCharVector || vec.ElementTypeName == "NIL" => vec.ToCharString(),
            _ => str.ToString()
        };
        var stream = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));

        int start = 0, end = s.Length;
        int kwCount = args.Length - 2;
        if (kwCount > 0 && kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("WRITE-STRING: odd number of keyword arguments"));
        bool allowOtherKeys = false;
        bool startSeen = false, endSeen = false;
        for (int i = 2; i < args.Length; i += 2)
        {
            if (i + 1 >= args.Length) break;
            var key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            if (key == "ALLOW-OTHER-KEYS" && IsTruthy(args[i + 1]))
                allowOtherKeys = true;
        }
        for (int i = 2; i < args.Length; i += 2)
        {
            if (i + 1 >= args.Length) break;
            var key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            if (key == "START")
            {
                if (!startSeen)
                {
                    if (args[i + 1] is not Fixnum fx || fx.Value < 0)
                        throw new LispErrorException(new LispTypeError("WRITE-STRING: invalid :START value", args[i + 1]));
                    start = (int)fx.Value;
                    startSeen = true;
                }
            }
            else if (key == "END")
            {
                if (!endSeen)
                {
                    if (args[i + 1] is not Nil)
                    {
                        if (args[i + 1] is not Fixnum fx2 || fx2.Value < 0)
                            throw new LispErrorException(new LispTypeError("WRITE-STRING: invalid :END value", args[i + 1]));
                        end = (int)fx2.Value;
                    }
                    endSeen = true;
                }
            }
            else if (key != "ALLOW-OTHER-KEYS" && !allowOtherKeys)
                throw new LispErrorException(new LispProgramError($"WRITE-STRING: unknown keyword argument :{key}"));
        }

        var writer = ResolveOutputStreamDesignator(stream);
        PprintFlushPendingBreak(writer);
        var substr = s.Substring(start, end - start);
        writer.Write(substr);
        PprintTrackWrite(substr);
        if (substr.Length > 0) UpdateAtLineStart(stream, substr[substr.Length - 1]);
        return str;
    }

    public static LispObject WriteLine(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("WRITE-LINE: wrong number of arguments"));
        var str = args[0];
        string s = str switch
        {
            LispString ls => ls.Value,
            LispVector vec when vec.IsCharVector || vec.ElementTypeName == "NIL" => vec.ToCharString(),
            _ => str.ToString()
        };
        var stream = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));

        int start = 0, end = s.Length;
        int kwCount = args.Length - 2;
        if (kwCount > 0 && kwCount % 2 != 0)
            throw new LispErrorException(new LispProgramError("WRITE-LINE: odd number of keyword arguments"));
        bool allowOtherKeys = false;
        bool startSeen = false, endSeen = false;
        for (int i = 2; i < args.Length; i += 2)
        {
            if (i + 1 >= args.Length) break;
            var key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            if (key == "ALLOW-OTHER-KEYS" && IsTruthy(args[i + 1]))
                allowOtherKeys = true;
        }
        for (int i = 2; i < args.Length; i += 2)
        {
            if (i + 1 >= args.Length) break;
            var key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            if (key == "START")
            {
                if (!startSeen)
                {
                    if (args[i + 1] is not Fixnum fx || fx.Value < 0)
                        throw new LispErrorException(new LispTypeError("WRITE-LINE: invalid :START value", args[i + 1]));
                    start = (int)fx.Value;
                    startSeen = true;
                }
            }
            else if (key == "END")
            {
                if (!endSeen)
                {
                    if (args[i + 1] is not Nil)
                    {
                        if (args[i + 1] is not Fixnum fx2 || fx2.Value < 0)
                            throw new LispErrorException(new LispTypeError("WRITE-LINE: invalid :END value", args[i + 1]));
                        end = (int)fx2.Value;
                    }
                    endSeen = true;
                }
            }
            else if (key != "ALLOW-OTHER-KEYS" && !allowOtherKeys)
                throw new LispErrorException(new LispProgramError($"WRITE-LINE: unknown keyword argument :{key}"));
        }

        var writer = ResolveOutputStreamDesignator(stream);
        writer.Write(s.Substring(start, end - start));
        writer.Write('\n');
        UpdateAtLineStart(stream, '\n');
        return str;
    }

    private static void ValidateDirectoryComponent(LispObject arg)
    {
        LispPathname? pn = arg as LispPathname;
        if (pn == null && arg is LispString ls)
            pn = LispPathname.FromString(ls.Value) as LispPathname;
        else if (pn == null && arg is LispVector vec && vec.IsCharVector)
            pn = LispPathname.FromString(vec.ToCharString()) as LispPathname;
        if (pn != null && pn.DirectoryComponent is Cons dirList)
        {
            bool isAbsolute = false;
            bool sawWildInferiors = false;
            var cur = (LispObject)dirList;
            while (cur is Cons cc)
            {
                if (cc.Car is Symbol sym)
                {
                    if (sym.Name == "ABSOLUTE") isAbsolute = true;
                    if (sym.Name == "WILD-INFERIORS") sawWildInferiors = true;
                    if (sym.Name == "UP" || sym.Name == "BACK")
                    {
                        if (isAbsolute || sawWildInferiors)
                        {
                            var err = new LispError("DIRECTORY: invalid directory component");
                            err.ConditionTypeName = "FILE-ERROR";
                            err.FileErrorPathnameRef = pn;
                            throw new LispErrorException(err);
                        }
                    }
                }
                cur = cc.Cdr;
            }
        }
    }

    public static LispObject LispDirectory(LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("DIRECTORY requires at least one argument"));

        ValidateDirectoryComponent(args[0]);

        var pathspec = args[0];
        string filePath = ResolvePhysicalPath(pathspec);

        // Check if path contains wildcards (in filename or directory parts)
        bool hasWild = filePath.Contains('*') || filePath.Contains('?');

        if (!hasWild)
        {
            // No wildcards - check if specific file exists
            var fullPath = Path.GetFullPath(filePath);
            if (File.Exists(fullPath))
            {
                return new Cons(LispPathname.FromString(fullPath), Nil.Instance);
            }
            return Nil.Instance;
        }

        // Detect directory-only search: pathspec has no name/type component,
        // or filePath ends with a directory separator (e.g. "path/*/").
        bool wantsDirs = false;
        if (pathspec is LispPathname pnCheck2)
            wantsDirs = (pnCheck2.NameComponent is Nil || pnCheck2.NameComponent == null)
                     && (pnCheck2.TypeComponent is Nil || pnCheck2.TypeComponent == null);
        else
            wantsDirs = filePath.EndsWith("/") || filePath.EndsWith("\\");

        // Has wildcards - handle wildcards in both directory and filename parts
        // e.g. "dists/*/distinfo.txt" should match "dists/quicklisp/distinfo.txt"
        try
        {
            if (wantsDirs)
            {
                // Strip trailing slash to get wildcard dir path, e.g. "C:/foo/*/" → "C:/foo/*"
                string dirWildPath = filePath.TrimEnd('/', '\\');
                var expandedDirs = ExpandWildDirectories(dirWildPath);
                if (expandedDirs.Length == 0) return Nil.Instance;
                LispObject result = Nil.Instance;
                for (int i = expandedDirs.Length - 1; i >= 0; i--)
                {
                    string fullDir = Path.GetFullPath(expandedDirs[i]);
                    result = new Cons(LispPathname.FromString(fullDir + Path.DirectorySeparatorChar), result);
                }
                return result;
            }

            var dir = Path.GetDirectoryName(filePath) ?? ".";
            var filePattern = Path.GetFileName(filePath) ?? "*";
            if (string.IsNullOrEmpty(dir)) dir = System.IO.Directory.GetCurrentDirectory();
            if (string.IsNullOrEmpty(filePattern)) filePattern = "*";

            bool dirHasWild = dir.Contains('*') || dir.Contains('?');

            string[] files;
            if (!dirHasWild)
            {
                // Simple case: wildcards only in filename
                if (!System.IO.Directory.Exists(dir))
                    return Nil.Instance;
                files = System.IO.Directory.GetFiles(dir, filePattern);
            }
            else
            {
                // Directory path contains wildcards - expand recursively
                files = ExpandWildDirectory(dir, filePattern);
            }

            if (files.Length == 0)
                return Nil.Instance;

            // Build list from results
            LispObject fileResult = Nil.Instance;
            for (int i = files.Length - 1; i >= 0; i--)
            {
                fileResult = new Cons(LispPathname.FromString(Path.GetFullPath(files[i])), fileResult);
            }
            return fileResult;
        }
        catch
        {
            return Nil.Instance;
        }
    }

    /// <summary>
    /// Expand a directory path containing wildcards and find matching files.
    /// E.g. "/foo/*/bar" with pattern "*.txt" matches "/foo/x/bar/*.txt".
    /// </summary>
    private static string[] ExpandWildDirectory(string dirPath, string filePattern)
    {
        // Split directory path into segments
        var sep = Path.DirectorySeparatorChar;
        var altSep = Path.AltDirectorySeparatorChar;
        var segments = dirPath.Replace(altSep, sep).Split(sep);

        // Find first wild segment
        int firstWild = -1;
        for (int i = 0; i < segments.Length; i++)
        {
            if (segments[i].Contains('*') || segments[i].Contains('?'))
            {
                firstWild = i;
                break;
            }
        }
        if (firstWild < 0)
            return System.IO.Directory.Exists(dirPath)
                ? System.IO.Directory.GetFiles(dirPath, filePattern)
                : Array.Empty<string>();

        // Build the non-wild root
        var root = string.Join(sep.ToString(), segments, 0, firstWild);
        if (string.IsNullOrEmpty(root)) root = sep.ToString();

        if (!System.IO.Directory.Exists(root))
            return Array.Empty<string>();

        // Recursively expand wild segments
        var candidates = new List<string> { root };
        for (int i = firstWild; i < segments.Length; i++)
        {
            var nextCandidates = new List<string>();
            var seg = segments[i];
            foreach (var cand in candidates)
            {
                if (!System.IO.Directory.Exists(cand)) continue;
                try
                {
                    foreach (var sub in System.IO.Directory.GetDirectories(cand, seg))
                        nextCandidates.Add(sub);
                }
                catch { }
            }
            candidates = nextCandidates;
        }

        // Now find files matching filePattern in each resolved directory
        var result = new List<string>();
        foreach (var d in candidates)
        {
            try
            {
                result.AddRange(System.IO.Directory.GetFiles(d, filePattern));
            }
            catch { }
        }
        return result.ToArray();
    }

    /// <summary>
    /// Expands a directory path with wildcards (possibly multiple levels) and returns
    /// all matching directory paths. Used by DIRECTORY when searching for subdirectories.
    /// </summary>
    private static string[] ExpandWildDirectories(string dirPath)
    {
        var sep = Path.DirectorySeparatorChar;
        var altSep = Path.AltDirectorySeparatorChar;
        var segments = dirPath.Replace(altSep, sep).Split(sep);

        int firstWild = -1;
        for (int i = 0; i < segments.Length; i++)
        {
            if (segments[i].Contains('*') || segments[i].Contains('?'))
            {
                firstWild = i;
                break;
            }
        }
        if (firstWild < 0)
            return System.IO.Directory.Exists(dirPath)
                ? new[] { dirPath }
                : Array.Empty<string>();

        var root = string.Join(sep.ToString(), segments, 0, firstWild);
        if (string.IsNullOrEmpty(root)) root = sep.ToString();
        if (!System.IO.Directory.Exists(root)) return Array.Empty<string>();

        var candidates = new List<string> { root };
        for (int i = firstWild; i < segments.Length; i++)
        {
            var nextCandidates = new List<string>();
            var seg = segments[i];
            foreach (var cand in candidates)
            {
                if (!System.IO.Directory.Exists(cand)) continue;
                try
                {
                    foreach (var sub in System.IO.Directory.GetDirectories(cand, seg))
                        nextCandidates.Add(sub);
                }
                catch { }
            }
            candidates = nextCandidates;
        }
        return candidates.ToArray();
    }

    public static LispObject ProbeFile(LispObject path)
    {
        if (path is LispStream && path is not LispFileStream)
            return Nil.Instance;

        // Convert string to pathname to check for logical/wild
        LispPathname? pn = null;
        if (path is LispString str && IsLogicalPathnameString(str.Value))
            pn = LispLogicalPathname.FromLogicalString(str.Value);
        else if (path is LispPathname pp)
            pn = pp;

        if (pn != null && HasWildComponent(pn))
        {
            var err = new LispError($"PROBE-FILE: wild pathname not allowed: {pn.ToNamestring()}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = pn;
            throw new LispErrorException(err);
        }

        string filePath = ResolvePhysicalPath(path);

        // Resolve relative path using *default-pathname-defaults*
        if (!Path.IsPathRooted(filePath))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                filePath = Path.Combine(defaults, filePath);
        }

        if (File.Exists(filePath))
            return LispPathname.FromString(Path.GetFullPath(filePath));
        if (Directory.Exists(filePath))
            return LispPathname.FromString(Path.GetFullPath(filePath));
        return Nil.Instance;
    }

    public static LispObject Truename(LispObject path)
    {
        if (path is LispStream && path is not LispFileStream)
        {
            var err = new LispError("TRUENAME: stream has no associated file");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = path;
            throw new LispErrorException(err);
        }

        // Convert string to pathname to check for logical/wild
        LispPathname? pn = null;
        if (path is LispString str && IsLogicalPathnameString(str.Value))
            pn = LispLogicalPathname.FromLogicalString(str.Value);
        else if (path is LispPathname pp)
            pn = pp;

        if (pn != null && HasWildComponent(pn))
        {
            var err = new LispError($"TRUENAME: wild pathname: {pn.ToNamestring()}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = pn;
            throw new LispErrorException(err);
        }

        string filePath = ResolvePhysicalPath(path);

        // Resolve relative path using *default-pathname-defaults*
        if (!Path.IsPathRooted(filePath))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                filePath = Path.Combine(defaults, filePath);
        }

        if (File.Exists(filePath) || Directory.Exists(filePath))
        {
            var fullPath = Path.GetFullPath(filePath);
            // For directories, ensure trailing / so last component is in directory, not name
            if (Directory.Exists(fullPath) && !fullPath.EndsWith('/'))
                fullPath += "/";
            return LispPathname.FromString(fullPath);
        }
        var fileErr = new LispError($"TRUENAME: file not found: {filePath}");
        fileErr.ConditionTypeName = "FILE-ERROR";
        fileErr.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
        throw new LispErrorException(fileErr);
    }

    private static string GetDefaultPathnameDefaults()
    {
        try
        {
            var sym = Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*");
            var val = DynamicBindings.Get(sym);
            if (val is LispPathname p)
            {
                var ns = p.ToNamestring();
                var dir = Path.GetDirectoryName(ns);
                return string.IsNullOrEmpty(dir) ? ns : dir;
            }
        }
        catch { }
        return "";
    }

    private static bool IsWildSymbol(LispObject? obj)
    {
        return obj is Symbol sym && (sym.Name == "WILD" || sym.Name == "WILD-INFERIORS");
    }

    private static bool HasWildComponent(LispPathname pn)
    {
        if (IsWildSymbol(pn.NameComponent) || IsWildSymbol(pn.TypeComponent) || IsWildSymbol(pn.Version))
            return true;
        if (pn.DirectoryComponent is Cons dir)
        {
            for (var c = dir; c != null; c = c.Cdr as Cons)
            {
                if (IsWildSymbol(c.Car)) return true;
            }
        }
        return false;
    }

    /// <summary>
    /// Resolve a pathname designator to a physical file path string.
    /// Translates logical pathnames to physical paths.
    /// </summary>
    public static string ResolvePhysicalPath(LispObject pathSpec)
    {
        // If it's a logical pathname, translate first
        if (pathSpec is LispLogicalPathname lp)
        {
            var translated = TranslateLogicalPathname(lp);
            return ((LispPathname)translated).ToNamestring();
        }
        // If it's a string that looks like a logical pathname, parse and translate
        string raw;
        if (pathSpec is LispString s) raw = s.Value;
        else if (pathSpec is LispPathname p)
        {
            raw = p.ToNamestring();
            // Fall through to DPD merge logic below for relative paths
            if (Path.IsPathRooted(raw)) return raw;
            goto mergeDpd;
        }
        else if (pathSpec is LispFileStream fs) return fs.FilePath;
        else if (pathSpec is LispVector v && v.IsCharVector) raw = v.ToCharString();
        else return pathSpec.ToString();

        if (IsLogicalPathnameString(raw))
        {
            var logPn = LispLogicalPathname.FromLogicalString(raw);
            var translated = TranslateLogicalPathname(logPn);
            return ((LispPathname)translated).ToNamestring();
        }
        // CL spec: file operations merge with *default-pathname-defaults*
        mergeDpd:
        if (!string.IsNullOrEmpty(raw) && !Path.IsPathRooted(raw))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                return Path.Combine(defaults, raw);
        }
        return raw;
    }

    public static LispObject EnsureDirectoriesExist(LispObject[] args)
    {
        if (args.Length < 1) throw new LispErrorException(new LispProgramError("ENSURE-DIRECTORIES-EXIST: wrong number of arguments: 0 (expected at least 1)"));
        var pathSpec = args[0];
        string filePath = ResolvePhysicalPath(pathSpec);
        // Check for wild pathnames — signal file-error
        var pn = pathSpec is LispPathname ? (LispPathname)pathSpec : LispPathname.FromString(filePath);
        if (HasWildComponent(pn))
        {
            var err = new LispError($"ENSURE-DIRECTORIES-EXIST: wild pathname: {filePath}") { ConditionTypeName = "FILE-ERROR" };
            err.FileErrorPathnameRef = pn;
            throw new LispErrorException(err);
        }
        // CLHS: returns the pathname (coerced from designator, not the raw string input)
        LispPathname resultPn = pn;
        var dir = Path.GetDirectoryName(filePath);
        if (!string.IsNullOrEmpty(dir) && !Directory.Exists(dir))
        {
            Directory.CreateDirectory(dir);
            return MultipleValues.Values(resultPn, T.Instance);
        }
        return MultipleValues.Values(resultPn, Nil.Instance);
    }

    public static LispObject DeleteFile(LispObject path)
    {
        string filePath = ResolvePhysicalPath(path);

        // Resolve relative path using *default-pathname-defaults*
        if (!Path.IsPathRooted(filePath))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                filePath = Path.Combine(defaults, filePath);
        }

        if (File.Exists(filePath))
        {
            File.Delete(filePath);
            return T.Instance;
        }
        // CLHS says: If delete-file fails, an error of type file-error is signaled
        var err = new LispError($"DELETE-FILE: could not delete {filePath}");
        err.ConditionTypeName = "FILE-ERROR";
        err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
        throw new LispErrorException(err);
    }

    /// <summary>
    /// Internal helper: recursively delete a directory.  Returns T if deleted, NIL otherwise.
    /// </summary>
    public static LispObject DeleteDirectory(LispObject path)
    {
        string dirPath = ResolvePhysicalPath(path);

        // Resolve relative path
        if (!Path.IsPathRooted(dirPath))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                dirPath = Path.Combine(defaults, dirPath);
        }

        try
        {
            if (Directory.Exists(dirPath))
            {
                Directory.Delete(dirPath, true);
                return T.Instance;
            }
            return Nil.Instance;
        }
        catch
        {
            return Nil.Instance;
        }
    }

    public static LispObject FileAuthor(LispObject path)
    {
        // CLHS: returns NIL or a string. Returning NIL is conforming.
        string filePath;
        if (path is LispFileStream fs)
            filePath = fs.FilePath;
        else if (path is LispStream)
            return Nil.Instance;
        else if (path is LispString s)
            filePath = s.Value;
        else if (path is LispPathname p)
        {
            if (HasWildComponent(p))
            {
                var err = new LispError($"FILE-AUTHOR: wild pathname not allowed: {p.ToNamestring()}");
                err.ConditionTypeName = "FILE-ERROR";
                err.FileErrorPathnameRef = path;
                throw new LispErrorException(err);
            }
            filePath = p.ToNamestring();
        }
        else if (path is LispVector v && v.IsCharVector)
            filePath = v.ToCharString();
        else
            filePath = path.ToString();

        // Resolve relative path using *default-pathname-defaults*
        if (!Path.IsPathRooted(filePath))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                filePath = Path.Combine(defaults, filePath);
        }

        if (!File.Exists(filePath) && !Directory.Exists(filePath))
        {
            var err = new LispError($"FILE-AUTHOR: file not found: {filePath}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = path is LispPathname ? path : (LispObject)LispPathname.FromString(filePath);
            throw new LispErrorException(err);
        }
        return Nil.Instance;
    }

    public static LispObject RenameFile(LispObject filespec, LispObject newName)
    {
        string oldFile = ResolvePhysicalPath(filespec);

        // Resolve relative old path
        if (!Path.IsPathRooted(oldFile))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                oldFile = Path.Combine(defaults, oldFile);
        }

        // Get old truename before renaming
        var oldTruename = LispPathname.FromString(Path.GetFullPath(oldFile));

        // Build defaulted-new-name preserving logical pathname type
        // CLHS: defaulted-new-name = (merge-pathnames new-name filespec)
        var newPn = (LispPathname)Pathname(newName);
        var oldPn = (LispPathname)Pathname(filespec);
        var defaultedNewName = newPn.MergeWith(oldPn);

        // Translate to physical for actual file operation
        string targetFile = ResolvePhysicalPath(defaultedNewName);

        // Resolve relative target path
        if (!Path.IsPathRooted(targetFile))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                targetFile = Path.Combine(defaults, targetFile);
        }

        File.Move(oldFile, targetFile);

        // Return 3 values: defaulted-new-name, old-truename, new-truename
        var newTruename = LispPathname.FromString(Path.GetFullPath(targetFile));
        return MultipleValues.Values(defaultedNewName, oldTruename, newTruename);
    }

    public static LispObject FileWriteDate(LispObject path)
    {
        string filePath = ResolvePhysicalPath(path);

        // Resolve relative path using *default-pathname-defaults*
        if (!Path.IsPathRooted(filePath))
        {
            var defaults = GetDefaultPathnameDefaults();
            if (!string.IsNullOrEmpty(defaults))
                filePath = Path.Combine(defaults, filePath);
        }
        // Check for wild pathnames — signal file-error
        if (path is LispPathname wp && HasWildComponent(wp))
        {
            var err = new LispError($"FILE-WRITE-DATE: wild pathname not allowed: {wp.ToNamestring()}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = path;
            throw new LispErrorException(err);
        }
        DateTime dt;
        if (File.Exists(filePath))
            dt = File.GetLastWriteTimeUtc(filePath);
        else if (Directory.Exists(filePath))
            dt = Directory.GetLastWriteTimeUtc(filePath);
        else
            return Nil.Instance;
        // Return as Universal Time (seconds since 1900-01-01)
        var epoch = new DateTime(1900, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        long seconds = (long)(dt - epoch).TotalSeconds;
        return Fixnum.Make(seconds);
    }

    public static LispObject DirectoryFunc(LispObject[] args)
    {
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("DIRECTORY: wrong number of arguments"));

        ValidateDirectoryComponent(args[0]);
        // Parse keyword args (accept any, only :allow-other-keys matters)
        // Get pathname from first arg — translate logical pathnames
        string namestring = ResolvePhysicalPath(args[0]);
        if (string.IsNullOrEmpty(namestring)) namestring = ".";

        // Detect directory-only search: pathname has no name/type component.
        // e.g. (directory "path/*/") has name=NIL, type=NIL — return directories, not files.
        bool wantsDirs = false;
        if (args[0] is LispPathname pnCheck)
            wantsDirs = (pnCheck.NameComponent is Nil || pnCheck.NameComponent == null)
                     && (pnCheck.TypeComponent is Nil || pnCheck.TypeComponent == null);
        else
            wantsDirs = namestring.EndsWith("/") || namestring.EndsWith("\\");

        // Check if pattern contains wildcards
        bool hasWild = namestring.Contains('*') || namestring.Contains('?');

        if (!hasWild)
        {
            // Non-wild: check if file/directory exists
            string fullPath = Path.GetFullPath(namestring);
            if (File.Exists(fullPath) || Directory.Exists(fullPath))
            {
                var result = LispPathname.FromString(fullPath);
                return new Cons(result, Nil.Instance);
            }
            return Nil.Instance;
        }

        // Wild: enumerate matching files or directories
        try
        {
            var entries = new List<LispObject>();

            // When pathspec has no name/type component (directory search like "path/*/"),
            // enumerate matching directories rather than files.
            if (wantsDirs)
            {
                // Strip trailing slashes to get the directory wildcard path.
                // e.g. "C:/foo/*/" → "C:/foo/*"
                string dirWildPath = namestring.TrimEnd('/', '\\');
                string searchDir = Path.GetDirectoryName(dirWildPath) ?? ".";
                string dirWild = Path.GetFileName(dirWildPath);
                if (string.IsNullOrEmpty(dirWild)) dirWild = "*";
                if (string.IsNullOrEmpty(searchDir)) searchDir = ".";

                if (!Directory.Exists(searchDir)) return Nil.Instance;
                foreach (var dir in Directory.EnumerateDirectories(searchDir, dirWild))
                {
                    // Return as directory-designating pathname (trailing separator)
                    string fullDir = Path.GetFullPath(dir);
                    entries.Add(LispPathname.FromString(fullDir + Path.DirectorySeparatorChar));
                }
                return Runtime.List(entries.ToArray());
            }

            string dirPath = Path.GetDirectoryName(namestring) ?? ".";
            string pattern = Path.GetFileName(namestring) ?? "*";
            if (string.IsNullOrEmpty(dirPath)) dirPath = ".";

            // Check for ** (recursive) pattern
            if (namestring.Contains("**"))
            {
                foreach (var file in Directory.EnumerateFileSystemEntries(
                    dirPath, pattern, SearchOption.AllDirectories))
                {
                    entries.Add(LispPathname.FromString(Path.GetFullPath(file)));
                }
            }
            else
            {
                foreach (var file in Directory.EnumerateFileSystemEntries(dirPath, pattern))
                {
                    entries.Add(LispPathname.FromString(Path.GetFullPath(file)));
                }
            }

            return Runtime.List(entries.ToArray());
        }
        catch
        {
            return Nil.Instance;
        }
    }

    public static LispObject MakeStringOutputStream()
    {
        var sw = new StringWriter();
        return new LispStringOutputStream(sw);
    }

    public static LispObject MakeStringOutputStreamToString(LispObject str)
    {
        if (str is LispVector vec && vec.IsCharVector && vec.HasFillPointer)
            return new LispFillPointerStringOutputStream(vec);
        throw new LispErrorException(new LispTypeError(
            "WITH-OUTPUT-TO-STRING: string argument must be a string with a fill-pointer", str));
    }

    public static LispObject GetOutputStreamString(LispObject stream)
    {
        if (stream is LispStringOutputStream sso)
        {
            var str = sso.GetStringAndReset();
            // When element-type is NIL, return a LispVector with element-type NIL
            if (sso.ElementTypeName == "NIL")
                return new LispVector(str.Length, Nil.Instance, "NIL");
            return new LispString(str);
        }
        throw new LispErrorException(new LispTypeError("GET-OUTPUT-STREAM-STRING: not a string output stream", stream));
    }

    public static LispObject MakeStringInputStream(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("MAKE-STRING-INPUT-STREAM: wrong number of arguments"));
        if (args.Length > 3) throw new LispErrorException(new LispProgramError($"MAKE-STRING-INPUT-STREAM: wrong number of arguments: {args.Length} (expected 1-3)"));
        string val;
        if (args[0] is LispString s)
            val = s.Value;
        else if (args[0] is LispVector v && v.IsCharVector)
        {
            var sb = new System.Text.StringBuilder(v.Length);
            for (int i = 0; i < v.Length; i++)
            {
                var e = v.GetElement(i);
                if (e is LispChar lc) sb.Append(lc.Value);
            }
            val = sb.ToString();
        }
        else
            throw new LispErrorException(new LispTypeError("MAKE-STRING-INPUT-STREAM: not a string", args[0]));
        int start = 0, end = val.Length;
        if (args.Length > 1 && args[1] is Fixnum startFx)
            start = (int)startFx.Value;
        if (args.Length > 2 && args[2] is not Nil)
        {
            if (args[2] is Fixnum endFx)
                end = (int)endFx.Value;
        }
        var sliced = val.Substring(start, end - start);
        var sr = new StringReader(sliced);
        return new LispStringInputStream(sr, start);
    }


    internal static void RegisterIOBuiltins()
    {
        // --- File operations ---
        Emitter.CilAssembler.RegisterFunction("DIRECTORY",
            new LispFunction(args => Runtime.DirectoryFunc(args), "DIRECTORY", -1));
        Emitter.CilAssembler.RegisterFunction("FILE-AUTHOR",
            new LispFunction(args => Runtime.FileAuthor(args[0])));
        Emitter.CilAssembler.RegisterFunction("DELETE-FILE",
            new LispFunction(args => Runtime.DeleteFile(args[0])));
        Emitter.CilAssembler.RegisterFunction("DOTCL-DELETE-DIRECTORY",
            new LispFunction(args => Runtime.DeleteDirectory(args[0])));
        Emitter.CilAssembler.RegisterFunction("RENAME-FILE",
            new LispFunction(args => Runtime.RenameFile(args[0], args[1])));
        Emitter.CilAssembler.RegisterFunction("ENSURE-DIRECTORIES-EXIST",
            new LispFunction(args => Runtime.EnsureDirectoriesExist(args), "ENSURE-DIRECTORIES-EXIST", -1));

        // --- READ-SEQUENCE, WRITE-SEQUENCE ---
        Emitter.CilAssembler.RegisterFunction("READ-SEQUENCE",
            new LispFunction(args => Runtime.ReadSequence(args)));
        Emitter.CilAssembler.RegisterFunction("WRITE-SEQUENCE",
            new LispFunction(args => Runtime.WriteSequence(args)));

        // --- READ-FROM-STRING ---
        Emitter.CilAssembler.RegisterFunction("READ-FROM-STRING", new LispFunction(Runtime.ReadFromString, "READ-FROM-STRING", -1));

        // --- READ-DELIMITED-LIST ---
        Emitter.CilAssembler.RegisterFunction("READ-DELIMITED-LIST", new LispFunction(args => {
            if (args.Length < 1)
                throw new LispErrorException(new LispProgramError("READ-DELIMITED-LIST: requires at least 1 argument"));
            if (args.Length > 3)
                throw new LispErrorException(new LispProgramError($"READ-DELIMITED-LIST: too many arguments: {args.Length} (expected at most 3)"));
            if (args[0] is not LispChar lc)
                throw new LispErrorException(new LispTypeError("READ-DELIMITED-LIST: not a character", args[0]));
            char delimChar = lc.Value;
            // Optional stream arg (default *STANDARD-INPUT*), with stream designator resolution
            LispObject streamObj;
            if (args.Length >= 2)
            {
                if (args[1] is T)
                    streamObj = DynamicBindings.Get(Startup.Sym("*TERMINAL-IO*"));
                else if (args[1] is Nil)
                    streamObj = DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                else
                    streamObj = args[1];
            }
            else
            {
                streamObj = DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
            }
            // recursive-p is args[2] if provided (ignored for now)
            System.IO.TextReader reader = Runtime.GetTextReader(streamObj);
            var lispReader = new Reader(reader) { LispStreamRef = streamObj };
            var items = new System.Collections.Generic.List<LispObject>();
            while (true) {
                lispReader.SkipWhitespace();
                int ch = lispReader.Peek();
                if (ch == -1) throw new LispErrorException(new LispError("READ-DELIMITED-LIST: unexpected end of input"));
                if (ch == delimChar) {
                    lispReader.ReadChar();
                    break;
                }
                if (lispReader.TryRead(out var obj))
                    items.Add(obj);
            }
            // Build list from items
            LispObject result = Nil.Instance;
            for (int i = items.Count - 1; i >= 0; i--)
                result = new Cons(items[i], result);
            return result;
        }, "READ-DELIMITED-LIST", -1));

        // --- Stream predicates/accessors ---
        Startup.RegisterUnary("OPEN-STREAM-P", Runtime.OpenStreamP);
        Startup.RegisterUnary("STREAMP", obj => {
            if (obj is LispStream) return T.Instance;
            if (obj is LispInstance gi && (Runtime.IsGrayOutputStream(gi) || Runtime.IsGrayInputStream(gi)))
                return T.Instance;
            return Nil.Instance;
        });
        Startup.RegisterUnary("INPUT-STREAM-P", obj => {
            if (obj is LispStream s) return s.IsInput ? T.Instance : Nil.Instance;
            if (obj is LispInstance gi && Runtime.IsGrayInputStream(gi)) return T.Instance;
            if (obj is LispInstance) return Nil.Instance; // CLOS stream but not input
            throw new LispErrorException(new LispTypeError("INPUT-STREAM-P: not a stream", obj, Startup.Sym("STREAM")));
        });
        Startup.RegisterUnary("OUTPUT-STREAM-P", obj => {
            if (obj is LispStream s) return s.IsOutput ? T.Instance : Nil.Instance;
            if (obj is LispInstance gi && Runtime.IsGrayOutputStream(gi)) return T.Instance;
            if (obj is LispInstance) return Nil.Instance; // CLOS stream but not output
            throw new LispErrorException(new LispTypeError("OUTPUT-STREAM-P: not a stream", obj, Startup.Sym("STREAM")));
        });
        Startup.RegisterUnary("INTERACTIVE-STREAM-P", obj => {
            if (obj is not LispStream) throw new LispErrorException(new LispTypeError("INTERACTIVE-STREAM-P: not a stream", obj, Startup.Sym("STREAM")));
            if (obj is LispSynonymStream syn) obj = DynamicBindings.Get(syn.Symbol);
            if (obj is LispInputStream li && li.Reader == Console.In) return T.Instance;
            if (obj is LispOutputStream lo && lo.Writer == Console.Out) return T.Instance;
            if (obj is LispBidirectionalStream bi && bi.Reader == Console.In) return T.Instance;
            return Nil.Instance;
        });
        Startup.RegisterUnary("STREAM-ELEMENT-TYPE", obj => {
            if (obj is not LispStream ls) throw new LispErrorException(new LispTypeError("STREAM-ELEMENT-TYPE: not a stream", obj, Startup.Sym("STREAM")));
            return ls.ElementType ?? Startup.Sym("CHARACTER");
        });
        Startup.RegisterUnary("STREAM-EXTERNAL-FORMAT", obj => {
            if (obj is not LispStream) throw new LispErrorException(new LispTypeError("STREAM-EXTERNAL-FORMAT: not a stream", obj, Startup.Sym("STREAM")));
            return Startup.Keyword("DEFAULT");
        });
        Startup.RegisterUnary("BROADCAST-STREAM-STREAMS", obj => {
            if (obj is not LispBroadcastStream bs) throw new LispErrorException(new LispTypeError("BROADCAST-STREAM-STREAMS: not a broadcast stream", obj, Startup.Sym("BROADCAST-STREAM")));
            LispObject result = Nil.Instance;
            for (int i = bs.Streams.Length - 1; i >= 0; i--)
                result = new Cons(bs.Streams[i], result);
            return result;
        });
        Startup.RegisterUnary("CONCATENATED-STREAM-STREAMS", obj => {
            if (obj is not LispConcatenatedStream cs) throw new LispErrorException(new LispTypeError("CONCATENATED-STREAM-STREAMS: not a concatenated stream", obj, Startup.Sym("CONCATENATED-STREAM")));
            LispObject result = Nil.Instance;
            for (int i = cs.Streams.Length - 1; i >= cs.CurrentIndex; i--)
                result = new Cons(cs.Streams[i], result);
            return result;
        });
        Startup.RegisterUnary("ECHO-STREAM-INPUT-STREAM", obj => {
            if (obj is not LispEchoStream es) throw new LispErrorException(new LispTypeError("ECHO-STREAM-INPUT-STREAM: not an echo stream", obj, Startup.Sym("ECHO-STREAM")));
            return es.InputStream;
        });
        Startup.RegisterUnary("ECHO-STREAM-OUTPUT-STREAM", obj => {
            if (obj is not LispEchoStream es) throw new LispErrorException(new LispTypeError("ECHO-STREAM-OUTPUT-STREAM: not an echo stream", obj, Startup.Sym("ECHO-STREAM")));
            return es.OutputStream;
        });
        Startup.RegisterUnary("TWO-WAY-STREAM-INPUT-STREAM", obj => {
            if (obj is not LispTwoWayStream ts) throw new LispErrorException(new LispTypeError("TWO-WAY-STREAM-INPUT-STREAM: not a two-way stream", obj, Startup.Sym("TWO-WAY-STREAM")));
            return ts.InputStream;
        });
        Startup.RegisterUnary("TWO-WAY-STREAM-OUTPUT-STREAM", obj => {
            if (obj is not LispTwoWayStream ts) throw new LispErrorException(new LispTypeError("TWO-WAY-STREAM-OUTPUT-STREAM: not a two-way stream", obj, Startup.Sym("TWO-WAY-STREAM")));
            return ts.OutputStream;
        });
        Startup.RegisterUnary("SYNONYM-STREAM-SYMBOL", obj => {
            if (obj is not LispSynonymStream ss) throw new LispErrorException(new LispTypeError("SYNONYM-STREAM-SYMBOL: not a synonym stream", obj, Startup.Sym("SYNONYM-STREAM")));
            return ss.Symbol;
        });
        Startup.RegisterUnary("STREAM-ERROR-STREAM", obj => {
            if (obj is LispInstanceCondition lic)
            {
                if (lic.Instance.Class.SlotIndex.TryGetValue("STREAM", out int idx))
                    return lic.Instance.Slots[idx] ?? Nil.Instance;
                if (lic.StreamErrorStreamRef != null) return lic.StreamErrorStreamRef;
            }
            if (obj is LispCondition lc && lc.StreamErrorStreamRef != null)
                return lc.StreamErrorStreamRef;
            return Nil.Instance;
        });

        // --- FILE-STRING-LENGTH ---
        Emitter.CilAssembler.RegisterFunction("FILE-STRING-LENGTH", new LispFunction(args => {
            Runtime.CheckArityExact("FILE-STRING-LENGTH", args, 2);
            var stream = args[0];
            // Broadcast stream: delegate to last component, or return 1 if empty
            if (stream is LispBroadcastStream bs)
            {
                if (bs.Streams.Length == 0) return new Fixnum(1);
                args[0] = bs.Streams[^1];
                return Runtime.Funcall(Startup.Sym("FILE-STRING-LENGTH").Function!, args);
            }
            if (stream is not LispFileStream) throw new LispErrorException(new LispTypeError("FILE-STRING-LENGTH: not a file stream", stream, Startup.Sym("FILE-STREAM")));
            string str = args[1] switch {
                LispChar lch => lch.Value.ToString(),
                LispString ls => ls.Value,
                LispVector vec when vec.IsCharVector => vec.ToCharString(),
                _ => throw new LispErrorException(new LispTypeError("FILE-STRING-LENGTH: not a string or character", args[1]))
            };
            return new Fixnum(System.Text.Encoding.UTF8.GetByteCount(str));
        }));

        // --- LISTEN, CLEAR-INPUT ---
        Startup.RegisterUnary("LISTEN", Runtime.Listen);
        Startup.RegisterUnary("CLEAR-INPUT", Runtime.ClearInput);

        // --- WRITE-BYTE, WRITE-STRING, WRITE-LINE, WRITE-CHAR ---
        Startup.RegisterBinary("WRITE-BYTE", Runtime.WriteByte);
        Emitter.CilAssembler.RegisterFunction("WRITE-STRING",
            new LispFunction(args => Runtime.WriteString(args)));
        Emitter.CilAssembler.RegisterFunction("WRITE-LINE",
            new LispFunction(args => Runtime.WriteLine(args)));
        Emitter.CilAssembler.RegisterFunction("WRITE-CHAR",
            new LispFunction(args => {
                var ch = args[0];
                var stream = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
                return Runtime.WriteChar(ch, stream);
            }));

        // --- MAKE-STRING-INPUT-STREAM, MAKE-STRING-OUTPUT-STREAM, MAKE-STRING-OUTPUT-STREAM-TO-STRING ---
        Emitter.CilAssembler.RegisterFunction("MAKE-STRING-INPUT-STREAM",
            new LispFunction(args => Runtime.MakeStringInputStream(args)));
        Emitter.CilAssembler.RegisterFunction("MAKE-STRING-OUTPUT-STREAM",
            new LispFunction(args => {
                // Validate keyword args: only :element-type is allowed
                if (args.Length > 0 && args.Length % 2 != 0)
                    throw new LispErrorException(new LispProgramError($"MAKE-STRING-OUTPUT-STREAM: odd number of keyword arguments"));
                bool allowOtherKeys = false;
                for (int i = 0; i < args.Length; i += 2)
                {
                    var key = args[i] is Symbol ks ? ks.Name : "";
                    if (key == "ALLOW-OTHER-KEYS" && Runtime.IsTruthy(args[i + 1]))
                        allowOtherKeys = true;
                }
                if (!allowOtherKeys)
                {
                    for (int i = 0; i < args.Length; i += 2)
                    {
                        var key = args[i] is Symbol ks ? ks.Name : "";
                        if (key != "ELEMENT-TYPE" && key != "ALLOW-OTHER-KEYS")
                            throw new LispErrorException(new LispProgramError($"MAKE-STRING-OUTPUT-STREAM: unknown keyword argument :{key}"));
                    }
                }
                // Extract :element-type if provided
                string? elemType = null;
                for (int i = 0; i < args.Length; i += 2)
                {
                    var key = args[i] is Symbol ks2 ? ks2.Name : "";
                    if (key == "ELEMENT-TYPE")
                    {
                        var val = args[i + 1];
                        if (val is Nil) elemType = "NIL";
                    }
                }
                var stream = Runtime.MakeStringOutputStream();
                if (elemType != null && stream is LispStringOutputStream sso)
                    sso.ElementTypeName = elemType;
                return stream;
            }));
        Startup.RegisterUnary("MAKE-STRING-OUTPUT-STREAM-TO-STRING", Runtime.MakeStringOutputStreamToString);

        // --- READ-BYTE ---
        Emitter.CilAssembler.RegisterFunction("READ-BYTE",
            new LispFunction(args => Runtime.ReadByte(args)));

        // --- READ-CHAR-NO-HANG, READ-CHAR, PEEK-CHAR, UNREAD-CHAR ---
        Emitter.CilAssembler.RegisterFunction("READ-CHAR-NO-HANG",
            new LispFunction(args => {
                if (args.Length > 4) throw new LispErrorException(new LispProgramError($"READ-CHAR-NO-HANG: wrong number of arguments: {args.Length} (expected 0-4)"));
                var stream = args.Length > 0 ? args[0] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                var eofErrorP = args.Length > 1 ? args[1] : T.Instance;
                var eofValue = args.Length > 2 ? args[2] : Nil.Instance;
                return Runtime.ReadCharNoHang(stream, eofErrorP, eofValue);
            }));
        Emitter.CilAssembler.RegisterFunction("READ-CHAR",
            new LispFunction(args => {
                if (args.Length > 4) throw new LispErrorException(new LispProgramError($"READ-CHAR: wrong number of arguments: {args.Length} (expected 0-4)"));
                var stream = args.Length > 0 ? args[0] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                var eofErrorP = args.Length > 1 ? args[1] : T.Instance;
                var eofValue = args.Length > 2 ? args[2] : Nil.Instance;
                return Runtime.ReadChar(stream, eofErrorP, eofValue);
            }));
        Emitter.CilAssembler.RegisterFunction("PEEK-CHAR",
            new LispFunction(args => {
                if (args.Length > 5) throw new LispErrorException(new LispProgramError($"PEEK-CHAR: wrong number of arguments: {args.Length} (expected 0-5)"));
                var peekType = args.Length > 0 ? args[0] : Nil.Instance;
                var stream = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                var eofErrorP = args.Length > 2 ? args[2] : T.Instance;
                var eofValue = args.Length > 3 ? args[3] : Nil.Instance;
                return Runtime.PeekChar(peekType, stream, eofErrorP, eofValue);
            }));
        Emitter.CilAssembler.RegisterFunction("UNREAD-CHAR",
            new LispFunction(args => {
                var ch = args[0];
                var stream = args.Length > 1 ? args[1] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                return Runtime.UnreadChar(ch, stream);
            }));

        // --- FILE-POSITION, FILE-LENGTH ---
        Emitter.CilAssembler.RegisterFunction("FILE-POSITION", new LispFunction(Runtime.FilePosition));
        Startup.RegisterUnary("FILE-LENGTH", Runtime.FileLength);

        // --- READ-PRESERVING-WHITESPACE ---
        Emitter.CilAssembler.RegisterFunction("READ-PRESERVING-WHITESPACE",
            new LispFunction(args => {
                if (args.Length > 4)
                    throw new LispErrorException(new LispProgramError($"READ-PRESERVING-WHITESPACE: too many arguments: {args.Length} (expected at most 4)"));
                var stream = args.Length > 0 ? args[0] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                var eofErrorP = args.Length > 1 ? args[1] : T.Instance;
                var eofValue = args.Length > 2 ? args[2] : Nil.Instance;
                return Runtime.ReadPreservingWhitespace(stream, eofErrorP, eofValue);
            }));

        // --- OPEN, CLOSE, PROBE-FILE, TRUENAME, FILE-WRITE-DATE, GET-OUTPUT-STREAM-STRING ---
        Emitter.CilAssembler.RegisterFunction("OPEN",
            new LispFunction(args => {
                if (args.Length < 1)
                    throw new LispErrorException(new LispProgramError("OPEN: wrong number of arguments: 0 (expected at least 1)"));
                var options = new LispObject[args.Length - 1];
                Array.Copy(args, 1, options, 0, options.Length);
                return Runtime.OpenFile(args[0], options);
            }, "OPEN", -1));
        Emitter.CilAssembler.RegisterFunction("CLOSE", new LispFunction(args => {
            if (args.Length < 1)
                throw new LispErrorException(new LispProgramError("CLOSE: wrong number of arguments: 0 (expected at least 1)"));
            return Runtime.CloseStream(args[0]);
        }, "CLOSE", -1));
        Startup.RegisterUnary("PROBE-FILE", Runtime.ProbeFile);
        Startup.RegisterUnary("TRUENAME", Runtime.Truename);
        Startup.RegisterUnary("FILE-WRITE-DATE", Runtime.FileWriteDate);
        Startup.RegisterUnary("GET-OUTPUT-STREAM-STRING", Runtime.GetOutputStreamString);

        // --- READ, READ-LINE ---
        Emitter.CilAssembler.RegisterFunction("READ",
            new LispFunction(args => {
                if (args.Length > 4)
                    throw new LispErrorException(new LispProgramError($"READ: too many arguments: {args.Length} (expected at most 4)"));
                var stream = args.Length > 0 ? args[0] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                var eofErrorP = args.Length > 1 ? args[1] : T.Instance;
                var eofValue = args.Length > 2 ? args[2] : Nil.Instance;
                return Runtime.ReadFromStream(stream, eofErrorP, eofValue);
            }, "READ", -1));
        Emitter.CilAssembler.RegisterFunction("READ-LINE",
            new LispFunction(args => {
                var stream = args.Length > 0 ? args[0] : DynamicBindings.Get(Startup.Sym("*STANDARD-INPUT*"));
                var eofErrorP = args.Length > 1 ? args[1] : T.Instance;
                var eofValue = args.Length > 2 ? args[2] : Nil.Instance;
                return Runtime.ReadLine(stream, eofErrorP, eofValue);
            }, "READ-LINE", -1));

        // --- Stream factory functions for composite stream types ---
        Emitter.CilAssembler.RegisterFunction("MAKE-BROADCAST-STREAM",
            new LispFunction(args => {
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i] is not LispStream s || !s.IsOutput)
                    {
                        // Expected type: (AND STREAM (SATISFIES OUTPUT-STREAM-P)) when arg is a stream but not output
                        var expectedType = args[i] is LispStream
                            ? (LispObject)Runtime.List(Startup.Sym("AND"), Startup.Sym("STREAM"), Runtime.List(Startup.Sym("SATISFIES"), Startup.Sym("OUTPUT-STREAM-P")))
                            : Startup.Sym("STREAM");
                        throw new LispErrorException(new LispTypeError("MAKE-BROADCAST-STREAM: argument is not an output stream", args[i], expectedType));
                    }
                }
                var streams = args.OfType<LispStream>().ToArray();
                return new LispBroadcastStream(streams);
            }, "MAKE-BROADCAST-STREAM", -1));
        Emitter.CilAssembler.RegisterFunction("MAKE-CONCATENATED-STREAM",
            new LispFunction(args => {
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i] is not LispStream s || !s.IsInput)
                        throw new LispErrorException(new LispTypeError("MAKE-CONCATENATED-STREAM: argument is not an input stream", args[i], Startup.Sym("STREAM")));
                }
                var streams = args.OfType<LispStream>().ToArray();
                return new LispConcatenatedStream(streams);
            }, "MAKE-CONCATENATED-STREAM", -1));
        Emitter.CilAssembler.RegisterFunction("MAKE-ECHO-STREAM",
            new LispFunction(args => {
                if (args.Length != 2) throw new LispErrorException(new LispProgramError($"MAKE-ECHO-STREAM: requires exactly 2 arguments, got {args.Length}"));
                if (args[0] is not LispStream input || !input.IsInput)
                    throw new LispErrorException(new LispTypeError("MAKE-ECHO-STREAM: first argument is not an input stream", args[0], Startup.Sym("STREAM")));
                if (args[1] is not LispStream output || !output.IsOutput)
                    throw new LispErrorException(new LispTypeError("MAKE-ECHO-STREAM: second argument is not an output stream", args[1], Startup.Sym("STREAM")));
                return new LispEchoStream(input, output);
            }, "MAKE-ECHO-STREAM", 2));
        Emitter.CilAssembler.RegisterFunction("MAKE-SYNONYM-STREAM",
            new LispFunction(args => {
                if (args.Length != 1) throw new LispErrorException(new LispProgramError($"MAKE-SYNONYM-STREAM: requires exactly 1 argument, got {args.Length}"));
                if (args[0] is not Symbol sym) throw new LispErrorException(new LispTypeError("MAKE-SYNONYM-STREAM: not a symbol", args[0], Startup.Sym("SYMBOL")));
                return new LispSynonymStream(sym);
            }, "MAKE-SYNONYM-STREAM", 1));
        Emitter.CilAssembler.RegisterFunction("MAKE-TWO-WAY-STREAM",
            new LispFunction(args => {
                if (args.Length != 2) throw new LispErrorException(new LispProgramError($"MAKE-TWO-WAY-STREAM: requires exactly 2 arguments, got {args.Length}"));
                if (args[0] is not LispStream input || !input.IsInput)
                {
                    var expectedType = args[0] is LispStream
                        ? (LispObject)Runtime.List(Startup.Sym("AND"), Startup.Sym("STREAM"), Runtime.List(Startup.Sym("SATISFIES"), Startup.Sym("INPUT-STREAM-P")))
                        : Startup.Sym("STREAM");
                    throw new LispErrorException(new LispTypeError("MAKE-TWO-WAY-STREAM: first argument is not an input stream", args[0], expectedType));
                }
                if (args[1] is not LispStream output || !output.IsOutput)
                {
                    var expectedType = args[1] is LispStream
                        ? (LispObject)Runtime.List(Startup.Sym("AND"), Startup.Sym("STREAM"), Runtime.List(Startup.Sym("SATISFIES"), Startup.Sym("OUTPUT-STREAM-P")))
                        : Startup.Sym("STREAM");
                    throw new LispErrorException(new LispTypeError("MAKE-TWO-WAY-STREAM: second argument is not an output stream", args[1], expectedType));
                }
                return new LispTwoWayStream(input, output);
            }, "MAKE-TWO-WAY-STREAM", 2));

        // --- finish-output, force-output, clear-output ---
        foreach (var fname in new[] { "FINISH-OUTPUT", "FORCE-OUTPUT", "CLEAR-OUTPUT" })
        {
            var fn = fname; // capture for closure
            Emitter.CilAssembler.RegisterFunction(fn,
                new LispFunction(args => {
                    if (args.Length > 1)
                        throw new LispErrorException(new LispProgramError($"{fn}: wrong number of arguments: {args.Length} (expected 0 or 1)"));
                    if (args.Length > 0 && args[0] is not (LispStream or Nil or T))
                        throw new LispErrorException(new LispTypeError($"{fn}: not a stream designator", args[0], Startup.Sym("STREAM")));
                    var stream = args.Length > 0 && args[0] is not Nil
                        ? args[0]
                        : DynamicBindings.Get(Startup.Sym("*STANDARD-OUTPUT*"));
                    FlushStream(stream);
                    return Nil.Instance;
                }));
        }
    }

    internal static void FlushStream(LispObject stream)
    {
        if (stream is LispBinaryStream bin)
        {
            bin.BaseStream.Flush();
            return;
        }
        if (stream is LispSynonymStream syn)
        {
            FlushStream(DynamicBindings.Get(syn.Symbol));
        }
        else if (stream is LispTwoWayStream tw)
        {
            FlushStream(tw.OutputStream);
        }
        else if (stream is LispEchoStream echo)
        {
            FlushStream(echo.OutputStream);
        }
        else if (stream is LispBroadcastStream bc)
        {
            foreach (var s in bc.Streams) FlushStream(s);
        }
        else if (stream is LispOutputStream outs)
        {
            outs.Writer.Flush();
        }
        else if (stream is LispFileStream fs && fs.OutputWriter != null)
        {
            fs.OutputWriter.Flush();
        }
        else if (stream is LispBidirectionalStream bidi)
        {
            bidi.Writer.Flush();
        }
        else
        {
            Console.Out.Flush();
        }
    }

}
