using System.Text;

namespace DotCL;

// The initial pprint dispatch table (CLHS 22.2.1.4): how lists, vectors and the
// common code forms print when *PRINT-PRETTY* is true and no user entry of
// *PRINT-PPRINT-DISPATCH* applies.
//
// A standard entry is not stored as a table entry. The printer builds the whole
// object as a queue of operations (text, logical block start and end,
// conditional newline, indentation) and then lays the queue out against the
// right margin with the rules of CLHS 22.2.1 (the XP algorithm, as SBCL
// implements it). Because the whole queue is known before layout, the decisions
// a streaming pretty printer makes as output arrives are made here by looking
// ahead, with the same results.
//
// The forms are written as small FORMAT-like control strings (the subset
// ~:< ~:> ~^ ~W ~_ ~I ~@{ ~} ~/name/) interpreted directly against the queue.
public static partial class Runtime
{
    private const byte PpText = 0, PpAtom = 1, PpBlockStart = 2, PpBlockEnd = 3, PpNewline = 4, PpIndent = 5;
    private const byte NlLinear = 0, NlFill = 1, NlMiser = 2, NlMandatory = 3, NlLiteral = 4;
    private const byte IndBlock = 0, IndCurrent = 1;

    private struct PpOpRec
    {
        public byte Op;       // PpText ...
        public byte Kind;     // newline kind or indentation kind
        public int Depth;     // open blocks when the op was queued
        public int Pos;       // flat position (characters queued before the op)
        public int Amount;    // indentation amount
        public int SectionEnd; // newline / block start: index of the next newline at depth <= Depth, -1 = end
        public int Match;     // block start: index of its block end
        public string? Str;   // text, or the suffix of a block start
    }

    /// <summary>Whether TABLE's standard entries apply to OBJ (a list, or a
    /// vector printed as #(...)).</summary>
    private static bool IsStdPprintObject(LispObject obj)
        => obj is Cons || IsStdPprintVector(obj);

    internal static bool IsStdPprintObjectPublic(LispObject obj) => IsStdPprintObject(obj);

    private static bool IsStdPprintVector(LispObject obj)
        => obj is LispVector v && !v.IsCharVector && !v.IsBitVector && v.Rank == 1
           && GetPrintArray() && (v.ElementTypeName == "T" || !GetPrintReadably());

    /// <summary>Print OBJ with the standard pretty printer starting at column
    /// START. The caller has already written any #n= label of OBJ.</summary>
    private static string StdPprintTop(LispObject obj, bool escape, int start, LispPprintDispatchTable table)
    {
        var p = new StdPprinter(escape, table);
        p.Obj(obj, checkCircle: false);
        return p.Layout(start);
    }

    private static int? GetPrintLines()
    {
        var sym = Startup.Sym("*PRINT-LINES*");
        var val = DynamicBindings.TryGet(sym, out var v) ? v : sym.Value;
        return val is Fixnum f ? (int)f.Value : null;
    }

    private static int GetPrintMiserWidth()
    {
        var sym = Startup.Sym("*PRINT-MISER-WIDTH*");
        var val = DynamicBindings.TryGet(sym, out var v) ? v : sym.Value;
        return val is Fixnum f ? (int)f.Value : -1;
    }

    // The function PPRINT-DISPATCH returns for an object a standard entry
    // prints: it prints the object the way the printer would.
    private static LispFunction? _stdPprintFunction;
    internal static LispFunction StdPprintFunction => _stdPprintFunction ??= new LispFunction(args =>
    {
        if (args.Length != 2)
            throw new LispErrorException(new LispProgramError("standard pprint function: requires 2 arguments"));
        var stream = args[0];
        var obj = args[1];
        var w = GetOutputWriter(stream);
        string text;
        var table = CurrentPprintDispatchTable();
        if (_pprintActive || table == null || !IsStdPprintObject(obj))
            text = FormatObjectNoDispatch(obj);
        else
            text = StdPprintTop(obj, GetPrintEscape(), DestinationColumn(stream), table);
        w.Write(text);
        PprintTrackWrite(text);
        w.Flush();
        NoteWritten(stream, text);
        return Nil.Instance;
    }, "PPRINT-STANDARD-ENTRY", 2);

    private static LispPprintDispatchTable? CurrentPprintDispatchTable()
    {
        var sym = Startup.Sym("*PRINT-PPRINT-DISPATCH*");
        return (DynamicBindings.TryGet(sym, out var v) ? v : sym.Value) as LispPprintDispatchTable;
    }

    // ---- control strings -------------------------------------------------

    private abstract class PpNode { }
    private sealed class PpLit : PpNode { public string S = ""; }
    private sealed class PpW : PpNode { }
    private sealed class PpUp : PpNode { }
    private sealed class PpNl : PpNode { public byte Kind; }
    private sealed class PpInd : PpNode { public byte Kind; public int N; }
    private sealed class PpBlk : PpNode { public PpNode[] Body = System.Array.Empty<PpNode>(); }
    private sealed class PpIter : PpNode { public PpNode[] Body = System.Array.Empty<PpNode>(); }
    private sealed class PpCall : PpNode { public string Fn = ""; public bool Colon; }

    /// <summary>Parse a control string whose whole text is one ~:<...~:> block
    /// and return that block's body.</summary>
    private static PpNode[] ParsePpControl(string s)
    {
        int i = 0;
        var top = ParsePpSeq(s, ref i, '\0');
        if (top.Length != 1 || top[0] is not PpBlk b)
            throw new System.InvalidOperationException("pprint control string must be a single ~:<...~:>: " + s);
        return b.Body;
    }

    private static PpNode[] ParsePpSeq(string s, ref int i, char close)
    {
        var nodes = new List<PpNode>();
        var lit = new StringBuilder();
        void FlushLit() { if (lit.Length > 0) { nodes.Add(new PpLit { S = lit.ToString() }); lit.Clear(); } }
        while (i < s.Length)
        {
            char c = s[i];
            if (c != '~') { lit.Append(c); i++; continue; }
            i++;
            int n = 0; bool hasN = false;
            while (i < s.Length && char.IsDigit(s[i])) { n = n * 10 + (s[i] - '0'); hasN = true; i++; }
            bool colon = false, at = false;
            while (i < s.Length && (s[i] == ':' || s[i] == '@')) { if (s[i] == ':') colon = true; else at = true; i++; }
            char d = s[i++];
            switch (d)
            {
                case '<':
                    FlushLit();
                    nodes.Add(new PpBlk { Body = ParsePpSeq(s, ref i, '>') });
                    break;
                case '{':
                    FlushLit();
                    nodes.Add(new PpIter { Body = ParsePpSeq(s, ref i, '}') });
                    break;
                case '>':
                case '}':
                    if (d != close) throw new System.InvalidOperationException("unbalanced pprint control string: " + s);
                    FlushLit();
                    return nodes.ToArray();
                case '^': FlushLit(); nodes.Add(new PpUp()); break;
                case 'W': FlushLit(); nodes.Add(new PpW()); break;
                case '_':
                    FlushLit();
                    nodes.Add(new PpNl { Kind = colon && at ? NlMandatory : colon ? NlFill : at ? NlMiser : NlLinear });
                    break;
                case 'I':
                    FlushLit();
                    nodes.Add(new PpInd { Kind = colon ? IndCurrent : IndBlock, N = hasN ? n : 0 });
                    break;
                case '/':
                {
                    FlushLit();
                    int end = s.IndexOf('/', i);
                    nodes.Add(new PpCall { Fn = s.Substring(i, end - i), Colon = colon });
                    i = end + 1;
                    break;
                }
                default:
                    throw new System.InvalidOperationException($"unsupported directive ~{d} in pprint control string: {s}");
            }
        }
        if (close != '\0') throw new System.InvalidOperationException("unterminated pprint control string: " + s);
        FlushLit();
        return nodes.ToArray();
    }

    // A function call: arguments filled after the operator.
    private static readonly PpNode[] PpFunCall = ParsePpControl("~:<~^~W~^ ~:_~:I~@{~W~^ ~:_~}~:>");

    // ---- the forms of the standard table (names after SBCL's printers) ----

    private static readonly PpNode[] PpBlockForm = ParsePpControl("~:<~^~W~^~3I ~:_~W~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpLambdaForm = ParsePpControl("~:<~^~W~^~3I ~:_~/LAMBDA-LIST/~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpFletForm = ParsePpControl(
        "~:<~^~W~^ ~@_~:<~@{~:<~^~W~^~3I ~:_~/LAMBDA-LIST/~1I~:@_~@{~W~^ ~_~}~:>~^ ~_~}~:>~1I~@:_~@{~W~^ ~_~}~:>");
    private static readonly PpNode[] PpLetForm = ParsePpControl(
        "~:<~^~W~^ ~@_~:<~@{~:<~^~W~@{ ~_~W~}~:>~^ ~_~}~:>~1I~^~:@_~@{~W~^ ~_~}~:>");
    private static readonly PpNode[] PpProgvForm = ParsePpControl("~:<~^~W~^~3I ~_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpProg2Form = ParsePpControl("~:<~^~W~^~3I ~:_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpCaseForm = ParsePpControl(
        "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~:/FILL/~^~@{ ~_~W~}~:>~}~:>");
    private static readonly PpNode[] PpTypecaseForm = ParsePpControl(
        "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~W~^~@{ ~_~W~}~:>~}~:>");
    private static readonly PpNode[] PpDefunForm = ParsePpControl(
        "~:<~^~W~^ ~@_~:I~W~^ ~:_~/LAMBDA-LIST/~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpDefmethodQualifiedForm = ParsePpControl(
        "~:<~^~W~^ ~@_~:I~W~^ ~W~^ ~:_~/LAMBDA-LIST/~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpDefpackageForm = ParsePpControl(
        "~:<~W~^ ~3I~:_~W~^~1I~@{~:@_~:<~^~W~^ ~:I~@_~@{~W~^ ~_~}~:>~}~:>");
    private static readonly PpNode[] PpDestructuringBindForm = ParsePpControl(
        "~:<~^~W~^~3I ~_~:/LAMBDA-LIST/~^ ~_~W~^~1I~@{ ~_~W~}~:>");
    private static readonly PpNode[] PpIfForm = ParsePpControl("~:<~^~W~^ ~:I~W~^ ~:@_~@{~W~^ ~:@_~}~:>");
    private static readonly PpNode[] PpSpreadFunCall = ParsePpControl("~:<~^~W~^ ~:_~:I~@{~W~^ ~:@_~}~:>");
    private static readonly PpNode[] PpDoBindings = ParsePpControl("~:<~@{~:<~^~W~^ ~@_~:I~W~@{ ~_~W~}~:>~^~:@_~}~:>");
    private static readonly PpNode[] PpDolistHead = ParsePpControl("~:<~^~W~^ ~:_~:I~W~@{ ~_~W~}~:>");

    private enum StdForm : byte
    {
        Block, Lambda, Flet, Let, Progn, Progv, Prog2, Setq, Tagbody, Case, Typecase, Defun,
        Defmethod, Defpackage, DestructuringBind, Do, Dolist, Loop, If, Prog, Declare, Quote,
    }

    private static Dictionary<Symbol, StdForm>? _stdForms;

    private static Dictionary<Symbol, StdForm> StdForms
    {
        get
        {
            if (_stdForms != null) return _stdForms;
            var d = new Dictionary<Symbol, StdForm>();
            void Add(StdForm f, params string[] names) { foreach (var nm in names) d[Startup.Sym(nm)] = f; }
            Add(StdForm.Lambda, "LAMBDA");
            Add(StdForm.Declare, "DECLARE", "DECLAIM");
            Add(StdForm.Block, "BLOCK", "CATCH", "RETURN-FROM", "THROW", "EVAL-WHEN", "MULTIPLE-VALUE-CALL",
                "MULTIPLE-VALUE-PROG1", "UNWIND-PROTECT",
                "DEFCONSTANT", "DEFPARAMETER", "DEFSTRUCT", "DEFVAR",
                "MULTIPLE-VALUE-SETQ", "PROG1", "PPRINT-LOGICAL-BLOCK", "PRINT-UNREADABLE-OBJECT",
                "UNLESS", "WHEN",
                "WITH-COMPILATION-UNIT", "WITH-SIMPLE-RESTART", "WITH-HASH-TABLE-ITERATOR",
                "WITH-PACKAGE-ITERATOR", "WITH-INPUT-FROM-STRING", "WITH-OUTPUT-TO-STRING",
                "WITH-OPEN-FILE", "WITH-OPEN-STREAM");
            Add(StdForm.Flet, "FLET", "LABELS", "MACROLET");
            Add(StdForm.Quote, "FUNCTION", "QUOTE");
            Add(StdForm.If, "IF");
            Add(StdForm.Let, "LET", "LET*", "SYMBOL-MACROLET");
            Add(StdForm.Progn, "LOCALLY", "PROGN", "STEP", "TIME", "WITH-STANDARD-IO-SYNTAX");
            Add(StdForm.Progv, "PROGV");
            Add(StdForm.Setq, "SETQ", "PSETQ", "SETF", "PSETF");
            Add(StdForm.Tagbody, "TAGBODY");
            Add(StdForm.Case, "CASE", "CCASE", "ECASE");
            Add(StdForm.Typecase, "CTYPECASE", "ETYPECASE", "TYPECASE");
            Add(StdForm.Defun, "DEFINE-MODIFY-MACRO", "DEFINE-SETF-EXPANDER", "DEFMACRO", "DEFSETF", "DEFTYPE", "DEFUN");
            Add(StdForm.Defmethod, "DEFMETHOD");
            Add(StdForm.Defpackage, "DEFPACKAGE");
            Add(StdForm.DestructuringBind, "DESTRUCTURING-BIND");
            Add(StdForm.Do, "DO", "DO*");
            Add(StdForm.Dolist, "DO-ALL-SYMBOLS", "DO-EXTERNAL-SYMBOLS", "DO-SYMBOLS", "DOLIST", "DOTIMES");
            Add(StdForm.Loop, "LOOP");
            Add(StdForm.Prog2, "MULTIPLE-VALUE-BIND", "PROG2");
            Add(StdForm.Prog, "PROG", "PROG*");
            return _stdForms = d;
        }
    }

    // ---- the printer -----------------------------------------------------

    private sealed class StdPprinter
    {
        private readonly List<PpOpRec> _ops = new();
        private readonly List<int> _open = new();
        private int _pos;
        private readonly bool _escape;
        private readonly LispPprintDispatchTable _table;
        // The object whose circle label and level were checked by Obj, so the
        // logical block printing it does not check them again.
        private LispObject? _entered;

        public StdPprinter(bool escape, LispPprintDispatchTable table)
        {
            _escape = escape;
            _table = table;
        }

        // ---- queue ----

        private void Add(PpOpRec op)
        {
            op.Depth = _open.Count;
            op.Pos = _pos;
            _ops.Add(op);
        }

        public void Text(string s) => AddText(s, PpText);

        private void AddText(string s, byte kind)
        {
            if (s.Length == 0) return;
            int nl = s.IndexOf('\n');
            if (nl < 0)
            {
                Add(new PpOpRec { Op = kind, Str = s });
                _pos += s.Length;
                return;
            }
            int start = 0;
            while (true)
            {
                nl = s.IndexOf('\n', start);
                if (nl < 0) break;
                if (nl > start) { Add(new PpOpRec { Op = kind, Str = s.Substring(start, nl - start) }); _pos += nl - start; }
                Add(new PpOpRec { Op = PpNewline, Kind = NlLiteral });
                start = nl + 1;
            }
            if (start < s.Length) { Add(new PpOpRec { Op = kind, Str = s.Substring(start) }); _pos += s.Length - start; }
        }

        public void Newline(byte kind) => Add(new PpOpRec { Op = PpNewline, Kind = kind });

        public void Indent(byte kind, int n) => Add(new PpOpRec { Op = PpIndent, Kind = kind, Amount = n });

        private void StartBlock(string prefix, string suffix)
        {
            Text(prefix);
            Add(new PpOpRec { Op = PpBlockStart, Str = suffix });
            _open.Add(_ops.Count - 1);
        }

        private void EndBlock()
        {
            int start = _open[_open.Count - 1];
            _open.RemoveAt(_open.Count - 1);
            Add(new PpOpRec { Op = PpBlockEnd });
            var s = _ops[start];
            s.Match = _ops.Count - 1;
            _ops[start] = s;
            Text(s.Str!);
        }

        // ---- objects ----

        /// <summary>Queue OBJ as output-object would print it.</summary>
        public void Obj(LispObject obj, bool checkCircle = true)
        {
            if (obj is Cons cons)
            {
                if (_table.Entries.Count > 0)
                {
                    var user = RunUserPprintDispatch(obj, _table, _escape);
                    if (user != null) { AddText(user, PpAtom); return; }
                }
                if (!EnterCompound(obj, checkCircle)) return;
                _entered = obj;
                try { StdCons(cons); }
                finally { _entered = null; }
                return;
            }
            if (IsStdPprintVector(obj))
            {
                if (_table.Entries.Count > 0)
                {
                    var user = RunUserPprintDispatch(obj, _table, _escape);
                    if (user != null) { AddText(user, PpAtom); return; }
                }
                if (!EnterCompound(obj, checkCircle)) return;
                PprintVector((LispVector)obj);
                return;
            }
            AddText(FormatObject(obj, _escape), PpAtom);
        }

        /// <summary>The #n= / #n# label and the *PRINT-LEVEL* cut of a compound
        /// object. False when that already printed the whole object.</summary>
        private bool EnterCompound(LispObject obj, bool checkCircle)
        {
            if (checkCircle && _circleTable != null && _circleTable.TryGetValue(obj, out int state))
            {
                if (state < 0) { AddText($"#{-state}#", PpAtom); return false; }
                if (state == 0)
                {
                    int label = ++_circleLabelCounter;
                    _circleTable[obj] = -label;
                    Text($"#{label}=");
                }
            }
            var level = GetPrintLevel();
            if (level.HasValue && _formatConsDepth >= level.Value) { AddText("#", PpAtom); return false; }
            if (_formatConsDepth >= MaxFormatConsDepth) { AddText(DepthGuardForm("(...)", obj), PpAtom); return false; }
            return true;
        }

        /// <summary>A logical block over LIST with PREFIX and SUFFIX, as
        /// PPRINT-LOGICAL-BLOCK: a non-list is printed as an object.</summary>
        private void ListBlock(LispObject list, string prefix, string suffix, System.Action<PpList> body)
        {
            if (list is not Cons && list is not Nil) { Obj(list); return; }
            if (!ReferenceEquals(list, _entered) || list is Nil)
            {
                if (!EnterCompound(list, list is Cons)) return;
            }
            _entered = null;
            StartBlock(prefix, suffix);
            _formatConsDepth++;
            try { body(new PpList(this, list)); }
            finally { _formatConsDepth--; }
            EndBlock();
        }

        private void PprintVector(LispVector vec)
        {
            StartBlock("#(", ")");
            _formatConsDepth++;
            try
            {
                int? printLength = GetPrintLength();
                int n = vec.Length;
                for (int i = 0; i < n; i++)
                {
                    if (i > 0) { Text(" "); Newline(NlFill); }
                    if (printLength.HasValue && i >= printLength.Value) { Text("..."); break; }
                    Obj(vec.ElementAt(i));
                }
            }
            finally { _formatConsDepth--; }
            EndBlock();
        }

        // ---- lists ----

        /// <summary>The list of a logical block, popped as PPRINT-POP does.</summary>
        public sealed class PpList
        {
            private readonly StdPprinter _p;
            private LispObject _cur;
            private LispObject _slow;
            private bool _moveSlow;
            private int _count;
            private readonly int? _printLength = GetPrintLength();

            public PpList(StdPprinter p, LispObject list) { _p = p; _cur = list; _slow = list; }

            public bool Exhausted => _cur is Nil;

            public LispObject Rest => _cur;

            /// <summary>Pop the next element into ITEM; false when the block
            /// ends here, after printing what ends it (". tail", "...", or a
            /// circular reference).</summary>
            public bool Pop(out LispObject item)
            {
                item = Nil.Instance;
                if (_cur is not Cons && _cur is not Nil) { _p.Text(". "); _p.Obj(_cur); return false; }
                if (_printLength.HasValue && _count >= _printLength.Value) { _p.Text("..."); return false; }
                if (_cur is not Cons c) { _count++; return true; }
                if (_count > 0 && _circleTable != null && _circleTable.TryGetValue(c, out int st) && st <= 0)
                {
                    _p.Text(". ");
                    _p.Obj(c);
                    return false;
                }
                if (_count > 0 && _circleTable == null)
                {
                    // Runaway guard: a circular list printed without *PRINT-CIRCLE*
                    // would never end.
                    if (_moveSlow) _slow = ((Cons)_slow).Cdr;
                    _moveSlow = !_moveSlow;
                    if (ReferenceEquals(c, _slow)) { _p.Text("..."); return false; }
                }
                _count++;
                item = c.Car;
                _cur = c.Cdr;
                return true;
            }
        }

        private enum PpExit { None, Up, Block }

        private PpExit Run(PpNode[] body, PpList src)
        {
            foreach (var node in body)
            {
                switch (node)
                {
                    case PpLit lit: Text(lit.S); break;
                    case PpW:
                        if (!src.Pop(out var o)) return PpExit.Block;
                        Obj(o);
                        break;
                    case PpUp:
                        if (src.Exhausted) return PpExit.Up;
                        break;
                    case PpNl nl: Newline(nl.Kind); break;
                    case PpInd ind: Indent(ind.Kind, ind.N); break;
                    case PpBlk blk:
                    {
                        if (!src.Pop(out var arg)) return PpExit.Block;
                        var inner = blk.Body;
                        ListBlock(arg, "(", ")", l => Run(inner, l));
                        break;
                    }
                    case PpIter it:
                        while (!src.Exhausted)
                        {
                            var r = Run(it.Body, src);
                            if (r == PpExit.Block) return PpExit.Block;
                            if (r == PpExit.Up) break;
                        }
                        break;
                    case PpCall call:
                    {
                        if (!src.Pop(out var arg)) return PpExit.Block;
                        CallNamed(call.Fn, call.Colon, arg);
                        break;
                    }
                }
            }
            return PpExit.None;
        }

        private void RunControl(LispObject list, PpNode[] body)
            => ListBlock(list, "(", ")", l => Run(body, l));

        private void CallNamed(string fn, bool colon, LispObject arg)
        {
            switch (fn)
            {
                case "FILL": Fill(arg, colon); break;
                case "LINEAR": Linear(arg, colon); break;
                case "LAMBDA-LIST": LambdaList(arg); break;
                default: throw new System.InvalidOperationException("unknown pprint function " + fn);
            }
        }

        /// <summary>PPRINT-FILL.</summary>
        private void Fill(LispObject list, bool colon = true)
            => ListBlock(list, colon ? "(" : "", colon ? ")" : "", l =>
            {
                while (!l.Exhausted)
                {
                    if (!l.Pop(out var o)) return;
                    Obj(o);
                    if (l.Exhausted) return;
                    Text(" ");
                    Newline(NlFill);
                }
            });

        /// <summary>PPRINT-LINEAR.</summary>
        private void Linear(LispObject list, bool colon = true)
            => ListBlock(list, colon ? "(" : "", colon ? ")" : "", l =>
            {
                while (!l.Exhausted)
                {
                    if (!l.Pop(out var o)) return;
                    Obj(o);
                    if (l.Exhausted) return;
                    Text(" ");
                    Newline(NlLinear);
                }
            });

        /// <summary>A list under the standard table: by its operator.</summary>
        private void StdCons(Cons list)
        {
            if (list.Car is Symbol head)
            {
                if (StdForms.TryGetValue(head, out var form))
                {
                    PrintStdForm(list, head, form);
                    return;
                }
                if (FboundpImpl(head) is not Nil)
                {
                    if (CallForm(list, head)) return;
                    RunControl(list, PpFunCall);
                    return;
                }
            }
            Fill(list);
        }

        /// <summary>'X and #'X for a two-element QUOTE / FUNCTION form.</summary>
        private bool PprintQuote(Cons list, Symbol head)
        {
            if (list.Cdr is not Cons rest || rest.Cdr is not Nil) return false;
            // A shared or circular cons needs its label, which the
            // abbreviation has nowhere to carry.
            if (_circleTable != null && (_circleTable.ContainsKey(list) || _circleTable.ContainsKey(rest)))
                return false;
            Text(ReferenceEquals(head, Startup.Sym("QUOTE")) ? "'" : "#'");
            Obj(rest.Car);
            return true;
        }

        // QUOTE and FUNCTION are printed as lists inside DECLARE / DECLAIM:
        // (DECLARE (FUNCTION F)), not (DECLARE #'F).
        private bool _noQuoteSugar;

        private void PrintStdForm(Cons list, Symbol head, StdForm f)
        {
            switch (f)
            {
                case StdForm.Block: RunControl(list, PpBlockForm); break;
                case StdForm.Lambda: RunControl(list, PpLambdaForm); break;
                case StdForm.Flet:
                    // (FLET FOO) and the like name a function rather than bind one.
                    if (list.Cdr is Cons c1 && c1.Cdr is Cons c2
                        && !ReferenceEquals(c2.Car, Startup.Keyword("IN")))
                        RunControl(list, PpFletForm);
                    else
                        Spaced(list);
                    break;
                case StdForm.Let: RunControl(list, PpLetForm); break;
                case StdForm.Progn: Linear(list); break;
                case StdForm.Progv: RunControl(list, PpProgvForm); break;
                case StdForm.Prog2: RunControl(list, PpProg2Form); break;
                case StdForm.Setq: Setq(list); break;
                case StdForm.Tagbody: Tagbody(list); break;
                case StdForm.Case: RunControl(list, PpCaseForm); break;
                case StdForm.Typecase: RunControl(list, PpTypecaseForm); break;
                case StdForm.Defun: RunControl(list, PpDefunForm); break;
                case StdForm.Defmethod:
                    // A qualifier between the name and the lambda list gets a
                    // place of its own.
                    if (list.Cdr is Cons d1 && d1.Cdr is Cons d2 && d2.Car is Cons)
                        RunControl(list, PpDefunForm);
                    else
                        RunControl(list, PpDefmethodQualifiedForm);
                    break;
                case StdForm.Defpackage: RunControl(list, PpDefpackageForm); break;
                case StdForm.DestructuringBind: RunControl(list, PpDestructuringBindForm); break;
                case StdForm.Do: Do(list); break;
                case StdForm.Dolist: Dolist(list); break;
                case StdForm.Loop:
                    if (list.Cdr is not Cons clauses || clauses.Car is Cons)
                        RunControl(list, PpSpreadFunCall);
                    else
                        ExtendedLoop(list);
                    break;
                case StdForm.If: RunControl(list, PpIfForm); break;
                case StdForm.Prog: Prog(list); break;
                case StdForm.Declare:
                {
                    bool saved = _noQuoteSugar;
                    _noQuoteSugar = true;
                    try { RunControl(list, PpSpreadFunCall); }
                    finally { _noQuoteSugar = saved; }
                    break;
                }
                case StdForm.Quote:
                    if (_noQuoteSugar || !PprintQuote(list, head)) Fill(list);
                    break;
            }
        }

        /// <summary>The elements separated by spaces, never broken.</summary>
        private void Spaced(LispObject list)
            => ListBlock(list, "(", ")", l =>
            {
                if (l.Exhausted) return;
                if (!l.Pop(out var o)) return;
                Obj(o);
                while (!l.Exhausted)
                {
                    Text(" ");
                    if (!l.Pop(out o)) return;
                    Obj(o);
                }
            });

        /// <summary>Place / value pairs, a pair to a line when they do not fit.</summary>
        private void Setq(Cons list)
            => ListBlock(list, "(", ")", l =>
            {
                if (l.Exhausted) return;
                if (!l.Pop(out var op)) return;
                Obj(op);
                if (l.Exhausted) return;
                Text(" ");
                if (list.Cdr is not Cons && list.Cdr is not Nil) Text(". ");
                Newline(NlMiser);
                ListBlock(list.Cdr, "", "", m =>
                {
                    while (true)
                    {
                        Indent(IndBlock, 2);
                        if (!m.Pop(out var place)) return;
                        Obj(place);
                        if (m.Exhausted) return;
                        Text(" ");
                        Newline(NlFill);
                        Indent(IndBlock, 0);
                        if (!m.Pop(out var value)) return;
                        Obj(value);
                        if (m.Exhausted) return;
                        Text(" ");
                        Newline(NlMandatory);
                    }
                });
            });

        /// <summary>The body of a TAGBODY: tags at the block's column, forms
        /// one further in.</summary>
        private void TagbodyGuts(StdPprinter.PpList l)
        {
            while (true)
            {
                if (l.Exhausted) return;
                Text(" ");
                if (!l.Pop(out var x)) return;
                Indent(IndBlock, x is Cons ? 1 : 0);
                Newline(NlLinear);
                Obj(x);
            }
        }

        private void Tagbody(Cons list)
            => ListBlock(list, "(", ")", l =>
            {
                if (l.Exhausted) return;
                if (!l.Pop(out var op)) return;
                Obj(op);
                TagbodyGuts(l);
            });

        private void Do(Cons list)
            => ListBlock(list, "(", ")", l =>
            {
                if (l.Exhausted) return;
                if (!l.Pop(out var op)) return;
                Obj(op);
                if (l.Exhausted) return;
                Text(" ");
                Indent(IndCurrent, 0);
                if (!l.Pop(out var bindings)) return;
                RunControl(bindings, PpDoBindings);
                if (l.Exhausted) return;
                Text(" ");
                Newline(NlLinear);
                if (!l.Pop(out var end)) return;
                Linear(end);
                TagbodyGuts(l);
            });

        private void Dolist(Cons list)
            => ListBlock(list, "(", ")", l =>
            {
                if (l.Exhausted) return;
                if (!l.Pop(out var op)) return;
                Obj(op);
                if (l.Exhausted) return;
                Indent(IndBlock, 3);
                Text(" ");
                Newline(NlFill);
                if (!l.Pop(out var head)) return;
                RunControl(head, PpDolistHead);
                TagbodyGuts(l);
            });

        private void Prog(Cons list)
            => ListBlock(list, "(", ")", l =>
            {
                if (l.Exhausted) return;
                if (!l.Pop(out var op)) return;
                Obj(op);
                if (l.Exhausted) return;
                Text(" ");
                Newline(NlMiser);
                if (!l.Pop(out var vars)) return;
                Fill(vars);
                TagbodyGuts(l);
            });

        private static readonly HashSet<string> LoopSeparatingClauses = new()
        {
            "AND", "WITH", "FOR", "INITIALLY", "FINALLY", "DO", "DOING", "COLLECT", "COLLECTING",
            "APPEND", "APPENDING", "NCONC", "NCONCING", "COUNT", "COUNTING", "SUM", "SUMMING",
            "MAXIMIZE", "MAXIMIZING", "MINIMIZE", "MINIMIZING", "IF", "WHEN", "UNLESS", "END",
            "WHILE", "UNTIL", "REPEAT", "ALWAYS", "NEVER", "THEREIS",
        };

        /// <summary>An extended LOOP: a clause keyword starts a line.</summary>
        private void ExtendedLoop(Cons list)
            => ListBlock(list, "(", ")", l =>
            {
                if (!l.Pop(out var op)) return;
                Obj(op);
                if (l.Exhausted) return;
                Text(" ");
                Indent(IndCurrent, 0);
                if (!l.Pop(out var first)) return;
                Obj(first);
                if (l.Exhausted) return;
                Text(" ");
                while (true)
                {
                    if (!l.Pop(out var thing)) return;
                    if (thing is Symbol s && LoopSeparatingClauses.Contains(s.Name))
                        Newline(NlMandatory);
                    Obj(thing);
                    if (l.Exhausted) return;
                    Text(" ");
                }
            });

        /// <summary>A lambda list: a lambda list keyword may start a line.</summary>
        private void LambdaList(LispObject lambdaList)
            => ListBlock(lambdaList, "(", ")", l =>
            {
                int state = 0; // 0 required, 1 optional / aux, 2 key
                bool first = true;
                while (true)
                {
                    if (l.Exhausted) return;
                    if (!first) Text(" ");
                    if (!l.Pop(out var arg)) return;
                    string? kw = arg is Symbol s && ReferenceEquals(s.HomePackage, Startup.CL) ? s.Name : null;
                    switch (kw)
                    {
                        case "&OPTIONAL": case "&AUX": state = 1; Newline(NlLinear); break;
                        case "&REST": case "&BODY": state = 0; Newline(NlLinear); break;
                        case "&KEY": state = 2; Newline(NlLinear); break;
                        default: Newline(NlFill); break;
                    }
                    if (state == 0)
                    {
                        ListBlock(arg, "(", ")", m =>
                        {
                            if (m.Exhausted) return;
                            while (true)
                            {
                                if (!m.Pop(out var o)) return;
                                Obj(o);
                                if (m.Exhausted) return;
                                Text(" ");
                                Newline(NlLinear);
                            }
                        });
                    }
                    else
                    {
                        bool key = state == 2;
                        ListBlock(arg, "(", ")", m =>
                        {
                            if (m.Exhausted) return;
                            if (!m.Pop(out var o)) return;
                            if (key)
                            {
                                ListBlock(o, "(", ")", k =>
                                {
                                    if (k.Exhausted) return;
                                    if (!k.Pop(out var e)) return;
                                    Obj(e);
                                    if (k.Exhausted) return;
                                    Text(" ");
                                    Newline(NlFill);
                                    if (!k.Pop(out e)) return;
                                    Obj(e);
                                    while (true)
                                    {
                                        if (k.Exhausted) return;
                                        Text(" ");
                                        Newline(NlFill);
                                        if (!k.Pop(out e)) return;
                                        Obj(e);
                                    }
                                });
                            }
                            else Obj(o);
                            while (true)
                            {
                                if (m.Exhausted) return;
                                Text(" ");
                                Newline(NlLinear);
                                if (!m.Pop(out o)) return;
                                Obj(o);
                            }
                        });
                    }
                    first = false;
                }
            });

        /// <summary>A macro whose lambda list has &BODY: the arguments before
        /// the body on lines of their own, the body indented by 2 (SBCL's
        /// pprint-call-form). False for anything else.</summary>
        private bool CallForm(Cons list, Symbol head)
        {
            if (MacroFunction(head) is not LispFunction mf) return false;
            LispObject lambdaList;
            try { lambdaList = FunctionLambdaList(mf); }
            catch (LispErrorException) { return false; }
            int? indentation = MacroIndentation(lambdaList);
            if (indentation == null) return false;
            int n = indentation.Value;
            ListBlock(list, "(", ")", l =>
            {
                if (!l.Pop(out var op)) return;
                Obj(op);
                if (l.Exhausted) return;
                Text(" ");
                for (int i = 0; i < n; i++)
                {
                    if (i > 0)
                    {
                        Indent(IndBlock, 3);
                        Newline(NlMandatory);
                    }
                    if (!l.Pop(out var arg)) return;
                    Obj(arg);
                    if (l.Exhausted) return;
                }
                Indent(IndBlock, 1);
                Newline(NlMandatory);
                while (true)
                {
                    if (!l.Pop(out var form)) return;
                    Obj(form);
                    if (l.Exhausted) return;
                    Newline(NlMandatory);
                }
            });
            return true;
        }

        /// <summary>How many arguments come before &BODY in LAMBDA-LIST (a
        /// dotted tail counts as &BODY), or null when it has no body.</summary>
        private static int? MacroIndentation(LispObject lambdaList)
        {
            int n = 0;
            var cur = lambdaList;
            while (cur is Cons c)
            {
                if (c.Car is Symbol s && ReferenceEquals(s.HomePackage, Startup.CL))
                {
                    if (s.Name == "&BODY") return n;
                    if (s.Name == "&OPTIONAL") { cur = c.Cdr; continue; }
                    if ((s.Name == "&WHOLE" || s.Name == "&ENVIRONMENT") && c.Cdr is Cons v)
                    {
                        cur = v.Cdr;
                        continue;
                    }
                }
                n++;
                cur = c.Cdr;
            }
            return cur is Nil ? null : n;
        }

        // ---- layout ----

        private struct PpBlockState
        {
            public int Start, Indent, SectionStartLine;
            public int Open; // index of the block start op, -1 for the outermost
        }

        /// <summary>Lay the queue out from column START against the right
        /// margin and return the text.</summary>
        public string Layout(int start)
        {
            var ops = _ops;
            int n = ops.Count;
            int total = _pos;

            // Section ends: the next newline at the same or an outer depth.
            int maxDepth = 0;
            for (int i = 0; i < n; i++) if (ops[i].Depth > maxDepth) maxDepth = ops[i].Depth;
            var nextLE = new int[maxDepth + 2];
            for (int d = 0; d < nextLE.Length; d++) nextLE[d] = -1;
            // forced[i]: mandatory or literal newlines among ops[0..i).
            var forced = new int[n + 1];
            for (int i = 0; i < n; i++)
                forced[i + 1] = forced[i] + (ops[i].Op == PpNewline && ops[i].Kind >= NlMandatory ? 1 : 0);
            for (int i = n - 1; i >= 0; i--)
            {
                var op = ops[i];
                if (op.Op == PpBlockStart || op.Op == PpNewline)
                {
                    op.SectionEnd = nextLE[op.Depth];
                    ops[i] = op;
                }
                if (op.Op == PpNewline)
                    for (int d = op.Depth; d < nextLE.Length; d++) nextLE[d] = i;
            }

            int margin = GetPrintRightMargin();
            int miser = GetPrintMiserWidth();
            bool readably = GetPrintReadably();
            int? printLines = readably ? null : GetPrintLines();

            var sb = new StringBuilder();
            int col = start;
            int cur = 0;          // flat position of the output so far
            int line = 0;
            int keep = 0;         // sb length that trailing-blank trimming must not cut into
            var blocks = new List<PpBlockState> { new PpBlockState { Start = 0, Indent = 0, SectionStartLine = 0, Open = -1 } };

            bool Misering()
                => miser >= 0 && margin - blocks[blocks.Count - 1].Start <= miser;

            int SuffixLength()
            {
                int len = 0;
                for (int b = 1; b < blocks.Count; b++) len += ops[blocks[b].Open].Str!.Length;
                return len;
            }

            bool Fits(int at, int until)
            {
                int available = margin;
                if (printLines.HasValue && printLines.Value == line)
                    available -= 3 + SuffixLength();
                int endIdx = until < 0 ? n : until;
                if (forced[endIdx] - forced[at + 1] > 0) return false;
                int endPos = until < 0 ? total : ops[until].Pos;
                return col + (endPos - cur) <= available;
            }

            for (int i = 0; i < n; i++)
            {
                var op = ops[i];
                switch (op.Op)
                {
                    case PpText:
                    case PpAtom:
                        sb.Append(op.Str);
                        col += op.Str!.Length;
                        cur += op.Str.Length;
                        if (op.Op == PpAtom) keep = sb.Length;
                        break;
                    case PpNewline:
                    {
                        bool brk;
                        switch (op.Kind)
                        {
                            case NlMiser: brk = Misering(); break;
                            case NlFill:
                                brk = Misering()
                                      || line > blocks[blocks.Count - 1].SectionStartLine
                                      || !Fits(i, op.SectionEnd);
                                break;
                            default: brk = true; break;
                        }
                        if (!brk) break;
                        bool literal = op.Kind == NlLiteral;
                        if (!literal)
                        {
                            int lineStart = 0;
                            for (int k = sb.Length - 1; k >= 0; k--) if (sb[k] == '\n') { lineStart = k + 1; break; }
                            int floor = System.Math.Max(keep, lineStart);
                            while (sb.Length > floor && sb[sb.Length - 1] == ' ') sb.Length--;
                        }
                        line++;
                        if (printLines.HasValue && line >= printLines.Value)
                        {
                            sb.Append(" ..");
                            for (int b = blocks.Count - 1; b >= 1; b--) sb.Append(ops[blocks[b].Open].Str);
                            return sb.ToString();
                        }
                        sb.Append('\n');
                        var top = blocks[blocks.Count - 1];
                        int ind = literal ? 0 : top.Indent;
                        if (ind > 0) sb.Append(' ', ind);
                        col = ind;
                        keep = sb.Length;
                        if (!literal)
                        {
                            top.SectionStartLine = line;
                            blocks[blocks.Count - 1] = top;
                        }
                        break;
                    }
                    case PpIndent:
                        if (!Misering())
                        {
                            var top = blocks[blocks.Count - 1];
                            top.Indent = System.Math.Max(0, (op.Kind == IndBlock ? top.Start : col) + op.Amount);
                            blocks[blocks.Count - 1] = top;
                        }
                        break;
                    case PpBlockStart:
                        if (Fits(i, op.SectionEnd))
                        {
                            // The whole block fits: print it as it is.
                            for (int j = i + 1; j < op.Match; j++)
                            {
                                var t = ops[j];
                                if (t.Op == PpText || t.Op == PpAtom)
                                {
                                    sb.Append(t.Str);
                                    col += t.Str!.Length;
                                    cur += t.Str.Length;
                                    if (t.Op == PpAtom) keep = sb.Length;
                                }
                            }
                            i = op.Match;
                        }
                        else
                        {
                            blocks.Add(new PpBlockState { Start = col, Indent = col, SectionStartLine = line, Open = i });
                        }
                        break;
                    case PpBlockEnd:
                        blocks.RemoveAt(blocks.Count - 1);
                        break;
                }
            }
            return sb.ToString();
        }
    }
}
