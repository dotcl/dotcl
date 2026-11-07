using System;
using System.Collections.Generic;

namespace DotCL;

public static partial class Runtime
{
    private sealed class CompilerUnit
    {
        public readonly string? File;
        public readonly List<UndefinedUse> Variables = new();
        public readonly List<UndefinedUse> Functions = new();
        public readonly List<Symbol> Bound = new();
        public LispObject Context = Nil.Instance;
        public Cons[]? ContextCells;
        public LispObject? TopForm;
        public int TopLine;
        public int TopPosition = -1;
        public LispObject? Form;
        public int FormLine;
        public int FormPosition = -1;

        public CompilerUnit(string? file) => File = file;
    }

    private sealed class UndefinedUse
    {
        public readonly LispObject Name;
        public readonly LispObject? Form;
        public readonly LispObject? TopForm;
        public readonly int Line;
        public readonly int Position;
        public readonly int TopLine;
        public readonly int TopPosition;

        public UndefinedUse(LispObject name, LispObject? form, CompilerUnit unit, int line, int position)
        {
            Name = name;
            Form = form;
            TopForm = unit.TopForm;
            Line = line;
            Position = position;
            TopLine = unit.TopLine;
            TopPosition = unit.TopPosition;
        }
    }

    [ThreadStatic] private static List<CompilerUnit>? s_compilerUnits;

    private static CompilerUnit? CurrentCompilerUnit =>
        s_compilerUnits is { Count: > 0 } l ? l[l.Count - 1] : null;

    private static Symbol? CompilerSym(string name, string package)
    {
        try { return Startup.SymInPkg(name, package); }
        catch { return null; }
    }

    private static Symbol? CompilerVarSym(string name)
    {
        try
        {
            var sym = Startup.Sym(name);
            return sym.HomePackage?.Name == "COMMON-LISP" ? null : sym;
        }
        catch { return null; }
    }

    private static LispObject IntOrNil(int n) => n > 0 ? Fixnum.Make(n) : Nil.Instance;

    private const int CtxFile = 0, CtxLine = 1, CtxPosition = 2, CtxSubformLine = 3,
        CtxSubformPosition = 4, CtxTopLevelLine = 5, CtxTopLevelPosition = 6, CtxForm = 7,
        CtxTopLevelForm = 8;

    private static readonly string[] s_contextKeys =
    {
        "FILE", "LINE", "POSITION", "SUBFORM-LINE", "SUBFORM-POSITION",
        "TOP-LEVEL-LINE", "TOP-LEVEL-POSITION", "FORM", "TOP-LEVEL-FORM"
    };

    private static Symbol[]? s_contextKeywords;

    private static void PutContext(CompilerUnit u, int slot, LispObject value)
    {
        if (u.ContextCells != null) u.ContextCells[slot].Car = value;
    }

    private static LispObject ContextValue(CompilerUnit u, int slot) => slot switch
    {
        CtxFile => u.File is null ? Nil.Instance : new LispString(u.File),
        CtxLine or CtxTopLevelLine => IntOrNil(u.TopLine),
        CtxPosition or CtxTopLevelPosition => u.TopPosition >= 0 ? Fixnum.Make(u.TopPosition) : Nil.Instance,
        CtxSubformLine => IntOrNil(u.FormLine),
        CtxSubformPosition => u.FormPosition >= 0 ? Fixnum.Make(u.FormPosition) : Nil.Instance,
        CtxForm => u.Form ?? Nil.Instance,
        _ => u.TopForm ?? Nil.Instance,
    };

    private static LispObject MakeContext(CompilerUnit u, bool keepCells)
    {
        var keywords = s_contextKeywords;
        if (keywords == null)
        {
            keywords = new Symbol[s_contextKeys.Length];
            for (int i = 0; i < keywords.Length; i++) keywords[i] = Startup.Keyword(s_contextKeys[i]);
            s_contextKeywords = keywords;
        }
        var cells = keepCells ? new Cons[keywords.Length] : null;
        LispObject result = Nil.Instance;
        for (int i = keywords.Length - 1; i >= 0; i--)
        {
            var valueCell = new Cons(ContextValue(u, i), result);
            result = new Cons(keywords[i], valueCell);
            if (cells != null) cells[i] = valueCell;
        }
        if (cells != null) u.ContextCells = cells;
        return result;
    }

    private static bool TrySpan(LispObject form, out int line, out int position)
    {
        line = 0;
        position = -1;
        if (form is not Cons) return false;
        if (SourceLineTable is { } lines && lines.TryGetValue(form, out var span))
        {
            line = span.sl;
            if (SourcePositionTable is { } positions && positions.TryGetValue(form, out var p))
                position = p.start;
            return true;
        }
        return false;
    }

    private static LispFunction? s_noteCompilerFormFn, s_noteUndefinedFn;

    internal static void BeginCompilerUnit(string? file)
    {
        s_compilerUnits ??= new List<CompilerUnit>();
        var parent = CurrentCompilerUnit;
        var unit = new CompilerUnit(file ?? parent?.File);
        unit.Context = MakeContext(unit, keepCells: true);
        s_compilerUnits.Add(unit);

        var contextSym = CompilerSym("*COMPILER-CONTEXT*", "DOTCL");
        if (contextSym != null)
        {
            DynamicBindings.Push(contextSym, unit.Context);
            unit.Bound.Add(contextSym);
        }
        var noteSym = CompilerVarSym("*COMPILER-SOURCE-NOTE*");
        if (noteSym != null)
        {
            DynamicBindings.Push(noteSym, s_noteCompilerFormFn ??=
                new LispFunction(NoteCompilerForm, "%COMPILER-NOTE-FORM", -1).WithArgCount(1, 2));
            unit.Bound.Add(noteSym);
        }
        var undefinedSym = CompilerVarSym("*COMPILER-NOTE-UNDEFINED*");
        if (undefinedSym != null)
        {
            DynamicBindings.Push(undefinedSym, s_noteUndefinedFn ??=
                new LispFunction(NoteUndefined, "%COMPILER-NOTE-UNDEFINED", -1).WithArgCount(3, 3));
            unit.Bound.Add(undefinedSym);
        }
    }

    internal static void EndCompilerUnit()
    {
        if (s_compilerUnits is not { Count: > 0 } units) return;
        var unit = units[units.Count - 1];
        for (int i = unit.Bound.Count - 1; i >= 0; i--)
            DynamicBindings.Pop(unit.Bound[i]);
        units.RemoveAt(units.Count - 1);
    }

    internal static void SetCompilerTopLevel(LispObject form, int fallbackLine)
    {
        var unit = CurrentCompilerUnit;
        if (unit == null) return;
        int line = fallbackLine;
        int position = -1;
        if (TrySpan(form, out var spanLine, out var spanPosition))
        {
            line = spanLine;
            position = spanPosition;
        }
        unit.TopForm = form;
        unit.TopLine = line;
        unit.TopPosition = position;
        unit.Form = form;
        unit.FormLine = line;
        unit.FormPosition = position;
        PutContext(unit, CtxLine, IntOrNil(line));
        PutContext(unit, CtxPosition, position >= 0 ? Fixnum.Make(position) : Nil.Instance);
        PutContext(unit, CtxSubformLine, IntOrNil(line));
        PutContext(unit, CtxSubformPosition, position >= 0 ? Fixnum.Make(position) : Nil.Instance);
        PutContext(unit, CtxTopLevelLine, IntOrNil(line));
        PutContext(unit, CtxTopLevelPosition, position >= 0 ? Fixnum.Make(position) : Nil.Instance);
        PutContext(unit, CtxForm, form);
        PutContext(unit, CtxTopLevelForm, form);
    }

    private static LispObject NoteCompilerForm(LispObject[] args)
    {
        var unit = CurrentCompilerUnit;
        if (unit == null || args.Length == 0) return Nil.Instance;
        var form = args[0];
        bool top = args.Length > 1 && args[1] is not Nil;
        if (top)
        {
            SetCompilerTopLevel(form, unit.TopLine);
            return Nil.Instance;
        }
        if (!TrySpan(form, out var line, out var position)) return Nil.Instance;
        unit.Form = form;
        unit.FormLine = line;
        unit.FormPosition = position;
        PutContext(unit, CtxSubformLine, IntOrNil(line));
        PutContext(unit, CtxSubformPosition, position >= 0 ? Fixnum.Make(position) : Nil.Instance);
        PutContext(unit, CtxForm, form);
        return Nil.Instance;
    }

    private static LispObject NoteUndefined(LispObject[] args)
    {
        var unit = CurrentCompilerUnit;
        if (unit == null || args.Length < 3) return Nil.Instance;
        bool variable = args[0] is Symbol kind
                        && kind.Name == "VARIABLE";
        var name = args[1];
        var form = args[2] as Cons;
        var list = variable ? unit.Variables : unit.Functions;
        foreach (var u in list)
            if (ReferenceEquals(u.Name, name)) return Nil.Instance;
        int line = unit.FormLine;
        int position = unit.FormPosition;
        if (form == null || !TrySpan(form, out line, out position))
        {
            form = unit.Form as Cons;
            line = unit.FormLine;
            position = unit.FormPosition;
        }
        list.Add(new UndefinedUse(name, form, unit, line, position));
        return Nil.Instance;
    }

    private static void ApplyUse(CompilerUnit unit, UndefinedUse use)
    {
        unit.TopForm = use.TopForm;
        unit.TopLine = use.TopLine;
        unit.TopPosition = use.TopPosition;
        unit.Form = use.Form;
        unit.FormLine = use.Line;
        unit.FormPosition = use.Position;
        PutContext(unit, CtxLine, IntOrNil(use.TopLine));
        PutContext(unit, CtxPosition, use.TopPosition >= 0 ? Fixnum.Make(use.TopPosition) : Nil.Instance);
        PutContext(unit, CtxSubformLine, IntOrNil(use.Line));
        PutContext(unit, CtxSubformPosition, use.Position >= 0 ? Fixnum.Make(use.Position) : Nil.Instance);
        PutContext(unit, CtxTopLevelLine, IntOrNil(use.TopLine));
        PutContext(unit, CtxTopLevelPosition, use.TopPosition >= 0 ? Fixnum.Make(use.TopPosition) : Nil.Instance);
        PutContext(unit, CtxForm, use.Form ?? Nil.Instance);
        PutContext(unit, CtxTopLevelForm, use.TopForm ?? Nil.Instance);
    }

    // Undefined names a file's compilation found while a WITH-COMPILATION-UNIT is open: they
    // are reported when that unit ends (CLHS 3.2.5), once per name, and only if the name is
    // still undefined then.
    [ThreadStatic] private static List<(bool Variable, LispObject Name)>? s_deferredUndefined;

    private static bool CompilationUnitOpen()
    {
        var depthSym = Startup.Sym("*COMPILATION-UNIT-DEPTH*");
        return DynamicBindings.TryGet(depthSym, out var d) && d is Fixnum { Value: > 0 };
    }

    /// <summary>WITH-COMPILATION-UNIT's end: warn about the names still undefined.</summary>
    internal static LispObject FlushDeferredUndefined()
    {
        var pending = s_deferredUndefined;
        s_deferredUndefined = null;
        if (pending == null) return Nil.Instance;
        var warnSym = CompilerSym("%COMPILER-WARN-UNDEFINED", "DOTCL.CIL-COMPILER");
        if (warnSym?.Function is not LispFunction warn) return Nil.Instance;
        var seen = new HashSet<(bool, LispObject)>();
        foreach (var (isVar, name) in pending)
        {
            if (!seen.Add((isVar, name))) continue;
            if (!isVar && IsTruthy(FboundpImpl(name))) continue;
            if (isVar && name is Symbol vs && vs.IsBound) continue;
            warn.Invoke(new LispObject[] { isVar ? Startup.Keyword("VARIABLE") : Startup.Keyword("FUNCTION"), name });
        }
        return Nil.Instance;
    }

    internal static void FlushCompilerWarnings()
    {
        var unit = CurrentCompilerUnit;
        if (unit == null || (unit.Variables.Count == 0 && unit.Functions.Count == 0)) return;
        if (CompilationUnitOpen())
        {
            var list = s_deferredUndefined ??= new List<(bool, LispObject)>();
            foreach (var use in unit.Variables) list.Add((true, use.Name));
            foreach (var use in unit.Functions) list.Add((false, use.Name));
            unit.Variables.Clear();
            unit.Functions.Clear();
            return;
        }
        var warnSym = CompilerSym("%COMPILER-WARN-UNDEFINED", "DOTCL.CIL-COMPILER");
        if (warnSym?.Function is not LispFunction warn) return;
        var variable = Startup.Keyword("VARIABLE");
        var function = Startup.Keyword("FUNCTION");
        foreach (var use in unit.Variables)
        {
            ApplyUse(unit, use);
            warn.Invoke(new LispObject[] { variable, use.Name });
        }
        foreach (var use in unit.Functions)
        {
            if (IsTruthy(FboundpImpl(use.Name))) continue;
            ApplyUse(unit, use);
            warn.Invoke(new LispObject[] { function, use.Name });
        }
        unit.Variables.Clear();
        unit.Functions.Clear();
    }

    /// <summary>Snapshot the current compiler context onto CONDITION.</summary>
    internal static void CaptureCompilerSource(LispCondition condition)
    {
        if (condition.CompilerSource != null) return;
        var unit = CurrentCompilerUnit;
        if (unit != null) condition.CompilerSource = MakeContext(unit, keepCells: false);
    }

    /// <summary><lispdoc>(dotcl:compiler-condition-source &amp;optional condition) -- The source position of a condition signalled during compilation, as the plist (:file PATH :line N :position N :subform-line N :subform-position N :top-level-line N :top-level-position N :form FORM :top-level-form FORM) that DOTCL:*COMPILER-CONTEXT* holds. With CONDITION, the position saved when that condition was signalled; otherwise the current compilation's. NIL outside a compilation.</lispdoc></summary>
    public static LispObject CompilerConditionSource(LispObject[] args)
    {
        if (args.Length > 0 && args[0] is LispCondition c && c.CompilerSource != null)
            return c.CompilerSource;
        var unit = CurrentCompilerUnit;
        if (unit != null) return unit.Context;
        var sym = CompilerSym("*COMPILER-CONTEXT*", "DOTCL");
        if (sym != null && sym.IsBound && sym.Value is not null and not Nil)
            return sym.Value;
        return Nil.Instance;
    }
}
