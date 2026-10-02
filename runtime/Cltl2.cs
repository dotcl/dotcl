// CLtL2 environment access: DOTCL-CLTL2 package.
//
// trivial-cltl2 :use's a per-implementation CLtL2 package (sb-cltl2 on SBCL,
// etc.); dotcl had none, so the exported symbols (define-declaration /
// variable-information / ...) were unbound and trivia failed to load
// ("Unbound variable: OPTIMIZER": define-declaration compiled as a plain call).
//
// dotcl's macro &environment carries no lexical information (always NIL), so a
// full CLtL2 implementation is impossible. This is a DEFENSIVE MINIMAL backend:
// every introspection call NEVER errors and degrades to "no information" (nil /
// safe defaults), so type-i / trivia balland2006 just skip type optimization and
// emit correct (unoptimized) code instead of failing to compile.
//
// The functions are registered here (C#) rather than in cil-stdlib.lisp because
// the SIL serialization of a defun whose name lives in a non-CL-USER package does
// not round-trip the home package, so the function ends up unbound at runtime.
// The define-declaration / compiler-let MACROS live in cil-macros.lisp.
namespace DotCL;

public static class Cltl2
{
    public static Package Cltl2Pkg { get; private set; } = null!;

    public static void Init()
    {
        Cltl2Pkg = new Package("DOTCL-CLTL2");
        // Intern + export all the symbols trivial-cltl2 imports, including the two
        // macro names (define-declaration / compiler-let) whose expanders are set
        // from cil-macros.lisp.
        foreach (var n in new[] {
            "COMPILER-LET", "VARIABLE-INFORMATION", "FUNCTION-INFORMATION",
            "DECLARATION-INFORMATION", "AUGMENT-ENVIRONMENT", "DEFINE-DECLARATION",
            "PARSE-MACRO", "ENCLOSE", "MACROEXPAND-ALL" })
        {
            var (s, _) = Cltl2Pkg.Intern(n);
            Cltl2Pkg.Export(s);
        }

        Reg("VARIABLE-INFORMATION", -1, VariableInformation);
        Reg("FUNCTION-INFORMATION", -1, FunctionInformation);
        Reg("DECLARATION-INFORMATION", -1, DeclarationInformation);
        Reg("AUGMENT-ENVIRONMENT", -1, AugmentEnvironment);
        Reg("PARSE-MACRO", -1, ParseMacro);
        Reg("ENCLOSE", -1, Enclose);
        Reg("MACROEXPAND-ALL", -1, MacroexpandAll);
    }

    /// <summary>(macroexpand-all form &amp;optional env). The walker itself is
    /// %MACROEXPAND-ALL in cil-stdlib.lisp: a code walker is far easier to get
    /// right in Lisp, and the SIL round-trip problem that keeps the other
    /// functions here only affects a defun whose NAME lives in this package.
    /// Resolved lazily because the core is loaded after Init runs.</summary>
    private static LispObject MacroexpandAll(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 2)
            throw new LispErrorException(new LispProgramError(
                $"MACROEXPAND-ALL: wrong number of arguments: {args.Length} (expected 1-2)"));
        var fn = Emitter.CilAssembler.TryGetFunction("%MACROEXPAND-ALL")
                 ?? throw new LispErrorException(new LispError(
                     "MACROEXPAND-ALL: the code walker is not loaded"));
        return fn.Invoke(new[] { args[0], args.Length > 1 ? args[1] : Nil.Instance });
    }

    private static void Reg(string name, int arity, Func<LispObject[], LispObject> fn)
    {
        var (sym, _) = Cltl2Pkg.Intern(name);
        sym.Function = new LispFunction(fn, "DOTCL-CLTL2:" + name, arity);
    }

    /// <summary>(values nil nil nil): no information. env is always NIL on dotcl.</summary>
    private static LispObject NoInfo()
    {
        MultipleValues.Set(Nil.Instance, Nil.Instance, Nil.Instance);
        return Nil.Instance;
    }

    // (variable-information variable &optional env) => kind, local-p, decl-alist.
    // dotcl's macro environments carry no lexical bindings, so this answers for
    // the global environment only: :CONSTANT, :SPECIAL or :SYMBOL-MACRO from the
    // symbol's global state, NIL when nothing is known. LOCAL-P is always NIL.
    public static LispObject VariableInformation(LispObject[] args)
    {
        if (args.Length < 1 || args.Length > 2)
            throw new LispErrorException(new LispProgramError(
                $"VARIABLE-INFORMATION: wrong number of arguments: {args.Length} (expected 1-2)"));
        LispObject kind = Nil.Instance;
        var v = args[0];
        if (v is Nil || v is T)
            kind = Startup.Keyword("CONSTANT");
        else if (v is Symbol sym)
        {
            if (sym.IsConstant || sym.HomePackage == Startup.KeywordPkg)
                kind = Startup.Keyword("CONSTANT");
            else if (sym.IsSpecial || IsCompilerGlobalSpecial(sym))
                kind = Startup.Keyword("SPECIAL");
            else if (IsGlobalSymbolMacro(sym))
                kind = Startup.Keyword("SYMBOL-MACRO");
        }
        else
            throw new LispErrorException(new LispTypeError(
                "VARIABLE-INFORMATION: not a symbol", v, Startup.Sym("SYMBOL")));
        MultipleValues.Set(kind, Nil.Instance, Nil.Instance);
        return kind;
    }

    /// <summary>Whether the compiler holds SYM globally special without the
    /// runtime flag being set yet: a DEFVAR or a (DECLAIM (SPECIAL ...)) earlier
    /// in the file COMPILE-FILE is compiling takes effect in the compiler at
    /// compile time, and the symbol is marked only when the fasl is loaded. A
    /// walker asking about a binding of such a variable in the same file has to
    /// hear :SPECIAL, as the compiler will bind it dynamically.</summary>
    private static bool IsCompilerGlobalSpecial(Symbol sym)
    {
        foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
        {
            var pkg = Package.FindPackage(pkgName);
            if (pkg == null) continue;
            var (listSym, status) = pkg.FindSymbol("*GLOBAL-SPECIALS*");
            if (listSym == null || status == SymbolStatus.None) continue;
            for (var cur = DynamicBindings.Get(listSym); cur is Cons c; cur = c.Cdr)
                if (ReferenceEquals(c.Car, sym)) return true;
            return false;
        }
        return false;
    }

    /// <summary>Whether SYM names a global symbol macro, as DEFINE-SYMBOL-MACRO
    /// records it: in the runtime's table once loaded, and in the compiler's
    /// table while only compiled so far.</summary>
    private static bool IsGlobalSymbolMacro(Symbol sym)
    {
        if (Runtime.TryGetGlobalSymbolMacro(sym, out _)) return true;
        foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
        {
            var pkg = Package.FindPackage(pkgName);
            if (pkg == null) continue;
            var (tableSym, status) = pkg.FindSymbol("*GLOBAL-SYMBOL-MACROS*");
            if (tableSym != null && status != SymbolStatus.None
                && tableSym.Value is LispHashTable table)
                return table.TryGet(sym, out _);
        }
        return false;
    }

    // (function-information function &optional env): likewise.
    public static LispObject FunctionInformation(LispObject[] args) => NoInfo();

    // (declaration-information decl-name &optional env). Standard OPTIMIZE gets a
    // neutral default; custom declarations (e.g. trivia's OPTIMIZER) return NIL so
    // consumers' (when-let ((it (...))) ...) skip them.
    public static LispObject DeclarationInformation(LispObject[] args)
    {
        if (args.Length >= 1 && args[0] is Symbol s && s.Name == "OPTIMIZE")
            return Runtime.List(
                Runtime.List(Startup.Sym("SPEED"), Fixnum.Make(1)),
                Runtime.List(Startup.Sym("SAFETY"), Fixnum.Make(1)),
                Runtime.List(Startup.Sym("DEBUG"), Fixnum.Make(1)),
                Runtime.List(Startup.Sym("SPACE"), Fixnum.Make(1)),
                Runtime.List(Startup.Sym("COMPILATION-SPEED"), Fixnum.Make(1)));
        return Nil.Instance;
    }

    // (augment-environment env &key ...): no env object to extend; return it
    // unchanged so later *-information on it stays "no info".
    public static LispObject AugmentEnvironment(LispObject[] args)
        => args.Length >= 1 ? args[0] : Nil.Instance;

    // (parse-macro name lambda-list body &optional env) => a macro-expander lambda
    //   (lambda (whole env) (declare (ignorable env))
    //     (destructuring-bind <lambda-list> (cdr whole) . <body>))
    public static LispObject ParseMacro(LispObject[] args)
    {
        var lambdaList = args.Length >= 2 ? args[1] : Nil.Instance;
        var body = args.Length >= 3 ? args[2] : Nil.Instance;
        var whole = new Symbol("WHOLE");
        var env = new Symbol("ENV");
        var dsb = new Cons(Startup.Sym("DESTRUCTURING-BIND"),
                   new Cons(lambdaList,
                    new Cons(Runtime.List(Startup.Sym("CDR"), whole), body)));
        return Runtime.List(
            Startup.Sym("LAMBDA"),
            Runtime.List(whole, env),
            Runtime.List(Startup.Sym("DECLARE"),
                Runtime.List(Startup.Sym("IGNORABLE"), env)),
            dsb);
    }

    // (enclose lambda-expression &optional env) => function
    public static LispObject Enclose(LispObject[] args)
    {
        if (args.Length < 1) return Nil.Instance;
        return Runtime.Eval(Runtime.List(Startup.Sym("FUNCTION"), args[0]));
    }
}
