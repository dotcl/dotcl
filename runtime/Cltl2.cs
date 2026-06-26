// CLtL2 environment access — DOTCL-CLTL2 package.
//
// trivial-cltl2 :use's a per-implementation CLtL2 package (sb-cltl2 on SBCL,
// etc.); dotcl had none, so the exported symbols (define-declaration /
// variable-information / ...) were unbound and trivia failed to load
// ("Unbound variable: OPTIMIZER" — define-declaration compiled as a plain call).
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
            "PARSE-MACRO", "ENCLOSE" })
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

    // (variable-information variable &optional env) — no lexical env, so no info.
    public static LispObject VariableInformation(LispObject[] args) => NoInfo();

    // (function-information function &optional env) — likewise.
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

    // (augment-environment env &key ...) — no env object to extend; return it
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
