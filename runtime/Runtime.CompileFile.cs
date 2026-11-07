using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DotCL;

public static partial class Runtime
{
    /// <summary>The 8 hex digits that keep the module names (and so the load-time-value slot
    /// ids) of independently compiled FASLs apart: a hash of the source text, the output file
    /// name and the compiler version. The same source compiled to the same name gives the same
    /// suffix, which is what makes the FASL reproducible.</summary>
    internal static string ModuleNameSuffix(string inputPath, string outputPath)
    {
        using var sha = System.Security.Cryptography.SHA256.Create();
        var text = System.IO.File.ReadAllBytes(inputPath);
        var extra = System.Text.Encoding.UTF8.GetBytes(
            "\n" + System.IO.Path.GetFileName(outputPath) + "\n"
            + typeof(Runtime).Assembly.GetName().Version);
        var all = new byte[text.Length + extra.Length];
        Buffer.BlockCopy(text, 0, all, 0, text.Length);
        Buffer.BlockCopy(extra, 0, all, text.Length, extra.Length);
        var h = sha.ComputeHash(all);
        return BitConverter.ToString(h, 0, 4).Replace("-", "").ToLowerInvariant();
    }

    // Compile a single top-level form using the Lisp compiler
    // Cached at first use to prevent user code (e.g. SB-C::COMPILE-TOPLEVEL)
    // from overwriting the compiler's function in the flat name table.
    private static LispFunction? _cachedCompileTopLevel;
    private static LispFunction? _cachedCompileTopLevelEval;
    internal static LispObject CompileTopLevel(LispObject form)
    {
        if (!Compat.TryEnsureSufficientExecutionStackWithMargin())
            throw new LispErrorException(new LispStorageCondition("Stack overflow in compilation"));
        _cachedCompileTopLevel ??= DotCL.Emitter.CilAssembler.GetFunction("COMPILE-TOPLEVEL");
        return _cachedCompileTopLevel.Invoke1(form);
    }

    // Compile a top-level form whose calls and variable uses xref records under
    // the name XREFNAME gives: (:TOP-LEVEL-FORM file line source) or (:COMPILE name).
    internal static LispObject CompileTopLevel(LispObject form, LispObject xrefName)
    {
        var sym = Startup.Sym("*XREF-TOP-LEVEL-NAME*");
        DynamicBindings.Push(sym, xrefName);
        try { return CompileTopLevel(form); }
        finally { DynamicBindings.Pop(sym); }
    }

    // The xref registration compiled code makes: the caller and what its body
    // uses, printed into one string that DOTCL:%XREF-NOTE queues as it is.
    public static LispObject XrefNote(string packed)
    {
        var sym = Startup.SymInPkg("%XREF-NOTE", "DOTCL");
        if (sym?.Function is LispFunction fn) fn.Invoke1(new LispString(packed));
        return Nil.Instance;
    }

    // (:TOP-LEVEL-FORM file line source): SOURCE is the form as read, before
    // FlattenTopLevel expands it.
    internal static LispObject TopLevelFormXrefName(string path, int line, LispObject source)
        => new Cons(Startup.Keyword("TOP-LEVEL-FORM"),
                    new Cons(new LispString(MapSourcePath(Path.GetFullPath(path))),
                             new Cons(Fixnum.Make(line), new Cons(source, Nil.Instance))));

    // Compile a top-level form for EVAL (preserves MvReturn in tail position).
    internal static LispObject CompileTopLevelEval(LispObject form)
    {
        if (!Compat.TryEnsureSufficientExecutionStackWithMargin())
            throw new LispErrorException(new LispStorageCondition("Stack overflow in compilation"));
        _cachedCompileTopLevelEval ??= DotCL.Emitter.CilAssembler.GetFunction("COMPILE-TOPLEVEL-EVAL");
        return _cachedCompileTopLevelEval.Invoke1(form);
    }

    // --- Compile-file ---

    /// <summary>
    /// Returns true if a top-level form should be executed at compile time.
    /// Per CLHS, compile-file should only execute forms that need compile-time
    /// side effects (defmacro, defvar, in-package, etc.), not defun/defmethod.
    /// </summary>
    /// <summary>Check if a symbol name denotes a form with compile-time side effects.</summary>
    private static bool IsCompileTimeSideEffectForm(string name) => name switch
    {
        "IN-PACKAGE" or "DEFPACKAGE" or "DEFMACRO" or "DEFINE-COMPILER-MACRO"
        or "DECLAIM" or "PROCLAIM" or "USE-PACKAGE" or "SHADOW"
        or "SHADOWING-IMPORT" or "EXPORT" or "IMPORT" or "REQUIRE" or "PROVIDE"
        or "DEFTYPE"  // CLHS: deftype has compile-time effects (type name available during compilation)
        // DEFCLASS is not here. CLHS 7.7 (DEFCLASS) asks the compiler only to
        // recognize the name as a type and class name for later declarations,
        // specializers and :METACLASS options; it does not ask for the class
        // to exist. Running the whole form at compile time created the class
        // (and its metaclass's INITIALIZE-INSTANCE ran) during COMPILE-FILE,
        // so the LOAD that follows reinitialized it instead of creating it.
        // dotcl resolves class names at load time, so leaving the form to be
        // macroexpanded and emitted like any other is enough. SBCL does not
        // create the class at compile time either.
        or "DEFINE-CONDITION"  // CLHS: define-condition has compile-time effects (class name available for find-class during compilation)
        // CLHS 3.2.3.1: DEFCONSTANT's name must be recognized as a constant
        // variable while the rest of the file is compiled, and every
        // implementation makes the VALUE readable there too -- which is what a
        // macro expander in the same file needs. series keeps a list of special
        // operators in a DEFCONSTANT and reads it from a macro expander a few
        // hundred lines later; without this that read said "Unbound variable".
        or "DEFCONSTANT"
            => true,
        _ => false
    };

    /// <summary>EXPORT, IMPORT, SHADOW, SHADOWING-IMPORT and USE-PACKAGE have no
    /// compile-time effect in CLHS 3.2.3.1; COMPILE-FILE evaluates them early only
    /// as a convenience for older code. Their arguments are ordinary forms that may
    /// need the file to be loaded first, e.g. (defvar *sep* '^) (export *sep*),
    /// where the variable has no value until load. An error there is therefore not
    /// the file's error: it is dropped, and the form still runs at load time, where
    /// a real error is reported.</summary>
    private static bool IsCourtesyCompileTimeForm(LispObject form) =>
        form is Cons c && c.Car is Symbol s
        && s.Name is "EXPORT" or "IMPORT" or "SHADOW" or "SHADOWING-IMPORT" or "USE-PACKAGE";

    /// <summary>Run a top-level DECLAIM, PROCLAIM or package operation whose
    /// arguments are all constants by calling the function itself, and answer
    /// true; answer false for anything else. What the compiled form would do is
    /// exactly that call, and assembling and JIT-compiling a method to make one
    /// call cost far more than the call: a file that defines many small inline
    /// functions has several such forms per definition.
    ///
    /// DECLAIM runs PROCLAIM on the specs COMPILE-DECLAIM-FORM puts a load-time
    /// PROCLAIM in for (SPECIAL, DECLARATION, INLINE, NOTINLINE, OPTIMIZE), in
    /// order; the other specs have only their compile-time half, which the
    /// compiler has already applied.</summary>
    private static bool TryRunConstantCompileTimeForm(LispObject form)
    {
        if (form is not Cons c || c.Car is not Symbol op) return false;
        if (ReferenceEquals(op, Startup.Sym("DECLAIM")))
        {
            if (Startup.Sym("PROCLAIM").Function is not LispFunction proclaim) return false;
            for (var s = c.Cdr; s is Cons sc; s = sc.Cdr)
            {
                if (sc.Car is not Cons spec || spec.Car is not Symbol kind) continue;
                if (ReferenceEquals(kind, Startup.Sym("SPECIAL"))
                    || ReferenceEquals(kind, Startup.Sym("DECLARATION"))
                    || ReferenceEquals(kind, Startup.Sym("INLINE"))
                    || ReferenceEquals(kind, Startup.Sym("NOTINLINE"))
                    || ReferenceEquals(kind, Startup.Sym("OPTIMIZE")))
                    proclaim.Invoke(spec);
            }
            return true;
        }
        if (!(ReferenceEquals(op, Startup.Sym("PROCLAIM"))
              || ReferenceEquals(op, Startup.Sym("EXPORT"))
              || ReferenceEquals(op, Startup.Sym("IMPORT"))
              || ReferenceEquals(op, Startup.Sym("SHADOW"))
              || ReferenceEquals(op, Startup.Sym("SHADOWING-IMPORT"))
              || ReferenceEquals(op, Startup.Sym("USE-PACKAGE"))))
            return false;
        if (op.Function is not LispFunction fn) return false;
        var args = new List<LispObject>();
        var quote = Startup.Sym("QUOTE");
        for (var a = c.Cdr; ; a = ((Cons)a).Cdr)
        {
            if (a is Nil) break;
            if (a is not Cons ac) return false;
            var x = ac.Car;
            if (x is Cons q)
            {
                if (!ReferenceEquals(q.Car, quote) || q.Cdr is not Cons qc || qc.Cdr is not Nil)
                    return false;
                args.Add(qc.Car);
            }
            else if (x is Symbol xs && xs.HomePackage?.Name != "KEYWORD")
                return false;
            else
                args.Add(x);
        }
        fn.Invoke(args.ToArray());
        return true;
    }

    private static void RunCompileTimeSideEffect(LispObject form, LispObject instrList)
    {
        if (!IsCourtesyCompileTimeForm(form))
        {
            if (!TryRunConstantCompileTimeForm(form))
                DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
            return;
        }
        // Intercept at the signal point, before any outer handler (ASDF's, the
        // user's) sees an error that the language does not say happens here.
        var tag = new object();
        var handler = new LispFunction(
            hargs => throw new HandlerCaseInvocationException(
                tag, 0, hargs.Length > 0 ? hargs[0] : Nil.Instance),
            "%COMPILE-TIME-PACKAGE-OPERATION", -1);
        HandlerClusterStack.PushCluster(new[] { new HandlerBinding(Startup.Sym("ERROR"), handler) });
        try
        {
            if (!TryRunConstantCompileTimeForm(form))
                DotCL.Emitter.CilAssembler.AssembleAndRun(instrList);
        }
        catch (HandlerCaseInvocationException ex) when (ReferenceEquals(ex.Tag, tag))
        {
        }
        finally
        {
            HandlerClusterStack.PopCluster();
        }
    }

    private static bool ShouldExecuteAtCompileTime(LispObject form)
    {
        if (UnwrapTopLevelScope(form) is Cons c && c.Car is Symbol sym)
        {
            return IsCompileTimeSideEffectForm(sym.Name);
        }
        return false;
    }

    /// <summary>The one form inside a top level LOCALLY, MACROLET or
    /// SYMBOL-MACROLET that holds a single body form, looking through nested
    /// ones; FORM itself otherwise. FlattenTopLevel leaves each body form of such
    /// a scope wrapped in its own copy of it, and the body form keeps its compile
    /// time side effects there (CLHS 3.2.3.1): a DEFTYPE inside
    /// (locally (declare ...) (symbol-macrolet (...) ...)) names a type for the
    /// rest of the file. Running the wrapped form runs the definition under the
    /// declarations and local macros it was written with.</summary>
    private static LispObject UnwrapTopLevelScope(LispObject form)
    {
        for (int depth = 0; depth < 64; depth++)
        {
            if (form is not Cons c || c.Car is not Symbol sym) return form;
            LispObject body;
            if (sym.Name == "LOCALLY") body = c.Cdr;
            else if ((sym.Name == "MACROLET" || sym.Name == "SYMBOL-MACROLET")
                     && c.Cdr is Cons mrest) body = mrest.Cdr;
            else return form;
            while (body is Cons bc && bc.Car is Cons dc
                   && dc.Car is Symbol dsym && dsym.Name == "DECLARE")
                body = bc.Cdr;
            if (body is not Cons only || only.Cdr is not Nil) return form;
            form = only.Car;
        }
        return form;
    }

    /// <summary>
    /// Check if form is (eval-when (...) body...) and parse the situations.
    /// For compile-file: :compile-toplevel or :load-toplevel without :execute
    /// needs special handling because the compiler only emits code for :execute.
    /// </summary>
    private static bool IsEvalWhenForCompileFile(LispObject form,
        out LispObject body, out bool hasCompileToplevel, out bool hasLoadToplevel)
    {
        body = Nil.Instance;
        hasCompileToplevel = false;
        hasLoadToplevel = false;
        if (form is not Cons c || c.Car is not Symbol sym || sym.Name != "EVAL-WHEN")
            return false;
        if (c.Cdr is not Cons rest || rest.Car is not Cons situations)
            return false;

        for (var sit = situations; sit != null && sit is Cons sc; sit = sc.Cdr as Cons)
        {
            if (sc.Car is Symbol s)
            {
                if (s.Name == "COMPILE-TOPLEVEL") hasCompileToplevel = true;
                else if (s.Name == "LOAD-TOPLEVEL") hasLoadToplevel = true;
                // :EXECUTE is irrelevant at top level in compile-file (Figure 3-7).
            }
        }

        // Per CLHS 3.2.3.1 (Figure 3-7), at top level in compile-file the
        // behavior is determined solely by :compile-toplevel (eval now) and
        // :load-toplevel (place in fasl). :execute is irrelevant here and does
        // NOT promote the body into the fasl: e.g. {:compile-toplevel :execute}
        // is evaluated at compile time and discarded, NOT written to the fasl
        // (promoting it on :execute leaked the body into the load image).
        // A bare {:execute} (no ct/lt) is handled by the Lisp compile-eval-when
        // (returns here false) and discarded.
        if (hasCompileToplevel || hasLoadToplevel)
        {
            body = rest.Cdr ?? Nil.Instance;
            return true;
        }
        return false;
    }

    /// <summary>True when an EVAL-WHEN form lists :EXECUTE (or EVAL) among its situations.</summary>
    private static bool EvalWhenHasExecute(LispObject form)
    {
        if (form is not Cons c || c.Cdr is not Cons rest) return false;
        for (var sit = rest.Car; sit is Cons sc; sit = sc.Cdr)
            if (sc.Car is Symbol s && (s.Name == "EXECUTE" || s.Name == "EVAL")) return true;
        return false;
    }

    /// <summary>(eval-when (:execute) body...): :EXECUTE without :COMPILE-TOPLEVEL
    /// or :LOAD-TOPLEVEL (IsEvalWhenForCompileFile answers false for it).</summary>
    private static bool IsExecuteOnlyEvalWhen(LispObject form, out LispObject body)
    {
        body = Nil.Instance;
        if (form is not Cons c || c.Car is not Symbol sym || sym.Name != "EVAL-WHEN") return false;
        if (c.Cdr is not Cons rest || !EvalWhenHasExecute(form)) return false;
        body = rest.Cdr ?? Nil.Instance;
        return true;
    }

    /// <summary>
    /// Build (progn form1 form2 ...) from a list of forms.
    /// </summary>
    private static LispObject MakeProgn(LispObject formList)
    {
        return new Cons(Startup.Sym("PROGN"), formList);
    }

    /// <summary>
    /// Register proper macro expanders for WHEN, UNLESS, COND, AND, OR so that
    /// macroexpand-1 returns their actual expansions. Code walkers (e.g. iterate)
    /// need this to see inside these forms.
    /// </summary>
    private static void RegisterCompileFormExpanders()
    {
        var IF = Startup.Sym("IF");
        var NOT = Startup.Sym("NOT");
        var PROGN = Startup.Sym("PROGN");
        var AND = Startup.Sym("AND");
        var OR = Startup.Sym("OR");
        var LET = Startup.Sym("LET");

        // WHEN: (when test . body) -> (if test (progn . body))
        Runtime.RegisterMacroFunction(Startup.Sym("WHEN"), new LispFunction(args => {
            Runtime.CheckArityExact("WHEN-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons form || form.Cdr is not Cons rest)
                return args[0];
            var compilerFn = Startup.LookupCompilerMacro(Startup.Sym("WHEN"));
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var test = rest.Car;
            var body = rest.Cdr;
            var progn = new Cons(PROGN, body);
            return new Cons(IF, new Cons(test, new Cons(progn, Nil.Instance)));
        }, "WHEN-MACRO-EXPANDER", 2));

        // UNLESS: (unless test . body) -> (if (not test) (progn . body))
        Runtime.RegisterMacroFunction(Startup.Sym("UNLESS"), new LispFunction(args => {
            Runtime.CheckArityExact("UNLESS-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons form || form.Cdr is not Cons rest)
                return args[0];
            var compilerFn = Startup.LookupCompilerMacro(Startup.Sym("UNLESS"));
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var test = rest.Car;
            var body = rest.Cdr;
            var notTest = new Cons(NOT, new Cons(test, Nil.Instance));
            var progn = new Cons(PROGN, body);
            return new Cons(IF, new Cons(notTest, new Cons(progn, Nil.Instance)));
        }, "UNLESS-MACRO-EXPANDER", 2));

        // AND: (and) -> t  (and x) -> x  (and x . rest) -> (if x (and . rest) nil)
        Runtime.RegisterMacroFunction(AND, new LispFunction(args => {
            Runtime.CheckArityExact("AND-MACRO-EXPANDER", args, 2);
            var compilerFn = Startup.LookupCompilerMacro(AND);
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var form = args[0] as Cons;
            if (form == null) return args[0];
            var rest = form.Cdr;
            if (rest is Nil) return T.Instance; // (and) -> t
            if (rest is Cons c1)
            {
                if (c1.Cdr is Nil) return c1.Car; // (and x) -> x
                // (and x . rest) -> (if x (and . rest) nil)
                var andRest = new Cons(AND, c1.Cdr);
                return new Cons(IF, new Cons(c1.Car, new Cons(andRest, new Cons(Nil.Instance, Nil.Instance))));
            }
            return args[0];
        }, "AND-MACRO-EXPANDER", 2));

        // OR: (or) -> nil  (or x) -> x  (or x . rest) -> (let ((#:g x)) (if #:g #:g (or . rest)))
        Runtime.RegisterMacroFunction(OR, new LispFunction(args => {
            Runtime.CheckArityExact("OR-MACRO-EXPANDER", args, 2);
            var compilerFn = Startup.LookupCompilerMacro(OR);
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var form = args[0] as Cons;
            if (form == null) return args[0];
            var rest = form.Cdr;
            if (rest is Nil) return Nil.Instance; // (or) -> nil
            if (rest is Cons c1)
            {
                if (c1.Cdr is Nil) return c1.Car; // (or x) -> x
                var gSym = (Symbol)Runtime.Gensym(new LispString("OR-VAL"));
                var orRest = new Cons(OR, c1.Cdr);
                var binding = new Cons(new Cons(gSym, new Cons(c1.Car, Nil.Instance)), Nil.Instance);
                var ifForm = new Cons(IF, new Cons(gSym, new Cons(gSym, new Cons(orRest, Nil.Instance))));
                return new Cons(LET, new Cons(binding, new Cons(ifForm, Nil.Instance)));
            }
            return args[0];
        }, "OR-MACRO-EXPANDER", 2));

        // COND: (cond) -> nil
        // (cond (test) . rest) -> (let ((#:g test)) (if #:g #:g (cond . rest)))
        // (cond (test . body) . rest) -> (if test (progn . body) (cond . rest))
        Runtime.RegisterMacroFunction(Startup.Sym("COND"), new LispFunction(args => {
            Runtime.CheckArityExact("COND-MACRO-EXPANDER", args, 2);
            var compilerFn = Startup.LookupCompilerMacro(Startup.Sym("COND"));
            if (compilerFn != null) return compilerFn.Invoke(new LispObject[] { args[0] });
            var form = args[0] as Cons;
            if (form == null) return args[0];
            var clauses = form.Cdr;
            // Every clause must be a list, also those after a T clause.
            for (LispObject cl = clauses; cl is Cons cc; cl = cc.Cdr)
                if (cc.Car is not Cons)
                    throw new LispErrorException(new LispTypeError(
                        "COND: clause is not a list", cc.Car, Startup.Sym("CONS")));
            if (clauses is Nil) return Nil.Instance; // (cond) -> nil
            if (clauses is not Cons c1) return args[0];
            var clause = c1.Car as Cons;
            if (clause == null) return args[0];
            var test = clause.Car;
            var body = clause.Cdr;
            var restClauses = new Cons(Startup.Sym("COND"), c1.Cdr);
            if (body is Nil)
            {
                // (cond (test) . rest) -> (let ((#:g test)) (if #:g #:g (cond . rest)))
                var gSym = (Symbol)Runtime.Gensym(new LispString("COND-VAL"));
                var binding = new Cons(new Cons(gSym, new Cons(test, Nil.Instance)), Nil.Instance);
                var ifForm = new Cons(IF, new Cons(gSym, new Cons(gSym, new Cons(restClauses, Nil.Instance))));
                return new Cons(LET, new Cons(binding, new Cons(ifForm, Nil.Instance)));
            }
            var thenForm = new Cons(PROGN, body);
            return new Cons(IF, new Cons(test, new Cons(thenForm, new Cons(restClauses, Nil.Instance))));
        }, "COND-MACRO-EXPANDER", 2));

        // LAMBDA: (lambda . rest) -> (function (lambda . rest)), as CLHS specifies.
        // The compiler handles LAMBDA forms directly, but MACROEXPAND-1 used to
        // return the form unchanged, so code walkers (iterate, for one) saw a
        // macro that "won't expand" and skipped its body.
        Runtime.RegisterMacroFunction(Startup.Sym("LAMBDA"), new LispFunction(args => {
            Runtime.CheckArityExact("LAMBDA-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons) return args[0];
            return new Cons(Startup.Sym("FUNCTION"), new Cons(args[0], Nil.Instance));
        }, "LAMBDA-MACRO-EXPANDER", 2));

        // MULTIPLE-VALUE-LIST expands as SBCL does; walkers drive MACROEXPAND-1 themselves.
        Runtime.RegisterMacroFunction(Startup.Sym("MULTIPLE-VALUE-LIST"), new LispFunction(args => {
            Runtime.CheckArityExact("MULTIPLE-VALUE-LIST-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons form || form.Cdr is not Cons arg || arg.Cdr is not Nil)
                return args[0];
            var listFn = new Cons(Startup.Sym("FUNCTION"), new Cons(Startup.Sym("LIST"), Nil.Instance));
            return new Cons(Startup.Sym("MULTIPLE-VALUE-CALL"),
                            new Cons(listFn, new Cons(arg.Car, Nil.Instance)));
        }, "MULTIPLE-VALUE-LIST-MACRO-EXPANDER", 2));

        // handler-bind / handler-case / restart-bind / restart-case are implemented as
        // compile-form handlers (the compiler dispatches them directly, before macro
        // expansion), but MACRO-FUNCTION reported them as macros while MACROEXPAND-1
        // returned them unexpanded: an inconsistency that breaks code walkers.
        // Register real expanders so MACROEXPAND-1 yields a portable, walkable, eval-able
        // equivalent (CLHS: macro-function non-nil => macroexpand-1 expands). The compiler
        // is unaffected: compile-form checks its handler table first, and the Lisp
        // find-macro-expander only consults runtime macro-functions for DOTCL-package
        // symbols, so these CL-package entries never divert compilation/analysis.
        var QUOTE = Startup.Sym("QUOTE");
        var LIST = Startup.Sym("LIST");
        var CONS = Startup.Sym("CONS");
        var FUNCTION = Startup.Sym("FUNCTION");
        var LAMBDA = Startup.Sym("LAMBDA");
        var BLOCK = Startup.Sym("BLOCK");
        var TAGBODY = Startup.Sym("TAGBODY");
        var GO = Startup.Sym("GO");
        var SETQ = Startup.Sym("SETQ");
        var LOCALLY = Startup.Sym("LOCALLY");
        var MVCALL = Startup.Sym("MULTIPLE-VALUE-CALL");
        var RETURN_FROM = Startup.Sym("RETURN-FROM");
        var UNWIND_PROTECT = Startup.Sym("UNWIND-PROTECT");
        var PUSH_HB = Startup.Sym("%PUSH-HANDLER-CLUSTER");
        var POP_HB = Startup.Sym("%POP-HANDLER-CLUSTER");
        var PUSH_RB = Startup.Sym("%PUSH-RESTART-CLUSTER");
        var POP_RB = Startup.Sym("%POP-RESTART-CLUSTER");

        // (handler-bind ((type fn)...) . body)
        //   -> (progn (%push-handler-cluster (list (cons 'type fn)...))
        //            (unwind-protect (progn . body) (%pop-handler-cluster)))
        // Body stays INLINE in a progn (not a lambda thunk), so code walkers can walk it
        //. HandlerClusterStack.Signal restores clusters in a finally, so the
        // unwind-protect pop stays balanced even on a non-local exit from a handler.
        Runtime.RegisterMacroFunction(Startup.Sym("HANDLER-BIND"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var pairs = new System.Collections.Generic.List<LispObject>();
            for (var b = rest.Car; b is Cons bc; b = bc.Cdr)
                if (bc.Car is Cons bd && bd.Cdr is Cons bf)
                    pairs.Add(Runtime.List(CONS, Runtime.List(QUOTE, bd.Car), bf.Car));
            var listForm = new Cons(LIST, Runtime.List(pairs.ToArray()));
            var bodyProgn = new Cons(PROGN, rest.Cdr);
            var uw = Runtime.List(UNWIND_PROTECT, bodyProgn, Runtime.List(POP_HB));
            return Runtime.List(PROGN, Runtime.List(PUSH_HB, listForm), uw);
        }, "HANDLER-BIND-MACRO-EXPANDER", 2));

        // (restart-bind ((name fn . opts)...) . body)
        //   -> (progn (%push-restart-cluster
        //              (list (list 'name fn desc report test interactive)...))
        //            (unwind-protect (progn . body) (%pop-restart-cluster)))
        // Body inline.
        //
        // The options used to be DROPPED here, on the premise (recorded in the old
        // comment) that "restart options are kept by the compile-form handler; this
        // expansion is for macroexpand-1 / walkers". That premise is false for the
        // tree-walk interpreter: RESTART-CASE macroexpands into RESTART-BIND, which
        // macroexpands through HERE, so this is the real evaluation path whenever a
        // restart is established by EVAL: and the only one on emit-free builds.
        // Dropping the options meant an interpreted restart could not report itself
        // (printing gave "#<RESTART FOO>") and its :TEST never ran, so FIND-RESTART
        // returned a restart the test excludes (ansi-test RESTART-CASE.19/20/21).
        //
        // Option forms are spliced UNEVALUATED so they are evaluated in the
        // establishing environment, exactly like the function itself: :report may
        // be a closure over the surrounding scope. A string :report-function is the
        // description rather than a function (CLHS restart-case :report).
        Runtime.RegisterMacroFunction(Startup.Sym("RESTART-BIND"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var pairs = new System.Collections.Generic.List<LispObject>();
            for (var b = rest.Car; b is Cons bc; b = bc.Cdr)
                if (bc.Car is Cons bd && bd.Cdr is Cons bf)
                {
                    LispObject desc = Nil.Instance, report = Nil.Instance,
                               test = Nil.Instance, inter = Nil.Instance;
                    for (var o = bf.Cdr; o is Cons oc && oc.Cdr is Cons ov; o = ov.Cdr)
                    {
                        var key = (oc.Car as Symbol)?.Name;
                        var val = ov.Car;
                        // A bare symbol names the FUNCTION, not a variable (CLHS:
                        // the option takes a function designator). Splicing it raw
                        // made the interpreter read it as a variable: which only
                        // appeared to work while FLET bindings shared the variable
                        // alist. compile-restart-case does the same (function x)
                        // wrapping, so both paths agree.
                        LispObject AsFn(LispObject v) =>
                            v is Symbol
                                ? Runtime.List(FUNCTION, v) : v;
                        if (key == "REPORT-FUNCTION" || key == "REPORT")
                        {
                            if (val is LispString) desc = val; else report = AsFn(val);
                        }
                        else if (key == "TEST-FUNCTION" || key == "TEST") test = AsFn(val);
                        else if (key == "INTERACTIVE-FUNCTION" || key == "INTERACTIVE") inter = AsFn(val);
                    }
                    pairs.Add(Runtime.List(LIST, Runtime.List(QUOTE, bd.Car), bf.Car,
                                           desc, report, test, inter));
                }
            var listForm = new Cons(LIST, Runtime.List(pairs.ToArray()));
            var bodyProgn = new Cons(PROGN, rest.Cdr);
            var uw = Runtime.List(UNWIND_PROTECT, bodyProgn, Runtime.List(POP_RB));
            return Runtime.List(PROGN, Runtime.List(PUSH_RB, listForm), uw);
        }, "RESTART-BIND-MACRO-EXPANDER", 2));

        // (handler-case EXPR (type (var) . body)... [(:no-error ll . nbody)])
        //   -> (block #:b (let ((#:c nil)) (tagbody
        //        (handler-bind ((type (lambda (#:g) (setq #:c #:g) (go #:t)))...)
        //          (return-from #:b EXPR))   ; EXPR wrapped in mv-call of :no-error fn if present
        //        #:t (return-from #:b (let ((var #:c)) . body)) ...)))
        Runtime.RegisterMacroFunction(Startup.Sym("HANDLER-CASE"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var expr = rest.Car;
            var blk = (Symbol)Runtime.Gensym(new LispString("HC-BLOCK"));
            var cvar = (Symbol)Runtime.Gensym(new LispString("HC-COND"));
            LispObject noErrorClause = Nil.Instance;
            var handlerClauses = new System.Collections.Generic.List<Cons>();
            for (var c = rest.Cdr; c is Cons cc; c = cc.Cdr)
            {
                if (cc.Car is Cons clause)
                {
                    if (clause.Car is Symbol kw && kw.Name == "NO-ERROR") noErrorClause = clause;
                    else handlerClauses.Add(clause);
                }
            }
            var hbBindings = new System.Collections.Generic.List<LispObject>();
            var tagForms = new System.Collections.Generic.List<LispObject>();
            foreach (var clause in handlerClauses)
            {
                var type = clause.Car;
                var ll = (clause.Cdr is Cons cd) ? cd.Car : Nil.Instance;
                var cbody = (clause.Cdr is Cons cd2) ? cd2.Cdr : Nil.Instance;
                var tag = Runtime.Gensym(new LispString("HC-TAG"));
                var g = (Symbol)Runtime.Gensym(new LispString("HC-C"));
                // (lambda (#:g) (setq #:c #:g) (go #:tag))
                var hfn = Runtime.List(LAMBDA, Runtime.List(g),
                    Runtime.List(SETQ, cvar, g), Runtime.List(GO, tag));
                hbBindings.Add(Runtime.List(type, hfn));
                // var binding: (var #:c) or nothing
                LispObject clauseResult;
                // (error (nil) ...) names the constant NIL. NIL is not a Symbol
                // instance here, so it used to fall through to the no-variable
                // branch and be silently accepted; an EMPTY lambda list is the
                // real "no variable" spelling. CLHS 3.1.2.1.1.3.
                if (ll is Cons llcc && llcc.Car is Symbol cv0 && cv0.IsConstant)
                    throw new LispErrorException(new LispProgramError(
                        $"HANDLER-CASE: {cv0.Name} is a constant and cannot be bound as a variable"));
                if (ll is Cons llnil && llnil.Car is Nil)
                    throw new LispErrorException(new LispProgramError(
                        "HANDLER-CASE: NIL is a constant and cannot be bound as a variable"));
                if (ll is Cons llc && llc.Car is Symbol v)
                    clauseResult = new Cons(Startup.Sym("LET"),
                        new Cons(Runtime.List(Runtime.List(v, cvar)), cbody));
                else
                    clauseResult = new Cons(LOCALLY, cbody);
                tagForms.Add(tag);
                tagForms.Add(Runtime.List(RETURN_FROM, blk, clauseResult));
            }
            // protected form, wrapping with :no-error mv-call when present
            // CLHS HANDLER-CASE: the :NO-ERROR clause's body runs only after the
            // handlers are no longer active, so an error it signals must reach the
            // NEXT outer handler: not this form's own clauses. Wrapping EXPR in
            // MULTIPLE-VALUE-CALL of the :no-error function, as this expansion used
            // to, ran that body INSIDE the HANDLER-BIND, so
            //   (handler-case (handler-case (values)
            //                   (error () 'bad)
            //                   (:no-error () (error "foo")))
            //     (error () 'good))
            // answered BAD (ansi-test HANDLER-CASE.25). COMPILE-HANDLER-CASE gets
            // this right, so it showed only under EVAL.
            //
            // Instead stash EXPR's values, leave the HANDLER-BIND by GO, and apply
            // the :no-error function from a tagbody segment: the same place the
            // error clauses run, which is by construction outside the handlers.
            LispObject protectedReturn;
            Symbol? vvar = null;
            if (noErrorClause is Cons ne && ne.Cdr is Cons nec)
            {
                var nell = nec.Car; var nebody = nec.Cdr;
                vvar = (Symbol)Runtime.Gensym(new LispString("HC-VALS"));
                var neTag = Runtime.Gensym(new LispString("HC-NO-ERROR"));
                protectedReturn = Runtime.List(SETQ, vvar,
                    Runtime.List(Startup.Sym("MULTIPLE-VALUE-LIST"), expr));
                tagForms.Add(neTag);
                tagForms.Add(Runtime.List(RETURN_FROM, blk,
                    Runtime.List(Startup.Sym("APPLY"),
                        new Cons(LAMBDA, new Cons(nell, nebody)), vvar)));
                // The GO must sit AFTER the handler-bind, not inside it.
                protectedReturn = Runtime.List(PROGN,
                    new Cons(Startup.Sym("HANDLER-BIND"),
                        new Cons(Runtime.List(hbBindings.ToArray()),
                            new Cons(protectedReturn, Nil.Instance))),
                    Runtime.List(GO, neTag));
            }
            else
            {
                protectedReturn = new Cons(Startup.Sym("HANDLER-BIND"),
                    new Cons(Runtime.List(hbBindings.ToArray()),
                        new Cons(Runtime.List(RETURN_FROM, blk, expr), Nil.Instance)));
            }
            var tagbody = new Cons(TAGBODY,
                new Cons(protectedReturn, Runtime.List(tagForms.ToArray())));
            var letBindings = vvar == null
                ? Runtime.List(Runtime.List(cvar, Nil.Instance))
                : Runtime.List(Runtime.List(cvar, Nil.Instance),
                               Runtime.List(vvar, Nil.Instance));
            var letc = Runtime.List(Startup.Sym("LET"), letBindings, tagbody);
            return Runtime.List(BLOCK, blk, letc);
        }, "HANDLER-CASE-MACRO-EXPANDER", 2));

        // (restart-case EXPR (name (args...) [:report r][:interactive i][:test t] . body)...)
        //   -> (block #:b (let ((#:av nil)) (tagbody
        //        (return-from #:b
        //          (restart-bind ((name (lambda (&rest #:a) (setq #:av #:a) (go #:tag))
        //                          opts...)...) EXPR))
        //        #:tag (return-from #:b (apply (lambda (args...) . body) #:av)) ...)))
        //
        // When EXPR is a signaling form, it is additionally wrapped so the restarts
        // this form establishes are associated with the condition it signals; see
        // WrapSignalingRestartCaseBody below.
        Runtime.RegisterMacroFunction(Startup.Sym("RESTART-CASE"), new LispFunction(margs => {
            if (margs[0] is not Cons form || form.Cdr is not Cons rest) return margs[0];
            var expr = rest.Car;
            var mEnv = margs.Length > 1 ? margs[1] : Nil.Instance;
            var blk = (Symbol)Runtime.Gensym(new LispString("RC-BLOCK"));
            var avar = (Symbol)Runtime.Gensym(new LispString("RC-ARGS"));
            var REST = Startup.Sym("&REST");
            var APPLY = Startup.Sym("APPLY");
            var rbBindings = new System.Collections.Generic.List<LispObject>();
            var tagForms = new System.Collections.Generic.List<LispObject>();
            for (var c = rest.Cdr; c is Cons cc; c = cc.Cdr)
            {
                if (cc.Car is not Cons clause) continue;
                var name = clause.Car;
                var ll = (clause.Cdr is Cons cd) ? cd.Car : Nil.Instance;
                var cbody = (clause.Cdr is Cons cd2) ? cd2.Cdr : Nil.Instance;
                // Split leading :report/:interactive/:test keyword options off the body.
                var opts = new System.Collections.Generic.List<LispObject>();
                while (cbody is Cons bc && bc.Car is Symbol osym && osym.Name.Length > 0
                       && (osym.Name == "REPORT" || osym.Name == "INTERACTIVE" || osym.Name == "TEST")
                       && bc.Cdr is Cons bv)
                {
                    opts.Add(Startup.Keyword(osym.Name + "-FUNCTION"));
                    // :report "str" stays a string; a symbol/lambda is a function designator.
                    opts.Add(bv.Car);
                    cbody = bv.Cdr;
                }
                var tag = Runtime.Gensym(new LispString("RC-TAG"));
                var g = Runtime.Gensym(new LispString("RC-A"));
                var rfn = Runtime.List(LAMBDA, Runtime.List(REST, g),
                    Runtime.List(SETQ, avar, g), Runtime.List(GO, tag));
                var bindItems = new System.Collections.Generic.List<LispObject> { name, rfn };
                bindItems.AddRange(opts);
                rbBindings.Add(Runtime.List(bindItems.ToArray()));
                tagForms.Add(tag);
                tagForms.Add(Runtime.List(RETURN_FROM, blk,
                    Runtime.List(APPLY, new Cons(LAMBDA, new Cons(ll, cbody)), avar)));
            }
            var rb = new Cons(Startup.Sym("RESTART-BIND"),
                new Cons(Runtime.List(rbBindings.ToArray()),
                         new Cons(WrapSignalingRestartCaseBody(expr, mEnv), Nil.Instance)));
            var tagbody = new Cons(TAGBODY,
                new Cons(Runtime.List(RETURN_FROM, blk, rb), Runtime.List(tagForms.ToArray())));
            var letc = Runtime.List(Startup.Sym("LET"),
                Runtime.List(Runtime.List(avar, Nil.Instance)), tagbody);
            return Runtime.List(BLOCK, blk, letc);
        }, "RESTART-CASE-MACRO-EXPANDER", 2));

        // MULTIPLE-VALUE-BIND, as MACROEXPAND-1 shows it:
        //   (multiple-value-call #'(lambda (&optional v1 ... &rest #:ignore)
        //                            (declare (ignore #:ignore)) . body)
        //                        values-form)
        // Every value travels through the special form, so a code walker that
        // rewrites the expansion (cl-cont's CPS transform) keeps them all. The
        // compiler's own expansion, in *MACROS*, is not consulted here: it binds
        // from a per-thread snapshot of the last values, which is only sound when
        // nothing runs between producing and reading them. The compiler lowers
        // this shape to that same binding (%MV-CALL-LAMBDA-BIND).
        Runtime.RegisterMacroFunction(Startup.Sym("MULTIPLE-VALUE-BIND"), new LispFunction(args => {
            Runtime.CheckArityExact("MULTIPLE-VALUE-BIND-MACRO-EXPANDER", args, 2);
            if (args[0] is not Cons form || form.Cdr is not Cons r1 || r1.Cdr is not Cons r2)
                throw new LispErrorException(new LispProgramError(
                    "MULTIPLE-VALUE-BIND: expected (multiple-value-bind (var*) values-form declaration* form*)"));
            var vars = new System.Collections.Generic.List<LispObject> { Startup.Sym("&OPTIONAL") };
            for (var v = r1.Car; v is Cons vc; v = vc.Cdr)
                vars.Add(vc.Car);
            var ignore = new Symbol("IGNORE");
            vars.Add(Startup.Sym("&REST"));
            vars.Add(ignore);
            var decl = Runtime.List(Startup.Sym("DECLARE"), Runtime.List(Startup.Sym("IGNORE"), ignore));
            var lambda = new Cons(Startup.Sym("LAMBDA"),
                new Cons(Runtime.List(vars.ToArray()), new Cons(decl, r2.Cdr)));
            return Runtime.List(Startup.Sym("MULTIPLE-VALUE-CALL"),
                Runtime.List(Startup.Sym("FUNCTION"), lambda), r2.Car);
        }, "MULTIPLE-VALUE-BIND-MACRO-EXPANDER", 2));
    }

    /// CLHS RESTART-CASE: when the protected form is a call to SIGNAL, ERROR,
    /// CERROR or WARN, "or a macro form which macroexpands into such a list",
    /// WITH-CONDITION-RESTARTS is used implicitly, so the restarts this
    /// RESTART-CASE establishes belong to the condition that form signals and are
    /// invisible to any other condition. compile-restart-case (cil-forms-control.lisp)
    /// already does this for compiled code; the macro expansion did not, so an
    /// interpreted (restart-case (error ...) (foo () ...)) left its FOO
    /// unassociated. A handler that resignals then saw the *inner* FOO through
    /// (find-restart 'foo c) and ran the wrong clause (ansi-test RESTART-CASE.25-31),
    /// and COMPUTE-RESTARTS on an unrelated condition wrongly listed it
    /// (COMPUTE-RESTARTS.9). Same shape as the rest of this family: the work that
    /// supports EVAL existed only on the compile path.
    ///
    /// (restart-bind (...) EXPR) becomes
    ///   (restart-bind (...)
    ///     (let ((#:rs (%top-cluster-restarts)))            ; the cluster just pushed
    ///       (handler-bind ((condition (lambda (#:c)
    ///                                   (%associate-condition-restarts #:c #:rs))))
    ///         EXPR)))
    /// The association happens at signal time rather than at establishment time
    /// because the condition object does not exist yet when the cluster is pushed.
    private static LispObject WrapSignalingRestartCaseBody(LispObject expr, LispObject env)
    {
        if (!IsSignalingForm(expr, env)) return expr;
        var rs = (Symbol)Runtime.Gensym(new LispString("RC-RESTARTS"));
        var cv = (Symbol)Runtime.Gensym(new LispString("RC-COND"));
        var handler = Runtime.List(Startup.Sym("LAMBDA"), Runtime.List(cv),
            Runtime.List(Startup.Sym("%ASSOCIATE-CONDITION-RESTARTS"), cv, rs));
        var mark = (Symbol)Runtime.Gensym(new LispString("RC-MARK"));
        var hb = Runtime.List(Startup.Sym("HANDLER-BIND"),
            Runtime.List(Runtime.List(Startup.Sym("CONDITION"), handler)), expr);
        // The associations go when the restarts do: on every way out of the body.
        var up = Runtime.List(Startup.Sym("UNWIND-PROTECT"), hb,
            Runtime.List(Startup.Sym("%CONDITION-RESTARTS-RESTORE"), mark));
        return Runtime.List(Startup.Sym("LET"),
            Runtime.List(Runtime.List(rs, Runtime.List(Startup.Sym("%TOP-CLUSTER-RESTARTS"))),
                         Runtime.List(mark, Runtime.List(Startup.Sym("%CONDITION-RESTARTS-MARK")))),
            up);
    }

    /// True when FORM is, or macroexpands in ENV into, a call to SIGNAL / ERROR /
    /// CERROR / WARN. ENV matters: ansi-test RESTART-CASE.29/30/31 reach the
    /// signaling form only through MACROLET / SYMBOL-MACROLET bindings, which are
    /// invisible to an environment-less MACROEXPAND-1.
    private static bool IsSignalingForm(LispObject form, LispObject env)
    {
        var expander = Emitter.CilAssembler.TryGetFunction("MACROEXPAND-1");
        for (var i = 0; i < 64; i++)
        {
            if (form is Cons c && c.Car is Symbol s
                && (s.Name == "ERROR" || s.Name == "SIGNAL"
                    || s.Name == "CERROR" || s.Name == "WARN"))
                return true;
            if (expander == null) return false;
            var next = MultipleValues.Primary(expander.Invoke(new[] { form, env }));
            // MACROEXPAND-1 returns the very same object when nothing expanded.
            if (ReferenceEquals(next, form)) return false;
            form = next;
        }
        return false;
    }

    /// When true, COMPILE-FILE wraps a compile error on a top-level form in a
    /// LispSourceException carrying the input file + the form's source line, so
    /// the project build driver (CompileProject) can report file:line and remap
    /// concat lines back to the original source (dotcl/dotcl#48). Thread-static and
    /// gated: ordinary interactive/script COMPILE-FILE leaves it false so the raw
    /// LispErrorException (a real condition) still propagates to handler-case.
    [ThreadStatic] internal static bool EmitBuildSourceLocations;

    /// True while a non-default *MACROEXPAND-HOOK* is running, so MACROEXPAND-1
    /// calls the expander directly instead of going through the hook again.
    ///
    /// An INTERPRETED hook re-enters without this. The tree-walk evaluator
    /// macroexpands the hook's own body on every call, so a hook whose body
    /// contains any macro, (incf count) is enough, expands it, which calls
    /// MACROEXPAND-1, which calls the hook, forever. The process dies on a .NET
    /// stack overflow, which cannot be caught. A COMPILED hook never showed this:
    /// its body was expanded once at compile time, so nothing expands while it
    /// runs and this guard never fires.
    ///
    /// The effect is that a hook does not observe expansions performed by its own
    /// body. That is the only terminating reading for an evaluator that expands at
    /// eval time, and it leaves what the hook is for, seeing the expansions of
    /// the code being walked, intact.
    [ThreadStatic] private static bool _inMacroexpandHook;

    /// <summary>
    /// When true, COMPILE-FILE emits a Portable PDB even without the
    /// DOTCL_EMIT_PDB env var: set by CompileProject on a Debug build so a
    /// dotcl project is source-debuggable without the caller exporting the var.
    /// </summary>
    [ThreadStatic] internal static bool BuildEmitPdb;

    /// <summary>
    /// PDB document override for a Debug project build. CompileProject compiles a
    /// concatenated unit (&lt;out&gt;.concat.lisp); pointing the document at the
    /// real source makes a single-source project break/step in its own .lisp
    /// rather than the generated concat file. Null falls back to the input path.
    /// Multi-source projects keep the concat unit until per-document mapping lands.
    /// </summary>
    [ThreadStatic] internal static string? BuildDebugSourceOverride;

    /// <summary>
    /// Per-source-file line map for a Debug project build over a concatenated unit
    /// (one (startLine, path) per component). Non-null makes COMPILE-FILE emit a
    /// multi-document PDB so each .lisp in a multi-file project is independently
    /// debuggable. Null keeps the single-document path. Set by CompileProject.
    /// </summary>
    [ThreadStatic] internal static (int startLine, string path)[]? BuildDebugLineMap;

    [ThreadStatic] private static List<LispCondition>? s_compilerResignal;

    /// <summary>The WARNING handler of COMPILE-FILE and COMPILE, as SBCL's: the
    /// warning goes to the outer handlers first, and only one none of them
    /// muffled is counted (RECORD gets whether it is a full WARNING), printed
    /// and muffled. An enclosing compilation's handler passes on a warning an
    /// inner one is asking about, so it is counted by the innermost only.</summary>
    private static LispFunction CompilerWarningHandler(Action<bool> record) => new(args =>
    {
        if (args[0] is not LispCondition c) return Nil.Instance;
        var asking = s_compilerResignal ??= new();
        foreach (var a in asking) if (ReferenceEquals(a, c)) return Nil.Instance;
        asking.Add(c);
        try { HandlerClusterStack.Signal(c); }
        finally { asking.RemoveAt(asking.Count - 1); }
        record(!IsTruthy(Typep(c, Startup.Sym("STYLE-WARNING"))));
        var restart = RestartClusterStack.FindRestartByName("MUFFLE-WARNING", c);
        if (restart == null || restart.IsBindRestart) return Nil.Instance;
        ConditionSystem.PrintWarning(c);
        throw new RestartInvocationException(restart.Tag, Array.Empty<LispObject>());
    });

    /// <summary>Bind the compiler's OPTIMIZE policy to its current values, so a
    /// DECLAIM in the file being compiled or loaded is in force to the end of
    /// that file only (as in SBCL).</summary>
    private static void PushPolicyBindings()
    {
        foreach (var policyName in new[] { "*OPTIMIZE-SAFETY*", "*OPTIMIZE-DEBUG*", "*PROCLAIMED-POLICY*" })
            if (Startup.Sym(policyName) is var policySym
                && DynamicBindings.TryGet(policySym, out var policyVal))
                DynamicBindings.Push(policySym, policyVal);
    }

    public static LispObject CompileFile(LispObject[] args)
    {
#if DOTCL_EMIT
        if (args.Length == 0)
            throw new LispErrorException(new LispProgramError("COMPILE-FILE: requires at least 1 argument"));

        // (compile-file input-file &key output-file verbose print external-format)
        var inputSpec = args[0];

        // Parse keyword arguments
        LispObject? outputFileArg = null;
        LispObject? externalFormatArg = null;
        LispObject? verboseArg = null;
        LispObject? printArg = null;
        LispObject? targetFeaturesArg = null;
        LispObject? moduleNameArg = null;
        LispObject? corlibNameArg = null;
        bool emitSil = false; // default: PE assembly (.fasl); :sil t -> also write text SIL
        bool silSeen = false;
        bool allowOtherKeys = false;

        // Check for odd number of keyword arguments
        if ((args.Length - 1) % 2 != 0)
            throw new LispErrorException(new LispProgramError("COMPILE-FILE: odd number of keyword arguments"));

        // First pass: check for :allow-other-keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol ks && ks.Name == "ALLOW-OTHER-KEYS" && args[i + 1] is not Nil)
            {
                allowOtherKeys = true;
                break;
            }
        }

        // Second pass: parse known keys
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            string key = args[i] switch
            {
                Symbol s => s.Name,
                _ => args[i].ToString()
            };
            switch (key)
            {
                case "OUTPUT-FILE": outputFileArg ??= args[i + 1]; break;
                case "VERBOSE": verboseArg ??= args[i + 1]; break;
                case "PRINT": printArg ??= args[i + 1]; break;
                case "SIL":
                    if (!silSeen) { emitSil = args[i + 1] is not Nil; silSeen = true; }
                    break;
                case "TARGET-FEATURES": targetFeaturesArg ??= args[i + 1]; break;
                // :module-name: pin the FASL's .NET assembly/module name to a
                // stable string instead of the default basename_<guid8>. Required
                // for build-time-linked (NativeAOT/IL2CPP) deployment, where the
                // assembly is referenced by a fixed name and the file name must
                // match it. The caller owns uniqueness across co-loaded modules.
                case "MODULE-NAME": moduleNameArg ??= args[i + 1]; break;
                // :corlib-name: rewrite the emitted fasl's corlib reference to a
                // portable facade (only "netstandard"). For build-time-linked
                // deployment on BCLs without System.Private.CoreLib (Unity IL2CPP/
                // WebGL). Default (null) keeps System.Private.CoreLib (CoreCLR/AOT).
                case "CORLIB-NAME": corlibNameArg ??= args[i + 1]; break;
                case "EXTERNAL-FORMAT": externalFormatArg ??= args[i + 1]; break;
                case "ALLOW-OTHER-KEYS": break; // already handled
                default:
                    if (!allowOtherKeys)
                        throw new LispErrorException(new LispProgramError($"COMPILE-FILE: unrecognized keyword argument :{key}"));
                    break;
            }
        }

        // Defaults from *compile-verbose* and *compile-print*
        var compileVerboseSym = Startup.Sym("*COMPILE-VERBOSE*");
        var compilePrintSym = Startup.Sym("*COMPILE-PRINT*");
        bool isVerbose = (verboseArg ?? DynamicBindings.Get(compileVerboseSym)) is not Nil;
        bool isPrint = (printArg ?? DynamicBindings.Get(compilePrintSym)) is not Nil;

        // Handle stream as pathname designator (CLHS: a file stream is a pathname designator)
        if (inputSpec is LispFileStream fs)
        {
            inputSpec = fs.OriginalPathname != null
                ? (LispObject)fs.OriginalPathname
                : LispPathname.FromString(fs.FilePath);
        }

        string inputPath = ResolvePhysicalPath(inputSpec);

        // Merge with *default-pathname-defaults* if file not found at given path
        if (!File.Exists(inputPath))
        {
            var dpd = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
            if (dpd is LispPathname dpdPath)
            {
                var merged = LispPathname.FromString(inputPath).MergeWith(dpdPath).ToNamestring();
                if (File.Exists(merged))
                    inputPath = merged;
            }
        }

        // Determine output path
        string? outputPath = null;
        // :OUTPUT-FILE is a pathname designator; NIL is none (SBCL: TYPE-ERROR). Leaving the
        // keyword out is how the default output name is asked for.
        if (outputFileArg is Nil)
            throw new LispErrorException(new LispTypeError(
                "COMPILE-FILE: :OUTPUT-FILE is NIL, not a pathname designator", outputFileArg,
                Runtime.List(Startup.Sym("OR"), Startup.Sym("STRING"), Startup.Sym("PATHNAME"))));
        if (outputFileArg != null)
        {
            outputPath = ResolvePhysicalPath(CompileFilePathname(new[] { inputSpec, Startup.Keyword("OUTPUT-FILE"), outputFileArg }));
        }

        if (outputPath == null)
        {
            var cfp = CompileFilePathname(new[] { inputSpec });
            outputPath = ResolvePhysicalPath(cfp);
        }

        if (!File.Exists(inputPath))
        {
            var err = new LispError($"COMPILE-FILE: file not found: {inputPath}");
            err.ConditionTypeName = "FILE-ERROR";
            err.FileErrorPathnameRef = inputSpec is LispPathname ? inputSpec : (LispObject)LispPathname.FromString(inputPath);
            throw new LispErrorException(err);
        }

        if (isVerbose)
        {
            var w = GetStandardOutputWriter();
            w.WriteLine($"; Compiling file {inputPath}");
            w.Flush();
        }

        // Bind *compile-file-pathname*, *compile-file-truename*, *package*, and *readtable*
        // CLHS: *compile-file-pathname* = (merge-pathnames (pathname input-file) *default-pathname-defaults*)
        // CLHS: *compile-file-truename* = (truename *compile-file-pathname*)
        var cfpSym = Startup.Sym("*COMPILE-FILE-PATHNAME*");
        var cftSym = Startup.Sym("*COMPILE-FILE-TRUENAME*");
        var packageSym = Startup.Sym("*PACKAGE*");
        var readtableSym = Startup.Sym("*READTABLE*");
        var oldPackage = DynamicBindings.Get(packageSym);
        var oldReadtable = DynamicBindings.Get(readtableSym);
        var cfpInputPath = inputSpec switch
        {
            LispPathname p => p,
            LispString s => FromNamestring(s.Value),
            _ => FromNamestring(inputSpec.ToString())
        };
        var dpd2 = DynamicBindings.Get(Startup.Sym("*DEFAULT-PATHNAME-DEFAULTS*"));
        var mergedCfp = MergePathnames(cfpInputPath, dpd2 is LispPathname ? dpd2 : LispPathname.FromString(""));
        var cftPath = LispPathname.FromString(Path.GetFullPath(inputPath));
        // Bound on this thread, not assigned: another thread compiling a file at
        // the same time must not see these.
        int bindDepth = DynamicBindings.Depth;
        DynamicBindings.Push(cfpSym, mergedCfp);
        DynamicBindings.Push(cftSym, cftPath);
        DynamicBindings.Push(packageSym, oldPackage);
        DynamicBindings.Push(readtableSym, oldReadtable);

        // Set *compile-file-mode* so the Lisp compiler handles eval-when
        // per CLHS 3.2.3.1 (eval :compile-toplevel, emit :load-toplevel).
        // Use Startup.Sym (not SymInPkg) to match the compiled code's LOAD-SYM resolution.
        var compileFileModeSym = Startup.Sym("*COMPILE-FILE-MODE*");
        DynamicBindings.Push(compileFileModeSym, T.Instance);
        var sameFileSym = Startup.Sym("*SAME-FILE-FUNCTIONS*");
        DynamicBindings.Push(sameFileSym, new LispHashTable("EQ"));
        DynamicBindings.Push(Startup.Sym("*SAME-FILE-INLINE-DEFS*"), new LispHashTable("EQ"));
        // Compared around each form below: only this file's failures may count.
        var cfFailuresSym = Startup.Sym("*MACROEXPANSION-FAILURES*");
        DynamicBindings.Push(cfFailuresSym, DynamicBindings.Get(cfFailuresSym));
        DynamicBindings.Push(Startup.Sym("*FILE-EXPANSION-FAILURES*"), new LispHashTable("EQ"));
        var cfErrorsSym = Startup.Sym("*COMPILE-ERRORS-DETECTED*");
        DynamicBindings.Push(cfErrorsSym, Nil.Instance);

        // Snapshot *modules*. A compile-time (require "x") (e.g. eval-when
        // :compile-toplevel) loads a contrib whose defuns are reverted by the
        // Function/SetfFunction strip in `finally` below: but the *modules*
        // push is a dynamic-var mutation not covered by that strip, so without
        // this it would persist, leaving the module marked-loaded yet undefined.
        // A later load-time (require "x") would then see it in *modules* and
        // skip re-loading, so the contrib's functions stay unbound. Reverting
        // *modules* keeps it consistent with the stripped defuns: the compile-time
        // require becomes fully transient and the load-time require re-loads for real.
        var modulesSym2 = Startup.Sym("*MODULES*");
        var oldModules = DynamicBindings.Get(modulesSym2);
        DynamicBindings.Push(modulesSym2, oldModules);

        // :target-features: rebind *features* so reader conditionals (#+/#-) and
        // os-cond macro expansions see the target platform, not the host.
        // Used for cross-compiling asdf.fasl for Linux/Win/macOS from any host.
        var featuresSym = Startup.Sym("*FEATURES*");
        if (targetFeaturesArg != null && targetFeaturesArg is not Nil)
            DynamicBindings.Push(featuresSym, targetFeaturesArg);

        // Snapshot Function / SetfFunction state for all symbols. compile-file
        // may evaluate defun / (defun (setf x) ...) at compile-time so that
        // sibling macros can reference the function during the same file
        // (try-eval), but per ANSI 3.2.3.1 those definitions must NOT
        // leak into the global environment after compile-file returns
        // (otherwise (compile-file foo.lisp) would side-effect (fboundp 'bar)
        // for any defun in foo.lisp: failing pfdietz COMPILE-FILE.* tests).
        var preDefs = new CompileFileDefinitions();
        // Definitions brought in by a nested LOAD (e.g. a compile-time
        // (require "module") that ASDF resolves and loads) are real global
        // side effects of loading OTHER files, not compile-time defuns of
        // THIS file, and must survive the post-compile strip. Runtime.Load
        // keeps every current definition in each CompileFileDefinitions on
        // this stack after each load, which whitelists everything it defined.
        // (Restoring *modules* alone is not enough: ASDF tracks completed
        // systems independently, so the later fasl-load's (require ...) is a
        // no-op and the stripped functions would stay undefined.)
        s_compileFilePreSets.Push(preDefs);
        (s_earlyDefunsTS ??= new()).Push(new());
        (s_pendingDeferredTS ??= new()).Push(new());
        s_compileFileRequired.Push(new HashSet<string>());
        Runtime.PushCompileTimeClasses();

        // FASL module name must be computed before the module-ID binding (declared below).
        // :module-name pins it to a stable string (build-time-link / AOT); otherwise
        // a per-compile guid suffix keeps module names (and thus LTV slot-ID
        // namespaces) unique across independently-compiled FASLs.
        var faslModuleName = (moduleNameArg is LispString mns && mns.Value.Length > 0)
            ? mns.Value
            : Path.GetFileNameWithoutExtension(outputPath)
                + "_" + ModuleNameSuffix(inputPath, outputPath);

        // Bind *current-module-id* to the FASL's unique module name.
        // The load-time-value compiler handler uses this to namespace LTV slot IDs per module,
        // preventing cross-run collisions when FASLs compiled in different sessions share IDs.
        // Use Startup.Sym (not SymInPkg) so we get the same symbol object that cil-out.sil's
        // (:LOAD-SYM "*CURRENT-MODULE-ID*") resolves to: cross-compiled code uses bare LOAD-SYM
        // (because *cross-compiling* suppresses LOAD-SYM-PKG emission), so the symbol lives in
        // DOTCL-INTERNAL, not DOTCL.CIL-COMPILER.
        Symbol? moduleIdSym = null;
        try { moduleIdSym = Startup.Sym("*CURRENT-MODULE-ID*"); }
        catch { /* should not throw, but guard defensively */ }
        if (moduleIdSym != null)
            DynamicBindings.Push(moduleIdSym, new LispString(faslModuleName));
        // The compiler leaves out code the retargeted fasl could not load (Vector128).
        var faslCorlibSym = Startup.Sym("*FASL-CORLIB*");
        DynamicBindings.Push(faslCorlibSym,
            corlibNameArg is LispString corlibStr && corlibStr.Value.Length > 0 ? corlibStr : Nil.Instance);
        PushPolicyBindings();

        try
        {
            // Read source, compile each form, and write instrList to .sil
            var source = ReadSourceText(inputPath, externalFormatArg, "COMPILE-FILE");
            var trackingReader = new PositionTrackingReader(new StringReader(source));
            var reader = new Reader(trackingReader) { LineSource = source };
            var compileStream = new LispStringInputStream(trackingReader, 0, source, source.Length);
            if (LispStringInputStream.Utf8Preamble(inputPath) is int compilePre && compilePre >= 0)
                compileStream.ReportFileBytes(compilePre);
            reader.LispStreamRef = compileStream;
            // Link this reader as the stream's live reader and share the #n=/#n#
            // tables with the stream. A Lisp reader macro (e.g. SBCL's cold-build
            // read-list on "(") reads elements via (read stream); without this link
            // ReadFromStream spins up a second Reader whose per-form table clearing
            // runs once per ELEMENT, so a #n= registered in one element is gone by
            // the time the sibling #n# is read: the placeholder then leaks into
            // the compiled fasl as an unresolved constant.
            compileStream.CachedReader = reader;
            reader.AdoptStreamShareTables(compileStream);

            var dir = Path.GetDirectoryName(outputPath);
            if (!string.IsNullOrEmpty(dir) && !Directory.Exists(dir))
                Directory.CreateDirectory(dir);

            bool warningsP = false;
            bool failureP = false;
            var warningHandler = CompilerWarningHandler(full =>
            {
                warningsP = true;
                if (full) failureP = true;
            });
            var handlerCluster = new HandlerBinding[] {
                new HandlerBinding(Startup.Sym("WARNING"), warningHandler)
            };
            HandlerClusterStack.PushCluster(handlerCluster);

            // FASL assembler (always: .fasl is the default output)
            var faslAsm = new DotCL.Emitter.FaslAssembler(faslModuleName);
            // Opt-in Portable PDB emission: DOTCL_EMIT_PDB writes a sidecar
            // .pdb mapping compiled forms to their source lines, so a debugger can
            // break/step in the .lisp. Off by default; the normal emit path is
            // unchanged. Turns on the Reader's source-line tracking and installs the
            // line-lookup fn in the compiler's *EMIT-SOURCE-LINES* so COMPILE-EXPR
            // emits (:line N) markers.
            bool emitPdb = Environment.GetEnvironmentVariable("DOTCL_EMIT_PDB") != null
                           || BuildEmitPdb;
#if NET9_0_OR_GREATER
            if (!emitPdb && FaslInMemoryLoadEnabled()) faslAsm.AlsoBuildInMemory();
#endif
            Symbol? emitLinesSym = null;
            LispObject? savedEmitLines = null;
            var savedSourceLines = SourceLineTable;
            var savedSourcePositions = SourcePositionTable;
            // Kept for each top level form only: nested definitions need their own
            // lines, and holding the whole file's forms would keep every cons alive.
            SourceLineTable = new Dictionary<object, SourceSpan>(ReferenceEqualityComparer.Instance);
            SourcePositionTable = new Dictionary<object, SourceRange>(ReferenceEqualityComparer.Instance);
            BeginCompilerUnit(Path.GetFullPath(inputPath));
            if (emitPdb)
            {
                if (BuildDebugLineMap != null)
                    faslAsm.EnableDebugInfoMap(BuildDebugLineMap);
                else
                    faslAsm.EnableDebugInfo(BuildDebugSourceOverride ?? Path.GetFullPath(inputPath));
                // The compiled compiler reads *EMIT-SOURCE-LINES* via a bare-name
                // (:LOAD-SYM ...) = Startup.Sym, so set the SAME symbol instance;
                // SymInPkg("...","DOTCL.CIL-COMPILER") would be a different symbol
                // (symbol-identity gotcha) and the compiler would never see it.
                emitLinesSym = Startup.Sym("*EMIT-SOURCE-LINES*");
                if (emitLinesSym != null)
                {
                    savedEmitLines = emitLinesSym.IsBound ? emitLinesSym.Value : null;
                    emitLinesSym.Value = new LispFunction(FormLineLookup, "%FORM-LINE-LOOKUP", 1);
                }
            }

            var hostFeaturePrev = HostFeatures.Begin(faslAsm.HostFeatureRecord);
            try
            {
                // SIL text writer (when :sil t or dotcl:*save-sil* is true)
                if (!emitSil)
                {
                    var saveSilSym = Startup.SymInPkg("*SAVE-SIL*", "DOTCL");
                    if (saveSilSym != null && saveSilSym.IsBound && saveSilSym.Value is not Nil)
                        emitSil = true;
                }
                var silPath = emitSil ? Path.ChangeExtension(outputPath, ".sil") : null;
                StreamWriter? writer = silPath != null ? new StreamWriter(silPath) : null;
                try
                {
                writer?.WriteLine(";; -*- dotcl-compiled -*-");
                int formLine = 0;
                LispObject xrefSource = Nil.Instance;
                // CLHS 3.2.3.1: process one top level form. compileTimeToo is the
                // compile-time-too mode of Figure 3-7: every form processed in it is
                // also evaluated at compile time, a form at a time, so that what one
                // form does is in place before the next one is macroexpanded.
                // Compiling the whole EVAL-WHEN body first and running it afterwards
                // expanded later forms before earlier ones had run.
                // loadOnly: inside an EVAL-WHEN that names :LOAD-TOPLEVEL and puts
                // its body in not-compile-time mode. The file asked for no
                // compile-time evaluation there, so the courtesy early evaluation of
                // EXPORT / IMPORT / SHADOW / SHADOWING-IMPORT / USE-PACKAGE is not
                // done (it would run a package change the file meant for load time
                // only, e.g. an UNEXPORT + SHADOWING-IMPORT + EXPORT swap).
                void ProcessTopLevelForm(LispObject subForm, bool compileTimeToo, bool loadOnly = false)
                {
                    if (s_flattenedDefinitionNames != null)
                        s_flattenedDefinitionLine = SourceLineOf(subForm, s_flattenedDefinitionLine);
                    if (IsEvalWhenForCompileFile(subForm, out var ewBody,
                        out bool hasCT, out bool hasLT))
                    {
                        if (hasLT)
                        {
                            // {CT LT} -> compile-time-too; {LT EX} keeps the current
                            // mode; {LT} alone -> not-compile-time.
                            bool ctt = hasCT || (compileTimeToo && EvalWhenHasExecute(subForm));
                            for (var b = ewBody; b is Cons bc; b = bc.Cdr)
                                foreach (var sf in FlattenTopLevel(bc.Car, null, s_flattenedDefinitionLine))
                                    ProcessTopLevelForm(sf, ctt, loadOnly || !ctt);
                        }
                        else
                        {
                            // {CT} without LT: evaluate, as EVAL would, a form at a time.
                            for (var b = ewBody; b is Cons bc; b = bc.Cdr)
                                foreach (var sf in FlattenTopLevel(bc.Car, null, s_flattenedDefinitionLine))
                                    EvaluateAtCompileTime(sf);
                        }
                        return;
                    }
                    if (compileTimeToo && IsExecuteOnlyEvalWhen(subForm, out var exBody))
                    {
                        // {EX} in compile-time-too mode: evaluate, nothing for the fasl.
                        for (var b = exBody; b is Cons bc; b = bc.Cdr)
                            foreach (var sf in FlattenTopLevel(bc.Car, null, s_flattenedDefinitionLine))
                                EvaluateAtCompileTime(sf);
                        return;
                    }
                    // Phase boundaries for DOTCL_PHASE_PROF (see PhaseTimer):
                    // "compile" is the Lisp compiler, "eval-ct" is running a
                    // form at compile time, "fasl-add" is the backend turning
                    // the instruction list into IL.
                    var ctBefore = compileTimeToo ? SnapshotStrippableDefinitions() : null;
                    LispObject instrList;
                    try
                    {
                        var failuresSym = Startup.Sym("*MACROEXPANSION-FAILURES*");
                        var failuresBefore = DynamicBindings.Get(failuresSym);
                        instrList = DotCL.Diagnostics.PhaseTimer.Time(
                            "compile", () => CompileTopLevel(subForm, TopLevelFormXrefName(inputPath, formLine, xrefSource)));
                        // A form whose expansion failed was reported and compiled to
                        // signal when loaded; running that at compile time too would
                        // turn the report into an error out of COMPILE-FILE. SBCL
                        // reports it once and goes on.
                        bool expansionFailed = !Runtime.IsTrueEql(failuresBefore, DynamicBindings.Get(failuresSym));
                        if (compileTimeToo && !expansionFailed)
                        {
                            RunAtCompileTime(instrList, ctBefore!);
                            // CLHS 3.2.3.1: in compile-time-too mode the form is
                            // evaluated first and then compiled. Its own evaluation
                            // can change what its macros expand to (lift's
                            // DEFTESTSUITE records the suite's slot names in a form
                            // whose later methods expand against them), so the code
                            // for the fasl is compiled again after the run, as SBCL
                            // does, instead of reusing what was run. The run made
                            // the form's DEFUNs for real, so the early definitions
                            // this compilation makes of them are put back to the
                            // run's at once; left to the end of COMPILE-FILE they
                            // would be undone to what was there before the file.
                            s_earlyDefunsTS!.Push(new());
                            try
                            {
                                instrList = DotCL.Diagnostics.PhaseTimer.Time(
                                    "compile", () => CompileTopLevel(subForm, TopLevelFormXrefName(inputPath, formLine, xrefSource)));
                            }
                            finally { RestoreEarlyDefuns(s_earlyDefunsTS.Pop()); }
                        }
                    }
                    finally { if (ctBefore != null) DefinitionJournal.Close(ctBefore); }
                    if (!compileTimeToo && ShouldExecuteAtCompileTime(subForm)
                             && !(loadOnly && IsCourtesyCompileTimeForm(subForm)))
                        DotCL.Diagnostics.PhaseTimer.Time("eval-ct",
                            () => RunCompileTimeSideEffect(subForm, instrList));
                    writer?.WriteLine(instrList.ToString());
                    DotCL.Diagnostics.PhaseTimer.Time("fasl-add",
                        () => faslAsm.AddTopLevelForm(instrList, formLine));
                    faslAsm.FlushInitForms();
                }
                void EvaluateAtCompileTime(LispObject subForm)
                {
                    var ctBefore = SnapshotStrippableDefinitions();
                    // Compiled only to be run now: a macro whose expander fails
                    // here fails the way EVAL would see it, not as a form reported
                    // and compiled to fail at load time (there is no load time).
                    var ctEvalSym = Startup.Sym("*EVALUATING-AT-COMPILE-TIME*");
                    // CLHS 3.2.3.1: an EVAL-WHEN evaluated here is not a top level form.
                    var cfmSym = Startup.Sym("*COMPILE-FILE-MODE*");
                    DynamicBindings.Push(ctEvalSym, T.Instance);
                    DynamicBindings.Push(cfmSym, Nil.Instance);
                    try
                    {
                        var instrList = DotCL.Diagnostics.PhaseTimer.Time(
                            "compile", () => CompileTopLevel(subForm));
                        RunAtCompileTime(instrList, ctBefore);
                    }
                    finally
                    {
                        DynamicBindings.Pop(cfmSym);
                        DynamicBindings.Pop(ctEvalSym);
                        DefinitionJournal.Close(ctBefore);
                    }
                }
                // What an explicit :COMPILE-TOPLEVEL evaluation defines is a real
                // definition (SBCL keeps it after COMPILE-FILE), not the early
                // evaluation of a plain DEFUN that the strip in the finally below
                // exists to undo. What is strippable is noted before compiling
                // (compiling may already evaluate DEFUNs early); whatever changed is kept.
                void RunAtCompileTime(LispObject instrList,
                    DefinitionJournal.Window ctBefore)
                {
                    DotCL.Diagnostics.PhaseTimer.Time("eval-ct",
                        () => DotCL.Emitter.CilAssembler.AssembleAndRun(instrList));
                    KeepDefinitionsMadeSince(ctBefore, preDefs);
                }
                bool readAborted = false;
                while (true)
                {
                    // An error while reading a form (a reader error, or one in the #.
                    // evaluation) ends the compilation as SBCL does: it is reported, the
                    // output is not kept, and COMPILE-FILE returns NIL, T, T.
                    LispObject? readForm;
                    var readCluster = new HandlerBinding[] {
                        new HandlerBinding(Startup.Sym("ERROR"),
                            new LispFunction(hargs => throw new CompileFileReadAbort((LispCondition)hargs[0])))
                    };
                    HandlerClusterStack.PushCluster(readCluster);
                    try
                    {
                        readForm = DotCL.Diagnostics.PhaseTimer.Time("read", () => reader.TryRead(out var f) ? f : null);
                    }
                    catch (CompileFileReadAbort abort)
                    {
                        var ew = GetTextWriter(DynamicBindings.Get(Startup.Sym("*ERROR-OUTPUT*")));
                        ew.WriteLine();
                        ew.WriteLine("; caught ERROR:");
                        ew.WriteLine($";   READ error during COMPILE-FILE: {abort.Condition.Message}");
                        ew.WriteLine("; compilation aborted because of input error");
                        ew.Flush();
                        readAborted = true;
                        break;
                    }
                    finally { HandlerClusterStack.PopCluster(); }
                    if (readForm is not { } form) break;
                    // Line where this top-level form began: used to attribute a
                    // compile error to the source location under a project build,
                    // and to record definition locations for frame-source-location.
                    formLine = reader.LastFormLine;
                    xrefSource = form;
                    SetCompilerTopLevel(form, formLine);
                    RecordDefinitionSources(form, inputPath, formLine, useSourceLines: true);
                    AddFaslDefinitionSources(faslAsm, form, inputPath, formLine);
                    try
                    {
                    var defNames = new List<(DefinitionEntry Entry, int Line)>();
                    var savedDefNames = s_flattenedDefinitionNames;
                    var savedRunning = s_runningTopLevelForm;
                    s_flattenedDefinitionNames = defNames;
                    s_runningTopLevelForm = false;
                    try
                    {
                    foreach (var subForm in FlattenTopLevel(form, null, formLine))
                    {
                        ProcessTopLevelForm(subForm, false);

                        if (isPrint)
                        {
                            var w = GetStandardOutputWriter();
                            w.WriteLine(FormatTop(subForm, true));
                            w.Flush();
                        }
                    }
                    }
                    finally
                    {
                        s_flattenedDefinitionNames = savedDefNames;
                        s_runningTopLevelForm = savedRunning;
                    }
                    var expandedDefs = ExpandedDefinitionNames(form, defNames);
                    RecordDefinitionNames(expandedDefs, inputPath, formLine);
                    AddFaslDefinitionNames(faslAsm, expandedDefs, inputPath, formLine);
                    }
                    catch (LispSourceException) { throw; } // already located (nested load)
                    catch (Exception ex) when (EmitBuildSourceLocations
                                               && ex is not System.Threading.ThreadInterruptedException)
                    {
                        throw new LispSourceException(inputPath, formLine, ex);
                    }
                    SourceLineTable?.Clear();
                    SourcePositionTable?.Clear();
                }

                if (readAborted)
                {
                    FlushCompilerWarnings();
                    try { if (File.Exists(outputPath)) File.Delete(outputPath); } catch (IOException) { }
                    return MultipleValues.Values(Nil.Instance, T.Instance, T.Instance);
                }
                // Emit any remaining init forms (cycle members, etc.)
                faslAsm.FlushRemainingInitForms();

                var corlibName = (corlibNameArg is LispString cln && cln.Value.Length > 0)
                    ? cln.Value
                    : null;
                DotCL.Diagnostics.PhaseTimer.Time("fasl-save",
                    () => faslAsm.Save(outputPath, corlibName));
                FlushCompilerWarnings();
                }
                finally { writer?.Dispose(); }
            }
            finally
            {
                HostFeatures.End(hostFeaturePrev);
                HandlerClusterStack.PopCluster();
                EndCompilerUnit();
                SourceLineTable = savedSourceLines;
                SourcePositionTable = savedSourcePositions;
                if (emitPdb && emitLinesSym != null) emitLinesSym.Value = savedEmitLines ?? Nil.Instance;
            }

            if (IsTruthy(DynamicBindings.Get(cfErrorsSym))) warningsP = failureP = true;
            // Return (values output-truename warnings-p failure-p)
            var outPathname = LispPathname.FromString(Path.GetFullPath(outputPath));
            return MultipleValues.Values(outPathname,
                warningsP ? (LispObject)T.Instance : Nil.Instance,
                failureP ? (LispObject)T.Instance : Nil.Instance);
        }
        finally
        {
            s_compileFilePreSets.Pop();
            RestoreEarlyDefuns(s_earlyDefunsTS!.Pop());
            s_pendingDeferredTS!.Pop();
            var requiredHere = s_compileFileRequired.Pop();
            Runtime.PopCompileTimeClasses();
            // Strip newly-defined Function / SetfFunction values that escaped
            // from compile-time defun/defmethod try-eval. Anything that was already
            // fbound before compile-file is left untouched: only WE clean up
            // the side-effects WE introduced.
            // Also clear the GF registry for stripped symbols so that when the
            // compiled fasl is later loaded, %find-gf returns NIL and the GF is
            // properly re-registered (and sym.Function re-set). Without this,
            // defclass accessors defined at compile-time would be in _gfRegistry
            // but not in sym.Function after the cleanup, causing %find-gf to skip
            // re-registration and leaving the accessor unbound.
            // Only a cell assigned during the compilation can have been defined by it.
            foreach (var s in preDefs.Close())
            {
                if (s.Function != null && !preDefs.KeepsFn(s))
                {
                    s.Function = null;
                    Runtime.RemoveGfRegistryEntry(s);
                }
                if (s.SetfFunction != null && !preDefs.KeepsSetf(s))
                {
                    s.SetfFunction = null;
                    Runtime.RemoveGfRegistryEntry(s, isSetf: true);
                }
            }

            // For each module a compile-time REQUIRE added to *modules* (one that
            // went through asdf:module-provide-asdf; a module the file only PROVIDEs
            // is left alone, see s_compileFileRequired), clear the ASDF
            // registry entry so a load-time re-require actually re-loads. module-provide-asdf
            // registers the system in *registered-systems* and sets the load-op stamp
            // (component-loaded-p -> T); without clear-system, the load-time require's
            // module-provide-asdf -> find-system returns the stale registered system,
            // component-loaded-p is T, load-system no-ops, and the fasl is never loaded.
            // clear-system does (remhash name *registered-systems*) +
            // (unset-asdf-cache-entry '(find-system name)), making the old stamped system
            // unreachable so a fresh find-system rebuilds an unstamped one.
            // module-provide-asdf looks up systems by (string-downcase name), so
            // clear-system must be called with the downcased name to match.
            var clearSystem = Package.FindPackage("ASDF")?.FindSymbol("CLEAR-SYSTEM").Item1?.Function
                              as LispFunction;
            if (clearSystem != null)
            {
                var beforeSet = new System.Collections.Generic.HashSet<string>();
                for (LispObject c = oldModules; c is Cons cc; c = cc.Cdr)
                    if (cc.Car is LispString s) beforeSet.Add(s.Value);
                for (LispObject c = DynamicBindings.Get(modulesSym2); c is Cons cc; c = cc.Cdr)
                    if (cc.Car is LispString s && !beforeSet.Contains(s.Value)
                        && requiredHere.Contains(s.Value))
                    {
                        try { clearSystem.Invoke(new LispObject[] { new LispString(s.Value.ToLowerInvariant()) }); }
                        catch { /* a module asdf does not own (plain provide/require); nothing to clear */ }
                    }
            }

            // Unbinding *modules* drops a compile-time (require ...), which would
            // otherwise leave a module marked loaded whose defuns were stripped above.
            DynamicBindings.TruncateTo(bindDepth);
        }
#else
        throw new LispErrorException(new LispProgramError(
            "COMPILE-FILE requires System.Reflection.Emit, which is unavailable in this runtime build"));
#endif
    }

    /// <lispdoc>(dotcl:build-library &amp;key files system references search-paths output) -- Write a .NET library DLL from Lisp sources: load the REFERENCES assemblies, then the FILES in order, with the one dotnet:library form among them writing to OUTPUT (its assembly name has to be OUTPUT's file name). With SYSTEM (a name, or the path of its .asd) instead of FILES, the ASDF system is loaded and then the file of its (:dotnet-library ...) component, as (asdf:make ...) does with the dotnet-library contrib; OUTPUT then defaults to the system's :build-pathname or the form's :path, and SEARCH-PATHS are directories ASDF looks for systems in. Every function of those sources that a library body reaches is compiled into the DLL. What `dotcl build-library` and the DotCL.Sdk LispLibrary / LispSystem items run. Returns the path written.</lispdoc>
    [LispDoc("DOTCL:BUILD-LIBRARY")]
    public static LispObject BuildLibraryLisp(LispObject[] args)
    {
        if (args.Length % 2 != 0)
            throw new LispErrorException(new LispProgramError(
                "BUILD-LIBRARY: odd number of keyword arguments"));
        var files = new List<string>();
        var refs = new List<string>();
        var searchPaths = new List<string>();
        string? output = null, system = null;
        static IEnumerable<LispObject> Items(LispObject o)
        {
            if (o is Cons) { for (var c = o; c is Cons cc; c = cc.Cdr) yield return cc.Car; }
            else if (o is not Nil) yield return o;
        }
        for (int i = 0; i < args.Length; i += 2)
        {
            string key = args[i] is Symbol ks ? ks.Name : args[i].ToString();
            switch (key)
            {
                case "FILES": files.AddRange(Items(args[i + 1]).Select(ResolvePhysicalPath)); break;
                case "REFERENCES": refs.AddRange(Items(args[i + 1]).Select(ResolvePhysicalPath)); break;
                case "SEARCH-PATHS": searchPaths.AddRange(Items(args[i + 1]).Select(ResolvePhysicalPath)); break;
                case "OUTPUT": if (args[i + 1] is not Nil) output = ResolvePhysicalPath(args[i + 1]); break;
                case "SYSTEM":
                    system = args[i + 1] switch
                    {
                        Nil => null,
                        LispString str => str.Value,
                        Symbol sym => sym.Name.ToLowerInvariant(),
                        var other => ResolvePhysicalPath(other),
                    };
                    break;
                default:
                    throw new LispErrorException(new LispProgramError(
                        $"BUILD-LIBRARY: unknown keyword :{key}"));
            }
        }
        if (system == null && output == null)
            throw new LispErrorException(new LispProgramError("BUILD-LIBRARY: :output is required"));
        if (system == null && files.Count == 0)
            throw new LispErrorException(new LispProgramError("BUILD-LIBRARY: :files or :system is required"));
        if (system != null && files.Count > 0)
            throw new LispErrorException(new LispProgramError("BUILD-LIBRARY: give :files or :system, not both"));
        var written = DotclBuild.BuildLibrary(files.ToArray(), refs.ToArray(), output, system,
            searchPaths.ToArray(), null);
        return new LispString(System.IO.Path.GetFullPath(written));
    }
}

/// <summary>Thrown by the error handler COMPILE-FILE installs around the reader, to
/// leave the read loop with the condition that stopped it.</summary>
internal sealed class CompileFileReadAbort : Exception
{
    public LispCondition Condition { get; }
    public CompileFileReadAbort(LispCondition condition) : base(condition.Message) => Condition = condition;
}
