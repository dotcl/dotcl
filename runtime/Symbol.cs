namespace DotCL;

public sealed class Symbol : LispObject
{
    public string Name { get; }

    /// <summary>
    /// The LISP string SYMBOL-NAME answers with, made once per symbol.
    /// A symbol's name never changes, and CLHS leaves modifying the returned
    /// string undefined, so one instance can be shared: SBCL does the same
    /// ((eq (symbol-name 'x) (symbol-name 'x)) is true there).
    ///
    /// Worth caching because the compiler asks constantly: its locals /
    /// free-variable machinery is string-keyed (VAR-NAME), so a fresh string
    /// per call put 11.7M LispStrings, 28% of all objects allocated, into a
    /// single COMPILE-FILE of contrib/asdf/asdf.lisp.
    /// </summary>
    private LispString? _nameString;
    public LispString NameString => _nameString ??= new LispString(Name);
    public Package? HomePackage { get; set; }
    // Mutable Symbol slots are public volatile fields so cross-
    // thread reads see a consistent reference. Reference assignment to a
    // volatile field on .NET is atomic, and the volatile modifier emits the
    // memory barriers that keep one thread's defun/setf-symbol-value visible
    // to other threads without requiring _evalLock to serialize the entire
    // eval. This is preparation for removing _evalLock in concurrent host
    // scenarios (ASP.NET); per-symbol locking / CAS is reserved for Step 2+
    // when contention shows up.
    public volatile LispObject? Value;
    private volatile LispObject? _function;
    private volatile LispObject? _setfFunction;
    public LispObject? Function
    {
        get => _function;
        set
        {
            if (DefinitionJournal.Depth != 0) DefinitionJournal.Note(this);
            _function = value;
        }
    }
    /// <summary>
    /// The (setf name) function for this symbol.
    /// E.g. for symbol CAR, SetfFunction holds the function defined by (defun (setf car) ...).
    /// This is the authoritative storage for setf functions (Phase 1).
    /// </summary>
    public LispObject? SetfFunction
    {
        get => _setfFunction;
        set
        {
            if (DefinitionJournal.Depth != 0) DefinitionJournal.Note(this);
            _setfFunction = value;
        }
    }
    public LispObject Plist { get; set; }

    /// <summary>FIND-CLASS's last answer for this symbol and the class-table
    /// epoch it was computed under (see Runtime.FindClassOrNil).</summary>
    internal sealed class FindClassMemo
    {
        internal readonly LispObject Class;
        internal readonly int Epoch;
        internal FindClassMemo(LispObject cls, int epoch) { Class = cls; Epoch = epoch; }
    }
    internal volatile FindClassMemo? ClassMemo;

    /// <summary>The type tables' answer for this symbol as a type name (see
    /// Runtime.TypeNameInfo): its DEFTYPE expander, if any, under the epoch and
    /// home package it was looked up with, and whether its name is built in.</summary>
    internal sealed class TypeNameMemo
    {
        internal readonly int Epoch;
        internal readonly Package? Package;
        internal readonly string? PackageName;
        internal readonly LispObject? Expander;
        internal readonly bool Builtin;
        /// <summary>Expander's result for the bare symbol, once asked for, with the
        /// class-table epoch it was computed under: an expander may ask FIND-CLASS
        /// (or TYPEP of a class name), so a class defined, redefined or removed
        /// since then can change the answer.</summary>
        internal volatile ExpansionMemo? Expansion;
        internal TypeNameMemo(int epoch, Package? pkg, string? pkgName, LispObject? expander, bool builtin)
        { Epoch = epoch; Package = pkg; PackageName = pkgName; Expander = expander; Builtin = builtin; }
    }
    internal sealed class ExpansionMemo
    {
        internal readonly LispObject Expansion;
        internal readonly int ClassEpoch;
        internal ExpansionMemo(LispObject expansion, int classEpoch) { Expansion = expansion; ClassEpoch = classEpoch; }
    }
    internal volatile TypeNameMemo? TypeMemo;

    /// <summary>The TYPEP test built for this symbol (Runtime.SymbolTypeTest).</summary>
    internal volatile Runtime.SymbolTypeTestMemo? TypeTestMemo;
    public bool IsSpecial { get; set; }

    // True once this symbol has ever been dynamically bound (LET of a special,
    // PROGV, a restored snapshot) on any thread. A symbol that has not been is
    // the common case for a DEFVAR that is only ever SETQ'd, and its reads can
    // skip the binding stack entirely -- the scan (plus the three thread-static
    // reads it needs) measured 5% of a call-heavy profile. Set, never cleared:
    // a stale TRUE only costs a scan that finds nothing, while a stale FALSE
    // would read past a live binding, so the flag is deliberately one-way.
    public bool EverDynamicallyBound;

    public bool IsConstant { get; set; }

    /// <summary>
    /// Proclaimed as a declaration name, via (proclaim '(declaration NAME)).
    /// CLHS TYPE: a symbol cannot name both a type and a declaration, so this
    /// also locks the symbol out of deftype / defclass / defstruct /
    /// define-condition.
    /// </summary>
    public bool IsDeclarationName { get; set; }

    /// <summary>
    /// Globally proclaimed NOTINLINE, via (proclaim '(notinline NAME)) or the
    /// declaim that expands to it. CLHS 3.2.2.1.1: a NOTINLINE declaration in
    /// scope suppresses the function's compiler macro, and a global
    /// proclamation is in scope everywhere. Cleared by an INLINE proclamation.
    /// </summary>
    public bool IsNotinlineProclaimed { get; set; }

    /// <summary>
    /// Globally proclaimed INLINE, via (proclaim '(inline NAME)) or the declaim
    /// that expands to it. Distinct from !IsNotinlineProclaimed: the default
    /// state is neither, and only an explicit INLINE licenses the compiler to
    /// substitute the definition at a call site (CLHS 3.2.2.1.3, which requires
    /// the proclamation to precede the DEFUN for it to take effect). Cleared by
    /// a NOTINLINE proclamation.
    /// </summary>
    public bool IsInlineProclaimed { get; set; }

    public Symbol(string name, Package? homePackage = null)
    {
        Name = name;
        HomePackage = homePackage;
        Plist = Nil.Instance;
    }

    public bool IsBound => Value != null;
    public bool IsFBound => Function != null;

    public override string ToString()
    {
        if (HomePackage == null)
            return $"#:{Name}";
        if (HomePackage.Name == "KEYWORD")
            return $":{Name}";
        return Name;
    }
}

/// <summary>
/// Records which symbols had their function or setf function cell assigned
/// while a window is open, with the cells' values when the window opened.
/// COMPILE-FILE opens one around each form it evaluates at compile time, to
/// tell the definitions that evaluation made (kept after the file is compiled)
/// from the early definitions of plain DEFUNs (removed then). Comparing every
/// symbol of every package before and after each such form cost about 6 ms a
/// scan in an image with a few libraries loaded.
///
/// A window records only the assignments made on the thread that opened it.
/// What COMPILE-FILE removes at the end is what its own compilation defined;
/// a function another thread defines meanwhile (a REPL next to a SLIME or SLY
/// worker that compiles a file) is not part of that and has to stay.
/// </summary>
internal static class DefinitionJournal
{
    internal sealed class Window
    {
        internal readonly Dictionary<Symbol, (LispObject? Fn, LispObject? Setf)> Before =
            new(ReferenceEqualityComparer.Instance);
        internal readonly int OwnerThreadId = System.Environment.CurrentManagedThreadId;
        internal bool Closed;
    }

    private static readonly object s_lock = new();
    private static readonly List<Window> s_open = new();

    /// <summary>The number of open windows; the cell setters check it before
    /// taking the lock.</summary>
    internal static volatile int Depth;

    internal static Window Open()
    {
        var w = new Window();
        lock (s_lock) { s_open.Add(w); Depth = s_open.Count; }
        return w;
    }

    internal static void Close(Window w)
    {
        lock (s_lock)
        {
            if (w.Closed) return;
            w.Closed = true;
            s_open.Remove(w);
            Depth = s_open.Count;
        }
    }

    /// <summary>The cells SYM had when W opened, if they have been assigned since.</summary>
    internal static bool TryGetBefore(Window w, Symbol sym, out (LispObject? Fn, LispObject? Setf) before)
    {
        lock (s_lock) return w.Before.TryGetValue(sym, out before);
    }

    /// <summary>The symbols whose cells have been assigned since W opened.</summary>
    internal static List<Symbol> Touched(Window w)
    {
        lock (s_lock) return new List<Symbol>(w.Before.Keys);
    }

    /// <summary>Called before SYM's function or setf function cell is assigned:
    /// each window this thread opened that has not seen SYM yet records both
    /// cells as they are now.</summary>
    internal static void Note(Symbol sym)
    {
        int thread = System.Environment.CurrentManagedThreadId;
        lock (s_lock)
        {
            var before = (sym.Function, sym.SetfFunction);
            foreach (var w in s_open)
                if (w.OwnerThreadId == thread) w.Before.TryAdd(sym, before);
        }
    }
}
