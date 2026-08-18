namespace DotCL;

public class Symbol : LispObject
{
    public string Name { get; }

    /// <summary>
    /// The LISP string SYMBOL-NAME answers with, made once per symbol.
    /// A symbol's name never changes, and CLHS leaves modifying the returned
    /// string undefined, so one instance can be shared — SBCL does the same
    /// ((eq (symbol-name 'x) (symbol-name 'x)) is true there).
    ///
    /// Worth caching because the compiler asks constantly: its locals /
    /// free-variable machinery is string-keyed (VAR-NAME), so a fresh string
    /// per call put 11.7M LispStrings — 28% of all objects allocated — into a
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
    public volatile LispObject? Function;
    /// <summary>
    /// The (setf name) function for this symbol.
    /// E.g. for symbol CAR, SetfFunction holds the function defined by (defun (setf car) ...).
    /// This is the authoritative storage for setf functions (Phase 1).
    /// </summary>
    public volatile LispObject? SetfFunction;
    public LispObject Plist { get; set; }
    public bool IsSpecial { get; set; }
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

