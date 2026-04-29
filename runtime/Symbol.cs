namespace DotCL;

public class Symbol : LispObject
{
    public string Name { get; }
    public Package? HomePackage { get; set; }
    // Mutable Symbol slots are public volatile fields (#171 Step 1) so cross-
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
    /// This is the authoritative storage for setf functions (Phase 1 of issue #58).
    /// </summary>
    public volatile LispObject? SetfFunction;
    public LispObject Plist { get; set; }
    public bool IsSpecial { get; set; }
    public bool IsConstant { get; set; }

    /// <summary>
    /// Stack of saved dynamic binding values.
    /// Used by DynamicBindings.Push/Pop for save/restore of Symbol.Value.
    /// </summary>
    internal DynamicBindingNode? DynamicSaved;

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
/// Linked list node for per-symbol dynamic binding save stack.
/// </summary>
internal class DynamicBindingNode
{
    public LispObject? SavedValue;
    public DynamicBindingNode? Next;

    public DynamicBindingNode(LispObject? savedValue, DynamicBindingNode? next)
    {
        SavedValue = savedValue;
        Next = next;
    }
}
