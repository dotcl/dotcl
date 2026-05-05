namespace DotCL;

public static partial class Runtime
{

    // --- Setf support ---

    public static LispObject Rplaca(LispObject cons, LispObject val)
    {
        if (cons is not Cons c)
            throw new LispErrorException(new LispTypeError("RPLACA: not a cons", cons));
        c.Car = val;
        return c;
    }

    public static LispObject Rplacd(LispObject cons, LispObject val)
    {
        if (cons is not Cons c)
            throw new LispErrorException(new LispTypeError("RPLACD: not a cons", cons));
        c.Cdr = val;
        return c;
    }

    // --- List utilities ---

    public static LispObject Nreverse(LispObject list)
    {
        if (list is Nil) return Nil.Instance;
        if (list is LispString s)
        {
            var chars = s.Value.ToCharArray();
            Array.Reverse(chars);
            return new LispString(new string(chars));
        }
        if (list is LispVector nv)
        {
            int lo = 0, hi = nv.Length - 1;
            while (lo < hi)
            {
                var tmp = nv.ElementAt(lo);
                nv.SetElement(lo, nv.ElementAt(hi));
                nv.SetElement(hi, tmp);
                lo++; hi--;
            }
            return nv;
        }
        if (list is Cons)
        {
            LispObject prev = Nil.Instance;
            LispObject current = list;
            while (current is Cons c)
            {
                LispObject next = c.Cdr;
                c.Cdr = prev;
                prev = c;
                current = next;
            }
            return prev;
        }
        throw new LispErrorException(new LispTypeError("NREVERSE: not a sequence", list));
    }

    public static LispObject Nth(LispObject n, LispObject list)
    {
        if (n is not Fixnum f || f.Value < 0)
            throw new LispErrorException(new LispTypeError("NTH: index must be a non-negative integer", n, Startup.Sym("UNSIGNED-BYTE")));
        int idx = (int)f.Value;
        LispObject current = list;
        for (int i = 0; i < idx; i++)
        {
            if (current is Cons c) current = c.Cdr;
            else return Nil.Instance;
        }
        if (current is Cons cc) return cc.Car;
        return Nil.Instance;
    }

    public static LispObject Nthcdr(LispObject n, LispObject list)
    {
        if (n is not Fixnum f || f.Value < 0)
            throw new LispErrorException(new LispTypeError("NTHCDR: index must be a non-negative integer", n, Startup.Sym("UNSIGNED-BYTE")));
        int idx = (int)f.Value;
        LispObject current = list;
        for (int i = 0; i < idx; i++)
        {
            if (current is Cons c) current = c.Cdr;
            else if (current is Nil) return Nil.Instance;
            else throw new LispErrorException(new LispTypeError("NTHCDR: not a proper list", current, Startup.Sym("LIST")));
        }
        return current;
    }

    public static LispObject Last(LispObject list)
    {
        if (list is Nil) return Nil.Instance;
        LispObject current = list;
        while (current is Cons c && c.Cdr is Cons)
            current = c.Cdr;
        return current;
    }

    /// <summary>Binary nconc helper: copies a then appends b (non-destructive).
    /// Used only by CLOS method combination. User-visible nconc is in cil-stdlib.lisp
    /// and is properly destructive per CL spec.</summary>
    public static LispObject Nconc2(LispObject a, LispObject b)
    {
        if (a is Nil) return b;
        if (b is Nil) return a;
        // Copy a to avoid creating circular lists when backquote splicing
        // shares list structure (e.g., SBCL's BINDING* macro)
        var head = new Cons(((Cons)a).Car, Nil.Instance);
        var tail = head;
        var cur = ((Cons)a).Cdr;
        while (cur is Cons c)
        {
            var newCell = new Cons(c.Car, Nil.Instance);
            tail.Cdr = newCell;
            tail = newCell;
            cur = c.Cdr;
        }
        tail.Cdr = b;
        return head;
    }

    public static LispObject Butlast(LispObject list)
    {
        if (list is Nil || list is not Cons) return Nil.Instance;
        var head = new Cons(((Cons)list).Car, Nil.Instance);
        var tail = head;
        var cur = ((Cons)list).Cdr;
        while (cur is Cons c && c.Cdr is Cons)
        {
            var newCell = new Cons(c.Car, Nil.Instance);
            tail.Cdr = newCell;
            tail = newCell;
            cur = c.Cdr;
        }
        // If list has only one element, head was created but we should return NIL
        if (((Cons)list).Cdr is not Cons) return Nil.Instance;
        return head;
    }

    public static LispObject CopyList(LispObject list)
    {
        if (list is Nil) return Nil.Instance;
        if (list is not Cons first)
            throw new LispErrorException(new LispTypeError("COPY-LIST: not a list", list));
        var head = new Cons(first.Car, Nil.Instance);
        var tail = head;
        var current = first.Cdr;
        while (current is Cons c)
        {
            var newCons = new Cons(c.Car, Nil.Instance);
            tail.Cdr = newCons;
            tail = newCons;
            current = c.Cdr;
        }
        tail.Cdr = current; // preserve dotted pair
        return head;
    }

    public static LispObject Member(LispObject item, LispObject list)
    {
        if (list is not Nil && list is not Cons)
            throw new LispErrorException(new LispTypeError("MEMBER: not a list", list, Startup.Sym("LIST")));
        var current = list;
        while (current is Cons c)
        {
            if (IsTrueEql(item, c.Car)) return current;
            current = c.Cdr;
        }
        return Nil.Instance;
    }

    public static LispObject Assoc(LispObject key, LispObject alist)
    {
        if (alist is not Nil && alist is not Cons)
            throw new LispErrorException(new LispTypeError("ASSOC: not a list", alist, Startup.Sym("LIST")));
        var current = alist;
        while (current is Cons c)
        {
            var entry = c.Car;
            if (entry is not Nil && entry is not Cons)
                throw new LispErrorException(new LispTypeError("ASSOC: alist entry is not a cons", entry, Startup.Sym("CONS")));
            if (entry is Cons pair && IsTrueEql(key, pair.Car))
                return pair;
            current = c.Cdr;
        }
        if (current is not Nil)
            throw new LispErrorException(new LispTypeError("ASSOC: not a proper list", current, Startup.Sym("LIST")));
        return Nil.Instance;
    }

    /// <summary>
    /// Fast path for (member item list :test #'eq) — uses reference equality.
    /// </summary>
    public static LispObject MemberEq(LispObject item, LispObject list)
    {
        var current = list;
        while (current is Cons c)
        {
            if (IsEqRef(item, c.Car)) return current;
            current = c.Cdr;
        }
        return Nil.Instance;
    }

    /// <summary>
    /// Fast path for (assoc key alist :test #'eq) — uses reference equality.
    /// </summary>
    public static LispObject AssocEq(LispObject key, LispObject alist)
    {
        var current = alist;
        while (current is Cons c)
        {
            if (c.Car is Cons pair && IsEqRef(key, pair.Car))
                return pair;
            current = c.Cdr;
        }
        return Nil.Instance;
    }

    // Compound accessors
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Cadr(LispObject obj) => Car(Cdr(obj));
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static LispObject Cddr(LispObject obj) => Cdr(Cdr(obj));
    public static LispObject Caar(LispObject obj) => Car(Car(obj));
    public static LispObject Cdar(LispObject obj) => Cdr(Car(obj));
    public static LispObject Caddr(LispObject obj) => Car(Cdr(Cdr(obj)));

    // --- Plist operations ---

    public static LispObject Getf(LispObject plist, LispObject indicator, LispObject defaultVal)
    {
        var cur = plist;
        while (cur is Cons c1)
        {
            if (c1.Cdr is not Cons c2)
                throw new LispErrorException(new LispTypeError("GETF: not a proper plist", c1.Cdr, Startup.Sym("LIST")));
            if (IsTrueEq(c1.Car, indicator))
                return c2.Car;
            cur = c2.Cdr;
        }
        if (cur is not Nil)
            throw new LispErrorException(new LispTypeError("GETF: not a proper plist", cur, Startup.Sym("LIST")));
        return defaultVal;
    }

    public static LispObject Putf(LispObject plist, LispObject indicator, LispObject value)
    {
        // Returns a new plist with indicator set to value.
        // If indicator already exists, replace its value.
        var cur = plist;
        while (cur is Cons c1 && c1.Cdr is Cons c2)
        {
            if (IsTrueEq(c1.Car, indicator))
            {
                c2.Car = value; // destructive update
                return plist;
            }
            cur = c2.Cdr;
        }
        // Not found: cons indicator and value onto front
        return new Cons(indicator, new Cons(value, plist));
    }

    // --- &key codegen helper ---

    public static LispObject? FindKeyArg(LispObject[] args, int startIndex, string keyName)
    {
        for (int i = startIndex; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol sym && sym.Name == keyName && sym.HomePackage?.Name == "KEYWORD")
                return args[i + 1];
        }
        return null; // C# null = not found (distinct from Nil.Instance)
    }

    /// <summary>Find keyword arg by exact symbol identity (name + package) for explicit keyword specs like ((foo bar) default).</summary>
    public static LispObject? FindKeyArgByName(LispObject[] args, int startIndex, string keyName, string packageName)
    {
        for (int i = startIndex; i < args.Length - 1; i += 2)
        {
            if (args[i] is Symbol sym && sym.Name == keyName &&
                (packageName == "" ? sym.HomePackage == null : sym.HomePackage?.Name == packageName))
                return args[i + 1];
        }
        return null;
    }

    // --- Load-time-value slots ---
    // Each load-time-value gets a unique integer ID. The slot stores the evaluated result.
    private static readonly Dictionary<int, LispObject> _ltvSlots = new();
    private static int _ltvNextId;

    public static int AllocateLtvSlot() => Interlocked.Increment(ref _ltvNextId);

    public static LispObject GetLtvSlot(int id)
    {
        return _ltvSlots.TryGetValue(id, out var val) ? val : Nil.Instance;
    }

    public static void SetLtvSlot(int id, LispObject value)
    {
        _ltvSlots[id] = value;
    }

    public static bool HasLtvSlot(int id) => _ltvSlots.ContainsKey(id);

    // --- Multiple values ---

    public static LispObject MultipleValuesList()
    {
        var vals = MultipleValues.Get();
        return List(vals);
    }

    public static LispObject MultipleValuesList1(LispObject primary)
    {
        // Stack-based path: MvReturn carries all values directly
        if (primary is MvReturn mv)
        {
            MultipleValues.Reset(); // Consume MV state
            return List(mv.Values);
        }

        // Fallback: thread-static path (backward compat during transition)
        int count = MultipleValues.Count;
        LispObject[] vals = count > 0 ? MultipleValues.Get() : Array.Empty<LispObject>();
        // Consume MV state so it doesn't leak to outer multiple-value-list
        MultipleValues.Reset();
        if (count > 0)
        {
            if (vals.Length > 0 && ReferenceEquals(vals[0], primary))
                return List(vals);
            return new Cons(primary, Nil.Instance);
        }
        if (count == 0)
        {
            return Nil.Instance;
        }
        // count < 0 (sentinel from Reset): non-values function, wrap primary
        return new Cons(primary, Nil.Instance);
    }

    // --- Rest args ---

    public static LispObject CollectRestArgs(LispObject[] args, int startIndex)
    {
        LispObject result = Nil.Instance;
        for (int i = args.Length - 1; i >= startIndex; i--)
            result = new Cons(args[i], result);
        return result;
    }

    // --- Apply / Funcall ---

    public static LispObject Apply(LispObject func, LispObject argList)
    {
        var fn = CoerceToFunction(func);
        var args = new List<LispObject>();
        var current = argList;
        while (current is Cons c)
        {
            args.Add(c.Car);
            current = c.Cdr;
        }
        if (current is not Nil)
            throw new LispErrorException(new LispTypeError("APPLY: last argument is not a proper list", argList, Startup.Sym("LIST")));
        return fn.Invoke(args.ToArray());
    }

    // CL special operators — fdefinition on these should not throw; they are fbound
    public static readonly HashSet<string> _specialOperators = new(StringComparer.OrdinalIgnoreCase)
    {
        "BLOCK", "CATCH", "EVAL-WHEN", "FLET", "FUNCTION", "GO", "IF", "LABELS",
        "LET", "LET*", "LOAD-TIME-VALUE", "LOCALLY", "MACROLET", "MULTIPLE-VALUE-CALL",
        "MULTIPLE-VALUE-PROG1", "PROGN", "PROGV", "QUOTE", "RETURN-FROM", "SETQ",
        "SYMBOL-MACROLET", "TAGBODY", "THE", "THROW", "UNWIND-PROTECT"
    };

    public static LispObject Fdefinition(LispObject name)
    {
        // Handle (setf sym) names — sym.SetfFunction is authoritative (D683, #113).
        if (name is Cons c2 && c2.Car is Symbol setfKw && setfKw.Name == "SETF"
            && c2.Cdr is Cons rest2 && rest2.Car is Symbol setfTarget && rest2.Cdr is Nil)
        {
            if (setfTarget.SetfFunction is LispFunction setfFn) return setfFn;
            throw new LispErrorException(new LispUndefinedFunction(name));
        }
        // Any other cons is not a valid function name — signal type-error (CLHS).
        if (name is Cons)
            throw new LispErrorException(new LispTypeError(
                "FDEFINITION: not a valid function name (expected symbol or (setf symbol))", name));
        var sym = GetSymbol(name, "FDEFINITION");
        if (sym.Function is LispFunction fn) return fn;
        // Check macro table before erroring
        if (_macroFunctions.TryGetValue(sym, out var mfn)) return mfn;
        // Special operators are fbound — return a stub rather than erroring
        if (_specialOperators.Contains(sym.Name))
            return new LispFunction(_ => throw new LispErrorException(
                new LispError($"Cannot call special operator {sym.Name} as a function")), sym.Name);
        throw new LispErrorException(new LispUndefinedFunction(sym));
    }

    /// <summary>SYMBOL-FUNCTION: like FDEFINITION but only accepts symbols.</summary>
    public static LispObject SymbolFunction(LispObject name)
    {
        if (name is not Symbol)
            throw new LispErrorException(new LispTypeError(
                "SYMBOL-FUNCTION: argument must be a symbol", name));
        var sym = (Symbol)name;
        if (sym.Function is LispFunction fn) return fn;
        if (_macroFunctions.TryGetValue(sym, out var mfn)) return mfn;
        if (_specialOperators.Contains(sym.Name))
            return new LispFunction(_ => throw new LispErrorException(
                new LispError($"Cannot call special operator {sym.Name} as a function")), sym.Name);
        throw new LispErrorException(new LispUndefinedFunction(sym));
    }

    public static LispObject SpecialOperatorP(LispObject name)
    {
        var sym = GetSymbol(name, "SPECIAL-OPERATOR-P");
        return _specialOperators.Contains(sym.Name) ? T.Instance : Nil.Instance;
    }

    // Get the canonical string key for a function name (symbol or (setf sym))
    internal static string GetFunctionNameKey(LispObject name, string fn)
    {
        if (name is Symbol sym) return sym.Name;
        if (name is Cons c && c.Car is Symbol setfSym && setfSym.Name == "SETF" && c.Cdr is Cons rest && rest.Car is Symbol target && rest.Cdr is Nil)
            return $"(SETF {target.Name})";
        throw new LispErrorException(new LispTypeError($"{fn}: not a valid function name", name));
    }

    internal static void RemoveCompilerMacro(string name)
    {
        foreach (var pkgName in new[] { "DOTCL-INTERNAL", "DOTCL.CIL-COMPILER" })
        {
            var pkg = Package.FindPackage(pkgName);
            if (pkg != null)
            {
                var (macrosSym, status) = pkg.FindSymbol("*MACROS*");
                if (macrosSym != null && status != SymbolStatus.None
                    && macrosSym.Value is LispHashTable macrosTable)
                {
                    macrosTable.Remove(new LispString(name));
                    break;
                }
            }
        }
    }

    public static LispObject Fmakunbound(LispObject name)
    {
        // (setf sym) form: clear SetfFunction on the target symbol (D683, #113).
        if (name is Cons fc && fc.Car is Symbol fsetfKw && fsetfKw.Name == "SETF"
            && fc.Cdr is Cons frest && frest.Car is Symbol ftarget && frest.Cdr is Nil)
        {
            ftarget.SetfFunction = null;
            return name;
        }
        if (name is Cons)
            throw new LispErrorException(new LispTypeError("FMAKUNBOUND: not a valid function name", name));
        var sym = GetSymbol(name, "FMAKUNBOUND");
        sym.Function = null;
        UnregisterMacroFunction(sym);
        // Also remove from compiler's *macros* hash table
        RemoveCompilerMacro(sym.Name);
        return sym;
    }

    public static LispObject Makunbound(LispObject name)
    {
        var sym = GetSymbol(name, "MAKUNBOUND");
        return DynamicBindings.Makunbound(sym);
    }

    public static LispObject SymbolConstantP(LispObject obj)
    {
        if (obj is Symbol sym && sym.IsConstant)
            return T.Instance;
        return Nil.Instance;
    }

    public static LispObject SetSymbolConstant(LispObject obj)
    {
        if (obj is Symbol sym)
            sym.IsConstant = true;
        return obj;
    }

    public static LispObject MarkSpecial(LispObject obj)
    {
        if (obj is Symbol sym)
            sym.IsSpecial = true;
        return obj;
    }

    // Convert element-type specifier to an ElementTypeName string for LispVector.
    // Expands user-defined types (deftype) before parsing (D601).
    internal static string ParseElementTypeName(LispObject typeSpec)
    {
        if (typeSpec is Nil) return "NIL";
        if (typeSpec is T) return "T";
        if (typeSpec is Symbol ets)
        {
            var known = ets.Name switch
            {
                "T" => "T",
                "NIL" => "NIL",
                "CHARACTER" or "BASE-CHAR" or "STANDARD-CHAR" => ets.Name,
                "BIT" => "BIT",
                "FIXNUM" => "FIXNUM",
                "INTEGER" => "INTEGER",
                "UNSIGNED-BYTE" => "UNSIGNED-BYTE",
                "SIGNED-BYTE" => "SIGNED-BYTE",
                "SINGLE-FLOAT" or "SHORT-FLOAT" => "SINGLE-FLOAT",
                "DOUBLE-FLOAT" or "LONG-FLOAT" => "DOUBLE-FLOAT",
                "FLOAT" => "FLOAT",
                "RATIONAL" => "RATIONAL",
                "REAL" => "REAL",
                "NUMBER" => "NUMBER",
                _ => (string?)null
            };
            if (known != null) return known;
            // Try expanding via deftype (e.g. 'octet → '(unsigned-byte 8))
            if (Runtime.TypeExpanders.TryGetValue(ets.Name, out var expander))
            {
                var expanded = Funcall(expander);
                if (expanded != typeSpec)
                    return ParseElementTypeName(expanded);
            }
            return ets.Name;
        }
        if (typeSpec is Cons etCons && etCons.Car is Symbol etHead)
        {
            string hname = etHead.Name;
            if ((hname == "UNSIGNED-BYTE" || hname == "SIGNED-BYTE") && etCons.Cdr is Cons nbC && nbC.Car is Fixnum nbF)
                return $"{hname}-{nbF.Value}";
            if (hname == "UNSIGNED-BYTE" || hname == "SIGNED-BYTE") return hname;
            if (hname is "SINGLE-FLOAT" or "DOUBLE-FLOAT" or "SHORT-FLOAT" or "LONG-FLOAT" or "FLOAT") return hname;
            if (hname == "INTEGER") return "INTEGER";
            if (hname == "COMPLEX") return etCons.Cdr is Cons cc && cc.Car is Symbol cs ? $"COMPLEX-{cs.Name}" : "COMPLEX";
            // Try expanding compound user-defined types
            if (Runtime.TypeExpanders.TryGetValue(hname, out var compExpander))
            {
                var compArgs = Runtime.ToList(etCons.Cdr).ToArray();
                var expanded = Funcall(compExpander, compArgs);
                if (expanded != typeSpec)
                    return ParseElementTypeName(expanded);
            }
            return hname;
        }
        return "T";
    }

    public static LispFunction CoerceToFunction(LispObject designator)
    {
        designator = Primary(designator);
        if (designator is LispFunction fn) return fn;
        if (designator is Symbol sym)
        {
            if (sym.Function is LispFunction sfn) return sfn;
            // Cross-package bridge: closure defuns compiled inside a let may
            // register via RegisterFunction(string,fn) which lands on a
            // DOTCL-INTERNAL symbol rather than the home-package symbol. Mirror
            // the same fallback used by GetFunctionBySymbol so that (funcall sym)
            // finds the function even when sym.Function is null.
            foreach (var pkg in Package.AllPackages)
            {
                if (pkg == sym.HomePackage) continue;
                var (other, status) = pkg.FindSymbol(sym.Name);
                if (status != SymbolStatus.None && other.Function is LispFunction otherFn)
                {
                    sym.Function = otherFn;
                    return otherFn;
                }
            }
            throw new LispErrorException(new LispUndefinedFunction(sym));
        }
        throw new LispErrorException(new LispTypeError("FUNCALL: not a function designator", designator));
    }

    public static LispObject Funcall(LispObject func, params LispObject[] args)
    {
        var fn = CoerceToFunction(func);
        return fn.Invoke(args);
    }

    // --- Mapcar ---

    public static LispObject Mapcar(LispObject func, LispObject list)
    {
        if (list is not Nil && list is not Cons)
            throw new LispErrorException(new LispTypeError("MAPCAR: not a list", list, Startup.Sym("LIST")));
        var fn = CoerceToFunction(func);
        if (list is not Cons firstCons)
            return Nil.Instance; // Empty list
        // Build result list directly without intermediate List<>
        var head = new Cons(UnwrapMv(fn.Invoke1(firstCons.Car)), Nil.Instance);
        var tail = head;
        var current = firstCons.Cdr;
        while (current is Cons c)
        {
            var next = new Cons(UnwrapMv(fn.Invoke1(c.Car)), Nil.Instance);
            tail.Cdr = next;
            tail = next;
            current = c.Cdr;
        }
        if (current is not Nil)
            throw new LispErrorException(new LispTypeError("MAPCAR: not a proper list", current, Startup.Sym("LIST")));
        return head;
    }

    public static LispObject MapcarN(LispObject func, LispObject[] lists)
    {
        var fn = CoerceToFunction(func);
        int nLists = lists.Length;
        var cursors = new LispObject[nLists];
        for (int i = 0; i < nLists; i++) cursors[i] = lists[i];
        var results = new List<LispObject>();
        while (true)
        {
            var args = new LispObject[nLists];
            bool done = false;
            for (int i = 0; i < nLists; i++)
            {
                if (cursors[i] is Cons c)
                {
                    args[i] = c.Car;
                    cursors[i] = c.Cdr;
                }
                else { done = true; break; }
            }
            if (done) break;
            results.Add(UnwrapMv(fn.Invoke(args)));
        }
        return List(results.ToArray());
    }

    // --- Multiple values ---

    public static LispObject Values(params LispObject[] args) =>
        MultipleValues.Values(args);

    /// <summary>
    /// Unwrap MvReturn to its primary value. Inserted by compiler at non-MV positions.
    /// </summary>
    public static LispObject UnwrapMv(LispObject obj)
    {
        if (obj is MvReturn mv)
        {
            var primary = mv.Values.Length > 0 ? mv.Values[0] : Nil.Instance;
            // Also update thread-static to single value for backward compat
            MultipleValues.Primary(primary);
            return primary;
        }
        return obj;
    }

    public static LispObject ValuesList(LispObject list)
    {
        var items = new System.Collections.Generic.List<LispObject>();
        var cur = list;
        while (cur is Cons c)
        {
            items.Add(c.Car);
            cur = c.Cdr;
        }
        if (cur is not Nil)
            throw new LispErrorException(new LispTypeError("VALUES-LIST: not a proper list", list));
        return MultipleValues.Values(items.ToArray());
    }

    // --- Control flow helpers ---

    /// <summary>
    /// Throws BlockReturnException. Return type is LispObject so it can be used
    /// in expression positions (ternary branches, assignment RHS).
    /// </summary>
    public static LispObject ThrowBlockReturn(object tag, LispObject value) =>
        throw new BlockReturnException(tag, value);

    /// <summary>
    /// Throws GoException. Same expression-position trick as ThrowBlockReturn.
    /// </summary>
    public static LispObject ThrowGo(object tagbodyId, int targetLabel) =>
        throw new GoException(tagbodyId, targetLabel);

    /// <summary>
    /// Wrap a raw .NET exception message as a LispCondition for handler-case.
    /// Called from compiled handler-case catch blocks for System.Exception.
    /// </summary>
    public static LispObject WrapDotNetException(string message)
    {
        return new LispProgramError(message);
    }

    /// <summary>
    /// For handler-bind catch(System.Exception): rethrow Lisp control exceptions
    /// (BlockReturn, CatchThrow, Go, Restart, LispError), wrap others as LispError.
    /// Takes the exception object from the stack.
    /// </summary>
    public static bool IsLispControlFlowException(Exception ex)
        => ex is BlockReturnException || ex is CatchThrowException ||
           ex is GoException || ex is RestartInvocationException ||
           ex is HandlerCaseInvocationException;

    /// <summary>
    /// Signal CONTROL-ERROR for unmatched THROW (no active CATCH for the tag).
    /// Called from compiled throw when CatchTagStack.HasMatchingCatch returns false.
    /// </summary>
    public static void ThrowControlError(LispObject formatStr, LispObject tag)
    {
        string msg = $"Attempt to THROW to tag {tag} but no matching CATCH is active";
        ConditionSystem.Error(new LispControlError(msg));
    }

    public static void RewrapNonLispException(Exception ex)
    {
        // Rethrow Lisp control flow exceptions as-is
        if (ex is BlockReturnException || ex is CatchThrowException ||
            ex is GoException || ex is RestartInvocationException ||
            ex is LispErrorException || ex is HandlerCaseInvocationException)
            throw ex;
        // Wrap unknown .NET exceptions and signal through handler-bind
        var condition = new LispProgramError(ex.Message);
        throw new LispErrorException(condition);
    }

    internal static void RegisterCoreBuiltins()
    {
        // CAR, CDR
        Startup.RegisterUnary("CAR", Runtime.Car);
        Startup.RegisterUnary("CDR", Runtime.Cdr);
        // cXXr compound accessors
        Startup.RegisterUnary("CAAR", o => Runtime.Car(Runtime.Car(o)));
        Startup.RegisterUnary("CADR", o => Runtime.Car(Runtime.Cdr(o)));
        Startup.RegisterUnary("CDAR", o => Runtime.Cdr(Runtime.Car(o)));
        Startup.RegisterUnary("CDDR", o => Runtime.Cdr(Runtime.Cdr(o)));
        Startup.RegisterUnary("CAAAR", o => Runtime.Car(Runtime.Car(Runtime.Car(o))));
        Startup.RegisterUnary("CAADR", o => Runtime.Car(Runtime.Car(Runtime.Cdr(o))));
        Startup.RegisterUnary("CADAR", o => Runtime.Car(Runtime.Cdr(Runtime.Car(o))));
        Startup.RegisterUnary("CADDR", o => Runtime.Car(Runtime.Cdr(Runtime.Cdr(o))));
        Startup.RegisterUnary("CDAAR", o => Runtime.Cdr(Runtime.Car(Runtime.Car(o))));
        Startup.RegisterUnary("CDADR", o => Runtime.Cdr(Runtime.Car(Runtime.Cdr(o))));
        Startup.RegisterUnary("CDDAR", o => Runtime.Cdr(Runtime.Cdr(Runtime.Car(o))));
        Startup.RegisterUnary("CDDDR", o => Runtime.Cdr(Runtime.Cdr(Runtime.Cdr(o))));
        Startup.RegisterUnary("CAAAAR", o => Runtime.Car(Runtime.Car(Runtime.Car(Runtime.Car(o)))));
        Startup.RegisterUnary("CAAADR", o => Runtime.Car(Runtime.Car(Runtime.Car(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CAADAR", o => Runtime.Car(Runtime.Car(Runtime.Cdr(Runtime.Car(o)))));
        Startup.RegisterUnary("CAADDR", o => Runtime.Car(Runtime.Car(Runtime.Cdr(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CADAAR", o => Runtime.Car(Runtime.Cdr(Runtime.Car(Runtime.Car(o)))));
        Startup.RegisterUnary("CADADR", o => Runtime.Car(Runtime.Cdr(Runtime.Car(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CADDAR", o => Runtime.Car(Runtime.Cdr(Runtime.Cdr(Runtime.Car(o)))));
        Startup.RegisterUnary("CADDDR", o => Runtime.Car(Runtime.Cdr(Runtime.Cdr(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CDAAAR", o => Runtime.Cdr(Runtime.Car(Runtime.Car(Runtime.Car(o)))));
        Startup.RegisterUnary("CDAADR", o => Runtime.Cdr(Runtime.Car(Runtime.Car(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CDADAR", o => Runtime.Cdr(Runtime.Car(Runtime.Cdr(Runtime.Car(o)))));
        Startup.RegisterUnary("CDADDR", o => Runtime.Cdr(Runtime.Car(Runtime.Cdr(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CDDAAR", o => Runtime.Cdr(Runtime.Cdr(Runtime.Car(Runtime.Car(o)))));
        Startup.RegisterUnary("CDDADR", o => Runtime.Cdr(Runtime.Cdr(Runtime.Car(Runtime.Cdr(o)))));
        Startup.RegisterUnary("CDDDAR", o => Runtime.Cdr(Runtime.Cdr(Runtime.Cdr(Runtime.Car(o)))));
        Startup.RegisterUnary("CDDDDR", o => Runtime.Cdr(Runtime.Cdr(Runtime.Cdr(Runtime.Cdr(o)))));

        // LENGTH
        Startup.RegisterUnary("LENGTH", Runtime.Length);

        // Symbol accessors
        Startup.RegisterUnary("SYMBOL-NAME", Runtime.SymbolName);
        Startup.RegisterUnary("SYMBOL-VALUE", Runtime.SymbolValue);
        Startup.RegisterUnary("SYMBOL-PACKAGE", Runtime.SymbolPackage);
        Emitter.CilAssembler.RegisterFunction("(SETF SYMBOL-VALUE)", new LispFunction(args => {
            if (args.Length != 2)
                throw new LispErrorException(new LispProgramError("(SETF SYMBOL-VALUE): expected 2 arguments"));
            return Runtime.SetSymbolValue(args[1], args[0]);
        }, "(SETF SYMBOL-VALUE)", 2));
        Emitter.CilAssembler.RegisterFunction("(SETF SYMBOL-FUNCTION)", new LispFunction(args => {
            if (args.Length != 2)
                throw new LispErrorException(new LispProgramError("(SETF SYMBOL-FUNCTION): expected 2 arguments"));
            if (args[0] is not LispFunction fn)
                throw new LispErrorException(new LispTypeError("(SETF SYMBOL-FUNCTION): not a function", args[0]));
            var sym = Runtime.GetSymbol(args[1], "(SETF SYMBOL-FUNCTION)");
            sym.Function = fn;
            Emitter.CilAssembler.RegisterFunction(sym.Name, fn);
            return args[0];
        }, "(SETF SYMBOL-FUNCTION)", 2));

        // FIND-ALL-SYMBOLS
        Startup.RegisterUnary("FIND-ALL-SYMBOLS", Runtime.FindAllSymbols);

        // %PROGV-BIND, %PROGV-UNBIND
        Startup.RegisterBinary("%PROGV-BIND", DynamicBindings.ProgvBind);
        Startup.RegisterUnary("%PROGV-UNBIND", DynamicBindings.ProgvUnbind);

        // %OBJECT-ID, FUNCTION-LAMBDA-EXPRESSION
        Startup.RegisterUnary("%OBJECT-ID", obj =>
            new Fixnum(System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj)));
        Startup.RegisterUnary("FUNCTION-LAMBDA-EXPRESSION", obj =>
            MultipleValues.Values(Nil.Instance, Nil.Instance, Nil.Instance));

        // BOUNDP
        Emitter.CilAssembler.RegisterFunction("BOUNDP", new LispFunction(args => {
            if (args.Length != 1) throw new LispErrorException(new LispProgramError($"BOUNDP: wrong number of arguments: {args.Length}"));
            return Runtime.Boundp(args[0]);
        }));
        // %SYMBOL-SPECIAL-P
        Startup.RegisterUnary("%SYMBOL-SPECIAL-P", obj => {
            var sym = Runtime.GetSymbol(obj, "%SYMBOL-SPECIAL-P");
            return sym.IsSpecial ? T.Instance : Nil.Instance;
        });
        // %GET-VARIABLE-DOCUMENTATION
        Startup.RegisterUnary("%GET-VARIABLE-DOCUMENTATION", Runtime.GetVariableDocumentation);

        // SPECIAL-OPERATOR-P (re-register as variadic for funcall compatibility)
        Startup.RegisterUnary("SPECIAL-OPERATOR-P", Runtime.SpecialOperatorP);

        // SET, SYMBOL-CONSTANT-P, SET-SYMBOL-CONSTANT
        Startup.RegisterBinary("SET", (symObj, val) => {
            if (symObj is not Symbol sym)
                throw new LispErrorException(new LispTypeError("SET: not a symbol", symObj));
            DynamicBindings.Set(sym, val);
            return val;
        });
        Startup.RegisterUnary("SYMBOL-CONSTANT-P", Runtime.SymbolConstantP);
        Startup.RegisterUnary("SET-SYMBOL-CONSTANT", Runtime.SetSymbolConstant);

        // COPY-STRUCTURE, MAKE-SYMBOL
        Startup.RegisterUnary("COPY-STRUCTURE", obj => {
            if (obj is not LispStruct s)
                throw new LispErrorException(new LispTypeError("COPY-STRUCTURE: not a structure", obj));
            return new LispStruct(s.TypeName, (LispObject[])s.Slots.Clone());
        });
        Startup.RegisterUnary("MAKE-SYMBOL", obj =>
            obj is LispString ls ? new Symbol(ls.Value)
            : obj is LispVector lv && lv.IsCharVector ? new Symbol(lv.ToCharString())
            : throw new LispErrorException(new LispTypeError("MAKE-SYMBOL: not a string", obj)));

        // MAPCAR (variadic)
        Emitter.CilAssembler.RegisterFunction("MAPCAR",
            new LispFunction(args => {
                if (args.Length < 2)
                    throw new LispErrorException(new LispProgramError("MAPCAR: too few arguments"));
                if (args.Length == 2)
                    return Runtime.Mapcar(args[0], args[1]);
                var lists = new LispObject[args.Length - 1];
                Array.Copy(args, 1, lists, 0, lists.Length);
                return Runtime.MapcarN(args[0], lists);
            }));

        // STRING-TRIM, STRING-LEFT-TRIM, STRING-RIGHT-TRIM
        Startup.RegisterBinary("STRING-TRIM", Runtime.StringTrim);
        Startup.RegisterBinary("STRING-LEFT-TRIM", Runtime.StringLeftTrim);
        Startup.RegisterBinary("STRING-RIGHT-TRIM", Runtime.StringRightTrim);

        // Character comparisons (variadic N-arg)
        Emitter.CilAssembler.RegisterFunction("CHAR=", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("CHAR=: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (!Runtime.IsTruthy(Runtime.CharEqual(args[i-1], args[i]))) return Nil.Instance;
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR<", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("CHAR<: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (!Runtime.IsTruthy(Runtime.CharLt(args[i-1], args[i]))) return Nil.Instance;
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR>", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("CHAR>: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (!Runtime.IsTruthy(Runtime.CharGt(args[i-1], args[i]))) return Nil.Instance;
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR<=", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("CHAR<=: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (!Runtime.IsTruthy(Runtime.CharLe(args[i-1], args[i]))) return Nil.Instance;
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR>=", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("CHAR>=: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (!Runtime.IsTruthy(Runtime.CharGe(args[i-1], args[i]))) return Nil.Instance;
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR/=", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("CHAR/=: requires at least 1 argument"));
            for (int i = 0; i < args.Length; i++)
                for (int j = i + 1; j < args.Length; j++)
                    if (Runtime.IsTruthy(Runtime.CharEqual(args[i], args[j]))) return Nil.Instance;
            return T.Instance;
        }));
        // Case-insensitive character comparisons
        static char foldCI(LispObject o, string fn) => o is LispChar c ? Runtime.CharFoldCase(c)
            : throw new LispErrorException(new LispTypeError($"{fn}: not a character", o));
        Emitter.CilAssembler.RegisterFunction("CHAR-EQUAL", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("CHAR-EQUAL: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (foldCI(args[i-1], "CHAR-EQUAL") != foldCI(args[i], "CHAR-EQUAL")) return Nil.Instance;
            if (args.Length == 1) foldCI(args[0], "CHAR-EQUAL");
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR-NOT-EQUAL", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("CHAR-NOT-EQUAL: requires at least 1 argument"));
            for (int i = 0; i < args.Length; i++)
                for (int j = i+1; j < args.Length; j++)
                    if (foldCI(args[i], "CHAR-NOT-EQUAL") == foldCI(args[j], "CHAR-NOT-EQUAL")) return Nil.Instance;
            if (args.Length == 1) foldCI(args[0], "CHAR-NOT-EQUAL");
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR-LESSP", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("CHAR-LESSP: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (foldCI(args[i-1], "CHAR-LESSP") >= foldCI(args[i], "CHAR-LESSP")) return Nil.Instance;
            if (args.Length == 1) foldCI(args[0], "CHAR-LESSP");
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR-GREATERP", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("CHAR-GREATERP: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (foldCI(args[i-1], "CHAR-GREATERP") <= foldCI(args[i], "CHAR-GREATERP")) return Nil.Instance;
            if (args.Length == 1) foldCI(args[0], "CHAR-GREATERP");
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR-NOT-LESSP", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("CHAR-NOT-LESSP: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (foldCI(args[i-1], "CHAR-NOT-LESSP") < foldCI(args[i], "CHAR-NOT-LESSP")) return Nil.Instance;
            if (args.Length == 1) foldCI(args[0], "CHAR-NOT-LESSP");
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("CHAR-NOT-GREATERP", new LispFunction(args => {
            if (args.Length < 1) throw new LispErrorException(new LispProgramError("CHAR-NOT-GREATERP: requires at least 1 argument"));
            for (int i = 1; i < args.Length; i++)
                if (foldCI(args[i-1], "CHAR-NOT-GREATERP") > foldCI(args[i], "CHAR-NOT-GREATERP")) return Nil.Instance;
            if (args.Length == 1) foldCI(args[0], "CHAR-NOT-GREATERP");
            return T.Instance;
        }));

        // Readtable functions
        Emitter.CilAssembler.RegisterFunction("COPY-READTABLE",
            new LispFunction(args => {
                if (args.Length > 2)
                    throw new LispErrorException(new LispProgramError($"COPY-READTABLE: too many arguments: {args.Length} (expected at most 2)"));
                LispReadtable from;
                if (args.Length == 0 || args[0] is not Nil)
                {
                    if (args.Length > 0 && args[0] is LispReadtable rt)
                        from = rt;
                    else if (args.Length == 0)
                        from = (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
                    else
                        from = Startup.StandardReadtable;
                }
                else
                {
                    from = Startup.StandardReadtable;
                }
                if (args.Length >= 2 && args[1] is LispReadtable toRt)
                {
                    toRt.CopyFrom(from);
                    return toRt;
                }
                return from.Clone();
            }));
        Emitter.CilAssembler.RegisterFunction("READTABLEP",
            new LispFunction(args => {
                if (args.Length != 1)
                    throw new LispErrorException(new LispProgramError($"READTABLEP: expected 1 argument, got {args.Length}"));
                return args[0] is LispReadtable ? (LispObject)T.Instance : Nil.Instance;
            }));
        Emitter.CilAssembler.RegisterFunction("READTABLE-CASE", new LispFunction(args => {
            if (args.Length != 1)
                throw new LispErrorException(new LispProgramError($"READTABLE-CASE: wrong number of arguments: {args.Length} (expected 1)"));
            if (args[0] is not LispReadtable rt)
                throw new LispErrorException(new LispTypeError("READTABLE-CASE: not a readtable", args[0], Startup.Sym("READTABLE")));
            return Startup.Keyword(rt.Case.ToString().ToUpperInvariant());
        }));
        Emitter.CilAssembler.RegisterFunction("%SET-READTABLE-CASE", new LispFunction(args => {
            if (args.Length < 2) throw new System.Exception("%SET-READTABLE-CASE: requires readtable and case arguments");
            if (args[0] is not LispReadtable rt) throw new LispErrorException(new LispTypeError("%SET-READTABLE-CASE: not a readtable", args[0]));
            var caseSym = args[1] as Symbol ?? throw new LispErrorException(new LispTypeError("%SET-READTABLE-CASE: case must be a keyword symbol", args[1]));
            rt.Case = caseSym.Name switch {
                "UPCASE" => ReadtableCase.Upcase,
                "DOWNCASE" => ReadtableCase.Downcase,
                "PRESERVE" => ReadtableCase.Preserve,
                "INVERT" => ReadtableCase.Invert,
                _ => throw new LispErrorException(new LispTypeError($"%SET-READTABLE-CASE: invalid case {caseSym.Name}", args[1]))
            };
            return args[1];
        }));
        Emitter.CilAssembler.RegisterFunction("SET-SYNTAX-FROM-CHAR", new LispFunction(args => {
            if (args.Length < 2) throw new System.Exception("SET-SYNTAX-FROM-CHAR: requires 2+ arguments");
            char toChar = ((LispChar)args[0]).Value;
            char fromChar = ((LispChar)args[1]).Value;
            var toRt = args.Length >= 3 && args[2] is LispReadtable rt1 ? rt1
                : (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            var fromRt = args.Length >= 4 && args[3] is LispReadtable rt2 ? rt2
                : Startup.StandardReadtable;
            toRt.CopySyntax(toChar, fromChar, fromRt);
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("GET-MACRO-CHARACTER", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("GET-MACRO-CHARACTER: too few arguments: 0 (expected at least 1)"));
            if (args.Length > 2) throw new LispErrorException(new LispProgramError($"GET-MACRO-CHARACTER: too many arguments: {args.Length} (expected at most 2)"));
            char ch = ((LispChar)args[0]).Value;
            var rt = args.Length >= 2 && args[1] is LispReadtable rt1 ? rt1
                : (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            var fn = rt.GetMacroFunction(ch);
            if (fn == null) return Nil.Instance;
            bool nonTerm = rt.IsNonTerminating(ch);
            var lispFn = rt.GetLispMacroFunction(ch);
            if (lispFn == null)
            {
                var csFn = fn;
                lispFn = new LispFunction(fargs => {
                    var textReader = Runtime.GetTextReader(fargs[0]);
                    var reader = new Reader(textReader) { LispStreamRef = fargs[0] };
                    // Share #n=/#n# label tables with the stream so share references
                    // work across Reader instances (e.g. when SBCL's Lisp reader calls
                    // C# dispatch macros through GET-MACRO-CHARACTER)
                    if (fargs[0] is LispStream ls5)
                        reader.AdoptStreamShareTables(ls5);
                    // Transfer unread char from stream to reader pushback
                    if (fargs[0] is LispStream ls2 && ls2.UnreadCharValue != -1)
                    {
                        reader.UnreadChar(ls2.UnreadCharValue);
                        ls2.UnreadCharValue = -1;
                    }
                    char macroChar = ((LispChar)fargs[1]).Value;
                    var result = csFn(reader, macroChar);
                    if (result == null)
                    {
                        // C# macro returned no value — signal zero values to Lisp caller
                        MultipleValues.Set();
                        return Nil.Instance;
                    }
                    return result;
                });
            }
            return MultipleValues.Values(lispFn, nonTerm ? T.Instance : Nil.Instance);
        }));
        Emitter.CilAssembler.RegisterFunction("SET-MACRO-CHARACTER", new LispFunction(args => {
            if (args.Length < 2) throw new System.Exception("SET-MACRO-CHARACTER: requires char and function");
            char ch = ((LispChar)args[0]).Value;
            var lispFn = args[1];
            bool nonTerm = args.Length >= 3 && Runtime.IsTruthy(args[2]);
            var rt = args.Length >= 4 && args[3] is LispReadtable rt1 ? rt1
                : (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            rt.SetMacroCharacter(ch, (reader, c) => {
                LispObject stream = reader.LispStreamRef ?? new LispInputStream(reader.Input);
                var result = Runtime.Funcall(lispFn, new LispObject[] { stream, LispChar.Make(c) });
                // CLHS: reader macro returning zero values means "skip" (like a comment).
                // Returning NIL means the read object is NIL. Check MultipleValues.Count
                // to distinguish (values) from returning nil.
                return (result is Nil && MultipleValues.Count == 0) ? null : result;
            }, nonTerm, lispFn);
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("MAKE-DISPATCH-MACRO-CHARACTER", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("MAKE-DISPATCH-MACRO-CHARACTER: too few arguments: 0 (expected at least 1)"));
            if (args.Length > 3) throw new LispErrorException(new LispProgramError($"MAKE-DISPATCH-MACRO-CHARACTER: too many arguments: {args.Length} (expected at most 3)"));
            char ch = ((LispChar)args[0]).Value;
            bool nonTerm = args.Length >= 2 && Runtime.IsTruthy(args[1]);
            var rt = args.Length >= 3 && args[2] is LispReadtable rt1 ? rt1
                : (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            rt.MakeDispatchMacroCharacter(ch, nonTerm);
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("SET-DISPATCH-MACRO-CHARACTER", new LispFunction(args => {
            if (args.Length < 3) throw new System.Exception("SET-DISPATCH-MACRO-CHARACTER: requires 3+ arguments");
            char dispChar = ((LispChar)args[0]).Value;
            char subChar = ((LispChar)args[1]).Value;
            var lispFn = args[2];
            var rt = args.Length >= 4 && args[3] is LispReadtable rt1 ? rt1
                : (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            rt.SetDispatchMacroCharacter(dispChar, subChar, (reader, c, n) => {
                LispObject stream = reader.LispStreamRef ?? new LispInputStream(reader.Input);
                var result = Runtime.Funcall(lispFn, new LispObject[] {
                    stream, LispChar.Make(c),
                    n >= 0 ? (LispObject)Fixnum.Make(n) : Nil.Instance
                });
                return (result is Nil && MultipleValues.Count == 0) ? null : result;
            }, lispFn);
            return T.Instance;
        }));
        Emitter.CilAssembler.RegisterFunction("GET-DISPATCH-MACRO-CHARACTER", new LispFunction(args => {
            if (args.Length < 2) throw new System.Exception("GET-DISPATCH-MACRO-CHARACTER: requires 2 arguments");
            char dispChar = ((LispChar)args[0]).Value;
            char subChar = ((LispChar)args[1]).Value;
            var rt = args.Length >= 3 && args[2] is LispReadtable rt1 ? rt1
                : (LispReadtable)DynamicBindings.Get(Startup.Sym("*READTABLE*"));
            if (rt.GetDispatchTable(dispChar) == null)
                throw new LispErrorException(new LispError($"{dispChar} is not a dispatching macro character in the current readtable"));
            // Digit sub-chars return nil per CLHS
            if (char.IsAsciiDigit(subChar)) return Nil.Instance;
            var lispFn2 = rt.GetLispDispatchMacroFunction(dispChar, subChar);
            if (lispFn2 != null) return lispFn2;
            // Built-in C# dispatch functions have no Lisp counterpart; return nil
            return Nil.Instance;
        }));
    }


}
