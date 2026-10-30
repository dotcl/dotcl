using System.Reflection;
using System.Reflection.Emit;

namespace DotCL.Emitter;

public partial class CilAssembler
{
    /// <summary>The values of (MAKE-LOAD-FORM OBJ), asked once per object per
    /// file (CLHS 3.2.4.4 lets a method count on being called once for each
    /// object a file names): several places look at the answer, the
    /// reader-route check and the emitters among them.</summary>
    [ThreadStatic] private static Dictionary<LispObject, LispObject>? s_mlfMemo;

    /// <summary>Forget the answers when the file is done.</summary>
    internal static void ClearMakeLoadFormMemo() => s_mlfMemo = null;

    internal static Dictionary<LispObject, LispObject>? MakeLoadFormMemo
    {
        get => s_mlfMemo;
        set => s_mlfMemo = value;
    }

    private static LispObject CallMakeLoadForm(LispObject mlfFn, LispObject obj)
    {
        var memo = s_mlfMemo;
        if (memo != null && memo.TryGetValue(obj, out var known)) return known;
        // Every caller treats an error here as "no load form" and falls back,
        // so the error is taken where it is signalled, before an outer handler
        // or *DEBUGGER-HOOK* can see it.
        var tag = new object();
        HandlerClusterStack.PushCluster(new[] { new HandlerBinding(Startup.Sym("ERROR"),
            new LispFunction(a => throw new HandlerCaseInvocationException(tag, 0, a[0]),
                "%MAKE-LOAD-FORM-GUARD", 1)) });
        LispObject raw;
        try
        {
            raw = Runtime.Funcall(mlfFn, obj);
        }
        finally
        {
            HandlerClusterStack.PopCluster();
        }
        if (memo != null) memo[obj] = raw;
        return raw;
    }

    private static bool StructReadsBackAsPrinted(LispStruct ls)
    {
        if (Runtime.FindClassOrNil(ls.TypeName) is not LispClass cls
            || !cls.IsStructureClass || cls.StructSlotNames == null
            || cls.StructSlotNames.Length != ls.SlotCount)
            return false;

        // No make-load-form to honour -- no method, or one that signals -- is the
        // case TryEmitViaLoadForm already answers by falling back to the direct
        // intern path, which allocates the struct and fills its slots. That is
        // what #S does, so the text route is equivalent there too.
        var mlfSym = Startup.Sym("MAKE-LOAD-FORM");
        if (mlfSym?.Function == null) return true;
        LispObject form;
        try
        {
            var raw = CallMakeLoadForm(mlfSym.Function, ls);
            form = raw is MvReturn mv && mv.Count > 0 ? mv[0] : raw;
        }
        catch { return true; }
        if (form is Nil) return true;   // same fallback
        return FormIsSlotSaving(ls, cls, form);
    }

    /// <summary>Whether a make-load-form creation form is exactly what
    /// make-load-form-saving-slots produces for this object:
    ///   (let ((v (allocate-instance (find-class 'TYPE))))
    ///     (setf (slot-value v 'SLOT) 'VALUE) ...
    ///     v)
    /// with a setf for every slot and each value the object's own. When it is,
    /// the form carries no information beyond "allocate this type and put these
    /// values in its slots", which both the text route (#K) and the direct
    /// intern route express without building the form or evaluating it.</summary>
    private static bool FormIsSlotSaving(LispStruct ls, LispClass cls, LispObject form)
    {
        var slotNames = cls.StructSlotNames;
        if (slotNames == null || slotNames.Length != ls.SlotCount) return false;
        // (let ((v <alloc>)) <setf>... v)
        if (form is not Cons let || !IsSym(let.Car, "LET")) return false;
        if (Nth(let, 1) is not Cons bindings || bindings.Cdr is not Nil) return false;
        if (bindings.Car is not Cons binding || binding.Car is not Symbol var) return false;
        if (Nth(binding, 1) is not Cons alloc || !IsSym(alloc.Car, "ALLOCATE-INSTANCE"))
            return false;
        if (Nth(alloc, 1) is not Cons findClass || !IsSym(findClass.Car, "FIND-CLASS")
            || Unquote(Nth(findClass, 1)) is not Symbol typeName
            || !ReferenceEquals(typeName, ls.TypeName))
            return false;

        var assigned = new bool[ls.SlotCount];
        var body = let.Cdr is Cons c2 ? c2.Cdr : Nil.Instance;
        while (body is Cons b)
        {
            if (b.Cdr is Nil)
                return ReferenceEquals(b.Car, var) && System.Array.TrueForAll(assigned, x => x);
            if (b.Car is not Cons setf || !IsSym(setf.Car, "SETF")) return false;
            if (Nth(setf, 1) is not Cons place || !IsSym(place.Car, "SLOT-VALUE")
                || !ReferenceEquals(Nth(place, 1), var)
                || Unquote(Nth(place, 2)) is not Symbol slotName)
                return false;
            int idx = System.Array.FindIndex(slotNames, n => ReferenceEquals(n, slotName));
            if (idx < 0 || assigned[idx]) return false;
            if (!ReferenceEquals(Unquote(Nth(setf, 2)), ls.GetSlot(idx))) return false;
            assigned[idx] = true;
            body = b.Cdr;
        }
        return false;
    }

    private static bool IsSym(LispObject o, string name) =>
        o is Symbol s && s.Name == name;

    private static LispObject Nth(LispObject list, int n)
    {
        while (n-- > 0 && list is Cons c) list = c.Cdr;
        return list is Cons head ? head.Car : Nil.Instance;
    }

    /// <summary>(QUOTE x) -> x; anything else unchanged. The creation form quotes
    /// every value it stores, so the comparison is against what was quoted.</summary>
    private static LispObject Unquote(LispObject o) =>
        o is Cons c && IsSym(c.Car, "QUOTE") && c.Cdr is Cons q ? q.Car : o;

    private static void Bump(Dictionary<string, int> d, string k) =>
        d[k] = d.TryGetValue(k, out var n) ? n + 1 : 1;

    /// <summary>Census only: the parts of an unprintable node that are themselves
    /// literal graph. Slots of a struct, elements of any array. Best effort --
    /// this drives a measurement, not code generation.</summary>
    private static IEnumerable<LispObject> CensusChildren(LispObject node)
    {
        switch (node)
        {
            case LispStruct ls:
                for (int i = 0; i < ls.SlotCount; i++)
                    if (ls.GetSlot(i) is LispObject o) yield return o;
                break;
            case LispVector v:
                for (int i = 0; i < v.Length; i++) yield return v.ElementAt(i);
                break;
        }
    }

    /// <summary>Emit IL that reconstructs a constant at load time by reading its
    /// *print-circle* representation, instead of the naive inline cons-walk (which
    /// loops forever on a cycle and duplicates shared structure). The representation
    /// is computed now and re-read via read-from-string in the loading process.
    /// Returns false (emitting nothing) if the value cannot be printed readably;
    /// the caller then errors (cyclic) or falls back to inline (shared).</summary>
    private bool TryEmitConstantViaReader(LispObject val)
    {
        string repr;
        var pcSym = Startup.Sym("*PRINT-CIRCLE*");
        var prSym = Startup.Sym("*PRINT-READABLY*");
        var plSym = Startup.Sym("*PRINT-LEVEL*");
        var pnSym = Startup.Sym("*PRINT-LENGTH*");
        var pkgSym = Startup.Sym("*PACKAGE*");
        var rdffSym = Startup.Sym("*READ-DEFAULT-FLOAT-FORMAT*");
        DynamicBindings.Push(pcSym, T.Instance);
        DynamicBindings.Push(prSym, T.Instance);
        DynamicBindings.Push(plSym, Nil.Instance);   // never truncate the repr
        DynamicBindings.Push(pnSym, Nil.Instance);
        // Print with *package* = KEYWORD so every interned symbol carries a full
        // package qualifier. The repr is re-read in the LOADING process (or later
        // in this one) under an arbitrary *package*; an unqualified name would
        // re-intern relative to that package (e.g. SBCL's cold-build load re-read
        // NIL/FIXNUM into its XC packages, corrupting reconstructed constants).
        DynamicBindings.Push(pkgSym, Startup.KeywordPkg);
        // Print floats under the standard default format, and read them back the
        // same way (ReadConstantFromString pins the same value). Which floats get
        // an exponent marker depends on *read-default-float-format*, so a file
        // compiled under DOUBLE-FLOAT wrote its doubles bare: and a loading
        // process with the standard default read them back as single floats.
        // Silent: the literal is still a number of the right value, just the
        // wrong type.
        DynamicBindings.Push(rdffSym, Startup.Sym("SINGLE-FLOAT"));
        // ...and with `::` for every qualified symbol, so the text does not depend on
        // which symbols happen to be external in THIS image. See ForceInternalSymbolSyntax.
        Runtime.ForceInternalSymbolSyntax = true;
        // Uninterned symbols print as `#<n>U`, an index into this fasl's symbol
        // table, so the literal round-trips with EQ intact. Only in fasl mode:
        // AssembleAndRun has no table to index, and there `#:NAME` losing identity
        // is the behaviour that was already there.
        bool namesTable = _faslMode && _faslStructMap?.UninternedTypeBuilder != null;
        bool usedTable = false;
        Runtime.FaslStructSyntax = _faslMode;
        if (namesTable)
        {
            var map = _faslStructMap!;
            Runtime.UninternedSymbolIndexer = sym =>
            {
                usedTable = true;
                return map.GetOrCreateUninternedSymbolIndex(sym);
            };
        }
        try { repr = ((LispString)Runtime.WriteToString(val)).Value; }
        catch { return false; }                      // unprintable (e.g. unreadable struct)
        finally
        {
            Runtime.UninternedSymbolIndexer = null;
            Runtime.FaslStructSyntax = false;
            Runtime.ForceInternalSymbolSyntax = false;
            DynamicBindings.Pop(rdffSym);
            DynamicBindings.Pop(pkgSym);
            DynamicBindings.Pop(pnSym); DynamicBindings.Pop(plSym);
            DynamicBindings.Pop(prSym); DynamicBindings.Pop(pcSym);
        }

        // Runtime.ReadConstantFromString(repr): reads one object, no multiple-values
        // side effect (READ-FROM-STRING leaves a stray position value that can leak
        // through the FASL module-init).
        // The inline path records every symbol it names so the fasl interns them
        // at load even when the literal itself is not built until later (a symbol
        // that appears only inside a macro's backquote template, which a later
        // file's defpackage :import-from expects to exist). Reading the literal
        // interns its symbols too, but only when that literal runs, so the
        // guarantee has to be restated here.
        if (_faslMode) RecordSymbolsForPreintern(val);

        // Self-check: read the text back now and compare. The route is only
        // correct if print then read gives the same object graph, and a literal
        // that does not survive it fails much later as a wrong VALUE -- not as an
        // error at the point it was written. Off unless asked.
        if (Environment.GetEnvironmentVariable("DOTCL_LITERAL_VERIFY") == "1")
        {
            try
            {
                var utf8 = System.Text.Encoding.UTF8.GetBytes(repr);
                var back = namesTable && usedTable
                    ? Runtime.ReadConstantFromUtf8(utf8, BuildVerifyTable())
                    : Runtime.ReadConstantFromUtf8(utf8);
                // Compare the TEXT, not the objects: EQUAL is EQ on structures, so
                // comparing objects calls every struct literal a mismatch. What the
                // route has to guarantee is that printing the result again gives
                // the same text.
                var again = PrintFaslRepr(back, namesTable);
                if (again != repr)
                    Console.Error.WriteLine(
                        $"#LITROUNDTRIP mismatch:\n  wrote: {Truncate(repr, 160)}\n  read : {Truncate(again ?? "<unprintable>", 160)}");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(
                    $"#LITROUNDTRIP error {ex.GetType().Name}: {Truncate(repr, 200)}");
            }
        }

        if (!EmitReprAsData(repr, usedTable))
        {
            _il.Emit(OpCodes.Ldstr, Track(repr));
            _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("ReadConstantFromString", new[] { typeof(string) })!);
        }
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
        return true;
    }

    /// <summary>Push an interned symbol, resolved once per site rather than on
    /// every execution. The symbol is immortal, so a site can remember it.
    ///
    /// The FASL form fills a static field on first execution rather than in the
    /// type initializer: interning at load time is too early, because uiop's
    /// DEFINE-PACKAGE recycles a package by uninterning and re-homing its
    /// symbols, so a symbol interned before the defining form has run is a
    /// different object from the one the later DEFUN attaches a function to --
    /// asdf died with "Undefined function: ENSURE-PACKAGE" loading itself.</summary>
    /// <summary>Load the keyword NAME, resolved once rather than at every
    /// evaluation (FASL: a static field the type initializer fills; in memory:
    /// the constant pool). Every evaluated keyword literal used to pay the
    /// string-keyed lookup in Startup.Keyword, including the keyword tests a
    /// &amp;key parser runs on every call.</summary>
    private void EmitKeyword(string name)
    {
        if (_faslMode && _faslStructMap?.UninternedTypeBuilder != null
            && _faslStructMap.SymFnSiteInitIl != null)
        {
            _il.Emit(OpCodes.Ldsfld, _faslStructMap.GetOrCreateKeywordField(name));
        }
        else if (!_faslMode)
        {
            EmitConstantAs(AddSymbolConstant(Startup.Keyword(name)), typeof(Symbol));
        }
        else
        {
            _il.Emit(OpCodes.Ldstr, Track(name));
            _il.Emit(OpCodes.Call, _methodCache["Startup.Keyword"]);
        }
    }

    /// <summary>FASL mode: push the inline cache cell (a ReaderCache or a
    /// WriterCache, whose constructor takes the accessor's symbol) of this
    /// READER-IC / WRITER-IC site. The cell is a static field of its own per
    /// site, so the (class, slot index) a call fills stays for the next call, as
    /// the constant-pool cell does in memory. It is made by the first execution
    /// of the site, not by the type initializer: that runs before the file's
    /// top-level forms, and the accessor's package may be one they define (see
    /// GetOrCreateSymPkgField). Two threads that both find the field empty each
    /// make a cell and one of them is kept; either is a valid empty cache.
    /// Soundness does not depend on the cell's lifetime: every hit is checked
    /// against the generic function's MethodEpoch.
    ///
    /// With no type to hang a field on (split/inner assemblers) a fresh cell
    /// is made on every call, which always misses and resolves the accessor.</summary>
    private void EmitFaslInlineCacheCell(string name, string pkg, Type cellType)
    {
        _faslStructMap?.RecordPackageName(pkg);
        var ctor = cellType.GetConstructor(new[] { typeof(Symbol) })!;
        if (_faslStructMap?.UninternedTypeBuilder != null
            && _faslStructMap.SymFnSiteInitIl != null)
        {
            var field = _faslStructMap.CreateInlineCacheField(cellType);
            var haveCell = _il.DefineLabel();
            _il.Emit(OpCodes.Ldsfld, field);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Brtrue, haveCell);
            _il.Emit(OpCodes.Pop);
            _il.Emit(OpCodes.Ldstr, Track(name));
            _il.Emit(OpCodes.Ldstr, Track(pkg));
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            _il.Emit(OpCodes.Newobj, ctor);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Stsfld, field);
            _il.MarkLabel(haveCell);
        }
        else
        {
            _il.Emit(OpCodes.Ldstr, Track(name));
            _il.Emit(OpCodes.Ldstr, Track(pkg));
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            _il.Emit(OpCodes.Newobj, ctor);
        }
    }

    private LocalBuilder? _icFrame;
    private static readonly MethodInfo s_gfCallIC1 = typeof(Runtime).GetMethod("GFCallIC1")!;

    /// <summary>(:GF-IC1 NAME PKG [:MODE]): the function, the argument and, with
    /// :MODE, the value mode are on the stack, as for INVOKE1[M]. Calls through the
    /// site's GFCallCache, else as INVOKE1[M] would.</summary>
    private void EmitGFCallIC1(Cons c)
    {
        bool mode = Cdr(Cdr(Cdr(c))) is Cons;
        // A cell made afresh on every call would only ever miss.
        if (_runOnceInMemory
            || (_faslMode && (_faslStructMap?.UninternedTypeBuilder == null || _faslStructMap.SymFnSiteInitIl == null)))
        {
            EmitCallvirt(mode ? "LispFunction.Invoke1M" : "LispFunction.Invoke1");
            return;
        }
        var name = GetString(Cadr(c));
        var pkg = GetString(Caddr(c));
        var il = _il;
        var modeL = mode ? il.DeclareLocal(typeof(int)) : null;
        if (mode) il.EmitLoc(OpCodes.Stloc, modeL!);
        var argL = il.DeclareLocal(typeof(LispObject));
        il.EmitLoc(OpCodes.Stloc, argL);
        var fnL = il.DeclareLocal(typeof(LispFunction));
        il.EmitLoc(OpCodes.Stloc, fnL);
        _icFrame ??= il.DeclareLocal(typeof(LispFunction.Frame));
        if (_faslMode)
            EmitFaslInlineCacheCell(name, pkg, typeof(GFCallCache));
        else
        {
            EmitConstantAs(AddConstant(new GFCallCache(Startup.SymInPkg(name, pkg))),
                           typeof(GFCallCache));
        }
        il.EmitLoc(OpCodes.Ldloc, fnL);
        il.EmitLoc(OpCodes.Ldloc, argL);
        if (mode) il.EmitLoc(OpCodes.Ldloc, modeL!); else il.Emit(OpCodes.Ldc_I4_M1);
        il.EmitLoc(OpCodes.Ldloca, _icFrame);
        il.Emit(OpCodes.Call, s_gfCallIC1);
        var done = il.DefineLabel();
        il.Emit(OpCodes.Dup);
        il.Emit(OpCodes.Brtrue, done);
        il.Emit(OpCodes.Pop);
        il.EmitLoc(OpCodes.Ldloc, fnL);
        il.EmitLoc(OpCodes.Ldloc, argL);
        if (mode) il.EmitLoc(OpCodes.Ldloc, modeL!);
        il.Emit(OpCodes.Call, typeof(LispFunction).GetMethod(mode ? "Call1M" : "Call1")!);
        il.MarkLabel(done);
        _lastPushedType = typeof(LispObject);
    }

    private void EmitInternedSymbol(string name, string pkg)
    {
        if (_faslMode && _cachedLiteralDepth == 0 && !_runOnce && _faslStructMap?.UninternedTypeBuilder != null
            && _faslStructMap.SymFnSiteInitIl != null)
        {
            var symPkgField = _faslStructMap.GetOrCreateSymPkgField(name, pkg);
            var haveSym = _il.DefineLabel();
            _il.Emit(OpCodes.Ldsfld, symPkgField);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Brtrue, haveSym);
            _il.Emit(OpCodes.Pop);
            _il.Emit(OpCodes.Ldstr, Track(name));
            _il.Emit(OpCodes.Ldstr, Track(pkg));
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Stsfld, symPkgField);
            _il.MarkLabel(haveSym);
        }
        else if (!_faslMode)
        {
            EmitConstantAs(AddSymbolConstant(Startup.SymInPkg(name, pkg)), typeof(Symbol));
        }
        else
        {
            _il.Emit(OpCodes.Ldstr, Track(name));
            _il.Emit(OpCodes.Ldstr, Track(pkg));
            _il.Emit(OpCodes.Call, _methodCache["Startup.SymInPkg"]);
        }
    }

    /// <summary>Walk a constant graph and record its interned symbols for
    /// load-time pre-interning. Cycle-safe; stops at anything that is not a
    /// cons, vector or symbol, since those cannot hold further symbols that
    /// the reader path would reach.</summary>
    private void RecordSymbolsForPreintern(LispObject val)
    {
        if (_faslStructMap == null) return;
        var stack = new Stack<LispObject>();
        stack.Push(val);
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        while (stack.Count > 0)
        {
            switch (stack.Pop())
            {
                case Symbol sym when sym.HomePackage != null:
                    _faslStructMap.RecordSymbolReference(sym.Name, sym.HomePackage.Name);
                    break;
                case Cons c when seen.Add(c):
                    stack.Push(c.Car); stack.Push(c.Cdr);
                    break;
                case LispVector vec when seen.Add(vec):
                    for (int i = 0; i < vec.Length; i++) stack.Push(vec.ElementAt(i));
                    break;
            }
        }
    }

    /// <summary>Emit the printed representation as UTF-8 in the PE data section and
    /// a load-time read of it, leaving the reconstructed object on the stack.
    /// Returns false when there is no type to hang data on (the in-process JIT path,
    /// which has no PE at all) so the caller falls back to Ldstr.</summary>
    /// <remarks>Ldstr would put the representation in the module-wide #US heap,
    /// which the token encoding caps at 16 MB for the whole module: shared with
    /// every other string literal and symbol name. That cap is the next wall in
    /// front of holding literals as data, and it does not apply here: the data
    /// section has no such limit, and the blob is copied out in one memcpy
    /// (InitializeArray) instead of being decoded token by token.</remarks>
    private bool EmitReprAsData(string repr, bool withUninternedTable = false)
    {
        if (!_faslMode || _faslStructMap == null || _faslTypeBuilder == null) return false;
        var utf8 = System.Text.Encoding.UTF8.GetBytes(repr);
        var initArray = typeof(System.Runtime.CompilerServices.RuntimeHelpers)
            .GetMethod("InitializeArray", new[] { typeof(Array), typeof(RuntimeFieldHandle) })!;

        void EmitBlob(byte[] blob)
        {
            var field = _faslStructMap.DefineLiteralData(blob);
            _il.Emit(OpCodes.Ldc_I4, blob.Length);
            _il.Emit(OpCodes.Newarr, typeof(byte));
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Ldtoken, field);
            _il.Emit(OpCodes.Call, initArray);
        }

        if (utf8.Length <= MaxLiteralDataChunk)
        {
            EmitBlob(utf8);
            if (withUninternedTable)
            {
                _il.Emit(OpCodes.Ldsfld, _faslStructMap.UninternedTableField());
                _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("ReadConstantFromUtf8",
                    new[] { typeof(byte[]), typeof(Symbol[]) })!);
            }
            else
                _il.Emit(OpCodes.Call,
                    typeof(Runtime).GetMethod("ReadConstantFromUtf8", new[] { typeof(byte[]) })!);
            return true;
        }
        // Larger than one data field may hold: emit the pieces into a byte[][] and
        // let the runtime join them before reading.
        int parts = (utf8.Length + MaxLiteralDataChunk - 1) / MaxLiteralDataChunk;
        _il.Emit(OpCodes.Ldc_I4, parts);
        _il.Emit(OpCodes.Newarr, typeof(byte[]));
        for (int i = 0; i < parts; i++)
        {
            int start = i * MaxLiteralDataChunk;
            int len = Math.Min(MaxLiteralDataChunk, utf8.Length - start);
            var chunk = new byte[len];
            Array.Copy(utf8, start, chunk, 0, len);
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Ldc_I4, i);
            EmitBlob(chunk);
            _il.Emit(OpCodes.Stelem_Ref);
        }
        if (withUninternedTable)
        {
            _il.Emit(OpCodes.Ldsfld, _faslStructMap.UninternedTableField());
            _il.Emit(OpCodes.Call, typeof(Runtime).GetMethod("ReadConstantFromUtf8Parts",
                new[] { typeof(byte[][]), typeof(Symbol[]) })!);
        }
        else
            _il.Emit(OpCodes.Call,
                typeof(Runtime).GetMethod("ReadConstantFromUtf8Parts", new[] { typeof(byte[][]) })!);
        return true;
    }

    private void EmitFaslInstanceInline(LispInstance li)
    {
        if (_faslStructMap == null) { EmitFaslInstanceFallback(li); return; }
        // make-load-form protocol (CLHS 3.2.4.2)
        if (_faslStructMap.TryGetInstanceKey(li, out var existingKey))
        {
            // Already registered: emit lookup (creation form already emitted or in progress)
            EmitInstanceLookup(existingKey);
            return;
        }
        var mlfSym = Startup.Sym("MAKE-LOAD-FORM");
        if (mlfSym?.Function != null)
        {
            // Emit the creation form as its OWN top-level method (ahead of the
            // referencing form in _initIl) so creation and init forms interleave
            // per object at load time, (create a)(init a)(create b)(init b),
            // matching CLHS make-load-form ordering and SBCL. Dependencies are
            // emitted first; the inline reference here becomes an intern lookup.
            var key = EnsureInstanceCreatedTopLevel(li, mlfSym.Function);
            if (key != null) { EmitInstanceLookup(key); return; }
            // key == null: make-load-form errored, or a creation-form cycle: fall
            // through to the direct-slot fallback below (old inline behavior).
        }
        // No make-load-form defined: fallback to direct slot serialization
        _faslStructMap.RegisterInstance(li, null);
        _faslStructMap.MarkCreated(li);
        EmitFaslInstanceFallback(li);
    }

    /// <summary>Emit an intern-by-key lookup of an already-created make-load-form
    /// instance (InternViaEval with a NIL form designator = "look up existing").</summary>
    private void EmitInstanceLookup(string key)
    {
        _il.Emit(OpCodes.Ldstr, Track(key));
        _il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
        _il.Emit(OpCodes.Call, _internViaEvalInstance);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    /// <summary>
    /// Register an instance and emit its make-load-form creation form as a standalone
    /// top-level method appended to _initIl, after first ensuring the creation form's
    /// referenced instances are themselves created (so their forms precede this one).
    /// After the creation, eagerly emits any init forms whose dependencies are now
    /// satisfied: giving the per-object (create,init) interleave CLHS/SBCL require.
    /// Returns the intern key, or null if make-load-form is unusable here (errored, or
    /// an in-progress creation cycle) so the caller can fall back to inline emission.
    /// </summary>
    private string? EnsureInstanceCreatedTopLevel(LispObject li, LispObject mlfFn)
    {
        if (_faslStructMap!.TryGetInstanceKey(li, out var existing))
            // Registered but not yet created => we are inside its own dep recursion
            // (a creation-form cycle, which is degenerate). Signal fallback.
            return _faslStructMap.IsCreated(li) ? existing : null;

        LispObject creationForm, initForm;
        try
        {
            var raw = CallMakeLoadForm(mlfFn, li);
            if (raw is MvReturn mv && mv.Count >= 1)
            {
                creationForm = mv[0];
                initForm = mv.Count >= 2 ? mv[1] : Nil.Instance;
            }
            else { creationForm = raw; initForm = Nil.Instance; }
        }
        catch { return null; }
        return EnsureCreatedTopLevelWith(li, creationForm, initForm);
    }

    /// <summary>EnsureInstanceCreatedTopLevel once the object's MAKE-LOAD-FORM
    /// has been called: register it, emit its creation (dependencies first) into
    /// ModuleInit, and the init forms that become runnable.</summary>
    private string? EnsureCreatedTopLevelWith(LispObject li, LispObject creationForm, LispObject initForm)
    {
        var key = _faslStructMap!.RegisterInstance(li, initForm is Nil ? null : initForm);

        // Creation forms for instances referenced by THIS creation form must run first.
        var deps = new List<LispInstance>();
        FaslStructInternMap.CollectInstanceRefs(creationForm, deps,
            new HashSet<LispInstance>(ReferenceEqualityComparer.Instance));
        foreach (var dep in deps)
        {
            if (ReferenceEquals(dep, li)) continue;
            if (_faslStructMap.IsCreated(dep)) continue;
            if (_faslStructMap.TryGetInstanceKey(dep, out _)) continue; // in progress
            var mlf = Startup.Sym("MAKE-LOAD-FORM");
            if (mlf?.Function != null) EnsureInstanceCreatedTopLevel(dep, mlf.Function);
        }

        // Emit li's creation form (deps now resolve to lookups), then mark created
        // and flush any init forms unblocked by it.
        EmitMlfTopLevelMethod(creationForm, intern: true, key: key);
        _faslStructMap.MarkCreated(li);
        foreach (var (_, init) in _faslStructMap.PopEagerInitForms())
            EmitMlfTopLevelMethod(init, intern: false, key: null);
        return key;
    }

    /// <summary>Define a static _mlf_N method that, at load time, either interns the
    /// creation form's value under KEY (intern=true) or evaluates an init form for
    /// effect (intern=false), and append a call to it in the module-init sequence.</summary>
    private void EmitMlfTopLevelMethod(LispObject form, bool intern, string? key)
    {
        var tb = _faslStructMap!.UninternedTypeBuilder!;
        var initIl = _faslStructMap.UninternedInitIl!;
        if (intern && MatchSlotSavingCreationForm(form) is var (bClass, bSets) && bClass != null
            && TryEmitMlfIntoBatch(key!, bClass, bSets!))
            return;
        if (intern && MatchSlotSavingCreationForm(form).Item1 == null
            && TryEmitMlfEvalIntoBatch(key!, form))
            return;
        int id = _faslStructMap.TopLevelMethodCount++;
        var m = tb.DefineMethod("_mlf_" + id,
            MethodAttributes.Public | MethodAttributes.Static,
            typeof(LispObject), Type.EmptyTypes);
        var il = m.GetILGenerator().ForLibrary();
        var inner = new CilAssembler
        {
            _il = il, _faslMode = true,
            _faslTypeBuilder = tb, _faslStructMap = _faslStructMap,
        };
        if (intern && MatchSlotSavingCreationForm(form) is var (className, sets) && className != null)
        {
            // The creation form MAKE-LOAD-FORM-SAVING-SLOTS returns:
            // (LET ((v (ALLOCATE-INSTANCE (FIND-CLASS 'C)))) (SETF (SLOT-VALUE v 'S) 'X) ... v).
            // Run it directly instead of handing the form to EVAL at load, which
            // compiled and JIT-compiled it once per object. As with InternViaEval,
            // nothing runs when the key is already interned.
            inner._runOnce = true;
            var skip = il.DefineLabel();
            il.Emit(OpCodes.Ldstr, Track(key!));
            il.Emit(OpCodes.Call, _isInterned);
            il.Emit(OpCodes.Brtrue, skip);
            il.Emit(OpCodes.Ldstr, Track(key!));
            inner.EmitLoadConstInline(className);
            il.Emit(OpCodes.Call, _internAllocateInstance);
            foreach (var (slot, value) in sets!)
            {
                il.Emit(OpCodes.Dup);
                inner.EmitLoadConstInline(slot);
                inner.EmitLoadConstInline(value);
                il.Emit(OpCodes.Call, _setSlotValue);
                il.Emit(OpCodes.Pop);
            }
            il.Emit(OpCodes.Pop);
            il.MarkLabel(skip);
            il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
        }
        else if (intern)
        {
            il.Emit(OpCodes.Ldstr, Track(key!));
            inner.EmitLoadConstInline(form);
            il.Emit(OpCodes.Call, _internViaEvalInstance);
        }
        else
        {
            inner.EmitLoadConstInline(form);
            il.Emit(OpCodes.Call, _methodCache["Runtime.Eval"]);
        }
        il.Emit(OpCodes.Pop);
        il.Emit(OpCodes.Ldsfld, typeof(Nil).GetField("Instance")!);
        il.Emit(OpCodes.Ret);
        initIl.Emit(OpCodes.Call, m);
        initIl.Emit(OpCodes.Pop);
    }

    internal sealed class MlfBatch
    {
        public required FaslDataWriter Writer;
        public required int PartIndex;
        /// <summary>True for a batch of creation forms to evaluate
        /// (FaslData.RunMlfEvalBatch), false for slot-saving objects.</summary>
        public required bool Eval;
        public int Count;
        public int InitOffset;
    }

    // Objects per batch. A batch is one payload read in one call, so this only
    // bounds how much is decoded per call.
    private const int MaxMlfBatchObjects = 256;

    private FaslDataWriter NewFaslDataWriter()
    {
        var map = _faslStructMap!;
        var w = new FaslDataWriter
        {
            UninternedIndex = map.GetOrCreateUninternedSymbolIndex,
            InstanceKey = li => map.TryGetInstanceKey(li, out var k) ? k : null,
            StructKey = ls => !_skipStructIntern && StructBuiltFromSlots(ls) ? map.GetOrCreate(ls) : null,
            OnSymbol = map.RecordSymbolReference,
            SharedSlot = map.CrossLiteralSlotFor,
        };
        // As the code route: a table past the inline size is a copy of a
        // prototype the fasl keeps, unless it holds an object that has to
        // stay one object with something outside it.
        w.HtPrototypePart = ht => ht.Count > MaxHtInline && !ReachesSharedNode(ht, w.LocalShared)
            ? HtPrototypeDataPart(ht) : -2;
        return w;
    }

    private bool ReachesSharedNode(LispHashTable ht, HashSet<LispObject>? local)
    {
        var cross = _faslStructMap?.CrossLiteralShared;
        if (cross == null && local == null) return false;
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        var st = new Stack<LispObject>();
        st.Push(ht);
        while (st.Count > 0)
        {
            var node = st.Pop();
            if (!HasLiteralIdentity(node) || !seen.Add(node)) continue;
            if (!ReferenceEquals(node, ht)
                && ((cross?.ContainsKey(node) ?? false) || (local?.Contains(node) ?? false)))
                return true;
            switch (node)
            {
                case Cons c: st.Push(c.Cdr); st.Push(c.Car); break;
                case LispVector v when v.ElementTypeName == "T":
                    for (int i = 0; i < v.Length; i++) st.Push(v.ElementAt(i));
                    break;
                case LispHashTable h:
                    foreach (var kv in h.Entries) { st.Push(kv.Value); st.Push(kv.Key); }
                    break;
            }
        }
        return false;
    }

    private static readonly int DataRouteMinNodes =
        int.TryParse(Environment.GetEnvironmentVariable("DOTCL_LITERAL_DATA_MIN_NODES"),
                     out var v) && v > 0 ? v : 6;

    /// <summary>Emit an aggregate literal as a data part and a load of it
    /// (FaslData.ReadValue). False, with nothing emitted, when the literal is
    /// small (its IL is cheaper than the call) or holds anything that cannot
    /// go as data.</summary>
    private bool TryEmitConstantAsData(LispObject val)
    {
        var map = _faslStructMap;
        if (map?.UninternedTypeBuilder == null || val is not (Cons or LispVector or LispStruct)) return false;
        // A dry run first: no instance is created for a literal that then
        // goes the IL route anyway (that route creates them in its own order).
        var local = CollectSharedLiteralNodes(val);
        var probe = NewFaslDataWriter();
        probe.InstanceKey = o => LispStruct.TryAs(o, out var ls) && StructBuiltFromSlots(ls) ? null : "";
        probe.StructKey = _ => "";
        probe.HtPrototypePart = _ => -2;
        probe.SharedSlot = n => map.CrossLiteralShared != null && map.CrossLiteralShared.ContainsKey(n)
            && !map.CrossLiteralBuilding.Contains(n) ? 0 : -1;
        if (local.Count > 0) probe.LocalShared = local;
        if (!probe.TryValue(val) || probe.Nodes < DataRouteMinNodes) return false;
        if (!EnsureInstancesForData(val)) return false;
        var w = NewFaslDataWriter();
        if (local.Count > 0) w.LocalShared = local;
        if (!w.TryValue(val)) return false;
        int part = map.AddDataPart(w.ToArray());
        _il.Emit(OpCodes.Ldsfld, map.DataPartsField());
        _il.Emit(OpCodes.Ldc_I4, part);
        _il.Emit(OpCodes.Ldsfld, map.HtCacheField());
        _il.Emit(OpCodes.Ldsfld, map.UninternedTableField());
        _il.Emit(OpCodes.Ldsfld, map.SharedSlotsField());
        _il.Emit(OpCodes.Call, typeof(FaslData).GetMethod("ReadValue")!);
        return true;
    }

    /// <summary>Have every instance under ROOT that this fasl has not yet
    /// registered created by its MAKE-LOAD-FORM, in the order the IL route
    /// would reach them (and with the same effect: the creation goes into
    /// ModuleInit before the form being written). False when one cannot be,
    /// which leaves the value to the IL route.</summary>
    private bool EnsureInstancesForData(LispObject root)
    {
        var map = _faslStructMap!;
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        bool Walk(LispObject o, int depth)
        {
            if (depth > 1000) return false;
            switch (o)
            {
                case Cons c:
                    for (LispObject cur = c; ; )
                    {
                        if (cur is Cons cc)
                        {
                            if (!seen.Add(cc)) return true;
                            if (!Walk(cc.Car, depth + 1)) return false;
                            cur = cc.Cdr;
                        }
                        else return Walk(cur, depth + 1);
                    }
                case LispVector v when v.ElementTypeName == "T":
                    if (!seen.Add(v)) return true;
                    for (int i = 0; i < v.Length; i++)
                        if (!Walk(v.ElementAt(i), depth + 1)) return false;
                    return true;
                case LispHashTable ht:
                    if (!seen.Add(ht)) return true;
                    foreach (var kv in ht.Entries)
                        if (!Walk(kv.Key, depth + 1) || !Walk(kv.Value, depth + 1)) return false;
                    return true;
                case LispInstance li:
                    if (map.TryGetInstanceKey(li, out _)) return true;
                    var mlf = Startup.Sym("MAKE-LOAD-FORM");
                    return mlf?.Function != null && EnsureInstanceCreatedTopLevel(li, mlf.Function) != null;
                case LispStruct ls:
                    if (map.TryGetInstanceKey(ls, out _)) return true;
                    if (StructBuiltFromSlots(ls))
                    {
                        if (!seen.Add(ls)) return true;
                        foreach (var sv in ls.SlotsSnapshot())
                            if (sv != null && !Walk(sv, depth + 1)) return false;
                        return true;
                    }
                    var mlfs = Startup.Sym("MAKE-LOAD-FORM");
                    return mlfs?.Function != null && EnsureInstanceCreatedTopLevel(ls, mlfs.Function) != null;
                default:
                    return true;
            }
        }
        return Walk(root, 0);
    }

    /// <summary>Write one MAKE-LOAD-FORM-SAVING-SLOTS creation (key, class name,
    /// then the slot count and the (slot, value) pairs, behind a length so the
    /// loader can skip an object already interned). False, with nothing
    /// written, when a value cannot go as data.</summary>
    private static bool WriteMlfObject(FaslDataWriter w, string key, Symbol className,
        List<(LispObject, LispObject)> sets)
    {
        int mark = w.Mark;
        w.Str(key);
        int len = w.ReserveLength();
        bool ok = w.TryValue(className);
        if (ok)
        {
            w.Varint((ulong)sets.Count);
            foreach (var (slot, value) in sets)
                if (!w.TryValue(slot) || !w.TryValue(value)) { ok = false; break; }
        }
        if (!ok) { w.Reset(mark); return false; }
        w.PatchLength(len);
        return true;
    }

    /// <summary>Append one MAKE-LOAD-FORM-SAVING-SLOTS creation to the open
    /// batch, opening one if needed. The batch is data (see FaslData): the
    /// objects are made at load by FaslData.RunMlfBatch, called from ModuleInit
    /// where the batch was opened. Same effect as the one-method form: nothing
    /// when KEY is already interned, else allocate, register and set the slots.
    /// Returns false when the object cannot go as data.</summary>
    private bool TryEmitMlfIntoBatch(string key, Symbol className, List<(LispObject, LispObject)> sets)
    {
        var map = _faslStructMap!;
        foreach (var (_, value) in sets)
            if (!EnsureInstancesForData(value)) return false;
        // Try it on its own first, so that an object that cannot go as data
        // does not open a batch.
        if (!WriteMlfObject(NewFaslDataWriter(), key, className, sets)) return false;
        var b = OpenMlfBatchFor(eval: false);
        if (!WriteMlfObject(b.Writer, key, className, sets))
            throw new InvalidOperationException("fasl: MLF object written once but not twice");
        b.Count++;
        return true;
    }

    /// <summary>Append one creation form that is evaluated at load (any
    /// MAKE-LOAD-FORM other than the slot-saving shape) to the open batch of
    /// such forms. Same effect as the one-method form: nothing when KEY is
    /// already interned, else LispInstance.InternViaEval. False when the form
    /// cannot go as data.</summary>
    private bool TryEmitMlfEvalIntoBatch(string key, LispObject form)
    {
        if (!EnsureInstancesForData(form)) return false;
        static bool Write(FaslDataWriter w, string key, LispObject form)
        {
            int mark = w.Mark;
            w.Str(key);
            if (w.TryValue(form)) return true;
            w.Reset(mark);
            return false;
        }
        if (!Write(NewFaslDataWriter(), key, form)) return false;
        var b = OpenMlfBatchFor(eval: true);
        if (!Write(b.Writer, key, form))
            throw new InvalidOperationException("fasl: MLF form written once but not twice");
        b.Count++;
        return true;
    }

    /// <summary>The open MLF batch of the given kind that the next object can
    /// join (nothing written to ModuleInit since it was opened), else a new
    /// one whose call is placed in ModuleInit now. Counts the object.</summary>
    private MlfBatch OpenMlfBatchFor(bool eval)
    {
        var map = _faslStructMap!;
        var initIl = map.UninternedInitIl!;
        var b = map.OpenMlfBatch;
        if (b != null && (b.Eval != eval || b.Count >= MaxMlfBatchObjects || initIl.ILOffset != b.InitOffset))
        {
            CloseMlfBatch(map);
            b = null;
        }
        if (b == null)
        {
            int part = map.ReserveDataPart();
            initIl.Emit(OpCodes.Ldsfld, map.DataPartsField());
            initIl.Emit(OpCodes.Ldc_I4, part);
            initIl.Emit(OpCodes.Ldsfld, map.HtCacheField());
            initIl.Emit(OpCodes.Ldsfld, map.UninternedTableField());
            initIl.Emit(OpCodes.Ldsfld, map.SharedSlotsField());
            initIl.Emit(OpCodes.Call, typeof(FaslData).GetMethod(eval ? "RunMlfEvalBatch" : "RunMlfBatch")!);
            initIl.Emit(OpCodes.Pop);
            b = new MlfBatch { Writer = NewFaslDataWriter(), PartIndex = part, InitOffset = initIl.ILOffset, Eval = eval };
            map.OpenMlfBatch = b;
        }
        // Counted per object, like the one-method form: the merged top level
        // helpers check this counter to see that no creation was emitted.
        map.TopLevelMethodCount++;
        return b;
    }

    /// <summary>Finish the open MLF batch, if any, into its data part. Called
    /// before a new batch is opened and before the fasl's types are created.</summary>
    internal static void CloseMlfBatch(FaslStructInternMap map)
    {
        var b = map.OpenMlfBatch;
        if (b == null) return;
        map.OpenMlfBatch = null;
        map.SetDataPart(b.PartIndex, b.Writer.ToArray());
    }

    // A structure is interned as well as an instance, so the check asks for
    // any object under the key (TryGetInterned answers instances only and is
    // kept for fasls that already call it).
    private static readonly MethodInfo _isInterned =
        typeof(LispInstance).GetMethod("IsInterned", new[] { typeof(string) })!;
    private static readonly MethodInfo _internAllocateInstance =
        typeof(LispInstance).GetMethod("InternAllocateInstance", new[] { typeof(string), typeof(LispObject) })!;
    private static readonly MethodInfo _setSlotValue =
        typeof(Runtime).GetMethod("SetSlotValue", new[] { typeof(LispObject), typeof(LispObject), typeof(LispObject) })!;

    private static bool IsClSymbol(LispObject o, string name) =>
        o is Symbol s && s.Name == name && s.HomePackage?.Name == "COMMON-LISP";

    /// <summary>(QUOTE x) -> x, else null.</summary>
    private static LispObject? QuotedObject(LispObject o) =>
        o is Cons q && IsClSymbol(q.Car, "QUOTE") && q.Cdr is Cons qa && qa.Cdr is Nil ? qa.Car : null;

    /// <summary>The class name and the (slot name, value) pairs of
    /// (LET ((v (ALLOCATE-INSTANCE (FIND-CLASS 'C)))) (SETF (SLOT-VALUE v 'S) 'X) ... v),
    /// else (null, null).</summary>
    private static (Symbol?, List<(LispObject, LispObject)>?) MatchSlotSavingCreationForm(LispObject form)
    {
        if (form is not Cons l || !IsClSymbol(l.Car, "LET") || l.Cdr is not Cons lb
            || lb.Car is not Cons bindings || bindings.Cdr is not Nil
            || bindings.Car is not Cons b || b.Car is not Symbol v
            || b.Cdr is not Cons bi || bi.Cdr is not Nil
            || bi.Car is not Cons ai || !IsClSymbol(ai.Car, "ALLOCATE-INSTANCE")
            || ai.Cdr is not Cons aa || aa.Cdr is not Nil
            || aa.Car is not Cons fc || !IsClSymbol(fc.Car, "FIND-CLASS")
            || fc.Cdr is not Cons fa || fa.Cdr is not Nil
            || QuotedObject(fa.Car) is not Symbol className)
            return (null, null);
        var sets = new List<(LispObject, LispObject)>();
        for (var cur = lb.Cdr; cur is Cons cc; cur = cc.Cdr)
        {
            if (cc.Cdr is Nil)
                return ReferenceEquals(cc.Car, v) ? (className, sets) : (null, null);
            if (cc.Car is Cons sf && IsClSymbol(sf.Car, "SETF")
                && sf.Cdr is Cons s1 && s1.Car is Cons place && s1.Cdr is Cons s2 && s2.Cdr is Nil
                && IsClSymbol(place.Car, "SLOT-VALUE")
                && place.Cdr is Cons p1 && ReferenceEquals(p1.Car, v)
                && p1.Cdr is Cons p2 && p2.Cdr is Nil
                && QuotedObject(p2.Car) is Symbol slot
                && QuotedObject(s2.Car) is { } value)
                sets.Add((slot, value));
            else
                return (null, null);
        }
        return (null, null);
    }

    private void EmitFaslInstanceFallback(LispInstance li)
    {
        li.EnsureCurrent();
        string pkgName = li.Class.Name.HomePackage?.Name ?? "COMMON-LISP";
        string symName = li.Class.Name.Name;
        _il.Emit(OpCodes.Ldstr, pkgName);
        _il.Emit(OpCodes.Ldstr, symName);
        int slotCount = li.Slots.Length;
        _il.Emit(OpCodes.Ldc_I4, slotCount);
        _il.Emit(OpCodes.Newarr, typeof(LispObject));
        for (int i = 0; i < slotCount; i++)
        {
            _il.Emit(OpCodes.Dup);
            _il.Emit(OpCodes.Ldc_I4, i);
            var slotVal = li.Slots[i] ?? Nil.Instance;
            EmitLoadConstInline(slotVal);
            _il.Emit(OpCodes.Stelem_Ref);
        }
        _il.Emit(OpCodes.Call, _makeFaslInstance);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    // Elements per helper method when a list literal is emitted in chunks. ~500
    // conses is ~15KB of IL, comfortably inside the JIT's cheap range.
    private const int MaxInlineListChunk = 500;

    /// <summary>
    /// FASL mode: build a long list literal through a chain of static helper methods,
    /// each prepending one chunk of elements onto the list built so far.
    /// Emitting the whole literal as one method is what makes a fasl expensive to
    /// LOAD: for the same 100k-cons data, load-time peak RSS was +13MB when the
    /// conses were spread over 200 methods but +680MB when they sat in 4, because
    /// the JIT's working set grows faster than the method body it compiles. The
    /// compiling process never sees this: it builds the literal directly.
    /// </summary>
    private void EmitFaslChunkedList(List<LispObject> cars, LispObject tail)
    {
        int id = Interlocked.Increment(ref _faslClosureCount);
        var consCtor = typeof(Cons).GetConstructor(new[] { typeof(LispObject), typeof(LispObject) })!;
        // Tail first (nil for a proper list, or the dotted cdr); each chunk method
        // then takes the list built so far and prepends its own slice, so the
        // chunks are called back-to-front.
        EmitLoadConstInline(tail);
        int chunkId = 0;
        for (int end = cars.Count; end > 0; end -= MaxInlineListChunk, chunkId++)
        {
            int start = Math.Max(0, end - MaxInlineListChunk);
            var m = _faslTypeBuilder!.DefineMethod(
                $"_lst_{id}_{chunkId}",
                MethodAttributes.Private | MethodAttributes.Static,
                typeof(LispObject), new[] { typeof(LispObject) });
            var il = m.GetILGenerator().ForLibrary();
            var inner = new CilAssembler
            {
                _il = il, _faslMode = true,
                _faslTypeBuilder = _faslTypeBuilder, _faslStructMap = _faslStructMap,
                _skipStructIntern = _skipStructIntern, _sharedMemo = _sharedMemo,
            };
            var acc = il.DeclareLocal(typeof(LispObject));
            il.Emit(OpCodes.Ldarg_0);
            il.EmitLoc(OpCodes.Stloc, acc);
            for (int i = end - 1; i >= start; i--)
            {
                inner.EmitLoadConstInline(cars[i]);   // car
                il.EmitLoc(OpCodes.Ldloc, acc);          // cdr
                il.Emit(OpCodes.Newobj, consCtor);
                il.EmitLoc(OpCodes.Stloc, acc);
            }
            il.EmitLoc(OpCodes.Ldloc, acc);
            il.Emit(OpCodes.Ret);
            _il.Emit(OpCodes.Call, m);
        }
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    /// <summary>
    /// FASL mode: fill a long vector literal's backing array through a chain of
    /// static helper methods, each populating one slice. The array is on the IL
    /// stack (from Newarr); this leaves it there afterwards for the ctor call.
    /// A single huge fill body blows up the JIT's load-time working set.
    /// </summary>
    private void EmitFaslChunkedVectorFill(LispVector vec, int vecLen)
    {
        int id = Interlocked.Increment(ref _faslClosureCount);
        var arrLocal = _il.DeclareLocal(typeof(LispObject[]));
        _il.EmitLoc(OpCodes.Stloc, arrLocal);
        int chunkId = 0;
        for (int start = 0; start < vecLen; start += MaxInlineListChunk, chunkId++)
        {
            int end = Math.Min(start + MaxInlineListChunk, vecLen);
            var m = _faslTypeBuilder!.DefineMethod(
                $"_vec_{id}_{chunkId}",
                MethodAttributes.Private | MethodAttributes.Static,
                typeof(void), new[] { typeof(LispObject[]) });
            var il = m.GetILGenerator().ForLibrary();
            var inner = new CilAssembler
            {
                _il = il, _faslMode = true,
                _faslTypeBuilder = _faslTypeBuilder, _faslStructMap = _faslStructMap,
                _skipStructIntern = _skipStructIntern, _sharedMemo = _sharedMemo,
            };
            for (int i = start; i < end; i++)
            {
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldc_I4, i);
                inner.EmitLoadConstInline(vec.ElementAt(i));
                il.Emit(OpCodes.Stelem_Ref);
            }
            il.Emit(OpCodes.Ret);
            _il.EmitLoc(OpCodes.Ldloc, arrLocal);
            _il.Emit(OpCodes.Call, m);
        }
        _il.EmitLoc(OpCodes.Ldloc, arrLocal);
    }

    /// <summary>
    /// FASL mode: continue a constant that exceeded the inline depth cap in a fresh
    /// static helper method, and call it. The call resets both the emitter's nesting
    /// depth and the IL evaluation stack (the helper returns a single value), which is
    /// what the cap exists to bound: while keeping the FASL self-contained.
    /// </summary>
    /// <summary>Aggregates whose materialization can be large enough to deserve
    /// its own method. Atoms emit a handful of bytes, less than the call site.</summary>
    private static bool IsSpillWorthy(LispObject val) =>
        val is Cons or LispVector or LispStruct or LispHashTable or LispInstance;

    private void EmitFaslDeepConstant(LispObject val)
    {
        int id = Interlocked.Increment(ref _faslClosureCount);
        var m = _faslTypeBuilder!.DefineMethod(
            $"_const_{id}",
            MethodAttributes.Private | MethodAttributes.Static,
            typeof(LispObject), Type.EmptyTypes);
        var il = m.GetILGenerator().ForLibrary();
        var inner = new CilAssembler
        {
            _il = il, _faslMode = true,
            _faslTypeBuilder = _faslTypeBuilder, _faslStructMap = _faslStructMap,
            _skipStructIntern = _skipStructIntern, _sharedMemo = _sharedMemo,
            // The set of structs currently being emitted is what breaks cycles.
            // It has to cross into the helper, or a cyclic struct graph would
            // spill back and forth between methods forever.
            _inlineVisited = _inlineVisited,
            _suppressLiteralCache = true,
            _cachedLiteralDepth = _cachedLiteralDepth,
            _runOnce = _runOnce,
        };
        inner.EmitLoadConstInline(val);
        il.Emit(OpCodes.Ret);
        _il.Emit(OpCodes.Call, m);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    /// <summary>
    /// FASL mode: emit a large hash table as a set of static helper methods in the FASL assembly.
    /// Splits entries into chunks to stay within .NET IL method size limits (~64KB body).
    /// Each chunk method takes a LispHashTable, populates a slice, returns void.
    /// The top-level method creates the HT and calls all chunk methods.
    /// This makes the FASL self-contained (no _constants cross-process dependency).
    /// </summary>
    private void EmitFaslLargeHashTable(LispHashTable ht, List<KeyValuePair<LispObject, LispObject>> entries)
    {
        if (TryEmitFaslHashTableFromPrototype(ht, entries)) return;
        const int ChunkSize = 1500; // ~1500 entries x ~30 bytes/entry = ~45KB, safely under 64KB IL limit
        int htId = Interlocked.Increment(ref _faslClosureCount);
        var setMethod = typeof(LispHashTable).GetMethod("Set",
            new[] { typeof(LispObject), typeof(LispObject) })!;
        var htType = typeof(LispHashTable);
        var htCtor = htType.GetConstructor(new[] { typeof(string) })!;

        // Emit chunk methods: static void _ht_N_chunk_C(LispHashTable ht)
        var chunkMethods = new List<MethodBuilder>();
        for (int chunkStart = 0; chunkStart < entries.Count; chunkStart += ChunkSize)
        {
            int chunkEnd = Math.Min(chunkStart + ChunkSize, entries.Count);
            int chunkId = chunkMethods.Count;
            var chunkMethod = _faslTypeBuilder!.DefineMethod(
                $"_ht_{htId}_chunk_{chunkId}",
                MethodAttributes.Private | MethodAttributes.Static,
                typeof(void), new[] { typeof(LispHashTable) });
            chunkMethods.Add(chunkMethod);

            var chunkIl = chunkMethod.GetILGenerator().ForLibrary();
            // Create a temporary inner assembler to emit keys/values into this method
            var inner = new CilAssembler();
            inner._il = chunkIl;
            inner._faslMode = true;
            inner._faslTypeBuilder = _faslTypeBuilder;
            inner._faslStructMap = _faslStructMap;
            inner._skipStructIntern = true;
            inner._sharedMemo = _sharedMemo;

            for (int i = chunkStart; i < chunkEnd; i++)
            {
                chunkIl.Emit(OpCodes.Ldarg_0);          // push ht
                inner.EmitLoadConstInline(entries[i].Key);
                inner.EmitLoadConstInline(entries[i].Value);
                chunkIl.Emit(OpCodes.Callvirt, setMethod);
            }
            chunkIl.Emit(OpCodes.Ret);
        }

        // Emit top-level builder: static LispHashTable _ht_N()
        var builderMethod = _faslTypeBuilder!.DefineMethod(
            $"_ht_{htId}",
            MethodAttributes.Private | MethodAttributes.Static,
            typeof(LispHashTable), Type.EmptyTypes);
        var builderIl = builderMethod.GetILGenerator().ForLibrary();
        builderIl.Emit(OpCodes.Ldstr, ht.TestName);
        builderIl.Emit(OpCodes.Newobj, htCtor);
        var htLocal = builderIl.DeclareLocal(typeof(LispHashTable));
        builderIl.EmitLoc(OpCodes.Stloc, htLocal);
        foreach (var chunkMethod in chunkMethods)
        {
            builderIl.EmitLoc(OpCodes.Ldloc, htLocal);
            builderIl.Emit(OpCodes.Call, chunkMethod);
        }
        builderIl.EmitLoc(OpCodes.Ldloc, htLocal);
        builderIl.Emit(OpCodes.Ret);

        // Call the builder method from the current context
        _il.Emit(OpCodes.Call, builderMethod);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
    }

    /// <summary>
    /// FASL mode: a large hash table literal as a copy of a prototype table the
    /// fasl builds once and keeps to itself. The prototype of a table that
    /// shares most of its entries (same key and value objects) with a table
    /// emitted earlier in the same file is built as a copy of that one plus the
    /// differences. A code walker that copies its environment tables for every
    /// nested scope otherwise puts the same thousand entries into the fasl as
    /// IL once per scope, all of it JIT-compiled at load to run once.
    ///
    /// Each literal still gets its own table (a copy). Entries are shared only
    /// where the compile-time tables held the very same objects. Declined (the
    /// caller builds the table in place) when any object under the table also
    /// occurs elsewhere in the literal being emitted: the per-literal memo
    /// fields that keep such objects one object are not visible from a
    /// prototype another literal may build first.
    /// </summary>
    private bool TryEmitFaslHashTableFromPrototype(LispHashTable ht,
        List<KeyValuePair<LispObject, LispObject>> entries)
    {
        var map = _faslStructMap;
        if (map == null || map.UninternedTypeBuilder == null || _faslTypeBuilder == null)
            return false;
        if (!HashTableIsSelfContained(ht)) return false;

        // As data when every key and value can go that way: FaslData.HtPrototype
        // builds it at first use.
        int dataPart = HtPrototypeDataPart(ht);
        if (dataPart >= 0)
        {
            EmitHtPrototypeDataUse(dataPart);
            return true;
        }

        var (snapshot, baseProto, removed, added) = PlanHtPrototype(ht, entries, data: false);

        int id = Interlocked.Increment(ref _faslClosureCount);
        var field = map.DefineLiteralCacheField();
        var protoMethod = _faslTypeBuilder.DefineMethod($"_htp_{id}",
            MethodAttributes.Private | MethodAttributes.Static,
            typeof(LispHashTable), Type.EmptyTypes);
        var il = protoMethod.GetILGenerator().ForLibrary();
        var done = il.DefineLabel();
        il.Emit(OpCodes.Ldsfld, field);
        il.Emit(OpCodes.Castclass, typeof(LispHashTable));
        il.Emit(OpCodes.Dup);
        il.Emit(OpCodes.Brtrue, done);
        il.Emit(OpCodes.Pop);
        if (baseProto != null)
        {
            il.Emit(OpCodes.Call, baseProto.Method!);
            il.Emit(OpCodes.Call, _htCopyLiteral);
        }
        else
        {
            il.Emit(OpCodes.Ldstr, ht.TestName);
            il.Emit(OpCodes.Newobj, typeof(LispHashTable).GetConstructor(new[] { typeof(string) })!);
        }
        foreach (var chunk in EmitHtPrototypeChunks(id, "r", removed.Select(k => new KeyValuePair<LispObject, LispObject>(k, k)).ToList(), remove: true))
        {
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Call, chunk);
        }
        foreach (var chunk in EmitHtPrototypeChunks(id, "s", added, remove: false))
        {
            il.Emit(OpCodes.Dup);
            il.Emit(OpCodes.Call, chunk);
        }
        il.Emit(OpCodes.Dup);
        il.Emit(OpCodes.Stsfld, field);
        il.MarkLabel(done);
        il.Emit(OpCodes.Ret);

        map.HtPrototypes.Add(new FaslStructInternMap.HtPrototype(ht.TestName, snapshot, protoMethod));

        _il.Emit(OpCodes.Call, protoMethod);
        _il.Emit(OpCodes.Call, _htCopyLiteral);
        _il.Emit(OpCodes.Castclass, typeof(LispObject));
        return true;
    }

    /// <summary>What a prototype for HT is built from: the snapshot of its
    /// entries, and the best earlier prototype of the same kind (data or
    /// code) to start from, with the keys to remove from a copy of it and the
    /// entries to set. The base is an earlier prototype of the same test
    /// sharing the most (key, value) pairs, used only when it covers at least
    /// half the entries.</summary>
    private (Dictionary<LispObject, LispObject> snapshot, FaslStructInternMap.HtPrototype? baseProto,
        List<LispObject> removed, List<KeyValuePair<LispObject, LispObject>> added)
        PlanHtPrototype(LispHashTable ht, List<KeyValuePair<LispObject, LispObject>> entries, bool data)
    {
        var map = _faslStructMap!;
        var snapshot = new Dictionary<LispObject, LispObject>(entries.Count, ReferenceEqualityComparer.Instance);
        foreach (var kv in entries) snapshot[kv.Key] = kv.Value;

        FaslStructInternMap.HtPrototype? baseProto = null;
        int best = 0;
        var protos = map.HtPrototypes;
        for (int i = protos.Count - 1, seen = 0; i >= 0 && seen < 32; i--, seen++)
        {
            var p = protos[i];
            if (p.Test != ht.TestName || (p.Method == null) != data) continue;
            int match = 0;
            foreach (var kv in entries)
                if (p.Entries.TryGetValue(kv.Key, out var v) && SameLiteralObject(v, kv.Value)) match++;
            if (match > best) { best = match; baseProto = p; }
        }
        if (best * 2 < entries.Count) baseProto = null;

        var removed = new List<LispObject>();
        var added = new List<KeyValuePair<LispObject, LispObject>>();
        if (baseProto != null)
        {
            foreach (var kv in baseProto.Entries)
                if (!snapshot.TryGetValue(kv.Key, out var v) || !SameLiteralObject(v, kv.Value))
                    removed.Add(kv.Key);
            // A removed key is emitted afresh to find the prototype's entry, which
            // only works for a key the table's test matches by content.
            if (removed.Any(k => !KeyFoundByContent(k, ht.TestName)))
            {
                baseProto = null;
                removed.Clear();
            }
        }
        if (baseProto != null)
        {
            foreach (var kv in entries)
                if (!baseProto.Entries.TryGetValue(kv.Key, out var v) || !SameLiteralObject(v, kv.Value))
                    added.Add(kv);
        }
        else added = entries;
        return (snapshot, baseProto, removed, added);
    }

    /// <summary>The data part that holds the prototype of HT (see
    /// FaslData.HtPrototype), written on first request; -1 when an entry
    /// cannot go as data. Emits no code itself, other than the creation of
    /// instances the entries name (EnsureInstancesForData), so a data payload
    /// being written can refer to it.</summary>
    private int HtPrototypeDataPart(LispHashTable ht)
    {
        var map = _faslStructMap!;
        if (map.HtDataParts.TryGetValue(ht, out int known)) return known;
        // A table that reaches itself has no prototype to refer to yet.
        if (!map.HtDataBuilding.Add(ht)) return -1;
        try { return HtPrototypeDataPartCore(ht); }
        finally { map.HtDataBuilding.Remove(ht); }
    }

    private int HtPrototypeDataPartCore(LispHashTable ht)
    {
        var map = _faslStructMap!;
        var entries = ht.Entries.ToList();
        var (snapshot, baseProto, removed, added) = PlanHtPrototype(ht, entries, data: true);
        foreach (var k in removed)
            if (!EnsureInstancesForData(k)) return -1;
        foreach (var kv in added)
            if (!EnsureInstancesForData(kv.Key) || !EnsureInstancesForData(kv.Value)) return -1;
        var w = NewFaslDataWriter();
        w.Str(ht.TestName);
        w.Varint((ulong)(baseProto == null ? 0 : baseProto.Part + 1));
        w.Varint((ulong)removed.Count);
        foreach (var k in removed)
            if (!w.TryValue(k)) return -1;
        w.Varint((ulong)added.Count);
        foreach (var kv in added)
            if (!w.TryValue(kv.Key) || !w.TryValue(kv.Value)) return -1;
        int part = map.AddDataPart(w.ToArray());
        map.HtPrototypes.Add(new FaslStructInternMap.HtPrototype(ht.TestName, snapshot, null, part));
        map.HtDataParts[ht] = part;
        return part;
    }

    /// <summary>A copy of the hash table prototype in data part PART.</summary>
    private void EmitHtPrototypeDataUse(int part)
    {
        var map = _faslStructMap!;
        _il.Emit(OpCodes.Ldsfld, map.DataPartsField());
        _il.Emit(OpCodes.Ldc_I4, part);
        _il.Emit(OpCodes.Ldsfld, map.HtCacheField());
        _il.Emit(OpCodes.Ldsfld, map.UninternedTableField());
        _il.Emit(OpCodes.Call, typeof(FaslData).GetMethod("HtPrototype")!);
        _il.Emit(OpCodes.Call, _htCopyLiteral);
    }

    // Hash table literals with more entries than this are built from a
    // prototype rather than inline.
    private const int MaxHtInline = 20;

    private static readonly MethodInfo _htCopyLiteral =
        typeof(LispHashTable).GetMethod("CopyLiteral", new[] { typeof(LispHashTable) })!;

    /// <summary>Methods (LispHashTable) -> void that Set, or Remove, the given
    /// entries. The prototype runs once per load, so the keys and values are
    /// emitted without per-site caches.</summary>
    private List<MethodBuilder> EmitHtPrototypeChunks(int id, string tag,
        List<KeyValuePair<LispObject, LispObject>> items, bool remove)
    {
        const int ChunkSize = 1500;
        var result = new List<MethodBuilder>();
        var setMethod = typeof(LispHashTable).GetMethod("Set", new[] { typeof(LispObject), typeof(LispObject) })!;
        var removeMethod = typeof(LispHashTable).GetMethod("Remove", new[] { typeof(LispObject) })!;
        for (int start = 0; start < items.Count; start += ChunkSize)
        {
            int end = Math.Min(start + ChunkSize, items.Count);
            var m = _faslTypeBuilder!.DefineMethod($"_htp_{id}_{tag}{result.Count}",
                MethodAttributes.Private | MethodAttributes.Static,
                typeof(void), new[] { typeof(LispHashTable) });
            result.Add(m);
            var cil = m.GetILGenerator().ForLibrary();
            var inner = new CilAssembler
            {
                _il = cil, _faslMode = true,
                _faslTypeBuilder = _faslTypeBuilder, _faslStructMap = _faslStructMap,
                _skipStructIntern = true,
                _suppressLiteralCache = true,
                _cachedLiteralDepth = 1,
            };
            for (int i = start; i < end; i++)
            {
                cil.Emit(OpCodes.Ldarg_0);
                inner.EmitLoadConstInline(items[i].Key);
                if (remove)
                {
                    cil.Emit(OpCodes.Callvirt, removeMethod);
                    cil.Emit(OpCodes.Pop);
                }
                else
                {
                    inner.EmitLoadConstInline(items[i].Value);
                    cil.Emit(OpCodes.Callvirt, setMethod);
                }
            }
            cil.Emit(OpCodes.Ret);
        }
        return result;
    }

    /// <summary>Whether a copy of KEY built from the literal is the same key to a
    /// table with test TEST: symbols always, numbers and characters under EQL
    /// and up, and under EQUAL and EQUALP also strings, pathnames and conses of
    /// such.</summary>
    private static bool KeyFoundByContent(LispObject key, string test)
    {
        switch (key)
        {
            case Symbol or Nil or T: return true;
            case Number or LispChar: return test != "EQ";
            case LispString or LispPathname: return test is "EQUAL" or "EQUALP";
            case Cons:
                if (test is not ("EQUAL" or "EQUALP")) return false;
                for (LispObject cur = key; ; )
                {
                    if (cur is Cons c)
                    {
                        if (!KeyFoundByContent(c.Car, test)) return false;
                        cur = c.Cdr;
                    }
                    else return KeyFoundByContent(cur, test);
                }
            default: return false;
        }
    }

    /// <summary>EQ for objects with identity; EQL for numbers and characters,
    /// which a literal rebuilds by value anyway.</summary>
    private static bool SameLiteralObject(LispObject a, LispObject b) =>
        ReferenceEquals(a, b)
        || (a is Number || a is LispChar) && Runtime.IsTrueEql(a, b);

    /// <summary>Whether no identity-carrying object under HT (HT included) is
    /// one the current literal, or another literal of the current form,
    /// reaches more than once.</summary>
    private bool HashTableIsSelfContained(LispHashTable ht)
    {
        var memoShared = _sharedMemo?.Shared;
        var cross = _faslStructMap?.CrossLiteralShared;
        if (memoShared == null && cross == null) return true;
        var seen = new HashSet<LispObject>(ReferenceEqualityComparer.Instance);
        var st = new Stack<LispObject>();
        st.Push(ht);
        while (st.Count > 0)
        {
            var node = st.Pop();
            if (!HasLiteralIdentity(node) || !seen.Add(node)) continue;
            if ((memoShared != null && memoShared.Contains(node))
                || (cross != null && cross.ContainsKey(node)))
                return false;
            switch (node)
            {
                case Cons c: st.Push(c.Cdr); st.Push(c.Car); break;
                case LispVector v:
                    if (v.ElementTypeName == "T")
                        for (int i = v.Length - 1; i >= 0; i--) st.Push(v.ElementAt(i));
                    break;
                case LispStruct ls:
                    foreach (var sl in ls.SlotsSnapshot()) if (sl is LispObject so) st.Push(so);
                    break;
                case LispHashTable h:
                    foreach (var kv in h.Entries) { st.Push(kv.Value); st.Push(kv.Key); }
                    break;
                case LispInstance li:
                    foreach (var sl in li.Slots) if (sl is LispObject so) st.Push(so);
                    break;
            }
        }
        return true;
    }

    /// <summary>Compute a deterministic content key for LispStruct interning.</summary>
    /// <summary>
    /// Try to emit IL for a struct constant via the make-load-form protocol (CLHS 3.2.4.2).
    /// Returns true and emits a LispStruct.InternViaEval call if make-load-form is defined
    /// for the struct's type; returns false to fall back to the direct Intern path.
    /// </summary>
    private bool TryEmitViaLoadForm(LispStruct ls, string internKey)
    {
        var mlfSym = Startup.Sym("MAKE-LOAD-FORM");
        if (mlfSym?.Function == null)
            return false;
        if (_faslStructMap != null && _faslStructMap.TryGetInstanceKey(ls, out var known))
        {
            // Already registered: created earlier in this file, or in progress
            // (a cycle through its own creation form, which the lookup then
            // reports at load as not yet created).
            EmitInstanceLookup(known);
            return true;
        }
        try
        {
            var raw = CallMakeLoadForm(mlfSym.Function, ls);
            LispObject form = raw is MvReturn mv && mv.Count > 0 ? mv[0] : raw;
            if (form is Nil) return false;
            // A creation form that only allocates and fills slots says nothing the
            // direct intern path does not already do, and that path neither builds
            // the form as a literal nor evaluates it at load. Coalton spent 44
            // seconds in 70,019 of these evaluations; the forms were all this shape.
            if (Runtime.FindClassOrNil(ls.TypeName) is LispClass cls2
                && cls2.IsStructureClass && cls2.StructSlotNames != null
                && cls2.StructSlotNames.Length == ls.SlotCount
                && FormIsSlotSaving(ls, cls2, form))
                return false;
            // In a fasl the structure goes through the make-load-form registry,
            // as an instance does: the creation form runs when the fasl is
            // loaded, in ModuleInit (CLHS 3.2.4.4), not when the literal is first
            // used, and the init form, if any, after it. The literal is then a
            // lookup of the object by its key.
            if (_faslStructMap?.UninternedTypeBuilder != null)
            {
                var initForm = raw is MvReturn mv2 && mv2.Count >= 2 ? mv2[1] : Nil.Instance;
                var regKey = EnsureCreatedTopLevelWith(ls, form, initForm);
                if (regKey == null) return false;
                EmitInstanceLookup(regKey);
                return true;
            }
            _il.Emit(OpCodes.Ldstr, Track(internKey));
            EmitLoadConstInline(form);
            _il.Emit(OpCodes.Call, typeof(LispStruct).GetMethod("InternViaEval",
                new[] { typeof(string), typeof(LispObject) })!);
            _il.Emit(OpCodes.Castclass, typeof(LispObject));
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>Whether TryEmitViaLoadForm leaves LS to the direct intern
    /// path (allocate and fill the slots) rather than to the make-load-form
    /// registry.</summary>
    private static bool StructBuiltFromSlots(LispStruct ls)
    {
        var mlfSym = Startup.Sym("MAKE-LOAD-FORM");
        if (mlfSym?.Function == null) return true;
        try
        {
            var raw = CallMakeLoadForm(mlfSym.Function, ls);
            LispObject form = raw is MvReturn mv && mv.Count > 0 ? mv[0] : raw;
            if (form is Nil) return true;
            return Runtime.FindClassOrNil(ls.TypeName) is LispClass cls
                && cls.IsStructureClass && cls.StructSlotNames != null
                && cls.StructSlotNames.Length == ls.SlotCount
                && FormIsSlotSaving(ls, cls, form);
        }
        catch { return true; }
    }

    private static string ComputeStructInternKey(LispStruct ls)
    {
        var sb = new System.Text.StringBuilder();
        var visited = new HashSet<LispStruct>(ReferenceEqualityComparer.Instance);
        AppendInternKey(sb, ls, 0, visited);
        return sb.ToString();
    }

    private static void AppendInternKey(System.Text.StringBuilder sb, LispObject obj, int depth, HashSet<LispStruct> visited)
    {
        if (depth > 20) { sb.Append("..."); return; }
        switch (obj)
        {
            case Nil: sb.Append("N"); break;
            case T: sb.Append("T"); break;
            case Fixnum f: sb.Append('I'); sb.Append(f.Value); break;
            case LispString s: sb.Append('"'); sb.Append(s.Value); sb.Append('"'); break;
            case LispChar c: sb.Append('\\'); sb.Append((int)c.Value); break;
            case SingleFloat sf: sb.Append('F'); sb.Append(Compat.SingleToInt32Bits(sf.Value)); break;
            case DoubleFloat df: sb.Append('D'); sb.Append(BitConverter.DoubleToInt64Bits(df.Value)); break;
            case Symbol sym:
                sb.Append(sym.HomePackage?.Name ?? "#");
                sb.Append(':');
                sb.Append(sym.Name);
                break;
            case LispStruct ls:
                if (!visited.Add(ls)) { sb.Append("#CYCLE"); break; }
                sb.Append("#S(");
                AppendInternKey(sb, ls.TypeName, depth + 1, visited);
                foreach (var slot in ls.SlotsSnapshot())
                {
                    sb.Append(' ');
                    AppendInternKey(sb, slot, depth + 1, visited);
                }
                sb.Append(')');
                visited.Remove(ls);
                break;
            case Cons cons:
                sb.Append('(');
                AppendInternKey(sb, cons.Car, depth + 1, visited);
                sb.Append('.');
                AppendInternKey(sb, cons.Cdr, depth + 1, visited);
                sb.Append(')');
                break;
            case Bignum bn: sb.Append('B'); sb.Append(bn.Value); break;
            case Ratio rat: sb.Append('R'); sb.Append(rat.Numerator); sb.Append('/'); sb.Append(rat.Denominator); break;
            case LispVector vec:
                sb.Append("#(");
                for (int i = 0; i < vec._elements.Length; i++)
                {
                    if (i > 0) sb.Append(' ');
                    AppendInternKey(sb, vec._elements[i], depth + 1, visited);
                }
                sb.Append(')');
                break;
            default:
                sb.Append(obj.GetType().Name);
                sb.Append('#');
                sb.Append(obj.GetHashCode());
                break;
        }
    }
}
