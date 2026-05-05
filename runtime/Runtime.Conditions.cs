namespace DotCL;

public static partial class Runtime
{
    // --- Error ---

    public static LispObject LispError(LispObject msg)
    {
        string message = msg switch
        {
            LispString s => s.Value,
            _ => msg.ToString()
        };
        return ConditionSystem.Error(new LispError(message));
    }

    public static LispObject LispErrorFormat(LispObject[] args)
    {
        if (args.Length == 0)
            return ConditionSystem.Error(new LispError("Unknown error"));
        // (error condition-object) — pass through
        if (args[0] is LispCondition cond)
            return ConditionSystem.Error(cond);
        if (args[0] is LispInstance errInst)
            return ConditionSystem.Error(new LispInstanceCondition(errInst));
        // (error 'condition-type) or (error 'condition-type :initarg val ...) — create condition instance
        if (args[0] is Symbol sym)
        {
            var initargs = args[1..];
            var condObj = MakeConditionFromType(sym, initargs);
            if (condObj is LispInstanceCondition lic)
                return ConditionSystem.Error(lic);
            return ConditionSystem.Error(condObj);
        }
        // (error function &rest args) — function is a format control (e.g. from formatter)
        if (args[0] is LispFunction fn)
        {
            // Call the function with a string-output-stream to get the message
            string msg = "";
            try
            {
                var sw = new System.IO.StringWriter();
                var stream = new LispStringOutputStream(sw);
                var callArgs = new LispObject[args.Length];
                callArgs[0] = stream;
                Array.Copy(args, 1, callArgs, 1, args.Length - 1);
                fn.Invoke(callArgs);
                msg = stream.GetString();
            }
            catch { /* ignore formatting errors */ }
            var simpleErr = new LispError(msg);
            simpleErr.ConditionTypeName = "SIMPLE-ERROR";
            simpleErr.FormatControl = args[0];
            simpleErr.FormatArguments = args.Length > 1 ? Runtime.List(args[1..]) : Nil.Instance;
            return ConditionSystem.Error(simpleErr);
        }
        if (args[0] is not LispString fmt)
            return ConditionSystem.Error(new LispError(args[0].ToString()));
        string message;
        try { message = ((LispString)Format(Nil.Instance, args)).Value; }
        catch { message = fmt.Value; }
        var err = new LispError(message);
        err.ConditionTypeName = "SIMPLE-ERROR";
        err.FormatControl = fmt;
        err.FormatArguments = Runtime.List(args[1..]);
        return ConditionSystem.Error(err);
    }

    // --- Restart operations ---

    public static LispObject InvokeRestart(LispObject name, LispObject[] args)
    {
        LispRestart? restart;
        if (name is LispRestart r)
        {
            restart = RestartClusterStack.FindRestart(name, null) as LispRestart;
        }
        else
        {
            string restartName = name switch
            {
                Symbol sym => sym.Name,
                LispString s => s.Value,
                _ => name.ToString()
            };
            restart = RestartClusterStack.FindRestartByName(restartName);
        }
        if (restart == null)
            throw new LispErrorException(new LispControlError($"INVOKE-RESTART: restart not found: {name}"));
        if (restart.IsBindRestart)
        {
            // For restart-bind: call handler directly in current dynamic context
            return restart.Handler(args);
        }
        throw new RestartInvocationException(restart.Tag, args);
    }

    public static LispObject RestartArg(LispObject[] args, int index)
    {
        return index < args.Length ? args[index] : Nil.Instance;
    }

    public static LispObject RestartArgsToList(LispObject[] args)
    {
        LispObject result = Nil.Instance;
        for (int i = args.Length - 1; i >= 0; i--)
            result = new Cons(args[i], result);
        return result;
    }

    public static LispObject RestartArgsAsList(LispObject[] args, int startIndex)
    {
        if (args == null) return Nil.Instance;
        LispObject result = Nil.Instance;
        for (int i = args.Length - 1; i >= startIndex; i--)
            result = new Cons(args[i], result);
        return result;
    }

    public static LispObject RestartKeyArg(LispObject[] args, LispObject keyword, int startIndex)
    {
        if (args == null) return Nil.Instance;
        for (int i = startIndex; i + 1 < args.Length; i += 2)
        {
            if (args[i] == keyword) return args[i + 1];
        }
        return Nil.Instance;
    }

    public static LispObject FindRestart(LispObject name)
    {
        var restart = RestartClusterStack.FindRestart(name, null);
        return restart as LispObject ?? Nil.Instance;
    }

    public static LispObject FindRestart(LispObject name, LispObject condition)
    {
        var restart = RestartClusterStack.FindRestart(name, condition);
        return restart as LispObject ?? Nil.Instance;
    }

    public static LispObject ComputeRestarts()
    {
        return RestartClusterStack.ComputeRestarts(null);
    }

    public static LispObject ComputeRestarts(LispObject condition)
    {
        return RestartClusterStack.ComputeRestarts(condition);
    }

    public static LispObject FindRestartN(params LispObject[] args)
    {
        if (args.Length == 0) throw new LispErrorException(new LispProgramError("FIND-RESTART: too few arguments"));
        var condition = args.Length > 1 ? args[1] : null;
        var restart = RestartClusterStack.FindRestart(args[0], condition);
        return restart as LispObject ?? Nil.Instance;
    }

    public static LispObject ComputeRestartsN(params LispObject[] args)
    {
        var condition = args.Length > 0 ? args[0] : null;
        return RestartClusterStack.ComputeRestarts(condition);
    }

    // --- Signal / Warn ---

    public static LispObject LispSignal(LispObject condition)
    {
        if (condition is LispCondition c)
            return ConditionSystem.Signal(c);
        if (condition is LispInstance inst)
            return ConditionSystem.Signal(new LispInstanceCondition(inst));
        if (condition is Symbol sym)
            return ConditionSystem.Signal(MakeConditionFromType(sym, Array.Empty<LispObject>()));
        // String format control => SIMPLE-CONDITION
        if (condition is LispString fmtStr)
        {
            var sc = new LispCondition(fmtStr.Value);
            sc.ConditionTypeName = "SIMPLE-CONDITION";
            sc.FormatControl = fmtStr;
            sc.FormatArguments = Nil.Instance;
            return ConditionSystem.Signal(sc);
        }
        throw new LispErrorException(new LispTypeError("SIGNAL: not a condition", condition));
    }

    public static LispObject LispSignalFormat(LispObject[] args)
    {
        if (args.Length == 0)
            return ConditionSystem.Signal(new LispCondition("Unknown signal"));
        if (args[0] is LispCondition c)
            return ConditionSystem.Signal(c);
        if (args[0] is LispInstance inst)
            return ConditionSystem.Signal(new LispInstanceCondition(inst));
        if (args[0] is Symbol sym)
        {
            var initargs = args.Length > 1 ? args[1..] : Array.Empty<LispObject>();
            return ConditionSystem.Signal(MakeConditionFromType(sym, initargs));
        }
        // String message — preserve format control/arguments
        var sc = new LispCondition(args[0].ToString());
        sc.ConditionTypeName = "SIMPLE-CONDITION";
        if (args[0] is LispString fmtStr)
        {
            sc.FormatControl = fmtStr;
            sc.FormatArguments = Runtime.List(args[1..]);
        }
        return ConditionSystem.Signal(sc);
    }

    public static LispObject MakeConditionPublic(LispObject typeSpec, LispObject[] initargs)
    {
        Symbol sym;
        if (typeSpec is Symbol s) sym = s;
        else if (typeSpec is LispClass lc) sym = lc.Name;
        else if (typeSpec is Cons cl)
        {
            // Handle compound type specifiers: (OR type1 type2 ...) or (AND type1 type2 ...)
            sym = ResolveCompoundConditionType(cl);
        }
        else throw new LispErrorException(new LispTypeError("MAKE-CONDITION: type must be a symbol", typeSpec));
        return MakeConditionFromType(sym, initargs);
    }

    private static Symbol ResolveCompoundConditionType(Cons typeSpec)
    {
        var head = typeSpec.Car;
        var orSym = Startup.Sym("OR");
        var andSym = Startup.Sym("AND");
        if (head is Symbol hs && hs == orSym)
        {
            // (OR type1 type2 ...): pick the first concrete condition type
            var rest = typeSpec.Cdr;
            while (rest is Cons c)
            {
                if (c.Car is Symbol ts) return ts;
                if (c.Car is Cons nested) return ResolveCompoundConditionType(nested);
                rest = c.Cdr;
            }
        }
        else if (head is Symbol ha && ha == andSym)
        {
            // (AND type1 type2 ...): collect types, find a subtype of all
            var types = new System.Collections.Generic.List<Symbol>();
            var rest = typeSpec.Cdr;
            while (rest is Cons c)
            {
                if (c.Car is Symbol ts) types.Add(ts);
                rest = c.Cdr;
            }
            if (types.Count > 0)
            {
                // Try each type — if one is a subtype of all others, use it
                foreach (var t in types)
                {
                    bool subtypeOfAll = true;
                    foreach (var other in types)
                    {
                        if (t == other) continue;
                        if (Runtime.Subtypep(t, other) is Nil)
                        {
                            subtypeOfAll = false;
                            break;
                        }
                    }
                    if (subtypeOfAll) return t;
                }
                // If no single type works, try known intersection types
                // e.g., (AND simple-condition type-error) → simple-type-error
                return FindIntersectionConditionType(types);
            }
        }
        throw new LispErrorException(new LispTypeError("MAKE-CONDITION: cannot resolve compound type", typeSpec));
    }

    private static Symbol FindIntersectionConditionType(System.Collections.Generic.List<Symbol> types)
    {
        // Try well-known intersection condition types
        var knownIntersections = new (string, string, string)[]
        {
            ("SIMPLE-ERROR", "TYPE-ERROR", "SIMPLE-TYPE-ERROR"),
            ("SIMPLE-CONDITION", "TYPE-ERROR", "SIMPLE-TYPE-ERROR"),
            ("SIMPLE-CONDITION", "ERROR", "SIMPLE-ERROR"),
            ("SIMPLE-CONDITION", "WARNING", "SIMPLE-WARNING"),
        };
        foreach (var (a, b, result) in knownIntersections)
        {
            var sa = Startup.Sym(a);
            var sb = Startup.Sym(b);
            var sr = Startup.Sym(result);
            if (types.Contains(sa) && types.Contains(sb)) return sr;
            if (types.Contains(sb) && types.Contains(sa)) return sr;
        }
        // Fallback: use the first type
        return types[0];
    }

    internal static LispCondition MakeConditionFromType(Symbol typeName, LispObject[] initargs)
    {
        // Try to make an instance of the condition class
        try
        {
            var inst = MakeInstanceWithInitargs(typeName, initargs);
            if (inst is LispInstance li)
            {
                var lic = new LispInstanceCondition(li);
                // Store format-control/format-arguments/package on wrapper
                for (int j = 0; j + 1 < initargs.Length; j += 2)
                {
                    if (initargs[j] is Symbol ik)
                    {
                        if (ik.Name == "FORMAT-CONTROL")
                            lic.FormatControl = initargs[j+1];
                        else if (ik.Name == "FORMAT-ARGUMENTS")
                            lic.FormatArguments = initargs[j+1];
                        else if (ik.Name == "PACKAGE")
                            lic.PackageRef = initargs[j+1];
                        else if (ik.Name == "PATHNAME")
                            lic.FileErrorPathnameRef = initargs[j+1];
                        else if (ik.Name == "OPERATION")
                            lic.OperationRef = initargs[j+1];
                        else if (ik.Name == "OPERANDS")
                            lic.OperandsRef = initargs[j+1];
                    }
                }
                return lic;
            }
        }
        catch (LispErrorException) { throw; }       // Propagate Lisp errors (e.g. invalid initarg)
        catch (BlockReturnException) { throw; }     // Propagate non-local exits
        catch (CatchThrowException) { throw; }
        catch (GoException) { throw; }
        catch (HandlerCaseInvocationException) { throw; }
        catch (RestartInvocationException) { throw; }
        catch { }
        // For built-in condition types, return the appropriate C# type
        // so that handler-case type checks work correctly
        string msg = typeName.Name;
        // Extract :format-control and :format-arguments if provided
        LispObject formatControl = Nil.Instance;
        LispObject formatArguments = Nil.Instance;
        for (int i = 0; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is Symbol k)
            {
                if (k.Name == "FORMAT-CONTROL")
                {
                    formatControl = initargs[i+1];
                    if (initargs[i+1] is LispString fs)
                        msg = fs.Value;
                }
                else if (k.Name == "FORMAT-ARGUMENTS")
                {
                    formatArguments = initargs[i+1];
                }
            }
        }
        var result = typeName.Name switch
        {
            "PROGRAM-ERROR" => (LispCondition)new LispProgramError(msg),
            "TYPE-ERROR" => ExtractTypeError(initargs, msg),
            "ERROR" or "SIMPLE-ERROR" => new LispError(msg),
            "CONTROL-ERROR" => new LispControlError(msg),
            "PACKAGE-ERROR" => new LispError(msg),
            "ARITHMETIC-ERROR" => new LispError(msg),
            "WARNING" or "SIMPLE-WARNING" => new LispWarning(msg),
            "SIMPLE-CONDITION" => new LispCondition(msg),
            "INTERACTIVE-INTERRUPT" => new LispInteractiveInterrupt(),
            _ => new LispCondition(msg)
        };
        result.FormatControl = formatControl;
        result.FormatArguments = formatArguments;
        // Extract :PACKAGE and :PATHNAME initargs for condition-specific slots
        for (int i = 0; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is Symbol pk)
            {
                if (pk.Name == "PACKAGE")
                    result.PackageRef = initargs[i+1];
                else if (pk.Name == "PATHNAME")
                    result.FileErrorPathnameRef = initargs[i+1];
                else if (pk.Name == "OPERATION")
                    result.OperationRef = initargs[i+1];
                else if (pk.Name == "OPERANDS")
                    result.OperandsRef = initargs[i+1];
            }
        }
        // Set the CL condition type name so TYPE-OF returns correctly
        if (result.ConditionTypeName == "CONDITION" || result.ConditionTypeName == "ERROR")
            result.ConditionTypeName = typeName.Name;
        return result;
    }

    private static LispTypeError ExtractTypeError(LispObject[] initargs, string msg)
    {
        LispObject? datum = null;
        LispObject? expectedType = null;
        for (int i = 0; i + 1 < initargs.Length; i += 2)
        {
            if (initargs[i] is Symbol k)
            {
                if (k.Name == "DATUM") datum = initargs[i+1];
                else if (k.Name == "EXPECTED-TYPE") expectedType = initargs[i+1];
            }
        }
        return new LispTypeError(msg, datum ?? Nil.Instance, expectedType ?? Nil.Instance);
    }

    public static LispObject LispWarn(LispObject msg)
    {
        if (msg is LispCondition c)
        {
            // Per CL spec: condition must be of type WARNING
            if (!IsTruthy(Typep(c, Startup.Sym("WARNING"))))
                throw new LispErrorException(new LispTypeError("WARN: condition is not of type WARNING", c, Startup.Sym("WARNING")));
            return ConditionSystem.Warn(c);
        }
        if (msg is LispInstance inst)
        {
            var lic = new LispInstanceCondition(inst);
            if (!IsTruthy(Typep(lic, Startup.Sym("WARNING"))))
                throw new LispErrorException(new LispTypeError("WARN: condition is not of type WARNING", lic, Startup.Sym("WARNING")));
            return ConditionSystem.Warn(lic);
        }
        if (msg is Symbol sym)
        {
            var condObj = MakeConditionFromType(sym, Array.Empty<LispObject>());
            // Check the created condition is a WARNING
            if (!IsTruthy(Typep(condObj, Startup.Sym("WARNING"))))
                throw new LispErrorException(new LispTypeError("WARN: condition type is not a subtype of WARNING", condObj, Startup.Sym("WARNING")));
            return ConditionSystem.Warn(condObj);
        }
        // String format control => SIMPLE-WARNING
        string message = msg switch
        {
            LispString s => s.Value,
            _ => msg.ToString()
        };
        var warn = new LispWarning(message);
        warn.ConditionTypeName = "SIMPLE-WARNING";
        if (msg is LispString fmtStr)
        {
            warn.FormatControl = fmtStr;
            warn.FormatArguments = Nil.Instance;
        }
        return ConditionSystem.Warn(warn);
    }

    public static double ObjToDouble(LispObject obj) => obj switch {
        DoubleFloat df => df.Value,
        SingleFloat sf => (double)sf.Value,
        Fixnum f => (double)f.Value,
        Bignum b => (double)b.Value,
        Ratio r => (double)r.Numerator / (double)r.Denominator,
        _ => Convert.ToDouble(obj)
    };

    public static LispObject LispWarnFormat(LispObject[] args)
    {
        if (args.Length == 0)
        {
            throw new LispErrorException(new LispProgramError("WARN: too few arguments"));
        }
        if (args[0] is LispCondition c)
        {
            // Extra args with condition object → type-error
            if (args.Length > 1)
                throw new LispErrorException(new LispTypeError("WARN: extra arguments with condition object", args[0]));
            if (!IsTruthy(Typep(c, Startup.Sym("WARNING"))))
                throw new LispErrorException(new LispTypeError("WARN: condition is not of type WARNING", c, Startup.Sym("WARNING")));
            return ConditionSystem.Warn(c);
        }
        if (args[0] is LispInstance inst)
        {
            if (args.Length > 1)
                throw new LispErrorException(new LispTypeError("WARN: extra arguments with condition object", args[0]));
            var lic = new LispInstanceCondition(inst);
            if (!IsTruthy(Typep(lic, Startup.Sym("WARNING"))))
                throw new LispErrorException(new LispTypeError("WARN: condition is not of type WARNING", lic, Startup.Sym("WARNING")));
            return ConditionSystem.Warn(lic);
        }
        if (args[0] is Symbol sym)
        {
            var initargs = args[1..];
            var condObj = MakeConditionFromType(sym, initargs);
            // Check if it's a WARNING type
            if (!IsTruthy(Typep(condObj, Startup.Sym("WARNING"))))
                throw new LispErrorException(new LispTypeError("WARN: condition type is not a subtype of WARNING", condObj, Startup.Sym("WARNING")));
            return ConditionSystem.Warn(condObj);
        }
        if (args[0] is not LispString fmt)
            return ConditionSystem.Warn(new LispWarning(args[0].ToString() ?? ""));
        string message;
        try { message = ((LispString)Format(Nil.Instance, args)).Value; }
        catch { message = fmt.Value; }
        var warn = new LispWarning(message);
        warn.ConditionTypeName = "SIMPLE-WARNING";
        warn.FormatControl = fmt;
        warn.FormatArguments = Runtime.List(args[1..]);
        return ConditionSystem.Warn(warn);
    }

    internal static void RegisterConditionsBuiltins()
    {
        // CERROR: continuable error
        Emitter.CilAssembler.RegisterFunction("CERROR",
            new LispFunction(args => {
                if (args.Length < 2)
                    throw new LispErrorException(new LispProgramError("CERROR: too few arguments"));

                var continueFormatString = args[0];
                var datum = args[1];
                var restArgs = args.Length > 2 ? args[2..] : Array.Empty<LispObject>();

                string continueDescription;
                if (continueFormatString is LispString cfs)
                {
                    try
                    {
                        var fmtResult = Runtime.Format(Nil.Instance,
                            new LispObject[] { cfs }.Concat(restArgs).ToArray());
                        continueDescription = fmtResult is LispString ls ? ls.Value : fmtResult.ToString();
                    }
                    catch
                    {
                        continueDescription = cfs.Value;
                    }
                }
                else
                {
                    continueDescription = continueFormatString?.ToString() ?? "Continue";
                }

                LispCondition condition;
                if (datum is LispCondition cond)
                {
                    condition = cond;
                }
                else if (datum is LispInstance errInst)
                {
                    condition = new LispInstanceCondition(errInst);
                }
                else if (datum is Symbol sym)
                {
                    condition = Runtime.MakeConditionFromType(sym, restArgs);
                }
                else if (datum is LispString errFmt)
                {
                    string template = errFmt.Value;
                    try
                    {
                        var fmtResult = Runtime.Format(Nil.Instance,
                            new LispObject[] { errFmt }.Concat(restArgs).ToArray());
                        var msg = fmtResult is LispString ls2 ? ls2.Value : fmtResult.ToString();
                        var err = new LispError(msg);
                        err.ConditionTypeName = "SIMPLE-ERROR";
                        err.FormatControl = errFmt;
                        err.FormatArguments = Runtime.List(restArgs);
                        condition = err;
                    }
                    catch
                    {
                        condition = new LispError(template);
                    }
                }
                else if (datum is LispFunction fmtFn)
                {
                    try
                    {
                        var fmtResult = Runtime.Format(Nil.Instance,
                            new LispObject[] { datum }.Concat(restArgs).ToArray());
                        var msg = fmtResult is LispString ls3 ? ls3.Value : fmtResult.ToString();
                        var err = new LispError(msg);
                        err.ConditionTypeName = "SIMPLE-ERROR";
                        err.FormatControl = datum;
                        err.FormatArguments = Runtime.List(restArgs);
                        condition = err;
                    }
                    catch
                    {
                        condition = new LispError("Error (function format control)");
                    }
                }
                else
                {
                    condition = new LispError(datum?.ToString() ?? "Unknown error");
                }

                var restart = new LispRestart("CONTINUE",
                    _ => Nil.Instance,
                    description: continueDescription);
                RestartClusterStack.PushCluster(new[] { restart });
                try
                {
                    HandlerClusterStack.Signal(condition);
                    throw new LispErrorException(condition);
                }
                catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, restart.Tag))
                {
                    return Nil.Instance;
                }
                finally
                {
                    RestartClusterStack.PopCluster();
                }
            }, "CERROR", -1));

        Emitter.CilAssembler.RegisterFunction("PRINT-NOT-READABLE-OBJECT",
            new LispFunction(args => {
                Runtime.CheckArityExact("PRINT-NOT-READABLE-OBJECT", args, 1);
                return Nil.Instance;
            }, "PRINT-NOT-READABLE-OBJECT", -1));
        Emitter.CilAssembler.RegisterFunction("UNBOUND-SLOT-INSTANCE",
            new LispFunction(args => {
                Runtime.CheckArityExact("UNBOUND-SLOT-INSTANCE", args, 1);
                var cond = args[0];
                if (cond is LispInstanceCondition lic)
                    cond = lic.Instance;
                if (cond is LispInstance inst && inst.Class.SlotIndex.TryGetValue("INSTANCE", out int idx))
                    return inst.Slots[idx] ?? Nil.Instance;
                return Nil.Instance;
            }, "UNBOUND-SLOT-INSTANCE", -1));

        Startup.RegisterUnary("CELL-ERROR-NAME", obj =>
            obj is LispCellError ce ? ce.Name :
            obj is LispInstanceCondition lic && lic.Instance.Class.SlotIndex.TryGetValue("NAME", out var idx) && idx < lic.Instance.Slots.Length ? (lic.Instance.Slots[idx] ?? Nil.Instance) :
            Nil.Instance);

        // MAKE-CONDITION
        Emitter.CilAssembler.RegisterFunction("MAKE-CONDITION",
            new LispFunction(args => {
                if (args.Length == 0) throw new LispErrorException(new LispProgramError("MAKE-CONDITION: missing type argument"));
                var type = args[0];
                var initargs = args.Length > 1 ? args[1..] : Array.Empty<LispObject>();
                return Runtime.MakeConditionPublic(type, initargs);
            }));

        // Condition accessor functions
        Startup.RegisterUnary("FILE-ERROR-PATHNAME", obj => {
            if (obj is LispCondition cond && cond.FileErrorPathnameRef != null)
                return cond.FileErrorPathnameRef;
            if (obj is LispInstanceCondition lic) return Runtime.SlotValue(lic.Instance, Startup.Sym("PATHNAME"));
            if (obj is LispInstance inst) return Runtime.SlotValue(inst, Startup.Sym("PATHNAME"));
            return Nil.Instance;
        });
        Startup.RegisterUnary("TYPE-ERROR-DATUM", obj => {
            if (obj is LispTypeError te) return te.Datum ?? Nil.Instance;
            if (obj is LispInstanceCondition lic) return Runtime.SlotValue(lic.Instance, Startup.Sym("DATUM"));
            if (obj is LispInstance inst) return Runtime.SlotValue(inst, Startup.Sym("DATUM"));
            return Nil.Instance;
        });
        Startup.RegisterUnary("TYPE-ERROR-EXPECTED-TYPE", obj => {
            if (obj is LispTypeError te) return te.ExpectedType ?? Nil.Instance;
            if (obj is LispInstanceCondition lic) return Runtime.SlotValue(lic.Instance, Startup.Sym("EXPECTED-TYPE"));
            if (obj is LispInstance inst) return Runtime.SlotValue(inst, Startup.Sym("EXPECTED-TYPE"));
            return Nil.Instance;
        });
        Startup.RegisterUnary("SIMPLE-CONDITION-FORMAT-CONTROL", obj => {
            if (obj is LispInstanceCondition lic)
            {
                if (lic.Instance.Class.SlotIndex.TryGetValue("FORMAT-CONTROL", out int idx)
                    && idx < lic.Instance.Slots.Length && lic.Instance.Slots[idx] != null)
                    return lic.Instance.Slots[idx]!;
                return lic.FormatControl;
            }
            if (obj is LispCondition cond) return cond.FormatControl;
            if (obj is LispInstance inst)
            {
                if (inst.Class.SlotIndex.TryGetValue("FORMAT-CONTROL", out int idx)
                    && idx < inst.Slots.Length && inst.Slots[idx] != null)
                    return inst.Slots[idx]!;
                return Nil.Instance;
            }
            return Nil.Instance;
        });
        Startup.RegisterUnary("SIMPLE-CONDITION-FORMAT-ARGUMENTS", obj => {
            if (obj is LispInstanceCondition lic)
            {
                if (lic.Instance.Class.SlotIndex.TryGetValue("FORMAT-ARGUMENTS", out int idx)
                    && idx < lic.Instance.Slots.Length && lic.Instance.Slots[idx] != null)
                    return lic.Instance.Slots[idx]!;
                return lic.FormatArguments;
            }
            if (obj is LispCondition cond) return cond.FormatArguments;
            if (obj is LispInstance inst)
            {
                if (inst.Class.SlotIndex.TryGetValue("FORMAT-ARGUMENTS", out int idx)
                    && idx < inst.Slots.Length && inst.Slots[idx] != null)
                    return inst.Slots[idx]!;
                return Nil.Instance;
            }
            return Nil.Instance;
        });

        // Restart functions
        Emitter.CilAssembler.RegisterFunction("INVOKE-RESTART",
            new LispFunction(args => {
                if (args.Length == 0) throw new LispErrorException(new LispProgramError("INVOKE-RESTART: too few arguments"));
                var rest = args.Skip(1).ToArray();
                return Runtime.InvokeRestart(args[0], rest);
            }, "INVOKE-RESTART", -1));
        Emitter.CilAssembler.RegisterFunction("FIND-RESTART", new LispFunction(args => {
            if (args.Length == 0) throw new LispErrorException(new LispProgramError("FIND-RESTART: too few arguments"));
            if (args.Length > 1)
                return Runtime.FindRestart(args[0], args[1]);
            return Runtime.FindRestart(args[0]);
        }, "FIND-RESTART", -1));
        Emitter.CilAssembler.RegisterFunction("COMPUTE-RESTARTS", new LispFunction(args => {
            if (args.Length > 0)
                return Runtime.ComputeRestarts(args[0]);
            return Runtime.ComputeRestarts();
        }, "COMPUTE-RESTARTS", -1));
        Startup.RegisterUnary("PACKAGE-ERROR-PACKAGE", obj => Runtime.PackageErrorPackage(obj));
        Startup.RegisterUnary("ARITHMETIC-ERROR-OPERATION", obj => {
            if (obj is LispInstanceCondition lic)
            {
                if (lic.Instance.Class.SlotIndex.TryGetValue("OPERATION", out int idx) && idx < lic.Instance.Slots.Length && lic.Instance.Slots[idx] != null)
                    return lic.Instance.Slots[idx]!;
                if (lic.OperationRef != null) return lic.OperationRef;
            }
            if (obj is LispCondition lc && lc.OperationRef != null)
                return lc.OperationRef;
            return Nil.Instance;
        });
        Startup.RegisterUnary("ARITHMETIC-ERROR-OPERANDS", obj => {
            if (obj is LispInstanceCondition lic)
            {
                if (lic.Instance.Class.SlotIndex.TryGetValue("OPERANDS", out int idx) && idx < lic.Instance.Slots.Length && lic.Instance.Slots[idx] != null)
                    return lic.Instance.Slots[idx]!;
                if (lic.OperandsRef != null) return lic.OperandsRef;
            }
            if (obj is LispCondition lc && lc.OperandsRef != null)
                return lc.OperandsRef;
            return Nil.Instance;
        });
        Startup.RegisterUnary("INVOKE-DEBUGGER", condition => {
            var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
            if (DynamicBindings.TryGet(hookSym, out var hookVal) && hookVal is not Nil)
            {
                DynamicBindings.Push(hookSym, Nil.Instance);
                try
                {
                    if (hookVal is LispFunction fn)
                        fn.Invoke(condition, hookVal);
                    else
                        Runtime.Funcall(hookVal, new[] { condition, hookVal });
                }
                finally
                {
                    DynamicBindings.Pop(hookSym);
                }
            }
            // Hook returned or was NIL → enter standard debugger
            // Debugger.Enter never returns normally (exits via restart invocation)
            return Debugger.Enter(condition);
        });
        DotCL.Emitter.CilAssembler.RegisterFunction("BREAK", new LispFunction(args => {
            // Build condition message
            string message = "Break";
            if (args.Length > 0)
            {
                // Use FORMAT to process format string + args
                var fmtArgs = new LispObject[args.Length];
                Array.Copy(args, fmtArgs, args.Length);
                var formatted = Runtime.Format(Nil.Instance, fmtArgs);
                if (formatted is LispString ls) message = ls.Value;
                else message = formatted.ToString() ?? "Break";
            }
            var condition = new LispCondition(message) { ConditionTypeName = "SIMPLE-CONDITION" };

            // Establish CONTINUE restart
            var continueTag = new object();
            var continueRestart = new LispRestart("CONTINUE",
                _ => Nil.Instance,
                description: "Return from BREAK.",
                tag: continueTag);
            RestartClusterStack.PushCluster(new[] { continueRestart });
            try
            {
                // Bind *debugger-hook* to NIL and enter debugger directly
                var hookSym = Startup.Sym("*DEBUGGER-HOOK*");
                DynamicBindings.Push(hookSym, Nil.Instance);
                try
                {
                    Debugger.Enter(condition);
                }
                finally
                {
                    DynamicBindings.Pop(hookSym);
                }
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, continueTag))
            {
                // CONTINUE restart invoked → return NIL from break
            }
            finally
            {
                RestartClusterStack.PopCluster();
            }
            return Nil.Instance;
        }, "BREAK", -1));
        Startup.RegisterUnary("RESTART-NAME", obj => {
            if (obj is LispRestart r)
            {
                if (r.NameSymbol != null) return r.NameSymbol;
                return r.Name != null ? (LispObject)Startup.Sym(r.Name) : Nil.Instance;
            }
            throw new LispErrorException(new LispTypeError("RESTART-NAME: not a restart", obj));
        });
        Startup.RegisterUnary("INVOKE-RESTART-INTERACTIVELY", obj => {
            LispRestart? restart = null;
            if (obj is LispRestart r)
                restart = r;
            else
            {
                string name = obj is Symbol s ? s.Name : obj is LispString ls ? ls.Value : obj.ToString()!;
                restart = RestartClusterStack.FindRestartByName(name) as LispRestart;
            }
            if (restart == null)
                throw new LispErrorException(new LispControlError($"INVOKE-RESTART-INTERACTIVELY: restart not found: {obj}"));
            LispObject[] args;
            if (restart.InteractiveFunction != null)
            {
                var argList = Runtime.Funcall(restart.InteractiveFunction);
                var argsList = new System.Collections.Generic.List<LispObject>();
                var cur = argList;
                while (cur is Cons c) { argsList.Add(c.Car); cur = c.Cdr; }
                args = argsList.ToArray();
            }
            else
                args = Array.Empty<LispObject>();
            return Runtime.InvokeRestart(obj, args);
        });
        Emitter.CilAssembler.RegisterFunction("STORE-VALUE", new LispFunction(args => {
            var val = args.Length > 0 ? args[0] : Nil.Instance;
            LispObject? condition = args.Length > 1 ? args[1] : null;
            var restart = RestartClusterStack.FindRestartByName("STORE-VALUE", condition);
            if (restart == null) return Nil.Instance;
            if (restart.IsBindRestart) return restart.Handler(new[] { val });
            throw new RestartInvocationException(restart.Tag, new[] { val });
        }, "STORE-VALUE", -1));
        Emitter.CilAssembler.RegisterFunction("USE-VALUE", new LispFunction(args => {
            var val = args.Length > 0 ? args[0] : Nil.Instance;
            LispObject? condition = args.Length > 1 ? args[1] : null;
            var restart = RestartClusterStack.FindRestartByName("USE-VALUE", condition);
            if (restart == null) return Nil.Instance;
            if (restart.IsBindRestart) return restart.Handler(new[] { val });
            throw new RestartInvocationException(restart.Tag, new[] { val });
        }, "USE-VALUE", -1));
        Emitter.CilAssembler.RegisterFunction("CONTINUE", new LispFunction(args => {
            LispObject? condition = args.Length > 0 ? args[0] : null;
            var restart = RestartClusterStack.FindRestartByName("CONTINUE", condition);
            if (restart == null) return Nil.Instance;
            if (restart.IsBindRestart) return restart.Handler(Array.Empty<LispObject>());
            throw new RestartInvocationException(restart.Tag, Array.Empty<LispObject>());
        }, "CONTINUE", -1));
        Emitter.CilAssembler.RegisterFunction("ABORT", new LispFunction(args => {
            LispObject? condition = args.Length > 0 ? args[0] : null;
            var restart = RestartClusterStack.FindRestartByName("ABORT", condition);
            if (restart == null)
                throw new LispErrorException(new LispControlError("ABORT restart not found"));
            if (restart.IsBindRestart) return restart.Handler(Array.Empty<LispObject>());
            throw new RestartInvocationException(restart.Tag, Array.Empty<LispObject>());
        }, "ABORT", -1));
        Emitter.CilAssembler.RegisterFunction("MUFFLE-WARNING", new LispFunction(args => {
            LispObject? condition = args.Length > 0 ? args[0] : null;
            var restart = RestartClusterStack.FindRestartByName("MUFFLE-WARNING", condition);
            if (restart == null)
                throw new LispErrorException(new LispControlError("MUFFLE-WARNING restart not found"));
            if (restart.IsBindRestart) return restart.Handler(Array.Empty<LispObject>());
            throw new RestartInvocationException(restart.Tag, Array.Empty<LispObject>());
        }, "MUFFLE-WARNING", -1));
        // with-condition-restarts helper functions
        Emitter.CilAssembler.RegisterFunction("%ASSOCIATE-CONDITION-RESTARTS", new LispFunction(args => {
            RestartClusterStack.AssociateConditionRestarts(args[0], args[1]);
            return Nil.Instance;
        }, "%ASSOCIATE-CONDITION-RESTARTS", 2));
        Emitter.CilAssembler.RegisterFunction("%DISASSOCIATE-CONDITION-RESTARTS", new LispFunction(args => {
            RestartClusterStack.DisassociateConditionRestarts(args[0], args[1]);
            return Nil.Instance;
        }, "%DISASSOCIATE-CONDITION-RESTARTS", 2));
        Emitter.CilAssembler.RegisterFunction("%TOP-CLUSTER-RESTARTS", new LispFunction(args => {
            return RestartClusterStack.GetTopClusterRestarts();
        }, "%TOP-CLUSTER-RESTARTS", 0));
    }

}
