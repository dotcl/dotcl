namespace DotCL;

/// <summary>
/// Wraps a .NET Thread as a Lisp object.
/// </summary>
public class LispThread : LispObject
{
    public Thread Thread { get; }
    public string ThreadName { get; }
    public LispObject? ReturnValue { get; set; }

    public LispThread(Thread thread, string name)
    {
        Thread = thread;
        ThreadName = name;
    }

    public override string ToString() => $"#<THREAD \"{ThreadName}\" {(Thread.IsAlive ? "RUNNING" : "FINISHED")}>";
}

/// <summary>
/// Wraps a .NET Monitor-based lock as a Lisp object.
/// .NET's System.Threading.Monitor is re-entrant, so this serves
/// both (bt:make-lock) and (bt:make-recursive-lock).
/// </summary>
public class LispLock : LispObject
{
    public object Monitor { get; } = new object();
    public string LockName { get; }
    public bool Recursive { get; }

    public LispLock(string name, bool recursive = false)
    {
        LockName = name;
        Recursive = recursive;
    }

    public override string ToString() =>
        $"#<{(Recursive ? "RECURSIVE-" : "")}LOCK \"{LockName}\">";
}

/// <summary>
/// Condition variable: wraps Monitor.Wait / Pulse on an internal object.
/// (bordeaux-threads compatible)
/// </summary>
public class LispConditionVariable : LispObject
{
    public object SyncObj { get; } = new object();
    public string CvName { get; }

    public LispConditionVariable(string name) => CvName = name;

    public override string ToString() => $"#<CONDITION-VARIABLE \"{CvName}\">";
}

/// <summary>
/// Counting semaphore: wraps System.Threading.SemaphoreSlim.
/// (bordeaux-threads / SBCL-style)
/// </summary>
public class LispSemaphore : LispObject
{
    public System.Threading.SemaphoreSlim Sem { get; }
    public string SemName { get; }

    public LispSemaphore(string name, int initialCount)
    {
        SemName = name;
        Sem = new System.Threading.SemaphoreSlim(initialCount, int.MaxValue);
    }

    public override string ToString() =>
        $"#<SEMAPHORE \"{SemName}\" count={Sem.CurrentCount}>";
}

/// <summary>
/// A single 64-bit cell supporting lock-free atomic compare-and-swap, increment,
/// and decrement via System.Threading.Interlocked. The atomic concurrency
/// primitive layer (alongside LispLock); bordeaux-threads' atomic-integer is meant
/// to back its counter cell with one of these. Values are treated as signed 64-bit,
/// which covers all realistic counter use (full unsigned-64 would need bit
/// reinterpretation on read).
/// </summary>
public sealed class LispAtomicLong : LispObject
{
    private long _value;
    public LispAtomicLong(long value) => _value = value;

    /// <summary>Atomic read.</summary>
    public long Read() => System.Threading.Interlocked.Read(ref _value);
    /// <summary>Atomic write, returns the new value.</summary>
    public long Write(long v) { System.Threading.Interlocked.Exchange(ref _value, v); return v; }
    /// <summary>CAS: if the current value == old, set to @new and return true.</summary>
    public bool CompareAndSwap(long old, long @new)
        => System.Threading.Interlocked.CompareExchange(ref _value, @new, old) == old;
    /// <summary>Atomic add, returns the new value.</summary>
    public long Add(long delta) => System.Threading.Interlocked.Add(ref _value, delta);

    public override string ToString() => $"#<ATOMIC-LONG {Read()}>";
}

public partial class Runtime
{
    [ThreadStatic]
    private static LispThread? _currentLispThread;

    private static readonly System.Collections.Concurrent.ConcurrentDictionary<int, LispThread>
        _threadRegistry = new();

    // --- atomic-long helpers (dotcl:make-atomic-long et al.) ---
    private static LispAtomicLong AtomicLongArg(LispObject[] args, int i, string fn)
        => (i < args.Length ? args[i] : Nil.Instance) as LispAtomicLong
           ?? throw new LispErrorException(new LispTypeError(
               $"{fn}: not an ATOMIC-LONG", i < args.Length ? args[i] : Nil.Instance,
               Startup.Sym("T")));
    private static long AtomicLongInt(LispObject o, string fn) => o switch
    {
        Fixnum f => f.Value,
        Bignum b => (long)b.Value,
        _ => throw new LispErrorException(new LispTypeError(
            $"{fn}: integer expected", o, Startup.Sym("INTEGER")))
    };
    private static LispObject AtomicLongResult(long v)
        => Bignum.MakeInteger((System.Numerics.BigInteger)v);

    internal static LispObject MakeAtomicLong(LispObject[] args)
        => new LispAtomicLong(args.Length > 0 && args[0] is not Nil
               ? AtomicLongInt(args[0], "MAKE-ATOMIC-LONG") : 0L);
    internal static LispObject AtomicLongValue(LispObject[] args)
        => AtomicLongResult(AtomicLongArg(args, 0, "ATOMIC-LONG-VALUE").Read());
    internal static LispObject SetAtomicLongValue(LispObject[] args)
        // (set-atomic-long-value al newval) and the (setf atomic-long-value) order
        // (newval al) are both accepted: pick the ATOMIC-LONG and the integer by type.
    {
        var al = (args[0] as LispAtomicLong) ?? (args.Length > 1 ? args[1] as LispAtomicLong : null)
                 ?? throw new LispErrorException(new LispTypeError(
                     "SET-ATOMIC-LONG-VALUE: no ATOMIC-LONG argument", args[0], Startup.Sym("T")));
        var nv = ReferenceEquals(al, args[0]) ? args[1] : args[0];
        al.Write(AtomicLongInt(nv, "SET-ATOMIC-LONG-VALUE"));
        return nv;
    }
    internal static LispObject AtomicLongCas(LispObject[] args)
        => AtomicLongArg(args, 0, "ATOMIC-LONG-CAS").CompareAndSwap(
               AtomicLongInt(args[1], "ATOMIC-LONG-CAS"),
               AtomicLongInt(args[2], "ATOMIC-LONG-CAS"))
           ? T.Instance : (LispObject)Nil.Instance;
    internal static LispObject AtomicLongIncf(LispObject[] args)
        => AtomicLongResult(AtomicLongArg(args, 0, "ATOMIC-LONG-INCF").Add(
               args.Length > 1 ? AtomicLongInt(args[1], "ATOMIC-LONG-INCF") : 1L));
    internal static LispObject AtomicLongDecf(LispObject[] args)
        => AtomicLongResult(AtomicLongArg(args, 0, "ATOMIC-LONG-DECF").Add(
               -(args.Length > 1 ? AtomicLongInt(args[1], "ATOMIC-LONG-DECF") : 1L)));

    /// <summary>
    /// (bt:make-thread function &key name)
    /// Creates and starts a new thread running FUNCTION.
    /// The new thread does NOT inherit the parent thread's dynamic bindings; it
    /// sees the global (top-level) values of special variables, matching SBCL and
    /// bordeaux-threads. (bt exposes *default-special-bindings* for opting specific
    /// specials in; a worker that needs the parent's stream bindings must establish
    /// them itself, as micros does via its own with-io-redirection.)
    /// </summary>
    public static LispObject MakeThread(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("MAKE-THREAD: requires a function"));

        var fn = args[0];
        string name = "Anonymous";
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol s && s.Name == "NAME")
                name = args[i + 1] is LispString ls ? ls.Value : args[i + 1].ToString();
        }

        LispThread? lispThread = null;
        var thread = new Thread(() =>
        {
            // Publish stable LispThread identity for (current-thread) inside this thread
            _currentLispThread = lispThread;
            // The dynamic-binding stack is [ThreadStatic] and starts empty here, so
            // special-variable reads see their global values (no parent inheritance).
            LispObject? result = null;
            // Establish a top-level ABORT restart for this worker thread, like the
            // REPL does for the main thread (Program.cs), so a concurrency library
            // that terminates a worker via (invoke-restart 'abort) finds one —
            // compute-restarts was empty in worker threads, breaking lparallel's
            // active-worker-replacement (invoke-abort-thread). The stack is
            // [ThreadStatic], so this cluster is private to this thread.
            var abortTag = new object();
            RestartClusterStack.PushCluster(new[] {
                new LispRestart("ABORT", _ => Nil.Instance,
                    description: "Abort this thread.", tag: abortTag) });
            try
            {
                if (fn is LispFunction lfn)
                    result = lfn.Invoke();
                else if (fn is Symbol sym && sym.Function is LispFunction sfn)
                    result = sfn.Invoke();
            }
            catch (RestartInvocationException rie) when (ReferenceEquals(rie.Tag, abortTag))
            {
                // (invoke-restart 'abort) in the worker → unwind the body and end
                // the thread cleanly, returning NIL.
                result = Nil.Instance;
            }
            catch (Exception ex)
            {
                // Don't let thread exceptions crash the process
                var w = Console.Error;
                w.WriteLine($"Thread \"{name}\" error: {ex.Message}");
                w.Flush();
            }
            finally
            {
                RestartClusterStack.PopCluster();
                if (lispThread != null)
                {
                    lispThread.ReturnValue = result ?? Nil.Instance;
                    _threadRegistry.TryRemove(lispThread.Thread.ManagedThreadId, out _);
                }
            }
        })
        {
            Name = name,
            IsBackground = true
        };

        lispThread = new LispThread(thread, name);
        _threadRegistry[thread.ManagedThreadId] = lispThread;
        thread.Start();
        return lispThread;
    }

    /// <summary>(bt:current-thread) → thread object</summary>
    public static LispObject CurrentThread(LispObject[] args)
    {
        if (_currentLispThread == null)
        {
            _currentLispThread = new LispThread(Thread.CurrentThread, Thread.CurrentThread.Name ?? "main");
            _threadRegistry[Thread.CurrentThread.ManagedThreadId] = _currentLispThread;
        }
        return _currentLispThread;
    }

    /// <summary>(bt:thread-alive-p thread) → boolean</summary>
    public static LispObject ThreadAliveP(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispThread lt)
            throw new LispErrorException(new LispProgramError("THREAD-ALIVE-P: requires a thread"));
        return lt.Thread.IsAlive ? T.Instance : Nil.Instance;
    }

    /// <summary>(dotcl:thread-object thread) → the underlying System.Threading.Thread,
    /// wrapped as a .NET object so it can be inspected or passed to .NET APIs
    /// (e.g. ManagedThreadId, Priority, IsBackground) (dotcl/dotcl#26).</summary>
    public static LispObject ThreadObject(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispThread lt)
            throw new LispErrorException(new LispProgramError("THREAD-OBJECT: requires a thread"));
        return new LispDotNetObject(lt.Thread);
    }

    /// <summary>(bt:destroy-thread thread)</summary>
    public static LispObject DestroyThread(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispThread lt)
            throw new LispErrorException(new LispProgramError("DESTROY-THREAD: requires a thread"));
        // .NET doesn't support Thread.Abort in modern .NET; use interrupt
        lt.Thread.Interrupt();
        return T.Instance;
    }

    /// <summary>(bt:thread-name thread) → string</summary>
    public static LispObject ThreadName(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispThread lt)
            throw new LispErrorException(new LispProgramError("THREAD-NAME: requires a thread"));
        return new LispString(lt.ThreadName);
    }

    /// <summary>(bt:threadp object) → boolean</summary>
    public static LispObject Threadp(LispObject[] args)
    {
        if (args.Length < 1) return Nil.Instance;
        return args[0] is LispThread ? T.Instance : Nil.Instance;
    }

    /// <summary>(bt:make-lock &optional name) → lock</summary>
    public static LispObject MakeLock(LispObject[] args)
    {
        string name = args.Length > 0 && args[0] is LispString ls ? ls.Value : "anonymous";
        return new LispLock(name);
    }

    /// <summary>(bt:acquire-lock lock &optional wait-p timeout-sec) → boolean</summary>
    public static LispObject AcquireLock(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispLock lk)
            throw new LispErrorException(new LispProgramError("ACQUIRE-LOCK: requires a lock"));
        bool wait = args.Length < 2 || args[1] is not Nil;
        // Optional 3rd arg: timeout in seconds
        if (args.Length >= 3 && args[2] is not Nil)
        {
            var v = args[2];
            double timeoutSec = v switch
            {
                Fixnum f => (double)f.Value,
                SingleFloat sf => sf.Value,
                DoubleFloat df => df.Value,
                Ratio r => (double)r.Numerator / (double)r.Denominator,
                _ => 0.0
            };
            int timeoutMs = Math.Max(0, (int)(timeoutSec * 1000));
            bool entered = System.Threading.Monitor.TryEnter(lk.Monitor, timeoutMs);
            return entered ? T.Instance : Nil.Instance;
        }
        if (wait)
        {
            System.Threading.Monitor.Enter(lk.Monitor);
            return T.Instance;
        }
        return System.Threading.Monitor.TryEnter(lk.Monitor) ? T.Instance : Nil.Instance;
    }

    /// <summary>(bt:release-lock lock)</summary>
    public static LispObject ReleaseLock(LispObject[] args)
    {
        if (args.Length < 1)
            throw new LispErrorException(new LispProgramError("RELEASE-LOCK: requires a lock"));
        return ReleaseLock1(args[0]);
    }

    // 1-arg direct-delegate entry (same code path as ReleaseLock with one arg;
    // the not-a-lock case raises the identical error).
    public static LispObject ReleaseLock1(LispObject a)
    {
        if (a is not LispLock lk)
            throw new LispErrorException(new LispProgramError("RELEASE-LOCK: requires a lock"));
        System.Threading.Monitor.Exit(lk.Monitor);
        return T.Instance;
    }

    /// <summary>(bt:thread-join thread) → thread's return value</summary>
    public static LispObject ThreadJoin(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispThread lt)
            throw new LispErrorException(new LispProgramError("THREAD-JOIN: requires a thread"));
        lt.Thread.Join();
        return lt.ReturnValue ?? Nil.Instance;
    }

    /// <summary>(bt:thread-yield) — hint to scheduler</summary>
    public static LispObject ThreadYield(LispObject[] args)
    {
        Thread.Yield();
        return T.Instance;
    }

    /// <summary>(bt:make-recursive-lock &optional name) → lock (re-entrant)</summary>
    public static LispObject MakeRecursiveLock(LispObject[] args)
    {
        string name = args.Length > 0 && args[0] is LispString ls ? ls.Value : "anonymous";
        return new LispLock(name, recursive: true);
    }

    // --- Condition variables ---

    /// <summary>(bt:make-condition-variable &key name) → cv</summary>
    public static LispObject MakeConditionVariable(LispObject[] args)
    {
        string name = "anonymous";
        for (int i = 0; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol s && s.Name == "NAME")
                name = args[i + 1] is LispString ls ? ls.Value : args[i + 1].ToString()!;
        }
        return new LispConditionVariable(name);
    }

    /// <summary>
    /// (bt:condition-wait cv lock &key timeout)
    /// Caller must hold LOCK. Atomically releases lock and waits for notification;
    /// re-acquires lock before returning. Returns T (or NIL on timeout).
    /// </summary>
    public static LispObject ConditionWait(LispObject[] args)
    {
        if (args.Length < 2 || args[0] is not LispConditionVariable cv || args[1] is not LispLock lk)
            throw new LispErrorException(new LispProgramError(
                "CONDITION-WAIT: requires (cv lock)"));
        // Optional :timeout seconds
        double? timeoutSec = null;
        for (int i = 2; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol s && s.Name == "TIMEOUT")
            {
                var v = args[i + 1];
                timeoutSec = v switch
                {
                    Fixnum f => (double)f.Value,
                    SingleFloat sf => sf.Value,
                    DoubleFloat df => df.Value,
                    Ratio r => (double)r.Numerator / (double)r.Denominator,
                    _ => (double?)null
                };
            }
        }

        // Enter CV monitor before releasing lock so a concurrent notify
        // cannot slip through between Exit(lock) and Wait(cv.SyncObj).
        System.Threading.Monitor.Enter(cv.SyncObj);
        bool signaled = true;
        try
        {
            System.Threading.Monitor.Exit(lk.Monitor);
            if (timeoutSec.HasValue)
                signaled = System.Threading.Monitor.Wait(cv.SyncObj,
                    TimeSpan.FromSeconds(timeoutSec.Value));
            else
                System.Threading.Monitor.Wait(cv.SyncObj);
        }
        finally
        {
            System.Threading.Monitor.Exit(cv.SyncObj);
            System.Threading.Monitor.Enter(lk.Monitor);
        }
        return signaled ? T.Instance : Nil.Instance;
    }

    /// <summary>(bt:condition-notify cv) — wake one waiter</summary>
    public static LispObject ConditionNotify(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispConditionVariable cv)
            throw new LispErrorException(new LispProgramError("CONDITION-NOTIFY: requires cv"));
        System.Threading.Monitor.Enter(cv.SyncObj);
        try { System.Threading.Monitor.Pulse(cv.SyncObj); }
        finally { System.Threading.Monitor.Exit(cv.SyncObj); }
        return T.Instance;
    }

    /// <summary>(bt:condition-broadcast cv) — wake all waiters</summary>
    public static LispObject ConditionBroadcast(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispConditionVariable cv)
            throw new LispErrorException(new LispProgramError("CONDITION-BROADCAST: requires cv"));
        System.Threading.Monitor.Enter(cv.SyncObj);
        try { System.Threading.Monitor.PulseAll(cv.SyncObj); }
        finally { System.Threading.Monitor.Exit(cv.SyncObj); }
        return T.Instance;
    }

    // --- Semaphores ---

    /// <summary>(bt:make-semaphore &key name count) → semaphore</summary>
    public static LispObject MakeSemaphore(LispObject[] args)
    {
        string name = "anonymous";
        int count = 0;
        for (int i = 0; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol s)
            {
                if (s.Name == "NAME")
                    name = args[i + 1] is LispString ls ? ls.Value : args[i + 1].ToString()!;
                else if (s.Name == "COUNT" && args[i + 1] is Fixnum f)
                    count = (int)f.Value;
            }
        }
        if (count < 0)
            throw new LispErrorException(new LispProgramError("MAKE-SEMAPHORE: count must be non-negative"));
        return new LispSemaphore(name, count);
    }

    /// <summary>(bt:signal-semaphore sem &optional n) — release N tokens (default 1)</summary>
    public static LispObject SignalSemaphore(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispSemaphore sem)
            throw new LispErrorException(new LispProgramError("SIGNAL-SEMAPHORE: requires a semaphore"));
        int n = args.Length > 1 && args[1] is Fixnum f ? (int)f.Value : 1;
        if (n < 1) return T.Instance;
        sem.Sem.Release(n);
        return T.Instance;
    }

    /// <summary>(bt:wait-on-semaphore sem &key timeout) — acquire 1 token, block if none</summary>
    public static LispObject WaitOnSemaphore(LispObject[] args)
    {
        if (args.Length < 1 || args[0] is not LispSemaphore sem)
            throw new LispErrorException(new LispProgramError("WAIT-ON-SEMAPHORE: requires a semaphore"));
        double? timeoutSec = null;
        for (int i = 1; i + 1 < args.Length; i += 2)
        {
            if (args[i] is Symbol s && s.Name == "TIMEOUT")
            {
                var v = args[i + 1];
                timeoutSec = v switch
                {
                    Fixnum f => (double)f.Value,
                    SingleFloat sf => sf.Value,
                    DoubleFloat df => df.Value,
                    Ratio r => (double)r.Numerator / (double)r.Denominator,
                    _ => (double?)null
                };
            }
        }
        if (timeoutSec.HasValue)
        {
            bool got = sem.Sem.Wait(TimeSpan.FromSeconds(timeoutSec.Value));
            return got ? T.Instance : Nil.Instance;
        }
        sem.Sem.Wait();
        return T.Instance;
    }

    /// <summary>(dotcl:all-threads) → list of all live LispThread objects</summary>
    public static LispObject AllThreads(LispObject[] args)
    {
        CurrentThread([]);  // Ensure main thread is registered
        LispObject result = Nil.Instance;
        foreach (var lt in _threadRegistry.Values)
            if (lt.Thread.IsAlive)   // a finished thread may linger until its finally prunes
                result = new Cons(lt, result);
        return result;
    }

    /// <summary>(dotcl:lockp x) → T if X is a lock (recursive or not).</summary>
    public static LispObject Lockp(LispObject[] args)
        => args.Length > 0 && args[0] is LispLock ? T.Instance : Nil.Instance;

    /// <summary>(dotcl:recursive-lock-p x) → T if X is a recursive lock.</summary>
    public static LispObject RecursiveLockP(LispObject[] args)
        => args.Length > 0 && args[0] is LispLock { Recursive: true } ? T.Instance : Nil.Instance;

    internal static void RegisterThreadBuiltins()
    {
        Emitter.CilAssembler.RegisterFunction("%MAKE-THREAD",
            new LispFunction(Runtime.MakeThread, "%MAKE-THREAD"));
        Emitter.CilAssembler.RegisterFunction("%CURRENT-THREAD",
            new LispFunction(Runtime.CurrentThread, "%CURRENT-THREAD"));
        Emitter.CilAssembler.RegisterFunction("%THREAD-ALIVE-P",
            new LispFunction(Runtime.ThreadAliveP, "%THREAD-ALIVE-P"));
        Emitter.CilAssembler.RegisterFunction("%DESTROY-THREAD",
            new LispFunction(Runtime.DestroyThread, "%DESTROY-THREAD"));
        Emitter.CilAssembler.RegisterFunction("%THREAD-NAME",
            new LispFunction(Runtime.ThreadName, "%THREAD-NAME"));
        Emitter.CilAssembler.RegisterFunction("%THREADP",
            new LispFunction(Runtime.Threadp, "%THREADP"));
        Emitter.CilAssembler.RegisterFunction("%ALL-THREADS",
            new LispFunction(Runtime.AllThreads, "%ALL-THREADS"));
        Emitter.CilAssembler.RegisterFunction("%LOCKP",
            new LispFunction(Runtime.Lockp, "%LOCKP"));
        Emitter.CilAssembler.RegisterFunction("%RECURSIVE-LOCK-P",
            new LispFunction(Runtime.RecursiveLockP, "%RECURSIVE-LOCK-P"));
        Emitter.CilAssembler.RegisterFunction("%MAKE-LOCK",
            new LispFunction(Runtime.MakeLock, "%MAKE-LOCK"));
        Emitter.CilAssembler.RegisterFunction("%ACQUIRE-LOCK",
            new LispFunction(Runtime.AcquireLock, "%ACQUIRE-LOCK"));
        Emitter.CilAssembler.RegisterFunction("%RELEASE-LOCK",
            new LispFunction(Runtime.ReleaseLock, "%RELEASE-LOCK"));
        Emitter.CilAssembler.RegisterFunction("%THREAD-JOIN",
            new LispFunction(Runtime.ThreadJoin, "%THREAD-JOIN"));
        Emitter.CilAssembler.RegisterFunction("%THREAD-YIELD",
            new LispFunction(Runtime.ThreadYield, "%THREAD-YIELD"));
        Emitter.CilAssembler.RegisterFunction("%MAKE-RECURSIVE-LOCK",
            new LispFunction(Runtime.MakeRecursiveLock, "%MAKE-RECURSIVE-LOCK"));
        Emitter.CilAssembler.RegisterFunction("%MAKE-CONDITION-VARIABLE",
            new LispFunction(Runtime.MakeConditionVariable, "%MAKE-CONDITION-VARIABLE"));
        Emitter.CilAssembler.RegisterFunction("%CONDITION-WAIT",
            new LispFunction(Runtime.ConditionWait, "%CONDITION-WAIT"));
        Emitter.CilAssembler.RegisterFunction("%CONDITION-NOTIFY",
            new LispFunction(Runtime.ConditionNotify, "%CONDITION-NOTIFY"));
        Emitter.CilAssembler.RegisterFunction("%CONDITION-BROADCAST",
            new LispFunction(Runtime.ConditionBroadcast, "%CONDITION-BROADCAST"));
        Emitter.CilAssembler.RegisterFunction("%MAKE-SEMAPHORE",
            new LispFunction(Runtime.MakeSemaphore, "%MAKE-SEMAPHORE"));
        Emitter.CilAssembler.RegisterFunction("%SIGNAL-SEMAPHORE",
            new LispFunction(Runtime.SignalSemaphore, "%SIGNAL-SEMAPHORE"));
        Emitter.CilAssembler.RegisterFunction("%WAIT-ON-SEMAPHORE",
            new LispFunction(Runtime.WaitOnSemaphore, "%WAIT-ON-SEMAPHORE"));
    }
}
