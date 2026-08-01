namespace DotCL;

/// <summary>
/// Wraps a .NET Thread as a Lisp object.
/// </summary>
public class LispThread : LispObject
{
    public Thread Thread { get; }
    public string ThreadName { get; }
    public LispObject? ReturnValue { get; set; }

    /// <summary>
    /// Functions queued by INTERRUPT-THREAD, to run ON this thread the next time
    /// it notices — which, on .NET, means when a blocking wait it is sitting in
    /// throws ThreadInterruptedException. See Runtime.InterruptThread.
    /// </summary>
    public System.Collections.Concurrent.ConcurrentQueue<LispObject> PendingInterrupts { get; }
        = new System.Collections.Concurrent.ConcurrentQueue<LispObject>();

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

    // Typed direct delegates for the hot 1-arg / 2-arg calls (e.g. the compiler's
    // uninterned-var counter increments once per fresh gensym during a compile).
    // A LispAtomicLong argument reuses the array validator by wrapping the single
    // arg; the delta-less form is the common one.
    private static LispAtomicLong AtomicLong1(LispObject o, string fn)
        => o as LispAtomicLong ?? throw new LispErrorException(new LispTypeError(
               $"{fn}: not an ATOMIC-LONG", o, Startup.Sym("T")));
    internal static LispObject AtomicLongIncf1(LispObject a)
        => AtomicLongResult(AtomicLong1(a, "ATOMIC-LONG-INCF").Add(1L));
    internal static LispObject AtomicLongIncf2(LispObject a, LispObject b)
        => AtomicLongResult(AtomicLong1(a, "ATOMIC-LONG-INCF").Add(AtomicLongInt(b, "ATOMIC-LONG-INCF")));
    internal static LispObject AtomicLongDecf1(LispObject a)
        => AtomicLongResult(AtomicLong1(a, "ATOMIC-LONG-DECF").Add(-1L));
    internal static LispObject AtomicLongDecf2(LispObject a, LispObject b)
        => AtomicLongResult(AtomicLong1(a, "ATOMIC-LONG-DECF").Add(-AtomicLongInt(b, "ATOMIC-LONG-DECF")));

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
            catch (System.Threading.ThreadInterruptedException)
            {
                // DESTROY-THREAD. Asking a thread to die is not an error to
                // report — and the caller doing it routinely (bordeaux-threads'
                // WITH-TIMEOUT retires its watchdog this way on every successful
                // body) would otherwise print on every normal completion.
                result = Nil.Instance;
            }
            catch (Exception ex)
            {
                // Don't let thread exceptions crash the process
                var w = Console.Error;
                w.WriteLine($"Thread \"{name}\" error: {ex.Message}");
                if (Startup.DebugStacktrace && !string.IsNullOrEmpty(ex.StackTrace))
                    w.WriteLine(ex.StackTrace);
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

    /// <summary>
    /// (dotcl:interrupt-thread thread function) — queue FUNCTION to run on
    /// THREAD and poke the thread so it notices.
    ///
    /// Delivery is what .NET can offer without VM support: Thread.Interrupt
    /// unblocks a thread sitting in a wait (SLEEP, JOIN, lock/condition-variable/
    /// semaphore) and nothing else. A thread busy in a computation is NOT
    /// interrupted — the function stays queued until that thread next blocks.
    /// This covers the timeout cases that matter in practice (I/O, locks,
    /// condition variables); killing a compute loop needs cooperative safepoints
    /// in generated code, which is a separate, much larger change.
    ///
    /// The function runs on the target thread at the point where the wait was
    /// interrupted, so a non-local exit from it (SIGNAL, THROW, an ABORT
    /// restart) unwinds that thread — which is how a timeout is delivered.
    /// If it returns normally, the interrupted wait gives up and returns.
    /// </summary>
    public static LispObject InterruptThread(LispObject[] args)
    {
        if (args.Length < 2 || args[0] is not LispThread lt)
            throw new LispErrorException(new LispProgramError(
                "INTERRUPT-THREAD: requires a thread and a function"));
        var fn = args[1];
        if (fn is not LispFunction && !(fn is Symbol s && s.Function is LispFunction))
            throw new LispErrorException(new LispProgramError(
                "INTERRUPT-THREAD: second argument must be a function"));
        if (!lt.Thread.IsAlive) return Nil.Instance;
        lt.PendingInterrupts.Enqueue(fn);
        lt.Thread.Interrupt();
        return T.Instance;
    }

    /// <summary>
    /// Run every function INTERRUPT-THREAD queued for the current thread, in
    /// order; return whether there was anything to run. Called from the blocking
    /// primitives when their wait is cut short by Thread.Interrupt. A queued
    /// function that exits non-locally takes the rest of the queue with it —
    /// same as any handler that unwinds.
    ///
    /// A false return means the interrupt was not one of ours: DESTROY-THREAD
    /// also pokes the thread (that is all .NET offers), and its contract is that
    /// the thread dies. Callers rethrow in that case, which is what the
    /// blocking primitives did before INTERRUPT-THREAD existed.
    /// </summary>
    public static bool RunPendingInterrupts()
    {
        // The main thread has no LispThread until someone asks for one, and
        // INTERRUPT-THREAD can only have been handed one that exists — so look
        // it up rather than creating a fresh (empty) one here.
        var self = _currentLispThread
                   ?? (_threadRegistry.TryGetValue(Thread.CurrentThread.ManagedThreadId, out var reg)
                       ? reg : null);
        if (self == null) return false;
        bool ran = false;
        while (self.PendingInterrupts.TryDequeue(out var fn))
        {
            ran = true;
            if (fn is LispFunction lfn) lfn.Invoke();
            else if (fn is Symbol sym && sym.Function is LispFunction sfn) sfn.Invoke();
        }
        return ran;
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

    // 1-arg direct-delegate entry: (acquire-lock lock) with both optionals
    // defaulted, which is what WITH-LOCK-HELD expands to and therefore the shape
    // nearly every acquire takes. Same path AcquireLock runs for one argument
    // (wait defaults true, no timeout), same error for a non-lock. Calls that do
    // pass the optionals still go through the args-array wrapper.
    public static LispObject AcquireLock1(LispObject a)
    {
        if (a is not LispLock lk)
            throw new LispErrorException(new LispProgramError("ACQUIRE-LOCK: requires a lock"));
        System.Threading.Monitor.Enter(lk.Monitor);
        return T.Instance;
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
        try { lt.Thread.Join(); }
        catch (System.Threading.ThreadInterruptedException) { if (!RunPendingInterrupts()) throw; }
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
        bool interrupted = false;
        try
        {
            System.Threading.Monitor.Exit(lk.Monitor);
            if (timeoutSec.HasValue)
                signaled = System.Threading.Monitor.Wait(cv.SyncObj,
                    TimeSpan.FromSeconds(timeoutSec.Value));
            else
                System.Threading.Monitor.Wait(cv.SyncObj);
        }
        catch (System.Threading.ThreadInterruptedException)
        {
            // INTERRUPT-THREAD cut the wait short. The finally below still
            // restores the documented lock state before the queued function
            // runs, so it sees CONDITION-WAIT's normal postcondition.
            interrupted = true;
        }
        finally
        {
            System.Threading.Monitor.Exit(cv.SyncObj);
            System.Threading.Monitor.Enter(lk.Monitor);
        }
        if (interrupted)
        {
            if (!RunPendingInterrupts())
                throw new System.Threading.ThreadInterruptedException();
            return Nil.Instance;   // woke without a notification
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
        try
        {
            if (timeoutSec.HasValue)
            {
                bool got = sem.Sem.Wait(TimeSpan.FromSeconds(timeoutSec.Value));
                return got ? T.Instance : Nil.Instance;
            }
            sem.Sem.Wait();
        }
        catch (System.Threading.ThreadInterruptedException)
        {
            if (!RunPendingInterrupts()) throw;
            return Nil.Instance;   // woke without acquiring
        }
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
        // The % variants are what compiled code calls; give them the same 1-arg
        // direct delegates as the DOTCL-package registrations in Startup.
        var pctAcquire = new LispFunction(Runtime.AcquireLock, "%ACQUIRE-LOCK");
        pctAcquire.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.AcquireLock1);
        Emitter.CilAssembler.RegisterFunction("%ACQUIRE-LOCK", pctAcquire);
        var pctRelease = new LispFunction(Runtime.ReleaseLock, "%RELEASE-LOCK");
        pctRelease.SetDirectDelegate((Func<LispObject, LispObject>)Runtime.ReleaseLock1);
        Emitter.CilAssembler.RegisterFunction("%RELEASE-LOCK", pctRelease);
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
