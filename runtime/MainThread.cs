using System.Collections.Concurrent;
using System.Runtime.ExceptionServices;

namespace DotCL;

/// <summary>
/// The process main thread, usable as a work queue.
///
/// dotcl does not run Lisp on the thread the process starts on: the entry point
/// hands the work to a thread with a 256 MB stack, because deeply nested macro
/// expansion (the SBCL cross-compiler) overflows the OS default. That is fine
/// everywhere except for UI toolkits that require thread 0. macOS AppKit is the
/// strict case -- an Avalonia / MonoMac / MAUI window created from any other
/// thread fails with "IDispatcherImpl belongs to a different thread". Win32
/// message loops are per-thread and X11 does not care, so this only shows up on
/// macOS.
///
/// A main thread cannot be created after the fact, so instead the entry point
/// keeps it: it starts the worker and then pumps this queue (rather than just
/// joining the worker). Lisp code that needs thread 0 submits work with
/// DOTCL:CALL-ON-MAIN-THREAD, which blocks until the work returns -- so wrapping
/// a GUI event loop in it hands the main thread to the GUI for as long as the
/// application runs, which is what the toolkit wants.
///
/// With nothing submitted the behaviour is exactly the old one: the main thread
/// sits blocked until the worker finishes.
/// </summary>
public static class MainThread
{
    private static int _mainThreadId = -1;
    private static BlockingCollection<Action>? _queue;

    /// <summary>True on the thread the process started on (once Install ran).</summary>
    public static bool IsMainThread =>
        _mainThreadId == Environment.CurrentManagedThreadId;

    /// <summary>True when a pump is running (or about to), i.e. work can be submitted.</summary>
    public static bool IsAvailable => _queue != null;

    /// <summary>Claim the calling thread as the main thread and open the queue.
    /// Called from the entry point before the Lisp worker starts. A host that embeds
    /// the runtime and never calls this leaves CALL-ON-MAIN-THREAD running work in
    /// place.</summary>
    public static void Install()
    {
        _mainThreadId = Environment.CurrentManagedThreadId;
        _queue = new BlockingCollection<Action>();
    }

    /// <summary>Run submitted work until <see cref="Shutdown"/> and the queue drains.
    /// Called from the entry point in place of joining the worker.</summary>
    public static void Pump()
    {
        var q = _queue;
        if (q == null) return;
        foreach (var work in q.GetConsumingEnumerable())
        {
            // Submitters capture their own errors; this guard only keeps a stray
            // one from tearing down the pump and stranding the rest of the queue.
            try { work(); } catch { }
        }
    }

    /// <summary>Stop the pump once the queue drains. Called when the Lisp worker exits.</summary>
    public static void Shutdown()
    {
        try { _queue?.CompleteAdding(); }
        catch (ObjectDisposedException) { }
    }

    /// <summary>Run <paramref name="work"/> on the main thread and wait for it.
    /// Runs it in place when already on the main thread, or when no pump is
    /// installed (embedded host). Exceptions propagate to the caller.</summary>
    public static void Send(Action work)
    {
        var q = _queue;
        if (q == null || IsMainThread) { work(); return; }

        ExceptionDispatchInfo? error = null;
        var done = new ManualResetEventSlim();
        Action item = () =>
        {
            try { work(); }
            catch (Exception ex) { error = ExceptionDispatchInfo.Capture(ex); }
            finally { done.Set(); }
        };

        try { q.Add(item); }
        catch (InvalidOperationException)
        {
            // The pump has stopped accepting work (the Lisp worker returned and
            // the process is on its way out). Say so rather than running the work
            // on this thread, which is the very thing the caller asked to avoid.
            done.Dispose();
            throw new LispErrorException(new LispProgramError(
                "DOTCL:CALL-ON-MAIN-THREAD: the main thread is no longer accepting work"));
        }

        done.Wait();
        done.Dispose();
        error?.Throw();
    }

    // (dotcl:call-on-main-thread (lambda () ...)) -> the function's value
    public static LispObject CallOnMainThread(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTCL:CALL-ON-MAIN-THREAD: expected 1 argument (a function)"));

        // Already there (or nothing to marshal onto): run it in place, in the
        // caller's own dynamic environment -- handlers and restarts included.
        if (_queue == null || IsMainThread) return Runtime.Funcall(args[0]);

        LispObject result = Nil.Instance;
        try
        {
            Send(() => { result = FuncallAtBoundary(args[0]); });
        }
        catch (LispErrorException ex)
        {
            // The condition was signalled on the OTHER thread, so no handler on
            // this one ever saw it -- handler clusters are per-thread. Re-signal
            // it here, which is what gives the caller's HANDLER-CASE its turn.
            //
            // Rethrowing the exception object alone was enough for a compiled
            // HANDLER-CASE, whose generated code catches raw exceptions, but not
            // for an interpreted one: there HANDLER-CASE is a macro over
            // HANDLER-BIND and sees only conditions that went through SIGNAL. The
            // same call therefore answered differently on an emit-free build.
            //
            // If nothing handles it, SIGNAL returns and the original exception
            // continues on its way, unchanged.
            HandlerClusterStack.Signal(ex.Condition);
            throw;
        }
        return result;
    }

    /// <summary>Call a Lisp function on the main thread, converting a condition it
    /// does not handle itself into an exception the caller's thread can re-raise.
    /// Handler clusters are per-thread, so the caller's HANDLER-CASE is not in scope
    /// here: without this the ERROR function would find no handler and drop into the
    /// debugger on the main thread -- on stdin the caller is not reading.</summary>
    private static LispObject FuncallAtBoundary(LispObject fn)
    {
        var tag = new object();
        var handler = new LispFunction(
            hargs => throw new HandlerCaseInvocationException(
                tag, 0, hargs.Length > 0 ? hargs[0] : Nil.Instance),
            "%MAIN-THREAD-BOUNDARY", -1);
        HandlerClusterStack.PushCluster(
            new[] { new HandlerBinding(Startup.Sym("ERROR"), handler) });
        LispCondition? caught = null;
        try
        {
            return Runtime.Funcall(fn);
        }
        catch (HandlerCaseInvocationException ex) when (ReferenceEquals(ex.Tag, tag))
        {
            caught = ex.Condition as LispCondition
                     ?? new LispError(ex.Condition.ToString() ?? "error");
        }
        finally
        {
            HandlerClusterStack.PopCluster();
        }
        // Thrown after the finally: the cluster above is still live inside the catch,
        // so anything that re-enters the condition system there bounces back into it.
        throw new LispErrorException(caught);
    }

    // (dotcl:main-thread-p) -> T on the thread the process started on
    public static LispObject MainThreadP(LispObject[] args) =>
        IsMainThread ? Startup.Sym("T") : (LispObject)Nil.Instance;
}
