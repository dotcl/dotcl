using System.Runtime.ExceptionServices;

namespace DotCL;

/// <summary>
/// A single-threaded-apartment thread with a message loop, and the bridge onto it.
/// dotnet:ui-invoke runs a Lisp lambda on the STA thread and returns the result.
/// dotnet:ui-post   runs a Lisp lambda on the STA thread without waiting.
///
/// A window belongs to the thread that created it and only that thread may pump its
/// messages, so UI work cannot run on the REPL's thread — hence the dedicated STA
/// thread and the marshalling. Apartments are a COM concept, which is why this is
/// Windows-only: the dependency is Thread.SetApartmentState, not any UI framework.
/// System.Windows.Forms is never referenced here; the caller loads it (or WPF, or
/// anything else needing an apartment) and it is looked up reflectively.
/// </summary>
internal static class DotNetSta
{
    private static Thread? _uiThread;
    private static SynchronizationContext? _uiContext;

    // (dotnet:ui-invoke (lambda () (dotnet:invoke form "Show") form))
    public static LispObject UiInvoke(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:UI-INVOKE: expected 1 argument (a function)"));
        EnsureUiThread();

        LispObject? result = null;
        ExceptionDispatchInfo? error = null;
        var done = new ManualResetEventSlim();

        _uiContext!.Send(_ =>
        {
            try   { result = Runtime.Funcall(args[0]); }
            catch (Exception ex) { error = ExceptionDispatchInfo.Capture(ex); }
            finally { done.Set(); }
        }, null);

        done.Wait();
        error?.Throw();
        return result ?? Nil.Instance;
    }

    // (dotnet:ui-post (lambda () (dotnet:invoke form "Show")))
    public static LispObject UiPost(LispObject[] args)
    {
        if (args.Length != 1)
            throw new LispErrorException(new LispProgramError(
                "DOTNET:UI-POST: expected 1 argument (a function)"));
        EnsureUiThread();

        _uiContext!.Post(_ =>
        {
            try { Runtime.Funcall(args[0]); }
            catch { /* fire-and-forget: swallow errors */ }
        }, null);

        return Nil.Instance;
    }

    private static void EnsureUiThread()
    {
        if (_uiThread != null && _uiThread.IsAlive) return;

        // System.Windows.Forms must already be loaded.
        // Type.GetType won't find Assembly.LoadFrom assemblies, so search AppDomain.
        static Type? FindType(string name) =>
            AppDomain.CurrentDomain.GetAssemblies()
                .Select(a => a.GetType(name))
                .FirstOrDefault(t => t != null);

        var appType = FindType("System.Windows.Forms.Application")
            ?? throw new LispErrorException(new LispProgramError(
                "DOTNET:UI-INVOKE: System.Windows.Forms not loaded — call " +
                "(dotnet:load-assembly \"System.Windows.Forms\") first"));

        var ctxType = FindType("System.Windows.Forms.WindowsFormsSynchronizationContext")!;

        var ready = new ManualResetEventSlim();

        _uiThread = new Thread(() =>
        {
            appType.GetMethod("EnableVisualStyles")!.Invoke(null, null);
            appType.GetMethod("SetCompatibleTextRenderingDefault",
                              new[] { typeof(bool) })!.Invoke(null, new object[] { false });

            var ctx = (SynchronizationContext)Activator.CreateInstance(ctxType)!;
            SynchronizationContext.SetSynchronizationContext(ctx);
            _uiContext = ctx;
            ready.Set();

            // Run message loop until Application.Exit() is called
            appType.GetMethod("Run", Type.EmptyTypes)!.Invoke(null, null);
        });

        _uiThread.SetApartmentState(ApartmentState.STA);
        _uiThread.IsBackground = true;
        _uiThread.Name = "dotcl-ui";
        _uiThread.Start();

        if (!ready.Wait(TimeSpan.FromSeconds(10)))
            throw new LispErrorException(new LispProgramError(
                "DOTNET:UI-INVOKE: UI thread failed to start within 10 seconds"));
    }
}
