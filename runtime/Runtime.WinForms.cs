using System.Runtime.ExceptionServices;

namespace DotCL;

/// <summary>
/// STA UI thread support for Windows Forms.
/// dotnet:ui-invoke runs a Lisp lambda on the STA thread and returns the result.
/// dotnet:ui-post   runs a Lisp lambda on the STA thread without waiting.
/// </summary>
internal static class DotNetWinForms
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
