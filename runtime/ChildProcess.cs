using System;
using System.Runtime.InteropServices;

namespace DotCL;

/// <summary>A child started by LAUNCH-PROCESS: either a System.Diagnostics.Process,
/// or on Unix a child spawned with posix_spawn into a process group of its own.</summary>
public abstract class ChildProcess
{
    public abstract int Id { get; }
    public abstract bool HasExited { get; }
    public abstract int ExitCode { get; }
    public abstract void WaitForExit();
    /// <summary>Kill the child and its descendants.</summary>
    public abstract void Kill();
    public abstract System.IO.StreamWriter StandardInput { get; }
    public abstract System.IO.StreamReader StandardOutput { get; }
    public abstract System.IO.StreamReader StandardError { get; }
}

internal sealed class DotnetChildProcess : ChildProcess
{
    private readonly System.Diagnostics.Process _p;
    public DotnetChildProcess(System.Diagnostics.Process p) { _p = p; }
    public override int Id => _p.Id;
    public override bool HasExited => _p.HasExited;
    public override int ExitCode => _p.ExitCode;
    public override void WaitForExit() => _p.WaitForExit();
    public override void Kill()
    {
#if NETSTANDARD2_0
        _p.Kill();
#else
        _p.Kill(entireProcessTree: true);
#endif
    }
    public override System.IO.StreamWriter StandardInput => _p.StandardInput;
    public override System.IO.StreamReader StandardOutput => _p.StandardOutput;
    public override System.IO.StreamReader StandardError => _p.StandardError;
}

/// <summary>Unix: a child started with posix_spawn and POSIX_SPAWN_SETPGROUP, so it
/// leads a new process group (Process.Start has no way to do this). Streams,
/// argv[0], the environment and the exit code follow what Process.Start gives.</summary>
internal sealed unsafe class SpawnedChildProcess : ChildProcess
{
    private readonly int _pid;
    private readonly System.Threading.ManualResetEventSlim _exited = new(false);
    private int _exitCode;
    private readonly System.IO.StreamWriter? _stdin;
    private readonly System.IO.StreamReader? _stdout, _stderr;

    private SpawnedChildProcess(int pid, System.IO.StreamWriter? stdin,
                                System.IO.StreamReader? stdout, System.IO.StreamReader? stderr)
    {
        _pid = pid;
        _stdin = stdin;
        _stdout = stdout;
        _stderr = stderr;
        var reaper = new System.Threading.Thread(Reap) { IsBackground = true, Name = "dotcl-waitpid-" + pid };
        reaper.Start();
    }

    private void Reap()
    {
        int status = 0, r;
        do { r = waitpid(_pid, &status, 0); }
        while (r == -1 && Marshal.GetLastWin32Error() == EINTR);
        int sig = status & 0x7f;
        _exitCode = r == -1 ? -1 : sig == 0 ? (status >> 8) & 0xff : 128 + sig;
        _exited.Set();
    }

    public override int Id => _pid;
    public override bool HasExited => _exited.IsSet;
    public override int ExitCode => _exited.IsSet ? _exitCode
        : throw new InvalidOperationException("Process must exit before requested information can be determined.");
    public override void WaitForExit() => _exited.Wait();
    public override void Kill()
    {
        if (_exited.IsSet) return;
#if NETSTANDARD2_0
        kill(_pid, SIGKILL);
#else
        try { System.Diagnostics.Process.GetProcessById(_pid).Kill(entireProcessTree: true); }
        catch (Exception) { kill(_pid, SIGKILL); }
#endif
    }
    public override System.IO.StreamWriter StandardInput =>
        _stdin ?? throw new InvalidOperationException("StandardIn has not been redirected.");
    public override System.IO.StreamReader StandardOutput =>
        _stdout ?? throw new InvalidOperationException("StandardOut has not been redirected.");
    public override System.IO.StreamReader StandardError =>
        _stderr ?? throw new InvalidOperationException("StandardError has not been redirected.");

    /// <summary>Spawn PATH (already resolved) with ARGV, the redirections, working
    /// directory, environment and encodings of PSI. Returns null where the posix_spawn
    /// functions needed are not available; a failure to start throws as
    /// Process.Start does.</summary>
    public static ChildProcess? TryStart(string path, System.Collections.Generic.IList<string> argv,
                                         System.Diagnostics.ProcessStartInfo psi)
    {
        if (!RuntimeInformation.IsOSPlatform(OSPlatform.Linux) && !RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            return null;
        try { return Start(path, argv, psi); }
        catch (EntryPointNotFoundException) { return null; }
        catch (DllNotFoundException) { return null; }
    }

    private static ChildProcess Start(string path, System.Collections.Generic.IList<string> argv,
                                      System.Diagnostics.ProcessStartInfo psi)
    {
        var env = new System.Collections.Generic.List<string>();
        foreach (var kv in psi.Environment)
            if (kv.Value != null) env.Add(kv.Key + "=" + kv.Value);

        var fa = Marshal.AllocHGlobal(1024);
        var attr = Marshal.AllocHGlobal(1024);
        var sigs = Marshal.AllocHGlobal(256);
        var strings = new System.Collections.Generic.List<IntPtr>();
        int[] inP = { -1, -1 }, outP = { -1, -1 }, errP = { -1, -1 };
        bool faInit = false, attrInit = false, ok = false;
        try
        {
            if (psi.RedirectStandardInput) MakePipe(inP);
            if (psi.RedirectStandardOutput) MakePipe(outP);
            if (psi.RedirectStandardError) MakePipe(errP);

            Check(posix_spawn_file_actions_init(fa)); faInit = true;
            if (inP[0] >= 0) Check(posix_spawn_file_actions_adddup2(fa, inP[0], 0));
            if (outP[1] >= 0) Check(posix_spawn_file_actions_adddup2(fa, outP[1], 1));
            if (errP[1] >= 0) Check(posix_spawn_file_actions_adddup2(fa, errP[1], 2));
            if (!string.IsNullOrEmpty(psi.WorkingDirectory))
                Check(posix_spawn_file_actions_addchdir_np(fa, Utf8z(psi.WorkingDirectory)));

            Check(posix_spawnattr_init(attr)); attrInit = true;
            Check(posix_spawnattr_setpgroup(attr, 0));
            sigemptyset(sigs);
            Check(posix_spawnattr_setsigmask(attr, sigs));
            // .NET ignores SIGPIPE; Process.Start gives the child the default.
            sigaddset(sigs, SIGPIPE);
            Check(posix_spawnattr_setsigdefault(attr, sigs));
            Check(posix_spawnattr_setflags(attr,
                (short)(POSIX_SPAWN_SETPGROUP | POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSIGMASK)));

            var argvBlock = StringArray(argv, strings);
            var envBlock = StringArray(env, strings);
            int pid;
            int err;
            fixed (IntPtr* a = argvBlock)
            fixed (IntPtr* e = envBlock)
                err = posix_spawn(&pid, Utf8z(path), fa, attr, a, e);
            if (err != 0)
                throw new System.ComponentModel.Win32Exception(err,
                    $"An error occurred trying to start process '{psi.FileName}' with working directory '"
                    + (string.IsNullOrEmpty(psi.WorkingDirectory) ? System.IO.Directory.GetCurrentDirectory() : psi.WorkingDirectory)
                    + "'. " + new System.ComponentModel.Win32Exception(err).Message);

            CloseFd(ref inP[0]); CloseFd(ref outP[1]); CloseFd(ref errP[1]);
            var utf8 = new System.Text.UTF8Encoding(false);
            System.IO.StreamWriter? stdin = inP[1] < 0 ? null
                : new System.IO.StreamWriter(PipeStream(inP[1], System.IO.FileAccess.Write),
                                             utf8, 4096) { AutoFlush = true };
            System.IO.StreamReader? stdout = outP[0] < 0 ? null
                : new System.IO.StreamReader(PipeStream(outP[0], System.IO.FileAccess.Read),
                                             psi.StandardOutputEncoding ?? utf8, true, 4096);
            System.IO.StreamReader? stderr = errP[0] < 0 ? null
                : new System.IO.StreamReader(PipeStream(errP[0], System.IO.FileAccess.Read),
                                             psi.StandardErrorEncoding ?? utf8, true, 4096);
            ok = true;
            return new SpawnedChildProcess(pid, stdin, stdout, stderr);
        }
        finally
        {
            if (faInit) posix_spawn_file_actions_destroy(fa);
            if (attrInit) posix_spawnattr_destroy(attr);
            Marshal.FreeHGlobal(fa);
            Marshal.FreeHGlobal(attr);
            Marshal.FreeHGlobal(sigs);
            foreach (var s in strings) Marshal.FreeHGlobal(s);
            CloseFd(ref inP[0]); CloseFd(ref outP[1]); CloseFd(ref errP[1]);
            if (!ok) { CloseFd(ref inP[1]); CloseFd(ref outP[0]); CloseFd(ref errP[0]); }
        }
    }

    private static System.IO.FileStream PipeStream(int fd, System.IO.FileAccess access)
        => new(new Microsoft.Win32.SafeHandles.SafeFileHandle((IntPtr)fd, true), access, 1);

    private static void MakePipe(int[] fds)
    {
        int r;
        bool linux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux);
        fixed (int* p = fds)
            r = linux ? pipe2(p, O_CLOEXEC_LINUX) : pipe(p);
        if (r != 0) throw new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error());
        // fcntl is variadic, which P/Invoke cannot call correctly on Apple arm64.
        if (!linux && (ioctl(fds[0], FIOCLEX_DARWIN) != 0 || ioctl(fds[1], FIOCLEX_DARWIN) != 0))
        {
            int err = Marshal.GetLastWin32Error();
            close(fds[0]); close(fds[1]);
            fds[0] = fds[1] = -1;
            throw new System.ComponentModel.Win32Exception(err);
        }
    }

    private static void CloseFd(ref int fd)
    {
        if (fd >= 0) { close(fd); fd = -1; }
    }

    private static void Check(int err)
    {
        if (err != 0) throw new System.ComponentModel.Win32Exception(err);
    }

    private static byte[] Utf8z(string s)
    {
        var n = System.Text.Encoding.UTF8.GetByteCount(s);
        var b = new byte[n + 1];
        System.Text.Encoding.UTF8.GetBytes(s, 0, s.Length, b, 0);
        return b;
    }

    private static IntPtr[] StringArray(System.Collections.Generic.IList<string> items,
                                        System.Collections.Generic.List<IntPtr> owned)
    {
        var arr = new IntPtr[items.Count + 1];
        for (int i = 0; i < items.Count; i++)
        {
            var b = Utf8z(items[i]);
            var p = Marshal.AllocHGlobal(b.Length);
            owned.Add(p);
            Marshal.Copy(b, 0, p, b.Length);
            arr[i] = p;
        }
        return arr;
    }

    private const int EINTR = 4, SIGKILL = 9, SIGPIPE = 13;
    private const int O_CLOEXEC_LINUX = 0x80000;
    private const ulong FIOCLEX_DARWIN = 0x20006601;
    private const int POSIX_SPAWN_SETPGROUP = 0x02, POSIX_SPAWN_SETSIGDEF = 0x04, POSIX_SPAWN_SETSIGMASK = 0x08;

    [DllImport("libc", SetLastError = true)] private static extern int pipe(int* fds);
    [DllImport("libc", SetLastError = true)] private static extern int pipe2(int* fds, int flags);
    [DllImport("libc", SetLastError = true)] private static extern int ioctl(int fd, ulong request);
    [DllImport("libc", SetLastError = true)] private static extern int close(int fd);
    [DllImport("libc", SetLastError = true)] private static extern int kill(int pid, int sig);
    [DllImport("libc", SetLastError = true)] private static extern int waitpid(int pid, int* status, int options);
    [DllImport("libc")] private static extern int sigemptyset(IntPtr set);
    [DllImport("libc")] private static extern int sigaddset(IntPtr set, int sig);
    [DllImport("libc")] private static extern int posix_spawn_file_actions_init(IntPtr fa);
    [DllImport("libc")] private static extern int posix_spawn_file_actions_destroy(IntPtr fa);
    [DllImport("libc")] private static extern int posix_spawn_file_actions_adddup2(IntPtr fa, int fd, int newfd);
    [DllImport("libc")] private static extern int posix_spawn_file_actions_addchdir_np(IntPtr fa, byte[] path);
    [DllImport("libc")] private static extern int posix_spawnattr_init(IntPtr attr);
    [DllImport("libc")] private static extern int posix_spawnattr_destroy(IntPtr attr);
    [DllImport("libc")] private static extern int posix_spawnattr_setflags(IntPtr attr, short flags);
    [DllImport("libc")] private static extern int posix_spawnattr_setpgroup(IntPtr attr, int pgroup);
    [DllImport("libc")] private static extern int posix_spawnattr_setsigmask(IntPtr attr, IntPtr set);
    [DllImport("libc")] private static extern int posix_spawnattr_setsigdefault(IntPtr attr, IntPtr set);
    [DllImport("libc")] private static extern int posix_spawn(int* pid, byte[] path, IntPtr fa, IntPtr attr,
                                                            IntPtr* argv, IntPtr* envp);
}
