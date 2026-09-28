using System;
using System.IO;
using System.Runtime.InteropServices;

namespace DotCL;

/// <summary>
/// A write-only stream over a file handle that the OS itself keeps at end of
/// file: every Write lands after whatever any other writer (another stream in
/// this process, or another process) has appended, and the positioning and the
/// write are one step inside the kernel.
///
/// Why not FileStream: FileMode.Append only seeks to the end once, at open, on
/// every OS (.NET does not pass O_APPEND on Unix), and FileStream then writes at
/// the offset it tracks itself. Two appenders overwrite each other. Measured on
/// Windows and Linux, both in-process (two streams, interleaved writes: 100 of
/// 201 lines lost) and across two processes.
///
/// How: Windows opens with FILE_APPEND_DATA and no FILE_WRITE_DATA, which makes
/// the file system direct every write to end of file. Unix opens with O_APPEND.
/// Writes go straight to WriteFile / write(2) rather than through a FileStream
/// wrapping the handle, because FileStream issues positional writes (pwrite),
/// and what pwrite does on an O_APPEND descriptor differs between systems
/// (Linux appends and ignores the offset; POSIX does not require that).
/// The macOS branch below has not been run on a Mac.
///
/// Atomicity is per Write call. The StreamWriter above this stream buffers, so a
/// line written without FINISH-OUTPUT may still be split across two writes and
/// interleave with another writer's data, but no data is lost.
/// </summary>
internal sealed class AppendFileStream : Stream
{
    // Windows: the HANDLE. Unix: the file descriptor.
    private readonly AppendHandle _h;
    private readonly bool _windows;

    private AppendFileStream(AppendHandle h, bool windows) { _h = h; _windows = windows; }

    /// <summary>Open <paramref name="path"/> for atomic appending, creating it
    /// when <paramref name="create"/> is true. Returns null where this is not
    /// available (an OS other than Windows, Linux or macOS, a 32-bit Unix
    /// process, or a platform without P/Invoke); the caller then falls back to
    /// seek-to-end, which is correct for a single writer.</summary>
    public static Stream? TryOpen(string path, bool create)
    {
        try
        {
            if (Compat.IsBrowser()) return null;
            string full = Path.GetFullPath(path);
            if (Compat.IsWindows())
            {
                var h = CreateFileW(full, FILE_APPEND_DATA | SYNCHRONIZE | FILE_READ_ATTRIBUTES,
                                    FILE_SHARE_READ | FILE_SHARE_WRITE,
                                    IntPtr.Zero, create ? OPEN_ALWAYS : OPEN_EXISTING,
                                    FILE_ATTRIBUTE_NORMAL, IntPtr.Zero);
                if (h == INVALID_HANDLE_VALUE)
                    throw ErrorFor(Marshal.GetLastWin32Error(), full, windows: true);
                return new AppendFileStream(new AppendHandle(h, windows: true), windows: true);
            }
            if (!Environment.Is64BitProcess) return null; // off_t of lseek below is 64-bit
            int flags;
            if (Compat.IsLinux()) flags = 0x1 | 0x400 | 0x80000;          // O_WRONLY|O_APPEND|O_CLOEXEC
            else if (Compat.IsMacOS()) flags = 0x1 | 0x8 | 0x1000000;     // same, Darwin values
            else return null;
            // open(2) is variadic and its mode argument only matters with
            // O_CREAT. A fixed-arity P/Invoke passes it in the wrong place on
            // Apple arm64, so create the file managed-side (OpenOrCreate never
            // truncates, and two processes racing here both just open it) and
            // open without O_CREAT.
            if (create)
                new FileStream(full, FileMode.OpenOrCreate, FileAccess.Write, FileShare.ReadWrite).Dispose();
            int fd = open_(System.Text.Encoding.UTF8.GetBytes(full + "\0"), flags, 0);
            if (fd < 0)
                throw ErrorFor(Marshal.GetLastWin32Error(), full, windows: false);
            return new AppendFileStream(new AppendHandle((IntPtr)fd, windows: false), windows: false);
        }
        catch (DllNotFoundException) { return null; }
        catch (EntryPointNotFoundException) { return null; }
    }

    private static Exception ErrorFor(int err, string path, bool windows)
    {
        // Windows: ERROR_FILE_NOT_FOUND 2, ERROR_PATH_NOT_FOUND 3, ERROR_ACCESS_DENIED 5.
        // Unix: ENOENT 2, EACCES 13.
        if (err == 2 || (windows && err == 3))
            return new FileNotFoundException($"Could not find file '{path}'.", path);
        if ((windows && err == 5) || (!windows && err == 13))
            return new UnauthorizedAccessException($"Access to the path '{path}' is denied.");
        return new IOException(windows
            ? $"{new System.ComponentModel.Win32Exception(err).Message} : '{path}'"
            : $"open(2) failed with errno {err} : '{path}'");
    }

    public override bool CanRead => false;
    public override bool CanWrite => !_h.IsClosed;
    // Seekable only in the sense that the end can be asked for: see Seek.
    public override bool CanSeek => !_h.IsClosed;

    public override unsafe void Write(byte[] buffer, int offset, int count)
    {
        if (buffer == null) throw new ArgumentNullException(nameof(buffer));
        if (offset < 0 || count < 0 || offset + count > buffer.Length)
            throw new ArgumentOutOfRangeException(nameof(offset));
        if (_h.IsClosed) throw new ObjectDisposedException(nameof(AppendFileStream));
        fixed (byte* p = buffer)
        {
            byte* q = p + offset;
            while (count > 0)
            {
                int n;
                if (_windows)
                {
                    if (!WriteFile(_h.DangerousGetHandle(), q, count, out n, IntPtr.Zero))
                        throw new IOException(new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error()).Message);
                }
                else
                {
                    long r = (long)write_((int)_h.DangerousGetHandle(), q, (IntPtr)count);
                    if (r < 0)
                    {
                        int errno = Marshal.GetLastWin32Error();
                        if (errno == 4) continue; // EINTR
                        throw new IOException($"write(2) failed with errno {errno}");
                    }
                    n = (int)r;
                }
                q += n; count -= n;
            }
        }
    }

    /// <summary>The file's current size, which is where the next write lands.</summary>
    public override long Length
    {
        get
        {
            if (_h.IsClosed) throw new ObjectDisposedException(nameof(AppendFileStream));
            // Moving the file pointer is harmless here: the OS ignores it for
            // appending writes on both kinds of handle.
            if (_windows)
            {
                if (!SetFilePointerEx(_h.DangerousGetHandle(), 0, out long end, 2 /* FILE_END */))
                    throw new IOException(new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error()).Message);
                return end;
            }
            long e = lseek_((int)_h.DangerousGetHandle(), 0, 2 /* SEEK_END */);
            if (e < 0) throw new IOException($"lseek(2) failed with errno {Marshal.GetLastWin32Error()}");
            return e;
        }
    }

    /// <summary>Always end of file: every write goes there.</summary>
    public override long Position
    {
        get => Length;
        set => Seek(value, SeekOrigin.Begin);
    }

    /// <summary>Only a seek that lands on the current end succeeds, since that is
    /// the one position a write can go to. Anything else would promise that the
    /// next write lands there, which it would not.</summary>
    public override long Seek(long offset, SeekOrigin origin)
    {
        long end = Length;
        long target = origin switch
        {
            SeekOrigin.Begin => offset,
            SeekOrigin.Current => end + offset,
            _ => end + offset,
        };
        if (target != end)
            throw new NotSupportedException("An append-mode file stream can only be positioned at end of file.");
        return end;
    }

    public override void Flush() { }
    public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();
    public override void SetLength(long value) => throw new NotSupportedException();

    protected override void Dispose(bool disposing)
    {
        if (disposing) _h.Dispose();
        base.Dispose(disposing);
    }

    private sealed class AppendHandle : SafeHandle
    {
        private readonly bool _windows;
        public AppendHandle(IntPtr h, bool windows) : base(new IntPtr(-1), true) { _windows = windows; SetHandle(h); }
        public override bool IsInvalid => handle == new IntPtr(-1);
        protected override bool ReleaseHandle()
            => _windows ? CloseHandle(handle) : close_((int)handle) == 0;
    }

    private const uint FILE_APPEND_DATA = 0x4, FILE_READ_ATTRIBUTES = 0x80, SYNCHRONIZE = 0x100000;
    private const uint FILE_SHARE_READ = 1, FILE_SHARE_WRITE = 2;
    private const uint OPEN_EXISTING = 3, OPEN_ALWAYS = 4, FILE_ATTRIBUTE_NORMAL = 0x80;
    private static readonly IntPtr INVALID_HANDLE_VALUE = new IntPtr(-1);

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    private static extern IntPtr CreateFileW(string name, uint access, uint share, IntPtr sa,
                                             uint disposition, uint flags, IntPtr template);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern unsafe bool WriteFile(IntPtr h, byte* buf, int n, out int written, IntPtr overlapped);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool SetFilePointerEx(IntPtr h, long distance, out long newPointer, uint method);
    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern bool CloseHandle(IntPtr h);

    [DllImport("libc", SetLastError = true, EntryPoint = "open")]
    private static extern int open_(byte[] path, int flags, int mode); // NUL-terminated UTF-8
    [DllImport("libc", SetLastError = true, EntryPoint = "write")]
    private static extern unsafe IntPtr write_(int fd, byte* buf, IntPtr n);
    [DllImport("libc", SetLastError = true, EntryPoint = "lseek")]
    private static extern long lseek_(int fd, long offset, int whence);
    [DllImport("libc", SetLastError = true, EntryPoint = "close")]
    private static extern int close_(int fd);
}
