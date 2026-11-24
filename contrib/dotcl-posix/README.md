# dotcl-posix

A thin POSIX layer for Linux and macOS: pseudo-terminals and signals, in the
shape of Python's `pty`, `os` and `signal`. Windows is not supported (loading
it there signals an error that says so; ConPTY is not covered).

    (require "dotcl-posix")
    (dotcl-posix:with-pty-process (p '("/bin/sh" "-c" "tty; sleep 1"))
      (dotcl-posix:pty-expect p "/dev/" :timeout 5)
      (dotcl-posix:kill (dotcl-posix:pty-process-pid p) :term)
      (dotcl-posix:pty-wait p))
    ;; => T, NIL, 15   (exited, no exit code, ended by signal 15)

- `spawn-in-pty argv &key environment directory` starts a program (the first
  element of ARGV is looked up on PATH) in a new session whose controlling
  terminal is a new pseudo-terminal, and returns a `pty-process`.
  `with-pty-process` does that and kills, reaps and closes at the end.
- `pty-write`, `pty-read` (`""` when nothing came in time, `:eof` when the child
  closed its side), `pty-expect` (read until a text appears) and `pty-drain`
  (read for a while). What was read is kept in `pty-process-text`.
- `kill pid signal` (a number or `:int`, `:term`, `:kill`, ...), `wait-pid`,
  `pty-wait` (the exit code, or the signal that ended the child).
- Errors are `posix-error`, with the libc function and the errno.

There is no fork: a .NET process is multi-threaded, so the child is started with
`posix_spawn`, with `POSIX_SPAWN_SETSID` and the slave side opened as its standard
input. Everything is a call into libc through `dotnet:%ffi-call`; the flag values
and signal numbers that differ between Linux and macOS are in one table at the top
of `dotcl-posix.lisp`.

- `pty-window-size process` returns the rows and columns of the child's terminal, and
  `(setf (pty-window-size process) '(rows columns))` resizes it (the child gets SIGWINCH).
  `spawn-in-pty` takes `:window-size '(rows columns)` for the size it starts with.

Not here yet: termios (raw mode) and fcntl.
