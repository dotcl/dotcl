#!/usr/bin/env python3
"""Integration test: Ctrl-C (SIGINT) interrupts the dotcl REPL instead of killing it.

On Unix the REPL reads raw fd 0 and avoids .NET's Unix console driver, so
Console.CancelKeyPress never fires; SIGINT is instead delivered through a
PosixSignalRegistration. This test drives a real pty, starts an infinite
call-heavy loop, sends SIGINT to the process, and asserts the REPL survives
(the debugger's ABORT restart returns to top level and further forms evaluate).

Not part of `make test-regression` — signal wiring cannot be exercised from the
Lisp test harness, so run this manually:

    dotnet build runtime/runtime.csproj -c Release -f net10.0
    python3 test/sigint-repl.py

Exit status 0 on pass, 1 on failure.
"""
import os, pty, signal, time, sys, select

DLL = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                   "..", "runtime", "bin", "Release", "net10.0", "runtime.dll")

master, slave = pty.openpty()
pid = os.fork()
if pid == 0:
    os.setsid()
    os.dup2(slave, 0); os.dup2(slave, 1); os.dup2(slave, 2)
    os.close(master); os.close(slave)
    try:
        import fcntl, termios
        fcntl.ioctl(0, termios.TIOCSCTTY, 0)
    except Exception:
        pass
    env = dict(os.environ); env["DOTNET_GCConserveMemory"] = "7"
    # --no-readline: plain line reader so the test can feed input over the pty
    # without driving the raw ReadKey line editor.
    os.execvpe("dotnet", ["dotnet", DLL, "--no-readline"], env)
    os._exit(127)

os.close(slave)
buf = bytearray()

def drain(timeout):
    end = time.time() + timeout
    while time.time() < end:
        r, _, _ = select.select([master], [], [], 0.2)
        if r:
            try:
                data = os.read(master, 4096)
            except OSError:
                break
            if not data:
                break
            buf.extend(data)
    return bytes(buf)

drain(8.0)                                    # REPL banner
os.write(master, b"(loop for i from 0 do (identity i))\n")
time.sleep(2.0)                               # let the loop spin
mark = len(buf)
os.kill(pid, signal.SIGINT)                   # the actual Ctrl-C
drain(3.0)
os.write(master, b"(+ 40 2)\n")               # REPL still alive?
drain(3.0)
os.write(master, b"\x04")                     # Ctrl-D to exit
time.sleep(0.5)

after = bytes(buf).decode(errors="replace")[mark:]
interrupted = ("INTERACTIVE-INTERRUPT" in after) or ("interrupt" in after.lower())
alive = "42" in after
print(after)
print("RESULT interrupted=%s repl_alive_after=%s" % (interrupted, alive))
try:
    os.kill(pid, signal.SIGKILL)
except Exception:
    pass
sys.exit(0 if (interrupted and alive) else 1)
