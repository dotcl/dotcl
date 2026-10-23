# dotcl-dap

A Debug Adapter Protocol server inside the running process, so a DAP client
(VS Code, or any other) can attach to a live dotcl.

    (require "dotcl-dap")
    (dotcl-dap:start-server :port 4711)   ; :port 0 picks a free one
    (dotcl-dap:server-port dotcl-dap:*server*)

A function breakpoint stops only the thread that calls the function, before
the function runs. With the `uncaught` exception filter on (the default), a
condition that would enter the debugger stops the thread it was signaled on. The REPL and the other threads keep running. While the
thread is stopped the client can read its stack and the frames' variables,
and `evaluate` runs on that thread with the frame's variables bound, so
special bindings and the thread's own state are the ones in effect there.
`continue` lets it go on.

Requests answered: `initialize`, `attach`, `configurationDone`,
`setBreakpoints`, `setFunctionBreakpoints`, `setExceptionBreakpoints`,
`exceptionInfo`, `threads`, `stackTrace`, `scopes`, `variables`, `evaluate`,
`continue`, `next`, `stepIn`, `stepOut`, `disconnect`. Breakpoint names are read in `CL-USER`.

A line breakpoint is placed inside a `defun` of a file that has been loaded:
the `defun` is read from the file again and compiled with a stop in front of
the outermost form that starts on the line (through
`dotcl:*emit-form-prefix*`), and with `dotcl:*emit-frame-locals*` true, so the
stopped frame shows its local variables. Clearing the breakpoints of a file
puts the earlier definitions back. A line with no form on it, a line outside
any `defun`, and a function that is not defined yet are reported as not
verified with a message; so is every line on a build without an emitter. A
`defun` loaded again while it has line breakpoints loses them until they are
set again.

Every line of a `defun` compiled for a line breakpoint also gets a step point.
`next` stops the thread at the next step point no deeper than where it
stopped, `stepIn` at the next step point anywhere, and `stepOut` at the next
one less deep. Only those `defun`s have step points, so a step into or out to
other code runs on until it reaches one (or a breakpoint).

A thread stopped on a condition shows a `Condition` and a `Restarts` scope.
It is resumed through one of its restarts in two ways:

- `evaluate` of `:restarts` lists them, and `:restart N expr...` invokes
  restart N with the values of the expressions (evaluated on that thread).
  This works from any client's debug console.
- the custom requests `dotcl/restarts` (`threadId`) and
  `dotcl/invokeRestart` (`threadId`, `index`, and optionally `arguments`, a
  list of expression strings, or `argumentText`, one string of expressions).

`continue` invokes the `CONTINUE` restart when there is one; otherwise the
server declines the condition, and what would have happened without it
happens (the previous `*debugger-hook*`, or the debugger).

The breakpoint replaces the function's global definition with a wrapper, and
clearing it puts the newest definition back. While it is set, the name is
proclaimed `NOTINLINE` and a new definition of it is wrapped as well, so a
definition made then also stops on the calls it makes to itself. A function
that already calls itself is compiled again from its lambda expression when
one is known; otherwise the breakpoint is set with a message saying that its
calls to itself do not stop until it is defined again. The variables of frames other than the stopped function's own are
shown when their code was compiled with `dotcl:*emit-frame-locals*` true.

From VS Code, an `attach` configuration with `debugServer` set to the port
connects to a running server:

    {
      "type": "dotcl", "request": "attach", "name": "Attach to dotcl",
      "debugServer": 4711
    }

One client at a time. Depends on the `dotcl-thread` and `dotcl-socket`
contribs.
