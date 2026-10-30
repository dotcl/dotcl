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

Two exception filters are offered. `uncaught` (on by default) stops where a
condition would enter the debugger. `error` stops where an error is signaled,
before any handler runs, so an error that a `handler-case` would take stops
too; `continue` lets the handlers run. With both on, an error nobody handles
stops twice, at the signal and then at the debugger. `error` works through
`*break-on-signals*` and `dotcl:*break-on-signals-hook*` (when the hook is a
function, it is called with the condition instead of `break`).

Requests answered: `initialize`, `attach`, `configurationDone`,
`setBreakpoints`, `setFunctionBreakpoints`, `setExceptionBreakpoints`,
`exceptionInfo`, `threads`, `stackTrace`, `scopes`, `variables`, `evaluate`,
`continue`, `next`, `stepIn`, `stepOut`, `disconnect`. Breakpoint names are read in `CL-USER`.

A line breakpoint is placed inside a `defun` or `defmethod` of a file that has been loaded:
the definition is read from the file again and compiled with a stop in front of
the outermost form that starts on the line (through
`dotcl:*emit-form-prefix*`), and with `dotcl:*emit-frame-locals*` true, so the
stopped frame shows its local variables. Clearing the breakpoints of a file
puts the earlier definitions back. For a `defmethod` the method it defines
replaces the one the file defined, and the other methods of the generic
function are left alone. A line with no form on it, a line outside any `defun`
or `defmethod` (`defmacro` and top-level forms included), and a function or
method that is not defined yet are reported as not verified with a message; so
is every line on a build without an emitter.

While line breakpoints exist, `load` and `compile-file` are wrapped. After one
returns, the breakpoints of every file whose definitions were replaced, or that
were waiting for a definition, are placed again, and a `breakpoint` event
(reason `changed`) reports the lines whose verdict changed. Loading from C#
code without going through `load` is not noticed.

Every line of a `defun` compiled for a line breakpoint also gets a step point.
`next` stops the thread at the next step point no deeper than where it
stopped, `stepIn` at the next step point anywhere, and `stepOut` at the next
one less deep. Only those `defun`s have step points, so a step into or out to
other code runs on until it reaches one (or a breakpoint).

Function and line breakpoints take a `condition` (a form evaluated with the
frame's variables bound; the thread stops where it is true), a `hitCondition`
(`N` or `=N`, `>N`, `>=N`, `<N`, `<=N`, `%N`, counted over the hits whose
condition holds) and a `logMessage` (a logpoint: `{form}` is replaced by the
form's value, `{{` and `}}` are braces; the message goes to the console as an
`output` event and the thread does not stop). A condition that signals an
error stops the thread and says so in the console.

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

Any client that connects to the TCP port can use it. VS Code needs an
extension that declares a debugger type (`contributes.debuggers`) and answers
for it with a `DebugAdapterServer` on the port; `debugServer` in launch.json is
read only for a type that is already declared. With such an extension (type
`dotcl` here) the configuration is

    {
      "type": "dotcl", "request": "attach", "name": "Attach to dotcl",
      "host": "127.0.0.1", "port": 4711
    }

One client at a time. Depends on the `dotcl-thread` and `dotcl-socket`
contribs.

## How to tell it is connected

The server prints one line per event to the stream that was
`*standard-output*` when `start-server` was called (the REPL):

    dotcl-dap: client connected from 127.0.0.1:52114
    dotcl-dap: attached (Visual Studio Code)
    dotcl-dap: thread "demo-worker" stopped at FACT (function breakpoint)
    dotcl-dap: client disconnected

Requests are not logged. Set `dotcl-dap:*log-stream*` to `nil` to silence the
lines, or to a stream to send them there.

In VS Code, after Attach:

- The Debug Console shows `Attached to dotcl 0.1.35 (pid 12345)`.
- The debug toolbar (continue, step, stop) appears and the status bar turns orange.
- Under CALL STACK every live Lisp thread is listed by name: the thread that
  called `start-server` (the REPL) first, then the threads Lisp made, the
  server's own `dap-server`, and the stopped ones. (VS Code draws no row for
  a session's only thread while it runs, so without `dap-server` the REPL
  would show only as "Running" in the CALL STACK header.) A thread stopped at a
  breakpoint shows its frames; the others show as running, and asking for their
  stack gives an empty one. Threads that start or end later are announced with
  `thread` events (checked every quarter second).
- If the Call Stack is empty although the toolbar is there, the server is
  older than this list: restart the REPL and `(require "dotcl-dap")` again.
