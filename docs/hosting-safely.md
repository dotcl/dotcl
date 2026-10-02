# Hosting dotcl safely

What to know before putting dotcl inside an application: what Lisp code can
reach, and how the runtime behaves when several threads call into it.

## dotcl is not a sandbox

Lisp code runs in your process with the same rights as your own code. It can
call any .NET API (`dotnet:invoke`, `dotnet:static`), read and write files,
open sockets, load assemblies and native libraries, and redefine functions,
including ones your host relies on.

There is no switch that limits this. Treat Lisp code the way you would treat a
C# plugin: run only code you trust. To run code you do not trust, put it in a
separate process with the least privilege the operating system gives you, or
in a container, and talk to it over a pipe or a socket.

`PrecompiledOnly` does not change this. It stops new code from being compiled
at run time, but code that is already compiled can still call any .NET API.

## One image per process

The runtime is static: there is one Lisp image per process. Every
`DotclHost` call, from any thread and any component, sees the same packages,
functions and global variables. A definition made by one part of the program
is visible to every other part, and two components that define the same
function name in the same package replace each other's definition. Give each
component its own package.

## Threads

Any thread may call into dotcl, and calls from different threads run at the
same time:

- `DotclHost.Initialize` and `EnsureCore` may be called from several threads
  at once; the runtime boots and loads the core exactly once.
- `Call` runs compiled code without taking any lock.
- `EvalString` and `LoadLispFile` compile what they read under one
  process-wide lock, because the compiler is not thread-safe, and release it
  while the compiled code runs. Two threads evaluating at once take turns
  compiling, not running.

What the threads share is the usual Common Lisp split:

- **Per thread:** dynamic bindings. A `let` of a special variable on one thread
  is not seen by another; the other thread sees the global value.
- **Shared:** global definitions and the global values of variables. A `setf`
  of a global variable, or a `defun`, is seen by every thread.
- **Your responsibility:** data that several threads change. Lists, arrays and
  objects are not locked for you. A hash table is safe to change from several
  threads only when it was made with `(make-hash-table :synchronized t)`.

### Redefining a function while it is called

A `defun` on one thread while other threads keep calling the function is safe:
nothing crashes, and every call that starts after the redefinition returns uses
the new definition. A call already in progress finishes the way it started for
its own recursive calls, and picks up the new definition the next time it calls
any other function by name.

### Stack depth

Lisp code runs on the stack of the thread that calls it, and dotcl does not
make that stack larger. A thread created with `new Thread(...)` and the default
stack size is enough for ordinary code, but not for deep recursion or deeply
nested macro expansion: on such a thread a recursion a few thousand calls deep,
or a macro that expands into itself about a hundred times, runs out. When it
does, the call fails with a Lisp `storage-condition` (a
`DotclConditionException` with the hook installed); the process does not die.

The command-line `dotcl`, and on 64-bit hosts every thread Lisp creates
itself, runs on a 256 MB stack. To give a host the same headroom, call Lisp from a thread with
that stack size:

```csharp
var t = new Thread(() => DotclHost.LoadLispFile("big-system.lisp"), 256 * 1024 * 1024);
t.Start();
t.Join();
```

### Lisp threads and host threads

A thread Lisp creates (`bordeaux-threads`' `make-thread`, or
`dotcl:make-thread`) is an ordinary .NET thread, with a 256 MB stack on 64-bit
hosts. A host thread that calls into Lisp is a Lisp thread too, as far as Lisp
is concerned: `bt:current-thread` returns an object for it, named after
`Thread.Name`, or `"main"` when the thread has no name. Do not find the main
thread by its name; give your threads names if Lisp code needs to tell them
apart.

## Untrusted input that is not code

Reading data with the Lisp reader evaluates `#.` forms, which run arbitrary
code. Bind `*read-eval*` to `nil` while reading anything that came from
outside; the reader then signals a `reader-error` on `#.` instead:

```lisp
(let ((*read-eval* nil))
  (read-from-string untrusted-text))
```

The same applies to `EvalString`: it reads before it evaluates, so text you
did not write must not reach it at all.
