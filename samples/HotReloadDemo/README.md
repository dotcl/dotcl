# HotReloadDemo — edit Lisp while the .NET host is running

A plain .NET console host embeds dotcl, loads `handlers.lisp`, and calls
`handle-request` once a second. A `FileSystemWatcher` re-loads the file on
every save, so the next call runs the new definition — no restart, no
connection drop.

## Run

```console
$ cd samples/HotReloadDemo
$ dotnet run
host running — edit handlers.lisp and save to hot-reload (Ctrl+C to quit)
  request 1 -> hello from Lisp, request #1
  request 2 -> hello from Lisp, request #2
```

Now open `handlers.lisp` in any editor, change the greeting, save:

```console
[reload] file changed: handlers.lisp loaded
  request 7 -> HELLO AGAIN, request #7
```

## Why this is easy here

Common Lisp has redefinition semantics built into the language: `load`-ing a
file that redefines a function atomically swaps the function cell, calls that
are already running finish on the old body, and the next call gets the new
one. So unlike .NET Hot Reload (`MetadataUpdateHandler`), there is no list of
forbidden edits — signatures, new functions, macros, and classes all just
work, because a re-load is ordinary program execution.

Two properties worth stealing for real apps:

- **A broken edit does not kill the host.** The load is wrapped in a
  try/catch; on a typo the host reports the error and keeps serving with the
  previous definitions.
- **Reload residue is tiny.** Re-loading a source file leaves on the order of
  a few hundred bytes behind, so an edit-save loop running all day is fine.
  (Re-loading compiled `.fasl` assemblies is different — loaded assemblies
  cannot be unloaded — so keep the hot path on `.lisp` source.)

## Getting values out of Lisp

`DotclHost.Call` returns a `LispObject`; its `ToString()` is the *printed*
representation (a Lisp string prints with quotes). Use
`DotclHost.ToClr<string>(...)` (or `ToClr<T>`) when you want the value.
