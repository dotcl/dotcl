# clrmd

Walk the managed heap of the running process from Lisp, through
Microsoft.Diagnostics.Runtime (ClrMD, the engine inside WinDbg SOS and
dotnet-dump). It answers questions evaluating forms cannot: which live instances
of a type exist right now (`clrmd:instances-of`, `count-of`), what the heap is
made of (`heap-report`, `heap-histogram`), and a few dotcl-specific counts
(`symbols-by-package`, `functions-by-name`, `list-stats`). The walk is over a
snapshot of the current process, so it never sees a half-mutated heap.

    (require "dotcl-clrmd")

Depends on the `nuget` contrib: the ClrMD package is resolved on first use, at
the version in `clrmd:*package-version*` (`"*"`, latest stable, by default --
bind it to pin).
