# Ahead-of-time compiled FASLs (ReadyToRun)

A dotcl FASL is a .NET assembly, and a .NET assembly is IL that the JIT turns
into native code the first time each method runs. ReadyToRun (R2R) does that
compilation ahead of time and stores the result in the assembly, so loading it
costs much less JIT.

dotcl applies this to FASLs through a **sibling file**: next to `foo.fasl` there
may be a `foo.fasl.r2r-<rid>`, holding the same module already compiled for that
platform. `load` prefers the sibling when it finds one, and falls back to the
plain FASL otherwise.

It is a sibling rather than a replacement so that the `.fasl` stays the artifact
ASDF compares against its source: nothing has to be told that the file it
planned around was swapped. A sibling older than its FASL is ignored, so a
rebuilt FASL is never silently shadowed by native code from the previous
version. The marker goes after the whole filename, not into the stem, so a
sibling does not match a `*.fasl` glob.

## When it helps

At **load time**, and in proportion to how much code is being loaded. Loading
Coalton -- the largest system regularly built on dotcl -- takes 33 seconds
without the siblings and 10 with them.

It does nothing for code that is compiled in the running image (`eval`, `load`
of a `.lisp`, the REPL): there is no FASL to have a sibling. So the shape it
pays off in is "start a process, load a lot of already-compiled code, do some
work" -- a command-line tool, a test run, an editor connecting to a fresh image.

## What already has siblings

The packages dotcl ships carry ahead-of-time builds of the runtime core, ASDF
and the bundled contribs, for `win-x64`, `win-arm64`, `linux-x64`,
`linux-arm64`, `osx-x64` and `osx-arm64`. Each per-platform package carries only
its own platform's set. You do not have to do anything to get these.

## Writing siblings for your own code

Off by default. Set `dotcl:*compile-r2r*` to a true value and ASDF writes a
sibling for each FASL it compiles:

```lisp
(setf dotcl:*compile-r2r* t)
(asdf:load-system "my-app")
```

It is a special variable rather than a build setting because whether the trade
is worth taking depends on how the file is going to be used, which the image
knows and the build does not. It is read at the moment ASDF compiles, so it can
also be bound around a single operation. `DOTCL_R2R_AFTER_COMPILE=1` in the
environment picks the initial value, for a `make` or CI invocation with no Lisp
to run first.

For a FASL you already have, `dotcl:write-r2r-sibling` writes one directly and
answers `t` when a sibling ended up on disk:

```lisp
(dotcl:write-r2r-sibling "contrib/dotcl-gray/dotcl-gray.fasl")
```

The trade: writing a sibling costs a run of `crossgen2` per file and several
times the disk, for roughly a tenth more time on the compile. Reading one is
what removes the JIT at load.

### crossgen2

Writing siblings needs `crossgen2`, which is not part of the .NET SDK proper --
it is a NuGet package the SDK restores the first time something asks for
ReadyToRun. If it is missing, `*compile-r2r*` cannot do anything, and dotcl says
so once on standard error with the command that restores it:

```
dotnet publish -p:PublishReadyToRun=true -r <rid>
```

Everything else about this path is silent by design: a FASL without a sibling is
complete, and loading falls back to it.

## Checking that it is being used

Every way this path has broken looks like success from outside -- the load
works, the answers are right, it is just slow. `dotcl:r2r-stats` makes it
visible. It answers a cons of how many FASLs were loaded through a sibling and
how many were loaded at all, counted since the image started:

```lisp
(dotcl:r2r-stats)
;; => (0 . 1)     no sibling was used
;; => (300 . 300) every load took the ahead-of-time path
```

A `0` on the left when you expected otherwise usually means one of: no sibling
was written (no `crossgen2`), the sibling is older than its FASL and is being
ignored, or the sibling is for another platform.

## Turning it off

Set `dotcl:*load-r2r*` to `nil` and `load` ignores siblings entirely, which is
the way to compare a run with and without them. `DOTCL_NO_R2R_FASL=1` in the
environment sets the initial value.
