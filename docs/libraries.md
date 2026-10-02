# Using libraries

dotcl reaches two ecosystems, and they are loaded differently:

- **Common Lisp systems**: ASDF and Quicklisp, exactly as on any other
  implementation.
- **.NET packages**: NuGet, resolved at run time and handed to the same
  `dotnet:` interop you use for the framework itself.

Neither needs a build step: both work from a running image.

## What ships with dotcl

`require` loads a bundled module. No download, no configuration:

```lisp
(require "asdf")             ; ASDF 3.3.7.4
(require "quicklisp")        ; the Quicklisp client
(require "dotcl-nuget")      ; NuGet package resolution
(require "dotcl-nuget-asdf") ; declare a NuGet package in a system definition
(require "dotnet-class")     ; dotnet:define-class: subclass a .NET type
(require "dotnet-ffi")       ; P/Invoke to native libraries
(require "dotcl-socket")     ; sockets
(require "dotcl-kestrel")    ; HTTP server (ASP.NET Core's Kestrel), serves a Lack app
(require "dotcl-thread")     ; threads (bordeaux-threads style)
(require "dotcl-gray")       ; Gray streams
(require "dotcl-repl")       ; a nicer REPL, with TAB completion and ,commands
(require "dotcl-lsp-api")    ; completion candidates for an editor to ask for
(require "dotcl-advice")     ; function advice
(require "dotcl-clrmd")      ; inspect a .NET heap (ClrMD)
(require "dotcl-decompiler") ; decompile .NET methods
(require "dotcl-cs")         ; compile C# from Lisp
(require "dotcl-float")      ; float utilities
```

Editor integration is not in that list: the SLIME / SLY backend is `micros`, and
it comes from Quicklisp: `(ql:quickload "micros")` after the setup below.

The argument hints those editors show come from `dotcl:function-lambda-list`,
which is the entry point a swank or slynk backend has to call for them; the
standard offers none, and `function-lambda-expression` is allowed to answer
`nil` and does. It returns the lambda list and a second value saying whether one
was found, so a caller can tell "takes no arguments" from "unknown" and show
nothing rather than something wrong:

```lisp
(defun f (a &optional b &key c) (list a b c))
(dotcl:function-lambda-list #'f)   ; => (A &OPTIONAL B &KEY C), T
```

Compiled functions, macros, generic functions and interpreted closures answer,
and so do functions loaded from a FASL, including the standard functions in the
core image. Given a symbol, a macro binding wins over a function binding: what a
macro takes is its own lambda list, not the `(form env)` pair its expander
takes. The rough edge is the built-ins written in C#: most answer `nil`, and a
few answer a placeholder with generated names, which tells you the arity and
nothing more.

## Common Lisp libraries

ASDF is bundled, so a system on disk loads with no setup:

```lisp
(require "asdf")
(push #p"/path/to/my-systems/" asdf:*central-registry*)
(asdf:load-system "my-system")
```

Quicklisp ships with dotcl:

```lisp
(require "quicklisp")        ; loads the client; touches no network
(ql:quickload "alexandria")  ; installs a dist first if this home has none
(alexandria:flatten '(1 (2 (3))))   ; => (1 2 3)
```

`require` deliberately stays off the network, so a fresh Quicklisp home has no
dist when the client loads. The first `quickload` installs one, asking for a
library by name is the request to go and fetch it, which makes that call take
a few seconds longer the first time. `(ql:setup)` does the same thing up front,
if you would rather pay it at a moment you choose.

Setup installs two dists: the stock Quicklisp one, and dotcl's overlay
(`https://dotcl.github.io/dist/dotcl.txt`). The overlay carries patched releases
for the few libraries that need a change to run here, and is given the higher
preference, so `quickload` takes the patched release for those names and the
stock release for everything else. It shrinks as patches land upstream; an empty
overlay is the goal. For a stock-only home, `(setf ql-setup:*offer-dotcl-dist*
nil)` before the first `quickload` (or before `ql:setup`).

The Quicklisp home is dotcl's own; `%APPDATA%\dotcl\quicklisp\` on Windows,
`$XDG_DATA_HOME/dotcl/quicklisp/` (i.e. under `~/.local/share`) elsewhere, so
it does not disturb a Quicklisp you already use from another implementation. An
existing `~/quicklisp/` wins over both, so a machine set up by the stock
installer keeps working as it is.

How far a given library gets depends on what it assumes. Portable
Common Lisp works; a library that reaches into another implementation's
internals does not. Libraries that lean on `sb-` packages, on a specific
FASL format, or on foreign-function details are the ones to expect trouble
from. Several widely used systems, alexandria, cl-ppcre, esrap, fset,
cl-store, iterate, trivia, babel, are exercised against dotcl regularly.

## .NET packages

There are three ways to say that something needs a NuGet package. They differ in
where the declaration lives, and that is what decides which one you want:

| How | Where you write it | Use it when |
| --- | --- | --- |
| `(nuget:require "Id" :version "13.0.3")` | in a form you evaluate -- the REPL, a script | you are exploring, or the script *is* the project |
| `(:nuget "Id" :nuget-version "13.0.3")` component | in the `.asd`, with `:defsystem-depends-on ("dotcl-nuget-asdf")` | an ASDF system needs the package, and the need should travel with the system |
| `<PackageReference Include="Id" Version="13.0.3" />` | in the `.csproj` of a .NET project that hosts dotcl (`<Project Sdk="DotCL.Sdk">`, or your own project with a `DotCL.Runtime` package reference) | the application is a .NET application. Restore is then a build step like any other, and the Lisp side just uses the types |

The third is not a dotcl mechanism at all -- it is how .NET projects have always
declared dependencies, and `samples/` has projects built that way. The two Lisp
ones are below.

### At the REPL

`nuget:require` resolves a package and its transitive dependencies, then
registers every managed assembly and RID-specific native library with dotcl's
assembly resolver. After that the types are visible to `dotnet:`:

```lisp
(require "dotcl-nuget")
(nuget:require "Newtonsoft.Json")

(dotnet:static "Newtonsoft.Json.JsonConvert" "SerializeObject"
               (dotnet:new "System.Collections.Generic.List`1[System.String]"))
;; => "[]"
```

The package identity has more axes than a name, so they are keywords:

| keyword | meaning |
| --- | --- |
| `:version` | exact (`"13.0.3"`), a range (`"[1.0,2.0)"`), or floating (`"13.*"`). Omitted means the latest stable release. |
| `:prerelease` | when true and `:version` is omitted, take the latest prerelease. |
| `:source` | an extra feed URI, appended to the default sources; for a private feed. |
| `:rid` | target RuntimeIdentifier. Defaults to the running process's, which selects the native assets laid out. |
| `:tfm` | target framework moniker. Defaults to the running runtime's. |

`nuget:resolve` is the same thing but returns the counts and the output
directory, if you want to see what was laid down.

Everything asked for in one image is resolved as one set, so packages that share
a dependency get one version of it. A package registered earlier keeps its
version: asking for something that needs it moved is an error naming the
package, and the fix is a new image that asks for everything before using any of
it. `nuget:require` neither reads nor writes the project's lock file; that is
for declarations, below.

[`examples/http-json.lisp`](../examples/http-json.lisp) puts this together in a
script you can run: it resolves a package, awaits an async .NET method, and
reads the JSON that comes back.

### In a system definition

A system can name the packages it needs instead of calling `nuget:require` from
somewhere in its own code. Load the component class through
`:defsystem-depends-on`, then write one `(:nuget ...)` component per package:

```lisp
(defsystem "my-app"
  :defsystem-depends-on ("dotcl-nuget-asdf")
  :serial t
  :components ((:nuget "Newtonsoft.Json" :nuget-version "13.0.3")
               (:file "app")))
```

Loading the system resolves the package and registers its assemblies, so
`app.lisp` can name the types. Ordering is the system's business as with any
other component: put the `:nuget` component before the files that need it and
mark the system `:serial t`, or name it in a `:depends-on`.

The options are the keywords `nuget:require` takes, with the component's name as
the package id -- with one rename:

| keyword | meaning |
| --- | --- |
| `:nuget-version` | the version. **Not `:version`** -- see below. |
| `:source`, `:prerelease`, `:rid`, `:tfm` | as in the table above. |

`:version` is ASDF's own component version, and `defsystem` takes it for itself
before the component is built, so a NuGet version written there would never
reach NuGet. Writing `:version` on a `:nuget` component is therefore an error
that tells you to use `:nuget-version`.

**Versions come from a lock file.** Unlike `nuget:require` typed at the REPL --
which is you asking for something right now -- a `(:nuget ...)` component is a
declaration that a later `load-system` acts on, possibly on someone else's
machine. So loading a system resolves declared packages through
`dotcl-nuget.lock.json` in the project directory (`nuget:*project-directory*`,
by default the current directory). The file is NuGet's own lock-file format and
records the version of every package in the closure:

- What the lock file records is used as recorded, without asking NuGet. Once a
  layout for those versions is in the cache, loading touches neither the network
  nor the .NET SDK.
- An exact version it does not record yet is resolved and recorded, and dotcl
  says which packages it is fetching.
- A floating version (`"13.*"`), a range, or no version at all, is **not**
  resolved by loading: the answer would depend on the day it runs. Loading stops
  and says so. Pin the version, or run `(nuget:restore)` once: it resolves what
  the image has declared, writes the lock file, and from then on loading follows
  it. Run it again to move a recorded floating version forward.

Commit the lock file with the project, as you would `packages.lock.json`.

Every `(:nuget ...)` a system reaches, through its own components and its
dependencies', is resolved in one go when the system is loaded, so NuGet unifies
a dependency they share. Within one image a registered version stays where it
is: a later resolution that would move it is an error asking you to start a new
image, since the old assembly may already be loaded.

Set `DOTCL_NUGET_OFFLINE=1` to forbid the network entirely, for CI: only the lock
file plus an already laid-out cache, or a bundled layout, can then answer.

None of this runs in a packaged application. `dotcl pack` lays out the packages
the system declares for each platform it builds a package for, and carries them
beside the executable; a bundled layout is preferred over every other route,
whatever the version spec says. An installed program therefore goes neither to
the network nor to the .NET SDK. See [Packaging an app](dotcl-pack.md).

## Shipping an application that uses them

The mechanisms above resolve dependencies at run time, which is what you want
while developing. To turn the result into something distributable, an ASDF
system packaged as a .NET tool, with its dependencies bundled, see
[Packaging an app](dotcl-pack.md).
