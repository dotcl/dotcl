# Using libraries

dotcl reaches two ecosystems, and they are loaded differently:

- **Common Lisp systems** — ASDF and Quicklisp, exactly as on any other
  implementation.
- **.NET packages** — NuGet, resolved at run time and handed to the same
  `dotnet:` interop you use for the framework itself.

Neither needs a build step: both work from a running image.

## What ships with dotcl

`require` loads a bundled module. No download, no configuration:

```lisp
(require "asdf")          ; ASDF 3.3.7.4
(require "quicklisp")     ; the Quicklisp client
(require "nuget")         ; NuGet package resolution
(require "dotnet-class")  ; dotnet:define-class — subclass a .NET type
(require "dotnet-ffi")    ; P/Invoke to native libraries
(require "dotcl-socket")  ; sockets
(require "dotcl-thread")  ; threads (bordeaux-threads style)
(require "dotcl-gray")    ; Gray streams
(require "dotcl-repl")    ; a nicer REPL
(require "advice")        ; function advice
(require "clrmd")         ; inspect a .NET heap (ClrMD)
(require "decompiler")    ; decompile .NET methods
(require "dotcl-jitdisasm") ; native disassembly of a compiled Lisp function
(require "dotcl-cs")      ; compile C# from Lisp
(require "dotcl-float")   ; float utilities
```

Editor integration is not in that list: the SLIME / SLY backend is `micros`, and
it comes from Quicklisp — `(ql:quickload "micros")` after the setup below.

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
dist when the client loads. The first `quickload` installs one — asking for a
library by name is the request to go and fetch it — which makes that call take
a few seconds longer the first time. `(ql:setup)` does the same thing up front,
if you would rather pay it at a moment you choose.

Setup installs two dists: the stock Quicklisp one, and dotcl's overlay
(`https://dotcl.github.io/dist/dotcl.txt`). The overlay carries patched releases
for the few libraries that need a change to run here, and is given the higher
preference, so `quickload` takes the patched release for those names and the
stock release for everything else. It shrinks as patches land upstream; an empty
overlay is the goal. For a stock-only home, `(setf ql-setup:*offer-dotcl-dist*
nil)` before the first `quickload` (or before `ql:setup`).

The Quicklisp home is dotcl's own — `%APPDATA%\dotcl\quicklisp\` on Windows,
`$XDG_DATA_HOME/dotcl/quicklisp/` (i.e. under `~/.local/share`) elsewhere — so
it does not disturb a Quicklisp you already use from another implementation. An
existing `~/quicklisp/` wins over both, so a machine set up by the stock
installer keeps working as it is.

How far a given library gets depends on what it assumes. Portable
Common Lisp works; a library that reaches into another implementation's
internals does not. Libraries that lean on `sb-` packages, on a specific
FASL format, or on foreign-function details are the ones to expect trouble
from. Several widely used systems — alexandria, cl-ppcre, esrap, fset,
cl-store, iterate, trivia, babel — are exercised against dotcl regularly.

## .NET packages

`nuget:require` resolves a package and its transitive dependencies, then
registers every managed assembly and RID-specific native library with dotcl's
assembly resolver. After that the types are visible to `dotnet:`:

```lisp
(require "nuget")
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
| `:source` | an extra feed URI, appended to the default sources — for a private feed. |
| `:rid` | target RuntimeIdentifier. Defaults to the running process's, which selects the native assets laid out. |
| `:tfm` | target framework moniker. Defaults to the running runtime's. |

`nuget:resolve` is the same thing but returns the counts and the output
directory, if you want to see what was laid down.

## Shipping an application that uses them

The mechanisms above resolve dependencies at run time, which is what you want
while developing. To turn the result into something distributable — an ASDF
system packaged as a .NET tool, with its dependencies bundled — see
[Packaging an app](dotcl-pack.md).
