# Getting started

From an empty machine to a Common Lisp executable you can hand to someone
else. Every command below was run as written; the output quoted under each
one is the real output, on Windows on ARM64 with dotcl 0.1.28.

Along the way: install dotcl, create a project from a template, run it, add a
library to it, and publish a standalone `.exe`.

## 1. What you need

The .NET SDK, version 10 or newer. Nothing else: no Roswell, no SBCL, no
`make`. Those are needed only to build dotcl itself (step 2c).

```console
$ dotnet --version
10.0.302
```

If `dotnet` is missing:

| OS | Command |
|----|---------|
| macOS (Homebrew) | `brew install --cask dotnet-sdk` |
| Ubuntu 24.04+ | `sudo apt install dotnet-sdk-10.0` |
| Debian | add the Microsoft package repository, then `apt install dotnet-sdk-10.0`; see the [official guide](https://learn.microsoft.com/dotnet/core/install/linux-debian) |
| Windows (winget) | `winget install Microsoft.DotNet.SDK.10` |
| Windows (Scoop) | `scoop install dotnet-sdk` |
| Cross-platform script | [`dotnet-install.sh` / `dotnet-install.ps1`](https://learn.microsoft.com/dotnet/core/tools/dotnet-install-script) |
| Other | https://dotnet.microsoft.com/download |

## 2. Install dotcl

Three ways. Pick one; (a) is the shortest.

### 2a. As a .NET tool

```console
$ dotnet tool install --global dotcl
You can invoke the tool using the following command: dotcl
Tool 'dotcl' (version '0.1.28') was successfully installed.
```

`--global` puts it on `PATH` (`~/.dotnet/tools`). To keep it out of the way,
install it into a directory of your own with `--tool-path <dir>` and call it
by path.

Check it:

```console
$ dotcl --eval "(format t \"hello, ~a ~a~%\" (lisp-implementation-type) (lisp-implementation-version))"
hello, dotcl 0.1.28

$ dotcl repl
dotcl REPL. Ctrl+D to exit.
CL-USER> (+ 1 2)
3
```

A form does not have to fit on a line. Enter looks at the brackets: closed and
the form goes to the reader, still open and it opens the next line and indents
it for you. Pasting a definition of any length works the same way, and the up
arrow brings the whole of it back.

```console
CL-USER> (defun add1 (x)
           (+ x 1))
ADD1
```

A line that starts with a comma is a command to the REPL rather than a form to
evaluate: `,cd` and `,in-package` move where you are, `,doc` and `,args` and
`,apropos` look things up, `,time` and `,load` and `,ql` do work. `,help` lists
them all and `,help <command>` explains one. You can add your own with
`dotcl-repl:define-command` in your init file; the
[contrib README](../contrib/dotcl-repl/README.md) has the full table and an
example.

**You do not need `rlwrap`.** The editing above is dotcl's own, and `rlwrap`
adds nothing to it. If you already reach for `rlwrap` with every Lisp, then
`rlwrap -n dotcl` works and leaves dotcl's editor in charge; the `-n` silences
a warning `rlwrap` prints because dotcl reads keypresses itself. You will still
see your first input line echoed once more underneath itself, because `rlwrap`
adds a line break that dotcl's redraw does not know about. It is cosmetic, and
it happens once per session.

What `rlwrap` still has that dotcl does not is your `~/.inputrc` and vi mode.
To use either, turn dotcl's editor off with `rlwrap dotcl --no-readline` - but
that is a real trade, because multi-line editing, the comma commands and TAB
completion all live in the editor you just turned off.

[The REPL](repl.md) is the page for the rest of it: the init file, where what
you type is kept between sessions, what the debugger does and does not open
for, and the parts that are not there yet.

### 2b. From a per-RID tarball

Every release page carries one archive per runtime identifier, named
`dotcl-<rid>-<version>.tar.bz2`, for the six desktop RIDs (`win-x64`,
`win-arm64`, `linux-x64`, `linux-arm64`, `osx-x64`, `osx-arm64`). It holds the
runtime, the core image and the bundled contrib tree, already compiled
ahead-of-time for that RID. This is the route Roswell users take.

```console
$ curl -sLO https://github.com/dotcl/dotcl/releases/download/v0.1.28/dotcl-win-arm64-0.1.28.tar.bz2
$ ls -l dotcl-win-arm64-0.1.28.tar.bz2
-rw-r--r-- 1 ... 13046308 ... dotcl-win-arm64-0.1.28.tar.bz2

$ mkdir dotcl && tar -xjf dotcl-win-arm64-0.1.28.tar.bz2 -C dotcl
$ ls dotcl
DotCL.Runtime.dll  contrib  dotcl.core  runtime.deps.json
runtime.dll  runtime.exe  runtime.runtimeconfig.json
```

The executable in the archive is `runtime.exe` (`runtime` on Linux and macOS),
and it takes the same arguments the `dotcl` tool does:

```console
$ dotcl/runtime.exe --eval "(format t \"~a ~a on ~a~%\" (lisp-implementation-type) (lisp-implementation-version) (software-type))"
dotcl 0.1.28 on Microsoft Windows 10.0.26200
```

13 MB to download, 47 MB unpacked. The archive is framework-dependent: it
still wants the .NET 10 runtime installed (it does not carry its own).

### 2c. From source

Only if you want to work on dotcl itself. Clone the repository and bootstrap
the compiler with [Roswell](https://github.com/roswell/roswell)/SBCL:

```bash
make cross-compile        # uses Roswell/SBCL to bootstrap the compiler
make compile-asdf-fasl    # pre-compiles ASDF (required by samples)
make install              # builds and installs the local nupkg as `dotcl`
```

After the first cross-compile, dotcl can build itself:
`DOTCL_LISP=dotcl make cross-compile` rebuilds the compiler with dotcl.

On Windows:

- **`make`**: the build needs GNU Make. Git Bash (bundled with
  [Git for Windows](https://gitforwindows.org/)) ships GNU Make and works;
  run the commands above from a Git Bash prompt. WSL works too.
- **Path translation**: run the build from Git Bash (`/c/...` paths) rather
  than a shell that rewrites paths to Cygwin form (`/cygdrive/c/...`). The
  Roswell/SBCL bootstrap reads the paths verbatim, so `/cygdrive/...` paths
  it can't open surface as a `SB-INT:SIMPLE-FILE-ERROR` during
  `make cross-compile`.
- **`dotcl` not found after `make install`**: `make install` registers
  `dotcl` as a .NET global tool under `~/.dotnet/tools`, which is on `PATH`
  in PowerShell but often not in Git Bash. Add it there with
  `export PATH="$HOME/.dotnet/tools:$PATH"` (or run `dotcl` from PowerShell).

## 3. Your first project

The templates ship as their own package:

```console
$ dotnet new install DotCL.Templates
Success: DotCL.Templates@0.1.28 installed the following templates:
Template Name              Short Name      Language     Tags
-------------------------  --------------  -----------  ---------------------------
Common Lisp Class Library  dotcl-classlib  Common Lisp  Library/Windows/Linux/macOS
Common Lisp Console App    dotcl-app       Common Lisp  Console/Windows/Linux/macOS
```

Create an app and run it:

```console
$ dotnet new dotcl-app -n hello
The template "Common Lisp Console App" was created successfully.

$ cd hello
$ dotnet run
[build] hello: compiling 1 source(s)
[build]   .../hello/app.lisp
Hello from dotcl
```

That is the whole loop: edit `app.lisp`, `dotnet run` again.

### What the template wrote

Four files:

| File | What it is |
|------|------------|
| `app.lisp` | your Lisp code, with `app-main` as the entry point |
| `hello.asd` | the ASDF system: which files, in which order, and what they depend on |
| `hello.csproj` | an ordinary .NET project that adds two things (below) |
| `Program.cs` | a five-line C# `Main` that boots the runtime and calls `APP:APP-MAIN` |

The csproj is an ordinary `Microsoft.NET.Sdk` project plus one package
reference and one property:

```xml
<PropertyGroup>
  <DotclProjectAsd>$(MSBuildProjectDirectory)/hello.asd</DotclProjectAsd>
</PropertyGroup>

<ItemGroup>
  <PackageReference Include="DotCL.Runtime" Version="0.1.28" />
</ItemGroup>
```

`DotclProjectAsd` is what turns a .NET build into a Lisp build: the package
brings MSBuild targets that walk the `.asd`, compile the system to a FASL, and
copy the FASL plus the base core image into the output as `dotcl-fasl/`.
`Program.cs` then loads `dotcl-fasl/dotcl-deps.txt` at startup. Nothing is
compiled at run time, and no `dotnet tool install` is needed to build: the
compile happens in-process, out of the package.

The properties that steer this are listed in [SDK and MSBuild
properties](sdk.md). There is also a `DotCL.Sdk` MSBuild SDK, which writes the
`PackageReference` for you; the template does not use it so that the generated
project stays an ordinary csproj you can read.

## 4. Add a library

Dependencies go in the `.asd`, the way they do in any ASDF system. dotcl does
not scan `~/quicklisp`, so a build points at the library's directory itself
with `DotclAsdSearchPath` (one item per directory; each is pushed onto
`asdf:*central-registry*` before dependency resolution).

Get the library:

```console
$ git clone --depth 1 https://gitlab.common-lisp.net/alexandria/alexandria.git ../lib/alexandria
Cloning into '../lib/alexandria'...
```

Declare it in `hello.asd`:

```lisp
(defsystem "hello"
  :depends-on ("alexandria")
  :components ((:file "app")))
```

Point the build at it, in `hello.csproj`:

```xml
<ItemGroup>
  <DotclAsdSearchPath Include="../lib/alexandria/" />
</ItemGroup>
```

Use it in `app.lisp`:

```lisp
(defpackage :app (:use :cl) (:export #:app-main))
(in-package :app)

(defun app-main ()
  (format t "~a~%" (alexandria:iota 5))
  (format t "~a~%" (alexandria:hash-table-keys
                    (alexandria:alist-hash-table '((:a . 1))))))
```

```console
$ dotnet run
[resolve-deps] compiling alexandria...
[build] hello: compiling 1 source(s)
[build]   .../hello/app.lisp
(0 1 2 3 4)
(A)
```

The dependency is compiled once and bundled next to your own FASL, so the
published app does not depend on where the sources were.

Two things this is not:

- **Not the only route.** Systems bundled with dotcl (`dotcl-thread`,
  `dotnet-ffi`, `nuget`, and the rest of contrib) are found without a search
  path. NuGet packages are a separate axis: a `PackageReference` in the csproj,
  or a `(:nuget "Id" :nuget-version "...")` component in the `.asd`.
  [Using libraries](libraries.md) covers both.
- **Not the same as `CL_SOURCE_REGISTRY`.** That variable works when you run
  the `dotcl` command, and is ignored by the MSBuild build. If a system loads
  from the command line but not from `dotnet build`, this asymmetry is why:
  add a `DotclAsdSearchPath` item.

## 5. Publish an executable

```console
$ dotnet publish -c Release -r win-arm64 --self-contained
[resolve-deps] compiling alexandria...
[build] hello: compiling 1 source(s)
  hello -> .../hello/bin/Release/net10.0/win-arm64/publish/
```

Use your own RID in place of `win-arm64` (`win-x64`, `linux-x64`,
`linux-arm64`, `osx-x64`, `osx-arm64`).

What comes out, in `bin/Release/net10.0/win-arm64/publish/`:

| Path | Size |
|------|------|
| `hello.exe` | 137 KB |
| `dotcl-fasl/dotcl.core` | 1.9 MB (the base image) |
| `dotcl-fasl/alexandria.fasl` | 656 KB (the dependency) |
| `dotcl-fasl/hello.fasl` | 5.5 KB (your code) |
| the whole directory | 100 MB, 249 files (most of it the self-contained .NET runtime) |

```console
$ ./bin/Release/net10.0/win-arm64/publish/hello.exe
(0 1 2 3 4)
(A)
```

That directory is the thing you ship. With `--self-contained` the target
machine needs no .NET installed; drop `--self-contained` for a much smaller
output that requires the .NET 10 runtime on the target.

Smaller and faster variants are a separate topic: ahead-of-time compiled FASLs
in [ReadyToRun siblings](readytorun.md), and native AOT in
`samples/PrecompiledLispDemoAot/`.

## Where to go next

- [Embedding dotcl](embedding.md): the C# side of what the template's
  `Program.cs` does, and what else a host can do
- [dotcl for Lisp programmers](for-lispers.md): what differs from SBCL, and
  the REPL and editors
- [Using libraries](libraries.md): ASDF, Quicklisp, NuGet dependencies
- [SDK and MSBuild properties](sdk.md): every property the build reads
- [Writing scripts](scripting.md): no project at all, just a `.lisp` file
- [Packaging an app](dotcl-pack.md): ship an ASDF system as a `dotnet tool`
- [Calling .NET from Lisp](dotnet-package.md): the `dotnet:` package
