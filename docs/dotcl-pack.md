# Packaging a Lisp app as a .NET tool (`dotcl pack`)

> **Beta.** `dotcl pack` and this guide are early and only lightly exercised.
> Expect rough edges; feedback is welcome.

`dotcl pack` turns an ASDF system into a
[.NET tool](https://learn.microsoft.com/dotnet/core/tools/global-tools) package.
Your users install it with `dotnet tool install` and run it as an ordinary
command -- they never install Lisp or dotcl.

## What you need

- **Your app as an ASDF system**, with an entry-point function that is
  **exported**. The entry point must be external (`(:export #:main)`), or
  `dotcl pack` fails with `Symbol "MAIN" is not external`.
- **The dotcl runtime packages to build on**, gathered in one directory
  (`--from`): the base `dotcl.<version>.nupkg` plus one
  `dotcl.<rid>.<version>.nupkg` for each platform you target. Fetch them from
  NuGet into a folder (set `V` to the dotcl version you want to build on --
  the published versions are listed on
  [nuget.org/packages/dotcl](https://www.nuget.org/packages/dotcl) -- and add a
  line per RID you want):

  ```
  V=<version>
  mkdir dotcl-pkgs
  curl -L -o dotcl-pkgs/dotcl.$V.nupkg \
    https://api.nuget.org/v3-flatcontainer/dotcl/$V/dotcl.$V.nupkg
  curl -L -o dotcl-pkgs/dotcl.win-arm64.$V.nupkg \
    https://api.nuget.org/v3-flatcontainer/dotcl.win-arm64/$V/dotcl.win-arm64.$V.nupkg
  ```

  Keep the filenames exactly as above -- `dotcl pack` looks them up by name.

## Package it

Given `hello.asd` defining a `hello` system whose exported `hello:main` prints a
greeting:

```
dotcl pack --system hello --id hello-tool --command hello --version 0.1.0 \
           -o out/ --from ./dotcl-pkgs/ --dotcl-version "$V" \
           --rids win-arm64 --toplevel hello:main --asd-search-path .
```

- `--system` -- the ASDF system to compile.
- `--id` -- NuGet id of the tool you produce.
- `--command` -- the command your users will type.
- `--version` -- your tool's version. Optional: it defaults to the `:version` in
  your `.asd`, so a project with a version there does not have to repeat it
  here. Pass it to override that, as a nightly build would.
- `-o` -- output directory.
- `--from` -- the directory of dotcl runtime packages (above).
- `--dotcl-version` -- which dotcl version in `--from` to build on. Optional:
  it defaults to the version of the single `dotcl.<version>.nupkg` found in
  `--from`, and is required when `--from` holds more than one.
- `--rids` -- target platforms (see Options).
- `--toplevel` -- the exported function to call at startup. Optional: it
  defaults to the `:entry-point` in your `.asd` (the same option ASDF's
  `program-op` uses), and pack prints which one it picked. With neither, pack
  warns and the tool only loads your system -- right for a system that runs
  itself at load time, and otherwise a tool that does nothing.
- `--asd-search-path` -- where your `.asd` lives, if not already on the ASDF
  source registry. See *Making a dependency visible* below for what this does
  and does not reach.

This writes `out/obj/hello.fasl`, a base package `out/hello-tool.0.1.0.nupkg`,
and one `out/hello-tool.<rid>.0.1.0.nupkg` per RID.

## Install and run

```
dotnet tool install -g hello-tool --add-source out/
hello
```

```
Hello from a packed dotcl tool!
```

On Windows the installed command is a `.cmd` shim named after `--command`.

## Making a dependency visible

`pack` compiles your system the same way any `dotcl` command does: by asking
ASDF to find each system in `:depends-on`. Anything not already on the ASDF
source registry has to be added, and there is more than one way to add it.
The examples below use a `bundleapp` system that depends on `alexandria`, and
were run against this doc.

### `--asd-search-path <dir>` -- single directories only

`--asd-search-path` (repeatable) pushes exactly the directory you name onto
`asdf:*central-registry*`. ASDF's central registry never recurses: it looks
for `<dir>/<system-name>.asd` in each entry, not in subdirectories of it. A
tree of vendored libraries, one subdirectory per system, needs one
`--asd-search-path` per subdirectory -- or one of the tree-aware mechanisms
below.

```
dotcl pack --system myapp --asd-search-path . --asd-search-path vendor/alexandria \
           ...
```

Confirmed: pointing `--asd-search-path` at the parent (`vendor/`, one level
above `vendor/alexandria/alexandria.asd`) fails with `alexandria: not found`,
and a trailing `vendor//` makes no difference -- `--asd-search-path` does not
give `//` any special meaning. `//` is a `CL_SOURCE_REGISTRY` /
`source-registry.conf.d` directive syntax (below), not a central-registry
pathname convention.

### `CL_SOURCE_REGISTRY` -- the standard ASDF environment variable

`pack` runs as an ordinary `dotcl` process, so the standard ASDF source
registry applies: setting `CL_SOURCE_REGISTRY` before running `pack` is
enough, no dotcl-specific flag needed. A directory suffixed with `//` is
searched recursively (ASDF's `:tree` shorthand); a single `/` is not.

```
CL_SOURCE_REGISTRY="/path/to/vendor//" \
dotcl pack --system myapp --asd-search-path . ...
```

This also covers a directory of vendored libraries with one subdirectory per
system, without listing each one.

### `source-registry.conf.d` -- persistent, no environment variable

For a setting that should not depend on how `pack` gets invoked, drop a
`.conf` file under `$XDG_CONFIG_HOME/common-lisp/source-registry.conf.d/`
(default `~/.config/...`) containing a `:tree` directive:

```lisp
;; ~/.config/common-lisp/source-registry.conf.d/myapp.conf
(:tree "/path/to/vendor/")
```

`pack` picks this up the same way any ASDF program does, with no flag or
environment variable at invocation time.

### Quicklisp bundle (`ql:bundle-systems`)

`ql:bundle-systems` copies a system and its dependency closure out of an
existing Quicklisp installation into a self-contained `software/` directory,
so a build does not depend on the machine's Quicklisp:

```lisp
(load "~/quicklisp/setup.lisp")   ; or ~/.roswell/lisp/quicklisp/setup.lisp
(ql:quickload "alexandria")
(ql:bundle-systems '("alexandria") :to #p"bundle/")
```

Point `pack` at the bundle's `software/` directory the same way as any other
tree, with `CL_SOURCE_REGISTRY` (or a `source-registry.conf.d` entry):

```
CL_SOURCE_REGISTRY="$(pwd)/bundle/software//" \
dotcl pack --system myapp --asd-search-path . ...
```

### qlot -- use `qlot bundle`, not `qlot exec`

`qlot exec <command>` sets `QUICKLISP_HOME` to the project's `.qlot/`
directory for `<command>` to read; that only helps a command that itself
loads Quicklisp's `setup.lisp` and consults that variable. `dotcl` does not
load Quicklisp, so `qlot exec dotcl pack ...` leaves your dependencies
unreachable (confirmed: it fails the same way as running `pack` with nothing
set at all).

`qlot bundle` is the mechanism that works here: it writes the same kind of
self-contained `software/` directory `ql:bundle-systems` does, at
`.bundle-libs/` by default:

```
qlot bundle
CL_SOURCE_REGISTRY="$(pwd)/.bundle-libs/software//" \
dotcl pack --system myapp --asd-search-path . ...
```

## Options for real projects

- **`--rids`** -- comma-separated target platforms. Default:
  `win-x64,win-arm64,linux-x64,linux-arm64,osx-x64,osx-arm64,any`. Each RID needs
  a matching `dotcl.<rid>.<version>.nupkg` in `--from`.
- **`--bundle <dir>`** -- extra files to ship alongside the FASL. The contents of
  `<dir>` land next to the installed executable. See *Shipping NuGet packages*
  below for the one layout dotcl looks for there by name.
- **`--prelude <file>`** -- a source file compiled into the image ahead of your
  system and everything it depends on, and loaded into the build itself before
  the closure is collected. Repeatable. For whatever has to be in place before
  any library code runs. The build needs it first because collecting the closure
  builds any system that generates its own sources, and that build needs it too:
  `trivial-gray-streams` names its Gray stream package with
  `(:import-from #+dotcl :dotcl-gray ...)`, and the package has to exist when
  that `defpackage` is *evaluated* at compile time, which happens inside
  `cl-unicode`'s table generator before a line of your own closure is compiled.
  So `(eval-when (:compile-toplevel :load-toplevel :execute) (require "dotcl-gray"))`
  is the prelude you are most likely to need. `dotcl:save-application`'s
  `:prelude` means the same thing and behaves the same way.
- **`--r2r`** -- also compile the FASL ahead of time with crossgen2 and ship the
  result beside it, one image per RID. The installed tool then maps native code
  instead of running its own code through the JIT at every start: worth roughly
  2x on a short run. It costs a crossgen2 run per RID and roughly doubles the
  package, which is why it is opt-in. The packing host does not need to be the
  target platform, but it does need the .NET SDK; the first use restores the
  crossgen2 and runtime packs. If crossgen2 cannot produce an image, pack says so
  and the package is produced without one, which is correct, only slower.
- **`--dry-run`** -- print the planned FASL and packages without producing them.
  Note: dry-run does not compile, so it will not catch a build error such as an
  unexported entry point -- do a real run to validate the build.

### Package metadata for publishing

Your system definition already says most of this, so pack reads it: `:version`,
`:description`, `:homepage`, `:source-control`, `:author` and `:license` become
the corresponding nuspec fields, and a `README.md` next to the `.asd` is
packaged as the embedded README. A field the `.asd` does not state and no flag
supplies is left out of the nuspec rather than inherited from the dotcl packages
being restamped, so your package never claims dotcl's URLs as its own. NuGet
itself requires a description and an author; pack refuses to build a package
missing either, naming both ways to supply it.

The flags override the `.asd` where you want something else:

- **`--description <text>`**, **`--project-url <url>`**, **`--tags <csv>`**
  (comma / semicolon / space separated), **`--authors <text>`**,
  **`--copyright <text>`**.
- **`--repository <url[#commit]>`** -- e.g.
  `https://github.com/you/app.git#<sha>`. Omit `#commit` to leave it out.
- **`--readme <file>`** -- the file whose contents become the package's embedded
  README shown on nuget.org.

## Shipping NuGet packages

An application that calls `nuget:require` resolves by running `dotnet build` on a
throwaway project. That is fine while you develop and wrong for something you
hand to someone else: it wants the .NET SDK and the network, and neither is
promised on the machine your tool is installed on.

Ship the packages instead. `nuget:cache-root` names the directory where a
resolved package was laid out, one subdirectory per request:

```lisp
(require "dotcl-nuget")
(nuget:resolve "Newtonsoft.Json" :version "13.0.3")
(nuget:cache-root)
;; => ".../cache/dotcl-nuget"    with "Newtonsoft.Json_13.0.3_win-arm64_net10.0" inside
```

Copy the subdirectories you need into `<bundle>/nuget/` and pass `--bundle
<bundle>`. At run time dotcl looks beside the executable first, finds the layout,
and registers it without building anything:

```
mybundle/
  nuget/
    Newtonsoft.Json_13.0.3_win-arm64_net10.0/
```

The name of each subdirectory identifies the request -- package, version, RID and
target framework -- so ship the one for the RID you are packaging for.

A bundled layout is used even when the program asks for a floating version
(`"13.*"`, or no `:version` at all). Floating means "whatever is newest", and a
program that has been installed somewhere should not go and find out; what it
shipped with is the answer. Outside a bundle the rule is the opposite: a floating
request is resolved afresh every time, and only an exact version is reused from
the cache.

## How it works

Two steps. `dotcl pack` first compiles your system to a self-contained FASL --
your system and its whole dependency closure, one source at a time in dependency
order, the same way `dotcl:save-application :system` does, so read-time eval
(`#.`) and readtable definitions behave as they do under a normal load -- then
rewrites ("restamps") each `dotcl.<rid>` runtime package from `--from` into your
tool -- your id, command, and version -- with the FASL injected, so the runtime
runs your program instead of starting a REPL.
