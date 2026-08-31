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
  NuGet into a folder (adjust the version and add a line per RID you want):

  ```
  mkdir dotcl-pkgs
  curl -L -o dotcl-pkgs/dotcl.0.1.26.nupkg \
    https://api.nuget.org/v3-flatcontainer/dotcl/0.1.26/dotcl.0.1.26.nupkg
  curl -L -o dotcl-pkgs/dotcl.win-arm64.0.1.26.nupkg \
    https://api.nuget.org/v3-flatcontainer/dotcl.win-arm64/0.1.26/dotcl.win-arm64.0.1.26.nupkg
  ```

  Keep the filenames exactly as above -- `dotcl pack` looks them up by name.

## Package it

Given `hello.asd` defining a `hello` system whose exported `hello:main` prints a
greeting:

```
dotcl pack --system hello --id hello-tool --command hello --version 0.1.0 \
           -o out/ --from ./dotcl-pkgs/ --dotcl-version 0.1.26 \
           --rids win-arm64 --toplevel hello:main --asd-search-path .
```

- `--system` -- the ASDF system to compile.
- `--id` -- NuGet id of the tool you produce.
- `--command` -- the command your users will type.
- `--version` -- your tool's version.
- `-o` -- output directory.
- `--from` -- the directory of dotcl runtime packages (above).
- `--dotcl-version` -- which dotcl version in `--from` to build on.
- `--rids` -- target platforms (see Options).
- `--toplevel` -- the exported function to call at startup.
- `--asd-search-path` -- where your `.asd` lives, if not already on the ASDF
  source registry.

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

## Options for real projects

- **`--rids`** -- comma-separated target platforms. Default:
  `win-x64,win-arm64,linux-x64,linux-arm64,osx-x64,osx-arm64,any`. Each RID needs
  a matching `dotcl.<rid>.<version>.nupkg` in `--from`.
- **`--bundle <dir>`** -- extra files to ship alongside the FASL. The contents of
  `<dir>` land next to the installed executable. See *Shipping NuGet packages*
  below for the one layout dotcl looks for there by name.
- **`--dry-run`** -- print the planned FASL and packages without producing them.
  Note: dry-run does not compile, so it will not catch a build error such as an
  unexported entry point -- do a real run to validate the build.

### Package metadata for publishing

Restamping reuses the dotcl runtime packages, so without these flags your tool
inherits dotcl's nuspec metadata -- its description, project URL, repository,
embedded README and tags. When you publish under your own id, override them:

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
(require "nuget")
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

Two steps. `dotcl pack` first compiles your system to a self-contained FASL, then
rewrites ("restamps") each `dotcl.<rid>` runtime package from `--from` into your
tool -- your id, command, and version -- with the FASL injected, so the runtime
runs your program instead of starting a REPL.
