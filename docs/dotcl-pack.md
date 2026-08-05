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
  curl -L -o dotcl-pkgs/dotcl.0.1.19.nupkg \
    https://api.nuget.org/v3-flatcontainer/dotcl/0.1.19/dotcl.0.1.19.nupkg
  curl -L -o dotcl-pkgs/dotcl.win-arm64.0.1.19.nupkg \
    https://api.nuget.org/v3-flatcontainer/dotcl.win-arm64/0.1.19/dotcl.win-arm64.0.1.19.nupkg
  ```

  Keep the filenames exactly as above -- `dotcl pack` looks them up by name.

## Package it

Given `hello.asd` defining a `hello` system whose exported `hello:main` prints a
greeting:

```
dotcl pack --system hello --id hello-tool --command hello --version 0.1.0 \
           -o out/ --from ./dotcl-pkgs/ --dotcl-version 0.1.19 \
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
- **`--bundle <dir>`** -- extra files to ship alongside the FASL.
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

## How it works

Two steps. `dotcl pack` first compiles your system to a self-contained FASL, then
rewrites ("restamps") each `dotcl.<rid>` runtime package from `--from` into your
tool -- your id, command, and version -- with the FASL injected, so the runtime
runs your program instead of starting a REPL.
