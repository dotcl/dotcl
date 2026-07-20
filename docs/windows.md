# dotcl on Windows

Windows-specific behaviour worth knowing. General .NET interop examples live in
`examples/windows/`.

## Console I/O

- **UTF-8 always** for stdin/stdout -- independent of `chcp`, the parent code
  page, or `%LANG%`. Non-ASCII `format` / `read-line` works as-is. Garbled
  output is a terminal font issue, not dotcl (the bytes are valid UTF-8).
- **ANSI/VT escapes** (colour, cursor) are on at startup, even in legacy
  conhost / cmd.exe. Set `DOTCL_NO_VT=1` to disable.

## Pathnames

- Both `C:/foo` and `C:\foo` are accepted; `namestring` canonicalises to forward
  slashes (`(pathname "C:\\Users\\you")` -> `#P"C:/Users/you"`).
- `*default-pathname-defaults*` carries the drive letter. Wildcards,
  `probe-file`, `truename` (junction/symlink), and non-ASCII filenames all work.

## Calling Windows APIs

Use the `dotnet:` package. For example, reading the registry:

```lisp
(dotnet:static "Microsoft.Win32.Registry, Microsoft.Win32.Registry" "GetValue"
  "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
  "ProductName" "unknown")
```

(the `, Microsoft.Win32.Registry` suffix is an assembly qualification.) APIs not
bundled with the runtime -- e.g. `System.Management` for WMI -- load with
`(dotnet:require "System.Management")`. More examples (Environment, WMI, SAPI,
StringBuilder) are in `examples/windows/dotnet-interop-examples.lisp`.

## UI frameworks (WinForms / WPF / MAUI / WinUI / MonoGame)

Frameworks that need subclassing (`Form`, `Application`, `ContentPage`, `Game`, ...)
use `dotnet:define-class` to emit a named .NET class from Lisp. UI must run on an
**STA thread** with a message pump (`Application.Run` for WinForms,
`Dispatcher.Run` for WPF).

- **WinForms** works from the plain `dotcl` runtime.
- **WPF** needs a **WindowsDesktop host** -- an embedder project targeting
  `net10.0-windows` with `<UseWPF>true</UseWPF>`, referencing `DotCL.Runtime`.
  The bare `net10.0` runtime loads the assemblies but trips on WPF's type
  forwarders.

Working samples: `samples/MauiLispDemo/` (MAUI), `samples/AspNetLispDemo/`
(ASP.NET Core), `samples/MonoGameLispDemo/` (MonoGame), and
`examples/windows/winforms-demo.lisp`.

## COM

Late-bound COM (`Type.GetTypeFromProgID` + `Activator.CreateInstance`) works;
see `examples/windows/`.
