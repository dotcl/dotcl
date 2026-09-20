# decompiler

Decompile a .NET assembly back to readable C# from Lisp, through
ICSharpCode.Decompiler (the ILSpy engine): `decompiler:type-source` for a whole
type, `decompiler:method-source` for one method. The target is located by name
through dotcl's own type resolver, so anything `dotnet:resolve-type` can find is
decompilable from its on-disk file.

    (require "dotcl-decompiler")

Depends on the `nuget` contrib, which resolves ICSharpCode.Decompiler on first
use (`decompiler:*package-version*` pins it). The engine itself is a single
netstandard2.0 assembly that uses no Reflection.Emit; on a host with no .NET SDK
to resolve from, ship the assembly alongside the runtime and let
`decompiler:ensure` load it by name.
