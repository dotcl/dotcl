# dotcl-cs

Compile and embed C# in dotcl through Roslyn. `dotcl-cs:disassemble-cs` compiles
a C# body and returns its IL as a dotcl instruction list, and the
`dotcl-cs:inline-cs` macro splices that IL straight into the enclosing Lisp
function. The inline path is deliberately narrow for now: `long` parameters and
a `long` return.

    (require "dotcl-cs")

Needs this directory's `lib/`, which holds Roslyn and the small C# helper
(`DotCL.Contrib.DotclCs.RoslynCompiler`) that the Lisp side calls in-process.
The packaged dotcl ships it; a source tree builds it with
`make contrib-dotcl-cs`.
