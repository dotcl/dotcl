# dotnet-class

`dotnet:define-class` -- emit a real, named .NET type from Lisp, so .NET code
that expects a subclass or an interface implementation can be handed one. A
class body declares `:fields`, a `:ctor`, `:methods` (each with `:returns`, and
`:override t` for a virtual method of the base), `:implements` for interfaces
and `:attributes`.

    (require "dotnet-class")

No dependencies. The full syntax is in
[docs/define-class.md](../../docs/define-class.md).
