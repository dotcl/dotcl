# dotnet-library

Build an ASDF system into a .NET library DLL that C# references, with the
system's code compiled into it.

    (defsystem "word-stats"
      :defsystem-depends-on ("dotnet-library")
      :build-operation "dotnet-library-op"
      :depends-on ("text-util")
      :serial t
      :components ((:file "package")
                   (:file "stats")
                   (:dotnet-library "plugin")))

    (asdf:make "word-stats")

`plugin.lisp` holds the `dotnet:library` form. Loading the system leaves it
alone; `asdf:make` loads the system and then that file, and compiles in every
function of the system, and of the systems it depends on, that the library's
bodies reach. `:build-pathname` names the DLL. The same build is
`(dotcl:build-library :system ...)`, `dotcl build-library --system ...` and the
SDK's `<LispSystem>` item. Depends on `asdf` and `dotnet-class`. See
[docs/define-class.md](../../docs/define-class.md#building-the-library-from-an-asdf-system).
