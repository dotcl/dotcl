;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name
;;; resolve, which is what :defsystem-depends-on needs.
(defsystem "dotnet-library"
  :description "Build an ASDF system into a .NET library DLL: LIBRARY-OP, run by (asdf:make ...) through :build-operation, and the (:dotnet-library ...) component that holds the dotnet:library form."
  :version "1.0"
  :depends-on ("asdf" "dotnet-class")
  :class require-system)
