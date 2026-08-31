;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotnet-class"
  :description "dotnet:define-class — emit named .NET classes from Lisp."
  :version "1.0"
  :class require-system)