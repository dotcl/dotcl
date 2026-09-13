;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name
;;; resolve, which is what :defsystem-depends-on needs.
(defsystem "dotcl-nuget-asdf"
  :description "An ASDF component class, (:nuget \"Package.Id\" ...), that resolves a NuGet package when the system is loaded."
  :version "1.0"
  :class require-system)
