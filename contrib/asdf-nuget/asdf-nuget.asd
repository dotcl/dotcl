;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name
;;; resolve, which is what :defsystem-depends-on needs.
(defsystem "asdf-nuget"
  :description "An ASDF component class, (:nuget \"Package.Id\" ...), that resolves a NuGet package when the system is loaded; packing a system as a NuGet package; CHECK-PORTABILITY-OP, which finds what ties a system's fasls to one OS or CPU."
  :version "1.0"
  :depends-on ("asdf" "dotnet-nuget")
  :class require-system)
