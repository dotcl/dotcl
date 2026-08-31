;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "nuget"
  :description "Resolve a NuGet package and its transitive dependencies (managed + RID-specific native) and register them with the dotcl assembly/native resolver."
  :version "1.0"
  :class require-system)