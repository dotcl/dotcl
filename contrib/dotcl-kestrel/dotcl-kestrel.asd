;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-kestrel"
  :description "HTTP server for dotcl, on ASP.NET Core's Kestrel (built-in)."
  :version "1.0"
  :class require-system)