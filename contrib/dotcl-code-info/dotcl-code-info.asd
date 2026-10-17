;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-code-info"
  :description "Editor queries against a live image: completion candidates for Lisp symbols and .NET members."
  :version "1.0"
  :class require-system)
