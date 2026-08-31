;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-gray"
  :description "Gray streams implementation for dotcl (built-in)."
  :version "1.0"
  :class require-system)