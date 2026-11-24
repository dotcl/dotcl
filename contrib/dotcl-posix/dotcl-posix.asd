;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-posix"
  :description "A thin POSIX layer: pseudo-terminals and signals (Unix only)."
  :version "0.1"
  :class require-system)
