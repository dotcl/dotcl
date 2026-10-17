;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-simd"
  :description "128-bit SIMD packs and operations for dotcl, in the style of sb-simd."
  :version "1.0"
  :class require-system)
