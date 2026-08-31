;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-float"
  :description "IEEE float bit<->value primitives for dotcl, backed by System.BitConverter."
  :version "1.0"
  :class require-system)