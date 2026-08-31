;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "advice"
  :description "Interactive advice (watch / patch) on live .NET methods from Lisp, via Lib.Harmony."
  :version "0.1"
  :class require-system)