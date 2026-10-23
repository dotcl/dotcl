;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-lsp-coalton"
  :description "Coalton type errors and types for dotcl-lsp. Needs Coalton loaded."
  :version "0.1"
  :depends-on ("dotcl-lsp")
  :class require-system)
