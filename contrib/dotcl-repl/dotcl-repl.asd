;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-repl"
  :description "Terminal readline for dotcl: history, completion, CJK-aware editing."
  :version "0.1"
  :author "SANO Masatoshi"
  :license "MIT"
  :class require-system)