;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-lsp"
  :description "A Language Server Protocol server inside a running dotcl."
  :version "0.1"
  :depends-on ("dotcl-thread" "dotcl-socket" "dotcl-jsonrpc" "dotcl-code-info")
  :class require-system)
