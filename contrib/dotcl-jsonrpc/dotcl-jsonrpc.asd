;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-jsonrpc"
  :description "JSON and Content-Length framed messages, as DAP and LSP use them."
  :version "0.1"
  :class require-system)
