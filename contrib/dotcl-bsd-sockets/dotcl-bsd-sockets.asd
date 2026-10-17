;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-bsd-sockets"
  :description "BSD-style sockets over System.Net.Sockets, shaped like SBCL's sb-bsd-sockets (built-in)."
  :version "1.0"
  :class require-system)
