;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-lock"
  :description "Experimental: load the systems a lock file names, putting conflicting versions in package units."
  :version "0.1"
  :depends-on ("asdf" "asdf-version-range")
  :class require-system)
