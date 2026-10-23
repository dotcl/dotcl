;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "asdf-version-range"
  :description "Version ranges in ASDF :version dependencies, such as (:version \"foo\" (:^ \"1.2\"))."
  :version "0.1"
  :depends-on ("asdf")
  :class require-system)
