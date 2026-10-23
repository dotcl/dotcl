;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name
;;; resolve.
(defsystem "asdf-extra-deps"
  :description "Add the dependencies a system uses but does not declare, from a supplement file, without editing its .asd."
  :version "0.1"
  :depends-on ("asdf")
  :class require-system)
