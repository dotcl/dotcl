;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name
;;; resolve.
(defsystem "asdf-parallel"
  :description "Perform an ASDF plan with several compile-ops at once, following the plan's dependencies: threads on dotcl, forked processes on SBCL."
  :version "0.1"
  :depends-on ("asdf")
  :class require-system)
