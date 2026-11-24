;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-gpu"
  :description "Run a Lisp function over arrays on an ILGPU accelerator (GPU-MAP)."
  :version "0.1"
  :depends-on ("dotnet-nuget" "dotcl-thread")
  :class require-system)
