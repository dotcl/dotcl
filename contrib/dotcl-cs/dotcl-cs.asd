;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "dotcl-cs"
  :description "Compile and embed C# code in dotcl via Roslyn (disassemble-cs + inline-cs macro)."
  :version "1.0"
  :class require-system)