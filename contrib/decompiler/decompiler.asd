;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "decompiler"
  :description "Decompile a .NET assembly back to readable C# from Lisp, via ICSharpCode.Decompiler (the ILSpy engine). The 'read' layer for inspecting live/on-disk assemblies whose source is unavailable."
  :version "0.1"
  :class require-system)