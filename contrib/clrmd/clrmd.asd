;;; require-system: loading is REQUIRE's job -- dotcl's module provider finds
;;; the fasl (or sil, or source) under contrib/. This file only makes the name resolve.
(defsystem "clrmd"
  :description "Walk the managed heap of the running process (or a dump) from Lisp via Microsoft.Diagnostics.Runtime (ClrMD): find live instances by type, read fields, trace GC roots. The 'observe' layer for live introspection."
  :version "0.1"
  :class require-system)