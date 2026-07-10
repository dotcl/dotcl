(defsystem "clrmd"
  :description "Walk the managed heap of the running process (or a dump) from Lisp via Microsoft.Diagnostics.Runtime (ClrMD): find live instances by type, read fields, trace GC roots. The 'observe' layer for live introspection."
  :version "0.1"
  :components ((:file "clrmd")))
