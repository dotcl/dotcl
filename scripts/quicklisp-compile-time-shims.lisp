;;;; Compile-time shims for bundling the quicklisp client as a single file.
;;;; Emitted between the client's package.lisp and its remaining components.
;;;;
;;;; The client is normally built by ASDF with :serial t, where each file is
;;;; LOADed before the next is compiled — so a plain toplevel DEFVAR in file N
;;;; already has its value when file N+1 is macroexpanded. Concatenating the
;;;; components into one COMPILE-FILE removes that: toplevel forms get compiled,
;;;; not executed, and a macro expander that reads such a variable at expansion
;;;; time finds it unbound.
;;;;
;;;; ql-impl:definterface is the one expander that does this — it records each
;;;; interface in *interfaces* while expanding. Binding the variable here, at
;;;; compile time, is enough: the client's own
;;;;
;;;;   (defvar *interfaces* (make-hash-table) "...")
;;;;
;;;; then finds it already bound and leaves the value alone, which is exactly
;;;; what DEFVAR is specified to do.
;;;;
;;;; Keeping this on the dotcl side is deliberate. The need comes from how dotcl
;;;; bundles the client, not from a defect in it, so the shipped branch stays
;;;; identical to what was submitted upstream.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar ql-impl::*interfaces* (make-hash-table)))
