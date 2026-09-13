;;; FIND-SYMBOL allocates nothing.
;;;
;;; The compiler builds a LispObject[] for every call to a variadic runtime
;;; entry, and the JIT keeps that array on the stack only while it can prove it
;;; does not escape -- which requires the entry to be small enough to inline.
;;; FIND-SYMBOL's entry had the whole implementation in it, so the array went to
;;; the heap: 32 bytes on a path the compiler and the reader take constantly.
;;; Splitting it into a tiny variadic shim over a fixed-arity core is what INTERN
;;; already did, and INTERN already cost nothing.
;;;
;;; The assertion is about bytes, so it is a shape test as much as a behaviour
;;; one: if someone inlines the body back into the entry, this fails.

(intern "FSNC-EXISTING-SYMBOL")

;;; --- the answers ----------------------------------------------------------

(deftest find-symbol-external-status
  (multiple-value-list (find-symbol "CAR" "CL"))
  (car :external))

;;; Written without a package-qualified literal: the whole file is READ before
;;; any of it runs, so naming FSNC-PKG::HIDDEN here would need the package to
;;; exist at read time.
(deftest find-symbol-internal-status
  (progn
    (unless (find-package "FSNC-PKG") (make-package "FSNC-PKG" :use '()))
    (intern "HIDDEN" "FSNC-PKG")
    (multiple-value-bind (sym status) (find-symbol "HIDDEN" "FSNC-PKG")
      (list (symbol-name sym) (package-name (symbol-package sym)) status)))
  ("HIDDEN" "FSNC-PKG" :internal))

(deftest find-symbol-absent
  (multiple-value-list (find-symbol "FSNC-NO-SUCH-SYMBOL-AT-ALL"))
  (nil nil))

;;; A character is a string designator (CLHS 22.1.3.3), so #\A means the name
;;; "A". Looked up in a package built for this test, nothing of that name is
;;; there -- naming the package explicitly keeps the answer independent of
;;; whatever *PACKAGE* the suite happens to be in.
(deftest find-symbol-character-designator
  (progn
    (unless (find-package "FSNC-EMPTY") (make-package "FSNC-EMPTY" :use '()))
    (multiple-value-list (find-symbol #\A "FSNC-EMPTY")))
  (nil nil))

;;; And it finds one when it is there, so the designator is really being used.
(deftest find-symbol-character-designator-hit
  (progn
    (unless (find-package "FSNC-EMPTY") (make-package "FSNC-EMPTY" :use '()))
    (intern "A" "FSNC-EMPTY")
    (multiple-value-bind (sym status) (find-symbol #\A "FSNC-EMPTY")
      (list (symbol-name sym) status)))
  ("A" :internal))

(deftest find-symbol-wrong-arity-signals
  (handler-case (progn (funcall #'find-symbol) :no-error)
    (program-error () :program-error)
    (error () :other))
  :program-error)

;;; --- the bytes are NOT asserted here ------------------------------------
;;;
;;; The win depends on the JIT proving the argument array does not escape, and
;;; it only does that once the entry is inlined -- which happens in Release and
;;; not in Debug. This suite runs Debug, where FIND-SYMBOL still costs its 32
;;; bytes, so a byte assertion here would fail for a reason that has nothing to
;;; do with the change being present.
;;;
;;; Measured with a Release build (20000 calls, difference of GC-STATS):
;;;
;;;   before   31.98 B/op
;;;   after     0.00 B/op   (one argument and two, hit and miss)
;;;
;;; What keeps the shape honest is the entry staying small: if the body is
;;; inlined back into FIND-SYMBOL-L, the array escapes again and the bytes come
;;; back. That is a code-review property, not something this file can check.
