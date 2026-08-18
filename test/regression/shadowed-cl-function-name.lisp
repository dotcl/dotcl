;;; A package that SHADOWs a COMMON-LISP function name and defines its own
;;; function on the shadowing symbol.
;;;
;;; Bug: an unqualified compiled call emits (:load-sym-fn NAME PKG), and
;;; Startup.SymFn resolved NAME against the CL package BEFORE the call site's
;;; own package. Any name CL also has — and CL has FIRST, REST, NTH, CONSP,
;;; SECOND, ATOM, NULL, UNION, … — therefore reached the CL function, and the
;;; package's own definition was unreachable from inside that package. The
;;; symbol's function cell was set correctly, so (symbol-function 'pkg::nth)
;;; returned the right function while (nth ...) and #'nth did not: the call
;;; silently ran CL:NTH on an object of the package's own type and returned
;;; NIL (or signalled a type error deep inside the callee).
;;;
;;; concrete-syntax-tree shadows exactly this set, which is how coalton hit it.
;;;
;;; Fix: SymFn asks the call site's own package first. A package that merely
;;; uses CL finds the inherited CL symbol there, so nothing changes for it.

(defpackage #:scfn (:use #:cl) (:shadow #:nth #:union #:first #:consp))
(defpackage #:scfn-gf (:use #:cl) (:shadow #:second #:rest))

(defun scfn::nth (n l) (declare (ignore n l)) :scfn-nth)
(defun scfn::union (a b) (declare (ignore a b)) :scfn-union)
(defun scfn::first (l) (declare (ignore l)) :scfn-first)
(defun scfn::consp (x) (declare (ignore x)) :scfn-consp)

(defun %scfn-compile-in-package (package-name string)
  "Compile STRING read in PACKAGE-NAME, with *PACKAGE* bound there so the call
   sites compile the way they would inside that package's own source file."
  (let ((*package* (find-package package-name)))
    (compile nil (read-from-string string))))

;;; An unqualified call inside the shadowing package reaches that package's
;;; definition, not CL's.
(deftest shadowed-cl-function-name.unqualified-call-uses-own-definition
  (funcall (%scfn-compile-in-package
            "SCFN"
            "(lambda () (list (nth 1 '(a b)) (union '(1) '(2))
                              (first '(a b)) (consp '(a))))"))
  (:scfn-nth :scfn-union :scfn-first :scfn-consp))

;;; #'name resolves the same way as the call.
(deftest shadowed-cl-function-name.function-special-form-uses-own-definition
  (list (funcall (funcall (%scfn-compile-in-package "SCFN" "(lambda () #'nth)")) 1 '(a b))
        (funcall (funcall (%scfn-compile-in-package "SCFN" "(lambda () #'first)")) '(a b)))
  (:scfn-nth :scfn-first))

;;; ... and so does SYMBOL-FUNCTION (this half always worked).
(deftest shadowed-cl-function-name.symbol-function-uses-own-definition
  (list (funcall (symbol-function 'scfn::nth) 1 '(a b))
        (funcall (symbol-function 'scfn::first) '(a b)))
  (:scfn-nth :scfn-first))

;;; The CL functions themselves are untouched — the shadowing package's
;;; definitions must not leak into calls that name the CL symbols.
(deftest shadowed-cl-function-name.cl-functions-unaffected
  (list (nth 1 '(a b)) (first '(a b)) (consp '(a))
        (funcall (%scfn-compile-in-package "CL-USER" "(lambda () (nth 1 '(a b)))")))
  (b a t b))

;;; The shape concrete-syntax-tree actually uses: a generic function on a
;;; shadowed name, called from a method body in the same package. The reader
;;; methods worked before the fix (accessor call sites are package-qualified);
;;; the DEFGENERIC ones did not.
(deftest shadowed-cl-function-name.generic-function-on-shadowed-name
  (progn
    (funcall (%scfn-compile-in-package
              "SCFN-GF"
              "(lambda ()
                 (defclass node () ((a :initarg :a :reader node-a)
                                    (d :initarg :d :reader node-d)))
                 (defgeneric rest (x) (:method ((x node)) (node-d x)))
                 (defgeneric second (x) (:method ((x node)) (node-a (rest x)))))"))
    (let ((node (funcall (%scfn-compile-in-package
                          "SCFN-GF"
                          "(lambda () (make-instance 'node :a 1
                                       :d (make-instance 'node :a 2 :d nil)))"))))
      (funcall (%scfn-compile-in-package "SCFN-GF" "(lambda (n) (second n))") node)))
  2)
