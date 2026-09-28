;;; Where the compiler may rely on a return type it inferred itself.
;;;
;;; A DEFUN whose body ends in a declared FIXNUM (or DOUBLE-FLOAT, ...) has that
;;; return type inferred, and a call compiled against it can take the native
;;; path: unbox the result as that type instead of handling any object. The
;;; inference describes ONE definition. CLHS 3.2.2.3 lets a compiler assume
;;; which definition a call reaches in two cases only: a recursive call from
;;; the function's own body, and a call from the same file. Everywhere else the
;;; function may be redefined before the call runs, and a call compiled against
;;; the old definition's type would then fail on a value that is perfectly
;;; legal. So:
;;;
;;; - a redefinition is inferred afresh (the old type does not stick), and
;;; - an inferred type is used only by recursive calls and by calls in the same
;;;   COMPILE-FILE. At the REPL or in a source LOAD, a call to another function
;;;   is compiled without it.
;;;
;;; A declaimed FTYPE is the user's promise about every definition and is used
;;; everywhere; that is unchanged.

(setf dotcl:*save-sil* t)

(defun %irts-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %irts-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- redefinition at the REPL ----

;; The shape from the report: a FIXNUM definition, replaced by one returning a
;; bignum, and callers compiled after the replacement.
(defun %irts-f (x) (declare (fixnum x)) x)
(defun %irts-f (x) (* x most-positive-fixnum))
(defun %irts-g (y) (declare (fixnum y)) (logand (%irts-f y) y))
(defun %irts-g2 (y)
  (declare (fixnum y))
  (let ((z (%irts-f y))) (declare (ignorable z)) (logand z 255)))

(deftest inferred-return-type-scope.redefined-logand
  (%irts-g 3)
  #.(logand (* 3 most-positive-fixnum) 3))

(deftest inferred-return-type-scope.redefined-let
  (%irts-g2 3)
  #.(logand (* 3 most-positive-fixnum) 255))

;; The same with the definitions in the other order: a call compiled while the
;; function is still undefined-typed, then a FIXNUM redefinition, then a call.
(defun %irts-h (x) (list x))
(defun %irts-h (x) (declare (fixnum x)) x)
(defun %irts-h-use (y) (declare (fixnum y)) (+ (%irts-h y) 1))

(deftest inferred-return-type-scope.redefined-to-fixnum
  (%irts-h-use 41)
  42)

;; Multiple values: a type-inferred definition is replaced by one answering two
;; values. A later caller that asks for them all must see both.
(defun %irts-mv (x) (declare (fixnum x)) x)
(defun %irts-mv (x) (values x 2))
(defun %irts-mv-use (y) (let ((r (multiple-value-list (%irts-mv y)))) r))
(defun %irts-mv-first (y) (let ((a (%irts-mv y))) a))

(deftest inferred-return-type-scope.redefined-multiple-values
  (list (%irts-mv-use 1) (multiple-value-list (%irts-mv-first 5)))
  ((1 2) (5)))

;;; ---- what is still used ----

;; A recursive call reaches the definition it is in.
(defun %irts-fib (n)
  (declare (fixnum n))
  (if (< n 2) n (the fixnum (+ (%irts-fib (- n 1)) (%irts-fib (- n 2))))))

(deftest inferred-return-type-scope.recursive
  (%irts-fib 20)
  6765)

;; A declaimed type is used by any caller, and survives redefinition.
(declaim (ftype (function (fixnum) fixnum) %irts-decl))
(defun %irts-decl (x) (declare (fixnum x)) (+ x 1))
(defun %irts-decl-use (y) (declare (fixnum y)) (logand (%irts-decl y) y))

(deftest inferred-return-type-scope.declaimed
  (%irts-decl-use 6)
  6)

;;; ---- COMPILE-FILE ----

(defun %irts-compile-load (name text)
  (let ((src (regression-temp-file (concatenate 'string name ".lisp"))))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (write-string text o))
    (load (compile-file src))))

;; Within one file a call may rely on a sibling's inferred type (and the
;; recursive call keeps its native entry). A function redefined individually
;; afterwards is outside what 3.2.2.3 specifies; what dotcl does then is signal
;; an error at the old call, never answer with a wrong value.
;; A second file that calls the same function compiles without the type, so
;; it keeps working across the redefinition.
(deftest-emitting-only inferred-return-type-scope.compile-file-scope
  (progn
    (%irts-compile-load "irts-a"
      "(defun irts-cf-f (x) (declare (fixnum x)) x)
       (defun irts-cf-g (y) (declare (fixnum y)) (logand (irts-cf-f y) y))")
    (%irts-compile-load "irts-b"
      "(defun irts-cf-other (y) (declare (fixnum y)) (logand (irts-cf-f y) y))")
    (let ((before (list (irts-cf-g 7) (irts-cf-other 7))))
      (eval '(defun irts-cf-f (x) (* x most-positive-fixnum)))
      (list before
            (handler-case (progn (irts-cf-g 3) :no-error)
              (error () :error))
            (irts-cf-other 3))))
  ((7 7) :error #.(logand (* 3 most-positive-fixnum) 3)))

;;; ---- emitted code ----

;; A REPL call to another function's inferred FIXNUM: the result is not
;; unboxed (the parameter still is, by its own declaration).
(deftest-emitting-only inferred-return-type-scope.repl-call-is-generic
  (progn
    (eval '(defun %irts-fx (x) (declare (fixnum x)) x))
    (eval '(defun %irts-fx-use (y) (declare (fixnum y)) (logand (%irts-fx y) y)))
    (list (%irts-count "Invoke1) (UNBOX-FIXNUM" (%irts-sil #'%irts-fx-use))
          (%irts-fx-use 5)))
  (0 5))

;; The recursive calls are compiled against the inferred type: both results
;; are unboxed straight into int64 locals.
(deftest-emitting-only inferred-return-type-scope.recursive-call-is-native
  (%irts-count "(UNBOX-FIXNUM) (STLOC" (%irts-sil #'%irts-fib))
  2)

;; The declaimed type is used from the REPL.
(deftest-emitting-only inferred-return-type-scope.declaimed-call-is-native
  (%irts-count "Invoke1) (UNBOX-FIXNUM" (%irts-sil #'%irts-decl-use))
  1)
