;;; Regression: a closure body is a separate CLR method with its own locals, so
;;; the per-compilation tables that describe the ENCLOSING body's slots must not
;;; decide anything inside it.
;;;
;;; The compile-state registry rebinds those tables around every closure body,
;;; but only the tables declared to participate. The native-representation and
;;; proven-range tables were split across the two declaration forms — the Int64
;;; one participated while its double/single analogs did not — so the invariant
;;; held by accident (entries are pinned to a slot key, and the closure body's
;;; fresh locals never re-derive an outer key) rather than by the reset.
;;;
;;; These cases pin the behavior from the outside: an inner binding that shares
;;; a name with an outer declared variable must be compiled from its own type,
;;; and a captured declared variable must still read correctly.

(defpackage :cbd-a (:use))
(defpackage :cbd-b (:use))

;;; A closure parameter shadowing a fixnum-declared outer local: the parameter
;;; is not declared, so a non-fixnum argument must survive unmolested.
(deftest closure-param-shadows-fixnum-decl
  (let ((n 5))
    (declare (fixnum n))
    (let ((f (lambda (n) (list n (+ n 1)))))
      (list (funcall f 2.5d0) n)))
  ((2.5d0 3.5d0) 5))

;;; Same, but the shadowing binding is a LET inside the closure body.
(deftest closure-inner-let-shadows-fixnum-decl
  (let ((n 5))
    (declare (fixnum n))
    (let ((f (lambda () (let ((n 2.5d0)) (* n 2)))))
      (funcall f)))
  5.0d0)

;;; double-float declaration (native r8 slot) vs a closure parameter of the
;;; same name bound to a non-number.
(deftest closure-param-shadows-double-decl
  (let ((d 1.5d0))
    (declare (double-float d))
    (let ((f (lambda (d) (list d))))
      (list (funcall f "x") d)))
  (("x") 1.5d0))

;;; The mirror case: an integer binding inside the closure body while an outer
;;; native double slot of the same name is live.
(deftest closure-inner-let-shadows-double-decl
  (let ((d 1.5d0))
    (declare (double-float d))
    (let ((f (lambda () (let ((d 3)) (* d 2)))))
      (list (funcall f) d)))
  (6 1.5d0))

;;; single-float (native r4 slot) analog.
(deftest closure-param-shadows-single-decl
  (let ((s 1.5f0))
    (declare (single-float s))
    (let ((f (lambda (s) (list s))))
      (list (funcall f :sym) s)))
  ((:SYM) 1.5f0))

;;; Cross-package: the shadowing binding is a different symbol that only shares
;;; a print name with the declared one.
(deftest closure-param-cross-package-shadow
  (let ((cbd-a::v 4))
    (declare (fixnum cbd-a::v))
    (let ((f (lambda (cbd-b::v) (list cbd-b::v))))
      (list (funcall f 1.25d0) (+ cbd-a::v 1))))
  ((1.25d0) 5))

;;; Two closure levels deep — the reset must hold at every boundary, not just
;;; the outermost one.
(deftest nested-closure-param-shadows-fixnum-decl
  (let ((v 10))
    (declare (fixnum v))
    (let ((f (lambda ()
               (let ((g (lambda (v) (list v))))
                 (funcall g 1.25d0)))))
      (list (funcall f) v)))
  ((1.25d0) 10))

;;; The opposite direction: capturing a declared variable must keep working —
;;; the closure body reads it through its env slot and the declaration still
;;; describes the value it holds.
(deftest closure-captures-fixnum-declared-var
  (let ((n 7))
    (declare (fixnum n))
    (let ((f (lambda () (+ n 1))))
      (funcall f)))
  8)

(deftest closure-captures-double-declared-var
  (let ((d 2.5d0))
    (declare (double-float d))
    (let ((f (lambda () (* d 2))))
      (funcall f)))
  5.0d0)
