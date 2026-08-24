;;; Regression: two same-named variables from DIFFERENT packages, captured by one
;;; closure, must stay distinct.
;;;
;;; The free-variable table used to be keyed by variable NAME string, so XP-A::V
;;; and XP-B::V shared one slot: the closure captured a single value and both
;;; references read it. The failure was silent — (list xp-a::v xp-b::v) returned
;;; (2 2) instead of (1 2) — because the env slot was interned into the compiler's
;;; own package, which the lookup rule treats as matching any package.
;;;
;;; Keying the table by symbol identity (and keying the env slot by the captured
;;; symbol itself) makes the two variables two captures.

(defpackage :xpc-a (:use))
(defpackage :xpc-b (:use))

(deftest cross-package-capture-list
  (let ((xpc-a::v 1))
    (let ((xpc-b::v 2))
      (funcall (lambda () (list xpc-a::v xpc-b::v)))))
  (1 2))

;;; Same, with the captures used in arithmetic (distinct env slots, not just
;;; distinct printed values).
(deftest cross-package-capture-arith
  (let ((xpc-a::w 10))
    (let ((xpc-b::w 20))
      (funcall (lambda () (+ xpc-a::w (* 100 xpc-b::w))))))
  2010)

;;; Mutation through one capture must not be visible through the other.
(deftest cross-package-capture-mutation
  (let ((xpc-a::m 1))
    (let ((xpc-b::m 1))
      (funcall (lambda () (setq xpc-a::m 99)))
      (list xpc-a::m xpc-b::m)))
  (99 1))

;;; Uninterned symbols with identical names are distinct variables (CL scoping is
;;; by symbol identity), so they must not share a capture either.
(deftest cross-package-capture-gensym
  (let* ((g1 (make-symbol "G"))
         (g2 (make-symbol "G")))
    (funcall (eval `(let ((,g1 :first))
                      (let ((,g2 :second))
                        (lambda () (list ,g1 ,g2)))))))
  (:first :second))

;;; A DEFVAR in one package must not make another package's same-named LEXICAL
;;; variable special.
;;;
;;; SPECIAL-VAR-P / GLOBAL-SPECIAL-P used to fall back to comparing SYMBOL-NAME
;;; strings, so once any package proclaimed QV special, every QV was treated as
;;; special. A parameter named QV was then bound dynamically and read
;;; dynamically, which agrees with itself while the binding is live but loses the
;;; value the moment a closure outlives the call: the closure skipped capture
;;; (globally special vars are read from the dynamic environment, not the env
;;; array) and hit "Unbound variable" when called later.

(defpackage :xpc-sp (:use :cl))
(defvar xpc-sp::qv 1)

(defun %xpc-thunk (qv) (lambda () qv))

(deftest cross-package-special-name-lexical
  (funcall (%xpc-thunk 42))
  42)

;;; The other side of the same rule: the symbol that really is special keeps
;;; dynamic semantics, i.e. a closure over it reads the innermost binding at call
;;; time rather than capturing the value.
(deftest cross-package-special-stays-dynamic
  (let ((f (lambda () xpc-sp::qv)))
    (list (funcall f)
          (let ((xpc-sp::qv 5)) (funcall f))
          (funcall f)))
  (1 5 1))
