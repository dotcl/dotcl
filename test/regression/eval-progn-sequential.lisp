;;; (eval '(progn a b)) has to evaluate a and THEN b.
;;;
;;; The compiling evaluator handed the whole PROGN to the compiler as one unit,
;;; so b was compiled before a had run. Anything a installs at run time that b's
;;; compilation needs — a setf expander from DEFSETF, a symbol macro from
;;; DEFINE-SYMBOL-MACRO — was not there yet, and b compiled against no
;;; definition at all: (setf (place 1) 2) became a call to an undefined
;;; #'(setf place).
;;;
;;; The tree-walk interpreter always did this right, and the same expressions in
;;; a file are fine too (COMPILE-FILE splits a top level PROGN per CLHS 3.2.3.1),
;;; so the gap was EVAL-specific. These run in whichever evaluator the suite is
;;; using, and must pass in both.

;;; A setf expander defined and used inside one EVAL.

(deftest eval-progn-sequential.defsetf-then-use
  (eval '(progn
          (defsetf %eps-place (i) (v) `(list :set ,i ,v))
          (setf (%eps-place 1) 2)))
  (:set 1 2))

;;; Same for DEFINE-SETF-EXPANDER, which installs the expander by a different
;;; door.

(deftest eval-progn-sequential.define-setf-expander-then-use
  (eval '(progn
          (define-setf-expander %eps-place2 (i)
            (let ((store (gensym)))
              (values nil nil (list store)
                      `(list :set2 ,i ,store)
                      `(list :get2 ,i))))
          (setf (%eps-place2 3) 4)))
  4)

;;; And for a symbol macro, where the use is a plain variable reference that the
;;; compiler has to know is not a variable.

(deftest eval-progn-sequential.define-symbol-macro-then-use
  (eval '(progn
          (define-symbol-macro %eps-sym (list :sym 7))
          %eps-sym))
  (:sym 7))

;;; The ordinary meaning of PROGN is untouched.

(deftest eval-progn-sequential.empty-progn
  (eval '(progn))
  nil)

(deftest eval-progn-sequential.value-is-last-form
  (eval '(progn 1 2 3))
  3)

(deftest eval-progn-sequential.multiple-values-of-last-form
  (multiple-value-list (eval '(progn (values 1 2 3))))
  (1 2 3))

(deftest eval-progn-sequential.nested-progn-value
  (eval '(progn (progn 1 (progn 2 4))))
  4)

(defparameter *eps-order* nil)

(deftest eval-progn-sequential.side-effects-in-order
  (progn (setf *eps-order* nil)
         (eval '(progn (push :first *eps-order*) (push :second *eps-order*)))
         (reverse *eps-order*))
  (:first :second))
