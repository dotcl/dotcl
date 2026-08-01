;;; A required+&optional function with constant optional defaults now carries
;;; typed direct delegates for each concrete arity (in addition to the args-array
;;; XEP), so a fixed-arity call skips the InvokeSlow detour. The direct delegate
;;; for a shorter arity binds the omitted optionals to their defaults via a
;;; wrapping LET, so it must return exactly what the array XEP would. These tests
;;; pin the observable behaviour across every arity, default use/override, an
;;; early return-from in the body, and a self-recursive &optional (which is left
;;; on the array XEP — its direct body would need self-arg0 threading).

(defun od-two (a &optional (b 10) (c 100))
  (+ a b c))

(deftest od-two-required-only (od-two 1) 111)      ; b,c defaulted
(deftest od-two-one-optional  (od-two 1 2) 103)    ; c defaulted
(deftest od-two-all-present   (od-two 1 2 3) 6)

;; early return-from inside an &optional function (direct body wrapped in block)
(defun od-early (x &optional (tag :none))
  (when (minusp x) (return-from od-early (list :neg tag)))
  (list :pos tag x))

(deftest od-early-default   (od-early 5) (:pos :none 5))
(deftest od-early-override  (od-early 5 :hi) (:pos :hi 5))
(deftest od-early-return    (od-early -1) (:neg :none))
(deftest od-early-return-ov (od-early -1 :hi) (:neg :hi))

;; default is a quoted list constant
(defun od-quoted (a &optional (xs '(x y)))
  (cons a xs))

(deftest od-quoted-default  (od-quoted 0) (0 x y))
(deftest od-quoted-override (od-quoted 0 '(q)) (0 q))

;; self-recursive &optional: stays on the array XEP (still correct), sums 1..n
(defun od-selfrec (n &optional (acc 0))
  (if (<= n 0) acc (od-selfrec (1- n) (+ acc n))))

(deftest od-selfrec-default (od-selfrec 5) 15)
(deftest od-selfrec-seed    (od-selfrec 5 100) 115)

;; funcall / apply still reach the array XEP at every arity
(deftest od-funcall-short (funcall #'od-two 1) 111)
(deftest od-apply-full    (apply #'od-two '(1 2 3)) 6)
(deftest od-apply-mid     (apply #'od-two '(1 2)) 103)

;;; Non-constant &optional defaults also get direct delegates. The absent
;;; optionals are bound by a LET* inside the direct body, so the default form is
;;; evaluated at call time exactly where the args-array entry would evaluate it —
;;; including a special variable's current value and a default that reads an
;;; earlier parameter. Before this, only literal defaults qualified, which left
;;; the whole (fn x &optional (pkg *package*)) family on the slow path.

(defvar *od-dyn* :outer)
(defun od-dynamic-default (x &optional (tag *od-dyn*)) (list x tag))
(deftest od-dynamic-default-reads-binding-at-call-time
  (list (od-dynamic-default 1)
        (let ((*od-dyn* :inner)) (od-dynamic-default 1))
        (od-dynamic-default 1 :explicit))
  ((1 :outer) (1 :inner) (1 :explicit)))

(defun od-default-from-required (a &optional (b (* a 10)) (c (+ a b))) (list a b c))
(deftest od-default-sees-earlier-params
  (list (od-default-from-required 1)
        (od-default-from-required 1 2)
        (od-default-from-required 1 2 3))
  ((1 10 11) (1 2 3) (1 2 3)))

(defvar *od-calls* 0)
(defun od-count () (incf *od-calls*))
(defun od-side-effecting-default (x &optional (y (od-count))) (list x y))
(deftest od-default-evaluated-only-when-absent
  (let ((*od-calls* 0))
    (list (od-side-effecting-default :a)
          (od-side-effecting-default :b 99)
          *od-calls*))
  ((:a 1) (:b 99) 1))

;;; The hot case this was found for: (export syms &optional (pkg *package*)).
(deftest od-export-two-arg
  (let ((p (or (find-package "OD-EXPORT-TEST")
               (make-package "OD-EXPORT-TEST" :use '("CL")))))
    (let ((s (intern "OD-THING" p)))
      (export s p)
      (multiple-value-bind (sym status) (find-symbol "OD-THING" p)
        (list (eq sym s) status))))
  (t :external))

;;; &aux no longer forces the args-array path. It is not an argument-passing
;;; feature but sequential binding, so the direct path binds it with a LET* around
;;; the body — under any leading declarations, which belong to the parameters.

(defun aux-direct-basic (x &aux (y 10)) (+ x y))
(deftest aux-direct-basic-call
  (aux-direct-basic 5)
  15)

(defun aux-direct-chained (n &aux (b (* n 2)) (c (+ b 1))) (list n b c))
(deftest aux-direct-chained-defaults
  (aux-direct-chained 3)
  (3 6 7))

(defun aux-direct-no-init (x &aux y) (list x y))
(deftest aux-direct-no-init-is-nil
  (aux-direct-no-init 1)
  (1 nil))

;;; A leading (declare (special x)) still applies to the PARAMETER.
(defvar *aux-direct-dyn* :outer)
(defun aux-direct-special (*aux-direct-dyn* &aux (seen *aux-direct-dyn*))
  (declare (special *aux-direct-dyn*))
  (list seen (symbol-value '*aux-direct-dyn*)))
(deftest aux-direct-special-param
  (aux-direct-special :inner)
  (:inner :inner))

;;; A defun nested in a lexical scope goes through the other direct-body path.
(deftest aux-direct-nested-defun
  (let ((r (progn (defun aux-direct-nested (x &aux (y 10)) (declare (ignorable x)) (+ x y))
                  (aux-direct-nested 5))))
    r)
  15)

;;; ...and so does a LAMBDA with &aux.
(deftest aux-direct-lambda
  (funcall (lambda (x &aux (y 3)) (* x y)) 7)
  21)

;;; An early return-from still resolves when &aux is present.
(defun aux-direct-early (x &aux (y 10))
  (when (minusp x) (return-from aux-direct-early :negative))
  (+ x y))
(deftest aux-direct-early-return
  (list (aux-direct-early 5) (aux-direct-early -1))
  (15 :negative))
