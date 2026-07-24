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
