;;; cons.lisp — Tests from ansi-test/cons/{cons,consp,atom,list}.lsp
;;; Excluded: def-fold-test, *universe*, check-predicate,
;;;           notnot-mv, macrolet, check-type-predicate
;;; Note: signals-error type-error tests now in car-cdr.lisp

;;; --- cons ---

(deftest cons-of-symbols
  (cons 'a 'b)
  (a . b))

(deftest cons-with-nil
  (cons 'a nil)
  (a))

(deftest cons-eq-equal
  (let ((x (cons 'a 'b))
        (y (cons 'a 'b)))
    (and (not (eqt x y))
         (equalt x y)))
  t)

(deftest cons-equal-list
  (equalt (cons 'a (cons 'b (cons 'c nil)))
          (list 'a 'b 'c))
  t)

(deftest cons.order.1
  (let ((i 0)) (values (cons (incf i) (incf i)) i))
  (1 . 2) 2)

;;; --- consp ---

(deftest consp-list
  (notnot (consp '(a)))
  t)

(deftest consp-cons
  (notnot (consp (cons nil nil)))
  t)

(deftest consp-nil
  (consp nil)
  nil)

(deftest consp-empty-list
  (consp (list))
  nil)

(deftest consp-single-element-list
  (notnot (consp (list 'a)))
  t)

(deftest consp.order.1
  (let ((i 0))
    (values (consp (incf i)) i))
  nil 1)

;;; --- atom ---

(deftest atom.order.1
  (let ((i 0))
    (values (atom (progn (incf i) '(a b))) i))
  nil 1)

;;; --- list ---

(deftest list.1
  (list 'a 'b 'c)
  (a b c))

(deftest list.2
  (list)
  nil)

(deftest list.order.1
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4))

;;; --- list* ---

(deftest list*.1
  (list* 1 2 3)
  (1 2 . 3))

(deftest list*.2
  (list* 'a)
  a)

(deftest list-list*.1
  (list* 'a 'b 'c (list 'd 'e 'f))
  (a b c d e f))

(deftest list*.3
  (list* 1)
  1)

(deftest list*.order.1
  (let ((i 0))
    (list* (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 . 4))

;;; --- push ---
;;; PUSH.5: push into a macrolet-expanded place
(deftest push.5
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values
      (push 1 (%m x))
      x)))
  (1) (1))

(do-tests-summary)
