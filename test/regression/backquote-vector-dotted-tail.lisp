;;; A vector in the dotted tail of a backquoted list is a template of its own
;;; (CLHS 2.4.6): its , and ,@ are processed like those of a vector element.
;;; It used to be quoted as a literal, leaving the UNQUOTE forms inside it.
;;; eclector's quasiquote test compares its expansion with the host's on
;;; random templates and hit this.

(defparameter *bqv-x* 3)

(defun %bqv-shape (x)
  "X with every vector turned into (:VECTOR . elements), so EQUAL can compare."
  (cond ((consp x) (cons (%bqv-shape (car x)) (%bqv-shape (cdr x))))
        ((and (vectorp x) (not (stringp x)))
         (cons :vector (map 'list #'%bqv-shape x)))
        (t x)))

(deftest backquote-vector-dotted-tail.splice
  (%bqv-shape `(a . #(,@'(1 2))))
  (a :vector 1 2))

(deftest backquote-vector-dotted-tail.unquote
  (%bqv-shape `(a . #(,*bqv-x* x)))
  (a :vector 3 x))

(deftest backquote-vector-dotted-tail.after-splice
  (%bqv-shape `(a b . #(,@(list 1 2) 3)))
  (a b :vector 1 2 3))

(deftest backquote-vector-dotted-tail.fresh
  (flet ((f () `(a . #(,*bqv-x*))))
    (eq (cdr (f)) (cdr (f))))
  nil)

(deftest backquote-vector-dotted-tail.constant
  (%bqv-shape `(a . #(1 2)))
  (a :vector 1 2))

(deftest backquote-vector-dotted-tail.nested-vector
  (%bqv-shape `(a . #(#(,*bqv-x*) 1)))
  (a :vector (:vector 3) 1))
