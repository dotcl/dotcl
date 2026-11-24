;;; MAKE-SEQUENCE of a vector type with a specialised element type and no :INITIAL-ELEMENT fills the
;;; vector with an element of that type, instead of signalling a type-error.

(deftest make-sequence-specialised-default-element.floats
  (list (every (lambda (x) (typep x 'double-float)) (make-sequence '(vector double-float) 3))
        (every (lambda (x) (typep x 'single-float)) (make-sequence '(vector single-float 2) 2))
        (every (lambda (x) (typep x 'double-float)) (make-sequence '(simple-array double-float (*)) 2))
        (length (make-sequence '(vector double-float) 0)))
  (t t t 0))

(deftest make-sequence-specialised-default-element.chars-and-integers
  (list (every (lambda (x) (typep x 'character)) (make-sequence '(vector character) 2))
        (every (lambda (x) (typep x 'base-char)) (make-sequence '(vector base-char) 2))
        (every (lambda (x) (typep x '(integer 5 10))) (make-sequence '(vector (integer 5 10)) 2))
        (every (lambda (x) (typep x '(integer -5 -1))) (make-sequence '(vector (integer -5 -1)) 2))
        (every (lambda (x) (typep x '(member 1 2))) (make-sequence '(vector (member 1 2)) 2))
        (every (lambda (x) (typep x '(complex double-float))) (make-sequence '(vector (complex double-float)) 2)))
  (t t t t t t))

(deftest make-sequence-specialised-default-element.explicit-element-kept
  (list (coerce (make-sequence '(vector double-float) 2 :initial-element 1.5d0) 'list)
        (coerce (make-sequence '(vector (integer 5 10)) 2 :initial-element 7) 'list)
        (coerce (make-sequence '(vector t) 2 :initial-element nil) 'list)
        (coerce (make-sequence '(vector (or null integer)) 2 :initial-element nil) 'list))
  ((1.5d0 1.5d0) (7 7) (nil nil) (nil nil)))
