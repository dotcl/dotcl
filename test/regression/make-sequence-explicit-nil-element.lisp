;;; An explicit :INITIAL-ELEMENT NIL is an element, not "no element": a string or a bit
;;; vector rejects it, a list or a T vector keeps it, and a valid element is stored.

(deftest make-sequence-explicit-nil-element.string-and-bit-vector-reject-nil
  (flet ((rejected (f) (handler-case (progn (funcall f) nil) (type-error () t))))
    (list (rejected (lambda () (make-sequence 'string 2 :initial-element nil)))
          (rejected (lambda () (make-sequence 'simple-string 2 :initial-element nil)))
          (rejected (lambda () (make-sequence 'base-string 2 :initial-element nil)))
          (rejected (lambda () (make-sequence '(string 2) 2 :initial-element nil)))
          (rejected (lambda () (make-sequence 'bit-vector 2 :initial-element nil)))
          (rejected (lambda () (make-sequence 'simple-bit-vector 2 :initial-element nil)))
          (rejected (lambda () (make-sequence 'string 2 :initial-element 5)))
          (rejected (lambda () (make-sequence 'bit-vector 2 :initial-element 'a)))))
  (t t t t t t t t))

(deftest make-sequence-explicit-nil-element.valid-and-default-elements
  (list (make-sequence 'string 3 :initial-element #\x)
        (coerce (make-sequence 'bit-vector 3 :initial-element 1) 'list)
        (make-sequence 'simple-base-string 2 :initial-element #\a)
        (make-sequence 'list 2 :initial-element nil)
        (coerce (make-sequence 'vector 2 :initial-element nil) 'list)
        (coerce (make-sequence '(vector (or null integer)) 2 :initial-element nil) 'list)
        (coerce (make-sequence '(vector (unsigned-byte 8)) 2 :initial-element 7) 'list)
        (length (make-sequence 'string 3))
        (length (make-sequence 'bit-vector 3))
        (make-sequence 'string 0 :initial-element #\z))
  ("xxx" (1 1 1) "aa" (nil nil) (nil nil) (nil nil) (7 7) 3 3 ""))
