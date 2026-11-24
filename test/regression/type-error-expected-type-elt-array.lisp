;;; Expected values come from SBCL. A TYPE-ERROR from ELT past the end names the range
;;; the index had to fall in ((integer 0 2) for a 3-element sequence, (integer 0 (0)) for an
;;; empty one), from CODE-CHAR a code type, and from a store into a typed array the element
;;; type. These used to have no expected type.

(defun %teet (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))))

(deftest type-error-expected-type-elt-array.elt
  (list (%teet (lambda () (elt '(1 2 3) 3)))
        (%teet (lambda () (elt (vector 1 2 3) 7)))
        (%teet (lambda () (elt nil 0)))
        (%teet (lambda () (elt (vector) 0)))
        (%teet (lambda () (elt "" 0)))
        (%teet (lambda () (setf (elt (list 1) 5) 1)))
        (%teet (lambda () (setf (elt (vector 1) 5) 1)))
        (%teet (lambda () (setf (elt "a" 5) #\a))))
  ((3 (integer 0 2)) (7 (integer 0 2)) (0 (integer 0 (0))) (0 (integer 0 (0))) (0 (integer 0 (0)))
   (5 (integer 0 0)) (5 (integer 0 0)) (5 (integer 0 0))))

(deftest type-error-expected-type-elt-array.typed-store
  (list (%teet (lambda () (setf (aref (make-array 2 :element-type 'fixnum) 0) 'a)))
        (%teet (lambda () (setf (aref (make-array 2 :element-type 'double-float) 0) 'a)))
        (%teet (lambda () (setf (aref (make-array 2 :element-type 'single-float) 0) 1)))
        (%teet (lambda () (setf (aref (make-array 2 :element-type 'character) 0) 1)))
        (%teet (lambda () (setf (aref (make-array '(2 2) :element-type 'fixnum) 0 0) 'x)))
        (%teet (lambda () (fill (make-array 2 :element-type 'fixnum) 'a)))
        (%teet (lambda () (replace (make-array 2 :element-type 'fixnum) '(a b))))
        (%teet (lambda () (setf (aref (make-array 2 :element-type '(unsigned-byte 8)) 0) 256))))
  ((a fixnum) (a double-float) (1 single-float) (1 character) (x fixnum) (a fixnum) (a fixnum)
   (256 (unsigned-byte 8))))

(deftest type-error-expected-type-elt-array.code-char
  (list (first (%teet (lambda () (code-char 'a))))
        (first (%teet (lambda () (code-char -1))))
        (first (%teet (lambda () (code-char 1.5)))))
  (a -1 1.5))
