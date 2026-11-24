;;; Expected values come from SBCL, except the two (vector t N) forms in the .length test:
;;; compiled, SBCL 2.6.8 signals a simple-error for those (evaluated, a type-error with the
;;; same datum and expected-type as here); dotcl signals a type-error either way. COERCE into a typed vector signals a TYPE-ERROR whose
;;; datum is the offending element and whose expected-type is the element type; a length
;;; mismatch reports the length and (eql N).

(defun %tecoerce (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))))

(deftest type-error-expected-type-coerce.element
  (list (%tecoerce (lambda () (coerce '(1 2) 'string)))
        (%tecoerce (lambda () (coerce '(1) '(vector character))))
        (%tecoerce (lambda () (coerce '(a) '(vector fixnum))))
        (%tecoerce (lambda () (coerce '(300) '(vector (unsigned-byte 8)))))
        (%tecoerce (lambda () (coerce '(a) '(vector double-float))))
        (%tecoerce (lambda () (coerce "abc" 'bit-vector)))
        (%tecoerce (lambda () (coerce 'a 'float))))
  ((1 character) (1 character) (a fixnum) (300 (unsigned-byte 8)) (a double-float)
   (#\a bit) (a real)))

(deftest type-error-expected-type-coerce.length
  (list (%tecoerce (lambda () (coerce '(1 2) '(vector t 3))))
        (%tecoerce (lambda () (coerce '(1 2 3) '(vector t 2))))
        (%tecoerce (lambda () (coerce "ab" '(string 3))))
        (%tecoerce (lambda () (coerce 5 '(satisfies evenp)))))
  ((2 (eql 3)) (3 (eql 2)) (2 (eql 3)) (5 (satisfies evenp))))
