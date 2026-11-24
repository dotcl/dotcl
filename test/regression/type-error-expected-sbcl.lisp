;;; The type-error of a built-in function names the type it expected, as SBCL does (the
;;; expected values below are SBCL's), instead of NIL.

(defmacro %tee (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-sbcl.isqrt
  (list (%tee (isqrt -1)) (%tee (isqrt 'a)) (%tee (isqrt 1.5)))
  ((-1 unsigned-byte) (a unsigned-byte) (1.5 unsigned-byte)))

(deftest type-error-sbcl.random
  (list (%tee (random 0)) (%tee (random -1)) (%tee (random 'a)) (%tee (random 0.0)))
  ((0 (or (float (0.0)) (integer 1))) (-1 (or (float (0.0)) (integer 1)))
   (a (or (float (0.0)) (integer 1))) (0.0 (or (float (0.0)) (integer 1)))))

(deftest type-error-sbcl.format
  (list (%tee (format nil 5)) (%tee (format nil nil)) (%tee (format nil 'a)))
  ((5 (or string function)) (nil (or string function)) (a (or string function))))

(deftest type-error-sbcl.make-array
  ;; SBCL: (OR LIST (UNSIGNED-BYTE 45)); the width of the integer type is the implementation's
  (loop for form in (list (%tee (make-array 'a)) (%tee (make-array -1)))
        collect (let ((ty (second form)))
                  (list (typep (first form) ty)
                        (subtypep 'cons ty)
                        (subtypep '(unsigned-byte 8) ty))))
  ((nil t t) (nil t t)))

(deftest type-error-sbcl.concatenate-unknown-type
  (let ((r (%tee (concatenate 'foo "a"))))
    (list (first r) (not (null (second r))) (typep (first r) (second r))))
  (foo t nil))

(deftest type-error-sbcl.length-dotted
  (list (%tee (length '(1 . 2))) (%tee (length '(1 2 . 3))) (%tee (length 5)))
  ((2 list) (3 list) (5 sequence)))

(deftest type-error-sbcl.bounding-indices
  (list (%tee (subseq "abc" 2 1)) (%tee (subseq '(1 2 3) 2 1)) (%tee (subseq (vector 1 2 3) 2 1))
        (%tee (subseq "abc" 1 9)) (%tee (position 1 '(1 2 3) :start 2 :end 1)))
  (((2 . 1) (cons (integer 0 3) (integer 2 3)))
   ((2 . 1) (cons (integer 0 3) (integer 2 3)))
   ((2 . 1) (cons (integer 0 3) (integer 2 3)))
   ((1 . 9) (cons (integer 0 3) (integer 1 3)))
   ((2 . 1) (cons (integer 0 3) (integer 2 3)))))

(deftest type-error-sbcl.funcall-nil-and-t-are-undefined-functions
  (list (%tee (funcall nil)) (%tee (funcall t)) (%tee (apply nil nil)) (%tee (mapcar nil '(1)))
        (handler-case (funcall nil) (undefined-function (e) (cell-error-name e)))
        (handler-case (funcall t) (undefined-function (e) (cell-error-name e)))
        (%tee (funcall 5)))
  ((:other undefined-function) (:other undefined-function) (:other undefined-function)
   (:other undefined-function) nil t (5 (or function symbol))))
