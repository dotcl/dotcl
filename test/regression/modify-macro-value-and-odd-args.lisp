;;; INCF / DECF / a DEFINE-MODIFY-MACRO macro on a GETF (or LDB) place return the new value,
;;; not the plist the storing form returns; SETF / PSETQ / PSETF with an odd number of arguments and
;;; SHIFTF with one are PROGRAM-ERRORs at macroexpansion.

(define-modify-macro mvoa-incf (&optional (n 1)) +)

(deftest modify-macro-value-and-odd-args.getf-value
  (list (let ((pl (list :a 1))) (incf (getf pl :a)))
        (let ((pl (list :a 1))) (list (incf (getf pl :a) 10) (getf pl :a) pl))
        (let ((pl (list :a 1))) (list (decf (getf pl :a)) pl))
        (let ((pl (list :a 1))) (list (incf (getf pl :zz 0) 3) pl))
        (let ((pl (list :a 1))) (list (mvoa-incf (getf pl :a) 3) pl))
        (let ((x #x13)) (list (incf (ldb (byte 4 0) x) 2) x)))
  (2 (11 11 (:a 11)) (0 (:a 0)) (3 (:zz 3 :a 1)) (4 (:a 4)) (5 #x15)))

(deftest modify-macro-value-and-odd-args.other-places-unchanged
  (list (let ((l (list 1 2))) (list (incf (nth 1 l) 5) l))
        (let ((v (vector 1 2))) (list (incf (aref v 1) 5) (coerce v 'list)))
        (let ((h (make-hash-table))) (list (incf (gethash :a h 0) 5) (gethash :a h)))
        (let ((x 1)) (list (incf x) (decf x 5) x))
        (let ((pl (list :a 1))) (list (push 5 (getf pl :a)) pl)))
  ((7 (1 7)) (7 (1 7)) (5 5) (2 -3 -3) ((5 . 1) (:a (5 . 1)))))

(defmacro %mvoa-error-p (form)
  `(handler-case (progn (macroexpand-1 ',form) nil) (program-error () t)))

(deftest modify-macro-value-and-odd-args.odd-arguments
  (list (%mvoa-error-p (setf a))
        (%mvoa-error-p (setf a 1 b))
        (%mvoa-error-p (psetq a 1 b))
        (%mvoa-error-p (psetf a 1 b))
        (%mvoa-error-p (shiftf a)))
  (t t t t t))

(deftest modify-macro-value-and-odd-args.valid-forms
  (list (%mvoa-error-p (setf))
        (%mvoa-error-p (setf a 1))
        (%mvoa-error-p (setf a 1 b 2))
        (%mvoa-error-p (psetq))
        (%mvoa-error-p (psetf a 1 b 2))
        (%mvoa-error-p (shiftf a b)))
  (nil nil nil nil nil nil))
