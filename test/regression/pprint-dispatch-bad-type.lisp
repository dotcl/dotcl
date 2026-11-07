;;; A SET-PPRINT-DISPATCH entry whose type is malformed is refused, and one whose
;;; type is not defined matches nothing instead of making every print fail.

(deftest pprint-dispatch-bad-type.malformed
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (handler-case (progn (set-pprint-dispatch '(1 . 2) (lambda (s o) (declare (ignore s o)) nil)) :accepted)
      (error () :error)))
  :error)

(deftest pprint-dispatch-bad-type.printing-still-works
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (handler-case (set-pprint-dispatch '(1 . 2) (lambda (s o) (declare (ignore s o)) nil))
      (error () nil))
    (let ((*print-pretty* t)) (princ-to-string '(1 2))))
  "(1 2)")

(deftest pprint-dispatch-bad-type.undefined-type
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'pprint-dispatch-bad-type-undefined (lambda (s o) (declare (ignore s o)) nil))
    (let ((*print-pretty* t)) (princ-to-string '(1 2))))
  "(1 2)")
