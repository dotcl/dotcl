;;; A pprint dispatch entry whose type specifier is not a known type matches nothing; printing and
;;; PPRINT-DISPATCH still work with the entries that are valid.

(defun %pdu-fn (s o) (declare (ignore o)) (write-string "F" s))

(deftest pprint-dispatch-unknown-type-entry.lookup
  (let ((tb (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'pdu-no-such-type #'%pdu-fn 0 tb)
    (set-pprint-dispatch 'integer #'%pdu-fn 0 tb)
    (list (multiple-value-bind (f found) (pprint-dispatch 5 tb) (list (eq f #'%pdu-fn) found))
          (nth-value 1 (pprint-dispatch "s" tb))))
  ((t t) nil))

(deftest pprint-dispatch-unknown-type-entry.print
  (let ((tb (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'pdu-no-such-type #'%pdu-fn 0 tb)
    (set-pprint-dispatch 'integer #'%pdu-fn 0 tb)
    (let ((*print-pprint-dispatch* tb) (*print-pretty* t))
      (list (write-to-string 5) (write-to-string "s") (write-to-string '(a b)))))
  ("F" "\"s\"" "(A B)"))
