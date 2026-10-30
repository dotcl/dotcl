;;; The type specifier T matches every object (CLHS SET-PPRINT-DISPATCH).

(deftest pprint-dispatch-t-type.default-entry
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
        (*print-pretty* t))
    (set-pprint-dispatch t
                         (lambda (s o)
                           (declare (ignore o))
                           (write-string "<DEFAULT>" s))
                         1)
    (prin1-to-string 42))
  "<DEFAULT>")
