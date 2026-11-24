;;; PPRINT-TABULAR's tabsize is a non-negative integer (NIL is the default); anything else is a
;;; type-error.

(defmacro %ptt (form) `(handler-case (progn ,form :no-error) (type-error () :type-error)))

(deftest pprint-tabular-tabsize-type.bad
  (list (%ptt (pprint-tabular nil '(1 2) t nil 1.5))
        (%ptt (pprint-tabular nil '(1 2) t nil -1))
        (%ptt (pprint-tabular nil '(1 2) t nil "8"))
        (%ptt (pprint-tabular nil '(1 2) t nil #\a)))
  (:type-error :type-error :type-error :type-error))

(deftest pprint-tabular-tabsize-type.good
  (list (with-output-to-string (s) (pprint-tabular s '(a b) t nil 8))
        (with-output-to-string (s) (pprint-tabular s '(a b) t nil nil))
        (%ptt (pprint-tabular (make-broadcast-stream) '(a b) t nil 0)))
  ("(A       B)" "(A               B)" :no-error))
