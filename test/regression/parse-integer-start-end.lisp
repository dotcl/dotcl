;;; (PARSE-INTEGER string :start S :end E) reaches a direct runtime entry, so a
;;; single-value call allocates neither an argument array nor an MvReturn.

(defun %pise-call (s a b)
  (declare (simple-string s) (fixnum a b))
  (parse-integer s :start a :end b))

(defun %pise-loop (n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n s)
      (incf s (the fixnum (parse-integer "12345abc" :start 0 :end 5))))))

(deftest parse-integer-start-end.values
  (list (%pise-call "123abc" 0 3)
        (%pise-call "  42  " 0 6)
        (multiple-value-list (parse-integer "  -42x" :start 0 :end 5))
        (multiple-value-list (parse-integer "77x" :end 2 :start 0))
        (multiple-value-list (parse-integer "9999999999999999999999" :start 0 :end 22))
        (multiple-value-list (parse-integer "abc42" :start 3 :end 5))
        (multiple-value-list (parse-integer (make-array 3 :element-type 'character
                                                        :initial-contents "123")
                                            :start 0 :end 2)))
  (123 42 (-42 5) (77 2) (9999999999999999999999 22) (42 5) (12 2)))

(deftest parse-integer-start-end.generic-keywords
  (list (multiple-value-list (parse-integer "ff" :start 0 :end 2 :radix 16))
        (multiple-value-list (parse-integer "12x" :start 0 :end 3 :junk-allowed t))
        (multiple-value-list (parse-integer "12  " :start 0 :end 4))
        (multiple-value-list (parse-integer "  " :start 0 :end 2 :junk-allowed t)))
  ((255 2) (12 2) (12 4) (nil 2)))

(deftest parse-integer-start-end.errors
  (list (handler-case (parse-integer "12x" :start 0 :end 3) (error () :error))
        (handler-case (parse-integer "123" :start 5 :end 2) (error () :error))
        (handler-case (parse-integer 5 :start 0 :end 2) (type-error () :type-error))
        (handler-case (parse-integer "123" :start :bad :end 2) (type-error () :type-error))
        (handler-case (parse-integer "123" :start 0 :end :bad) (type-error () :type-error)))
  (:error :error :type-error :type-error :type-error))

(deftest-compiled-only parse-integer-start-end.loop-and-no-allocation
  (progn
    (%pise-loop 1000)
    (list (%pise-loop 10)
          (< (min-allocated-bytes (lambda () (%pise-loop 10000)) 5) 80000)))
  (123450 t))
