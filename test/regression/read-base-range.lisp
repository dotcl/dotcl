;;; *READ-BASE* is an integer from 2 to 36 (CLHS 2.2); reading with another value
;;; is a type-error instead of reading in that base (37) or as decimal (1).

(defun %rbr-read (base string)
  (handler-case (let ((*read-base* base)) (values (read-from-string string)))
    (type-error () :type-error)))

(deftest read-base-range.out-of-range
  (list (%rbr-read 1 "10") (%rbr-read 0 "10") (%rbr-read 37 "10") (%rbr-read -2 "10")
        (%rbr-read 2.0 "10") (%rbr-read nil "10"))
  (:type-error :type-error :type-error :type-error :type-error :type-error))

(deftest read-base-range.limits
  (list (%rbr-read 2 "101") (%rbr-read 36 "zz") (%rbr-read 16 "ff") (%rbr-read 10 "10"))
  (5 1295 255 10))

(deftest read-base-range.restored
  (progn (%rbr-read 37 "10") (values (read-from-string "10")))
  10)
