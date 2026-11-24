;;; READ-FROM-STRING of a plain decimal integer takes a shortcut that must not change an answer.

(deftest rfs-plain-integer.values
  (list (multiple-value-list (read-from-string "12345"))
        (multiple-value-list (read-from-string "-7"))
        (multiple-value-list (read-from-string "+42"))
        (multiple-value-list (read-from-string "007"))
        (multiple-value-list (read-from-string "0"))
        (multiple-value-list (read-from-string "123456789012345678"))
        (multiple-value-list (read-from-string "1234567890123456789012")))
  ((12345 5) (-7 2) (42 3) (7 3) (0 1) (123456789012345678 18) (1234567890123456789012 22)))

(deftest rfs-plain-integer.other-forms-unchanged
  (list (multiple-value-list (read-from-string "12345 "))
        (multiple-value-list (read-from-string "1.5"))
        (multiple-value-list (read-from-string "1/2"))
        (multiple-value-list (read-from-string "+"))
        (multiple-value-list (read-from-string "-"))
        (multiple-value-list (read-from-string "12a"))
        (multiple-value-list (read-from-string "12345" nil nil))
        (multiple-value-list (read-from-string "12345" t nil :start 2))
        (multiple-value-list (read-from-string "12345" t nil :end 3)))
  ((12345 6) (1.5 3) (1/2 3) (+ 1) (- 1) (|12A| 3) (12345 5) (345 5) (123 3)))

(deftest rfs-plain-integer.read-base
  (list (let ((*read-base* 16)) (read-from-string "1F"))
        (let ((*read-base* 16)) (read-from-string "10"))
        (let ((*read-base* 8)) (read-from-string "17"))
        (read-from-string "10"))
  (31 16 15 10))

(deftest rfs-plain-integer.read-suppress
  (list (let ((*read-suppress* t)) (read-from-string "12345")) (read-from-string "12345"))
  (nil 12345))

(deftest rfs-plain-integer.readtable-with-digit-macro
  ;; A readtable whose digit is a macro character: the shortcut stays out of the way.
  (let ((rt (copy-readtable)))
    (set-macro-character #\7 (lambda (stream char) (declare (ignore stream char)) :seven) nil rt)
    (list (let ((*readtable* rt)) (read-from-string "7"))
          (let ((*readtable* rt)) (read-from-string "12"))
          (read-from-string "7")
          (let ((*readtable* rt)) (read-from-string "7"))))
  (:seven 12 7 :seven))

(deftest rfs-plain-integer.same-readtable-changed-after-use
  (let ((rt (copy-readtable)) (r '()))
    (let ((*readtable* rt))
      (push (read-from-string "7") r)
      (set-macro-character #\7 (lambda (stream char) (declare (ignore stream char)) :seven) nil)
      (push (read-from-string "7") r)
      (set-syntax-from-char #\7 #\7 rt (copy-readtable nil))
      (push (read-from-string "7") r))
    (nreverse r))
  (7 :seven 7))
