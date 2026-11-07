;;; A PRINT-OBJECT method of the program on a built-in class, or EQL to a value,
;;; is the one the printer uses (CLHS 22.1.2). Each case runs in its own process
;;; state through a method defined and removed inside the test.

(defclass pob-c () ())

(defvar *pob-o* (make-instance 'pob-c))

(deftest print-object-builtin.integer
  (progn
    (defmethod print-object ((x integer) stream) (write-string "#<INT>" stream))
    (unwind-protect
        (list (prin1-to-string 42) (format nil "~S ~A" 1 2) (prin1-to-string (list 7))
              (prin1-to-string 1.5) (format nil "~D" 42))
      (remove-method #'print-object (find-method #'print-object nil (list (find-class 'integer) (find-class t))))))
  ("#<INT>" "#<INT> #<INT>" "(#<INT>)" "1.5" "42"))

(deftest print-object-builtin.after-removal
  (prin1-to-string 42)
  "42")

(deftest print-object-builtin.string-and-character
  (progn
    (defmethod print-object ((x string) stream) (write-string "#<STR>" stream))
    (defmethod print-object ((x character) stream) (write-string "#<CHR>" stream))
    (unwind-protect (list (prin1-to-string "ab") (prin1-to-string #\a))
      (remove-method #'print-object (find-method #'print-object nil (list (find-class 'string) (find-class t))))
      (remove-method #'print-object (find-method #'print-object nil (list (find-class 'character) (find-class t))))))
  ("#<STR>" "#<CHR>"))

(deftest print-object-builtin.call-next-method
  (progn
    (defmethod print-object ((x integer) stream)
      (write-string "<" stream) (call-next-method) (write-string ">" stream))
    (unwind-protect (prin1-to-string (list 4 5))
      (remove-method #'print-object (find-method #'print-object nil (list (find-class 'integer) (find-class t))))))
  "(<4> <5>)")

(deftest print-object-builtin.eql-number
  (progn
    (defmethod print-object ((x (eql 43)) stream) (write-string "#<FORTY-THREE>" stream))
    (unwind-protect (list (prin1-to-string 43) (prin1-to-string 44))
      (remove-method #'print-object (find-method #'print-object nil (list '(eql 43) (find-class t))))))
  ("#<FORTY-THREE>" "44"))

(deftest print-object-builtin.eql-instance
  (progn
    (eval `(defmethod print-object ((x (eql ,*pob-o*)) stream) (write-string "#<SPECIAL>" stream)))
    (list (prin1-to-string *pob-o*)
          (prin1-to-string (make-instance 'pob-c))
          (let ((*print-circle* t)) (prin1-to-string *pob-o*))))
  ("#<SPECIAL>" "#<POB-C>" "#<SPECIAL>"))
