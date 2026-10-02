;;; Under *PRINT-CIRCLE*, a structure printed more than once whose PRINT-OBJECT
;;; method calls CALL-NEXT-METHOD (Coalton's type structures do when
;;; *PRINT-READABLY* is true) prints as #1=#S(...) the first time and #1# after.
;;; The default method printing the object again found its label already
;;; written and printed #1=#1#, which does not read back (and reading it sent
;;; the reader into a loop).

(defstruct pccnm a)
(defmethod print-object ((x pccnm) s)
  (if *print-readably* (call-next-method) (format s "<pccnm ~a>" (pccnm-a x))))

(deftest print-circle-call-next-method.struct
  (let ((f (make-pccnm :a 1)))
    (with-standard-io-syntax
      (let ((*print-circle* t) (*package* (find-package :cl-user)))
        (list (prin1-to-string (list f f))
              (prin1-to-string (list f))))))
  ("(#1=#S(PCCNM :A 1) #1#)" "(#S(PCCNM :A 1))"))

(deftest print-circle-call-next-method.reads-back
  (let ((f (make-pccnm :a 2)))
    (with-standard-io-syntax
      (let* ((*print-circle* t) (*package* (find-package :cl-user))
             (back (read-from-string (prin1-to-string (list f f)))))
        (list (eq (first back) (second back)) (pccnm-a (first back))))))
  (t 2))
