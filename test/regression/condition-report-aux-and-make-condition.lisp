;;; MAKE-CONDITION takes only a condition type, and the auxiliary PRINT-OBJECT methods of a
;;; condition with no :REPORT run around its default report (CLHS 7.6.6.2, 11.1.2.1.2).

(defclass cra-not-a-condition () ())
(define-condition cra-plain (error) ())

(define-condition cra-no-report (error) ())
(defmethod print-object :before ((c cra-no-report) s) (write-string "before " s))
(defmethod print-object :around ((c cra-no-report) s)
  (write-string "[" s) (call-next-method) (write-string "]" s))
(defmethod print-object :after ((c cra-no-report) s) (write-string " after" s))

(define-condition cra-with-report (error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "the report" s))))
(defmethod print-object :around ((c cra-with-report) s)
  (write-string "<" s) (call-next-method) (write-string ">" s))

(defun %cra-outcome (thunk)
  (handler-case (list :returned (type-of (funcall thunk)))
    (type-error (e) (list :type-error (type-error-datum e)))))

(deftest condition-report-aux.make-condition-rejects-non-conditions
  (list (%cra-outcome (lambda () (make-condition 'standard-object)))
        (%cra-outcome (lambda () (make-condition 'cra-not-a-condition)))
        (%cra-outcome (lambda () (make-condition 'cra-plain)))
        (%cra-outcome (lambda () (make-condition 'simple-error :format-control "x"))))
  ((:type-error standard-object) (:type-error cra-not-a-condition)
   (:returned cra-plain) (:returned simple-error)))

(deftest condition-report-aux.default-report-is-wrapped
  (list (princ-to-string (make-condition 'cra-no-report))
        (let ((text (prin1-to-string (make-condition 'cra-no-report))))
          (list (and (search "[before #<" text) t) (and (search " after]" text) t))))
  ("[before Condition COMMON-LISP-USER::CRA-NO-REPORT was signalled. after]" (t t)))

(deftest condition-report-aux.explicit-report-is-wrapped
  (princ-to-string (make-condition 'cra-with-report))
  "<the report>")
