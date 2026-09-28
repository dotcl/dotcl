;;; Under *PRINT-READABLY*, the default PRINT-OBJECT of a standard-object prints
;;; #<...>, which does not read back, so it must signal PRINT-NOT-READABLE
;;; (CLHS PRINT-UNREADABLE-OBJECT, 22.1.3). It printed "#<MOCK>" and returned.
;;; A user method that defers to CALL-NEXT-METHOD under *PRINT-READABLY*
;;; (utilities.print-items does) lands on the same default. Expected results are
;;; SBCL's.

(defclass %prso-plain () ())
(defclass %prso-deferring () ())
(defmethod print-object ((o %prso-deferring) stream)
  (if *print-readably*
      (call-next-method)
      (write-string "DEFERRING" stream)))

(defun %prso-try (thunk)
  (handler-case (funcall thunk)
    (print-not-readable (e)
      (list :not-readable (type-of (print-not-readable-object e))))))

(deftest print-readably-standard-object.default-method
  (%prso-try (lambda ()
               (let ((*print-readably* t))
                 (prin1-to-string (make-instance '%prso-plain)))))
  (:not-readable %prso-plain))

(deftest print-readably-standard-object.call-next-method
  (%prso-try (lambda ()
               (let ((*print-readably* t))
                 (prin1-to-string (make-instance '%prso-deferring)))))
  (:not-readable %prso-deferring))

(deftest print-readably-standard-object.write-keyword
  (%prso-try (lambda ()
               (write-to-string (make-instance '%prso-plain) :readably t)))
  (:not-readable %prso-plain))

;;; PRINC and ~A bind *PRINT-READABLY* to NIL, so they still print.
(deftest print-readably-standard-object.princ-unaffected
  (let ((*print-readably* t))
    (list (princ-to-string (make-instance '%prso-plain))
          (format nil "~a" (make-instance '%prso-deferring))))
  ("#<%PRSO-PLAIN>" "DEFERRING"))

;;; Without *PRINT-READABLY* nothing changes.
(deftest print-readably-standard-object.ordinary-printing
  (list (prin1-to-string (make-instance '%prso-plain))
        (prin1-to-string (make-instance '%prso-deferring)))
  ("#<%PRSO-PLAIN>" "DEFERRING"))
