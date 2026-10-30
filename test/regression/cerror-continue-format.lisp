;;; CERROR formats its continue format control lazily, like FORMAT.

(define-condition cerror-continue-format-probe (error)
  ((x :initarg :x :initform 1)))

(defun %continue-report (thunk)
  (block done
    (handler-bind ((error
                    (lambda (c)
                      (let ((r (find-restart 'continue c)))
                        (when r
                          (return-from done (princ-to-string r)))))))
      (funcall thunk)
      :no-condition)))

(deftest cerror-continue-format.string-control
  (%continue-report (lambda () (cerror "Continue ~a" 'cerror-continue-format-probe :x 2)))
  "Continue X")

(deftest cerror-continue-format.function-control
  (%continue-report (lambda ()
                      (cerror (formatter "Continue ~a")
                              'cerror-continue-format-probe :x 2)))
  "Continue X")

(deftest cerror-continue-format.format-error
  (handler-case
      (%continue-report
       (lambda ()
         (cerror "Continue ~*~a" (make-condition 'cerror-continue-format-probe) 4)))
    (error () :format-error))
  :format-error)
