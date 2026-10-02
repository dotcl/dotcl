;;; A condition signalled by the runtime (ERROR with a format control, a
;;; TYPE-ERROR from CAR, ...) prints as #<TYPE: message> with its own type,
;;; as TYPE-OF reports it. It printed as #<ERROR: message> whatever the type,
;;; so a test that looks for the type in the printed condition (prove's
;;; IS-ERROR report) did not find SIMPLE-ERROR.

(defun %cptn-printed (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (e) (list (type-of e) (prin1-to-string e)))))

(defun %cptn-starts-with-type-p (entry)
  (let ((prefix (format nil "#<~a" (first entry))))
    (and (>= (length (second entry)) (length prefix))
         (string= prefix (second entry) :end2 (length prefix)))))

(deftest condition-print-type-name.simple-error
  (%cptn-starts-with-type-p (%cptn-printed (lambda () (error "Raising ~a" "an error"))))
  t)

(deftest condition-print-type-name.type-error
  (let ((entry (%cptn-printed (lambda () (car (eval 1))))))
    (list (first entry) (%cptn-starts-with-type-p entry)))
  (type-error t))

(deftest condition-print-type-name.message-kept
  (let ((printed (second (%cptn-printed (lambda () (error "Raising ~a" "an error"))))))
    (and (search "Raising an error" printed) t))
  t)
