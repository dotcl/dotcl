;;; Run test/regression/run.lisp form by form and print the wall time of each
;;; top-level form, labelled by the first test/regression/*.lisp it names.
;;; make regression-time runs the suite through this file.
;;;
;;; Each form prints one line as soon as it finishes, before the suite's own
;;; DOTCL:QUIT, so a log cut short still has the files that ran:
;;;
;;;   FILE-TIME <milliseconds> <label> <tests>
;;;
;;; <tests> is how many tests the form ran. Forms that name no test file are
;;; labelled "<other>".

(defun %file-times-test-count ()
  (let ((s (find-symbol "*TEST-COUNT*" "CL-USER")))
    (if (and s (boundp s)) (symbol-value s) 0)))

(defun %file-times-label (form)
  (cond ((and (stringp form)
              (> (length form) 16)
              (string= "test/regression/" form :end2 16))
         form)
        ((consp form)
         (or (%file-times-label (car form))
             (%file-times-label (cdr form))))
        (t nil)))

(with-open-file (in "test/regression/run.lisp")
  (let ((eof (list nil)))
    (loop
      (let ((form (read in nil eof)))
        (when (eq form eof) (return))
        (let ((label (or (%file-times-label form) "<other>"))
              (start (get-internal-real-time))
              (tests (%file-times-test-count)))
          (flet ((report ()
                   (format t "~&FILE-TIME ~d ~a ~d~%"
                           (round (* 1000 (- (get-internal-real-time) start))
                                  internal-time-units-per-second)
                           label
                           (- (%file-times-test-count) tests))
                   (finish-output)))
            (if (and (consp form) (eq (car form) 'if))
                (progn (report) (eval form))
                (progn (eval form) (report)))))))))
