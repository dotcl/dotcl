;;; Run only the regression files named in the environment variable DOTCL_FILES
;;; (space separated), in the mode the process was started in. Used by
;;; `make test-regression-files FILES="test/regression/a.lisp ..." MODE=compiled|interp|emitfree`.
;;; Prints the PASSED / FAILED counts and the names of the failing tests; exits non-zero on a
;;; failure or when no file was named.

(load "test/framework.lisp")

(let* ((spec (or (dotcl:getenv "DOTCL_FILES") ""))
       (files (let ((r nil) (start nil))
                (dotimes (i (1+ (length spec)))
                  (let ((sep (or (= i (length spec)) (member (char spec i) '(#\Space #\Tab #\Newline)))))
                    (cond ((and sep start) (push (subseq spec start i) r) (setf start nil))
                          ((and (not sep) (not start)) (setf start i)))))
                (nreverse r))))
  (when (null files)
    (format t "test-regression-files: no files given (set FILES=\"test/regression/x.lisp ...\")~%")
    (dotcl:quit 2))
  (dolist (f files) (load f))
  (do-tests-summary)
  (cleanup-regression-temp-dir)
  (dotcl:quit (if (= *fail-count* 0) 0 1)))
