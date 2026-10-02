;;; OPEN signals FILE-ERROR when the file system refuses the open.
;;;
;;; The .NET exception for a missing directory, a denied permission or too many
;;; open files used to come out as a PROGRAM-ERROR, so a handler for FILE-ERROR
;;; (osicat's OPEN-TEMPORARY-FILE, for one) did not see it. Only the missing
;;; :INPUT file, which OPEN checks for itself, was a FILE-ERROR.

(defparameter *ofe-missing*
  (concatenate 'string (regression-temp-dir) "/no-such-dir/x.txt"))

(defun ofe-try (&rest args)
  (handler-case (progn (close (apply #'open *ofe-missing* args)) :opened)
    (file-error (e) (list :file-error (equal (namestring (file-error-pathname e))
                                             *ofe-missing*)))
    (error (e) (list :other (type-of e)))))

(deftest open-file-error.missing-directory-output
  (ofe-try :direction :output :if-does-not-exist :create)
  (:file-error t))

(deftest open-file-error.missing-directory-io
  (ofe-try :direction :io :if-does-not-exist :create)
  (:file-error t))

(deftest open-file-error.missing-directory-input
  (ofe-try :direction :input)
  (:file-error t))
