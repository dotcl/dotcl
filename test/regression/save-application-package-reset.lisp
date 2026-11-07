;;; Each source given to save-application starts in the package the build began in:
;;; an IN-PACKAGE in one file does not carry over into the next (it used to, so a
;;; :load file after an ASDF closure was read in the closure's last package).

(defun sapr-build ()
  (let* ((tmp (format nil "~a/dotcl-sapr-~a" (regression-temp-dir) (get-internal-real-time)))
         (a (format nil "~a/a.lisp" tmp))
         (b (format nil "~a/b.lisp" tmp))
         (out (format nil "~a/out.fasl" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s a :direction :output :if-exists :supersede)
      (format s "(defpackage :sapr-other (:use :cl))~%(in-package :sapr-other)~%(defun sapr-in-a () 1)~%"))
    (with-open-file (s b :direction :output :if-exists :supersede)
      (format s "(defun sapr-in-b () 2)~%"))
    (dotcl:save-application out :load (list a b))
    (list (and (find-symbol "SAPR-IN-B" :cl-user)
               (not (find-symbol "SAPR-IN-B" :sapr-other))
               t)
          (and (find-symbol "SAPR-IN-A" :sapr-other) t))))

(deftest-compiled-only save-application-package-reset
  (sapr-build)
  (t t))
