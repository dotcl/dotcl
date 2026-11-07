;;; (compile-file f :output-file nil) is a TYPE-ERROR (the output file is a pathname
;;; designator; SBCL signals it). Leaving :output-file out gives the default output name,
;;; and a string or a pathname gives that file.

(defun %cfonil-source (name)
  (let ((path (merge-pathnames name (regression-temp-dir))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(defun cfonil-f () 1)" s))
    path))

(deftest-compiled-only compile-file-output-file-nil.nil-is-an-error
  (let ((*error-output* (make-broadcast-stream)))
    (handler-case (progn (compile-file (%cfonil-source "cfonil1.lisp") :output-file nil) :no-error)
      (type-error () :type-error)))
  :type-error)

(deftest-compiled-only compile-file-output-file-nil.valid-forms
  (let* ((*error-output* (make-broadcast-stream))
         (src (%cfonil-source "cfonil2.lisp"))
         (named (merge-pathnames "cfonil2-out.fasl" (regression-temp-dir))))
    (list (and (probe-file (compile-file src :output-file named)) t)
          (and (probe-file (compile-file src :output-file (namestring named))) t)
          (and (probe-file (compile-file src)) t)))
  (t t t))
