;;; ql:bundle-systems writes the bundle's loader (bundle.lisp) from a template.
;;;
;;; The client's bundle.lisp reads that template from beside itself, finding it
;;; with #.(merge-pathnames "bundle-template.lisp" *compile-file-truename*). dotcl
;;; ships the client as one concatenated file, where "beside itself" is
;;; contrib/quicklisp/ and holds no template -- and a path read at compile time
;;; would be the build machine's anyway. So bundle-systems copied every system
;;; and then failed on the last step. The build now puts the template's lines
;;; into the concatenated file; what the loader writes must be the template.

(require "quicklisp")

(defun qbl-template-lines ()
  (let ((path (merge-pathnames "quicklisp-client/quicklisp/bundle-template.lisp")))
    (when (probe-file path)
      (with-open-file (s path)
        (loop for line = (read-line s nil) while line collect line)))))

(defun qbl-written-lines ()
  (let ((text (with-output-to-string (s)
                (funcall (find-symbol "WRITE-LOADER-SCRIPT" "QL-BUNDLE")
                         (make-instance (find-symbol "BUNDLE" "QL-BUNDLE")) s))))
    (with-input-from-string (s text)
      (loop for line = (read-line s nil) while line collect line))))

;;; Writing the loader needs no file at run time.
(deftest quicklisp-bundle-loader.writes-the-template
  (let ((written (qbl-written-lines)))
    (list (first written)
          (and (find "(cl:in-package #:cl-user)" written :test #'string=) t)
          ;; against the client's own template, when its checkout is here
          (let ((template (qbl-template-lines)))
            (or (null template) (equal written template)))))
  ("(cl:in-package #:cl-user)" t t))
