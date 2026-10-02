;;;; Load pfdietz's random integer form generator from ansi-test.
;;;; Portable: runs on dotcl and on SBCL (the reference for replay).
;;;; Must be loaded with the dotcl tree as the current directory, after
;;;; `make setup-ansi-test`.

(in-package :cl-user)

(defparameter *rf-ansi-dir* (merge-pathnames "ansi-test/" (truename ".")))

(defun %rf-ansi (sub name)
  (merge-pathnames name (merge-pathnames sub *rf-ansi-dir*)))

;;; ansi-test calls COMPILE-AND-LOAD with logical names (ANSI-TESTS:AUX;x.lsp)
;;; or names relative to the random/ directory.  Map both onto the checkout.
(defun compile-and-load (path &key force)
  (declare (ignore force))
  (let* ((s (namestring path))
         (aux-prefix "ANSI-TESTS:AUX;")
         (name (if (and (>= (length s) (length aux-prefix))
                        (string-equal aux-prefix s :end2 (length aux-prefix)))
                   (subseq s (length aux-prefix))
                   s))
         (aux (%rf-ansi "auxiliary/" name)))
    (load (if (probe-file aux) aux (%rf-ansi "random/" name)))))

(defun compile-and-load* (path &key force)
  (declare (ignore force))
  (load (%rf-ansi "auxiliary/" path)))

(ensure-directories-exist "sandbox/dummy.txt")
(let ((*default-pathname-defaults* *rf-ansi-dir*))
  (load (merge-pathnames "rt-package.lsp" *rf-ansi-dir*))
  (load (merge-pathnames "rt.lsp" *rf-ansi-dir*))
  (load (merge-pathnames "cl-test-package.lsp" *rf-ansi-dir*)))

(in-package :cl-test)

(compile-and-load "ANSI-TESTS:AUX;ansi-aux-macros.lsp")
(load (cl-user::%rf-ansi "" "universe.lsp"))
(load (cl-user::%rf-ansi "" "cl-symbol-names.lsp"))
(compile-and-load "ANSI-TESTS:AUX;ansi-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;random-aux.lsp")
(compile-and-load "random-int-form.lsp")
