;;; A fasl literal must not depend on the image that compiled it.
;;;
;;; Whether a symbol is external is state of the compiling image, and the literal
;;; writer printed package-qualified symbols accordingly: one colon for external,
;;; two for internal. A file that quotes the very symbols its own toplevel EXPORT
;;; makes external -- cl+ssl's package.lisp is exactly that shape -- compiled in
;;; an image where those symbols were already external got a literal written with
;;; single colons. Loading that fasl into a fresh image reads the literal at a
;;; point where the export has not run yet, and the reader refuses:
;;;
;;;   Symbol "MAKE-SSL-CLIENT-STREAM" is not external in package "CL+SSL"
;;;
;;; The compile succeeded and the first load worked, so it surfaced only on the
;;; second run of a program -- from the fasl the first run wrote.

(defvar *fss-source*
  (concatenate 'string
               (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp"))
               "/dotcl-fasl-symbol-syntax.lisp"))

;;; Enough names that the literal goes through the print-and-read route rather
;;; than being built by inline IL: that route is where the printed text is what
;;; the loading image has to make sense of. cl+ssl exports about fifty.
(defun %fss-names (count)
  (let ((names '()))
    (dotimes (i count (nreverse names))
      (push (format nil "marker-~a" i) names))))

(defun %fss-write (path package)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "(defpackage :~a (:use :cl))~%" package)
    (format out "(in-package :~a)~%" package)
    ;; Toplevel export, then a literal naming what it just exported.
    (format out "(export '(~{~a ~}))~%" (%fss-names 80))
    (format out "(defvar *quoted* '(~{~a ~}))~%" (%fss-names 80)))
  path)

(%fss-write *fss-source* "fasl-symbol-syntax-test")

;;; Load first, so MARKER is external while the file is compiled -- the situation
;;; that used to poison the fasl. Then drop the package and load the fasl alone.
(deftest fss-literal-survives-a-fresh-image
  (progn
    (load *fss-source*)
    (let ((fasl (compile-file *fss-source*)))
      (delete-package "FASL-SYMBOL-SYNTAX-TEST")
      (load fasl)
      (nth-value 1 (find-symbol "MARKER-0" "FASL-SYMBOL-SYNTAX-TEST"))))
  :external)

;;; The ordinary case -- compiled without the package loaded first, so the symbols
;;; are internal while the literal is written -- still works.
(deftest fss-literal-when-compiled-cold
  (let ((source (%fss-write (concatenate 'string
                                         (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                                 (dotcl:getenv "TEMP") "/tmp"))
                                         "/dotcl-fasl-symbol-syntax-cold.lisp")
                            "fasl-symbol-syntax-cold")))
    (load (compile-file source))
    (list (nth-value 1 (find-symbol "MARKER-0" "FASL-SYMBOL-SYNTAX-COLD"))
          (length (symbol-value (find-symbol "*QUOTED*" "FASL-SYMBOL-SYNTAX-COLD")))))
  (:external 80))
