;;; regression: skip-under-interp
;;; EQUAL quoted lists of one file are one object once the file is loaded from
;;; its .fasl, as in SBCL (CLHS 3.2.4.4 allows a file compiler to coalesce
;;; similar constants). Libraries tested on SBCL compare such literals with
;;; EQL: (eql '(1) (concat '((1)))) in listopia's own tests.

(defun %fasl-coalesce-case ()
  (let* ((tmp (format nil "~a/dotcl-coalesce-~a"
                      (regression-temp-dir)
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defpackage #:coalesce-pkg (:use :cl))~%")
      (format s "(in-package #:coalesce-pkg)~%")
      (format s "(defun f () '(1))~%")
      (format s "(defun g () '(1))~%")
      (format s "(defun h () '((1) (2 3)))~%")
      (format s "(defun i () '(2 3))~%")
      (format s "(defun j () '(1 \"ab\" #\\c))~%")
      (format s "(defun k () '(1 \"ab\" #\\c))~%")
      (format s "(defun l () '(1 2))~%")
      (format s "(defun m () '(1 2 3))~%"))
    (compile-file src)
    (load (concatenate 'string (subseq src 0 (- (length src) 5)) ".fasl"))
    (flet ((call (n) (funcall (find-symbol n "COALESCE-PKG"))))
      (list (eq (call "F") (call "G"))
            (eq (car (call "H")) (call "F"))
            (eq (cadr (call "H")) (call "I"))
            (eq (call "J") (call "K"))
            ;; a different list is a different object
            (eq (call "L") (call "M"))
            (eq (cdr (call "L")) (cdr (call "M")))
            (equal (call "M") '(1 2 3))))))

(deftest-compiled-only fasl-coalesce-list-literals
  (%fasl-coalesce-case)
  (t t t t nil nil t))
