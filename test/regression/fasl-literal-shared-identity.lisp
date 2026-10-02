;;; An object that occurs more than once in one literal is still one object
;;; after COMPILE-FILE and LOAD (CLHS 3.2.4.4). Conses and general vectors
;;; already kept it (their literal goes through print and read). Anything else
;;; was built afresh at every occurrence: '(#1=#P"foo" #1#) came back as two
;;; pathnames. Also hash tables and specialized arrays, and the same object
;;; seen from a list chunk or a hash table built by a helper method.

(defvar *flsi-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-flsi-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %flsi-compile-and-load (source name)
  (let ((lisp (concatenate 'string *flsi-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-compiled-only flsi-shared-atoms
  (progn
    (%flsi-compile-and-load
     "(in-package :cl-user)
      (defparameter *flsi-path* '(#1=#P\"foo\" #1#))
      (defparameter *flsi-ht* '(#2=#.(make-hash-table) #2#))
      (defparameter *flsi-dvec*
        '(#3=#.(make-array 2 :element-type 'double-float :initial-element 1d0) #3#))
      (defparameter *flsi-path-in-vec* '(#4=#P\"bar\" #(#4#)))
      (defparameter *flsi-mixed* '(#5=(a) #5# #6=#P\"x\" #6#))"
     "flsi-atoms")
    (flet ((same (v) (eq (first (symbol-value v)) (second (symbol-value v)))))
      (list (same (intern "*FLSI-PATH*"))
            (pathnamep (first (symbol-value (intern "*FLSI-PATH*"))))
            (same (intern "*FLSI-HT*"))
            (same (intern "*FLSI-DVEC*"))
            (let ((l (symbol-value (intern "*FLSI-PATH-IN-VEC*"))))
              (eq (first l) (aref (second l) 0)))
            (let ((l (symbol-value (intern "*FLSI-MIXED*"))))
              (list (eq (first l) (second l)) (eq (third l) (fourth l)))))))
  (t t t t t (t t)))

;;; A long list is built by helper methods, one per chunk; a large hash table
;;; likewise. The object has to be the same across those methods.
(deftest-compiled-only flsi-shared-across-helper-methods
  (progn
    (%flsi-compile-and-load
     "(in-package :cl-user)
      (defparameter *flsi-long*
        '#.(let ((p (pathname \"long\")))
             (append (list p) (make-list 1200 :initial-element 0) (list p))))
      (defparameter *flsi-big-ht*
        '#.(let ((p (pathname \"big\")) (h (make-hash-table)))
             (dotimes (i 40) (setf (gethash i h) p))
             (list p h)))"
     "flsi-helpers")
    (let ((l (symbol-value (intern "*FLSI-LONG*")))
          (h (symbol-value (intern "*FLSI-BIG-HT*"))))
      (list (length l)
            (eq (first l) (car (last l)))
            (eq (first h) (gethash 0 (second h)))
            (eq (gethash 0 (second h)) (gethash 39 (second h))))))
  (1202 t t t))
