;;; Accessor reads and writes in a compile-file'd function keep their inline
;;; cache cell across calls. The cell used to be made afresh on every call in
;;; a fasl (the constant pool it lives in for in-memory code is not saved), so
;;; each call allocated it and always missed. Now each site has a static field,
;;; filled on its first execution. These tests load such a fasl, whose own
;;; package and class are defined by the same file, and check that the warm
;;; sites stay correct when the class layout or the accessor's methods change,
;;; and that a warm read/write loop allocates nothing.

(defparameter *faic-src* "faic-src-tmp.lisp")
(defparameter *faic-fasl* "faic-src-tmp.fasl")

(defun faic-sym (name) (find-symbol name "FAIC-PKG"))
(defun faic-call (name &rest args) (apply (symbol-function (faic-sym name)) args))

(defun faic-load ()
  (unless (find-package "FAIC-PKG")
    (with-open-file (s *faic-src* :direction :output :if-exists :supersede)
      (write-string "(defpackage :faic-pkg (:use :cl))
(in-package :faic-pkg)
(defclass pt () ((a :initarg :a :accessor pt-a) (x :initarg :x :accessor pt-x)))
(defun get-x (p) (pt-x p))
(defun set-x (p v) (setf (pt-x p) v))
(defun spin (p n)
  (dotimes (i n) (setf (pt-x p) (pt-x p)))
  (pt-x p))
" s))
    (unwind-protect
        (progn (compile-file *faic-src* :output-file *faic-fasl*)
               (load *faic-fasl*))
      (ignore-errors (delete-file *faic-src*))
      (ignore-errors (delete-file *faic-fasl*)))))

(deftest-compiled-only fasl-accessor-ic.warm-read-write
  (progn
    (faic-load)
    (let ((p (make-instance (faic-sym "PT") :a 1 :x 2)))
      (dotimes (i 5) (faic-call "SET-X" p i))
      (list (faic-call "GET-X" p) (faic-call "SET-X" p :v) (faic-call "GET-X" p))))
  (4 :v :v))

;; A subclass puts the slot at another index: the warm sites must refill.
(deftest-compiled-only fasl-accessor-ic.subclass-refill
  (progn
    (faic-load)
    (eval `(defclass faic-sub (,(faic-sym "PT")) ((b :initform 0) (c :initform 0))))
    (let ((p (make-instance (faic-sym "PT") :a 1 :x 2))
          (q (make-instance 'faic-sub :a 10 :x 20)))
      (dotimes (i 5) (faic-call "SET-X" p i) (faic-call "GET-X" p))
      (faic-call "SET-X" q 21)
      (list (faic-call "GET-X" p) (faic-call "GET-X" q)
            (slot-value q (faic-sym "A")) (slot-value q (faic-sym "X")))))
  (4 21 10 21))

(deftest-compiled-only fasl-accessor-ic.no-allocation-when-warm
  (progn
    (faic-load)
    (let ((p (make-instance (faic-sym "PT") :a 1 :x :k)))
      (faic-call "SPIN" p 1000)
      (let ((b0 (nth 4 (dotcl:gc-stats))))
        (faic-call "SPIN" p 100000)
        ;; Two fresh cells per iteration were 6.4 MB here.
        (< (- (nth 4 (dotcl:gc-stats)) b0) 1000000))))
  t)

;; Methods added after the sites are warm must be honored (MethodEpoch).
(deftest-compiled-only fasl-accessor-ic.methods-after-warm
  (progn
    (faic-load)
    (let ((p (make-instance (faic-sym "PT") :a 1 :x 2)))
      (dotimes (i 5) (faic-call "SET-X" p i) (faic-call "GET-X" p))
      (eval `(defmethod (setf ,(faic-sym "PT-X")) :around (v (p ,(faic-sym "PT")))
               (call-next-method (list :w v) p)))
      (eval `(defmethod ,(faic-sym "PT-X") :around ((p ,(faic-sym "PT")))
               (list :r (call-next-method))))
      (faic-call "SET-X" p 7)
      (faic-call "GET-X" p)))
  (:r (:w 7)))
