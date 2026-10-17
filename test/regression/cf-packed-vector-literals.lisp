;;; A long BIT or (unsigned-byte 8) vector literal is stored in the fasl as its
;;; packed bytes, not as one store per element in generated code.

(defun %cfpv-compile (name forms)
  (let ((src (regression-temp-file (format nil "~a.lisp" name)))
        (fasl (regression-temp-file (format nil "~a.fasl" name))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(defvar *cfpv-bits*
  (let ((v (make-array 50001 :element-type 'bit :initial-element 0)))
    (dotimes (i (length v) v)
      (when (or (zerop (mod i 3)) (= i 50000)) (setf (sbit v i) 1)))))

(defvar *cfpv-octets*
  (let ((v (make-array 301 :element-type '(unsigned-byte 8))))
    (dotimes (i (length v) v) (setf (aref v i) (mod (* i 7) 256)))))

(defvar *cfpv-short-bits* (make-array 40 :element-type 'bit :initial-element 1))

(defmacro cfpv-lit (x) `',(symbol-value x))

(deftest-compiled-only cf-packed-vector-literals.contents
  (let ((fasl (%cfpv-compile "cfpv-a" '((defun cfpv-bits () (cfpv-lit *cfpv-bits*))
                                        (defun cfpv-octets () (cfpv-lit *cfpv-octets*))
                                        (defun cfpv-short-bits () (cfpv-lit *cfpv-short-bits*))))))
    (load fasl)
    (let ((b (funcall (intern "CFPV-BITS")))
          (o (funcall (intern "CFPV-OCTETS")))
          (sb (funcall (intern "CFPV-SHORT-BITS"))))
      (list (equal b *cfpv-bits*)
            (simple-bit-vector-p b)
            (equalp o *cfpv-octets*)
            (typep o '(simple-array (unsigned-byte 8) (301)))
            (equal sb *cfpv-short-bits*)
            (< (with-open-file (s fasl :element-type '(unsigned-byte 8)) (file-length s))
               100000))))
  (t t t t t t))
