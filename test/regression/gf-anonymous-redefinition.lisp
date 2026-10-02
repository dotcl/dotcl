;;; A generic function made with MAKE-INSTANCE 'STANDARD-GENERIC-FUNCTION is in
;;; no name table. Redefining a class drops every generic function's dispatch
;;; caches, and that used to walk the named ones only: an anonymous one kept
;;; the method chosen under the old precedence list, and an accessor method
;;; moved onto one kept the old slot position.

(defclass agr-a () ())
(defclass agr-b () ())
(defclass agr-c (agr-b) ())

(defun %agr-method (class value)
  (make-instance 'standard-method
                 :qualifiers '()
                 :lambda-list '(x)
                 :specializers (list (find-class class))
                 :function (lambda (args next-methods)
                             (declare (ignore args next-methods))
                             value)))

(deftest gf-anonymous-redefinition.superclass-change
  (let ((gf (make-instance 'standard-generic-function :lambda-list '(x)))
        (c (make-instance 'agr-c)))
    (add-method gf (%agr-method 'agr-a :a))
    (add-method gf (%agr-method 'agr-b :b))
    (dotimes (k 4) (funcall gf c))
    (let ((before (funcall gf c)))
      (defclass agr-c (agr-a) ())
      (list before (funcall gf c))))
  (:b :a))

;; The accessor's methods, moved onto anonymous generic functions: the reader
;; and writer shortcuts are cached there.
(defclass agr-base () ((n :initform 0 :accessor agr-n)))
(defclass agr-s (agr-base) ((a :initform 1)))

(deftest gf-anonymous-redefinition.slot-moved
  (let* ((rd (make-instance 'standard-generic-function :lambda-list '(x)))
         (wr (make-instance 'standard-generic-function :lambda-list '(v x)))
         (rm (find-method #'agr-n '() (list (find-class 'agr-base))))
         (wm (find-method #'(setf agr-n) '() (list (find-class t) (find-class 'agr-base))))
         (o (make-instance 'agr-s)))
    (remove-method #'agr-n rm)
    (remove-method #'(setf agr-n) wm)
    (add-method rd rm)
    (add-method wr wm)
    (dotimes (k 3) (funcall wr (+ (funcall rd o) 1) o))
    (defclass agr-s (agr-base) ((z :initform 9) (a :initform 1)))
    (dotimes (k 3) (funcall wr (+ (funcall rd o) 1) o))
    (list (funcall rd o) (slot-value o 'a) (slot-value o 'z)))
  (6 1 9))
