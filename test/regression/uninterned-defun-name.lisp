;;; A function named by an uninterned symbol lives on that symbol only. It used to be
;;; registered under the bare name too, so (defun #:foo ...) defined FOO in the current
;;; package, and (defun #:ignore (&key a) ...) failed on the locked COMMON-LISP symbol
;;; (xuriella's DEFSTRUCT with (:constructor #:ignore)).

(defun %udn-define (name lambda-list &rest body)
  (let ((s (make-symbol name)))
    (eval `(defun ,s ,lambda-list ,@body))
    s))

(deftest uninterned-defun-name.does-not-define-the-interned-symbol
  (let ((s (%udn-define "UDN-NOT-DEFINED-1" '(a) 'a))
        (k (%udn-define "UDN-NOT-DEFINED-2" '(&key x) 'x))
        (o (%udn-define "UDN-NOT-DEFINED-3" '(&optional x) 'x)))
    (list (fboundp (intern "UDN-NOT-DEFINED-1"))
          (fboundp (intern "UDN-NOT-DEFINED-2"))
          (fboundp (intern "UDN-NOT-DEFINED-3"))
          (funcall s 1) (funcall k :x 2) (funcall o 3)))
  (nil nil nil 1 2 3))

(deftest uninterned-defun-name.cl-symbol-names
  (let ((i (%udn-define "IGNORE" '(&key a) 'a))
        (l (%udn-define "LIST" '(&optional a) '(list a))))
    (list (funcall i :a 5) (funcall l 6) (list 1 2)))
  (5 (6) (1 2)))

(deftest uninterned-defun-name.struct-constructor
  (progn
    (eval '(defstruct (udn-struct (:constructor #:ignore)) a))
    (list (fboundp 'ignore) (and (find-class 'udn-struct) t)))
  (nil t))
