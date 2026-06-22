;;;; IL-verifiability stress fixture (compiled, then checked with ilverify).
;;;;
;;;; Purpose: dotcl's emitter must produce VERIFIABLE CIL — no covariant calls
;;;; (passing a base LispObject where a derived type like Symbol is required
;;;; without a castclass), no stack-type mismatches, etc. CoreCLR's JIT tolerates
;;;; such IL, but strict AOT C++ codegens (Unity IL2CPP / WebGL) reject it. This
;;;; file exercises the emit-heavy paths so a regression surfaces here in seconds
;;;; rather than in a 25-minute IL2CPP build. See scripts/ilverify-check.sh and
;;;; `make ilverify`.
(defpackage :dotcl-ilverify (:use :cl))
(in-package :dotcl-ilverify)

;; Global special bindings (defvar/defparameter/defconstant init = ModuleInit;
;; the historical covariant-IL site, see the DynamicBindings.Set(Symbol,...) calls).
(defvar *a* 0)
(defparameter *b* 1)
(defconstant +c+ 2)

;; defstruct + setf places + read-modify-write + rotatef.
(defstruct pt x y)
(defun places (lst v)
  (setf (car lst) v
        (cdr lst) (list v))
  (incf (car lst))
  (push 9 (cdr lst))
  (let ((p (make-pt :x 1 :y 2)))
    (setf (pt-x p) 10)
    (incf (pt-y p))
    (rotatef (pt-x p) (pt-y p))
    (list (pt-x p) (pt-y p))))

;; CLOS: inheritance, generic dispatch, call-next-method, setf on accessor.
(defclass shape () ((name :initarg :name :accessor shape-name)))
(defclass circ (shape) ((r :initarg :r :accessor circ-r)))
(defgeneric area (s))
(defmethod area ((s circ)) (* 3 (circ-r s) (circ-r s)))
(defgeneric describe-it (x))
(defmethod describe-it ((s shape)) (list :shape (shape-name s)))
(defmethod describe-it ((s circ)) (cons :circ (call-next-method)))
(defun rename (c) (setf (shape-name c) "z"))

;; Control flow: dotimes, handler-case, multiple-value, tagbody/go,
;; non-local exit across flet/labels, loop, hash iteration.
(defun ctrl (n)
  (let ((acc 0))
    (dotimes (i n) (incf acc i))
    (handler-case (if (> acc 5) (error "big") acc)
      (error (e) (declare (ignore e)) -1))))
(defun mv () (multiple-value-bind (q r) (floor 17 5) (+ q r)))
(defun tags (n)
  (let ((s 0))
    (tagbody
     top (when (>= s n) (go end)) (incf s) (go top)
     end)
    s))
(defun nle (n)
  (block done
    (flet ((bail (v) (return-from done v)))
      (labels ((rec (k acc) (if (zerop k) (bail acc) (rec (1- k) (+ acc k)))))
        (rec n 0)))))
(defun lp (xs)
  (loop for x in xs
        for i from 0
        when (evenp x) collect (cons i x) into evens
        else sum x into odds
        finally (return (values evens odds))))
(defun fmt (n) (format nil "~r ~:d ~5,'0d ~x" n (* n 1000) n n))
(defun hashing (kvs)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (kv kvs) (setf (gethash (car kv) h) (cdr kv)))
    (loop for k being the hash-keys of h using (hash-value v) collect (cons k v))))

;; A closure capturing a mutated variable.
(defun adder (n) (lambda (x) (incf n x)))
