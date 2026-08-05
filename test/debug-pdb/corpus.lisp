;;; corpus.lisp — a spread of forms compiled BOTH with and without DOTCL_EMIT_PDB
;;; to check the debug codegen path (markers + disabled slot-merge) produces the
;;; same results as the normal path. Each function is deterministic (no time /
;;; random / hash-table iteration order). (run-corpus) prints one line per case;
;;; the harness diffs the debug-on vs debug-off output.
(in-package :cl-user)

;; recursion + params
(defun fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))

;; let / let* / shadowing / native-rep locals
(defun poly (x)
  (let* ((a (* x x))
         (b (+ a x)))
    (let ((a (- b 1)))          ; shadows outer a
      (+ a b))))

(defun sum-sq (n)
  (declare (fixnum n))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i n) (incf acc (* i i)))
    acc))

;; closure capturing + mutating a variable (boxed)
(defun make-counter (start)
  (lambda () (prog1 start (incf start))))

(defun run-counter ()
  (let ((c (make-counter 10)))
    (list (funcall c) (funcall c) (funcall c))))

;; A boxed var (mutated + captured) that is ALSO read and written in the
;; defining frame — exercises the box read (compile-var-ref) and box write
;; (compile-setq) paths in the defining frame, not just inside the closure.
(defun boxed-in-frame ()
  (let ((total 0))
    (let ((bump (lambda (d) (incf total d))))   ; total captured + mutated -> boxed
      (funcall bump 5)                           ; closure mutates the box
      (setf total (+ total 1))                   ; defining-frame box write
      (list total (funcall bump 10) total))))    ; defining-frame box read

;; handler-case (exception scope)
(defun safe-div (a b)
  (handler-case (/ a b)
    (division-by-zero () :divide-by-zero)))

;; loop macro
(defun collatz-steps (n)
  (loop for x = n then (if (evenp x) (/ x 2) (1+ (* 3 x)))
        for steps from 0
        until (= x 1)
        finally (return steps)))

;; &optional / &key params
(defun greet (name &optional (greeting "hi") &key (loud nil))
  (let ((s (format nil "~a ~a" greeting name)))
    (if loud (string-upcase s) s)))

;; labels — local mutual recursion
(defun parity (n)
  (labels ((ev (k) (if (zerop k) t (od (1- k))))
           (od (k) (if (zerop k) nil (ev (1- k)))))
    (if (ev n) :even :odd)))

;; multiple values
(defun minmax (list)
  (values (reduce #'min list) (reduce #'max list)))

(defun mm-string (list)
  (multiple-value-bind (lo hi) (minmax list)
    (format nil "~a..~a" lo hi)))

;; higher-order + lambda
(defun scaled (list k) (mapcar (lambda (x) (* x k)) list))

;; simple CLOS
(defclass pt () ((x :initarg :x :accessor pt-x) (y :initarg :y :accessor pt-y)))
(defmethod norm2 ((p pt)) (+ (* (pt-x p) (pt-x p)) (* (pt-y p) (pt-y p))))

(defun run-corpus ()
  (format t "fact=~a~%" (fact 6))
  (format t "poly=~a~%" (poly 3))
  (format t "sum-sq=~a~%" (sum-sq 5))
  (format t "counter=~a~%" (run-counter))
  (format t "boxed-in-frame=~a~%" (boxed-in-frame))
  (format t "safe-div=~a~%" (list (safe-div 10 2) (safe-div 1 0)))
  (format t "collatz=~a~%" (collatz-steps 27))
  (format t "greet=~a~%" (list (greet "bob") (greet "bob" "hey" :loud t)))
  (format t "parity=~a~%" (list (parity 4) (parity 7)))
  (format t "minmax=~a~%" (mm-string '(3 1 4 1 5 9 2 6)))
  (format t "scaled=~a~%" (scaled '(1 2 3) 10))
  (format t "norm2=~a~%" (norm2 (make-instance 'pt :x 3 :y 4))))
