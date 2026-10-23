;;; bench/probes/micro-compile-specials.lisp -- does compiling get slower as
;;; the image holds more special variables?
;;;
;;; Usage (from the project root):
;;;   CS_SPECIALS=12000 dotnet run --project runtime/runtime.csproj -- \
;;;     --asm compiler/cil-out.sil --load bench/probes/micro-compile-specials.lisp
;;;
;;; Environment:
;;;   CS_SPECIALS  how many DEFVARs to evaluate first (default 0)
;;;   CS_FORMS     how many functions to compile per round (default 300)
;;;   CS_ROUNDS    rounds; the minimum is reported (default 7)
;;;
;;; Prints "SPECIALS n=N forms=F min-ms=M median-ms=D".

(defpackage :compile-specials-probe (:use :cl))
(in-package :compile-specials-probe)

(defun env-int (name default)
  (let ((v (dotcl:getenv name))) (if v (parse-integer v) default)))

(defparameter *nspecials* (env-int "CS_SPECIALS" 0))
(defparameter *nforms* (env-int "CS_FORMS" 300))
(defparameter *rounds* (env-int "CS_ROUNDS" 7))

(dotimes (i *nspecials*)
  (eval `(defvar ,(intern (format nil "*CSP-VAR-~D*" i)) ,i)))

(defun probe-form (i)
  `(lambda (a b &optional (c ,i) &key (d 2))
     (let* ((x (+ a b)) (y (* x c)) (z (list x y d)))
       (flet ((f (p q) (let ((r (+ p q))) (* r ,i))))
         (loop for k from 0 below a
               for acc = (f k y) then (+ acc (f k x))
               finally (return (let ((w (length z))) (+ acc w))))))))

(defun one-round ()
  (let ((start (get-internal-real-time)))
    (dotimes (i *nforms*) (compile nil (probe-form i)))
    (/ (- (get-internal-real-time) start)
       (/ internal-time-units-per-second 1000d0))))

(one-round)
(let ((times (sort (loop repeat *rounds* collect (one-round)) #'<)))
  (format t "SPECIALS n=~D forms=~D min-ms=~,1F median-ms=~,1F~%"
          *nspecials* *nforms* (first times) (nth (floor *rounds* 2) times)))
