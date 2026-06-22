;;;; Interpreted def-forms: defun / defvar / defparameter / defconstant evaluated
;;;; by the tree-walk interpreter (%mini-eval) with no compilation. Asserts the
;;;; resulting functions/vars behave correctly. Run under :interpret so eval and
;;;; the def-forms it contains all route through %mini-eval.
;;;; Run: dotnet run ... -- --asm compiler/cil-out.sil test/interp/defun.lisp

(defpackage :dotcl-interp-defun (:use :cl))
(in-package :dotcl-interp-defun)

(setq dotcl:*evaluator-mode* :interpret)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro expect (form expected)
  `(let ((got (eval ',form)))
     (if (equal got ,expected)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "FAIL: ~S~%  got ~S, want ~S~%" ',form got ,expected)))))

;; --- defun shapes ---
(eval '(defun id-sq (x) (* x x)))
(expect (id-sq 9) 81)

(eval '(defun rec-fact (n) (if (< n 2) 1 (* n (rec-fact (1- n))))))   ; recursion
(expect (rec-fact 6) 720)

(eval '(defun va-sum (&rest xs) (let ((s 0)) (dolist (x xs s) (incf s x)))))  ; &rest
(expect (va-sum 1 2 3 4 5) 15)

(eval '(defun opt-add (x &optional (d 10)) (+ x d)))                  ; &optional default
(expect (opt-add 5) 15)
(expect (opt-add 5 1) 6)

(eval '(defun key-pt (&key (x 0) (y 0)) (list x y)))                  ; &key defaults
(expect (key-pt) '(0 0))
(expect (key-pt :y 7) '(0 7))
(expect (key-pt :x 3 :y 4) '(3 4))

(eval '(defun finder (item lst)                                       ; implicit block
         (dolist (x lst nil) (when (eql x item) (return-from finder t)))))
(expect (finder 3 '(1 2 3)) t)
(expect (finder 9 '(1 2 3)) nil)

;; interpreted fn calling another interpreted fn
(eval '(defun caller (n) (+ (id-sq n) (opt-add n))))
(expect (caller 4) (+ 16 14))

;; --- variables ---
(eval '(defvar *dv* 41))
(expect (symbol-value '*dv*) 41)
(eval '(defvar *dv* 999))            ; defvar: no re-init when already bound
(expect (symbol-value '*dv*) 41)
(eval '(defparameter *dp* (+ 2 3)))
(expect (symbol-value '*dp*) 5)
(eval '(defparameter *dp* 7))        ; defparameter: always re-init
(expect (symbol-value '*dp*) 7)
(eval '(defconstant +dc+ 100))
(expect (symbol-value '+dc+) 100)

;; dynamic binding of an interpreted defvar honored by an interpreted fn
(eval '(defvar *scale* 2))
(eval '(defun scaled (x) (* x *scale*)))
(expect (let ((*scale* 10)) (scaled 4)) 40)

;; --- interpreted defmacro ---
(eval '(defmacro m-when (c &body body) `(if ,c (progn ,@body) nil)))
(expect (m-when t 1 2 3) 3)
(expect (m-when nil 1 2 3) nil)
(eval '(defmacro m-swap (a b) (let ((g (gensym))) `(let ((,g ,a)) (setf ,a ,b) (setf ,b ,g)))))
(expect (let ((x 1) (y 2)) (m-swap x y) (list x y)) '(2 1))
(eval '(defmacro m-inc (place &optional (n 1)) `(setf ,place (+ ,place ,n))))
(expect (let ((c 10)) (m-inc c) (m-inc c 5) c) 16)
;; macro consumed by an interpreted defun
(eval '(defun m-user (x) (m-when (> x 0) (* x 10))))
(expect (m-user 4) 40)
(expect (m-user -1) nil)

(format t "~%=== interp defun: ~D PASSED  ~D FAILED ===~%" *pass* *fail*)
(dotcl:quit (if (zerop *fail*) 0 1))
