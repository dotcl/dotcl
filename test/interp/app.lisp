;;;; A cohesive multi-form program run entirely by the tree-walk interpreter
;;;; (:interpret). Unlike the form-unit diff harness, this validates that
;;;; interpreted def-forms (defmacro/defun/defclass/defgeneric/defmethod/
;;;; define-condition/defstruct) defined EARLIER in the file are correctly used
;;;; by LATER interpreted code — cross-form interpreter state, closures with
;;;; accumulated state, and conditions/restarts crossing function boundaries.
;;;; This is the "real app on the ns2.0/AOT eval path" integration check.
;;;; Run: dotnet run ... -- --asm compiler/cil-out.sil test/interp/app.lisp

(defpackage :dotcl-interp-app (:use :cl))
(in-package :dotcl-interp-app)

(setq dotcl:*evaluator-mode* :interpret)

(defvar *pass* 0)
(defvar *fail* 0)
(defmacro expect (label form expected)
  `(let ((got ,form))
     (if (equal got ,expected) (incf *pass*)
         (progn (incf *fail*)
                (format t "FAIL ~A: got ~S want ~S~%" ,label got ,expected)))))

;; --- a user macro, used by functions defined later ---
(defvar *trace* nil)
(defmacro deftraced (name args &body body)
  `(defun ,name ,args (push ',name *trace*) ,@body))
(deftraced dbl (x) (* x 2))
(deftraced inc (x) (1+ x))
(expect "macro-fn" (list (dbl 5) (inc 5)) '(10 6))
(expect "macro-trace-order"
        (progn (setf *trace* nil) (dbl (inc 3)) (reverse *trace*)) '(inc dbl))

;; --- closures with accumulated state ---
(defun make-counter (start) (lambda () (prog1 start (incf start))))
(expect "closure-state"
        (let ((c (make-counter 10))) (list (funcall c) (funcall c) (funcall c)))
        '(10 11 12))

;; --- mutual + simple recursion across interpreted defuns ---
(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(expect "recursion" (mapcar #'fib '(0 1 2 3 4 5 6 7 8 9 10))
        '(0 1 1 2 3 5 8 13 21 34 55))
(defun evenp* (n) (if (zerop n) t (oddp* (1- n))))
(defun oddp* (n) (if (zerop n) nil (evenp* (1- n))))
(expect "mutual-recursion" (list (evenp* 8) (oddp* 8) (evenp* 7)) '(t nil nil))

;; --- CLOS hierarchy; a generic that calls another generic ---
(defclass shape () ())
(defgeneric area (s))
(defgeneric describe-shape (s))
(defmethod describe-shape ((s shape)) (format nil "area=~,1F" (area s)))
(defclass circle (shape) ((r :initarg :r :reader r)))
(defmethod area ((c circle)) (* 3 (r c) (r c)))     ; pi≈3 for exact ints
(defclass square (shape) ((side :initarg :side :reader side)))
(defmethod area ((s square)) (* (side s) (side s)))
(expect "clos-dispatch"
        (list (area (make-instance 'circle :r 2)) (area (make-instance 'square :side 5)))
        '(12 25))
(expect "generic-calls-generic"
        (describe-shape (make-instance 'square :side 4)) "area=16.0")

;; --- define-condition + restart-case crossing a function boundary ---
(define-condition negative-input (error) ((val :initarg :val :reader bad-val)))
(defun squared-nonneg (x)
  (if (< x 0)
      (restart-case (error 'negative-input :val x) (use-zero () 0))
      (* x x)))
(defun process (xs)
  (handler-bind ((negative-input
                  (lambda (c) (declare (ignore c)) (invoke-restart 'use-zero))))
    (mapcar #'squared-nonneg xs)))
(expect "restart-cross-fn" (process '(3 -2 4 -1)) '(9 0 16 0))

;; --- loop + format building a report string ---
(defun report (pairs)
  (with-output-to-string (s)
    (loop for (name . val) in pairs do (format s "~A:~D " name val))))
(expect "loop-format" (report '(("a" . 1) ("b" . 2) ("c" . 3))) "a:1 b:2 c:3 ")

;; --- defstruct mutated across functions, overdraft skipped via restart ---
(defstruct (acct (:conc-name a-)) (balance 0) owner)
(defun deposit (a n) (incf (a-balance a) n) a)
(defun withdraw (a n)
  (if (> n (a-balance a))
      (restart-case (error "overdraft") (skip () a))
      (progn (decf (a-balance a) n) a)))
(expect "struct+restart"
        (let ((a (make-acct :owner "x")))
          (deposit a 100) (deposit a 50)
          (handler-bind ((error (lambda (c) (declare (ignore c))
                                  (invoke-restart 'skip))))
            (withdraw a 200))          ; 200 > 150 -> skipped, balance stays 150
          (withdraw a 30)              ; 150 -> 120
          (a-balance a))
        120)

;; --- a small stack VM: macro-defined ops + closure stack + reduce ---
(defvar *ops* (make-hash-table))
(defmacro defop (sym fn) `(setf (gethash ,sym *ops*) ,fn))
(defop '+ #'+) (defop '- #'-) (defop '* #'*)
(defun rpn (tokens)
  (let ((stack '()))
    (dolist (tok tokens (car stack))
      (if (numberp tok)
          (push tok stack)
          (let ((b (pop stack)) (a (pop stack)))
            (push (funcall (gethash tok *ops*) a b) stack))))))
(expect "rpn-vm" (rpn '(3 4 + 5 *)) 35)            ; (3+4)*5
(expect "rpn-vm2" (rpn '(10 2 - 3 *)) 24)           ; (10-2)*3

(format t "~%=== interp APP: ~D PASSED  ~D FAILED ===~%" *pass* *fail*)
(dotcl:quit (if (zerop *fail*) 0 1))
