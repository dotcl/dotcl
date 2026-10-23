;;; A DEFUN compiled under (debug 0) records no frame, and from a .fasl it is
;;; still called through an entry pointer, as a framed DEFUN is. Without the
;;; entry every call to it went through InvokeN, which made (debug 0) code
;;; slower than the default.

(defvar *d0e-bt* nil)
(defun %d0e-depth () (length (dotcl:backtrace)))
(defun %d0e-state (f) (dotnet:static "DotCL.LispFunction" "EntryState" f))
(defun %d0e-probe () (setf *d0e-bt* (dotcl:backtrace)) 0)

(defparameter *d0e-source* "
(declaim (optimize (debug 0)))
(defun %d0e-f0 () :zero)
(defun %d0e-f1 (a) (list :one a))
(defun %d0e-f4 (a b c d) (list a b c d))
(defun %d0e-f5 (a b c d e) (list a b c d e))
(defun %d0e-mv (a) (values a (1+ a)))
(defun %d0e-even (n) (if (zerop n) t (%d0e-odd (1- n))))
(defun %d0e-odd (n) (if (zerop n) nil (%d0e-even (1- n))))
(defun %d0e-sum (x) (if (consp x) (+ (%d0e-sum (car x)) (%d0e-sum (cdr x))) (if x 1 0)))
(defun %d0e-rec (n) (if (zerop n) 0 (+ 1 (%d0e-rec (- n 1)))))
(defun %d0e-probe-call (a) (+ a (%d0e-probe)))
(defun %d0e-self (n acc) (if (zerop n) acc (%d0e-self (1- n) (cons n acc))))
(declaim (optimize (debug 1)))
(defun %d0e-framed-tail (a) (%d0e-probe-call a))
")

(defun %d0e-load ()
  (let ((src (regression-temp-file "debug0-entry-src.lisp"))
        (fasl (regression-temp-file "debug0-entry-src.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *d0e-source* s))
    (load (compile-file src :output-file fasl))))

(when (compiled-mode-p) (%d0e-load))

(deftest-compiled-only debug0-entry.state
  (mapcar (lambda (s) (%d0e-state (symbol-function s)))
          '(%d0e-f0 %d0e-f1 %d0e-f4 %d0e-f5 %d0e-framed-tail))
  ("closure-entry" "closure-entry" "closure-entry" "none" "entry"))

(deftest-compiled-only debug0-entry.calls
  (list (%d0e-f0) (%d0e-f1 1) (funcall '%d0e-f1 2) (apply #'%d0e-f1 '(3))
        (mapcar #'%d0e-f1 '(4 5)) (%d0e-f4 1 2 3 4) (apply #'%d0e-f4 1 '(2 3 4))
        (%d0e-f5 1 2 3 4 5)
        (handler-case (funcall #'%d0e-f1 1 2) (program-error () :program-error))
        (handler-case (apply #'%d0e-f4 '(1)) (program-error () :program-error))
        (multiple-value-list (%d0e-mv 1))
        (%d0e-sum '((a b) (c (d e)) f))
        (length (%d0e-self 1000 nil)))
  (:zero (:one 1) (:one 2) (:one 3) ((:one 4) (:one 5)) (1 2 3 4) (1 2 3 4)
   (1 2 3 4 5) :program-error :program-error (1 2) 6 1000))

;; Mutual tail calls between frameless bodies run in constant stack.
(deftest-compiled-only debug0-entry.mutual-tail
  (list (%d0e-even 1000000) (%d0e-odd 1000001))
  (t t))

;; A frameless body does not show in the backtrace, a framed caller still does,
;; and the frame list is back where it was after a tail call, a return and an
;; error.
(deftest-compiled-only debug0-entry.frames
  (let ((base (%d0e-depth)) (r '()))
    (setf *d0e-bt* nil)
    (push (%d0e-framed-tail 1) r)
    (push (and (member "%D0E-FRAMED-TAIL" *d0e-bt* :test #'string=) t) r)
    (push (and (member "%D0E-PROBE-CALL" *d0e-bt* :test #'string=) t) r)
    (push (- (%d0e-depth) base) r)
    (push (handler-case (%d0e-framed-tail "x") (type-error () :type-error)) r)
    (push (- (%d0e-depth) base) r)
    (nreverse r))
  (1 t nil 0 :type-error 0))

(deftest-compiled-only debug0-entry.stack-overflow-catchable
  (list (%d0e-rec 1000)
        (call-on-small-stack
         (lambda ()
           (handler-case (progn (%d0e-rec 10000000) :no-overflow)
             (error () :caught-error)
             (storage-condition () :caught-storage-condition)))))
  (1000 :caught-storage-condition))

(deftest-compiled-only debug0-entry.redefine
  (let ((old (symbol-function '%d0e-f1)))
    (unwind-protect
         (progn
           (setf (symbol-function '%d0e-f1) (lambda (a) (list :new a)))
           (list (mapcar #'%d0e-f1 '(1)) (%d0e-sum '(a))))
      (setf (symbol-function '%d0e-f1) old)))
  (((:new 1)) 1))
