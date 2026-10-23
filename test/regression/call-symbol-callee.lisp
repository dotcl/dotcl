;;; An ordinary call site takes the function its name's symbol keeps without a
;;; type check. Every way of changing the function under the name between two
;;; calls must be seen by the next call, in memory and from a fasl: in plain,
;;; multiple-value and tail position.

(declaim (notinline %csc-f))
(defun %csc-f (a b) (+ a b))
(defun %csc-other (a b) (- a b 1000))

(defun %csc-call (a b) (list (%csc-f a b)))
(defun %csc-mv (a b) (multiple-value-list (%csc-f a b)))
(defun %csc-tail (a b) (%csc-f a b))

(defun %csc-sequence (call define)
  (list (funcall call 1 2)
        (progn (funcall define '(lambda (a b) (* a b))) (funcall call 3 4))
        (progn (setf (fdefinition '%csc-f) #'%csc-other) (funcall call 3 4))
        (let ((k 7))
          (setf (fdefinition '%csc-f) (lambda (a b) (+ a b k)))
          (funcall call 10 4))
        (progn (funcall define '(lambda (a b) (values b a))) (funcall call 1 2))
        (progn (fmakunbound '%csc-f)
               (handler-case (funcall call 1 1)
                 (undefined-function (c) (list :undefined (cell-error-name c)))))
        (progn (funcall define '(lambda (a b &optional (c 100)) (+ a b c)))
               (funcall call 1 2))
        (progn (funcall define '(lambda (a) a))
               (handler-case (funcall call 1 2) (program-error () :program-error)))
        (progn (fmakunbound '%csc-f)
               (eval '(defgeneric %csc-f (a b)))
               (eval '(defmethod %csc-f ((a integer) b) (list :gf a b)))
               (funcall call 1 2))
        (eq (symbol-function '%csc-f) (fdefinition '%csc-f))
        (progn (fmakunbound '%csc-f)
               (funcall define '(lambda (a b) (+ a b))) (funcall call 5 6))))

;;; TRACE replaces the function under the name and UNTRACE puts it back.
(defun %csc-trace-sequence (call define)
  (let ((out (make-string-output-stream)))
    (funcall define '(lambda (a b) (+ a b 50)))
    (eval '(trace %csc-f))
    (list (let ((*trace-output* out)) (funcall call 1 2))
          (progn (eval '(untrace %csc-f)) (funcall call 1 2))
          (plusp (length (get-output-stream-string out)))
          (progn (funcall define '(lambda (a b) (+ a b))) (funcall call 5 6)))))

(defun %csc-define-in-memory (lambda)
  (eval `(defun %csc-f ,@(cdr lambda))))

(defun %csc-expected (wrap first)
  (list (funcall wrap 3) (funcall wrap 12) (funcall wrap -1001) (funcall wrap 21)
        first
        '(:undefined %csc-f)
        (funcall wrap 103) :program-error (funcall wrap '(:gf 1 2))
        t (funcall wrap 11)))

(defun %csc-trace-expected (wrap)
  (list (funcall wrap 53) (funcall wrap 53) t (funcall wrap 11)))

(defvar *csc-callers* '(%csc-call %csc-mv %csc-tail))

(defun %csc-all (callers define)
  (loop for c in callers collect (%csc-sequence c define)))

(defun %csc-all-expected ()
  (list (%csc-expected #'list '(2))
        (%csc-expected #'list '(2 1))
        (%csc-expected #'identity 2)))

(defun %csc-trace-all (callers define)
  (loop for c in callers collect (%csc-trace-sequence c define)))

(defun %csc-trace-all-expected ()
  (list (%csc-trace-expected #'list) (%csc-trace-expected #'list)
        (%csc-trace-expected #'identity)))

(deftest call-symbol-callee.in-memory
  (equal (%csc-all *csc-callers* #'%csc-define-in-memory) (%csc-all-expected))
  t)

;; Not under the interpreter: there UNTRACE leaves the original function
;; findable by name, and a later FMAKUNBOUND does not reach it.
(deftest-emitting-only call-symbol-callee.trace
  (equal (%csc-trace-all *csc-callers* #'%csc-define-in-memory) (%csc-trace-all-expected))
  t)

(defvar *csc-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-csc-test/")))
    (ensure-directories-exist dir)
    dir))

(defvar *csc-fasl-count* 0)

(defun %csc-compile-and-load (source)
  (let ((lisp (format nil "~acsc-~d.lisp" *csc-dir* (incf *csc-fasl-count*))))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string "(in-package :cl-user)" s)
      (write-string source s))
    (load (compile-file lisp))
    t))

(defun %csc-define-from-fasl (lambda)
  (%csc-compile-and-load (format nil "~s" `(defun %csc-f ,@(cdr lambda)))))

(deftest-compiled-only call-symbol-callee.fasl
  (progn
    (%csc-define-from-fasl '(lambda (a b) (+ a b)))
    (%csc-compile-and-load
     "(defun %csc-fasl-call (a b) (list (%csc-f a b)))
      (defun %csc-fasl-mv (a b) (multiple-value-list (%csc-f a b)))
      (defun %csc-fasl-tail (a b) (%csc-f a b))")
    (let ((callers (mapcar #'intern '("%CSC-FASL-CALL" "%CSC-FASL-MV" "%CSC-FASL-TAIL"))))
      (list (equal (%csc-all callers #'%csc-define-from-fasl) (%csc-all-expected))
            (equal (%csc-all callers #'%csc-define-in-memory) (%csc-all-expected))
            (equal (%csc-trace-all callers #'%csc-define-from-fasl) (%csc-trace-all-expected)))))
  (t t t))
