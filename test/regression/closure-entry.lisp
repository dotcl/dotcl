;;; A closure from a .fasl with up to four required parameters is called through
;;; an entry pointer, the way a DEFUN is: FUNCALL goes straight to the body
;;; instead of through InvokeN and the body delegate. The entry does what InvokeN
;;; did before the body (the periodic stack check), records no frame, and is
;;; never taken by a tail call that hands its frame on.

(defvar *ce-bt* nil)
(defun %ce-probe () (setf *ce-bt* (dotcl:backtrace)) 0)
(defun %ce-depth () (length (dotcl:backtrace)))
(defun %ce-state (f) (dotnet:static "DotCL.LispFunction" "EntryState" f))

(defparameter *ce-source* "
(defun %ce-mk0 (k) (lambda () k))
(defun %ce-mk1 (k) (lambda (a) (list k a)))
(defun %ce-mk2 (k) (lambda (a b) (list k a b)))
(defun %ce-mk4 (k) (lambda (a b c d) (list k a b c d)))
(defun %ce-mk5 (k) (lambda (a b c d e) (list k a b c d e)))
(defun %ce-mkopt (k) (lambda (a &optional (b 2)) (list k a b)))
(defun %ce-mkmv (k) (lambda (a) (values k a)))
(defun %ce-mkprobe (k) (lambda (a) (+ k a (%ce-probe))))
(defun %ce-tail (f x) (funcall f x))
(defun %ce-tail-quiet (f x) (declare (optimize (debug 0))) (funcall f x))
(defun %ce-mkrec ()
  (let ((f nil))
    (setq f (lambda (n) (if (zerop n) 0 (+ 1 (funcall f (- n 1))))))
    f))
")

(defun %ce-load ()
  (let ((src (regression-temp-file "closure-entry-src.lisp"))
        (fasl (regression-temp-file "closure-entry-src.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *ce-source* s))
    (load (compile-file src :output-file fasl))))

(when (compiled-mode-p) (%ce-load))

(deftest-compiled-only closure-entry.state
  (list (%ce-state (%ce-mk0 1)) (%ce-state (%ce-mk1 1)) (%ce-state (%ce-mk4 1))
        (%ce-state (%ce-mk5 1)) (%ce-state (%ce-mkopt 1))
        (%ce-state (let ((k 1)) (lambda (a) (list k a)))))
  ("closure-entry" "closure-entry" "closure-entry" "none" "none" "none"))

(deftest-compiled-only closure-entry.calls
  (let ((f1 (%ce-mk1 :k)) (f2 (%ce-mk2 :k)) (f4 (%ce-mk4 :k)))
    (list (funcall (%ce-mk0 :k))
          (funcall f1 1) (apply f1 '(2)) (mapcar f1 '(3 4))
          (funcall f2 1 2) (apply f2 1 '(2))
          (funcall f4 1 2 3 4) (apply f4 '(1 2 3 4))
          (handler-case (funcall f1 1 2) (program-error () :program-error))
          (handler-case (apply f2 '(1)) (program-error () :program-error))))
  (:k (:k 1) (:k 2) ((:k 3) (:k 4)) (:k 1 2) (:k 1 2) (:k 1 2 3 4) (:k 1 2 3 4)
   :program-error :program-error))

(deftest-compiled-only closure-entry.values
  (multiple-value-list (funcall (%ce-mkmv :k) 1))
  (:k 1))

(deftest-compiled-only closure-entry.global-name
  (progn (setf (symbol-function '%ce-named) (%ce-mk1 :n))
         (list (%ce-named 5) (funcall '%ce-named 6)))
  ((:n 5) (:n 6)))

;; A self-framed DEFUN calling a closure in tail position keeps its own frame
;; while the closure runs, and the frame list is back where it was afterwards.
(deftest-compiled-only closure-entry.tail-call-from-framed-body
  (let ((base (%ce-depth)) (f (%ce-mkprobe 10)) (r '()))
    (setf *ce-bt* nil)
    (push (%ce-tail f 1) r)
    (push (and (member "%CE-TAIL" *ce-bt* :test #'string=) t) r)
    (push (- (%ce-depth) base) r)
    (push (%ce-tail-quiet f 2) r)
    (push (- (%ce-depth) base) r)
    (push (handler-case (%ce-tail f "x") (type-error () :type-error)) r)
    (push (- (%ce-depth) base) r)
    (nreverse r))
  (11 t 0 12 0 :type-error 0))

(deftest-compiled-only closure-entry.stack-overflow-catchable
  (let ((f (%ce-mkrec)))
    (list (%ce-state f)
          (funcall f 1000)
          (call-on-small-stack
           (lambda ()
             (handler-case (progn (funcall f 10000000) :no-overflow)
               (error () :caught-error)
               (storage-condition () :caught-storage-condition))))))
  ("closure-entry" 1000 :caught-storage-condition))
