;;; A thread made by MAKE-THREAD runs Lisp with as much stack as the main
;;; thread. It used to get the .NET default, much smaller than the main
;;; thread's, so code nested a few thousand levels deep (a COND or CASE with
;;; thousands of clauses) compiled at the main thread's REPL but overflowed the
;;; stack when a SLIME / SLY worker or a bordeaux-threads thread compiled or
;;; evaluated it.

(require "dotcl-thread")

(defun %tsdn-in-thread (thunk)
  (let ((result nil))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread
      (lambda ()
        (setq result (handler-case (funcall thunk)
                       (serious-condition (e) (princ-to-string e)))))))
    result))

(defun %tsdn-cond (n)
  `(lambda (x) (cond ,@(loop for i below n collect `((= x ,i) ,(* 2 i))) (t :none))))

(deftest-emitting-only thread-stack-deep-nesting.compile-cond
  (%tsdn-in-thread (lambda () (funcall (compile nil (%tsdn-cond 3000)) 2999)))
  5998)

(deftest thread-stack-deep-nesting.eval-cond
  (%tsdn-in-thread (lambda () (funcall (eval (%tsdn-cond 3000)) 2998)))
  5996)
