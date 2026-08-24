;;; Regression tests for DOTCL:CALL-ON-MAIN-THREAD / DOTCL:MAIN-THREAD-P.
;;;
;;; Lisp does not run on the thread the process starts on: the entry point moves
;;; it to a thread with a 256 MB stack so deep macro expansion does not overflow.
;;; UI toolkits that accept nothing but thread 0 (macOS AppKit) therefore need a
;;; way back onto it, which is what CALL-ON-MAIN-THREAD provides. The assertions
;;; here are about which thread the work runs on, so they hold on every platform.

;;; Lisp itself runs off the main thread.
(deftest main-thread-p-off-main
  (dotcl:main-thread-p)
  nil)

;;; Submitted work runs ON the main thread. This is the property the GUI needs.
(deftest call-on-main-thread-runs-on-main
  (dotcl:call-on-main-thread (lambda () (dotcl:main-thread-p)))
  t)

;;; The value of the function comes back to the caller.
(deftest call-on-main-thread-value
  (dotcl:call-on-main-thread (lambda () (+ 1 2)))
  3)

;;; The function keeps the caller's lexical environment.
(deftest call-on-main-thread-closure
  (let ((x 40))
    (dotcl:call-on-main-thread (lambda () (+ x 2))))
  42)

;;; Side effects are visible to the caller once the call returns.
(deftest call-on-main-thread-side-effect
  (let ((cell (list 0)))
    (dotcl:call-on-main-thread (lambda () (setf (car cell) 7)))
    (car cell))
  7)

;;; An error signalled on the main thread reaches the caller's handler rather
;;; than tearing down the pump.
(deftest call-on-main-thread-error
  (handler-case (dotcl:call-on-main-thread (lambda () (error "boom")))
    (error (c) (format nil "~a" c)))
  "boom")

;;; The pump survives that error and still takes work.
(deftest call-on-main-thread-after-error
  (dotcl:call-on-main-thread (lambda () (dotcl:main-thread-p)))
  t)

;;; Submitting from the main thread runs the work in place instead of queueing
;;; it behind the call that is already occupying the thread.
(deftest call-on-main-thread-reentrant
  (dotcl:call-on-main-thread
   (lambda ()
     (dotcl:call-on-main-thread (lambda () (list (dotcl:main-thread-p) :inner)))))
  (t :inner))
