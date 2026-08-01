;;; INTERRUPT-THREAD: run a function on another thread.
;;;
;;; .NET has no way to interrupt arbitrary running code (Thread.Abort is gone),
;;; so delivery rides Thread.Interrupt: a thread sitting in a wait (SLEEP, JOIN,
;;; condition-variable, semaphore) is thrown out of it, and the queued function
;;; runs there. A thread busy computing is not interrupted — the function stays
;;; queued until it next blocks. That boundary is the contract, not a bug; the
;;; tests below only exercise the blocking side.
;;;
;;; The point of the primitive is timeouts: the queued function usually exits
;;; non-locally, which unwinds the target thread.

(require "dotcl-thread")

(defvar *it-flag* nil)

;;; The queued function runs on the interrupted thread and its effects are
;;; visible after the thread finishes.
(deftest interrupt-thread-runs-function
  (progn
    (setf *it-flag* nil)
    (let ((th (dotcl-thread:make-thread
               (lambda () (sleep 30) :slept-through)
               :name "it-runs")))
      ;; Let the target reach the sleep before interrupting.
      (sleep 0.2)
      (dotcl:interrupt-thread th (lambda () (setf *it-flag* :ran)))
      (dotcl-thread:thread-join th)
      (list *it-flag* (dotcl-thread:thread-alive-p th))))
  (:ran nil))

;;; A non-local exit from the queued function unwinds the target thread — this
;;; is how a timeout is delivered. The thread's own handler catches it.
(deftest interrupt-thread-non-local-exit
  (let* ((result (dotcl-thread:make-thread
                  (lambda ()
                    (handler-case (progn (sleep 30) :slept-through)
                      (error () :interrupted)))
                  :name "it-nlx")))
    (sleep 0.2)
    (dotcl:interrupt-thread result (lambda () (error "time is up")))
    (dotcl-thread:thread-join result))
  :interrupted)

;;; Interrupting a thread that has already finished is a no-op, not an error.
(deftest interrupt-thread-dead-thread
  (let ((th (dotcl-thread:make-thread (lambda () :done) :name "it-dead")))
    (dotcl-thread:thread-join th)
    (dotcl:interrupt-thread th (lambda () (setf *it-flag* :should-not-run))))
  nil)

;;; A thread blocked on a semaphore is interruptible too (the wait gives up).
(deftest interrupt-thread-semaphore-wait
  (progn
    (setf *it-flag* nil)
    (let* ((sem (dotcl:make-semaphore :count 0))
           (th (dotcl-thread:make-thread
                (lambda () (dotcl:wait-on-semaphore sem) :acquired)
                :name "it-sem")))
      (sleep 0.2)
      (dotcl:interrupt-thread th (lambda () (setf *it-flag* :sem-ran)))
      (dotcl-thread:thread-join th)
      *it-flag*))
  :sem-ran)

;;; DESTROY-THREAD pokes the target the same way (that is all .NET offers), but
;;; queues nothing — so the interrupt must still end the thread rather than be
;;; swallowed as a spurious wakeup. bt's generic WITH-TIMEOUT destroys its
;;; watchdog on the normal path, and a watchdog that survived would go on to
;;; deliver a stale timeout.
(deftest interrupt-thread-destroy-still-kills
  (progn
    (setf *it-flag* nil)
    (let ((th (dotcl-thread:make-thread
               (lambda () (sleep 30) (setf *it-flag* :ran-past-sleep))
               :name "it-destroy")))
      (sleep 0.2)
      (dotcl-thread:destroy-thread th)
      (dotcl-thread:thread-join th)
      (list *it-flag* (dotcl-thread:thread-alive-p th))))
  (nil nil))

;;; Arguments are checked.
(deftest interrupt-thread-requires-thread
  (handler-case (progn (dotcl:interrupt-thread :not-a-thread (lambda ())) :no-error)
    (error () :error))
  :error)
