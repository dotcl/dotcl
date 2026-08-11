;;; Loop back-edge interrupt safepoints.
;;;
;;; A loop whose body contains no Lisp calls never reaches the periodic check
;;; in LispFunction.Invoke, so without a back-edge poll it cannot be stopped
;;; by Ctrl-C. The compiler emits ConditionSystem.PollInterrupt on the tagbody
;;; dispatch label (when the tagbody has a go) and before TCO back-branches.
;;; (declare (optimize (safety 0))) in the enclosing body opts the loops out.
;;; ConditionSystem.PollCount is a diagnostic counter; the tests read its
;;; delta, which is immune to some OTHER call consuming the pending-interrupt
;;; flag first.

;; A pending interrupt stops a call-free loop. If the poll is broken this
;; hangs the suite rather than passing silently — that is the point.
(deftest interrupt-poll.bare-loop-stops
  (handler-case
      (progn (dotnet:static "DotCL.ConditionSystem" "RequestInterrupt")
             (loop))
    (interactive-interrupt () :interrupted))
  :interrupted)

;; Every iteration of a call-free dotimes passes the safepoint.
(deftest interrupt-poll.callfree-loop-polls
  (let ((before (dotnet:static "DotCL.ConditionSystem" "PollCount"))
        (s 0))
    (dotimes (i 1000) (setq s (+ s 1)))
    (>= (- (dotnet:static "DotCL.ConditionSystem" "PollCount") before) 1000))
  t)

;; (optimize (safety 0)) opts the body's loops out: the same shape adds
;; (nearly) no polls.
(defun %s575-nopoll-spin ()
  (declare (optimize (safety 0)))
  (let ((s 0))
    (dotimes (i 100000) (setq s (+ s 1)))
    s))

(deftest-compiled-only interrupt-poll.safety0-opts-out
  (let ((before (dotnet:static "DotCL.ConditionSystem" "PollCount")))
    (%s575-nopoll-spin)
    (< (- (dotnet:static "DotCL.ConditionSystem" "PollCount") before) 1000))
  t)

;; A TCO'd self-call loop is a loop too: its back-branch polls.
(defun %s575-tco-count (i n)
  (if (>= i n) i (%s575-tco-count (+ i 1) n)))

(deftest interrupt-poll.tco-loop-polls
  (let ((before (dotnet:static "DotCL.ConditionSystem" "PollCount")))
    (%s575-tco-count 0 1000)
    (>= (- (dotnet:static "DotCL.ConditionSystem" "PollCount") before) 999))
  t)

;; Tier 2: INTERRUPT-THREAD reaches a thread that is COMPUTING (spinning in a
;; call-free loop), not waiting — the queued function runs at the safepoint
;; and its throw unwinds the worker out of the loop.
(deftest interrupt-poll.interrupt-thread-computing
  (let ((th (dotcl:make-thread
             (lambda () (catch 'stop (loop)) :stopped))))
    (sleep 0.3)
    (dotcl:interrupt-thread th (lambda () (throw 'stop nil)))
    (dotcl:thread-join th))
  :stopped)

;; A normally-returning interrupt must not poison the worker's next wait: the
;; Thread.Interrupt poke that accompanied the enqueue is absorbed at the
;; safepoint, so the later SLEEP completes instead of dying with a spurious
;; not-ours interrupt.
(defvar *s602-flag* nil)
(deftest interrupt-poll.interrupt-normal-return-then-wait
  (progn
    (setq *s602-flag* nil)
    (let ((th (dotcl:make-thread
               (lambda ()
                 (loop until *s602-flag*)
                 (sleep 0.2)
                 :done))))
      (sleep 0.3)
      (dotcl:interrupt-thread th (lambda () (setq *s602-flag* t)))
      (dotcl:thread-join th)))
  :done)

;; DESTROY-THREAD reaches a COMPUTING thread: the destroy sentinel is
;; delivered at a loop safepoint, so even a call-free spin dies (Thread
;; .Interrupt alone only reaches waiting threads). thread-join hangs here if
;; delivery is broken.
(deftest interrupt-poll.destroy-computing-thread
  (let ((th (dotcl:make-thread (lambda () (loop)))))
    (sleep 0.3)
    (dotcl:destroy-thread th)
    (dotcl:thread-join th)
    :dead)
  :dead)

;; DESTROY-THREAD is a control transfer, not an error: a handler-case (error)
;; in the worker must NOT swallow it (the ThreadInterruptedException passes
;; through the handler machinery unwrapped and ends the thread).
(defvar *s603-note* nil)
(deftest interrupt-poll.destroy-not-swallowed-by-handler-case
  (progn
    (setq *s603-note* nil)
    (let ((th (dotcl:make-thread
               (lambda ()
                 (handler-case (loop (sleep 0.05))
                   (error () (setq *s603-note* :swallowed)))
                 (setq *s603-note* (or *s603-note* :exited-normally))))))
      (sleep 0.3)
      (dotcl:destroy-thread th)
      (dotcl:thread-join th)
      *s603-note*))
  nil)
