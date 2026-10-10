;;; RELEASE-LOCK on a lock the current thread does not hold leaves it alone and
;;; returns NIL, as SBCL's RELEASE-MUTEX does by default (:IF-NOT-OWNER :PUNT).
;;; It threw SynchronizationLockException ("Object synchronization method was
;;; called from an unsynchronized block of code"). anypool releases its pool
;;; lock early inside an UNWIND-PROTECT whose cleanup releases it again, and 4
;;; of its tests failed on that. Expected values are SBCL 2.6.8's answers
;;; (SB-THREAD:RELEASE-MUTEX); a held lock is still released.
(require "dotcl-thread")

(deftest release-lock-not-held.never-acquired
  (let ((lock (dotcl:make-lock "rl-1")))
    (list (dotcl:release-lock lock)
          (progn (dotcl:acquire-lock lock) (dotcl:release-lock lock) :released)))
  (nil :released))

(deftest release-lock-not-held.twice
  (let ((lock (dotcl:make-lock "rl-2")))
    (dotcl:acquire-lock lock)
    (dotcl:release-lock lock)
    (list (dotcl:release-lock lock)
          ;; still usable afterwards
          (progn (dotcl:acquire-lock lock) (dotcl:release-lock lock) :ok)))
  (nil :ok))

;; Held by another thread: not released, and that thread still owns it.
(deftest release-lock-not-held.other-thread
  (let* ((lock (dotcl:make-lock "rl-3"))
         (state :start)
         (th (dotcl-thread:make-thread
              (lambda ()
                (dotcl:acquire-lock lock)
                (setf state :held)
                (loop until (eq state :done) do (sleep 0.01))
                (dotcl:release-lock lock)))))
    (loop until (eq state :held) do (sleep 0.01))
    (let ((r (dotcl:release-lock lock))
          (free-p (dotcl:acquire-lock lock nil)))
      (setf state :done)
      (dotcl-thread:thread-join th)
      (list r free-p)))
  (nil nil))
