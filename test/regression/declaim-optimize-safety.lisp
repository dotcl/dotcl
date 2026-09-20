;;; DECLAIM (OPTIMIZE (SAFETY 0)) has to reach the loop back-edge safepoints.
;;;
;;; The safepoint opt-out read the function body's own leading declarations and
;;; nothing else, so a file-level DECLAIM -- which CLHS 3.3.4 makes the default
;;; for everything compiled after it -- left every poll in place. The same loop
;;; then ran three times slower depending only on where the declaration was
;;; written, with nothing to tell the reader which one counted.
;;;
;;; Pinned here in all three directions: the global value applies to what is
;;; compiled after it, a later DECLAIM takes it back, and a declaration in the
;;; body outranks the global either way.
;;;
;;; The subject is emitted code, so these read the SIL. This file is also
;;; deliberately loaded just before interrupt-poll.lisp: if the DECLAIM below
;;; leaked past the end of this file, the polls the tests there count would be
;;; gone.

(setf dotcl:*save-sil* t)

(defun %dos-polls-p (fn)
  "True when FN's SIL still carries a loop back-edge interrupt poll."
  (and (search "PollInterrupt" (princ-to-string (dotcl:function-sil fn))) t))

;;; ---- with nothing in force, the loop polls ----

(defun %dos-default (n)
  (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))

(deftest-compiled-only declaim-optimize-safety.default-polls
  (%dos-polls-p #'%dos-default)
  t)

;;; ---- the global value applies to what is compiled after it ----

(declaim (optimize (safety 0)))

(defun %dos-after-declaim (n)
  (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))

(deftest-compiled-only declaim-optimize-safety.declaim-removes-poll
  (%dos-polls-p #'%dos-after-declaim)
  nil)

;;; ---- a body declaration outranks it, here putting the poll back ----

(defun %dos-body-safety-1 (n)
  (declare (optimize (safety 1)))
  (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))

(deftest-compiled-only declaim-optimize-safety.body-declaration-wins
  (%dos-polls-p #'%dos-body-safety-1)
  t)

;;; ---- a later DECLAIM takes the global back ----

(declaim (optimize (safety 1)))

(defun %dos-after-restore (n)
  (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))

(deftest-compiled-only declaim-optimize-safety.later-declaim-restores
  (%dos-polls-p #'%dos-after-restore)
  t)

;;; ---- and the body declaration still opts out under the restored global ----

(defun %dos-body-safety-0 (n)
  (declare (optimize (safety 0)))
  (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))

(deftest-compiled-only declaim-optimize-safety.body-safety-0-opts-out
  (%dos-polls-p #'%dos-body-safety-0)
  nil)

;;; ---- the declaration changes the polls and nothing else ----

(deftest declaim-optimize-safety.values-unchanged
  (list (%dos-default 10) (%dos-after-declaim 10) (%dos-body-safety-1 10)
        (%dos-after-restore 10) (%dos-body-safety-0 10))
  (45 45 45 45 45))

(setf dotcl:*save-sil* nil)
