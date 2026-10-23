;;; asdf-parallel.lisp -- build an ASDF plan with several compiles at once.
;;;
;;; Usage:
;;;
;;;   (require "asdf-parallel")                 ; dotcl
;;;   (load "asdf-parallel.lisp")               ; any other Lisp, after ASDF
;;;   (asdf-parallel:load-system "ironclad" :jobs 4)
;;;
;;; or, with plain ASDF calls:
;;;
;;;   (asdf:load-system "ironclad" :plan-class 'asdf-parallel:parallel-plan
;;;                                :plan-options '(:jobs 4))
;;;
;;; The plan is ASDF's own sequential plan. Performing it differs in one way:
;;; the compile-op of a Lisp source file is handed to a worker as soon as every
;;; action it depends on is done, while every other action (load-op, prepare-op,
;;; the system-level operations) runs in the calling thread in plan order, once
;;; its own dependencies are done. Loads therefore happen in an order ASDF's
;;; dependencies allow, and everything the compile of a file may see at compile
;;; time is what it would see in a serial build, as far as the system declares
;;; its dependencies.
;;;
;;; How a worker runs is up to an executor:
;;;
;;;   :thread  a thread in this image (dotcl). Compile-time side effects of
;;;            one file are visible to the others, as in a serial build.
;;;   :fork    a forked child process (SBCL, whose compiler holds a global lock
;;;            and does not run on two threads at once). Only the fasl comes
;;;            back; the parent loads it.
;;;   :serial  no workers: the plan runs as ASDF's would. The fallback.
;;;
;;; A compile that fails in a worker is not reported from there. It is
;;; performed again in the calling thread once every action before it in the
;;; plan is done, where a serial build performs it: then it either works (the
;;; system did not declare something the file needs, and *STATISTICS* lists it
;;; under :RETRIED), or fails with the error and restarts of a serial build.

(require "asdf")

(defpackage :asdf-parallel
  (:use :cl)
  (:export #:parallel-plan
           #:load-system
           #:*jobs*
           #:*executor*
           #:*worker-specials*
           #:parallel-action-p
           #:*statistics*))

(in-package :asdf-parallel)

(defvar *jobs* 4
  "How many compiles run at once.")

(defvar *executor*
  #+dotcl :thread
  #+(and sbcl unix) :fork
  #-(or dotcl (and sbcl unix)) :serial
  "How workers run: :THREAD, :FORK or :SERIAL.")

(defvar *worker-specials*
  '(*package* *readtable* *default-pathname-defaults*
    *compile-verbose* *compile-print* *load-verbose* *load-print*
    *standard-output* *error-output* *trace-output*
    uiop:*compile-file-warnings-behaviour* uiop:*compile-file-failure-behaviour*
    uiop:*uninteresting-conditions* uiop:*uninteresting-compiler-conditions*
    asdf::*verbose-out*)
  "Special variables a :THREAD worker binds, while it compiles, to their values
   in the calling thread when the build starts. A new thread otherwise sees the
   global values. A variable that the build itself changes globally, such as
   *FEATURES* when a library pushes onto it, must not be here: the worker would
   keep the value from the start.")

(defvar *statistics* nil
  "What the last parallel perform did: a plist of the number of actions, how
   many went to workers, the most that ran at once, and the actions a worker
   failed on and the calling thread performed again.")

(defclass parallel-plan (asdf:sequential-plan)
  ((jobs :initarg :jobs :initform *jobs* :reader plan-jobs)
   (executor :initarg :executor :initform *executor* :reader plan-executor))
  (:documentation "A sequential plan whose compile-ops are performed by workers."))

(defgeneric parallel-action-p (operation component)
  (:documentation "True when the action may be performed by a worker.")
  (:method (o c) (declare (ignore o c)) nil)
  (:method ((o asdf:compile-op) (c asdf:cl-source-file)) t))

(defun load-system (system &rest keys &key (jobs *jobs*) (executor *executor*)
                    &allow-other-keys)
  "ASDF:LOAD-SYSTEM with up to JOBS compiles at once."
  (let ((keys (loop for (k v) on keys by #'cddr
                    unless (member k '(:jobs :executor)) append (list k v))))
    (let ((*jobs* jobs) (*executor* executor)
          (asdf::*plan-class* 'parallel-plan))
      (apply #'asdf:load-system system keys))))

;;; ------------------------------------------------------------------
;;; Executors. SUBMIT starts a job (a cons of the action and the thunk
;;; that performs it); TAKE waits until some job has ended and returns
;;; it and T, or a string saying why it failed.

(defgeneric executor-submit (executor job))
(defgeneric executor-take (executor))
(defgeneric executor-close (executor))

(defun make-job (action thunk) (cons action thunk))
(defun job-action (job) (car job))
(defun job-thunk (job) (cdr job))

(defun run-job (job)
  "T, or what went wrong as a string."
  (handler-case (progn (funcall (job-thunk job)) t)
    (serious-condition (c)
      (or (ignore-errors (princ-to-string c)) (string (type-of c))))))

(defclass serial-executor ()
  ((done :initform nil :accessor done)))

(defmethod executor-submit ((e serial-executor) job)
  (push (cons job (run-job job)) (done e)))

(defmethod executor-take ((e serial-executor))
  (let ((r (pop (done e))))
    (values (car r) (cdr r))))

(defmethod executor-close ((e serial-executor)) nil)

#+dotcl
(progn
  (defclass thread-executor ()
    ((lock :initform (dotcl:make-lock "asdf-parallel") :reader lock)
     (queue :initform nil :accessor queue)
     (jobs-ready :initform (dotcl:make-semaphore) :reader jobs-ready)
     (done :initform nil :accessor done)
     (done-ready :initform (dotcl:make-semaphore) :reader done-ready)
     (threads :initform nil :accessor threads)))

  (defmacro with-executor-lock ((e) &body body)
    `(progn (dotcl:acquire-lock (lock ,e))
            (unwind-protect (progn ,@body) (dotcl:release-lock (lock ,e)))))

  (defun thread-worker (e syms vals)
    (progv syms vals
      (loop
        (dotcl:wait-on-semaphore (jobs-ready e))
        (let ((job (with-executor-lock (e) (pop (queue e)))))
          (when (eq job :stop) (return))
          (let ((result (run-job job)))
            (with-executor-lock (e) (push (cons job result) (done e)))
            (dotcl:signal-semaphore (done-ready e)))))))

  (defun make-thread-executor (n)
    (let ((e (make-instance 'thread-executor))
          (syms (remove-if-not #'boundp *worker-specials*)))
      (let ((vals (mapcar #'symbol-value syms)))
        (dotimes (i n)
          (push (dotcl:make-thread (lambda () (thread-worker e syms vals))
                                   :name (format nil "asdf-parallel ~D" i))
                (threads e))))
      e))

  (defmethod executor-submit ((e thread-executor) job)
    (with-executor-lock (e) (setf (queue e) (append (queue e) (list job))))
    (dotcl:signal-semaphore (jobs-ready e)))

  (defmethod executor-take ((e thread-executor))
    (dotcl:wait-on-semaphore (done-ready e))
    (let ((r (with-executor-lock (e) (pop (done e)))))
      (values (car r) (cdr r))))

  (defmethod executor-close ((e thread-executor))
    (with-executor-lock (e)
      (setf (queue e) (append (queue e) (make-list (length (threads e)) :initial-element :stop))))
    (dotcl:signal-semaphore (jobs-ready e) (length (threads e)))
    (dolist (th (threads e)) (dotcl:thread-join th))))

#+(and sbcl unix)
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (require :sb-posix))

  (defclass fork-executor ()
    ((children :initform (make-hash-table) :reader children)))

  (defmethod executor-submit ((e fork-executor) job)
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (let ((pid (sb-posix:fork)))
      (if (zerop pid)
          (let* ((result (run-job job))
                 (code (if (eq result t) 0 1)))
            (unless (eq result t)
              (ignore-errors (format *error-output* "~&; asdf-parallel: ~A~%" result)))
            (ignore-errors (finish-output *standard-output*))
            (ignore-errors (finish-output *error-output*))
            (sb-ext:exit :code code :abort t))
          (setf (gethash pid (children e)) job))))

  (defmethod executor-take ((e fork-executor))
    (loop
      (multiple-value-bind (pid status) (sb-posix:waitpid -1 0)
        (let ((job (gethash pid (children e))))
          (when job
            (remhash pid (children e))
            (return (values job (if (and (sb-posix:wifexited status)
                                         (zerop (sb-posix:wexitstatus status)))
                                    t
                                    (format nil "child process status ~D" status)))))))))

  (defmethod executor-close ((e fork-executor))
    (loop while (plusp (hash-table-count (children e)))
          do (executor-take e))))

(defun make-executor (kind jobs)
  (declare (ignorable jobs))
  (case kind
    #+dotcl (:thread (make-thread-executor jobs))
    #+(and sbcl unix) (:fork (make-instance 'fork-executor))
    (t (make-instance 'serial-executor))))

;;; ------------------------------------------------------------------
;;; Performing the plan.

(defun action-key (o c) (cons o c))

(defun plan-dependencies (actions)
  "A table from each action of ACTIONS to the actions of ACTIONS it waits for.
   A dependency outside ACTIONS is already done; the ones it depends on are
   followed through it."
  (let ((in-plan (make-hash-table :test 'equal))
        (memo (make-hash-table :test 'equal))
        (deps (make-hash-table :test 'equal)))
    (dolist (a actions) (setf (gethash a in-plan) t))
    (labels ((through (o c)
               (let ((key (action-key o c)))
                 (multiple-value-bind (v hit) (gethash key memo)
                   (if hit v
                       (progn
                         (setf (gethash key memo) nil)
                         (setf (gethash key memo) (direct o c)))))))
             (direct (o c)
               (let ((acc '()))
                 (asdf::map-direct-dependencies
                  o c (lambda (do dc)
                        (let ((k (action-key do dc)))
                          (if (gethash k in-plan)
                              (pushnew k acc :test 'equal)
                              (dolist (x (through do dc))
                                (pushnew x acc :test 'equal))))))
                 acc)))
      (dolist (a actions)
        (setf (gethash a deps)
              (remove a (direct (car a) (cdr a)) :test 'equal))))
    deps))

(defun worker-session ()
  "A session for a :THREAD worker: its own record of the actions being
   visited, and a copy of the calling session's cache."
  (let* ((s asdf::*asdf-session*)
         (cache (asdf::session-cache s))
         (copy (make-hash-table :test (hash-table-test cache)
                                :size (max 16 (hash-table-count cache)))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) cache)
    (make-instance (class-of s)
                   :ancestor (asdf::toplevel-asdf-session)
                   :forcing (asdf::forcing s)
                   :session-cache copy)))

(defun worker-thunk (kind o c)
  (if (eq kind :thread)
      (let ((session (worker-session)))
        (lambda ()
          (let ((asdf::*asdf-session* session))
            (asdf:perform o c))))
      (lambda () (asdf:perform o c))))

(defun note-performed-elsewhere (plan o c)
  "Record in this image that a worker performed (O . C): the stamps of its
   output files, then the action itself."
  (dolist (f (asdf:output-files o c))
    (asdf::register-file-stamp f))
  (asdf::mark-operation-done o c)
  (asdf::mark-as-done plan o c))

(defmethod asdf:perform-plan ((plan parallel-plan) &key)
  (let* ((actions (loop for a in (asdf::plan-actions plan)
                        for o = (asdf::action-operation a)
                        for c = (asdf::action-component a)
                        unless (asdf::action-already-done-p plan o c)
                          collect (action-key o c)))
         (jobs (max 1 (plan-jobs plan)))
         (kind (if (> jobs 1) (plan-executor plan) :serial)))
    (if (or (eq kind :serial)
            (notany (lambda (a) (parallel-action-p (car a) (cdr a))) actions))
        (call-next-method)
        (perform-in-parallel plan actions jobs kind))))

(defun perform-in-parallel (plan actions jobs kind)
  "Run ACTIONS, in plan order, as described at the top of this file. A compile
   that fails in a worker is put back and performed in the calling thread once
   every action before it in the plan is done, where a serial build performs it:
   the usual cause is a dependency the system does not declare."
  (let* ((deps (plan-dependencies actions))
         (order (coerce actions 'vector))
         (index (let ((h (make-hash-table :test 'equal)))
                  (loop for a across order for i from 0 do (setf (gethash a h) i))
                  h))
         (done (make-hash-table :test 'equal))
         (prefix 0)
         (fallback (make-hash-table :test 'equal))
         (remaining actions)
         (running 0)
         (dispatched 0)
         (peak 0)
         (retried '())
         (executor (make-executor kind jobs)))
    (labels ((ready-p (a)
               (if (gethash a fallback)
                   (>= prefix (gethash a index))
                   (every (lambda (d) (gethash d done)) (gethash a deps))))
             (worker-p (a)
               (and (parallel-action-p (car a) (cdr a)) (not (gethash a fallback))))
             (finish (a)
               (setf (gethash a done) t)
               (loop while (and (< prefix (length order)) (gethash (aref order prefix) done))
                     do (incf prefix)))
             (put-back (a)
               (setf (gethash a fallback) t
                     remaining (merge 'list (list a) remaining #'<
                                      :key (lambda (x) (gethash x index))))))
      (unwind-protect
           (loop while (or remaining (plusp running)) do
             (loop for a in remaining
                   while (< running jobs)
                   when (and (worker-p a) (ready-p a))
                     do (setf remaining (remove a remaining :test 'eq :count 1))
                        (incf running)
                        (incf dispatched)
                        (setf peak (max peak running))
                        (executor-submit executor
                                         (make-job a (worker-thunk kind (car a) (cdr a)))))
             (let ((next (find-if (lambda (a) (and (not (worker-p a)) (ready-p a)))
                                  remaining)))
               (cond
                 (next
                  (setf remaining (remove next remaining :test 'eq :count 1))
                  (asdf::perform-with-restarts (car next) (cdr next))
                  (asdf::mark-as-done plan (car next) (cdr next))
                  (finish next))
                 ((plusp running)
                  (multiple-value-bind (job result) (executor-take executor)
                    (decf running)
                    (let* ((a (job-action job)) (o (car a)) (c (cdr a)))
                      (cond ((eq result t)
                             (note-performed-elsewhere plan o c)
                             (finish a))
                            (t
                             (push (list (asdf:action-description o c) result) retried)
                             (put-back a))))))
                 (t
                  (error "asdf-parallel: no action of ~D can proceed" (length remaining))))))
        (loop while (plusp running)
              do (executor-take executor) (decf running))
        (executor-close executor)))
    (setf *statistics*
          (list :actions (length actions) :parallel dispatched :peak peak
                :jobs jobs :executor kind :retried (reverse retried)))
    (values)))
