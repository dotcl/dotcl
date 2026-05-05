;;; dotcl-thread.lisp — Thread support for dotcl (bordeaux-threads compatible)
;;;
;;; Usage: (require "dotcl-thread")
;;;
;;; Provides DOTCL-THREAD package with bordeaux-threads compatible API.

(defpackage :dotcl-thread
  (:use :cl)
  (:export #:make-thread
           #:current-thread
           #:thread-alive-p
           #:destroy-thread
           #:thread-name
           #:threadp
           #:thread-yield
           #:make-lock
           #:acquire-lock
           #:release-lock
           #:with-lock-held
           #:make-recursive-lock
           #:with-recursive-lock-held
           #:thread-join
           #:make-condition-variable
           #:condition-wait
           #:condition-notify
           #:condition-broadcast
           #:make-semaphore
           #:signal-semaphore
           #:wait-on-semaphore))

(in-package :dotcl-thread)

(defun make-thread (function &key (name "Anonymous"))
  "Create and start a new thread running FUNCTION.
   The new thread inherits the parent thread's dynamic bindings."
  (%make-thread function :name name))

(defun current-thread ()
  "Return the current thread object."
  (%current-thread))

(defun thread-alive-p (thread)
  "Return T if THREAD is still running."
  (%thread-alive-p thread))

(defun destroy-thread (thread)
  "Interrupt THREAD."
  (%destroy-thread thread))

(defun thread-name (thread)
  "Return the name of THREAD."
  (%thread-name thread))

(defun threadp (object)
  "Return T if OBJECT is a thread."
  (%threadp object))

(defun thread-yield ()
  "Hint to the scheduler to run other threads."
  (%thread-yield))

(defun make-lock (&optional (name "anonymous"))
  "Create a new lock (mutex)."
  (%make-lock name))

(defun acquire-lock (lock &optional (wait-p t))
  "Acquire LOCK. If WAIT-P is NIL, return immediately with NIL if unavailable."
  (%acquire-lock lock wait-p))

(defun release-lock (lock)
  "Release LOCK."
  (%release-lock lock))

(defmacro with-lock-held ((lock) &body body)
  "Execute BODY with LOCK held, releasing it on exit."
  (let ((l (gensym "LOCK")))
    `(let ((,l ,lock))
       (acquire-lock ,l)
       (unwind-protect
         (progn ,@body)
         (release-lock ,l)))))

(defun make-recursive-lock (&optional (name "anonymous"))
  "Create a new re-entrant lock (same thread may acquire multiple times)."
  (%make-recursive-lock name))

(defmacro with-recursive-lock-held ((lock) &body body)
  "Execute BODY with LOCK held, allowing re-entry from the same thread."
  (let ((l (gensym "RLOCK")))
    `(let ((,l ,lock))
       (acquire-lock ,l)
       (unwind-protect
         (progn ,@body)
         (release-lock ,l)))))

(defun thread-join (thread)
  "Wait for THREAD to finish."
  (%thread-join thread))

;;; Condition variables

(defun make-condition-variable (&key (name "anonymous"))
  "Create a new condition variable for use with CONDITION-WAIT and CONDITION-NOTIFY."
  (%make-condition-variable 'name name))

(defun condition-wait (condition-variable lock &key timeout)
  "Release LOCK, wait for notification on CONDITION-VARIABLE, then re-acquire LOCK.
   With :TIMEOUT (seconds), returns NIL if the timeout elapsed without notification."
  (if timeout
      (%condition-wait condition-variable lock 'timeout timeout)
      (%condition-wait condition-variable lock)))

(defun condition-notify (condition-variable)
  "Wake one thread waiting on CONDITION-VARIABLE."
  (%condition-notify condition-variable))

(defun condition-broadcast (condition-variable)
  "Wake all threads waiting on CONDITION-VARIABLE."
  (%condition-broadcast condition-variable))

;;; Counting semaphores

(defun make-semaphore (&key (name "anonymous") (count 0))
  "Create a counting semaphore with initial COUNT tokens."
  (%make-semaphore 'name name 'count count))

(defun signal-semaphore (semaphore &optional (n 1))
  "Release N tokens to SEMAPHORE (default 1)."
  (%signal-semaphore semaphore n))

(defun wait-on-semaphore (semaphore &key timeout)
  "Acquire one token from SEMAPHORE, blocking if none are available.
   With :TIMEOUT (seconds), returns NIL if the timeout elapsed."
  (if timeout
      (%wait-on-semaphore semaphore 'timeout timeout)
      (%wait-on-semaphore semaphore)))

;;; Atomic operations (CAS, atomic-incf, atomic-decf)
;;; Lock-based: correct for all setfable place forms, not truly lock-free.
;;; Defined in DOTCL package so the atomics portability library can delegate here.

(in-package :cl-user)

;; Use double-colon (internal access) throughout this definition section so that
;; the reader does not require COMPARE-AND-SWAP to be external at read time.
;; The load-time export below makes them single-colon accessible afterwards.

(defvar dotcl::%atomics-lock nil)

(defmacro dotcl::compare-and-swap (place old new)
  (let ((o (gensym "OLD")) (n (gensym "NEW")) (c (gensym "CUR")))
    (multiple-value-bind (temps vals stores writer reader)
        (get-setf-expansion place)
      `(let* (,@(mapcar #'list temps vals) (,o ,old) (,n ,new))
         (dotcl:acquire-lock dotcl::%atomics-lock)
         (unwind-protect
           (let ((,c ,reader))
             (if (eq ,c ,o)
               (let ((,(car stores) ,n))
                 ,writer
                 t)
               nil))
           (dotcl:release-lock dotcl::%atomics-lock))))))

(defmacro dotcl::atomic-incf (place &optional (delta 1))
  (let ((d (gensym "DELTA")))
    (multiple-value-bind (temps vals stores writer reader)
        (get-setf-expansion place)
      `(let* (,@(mapcar #'list temps vals) (,d ,delta))
         (dotcl:acquire-lock dotcl::%atomics-lock)
         (unwind-protect
           (let ((,(car stores) (+ ,reader ,d)))
             ,writer
             ,(car stores))
           (dotcl:release-lock dotcl::%atomics-lock))))))

(defmacro dotcl::atomic-decf (place &optional (delta 1))
  (let ((d (gensym "DELTA")))
    (multiple-value-bind (temps vals stores writer reader)
        (get-setf-expansion place)
      `(let* (,@(mapcar #'list temps vals) (,d ,delta))
         (dotcl:acquire-lock dotcl::%atomics-lock)
         (unwind-protect
           (let ((,(car stores) (- ,reader ,d)))
             ,writer
             ,(car stores))
           (dotcl:release-lock dotcl::%atomics-lock))))))

;; Export and initialize at load time
(dolist (sym '("COMPARE-AND-SWAP" "ATOMIC-INCF" "ATOMIC-DECF"))
  (export (intern sym :dotcl) :dotcl))
(setf dotcl::%atomics-lock (dotcl:make-lock "atomics"))

(in-package :dotcl-thread)

(provide "dotcl-thread")
