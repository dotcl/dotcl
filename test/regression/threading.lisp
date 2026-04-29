;;; D656 Phase A — multi-thread safety regression tests.
;;; Verifies that concurrent INTERN, MAKE-INSTANCE, SETF DOCUMENTATION,
;;; and GENTEMP do not corrupt their shared dictionaries / counters.
;;; Prior to D656 these would crash with Dictionary enumeration/modification
;;; races. The test is deterministic: each helper runs 4 threads x 500-1000
;;; iterations and simply verifies completion without exception.

(require "dotcl-thread")

(deftest d656-concurrent-intern
  (let* ((n 200)
         (threads
           (loop for i from 0 below 4
                 collect (dotcl-thread:make-thread
                          (lambda ()
                            (loop for j from 0 below n
                                  do (intern (format nil "TH-~D-S-~D" i j) :cl-user)))
                          :name (format nil "i-~D" i)))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    t)
  t)

(defclass %d656-cls () ((slot :initarg :s :accessor %d656-slot)))

(deftest d656-concurrent-make-instance
  (let* ((n 200)
         (threads
           (loop for i from 0 below 4
                 collect (dotcl-thread:make-thread
                          (lambda ()
                            (loop for j from 0 below n
                                  sum (%d656-slot (make-instance '%d656-cls :s j))))
                          :name (format nil "c-~D" i)))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    t)
  t)

(defun %d656-doc-fn () "initial")

(deftest d656-concurrent-setf-documentation
  (let* ((n 200)
         (threads
           (loop for i from 0 below 4
                 for ci = i
                 collect (dotcl-thread:make-thread
                          (lambda ()
                            (loop for j from 0 below n
                                  do (setf (documentation '%d656-doc-fn 'function)
                                           (format nil "d-~D-~D" ci j))))
                          :name (format nil "d-~D" i)))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    t)
  t)

(deftest d656-concurrent-gentemp
  (let* ((n 500)
         (threads
           (loop for i from 0 below 4
                 collect (dotcl-thread:make-thread
                          (lambda ()
                            (loop for j from 0 below n
                                  collect (gentemp "T")))
                          :name (format nil "g-~D" i)))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    t)
  t)

;;; D657 Phase D — bordeaux-threads API補完: condition-var, semaphore,
;;; recursive-lock, thread-yield.

(deftest d657-recursive-lock-reentry
  (let ((rl (dotcl-thread:make-recursive-lock "rl")))
    (dotcl-thread:with-recursive-lock-held (rl)
      (dotcl-thread:with-recursive-lock-held (rl)
        :ok)))
  :ok)

(deftest d657-semaphore-count
  ;; Initial count 2, two acquires should succeed non-blockingly.
  (let ((sem (dotcl-thread:make-semaphore :count 2)))
    (list (dotcl-thread:wait-on-semaphore sem :timeout 0.1)
          (dotcl-thread:wait-on-semaphore sem :timeout 0.1)
          ;; Third should timeout.
          (dotcl-thread:wait-on-semaphore sem :timeout 0.05)))
  (t t nil))

(deftest d657-semaphore-producer-consumer
  (let ((sem (dotcl-thread:make-semaphore :count 0))
        (acc 0))
    (dotcl-thread:make-thread (lambda () (dotcl-thread:wait-on-semaphore sem) (incf acc 1)))
    (dotcl-thread:make-thread (lambda () (dotcl-thread:wait-on-semaphore sem) (incf acc 10)))
    (sleep 0.05)
    (dotcl-thread:signal-semaphore sem 2)
    (sleep 0.1)
    acc)
  11)

(deftest d657-condition-wait-timeout
  (let ((lock (dotcl-thread:make-lock))
        (cv   (dotcl-thread:make-condition-variable)))
    (dotcl-thread:with-lock-held (lock)
      ;; No notifier — should time out and return NIL.
      (dotcl-thread:condition-wait cv lock :timeout 0.05)))
  nil)

(deftest d657-condition-notify-wakes
  (let ((lock (dotcl-thread:make-lock))
        (cv   (dotcl-thread:make-condition-variable))
        (ready nil)
        (done  nil))
    (dotcl-thread:make-thread
     (lambda ()
       (dotcl-thread:with-lock-held (lock)
         (loop while (not ready) do (dotcl-thread:condition-wait cv lock))
         (setq done t))))
    (sleep 0.05)
    (dotcl-thread:with-lock-held (lock)
      (setq ready t)
      (dotcl-thread:condition-notify cv))
    (sleep 0.1)
    done)
  t)

(deftest d657-condition-broadcast-wakes-all
  (let ((lock (dotcl-thread:make-lock))
        (cv   (dotcl-thread:make-condition-variable))
        (ready nil)
        (count 0))
    (dotimes (i 3)
      (dotcl-thread:make-thread
       (lambda ()
         (dotcl-thread:with-lock-held (lock)
           (loop while (not ready) do (dotcl-thread:condition-wait cv lock))
           (incf count)))))
    (sleep 0.05)
    (dotcl-thread:with-lock-held (lock)
      (setq ready t)
      (dotcl-thread:condition-broadcast cv))
    (sleep 0.2)
    count)
  3)

(deftest d657-thread-yield
  (progn (dotcl-thread:thread-yield) :ok)
  :ok)

;;; D658 Phase C — synchronized hash-tables.

(deftest d658-synchronized-ht-basic
  (let ((ht (make-hash-table :synchronized t :test 'equal)))
    (setf (gethash "a" ht) 1)
    (setf (gethash "b" ht) 2)
    (+ (gethash "a" ht) (gethash "b" ht)))
  3)

(deftest d658-synchronized-ht-concurrent-write
  (let* ((ht (make-hash-table :synchronized t))
         (n 200)
         (threads (loop for i from 0 below 4
                        for ci = i
                        collect (dotcl-thread:make-thread
                                 (lambda ()
                                   (dotimes (j n)
                                     (setf (gethash (list ci j) ht) t)))
                                 :name (format nil "ht-~D" i)))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    (hash-table-count ht))
  800)

;;; D659 Phase B — per-thread dynamic binding isolation.
;;; Before D659: (let ((*x* ...)) ...) wrote to Symbol.Value which was shared
;;; across threads, so concurrent bindings of the same special corrupted each
;;; other (and Push/Pop of the linked-list stack could crash).

(defvar %d659-shared :initial)

(deftest d659-dynamic-binding-per-thread
  (let ((t1-saw nil) (t2-saw nil))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread
      (lambda ()
        (let ((%d659-shared :thread1))
          (sleep 0.05)
          (setq t1-saw %d659-shared)))))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread
      (lambda ()
        (let ((%d659-shared :thread2))
          (sleep 0.05)
          (setq t2-saw %d659-shared)))))
    (list t1-saw t2-saw %d659-shared))
  (:thread1 :thread2 :initial))

(defun %d659-worker (base n lock errors-box)
  (dotimes (j n)
    (let ((expected (+ base j)))
      (let ((%d659-shared expected))
        (dotcl-thread:thread-yield)
        (unless (= %d659-shared expected)
          (dotcl-thread:with-lock-held (lock)
            (incf (car errors-box))))))))

(deftest d659-dynbind-concurrent-stress
  ;; 4 threads x 100 iterations: each binds *shared* to a unique value
  ;; and reads it back. Zero mismatches → per-thread isolation works.
  (let* ((n 100)
         (lock (dotcl-thread:make-lock))
         (errors (list 0))
         (threads
          (loop for i from 0 below 4
                collect
                (let ((base (* i 1000)))
                  (dotcl-thread:make-thread
                   (lambda () (%d659-worker base n lock errors)))))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    (car errors))
  0)

;;; D660 soak — multi-pattern concurrent stress tests on dotcl-thread.

;;; Dining philosophers: N=5, ordered-acquire avoids deadlock.
(defun %d660-phil-body (ci n forks eat-counts rounds)
  (let* ((left (nth ci forks))
         (right (nth (mod (1+ ci) n) forks))
         (lo-first (< ci (mod (1+ ci) n)))
         (lo (if lo-first left right))
         (hi (if lo-first right left)))
    (dotimes (k rounds)
      (dotcl-thread:with-lock-held (lo)
        (dotcl-thread:with-lock-held (hi)
          (incf (aref eat-counts ci)))))))

(deftest d660-dining-philosophers
  (let* ((n 5)
         (rounds 50)
         (forks (loop for i from 0 below n
                      collect (dotcl-thread:make-lock (format nil "f-~D" i))))
         (eat-counts (make-array n :initial-element 0))
         (threads
          (loop for i from 0 below n
                collect (let ((ci i))
                          (dotcl-thread:make-thread
                           (lambda () (%d660-phil-body ci n forks eat-counts rounds))
                           :name (format nil "phil-~D" ci))))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    (reduce #'+ eat-counts))
  250)  ; 5 philosophers × 50 rounds

;;; Producer-consumer with condition-variable, lock-protected increment.
(defparameter *%d660-queue* nil)
(defparameter *%d660-lock* nil)
(defparameter *%d660-cv* nil)
(defparameter *%d660-done* nil)
(defparameter *%d660-sum* 0)

(defun %d660-producer (base n)
  (dotimes (j n)
    (dotcl-thread:with-lock-held (*%d660-lock*)
      (push (+ base j) *%d660-queue*)
      (dotcl-thread:condition-notify *%d660-cv*))))

(defun %d660-consumer ()
  (loop
    (let (got)
      (dotcl-thread:with-lock-held (*%d660-lock*)
        (loop while (and (null *%d660-queue*) (not *%d660-done*))
              do (dotcl-thread:condition-wait *%d660-cv* *%d660-lock*))
        (when *%d660-queue*
          (setq got (pop *%d660-queue*))
          (incf *%d660-sum* got)))
      (unless got (return)))))

(deftest d660-producer-consumer-cv
  (progn
    (setq *%d660-queue* nil
          *%d660-lock*  (dotcl-thread:make-lock "pc")
          *%d660-cv*    (dotcl-thread:make-condition-variable)
          *%d660-done*  nil
          *%d660-sum*   0)
    (let* ((per 200)
           (ps (loop for i from 0 below 2
                     collect (let ((base (* i 10000)))
                               (dotcl-thread:make-thread
                                (lambda () (%d660-producer base per))
                                :name (format nil "p-~D" i)))))
           (cs (loop for i from 0 below 3
                     collect (dotcl-thread:make-thread
                              #'%d660-consumer
                              :name (format nil "c-~D" i)))))
      (dolist (th ps) (dotcl-thread:thread-join th))
      (dotcl-thread:with-lock-held (*%d660-lock*)
        (setq *%d660-done* t)
        (dotcl-thread:condition-broadcast *%d660-cv*))
      (dolist (th cs) (dotcl-thread:thread-join th))
      *%d660-sum*))
  ;; Expected = sum of [0..199] + sum of [10000..10199]
  ;;         = 19900      + 2019900 = 2039800
  2039800)
