;;; A variable that a closure captures and assigns, declared FIXNUM, lives in a cell that holds a raw
;;; long. The cases here pin the behavior the old LispObject[1] cell had, and the checks it lacked.

(require "dotcl-thread")

(defun %tcf-counter (n)
  (declare (fixnum n))
  (let ((col 0))
    (declare (fixnum col))
    (labels ((bump (k) (incf col k)))
      (dotimes (i n) (bump i)))
    col))

(defun %tcf-two-closures ()
  (let ((x 10))
    (declare (fixnum x))
    (let ((inc (lambda () (incf x)))
          (get (lambda () x))
          (dbl (lambda () (setq x (* x 2)))))
      (funcall inc) (funcall dbl) (funcall inc)
      (list (funcall get) x))))

(defun %tcf-closures-in-loop ()
  (let ((fns '()))
    (dotimes (i 3)
      (let ((v (* i 10)))
        (declare (fixnum v))
        (push (lambda () (incf v)) fns)))
    (mapcar #'funcall (nreverse fns))))

(defun %tcf-loop-shared ()
  (let ((total 0))
    (declare (fixnum total))
    (let ((add (lambda (k) (incf total k))))
      (dotimes (i 5) (funcall add i)))
    total))

(defun %tcf-unwind-protect ()
  (let ((n 0))
    (declare (fixnum n))
    (let ((bump (lambda () (incf n))))
      (catch 'out
        (unwind-protect (progn (funcall bump) (throw 'out nil))
          (funcall bump) (funcall bump))))
    n))

(defun %tcf-handler ()
  (let ((n 0))
    (declare (fixnum n))
    (let ((bump (lambda () (incf n 5))))
      (handler-case (progn (funcall bump) (error "x") (funcall bump))
        (error () (funcall bump))))
    n))

(defun %tcf-bad-setq ()
  (let ((x 1))
    (declare (fixnum x))
    (let ((set (lambda (v) (setq x v))))
      (list (funcall set 7)
            (handler-case (funcall set 2.5d0) (type-error () :type-error))
            (handler-case (funcall set (expt 2 70)) (type-error () :type-error))
            x))))

(defun %tcf-bad-setq-safe ()
  (declare (optimize (safety 3)))
  (let ((x 1))
    (declare (fixnum x))
    (let ((set (lambda (v) (setq x v))))
      (list (handler-case (funcall set "no") (type-error () :type-error)) x))))

(defun %tcf-overflow ()
  (let ((x most-positive-fixnum))
    (declare (fixnum x))
    (let ((inc (lambda () (incf x))))
      (handler-case (funcall inc) (error () :error)))))

(defun %tcf-nested-closure ()
  (let ((c 0))
    (declare (fixnum c))
    (let ((outer (lambda ()
                   (let ((inner (lambda () (incf c 100))))
                     (funcall inner) (incf c)))))
      (funcall outer) (funcall outer)
      c)))

(defun %tcf-threads ()
  ;; Two writers store 0 and -1 (all bits clear / all bits set) into one cell while the main
  ;; thread reads it: a torn write would show a value that is neither.
  (let ((n 0) (stop nil) (bad 0))
    (declare (fixnum n bad))
    (let* ((w0 (lambda () (loop until stop do (setq n 0))))
           (w1 (lambda () (loop until stop do (setq n -1))))
           (t0 (dotcl-thread:make-thread w0))
           (t1 (dotcl-thread:make-thread w1)))
      (dotimes (i 200000)
        (let ((v n)) (unless (or (eql v 0) (eql v -1)) (incf bad))))
      (setq stop t)
      (dotcl-thread:thread-join t0)
      (dotcl-thread:thread-join t1))
    bad))

(defun %tcf-threads-count ()
  ;; Threads that each add to their own cell, summed.
  (let ((total 0))
    (declare (fixnum total))
    (let* ((lock (dotcl-thread:make-lock))
           (mk (lambda ()
                 (dotcl-thread:make-thread
                  (lambda () (dotimes (i 1000)
                               (dotcl-thread:with-lock-held (lock) (incf total)))))))
           (ths (loop repeat 4 collect (funcall mk))))
      (mapc #'dotcl-thread:thread-join ths))
    total))

(deftest typed-cell-fixnum.counter
  (list (%tcf-counter 100) (%tcf-counter 0))
  (4950 0))

(deftest typed-cell-fixnum.two-closures-share
  (%tcf-two-closures)
  (23 23))

(deftest typed-cell-fixnum.closure-per-iteration
  (%tcf-closures-in-loop)
  (1 11 21))

(deftest typed-cell-fixnum.shared-total
  (%tcf-loop-shared)
  10)

(deftest typed-cell-fixnum.unwind-protect
  (%tcf-unwind-protect)
  3)

(deftest typed-cell-fixnum.handler
  (%tcf-handler)
  10)

(deftest-compiled-only typed-cell-fixnum.type-violating-setq
  (%tcf-bad-setq)
  (7 :type-error :type-error 7))

(deftest-compiled-only typed-cell-fixnum.type-violating-setq-safety-3
  (%tcf-bad-setq-safe)
  (:type-error 1))

(deftest-compiled-only typed-cell-fixnum.overflow-leaves-fixnum-range
  (%tcf-overflow)
  :error)

(deftest typed-cell-fixnum.nested-closure
  (%tcf-nested-closure)
  202)

(deftest-compiled-only typed-cell-fixnum.threads-do-not-tear
  (%tcf-threads)
  0)

(deftest-compiled-only typed-cell-fixnum.threads-locked-count
  (%tcf-threads-count)
  4000)
