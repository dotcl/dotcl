;;; Parallel eval (dotcl:set-parallel-eval) stress test.
;;;
;;; With the process-wide _evalLock dropped, many threads compile/define
;;; concurrently. This hammers the paths that used to need serialization:
;;; concurrent DEFMETHOD on a *shared* generic function (CLOS method-list
;;; copy-on-write), same-run DEFUN/DEFMACRO registration, macro-table writes,
;;; and gen-local/gen-label counters. The test is deterministic: it passes iff
;;; every iteration completed with no error AND the success count equals the
;;; expected total. Parallel mode is enabled only for the duration and restored
;;; afterwards so the rest of the suite stays serial.

(require "dotcl-thread")

(defgeneric %pe-gf (x))

(deftest parallel-eval-concurrent-compile
  (let ((errs nil)
        (errlock (dotcl:make-lock))
        (ok (dotcl:make-atomic-long 0))
        (nthreads 4)
        (rounds 120))
    (dotcl:set-parallel-eval t)
    (unwind-protect
         (let ((threads
                 (loop for i from 0 below nthreads
                       collect
                       (dotcl-thread:make-thread
                        (lambda ()
                          (loop for j from 0 below rounds do
                            (handler-case
                                (let ((tag (format nil "~D-~D" i j)))
                                  ;; arithmetic compile
                                  (eval `(+ ,i ,j (* ,i ,j)))
                                  ;; defun + immediate call (unique name)
                                  (let ((fn (intern (format nil "%PE-F-~A" tag) :cl-user)))
                                    (eval `(defun ,fn (a) (* a a)))
                                    (eval `(,fn ,j)))
                                  ;; concurrent defmethod on the SHARED gf
                                  (eval `(defmethod %pe-gf ((x (eql ,(intern tag :keyword)))) ,j))
                                  ;; defmacro + immediate use (unique name)
                                  (let ((mac (intern (format nil "%PE-M-~A" tag) :cl-user)))
                                    (eval `(defmacro ,mac () ,j))
                                    (eval `(,mac)))
                                  (dotcl:atomic-long-incf ok))
                              (error (e)
                                (dotcl:acquire-lock errlock)
                                (push (format nil "~A" e) errs)
                                (dotcl:release-lock errlock)))))
                        :name (format nil "pe-~D" i)))))
           (dolist (th threads) (dotcl-thread:thread-join th)))
      (dotcl:set-parallel-eval nil))
    (when errs
      (format t "~&parallel-eval errors (~D): ~{~%  ~A~}~%" (length errs) errs))
    (and (null errs)
         (= (dotcl:atomic-long-value ok) (* nthreads rounds))))
  t)
