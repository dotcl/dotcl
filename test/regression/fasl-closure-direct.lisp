;;; A closure defined in a .fasl gets the same per-arity direct entry point as one
;;; compiled in-process.
;;;
;;; The fasl emitter used to give every closure body the args-array signature
;;; (object[] env, LispObject[] args), and _funcN can only be installed when the
;;; body's signature carries the arguments. So every call to a closure that came
;;; from a compiled library allocated a LispObject[] — 32 bytes a call — and went
;;; through the slow args-array path, while the same source compiled in the
;;; session did not. Quickloaded libraries are all fasls, so this was the common
;;; case, not the rare one.
;;;
;;; What must not change is the argc contract: a direct body omits the compiler's
;;; arity-check prefix because the delegate signature makes argc structural, and
;;; the args-array wrapper runs the same check. A wrong argument count still has
;;; to be a PROGRAM-ERROR, not an index-out-of-range.

(defvar *fcd-dir*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-fcd-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fcd-compile-and-load (source name)
  (let ((lisp (concatenate 'string *fcd-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-compiled-only fasl-closure-direct.values-and-arity
  (progn
    (%fcd-compile-and-load
     "(in-package :cl-user)
      (defun %fcd-thunk (v) (lambda () v))
      (defun %fcd-adder (n) (lambda (x) (+ x n)))
      (defun %fcd-pair (a) (lambda (x y) (list a x y)))
      (defun %fcd-six (a) (lambda (p q r s u v) (list a p q r s u v)))
      ;; Seven parameters: past the direct-entry arity, so this one stays on the
      ;; args-array shape and must keep working the same way.
      (defun %fcd-seven (a) (lambda (p q r s u v w) (list a p q r s u v w)))"
     "fcd-values")
    (list (funcall (funcall (intern "%FCD-THUNK") :v))
          (funcall (funcall (intern "%FCD-ADDER") 10) 5)
          (funcall (funcall (intern "%FCD-PAIR") :a) 1 2)
          (funcall (funcall (intern "%FCD-SIX") :a) 1 2 3 4 5 6)
          (funcall (funcall (intern "%FCD-SEVEN") :a) 1 2 3 4 5 6 7)))
  (:v 15 (:a 1 2) (:a 1 2 3 4 5 6) (:a 1 2 3 4 5 6 7)))

;;; Too many and too few arguments both signal PROGRAM-ERROR, at every arity
;;; including the args-array fallback.

(deftest-compiled-only fasl-closure-direct.wrong-argc-signals
  (flet ((outcome (thunk) (handler-case (progn (funcall thunk) :no-error)
                            (program-error () :program-error)
                            (error (e) (type-of e)))))
    (let ((add (funcall (intern "%FCD-ADDER") 1))
          (seven (funcall (intern "%FCD-SEVEN") :a)))
      (list (outcome (lambda () (funcall add)))
            (outcome (lambda () (funcall add 1 2)))
            (outcome (lambda () (funcall seven 1))))))
  (:program-error :program-error :program-error))

;;; A closure over a variable the enclosing function keeps mutating reads the
;;; current value, not a copy: the direct body still receives the same boxed
;;; environment.

(deftest-compiled-only fasl-closure-direct.mutable-capture
  (progn
    (%fcd-compile-and-load
     "(in-package :cl-user)
      (defun %fcd-counter ()
        (let ((n 0))
          (list (lambda () (incf n)) (lambda () n))))"
     "fcd-capture")
    (let* ((pair (funcall (intern "%FCD-COUNTER")))
           (bump (first pair))
           (peek (second pair)))
      (funcall bump) (funcall bump)
      (list (funcall peek) (funcall bump) (funcall peek))))
  (2 3 3))

;;; The point of the change: calling a fasl closure no longer goes through the
;;; args-array slow path. InvokeSlow counts it when it does, so an empty bucket
;;; for the closure is the assertion.

(deftest-compiled-only fasl-closure-direct.no-invoke-slow
  (let ((add (funcall (intern "%FCD-ADDER") 1))
        (was (dotcl:collect-invoke-stats t)))
    (unwind-protect
         (progn (dotcl:reset-invoke-slow-stats)
                (dotimes (i 1000) (funcall add i))
                ;; Buckets are ((name . argc) . count); a closure body shows up
                ;; under an <anon:closure...> name.
                (count-if (lambda (entry)
                            (let ((name (car (car entry))))
                              (and (stringp name) (search "closure" name))))
                          (dotcl:invoke-slow-stats)))
      (dotcl:reset-invoke-slow-stats)
      (dotcl:collect-invoke-stats was)))
  0)
