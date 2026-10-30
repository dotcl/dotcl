;;; A typed function whose body is a leaf is called through its boxed entry
;;; (FUNCALL, APPLY, a call that does not know its type) without linking a
;;; frame while its arguments have the declared types. An argument of another
;;; type, or a signal in the body, still shows the function with its arguments
;;; on top of the backtrace, and a tail call that hands it a frame leaves the
;;; frame list as it was.

(declaim (notinline %tlsm-add %tlsm-r %tlsm-err %tlsm-cmp))
(declaim (ftype (function (double-float double-float) double-float) %tlsm-add))
(defun %tlsm-add (a b) (+ a b))
(declaim (ftype (function ((integer 0 10) fixnum) fixnum) %tlsm-r))
(defun %tlsm-r (a b) (+ a b))
(defun %tlsm-err (x) (error "tlsm ~A" x))
(declaim (ftype (function (double-float double-float) t) %tlsm-cmp))
(defun %tlsm-cmp (a b) (if (< a b) :lt :ge))
(defun %tlsm-t0 (f g h x) (funcall f g h x))
(defun %tlsm-t2 (h x) (funcall h x 1d0))

(defvar *tls-loaded*
  (when (compiled-mode-p)
    (let ((fasl (regression-temp-file "typed-leaf-shell.fasl")))
      (compile-file "test/regression/typed-leaf-shell/defs.lisp" :output-file fasl)
      (load fasl)
      t)))

(defun %tls-top-frame (thunk)
  (block out
    (handler-bind ((error (lambda (c)
                            (return-from out
                              (let ((s (with-output-to-string (o) (dotcl:print-backtrace o))))
                                (list (type-of c)
                                      (string-trim " " (subseq s (1+ (position #\: s))
                                                               (position #\Newline s)))))))))
      (funcall thunk))))

(defun %tls-leaf-p (f n) (dotnet:static "DotCL.LispFunction" "LeafEntryP" f n))
(defun %tls-depth () (length (dotcl:backtrace)))
(defun %tls-outer () (list (%tls-depth)))

(deftest typed-leaf-shell.values
  (list (funcall #'%tlsm-add 1d0 2d0) (apply #'%tlsm-add '(0.5d0 0.25d0))
        (funcall '%tlsm-r 3 4) (funcall #'%tlsm-r 0 -5)
        (%tlsm-t0 (lambda (g h x) (funcall g h x)) #'%tlsm-t2 #'%tlsm-add 2d0)
        (funcall #'%tlsm-cmp 1d0 2d0) (funcall #'%tlsm-cmp 2d0 1d0))
  (3d0 0.75d0 7 -5 3d0 :lt :ge))

(deftest-emitting-only typed-leaf-shell.entries
  (list (%tls-leaf-p #'%tlsm-add 2) (%tls-leaf-p #'%tlsm-r 2) (%tls-leaf-p #'%tlsm-cmp 2)
        (%tls-leaf-p #'%tlsm-err 1))
  (t t t nil))

(deftest-emitting-only typed-leaf-shell.backtrace
  (let ((base (%tls-depth)))
    (list (%tls-top-frame (lambda () (funcall #'%tlsm-add 1 2d0)))
          (%tls-top-frame (lambda () (funcall #'%tlsm-r 11 1)))
          (%tls-top-frame (lambda () (funcall #'%tlsm-r 10 most-positive-fixnum)))
          (%tls-top-frame (lambda () (funcall #'%tlsm-cmp 1d0 :x)))
          (%tlsm-t0 (lambda (g h x) (funcall g h x)) #'%tlsm-t2 #'%tlsm-add 2d0)
          (- (car (%tls-outer)) base)
          (%tlsm-t0 (lambda (g h x) (declare (ignore g)) (funcall h x 1d0)) nil #'%tlsm-add 2d0)
          (- (car (%tls-outer)) base)
          (%tls-top-frame (lambda () (%tlsm-err 5)))
          (- (%tls-depth) base)))
  ((type-error "(%TLSM-ADD 1 2.0d0)")
   (type-error "(%TLSM-R 11 1)")
   (type-error "(%TLSM-R 10 9223372036854775807)")
   (type-error "(%TLSM-CMP 1.0d0 :X)")
   3d0
   1
   3d0
   1
   (simple-error "(%TLSM-ERR 5)")
   0))

(deftest-compiled-only typed-leaf-shell.fasl
  (let ((base (%tls-depth)))
    (list (funcall #'cl-user::%tlsf-add 1d0 2d0) (funcall 'cl-user::%tlsf-r 3 4)
          (list (%tls-leaf-p #'cl-user::%tlsf-add 2) (%tls-leaf-p #'cl-user::%tlsf-cmp 2))
          (funcall #'cl-user::%tlsf-cmp 1d0 2d0) (funcall #'cl-user::%tlsf-cmp 2d0 1d0)
          (%tls-top-frame (lambda () (funcall #'cl-user::%tlsf-cmp :x 1d0)))
          (%tls-top-frame (lambda () (funcall #'cl-user::%tlsf-add 1 2d0)))
          (%tls-top-frame (lambda () (funcall #'cl-user::%tlsf-r 11 1)))
          (%tls-top-frame (lambda () (funcall #'cl-user::%tlsf-r 10 most-positive-fixnum)))
          (cl-user::%tlsf-t0 (lambda (g h x) (funcall g h x)) #'cl-user::%tlsf-t2 #'cl-user::%tlsf-add 2d0)
          (- (car (%tls-outer)) base)
          (cl-user::%tlsf-t0 (lambda (g h x) (declare (ignore g)) (funcall h x 1d0)) nil #'cl-user::%tlsf-add 2d0)
          (- (car (%tls-outer)) base)
          (%tls-top-frame (lambda () (cl-user::%tlsf-err 5)))
          (- (%tls-depth) base)))
  (3d0 7 (t t) :lt :ge
   (type-error "(%TLSF-CMP :X 1.0d0)")
   (type-error "(%TLSF-ADD 1 2.0d0)")
   (type-error "(%TLSF-R 11 1)")
   (type-error "(%TLSF-R 10 9223372036854775807)")
   3d0
   1
   3d0
   1
   (simple-error "(%TLSF-ERR 5)")
   0))
