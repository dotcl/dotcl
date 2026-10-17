;;; Deeply nested but legitimate source must compile; a macro that expands
;;; forever must still be reported as an error, and running out of stack
;;; while compiling must be an error too, never a dead process.
;;;
;;; COMPILE-FORM used to stop at a fixed nesting of 500 forms, meant to catch
;;; runaway macro expansion. Generated code that emits one statement per
;;; element of a literal sequence nests one level per element: Coalton's
;;; SEQ:MAKE of 2048 literals expands into a 2048-deep chain of
;;; VECTOR-PUSH-EXTEND forms, which SBCL compiles and dotcl refused.

(defun %deep-nest-progn (n)
  "(progn (vector-push-extend 1 v) (progn (vector-push-extend 2 v) ... v))"
  (let ((inner 'v))
    (loop for i from n downto 1
          do (setf inner `(progn (vector-push-extend ,i v) ,inner)))
    inner))

(defun %deep-nest-coalton-seq (n)
  "The chain Coalton generates for SEQ:MAKE (through VECTOR:MAKE) of N literals:
   a PROGN per element, each statement a LET of the item around a typed call."
  (let ((inner 'v))
    (loop for i from n downto 1
          do (setf inner
                   `(progn
                      (let ((item ,i))
                        (declare (ignorable item))
                        (locally
                          (declare)
                          (the (values fixnum &optional)
                               (let ((item item) (vec v))
                                 (values (vector-push-extend item vec))))))
                      ,inner)))
    inner))

(defun %deep-nest-form (body)
  `(lambda ()
     (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
       ,body)))

(defun %deep-nest-summary (v)
  (list (length v) (aref v 0) (aref v (1- (length v)))))

(deftest-runtime-once compile-deep-progn-2048
  (%deep-nest-summary
   (funcall (compile nil (%deep-nest-form (%deep-nest-progn 2048)))))
  (2048 1 2048))

(deftest-runtime-once compile-deep-coalton-seq-make-2048
  (%deep-nest-summary
   (funcall (compile nil (%deep-nest-form (%deep-nest-coalton-seq 2048)))))
  (2048 1 2048))

;;; Through EVAL, so the emit-free build (whose COMPILE is unavailable) walks
;;; the same 2048-deep form with its interpreter.
(deftest eval-deep-progn-2048
  (%deep-nest-summary
   (funcall (eval (%deep-nest-form (%deep-nest-progn 2048)))))
  (2048 1 2048))

;;; Runaway expansion is still caught, as an ordinary error the caller can
;;; handle, and the process goes on.
(defmacro %deep-runaway-same () '(%deep-runaway-same))
(defmacro %deep-runaway-growing (x) `(progn (%deep-runaway-growing ,x)))

(defun %deep-compile-outcome (form)
  (handler-case (progn (compile nil `(lambda () ,form)) :compiled)
    (error (e)
      (if (search "Compile depth limit exceeded" (princ-to-string e))
          :depth-limit
          (list :other-error (princ-to-string e))))))

;;; The default limit takes a couple of seconds to reach, so it is exercised
;;; once, on the growing form; the same-form loop runs under a smaller limit.
(deftest-runtime-once compile-runaway-macro-same-form
  (let ((dotcl-internal::*compile-depth-limit* 1000))
    (%deep-compile-outcome '(%deep-runaway-same)))
  :depth-limit)

(deftest-runtime-once compile-runaway-macro-growing
  (%deep-compile-outcome '(%deep-runaway-growing 1))
  :depth-limit)

;;; With the nesting limit out of the way, a thread whose stack is too small
;;; for the form gets an error from the compiler, not a stack overflow that
;;; takes the process down. 16MB holds roughly 5000 levels of this PROGN chain.
(defun %deep-compile-on-small-stack (n)
  (let* ((result nil)
         (form (%deep-nest-form (%deep-nest-progn n)))
         (th (dotnet:new "System.Threading.Thread"
                         (dotnet:make-delegate
                          "System.Threading.ThreadStart"
                          (lambda ()
                            (setf result
                                  (let ((dotcl-internal::*compile-depth-limit*
                                          most-positive-fixnum))
                                    (handler-case
                                        (progn (compile nil form) :compiled)
                                      (serious-condition () :error))))))
                         (* 16 1024 1024))))
    (dotnet:invoke th "Start")
    (dotnet:invoke th "Join")
    result))

(deftest-runtime-once compile-stack-exhausted-is-an-error
  (%deep-compile-on-small-stack 20000)
  :error)
