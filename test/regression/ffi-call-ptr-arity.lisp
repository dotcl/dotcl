;;; DOTNET:%FFI-CALL-PTR takes func-ptr, arg-types and ret-type, then the
;;; native arguments. Its check demanded two arguments and then read the third,
;;; so a two-argument call ended in an array index error instead of naming the
;;; missing ret-type. Every caller passes all three, even with no native args.

(defun %ffi-call-ptr-arity (args)
  (handler-case (progn (apply #'dotnet:%ffi-call-ptr args) :no-error)
    (program-error (c) (princ-to-string c))
    (error (c) (list :other (type-of c) (princ-to-string c)))))

(deftest ffi-call-ptr.too-few-arguments-signal-program-error
  ;; The argument-count check at the function's entry may report the arity
  ;; before the function's own check runs; either way it is a program-error.
  (mapcar (lambda (args) (stringp (%ffi-call-ptr-arity args)))
          (list '() '(0) '(0 ())))
  (t t t))

#+dotcl-emit
(deftest-emitting-only ffi-call-ptr.three-arguments-call-with-no-native-args
  (dotnet:%ffi-call-ptr
   (dotnet:make-ffi-callback (lambda () 42) '() :int)
   '() :int)
  42)
