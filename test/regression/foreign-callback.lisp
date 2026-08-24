;;; C#→Lisp callback boundary error handling.
;;; A Lisp error inside a callback invoked from .NET (a make-delegate delegate, an
;;; event handler, or a %define-class method override) must NOT tear through the
;;; host as TargetInvocationException. Runtime.InvokeForeignCallback catches the
;;; LispErrorException and routes the condition to dotcl:*foreign-callback-handler*
;;; (default: report + return NIL → marshaled to the return type's default).

;;; --- delegate boundary (CreateLispDelegate) ---

;; Success path is unaffected by the wrapping.
(deftest d273-delegate-success-unaffected
  (let ((fn (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                  (lambda (x) (* x 2)))))
    (dotnet:invoke fn "Invoke" 21))
  42)

;; Erroring Func<int,int>: default handler returns NIL -> int default 0, no crash.
(deftest d273-delegate-error-default-returns-default
  (let ((fn (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                  (lambda (x) (declare (ignore x)) (error "boom")))))
    (dotnet:invoke fn "Invoke" 5))
  0)

;; Erroring Action<int> (void): default handler swallows, no crash, control continues.
(deftest d273-delegate-error-void-no-crash
  (let ((ran nil))
    (let ((act (dotnet:make-delegate "System.Action`1[System.Int32]"
                                     (lambda (x) (declare (ignore x)) (error "boom")))))
      (dotnet:invoke act "Invoke" 7)
      (setf ran t))
    ran)
  t)

;; A bound *foreign-callback-handler* decides the callback's result and sees the
;; condition. Here it returns 99, which marshals back as the delegate's int result.
(deftest d273-handler-invoked-controls-result
  (let ((seen nil))
    (let ((dotcl:*foreign-callback-handler*
            (lambda (c) (setf seen c) 99)))
      (let* ((fn (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                       (lambda (x) (declare (ignore x)) (error "boom"))))
             (r (dotnet:invoke fn "Invoke" 5)))
        (list r (not (null seen))))))
  (99 t))

;; The handler receives the ORIGINAL error condition (an ERROR whose report still
;; reads "boom"), proving the handler-bind intercepts at the signal point rather
;; than after the error function would have entered the debugger.
(deftest d273-handler-receives-original-condition
  (let ((ok nil))
    (let ((dotcl:*foreign-callback-handler*
            (lambda (c) (setf ok (and (typep c 'error)
                                      (search "boom" (princ-to-string c)) t))
              0)))
      (let ((fn (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                      (lambda (x) (declare (ignore x)) (error "boom")))))
        (dotnet:invoke fn "Invoke" 1)))
    ok)
  t)

;;; --- %define-class method-override boundary (DispatchLispMethod) ---

;; An override whose body errors: default handler returns NIL -> String null ->
;; dotnet:invoke yields NIL, host not crashed.
(deftest d273-override-error-default-no-crash
  (progn
    (dotnet:%define-class "DotclTest.CallbackErrA" nil nil nil
      (list (list "Greet" "System.String" nil
                  (lambda (self) (declare (ignore self)) (error "boom")))))
    (let ((obj (dotnet:new "DotclTest.CallbackErrA")))
      (dotnet:invoke obj "Greet")))
  nil)

;; With a handler bound, the override's failure routes to it and its value is used.
(deftest d273-override-error-handler-result
  (progn
    (dotnet:%define-class "DotclTest.CallbackErrB" nil nil nil
      (list (list "Greet" "System.String" nil
                  (lambda (self) (declare (ignore self)) (error "boom")))))
    (let ((dotcl:*foreign-callback-handler* (lambda (c) (declare (ignore c)) "recovered")))
      (let ((obj (dotnet:new "DotclTest.CallbackErrB")))
        (dotnet:invoke obj "Greet"))))
  "recovered")

;;; --- native FFI callback (dotnet:make-ffi-callback) ---
;; Exposes a Lisp function as a native (C-callable) function pointer, e.g. for
;; CFFI's defcallback. Creation must return a non-zero integer pointer (built via
;; a per-signature Reflection.Emit delegate + a DynamicMethod thunk). The native
;; round-trip (C code calling back into Lisp, e.g. qsort with a Lisp comparator)
;; is OS-specific (needs a C runtime lib) and is exercised manually, not here.
(deftest make-ffi-callback-returns-pointer
  (let ((p (dotnet:make-ffi-callback (lambda (x) x) '(:int) :int)))
    (and (integerp p) (/= p 0)))
  t)

;;; A callback body that ends in a multiple-value form used to kill the call from
;;; C: the MvReturn has no native representation and reached the argument
;;; converter as-is, so every invocation raised TargetInvocationException. Only
;;; one value crosses the boundary, so the extras are dropped and the primary is
;;; returned -- what every other CL implementation's FFI does.
;;;
;;; cffi hits this for every :string-returning callback, because its conversion
;;; calls FOREIGN-STRING-ALLOC and that returns (values pointer size). Calling
;;; the pointer through %FFI-CALL-PTR is the same path C takes, and needs no C
;;; runtime library, so unlike the qsort-style round-trip it runs here.
(defun %fcb-call (fn arg-types ret-type &rest args)
  (apply #'dotnet:%ffi-call-ptr
         (dotnet:make-ffi-callback fn arg-types ret-type)
         arg-types ret-type args))

(deftest ffi-callback-multiple-values-takes-primary
  (list (%fcb-call (lambda () (values 1 2)) '() :int)
        (%fcb-call (lambda (x) (values (* x 2) :extra)) '(:int) :int 21)
        ;; the cffi :string shape
        (%fcb-call (lambda () (values 12345 99)) '() :pointer))
  (1 42 12345))

;;; (VALUES) has no primary; NIL is what the converter then sees.
(deftest ffi-callback-no-values-is-nil
  (%fcb-call (lambda () (values)) '() :int)
  0)

;;; A single value was never broken and must stay put, and a void callback
;;; ignores whatever the body produced.
(deftest ffi-callback-single-and-void-unchanged
  (list (%fcb-call (lambda () 7) '() :int)
        (%fcb-call (lambda (x) x) '(:int) :int 5)
        (%fcb-call (lambda () (values 1 2)) '() nil))
  (7 5 nil))

;;; --- dotcl:*foreign-callback-propagate* (opt-in) ---
;;; Containment keeps a .NET-driven loop alive, but a callback that Lisp itself
;;; triggered (a lambda handed to a .NET API) is far easier to debug when the error
;;; reaches the surrounding handler-case. Binding the variable to T asks for that.

(deftest fcb-propagate-reaches-caller
  (let ((dotcl:*foreign-callback-propagate* t))
    (handler-case
        (dotnet:invoke (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                             (lambda (x) (declare (ignore x)) (error "boom")))
                       "Invoke" 5)
      (error (e) (princ-to-string e))))
  "boom")

;;; ...including through several .NET frames (LINQ drives the callback here).
(deftest fcb-propagate-through-linq
  (let ((dotcl:*foreign-callback-propagate* t)
        (l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32")))))
    (dotnet:invoke l "Add" 1)
    (handler-case
        (dotnet:invoke (dotnet:invoke l "Select" (lambda (x) (error "boom ~a" x))) "ToList")
      (error (e) (princ-to-string e))))
  "boom 1")

;;; The switch is dynamic: outside the binding, containment is back.
(deftest fcb-propagate-is-dynamic
  (let ((fn (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                  (lambda (x) (declare (ignore x)) (error "boom")))))
    (list (let ((dotcl:*foreign-callback-propagate* t))
            (handler-case (dotnet:invoke fn "Invoke" 5) (error () :propagated)))
          (dotnet:invoke fn "Invoke" 5)))
  (:propagated 0))

;;; Successful callbacks are unaffected by the switch.
(deftest fcb-propagate-success-unaffected
  (let ((dotcl:*foreign-callback-propagate* t))
    (dotnet:invoke (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                         (lambda (x) (* x 2)))
                   "Invoke" 21))
  42)

;;; Propagation wins over *foreign-callback-handler*: the error reaches the caller
;;; instead of the handler deciding a result.
(deftest fcb-propagate-beats-handler
  (let ((called nil))
    (let ((dotcl:*foreign-callback-propagate* t)
          (dotcl:*foreign-callback-handler* (lambda (c) (declare (ignore c)) (setf called t) 99)))
      (list (handler-case
                (dotnet:invoke (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                                     (lambda (x) (declare (ignore x)) (error "boom")))
                               "Invoke" 5)
              (error () :propagated))
            called)))
  (:propagated nil))
