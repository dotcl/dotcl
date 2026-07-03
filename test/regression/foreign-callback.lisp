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
