;;; A Lisp non-local exit started inside a callback (a lambda handed to a .NET
;;; API) is passing THROUGH the reflection call on its way to its own target — it
;;; is not a failure of the .NET method. It used to be caught as a
;;; TargetInvocationException and rewritten into an error, so
;;;   (block b (dotnet:invoke fn "Invoke" 7))   ; lambda does (return-from b ...)
;;; reported "DOTNET:INVOKE Func`2.Invoke: block return" instead of unwinding.

(defun dncb-func (fn)
  (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]" fn))

(defun dncb-ints (&rest xs)
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32")))))
    (dolist (x xs l) (dotnet:invoke l "Add" x))))

(deftest dncb-return-from-through-callback
  (block b
    (dotnet:invoke (dncb-func (lambda (x) (return-from b (list :escaped x)))) "Invoke" 7)
    :fell-through)
  (:escaped 7))

(deftest dncb-throw-through-callback
  (catch 'dncb-tag
    (dotnet:invoke (dncb-func (lambda (x) (throw 'dncb-tag (list :thrown x)))) "Invoke" 9)
    :fell-through)
  (:thrown 9))

;;; ...and through a callback the .NET side drives several frames deep (LINQ).
(deftest dncb-return-from-through-linq
  (block b
    (dotnet:invoke (dotnet:invoke (dncb-ints 1 2 3) "Select"
                                  (lambda (x) (return-from b (list :escaped x))))
                   "ToList")
    :fell-through)
  (:escaped 1))

;;; UNWIND-PROTECT cleanups on the Lisp side still run while unwinding.
(deftest dncb-unwind-protect-runs
  (let ((log '()))
    (block b
      (unwind-protect
           (dotnet:invoke (dncb-func (lambda (x) (declare (ignore x)) (return-from b (reverse log))))
                          "Invoke" 1)
        (push :cleanup log)))
    (list (reverse log)))
  ((:cleanup)))

;;; The error-containment contract is unchanged: a Lisp ERROR inside a callback is
;;; still reported and the delegate yields the return type's default.
(deftest dncb-error-still-contained
  (dotnet:invoke (dncb-func (lambda (x) (declare (ignore x)) (error "boom"))) "Invoke" 5)
  0)

;;; A real .NET failure still surfaces as a Lisp error.
(deftest dncb-real-dotnet-failure-still-errors
  (handler-case (progn (dotnet:invoke (dncb-ints 1 2) "get_Item" 99) :no-error)
    (error () :error))
  :error)
