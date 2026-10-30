;;; dotnet:invoke on a member the receiver's own type does not expose: an
;;; explicitly implemented interface member, or an extension method.
;;;
;;; The first call finds it through the fallbacks after reflection's lookup on
;;; the type fails with an exception. Later calls with the same argument types go
;;; to the fallbacks directly. Both have to give the same answer, and the later
;;; ones must not raise a .NET exception along the way.

(defun dimc-dictionary ()
  (let ((d (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.Dictionary"
                                                 (list "System.String" "System.Int32")))))
    (dotnet:invoke d "Add" "a" 1)
    (dotnet:invoke d "Add" "b" 2)
    d))

(defun dimc-first-chance-count (thunk)
  "How many .NET exceptions are thrown, caught or not, while THUNK runs."
  (let* ((count 0)
         (domain (dotnet:static "System.AppDomain" "CurrentDomain"))
         (handler (dotnet:make-delegate
                   "System.EventHandler`1[System.Runtime.ExceptionServices.FirstChanceExceptionEventArgs]"
                   (lambda (sender args)
                     (declare (ignore sender args))
                     (incf count)))))
    (dotnet:invoke domain "add_FirstChanceException" handler)
    (unwind-protect (funcall thunk)
      (dotnet:invoke domain "remove_FirstChanceException" handler))
    count))

;;; IDictionary.IsReadOnly is explicit on Dictionary.
(defun dimc-idictionary ()
  (dotnet:cast (dimc-dictionary) "System.Collections.IDictionary"))

(deftest dimc-interface-member-repeated
  (let ((d (dimc-idictionary)))
    (list (dotnet:invoke d "IsReadOnly")
          (dotnet:invoke d "IsReadOnly")
          (dotnet:invoke d "IsFixedSize")))
  (nil nil nil))

;;; Extension methods (Enumerable.Max, Sum) on a receiver that has no such
;;; method of its own, called repeatedly.
(deftest dimc-extension-method-repeated
  (let ((d (dimc-dictionary)))
    (list (dotnet:invoke (dotnet:invoke d "Values") "Max")
          (dotnet:invoke (dotnet:invoke d "Values") "Max")
          (dotnet:invoke (dotnet:invoke d "Values") "Sum")))
  (2 2 3))

;;; Once a call has gone through the fallbacks, the same call throws nothing.
(deftest dimc-repeat-throws-nothing
  (let ((d (dimc-idictionary))
        (values (dotnet:invoke (dimc-dictionary) "Values")))
    (dotnet:invoke d "IsReadOnly")
    (dotnet:invoke values "Max")
    (dimc-first-chance-count
     (lambda ()
       (dotimes (i 5)
         (dotnet:invoke d "IsReadOnly")
         (dotnet:invoke values "Max")))))
  0)

;;; A member that does not exist anywhere is still an error every time.
(deftest dimc-missing-member-still-errors
  (let ((d (dimc-dictionary)))
    (list (handler-case (progn (dotnet:invoke d "NoSuchMemberAnywhere") :returned)
            (error () :error))
          (handler-case (progn (dotnet:invoke d "NoSuchMemberAnywhere") :returned)
            (error () :error))))
  (:error :error))
