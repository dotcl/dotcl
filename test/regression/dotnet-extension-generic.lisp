;;; Extension-method fallback with more than one type parameter.
;;;
;;; Only single-type-parameter extension methods used to resolve, so LINQ's
;;; Select<TSource,TResult> was uncallable: TResult cannot be inferred from a Lisp
;;; closure (it has no return type). Type parameters that the receiver does not
;;; determine now default to System.Object, which keeps such methods callable —
;;; values come back to Lisp unwrapped anyway.
;;;
;;; Overload choice also has to respect the Lisp function's arity: Select and Where
;;; each have a plain and an indexed overload with the same parameter count.

(defun dneg-ints (&rest xs)
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32")))))
    (dolist (x xs l) (dotnet:invoke l "Add" x))))

(defun dneg-to-list (enumerable)
  (let ((lst (dotnet:invoke enumerable "ToList")) (acc '()))
    (dotimes (i (dotnet:invoke lst "get_Count") (nreverse acc))
      (push (dotnet:invoke lst "get_Item" i) acc))))

(deftest dneg-select-two-type-params
  (dneg-to-list (dotnet:invoke (dneg-ints 1 2 3) "Select" (lambda (x) (* x 10))))
  (10 20 30))

;;; A 2-argument lambda selects the indexed overload, a 1-argument one does not.
(deftest dneg-select-indexed-overload
  (dneg-to-list (dotnet:invoke (dneg-ints 7 8 9) "Select" (lambda (x i) (+ x i))))
  (7 9 11))

(deftest dneg-where-indexed-overload
  (dneg-to-list (dotnet:invoke (dneg-ints 10 11 12 13) "Where"
                               (lambda (x i) (declare (ignore x)) (evenp i))))
  (10 12))

;;; Single-type-parameter resolution (the pre-existing path) still works.
(deftest dneg-where-plain
  (dneg-to-list (dotnet:invoke (dneg-ints 1 2 3 4) "Where" (lambda (x) (evenp x))))
  (2 4))

(deftest dneg-count-and-first
  (list (dotnet:invoke (dneg-ints 1 2 3) "Count")
        (dotnet:invoke (dneg-ints 5 6) "First"))
  (3 5))

;;; Chaining through the object-typed result of Select keeps working.
(deftest dneg-select-then-where
  (dneg-to-list (dotnet:invoke (dotnet:invoke (dneg-ints 1 2 3 4) "Select" (lambda (x) (* x 3)))
                               "Where" (lambda (x) (> x 5))))
  (6 9 12))

;;; A receiver of a different element type infers its own TSource.
(deftest dneg-select-over-strings
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.String")))))
    (dotnet:invoke l "Add" "ab")
    (dotnet:invoke l "Add" "cde")
    ;; Elements of a List<string> arrive as Lisp strings, so measure them with LENGTH.
    (dneg-to-list (dotnet:invoke l "Select" (lambda (s) (length s)))))
  (2 3))

;;; Non-generic extension methods are unaffected.
(deftest dneg-non-generic-extension
  (dotnet:invoke (dneg-ints 1 2 3) "Sum")
  6)

;;; An unknown member is still an error, not a silent extension-method match.
(deftest dneg-unknown-member-errors
  (handler-case (progn (dotnet:invoke (dneg-ints 1) "NoSuchMemberHere") :no-error)
    (error () :error))
  :error)

;;; A delegate the caller built says what the other type parameters are: a
;;; Func<int,string> pins TResult=string. Type-argument inference used to read the
;;; receiver only and default the rest to object, so the supplied Func<int,string>
;;; fit no parameter and the very same call that worked with a bare lambda
;;; resolved to nothing.
(deftest dneg-explicit-delegate-pins-result-type
  (dneg-to-list
   (dotnet:invoke (dneg-ints 1 2 3) "Select"
                  (dotnet:make-delegate "System.Func`2[System.Int32,System.String]"
                                        (lambda (x) (format nil "n~d" x)))))
  ("n1" "n2" "n3"))

;;; Naming the defining class outright is the same call. InvokeMember cannot
;;; instantiate a generic definition, so this failed while the extension-style call
;;; on the same list worked.
(deftest dneg-static-generic-extension-by-name
  (dneg-to-list (dotnet:static "System.Linq.Enumerable" "Select"
                               (dneg-ints 1 2 3) (lambda (x) (* x 10))))
  (10 20 30))

;;; …but only from the class that declares it: naming an unrelated type must not
;;; reach Enumerable's method.
(deftest dneg-static-generic-does-not-cross-classes
  (handler-case (progn (dotnet:static "System.Math" "Select"
                                      (dneg-ints 1 2 3) (lambda (x) x))
                       :no-error)
    (error () :error))
  :error)
