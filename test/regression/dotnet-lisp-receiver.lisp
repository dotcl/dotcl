;;; dotnet:invoke accepts a Lisp scalar as the receiver.
;;;
;;; .NET calls hand strings, characters and numbers back to Lisp unwrapped, so the
;;; obvious follow-up call — (dotnet:invoke (dotnet:invoke x "get_Name") "ToUpper") —
;;; used to fail with "first argument must be a .NET object". Inside a callback it
;;; was worse: the error was contained at the boundary and the result came back NIL.
;;;
;;; NIL and symbols stay errors on purpose: NIL is ambiguous between null, false and
;;; the empty list, and a symbol receiver almost always means a type name was meant
;;; (dotnet:static is the call for that).

(deftest dnlr-string-property
  (dotnet:invoke "abc" "get_Length")
  3)

(deftest dnlr-string-method
  (dotnet:invoke "abc" "ToUpper")
  "ABC")

(deftest dnlr-string-method-with-args
  (dotnet:invoke "abcdef" "Substring" 2 3)
  "cde")

(deftest dnlr-integer
  (dotnet:invoke 42 "ToString")
  "42")

(deftest dnlr-bignum
  (dotnet:invoke (expt 2 70) "ToString")
  "1180591620717411303424")

(deftest dnlr-double
  (dotnet:invoke 1.5d0 "ToString")
  "1.5")

(deftest dnlr-character
  (dotnet:invoke #\a "ToString")
  "a")

;;; The motivating shape: chain a call onto a string a .NET call returned.
(deftest dnlr-chained-on-returned-string
  (dotnet:invoke (dotnet:invoke (dotnet:new "System.Uri" "http://example.com/x") "get_Host")
                 "ToUpper")
  "EXAMPLE.COM")

;;; ...and the same inside a callback, where the failure used to be swallowed.
(deftest dnlr-inside-callback
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.String")))))
    (dotnet:invoke l "Add" "ab")
    (let ((mapped (dotnet:invoke (dotnet:invoke l "Select" (lambda (s) (dotnet:invoke s "ToUpper")))
                                 "ToList")))
      (dotnet:invoke mapped "get_Item" 0)))
  "AB")

(deftest dnlr-nil-still-errors
  (handler-case (progn (dotnet:invoke nil "ToString") :no-error)
    (error () :error))
  :error)

(deftest dnlr-symbol-still-errors
  (handler-case (progn (dotnet:invoke 'foo "ToString") :no-error)
    (error () :error))
  :error)

(deftest dnlr-list-still-errors
  (handler-case (progn (dotnet:invoke (list 1 2) "ToString") :no-error)
    (error () :error))
  :error)

;;; A .NET object receiver is unaffected.
(deftest dnlr-dotnet-object-unaffected
  (dotnet:invoke (dotnet:new "System.Text.StringBuilder" "hi") "ToString")
  "hi")
