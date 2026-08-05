;;; dotnet:-> — member chain in call order, and dotnet:doto — several members
;;; applied to one object. Both expand to plain dotnet:invoke calls; they exist
;;; because nested interop otherwise reads inside-out:
;;;   (dotnet:invoke (dotnet:invoke (dotnet:invoke uri "Host") "Substring" 0 7) "ToUpper")
;;;
;;; Member names are strings: the reader upcases bare symbols while .NET member
;;; names are case-sensitive. A symbol is accepted and contributes its name
;;; verbatim, so |Host| works.

(defun dnch-uri () (dotnet:new "System.Uri" "http://example.com/a/b?q=1"))

(deftest dnch-chain-two-steps
  (dotnet:-> (dnch-uri) "Host" "Length")
  11)

(deftest dnch-chain-with-args
  (dotnet:-> (dnch-uri) "Host" ("Substring" 0 7) "ToUpper")
  "EXAMPLE")

(deftest dnch-matches-nested-invoke
  (equal (dotnet:-> (dnch-uri) "Host" ("Substring" 0 7) "ToUpper")
         (dotnet:invoke (dotnet:invoke (dotnet:invoke (dnch-uri) "Host") "Substring" 0 7)
                        "ToUpper"))
  t)

(deftest dnch-single-step-is-plain-member
  (dotnet:-> (dnch-uri) "Host")
  "example.com")

(deftest dnch-no-steps-is-the-object
  (dotnet:invoke (dotnet:-> (dnch-uri)) "get_Host")
  "example.com")

;;; A bar-quoted symbol keeps its case and names the member.
(deftest dnch-symbol-member-name
  (dotnet:-> (dnch-uri) |Host|)
  "example.com")

;;; The chain is a place.
(deftest dnch-setf-property
  (let ((sb (dotnet:new "System.Text.StringBuilder" "hi")))
    (setf (dotnet:-> sb "Capacity") 64)
    (dotnet:-> sb "Capacity"))
  64)

(deftest dnch-setf-indexed-property
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32")))))
    (dotnet:invoke l "Add" 7)
    (setf (dotnet:-> l ("Item" 0)) 99)
    (dotnet:-> l ("Item" 0)))
  99)

;;; doto applies each step to the same object and returns that object.
(deftest dnch-doto-returns-object
  (dotnet:invoke (dotnet:doto (dotnet:new "System.Text.StringBuilder")
                              ("Append" "a") ("Append" "b") ("Append" 1))
                 "ToString")
  "ab1")

(deftest dnch-doto-evaluates-object-once
  (let ((calls 0))
    (flet ((mk () (incf calls) (dotnet:new "System.Text.StringBuilder")))
      (dotnet:doto (mk) ("Append" "x") ("Append" "y"))
      calls))
  1)

(deftest dnch-doto-no-steps
  (dotnet:invoke (dotnet:doto (dotnet:new "System.Text.StringBuilder" "z")) "ToString")
  "z")
