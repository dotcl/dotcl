;;; A malformed class spec is an error in the program's structure, so it has to
;;; reach the caller as PROGRAM-ERROR. The builder rejects these with C#
;;; ArgumentException (42 sites), and the entry point used to wrap them in a
;;; plain ERROR — which no (handler-case ... (program-error ...)) catches, and
;;; which nothing else about the message distinguishes from a genuine runtime
;;; failure.
;;;
;;; The message also carried the C# plumbing: "field name must be non-empty
;;; (Parameter 'fields')" names a parameter of a method the Lisp caller never
;;; wrote and cannot see.

(load "contrib/dotnet-class/dotnet-class.lisp")

(defun %dcse-outcome (thunk)
  "Condition class and message of THUNK's error, or :no-error."
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) (list (typep e 'program-error)
                     (and (search "(Parameter" (princ-to-string e)) t)))))

(deftest define-class-spec-errors.empty-name
  (%dcse-outcome (lambda () (dotnet:define-class "" () (:fields ("A" "System.Int32")))))
  (t nil))

(deftest define-class-spec-errors.empty-field-name
  (%dcse-outcome (lambda () (dotnet:define-class "DcseTest.A" ()
                              (:fields ("" "System.Int32")))))
  (t nil))

(deftest define-class-spec-errors.duplicate-method-overload
  (%dcse-outcome (lambda () (dotnet:define-class "DcseTest.B" ()
                              (:methods ("M" () :returns "System.Int32" 1)
                                        ("M" () :returns "System.Int32" 2)))))
  (t nil))

;;; A well-formed spec still defines its class — the mapping must not swallow
;;; anything that was working.

(dotnet:define-class "DcseTest.Ok" ()
  (:fields ("N" "System.Int32"))
  (:methods ("Twice" () :returns "System.Int32"
              (* 2 (dotnet:invoke self "N")))))

(deftest define-class-spec-errors.valid-spec-still-works
  (let ((o (dotnet:new "DcseTest.Ok")))
    (setf (dotnet:invoke o "N") 21)
    (dotnet:invoke o "Twice"))
  42)
