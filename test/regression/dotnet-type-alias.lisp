;;; Regression: DOTNET::*TYPE-ALIASES* applies to every path that names a type.
;;;
;;; The table was read only by DOTNET:DEFINE-CLASS, so a registered short name
;;; worked in a class definition and nowhere else: as a generic type argument it
;;; came out as "DOTNET: type not found: TEXTURE2D". The lookup now sits at the
;;; end of type resolution in the runtime, after every real lookup has failed,
;;; so an alias cannot shadow a real type and every naming path shares it.

(require "dotnet-class")

(setf (gethash "TEST-ALIAS-STR" dotnet::*type-aliases*) "System.String")

(defun %ta-int-list ()
  (let ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]")))
    (dotnet:invoke lst "Add" 1)
    (dotnet:invoke lst "Add" 2)
    lst))

(defun %ta-convert (type-spec)
  "Result element type of ConvertAll instantiated at TYPE-SPEC, as a string."
  (let ((res (dotnet:invoke-generic (%ta-int-list) "ConvertAll" (list type-spec)
                                    (lambda (x) (format nil "<~a>" x)))))
    (dotnet:invoke res "get_Item" 0)))

;;; The reported case: a user-registered alias as a generic type argument.
(deftest dotnet-type-alias.invoke-generic-string-alias
  (%ta-convert "TEST-ALIAS-STR")
  "<1>")

;;; A symbol reaches the same table (the reader upcases it to the key's form).
(deftest dotnet-type-alias.invoke-generic-symbol-alias
  (%ta-convert '|TEST-ALIAS-STR|)
  "<1>")

;;; The table ships the BCL short names, so those work here too.
(deftest dotnet-type-alias.invoke-generic-builtin-short-name
  (%ta-convert "STRING")
  "<1>")

;;; The full name keeps working -- the alias lookup only runs after a miss.
(deftest dotnet-type-alias.invoke-generic-full-name
  (%ta-convert "System.String")
  "<1>")

;;; Other type-naming paths share the table, not just generic arguments.
(deftest dotnet-type-alias.new-and-static-take-aliases
  (progn
    (setf (gethash "TEST-ALIAS-SB" dotnet::*type-aliases*) "System.Text.StringBuilder")
    (let ((sb (dotnet:new "TEST-ALIAS-SB")))
      (dotnet:invoke sb "Append" "ab")
      (list (dotnet:invoke sb "ToString")
            (dotnet:static "TEST-ALIAS-STR" "Concat" "x" "y"))))
  ("ab" "xy"))

;;; An alias must never shadow a type that resolves normally.
(deftest dotnet-type-alias.real-type-wins
  (progn
    (setf (gethash "SYSTEM.INT32" dotnet::*type-aliases*) "System.String")
    (dotnet:invoke (dotnet:resolve-type "System.Int32") "get_FullName"))
  "System.Int32")

;;; The table is mutable at run time: a re-registered name must take effect
;;; rather than staying at whatever the first lookup cached.
(deftest dotnet-type-alias.reregistration-takes-effect
  (flet ((resolved () (dotnet:invoke (dotnet:resolve-type "TEST-ALIAS-MOVING")
                                     "get_FullName")))
    (setf (gethash "TEST-ALIAS-MOVING" dotnet::*type-aliases*) "System.String")
    (let ((first (resolved)))
      (setf (gethash "TEST-ALIAS-MOVING" dotnet::*type-aliases*) "System.Object")
      (list first (resolved))))
  ("System.String" "System.Object"))

;;; An unregistered name is still an error, and still names what was not found.
(deftest dotnet-type-alias.unknown-name-still-errors
  (handler-case (progn (%ta-convert "NO-SUCH-ALIAS-HERE") :no-error)
    (error (e) (and (search "NO-SUCH-ALIAS-HERE" (princ-to-string e)) t)))
  t)
