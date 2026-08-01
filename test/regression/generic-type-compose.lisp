;;; Composing closed generic types from Lisp without hand-writing
;;; assembly-qualified name strings: dotnet:make-generic-type accepts an
;;; already-resolved System.Type for the open definition and for each type
;;; argument, so nested generics compose. Same for dotnet:static-generic's
;;; declaring type / type args and for dotnet:resolve-type (idempotent).
;;;
;;; The expected AQNs are never spelled out here — they carry the runtime's
;;; Version/PublicKeyToken — so each check compares a composed type against the
;;; same type resolved from a short AQN.

(defparameter *gtc-inner*
  (dotnet:make-generic-type "System.Collections.Generic.IReadOnlyList" (list "System.String")))
(defparameter *gtc-task*
  (dotnet:make-generic-type "System.Threading.Tasks.Task" (list *gtc-inner*)))
(defparameter *gtc-action*
  (dotnet:make-generic-type "System.Action" (list *gtc-task*)))

(defun gtc-aqn (type) (dotnet:invoke type "get_AssemblyQualifiedName"))

(deftest gtc-nested-compose-equals-handwritten
  (string= (gtc-aqn *gtc-action*)
           (gtc-aqn (dotnet:resolve-type
                     "System.Action`1[[System.Threading.Tasks.Task`1[[System.Collections.Generic.IReadOnlyList`1[[System.String, System.Private.CoreLib]], System.Private.CoreLib]], System.Private.CoreLib]]")))
  t)

(deftest gtc-composed-name
  (dotnet:invoke *gtc-action* "get_Name")
  "Action`1")

;;; A composed type is accepted wherever a type name is: make-delegate, new.
(deftest gtc-make-delegate-with-composed-type
  (dotnet:invoke (dotnet:invoke (dotnet:make-delegate *gtc-action* (lambda (task) (declare (ignore task)) nil))
                                "GetType")
                 "get_Name")
  "Action`1")

(deftest gtc-new-with-composed-type
  (let* ((list-of-int (dotnet:make-generic-type "System.Collections.Generic.List"
                                                (list "System.Int32")))
         (dict (dotnet:make-generic-type "System.Collections.Generic.Dictionary"
                                         (list "System.String" list-of-int)))
         (d (dotnet:new dict)))
    (dotnet:invoke d "Add" "a" (dotnet:new list-of-int))
    (dotnet:invoke d "get_Count"))
  1)

;;; The open definition may itself be a resolved System.Type.
(deftest gtc-open-definition-as-type
  (dotnet:invoke (dotnet:make-generic-type
                  (dotnet:resolve-type "System.Collections.Generic.List`1")
                  (list "System.Int32"))
                 "get_Name")
  "List`1")

;;; resolve-type accepts a name string, a symbol, and a System.Type (idempotent).
(deftest gtc-resolve-type-idempotent
  (string= (gtc-aqn (dotnet:resolve-type *gtc-action*)) (gtc-aqn *gtc-action*))
  t)

(deftest gtc-resolve-type-symbol
  (dotnet:invoke (dotnet:resolve-type '|System.Int32|) "get_Name")
  "Int32")

;;; Arity is still validated against the open definition.
(deftest gtc-arity-mismatch-errors
  (handler-case (progn (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32" "System.String"))
                       :no-error)
    (error () :error))
  :error)

;;; static-generic takes composed types for its type arguments.
(deftest gtc-static-generic-composed-type-arg
  (let* ((list-of-int (dotnet:make-generic-type "System.Collections.Generic.List"
                                                (list "System.Int32")))
         (empty (dotnet:static-generic "System.Array" "Empty" (list list-of-int))))
    (dotnet:invoke (dotnet:invoke empty "GetType") "get_Name"))
  "List`1[]")
