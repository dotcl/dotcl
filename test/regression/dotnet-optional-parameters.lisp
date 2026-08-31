;;; Omitting a trailing optional parameter.
;;;
;;; C# writes `JsonDocument.Parse(text)` for a method declared
;;; `Parse(string, JsonDocumentOptions = default)`, and the same is true of most
;;; of ASP.NET Core, whose extension methods end in `CancellationToken = default`.
;;; Two separate paths reported "method not found" for that spelling:
;;;
;;;   - the extension-method fallback required an exact arity match, so an
;;;     omitted optional parameter meant no candidate at all
;;;   - the optional-defaults fallback for ordinary methods picked one candidate
;;;     by arity alone and gave up if the supplied argument did not convert to
;;;     it. JsonDocument.Parse has five overloads, all of arity 2, so a string
;;;     was offered to whichever one reflection returned first
;;;
;;; System.Text.Json is part of the shared framework, so this needs no download.

(dotnet:load-assembly "System.Text.Json")

(defun dop-parse (text) (dotnet:static "System.Text.Json.JsonDocument" "Parse" text))
(defun dop-object-type () (dotnet:resolve-type "System.Object"))

;;; An ordinary static whose second parameter is optional, among four sibling
;;; overloads of the same arity that take a different first parameter.
(deftest dop-static-optional-omitted
  (dotnet:invoke (dotnet:invoke (dop-parse "{\"a\":1}") "RootElement") "ToString")
  "{\"a\":1}")

;;; Supplying the optional argument keeps working.
(deftest dop-static-optional-supplied
  (dotnet:invoke
   (dotnet:invoke (dotnet:static "System.Text.Json.JsonDocument" "Parse" "[1,2]"
                                 (dotnet:new "System.Text.Json.JsonDocumentOptions"))
                  "RootElement")
   "ToString")
  "[1,2]")

;;; An extension method -- JsonSerializer.Deserialize(this JsonDocument, Type,
;;; JsonSerializerOptions = null) -- called without its optional argument.
(deftest dop-extension-optional-omitted
  (let ((value (dotnet:invoke (dop-parse "{\"b\":2}") "Deserialize" (dop-object-type))))
    (and value (dotnet:invoke value "ToString")))
  "{\"b\":2}")

;;; The same call with the optional argument present.
(deftest dop-extension-optional-supplied
  (let ((value (dotnet:invoke (dop-parse "{\"c\":3}") "Deserialize"
                              (dop-object-type) (dotnet:null))))
    (and value (dotnet:invoke value "ToString")))
  "{\"c\":3}")

;;; A method that has no overload for the supplied argument is still an error,
;;; rather than being bound to something that happens to have the right arity.
(deftest dop-unbindable-argument-still-errors
  (handler-case (progn (dotnet:static "System.Text.Json.JsonDocument" "Parse"
                                      (dotnet:new "System.Text.StringBuilder"))
                       :no-error)
    (error () :error))
  :error)
