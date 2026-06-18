;;; Regression tests for D894 — dotnet:call-out: ref/out parameter support (#190)

;;; Int32.TryParse(string, out int) — static, successful parse
(deftest d894-try-parse-success
  (multiple-value-bind (ok n)
      (dotnet:call-out "System.Int32" "TryParse" "42")
    (list ok n))
  (t 42))

;;; Int32.TryParse — failure case returns nil + 0
(deftest d894-try-parse-failure
  (multiple-value-bind (ok n)
      (dotnet:call-out "System.Int32" "TryParse" "not-a-number")
    (list (not ok) n))
  (t 0))

;;; Double.TryParse
(deftest d894-double-try-parse
  (multiple-value-bind (ok n)
      (dotnet:call-out "System.Double" "TryParse" "3.14")
    (and ok (< (abs (- n 3.14)) 0.001)))
  t)

;;; Dictionary.TryGetValue — instance method with out param
(deftest d894-dict-try-get-value
  (let ((d (dotnet:new "System.Collections.Generic.Dictionary`2[System.String,System.Int32]")))
    (setf (dotnet:ref d "answer") 42)
    (multiple-value-bind (found val)
        (dotnet:call-out d "TryGetValue" "answer")
      (list found val)))
  (t 42))

;;; TryGetValue — key absent
(deftest d894-dict-try-get-missing
  (let ((d (dotnet:new "System.Collections.Generic.Dictionary`2[System.String,System.Int32]")))
    (multiple-value-bind (found val)
        (dotnet:call-out d "TryGetValue" "missing")
      (list (not found) val)))
  (t 0))

;;; D1119 (dotcl/dotcl#17) — dotnet:resolve-type exposes ResolveDotNetType.
(deftest d1119-resolve-type-fullname
  (dotnet:invoke (dotnet:resolve-type "System.String") "get_FullName")
  "System.String")

;;; Resolved Type unwraps where a System.Type is expected (round-trips through invoke).
(deftest d1119-resolve-type-name
  (dotnet:invoke (dotnet:resolve-type "System.Int32") "get_Name")
  "Int32")

(deftest d1119-resolve-type-not-found-errors
  (handler-case (progn (dotnet:resolve-type "No.Such.Type.ZZZ") nil)
    (error () t))
  t)

(deftest d1119-resolve-type-requires-string
  (handler-case (progn (dotnet:resolve-type 42) nil)
    (type-error () t))
  t)

;;; #304 — BCL types whose assembly is not the corelib and whose name doesn't
;;; match its assembly (System.Collections.Queue lives in
;;; System.Collections.NonGeneric) must resolve via mscorlib/netstandard facade
;;; forwarding, NOT fall through to the COM ProgID path. Queue is registered as a
;;; legacy .NET Framework COM component, so the old code activated it via mscoree
;;; and crashed the process uncatchably. A clean resolution proves the facade
;;; path runs first and the System.* COM guard holds.
(deftest issue304-resolve-noncorelib-bcl-type
  (dotnet:invoke (dotnet:resolve-type "System.Collections.Queue") "get_FullName")
  "System.Collections.Queue")

(deftest issue304-new-noncorelib-bcl-type
  (let ((q (dotnet:new "System.Collections.Queue")))
    (dotnet:invoke q "Enqueue" 1)
    (dotnet:invoke q "Enqueue" 2)
    (dotnet:invoke q "get_Count"))
  2)

;;; #303 — typed array interop: dotnet:new-array builds a T[], and a Lisp
;;; list/vector auto-marshals to an array-typed parameter/property.
(deftest issue303-new-array-string
  ;; new-array "System.String" -> string[]; passes into String.Join(string,string[]).
  (dotnet:static "System.String" "Join" ","
                 (dotnet:new-array "System.String" "a" "b" "c"))
  "a,b,c")

(deftest issue303-new-array-empty
  (dotnet:static "System.String" "Join" ","
                 (dotnet:new-array "System.String"))
  "")

(deftest issue303-new-array-int-length
  (dotnet:invoke (dotnet:new-array "System.Int32" 10 20 30) "get_Length")
  3)

(deftest issue303-new-array-apply-from-list
  (dotnet:static "System.String" "Join" ""
                 (apply #'dotnet:new-array "System.String" (list "1" "2" "3")))
  "123")

;;; A Lisp list / vector marshals to a string[] method parameter (the binder's
;;; runtime-type match misses this; the marshalling fallback recovers it).
(deftest issue303-list-marshals-to-array-property
  (progn
    (dotnet:%define-class "Probe303.Holder" "System.Object"
      nil nil nil nil '(("Tags" "System.String[]")))
    (let ((h (dotnet:new "Probe303.Holder")))
      (dotnet:invoke h "set_Tags" (list "alpha" "beta" "gamma"))
      (let ((back (dotnet:invoke h "get_Tags")))
        (list (dotnet:invoke back "get_Length")
              (dotnet:invoke back "get_Item" 1)))))
  (3 "beta"))

(deftest issue303-vector-marshals-to-array-property
  (progn
    (dotnet:%define-class "Probe303.Holder2" "System.Object"
      nil nil nil nil '(("Tags" "System.String[]")))
    (let ((h (dotnet:new "Probe303.Holder2")))
      (dotnet:invoke h "set_Tags" (vector "one" "two"))
      (dotnet:invoke (dotnet:invoke h "get_Tags") "get_Length")))
  2)

;;; #302 — enum-typed parameters marshal from a Lisp integer, name string, or
;;; symbol/keyword (Enum.Parse, case-insensitive, incl. flag combinations). Needed
;;; so callers can pass RoutingStrategies / StringComparison without first fetching
;;; the enum field object. String.Equals(string,string,StringComparison): 5 =
;;; OrdinalIgnoreCase, so "a"/"A" compare equal.
(deftest issue302-enum-from-int
  (dotnet:static "System.String" "Equals" "a" "A" 5)
  t)

(deftest issue302-enum-from-name
  (dotnet:static "System.String" "Equals" "a" "A" "OrdinalIgnoreCase")
  t)

(deftest issue302-enum-from-keyword
  (dotnet:static "System.String" "Equals" "a" "A" :ordinalignorecase)
  t)

(deftest issue302-enum-case-sensitive-distinguished
  ;; Ordinal (case-sensitive) → "a" and "A" differ.
  (dotnet:static "System.String" "Equals" "a" "A" "Ordinal")
  nil)

(deftest issue302-enum-flag-combo-string
  (dotnet:invoke (dotnet:box "Read, Write" "System.IO.FileShare") "ToString")
  "ReadWrite")

(deftest issue302-enum-flag-combo-int
  (dotnet:invoke (dotnet:box 3 "System.IO.FileShare") "ToString")
  "ReadWrite")

;;; D1120 (dotcl/dotcl#25) — DOTNET: built-ins carry function docstrings.
(deftest d1120-dotnet-invoke-has-doc
  (and (stringp (documentation 'dotnet:invoke 'function)) t)
  t)

(deftest d1120-dotnet-resolve-type-has-doc
  (and (stringp (documentation 'dotnet:resolve-type 'function)) t)
  t)

(deftest d1120-dotnet-new-has-doc
  (and (stringp (documentation 'dotnet:new 'function)) t)
  t)

;;; (setf documentation) still overrides a built-in's doc.
(deftest d1120-dotnet-doc-settable
  (progn (setf (documentation 'dotnet:box 'function) "custom")
         (prog1 (documentation 'dotnet:box 'function)
           (setf (documentation 'dotnet:box 'function) nil)))
  "custom")

;;; D1123 (dotcl/dotcl#24) — dotnet:invoke / dotnet:static may omit C# optional
;;; parameters; the missing trailing args are filled with their declared defaults.
;;; DotCL.TestSupport.OptionalArgs exists in DEBUG builds only (the test runner).
(deftest d1123-invoke-all-defaults
  (dotnet:invoke (dotnet:new "DotCL.TestSupport.OptionalArgs") "Greet")
  "hi world;")

(deftest d1123-invoke-partial-defaults
  (dotnet:invoke (dotnet:new "DotCL.TestSupport.OptionalArgs") "Greet" "Bob")
  "hi Bob;")

(deftest d1123-invoke-no-defaults-needed
  (dotnet:invoke (dotnet:new "DotCL.TestSupport.OptionalArgs") "Greet" "Bob" 2)
  "hi Bob;hi Bob;")

(deftest d1123-invoke-numeric-defaults
  (let ((o (dotnet:new "DotCL.TestSupport.OptionalArgs")))
    (list (dotnet:invoke o "Add" 1)
          (dotnet:invoke o "Add" 1 2)
          (dotnet:invoke o "Add" 1 2 3)))
  (111 103 6))

(deftest d1123-static-optional-default
  (dotnet:static "DotCL.TestSupport.OptionalArgs" "StaticAdd" 1)
  6)

;;; #294: dotnet:new overload scoring must consider assignability of a wrapped
;;; .NET arg, so a MemoryStream picks StreamReader(Stream), not StreamReader(string)
;;; (which would fail with "Object must implement IConvertible").
(deftest issue294-ctor-assignable-wrapped-arg
  (let* ((ms (dotnet:new "System.IO.MemoryStream"))
         (sr (dotnet:new "System.IO.StreamReader" ms)))
    (notnot (dotnet:invoke sr "get_BaseStream")))
  t)
