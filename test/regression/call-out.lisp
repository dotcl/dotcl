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
