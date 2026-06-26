;;; Regression tests for dotnet:call-out: ref/out parameter support

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

;;; dotnet:resolve-type exposes ResolveDotNetType (dotcl/dotcl#17).
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

;;; BCL types whose assembly is not the corelib and whose name doesn't
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

;;; Typed array interop: dotnet:new-array builds a T[], and a Lisp
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

;;; Enum-typed parameters marshal from a Lisp integer, name string, or
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

;;; DOTNET: built-ins carry function docstrings (dotcl/dotcl#25).
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

;;; dotnet:invoke / dotnet:static may omit C# optional (dotcl/dotcl#24)
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

;;; dotnet:new overload scoring must consider assignability of a wrapped
;;; .NET arg, so a MemoryStream picks StreamReader(Stream), not StreamReader(string)
;;; (which would fail with "Object must implement IConvertible").
(deftest issue294-ctor-assignable-wrapped-arg
  (let* ((ms (dotnet:new "System.IO.MemoryStream"))
         (sr (dotnet:new "System.IO.StreamReader" ms)))
    (notnot (dotnet:invoke sr "get_BaseStream")))
  t)

;;; dotnet:new ctor selection must admit a ctor whose trailing params are all
;;; optional (filling their defaults), so a wrapped value-type struct arg picks
;;; ColorBox(ColorVal, double=1) over the fixed-arity ColorBox(uint) — which
;;; would Convert.ChangeType(struct, uint) and fail "Object must implement
;;; IConvertible". Models Avalonia.Media.SolidColorBrush(Color).
(deftest issue357-ctor-optional-tail-prefers-struct-over-uint
  (let* ((c (dotnet:static "DotCL.TestSupport.ColorVal" "Parse" "#808080"))
         (b (dotnet:new "DotCL.TestSupport.ColorBox" c)))
    (dotnet:invoke b "Tag"))
  "color 128 op 1")

;;; dotnet:call-out-generic — generic method + out/ref parameters combined
;;; (dotcl/dotcl#45). dotnet:static-generic/invoke-generic handle generics but
;;; not out/ref; dotnet:call-out handles out/ref but not open generic defs.
;;; Enum.TryParse<TEnum>(string, out TEnum) exercises both at once.
(deftest issue45-call-out-generic-enum-tryparse-ok
  (multiple-value-bind (ok day)
      (dotnet:call-out-generic "System.Enum" "TryParse" '("System.DayOfWeek") "Monday")
    (list (notnot ok) (dotnet:invoke day "ToString")))
  (t "Monday"))

(deftest issue45-call-out-generic-enum-tryparse-fail
  (multiple-value-bind (ok day)
      (dotnet:call-out-generic "System.Enum" "TryParse" '("System.DayOfWeek") "Nonsense")
    (declare (ignore day))
    ok)
  nil)

;;; dotnet:make-array — sized typed .NET array creation, 1-D and multi-dim
;;; (dotcl/dotcl#45). Distinct from dotnet:new-array (which fills from elements).
(deftest issue45-make-array-1d
  (let ((a (dotnet:make-array "System.Int32" 3)))
    (dotnet:invoke a "set_Item" 0 42)
    (dotnet:invoke a "set_Item" 2 7)
    (list (dotnet:invoke a "get_Length")
          (dotnet:invoke a "get_Item" 0)
          (dotnet:invoke a "get_Item" 1)
          (dotnet:invoke a "get_Item" 2)))
  (3 42 0 7))

(deftest issue45-make-array-2d
  (let ((m (dotnet:make-array "System.Single" 2 3)))
    (dotnet:invoke m "set_Item" 1 2 9.5)
    (list (dotnet:invoke m "get_Length")
          (dotnet:invoke m "get_Rank")
          (dotnet:invoke m "get_Item" 1 2)))
  (6 2 9.5d0))

;;; dotnet:is-instance-of / dotnet:cast (dotcl/dotcl#45). is-instance-of replaces
;;; manual Type.IsAssignableFrom; cast verifies + re-wraps with a hint type for
;;; overload resolution (reference upcast).
(deftest issue45-is-instance-of
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (list (notnot (dotnet:is-instance-of sb "System.Object"))
          (dotnet:is-instance-of sb "System.IConvertible")
          (notnot (dotnet:is-instance-of "hi" "System.String"))
          (dotnet:is-instance-of "hi" "System.Int32")))
  (t nil t nil))

(deftest issue45-cast-upcast-hint
  (let* ((sb (dotnet:new "System.Text.StringBuilder"))
         (o (dotnet:cast sb "System.Object")))
    (dotnet:invoke (dotnet:hint-type o) "get_Name"))
  "Object")

(deftest issue45-cast-bad-errors
  (handler-case (progn (dotnet:cast (dotnet:new "System.Text.StringBuilder") "System.Int32") :no-error)
    (error () :errored))
  :errored)

;;; dotnet:enum-or — combine [Flags] enum members with bitwise OR (dotcl/dotcl#45).
;;; Members may be name strings/symbols (case-insensitive), integers, or enum values.
(deftest issue45-enum-or-names
  (dotnet:invoke (dotnet:enum-or "System.IO.FileAccess" "Read" "Write") "ToString")
  "ReadWrite")

(deftest issue45-enum-or-mixed-int-and-symbol
  (list (dotnet:invoke (dotnet:enum-or "System.IO.FileAccess" 1 "Write") "ToString")
        (dotnet:invoke (dotnet:enum-or "System.IO.FileAccess" :read :write) "ToString"))
  ("ReadWrite" "ReadWrite"))

(deftest issue45-enum-or-bad-member-errors
  (handler-case (progn (dotnet:enum-or "System.IO.FileAccess" "Nope") :ok)
    (error () :errored))
  :errored)

(deftest issue45-enum-or-non-enum-errors
  (handler-case (progn (dotnet:enum-or "System.Int32" 1) :ok)
    (error () :errored))
  :errored)

;;; dotnet:make-generic-type — construct a closed generic System.Type from an
;;; open definition + type-arg names; usable directly with dotnet:new (which now
;;; accepts a resolved System.Type as its first arg). (dotcl/dotcl#45)
(deftest issue45-make-generic-type-infer-arity
  (let* ((dty (dotnet:make-generic-type "System.Collections.Generic.Dictionary"
                                        '("System.String" "System.Int32")))
         (d (dotnet:new dty)))
    (dotnet:invoke d "set_Item" "x" 42)
    (list (dotnet:invoke d "get_Count") (dotnet:invoke d "get_Item" "x")))
  (1 42))

(deftest issue45-make-generic-type-explicit-backtick
  (dotnet:invoke (dotnet:make-generic-type "System.Collections.Generic.List`1" '("System.Int32"))
                 "get_Name")
  "List`1")

(deftest issue45-make-generic-type-arity-mismatch-errors
  (handler-case
      (progn (dotnet:make-generic-type "System.Collections.Generic.Dictionary" '("System.String")) :ok)
    (error () :errored))
  :errored)

;;; aref / (setf aref) transparency over wrapped .NET arrays (dotcl/dotcl#45).
;;; Standard aref now indexes a System.Array (e.g. from dotnet:make-array) directly,
;;; 1-D and multi-dimensional, marshalling values to the element type.
(deftest issue364-aref-1d
  (let ((a (dotnet:make-array "System.Int32" 3)))
    (setf (aref a 0) 10 (aref a 2) 99)
    (list (aref a 0) (aref a 1) (aref a 2)))
  (10 0 99))

(deftest issue364-aref-2d
  (let ((m (dotnet:make-array "System.Double" 2 3)))
    (setf (aref m 1 2) 4.5d0)
    (setf (aref m 0 0) 1.0d0)
    (list (aref m 0 0) (aref m 1 2) (aref m 0 1)))
  (1.0d0 4.5d0 0.0d0))

(deftest issue364-aref-string-elt
  (let ((s (dotnet:make-array "System.String" 2)))
    (setf (aref s 0) "hi")
    (aref s 0))
  "hi")

;;; (dotcl/dotcl#45): raw .NET exceptions caught by handler-case preserve
;;; their CLR type; dotnet:exception-type / dotnet:exception-typep expose it, and
;;; dotnet:handler-bind dispatches on specific .NET exception types.
;; A drive-less relative filename in an existing directory (the CWD) so the
;; open fails with FileNotFoundException on every OS. A "C:/..." path is a
;; *missing directory* on Unix → DirectoryNotFoundException, which made this
;; test platform-dependent (green on Windows, red on macOS/Linux).
(defun %i368-open-missing ()
  (dotnet:invoke (dotnet:static "System.IO.File" "OpenRead" "dotcl-nonexistent-zzz-file.txt")
                 "ReadByte"))

(deftest i368-exception-type-and-typep
  (handler-case (%i368-open-missing)
    (error (c)
      (list (dotnet:invoke (dotnet:exception-type c) "get_Name")  ; CLR type name
            (notnot (dotnet:exception-typep c "System.IO.FileNotFoundException"))
            (notnot (dotnet:exception-typep c "System.IO.IOException"))  ; base class
            (dotnet:exception-typep c "System.ArgumentException"))))     ; unrelated
  ("FileNotFoundException" t t nil))

(deftest i368-plain-condition-has-no-clr-type
  (handler-case (error "plain lisp error")
    (error (c) (dotnet:exception-type c)))
  nil)

(deftest i368-handler-bind-dispatch-on-type
  (block done
    (dotnet:handler-bind (("System.IO.IOException" (c)
                            (return-from done
                              (list :caught (dotnet:invoke (dotnet:exception-type c) "get_Name")))))
      (%i368-open-missing)))
  (:caught "FileNotFoundException"))

(deftest i368-handler-bind-non-match-propagates
  (handler-case
      (dotnet:handler-bind (("System.ArgumentException" (c) (declare (ignore c)) :wrong))
        (%i368-open-missing))
    (error () :propagated))
  :propagated)

;;; (dotcl/dotcl#45): extension-method resolution — dotnet:invoke falls back
;;; to a static [Extension] method whose first param accepts the receiver. Covers
;;; non-generic and single-type-param generic (inferred from IEnumerable<T>),
;;; e.g. LINQ Enumerable.Count/Where/First on a List<int>.
(defun %i369-int-list (&rest xs)
  (let ((lst (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                   '("System.Int32")))))
    (dolist (x xs lst) (dotnet:invoke lst "Add" x))))

(deftest i369-linq-count
  (dotnet:invoke (%i369-int-list 1 5 9) "Count")
  3)

(deftest i369-linq-where-and-first
  (let* ((ft (dotnet:make-generic-type "System.Func" '("System.Int32" "System.Boolean")))
         (pred (dotnet:make-delegate ft (lambda (x) (> x 3))))
         (filtered (dotnet:invoke (%i369-int-list 1 5 9) "Where" pred)))
    (list (dotnet:invoke filtered "Count") (dotnet:invoke filtered "First")))
  (2 5))

;; a genuinely missing method still errors (extension fallback doesn't mask it)
(deftest i369-missing-method-still-errors
  (handler-case (progn (dotnet:invoke (dotnet:new "System.Object") "NoSuchMethodXyz") :ok)
    (error () :errored))
  :errored)
