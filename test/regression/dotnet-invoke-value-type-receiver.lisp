;;; DOTNET:INVOKE on a value whose .NET type the compiler knows -- (dotnet:box V
;;; "T") or (dotnet:new "T" ...) -- compiles to a direct call. For a value type
;;; the receiver was always passed as a pointer into the value, which is what the
;;; type's own methods take, but not what a non-virtual method declared on a
;;; reference type takes: Object.GetType and Enum.HasFlag read a pointer as an
;;; object reference, and (dotnet:invoke (dotnet:box 1 "System.Int16") "GetType")
;;; was a NullReferenceException (an access violation outside a handler). The
;;; arguments had the same mismatch the other way: each was marshalled to its own
;;; static type, so a value passed to an Enum / Object parameter (HasFlag) went
;;; in unboxed, an invalid program.
;;;
;;; The forms below are compiled with the types in sight, so the direct path is
;;; the one taken; the same forms evaluated by the interpreter take the dynamic
;;; path, which already worked.

(defun %vtr-type-name (type-object)
  (dotnet:invoke type-object "get_FullName"))

(defmacro %vtr-compiled (form)
  `(funcall (compile nil '(lambda () ,form))))

(deftest dotnet-invoke-gettype-on-boxed-value-types
  (list (%vtr-type-name (dotnet:invoke (dotnet:box 1 "System.Int16") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:box 1 "System.Int32") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:box 1 "System.Int64") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:box 1 "System.Byte") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:box 1.5d0 "System.Double") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:box 1.5d0 "System.Decimal") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:box 3 "System.DayOfWeek") "GetType"))
        (%vtr-type-name (dotnet:invoke (dotnet:new "System.DateTime" 2000 1 2) "GetType")))
  ("System.Int16" "System.Int32" "System.Int64" "System.Byte" "System.Double"
   "System.Decimal" "System.DayOfWeek" "System.DateTime"))

(deftest-emitting-only dotnet-invoke-gettype-on-boxed-value-types-compiled
  (list (%vtr-type-name (%vtr-compiled (dotnet:invoke (dotnet:box 1 "System.Int16") "GetType")))
        (%vtr-type-name (%vtr-compiled (dotnet:invoke (dotnet:box 3 "System.DayOfWeek") "GetType")))
        (%vtr-type-name (%vtr-compiled (dotnet:invoke (dotnet:new "System.DateTime" 2000 1 2) "GetType"))))
  ("System.Int16" "System.DayOfWeek" "System.DateTime"))

;;; The other methods every value inherits from Object, on the same path. They
;;; are virtual and overridden by the value type, and went through before.
(deftest dotnet-invoke-object-methods-on-boxed-value-types
  (list (dotnet:invoke (dotnet:box 7 "System.Int16") "ToString")
        (dotnet:invoke (dotnet:box 7 "System.Int16") "GetHashCode")
        (dotnet:invoke (dotnet:box 7 "System.Int16") "Equals" (dotnet:box 7 "System.Int16"))
        (dotnet:invoke (dotnet:box 7 "System.Int16") "Equals" (dotnet:box 8 "System.Int16"))
        (dotnet:invoke (dotnet:box 3 "System.DayOfWeek") "ToString")
        (dotnet:invoke (dotnet:new "System.DateTime" 2000 1 2) "get_Day"))
  ("7" 7 t nil "Wednesday" 2))

;;; A value passed to a parameter of a reference type (Enum.HasFlag(Enum)), and
;;; one passed to a wider value type (Int32.Equals(Int32) given an Int16).
(deftest dotnet-invoke-value-argument-to-wider-parameter
  (list (dotnet:invoke (dotnet:box 3 "System.DayOfWeek") "HasFlag" (dotnet:box 1 "System.DayOfWeek"))
        (dotnet:invoke (dotnet:box 3 "System.DayOfWeek") "HasFlag" (dotnet:box 4 "System.DayOfWeek"))
        (dotnet:invoke (dotnet:box 7 "System.Int32") "Equals" (dotnet:box 7 "System.Int16")))
  (t nil t))

(deftest-emitting-only dotnet-invoke-value-argument-to-wider-parameter-compiled
  (list (%vtr-compiled (dotnet:invoke (dotnet:box 3 "System.DayOfWeek") "HasFlag"
                                      (dotnet:box 1 "System.DayOfWeek")))
        (%vtr-compiled (dotnet:invoke (dotnet:box 7 "System.Int32") "Equals"
                                      (dotnet:box 7 "System.Int16"))))
  (t t))
