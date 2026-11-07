;;; When no overload accepts the arguments because a value does not fit the parameter types
;;; (an integer outside a fixed-width parameter, a ratio for a System.Decimal), the error is a
;;; TYPE-ERROR naming the value and the width, as DOTNET:NEW-ARRAY already did, not a bare
;;; "method not found" PROGRAM-ERROR. Calls that fit are unchanged.

(defmacro %dnre-class (form)
  `(handler-case (progn ,form :ok)
     (type-error (e) (list :type-error (type-error-datum e)))
     (program-error () :program-error)
     (error () :other-error)))

(deftest dotnet-argument-range-error.out-of-range
  (list (%dnre-class (dotnet:static "System.Char" "ConvertFromUtf32" (expt 2 40)))
        (%dnre-class (dotnet:static "System.Decimal" "Multiply" 1/3 1))
        (%dnre-class (dotnet:new-array "System.Byte" 300)))
  ((:type-error 1099511627776) (:type-error 1/3) (:type-error 300)))

(deftest dotnet-argument-range-error.valid-calls
  (list (dotnet:static "System.Char" "ConvertFromUtf32" 65)
        (= 6 (dotnet:static "System.Decimal" "Multiply" 2 3))
        (dotnet:static "System.Math" "Max" 3 9)
        (%dnre-class (dotnet:static "System.Math" "NoSuchMethodHere" 1)))
  ("A" t 9 :program-error))
