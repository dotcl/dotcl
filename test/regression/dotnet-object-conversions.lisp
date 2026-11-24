;;; A ratio or bignum crossing to an object / IConvertible parameter arrives as
;;; the CLR value it stands for; Math keeps one double-overload rule for ratios.

(deftest ratio-object-change-type
  (list (dotnet:static "System.Convert" "ChangeType" 1/3
                       (dotnet:resolve-type "System.Double"))
        (dotnet:static "System.Convert" "ChangeType" -7/2
                       (dotnet:resolve-type "System.Double"))
        (dotnet:static "System.Convert" "ChangeType" 1/1000000000000000000000
                       (dotnet:resolve-type "System.Double")))
  (0.3333333333333333d0 -3.5d0 1.0d-21))

(deftest ratio-object-type-is-double
  (list (dotnet:invoke (dotnet:static "System.Convert" "GetTypeCode" 1/3) "ToString")
        (dotnet:invoke (dotnet:invoke (dotnet:box 1/3 "System.Object") "GetType") "get_Name")
        (dotnet:invoke (dotnet:invoke (dotnet:box 1/3 "System.IConvertible") "GetType") "get_Name"))
  ("Double" "Double" "Double"))

(deftest bignum-object-change-type
  (list (dotnet:static "System.Convert" "ChangeType" (expt 2 63)
                       (dotnet:resolve-type "System.Double"))
        (dotnet:static "System.Convert" "ChangeType" (expt 2 63)
                       (dotnet:resolve-type "System.UInt64"))
        (dotnet:static "System.Convert" "ChangeType" (1+ (expt 2 63))
                       (dotnet:resolve-type "System.UInt64")))
  (9.223372036854776d18 9223372036854775808 9223372036854775809))

(deftest bignum-object-type-depends-on-range
  (list (dotnet:invoke (dotnet:invoke (dotnet:box (expt 2 63) "System.Object") "GetType") "get_Name")
        (dotnet:invoke (dotnet:static "System.Convert" "GetTypeCode" (expt 2 63)) "ToString")
        (dotnet:invoke (dotnet:invoke (dotnet:box (expt 2 64) "System.Object") "GetType") "get_Name"))
  ("UInt64" "UInt64" "BigInteger"))

(deftest bignum-icvertible-beyond-ulong-signals
  (handler-case
      (progn (dotnet:box (expt 2 64) "System.IConvertible") :no-error)
    (type-error (e)
      (let ((m (princ-to-string e)))
        (list (and (search "IConvertible" m) t)
              (and (search "INTEGER" m) t)))))
  (t t))

(deftest bignum-at-2^64-still-overflows-uint64
  (handler-case
      (progn (dotnet:static "System.Convert" "ToUInt64" (expt 2 64)) :no-error)
    (error (e) (and (search "overflow" (princ-to-string e)) t)))
  t)

(deftest bignum-in-uint64-range-crosses-exactly
  (list (dotnet:static "System.Convert" "ToUInt64" (expt 2 63))
        (dotnet:static "System.Convert" "ToString" (expt 2 63) 16)
        (dotnet:static "System.Convert" "ToString" (expt 2 62) 16))
  (9223372036854775808 "8000000000000000" "4000000000000000"))

(deftest object-parameter-formats-the-number
  (list (dotnet:static "System.String" "Format" "{0}" 1/3)
        (dotnet:static "System.String" "Format" "{0}" (expt 2 63)))
  ("0.3333333333333333" "9223372036854775808"))

(deftest instance-object-parameter-takes-ratio-and-bignum
  (let ((l (dotnet:new "System.Collections.ArrayList")))
    (dotnet:invoke l "Add" 1/3)
    (dotnet:invoke l "Add" (expt 2 63))
    (dotnet:invoke l "Add" (expt 2 64))
    (list (dotnet:invoke l "get_Item" 0)
          (dotnet:invoke l "get_Item" 1)
          (dotnet:invoke l "get_Item" 2)))
  (0.3333333333333333d0 9223372036854775808 18446744073709551616))

(deftest math-ratio-rounding-takes-double
  (list (dotnet:static "System.Math" "Floor" 7/2)
        (dotnet:static "System.Math" "Ceiling" 7/2)
        (dotnet:static "System.Math" "Round" 7/2)
        (dotnet:static "System.Math" "Truncate" 7/2)
        (dotnet:static "System.Math" "Abs" -7/2)
        (typep (dotnet:static "System.Math" "Floor" 7/2) 'double-float))
  (3.0d0 4.0d0 4.0d0 3.0d0 3.5d0 t))

(deftest math-inexact-ratio-rounding-takes-double
  (list (dotnet:static "System.Math" "Floor" 1/3)
        (dotnet:static "System.Math" "Ceiling" 1/3)
        (dotnet:static "System.Math" "Abs" 1/3)
        (dotnet:static "System.Math" "Floor" -7/2)
        (dotnet:static "System.Math" "Floor" 1/1000000000000000000000))
  (0.0d0 1.0d0 0.3333333333333333d0 -4.0d0 0.0d0))

(deftest math-overload-family-ratio-takes-double
  (list (dotnet:static "System.Math" "Sign" 7/2)
        (dotnet:static "System.Math" "Sign" -7/2)
        (dotnet:static "System.Math" "Max" 7/2 1/2)
        (dotnet:static "System.Math" "Min" 7/2 1/2))
  (1 -1 3.5d0 0.5d0))

(deftest math-bignum-and-integer-take-double
  (list (dotnet:static "System.Math" "Floor" (expt 2 70))
        (dotnet:static "System.Math" "Floor" 7)
        (dotnet:static "System.Math" "Abs" -7/2))
  (1.1805916207174113d21 7.0d0 3.5d0))

(deftest ratio-beyond-double-signals
  (handler-case
      (progn (dotnet:static "System.Math" "Floor" (/ (expt 2 2000) 3)) :no-error)
    (type-error (e)
      (let ((m (princ-to-string e)))
        (list (and (search "RATIO" m) t)
              (and (search "Double" m) t)
              (and (search "out of range" m) t)))))
  (t t t))
