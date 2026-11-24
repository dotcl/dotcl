;;; A ratio or a bignum passed to a .NET Double / Single parameter converts by
;;; the rounding COERCE uses; a bignum beyond the format's range signals instead
;;; of becoming an infinity.

(deftest ratio-to-double-parameter
  (list (dotnet:static "System.Math" "Sqrt" 1/4)
        (dotnet:static "System.Math" "Sqrt" 1/3)
        (dotnet:static "System.Convert" "ToDouble" 1/3))
  (0.5d0 0.5773502691896257d0 0.3333333333333333d0))

(deftest ratio-to-single-parameter
  (dotnet:static "System.MathF" "Sqrt" 1/4)
  0.5)

(deftest bignum-to-double-parameter
  (list (dotnet:static "System.Math" "Sqrt" (expt 2 70))
        (dotnet:static "System.Math" "Abs" (expt 2 70))
        (dotnet:static "System.Math" "Abs" (expt 2 100)))
  (34359738368.0d0 1.1805916207174113d21 1.2676506002282294d30))

(deftest bignum-to-single-parameter
  (dotnet:static "System.MathF" "Sqrt" (expt 2 70))
  34359738368.0)

(deftest exact-real-prefers-double-overload
  (list (dotnet:static "System.Math" "Max" 1/2 1/3)
        (dotnet:static "System.Math" "Floor" 5/2)
        (dotnet:static "System.Math" "Floor" 7/2))
  (0.5d0 2.0d0 3.0d0))

(deftest convert-todecimal-ratio
  (let ((d (dotnet:static "System.Convert" "ToDecimal" 1/3)))
    (list (typep d 'dotcl:decimal)
          (princ-to-string d)))
  (t "#m0.333333333333333"))

(deftest bignum-beyond-double-signals
  (handler-case
      (progn (dotnet:static "System.Math" "Max" (expt 2 1024) 1) :no-error)
    (type-error (e)
      (let ((m (princ-to-string e)))
        (list (and (search "No overload of Math.Max" m) t)
              (and (search "Double" m) t)
              (and (search "Decimal" m) t)
              (typep (type-error-datum e) 'integer)))))
  (t t t t))

(deftest existing-numeric-calls-unchanged
  (list (dotnet:static "System.Math" "Pow" 2 10)
        (dotnet:static "System.Math" "Round" 2.567d0 2)
        (dotnet:static "System.Convert" "ToInt32" 7/2)
        (dotnet:static "System.Numerics.BigInteger" "Pow" (expt 2 70) 2))
  (1024.0d0 2.57d0 4
   1393796574908163946345982392040522594123776))
