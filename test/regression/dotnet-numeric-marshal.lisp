;;; Flavor B of the declaration-gated extension-numeric framework: CLR numeric
;;; types whose VALUES already live in the standard CL tower, so marshalling maps
;;; them onto standard types in both directions and no extension value ever
;;; escapes into ordinary code.

;;; --- System.Numerics.BigInteger <-> CL integer ---
;;; CL integers are unbounded, so the mapping is exact in both directions.

(deftest bigint-outbound                  ; BigInteger result becomes a CL integer
  (let ((v (dotnet:static "System.Numerics.BigInteger" "Pow" 2 100)))
    (list (integerp v) v))
  (t 1267650600228229401496703205376))

(deftest bigint-inbound-fixnum            ; a fixnum feeds a BigInteger parameter
  (dotnet:static "System.Numerics.BigInteger" "Add" 2 3)
  5)

(deftest bigint-inbound-bignum            ; and so does an integer too wide for one
  (dotnet:static "System.Numerics.BigInteger" "Add"
                 1267650600228229401496703205376 1)
  1267650600228229401496703205377)

(deftest bigint-round-trip                ; out of .NET and straight back in
  (let ((v (dotnet:static "System.Numerics.BigInteger" "Pow" 3 50)))
    (dotnet:static "System.Numerics.BigInteger" "GreatestCommonDivisor" v 9))
  9)

(deftest bigint-negative
  (dotnet:static "System.Numerics.BigInteger" "Negate"
                 1267650600228229401496703205376)
  -1267650600228229401496703205376)

;;; --- CL integer -> fixed-width CLR integer, range-checked ---
;;; An integer wider than a fixnum must be able to go back into the ulong-typed
;;; parameter it came out of; and a value that does not fit signals instead of
;;; wrapping silently.

(deftest fixed-width-ulong-inbound        ; a bignum-valued ulong feeds a ulong param
  (dotnet:static "System.Numerics.BitOperations" "PopCount" 18446744073709551615)
  64)

(deftest fixed-width-ulong-round-trip     ; …including one .NET just handed back
  (let ((m (dotnet:static "System.UInt64" "MaxValue")))
    (list (integerp m)
          (dotnet:static "System.Numerics.BitOperations" "PopCount" m)))
  (t 64))

(deftest fixed-width-in-range-still-works
  (dotnet:invoke (dotnet:new-array "System.Byte" 1 2 200) "get_Length")
  3)

(deftest fixed-width-ulong-max-in-range
  (dotnet:invoke (dotnet:new-array "System.UInt64" 18446744073709551615) "get_Length")
  1)

(deftest fixed-width-overflow-signals     ; (byte)300 used to wrap to 44 in silence
  (handler-case (progn (dotnet:new-array "System.Byte" 300) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest fixed-width-underflow-signals
  (handler-case (progn (dotnet:new-array "System.SByte" -200) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest fixed-width-unsigned-rejects-negative
  (handler-case (progn (dotnet:new-array "System.UInt64" -1) :no-error)
    (type-error () :type-error))
  :type-error)

;;; A signed target also takes the value written as an unsigned N-bit pattern.
;;; CL has no way to write a negative bit pattern — (ldb (byte 32 0) x) is the
;;; idiomatic name for 32 bits and is always non-negative — so rejecting it would
;;; make BitConverter-style reinterpretation unreachable.

(deftest fixed-width-signed-int-accepts-unsigned-pattern
  (let ((a (dotnet:new-array "System.Int32" #xFFFFFFFF #x7FFFFFFF)))
    (list (dotnet:invoke a "get_Item" 0) (dotnet:invoke a "get_Item" 1)))
  (-1 2147483647))

(deftest fixed-width-signed-long-accepts-unsigned-pattern
  (dotnet:invoke (dotnet:new-array "System.Int64" #xFFFFFFFFFFFFFFFF) "get_Item" 0)
  -1)

(deftest fixed-width-bit-pattern-round-trip     ; the float-features idiom
  (let ((f (dotnet:static "System.BitConverter" "Int32BitsToSingle" #xFF800000)))
    (ldb (byte 32 0) (dotnet:static "System.BitConverter" "SingleToInt32Bits" f)))
  #xFF800000)

(deftest fixed-width-bignum-too-wide-signals
  (handler-case (progn (dotnet:new-array "System.Int32" 1099511627776) :no-error)
    (type-error () :type-error))
  :type-error)

;;; --- Int128 / UInt128 <-> CL integer ---
;;; Same flavor-B mapping as the narrower widths: the values are ordinary CL
;;; integers, only the CLR-side representation is 128 bits wide.

(deftest int128-outbound
  (let ((v (dotnet:static "System.Int128" "MaxValue")))
    (list (integerp v) v))
  (t 170141183460469231731687303715884105727))

(deftest int128-min-outbound
  (dotnet:static "System.Int128" "MinValue")
  -170141183460469231731687303715884105728)

(deftest uint128-outbound
  (dotnet:static "System.UInt128" "MaxValue")
  340282366920938463463374607431768211455)

(deftest int128-parse-outbound
  (dotnet:static "System.Int128" "Parse" "12345678901234567890123456789")
  12345678901234567890123456789)

(deftest int128-inbound-fixnum
  (dotnet:static "System.Int128" "op_Multiply" 3 4)
  12)

(deftest int128-inbound-bignum            ; wider than a fixnum, still fits 128 bits
  (dotnet:static "System.Int128" "op_Multiply" 12345678901234567890 10)
  123456789012345678900)

(deftest int128-round-trip                ; out of .NET at full width and back in
  (let ((m (dotnet:static "System.Int128" "MaxValue")))
    (dotnet:static "System.Int128" "op_Subtraction" m 1))
  170141183460469231731687303715884105726)

(deftest int128-overflow-signals          ; 2^128: fits neither the signed nor the
  (handler-case (progn (dotnet:new-array   ; unsigned 128-bit form
                        "System.Int128"
                        340282366920938463463374607431768211456)
                       :no-error)
    (type-error () :type-error))
  :type-error)

(deftest int128-accepts-unsigned-pattern  ; Int128.MaxValue + 1 = the all-ones-sign pattern
  (dotnet:invoke (dotnet:new-array "System.Int128"
                                   170141183460469231731687303715884105728)
                 "get_Item" 0)
  -170141183460469231731687303715884105728)

(deftest uint128-rejects-negative
  (handler-case (progn (dotnet:new-array "System.UInt128" -1) :no-error)
    (type-error () :type-error))
  :type-error)

;;; --- System.Half <-> single-float ---
;;; Half is 16-bit IEEE; single-float holds every Half value exactly, so outbound
;;; is lossless. Inbound narrows the same way the C# cast does: round to nearest,
;;; overflow to an infinity.

(deftest half-outbound-is-single-float
  (let ((v (dotnet:static "System.Half" "Parse" "1.5")))
    (list (typep v 'single-float) v))
  (t 1.5))

(deftest half-max-value-exact             ; 65504 is the largest finite Half
  (dotnet:static "System.Half" "MaxValue")
  65504.0)

(deftest half-inbound-single
  (dotnet:static "System.Half" "IsFinite" 1.5)
  t)

(deftest half-inbound-double
  (dotnet:static "System.Half" "IsFinite" 1.5d0)
  t)

(deftest half-inbound-integer
  (dotnet:static "System.Half" "IsFinite" 3)
  t)

(deftest half-round-trip
  (let ((h (dotnet:static "System.Half" "Parse" "-2.5")))
    (dotnet:static "System.Half" "Abs" h))
  2.5)

(deftest half-inbound-rounds-to-nearest   ; 0.1 is not representable in 16 bits
  (dotnet:static "System.Half" "Parse" "0.1")
  0.099975586)

(deftest half-inbound-overflows-to-infinity
  (dotnet:static "System.Half" "IsInfinity" 1d30)
  t)

;;; --- the same types in a signature dotcl itself emits ---
;;; A Lisp-defined .NET method declaring these as parameter/return types: the
;;; body computes with ordinary CL numbers and both boundary crossings go
;;; through the marshalling above.

(deftest wide-numeric-method-signature
  (progn
    (dotnet:%define-class "DotclTest.WideNum" nil nil nil
      (list (list "Doubled" "System.Int128" (list "System.Int128")
                  (lambda (self x) (declare (ignore self)) (* x 2)))
            (list "Halved" "System.Half" (list "System.Half")
                  (lambda (self h) (declare (ignore self)) (/ h 2)))
            (list "Fact" "System.Numerics.BigInteger" (list "System.Int32")
                  (lambda (self n) (declare (ignore self))
                    (let ((r 1)) (dotimes (i n r) (setf r (* r (1+ i)))))))))
    (let ((o (dotnet:new "DotclTest.WideNum")))
      (list (dotnet:invoke o "Doubled" 1267650600228229401496703205376)
            (dotnet:invoke o "Halved" 3.0)
            (dotnet:invoke o "Fact" 25))))
  (2535301200456458802993406410752 1.5 15511210043330985984000000))
