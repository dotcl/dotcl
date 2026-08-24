;;; :uint64 must cover the full 0 .. 2^64-1 range in both directions.
;;;
;;; Above 2^63-1 the value is a Bignum, and every path assumed Fixnum: writing
;;; threw a cast exception, the FFI argument converter refused the value, and
;;; reading came back through a signed Int64 so 0xFFFFFFFFFFFFFFFF read as -1.
;;; C's unsigned long long is exactly that range, so 6 of cffi's tests failed.

(defun %u64-roundtrip (v)
  (let ((p (dotnet:alloc-mem 8)))
    (unwind-protect
         (progn (dotnet:mem-write v :uint64 p 0)
                (dotnet:mem-read :uint64 p 0))
      (dotnet:free-mem p))))

(deftest ffi-uint64.roundtrip-high
  (list (%u64-roundtrip 18446744073709551615)   ; 2^64-1
        (%u64-roundtrip 9223372036854775808)    ; 2^63
        (%u64-roundtrip 9223372036854775807)    ; 2^63-1, still a fixnum
        (%u64-roundtrip 0))
  (18446744073709551615 9223372036854775808 9223372036854775807 0))

;;; Reads are always non-negative, never a sign-extended Int64.
(deftest ffi-uint64.read-is-unsigned
  (minusp (%u64-roundtrip 18446744073709551615))
  nil)

;;; :int64 keeps its signed meaning at the same addresses.
(deftest ffi-uint64.int64-still-signed
  (let ((p (dotnet:alloc-mem 8)))
    (unwind-protect
         (progn (dotnet:mem-write -1 :int64 p 0)
                (list (dotnet:mem-read :int64 p 0)
                      (dotnet:mem-read :uint64 p 0)))
      (dotnet:free-mem p)))
  (-1 18446744073709551615))

;;; Out of range is an error, not a silent wrap.
(deftest ffi-uint64.out-of-range-signals
  (let ((p (dotnet:alloc-mem 8)))
    (unwind-protect
         (list (handler-case (progn (dotnet:mem-write 18446744073709551616 :uint64 p 0) nil)
                 (error () t))
               (handler-case (progn (dotnet:mem-write -1 :uint64 p 0) nil)
                 (error () t)))
      (dotnet:free-mem p)))
  (t t))
