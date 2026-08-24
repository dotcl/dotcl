;;; Values keep their type and their magnitude across a memory round trip.
;;;
;;; Split out of ffi-value-fidelity.lisp, whose remaining tests call sprintf
;;; variadically and are therefore skipped wherever the variadic ABI is not
;;; covered yet (see ffi-varargs.lisp). These need no C call at all, so the same
;;; guard would only cost coverage: they are the tests that pin :uint64 spanning
;;; the full 0..2^64-1 range and :float reading back as a SINGLE-FLOAT, both of
;;; which are platform-independent.
;;;
;;;   - (mem-read :float p) returned a DOUBLE-FLOAT, so writing 2.5 and reading
;;;     it back changed the type of the value. The call path already returned a
;;;     single for a :float result, so the two disagreed with each other.

(deftest ffi-value-fidelity.float-round-trip-keeps-its-type
  (let ((p (dotnet:alloc-mem 32)))
    (unwind-protect
         (progn (dotnet:mem-write 2.5 :float p 0)
                (list (dotnet:mem-read :float p 0)
                      (type-of (dotnet:mem-read :float p 0))))
      (dotnet:free-mem p)))
  (2.5 single-float))

(deftest ffi-value-fidelity.double-round-trip-keeps-its-type
  (let ((p (dotnet:alloc-mem 32)))
    (unwind-protect
         (progn (dotnet:mem-write 1.25d0 :double p 0)
                (list (dotnet:mem-read :double p 0)
                      (type-of (dotnet:mem-read :double p 0))))
      (dotnet:free-mem p)))
  (1.25d0 double-float))

(deftest ffi-value-fidelity.memory-extremes
  (let ((p (dotnet:alloc-mem 64)))
    (unwind-protect
         (mapcar (lambda (spec)
                   (dotnet:mem-write (second spec) (first spec) p 0)
                   (dotnet:mem-read (first spec) p 0))
                 '((:int8 -128) (:uint8 255) (:int16 -32768) (:uint16 65535)
                   (:int32 -2147483648) (:uint32 4294967295)
                   (:int64 -9223372036854775808) (:uint64 18446744073709551615)))
      (dotnet:free-mem p)))
  (-128 255 -32768 65535 -2147483648 4294967295
   -9223372036854775808 18446744073709551615))
