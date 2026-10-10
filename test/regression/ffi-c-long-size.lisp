;;; C `long' and `unsigned long' have the size of the running OS's C ABI: 4
;;; bytes on Windows (LLP64, also on 64-bit), 8 bytes on 64-bit Linux and macOS
;;; (LP64). They used to be 4 bytes everywhere, so on Linux and macOS a struct
;;; with a long or a size_t spelled :unsigned-long (cl-libyaml's yaml_parser_t)
;;; was allocated too small and the C library wrote past its end, and a long
;;; above 2^32 did not survive a memory round trip.

(deftest ffi-c-long-size.type-size-and-alignment
  (list (dotnet:type-size :long) (dotnet:type-size :unsigned-long)
        (dotnet:type-align :long) (dotnet:type-align :unsigned-long))
  #+windows (4 4 4 4)
  #-windows (8 8 8 8))

;;; Writing a :long over an all-ones int64 shows how many bytes it covered.
(deftest ffi-c-long-size.write-covers-the-c-width
  (let ((p (dotnet:alloc-mem 16)))
    (unwind-protect
         (progn (dotnet:mem-write -1 :int64 p 0)
                (dotnet:mem-write 5 :long p 0)
                (dotnet:mem-read :int64 p 0))
      (dotnet:free-mem p)))
  #+windows -4294967291
  #-windows 5)

(deftest ffi-c-long-size.round-trip-extremes
  (let ((p (dotnet:alloc-mem 16)))
    (unwind-protect
         (mapcar (lambda (spec)
                   (dotnet:mem-write (second spec) (first spec) p 0)
                   (dotnet:mem-read (first spec) p 0))
                 #+windows '((:long -2147483648) (:long 2147483647)
                             (:unsigned-long 4294967295))
                 #-windows '((:long -9223372036854775808) (:long 1099511627776)
                             (:unsigned-long 18446744073709551615)))
      (dotnet:free-mem p)))
  #+windows (-2147483648 2147483647 4294967295)
  #-windows (-9223372036854775808 1099511627776 18446744073709551615))

;;; The call path: labs takes and returns a C long.
#-windows
(deftest ffi-c-long-size.labs-takes-and-returns-a-c-long
  (dotnet:%ffi-call-ptr (dotnet:find-symbol-any "labs") '(:long) :long
                        (- (expt 2 40)))
  1099511627776)

#+windows
(deftest ffi-c-long-size.labs-takes-and-returns-a-c-long
  (dotnet:ffi "msvcrt" "labs" :args '(:long) :ret :long -42)
  42)
