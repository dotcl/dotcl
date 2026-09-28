;;; A :pointer value accepts both spellings of the same 64 bits.
;;;
;;; A pointer has no sign, so #xFFFFFFFFFFFFFFFF and -1 name the same address.
;;; The signed spelling is a fixnum and always worked; the unsigned one is a
;;; bignum above 2^63-1 and was refused with "cannot convert ... to :ptr". That
;;; is the shape portable code produces: cl-sqlite builds SQLITE_TRANSIENT as
;;; (make-pointer (mod -1 (expt 2 (* 8 pointer-size)))), so every bound
;;; parameter failed. The accepted range is the union of signed and unsigned
;;; 64-bit; anything outside it is still an error, not a silent truncation.

(defun %ptr-write-read (v)
  (let ((p (dotnet:alloc-mem 8)))
    (unwind-protect
         (progn (dotnet:mem-write v :pointer p 0)
                (dotnet:mem-read :uint64 p 0))
      (dotnet:free-mem p))))

(deftest ffi-pointer-range.mem-write-both-spellings
  (list (%ptr-write-read (mod -1 (expt 2 64)))
        (%ptr-write-read -1)
        (%ptr-write-read (expt 2 63))
        (%ptr-write-read (- (expt 2 63)))
        (%ptr-write-read 0))
  (18446744073709551615 18446744073709551615
   9223372036854775808 9223372036854775808 0))

(deftest ffi-pointer-range.mem-write-out-of-range-signals
  (let ((p (dotnet:alloc-mem 8)))
    (unwind-protect
         (list (handler-case (progn (dotnet:mem-write (expt 2 64) :pointer p 0) nil)
                 (error () t))
               (handler-case (progn (dotnet:mem-write (- (1+ (expt 2 63))) :pointer p 0) nil)
                 (error () t)))
      (dotnet:free-mem p)))
  (t t))

;;; Reading a pointer gives the unsigned spelling, so a pointer read back is
;;; = to a sentinel built portably as (mod -1 (expt 2 64)) (MAP_FAILED,
;;; SQLITE_TRANSIENT). A signed -1 made osicat's and mmap's MAP_FAILED checks
;;; miss every failure. SBCL's sap-int is unsigned too.
(defun %ptr-write-read-ptr (v)
  (let ((p (dotnet:alloc-mem 8)))
    (unwind-protect
         (progn (dotnet:mem-write v :pointer p 0)
                (dotnet:mem-read :pointer p 0))
      (dotnet:free-mem p))))

(deftest ffi-pointer-range.mem-read-unsigned-spelling
  (list (%ptr-write-read-ptr -1)
        (%ptr-write-read-ptr (mod -1 (expt 2 64)))
        (%ptr-write-read-ptr (- (expt 2 63)))
        (%ptr-write-read-ptr 4096)
        (%ptr-write-read-ptr 0)
        (= (%ptr-write-read-ptr -1) (mod -1 (expt 2 64))))
  (18446744073709551615 18446744073709551615 9223372036854775808 4096 0 t))
