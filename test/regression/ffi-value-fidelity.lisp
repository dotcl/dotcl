;;; Argument conversion across the FFI boundary, with C as the oracle.
;;;
;;; A ratio or a bignum could not be passed where a :float or :double was
;;; wanted, although both are ordinary results of Lisp arithmetic ((/ 1 3),
;;; (expt 2 40)). The caller had to coerce by hand.
;;;
;;; These go through sprintf, so they sit with the other variadic tests behind
;;; the same platform guard (see ffi-varargs.lisp and the loader). The memory
;;; round trips that need no C call live in ffi-memory-fidelity.lisp, which runs
;;; everywhere.

(defvar *fvf-libc*
  #+windows "msvcrt"
  #+darwin "libc"
  #-(or windows darwin) "libc.so.6")

(defun %fvf-cstring (p)
  (with-output-to-string (s)
    (loop for i from 0 below 256
          for b = (dotnet:mem-read :uint8 p i)
          until (zerop b) do (write-char (code-char b) s))))

;;; --- argument conversion, checked through C's own formatter ---

(deftest ffi-value-fidelity.ratio-and-bignum-as-float-arguments
  (let ((b (dotnet:alloc-mem 128)))
    (unwind-protect
         (list (progn (dotnet:ffi *fvf-libc* "sprintf"
                                  :args '(:pointer :string :varargs :double) :ret :int
                                  b "%.6f" 1/3)
                      (%fvf-cstring b))
               (progn (dotnet:ffi *fvf-libc* "sprintf"
                                  :args '(:pointer :string :varargs :double) :ret :int
                                  b "%.1f" (expt 2 40))
                      (%fvf-cstring b)))
      (dotnet:free-mem b)))
  ("0.333333" "1099511627776.0"))

;;; What already worked and must keep working: each integer width at its
;;; extremes. (A double printed by C shows at most 17 significant digits on some
;;; platforms, so the values above stay well inside that.)
(deftest ffi-value-fidelity.integer-extremes
  (let ((b (dotnet:alloc-mem 128)))
    (unwind-protect
         (flet ((pr (type fmt v)
                  (dotnet:ffi *fvf-libc* "sprintf"
                              :args (list :pointer :string :varargs type) :ret :int b fmt v)
                  (%fvf-cstring b)))
           (list (pr :int "%d" -2147483648)
                 (pr :uint "%u" 4294967295)
                 (pr :int64 "%lld" 9223372036854775807)
                 (pr :int64 "%lld" -9223372036854775808)
                 (pr :uint64 "%llu" 18446744073709551615)))
      (dotnet:free-mem b)))
  ("-2147483648" "4294967295" "9223372036854775807" "-9223372036854775808"
   "18446744073709551615"))
