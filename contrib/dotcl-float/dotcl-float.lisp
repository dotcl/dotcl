;;; dotcl-float.lisp — IEEE float bit<->value primitives backed by
;;; System.BitConverter via dotnet:static. No C# helper DLL; System.BitConverter
;;; is in System.Runtime and always resolvable.
;;;
;;; Usage: (require "dotcl-float")
;;;   dotcl-float:single-float-bits, double-float-bits,
;;;   bits-single-float, bits-double-float,
;;;   dotcl-float:+single-float-positive-infinity+, dotcl-float:+single-float-nan+,
;;;   dotcl-float:+double-float-positive-infinity+, dotcl-float:+double-float-nan+

(defpackage :dotcl-float
  (:use :cl)
  (:export #:single-float-bits #:double-float-bits
           #:bits-single-float #:bits-double-float
           #:+single-float-positive-infinity+ #:+single-float-negative-infinity+
           #:+single-float-nan+
           #:+double-float-positive-infinity+ #:+double-float-negative-infinity+
           #:+double-float-nan+))

(in-package :dotcl-float)

(declaim (inline single-float-bits double-float-bits
                 bits-single-float bits-double-float))

(defun single-float-bits (float)
  (ldb (byte 32 0)
       (dotnet:static "System.BitConverter" "SingleToInt32Bits" float)))

(defun double-float-bits (float)
  (ldb (byte 64 0)
       (dotnet:static "System.BitConverter" "DoubleToInt64Bits" float)))

(defun bits-single-float (bits)
  ;; BitConverter.Int32BitsToSingle takes a signed int. ldb alone yields an
  ;; unsigned 32-bit pattern that is a fixnum whose (int) cast in C# reinterprets
  ;; the high bit as a sign, giving the correct bit pattern. coerce is needed
  ;; because Runtime.DotNetToLisp widens every float return to DoubleFloat.
  (coerce (dotnet:static "System.BitConverter" "Int32BitsToSingle"
                         (ldb (byte 32 0) bits))
          'single-float))

(defun bits-double-float (bits)
  ;; BitConverter.Int64BitsToDouble takes a signed long. ldb alone yields an
  ;; unsigned 64-bit pattern that is a bignum when bit 63 is set; dotnet:static
  ;; cannot marshal bignum to Int64. Convert to the signed fixnum in
  ;; [-2^63, 2^63) so it marshals to long with the same bit pattern.
  (let ((u (ldb (byte 64 0) bits)))
    (dotnet:static "System.BitConverter" "Int64BitsToDouble"
                   (if (logbitp 63 u) (- u (expt 2 64)) u))))

(defconstant +single-float-positive-infinity+ (bits-single-float #x7F800000))
(defconstant +single-float-negative-infinity+ (bits-single-float #xFF800000))
(defconstant +single-float-nan+               (bits-single-float #x7FC00000))
(defconstant +double-float-positive-infinity+ (bits-double-float #x7FF0000000000000))
(defconstant +double-float-negative-infinity+ (bits-double-float #xFFF0000000000000))
(defconstant +double-float-nan+               (bits-double-float #x7FF8000000000000))

(provide "dotcl-float")
