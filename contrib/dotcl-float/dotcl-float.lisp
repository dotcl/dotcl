;;; dotcl-float.lisp: IEEE float bit<->value primitives backed by
;;; System.BitConverter via dotnet:static. No C# helper DLL; System.BitConverter
;;; is in System.Runtime and always resolvable.
;;;
;;; Usage: (require "dotcl-float")
;;;   dotcl-float:single-float-bits, double-float-bits,
;;;   bits-single-float, bits-double-float,
;;;   dotcl-float:+single-float-positive-infinity+, dotcl-float:+single-float-nan+,
;;;   dotcl-float:+double-float-positive-infinity+, dotcl-float:+double-float-nan+,
;;;   dotcl-float:float-nan-p, dotcl-float:float-infinity-p

(defpackage :dotcl-float
  (:use :cl)
  (:export #:single-float-bits #:double-float-bits
           #:bits-single-float #:bits-double-float
           #:+single-float-positive-infinity+ #:+single-float-negative-infinity+
           #:+single-float-nan+
           #:+double-float-positive-infinity+ #:+double-float-negative-infinity+
           #:+double-float-nan+
           #:float-nan-p #:float-infinity-p))

(in-package :dotcl-float)

(declaim (inline single-float-bits double-float-bits
                 bits-single-float bits-double-float))

(defun single-float-bits (float)
  (ldb (byte 32 0)
       (dotnet:static "System.BitConverter" "SingleToInt32Bits" float)))

(defun double-float-bits (float)
  (ldb (byte 64 0)
       (dotnet:static "System.BitConverter" "DoubleToInt64Bits" float)))

;; Both take a SIGNED .NET integer, and (ldb (byte N 0) ...) is always
;; non-negative. Marshalling reinterprets an unsigned N-bit pattern for a signed
;; target, so the pattern goes straight in; and a .NET float arrives as a
;; single-float, so nothing needs coercing on the way out.
(defun bits-single-float (bits)
  (dotnet:static "System.BitConverter" "Int32BitsToSingle"
                 (ldb (byte 32 0) bits)))

(defun bits-double-float (bits)
  (dotnet:static "System.BitConverter" "Int64BitsToDouble"
                 (ldb (byte 64 0) bits)))

;; As sb-ext:float-nan-p and sb-ext:float-infinity-p: read the exponent and
;; the fraction from the bits, so no comparison (which a NaN never satisfies)
;; and no float trap is involved.
(defun float-nan-p (x)
  "True if the float X is a NaN, quiet or signalling."
  (etypecase x
    (double-float (let ((bits (double-float-bits x)))
                    (and (= (ldb (byte 11 52) bits) #x7FF)
                         (/= 0 (ldb (byte 52 0) bits)))))
    (single-float (let ((bits (single-float-bits x)))
                    (and (= (ldb (byte 8 23) bits) #xFF)
                         (/= 0 (ldb (byte 23 0) bits)))))))

(defun float-infinity-p (x)
  "True if the float X is positive or negative infinity."
  (etypecase x
    (double-float (let ((bits (double-float-bits x)))
                    (and (= (ldb (byte 11 52) bits) #x7FF)
                         (= 0 (ldb (byte 52 0) bits)))))
    (single-float (let ((bits (single-float-bits x)))
                    (and (= (ldb (byte 8 23) bits) #xFF)
                         (= 0 (ldb (byte 23 0) bits)))))))

(defconstant +single-float-positive-infinity+ (bits-single-float #x7F800000))
(defconstant +single-float-negative-infinity+ (bits-single-float #xFF800000))
(defconstant +single-float-nan+               (bits-single-float #x7FC00000))
(defconstant +double-float-positive-infinity+ (bits-double-float #x7FF0000000000000))
(defconstant +double-float-negative-infinity+ (bits-double-float #xFFF0000000000000))
(defconstant +double-float-nan+               (bits-double-float #x7FF8000000000000))

(provide "dotcl-float")
