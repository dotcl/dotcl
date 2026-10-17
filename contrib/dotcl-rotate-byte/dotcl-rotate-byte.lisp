;;; dotcl-rotate-byte.lisp: ROTATE-BYTE with the interface of sb-rotate-byte.
;;; The compiler turns a call on (BYTE 32 0) or (BYTE 64 0) whose operands are
;;; known to fit into a single rotate instruction.

(defpackage :dotcl-rotate-byte
  (:use :cl)
  (:export #:rotate-byte))

(in-package :dotcl-rotate-byte)

(defun rotate-byte (count bytespec integer)
  "INTEGER with the bits in BYTESPEC rotated left by COUNT (right when COUNT
is negative) and its other bits unchanged."
  (dotcl::%rotate-byte count bytespec integer))

(provide "dotcl-rotate-byte")
