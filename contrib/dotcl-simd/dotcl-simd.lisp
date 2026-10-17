;;; dotcl-simd.lisp: 128-bit SIMD values and operations, named as in sb-simd.
;;; The operations are C# functions installed into this package by
;;; %SIMD-INSTALL; this file only defines the package and the types.

(defpackage :dotcl-simd
  (:use :cl))

(in-package :dotcl-simd)

(dotcl::%simd-install (find-package :dotcl-simd))

(dolist (name (dotcl::%simd-type-names))
  (let ((type (find-symbol name :dotcl-simd))
        (pred (find-symbol (concatenate 'string name "P") :dotcl-simd)))
    (eval `(deftype ,type () '(satisfies ,pred)))))
