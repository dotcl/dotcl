(defpackage :ra (:use :cl) (:export #:x-version))
(in-package :ra)
(defun x-version () (rx:version))
