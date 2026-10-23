(defpackage :rd (:use :cl) (:export #:x-version))
(in-package :rd)
(defun x-version () (rx:version))
