(defpackage :rb (:use :cl) (:export #:x-version #:bye))
(in-package :rb)
(defun x-version () (rx:version))
(defun bye () (rx:farewell "rb"))
