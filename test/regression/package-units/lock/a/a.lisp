(defpackage :pulock-a (:use :cl) (:export #:x-version #:hello))
(in-package :pulock-a)
(defun x-version () (pulock-x:version))
(defun hello () (pulock-x:greet "a"))
