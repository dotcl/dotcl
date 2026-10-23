(defpackage :pulock-b (:use :cl) (:export #:x-version #:hello))
(in-package :pulock-b)
(defun x-version () (pulock-x:version))
(defun hello () (list (pulock-x:greet "b" :polite nil) (pulock-x:farewell "b")))
