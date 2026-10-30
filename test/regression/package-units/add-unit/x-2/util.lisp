(cl:defpackage #:puadd-x/util (:use :cl) (:export #:twice))
(in-package :puadd-x/util)
(defun twice (x) (list x x))
