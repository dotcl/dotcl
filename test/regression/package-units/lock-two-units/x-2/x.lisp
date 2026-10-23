(defpackage :pulock2-x (:use :cl) (:export #:version #:farewell))
(in-package :pulock2-x)
(defun version () "2")
(defun farewell (who) (list :bye who))
