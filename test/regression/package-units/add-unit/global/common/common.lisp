(defpackage :puadd-common (:use :cl) (:export #:*loads*))
(in-package :puadd-common)
(defvar *loads* 0)
(incf *loads*)
