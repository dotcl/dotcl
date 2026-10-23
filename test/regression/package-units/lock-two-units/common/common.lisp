(defpackage :pulock2-common (:use :cl) (:export #:*loads*))
(in-package :pulock2-common)
(defvar *loads* 0)
(incf *loads*)
