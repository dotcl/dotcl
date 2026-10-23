;;; A library no unit owns, required from inside units: loaded once, shared.

(defpackage "PU-COMMON" (:use "CL")
  (:export "*LOADS*" "*COUNTER*" "BUMP" "THING" "THING-TAG" "DESCRIBE-IT"))
(in-package "PU-COMMON")

(defvar *loads* 0)
(incf *loads*)
(defvar *counter* 0)
(defun bump () (incf *counter*))
(defclass thing () ((tag :initarg :tag :reader thing-tag)))
(defgeneric describe-it (x))
(defmethod describe-it ((x thing)) (list :thing (thing-tag x)))

(provide "PU-COMMON")
