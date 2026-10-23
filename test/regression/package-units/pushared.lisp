;;; A library no unit owns: loaded once, shared by every unit.

(defpackage "PUSHARED" (:use "CL") (:export "*COUNTER*" "BUMP" "THING" "THING-TAG" "DESCRIBE-IT"))
(in-package "PUSHARED")

(defvar *loads* 0)
(incf *loads*)
(defvar *counter* 0)
(defun bump () (incf *counter*))
(defclass thing () ((tag :initarg :tag :reader thing-tag)))
(defgeneric describe-it (x))
(defmethod describe-it ((x thing)) (list :thing (thing-tag x)))

(provide "PUSHARED-TEST-MODULE")
