(defpackage :rapp (:use :cl) (:export #:run))
(in-package :rapp)
(defun run ()
  (list (ra:x-version) (rd:x-version) (rb:x-version) (rb:bye)))
