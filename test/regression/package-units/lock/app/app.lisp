(defpackage :pulock-app (:use :cl) (:export #:run))
(in-package :pulock-app)
(defun run ()
  (list (pulock-a:x-version) (pulock-b:x-version) (pulock-a:hello) (pulock-b:hello)))
