(defpackage :pulock2-app (:use :cl) (:export #:run))
(in-package :pulock2-app)
(defun run () (list (pulock2-a:x-version) (pulock2-b:x-version)))
