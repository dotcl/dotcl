(defpackage :puadd-x
  (:use :cl :puadd-x/util)
  (:nicknames #:puadd-xn "PUADD-X-TWO")
  (:export #:version #:scratch-package))
(in-package :puadd-x)
(defun version () (twice "2"))
(defun scratch-package ()
  (or (find-package "PUADD-X-SCRATCH") (make-package "PUADD-X-SCRATCH" :use '())))
(provide :puadd-x-module)
