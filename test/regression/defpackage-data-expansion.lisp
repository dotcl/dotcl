;;; DEFPACKAGE expands to one call with the clauses as data, not to code per
;;; clause and per symbol. A package with a thousand exports made a method of
;;; hundreds of kilobytes of IL, run once and JITted in full each time the form
;;; was evaluated (closer-mop's C2CL).

(deftest defpackage-data-expansion.one-call
  (let ((exp (macroexpand-1
              `(defpackage :dpde-big (:use :cl)
                 (:export ,@(loop for i below 300 collect (format nil "S~d" i)))))))
    (list (length exp) (eq (car (second exp)) 'quote)))
  (2 t))

(deftest defpackage-data-expansion.clauses
  (progn
    (defpackage :dpde-src (:use :cl) (:export #:a #:b))
    (defpackage :dpde-a
      (:use :cl) (:nicknames :dpde-a-nick)
      (:shadow #:car)
      (:shadowing-import-from :dpde-src #:b)
      (:import-from :dpde-src #:a)
      (:intern "I")
      (:export #:e)
      (:documentation "doc"))
    (list (package-nicknames :dpde-a)
          (list (eq (find-symbol "CAR" :dpde-a) 'car)
                (nth-value 1 (find-symbol "CAR" :dpde-a)))
          (eq (find-symbol "A" :dpde-a) (find-symbol "A" :dpde-src))
          (eq (find-symbol "B" :dpde-a) (find-symbol "B" :dpde-src))
          (not (null (member (find-symbol "B" :dpde-a) (package-shadowing-symbols :dpde-a))))
          (nth-value 1 (find-symbol "I" :dpde-a))
          (nth-value 1 (find-symbol "E" :dpde-a))
          (documentation (find-package :dpde-a) t)))
  (("DPDE-A-NICK") (nil :internal) t t t :internal :external "doc"))

;; A missing symbol offers CONTINUE, which skips just that symbol.
(deftest defpackage-data-expansion.skip-missing-symbol
  (progn
    (defpackage :dpde-src2 (:use :cl) (:export #:here))
    (handler-bind ((package-error (lambda (c) (declare (ignore c))
                                    (invoke-restart 'continue))))
      (defpackage :dpde-b (:use :cl) (:import-from :dpde-src2 #:missing #:here)))
    (list (eq (find-symbol "HERE" :dpde-b) (find-symbol "HERE" :dpde-src2))
          (nth-value 1 (find-symbol "MISSING" :dpde-b))))
  (t nil))
