;;; DEFPARAMETER / DEFVAR of a name that is a constant variable, DEFINE-SYMBOL-MACRO
;;; of a constant or special variable, and DEFCONSTANT of a keyword to some other
;;; value are program errors. The nearby valid forms keep working: DEFCONSTANT
;;; again to the same value, DEFVAR / DEFPARAMETER of an ordinary name, a symbol
;;; macro over an unused name, DEFCONSTANT of a keyword to itself.

(defconstant soc-c 1)
(defvar soc-v 10)

(defmacro soc-errors-p (form)
  `(handler-case (progn ,form nil) (error () t)))

(deftest-emitting-only special-over-constant.defparameter-constant
  (list (soc-errors-p (eval '(defparameter soc-c 11))) soc-c (eval 'soc-c))
  (t 1 1))
(deftest-emitting-only special-over-constant.defvar-constant
  (list (soc-errors-p (eval '(defvar soc-c 12))) (soc-errors-p (eval '(defvar soc-c))) soc-c)
  (t t 1))
(deftest-emitting-only special-over-constant.symbol-macro-constant
  (soc-errors-p (eval '(define-symbol-macro soc-c 2)))
  t)
(deftest-emitting-only special-over-constant.symbol-macro-special
  (list (soc-errors-p (eval '(define-symbol-macro soc-v 2))) soc-v)
  (t 10))
(deftest-emitting-only special-over-constant.keyword-other-value
  (list (soc-errors-p (eval '(defconstant :soc-kk 1)))
        (soc-errors-p (eval '(defvar :soc-kk 1)))
        (soc-errors-p (eval '(defparameter :soc-kk 1))))
  (t t t))

;; valid neighbours
(deftest-emitting-only special-over-constant.same-value-constant
  (list (eval '(defconstant soc-c 1)) (eval '(defconstant soc-d (+ 1 2))) (eval '(defconstant soc-d 3)) soc-d)
  (soc-c soc-d soc-d 3))
(deftest-emitting-only special-over-constant.keyword-itself
  (eval '(defconstant :soc-self :soc-self))
  :soc-self)
(deftest-emitting-only special-over-constant.ordinary-variables
  (list (eval '(defvar soc-v2 1)) (eval '(defvar soc-v2 2)) soc-v2
        (eval '(defparameter soc-p 3)) (eval '(defparameter soc-p 4)) soc-p)
  (soc-v2 soc-v2 1 soc-p soc-p 4))
(deftest-emitting-only special-over-constant.symbol-macro-ordinary
  (progn (eval '(define-symbol-macro soc-sm (+ 1 2)))
         (eval '(define-symbol-macro soc-sm (+ 2 3)))
         (eval 'soc-sm))
  5)
