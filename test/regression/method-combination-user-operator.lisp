;;; The short form of DEFINE-METHOD-COMBINATION takes any function, macro or
;;; special operator as :OPERATOR, and the effective method is
;;; (operator (call-method m1) (call-method m2) ...). Only the standard
;;; operator names (+, AND, LIST, ...) used to be recognized; anything else,
;;; such as NST's (define-method-combination nst-results :operator
;;; check-result-union), failed at call time with "Unknown method combination
;;; operator".

(defun mcuo-sum-list (&rest xs) (list :sum (reduce #'+ xs) :n (length xs)))
(define-method-combination mcuo-sum :operator mcuo-sum-list)
(defgeneric mcuo-f (x) (:method-combination mcuo-sum))
(defmethod mcuo-f mcuo-sum ((x integer)) 1)
(defmethod mcuo-f mcuo-sum ((x number)) 10)

(deftest method-combination-user-function-operator
  (list (mcuo-f 5) (mcuo-f 2.5))
  ((:sum 11 :n 2) (:sum 10 :n 1)))

;; A macro operator gets the method calls unevaluated, so it decides which
;; run: here the least specific method signals if it is ever called.
(defmacro mcuo-first-true (&rest forms) `(or ,@forms))
(define-method-combination mcuo-or :operator mcuo-first-true)
(defgeneric mcuo-g (x) (:method-combination mcuo-or))
(defmethod mcuo-g mcuo-or ((x integer)) nil)
(defmethod mcuo-g mcuo-or ((x number)) :number)
(defmethod mcuo-g mcuo-or ((x t)) (error "not reached"))

(deftest method-combination-user-macro-operator
  (list (mcuo-g 5) (mcuo-g 2.5))
  (:number :number))

;; :AROUND methods still wrap the combined call.
(defmethod mcuo-f :around ((x integer)) (list :around (call-next-method)))

(deftest method-combination-user-operator-around
  (mcuo-f 7)
  (:around (:sum 11 :n 2)))
