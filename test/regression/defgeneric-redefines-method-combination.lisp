;;; A DEFGENERIC that redefines a generic function replaces its method combination: moving from
;;; OR or + back to the standard combination (or the other way) takes effect, the order option goes
;;; back to most-specific-first when the new form does not give one, and a form that does not name a
;;; combination means STANDARD.

(defgeneric dmc-a (x) (:method-combination or) (:method or ((x integer)) nil) (:method or ((x number)) :num))
(defgeneric dmc-a (x) (:method-combination standard) (:method ((x integer)) :std))
(deftest defgeneric-redefines-method-combination.or-to-standard
  (list (dmc-a 1) (length (generic-function-methods #'dmc-a)))
  (:std 1))

(defgeneric dmc-a (x) (:method-combination or) (:method or ((x integer)) nil) (:method or ((x number)) :num))
(deftest defgeneric-redefines-method-combination.standard-to-or
  (list (dmc-a 1) (dmc-a 2.5))
  (:num :num))

(defgeneric dmc-b (x) (:method-combination +))
(defmethod dmc-b + ((x integer)) 1)
(defmethod dmc-b + ((x number)) 10)
(deftest defgeneric-redefines-method-combination.plus
  (dmc-b 5)
  11)
;; The + methods are not valid for the standard combination (an applicable method with an invalid
;; qualifier is an error), so they go before it is put back.
(remove-method #'dmc-b (find-method #'dmc-b '(+) (list (find-class 'integer))))
(remove-method #'dmc-b (find-method #'dmc-b '(+) (list (find-class 'number))))
(defgeneric dmc-b (x))                 ; no option: the standard combination
(defmethod dmc-b ((x integer)) :std)
(deftest defgeneric-redefines-method-combination.no-option-is-standard
  (dmc-b 5)
  :std)

;; :most-specific-last, then a form without it
(defgeneric dmc-c (x) (:method-combination list :most-specific-last)
  (:method list ((x integer)) :int) (:method list ((x number)) :num))
(deftest defgeneric-redefines-method-combination.order-last
  (dmc-c 1)
  (:num :int))
(defgeneric dmc-c (x) (:method-combination list)
  (:method list ((x integer)) :int) (:method list ((x number)) :num))
(deftest defgeneric-redefines-method-combination.order-first-again
  (dmc-c 1)
  (:int :num))

;; the same option twice in a row, and a plain generic function, are unchanged
(defgeneric dmc-d (x) (:method-combination append))
(defmethod dmc-d append ((x integer)) (list 1))
(defgeneric dmc-d (x) (:method-combination append))
(deftest defgeneric-redefines-method-combination.same-combination
  (dmc-d 5)
  (1))
(defgeneric dmc-e (x))
(defmethod dmc-e ((x integer)) (list :a))
(defmethod dmc-e :around ((x integer)) (cons :around (call-next-method)))
(deftest defgeneric-redefines-method-combination.plain-unchanged
  (dmc-e 1)
  (:around :a))
