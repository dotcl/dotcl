;;; A DEFSTRUCT option other than :CONSTRUCTOR given twice is an error.

(deftest defstruct-duplicate-options.errors
  (values (handler-case (progn (eval '(defstruct (dsdo-a (:type list) (:type vector)) x)) :no-error) (error () :error))
          (handler-case (progn (eval '(defstruct (dsdo-b (:conc-name a-) (:conc-name b-)) x)) :no-error) (error () :error))
          (handler-case (progn (eval '(defstruct (dsdo-c (:copier nil) (:copier copy-c)) x)) :no-error) (error () :error))
          (handler-case (progn (eval '(defstruct (dsdo-d (:predicate p1) (:predicate p2)) x)) :no-error) (error () :error)))
  :error :error :error :error)

(deftest defstruct-duplicate-options.valid
  (values (progn (eval '(defstruct (dsdo-e (:constructor make-e1) (:constructor make-e2 (a)) (:conc-name e-)) a)) (e-a (make-e1 :a 3)))
          (progn (eval '(defstruct (dsdo-f (:type list) :named (:initial-offset 1)) a)) (length (make-dsdo-f :a 1))))
  3 3)
