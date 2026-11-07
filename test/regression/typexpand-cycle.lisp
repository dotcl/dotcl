;;; DOTCL:TYPEXPAND on a DEFTYPE that expands to itself, or grows for ever, signals an error
;;; instead of looping; ordinary chains still expand.

(deftype %txc-self () '(%txc-self))
(deftype %txc-a () '(%txc-b))
(deftype %txc-b () '(%txc-a))
(deftype %txc-grow (&optional (x 0)) `(%txc-grow (list ,x)))
(deftype %txc-chain-1 () '%txc-chain-2)
(deftype %txc-chain-2 () 'integer)

(deftest typexpand-cycle.errors
  (list (handler-case (progn (dotcl:typexpand '%txc-self) :returned) (error () :error))
        (handler-case (progn (dotcl:typexpand '%txc-a) :returned) (error () :error))
        (handler-case (progn (dotcl:typexpand '%txc-grow) :returned) (error () :error)))
  (:error :error :error))

(deftest typexpand-cycle.ordinary-chains
  (list (multiple-value-list (dotcl:typexpand '%txc-chain-1))
        (multiple-value-list (dotcl:typexpand 'integer))
        (multiple-value-list (dotcl:typexpand '(integer 1 5)))
        (multiple-value-list (dotcl:typexpand-1 '%txc-self)))
  ((integer t) (integer nil) ((integer 1 5) nil) ((%txc-self) t)))
