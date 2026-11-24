;;; COERCE of a dotted list to a vector or string is a TYPE-ERROR; a proper list still converts
;;; and a dotted list stays a LIST when that is the type asked for.

(defun %cil-class (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest coerce-improper-list.dotted
  (list (%cil-class (lambda () (coerce (cons #\a #\b) 'string)))
        (%cil-class (lambda () (coerce (cons 1 2) 'vector)))
        (%cil-class (lambda () (coerce (list* 1 2 3) 'simple-vector)))
        (%cil-class (lambda () (coerce (list* #\a #\b #\c) '(vector character)))))
  (:type-error :type-error :type-error :type-error))

(deftest coerce-improper-list.valid
  (list (coerce '(#\a #\b) 'string) (coerce (coerce '(1 2) 'vector) 'list) (length (coerce nil 'vector))
        (coerce (cons 1 2) 'list) (coerce (cons 1 2) t))
  ("ab" (1 2) 0 (1 . 2) (1 . 2)))
