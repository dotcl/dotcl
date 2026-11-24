;;; DESTRUCTURING-BIND with a malformed destructuring lambda list signals a
;;; PROGRAM-ERROR; the valid shapes still work.

(defmacro %dbp-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest destructuring-bind-pattern-shape.invalid
  (list (%dbp-pe (destructuring-bind a '(1) a))
        (%dbp-pe (destructuring-bind (a &rest) '(1) a))
        (%dbp-pe (destructuring-bind (a &rest b c) '(1) a))
        (%dbp-pe (destructuring-bind (&key &optional a) '(1) a))
        (%dbp-pe (destructuring-bind (&whole a &whole b) '(1) a))
        (%dbp-pe (destructuring-bind (a &environment e) '(1) a))
        (%dbp-pe (destructuring-bind (&optional (a 1 2 3)) '() a))
        (%dbp-pe (destructuring-bind (&key (a 1 2 3)) '() a))
        (%dbp-pe (destructuring-bind (&aux (a 1 2)) '() a))
        (%dbp-pe (destructuring-bind (&whole) '(1) 1))
        (%dbp-pe (destructuring-bind (1) '(1) 1))
        (%dbp-pe (destructuring-bind (a &bogus) '(1) a))
        (%dbp-pe (destructuring-bind (&allow-other-keys) '() 1))
        (%dbp-pe (destructuring-bind (a &rest b c) '(1) a)))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error))

(deftest destructuring-bind-pattern-shape.valid
  (list (destructuring-bind (a (b c) &optional (d 4 dp) &rest r) '(1 (2 3)) (list a b c d dp r))
        (destructuring-bind (&whole w a . b) '(1 2 3) (list w a b))
        (destructuring-bind (a &key ((:k kk) 5 kp) (z 6) &allow-other-keys) '(1 :q 2) (list a kk kp z))
        (destructuring-bind (&optional ((x y) '(7 8))) '() (list x y))
        (destructuring-bind (a &body b &aux (c (* a 2))) '(1 2 3) (list a b c))
        (destructuring-bind () '() 9))
  ((1 2 3 4 nil nil) ((1 2 3) 1 (2 3)) (1 5 nil 6) (7 8) (1 (2 3) 2) 9))
