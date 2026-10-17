;;; COERCE of a number to the type it already has returns it, without going
;;; through the type tests; the other numeric coercions are unchanged.

(defun %cstn (x type) (coerce x type))

(deftest coerce-same-type-number.identity
  (let ((big (expt 2 80)) (sf 1.5f0) (df 2.5d0))
    (list (eql (%cstn 7 'integer) 7) (eq (%cstn big 'integer) big)
          (eq (%cstn sf 'single-float) sf) (eq (%cstn df 'double-float) df)))
  (t t t t))

(deftest coerce-same-type-number.conversions
  (list (%cstn 1 'single-float) (%cstn 1.5d0 'single-float) (%cstn 1.5f0 'double-float)
        (%cstn 3 'number) (%cstn 1/2 'float))
  (1.0f0 1.5f0 1.5d0 3 0.5f0))

(deftest coerce-same-type-number.not-an-integer
  (list (handler-case (%cstn 1.5f0 'integer) (type-error () :type-error))
        (handler-case (%cstn 1/2 'integer) (type-error () :type-error)))
  (:type-error :type-error))
