;;; A number or string where a place is required, a dotted variable list in
;;; MULTIPLE-VALUE-SETQ and a WITH-OPEN-FILE specification without a file name
;;; signal PROGRAM-ERROR (not a plain ERROR); the valid shapes still work.

(defmacro %aps-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest atom-place-shape.invalid
  (list (%aps-pe (setf 1 2))
        (%aps-pe (setf "a" 2))
        (%aps-pe (psetf 1 2))
        (%aps-pe (push 1 2))
        (%aps-pe (shiftf 1 2))
        (%aps-pe (rotatef 1 2))
        (%aps-pe (incf 1))
        (%aps-pe (multiple-value-setq (a . b) 1))
        (%aps-pe (with-open-file (s)))
        (%aps-pe (with-open-file s "test/regression/atom-place-shape.lisp")))
  (:program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error))

(deftest atom-place-shape.valid
  (list (let (a b) (multiple-value-setq (a b) (values 1 2)) (list a b))
        (let (a) (multiple-value-setq (a) 3) a)
        (with-open-file (s "test/regression/atom-place-shape.lisp") (and (read-line s nil nil) t)))
  ((1 2) 3 t))
