;;; SBCL accepts NIL in these three places. A :DOCUMENTATION of NIL means no documentation in a DEFCLASS slot,
;;; a DEFGENERIC option and a DEFINE-CONDITION option (cl-patterns writes
;;; :documentation #.(documentation 'x 'function), which is NIL at read time). Other
;;; non-strings stay a PROGRAM-ERROR in dotcl (SBCL differs: a type error for the slot and
;;; define-condition cases, and it accepts any value in a DEFGENERIC option).

(defun %dna (form) (handler-case (progn (eval form) :ok) (program-error () :program-error)))

(deftest documentation-nil-accepted.nil
  (list (%dna '(defclass dna-c1 () ((a :documentation nil))))
        (%dna '(defgeneric dna-g1 (a) (:documentation nil)))
        (%dna '(define-condition dna-e1 (error) () (:documentation nil)))
        (documentation 'dna-g1 'function))
  (:ok :ok :ok nil))

(deftest documentation-nil-accepted.non-string
  (list (%dna '(defclass dna-c2 () ((a :documentation 5))))
        (%dna '(defgeneric dna-g2 (a) (:documentation 5)))
        (%dna '(define-condition dna-e2 (error) () (:documentation 5))))
  (:program-error :program-error :program-error))
