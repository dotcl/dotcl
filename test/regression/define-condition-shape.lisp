;;; DEFINE-CONDITION with a malformed name, parent list, slot specification or option
;;; signals a PROGRAM-ERROR; the valid shapes still work.

(defmacro %dcn-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest define-condition-shape.invalid
  (list (%dcn-pe (define-condition %dcn-a1))
        (%dcn-pe (define-condition %dcn-a2 ()))
        (%dcn-pe (define-condition %dcn-a3 (error)))
        (%dcn-pe (define-condition %dcn-a4 1 ()))
        (%dcn-pe (define-condition %dcn-a5 (error) ((s :initarg))))
        (%dcn-pe (define-condition %dcn-a6 (error) (1)))
        (%dcn-pe (define-condition %dcn-a7 (error) () (:report)))
        (%dcn-pe (define-condition %dcn-a8 (error) () (:report "a" "b")))
        (%dcn-pe (define-condition %dcn-a9 (error) () (:report "a") (:report "b")))
        (%dcn-pe (define-condition %dcn-a10 (error) () (:default-initargs :a)))
        (%dcn-pe (define-condition %dcn-a11 (error) () (:documentation)))
        (%dcn-pe (define-condition %dcn-a12 (error) () 1)))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error))

(deftest define-condition-shape.valid
  (list (progn (eval '(define-condition %dcn-b1 (error) ())) t)
        (progn (eval '(define-condition %dcn-b2 (%dcn-b1) ((s :initarg :s :reader %dcn-b2-s))
                       (:report "b2") (:documentation "d") (:default-initargs :s 1)))
               (%dcn-b2-s (make-condition '%dcn-b2)))
        (progn (eval '(define-condition %dcn-b3 (error) () (:report (lambda (c s) (declare (ignore c)) (write-string "x" s)))))
               (princ-to-string (make-condition '%dcn-b3))))
  (t 1 "x"))
