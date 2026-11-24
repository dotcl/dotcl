;;; DEFINE-CONDITION named by a COMMON-LISP symbol (error, list) used to go through
;;; far enough to break the class precedence lists: every later DEFINE-CONDITION failed
;;; with "inconsistent precedence graph". The name is refused up front now.

(deftest define-condition-cl-symbol-name.refused-without-damage
  (flet ((try (form) (handler-case (progn (eval form) :defined) (error () :error))))
    (list (try '(define-condition error (error) ()))
          (try '(define-condition list (error) ()))
          (try '(define-condition dcclsn-ok (error) ()))
          (typep (make-condition 'dcclsn-ok) 'dcclsn-ok)
          (typep (make-condition 'simple-error) 'error)))
  (:error :error :defined t t))
