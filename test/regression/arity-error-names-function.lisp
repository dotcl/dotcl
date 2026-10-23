;;; A wrong argument count names the function in the message, whether the call
;;; is a direct call to a built-in the compiler open-codes, a call to a user
;;; function, or a call through FUNCALL / APPLY.

(defclass %aen-class () ((x :initarg :x)))

(defun %aen-two (p q) (list p q))

(defun %aen-message (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (program-error (e) (princ-to-string e))))

(deftest arity-error-names-function.builtin-direct
  (let ((a (make-instance '%aen-class)))
    (list (%aen-message (lambda () (slot-boundp a 'x 'extra)))
          (%aen-message (lambda () (slot-value a 'x 'extra)))
          (%aen-message (lambda () (car a 'extra)))))
  ("SLOT-BOUNDP: wrong number of arguments: 3 (expected 2)"
   "SLOT-VALUE: wrong number of arguments: 3 (expected 2)"
   "CAR: wrong number of arguments: 2 (expected 1)"))

(deftest arity-error-names-function.builtin-funcall
  (let ((a (make-instance '%aen-class)))
    (list (%aen-message (lambda () (funcall #'slot-boundp a 'x 'extra)))
          (%aen-message (lambda () (apply 'slot-boundp (list a 'x 'extra))))))
  ("SLOT-BOUNDP: wrong number of arguments: 3 (expected 2)"
   "SLOT-BOUNDP: wrong number of arguments: 3 (expected 2)"))

(deftest arity-error-names-function.user-defun
  (list (%aen-message (lambda () (%aen-two 1 2 3)))
        (%aen-message (lambda () (funcall #'%aen-two 1)))
        (%aen-message (lambda () (apply '%aen-two '(1 2 3)))))
  ("%AEN-TWO: wrong number of arguments: 3 (expected 2)"
   "%AEN-TWO: wrong number of arguments: 1 (expected 2)"
   "%AEN-TWO: wrong number of arguments: 3 (expected 2)"))

(deftest-emitting-only arity-error-names-function.compile-nil
  (%aen-message (compile nil '(lambda () (slot-boundp (make-instance '%aen-class) 'x 'extra))))
  "SLOT-BOUNDP: wrong number of arguments: 3 (expected 2)")
