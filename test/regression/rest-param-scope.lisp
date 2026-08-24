;;; Regression: the &rest parameter is bound by the lambda list, so a body
;;; reference to it is not a free variable.
;;;
;;; The free-variable analysis skipped the &rest var. Every function that used
;;; its own &rest var therefore looked like it captured something, which
;;; (a) pushed DEFUN off the :defmethod path onto runtime registration, whose
;;; arity check is emitted with an empty name, so (f) reported ": too few
;;; arguments" with nothing in front of the colon, and (b) made a bare
;;; (lambda (&rest r) r) build an env array capturing an outer R that the
;;; parameter then shadows.

(defun %rp-arity-message (fn)
  "Call FN with no arguments and return the report of the resulting error."
  (handler-case (progn (funcall fn) :no-error)
    (error (e) (princ-to-string e))))

(defun %rp-names-p (fn name)
  "T when the arity error report starts with NAME — the point of the message is
   to say which call was wrong."
  (let ((msg (%rp-arity-message fn)))
    (and (stringp msg)
         (>= (length msg) (length name))
         (string= name msg :end2 (length name))
         t)))

(defun rp-required (x) x)
(defun rp-optional (x &optional y) (list x y))
(defun rp-key (x &key a) (list x a))
(defun rp-rest (x &rest r) (cons x r))
(defun rp-rest-only (&rest r) r)
(defun rp-optional-rest (x &optional y &rest r) (list x y r))

;;; The &rest cases are the regression; the others record that they were
;;; already right, so a future change cannot move the behaviour between paths.
(deftest rest-param-scope.arity-error-names-the-function
  (list (%rp-names-p #'rp-required "RP-REQUIRED")
        (%rp-names-p #'rp-optional "RP-OPTIONAL")
        (%rp-names-p #'rp-key "RP-KEY")
        (%rp-names-p #'rp-rest "RP-REST")
        (%rp-names-p #'rp-optional-rest "RP-OPTIONAL-REST"))
  (t t t t t))

;;; A DEFUN nested in a binding form takes the runtime-registration path even
;;; when nothing is captured, so it exercises the second half of the fix.
(let ((ignored 0))
  (declare (ignorable ignored))
  (defun rp-nested-rest (x &rest r) (cons x r)))

(deftest rest-param-scope.nested-defun-arity-error-names-the-function
  (%rp-names-p #'rp-nested-rest "RP-NESTED-REST")
  t)

(deftest rest-param-scope.rest-functions-still-work
  (list (rp-rest 1 2 3)
        (rp-rest 1)
        (rp-rest-only)
        (rp-rest-only 1 2)
        (rp-optional-rest 1 2 3 4)
        (apply #'rp-rest '(1 2 3))
        (rp-nested-rest 1 2))
  ((1 2 3) (1) nil (1 2) (1 2 (3 4)) (1 2 3) (1 2)))

;;; An outer variable of the same name must not leak in through a capture: the
;;; parameter shadows it, and there is nothing to capture in the first place.
(deftest rest-param-scope.no-capture-of-same-named-outer-var
  (let ((r :outer))
    (declare (ignorable r))
    (list (funcall (lambda (&rest r) r) 1 2)
          (funcall (lambda (x &rest r) (cons x r)) 1 2)
          r))
  ((1 2) (1 2) :outer))

;;; A &rest function that really does capture must still become a closure.
(deftest rest-param-scope.real-capture-still-closes-over
  (let ((base 10))
    (funcall (lambda (&rest r) (cons base r)) 1 2))
  (10 1 2))

;;; The interpreter shares the lambda-list scoping rule.
(deftest rest-param-scope.interpreted-rest-scope
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(funcall (lambda (&rest r) r) 1 2))
          (eval '(let ((r :outer)) (declare (ignorable r))
                   (funcall (lambda (&rest r) r) 3 4)))
          (eval '(let ((base 10)) (funcall (lambda (&rest r) (cons base r)) 1)))))
  ((1 2) (3 4) (10 1)))
