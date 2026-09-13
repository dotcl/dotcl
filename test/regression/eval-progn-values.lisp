;;; EVAL of a PROGN whose non-final form returns no values.
;;;
;;; PROGN returns the values of its last form and discards the rest, and
;;; discarding has to include the thread value state. A subform that published NO
;;; values left the count at zero, and a last form that publishes nothing of its
;;; own -- a constant, a variable reference -- inherited it, so
;;; (eval '(progn (values) x)) answered no values instead of X.
;;;
;;; Reached from anything that evaluates rather than compiles: the REPL, LOAD of a
;;; source file, a test harness that EVALs its forms. It was found through the
;;; cffi suite, where a :VOID foreign call is the form that publishes nothing and
;;; (progn (some-void-call) *result*) is how a test reads what the call did.

(defvar *epv-x* 1984)
(defun %epv-zero () (values))
(defun %epv-two () (values 7 8))

;;; The defect: a form publishing no values, then a form publishing none of its
;;; own. The value of the last form is the value of the PROGN.

(deftest eval-progn-values.zero-then-variable
  (multiple-value-list (eval '(progn (%epv-zero) *epv-x*)))
  (1984))

(deftest eval-progn-values.zero-then-constant
  (multiple-value-list (eval '(progn (%epv-zero) 42)))
  (42))

(deftest eval-progn-values.several-zeros
  (multiple-value-list (eval '(progn (%epv-zero) (%epv-zero) 42)))
  (42))

(deftest eval-progn-values.literal-values-form
  (multiple-value-list (eval '(progn (values) 42)))
  (42))

(deftest eval-progn-values.nested-progn
  (multiple-value-list (eval '(progn (progn (%epv-zero)) 42)))
  (42))

;;; What must NOT change: the last form's own values still ride along, including
;;; when the last form is the one that returns none.

(deftest eval-progn-values.last-form-multiple
  (list (multiple-value-list (eval '(progn 1 (%epv-two))))
        (multiple-value-list (eval '(progn (%epv-zero) (values 1 2)))))
  ((7 8) (1 2)))

(deftest eval-progn-values.last-form-zero
  (list (multiple-value-list (eval '(progn 1 (%epv-zero))))
        (multiple-value-list (eval '(progn (values 1 2) (values)))))
  (nil nil))

(deftest eval-progn-values.empty-and-single
  (list (multiple-value-list (eval '(progn)))
        (multiple-value-list (eval '(progn 42)))
        (multiple-value-list (eval '(progn (%epv-two)))))
  ((nil) (42) (7 8)))

;;; The other readers of the value state agree with MULTIPLE-VALUE-LIST.
(deftest eval-progn-values.other-readers
  (list (eval '(progn (%epv-zero) *epv-x*))
        (eval '(nth-value 0 (progn (%epv-zero) 42)))
        (eval '(multiple-value-bind (a b) (progn (%epv-zero) 42) (list a b)))
        (eval '(multiple-value-call #'list (progn (%epv-zero) 42))))
  (1984 42 (42 nil) (42)))
