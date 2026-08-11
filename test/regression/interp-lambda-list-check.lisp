;;; Interpreted closures must reject calls that do not match their lambda list.
;;;
;;; %mini-bind-params bound whatever it was given: a missing required argument
;;; silently became NIL, an extra one was dropped, and an unknown :KEY was
;;; ignored. Compiled functions have always checked, so this only showed up
;;; where a function is INTERPRETED — and the biggest such source is a DEFSTRUCT
;;; created through EVAL, whose accessors then are interpreted closures:
;;;
;;;   (defstruct sa a b)          ; LOAD compiles it   -> (sa-p) signals
;;;   (eval '(defstruct sb a b))  ; interpreted        -> (sb-p) returned NIL
;;;
;;; ansi-test's DEFSTRUCT-WITH-TESTS uses exactly that EVAL form, which is why
;;; this one gap accounted for all 370 failures of the structures category
;;; under :interpret (CLHS 3.5.1.2 / 3.5.1.4 / 3.5.1.6).
;;;
;;; Each case asserts both evaluator paths by binding dotcl:*evaluator-mode*
;;; around the EVAL, so this runs under the ordinary compiled harness.

(defun %illc (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (progn (eval form) :no-error)
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

;;; --- required arguments

(deftest interp-lambda-list-check.too-few-compile
  (%illc :compile '(funcall (lambda (a b) (list a b)) 1))
  :program-error)

(deftest interp-lambda-list-check.too-few-interpret
  (%illc :interpret '(funcall (lambda (a b) (list a b)) 1))
  :program-error)

(deftest interp-lambda-list-check.too-many-interpret
  (%illc :interpret '(funcall (lambda (a) a) 1 2))
  :program-error)

;;; The ansi-test shape: a DEFSTRUCT built by EVAL has interpreted accessors.
(deftest interp-lambda-list-check.eval-defstruct-predicate-interpret
  (%illc :interpret '(progn (eval '(defstruct illc-s a b)) (illc-s-p)))
  :program-error)

(deftest interp-lambda-list-check.eval-defstruct-accessor-interpret
  (%illc :interpret '(progn (eval '(defstruct illc-s2 a b))
                            (illc-s2-a (make-illc-s2) nil)))
  :program-error)

;;; --- &key

(deftest interp-lambda-list-check.unknown-key-interpret
  (%illc :interpret '(funcall (lambda (&key a) a) :b 1))
  :program-error)

(deftest interp-lambda-list-check.odd-keys-interpret
  (%illc :interpret '(funcall (lambda (&key a) a) :a))
  :program-error)

;;; &allow-other-keys in the lambda list, and :ALLOW-OTHER-KEYS T in the call,
;;; both permit an unknown key (CLHS 3.5.1.4).
(deftest interp-lambda-list-check.allow-other-keys-declared-interpret
  (%illc :interpret '(funcall (lambda (&key a &allow-other-keys) a) :b 1))
  :no-error)

(deftest interp-lambda-list-check.allow-other-keys-in-call-interpret
  (%illc :interpret '(funcall (lambda (&key a) a) :b 1 :allow-other-keys t))
  :no-error)

;;; --- shapes that must NOT be rejected (guarding against false positives)

(deftest interp-lambda-list-check.optional-omitted-interpret
  (%illc :interpret '(funcall (lambda (a &optional b) (list a b)) 1))
  :no-error)

(deftest interp-lambda-list-check.rest-absorbs-interpret
  (%illc :interpret '(funcall (lambda (a &rest r) (list a r)) 1 2 3 4))
  :no-error)

(deftest interp-lambda-list-check.aux-consumes-nothing-interpret
  (%illc :interpret '(funcall (lambda (a &aux (b 2)) (list a b)) 1))
  :no-error)

(deftest interp-lambda-list-check.key-with-rest-interpret
  (%illc :interpret '(funcall (lambda (a &rest r &key b) (list a r b)) 1 :b 2))
  :no-error)

;;; Values still bind correctly — the check must not disturb the binding itself.
(deftest interp-lambda-list-check.binding-still-correct-interpret
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(funcall (lambda (a &optional (b 9) &key (c 7)) (list a b c)) 1)))
  (1 9 7))
