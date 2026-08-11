;;; When :ALLOW-OTHER-KEYS appears more than once, only the first one counts.
;;;
;;; CLHS 3.4.1.4.1: when a keyword is repeated, the LEFTMOST pair is the one used,
;;; and :ALLOW-OTHER-KEYS is no exception.
;;;
;;; The interpreted closure's argument check (%MINI-CHECK-ARGS) asked whether ANY
;;; :ALLOW-OTHER-KEYS pair had a true value, so
;;;   (:allow-other-keys nil :allow-other-keys t :foo t)
;;; accepted :FOO. The leftmost value is NIL, so it must be rejected
;;; (ansi-test COMPLEMENT.ERROR.6).
;;;
;;; The reverse order (T first) was already correct, so looking at only one of them
;;; makes a broken implementation look fixed. Both orders are asserted as a pair.

(defun %aok (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (list :value (eval form))
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

;;; --- leftmost NIL: an unknown key is rejected

(defparameter %aok-nil-then-t
  '(funcall (lambda (&key a) a) :allow-other-keys nil :allow-other-keys t :b 1))

(deftest interp-allow-other-keys-first.nil-then-t-compile
  (%aok :compile %aok-nil-then-t)
  :program-error)

(deftest interp-allow-other-keys-first.nil-then-t-interpret
  (%aok :interpret %aok-nil-then-t)
  :program-error)

;;; the shape ansi COMPLEMENT.ERROR.6 uses (&key-only lambda through COMPLEMENT)
(deftest interp-allow-other-keys-first.complement-interpret
  (%aok :interpret '(funcall (complement (lambda (&key) t))
                     :allow-other-keys nil :allow-other-keys t :foo t))
  :program-error)

;;; --- leftmost T: accepted. The reverse order, which an always-reject fix fails

(defparameter %aok-t-then-nil
  '(funcall (lambda (&key a) a) :allow-other-keys t :allow-other-keys nil :b 1))

(deftest interp-allow-other-keys-first.t-then-nil-compile
  (%aok :compile %aok-t-then-nil)
  (:value nil))

(deftest interp-allow-other-keys-first.t-then-nil-interpret
  (%aok :interpret %aok-t-then-nil)
  (:value nil))

;;; --- over-fix guards: a single :ALLOW-OTHER-KEYS behaves as before

(deftest interp-allow-other-keys-first.single-t-interpret
  (%aok :interpret '(funcall (lambda (&key a) a) :b 1 :allow-other-keys t))
  (:value nil))

(deftest interp-allow-other-keys-first.single-nil-interpret
  (%aok :interpret '(funcall (lambda (&key a) a) :b 1 :allow-other-keys nil))
  :program-error)

(deftest interp-allow-other-keys-first.none-interpret
  (%aok :interpret '(funcall (lambda (&key a) a) :b 1))
  :program-error)

;;; &allow-other-keys in the lambda list accepts regardless of what the caller says
(deftest interp-allow-other-keys-first.lambda-list-aok-interpret
  (%aok :interpret '(funcall (lambda (&key a &allow-other-keys) a)
                     :allow-other-keys nil :b 1))
  (:value nil))

;;; ordinary calls, and the odd-number-of-keyword-arguments check, still work
(deftest interp-allow-other-keys-first.ordinary-interpret
  (%aok :interpret '(list (funcall (lambda (&key a) a) :a 7)
                    (funcall (lambda (&key a) a))))
  (:value (7 nil)))

(deftest interp-allow-other-keys-first.odd-keywords-interpret
  (%aok :interpret '(funcall (lambda (&key a) a) :a))
  :program-error)
