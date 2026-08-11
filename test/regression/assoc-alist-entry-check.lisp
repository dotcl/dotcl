;;; #'ASSOC must not let a malformed alist entry through.
;;;
;;; CLHS: an alist element is a cons or NIL. NIL is skipped; any other non-cons is
;;; a type-error (ansi-test ASSOC.ERROR.11).
;;;
;;; ASSOC has two implementations:
;;;   * Runtime.Assoc — what the compiler emits inline for a 2-argument call
;;;   * AssocCore     — the registered #'ASSOC (funcall / apply / interpreted path)
;;; Only the former checked the entries, so a literal (assoc ...) errored while the
;;; same call through #'ASSOC returned NIL — a divergence present on BOTH evaluator
;;; paths, the same shape as unary #'- .
;;;
;;; That is why the compiled cases below also fail before the fix.

(defun %ae (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (list :value (eval form))
      (type-error () :type-error)
      (error (e) (list :other (type-of e))))))

;;; --- a malformed entry is a type-error (ansi ASSOC.ERROR.11)

(deftest assoc-alist-entry.bad-entry-compile
  (%ae :compile '(funcall #'assoc 'z '((a . b) :bad (c . d))))
  :type-error)

(deftest assoc-alist-entry.bad-entry-interpret
  (%ae :interpret '(funcall #'assoc 'z '((a . b) :bad (c . d))))
  :type-error)

;;; the keyword-argument path is a different loop from the eq/eql fast paths
(deftest assoc-alist-entry.bad-entry-with-test-interpret
  (%ae :interpret '(funcall #'assoc 'z '((a . b) :bad (c . d)) :test #'eq))
  :type-error)

(deftest assoc-alist-entry.bad-entry-with-key-interpret
  (%ae :interpret '(funcall #'assoc 'z '((a . b) :bad) :key #'identity))
  :type-error)

;;; --- over-fix guard: a NIL element is legal and simply skipped

(deftest assoc-alist-entry.nil-entry-allowed-compile
  (%ae :compile '(funcall #'assoc 'c '((a . b) nil (c . d))))
  (:value (c . d)))

(deftest assoc-alist-entry.nil-entry-allowed-interpret
  (%ae :interpret '(funcall #'assoc 'c '((a . b) nil (c . d))))
  (:value (c . d)))

;;; --- ordinary lookups still work (exercises all three loops)

(deftest assoc-alist-entry.ordinary-lookups-interpret
  (%ae :interpret '(list (funcall #'assoc 'c '((a . b) (c . d)))
                    (funcall #'assoc "c" '(("a" . 1) ("c" . 3)) :test #'string=)
                    (funcall #'assoc 2 '((1 . a) (2 . b)) :key #'identity)
                    (funcall #'assoc 1 '((1 . a) (2 . b)) :test-not #'eql)
                    (funcall #'assoc 'a nil)
                    (funcall #'assoc 'z '((a . b)))))
  (:value ((c . d) ("c" . 3) (2 . b) (2 . b) nil nil)))
