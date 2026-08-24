;;; MULTIPLE-VALUE-BIND takes its values out of a per-thread snapshot instead of
;;; building a list. Two things have to keep holding.
;;;
;;; 1. The CL semantics: extra vars are NIL, extra values are dropped, a
;;;    non-VALUES form is one value, zero values are all NIL.
;;; 2. The snapshot is only sound where the values are read immediately, which
;;;    is true of compiled code and NOT of the interpreter (its own steps run
;;;    MULTIPLE-VALUE-BIND in between). The interpreter therefore handles the
;;;    form before macroexpansion; these run it both ways.

(defun %mvb-two () (values 1 2))
(defun %mvb-one () 7)
(defun %mvb-none () (values))

(deftest mvb-snapshot.exact
  (multiple-value-bind (a b) (%mvb-two) (list a b))
  (1 2))

(deftest mvb-snapshot.more-vars-than-values
  (multiple-value-bind (a b c) (%mvb-two) (list a b c))
  (1 2 nil))

(deftest mvb-snapshot.fewer-vars-than-values
  (multiple-value-bind (a) (%mvb-two) a)
  1)

(deftest mvb-snapshot.single-value-form
  (multiple-value-bind (a b) (%mvb-one) (list a b))
  (7 nil))

(deftest mvb-snapshot.zero-values
  (multiple-value-bind (a b) (%mvb-none) (list a b))
  (nil nil))

(deftest mvb-snapshot.constant-form
  (multiple-value-bind (a b) 5 (list a b))
  (5 nil))

(deftest mvb-snapshot.nested
  (multiple-value-bind (a b) (%mvb-two)
    (multiple-value-bind (c d) (floor 17 5)
      (list a b c d)))
  (1 2 3 2))

(deftest mvb-snapshot.value-form-is-itself-a-bind
  (multiple-value-bind (a b) (multiple-value-bind (c d) (floor 17 5) (values d c))
    (list a b))
  (2 3))

(deftest mvb-snapshot.gethash-present-p
  (let ((h (make-hash-table)))
    (setf (gethash 'k h) 9)
    (list (multiple-value-bind (v p) (gethash 'k h) (list v p))
          (multiple-value-bind (v p) (gethash 'zz h) (list v p))))
  ((9 t) (nil nil)))

(deftest mvb-snapshot.body-in-mv-context
  (multiple-value-list
   (multiple-value-bind (a b) (%mvb-two) (values b a)))
  (2 1))

(deftest mvb-snapshot.closure-over-bound-vars
  (funcall (multiple-value-bind (a b) (%mvb-two) (lambda () (list a b))))
  (1 2))

;;; The interpreter reads the values through a different route (it cannot use
;;; the snapshot). Run the same shapes under it.

(deftest mvb-snapshot.interpreted
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(multiple-value-bind (a b) (floor 17 5) (list a b)))
          (eval '(multiple-value-bind (a b c) (values 1 2) (list a b c)))
          (eval '(multiple-value-bind (a b) 5 (list a b)))
          (eval '(multiple-value-bind (a b) (values) (list a b)))))
  ((3 2) (1 2 nil) (5 nil) (nil nil)))

(deftest mvb-snapshot.interpreted-get-setf-expansion
  ;; 5 values, and the shape that caught the snapshot bug (PSETF / PSETQ expand
  ;; into GET-SETF-EXPANSION consumed by MULTIPLE-VALUE-BIND).
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(let ((a (gensym)) (b (gensym)))
             (setf (symbol-value a) 1 (symbol-value b) 2)
             (psetf (symbol-value a) (symbol-value b)
                    (symbol-value b) (symbol-value a))
             (list (symbol-value a) (symbol-value b)))))
  (2 1))

;;; (values) with no arguments answers a shared marker; it must still behave.
(deftest mvb-snapshot.zero-values-forms
  (list (multiple-value-list (%mvb-none))
        (let ((x (%mvb-none))) x)
        (nth-value 0 (%mvb-none))
        (multiple-value-call #'list (%mvb-none) 1 (values 2 3))
        (length (multiple-value-list (values))))
  (nil nil nil (1 2 3) 0))
