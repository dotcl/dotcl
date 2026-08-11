;;; (FUNCTION name) must find a lexical FLET / LABELS binding before the global
;;; definition. The tree-walk interpreter went straight to SYMBOL-FUNCTION, so
;;; #'f inside (flet ((f ...)) ...) missed the local function entirely — an
;;; UNDEFINED-FUNCTION when nothing global had that name, and silently the WRONG
;;; function when something did. ansi-test BLOCK.5 / BLOCK.10 are the shape that
;;; caught it: #'%f handed to MAPCAR, where %f is an FLET that RETURN-FROMs.
;;;
;;; Both evaluator paths are asserted for each case: dotcl:*evaluator-mode* is
;;; bound around the EVAL, so these run under the ordinary compiled harness and
;;; still exercise the interpreter (which on emit-free builds is the only
;;; evaluator there is).

(defun %ilf-eval (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

;;; The ansi-test BLOCK.5 shape: #'%f is a local function that exits an outer
;;; block. Nothing defines %f globally, so the old path signalled.
(defparameter %ilf-block5
  '(block done
     (flet ((%ilf-f (x) (return-from done x)))
       (mapcar #'%ilf-f '(good bad bad)))
     'bad))

(deftest interp-lexical-function.flet-funarg-compile
  (%ilf-eval :compile %ilf-block5)
  good)

(deftest interp-lexical-function.flet-funarg-interpret
  (%ilf-eval :interpret %ilf-block5)
  good)

;;; A lexical binding must SHADOW an existing global of the same name — the
;;; failure mode that stays silent rather than signalling.
(defun %ilf-shadowed () :global)

(defparameter %ilf-shadow-form
  '(flet ((%ilf-shadowed () :lexical))
     (funcall #'%ilf-shadowed)))

(deftest interp-lexical-function.flet-shadows-global-compile
  (%ilf-eval :compile %ilf-shadow-form)
  :lexical)

(deftest interp-lexical-function.flet-shadows-global-interpret
  (%ilf-eval :interpret %ilf-shadow-form)
  :lexical)

;;; LABELS, including self-reference through #'.
(defparameter %ilf-labels-form
  '(labels ((%ilf-fact (n) (if (< n 2) 1 (* n (funcall #'%ilf-fact (1- n))))))
     (funcall #'%ilf-fact 5)))

(deftest interp-lexical-function.labels-self-ref-compile
  (%ilf-eval :compile %ilf-labels-form)
  120)

(deftest interp-lexical-function.labels-self-ref-interpret
  (%ilf-eval :interpret %ilf-labels-form)
  120)

;;; Outside any lexical binding, #'name still resolves globally.
(deftest interp-lexical-function.global-still-found-interpret
  (%ilf-eval :interpret '(funcall #'%ilf-shadowed))
  :global)
