;;; LET* is sequential: each binding is in effect while the REMAINING init forms
;;; are evaluated (CLHS 3.1.2.1.1.2). The interpreter collected the special
;;; bindings and PROGV'd them all at the end, which got that backwards — a later
;;; init form ran with the special still at its outer value, and then PROGV
;;; installed the saved value over whatever that form had done to it.
;;;
;;; Found via ansi-test STRUCTURE-2-3..2-7, where a DEFSTRUCT slot default is
;;; (incf *counter*): the counter WAS incremented, but the increment landed on
;;; the outer binding and was then masked, so the body read the pre-increment
;;; value.
;;;
;;; Both evaluator paths are asserted per case by binding dotcl:*evaluator-mode*
;;; around the EVAL, so this runs under the ordinary compiled harness.

(defvar *ilss-ctr* 0)

(defun %ilss (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

;;; A later init form mutating an earlier special binding must be visible in the
;;; body. This is the exact ansi-test shape.
(defparameter %ilss-mutate
  '(let* ((*ilss-ctr* 0) (x (incf *ilss-ctr*)))
     (list x *ilss-ctr*)))

(deftest interp-let-star-special.later-init-mutates-compile
  (%ilss :compile %ilss-mutate)
  (1 1))

(deftest interp-let-star-special.later-init-mutates-interpret
  (%ilss :interpret %ilss-mutate)
  (1 1))

;;; A later init form must READ the binding established by an earlier one.
(defparameter %ilss-read
  '(let* ((*ilss-ctr* 41) (x (1+ *ilss-ctr*)))
     (list x *ilss-ctr*)))

(deftest interp-let-star-special.later-init-reads-compile
  (%ilss :compile %ilss-read)
  (42 41))

(deftest interp-let-star-special.later-init-reads-interpret
  (%ilss :interpret %ilss-read)
  (42 41))

;;; The binding must be undone on exit, and the outer value restored.
(deftest interp-let-star-special.unwinds-interpret
  (progn (setq *ilss-ctr* 7)
         (%ilss :interpret '(let* ((*ilss-ctr* 0) (x (incf *ilss-ctr*))) x))
         *ilss-ctr*)
  7)

;;; Lexical bindings in LET* keep working, including a lexical whose init form
;;; reads an earlier special binding.
(deftest interp-let-star-special.mixed-lexical-interpret
  (%ilss :interpret '(let* ((a 1) (*ilss-ctr* 10) (b (+ a *ilss-ctr*)))
                       (list a *ilss-ctr* b)))
  (1 10 11))

;;; LET (parallel) is unchanged: init forms see the OUTER value.
(deftest interp-let-star-special.plain-let-is-parallel-interpret
  (progn (setq *ilss-ctr* 5)
         (%ilss :interpret '(let ((*ilss-ctr* 0) (x *ilss-ctr*)) (list x))))
  (5))
