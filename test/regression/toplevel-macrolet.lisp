;;; The body of a top level MACROLET / SYMBOL-MACROLET is processed as top level
;;; forms (CLHS 3.2.3.1), with the local macros and declarations still in effect.
;;;
;;; The flattener descended PROGN, EVAL-WHEN and LOCALLY but stopped at these two,
;;; so a whole MACROLET body stayed one form and compiled into one method — the
;;; same failure LOCALLY had (a file that puts a few hundred definitions inside one
;;; top level MACROLET produced multi-MB of IL in a single method, JITted at load).
;;; These tests pin the semantics the splitting must not disturb.

;;; Each body form runs, in order, and the local macro reaches every one of them.
(macrolet ((%tl-ml-double (x) `(* 2 ,x)))
  (defvar *tl-ml-a* (%tl-ml-double 1))
  (defvar *tl-ml-b* (%tl-ml-double *tl-ml-a*))
  (defun %tl-ml-f (x) (%tl-ml-double x)))

(deftest toplevel-macrolet-body-forms-all-run
  (list *tl-ml-a* *tl-ml-b* (%tl-ml-f 5))
  (2 4 10))

;;; A local macro shadowing a global macro of the same name must still win in
;;; every body form. The flattener knows only the global macro table, so it must
;;; not macroexpand inside a MACROLET body.
(defmacro %tl-ml-shadowed (x) `(list :global ,x))

(macrolet ((%tl-ml-shadowed (x) `(list :local ,x)))
  (defun %tl-ml-uses-shadow-1 () (%tl-ml-shadowed 1))
  (defun %tl-ml-uses-shadow-2 () (%tl-ml-shadowed 2)))

(deftest toplevel-macrolet-local-macro-shadows-global
  (list (%tl-ml-uses-shadow-1) (%tl-ml-uses-shadow-2) (%tl-ml-shadowed 3))
  ((:local 1) (:local 2) (:global 3)))

;;; Compile-time side effects still reach the following body form: the macro is
;;; defined and used within one MACROLET body.
(macrolet ((%tl-ml-id (x) x))
  (defmacro %tl-ml-plus100 (x) `(+ ,x 100))
  (defvar *tl-ml-c* (%tl-ml-id (%tl-ml-plus100 5))))

(deftest toplevel-macrolet-macro-visible-to-next-body-form
  *tl-ml-c*
  105)

;;; Declarations between the bindings and the body survive the split — each form
;;; is re-wrapped carrying them.
(macrolet ((%tl-ml-get (name) `(symbol-value ',name)))
  (declare (special *tl-ml-d*))
  (defun %tl-ml-get-d () *tl-ml-d*)
  (defun %tl-ml-get-d2 () *tl-ml-d*))

(deftest toplevel-macrolet-declaration-reaches-each-body-form
  (let ((*tl-ml-d* 42))
    (declare (special *tl-ml-d*))
    (list (%tl-ml-get-d) (%tl-ml-get-d2)))
  (42 42))

;;; A single body form is left alone (splitting it would only add a wrapper).
(macrolet ((%tl-ml-one () :only))
  (defvar *tl-ml-single* (%tl-ml-one)))

(deftest toplevel-macrolet-single-body-form
  *tl-ml-single*
  :only)

;;; No body at all: must not vanish or signal.
(macrolet ((%tl-ml-none () :nothing)))

(deftest toplevel-macrolet-empty-body
  :survived
  :survived)

;;; A nested PROGN inside the MACROLET flattens too, keeps order, and the local
;;; macro is still in scope inside it.
(defvar *tl-ml-order* nil)
(macrolet ((%tl-ml-note (k) `(push ,k *tl-ml-order*)))
  (progn (%tl-ml-note :first)
         (%tl-ml-note :second))
  (%tl-ml-note :third))

(deftest toplevel-macrolet-nested-progn-order
  (reverse *tl-ml-order*)
  (:first :second :third))

;;; Not at top level, MACROLET keeps its ordinary meaning and value.
(deftest nested-macrolet-returns-last-form
  (macrolet ((%tl-ml-sq (x) `(* ,x ,x)))
    (%tl-ml-sq 2)
    (%tl-ml-sq 3))
  9)

;;; SYMBOL-MACROLET: same split, and the symbol macro reaches every body form.
(defvar *tl-sml-store* (list 7))

(symbol-macrolet ((%tl-sml-slot (car *tl-sml-store*)))
  (defun %tl-sml-read () %tl-sml-slot)
  (defun %tl-sml-write (v) (setf %tl-sml-slot v)))

(deftest toplevel-symbol-macrolet-body-forms-all-run
  (list (%tl-sml-read) (progn (%tl-sml-write 9) (%tl-sml-read)) (car *tl-sml-store*))
  (7 9 9))

;;; A symbol macro shadowing a global symbol macro must win in every body form.
(define-symbol-macro %tl-sml-shadowed :global)

(symbol-macrolet ((%tl-sml-shadowed :local))
  (defun %tl-sml-uses-shadow-1 () %tl-sml-shadowed)
  (defun %tl-sml-uses-shadow-2 () %tl-sml-shadowed))

(deftest toplevel-symbol-macrolet-shadows-global
  (list (%tl-sml-uses-shadow-1) (%tl-sml-uses-shadow-2) %tl-sml-shadowed)
  (:local :local :global))

;;; Not at top level, SYMBOL-MACROLET keeps its ordinary meaning and value.
(deftest nested-symbol-macrolet-returns-last-form
  (let ((cell (list 3)))
    (symbol-macrolet ((slot (car cell)))
      (setf slot (* slot 4))
      slot))
  12)
