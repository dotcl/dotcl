;;; The body of a top level LOCALLY is processed as top level forms
;;; (CLHS 3.2.3.1), with its declarations still in effect.
;;;
;;; The flattener used to stop at PROGN and EVAL-WHEN, so a whole LOCALLY body
;;; stayed one form and compiled into one method. A file that puts a few hundred
;;; definitions inside one top level LOCALLY then produced multi-MB of IL in a
;;; single method, and JITting it at load cost gigabytes. These tests pin the
;;; semantics that the splitting must not disturb.

;;; Each body form runs, in order: the second form's init reads the first's value.
(locally (declare (optimize (speed 1)))
  (defvar *tl-locally-a* 1)
  (defvar *tl-locally-b* (+ *tl-locally-a* 1))
  (defun %tl-locally-f (x) (* x 10)))

(deftest toplevel-locally-body-forms-all-run
  (list *tl-locally-a* *tl-locally-b* (%tl-locally-f 3))
  (1 2 30))

;;; Compile-time side effects still reach the following body form: the macro is
;;; defined and used within one LOCALLY.
(locally (declare (optimize (speed 1)))
  (defmacro %tl-locally-m (x) `(+ ,x 100))
  (defvar *tl-locally-c* (%tl-locally-m 5)))

(deftest toplevel-locally-macro-visible-to-next-body-form
  *tl-locally-c*
  105)

;;; The declarations survive the split — each form is re-wrapped carrying them.
(locally (declare (special *tl-locally-d*))
  (defun %tl-locally-get-d () *tl-locally-d*)
  (defun %tl-locally-get-d2 () *tl-locally-d*))

(deftest toplevel-locally-declaration-reaches-each-body-form
  (let ((*tl-locally-d* 42))
    (declare (special *tl-locally-d*))
    (list (%tl-locally-get-d) (%tl-locally-get-d2)))
  (42 42))

;;; A single body form is left alone (splitting it would only add a wrapper).
(locally (declare (optimize (speed 1)))
  (defvar *tl-locally-single* :only))

(deftest toplevel-locally-single-body-form
  *tl-locally-single*
  :only)

;;; Declarations only, no body: must not vanish or signal.
(locally (declare (optimize (speed 1))))

(deftest toplevel-locally-declarations-only
  :survived
  :survived)

;;; A nested PROGN inside the LOCALLY flattens too, and stays in order.
(defvar *tl-locally-order* nil)
(locally (declare (optimize (speed 1)))
  (progn (push :first *tl-locally-order*)
         (push :second *tl-locally-order*))
  (push :third *tl-locally-order*))

(deftest toplevel-locally-nested-progn-order
  (reverse *tl-locally-order*)
  (:first :second :third))

;;; Not at top level, LOCALLY keeps its ordinary meaning and value.
(deftest nested-locally-returns-last-form
  (let ((x 5))
    (locally (declare (optimize (speed 1)))
      (incf x)
      (* x 2)))
  12)
