;;; xref (who-calls) — the compiler records caller→callee edges for every
;;; named call and #'name reference while a defun body compiles, and plants a
;;; load-time registration so the table also rebuilds on fasl load.
;;; dotcl:who-calls returns callers of a name; dotcl:who-is-called-by returns
;;; the callees recorded for a definition.
;;;
;;; Every test here is compiled-only: a build without a compiler records no edges
;;; at all, so the who-calls tests fail and the shadowing test (which asserts an
;;; ABSENCE) would pass for the wrong reason.

(defun %xref-t-callee-a (x) (1+ x))
(defun %xref-t-callee-b (x) (1- x))
(defun %xref-t-caller-direct (x) (%xref-t-callee-a x))
(defun %xref-t-caller-indirect (lst) (mapcar #'%xref-t-callee-a lst))
(defun %xref-t-caller-nested (lst)
  (funcall (lambda (l) (mapcar #'%xref-t-callee-b l)) lst))
(defun %xref-t-shadow ()
  (flet ((%xref-t-callee-a (x) x))
    (%xref-t-callee-a 1)))

(deftest-compiled-only xref-who-calls-direct-and-function-ref
  (sort (mapcar #'symbol-name (dotcl:who-calls '%xref-t-callee-a)) #'string<)
  ("%XREF-T-CALLER-DIRECT" "%XREF-T-CALLER-INDIRECT"))

;;; a call inside a nested lambda attributes to the enclosing defun
(deftest-compiled-only xref-who-calls-nested-lambda
  (mapcar #'symbol-name (dotcl:who-calls '%xref-t-callee-b))
  ("%XREF-T-CALLER-NESTED"))

(deftest-compiled-only xref-who-is-called-by
  (mapcar #'symbol-name (dotcl:who-is-called-by '%xref-t-caller-direct))
  ("%XREF-T-CALLEE-A"))

;;; a local flet call shadowing a global name is NOT an edge to the global
(deftest-compiled-only xref-flet-shadow-not-recorded
  (member "%XREF-T-SHADOW"
          (mapcar #'symbol-name (dotcl:who-calls '%xref-t-callee-a))
          :test #'string=)
  nil)

;;; redefinition replaces the caller's edge set (stale edges drop)
(deftest-compiled-only xref-redefinition-replaces-edges
  (progn
    (defun %xref-t-redef (x) (%xref-t-callee-a x))
    (let ((before (and (member '%xref-t-redef (dotcl:who-calls '%xref-t-callee-a)) t)))
      (defun %xref-t-redef (x) (%xref-t-callee-b x))
      (list before
            (and (member '%xref-t-redef (dotcl:who-calls '%xref-t-callee-a)) t)
            (and (member '%xref-t-redef (dotcl:who-calls '%xref-t-callee-b)) t))))
  (t nil t))
