;;; A FORMAT ~<...~:> print-function keeps nested structure slot text in order.

(defstruct psl-inner list)

(defun psl-print-node (node stream depth)
  (declare (ignore depth))
  (format stream "~<#psl-node<~;~S>~:>" (list (psl-node-key node))))

(defstruct (psl-node (:print-function psl-print-node)) key)

(defun psl-text (node margin pretty)
  (let ((*print-right-margin* margin)
        (*print-pretty* pretty)
        (*print-readably* nil)
        (*print-circle* nil)
        (*print-level* nil)
        (*print-length* nil)
        (*print-miser-width* nil))
    (prin1-to-string node)))

(defun psl-node (&rest list)
  (make-psl-node :key (make-psl-inner :list list)))

(deftest pprint-struct-slot-order.compact
  (psl-text (psl-node (cons 'alpha 1) (cons 'bravo 2) (cons 'charlie 3)) 80 t)
  "#psl-node<#S(PSL-INNER :LIST ((ALPHA . 1) (BRAVO . 2) (CHARLIE . 3)))>")

(deftest pprint-struct-slot-order.narrow
  (let ((text (psl-text (psl-node (cons 'alpha 1) (cons 'bravo 2) (cons 'charlie 3)) 30 t)))
    (< (search "#S(PSL-INNER" text) (search "(ALPHA" text)))
  t)

(deftest pprint-struct-slot-order.not-pretty
  (psl-text (psl-node (cons 'alpha 1) (cons 'bravo 2) (cons 'charlie 3)) 30 nil)
  "#psl-node<#S(PSL-INNER :LIST ((ALPHA . 1) (BRAVO . 2) (CHARLIE . 3)))>")
