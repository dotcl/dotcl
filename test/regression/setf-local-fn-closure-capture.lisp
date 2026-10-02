;;; A local function named (SETF G) could not be reached from a closure or from
;;; another LABELS definition: the box that holds a LABELS function, and the
;;; slot of an FLET function, were made visible for capture only under symbol
;;; names, and the free-variable analysis did not look at #'(SETF G). The
;;; reference fell through to the global function and failed with "Undefined
;;; function: (SETF G)".

(defun %slfcc-labels-self ()
  (labels (((setf g) (v n) (if (> n 0) (setf (g (1- n)) v) (list v n))))
    (setf (g 3) 5)))

(defun %slfcc-labels-sibling ()
  (labels (((setf g) (v n) (list v n))
           (h () (setf (g 3) 5)))
    (h)))

(defun %slfcc-flet-closure ()
  (flet (((setf g) (v) (list v)))
    (labels ((h () (funcall #'(setf g) 2)))
      (list (h) (funcall (lambda () (setf (g) 7)))))))

(deftest setf-local-fn-closure-capture
  (list (%slfcc-labels-self) (%slfcc-labels-sibling) (%slfcc-flet-closure))
  ((5 0) (5 3) ((2) (7))))
