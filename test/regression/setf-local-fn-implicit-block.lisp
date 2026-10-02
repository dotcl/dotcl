;;; A local function named (SETF G) has an implicit BLOCK named G around its
;;; body (CLHS 3.4.11 via FLET / LABELS). The compiler built no block for a
;;; (SETF G) name, so (RETURN-FROM G ...) in the body failed with "no block
;;; named G".

(defun %slfib-flet ()
  (let ((r 0))
    (flet (((setf g) (v)
             (when (> v 1) (return-from g :big))
             (setq r v)))
      (list (setf (g) 5) (setf (g) 1) r))))

(defun %slfib-labels-closure ()
  (labels (((setf g) (v) (funcall (lambda () (return-from g (list v))))))
    (setf (g) 5)))

(deftest setf-local-fn-implicit-block
  (list (%slfib-flet) (%slfib-labels-closure))
  ((:big 1 1) (5)))
