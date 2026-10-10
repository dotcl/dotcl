;;; SETF of a place whose DEFINE-SETF-EXPANDER returns more than one store
;;; variable.
;;;
;;; CLHS 5.1.2.9 / 5.1.1.2: the store variables are bound to the values of the
;;; new-value form (missing ones to NIL) and the storing form returns them, so
;;; the values of SETF are the values of the storing form. The expansion used
;;; to put a (VALUES store...) after the MULTIPLE-VALUE-BIND that binds the
;;; store variables, outside their scope: every such SETF signalled
;;; "Unbound variable" after the store had been done. Expected values are what
;;; SBCL returns.

(defvar *sems-cell* (list 0 0))

(define-setf-expander sems-two ()
  (let ((a (gensym)) (b (gensym)))
    (values nil nil (list a b)
            `(progn (setf (car *sems-cell*) ,a (cadr *sems-cell*) ,b)
                    (values ,a ,b))
            `(values (car *sems-cell*) (cadr *sems-cell*)))))

(deftest setf-expander-multiple-stores.single-value
  (list (multiple-value-list (setf (sems-two) 1)) (copy-list *sems-cell*))
  ((1 nil) (1 nil)))

(deftest setf-expander-multiple-stores.multiple-values
  (list (multiple-value-list (setf (sems-two) (values 5 6))) (copy-list *sems-cell*))
  ((5 6) (5 6)))

(deftest setf-expander-multiple-stores.eval
  (multiple-value-list (eval '(setf (sems-two) (values 7 8))))
  (7 8))

;; The storing form decides the result, not the store variables.
(define-setf-expander sems-count ()
  (let ((a (gensym)) (b (gensym)))
    (values nil nil (list a b)
            `(progn (setf (car *sems-cell*) ,a (cadr *sems-cell*) ,b) :stored)
            `(values (car *sems-cell*) (cadr *sems-cell*)))))

(deftest setf-expander-multiple-stores.storing-form-result
  (multiple-value-list (setf (sems-count) (values 3 4)))
  (:stored))
