;;; A DEFINE-SETF-EXPANDER expander gets the environment of the SETF it
;;; expands, and the one GET-SETF-EXPANSION was given. binding-arrows uses
;;; that environment as a true value.

(defvar *sxe-seen* nil)
(define-setf-expander sxe-place (x &environment env)
  (setf *sxe-seen* (if env :env :null))
  (let ((s (gensym)))
    (values nil nil (list s) `(list ,x ,s) `(list ,x))))

(deftest-compiled-only setf-expander-environment.setf-in-function
  (progn (setf *sxe-seen* nil)
         (funcall (compile nil '(lambda () (setf (sxe-place 1) 2))))
         *sxe-seen*)
  :env)

(deftest setf-expander-environment.get-setf-expansion-without-env
  (progn (setf *sxe-seen* nil)
         (get-setf-expansion '(sxe-place 1))
         *sxe-seen*)
  :null)

(defmacro sxe-expand-with-env (&environment env)
  (get-setf-expansion '(sxe-place 1) env)
  nil)

(deftest-compiled-only setf-expander-environment.get-setf-expansion-with-env
  (progn (setf *sxe-seen* nil)
         (funcall (compile nil '(lambda () (sxe-expand-with-env))))
         *sxe-seen*)
  :env)
