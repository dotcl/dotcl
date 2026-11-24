;;; A macro that expands to (RETURN-FROM <local function name>) returns from the implicit block of
;;; the LABELS / FLET function that uses it, as it does from a DEFUN's.

(defmacro %lfrm-global () `(return-from lfrm-g 42))

(defun %lfrm-global-labels () (labels ((lfrm-g () (%lfrm-global))) (lfrm-g)))
(defun %lfrm-global-flet () (flet ((lfrm-g () (%lfrm-global))) (lfrm-g)))
(defun %lfrm-local-labels ()
  (macrolet ((m () `(return-from lfrm-h 7)))
    (labels ((lfrm-h () (m) 1)) (lfrm-h))))
(defun %lfrm-local-flet ()
  (macrolet ((m () `(return-from lfrm-h 8)))
    (flet ((lfrm-h () (m) 1)) (lfrm-h))))
(defun %lfrm-loop-labels ()
  (labels ((lfrm-g (n) (if (= n 0) 0 (progn (when (= n 3) (%lfrm-global)) (lfrm-g (1- n))))))
    (lfrm-g 5)))

(deftest local-function-return-from-via-macro.all
  (list (%lfrm-global-labels) (%lfrm-global-flet) (%lfrm-local-labels) (%lfrm-local-flet)
        (%lfrm-loop-labels))
  (42 42 7 8 42))
