;;; A name is a function or a macro, not both (CLHS 5.1.2.5): DEFUN, (SETF FDEFINITION) and
;;; (SETF SYMBOL-FUNCTION) on a name that was a macro make it a function, and calls compiled
;;; afterwards call the function. DEFMACRO on a function name still works, in both directions.

(defmacro drm-a (x) `(list ,x))
(defun drm-a (x) (+ x 100))
(defun drm-a-caller (x) (drm-a x))

(deftest defun-replaces-macro.defun
  (list (and (macro-function 'drm-a) t) (fboundp 'drm-a) (funcall 'drm-a 1) (drm-a-caller 1))
  (nil t 101 101))

;; and back, and again
(defmacro drm-a (x) `(list ,x ,x))
(defun drm-a-caller-2 (x) (drm-a x))
(deftest defun-replaces-macro.macro-again
  (list (and (macro-function 'drm-a) t) (drm-a-caller-2 3))
  (t (3 3)))
(defun drm-a (x) (* x 2))
(defun drm-a-caller-3 (x) (drm-a x))
(deftest defun-replaces-macro.function-again
  (list (and (macro-function 'drm-a) t) (drm-a-caller-3 4) (funcall 'drm-a 5))
  (nil 8 10))

(defmacro drm-b (x) x)
(setf (fdefinition 'drm-b) (lambda (x) (+ x 7)))
(defun drm-b-caller (x) (drm-b x))
(deftest defun-replaces-macro.setf-fdefinition
  (list (and (macro-function 'drm-b) t) (drm-b-caller 1))
  (nil 8))

(defmacro drm-c (x) x)
(setf (symbol-function 'drm-c) (lambda (x) (+ x 9)))
(defun drm-c-caller (x) (drm-c x))
(deftest defun-replaces-macro.setf-symbol-function
  (list (and (macro-function 'drm-c) t) (drm-c-caller 1))
  (nil 10))

;; Nearby valid forms are unchanged: a plain macro stays a macro, a plain function stays a function,
;; and a function replaced by a function is still a function.
(defmacro drm-d (x) `(- ,x))
(defun drm-e (x) x)
(defun drm-e (x) (1+ x))
(defun drm-d-caller (x) (drm-d x))
(deftest defun-replaces-macro.others-unchanged
  (list (and (macro-function 'drm-d) t) (drm-d-caller 5) (and (macro-function 'drm-e) t) (drm-e 1))
  (t -5 nil 2))
