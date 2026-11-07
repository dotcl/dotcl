;;; FLET / LABELS / MACROLET / SYMBOL-MACROLET / TAGBODY and the standard macros
;;; WHEN, UNLESS, CASE, TYPECASE, MULTIPLE-VALUE-BIND, NTH-VALUE, DEFVAR,
;;; DEFUN ... with a bad shape signal PROGRAM-ERROR; valid shapes still work,
;;; and a local macro or function of the same name hides the check.

(defmacro %sfm-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest special-form-shape-macros.invalid
  (values (%sfm-pe (flet)) (%sfm-pe (labels)) (%sfm-pe (macrolet)) (%sfm-pe (symbol-macrolet))
          (%sfm-pe (flet 1)) (%sfm-pe (flet (f . 1) 2))
          (%sfm-pe (tagbody "a")) (%sfm-pe (tagbody 1.5)) (%sfm-pe (tagbody . 1))
          (%sfm-pe (when)) (%sfm-pe (unless)) (%sfm-pe (case)) (%sfm-pe (typecase))
          (%sfm-pe (multiple-value-bind)) (%sfm-pe (multiple-value-bind (a)))
          (%sfm-pe (multiple-value-list)) (%sfm-pe (multiple-value-setq (a)))
          (%sfm-pe (nth-value)) (%sfm-pe (nth-value 0))
          (%sfm-pe (defvar)) (%sfm-pe (defvar 1)) (%sfm-pe (defvar a 1 "doc" 4))
          (%sfm-pe (defparameter a)) (%sfm-pe (defconstant a))
          (%sfm-pe (defun)) (%sfm-pe (defun %sfm-f)) (%sfm-pe (defun 1 ()))
          (%sfm-pe (prog)))
  :program-error :program-error :program-error :program-error
  :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error :program-error :program-error
  :program-error :program-error
  :program-error :program-error
  :program-error :program-error
  :program-error :program-error :program-error
  :program-error :program-error
  :program-error :program-error :program-error
  :program-error)

(defvar *sfm-v* 0)

(deftest special-form-shape-macros.valid
  (values (eval '(flet ())) (eval '(flet ((f () 1)) (f))) (eval '(labels ((f () 2)) (f)))
          (eval '(macrolet ()  4)) (eval '(symbol-macrolet ((s 5)) s))
          (eval '(tagbody)) (eval '(tagbody a 1 b (go c) c))
          (eval '(when t)) (eval '(unless nil)) (eval '(when t 1 2))
          (eval '(case 1)) (eval '(case 1 (1 :one))) (eval '(typecase 1 (integer :i)))
          (eval '(multiple-value-bind (a b)  (values 1 2) (list a b)))
          (eval '(multiple-value-bind () 1))
          (eval '(multiple-value-list (values 1 2)))
          (eval '(nth-value 1 (values 1 2)))
          (eval '(defvar *sfm-v*)) (eval '(defvar *sfm-v* 9 "doc"))
          (eval '(prog ())) (eval '(prog (a) (return a)))
          (eval '(macrolet ((when (x) `(quote ,x))) (when 3))))
  nil 1 2 4 5 nil nil nil nil 2 nil :one :i (1 2) nil (1 2) 2 *sfm-v* *sfm-v* nil nil 3)

(deftest special-form-shape-macros.case-clauses
  (values (%sfm-pe (case nil (() . 1))) (%sfm-pe (ecase 1 (1 . 2))) (%sfm-pe (typecase 1 (integer . 1)))
          (eval '(case 1 (1 :a) (t :b))) (eval '(case 2 ((1 2) :a))) (eval '(typecase 1 (integer)))
          (eval '(ecase 1 (1))))
  :program-error :program-error :program-error :a :a nil nil)
