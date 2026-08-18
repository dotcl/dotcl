;;; DEFCLASS with a custom :METACLASS must work where there is no compiler.
;;;
;;; DEFCLASS lowers to (%register-class (%make-class ...)) normally, but to
;;; (%register-class (%make-class-full ... (%find-or-forward-class 'meta))) when the
;;; class names a metaclass. %MAKE-CLASS-FULL had a compiler intrinsic and no
;;; function binding, so compiled code — which emits Runtime.MakeClassFull inline
;;; from its own table and never looks the name up — was fine, while the tree-walk
;;; evaluator, which runs the expansion as ordinary code, died with "Undefined
;;; function: %MAKE-CLASS-FULL". Same shape as %MAKE-SLOT-DEF-WITH-ALLOCATION
;;; before it: the members of this family that only appear in a rarer DEFCLASS
;;; option are the ones that go missing.

(defun %idm (form)
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval form)))

(deftest interp-defclass-metaclass.class-of
  (%idm '(progn
          (defclass %idm-meta (standard-class) ())
          (defmethod dotcl-mop:validate-superclass ((c %idm-meta) (s standard-class)) t)
          (defclass %idm-user () ((a :initarg :a :accessor %idm-a)) (:metaclass %idm-meta))
          (eq (class-of (find-class '%idm-user)) (find-class '%idm-meta))))
  t)

;;; The class must be usable, not merely constructible.

(deftest interp-defclass-metaclass.instance-works
  (%idm '(%idm-a (make-instance '%idm-user :a 7)))
  7)
