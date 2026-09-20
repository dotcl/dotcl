;;; A SETF expansion has to be a Lisp form that a code walker can walk.
;;;
;;; CLHS 3.1.2.1.2 says the operator of a compound form is a symbol or a lambda
;;; expression. ((SETF acc) new obj) is neither, so it is not a Lisp form at
;;; all, even though the compiler here happened to accept it. A macro that walks
;;; its body -- iterate is the one that found this -- sees the expansion of a
;;; SETF inside that body and refuses the bare form.
;;;
;;; Two expanders emitted it: the one DEFCLASS registers for an :accessor, and
;;; the one for a local (SETF f) function bound by FLET/LABELS. Both now go
;;; through FUNCALL of #'(SETF name), which calls the same function.

(defclass setf-op-form-class ()
  ((s :accessor setf-op-form-s :initform nil)))

(deftest setf-accessor-expansion-calls-through-a-symbol
  ;; (LET ((tmp val)) (FUNCALL #'(SETF acc) tmp obj) tmp)
  (let* ((form (macroexpand-1 '(setf (setf-op-form-s obj) val)))
         (call (third form)))
    (list (car form) (car call) (symbolp (car call))))
  (let funcall t))

(deftest setf-accessor-still-stores-and-returns-the-value
  (let ((o (make-instance 'setf-op-form-class)))
    (list (setf (setf-op-form-s o) 42) (setf-op-form-s o)))
  (42 42))

(deftest setf-accessor-read-modify-write-still-works
  (let ((o (make-instance 'setf-op-form-class)))
    (setf (setf-op-form-s o) 1)
    (incf (setf-op-form-s o) 5)
    (push 9 (setf-op-form-s o))
    (setf-op-form-s o))
  (9 . 6))

;;; The expander calls the generic function on purpose, so qualifier methods on
;;; (SETF acc) still run. Going through FUNCALL must not change that.

(defmethod (setf setf-op-form-s) :around ((val number) (obj setf-op-form-class))
  (call-next-method (* 10 val) obj))

(deftest setf-accessor-still-dispatches-qualifier-methods
  (let ((o (make-instance 'setf-op-form-class)))
    (setf (setf-op-form-s o) 4)
    (setf-op-form-s o))
  40)

;;; A local (SETF f) function is the other expander. It has to keep calling the
;;; lexical binding, and to keep returning what the setter returned rather than
;;; the new value.

(defun setf-op-form-local ()
  (let ((cell (list nil)))
    (flet (((setf lf) (v c) (setf (car c) v) :from-setter))
      (list (setf (lf cell) 7) (car cell)))))

(deftest setf-local-function-expansion-calls-the-lexical-binding
  (setf-op-form-local)
  (:from-setter 7))

;;; A global (SETF f) defined with DEFUN takes neither path, but must keep
;;; working alongside them.

(defvar *setf-op-form-global* nil)
(defun (setf setf-op-form-global) (v) (setf *setf-op-form-global* v) v)

(deftest setf-global-setf-function-still-works
  (progn (setf (setf-op-form-global) 3) *setf-op-form-global*)
  3)
