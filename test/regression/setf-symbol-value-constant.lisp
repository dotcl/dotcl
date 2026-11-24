;;; (setf (symbol-value x) v) refuses a constant, as SET and SETQ do, and still sets a variable.

(defvar *ssvc-var* 1)
(defconstant +ssvc-const+ 5)

(defun %ssvc-set (sym val) (handler-case (progn (setf (symbol-value sym) val) :set) (error () :error)))

(deftest setf-symbol-value-constant.pi
  (list (%ssvc-set 'pi 3) (> pi 3.14))
  (:error t))

(deftest setf-symbol-value-constant.defconstant
  (list (%ssvc-set '+ssvc-const+ 6) +ssvc-const+)
  (:error 5))

(deftest setf-symbol-value-constant.nil-t-keyword
  (list (%ssvc-set nil 1) (%ssvc-set t 1) (%ssvc-set :kw 1) (eq :kw (symbol-value :kw)))
  (:error :error :error t))

(deftest setf-symbol-value-constant.variable
  (progn (setf (symbol-value '*ssvc-var*) 7) *ssvc-var*)
  7)
