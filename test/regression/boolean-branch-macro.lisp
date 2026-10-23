;;; The test of an IF that is a call of a user macro is compiled as a branch on
;;; the macro's expansion, so (AND (< A B) T) from a macro branches on the
;;; comparison itself instead of building T / NIL and asking IsTruthy. Local
;;; macros and functions shadowing the name are respected.

(setf dotcl:*save-sil* t)

(defmacro %bbm-to-boolean (x) `(and ,x t))
(defmacro %bbm-between (x lo hi) `(and (<= ,lo ,x) (<= ,x ,hi)))

(defun %bbm-cmp (a b)
  (declare (double-float a b))
  (if (%bbm-to-boolean (< a b)) :lt :ge))

(defun %bbm-range (i)
  (declare (fixnum i))
  (if (not (%bbm-between i 0 9)) :out :in))

(defun %bbm-local (x)
  (macrolet ((%bbm-to-boolean (y) `(not ,y)))
    (if (%bbm-to-boolean x) :yes :no)))

(defun %bbm-flet (x)
  (flet ((%bbm-to-boolean (y) (eql y 3)))
    (if (%bbm-to-boolean x) :three :other)))

(deftest boolean-branch-macro.values
  (list (%bbm-cmp 1d0 2d0) (%bbm-cmp 2d0 1d0)
        (%bbm-range 5) (%bbm-range 10) (%bbm-range -1)
        (%bbm-local nil) (%bbm-local 1) (%bbm-flet 3) (%bbm-flet 4))
  (:lt :ge :in :out :out :yes :no :three :other))

(deftest-compiled-only boolean-branch-macro.branches
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "IsTruthy" #'%bbm-cmp) (has "IsTruthy" #'%bbm-range)))
  (nil nil))

(setf dotcl:*save-sil* nil)
