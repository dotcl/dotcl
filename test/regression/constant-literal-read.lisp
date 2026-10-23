;;; A constant variable whose value is a number, a character, NIL, T or an
;;; interned symbol is read as that literal, and (EQ X NIL) against NIL or such a
;;; constant tests for NIL the way EQ does.

(setf dotcl:*save-sil* t)

(defconstant +clr-nil+ nil)
(defconstant +clr-k+ 42)
(defconstant +clr-sym+ 'clr-foo)
(defconstant +clr-ch+ #\a)
(defconstant +clr-list+ '(1 2))

(defun %clr-values () (list +clr-nil+ +clr-k+ +clr-sym+ +clr-ch+ +clr-list+))
(defun %clr-eq (x)
  (list (if (eq +clr-nil+ x) :nil :non-nil)
        (if (eq x nil) :nil :non-nil)
        (if (eql nil x) :nil :non-nil)
        (eq x +clr-nil+)))

(deftest constant-literal-read.values
  (list (%clr-values)
        (eq (fifth (%clr-values)) +clr-list+))
  ((nil 42 clr-foo #\a (1 2)) t))

(deftest constant-literal-read.eq-nil
  (list (%clr-eq nil) (%clr-eq '()) (%clr-eq 'nil) (%clr-eq :a) (%clr-eq 0) (%clr-eq (list nil)))
  ((:nil :nil :nil t) (:nil :nil :nil t) (:nil :nil :nil t)
   (:non-nil :non-nil :non-nil nil) (:non-nil :non-nil :non-nil nil)
   (:non-nil :non-nil :non-nil nil)))

(deftest-emitting-only constant-literal-read.no-lookup
  (let ((v (%flat-text (dotcl:function-sil #'%clr-values)))
        (e (%flat-text (dotcl:function-sil #'%clr-eq))))
    (list (search "+CLR-K+" v) (search "+CLR-NIL+" v) (search "+CLR-SYM+" v)
          (and (search "+CLR-LIST+" v) t)
          (search "IsTrueEq" e)))
  (nil nil nil t nil))
