;;; A lexical variable holds one value, never the multiple-values object, so
;;; the test of it for NIL and its EQ with a symbol literal compile to plain
;;; reference comparisons. The values the variable got from forms that return
;;; several values must still test as their primary value.

(setf dotcl:*save-sil* t)

(defun %lte-if (x) (if x :yes :no))
(defun %lte-eq (x) (eq x 'a))
(defun %lte-eq-first (x) (eq 'a x))

(defun %lte-two (v) (values v 2))
(declaim (notinline %lte-two))

(defun %lte-bound ()
  (let ((x (%lte-two 'a)) (y (%lte-two nil)) (z (values nil 1)))
    (list (eq x 'a) (if y :yes :no) (if z :yes :no) (eq 'a x))))

(defun %lte-setq ()
  (let ((x nil) (y t))
    (setq x (%lte-two 'a) y (floor 5 7))
    (list (eq x 'a) (if (zerop y) :zero :other) (if x :yes :no))))

(defun %lte-loop-for= ()
  (loop for s in '("a" "b" "nil")
        for sym = (read-from-string s)
        collect (list (eq sym 'a) (if sym :yes :no))))

(deftest local-truth-eq.values
  (list (%lte-if nil) (%lte-if 0) (%lte-eq 'a) (%lte-eq 'b) (%lte-eq-first 'a))
  (:no :yes t nil t))

(deftest local-truth-eq.from-multiple-values
  (list (%lte-bound) (%lte-setq) (%lte-loop-for=))
  ((t :no :no t) (t :zero :yes) ((t :yes) (nil :yes) (nil :no))))

(deftest-compiled-only local-truth-eq.shape
  (list (and (search "Runtime.IsNotNil" (%flat-text (dotcl:function-sil #'%lte-if))) t)
        (search "Runtime.Primary" (%flat-text (dotcl:function-sil #'%lte-eq)))
        (search "Runtime.Primary" (%flat-text (dotcl:function-sil #'%lte-eq-first))))
  (t nil nil))
