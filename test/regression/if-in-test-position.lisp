;;; IF, WHEN, UNLESS, COND and PROGN as the test of another conditional: each
;;; arm is branched on as a test, and the value is never built.

(setf dotcl:*save-sil* t)

(defmacro %itp-zerop (x) `(if (numberp ,x) (zerop ,x)))

(defun %itp-zero-either (x y) (if (or (%itp-zerop x) (%itp-zerop y)) :zero :nonzero))
(defun %itp-nested (a b c) (if (if a b c) :yes :no))
(defun %itp-not-nested (a b c) (if (not (if a b c)) :yes :no))
(defun %itp-when (a b) (if (when a b) :yes :no))
(defun %itp-unless (a b) (if (unless a b) :yes :no))
(defun %itp-cond (a b c)
  (if (cond ((eq a :x) b) (c) ((eq a :y) nil) (t (list a))) :yes :no))
(defun %itp-and-cond (a b) (and (cond ((numberp a) (> a 0)) (t b)) :ok))

(defvar *itp-log* nil)
(defun %itp-note (x) (push x *itp-log*) x)
(defun %itp-effects (a)
  (setq *itp-log* nil)
  (list (if (progn (%itp-note :p1) (when (%itp-note a) (%itp-note :w) (%itp-note (not a))))
            :t :f)
        (reverse *itp-log*)))
(defun %itp-mv (a) (if (if a (values nil t) (values t nil)) :yes :no))

(deftest if-in-test-position.values
  (list (%itp-zero-either 0 1) (%itp-zero-either 1 0.0) (%itp-zero-either 'a 2)
        (%itp-zero-either 3 'b) (%itp-zero-either (expt 2 70) 1/2)
        (%itp-nested t 1 nil) (%itp-nested t nil 1) (%itp-nested nil nil 1)
        (%itp-nested nil 1 nil)
        (%itp-not-nested t 1 nil) (%itp-not-nested nil 1 nil)
        (%itp-when t 2) (%itp-when nil 2) (%itp-when t nil)
        (%itp-unless nil 2) (%itp-unless t 2)
        (%itp-cond :x 1 nil) (%itp-cond :x nil nil) (%itp-cond :x nil 3)
        (%itp-cond :y nil nil) (%itp-cond :z nil nil) (%itp-cond :z nil 3)
        (%itp-and-cond 1 nil) (%itp-and-cond -1 t) (%itp-and-cond 'a t) (%itp-and-cond 'a nil)
        (%itp-mv t) (%itp-mv nil))
  (:zero :zero :nonzero :nonzero :nonzero
   :yes :no :yes :no
   :no :yes
   :yes :no :no
   :yes :no
   :yes :no :no :no :yes :yes
   :ok nil :ok nil
   :no :yes))

(deftest if-in-test-position.effects
  (list (%itp-effects 1) (%itp-effects nil))
  ((:f (:p1 1 :w nil)) (:f (:p1 nil))))

(deftest-compiled-only if-in-test-position.no-value-built
  (let ((s (%flat-text (dotcl:function-sil #'%itp-zero-either))))
    (list (and (search "Runtime.IsTrueZerop" s) t)
          (search "\"Runtime.Zerop\"" s)
          (search "Runtime.IsTruthy" s)))
  (t nil nil))
