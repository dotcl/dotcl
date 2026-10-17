;;; The value of a substituted local function is single-float typed when its
;;; body is a MACROLET, or converts (THE <integer type> E) of a parameter with
;;; FLOAT; a name the MACROLET binds is a macro there, whatever it names outside.

(setf dotcl:*save-sil* t)

(declaim (ftype (function (t) single-float) %sit-global))
(defun %sit-global (x) (declare (ignore x)) 1.5f0)

(defun %sit-macrolet (a b)
  (declare (single-float a b))
  (flet ((bic (x y)
           (declare (single-float x y))
           (macrolet ((sq (v) `(* ,v ,v)))
             (let ((s (sq x)))
               (declare (single-float s))
               (+ s y)))))
    (declare (inline bic))
    (let* ((v (bic a b)) (r (round v)))
      r)))

(defun %sit-the (a acc)
  (declare (single-float a) (function acc))
  (flet ((px (x f)
           (declare (single-float x) (function f))
           (* x (float (the (unsigned-byte 8) (funcall f 2)) 1f0))))
    (declare (inline px))
    (let* ((v (px a acc)) (r (round v)))
      r)))

(defun %sit-shadow (a)
  (declare (single-float a))
  (macrolet ((%sit-global (v) `(list ,v)))
    (let ((v (%sit-global a)))
      v)))

(deftest single-inline-typing.values
  (list (%sit-macrolet 1.5f0 0.25f0) (%sit-the 1.5f0 (lambda (n) (* n 3)))
        (%sit-shadow 2f0))
  (2 9 (2.0)))

(deftest-compiled-only single-inline-typing.raw
  (list (search "RoundOp" (%flat-text (dotcl:function-sil #'%sit-macrolet)))
        (search "RoundOp" (%flat-text (dotcl:function-sil #'%sit-the))))
  (nil nil))

(setf dotcl:*save-sil* nil)
