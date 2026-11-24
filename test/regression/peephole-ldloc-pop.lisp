;;; A local that is loaded only to be popped is dead: a raw structure slot store of a variable, in
;;; statement position, used to leave (LDLOC X) (POP) behind (the stored variable, read for the
;;; value of the SETF). The peephole pass drops the pair.

(setf dotcl:*save-sil* t)

(defstruct (plp-st (:constructor make-plp-st ())) (n 0 :type fixnum) (d 0d0 :type double-float))

(defun %plp-set-fixnum (s k)
  (declare (type plp-st s) (fixnum k))
  (let ((m (1+ k))) (declare (fixnum m)) (setf (plp-st-n s) m) nil))

(defun %plp-set-double (s x)
  (declare (type plp-st s) (double-float x))
  (let ((y (* x 2d0))) (declare (double-float y)) (setf (plp-st-d s) y) nil))

(defun %plp-dead-pairs (fn)
  (let ((n 0))
    (loop for l on (dotcl:function-sil fn)
          do (when (and (consp (first l)) (eq (car (first l)) :ldloc)
                        (consp (second l)) (eq (car (second l)) :pop))
               (incf n)))
    n))

(deftest-emitting-only peephole-ldloc-pop.no-dead-pair
  (list (%plp-dead-pairs #'%plp-set-fixnum) (%plp-dead-pairs #'%plp-set-double))
  (0 0))

(deftest peephole-ldloc-pop.values
  (let ((s (make-plp-st)))
    (%plp-set-fixnum s 41)
    (%plp-set-double s 1.5d0)
    (list (plp-st-n s) (plp-st-d s)))
  (42 3.0d0))

(setf dotcl:*save-sil* nil)
