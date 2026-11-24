;;; MULTIPLE-VALUE-BIND of two variables on GETHASH does one lookup and needs no values channel.
;;; Expected values from SBCL.

(setf dotcl:*save-sil* t)

(defvar *mgl-table* (let ((h (make-hash-table)))
                      (setf (gethash 5 h) 6 (gethash :nilval h) nil)
                      h))

(defun mgl-1 (k) (multiple-value-bind (v p) (gethash k *mgl-table*) (list v p)))
(defun mgl-2 (k) (multiple-value-bind (v p) (gethash k *mgl-table* 'dflt) (list v p)))
(defun mgl-3 (k) (multiple-value-bind (v p) (gethash k *mgl-table*) (declare (ignore p)) v))
(defun mgl-4 (k) (multiple-value-bind (v p) (gethash k *mgl-table*) (declare (ignore v)) p))
(defun mgl-5 (k) (multiple-value-bind (v p x) (gethash k *mgl-table*) (list v p x)))
(defun mgl-6 (k)
  (let ((n 0))
    (multiple-value-bind (v p) (gethash (progn (incf n) k) *mgl-table* (progn (incf n 10) 'z))
      (list v p n))))
(defun mgl-7 (k) (multiple-value-bind (v) (gethash k *mgl-table* 'dflt) v))

(deftest mvb-gethash-one-lookup.present-and-absent
  (list (mgl-1 5) (mgl-1 7) (mgl-1 :nilval))
  ((6 t) (nil nil) (nil t)))

(deftest mvb-gethash-one-lookup.default
  (list (mgl-2 5) (mgl-2 7) (mgl-2 :nilval) (mgl-7 7))
  ((6 t) (dflt nil) (nil t) dflt))

(deftest mvb-gethash-one-lookup.ignore
  (list (mgl-3 5) (mgl-3 7) (mgl-4 5) (mgl-4 7))
  (6 nil t nil))

(deftest mvb-gethash-one-lookup.third-variable
  (list (mgl-5 5) (mgl-5 7))
  ((6 t nil) (nil nil nil)))

(deftest mvb-gethash-one-lookup.argument-order
  (list (mgl-6 5) (mgl-6 7))
  ((6 t 11) (z nil 11)))

(deftest mvb-gethash-one-lookup.not-a-table
  (let ((*mgl-table* 5))
    (handler-case (mgl-1 1) (type-error () :type-error)))
  :type-error)

(deftest mvb-gethash-one-lookup.shadowed-names
  (let ((p 1) (v 2))
    (declare (ignorable p v))
    (multiple-value-bind (p v) (gethash 5 *mgl-table*) (list p v)))
  (6 t))

(deftest mvb-gethash-one-lookup.funcall-and-apply
  (list (multiple-value-list (funcall #'gethash 5 *mgl-table*))
        (multiple-value-list (apply #'gethash (list 7 *mgl-table* 'd))))
  ((6 t) (d nil)))

;;; The four shapes of a lookup (hit / miss, with / without a default) through one function each.
(defun mgl-hit (k) (multiple-value-bind (v p) (gethash k *mgl-table*) (if p v -1)))
(defun mgl-miss (k) (multiple-value-bind (v p) (gethash (+ 100 k) *mgl-table*) (if p v -1)))
(defun mgl-hit-d (k) (multiple-value-bind (v p) (gethash k *mgl-table* 0) (if p v -1)))
(defun mgl-miss-d (k) (multiple-value-bind (v p) (gethash (+ 100 k) *mgl-table* 0) (if p v -2)))
(defun mgl-miss-d-val (k) (multiple-value-bind (v p) (gethash (+ 100 k) *mgl-table* 'd) (list v p)))

(setf dotcl:*save-sil* nil)

(deftest mvb-gethash-one-lookup.four-shapes
  (list (mgl-hit 5) (mgl-miss 5) (mgl-hit-d 5) (mgl-miss-d 5) (mgl-miss-d-val 5))
  (6 -1 6 -2 (d nil)))

(deftest-emitting-only mvb-gethash-one-lookup.the-expansion-is-used
  (flet ((uses-channel (fn)
           (and (search "CapturePair" (%flat-text (dotcl:function-sil fn))) t)))
    (list (uses-channel #'mgl-hit) (uses-channel #'mgl-miss)
          (uses-channel #'mgl-hit-d) (uses-channel #'mgl-miss-d)
          (uses-channel #'mgl-5)))
  (nil nil nil nil t))
