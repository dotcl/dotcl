;;; GETHASH / FLOOR / TRUNCATE / CEILING / ROUND whose values are read at once
;;; by MULTIPLE-VALUE-BIND or NTH-VALUE.

(defvar *mpc-h* (let ((h (make-hash-table))) (setf (gethash :a h) 1 (gethash :n h) nil) h))

(defun %mpc-bind (k)
  (list (multiple-value-bind (v f) (gethash k *mpc-h*) (list v f))
        (multiple-value-bind (v f) (gethash k *mpc-h* :dflt) (list v f))
        (multiple-value-bind (v) (gethash k *mpc-h*) v)
        (multiple-value-bind (v f x) (gethash k *mpc-h*) (list v f x))))

(deftest mv-pair-capture.gethash-bind
  (list (%mpc-bind :a) (%mpc-bind :n) (%mpc-bind :z))
  (((1 t) (1 t) 1 (1 t nil))
   ((nil t) (nil t) nil (nil t nil))
   ((nil nil) (:dflt nil) nil (nil nil nil))))

(defun %mpc-nth (k)
  (list (nth-value 0 (gethash k *mpc-h*)) (nth-value 1 (gethash k *mpc-h*))
        (nth-value 2 (gethash k *mpc-h*))))

(deftest mv-pair-capture.gethash-nth
  (list (%mpc-nth :a) (%mpc-nth :z))
  ((1 t nil) (nil nil nil)))

(defun %mpc-div (a b)
  (list (multiple-value-bind (q r) (floor a b) (list q r))
        (multiple-value-bind (q r) (truncate a b) (list q r))
        (multiple-value-bind (q r) (ceiling a b) (list q r))
        (multiple-value-bind (q r) (round a b) (list q r))
        (nth-value 1 (floor a b)) (nth-value 1 (truncate a b))
        (nth-value 1 (ceiling a b)) (nth-value 1 (round a b))))

(deftest mv-pair-capture.division
  (list (%mpc-div 7 2) (%mpc-div -7 2) (%mpc-div 7/2 1))
  (((3 1) (3 1) (4 -1) (4 -1) 1 1 -1 -1)
   ((-4 1) (-3 -1) (-3 -1) (-4 1) 1 -1 -1 1)
   ((3 1/2) (3 1/2) (4 -1/2) (4 -1/2) 1/2 1/2 -1/2 -1/2)))

(defun %mpc-errors ()
  (list (handler-case (multiple-value-bind (v f) (gethash :a 5) (list v f))
          (type-error () :type-error))
        (handler-case (nth-value 1 (floor 1 0)) (division-by-zero () :div0))))

(deftest mv-pair-capture.errors
  (%mpc-errors)
  (:type-error :div0))

;; The values must not stay in the thread state for a later reader.
(defun %mpc-one () :one)
(defun %mpc-after (k)
  (multiple-value-bind (v f) (gethash k *mpc-h*) (declare (ignore v f)))
  (nth-value 1 (gethash k *mpc-h*))
  (multiple-value-list (%mpc-one)))

(deftest mv-pair-capture.no-leftover
  (%mpc-after :a)
  (:one))

(defun %mpc-gethash-loop (n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n r)
      (multiple-value-bind (v f) (gethash :a *mpc-h*) (setq r (and f v)))
      (setq r (nth-value 1 (gethash :a *mpc-h*))))))

(defun %mpc-truncate-loop (n)
  (declare (fixnum n))
  (let ((r nil))
    (dotimes (i n r)
      (multiple-value-bind (q rr) (truncate 7 (if r 2 3)) (setq r (and q rr))))))

(deftest-compiled-only mv-pair-capture.allocates-nothing
  (list (< (bytes-per-op #'%mpc-gethash-loop) 1)
        (< (bytes-per-op #'%mpc-truncate-loop) 1))
  (t t))
