;;; The multiple-value-bind shortcuts (parse-integer, floor, gethash, ...) read the call as the
;;; global operator; a local FLET / LABELS / MACROLET definition of the same name wins.

(deftest mv-split-local.flet-parse-integer
  (flet ((parse-integer (x) (declare (ignore x)) (values 1 99)))
    (multiple-value-bind (a b) (parse-integer "5") (list a b)))
  (1 99))

(deftest mv-split-local.labels-parse-integer
  (labels ((parse-integer (x) (declare (ignore x)) (values 1 99)))
    (multiple-value-bind (a b) (parse-integer "5") (list a b)))
  (1 99))

(deftest mv-split-local.macrolet-parse-integer
  (macrolet ((parse-integer (x) (declare (ignore x)) '(values 1 99)))
    (multiple-value-bind (a b) (parse-integer "5") (list a b)))
  (1 99))

(deftest mv-split-local.flet-length-under-parse-integer
  (flet ((length (x) (declare (ignore x)) 99))
    (multiple-value-bind (a b) (parse-integer "5") (list a b)))
  (5 1))

(deftest mv-split-local.flet-floor
  (flet ((floor (x y) (declare (ignore x y)) (values 1 77)))
    (multiple-value-bind (a b) (floor 7 2) (list a b)))
  (1 77))

(deftest mv-split-local.macrolet-floor
  (macrolet ((floor (x y) (declare (ignore x y)) '(values 1 77)))
    (multiple-value-bind (a b) (floor 7 2) (list a b)))
  (1 77))

(deftest mv-split-local.flet-mod-under-floor
  (flet ((mod (x y) (declare (ignore x y)) 55))
    (multiple-value-bind (a b) (floor 7 2) (list a b)))
  (3 1))

(deftest-emitting-only mv-split-local.flet-floor-multiple-value-setq
  (flet ((floor (x y) (declare (ignore x y)) (values 1 77)))
    (let (a b) (multiple-value-setq (a b) (floor 7 2)) (list a b)))
  (1 77))

(deftest mv-split-local.flet-gethash
  (let ((h (make-hash-table)))
    (flet ((gethash (k tb) (declare (ignore k tb)) (values :x 88)))
      (multiple-value-bind (a b) (gethash 1 h) (list a b))))
  (:x 88))

(deftest mv-split-local.macrolet-gethash
  (macrolet ((gethash (k tb) (declare (ignore k tb)) '(values :x 88)))
    (multiple-value-bind (a b) (gethash 1 nil) (list a b)))
  (:x 88))

(deftest mv-split-local.flet-eq-under-gethash
  (flet ((eq (x y) (declare (ignore x y)) nil))
    (multiple-value-bind (a b) (gethash 1 (make-hash-table)) (list a b)))
  (nil nil))

;; The ordinary forms are unchanged.
(defun %mvsl-parse (s) (multiple-value-bind (a b) (parse-integer s) (list a b)))
(defun %mvsl-parse-radix (s) (multiple-value-bind (a b) (parse-integer s :radix 16) (list a b)))
(defun %mvsl-floor (x y) (multiple-value-bind (q r) (floor x y) (list q r)))
(defun %mvsl-gethash (h k) (multiple-value-bind (v p) (gethash k h 'dflt) (list v p)))

(deftest mv-split-local.ordinary-parse-integer
  (list (%mvsl-parse " 5 ") (%mvsl-parse-radix "ff"))
  ((5 3) (255 2)))

(deftest mv-split-local.ordinary-floor
  (%mvsl-floor 7 2)
  (3 1))

(deftest mv-split-local.ordinary-gethash
  (let ((h (make-hash-table)))
    (setf (gethash 1 h) :one)
    (list (%mvsl-gethash h 1) (%mvsl-gethash h 2)))
  ((:one t) (dflt nil)))
