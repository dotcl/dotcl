;;; The assigned-binding solver walks COND and CASE: an undeclared LET variable assigned only constants
;;; in their branches has a known range and is an int64 slot, as one assigned in WHEN / IF branches
;;; already was. The values are those of the boxed variable, including a test that assigns, a clause
;;; with only a test, and a value that leaves the range of the constants.

(setf dotcl:*save-sil* t)

(defun acc-cond (a b)
  (declare (type (unsigned-byte 11) a b))
  (let ((c1 0) (c2 5) (c3 2))
    (cond ((zerop a) (setf c1 1))
          ((= b 2047) (setf c2 7))
          ((zerop b) (setf c2 131007) (setf c3 1)))
    (+ (* a 3) c1 c2 c3)))

(defun acc-cond-ref (a b)
  (declare (type (unsigned-byte 11) a b))
  (let ((c1 0) (c2 5) (c3 2))
    (if (zerop a) (setf c1 1)
        (if (= b 2047) (setf c2 7)
            (if (zerop b) (progn (setf c2 131007) (setf c3 1)))))
    (+ (* a 3) c1 c2 c3)))

(defun acc-cond-test-assigns (a)
  (declare (type (unsigned-byte 11) a))
  (let ((c 0) (d 0))
    (cond ((progn (setq c 5) (zerop a)) (setq d 1))
          ((progn (setq c 9) (= a 3)))
          (t (setq d 2)))
    (list c d)))

(defun acc-cond-wide (a v)
  (declare (type (unsigned-byte 11) a))
  (let ((c 0))
    (cond ((zerop a) (setq c 1))
          (t (setq c v)))
    (list c (+ c 1))))

(defun acc-case (a)
  (declare (type (unsigned-byte 11) a))
  (let ((c 0) (d 3))
    (case a
      (0 (setf c 1))
      ((1 2) (setf c 2) (setf d 4))
      (t (setf c 9)))
    (+ (* a 3) c d)))

(defun acc-case-no-default (a)
  (declare (type (unsigned-byte 11) a))
  (let ((c 0))
    (case a (0 (setf c 1)) (1 (setf c 2)))
    (+ (* a 3) c)))

(setf dotcl:*save-sil* nil)

(deftest avt-cond-case-range.cond-values
  (loop for a in '(0 1 7 2047) always
        (loop for b in '(0 5 2047) always (eql (acc-cond a b) (acc-cond-ref a b))))
  t)

(deftest avt-cond-case-range.test-that-assigns-and-wide-values
  (list (acc-cond-test-assigns 0) (acc-cond-test-assigns 3) (acc-cond-test-assigns 8)
        (acc-cond-wide 0 100) (acc-cond-wide 4 (expt 2 70)) (acc-cond-wide 4 -5))
  ((5 1) (9 0) (9 2) (1 2) (1180591620717411303424 1180591620717411303425) (-5 -4)))

(deftest avt-cond-case-range.case-values
  (list (acc-case 0) (acc-case 1) (acc-case 2) (acc-case 5) (acc-case-no-default 0)
        (acc-case-no-default 1) (acc-case-no-default 9))
  (4 9 12 27 1 5 27))

(defun acc-slot-int64-p (sil name)
  (let ((p (search name sil)))
    (and p (search "Int64" sil :start2 p :end2 (min (length sil) (+ p 40))) t)))

(deftest-emitting-only avt-cond-case-range.the-variables-are-int64-slots
  (let ((a (%flat-text (dotcl:function-sil #'acc-cond)))
        (b (%flat-text (dotcl:function-sil #'acc-case))))
    (list (acc-slot-int64-p a "(DECLARE-LOCAL C1_") (acc-slot-int64-p a "(DECLARE-LOCAL C2_")
          (acc-slot-int64-p b "(DECLARE-LOCAL C_")))
  (t t t))
