;;; A LET variable that is not declared but whose body reads or writes the raw slots of one structure
;;; three or more times has its raw slot array fetched once, where it is bound. The fetch is a hint:
;;; anything that is not an instance of that structure at that layout version takes the code every
;;; access had before, so the answers and the errors are the same as without it.

(defstruct (sbi (:constructor make-sbi (x y z m)))
  (x 0d0 :type double-float) (y 0d0 :type double-float) (z 0d0 :type double-float) (m 0 :type fixnum))
;; More than four raw slots: the instance keeps them in an array rather than in fields.
(defstruct (sbi7 (:constructor make-sbi7 (a b c d e f g)))
  (a 0d0 :type double-float) (b 0d0 :type double-float) (c 0d0 :type double-float) (d 0d0 :type double-float)
  (e 0d0 :type double-float) (f 0d0 :type double-float) (g 0d0 :type double-float))
(defstruct (sbi-boxed (:constructor make-sbi-boxed (tag v w u)))
  (tag nil) (v 0d0 :type double-float) (w 0d0 :type double-float) (u 0d0 :type double-float))
(defstruct (sbi-parent (:constructor make-sbi-parent (a b c))) (a 0d0 :type double-float) (b 0d0 :type double-float) (c 0d0 :type double-float))
(defstruct (sbi-child (:include sbi-parent) (:constructor make-sbi-child (a b c d))) (d 0d0 :type double-float))

;; Three accesses (hoisted), two accesses (not).
(defun %sbi-three (o) (let ((p o)) (+ (sbi-x p) (sbi-y p) (sbi-z p))))
(defun %sbi-two (o) (let ((p o)) (+ (sbi-x p) (sbi-y p))))
(defun %sbi-seven (o) (let ((p o)) (+ (sbi7-a p) (sbi7-b p) (sbi7-c p) (sbi7-d p) (sbi7-e p) (sbi7-f p) (sbi7-g p))))
(defun %sbi-boxed (o) (let ((p o)) (list (sbi-boxed-tag p) (+ (sbi-boxed-v p) (sbi-boxed-w p) (sbi-boxed-u p)))))
(defun %sbi-parent-sum (o) (let ((p o)) (+ (sbi-parent-a p) (sbi-parent-b p) (sbi-parent-c p))))
(defun %sbi-write-read (o)
  (let ((p o))
    (setf (sbi-x p) 10d0)
    (setf (sbi-y p) (+ (sbi-x p) 1d0))
    (setf (sbi-m p) 7)
    (list (sbi-x p) (sbi-y p) (sbi-m p))))
(defun %sbi-assigned (o other)
  ;; P is assigned, so it is not fetched once: it may hold a different instance later.
  (let ((p o))
    (let ((a (sbi-x p)))
      (setq p other)
      (+ a (sbi-x p) (sbi-y p)))))
(defun %sbi-captured (o)
  (let ((p o))
    (let ((f (lambda () (setq p (make-sbi 100d0 200d0 300d0 1)))))
      (let ((a (sbi-x p)))
        (funcall f)
        (+ a (sbi-x p) (sbi-y p))))))

(defun %sbi-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) (type-of e))))

(deftest struct-backing-inferred.same-answers
  (list (%sbi-three (make-sbi 1d0 2d0 3d0 4))
        (%sbi-two (make-sbi 1d0 2d0 3d0 4))
        (%sbi-seven (make-sbi7 1d0 2d0 3d0 4d0 5d0 6d0 7d0))
        (%sbi-boxed (make-sbi-boxed :t 1d0 2d0 3d0))
        (%sbi-write-read (make-sbi 1d0 2d0 3d0 4))
        (%sbi-assigned (make-sbi 1d0 2d0 3d0 4) (make-sbi 10d0 20d0 30d0 4))
        (%sbi-captured (make-sbi 1d0 2d0 3d0 4)))
  (6d0 3d0 28d0 (:t 6d0) (10d0 11d0 7) 31d0 301d0))

(deftest struct-backing-inferred.included-structure
  ;; The parent's accessors on a child instance: a different layout, so the fetch declines, and
  ;; the access does what it did before.
  (list (%sbi-parent-sum (make-sbi-parent 1d0 2d0 3d0))
        (%sbi-parent-sum (make-sbi-child 1d0 2d0 3d0 4d0)))
  (6d0 6d0))

(deftest struct-backing-inferred.not-the-structure
  ;; What is not an instance of the structure is reported as it is without the fetch: the same
  ;; condition type for the three-access function (fetched) and the two-access one (not).
  (list (equal (%sbi-error (lambda () (%sbi-three 5))) (%sbi-error (lambda () (%sbi-two 5))))
        (equal (%sbi-error (lambda () (%sbi-three nil))) (%sbi-error (lambda () (%sbi-two nil))))
        (equal (%sbi-error (lambda () (%sbi-three (make-sbi7 1d0 2d0 3d0 4d0 5d0 6d0 7d0))))
               (%sbi-error (lambda () (%sbi-two (make-sbi7 1d0 2d0 3d0 4d0 5d0 6d0 7d0)))))
        (not (eq :no-error (%sbi-error (lambda () (%sbi-three 5))))))
  (t t t t))

(defun %sbi-loop (o n)
  (declare (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n)
      (let ((p o))
        (setq acc (+ acc (sbi7-a p) (sbi7-b p) (sbi7-c p) (sbi7-d p) (sbi7-e p) (sbi7-f p) (sbi7-g p)))))
    acc))

(deftest-compiled-only struct-backing-inferred.no-allocation
  (let ((o (make-sbi7 1d0 2d0 3d0 4d0 5d0 6d0 7d0)))
    (%sbi-loop o 10)
    (list (= (%sbi-loop o 1000) 28000d0)
          (< (min-allocated-bytes (lambda () (%sbi-loop o 100000))) 20000)))
  (t t))

;;; A LET* whose structure variable is read by the inits after it: compiled as nested LET*s so the
;;; reads in those inits use the fetched array too. The meaning is unchanged.
(defun %sbi-letstar (a b)
  (let* ((p a) (q b)
         (dx (- (sbi-x q) (sbi-x p))) (dy (- (sbi-y q) (sbi-y p))) (dz (- (sbi-z q) (sbi-z p)))
         (sum (+ dx dy dz)))
    (declare (double-float dx dy dz))
    (setf (sbi-x p) (+ (sbi-x p) sum))
    (setf (sbi-y p) (+ (sbi-y p) sum))
    (list sum (sbi-x p) (sbi-y p) (sbi-z q))))
(defun %sbi-letstar-declared (a)
  ;; The declaration is about P, bound before the split would be: no split, same answers.
  (let* ((p a) (x (sbi-x p)) (y (sbi-y p)) (z (sbi-z p)))
    (declare (type sbi p) (double-float x y z))
    (+ x y z (sbi-x p))))
(defun %sbi-letstar-assigned (a other)
  (let* ((p a) (x (sbi-x p)) (y (progn (setq p other) (sbi-y p))) (z (sbi-z p)))
    (declare (double-float x y z))
    (list x y z)))
(defun %sbi-letstar-shadow (a)
  (let* ((p a) (x (sbi-x p)) (p (make-sbi 5d0 6d0 7d0 1)) (y (sbi-y p)) (z (sbi-z p)))
    (declare (double-float x y z))
    (list x y z)))

(deftest struct-backing-inferred.let*-split
  (list (%sbi-letstar (make-sbi 1d0 2d0 3d0 4) (make-sbi 10d0 20d0 30d0 5))
        (%sbi-letstar-declared (make-sbi 1d0 2d0 3d0 4))
        (%sbi-letstar-assigned (make-sbi 1d0 2d0 3d0 4) (make-sbi 10d0 20d0 30d0 5))
        (%sbi-letstar-shadow (make-sbi 1d0 2d0 3d0 4))
        (%sbi-error (lambda () (%sbi-letstar 5 (make-sbi 1d0 2d0 3d0 1))))
        (equal (%sbi-error (lambda () (%sbi-letstar 5 (make-sbi 1d0 2d0 3d0 1))))
               (%sbi-error (lambda () (%sbi-letstar-assigned 5 (make-sbi 1d0 2d0 3d0 1))))))
  ((54d0 55d0 56d0 30d0) 7d0 (1d0 20d0 30d0) (1d0 6d0 7d0)
   #.(type-of (handler-case (%sbi-letstar-declared 5) (error (e) e))) t))
