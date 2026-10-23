;;; A generic function's loose cache grows as more argument-class pairs are seen,
;;; and past its size limit it overwrites entries. Every pair must still reach its
;;; own method, before and after the table is full, and after a method change.

(defstruct (gls-ty (:constructor nil)))

(defmacro %gls-define (n)
  `(progn
     ,@(loop for i from 0 below n
             collect `(defstruct (,(intern (format nil "GLS-~d" i)) (:include gls-ty))))))
(%gls-define 40)

(defvar *gls-objs*
  (coerce (loop for i from 0 below 40
                collect (funcall (intern (format nil "MAKE-GLS-~d" i))))
          'vector))

(defun %gls-index (x) (position x *gls-objs*))

(defgeneric gls-pair (x y))
(defmethod gls-pair ((x gls-ty) (y gls-ty)) (+ (* 100 (%gls-index x)) (%gls-index y)))
(defmethod gls-pair ((x gls-3) (y gls-5)) :three-five)

(defun %gls-check (n)
  (let ((bad 0))
    (dotimes (pass 2)
      (dotimes (i n)
        (dotimes (j n)
          (let ((want (if (and (= i 3) (= j 5)) :three-five (+ (* 100 i) j)))
                (got (gls-pair (aref *gls-objs* i) (aref *gls-objs* j))))
            (unless (eql want got) (incf bad))))))
    bad))

;; 9, 25, 100 and 1600 pairs: the last is more than the table holds.
(deftest gf-loose-cache-sizes.pairs
  (list (%gls-check 3) (%gls-check 5) (%gls-check 10) (%gls-check 40))
  (0 0 0 0))

(deftest gf-loose-cache-sizes.method-added-when-full
  (progn
    (%gls-check 40)
    (let ((m (defmethod gls-pair ((x gls-7) (y gls-ty)) :seven)))
      (prog1 (list (gls-pair (aref *gls-objs* 7) (aref *gls-objs* 39))
                   (gls-pair (aref *gls-objs* 7) (aref *gls-objs* 0))
                   (gls-pair (aref *gls-objs* 8) (aref *gls-objs* 0)))
        (remove-method #'gls-pair m))))
  (:seven :seven 800))

(defgeneric gls-one (x))
(defmethod gls-one ((x gls-ty)) (%gls-index x))

(deftest gf-loose-cache-sizes.one-arg
  (let ((bad 0))
    (dotimes (pass 3)
      (dotimes (i 40)
        (unless (eql i (gls-one (aref *gls-objs* i))) (incf bad))))
    bad)
  0)
