;;; A self tail call whose arguments are known to be of their parameters'
;;; checked types loops past the entry's checks; any other argument loops to the
;;; checks and is checked as on entry.

(setf dotcl:*save-sil* t)

(defstruct (%tsac-pt (:predicate nil)) (x 0))
(defstruct (%tsac-pt3 (:include %tsac-pt) (:predicate nil)))

(defun %tsac-same (p n) (declare (type %tsac-pt p))
  (if (<= n 0) (%tsac-pt-x p) (%tsac-same p (1- n))))
(defun %tsac-sub (p q n) (declare (type %tsac-pt p) (type %tsac-pt3 q))
  (if (<= n 0) (%tsac-pt-x p) (%tsac-sub q q (1- n))))
(defun %tsac-other (p k) (declare (type %tsac-pt p))
  (if (eql k 0) (%tsac-pt-x p) (%tsac-other k 0)))
(defun %tsac-labels (p n)
  (labels ((lp (a b i acc) (declare (type %tsac-pt a b))
             (if (<= i 0) acc (lp a a (1- i) (+ acc (%tsac-pt-x b))))))
    (lp p p n 0)))
(defun %tsac-labels-bad (p n)
  (labels ((lp (a i) (declare (type %tsac-pt a))
             (if (<= i 0) a (lp i (1- i)))))
    (lp p n)))

(defun %tsac-te (thunk)
  (handler-case (funcall thunk)
    (type-error (e)
      (let ((d (type-error-datum e)))
        (list :type-error (if (typep d 'structure-object) (type-of d) d))))))

(deftest tco-skip-arg-checks.values
  (list (%tsac-same (make-%tsac-pt :x 5) 100)
        (%tsac-sub (make-%tsac-pt :x 1) (make-%tsac-pt3 :x 2) 3)
        (%tsac-te (lambda () (%tsac-sub (make-%tsac-pt) (make-%tsac-pt) 3)))
        (%tsac-te (lambda () (%tsac-other (make-%tsac-pt :x 7) 0)))
        (%tsac-te (lambda () (%tsac-other (make-%tsac-pt :x 7) 9)))
        (%tsac-labels (make-%tsac-pt :x 3) 10)
        (%tsac-te (lambda () (%tsac-labels-bad (make-%tsac-pt) 4))))
  (5 2 (:type-error %tsac-pt) 7 (:type-error 9) 30 (:type-error 4)))

(deftest-compiled-only tco-skip-arg-checks.loops-past-checks
  (flet ((past (fn) (and (search "(BR TCOCHK" (%flat-text (dotcl:function-sil fn))) t)))
    (mapcar #'past (list #'%tsac-same #'%tsac-sub #'%tsac-other #'%tsac-labels #'%tsac-labels-bad)))
  (t t nil t nil))

(setf dotcl:*save-sil* nil)
