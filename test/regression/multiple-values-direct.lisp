;;; VALUES of one, NTH-VALUE and VALUES-LIST reach their answer without building
;;; something to throw away.
;;;
;;;   - (VALUES x) went through the general entry, which takes an array; the
;;;     callee then returns element 0 and drops it. 32 bytes a call for a form
;;;     that produces no values object at all.
;;;   - NTH-VALUE expanded to (NTH n (MULTIPLE-VALUE-LIST form)) -- the whole
;;;     list of values consed to hand back one of them.
;;;   - VALUES-LIST grew a List and copied it with ToArray, on top of the array
;;;     the values object keeps.
;;;
;;; What must not change is which values come out, in what order, and what the
;;; missing ones are.

(defun %mvd-v3 () (values 1 2 3))
(defun %mvd-v0 () (values))

(deftest multiple-values-direct.values-of-one
  (list (multiple-value-list (values 1))
        ;; (VALUES x) truncates x to its primary value.
        (multiple-value-list (values (%mvd-v3)))
        (multiple-value-list (values (%mvd-v0)))
        (values 5))
  ((1) (1) (nil) 5))

(deftest multiple-values-direct.nth-value-constant-index
  (list (nth-value 0 (%mvd-v3)) (nth-value 1 (%mvd-v3)) (nth-value 2 (%mvd-v3))
        ;; Past the end is NIL, as missing values are.
        (nth-value 3 (%mvd-v3))
        (nth-value 0 (%mvd-v0)) (nth-value 1 (%mvd-v0))
        ;; A form with a single value is not a values object at all.
        (nth-value 0 42) (nth-value 1 42))
  (1 2 3 nil nil nil 42 nil))

;;; A computed index still goes through the macro expansion, and must agree.
(deftest multiple-values-direct.nth-value-computed-index
  (let ((i 1))
    (list (nth-value i (%mvd-v3)) (nth-value (+ i 1) (%mvd-v3))))
  (2 3))

;;; NTH-VALUE itself yields exactly one value.
(deftest multiple-values-direct.nth-value-is-single-valued
  (multiple-value-list (nth-value 1 (%mvd-v3)))
  (2))

;;; The form is evaluated once, left to right.
(defvar *mvd-side* nil)
(defun %mvd-note (x) (push x *mvd-side*) x)

(deftest multiple-values-direct.nth-value-evaluates-once
  (progn
    (setq *mvd-side* nil)
    (let ((r (nth-value 1 (values (%mvd-note :a) (%mvd-note :b)))))
      (list r (reverse *mvd-side*))))
  (:b (:a :b)))

(deftest multiple-values-direct.values-list-counts
  (list (multiple-value-list (values-list nil))
        (multiple-value-list (values-list (list 1)))
        (multiple-value-list (values-list (list 1 2)))
        (multiple-value-list (values-list (list 1 2 3)))
        (multiple-value-list (values-list (list 1 2 3 4 5))))
  (nil (1) (1 2) (1 2 3) (1 2 3 4 5)))

(deftest multiple-values-direct.values-list-primary
  (list (values-list (list :a :b :c)) (values-list (list :x)))
  (:a :x))

(deftest multiple-values-direct.values-list-improper
  (handler-case (progn (values-list (cons 1 (cons 2 3))) :no-error)
    (error () :error))
  :error)

;;; The consumers all still see the same values.
(deftest multiple-values-direct.consumers
  (list (multiple-value-bind (a b c) (values-list (list 1 2 3)) (list a b c))
        (multiple-value-bind (a b c) (values 1) (list a b c))
        (multiple-value-call #'list (%mvd-v3) (values 4 5))
        (multiple-value-list (multiple-value-prog1 (%mvd-v3) :ignored))
        (let (a b) (multiple-value-setq (a b) (%mvd-v3)) (list a b))
        (multiple-value-list (values (values 1 2) (values 3 4))))
  ((1 2 3) (1 nil nil) (1 2 3 4 5) (1 2 3) (1 2) (1 3)))

;;; --- the point ---

(defun %mvd-bytes () (nth 4 (dotcl:gc-stats)))

(defmacro %mvd-per-call (name &body body)
  `(progn
     (defun ,name (n)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000)
     (let ((best nil))
       (dotimes (r 5 best)
         (let ((before (%mvd-bytes)))
           (,name 50000)
           (let ((used (- (%mvd-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

(defvar *mvd-x* 7)
(defvar *mvd-l3* (list 1 2 3))

;;; (VALUES x) produces no object, so it allocates nothing.
(deftest-compiled-only multiple-values-direct.values-of-one-allocates-nothing
  (= 0 (%mvd-per-call %mvd-v1 (values *mvd-x*)))
  t)

;;; NTH-VALUE costs what the form itself costs -- no list on the side. Compared
;;; against reading value 0, which needs nothing beyond the call.
(deftest-compiled-only multiple-values-direct.nth-value-adds-nothing
  (<= (abs (- (%mvd-per-call %mvd-n1 (nth-value 1 (%mvd-v3)))
              (%mvd-per-call %mvd-n0 (nth-value 0 (%mvd-v3)))))
      50000)
  t)

;;; VALUES-LIST of three costs what (VALUES a b c) costs.
(deftest-compiled-only multiple-values-direct.values-list-costs-the-same
  (<= (abs (- (%mvd-per-call %mvd-vl (values-list *mvd-l3*))
              (%mvd-per-call %mvd-v3c (values 1 2 3))))
      50000)
  t)
