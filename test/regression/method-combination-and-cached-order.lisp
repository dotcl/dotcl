;;; A generic function with an operator method combination (AND, +, LIST, ...)
;;; re-partitioned and re-sorted its applicable methods on every call, even on a
;;; dispatch cache hit. The sorted lists are now kept in the cache entry. The
;;; order and the short-circuit must stay the same on the hits as on the first
;;; call, for each argument class, and with :AROUND methods.

(defclass mcac-a () ())
(defclass mcac-b (mcac-a) ())
(defclass mcac-c (mcac-b) ())
(defclass mcac-d (mcac-a) ())

(defvar *mcac-trace* nil)

(defgeneric mcac-and (x n &rest rest) (:method-combination and))
(defmethod mcac-and and ((x mcac-a) n &rest rest)
  (declare (ignore rest)) (push :a *mcac-trace*) (list :a n))
(defmethod mcac-and and ((x mcac-b) n &rest rest)
  (declare (ignore rest)) (push :b *mcac-trace*) (not (eql n 0)))
(defmethod mcac-and and ((x mcac-c) n &rest rest)
  (declare (ignore rest)) (push :c *mcac-trace*) t)
(defmethod mcac-and and ((x mcac-d) n &rest rest)
  (declare (ignore rest)) (push :d *mcac-trace*) t)

(defun %mcac-run (obj n)
  (let ((*mcac-trace* nil))
    (let ((v (mcac-and obj n)))
      (list v (reverse *mcac-trace*)))))

(deftest method-combination-and-cached-order
  ;; Each call twice: the second one is a cache hit.
  (let ((c (make-instance 'mcac-c)) (d (make-instance 'mcac-d)))
    (list (%mcac-run c 1) (%mcac-run c 1)
          (%mcac-run c 0) (%mcac-run c 0)
          (%mcac-run d 2) (%mcac-run d 2)))
  (((:a 1) (:c :b :a)) ((:a 1) (:c :b :a))
   (nil (:c :b)) (nil (:c :b))
   ((:a 2) (:d :a)) ((:a 2) (:d :a))))

(defgeneric mcac-list (x) (:method-combination list :most-specific-last))
(defmethod mcac-list list ((x mcac-a)) :a)
(defmethod mcac-list list ((x mcac-b)) :b)
(defmethod mcac-list list ((x mcac-c)) :c)

(deftest method-combination-list-most-specific-last-cached
  (let ((c (make-instance 'mcac-c)) (b (make-instance 'mcac-b)))
    (list (mcac-list c) (mcac-list c) (mcac-list b) (mcac-list b)))
  ((:a :b :c) (:a :b :c) (:a :b) (:a :b)))

(defgeneric mcac-sum (x) (:method-combination +))
(defmethod mcac-sum + ((x mcac-a)) 1)
(defmethod mcac-sum + ((x mcac-b)) 10)
(defmethod mcac-sum :around ((x mcac-b)) (list :outer (call-next-method)))
(defmethod mcac-sum :around ((x mcac-c)) (list :inner (call-next-method)))

(deftest method-combination-around-cached
  (let ((c (make-instance 'mcac-c)) (a (make-instance 'mcac-a)))
    (list (mcac-sum c) (mcac-sum c) (mcac-sum a) (mcac-sum a)))
  ((:inner (:outer 11)) (:inner (:outer 11)) 1 1))

;; Adding a method drops the cached lists.
(deftest method-combination-and-cache-invalidated
  (let ((c (make-instance 'mcac-c)))
    (mcac-sum c)
    (defmethod mcac-sum + ((x mcac-c)) 100)
    (prog1 (list (mcac-sum c) (mcac-sum c))
      (remove-method #'mcac-sum (find-method #'mcac-sum '(+) (list (find-class 'mcac-c))))))
  ((:inner (:outer 111)) (:inner (:outer 111))))

(defun %mcac-bytes () (nth 4 (dotcl:gc-stats)))

;; A hit does not allocate per applicable method (it used to sort a fresh
;; list with a fresh comparer every call: about 400 bytes per call here,
;; against about 70 now).
(defgeneric mcac-pure (x n) (:method-combination and))
(defmethod mcac-pure and ((x mcac-a) n) n)
(defmethod mcac-pure and ((x mcac-b) n) n)
(defmethod mcac-pure and ((x mcac-c) n) n)

(deftest-compiled-only method-combination-and-hit-no-alloc
  (let ((c (make-instance 'mcac-c)))
    (dotimes (k 10) (mcac-pure c 1))
    (let ((b0 (%mcac-bytes)))
      (dotimes (k 10000) (mcac-pure c 1))
      (< (/ (- (%mcac-bytes) b0) 10000) 160)))
  t)
