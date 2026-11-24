;;; Allocation is asserted only on emitted code, so the measurement is compiled-only.

(load "test/regression/assoc-member-find-allocation-rows.lisp")

(defun %amf-baseline ()
  (min-allocated-bytes
   (lambda () (dotimes (i 1000) (setq *amf-sink* nil)))
   5))

(defun %amf-check (rows)
  (let ((base (%amf-baseline)) (bad nil))
    (dolist (row rows (nreverse bad))
      (destructuring-bind (id op input variant position floor above fn) row
        (declare (ignore op input variant position))
        (unless above
          (funcall fn)
          (let ((got (- (min-allocated-bytes fn 5) base)))
            (when (> got (+ (* 1000 floor) 16))
              (push (list id got (* 1000 floor)) bad))))))))

(deftest-compiled-only assoc-member-find-allocation.floor
  (%amf-check *amf-rows*)
  nil)

;;; Behaviour of the changed paths, including the calls the test functions see.

(defparameter *amf-calls* nil)

(defun %amf-record (x)
  (push x *amf-calls*)
  (eql x 3))

(defun %amf-record-key (x)
  (push x *amf-calls*)
  x)

(deftest assoc-member-find-allocation.if-not-results
  (list (find-if-not #'%amf-never *amf-fixnums*)
        (position-if-not #'%amf-never *amf-fixnums*)
        (count-if-not #'%amf-never *amf-vector*)
        (member-if-not #'%amf-never *amf-fixnums*)
        (assoc-if-not #'%amf-never *amf-alist*)
        (rassoc-if-not #'%amf-never *amf-alist*))
  (1 0 8 (1 2 3 4 5 6 7 8) (a . 1) (a . 1)))

(deftest assoc-member-find-allocation.if-not-records
  (let ((*amf-calls* nil))
    (list (find-if-not #'%amf-record '(1 2 3 4))
          (nreverse *amf-calls*)))
  (1 (1)))

(deftest assoc-member-find-allocation.count-if-not-records
  (let ((*amf-calls* nil))
    (list (count-if-not #'%amf-record '(1 2 3 4))
          (nreverse *amf-calls*)))
  (3 (1 2 3 4)))

(deftest assoc-member-find-allocation.if-not-key-results
  (list (find-if-not #'%amf-never *amf-cons-list* :key #'car)
        (position-if-not #'%amf-never *amf-cons-list* :key #'car)
        (count-if-not #'%amf-never *amf-cons-list* :key #'car)
        (member-if-not #'%amf-never *amf-cons-list* :key #'car))
  ((a . 1) 0 8 ((a . 1) (b . 2) (c . 3) (d . 4)
                 (e . 5) (f . 6) (g . 7) (h . 8))))

(deftest assoc-member-find-allocation.if-not-key-records
  (let ((*amf-calls* nil))
    (list (count-if-not #'%amf-never *amf-fixnums* :key #'%amf-record-key)
          (nreverse *amf-calls*)))
  (8 (1 2 3 4 5 6 7 8)))

(deftest assoc-member-find-allocation.if-not-start-end-from-end
  (list (find-if-not #'%amf-never *amf-vector* :key #'identity :start 2)
        (find-if-not #'%amf-never *amf-vector* :key #'identity :from-end t)
        (position-if-not #'%amf-never *amf-vector* :key #'identity :from-end t)
        (count-if-not #'%amf-never *amf-vector* :key #'identity :from-end t)
        (find-if-not #'%amf-never *amf-vector* :start 2 :end 6)
        (position-if-not #'%amf-never *amf-vector* :start 2 :end 6)
        (count-if-not #'%amf-never *amf-vector* :start 2 :end 6))
  (3 8 7 8 3 2 4))

(deftest assoc-member-find-allocation.if-not-apply-agrees
  (list (find-if-not #'%amf-never *amf-vector* :key #'identity :start 2 :end 6)
        (apply #'find-if-not #'%amf-never *amf-vector*
               (list :key #'identity :start 2 :end 6))
        (position-if-not #'%amf-never *amf-vector* :key #'identity :from-end t)
        (apply #'position-if-not #'%amf-never *amf-vector*
               (list :key #'identity :from-end t))
        (count-if-not #'%amf-never *amf-vector* :key #'identity :start 2 :end 6)
        (apply #'count-if-not #'%amf-never *amf-vector*
               (list :key #'identity :start 2 :end 6)))
  (3 3 7 7 4 4))

(deftest assoc-member-find-allocation.count-from-end-list-records
  (let ((*amf-calls* nil))
    (list (count 1 '(1 2 1 3 1)
                 :test (lambda (a b) (push (list a b) *amf-calls*) (eql a b))
                 :from-end t)
          (nreverse *amf-calls*)))
  (3 ((1 1) (1 3) (1 1) (1 2) (1 1))))

(deftest assoc-member-find-allocation.count-if-from-end-list-records
  (let ((*amf-calls* nil))
    (list (count-if (lambda (x) (push x *amf-calls*) (eql x 1))
                    '(1 2 1 3 1) :from-end t)
          (nreverse *amf-calls*)))
  (3 (1 3 1 2 1)))

(deftest assoc-member-find-allocation.remove-duplicates-results
  (list (remove-duplicates '(1 2 2 3 1))
        (remove-duplicates '(1 2 2 3 1) :from-end t)
        (remove-duplicates '(0 1 2 2 3 1 9) :start 1 :end 6)
        (remove-duplicates '((a . 1) (b . 2) (a . 3)) :key #'car)
        (remove-duplicates '(1 2 3 4) :test-not #'eql)
        (remove-duplicates "aabbcc" :test #'char-equal)
        (length (remove-duplicates (loop for i below 20 collect (mod i 5)))))
  ((2 3 1) (1 2 3) (0 2 3 1 9) ((b . 2) (a . 3)) (4) "abc" 5))

(deftest assoc-member-find-allocation.remove-duplicates-records
  (let ((*amf-calls* nil))
    (list (remove-duplicates '(1 2 2 3 1)
                             :test (lambda (a b)
                                     (push (list a b) *amf-calls*)
                                     (eql a b)))
          (nreverse *amf-calls*)))
  ((2 3 1) ((1 2) (1 2) (1 3) (1 1) (2 2) (2 3) (2 1) (3 1))))

(deftest assoc-member-find-allocation.remove-duplicates-no-match-eq
  (let ((input (list 1 2 3 4)))
    (eq input (remove-duplicates input)))
  t)
