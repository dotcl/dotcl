;;; REMOVE on a vector or string costs the result and nothing else.
;;;
;;; The indexed path grew a List of every surviving element and copied it out,
;;; and the :FROM-END-with-:COUNT branch put a List of match positions and a
;;; HashSet of dropped indices on top of that. A REMOVE that matched nothing
;;; paid for all of it before finding out the answer was the sequence it was
;;; handed: (remove 9 #(1 2 3)) cost 96 bytes to return its own argument.
;;;
;;; The positions to drop now go into a bitmap -- on the stack for sequences up
;;; to 512 elements -- and the result is filled once at exactly the right size.
;;;
;;; (remove 2 #(1 2 3)) 224 -> 136 bytes, which is the 2-element vector itself;
;;; (remove 9 #(1 2 3)) 96 -> 0.
;;;
;;; Every expected value here was taken from SBCL.

(defun rvb-v () (vector 1 2 3 2 4 2))
(defun rvb-s () (copy-seq "abracadabra"))

(deftest rvb.basic
  (list (coerce (remove 2 (rvb-v)) 'list)
        (coerce (remove 9 (rvb-v)) 'list)
        (coerce (remove 2 (vector)) 'list)
        (coerce (remove 2 (vector 2)) 'list))
  ((1 3 4) (1 2 3 2 4 2) nil nil))

;;; Nothing removed: the argument itself, by identity.
(deftest rvb.nothing-removed-is-eq
  (let ((v (rvb-v))) (eq v (remove 9 v)))
  t)

(deftest rvb.count
  (list (coerce (remove 2 (rvb-v) :count 1) 'list)
        (coerce (remove 2 (rvb-v) :count 0) 'list)
        (coerce (remove 2 (rvb-v) :count 2) 'list)
        (coerce (remove 2 (rvb-v) :count 99) 'list))
  ((1 3 2 4 2) (1 2 3 2 4 2) (1 3 4 2) (1 3 4)))

;;; :FROM-END with :COUNT keeps the rightmost matches.
(deftest rvb.from-end
  (list (coerce (remove 2 (rvb-v) :from-end t :count 1) 'list)
        (coerce (remove 2 (rvb-v) :from-end t :count 2) 'list)
        (coerce (remove 2 (rvb-v) :from-end t :count 0) 'list)
        (coerce (remove 2 (rvb-v) :from-end t :count 99) 'list)
        (coerce (remove 2 (rvb-v) :from-end t) 'list))
  ((1 2 3 2 4) (1 2 3 4) (1 2 3 2 4 2) (1 3 4) (1 3 4)))

(deftest rvb.bounds
  (list (coerce (remove 2 (rvb-v) :start 2) 'list)
        (coerce (remove 2 (rvb-v) :end 3) 'list)
        (coerce (remove 2 (rvb-v) :start 1 :end 4) 'list)
        (coerce (remove 2 (rvb-v) :start 1 :end 4 :count 1) 'list)
        (coerce (remove 2 (rvb-v) :start 1 :end 4 :from-end t :count 1) 'list)
        (coerce (remove 2 (rvb-v) :start 3 :end 3) 'list)
        (coerce (remove 1 (rvb-v) :start 0 :end 0) 'list))
  ((1 2 3 4) (1 3 2 4 2) (1 3 4 2) (1 3 2 4 2) (1 2 3 4 2)
   (1 2 3 2 4 2) (1 2 3 2 4 2)))

(deftest rvb.test-and-key
  (list (coerce (remove 1 (rvb-v) :key #'1-) 'list)
        (coerce (remove 2 (rvb-v) :test #'/=) 'list)
        (coerce (remove 2 (rvb-v) :test-not #'=) 'list))
  ((1 3 4) (2 2 2) (2 2 2)))

(deftest rvb.remove-if
  (list (coerce (remove-if #'evenp (rvb-v)) 'list)
        (coerce (remove-if-not #'evenp (rvb-v)) 'list)
        (coerce (remove-if #'evenp (rvb-v) :count 2) 'list)
        (coerce (remove-if #'evenp (rvb-v) :from-end t :count 2) 'list))
  ((1 3) (2 2 4 2) (1 3 4 2) (1 2 3 2)))

(deftest rvb.string
  (list (remove #\a (rvb-s))
        (remove #\a (rvb-s) :count 2)
        (remove #\a (rvb-s) :from-end t :count 2)
        (remove #\z (rvb-s))
        (remove #\a (rvb-s) :start 3 :end 8))
  ("brcdbr" "brcadabra" "abracadbr" "abracadabra" "abrcdbra"))

;;; A bit vector keeps its element type.
(deftest rvb.bit-vector
  (list (coerce (remove 1 (copy-seq #*10110)) 'list)
        (coerce (remove 0 (copy-seq #*10110) :count 1) 'list)
        (bit-vector-p (remove 1 (copy-seq #*10110))))
  ((0 0) (1 1 1 0) t))

(deftest rvb.delete
  (list (coerce (delete 2 (rvb-v)) 'list)
        (coerce (delete 2 (rvb-v) :count 1) 'list)
        (delete #\a (rvb-s) :count 1))
  ((1 3 4) (1 3 2 4 2) "bracadabra"))

;;; The test sees the elements in the same order it did before, and the same
;;; number of times. Without :FROM-END the scan stops once :COUNT is reached.
(deftest rvb.predicate-call-order
  (list (let ((log '()))
          (remove-if (lambda (x) (push x log) (eql x 2)) (rvb-v) :count 2)
          (reverse log))
        (let ((log '()))
          (remove-if (lambda (x) (push x log) (eql x 2)) (rvb-v) :start 1 :end 5)
          (reverse log)))
  ((1 2 3 2) (2 3 2 4)))
