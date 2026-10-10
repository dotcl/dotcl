;;; EQL, EQUAL and EQUALP of two operands known to be fixnums compare the raw
;;; integers, like =. A structure slot declared FIXNUM is stored raw, and the
;;; generic entries take objects, so each comparison used to box both slot
;;; values first (EQUALP also went through a full call by symbol). A type
;;; checker that compares type variables by id did this on every candidate.
;;;
;;; The comparison is also branched on directly when it is an intermediate
;;; argument of an AND in value position, instead of building T or NIL and
;;; asking whether that is true. The AND's own value is unchanged: NIL as a
;;; single value when a test fails, the last argument's values otherwise.

(setf dotcl:*save-sil* t)

(defun %fxeq-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %fxeq-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defstruct fxeq
  (id 0 :type fixnum :read-only t)
  (kind nil :read-only t)
  (n 0 :type fixnum))

(defun %fxeq-eql (a b) (if (eql (fxeq-id a) (fxeq-id b)) :same :differ))
(defun %fxeq-equal (a b) (if (equal (fxeq-id a) (fxeq-n b)) :same :differ))
(defun %fxeq-equalp (a b) (if (equalp (fxeq-n a) (fxeq-n b)) :same :differ))
(defun %fxeq-and (a b)
  (and (equalp (fxeq-id a) (fxeq-id b))
       (eq (fxeq-kind a) (fxeq-kind b))
       (fxeq-kind a)))
(defun %fxeq-and-values (a b)
  (and (eql (fxeq-id a) (fxeq-id b)) (values (fxeq-kind a) (fxeq-n a))))
(defun %fxeq-and-generic (x) (and (= x 1) (values :one 1)))
(defun %fxeq-two-values () (values 2 3))

(deftest fixnum-eql-equalp-compare.results
  (let ((big (make-fxeq :id most-positive-fixnum :kind :k :n most-positive-fixnum))
        (big2 (make-fxeq :id most-positive-fixnum :kind :k :n (1- most-positive-fixnum)))
        (neg (make-fxeq :id most-negative-fixnum :kind :j :n -5))
        (neg2 (make-fxeq :id most-negative-fixnum :kind :k :n -5))
        (mid (make-fxeq :id 100000 :kind :k :n 100000)))
    (list (%fxeq-eql big big2) (%fxeq-eql big neg) (%fxeq-eql neg neg2)
          (%fxeq-equal mid mid) (%fxeq-equal big mid)
          (%fxeq-equalp neg neg2) (%fxeq-equalp big big2)))
  (:same :differ :same :same :differ :same :differ))

(deftest fixnum-eql-equalp-compare.and-values
  (let ((a (make-fxeq :id 123456789 :kind :star :n 7))
        (b (make-fxeq :id 123456789 :kind :star :n 8))
        (c (make-fxeq :id 123456789 :kind :arrow :n 8))
        (d (make-fxeq :id 987654321 :kind :star :n 8)))
    (list (multiple-value-list (%fxeq-and a b))
          (multiple-value-list (%fxeq-and a c))
          (multiple-value-list (%fxeq-and a d))
          (multiple-value-list (%fxeq-and-values a b))
          (multiple-value-list (%fxeq-and-values a d))
          (multiple-value-list (%fxeq-and-generic 1))
          (multiple-value-list (%fxeq-and-generic 2))
          (multiple-value-list (and (= (%fxeq-two-values) 1) t))))
  ((:star) (nil) (nil) (:star 7) (nil) (:one 1) (nil) (nil)))

(deftest-emitting-only fixnum-eql-equalp-compare.raw-compare
  (list (%fxeq-count "Runtime.StructRefI" (%fxeq-sil #'%fxeq-eql))
        (%fxeq-count "Runtime.StructRefI" (%fxeq-sil #'%fxeq-equal))
        (%fxeq-count "Runtime.StructRefI" (%fxeq-sil #'%fxeq-equalp))
        (%fxeq-count "\"EQUALP\"" (%fxeq-sil #'%fxeq-equalp))
        (%fxeq-count "Runtime.StructRefL" (%fxeq-sil #'%fxeq-and))
        (%fxeq-count "\"EQUALP\"" (%fxeq-sil #'%fxeq-and)))
  (0 0 0 0 2 0))
