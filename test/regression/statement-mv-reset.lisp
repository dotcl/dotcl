;;; Regression tests for the multiple-values reset between statements. A body's
;;; forms before the last were each followed by MultipleValues.Reset, so values
;;; one of them published could not reach the forms after it or whoever reads
;;; the body's values. A statement that cannot touch the values channel (raw
;;; arithmetic, an array or slot store) now gets no reset after it; the reset is
;;; placed once, before the first form after a statement that can. In a large
;;; typed body (ironclad's SALSA core) the JIT does not inline the reset, and
;;; one call per store was most of what was left in the round.

(setf dotcl:*save-sil* t)

(defun %smr-reset-count (fn)
  (let ((text (%flat-text (dotcl:function-sil fn))) (n 0) (start 0))
    (loop (let ((p (search "MultipleValues.Reset" text :start2 start)))
            (unless p (return n))
            (incf n)
            (setq start (1+ p))))))

;; One store and four stores in the same body: the stores add no reset.
(defun %smr-one (x)
  (declare (type (simple-array (unsigned-byte 32) (16)) x) (optimize speed))
  (setf (aref x 0) (logxor (aref x 1) (aref x 2)))
  x)
(defun %smr-four (x)
  (declare (type (simple-array (unsigned-byte 32) (16)) x) (optimize speed))
  (setf (aref x 0) (logxor (aref x 1) (aref x 2))
        (aref x 3) (logand #xffffffff (+ (aref x 0) (aref x 2)))
        (aref x 4) (logxor (aref x 3) (aref x 1)))
  (setf (aref x 5) (logand #xffffffff (+ (aref x 4) (aref x 3))))
  x)

(deftest-compiled-only smr-stores-add-no-reset
  (= (%smr-reset-count #'%smr-one) (%smr-reset-count #'%smr-four))
  t)

;; The struct-slots shape: a counted loop that stores a slot every iteration.
(defstruct (%smr-pt (:constructor %make-smr-pt (a b)))
  (a 0 :type fixnum)
  (b 0 :type fixnum))
(defun %smr-loop (p n)
  (declare (type %smr-pt p) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (%smr-pt-a p)) (the fixnum (%smr-pt-b p)))))
      (setf (%smr-pt-a p) i))))
(defun %smr-loop-nostore (p n)
  (declare (type %smr-pt p) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (%smr-pt-a p)) (the fixnum (%smr-pt-b p))))))))

(deftest-compiled-only smr-slot-store-adds-no-reset
  (= (%smr-reset-count #'%smr-loop) (%smr-reset-count #'%smr-loop-nostore))
  t)

(deftest smr-values
  (let ((x (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0))
        (p (%make-smr-pt 1 2)))
    (dotimes (i 16) (setf (aref x i) (* i 3)))
    (%smr-four x)
    (list (coerce (subseq x 0 6) 'list) (%smr-loop p 4) (%smr-pt-a p)))
  ((5 3 6 11 8 19) 12 3))

;;; ---- values published before the stores do not leak past them ----

(defvar *smr-v* (make-array 4 :element-type '(unsigned-byte 32) :initial-element 0))

(defun %smr-mv-1 ()
  (multiple-value-list (progn (floor 7 2) (setf (aref *smr-v* 0) 1) 3)))
(defun %smr-mv-2 (v)
  (declare (type (simple-array (unsigned-byte 32) (4)) v))
  (multiple-value-list (progn (values 3 4) (setf (aref v 0) 1 (aref v 1) 2) 3)))
(defun %smr-mv-3 (p)
  (declare (type %smr-pt p) (optimize (safety 0)))
  (multiple-value-list (progn (floor 7 2) (setf (%smr-pt-a p) 5) 3)))
(defun %smr-mv-4 (v)
  (declare (type (simple-array (unsigned-byte 32) (4)) v))
  (multiple-value-list (block b (floor 7 2) (setf (aref v 0) 1) (return-from b 3))))
(defun %smr-mv-5 (v)
  (declare (type (simple-array (unsigned-byte 32) (4)) v))
  (let ((r '()))
    (dotimes (i 2) (floor 7 2) (setf (aref v i) i) (push (multiple-value-list 3) r))
    r))
(defun %smr-mv-6 (v n)
  (declare (type (simple-array (unsigned-byte 32) (4)) v) (fixnum n))
  (multiple-value-list (progn (floor 7 2) (setf (aref v 0) n) (setf (aref v 1) n) n)))
(defun %smr-mv-7 (v)
  (declare (type (simple-array (unsigned-byte 32) (4)) v))
  (multiple-value-list (let ((a (floor 7 2))) (setf (aref v 0) a) (setf (aref v 1) 2) 3)))

(deftest smr-no-values-leak
  (list (%smr-mv-1) (%smr-mv-2 *smr-v*) (%smr-mv-3 (%make-smr-pt 0 0))
        (%smr-mv-4 *smr-v*) (%smr-mv-5 *smr-v*) (%smr-mv-6 *smr-v* 3)
        (%smr-mv-7 *smr-v*))
  ((3) (3) (3) (3) ((3) (3)) (3) (3)))

;; Values published by the last statement before the tail form are still
;; emptied, and a store between two value-publishing forms keeps the second
;; one's values intact for its reader.
(defun %smr-mv-8 (v)
  (declare (type (simple-array (unsigned-byte 32) (4)) v))
  (list (multiple-value-list (progn (setf (aref v 0) 1) (floor 7 2)))
        (multiple-value-list (progn (floor 9 2) (setf (aref v 0) 1) (truncate 7 2)))
        (multiple-value-list (progn (setf (aref v 0) 1) (values)))))

(deftest smr-tail-values-kept
  (%smr-mv-8 *smr-v*)
  ((3 1) (3 1) ()))
