;;; Above (safety 0), the buffer of a variable declared (SIMPLE-ARRAY
;;; DOUBLE-FLOAT / SINGLE-FLOAT (*)) is fetched by an entry that signals a
;;; TYPE-ERROR for a vector of another element type, so the loop is emitted once,
;;; with no copy for a missing buffer.

(defun %fadf-sumd (a)
  (declare (type (simple-array double-float (*)) a))
  (let ((s 0d0))
    (dotimes (i (length a) s) (incf s (aref a i)))))

(defun %fadf-sumf (a)
  (declare (type (simple-array single-float (*)) a))
  (let ((s 0f0))
    (dotimes (i (length a) s) (incf s (aref a i)))))

(defun %fadf-scale (a b)
  (declare (type (simple-array single-float (8)) a b))
  (prog ((x a) (y b) (i 0))
    (declare (fixnum i))
   top
    (when (< i 8)
      (setf (aref y i) (* 2f0 (aref x i)))
      (setq i (1+ i))
      (go top))
    (return (aref y 7))))

(deftest float-array-decl-fetch.values
  (list (%fadf-sumd (make-array 0 :element-type 'double-float))
        (%fadf-sumd (coerce '(1d0 2d0) '(vector double-float)))
        (%fadf-sumd (subseq (make-array 4 :element-type 'double-float :initial-element 1d0) 1))
        (%fadf-sumd (concatenate '(vector double-float) #(1d0) #(2d0)))
        (%fadf-sumd (make-array 2 :element-type 'long-float :initial-element 1l0))
        (%fadf-sumf (copy-seq (make-array 3 :element-type 'single-float :initial-element 2f0)))
        (%fadf-sumf (map '(vector single-float) #'identity '(1f0 2f0)))
        (%fadf-sumf (make-array 2 :element-type 'short-float :initial-element 1s0))
        (let ((a (make-array 8 :element-type 'single-float :initial-element 1.5f0))
              (b (make-array 8 :element-type 'single-float)))
          (list (%fadf-scale a b) (aref b 0))))
  (0d0 3d0 3d0 3d0 2d0 6f0 3f0 2f0 (3f0 3f0)))

(deftest float-array-decl-fetch.false-declaration
  (list (handler-case (%fadf-sumd (vector 1d0 2d0))
          (type-error (e) (list :type-error (type-error-expected-type e))))
        (handler-case (%fadf-sumf (make-array 2 :element-type 'double-float))
          (type-error (e) (list :type-error (type-error-expected-type e))))
        (handler-case (%fadf-sumd (make-array 2 :element-type 'double-float :adjustable t))
          (type-error () :type-error)))
  ((:type-error (simple-array double-float (*)))
   (:type-error (simple-array single-float (*)))
   :type-error))

(defvar *fadf-saved-save-sil* dotcl:*save-sil*)
(setf dotcl:*save-sil* t)
(defun %fadf-shape (a b)
  (declare (type (simple-array single-float (*)) a b))
  (let ((x a))
    (dotimes (i (length b))
      (setf (aref b i) (* 2f0 (aref x i) (aref a i))))))
(setf dotcl:*save-sil* *fadf-saved-save-sil*)

(defun %fadf-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

;; One copy of the loop: two element loads and one store, no helper.
(deftest-emitting-only float-array-decl-fetch.one-copy
  (let ((s (%flat-text (dotcl:function-sil #'%fadf-shape))))
    (list (%fadf-count "LDELEM-R4" s)
          (%fadf-count "STELEM-R4" s)
          (%fadf-count "ArefNumD" s)
          (%fadf-count "ArefSetNumD" s)))
  (2 1 0 0))
