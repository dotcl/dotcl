;;; (MASK-FIELD (BYTE S 0) V) with a size S that is not a literal but is proved to lie in [0, 64]
;;; (the shape of babel's and jzon's unsigned right shift, a size of 64 - count) is computed in the
;;; 64-bit lane: the low S bits of V. Every size from 0 to 64 is compared with the same value taken
;;; through LOGAND and ASH, for words of every kind: boundaries, negative numbers, bignums, and a
;;; million pseudo-random words.

(setf dotcl:*save-sil* t)

(defun mfl-typed (v count)
  (declare (type (unsigned-byte 64) v) (type (integer 0 64) count))
  (mask-field (byte count 0) v))
(defun mfl-from-count (v c)
  (declare (type (unsigned-byte 64) v) (type fixnum c))
  (let ((n (logand c 63)))
    (mask-field (byte (- 64 n) 0) (ash v (- n)))))
(defun mfl-untyped (v count)
  (declare (type (integer 0 64) count))
  (mask-field (byte count 0) v))
(defun mfl-shifted (v count)
  (declare (type (integer 0 64) count))
  (+ 1 (mask-field (byte count 0) (ash v -3))))
(defun mfl-unproved (v count)
  (mask-field (byte count 0) v))
(defun mfl-literal (v) (mask-field (byte 12 0) v))

(setf dotcl:*save-sil* nil)

(defun %mfl-ref (v count) (logand v (1- (ash 1 count))))

(defparameter *mfl-words*
  (let ((base (list 0 1 2 3 #x7f #x80 #xff #xffff #x10000 #x7fffffff #x80000000 #xffffffff
                    #x100000000 (1- (ash 1 62)) (ash 1 62) (1- (ash 1 63)) (ash 1 63)
                    (1+ (ash 1 63)) (1- (ash 1 64)) #x5555555555555555 #xAAAAAAAAAAAAAAAA
                    #x0123456789ABCDEF)))
    base))

(deftest mask-field-size-lane.every-size-for-a-typed-word
  (let ((bad nil))
    (dolist (v *mfl-words*)
      (dotimes (n 65)
        (unless (eql (mfl-typed v n) (%mfl-ref v n)) (when (< (length bad) 3) (push (list v n) bad)))))
    bad)
  nil)

(deftest mask-field-size-lane.every-size-for-any-integer
  (let ((bad nil)
        (words (append *mfl-words* (mapcar #'- *mfl-words*) (list (ash 1 64) (ash 1 70) (- (ash 1 70)) (1- (ash 1 100))))))
    (dolist (v words)
      (dotimes (n 65)
        (unless (eql (mfl-untyped v n) (%mfl-ref v n)) (when (< (length bad) 3) (push (list v n) bad)))))
    bad)
  nil)

(deftest mask-field-size-lane.a-right-shift-by-a-count
  (let ((bad nil))
    (dolist (v *mfl-words*)
      (dotimes (c 130)
        (let* ((n (logand c 63)))
          (unless (eql (mfl-from-count v c) (%mfl-ref (ash v (- n)) (- 64 n)))
            (when (< (length bad) 3) (push (list v c) bad))))))
    bad)
  nil)

(deftest mask-field-size-lane.shifted-operand-and-use-of-the-result
  (let ((bad nil))
    (dolist (v (append *mfl-words* (mapcar #'- *mfl-words*)))
      (dotimes (n 65)
        (unless (eql (mfl-shifted v n) (+ 1 (%mfl-ref (ash v -3) n)))
          (when (< (length bad) 3) (push (list v n) bad)))))
    bad)
  nil)

(deftest mask-field-size-lane.an-unproved-size-is-left-alone
  (list (mfl-unproved #xFFFF 8) (mfl-unproved -1 70) (mfl-unproved (ash 1 70) 0)
        (handler-case (mfl-unproved 1 -1) (error () :error))
        (handler-case (mfl-unproved 1.5 8) (error () :error))
        (mfl-literal #xFFFFF))
  (255 #.(1- (ash 1 70)) 0 :error :error #xFFF))

(deftest mask-field-size-lane.a-million-random-words
  (let ((x 88172645463325252) (bad nil))
    (dotimes (i 1000000)
      (setq x (ldb (byte 64 0) (+ (* x 6364136223846793005) 1442695040888963407)))
      (let ((n (mod i 65)))
        (unless (and (eql (mfl-typed x n) (%mfl-ref x n))
                     (eql (mfl-from-count x i) (%mfl-ref (ash x (- (logand i 63))) (- 64 (logand i 63)))))
          (when (< (length bad) 3) (push (list x n i) bad)))))
    bad)
  nil)

(deftest-emitting-only mask-field-size-lane.the-proved-forms-make-no-generic-call
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.MaskLow64" #'mfl-typed) (has "MASK-FIELD" #'mfl-typed)
          (has "Runtime.MaskLow64" #'mfl-from-count) (has "MASK-FIELD" #'mfl-from-count)
          (has "Runtime.MaskLow64" #'mfl-unproved)))
  (t nil t nil nil))
