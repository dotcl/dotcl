;;; An INLINE function whose typed array parameters are bound to the caller's
;;; arrays: the LET of each expansion shares the caller's buffers, and the
;;; caller's single test (its fast copy and its helper copy) covers them. The
;;; expansions are not unswitched again, which used to emit each expansion's
;;; body twice more inside both copies (ironclad's SKEIN-UBI shrank by a fifth).

(setf dotcl:*save-sil* t)

(declaim (inline %iau-step))
(defun %iau-step (n ck tw)
  (declare (type fixnum n) (type (simple-array (unsigned-byte 64) (*)) ck)
           (type (simple-array (unsigned-byte 64) (2)) tw)
           (optimize (speed 3) (safety 0)))
  (loop for i of-type fixnum from 0 below n do
    (setf (aref ck i) (logxor (aref ck i) (aref tw 0))))
  (setf (aref tw 1) (logxor (aref tw 0) (aref tw 1)))
  (setf (aref ck 1) (logxor (aref ck 2) (aref tw 1) (aref ck 3) (aref ck 4) (aref tw 0))
        (aref ck 5) (logxor (aref ck 6) (aref tw 1) (aref ck 7) (aref ck 0) (aref tw 0))
        (aref ck 2) (logxor (aref ck 1) (aref tw 1) (aref ck 3) (aref ck 4) (aref tw 0))
        (aref ck 3) (logxor (aref ck 6) (aref tw 1) (aref ck 7) (aref ck 0) (aref tw 0)))
  (setf (aref tw 0) (logand (+ (aref tw 0) (aref ck 0) (aref ck 1) (aref ck 2) (aref ck 3))
                            #xffffffffffffffff))
  (values))

(defun %iau-run (ck tw m)
  (declare (type (simple-array (unsigned-byte 64) (*)) ck)
           (type (simple-array (unsigned-byte 64) (2)) tw)
           (type fixnum m) (optimize (speed 3) (safety 0)))
  (loop repeat m do (%iau-step 8 ck tw) (%iau-step 8 ck tw) (%iau-step 8 ck tw))
  (aref ck 0))

(defun %iau-run-by-hand (ck tw m)
  (dotimes (i (* 3 m))
    (dotimes (j 8) (setf (aref ck j) (logxor (aref ck j) (aref tw 0))))
    (setf (aref tw 1) (logxor (aref tw 0) (aref tw 1)))
    (setf (aref ck 1) (logxor (aref ck 2) (aref tw 1) (aref ck 3) (aref ck 4) (aref tw 0))
          (aref ck 5) (logxor (aref ck 6) (aref tw 1) (aref ck 7) (aref ck 0) (aref tw 0))
          (aref ck 2) (logxor (aref ck 1) (aref tw 1) (aref ck 3) (aref ck 4) (aref tw 0))
          (aref ck 3) (logxor (aref ck 6) (aref tw 1) (aref ck 7) (aref ck 0) (aref tw 0)))
    (setf (aref tw 0) (logand (+ (aref tw 0) (aref ck 0) (aref ck 1) (aref ck 2) (aref ck 3))
                              #xffffffffffffffff)))
  (aref ck 0))

(defun %iau-arrays ()
  (values (make-array 8 :element-type '(unsigned-byte 64)
                        :initial-contents '(3 1 4 1 5 9 2 6))
          (make-array 2 :element-type '(unsigned-byte 64) :initial-contents '(5 7))))

(deftest inline-alias-unswitch.same-result
  (multiple-value-bind (ck tw) (%iau-arrays)
    (multiple-value-bind (ck2 tw2) (%iau-arrays)
      (let ((a (%iau-run ck tw 4)) (b (%iau-run-by-hand ck2 tw2 4)))
        (list (= a b) (equalp ck ck2) (equalp tw tw2)))))
  (t t t))

(defun %iau-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(deftest-compiled-only inline-alias-unswitch.one-unswitch
  (%iau-count "(LABEL ABUSLOW" (%flat-text (dotcl:function-sil #'%iau-run)))
  1)
