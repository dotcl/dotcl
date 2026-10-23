;;; A function proclaimed INLINE whose body is over the small-body size limit is
;;; substituted in the body of a DEFUN, unless the DEFUN's method would then be
;;; past the JIT's limits; then the DEFUN is compiled again with calls. The
;;; shape is ironclad's CAMELLIA-FEISTEL, called once per round of a block.

(setf dotcl:*save-sil* t)

(declaim (inline %ilb-feistel))
(defun %ilb-feistel (data data-start keys keys-start)
  (declare (type (simple-array (unsigned-byte 32) (*)) data keys)
           (type fixnum data-start keys-start)
           (optimize (speed 3) (safety 0)))
  (macrolet ((sbox (n k) `(logand (* (logxor (logand ,n 255) ,k) 2654435761) #xffffffff)))
    (let ((u 0) (d 0) (s1 0) (s2 0))
      (declare (type (unsigned-byte 32) u d s1 s2))
      (setf s1 (logxor (aref data data-start) (aref keys keys-start))
            u (logxor (sbox s1 1) (sbox (ash s1 -8) 2) (sbox (ash s1 -16) 3) (sbox (ash s1 -24) 4))
            s2 (logxor (aref data (+ data-start 1)) (aref keys (+ keys-start 1)))
            d (logxor (sbox s2 4) (sbox (ash s2 -8) 1) (sbox (ash s2 -16) 2) (sbox (ash s2 -24) 3)))
      (setf (aref data (+ data-start 2)) (logxor (aref data (+ data-start 2)) d u)
            (aref data (+ data-start 3)) (logxor (aref data (+ data-start 3)) d
                                                 (logior (ash u -8) (logand (ash u 24) #xffffffff))))
      (setf s1 (logxor (aref data (+ data-start 2)) (aref keys (+ keys-start 2)))
            u (logxor (sbox s1 2) (sbox (ash s1 -8) 3) (sbox (ash s1 -16) 4) (sbox (ash s1 -24) 1)))
      (setf (aref data data-start) (logxor (aref data data-start) u)
            (aref data (+ data-start 1)) (logxor (aref data (+ data-start 1)) u (ash u -3)))
      (values))))

(defun %ilb-block (keys rounds)
  (declare (type (simple-array (unsigned-byte 32) (*)) keys)
           (type fixnum rounds))
  (let ((data (make-array 4 :element-type '(unsigned-byte 32)
                            :initial-contents '(1 2 3 4)))
        (k 0))
    (declare (type fixnum k))
    (dotimes (i rounds)
      (%ilb-feistel data 0 keys k)
      (setf k (mod (+ k 3) 16)))
    (coerce data 'list)))

(defun %ilb-keys ()
  (make-array 20 :element-type '(unsigned-byte 32)
                 :initial-contents (loop for i below 20 collect (logand (* (1+ i) 2246822519) #xffffffff))))

(defmacro %ilb-many (n)
  `(progn ,@(loop for i below n collect `(%ilb-feistel data 0 keys ,(mod i 16)))))
(defun %ilb-many-calls (keys)
  (declare (type (simple-array (unsigned-byte 32) (*)) keys))
  (let ((data (make-array 4 :element-type '(unsigned-byte 32) :initial-contents '(1 2 3 4))))
    (%ilb-many 150)
    (coerce data 'list)))
(defun %ilb-in-lambda (keys)
  (let ((data (make-array 4 :element-type '(unsigned-byte 32) :initial-contents '(1 2 3 4))))
    (funcall (lambda () (%ilb-feistel data 0 keys 5)))
    (coerce data 'list)))

(deftest inline-large-body.values
  (list (%ilb-block (%ilb-keys) 1) (%ilb-block (%ilb-keys) 9))
  ((3556102010 3381360278 3937437941 2002206013)
   (620751384 543168344 273623870 2788679740)))

(deftest inline-large-body.no-values
  (multiple-value-list
   (let ((d (make-array 4 :element-type '(unsigned-byte 32) :initial-element 5)))
     (%ilb-feistel d 0 (%ilb-keys) 2)))
  nil)

(deftest inline-large-body.many-calls
  (%ilb-many-calls (%ilb-keys))
  (1449775736 1554311220 3389882761 847252080))

(deftest inline-large-body.in-lambda
  (%ilb-in-lambda (%ilb-keys))
  (1871476792 1652422460 190021042 2420526600))

(deftest-emitting-only inline-large-body.substituted
  (search "%ILB-FEISTEL" (%flat-text (dotcl:function-sil #'%ilb-block)))
  nil)

;;; 150 copies would take the method past the JIT's limits.
(deftest-emitting-only inline-large-body.too-many-stay-calls
  (not (search "%ILB-FEISTEL" (%flat-text (dotcl:function-sil #'%ilb-many-calls))))
  nil)

;;; The size the substitution charges against the caller's budget is the
;;; inlined function's own compiled size.
(deftest-emitting-only inline-large-body.size-recorded
  (integerp (gethash '%ilb-feistel dotcl-internal::*inline-def-il-sizes*))
  t)

;;; Six calls written in the body do not all fit the budget: none is substituted,
;;; not the first few (substituting only some was measured slower than none).
(defun %ilb-six (keys)
  (declare (type (simple-array (unsigned-byte 32) (*)) keys))
  (let ((data (make-array 4 :element-type '(unsigned-byte 32) :initial-contents '(1 2 3 4))))
    (%ilb-feistel data 0 keys 0) (%ilb-feistel data 0 keys 3) (%ilb-feistel data 0 keys 6)
    (%ilb-feistel data 0 keys 9) (%ilb-feistel data 0 keys 12) (%ilb-feistel data 0 keys 15)
    (coerce data 'list)))

(deftest inline-large-body.six-calls
  (%ilb-six (%ilb-keys))
  (749204326 691211529 1069842875 3306565669))

(deftest-emitting-only inline-large-body.six-calls-all-stay-calls
  (let ((text (%flat-text (dotcl:function-sil #'%ilb-six))))
    (loop with start = 0
          for i = (search "%ILB-FEISTEL" text :start2 start)
          while i count t do (setf start (1+ i))))
  6)
