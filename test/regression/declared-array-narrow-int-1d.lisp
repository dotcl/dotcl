;;; AREF / (SETF AREF) on parameters declared (simple-array (signed-byte 32) (*)) and
;;; (simple-array (unsigned-byte 16) (*)) in a counting loop take the bounded
;;; backing path: Runtime.AtI32 / AtU16 / SetAtI32 / SetAtU16 with the store
;;; checks CheckStoreI32 / CheckStoreU16. An out-of-width store signals a
;;; TYPE-ERROR and leaves the element alone. Values agree with SBCL 2.6.8.

(setf dotcl:*save-sil* t)

(defun %dn1-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %dn1-sum-s32 (v)
  (declare (type (simple-array (signed-byte 32) (*)) v) (optimize (speed 1) (safety 1) (debug 0)))
  (let ((s 0)) (declare (fixnum s)) (dotimes (i (length v) s) (incf s (aref v i)))))
(defun %dn1-sum-u16 (v)
  (declare (type (simple-array (unsigned-byte 16) (*)) v) (optimize (speed 1) (safety 1) (debug 0)))
  (let ((s 0)) (declare (fixnum s)) (dotimes (i (length v) s) (incf s (aref v i)))))
(defun %dn1-iota-s32 (v)
  (declare (type (simple-array (signed-byte 32) (*)) v) (optimize (speed 1) (safety 1) (debug 0)))
  (dotimes (i (length v) v) (setf (aref v i) (- i 2))))
(defun %dn1-iota-u16 (v)
  (declare (type (simple-array (unsigned-byte 16) (*)) v) (optimize (speed 1) (safety 1) (debug 0)))
  (dotimes (i (length v) v) (setf (aref v i) (* i 3))))
(defun %dn1-set-s32 (v i x)
  (declare (type (simple-array (signed-byte 32) (*)) v) (fixnum i x) (optimize (speed 1) (safety 1) (debug 0)))
  (setf (aref v i) x))
(defun %dn1-set-u16 (v i x)
  (declare (type (simple-array (unsigned-byte 16) (*)) v) (fixnum i x) (optimize (speed 1) (safety 1) (debug 0)))
  (setf (aref v i) x))
(defun %dn1-s32 (&rest xs) (make-array (length xs) :element-type '(signed-byte 32) :initial-contents xs))
(defun %dn1-u16 (&rest xs) (make-array (length xs) :element-type '(unsigned-byte 16) :initial-contents xs))
(defun %dn1-te (thunk) (handler-case (progn (funcall thunk) :no-error) (type-error () :type-error) (error () :error)))

(deftest declared-array-narrow-int-1d.reads
  (list (%dn1-sum-s32 (%dn1-s32 -2147483648 2147483647 5 -1))
        (%dn1-sum-u16 (%dn1-u16 0 65535 7))
        (%dn1-sum-s32 (%dn1-s32))
        (%dn1-sum-u16 (%dn1-u16)))
  (3 65542 0 0))

(deftest declared-array-narrow-int-1d.loop-stores
  (list (coerce (%dn1-iota-s32 (%dn1-s32 0 0 0 0)) 'list)
        (coerce (%dn1-iota-u16 (%dn1-u16 0 0 0 0)) 'list))
  ((-2 -1 0 1) (0 3 6 9)))

(deftest declared-array-narrow-int-1d.stores-at-the-limits
  (list (let ((v (%dn1-s32 0 0 0)))
          (list (%dn1-set-s32 v 0 2147483647) (%dn1-set-s32 v 1 -2147483648)
                (%dn1-set-s32 v 2 -1) (coerce v 'list)))
        (let ((v (%dn1-u16 0 0 0)))
          (list (%dn1-set-u16 v 0 65535) (%dn1-set-u16 v 1 0)
                (%dn1-set-u16 v 2 255) (coerce v 'list))))
  ((2147483647 -2147483648 -1 (2147483647 -2147483648 -1))
   (65535 0 255 (65535 0 255))))

(deftest declared-array-narrow-int-1d.out-of-width-store-signals
  (list (let ((v (%dn1-s32 7 7)))
          (list (%dn1-te (lambda () (%dn1-set-s32 v 0 2147483648)))
                (%dn1-te (lambda () (%dn1-set-s32 v 1 -2147483649)))
                (%dn1-te (lambda () (%dn1-set-s32 v 2 1)))
                (coerce v 'list)))
        (let ((v (%dn1-u16 7 7)))
          (list (%dn1-te (lambda () (%dn1-set-u16 v 0 65536)))
                (%dn1-te (lambda () (%dn1-set-u16 v 1 -1)))
                (%dn1-te (lambda () (%dn1-set-u16 v 2 1)))
                (coerce v 'list))))
  ((:type-error :type-error :type-error (7 7))
   (:type-error :type-error :type-error (7 7))))

(deftest declared-array-narrow-int-1d.wrong-array-signals
  (list (%dn1-te (lambda () (%dn1-sum-s32 (%dn1-u16 1 2))))
        (%dn1-te (lambda () (%dn1-sum-u16 (%dn1-s32 1 2)))))
  (:type-error :type-error))

(deftest-emitting-only declared-array-narrow-int-1d.uses-the-helpers
  (flet ((n (needle fn) (plusp (%dn1-count needle (%flat-text (dotcl:function-sil fn))))))
    (list (n "Runtime.AtI32" #'%dn1-sum-s32) (n "Runtime.AtU16" #'%dn1-sum-u16)
          (n "Runtime.SetAtI32" #'%dn1-iota-s32) (n "Runtime.SetAtU16" #'%dn1-iota-u16)
          (n "Runtime.CheckStoreI32" #'%dn1-iota-s32) (n "Runtime.CheckStoreU16" #'%dn1-iota-u16)
          (n "Runtime.CheckStoreI32" #'%dn1-set-s32) (n "Runtime.CheckStoreU16" #'%dn1-set-u16)))
  (t t t t t t t t))
