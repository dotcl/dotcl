;;; Storing a value outside an array's element type signals a TYPE-ERROR, also
;;; when the storage is wider than the type.
;;;
;;; An (unsigned-byte 32) array is stored in a long[], a (signed-byte 8) or
;;; (signed-byte 16) one in an int[], an (unsigned-byte 12) one in a ushort[].
;;; ARRAY-ELEMENT-TYPE reports the narrow type, but the store only checked the
;;; storage's own width, so (setf (aref a 0) #x1ffffffff) on an (unsigned-byte
;;; 32) array went in and came back out as #x1ffffffff. Same for SETF AREF,
;;; FILL, REPLACE and MAKE-ARRAY's initial element, and for the hoisted store
;;; through a SIMPLE-ARRAY declaration. SBCL signals in all of these above
;;; (safety 0); at (safety 0) the hoisted store takes the value unchecked (SBCL
;;; truncates it), which keeps that path as fast as before.

(setf dotcl:*save-sil* t)

(defun %nes-signals (thunk)
  (handler-case (progn (funcall thunk) :stored)
    (type-error () :type-error)))

(defun %nes-generic-set (a v) (setf (aref a 0) v))

;;; Each row: element type, the lowest and highest element, and the values one
;;; outside them.
(defparameter *nes-types*
  (list (list '(unsigned-byte 32) 0 #xffffffff -1 #x100000000)
        (list '(signed-byte 8) -128 127 -129 128)
        (list '(signed-byte 16) -32768 32767 -32769 32768)
        (list '(unsigned-byte 5) 0 31 -1 32)
        (list '(unsigned-byte 12) 0 4095 -1 4096)
        (list '(signed-byte 12) -2048 2047 -2049 2048)
        (list '(unsigned-byte 40) 0 (1- (expt 2 40)) -1 (expt 2 40))
        (list '(signed-byte 48) (- (expt 2 47)) (1- (expt 2 47))
              (- -1 (expt 2 47)) (expt 2 47))))

(deftest narrow-element-store.generic-setf-aref
  (loop for (type lo hi below above) in *nes-types*
        collect (let ((a (make-array 2 :element-type type)))
                  (list (%nes-signals (lambda () (%nes-generic-set a below)))
                        (%nes-signals (lambda () (%nes-generic-set a above)))
                        (progn (%nes-generic-set a lo) (aref a 0))
                        (progn (%nes-generic-set a hi) (= (aref a 0) hi)))))
  ((:type-error :type-error 0 t)
   (:type-error :type-error -128 t)
   (:type-error :type-error -32768 t)
   (:type-error :type-error 0 t)
   (:type-error :type-error 0 t)
   (:type-error :type-error -2048 t)
   (:type-error :type-error 0 t)
   (:type-error :type-error -140737488355328 t)))

(deftest narrow-element-store.fill-replace-make-array
  (loop for (type nil nil nil above) in *nes-types*
        collect (list (%nes-signals
                       (lambda () (fill (make-array 2 :element-type type) above)))
                      (%nes-signals
                       (lambda () (replace (make-array 2 :element-type type)
                                           (vector above))))
                      (%nes-signals
                       (lambda () (make-array 2 :element-type type
                                                :initial-element above)))
                      (%nes-signals
                       (lambda () (make-array 1 :element-type type
                                                :initial-contents (list above))))))
  ((:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error)))

;;; The hoisted store: the buffer is fetched at the binding, and the value is a
;;; raw int64 stored with a bare stelem.
(defun %nes-u32-s1 (a v)
  (declare (optimize (safety 1)) (type (simple-array (unsigned-byte 32) (*)) a)
           (type fixnum v))
  (setf (aref a 0) v))

(defun %nes-s8-s1 (a v)
  (declare (optimize (safety 1)) (type (simple-array (signed-byte 8) (*)) a)
           (type fixnum v))
  (setf (aref a 0) v))

(defun %nes-s16-s3 (a v)
  (declare (optimize (safety 3)) (type (simple-array (signed-byte 16) (*)) a)
           (type fixnum v))
  (setf (aref a 0) v))

(deftype %nes-u24-vector () '(simple-array (unsigned-byte 24) (*)))

(defun %nes-u24-s1 (a v)
  (declare (optimize (safety 1)) (type %nes-u24-vector a)
           (type fixnum v))
  (setf (aref a 0) v))

(defun %nes-u32-s0 (a v)
  (declare (optimize (safety 0)) (type (simple-array (unsigned-byte 32) (*)) a)
           (type fixnum v))
  (setf (aref a 0) v))

(deftest narrow-element-store.declared-store
  (list (let ((a (make-array 1 :element-type '(unsigned-byte 32))))
          (list (%nes-signals (lambda () (%nes-u32-s1 a #x100000000)))
                (%nes-signals (lambda () (%nes-u32-s1 a -1)))
                (progn (%nes-u32-s1 a #xffffffff) (aref a 0))))
        (let ((a (make-array 1 :element-type '(signed-byte 8))))
          (list (%nes-signals (lambda () (%nes-s8-s1 a 128)))
                (%nes-signals (lambda () (%nes-s8-s1 a -129)))
                (progn (%nes-s8-s1 a -128) (aref a 0))))
        (let ((a (make-array 1 :element-type '(signed-byte 16))))
          (list (%nes-signals (lambda () (%nes-s16-s3 a 32768)))
                (%nes-signals (lambda () (%nes-s16-s3 a -32769)))
                (progn (%nes-s16-s3 a 32767) (aref a 0))))
        (let ((a (make-array 1 :element-type '(unsigned-byte 24))))
          (list (%nes-signals (lambda () (%nes-u24-s1 a #x1000000)))
                (%nes-signals (lambda () (%nes-u24-s1 a -1)))
                (progn (%nes-u24-s1 a #xffffff) (aref a 0)))))
  ((:type-error :type-error #xffffffff)
   (:type-error :type-error -128)
   (:type-error :type-error 32767)
   (:type-error :type-error #xffffff)))

(defun %nes-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; Above (safety 0) the hoisted store checks the element type's width; at
;;; (safety 0) it does not, so the declared fast path costs what it did.
(deftest-emitting-only narrow-element-store.hoisted-check-by-safety
  (list (%nes-count "Runtime.CheckStoreUnsigned" #'%nes-u32-s1)
        (%nes-count "Runtime.CheckStoreSigned" #'%nes-s8-s1)
        (%nes-count "Runtime.CheckStoreI32" #'%nes-s8-s1)
        (%nes-count "Runtime.CheckStoreUnsigned" #'%nes-u24-s1)
        (%nes-count "Runtime.CheckStore" #'%nes-u32-s0)
        (%nes-count "(STELEM-I8)" #'%nes-u32-s0))
  (1 1 0 1 0 1))
