;;; A binding that copies a local whose element buffer is already fetched uses
;;; that buffer instead of fetching it again.
;;;
;;; An INLINE call's expansion binds each parameter to its argument. ironclad's
;;; UB64REF/LE expands UB32REF/LE twice and UB16REF/LE four times, and each
;;; expansion bound VECTOR to the caller's VECTOR and called Runtime.BackingU8
;;; on it: seven fetches of the same buffer per word read, and the same again
;;; for every word written. Now the fetch happens once, where the array was
;;; declared, and a loop that reaches the array only through INLINE calls is
;;; unswitched on that buffer like one that calls AREF itself.

(setf dotcl:*save-sil* t)

(defun %iba-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(deftype %iba-index () '(mod #.array-dimension-limit))
(deftype %iba-octets () '(simple-array (unsigned-byte 8) (*)))
(declaim (inline %iba-u16 (setf %iba-u16) %iba-u32 (setf %iba-u32)
                 %iba-u64 (setf %iba-u64)))
(defun %iba-u16 (vector offset)
  (declare (type %iba-octets vector) (type %iba-index offset))
  (dpb (aref vector (1+ offset)) (byte 8 8) (aref vector offset)))
(defun (setf %iba-u16) (value vector offset)
  (declare (type (unsigned-byte 16) value) (type %iba-octets vector)
           (type %iba-index offset))
  (setf (aref vector offset) (logand value #xff)
        (aref vector (1+ offset)) (ldb (byte 8 8) value))
  value)
(defun %iba-u32 (vector offset)
  (declare (type %iba-octets vector) (type %iba-index offset))
  (dpb (%iba-u16 vector (+ offset 2)) (byte 16 16) (%iba-u16 vector offset)))
(defun (setf %iba-u32) (value vector offset)
  (declare (type (unsigned-byte 32) value) (type %iba-octets vector)
           (type %iba-index offset))
  (setf (%iba-u16 vector offset) (logand value #xffff)
        (%iba-u16 vector (+ offset 2)) (ldb (byte 16 16) value))
  value)
(defun %iba-u64 (vector offset)
  (declare (type %iba-octets vector) (type %iba-index offset))
  (dpb (%iba-u32 vector (+ offset 4)) (byte 32 32) (%iba-u32 vector offset)))
(defun (setf %iba-u64) (value vector offset)
  (declare (type (unsigned-byte 64) value) (type %iba-octets vector)
           (type %iba-index offset))
  (setf (%iba-u32 vector offset) (logand value #xffffffff)
        (%iba-u32 vector (+ offset 4)) (ldb (byte 32 32) value))
  value)

(defun %iba-xor (len a as b bs c cs)
  (declare (type %iba-octets a b c) (type %iba-index len as bs cs)
           (optimize speed (safety 0)))
  (dotimes (k (floor len 8))
    (let ((o (* 8 k)))
      (setf (%iba-u64 c (+ cs o))
            (logxor (%iba-u64 a (+ as o)) (%iba-u64 b (+ bs o))))))
  c)

(defun %iba-read32 (v o)
  (declare (type %iba-octets v) (type %iba-index o))
  (%iba-u32 v o))

(defun %iba-sum16 (v n)
  (declare (type %iba-octets v) (type %iba-index n) (optimize speed (safety 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (k n s) (setq s (+ s (%iba-u16 v (* 2 k)))))))

(defun %iba-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

;;; Scope: the alias is of the array the init names where the init is
;;; evaluated, never of a binding that shadows it.
(defun %iba-shadow (a b)
  (declare (type %iba-octets a b))
  (list
   ;; LET: V is the outer A, not the sibling A.
   (let ((a b) (v a))
     (declare (type %iba-octets a v))
     (list (aref a 0) (aref v 0)))
   ;; LET*: V is the sibling A, which is B.
   (let* ((a b) (v a))
     (declare (type %iba-octets a v))
     (list (aref a 0) (aref v 0)))
   ;; The source shadowed inside the alias's body.
   (let ((v a))
     (declare (type %iba-octets v))
     (let ((a b))
       (declare (type %iba-octets a))
       (list (aref a 0) (aref v 0))))
   ;; An undeclared copy written through.
   (let ((tmp a))
     (setf (aref tmp 1) 77)
     (aref a 1))))

;; A declaration of another element type keeps its own fetch and its check.
(defun %iba-other-kind (a)
  (declare (type %iba-octets a) (optimize (safety 1)))
  (let ((v a))
    (declare (type (simple-array (unsigned-byte 16) (*)) v))
    (aref v 0)))

(defun %iba-closure (a)
  (declare (type %iba-octets a))
  (let ((v a))
    (declare (type %iba-octets v))
    (lambda (i) (aref v i))))

;; A buffer fetched by a nested LET inside an unswitched loop is not one the
;; loop tested: its accesses keep their own test.
(defun %iba-nested (a vs)
  (declare (type %iba-octets a) (optimize speed (safety 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (k (length a) s)
      (let ((w (svref vs k)))
        (declare (type %iba-octets w))
        (setq s (+ s (aref a k) (aref w 0)))))))

(deftest inline-backing-alias.values
  (let* ((rs (make-random-state nil))
         (a (make-array 77 :element-type '(unsigned-byte 8)))
         (b (make-array 77 :element-type '(unsigned-byte 8)))
         (c (make-array 77 :element-type '(unsigned-byte 8) :initial-element 9)))
    (dotimes (i 77) (setf (aref a i) (random 256 rs) (aref b i) (random 256 rs)))
    (%iba-xor 64 a 3 b 5 c 1)
    (list
     (and (= (aref c 0) 9) (= (aref c 65) 9)
          (loop for i below 64
                always (= (aref c (+ 1 i)) (logxor (aref a (+ 3 i)) (aref b (+ 5 i))))))
     (%iba-read32 (%iba-octets 1 2 3 4 5) 1)
     (%iba-sum16 (%iba-octets 1 2 3 4) 2)
     (%iba-shadow (%iba-octets 1 2) (%iba-octets 3 4))
     (funcall (%iba-closure (%iba-octets 5 6)) 1)
     (%iba-nested (%iba-octets 1 2 3)
                  (vector (%iba-octets 10) (%iba-octets 20) (%iba-octets 30)))))
  (t #x05040302 1540 ((3 1) (3 3) (3 1) 77) 6 66))

;;; The declaration's check is made by the compiled fetch; the emit-free
;;; evaluator does not check declarations.
(deftest-emitting-only inline-backing-alias.other-kind
  (handler-case (%iba-other-kind (%iba-octets 1 2)) (type-error () :type-error))
  :type-error)

;;; One fetch per declared array, however many expansions read it; the loops
;;; that reach the arrays only through the INLINE calls are unswitched (one
;;; test per buffer before the loop, none at the accesses).
(deftest-emitting-only inline-backing-alias.sil
  (list (%iba-count "Runtime.BackingU8" #'%iba-xor)
        (%iba-count "Runtime.BackingU8" #'%iba-read32)
        (%iba-count "Runtime.BackingU8" #'%iba-sum16)
        (%iba-count "ABFALL" #'%iba-xor)
        (%iba-count "ABFALL" #'%iba-sum16)
        (plusp (%iba-count "ABFALL" #'%iba-nested)))
  (3 1 1 0 0 t))

(setf dotcl:*save-sil* nil)
