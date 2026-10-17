;;; A store into an (unsigned-byte 8) array whose value is proven to be an
;;; octet carries no store check.
;;;
;;; (setf (aref c k) (logxor (aref a i) (aref b j))) on three declared octet
;;; arrays called Runtime.CheckStoreU8 for every byte, at every safety, also
;;; with the XOR wrapped in (logand #xff ...): the XOR of two octets cannot
;;; leave [0, 255], so the check could never signal. Now the check is left
;;; out when the value's range is inside the range the check enforces.
;;;
;;; Above (safety 0) the proof uses only what is stored: literals, the elements
;;; of integer arrays, and LOGAND / LOGIOR / LOGXOR of those. A variable's
;;; integer declaration is not checked when the variable is bound, so there the
;;; store check is what reports a value outside the element type, and it stays.

(setf dotcl:*save-sil* t)

(defun %psc-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defmacro %psc-def (name safety form)
  `(defun ,name (a b c n)
     (declare (type (simple-array (unsigned-byte 8) (*)) a b c)
              (fixnum n) (optimize (speed 3) (safety ,safety)))
     (dotimes (k n) (setf (aref c k) ,form))
     c))

(%psc-def %psc-xor-s0 0 (logxor (aref a k) (aref b k)))
(%psc-def %psc-xor-s1 1 (logxor (aref a k) (aref b k)))
(%psc-def %psc-xor-s3 3 (logxor (aref a k) (aref b k)))
(%psc-def %psc-ior-and-s1 1 (logior (logand (aref a k) 15) (logxor (aref b k) 7)))
(%psc-def %psc-mask-s1 1 (logand #xff (+ (aref a k) (aref b k))))
;; Can leave [0, 255]: the check stays.
(%psc-def %psc-sum-s1 1 (+ (aref a k) (aref b k)))
(%psc-def %psc-sum-s0 0 (+ (aref a k) (aref b k)))

;; A declared variable: trusted at (safety 0), checked above it.
(defun %psc-var-s1 (c v)
  (declare (type (simple-array (unsigned-byte 8) (*)) c)
           (type (integer 0 255) v) (optimize (speed 3) (safety 1)))
  (setf (aref c 0) v)
  c)

(defun %psc-var-s0 (c v)
  (declare (type (simple-array (unsigned-byte 8) (*)) c)
           (type (integer 0 255) v) (optimize (speed 3) (safety 0)))
  (setf (aref c 0) v)
  c)

;; (unsigned-byte 16) elements XORed into a (unsigned-byte 8) array: not an octet.
(defun %psc-wide-s1 (a c)
  (declare (type (simple-array (unsigned-byte 16) (*)) a)
           (type (simple-array (unsigned-byte 8) (*)) c)
           (optimize (speed 3) (safety 1)))
  (setf (aref c 0) (logxor (aref a 0) 1))
  c)

;; A DOTIMES body is compiled twice: with and without the bounds checks.
(deftest-emitting-only proven-store-no-check.sil
  (mapcar (lambda (f) (%psc-count "Runtime.CheckStore" f))
          (list #'%psc-xor-s0 #'%psc-xor-s1 #'%psc-xor-s3 #'%psc-ior-and-s1
                #'%psc-mask-s1 #'%psc-sum-s1 #'%psc-sum-s0
                #'%psc-var-s1 #'%psc-var-s0 #'%psc-wide-s1))
  (0 0 0 0 0 2 2 1 0 1))

(defun %psc-signals (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)))

(deftest proven-store-no-check.values
  (let* ((n 300)
         (a (make-array n :element-type '(unsigned-byte 8)))
         (b (make-array n :element-type '(unsigned-byte 8)))
         (fresh (lambda () (make-array n :element-type '(unsigned-byte 8)))))
    (dotimes (i n) (setf (aref a i) (mod (* i 7) 256) (aref b i) (mod (* i 13) 256)))
    (list (loop for f in (list #'%psc-xor-s0 #'%psc-xor-s1 #'%psc-xor-s3)
                always (let ((c (funcall f a b (funcall fresh) n)))
                         (loop for i below n
                               always (= (aref c i) (logxor (aref a i) (aref b i))))))
          (let ((c (%psc-ior-and-s1 a b (funcall fresh) n)))
            (loop for i below n
                  always (= (aref c i) (logior (logand (aref a i) 15)
                                               (logxor (aref b i) 7)))))
          (let ((c (%psc-mask-s1 a b (funcall fresh) n)))
            (loop for i below n
                  always (= (aref c i) (logand #xff (+ (aref a i) (aref b i))))))
          ;; 200 + 200 does not fit: still a TYPE-ERROR above (safety 0).
          (let ((x (make-array 1 :element-type '(unsigned-byte 8) :initial-element 200)))
            (%psc-signals (lambda () (%psc-sum-s1 x x (funcall fresh) 1))))
          (%psc-signals (lambda () (%psc-var-s1 (funcall fresh) 300)))
          (aref (%psc-var-s1 (funcall fresh) 255) 0)
          (let ((w (make-array 1 :element-type '(unsigned-byte 16) :initial-element 1000)))
            (%psc-signals (lambda () (%psc-wide-s1 w (funcall fresh)))))))
  (t t t :type-error :type-error 255 :type-error))

(setf dotcl:*save-sil* nil)
