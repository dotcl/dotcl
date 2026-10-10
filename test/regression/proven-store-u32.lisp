;;; A store into an (unsigned-byte 32) array whose value is proven to be an
;;; (unsigned-byte 32) carries no store check, also when the value is computed
;;; by a function proclaimed INLINE.
;;;
;;; Salsa20's round, (setf (aref x i) (logxor (aref x i) (rol32 (mod32+ ...) 7)))
;;; with X made by (make-array 16 :element-type '(unsigned-byte 32)) and ROL32
;;; and MOD32+ inline functions written as shifts and masks, called
;;; Runtime.CheckStoreUnsigned on every one of its 64 stores at (safety 1). The
;;; proof knew literals, array elements (at the range of the Int64[] storage,
;;; not of the element type) and LOGAND / LOGIOR / LOGXOR only. It now also
;;; reads an element of an array the function made itself at the element
;;; type's range, and follows ASH, + and -, the LET of a substituted inline
;;; call, and its BLOCK. A variable's declared type is still not used above
;;; (safety 0). A declared array's element type is: its buffer's fetch matches
;;; the vector's element type (declared-element-type-fetch.lisp).

(setf dotcl:*save-sil* t)

(defun %psu-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (inline %psu-add32 %psu-rol32 %psu-add-wide))
(defun %psu-add32 (a b)
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))
(defun %psu-rol32 (a s)
  (declare (type (unsigned-byte 32) a) (type (integer 0 32) s))
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))
;; Declared to return an (unsigned-byte 32), but the sum can be 2^33 - 2.
(defun %psu-add-wide (a b)
  (declare (type (unsigned-byte 32) a b))
  (+ a b))

(defmacro %psu-def (name form)
  `(defun ,name (x y)
     (declare (type (simple-array (unsigned-byte 32) (*)) x y)
              (optimize (speed 3) (safety 1)))
     (setf (aref x 0) ,form)
     x))

;; X is the function's own (UNSIGNED-BYTE 32) array, Y a declared one.
(defmacro %psu-made (name form)
  `(defun ,name (y)
     (declare (type (simple-array (unsigned-byte 32) (*)) y)
              (optimize (speed 3) (safety 1)))
     (let ((x (make-array 3 :element-type '(unsigned-byte 32))))
       (replace x y)
       (setf (aref x 0) ,form)
       (aref x 0))))

(%psu-made %psu-round (logxor (ash (aref x 0) -1)
                              (%psu-rol32 (%psu-add32 (aref x 1) (aref x 2)) 7)))
(%psu-made %psu-shr (ash (aref x 2) -3))
(%psu-def %psu-let (let ((v (aref y 0)) (w 5)) (logand v (+ w 1000))))
;; Can leave [0, 2^32 - 1]: the check stays (a left shift is stored through the
;; generic element store, which checks it there).
(%psu-def %psu-sum (+ (aref x 0) (aref y 1)))
(%psu-def %psu-shl (ash (aref y 0) 1))
(%psu-def %psu-wide (%psu-add-wide (aref x 0) (aref y 1)))
(%psu-def %psu-sub (- (aref x 0) (aref y 1)))

;; Elements of declared arrays: at the element type's range, matched when the
;; buffer is fetched.
(%psu-def %psu-xor (logxor (aref x 0) (aref y 1)))

;; A variable declared (unsigned-byte 32): not trusted above (safety 0).
(defun %psu-declared (x v)
  (declare (type (simple-array (unsigned-byte 32) (*)) x)
           (optimize (speed 3) (safety 1)))
  (let ((w (identity v)))
    (declare (type (unsigned-byte 32) w))
    (setf (aref x 0) (logand w #xffffffffff)))
  x)

;; The array made in the function (its type inferred from MAKE-ARRAY).
(defun %psu-local (y)
  (declare (type (simple-array (unsigned-byte 32) (*)) y)
           (optimize (speed 3) (safety 1)))
  (let ((x (make-array 2 :element-type '(unsigned-byte 32))))
    (setf (aref x 0) (ldb (byte 32 0) (aref y 0))
          (aref x 1) (logxor (aref x 0) (%psu-rol32 (ldb (byte 32 0) (aref y 1)) 9))
          (aref x 0) (logxor (aref x 0) (aref x 1)))
    (list (aref x 0) (aref x 1))))

(deftest-emitting-only proven-store-u32.sil
  (list (mapcar (lambda (f) (%psu-count "Runtime.CheckStore" f))
                (list #'%psu-round #'%psu-shr #'%psu-let #'%psu-local #'%psu-xor))
        (mapcar (lambda (f) (%psu-count "Runtime.CheckStore" f))
                (list #'%psu-sum #'%psu-wide #'%psu-sub #'%psu-declared)))
  ((0 0 0 0 0) (1 1 1 1)))

(defun %psu-u32 (&rest vals)
  (make-array (length vals) :element-type '(unsigned-byte 32) :initial-contents vals))

(defun %psu-signals (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error () :type-error)))

(defun %psu-rol-ref (a s) (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))

(deftest proven-store-u32.values
  (let ((a #xdeadbeef) (b #x89abcdef) (c #xfedcba98))
    (list (= (aref (%psu-xor (%psu-u32 a) (%psu-u32 0 b)) 0) (logxor a b))
          (= (%psu-round (%psu-u32 a b c))
             (logxor (ash a -1) (%psu-rol-ref (ldb (byte 32 0) (+ b c)) 7)))
          (%psu-shr (%psu-u32 0 0 #xffffffff))
          (aref (%psu-let (%psu-u32 0) (%psu-u32 #xffffffff)) 0)
          (let ((x1 (logxor a (%psu-rol-ref b 9))))
            (equal (%psu-local (%psu-u32 a b)) (list (logxor a x1) x1)))
          ;; A FIXNUM vector where (UNSIGNED-BYTE 32) is declared: the
          ;; buffer's fetch reports it.
          (%psu-signals (lambda ()
                          (%psu-xor (%psu-u32 0)
                                    (make-array 2 :element-type 'fixnum
                                                  :initial-contents (list 0 (expt 2 40))))))
          (aref (%psu-sum (%psu-u32 1) (%psu-u32 0 2)) 0)
          (%psu-signals (lambda () (%psu-sum (%psu-u32 #xffffffff) (%psu-u32 0 1))))
          (%psu-signals (lambda () (%psu-shl (%psu-u32 0) (%psu-u32 #x80000000))))
          (%psu-signals (lambda () (%psu-wide (%psu-u32 #xffffffff) (%psu-u32 0 1))))
          (%psu-signals (lambda () (%psu-sub (%psu-u32 0) (%psu-u32 0 1))))
          (%psu-signals (lambda () (%psu-declared (%psu-u32 0) (expt 2 36))))
          (aref (%psu-declared (%psu-u32 0) 7) 0)))
  (t t #x1fffffff 1005 t :type-error 3 :type-error :type-error :type-error :type-error :type-error 7))

(setf dotcl:*save-sil* nil)
