;;; The store of a word into adjacent octets, the way ironclad's portable
;;; (SETF UB16REF/LE) .. (SETF UB64REF/BE) are written: two octet stores of
;;; (LOGAND VALUE #XFF) and (LDB (BYTE 8 8) VALUE) for 16 bits, and the 32- and
;;; 64-bit words as two INLINE (SETF F) calls of the narrower one with the halves
;;; of the value. SETF of such a call is compiled as one range check and one
;;; wide store. The octets stored and the value returned have to be those of the
;;; octet stores, in both byte orders; a value outside the declared width, a
;;; range past the end, and a vector that is not of octets signal a TYPE-ERROR.

(deftype %oww-index () '(mod #.array-dimension-limit))
(deftype %oww-octets () '(simple-array (unsigned-byte 8) (*)))
(declaim (inline %oww-16le (setf %oww-16le) %oww-32le (setf %oww-32le)
                 %oww-64le (setf %oww-64le) %oww-16be (setf %oww-16be)
                 %oww-32be (setf %oww-32be) %oww-64be (setf %oww-64be)
                 (setf %oww-odd)))
(defun %oww-16le (v o)
  (declare (type %oww-octets v) (type %oww-index o))
  (dpb (aref v (1+ o)) (byte 8 8) (aref v o)))
(defun (setf %oww-16le) (value v o)
  (declare (type (unsigned-byte 16) value) (type %oww-octets v) (type %oww-index o))
  (setf (aref v o) (logand value #xff)
        (aref v (1+ o)) (ldb (byte 8 8) value))
  value)
(defun %oww-32le (v o)
  (declare (type %oww-octets v) (type %oww-index o))
  (dpb (%oww-16le v (+ o 2)) (byte 16 16) (%oww-16le v o)))
(defun (setf %oww-32le) (value v o)
  (declare (type (unsigned-byte 32) value) (type %oww-octets v) (type %oww-index o))
  (setf (%oww-16le v o) (logand value #xffff)
        (%oww-16le v (+ o 2)) (ldb (byte 16 16) value))
  value)
(defun %oww-64le (v o)
  (declare (type %oww-octets v) (type %oww-index o))
  (dpb (%oww-32le v (+ o 4)) (byte 32 32) (%oww-32le v o)))
(defun (setf %oww-64le) (value v o)
  (declare (type (unsigned-byte 64) value) (type %oww-octets v) (type %oww-index o))
  (setf (%oww-32le v o) (logand value #xffffffff)
        (%oww-32le v (+ o 4)) (ldb (byte 32 32) value))
  value)
(defun %oww-16be (v o)
  (declare (type %oww-octets v) (type %oww-index o))
  (dpb (aref v o) (byte 8 8) (aref v (1+ o))))
(defun (setf %oww-16be) (value v o)
  (declare (type (unsigned-byte 16) value) (type %oww-octets v) (type %oww-index o))
  (setf (aref v (1+ o)) (logand value #xff)
        (aref v o) (ldb (byte 8 8) value))
  value)
(defun %oww-32be (v o)
  (declare (type %oww-octets v) (type %oww-index o))
  (dpb (%oww-16be v o) (byte 16 16) (%oww-16be v (+ o 2))))
(defun (setf %oww-32be) (value v o)
  (declare (type (unsigned-byte 32) value) (type %oww-octets v) (type %oww-index o))
  (setf (%oww-16be v (+ o 2)) (logand value #xffff)
        (%oww-16be v o) (ldb (byte 16 16) value))
  value)
(defun %oww-64be (v o)
  (declare (type %oww-octets v) (type %oww-index o))
  (dpb (%oww-32be v o) (byte 32 32) (%oww-32be v (+ o 4))))
(defun (setf %oww-64be) (value v o)
  (declare (type (unsigned-byte 64) value) (type %oww-octets v) (type %oww-index o))
  (setf (%oww-32be v (+ o 4)) (logand value #xffffffff)
        (%oww-32be v o) (ldb (byte 32 32) value))
  value)
;; Neither byte order: compiled as the octet stores it is.
(defun %oww-odd (v o) (declare (ignore v o)) 0)
(defun (setf %oww-odd) (value v o)
  (declare (type (unsigned-byte 16) value) (type %oww-octets v) (type %oww-index o))
  (setf (aref v o) (logand value #xff)
        (aref v (+ o 2)) (ldb (byte 8 8) value))
  value)

(defun %oww-octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;; Each store into a fresh vector of 10 octets at O, with the value the SETF
;; returned.
(defun %oww-each (o w16 w32 w64)
  (declare (type %oww-index o))
  (let ((out '()))
    (macrolet ((one (place val)
                 `(let ((v (%oww-octets 10)))
                    (declare (type %oww-octets v))
                    (let ((r (setf (,place v o) ,val)))
                      (push (list r (coerce v 'list)) out)))))
      (one %oww-16le w16) (one %oww-32le w32) (one %oww-64le w64)
      (one %oww-16be w16) (one %oww-32be w32) (one %oww-64be w64)
      (one %oww-odd w16))
    (nreverse out)))

(deftest octet-word-write.values
  (list (%oww-each 0 #x0102 #x01020304 #x0102030405060708)
        (%oww-each 2 #xff80 #x80ff7f01 #xfedcba9876543210)
        (mapcar #'second (%oww-each 2 0 0 0)))
  (((#x0102 (2 1 0 0 0 0 0 0 0 0))
    (#x01020304 (4 3 2 1 0 0 0 0 0 0))
    (#x0102030405060708 (8 7 6 5 4 3 2 1 0 0))
    (#x0102 (1 2 0 0 0 0 0 0 0 0))
    (#x01020304 (1 2 3 4 0 0 0 0 0 0))
    (#x0102030405060708 (1 2 3 4 5 6 7 8 0 0))
    (#x0102 (2 0 1 0 0 0 0 0 0 0)))
   ((#xff80 (0 0 #x80 #xff 0 0 0 0 0 0))
    (#x80ff7f01 (0 0 1 #x7f #xff #x80 0 0 0 0))
    (#xfedcba9876543210 (0 0 #x10 #x32 #x54 #x76 #x98 #xba #xdc #xfe))
    (#xff80 (0 0 #xff #x80 0 0 0 0 0 0))
    (#x80ff7f01 (0 0 #x80 #xff #x7f 1 0 0 0 0))
    (#xfedcba9876543210 (0 0 #xfe #xdc #xba #x98 #x76 #x54 #x32 #x10))
    (#xff80 (0 0 #x80 0 #xff 0 0 0 0 0)))
   ((0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0))))

;; Write then read back, each width and order, at random places.
(defun %oww-roundtrip (v o w16 w32 w64)
  (declare (type %oww-octets v) (type %oww-index o))
  (list (progn (setf (%oww-16le v o) w16) (%oww-16le v o))
        (progn (setf (%oww-32le v o) w32) (%oww-32le v o))
        (progn (setf (%oww-64le v o) w64) (%oww-64le v o))
        (progn (setf (%oww-16be v o) w16) (%oww-16be v o))
        (progn (setf (%oww-32be v o) w32) (%oww-32be v o))
        (progn (setf (%oww-64be v o) w64) (%oww-64be v o))
        (coerce v 'list)))

(deftest octet-word-write.random
  (let ((rs (make-random-state nil)) (bad nil))
    (dotimes (n 200 bad)
      (let* ((len (+ 8 (random 16 rs)))
             (v (%oww-octets len))
             (o (random (- len 7) rs))
             (w16 (random #x10000 rs)) (w32 (random #x100000000 rs))
             (w64 (random #x10000000000000000 rs))
             (r (%oww-roundtrip v o w16 w32 w64)))
        (unless (and (equal (butlast r) (list w16 w32 w64 w16 w32 w64))
                     ;; The last store was the big-endian 64-bit word.
                     (loop for i below 8
                           always (= (nth (+ o i) (car (last r)))
                                     (ldb (byte 8 (* 8 (- 7 i))) w64))))
          (push (list n o) bad)))))
  nil)

(defun %oww-error (fn &rest args)
  (handler-case (progn (apply fn args) :no-error)
    (type-error () :type-error)
    (error (e) (type-of e))))

(defun %oww-w64 (v o x)
  (declare (type %oww-octets v) (type %oww-index o))
  (setf (%oww-64le v o) x))
(defun %oww-w32 (v o x)
  (declare (type %oww-octets v) (type %oww-index o))
  (setf (%oww-32be v o) x))
(defun %oww-w16 (v o x)
  (declare (type %oww-octets v) (type %oww-index o))
  (setf (%oww-16le v o) x))
;; No declaration at the call site.
(defun %oww-undeclared (v o x) (setf (%oww-32le v o) x))

(deftest octet-word-write.errors
  (let ((v (%oww-octets 9)))
    (list (%oww-error #'%oww-w64 v 2 0)
          (%oww-error #'%oww-w32 v 6 0)
          (%oww-error #'%oww-w16 v 8 0)
          (%oww-error #'%oww-w16 (%oww-octets 0) 0 0)
          (%oww-error #'%oww-w64 v 1 #xffffffffffffffff)
          (%oww-error #'%oww-w16 v 0 'a)
          (%oww-error #'%oww-undeclared v 6 0)))
  (:type-error :type-error :type-error :type-error :no-error :type-error :type-error))

;;; A value outside the declared width of the (SETF F) parameter. The binding
;;; of an (UNSIGNED-BYTE 64) is checked; the emit-free interpreter does not
;;; check it.
(deftest-emitting-only octet-word-write.value-errors
  (let ((v (%oww-octets 9)))
    (list (%oww-error #'%oww-w64 v 0 -1)
          (%oww-error #'%oww-w64 v 0 #x10000000000000000)))
  (:type-error :type-error))

;;; A narrower one is not checked in the inline expansion (no integer binding
;;; is): the octet stores take its low bits, and the SETF returns the value
;;; given. The emit-free interpreter calls the function, which checks it.
(deftest-emitting-only octet-word-write.narrow-values
  (let ((v (%oww-octets 6)))
    (declare (type %oww-octets v))
    (list (%oww-w32 v 0 #x1ffffffff) (%oww-w16 v 4 -2)
          (setf (%oww-16be v 0) #x12345) (setf (%oww-16le v 2) (- (expt 2 70) 1))
          (coerce v 'list)))
  (#x1ffffffff -2 #x12345 #x3fffffffffffffffff (#x23 #x45 #xff #xff #xfe #xff)))

;;; The emit-free interpreter does not check the declarations of the octet
;;; stores' vector.
(deftest-emitting-only octet-word-write.not-octets
  (list (%oww-error #'%oww-undeclared '(1 2 3 4) 0 0)
        (%oww-error #'%oww-undeclared (make-array 4 :initial-element 0) 0 0)
        (%oww-error #'%oww-undeclared
                    (make-array 4 :element-type '(unsigned-byte 16) :initial-element 0) 0 0)
        (%oww-error #'%oww-w64
                    (make-array 9 :element-type '(unsigned-byte 8) :adjustable t) 0 0))
  (:type-error :type-error :type-error :type-error))

;;; At safety 0 a non-simple octet vector under the declaration is written
;;; octet by octet, as the octet stores write it here. The declaration is
;;; false, so this is dotcl's choice: SBCL writes elsewhere.
(defun %oww-w32-0 (v o x)
  (declare (type %oww-octets v) (type %oww-index o) (optimize speed (safety 0)))
  (setf (%oww-32be v o) x)
  v)
(deftest-emitting-only octet-word-write.non-simple-safety-0
  (coerce (%oww-w32-0 (make-array 6 :element-type '(unsigned-byte 8) :adjustable t
                                     :initial-element 0)
                      1 #x01020304)
          'list)
  (0 1 2 3 4 0))

;;; The place's arguments are evaluated before the new value (CLHS 5.1.1.1).
(deftest octet-word-write.order
  (let ((log '()) (v (%oww-octets 8)))
    (declare (type %oww-octets v))
    (setf (%oww-32le (progn (push :vector log) v) (progn (push :index log) 1))
          (progn (push :value log) #x0a0b0c0d))
    (list (nreverse log) (coerce v 'list)))
  ((:vector :index :value) (0 #x0d #x0c #x0b #x0a 0 0 0)))

;;; One wide store per word: no octet stores in the code.
(setf dotcl:*save-sil* t)
(defun %oww-count (s k)
  (let ((n 0) (p 0))
    (loop (let ((q (search k s :start2 p)))
            (unless q (return n))
            (incf n) (setf p (1+ q))))))
(defun %oww-xor-sil (len a b c)
  (declare (type %oww-octets a b c) (type %oww-index len)
           (optimize speed (safety 0)))
  (dotimes (k (floor len 8))
    (let ((o (* 8 k)))
      (setf (%oww-64le c o) (logxor (%oww-64le a o) (%oww-64be b o)))))
  c)
(defun %oww-store-sil (v o x)
  (declare (type %oww-octets v) (type %oww-index o) (type (unsigned-byte 32) x))
  (setf (%oww-32be v o) x)
  (setf (%oww-16le v (+ o 4)) (logand x #xffff))
  v)
(deftest-emitting-only octet-word-write.wide-stores
  (loop for fn in (list #'%oww-xor-sil #'%oww-store-sil)
        collect (let ((s (%flat-text (dotcl:function-sil fn))))
                  (list (%oww-count s "STELEM-U1") (%oww-count s "Runtime.WriteU")
                        (%oww-count s "BoxU64"))))
  ((0 1 0) (0 2 0)))
(setf dotcl:*save-sil* nil)

(deftest octet-word-write.xor-values
  (let ((a (make-array 16 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3 4 5 6 7 #x88 #xf0 #xe0 #xd0 #xc0 #xb0 #xa0 #x90 #x80)))
        (b (make-array 16 :element-type '(unsigned-byte 8)
                          :initial-contents '(9 8 7 6 5 4 3 2 1 0 #xff #xfe #xfd #xfc #xfb #xfa)))
        (c (%oww-octets 16)))
    (coerce (%oww-xor-sil 16 a b c) 'list))
  (3 1 7 1 3 1 #x0f #x81 #x0a #x1b #x2c #x3d #x4e #x5f #x90 #x81))
