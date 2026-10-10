;;; A word read from adjacent octets, the way ironclad's portable UB16REF/LE ..
;;; UB64REF/BE are written: (dpb (aref v (1+ o)) (byte 8 8) (aref v o)) for 16
;;; bits, and the 32- and 64-bit words as two INLINE calls of the narrower one
;;; deposited into one word. A call of such a function is compiled as one range
;;; check and one wide read. The answers have to be those of the octet reads:
;;; both byte orders, the high bit of a 64-bit word set, a word at the very end
;;; of the vector, and a TYPE-ERROR for a word that does not fit, for a vector
;;; that is not one of octets, and above safety 0 for a non-simple one.

(deftype %owr-index () '(mod #.array-dimension-limit))
(deftype %owr-octets () '(simple-array (unsigned-byte 8) (*)))
(declaim (inline %owr-16le %owr-32le %owr-64le %owr-16be %owr-32be %owr-64be
                 %owr-32le-at3 %owr-shuffled %owr-32le-flat))
(defun %owr-16le (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (dpb (aref v (1+ o)) (byte 8 8) (aref v o)))
(defun %owr-32le (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (dpb (%owr-16le v (+ o 2)) (byte 16 16) (%owr-16le v o)))
(defun %owr-64le (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (dpb (%owr-32le v (+ o 4)) (byte 32 32) (%owr-32le v o)))
(defun %owr-16be (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (dpb (aref v o) (byte 8 8) (aref v (1+ o))))
(defun %owr-32be (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (dpb (%owr-16be v o) (byte 16 16) (%owr-16be v (+ o 2))))
(defun %owr-64be (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (dpb (%owr-32be v o) (byte 32 32) (%owr-32be v (+ o 4))))
;; A word that does not start at the index argument itself.
(defun %owr-32le-at3 (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (%owr-32le v (+ o 3)))
;; Neither byte order: compiled as the octet reads it is.
(defun %owr-shuffled (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (logior (aref v o) (ash (aref v (+ o 2)) 8) (ash (aref v (+ o 1)) 16)
          (ash (aref v (+ o 3)) 24)))
;; A word written with LOGIOR and ASH instead of DPB.
(defun %owr-32le-flat (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (logior (aref v o) (ash (aref v (+ o 1)) 8) (ash (aref v (+ o 2)) 16)
          (ash (aref v (+ o 3)) 24)))

(defun %owr-ref (v o k be)
  (let ((w 0))
    (dotimes (i k w)
      (setf w (+ w (* (aref v (+ o i)) (expt 256 (if be (- k 1 i) i))))))))

(defun %owr-all (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (list (%owr-16le v o) (%owr-32le v o) (%owr-64le v o)
        (%owr-16be v o) (%owr-32be v o) (%owr-64be v o)
        (%owr-32le-flat v o)))
(defun %owr-all-0 (v o)
  (declare (type %owr-octets v) (type %owr-index o) (optimize speed (safety 0)))
  (list (%owr-16le v o) (%owr-32le v o) (%owr-64le v o)
        (%owr-16be v o) (%owr-32be v o) (%owr-64be v o)
        (%owr-32le-flat v o)))
;; No declaration at the call site: the vector is whatever it is given.
(defun %owr-undeclared (v o) (list (%owr-32be v o) (%owr-64le v o)))
(defun %owr-other (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (list (%owr-32le-at3 v o) (%owr-shuffled v o)))
;; A word in arithmetic: the 32-bit ones as fixnums, the 64-bit ones in the
;; 64-bit lane.
(defun %owr-mix (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (list (+ (%owr-32le v o) (%owr-32be v o))
        (logxor (%owr-64le v o) (%owr-64be v o))
        (ldb (byte 64 0) (+ (%owr-64le v o) 1))))

(defun %owr-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(deftest octet-word-read.values
  (let ((v (%owr-octets 1 2 3 4 5 6 7 #x88 #xff)))
    (list (%owr-all v 0) (%owr-all v 1) (%owr-all-0 v 1)
          (%owr-undeclared v 1) (%owr-other v 0) (%owr-mix v 1)
          ;; The last 2 / 4 / 8 octets.
          (%owr-16le v 7) (%owr-32be v 5) (%owr-64be v 1)))
  ((#x0201 #x04030201 #x8807060504030201 #x0102 #x01020304 #x0102030405060788
    #x04030201)
   (#x0302 #x05040302 #xff88070605040302 #x0203 #x02030405 #x02030405060788ff
    #x05040302)
   (#x0302 #x05040302 #xff88070605040302 #x0203 #x02030405 #x02030405060788ff
    #x05040302)
   (#x02030405 #xff88070605040302)
   (#x07060504 #x04020301)
   (#x07070707 #xfd8b030303038bfd #xff88070605040303)
   #xff88 #x060788ff #x02030405060788ff))

(deftest octet-word-read.random
  (let ((rs (make-random-state nil)) (bad nil))
    (dotimes (n 200 bad)
      (let* ((len (+ 8 (random 24 rs)))
             (v (make-array len :element-type '(unsigned-byte 8))))
        (dotimes (i len) (setf (aref v i) (random 256 rs)))
        (let ((o (random (- len 7) rs)))
          (unless (equal (%owr-all v o)
                         (list (%owr-ref v o 2 nil) (%owr-ref v o 4 nil) (%owr-ref v o 8 nil)
                               (%owr-ref v o 2 t) (%owr-ref v o 4 t) (%owr-ref v o 8 t)
                               (%owr-ref v o 4 nil)))
            (push (list n o) bad))))))
  nil)

(defun %owr-error (fn &rest args)
  (handler-case (progn (apply fn args) :no-error)
    (type-error () :type-error)
    (error (e) (type-of e))))

(defun %owr-r64 (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (%owr-64le v o))
(defun %owr-r32 (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (%owr-32be v o))
(defun %owr-r16 (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (%owr-16le v o))

(deftest octet-word-read.out-of-range
  (let ((v (%owr-octets 1 2 3 4 5 6 7 8 9)))
    (list (%owr-error #'%owr-r64 v 2)
          (%owr-error #'%owr-r32 v 6)
          (%owr-error #'%owr-r16 v 8)
          (%owr-error #'%owr-r16 v 9)
          (%owr-error #'%owr-r16 (%owr-octets) 0)
          (%owr-error #'%owr-r64 (%owr-octets 1 2 3) 0)
          (%owr-error #'%owr-undeclared v 2)
          (%owr-error #'%owr-r64 v 1)))
  (:type-error :type-error :type-error :type-error :type-error :type-error
   :type-error :no-error))

;; The emit-free interpreter does not check the declaration of the octet reads.
(deftest-emitting-only octet-word-read.not-octets
  (list (%owr-error #'%owr-undeclared '(1 2 3 4 5 6 7 8) 0)
        (%owr-error #'%owr-undeclared (make-array 8 :initial-element 0) 0)
        (%owr-error #'%owr-undeclared
                    (make-array 8 :element-type '(unsigned-byte 16) :initial-element 0) 0))
  (:type-error :type-error :type-error))

;;; A non-simple octet vector under the SIMPLE-ARRAY declaration: above safety 0
;;; the declaration signals; at safety 0 the octets are read as they are.
(defun %owr-adjustable ()
  (make-array 9 :element-type '(unsigned-byte 8) :adjustable t
                :initial-contents '(1 2 3 4 5 6 7 #x88 #xff)))
(deftest-emitting-only octet-word-read.non-simple
  (list (%owr-error #'%owr-r64 (%owr-adjustable) 0)
        (%owr-error #'%owr-undeclared (%owr-adjustable) 0)
        (%owr-all-0 (%owr-adjustable) 1))
  (:type-error :type-error
   (#x0302 #x05040302 #xff88070605040302 #x0203 #x02030405 #x02030405060788ff
    #x05040302)))

;;; The words are single wide reads: no octet loads, no generic LOGIOR / ASH /
;;; LOGXOR, and the 64-bit ones are not boxed on their way into the XOR.
(setf dotcl:*save-sil* t)
(defun %owr-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %owr-count (s k)
  (let ((n 0) (p 0))
    (loop (let ((q (search k s :start2 p)))
            (unless q (return n))
            (incf n) (setf p (1+ q))))))
(defun %owr-xor-sil (len a b c)
  (declare (type %owr-octets a b c) (type %owr-index len)
           (optimize speed (safety 0)))
  (dotimes (k (floor len 8))
    (let ((o (* 8 k)))
      (setf (aref c k) (logand #xff (logxor (%owr-64le a o) (%owr-64be b o))))))
  c)
(defun %owr-read-sil (v o)
  (declare (type %owr-octets v) (type %owr-index o))
  (+ (%owr-32le v o) (%owr-16be v o)))
(deftest-emitting-only octet-word-read.wide-loads
  (loop for fn in (list #'%owr-xor-sil #'%owr-read-sil)
        collect (let ((s (%owr-sil fn)))
                  (list (%owr-count s "LDELEM-U1") (%owr-count s "Runtime.ReadU")
                        (%owr-count s "BoxU64") (%owr-count s "Logior2")
                        (%owr-count s "Logxor2") (%owr-count s "Runtime.Ash"))))
  ((0 2 0 0 0 0) (0 2 0 0 0 0)))
(setf dotcl:*save-sil* nil)

(deftest octet-word-read.xor-values
  (let ((a (%owr-octets 1 2 3 4 5 6 7 #x88 #xf0 #xe0 #xd0 #xc0 #xb0 #xa0 #x90 #x80))
        (b (%owr-octets 9 8 7 6 5 4 3 2 1 0 #xff #xfe #xfd #xfc #xfb #xfa))
        (c (make-array 2 :element-type '(unsigned-byte 8))))
    (coerce (%owr-xor-sil 16 a b c) 'list))
  (#x03 #x0a))
