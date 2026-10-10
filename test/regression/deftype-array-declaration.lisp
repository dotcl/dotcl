;;; A variable declared with a DEFTYPE name that expands to a specialized array
;;; type gets the same element access as one declared with the expansion. A
;;; DEFTYPE name is the same type as its expansion; ironclad declares its
;;; Blowfish S-boxes as (TYPE BLOWFISH-S-BOXES S-BOXES), and every read of them
;;; went through the generic AREF.

(setf dotcl:*save-sil* t)

(defun %dad-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %dad-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(deftype %dad-sbox () '(simple-array (unsigned-byte 32) (1024)))
(deftype %dad-octets (&optional (n '*)) `(simple-array (unsigned-byte 8) (,n)))

(defun %dad-named (s i)
  (declare (type %dad-sbox s) (type (unsigned-byte 8) i))
  (aref s (+ 768 i)))

(defun %dad-param (v i)
  (declare (type (%dad-octets) v) (type (unsigned-byte 8) i))
  (aref v i))

(defun %dad-set (s i x)
  (declare (type %dad-sbox s) (type (unsigned-byte 8) i)
           (type (unsigned-byte 32) x))
  (setf (aref s i) x))

;; Declared, but the array that arrives has element type T: the same outcome
;; as for the declaration written out in full (compiled code signals a
;; TYPE-ERROR; the emit-free interpreter does not check declarations).
(defun %dad-lied (s i)
  (declare (type %dad-sbox s) (type (unsigned-byte 8) i))
  (aref s i))

(defun %dad-lied-full (s i)
  (declare (type (simple-array (unsigned-byte 32) (1024)) s) (type (unsigned-byte 8) i))
  (aref s i))

(defparameter *dad-s*
  (let ((s (make-array 1024 :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (i 1024 s) (setf (aref s i) (* i 7)))))

(defparameter *dad-v*
  (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(5 6 250)))

(deftest deftype-array-declaration.values
  (list (%dad-named *dad-s* 3)
        (%dad-param *dad-v* 2)
        (progn (%dad-set *dad-s* 1 #xffffffff) (aref *dad-s* 1)))
  (#.(* 771 7) 250 #xffffffff))

(deftest deftype-array-declaration.lie-signals-like-full
  (let ((a (make-array 1024 :initial-element :x)))
    (equal (handler-case (%dad-lied a 4) (type-error () :type-error))
           (handler-case (%dad-lied-full a 4) (type-error () :type-error))))
  t)

(deftest-emitting-only deftype-array-declaration.named-is-raw
  (let ((d (%dad-sil #'%dad-named)))
    (list (%dad-count "Runtime.BackingI64" d)
          (%dad-count "Runtime.ArefL" d)
          (%dad-count "Runtime.Aref\"" d)))
  (1 0 0))

(deftest-emitting-only deftype-array-declaration.parameterized-is-raw
  (let ((d (%dad-sil #'%dad-param)))
    (list (%dad-count "Runtime.BackingU8" d)
          (%dad-count "Runtime.ArefL" d)))
  (1 0))
