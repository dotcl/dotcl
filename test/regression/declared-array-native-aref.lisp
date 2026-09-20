;;; AREF on an array-typed parameter reads the element unboxed.
;;;
;;; A LispVector whose element type upgrades to a fixed-width backing stores
;;; raw values, and Runtime.ArefNum*L reads one as an int64 with no object in
;;; between. Reaching that path needed the compiler to KNOW the backing, and
;;; the only thing it looked at was a (make-array ... :element-type '...) init
;;; it could prove -- so a function that RECEIVES the array, declared
;;; (simple-array fixnum (*)), got the generic Runtime.ArefL instead: a call
;;; that boxes the element (or hands back the cached Fixnum) only for the
;;; caller to cast it and read .Value straight back out.
;;;
;;; The float half of this was already wired -- (simple-array double-float (*))
;;; on a parameter has ridden the raw r8 path since the fft kernels needed it.
;;; The integer half was simply missing from the same extractor.
;;;
;;; A declaration is a promise, not a proof, so the runtime helpers re-check
;;; the backing and fall back to the boxed path when the array is not what was
;;; claimed. The last tests here pin that: a wrongly declared array still reads
;;; the right values.

(setf dotcl:*save-sil* t)

(defun %dan-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %dan-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

(defun %dan-walk (arr n)
  (declare (type (simple-array fixnum (*)) arr) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i)))) ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (aref arr i))))))))

(defun %dan-u8 (arr i)
  (declare (type (simple-array (unsigned-byte 8) (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

(defun %dan-set (arr i v)
  (declare (type (simple-array fixnum (*)) arr) (fixnum i v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref arr i) v))

(defun %dan-2d (arr i j)
  (declare (type (simple-array fixnum (* *)) arr) (fixnum i j)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i j)))

;; The float side, which already worked: it must keep working, and it must
;; still be the r8 helper rather than the int64 one.
(defun %dan-double (arr i)
  (declare (type (simple-array double-float (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (aref arr i)))

;; Element types with no unboxed backing must stay on the generic path: T has
;; none, and BIT is bit-packed rather than numeric-backed.
(defun %dan-t (arr i)
  (declare (type (simple-array t (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (aref arr i))

(defun %dan-bit (arr i)
  (declare (type (simple-array bit (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (aref arr i))

;; Declared, but what actually arrives is element-type T. The runtime re-checks
;; the backing, so this reads correctly through the fallback.
(defun %dan-lied (arr i)
  (declare (type (simple-array fixnum (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

;;; ---- inputs ----

(defparameter *dan-fix*
  (let ((a (make-array 8 :element-type 'fixnum :initial-element 0)))
    (dotimes (i 8 a) (setf (aref a i) (* i 10)))))
(defparameter *dan-u8v*
  (let ((a (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i 4 a) (setf (aref a i) (+ 250 i)))))
(defparameter *dan-2dv*
  (let ((a (make-array '(2 3) :element-type 'fixnum :initial-element 0)))
    (dotimes (i 2 a) (dotimes (j 3) (setf (aref a i j) (+ (* i 10) j))))))
(defparameter *dan-dv*
  (let ((a (make-array 3 :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref a 0) 1.5d0)
    (setf (aref a 1) -2.25d0)
    (setf (aref a 2) 1d100)
    a))
(defparameter *dan-tv* (make-array 3 :initial-contents (list 7 8 9)))
(defparameter *dan-bv*
  (make-array 3 :element-type 'bit :initial-contents (list 1 0 1)))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY: an emit-free build stores no SIL, so every count
;;; taken from FUNCTION-SIL would be 0 for the wrong reason.

;; The walk reads raw int64: no generic ArefL, and no box/unbox around the
;; element. The element buffer is fetched once, before the loop, so the read
;; itself is a bare ldelem and not even the ArefNumL helper remains -- see
;; array-backing-hoist for that path. The one Fixnum.Make is the box of the
;; returned ACC.
(deftest-emitting-only declared-array-native-aref.walk-is-raw
  (let ((d (%dan-sil #'%dan-walk)))
    (list (%dan-count "Runtime.BackingI64" d)
          (%dan-count "(LDELEM-I8)" d)
          (%dan-count "Runtime.ArefNumL" d)
          (%dan-count "Runtime.ArefL" d)
          (%dan-count "Fixnum.Make" d)))
  (1 1 0 0 1))

(deftest-emitting-only declared-array-native-aref.unsigned-byte-is-raw
  (let ((d (%dan-sil #'%dan-u8)))
    (list (%dan-count "Runtime.BackingU8" d)
          (%dan-count "(LDELEM-U1)" d)
          (%dan-count "Runtime.ArefL" d)))
  (1 1 0))

(deftest-emitting-only declared-array-native-aref.setf-is-raw
  (let ((d (%dan-sil #'%dan-set)))
    (list (%dan-count "(STELEM-I8)" d)
          (%dan-count "Runtime.ArefSetNumL" d)
          (%dan-count "Runtime.ArefSetL" d)))
  (1 0 0))

(deftest-emitting-only declared-array-native-aref.rank-2-is-raw
  (let ((d (%dan-sil #'%dan-2d)))
    (list (%dan-count "Runtime.ArefNum2DL" d) (%dan-count "Runtime.Aref2DL" d)))
  (1 0))

;; The float declaration keeps its own helper -- widening the extractor to
;; integer element types must not route a double array through the int64 one.
(deftest-emitting-only declared-array-native-aref.double-still-r8
  (let ((d (%dan-sil #'%dan-double)))
    (list (%dan-count "Runtime.ArefNumD" d) (%dan-count "Runtime.ArefNumL" d)))
  (1 0))

;; No unboxed backing exists for these, so the generic path is the correct one.
(deftest-emitting-only declared-array-native-aref.t-array-stays-generic
  (let ((d (%dan-sil #'%dan-t)))
    (list (%dan-count "Runtime.ArefNumL" d) (%dan-count "Runtime.ArefL" d)))
  (0 1))

(deftest-emitting-only declared-array-native-aref.bit-array-stays-generic
  (let ((d (%dan-sil #'%dan-bit)))
    (list (%dan-count "Runtime.ArefNumL" d) (%dan-count "Runtime.ArefL" d)))
  (0 1))

;;; ---- values ----

(deftest declared-array-native-aref.walk-value
  (%dan-walk *dan-fix* 8)
  280)

(deftest declared-array-native-aref.u8-values
  (list (%dan-u8 *dan-u8v* 0) (%dan-u8 *dan-u8v* 3))
  (250 253))

(deftest declared-array-native-aref.setf-roundtrip
  (progn (%dan-set *dan-fix* 2 1000000007)
         (let ((v (aref *dan-fix* 2)))
           (%dan-set *dan-fix* 2 20)
           (list v (aref *dan-fix* 2))))
  (1000000007 20))

(deftest declared-array-native-aref.rank-2-values
  (list (%dan-2d *dan-2dv* 0 0) (%dan-2d *dan-2dv* 1 2))
  (0 12))

(deftest declared-array-native-aref.double-values
  (list (%dan-double *dan-dv* 0) (%dan-double *dan-dv* 1) (%dan-double *dan-dv* 2))
  (1.5d0 -2.25d0 1d100))

(deftest declared-array-native-aref.generic-values
  (list (%dan-t *dan-tv* 1) (%dan-bit *dan-bv* 0) (%dan-bit *dan-bv* 1))
  (8 1 0))

;; The declaration is checked once, where the variable is bound, and a false
;; one is reported as the TYPE-ERROR it is. It used to be re-checked per element
;; instead, which let a false declaration read the right values off the boxed
;; path; hoisting the element buffer moved that check to the binding, and a
;; declaration the caller does not honor now says so rather than being quietly
;; absorbed. See array-backing-hoist for the shapes that are NOT hoisted and
;; therefore still take the per-element path.
;;
;; DEFTEST-EMITTING-ONLY: this is a property of code generated FROM the
;; declaration, and an interpreter is free to ignore declarations (CLHS 3.3.1).
;; The emit-free build does ignore them, so there is nothing there to check the
;; claim against -- and nothing wrong with that, because a declaration the
;; program did not honor is undefined either way.
(deftest-emitting-only declared-array-native-aref.wrong-declaration-signals
  (handler-case (progn (%dan-lied *dan-tv* 0) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest-emitting-only declared-array-native-aref.wrong-declaration-signals-on-entry
  (handler-case
      (progn (%dan-lied (make-array 2 :initial-contents (list 1000000000000 -5)) 0)
             :no-error)
    (type-error () :type-error))
  :type-error)

(setf dotcl:*save-sil* nil)
