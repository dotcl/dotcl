;;; The element buffer of a declared SIMPLE-ARRAY is fetched once per binding.
;;;
;;; AREF on a local declared (simple-array <integer type> (*)) used to call
;;; Runtime.ArefNum*L per element, and that helper re-asks the same questions
;;; every time: is this a LispVector, which backing kind, is it displaced, is it
;;; rank 1, is the subscript in range. The answers are loop-invariant and the
;;; JIT does not hoist them -- measured, a plain loop with no exception handling
;;; anywhere still paid them per element. So the compiler hoists instead: the
;;; concrete buffer (long[] / int[] / ushort[] / byte[]) is fetched where the
;;; variable is bound, into a slot of that array type, and each AREF becomes a
;;; bare ldelem against it.
;;;
;;; This is sound only because the declaration says SIMPLE-ARRAY. CLHS 1.4.4:
;;; a simple array is not displaced, not adjustable, and has no fill pointer, so
;;; its storage cannot be swapped underneath the binding -- ADJUST-ARRAY on one
;;; returns a fresh array instead of rewriting it. Everything that does not
;;; carry that promise keeps the per-element helper, and this file pins which
;;; is which:
;;;
;;;   hoisted     (simple-array fixnum (*)) / (unsigned-byte N) / (signed-byte N)
;;;               parameters and LET bindings that are never assigned
;;;   not hoisted (array ...) and (vector ...) declarations, which permit a
;;;               displaced or adjustable argument
;;;   not hoisted a variable the body SETQs, which could name a different array
;;;               after the buffer was taken
;;;   not hoisted rank /= 1, and float element types (those have their own
;;;               raw-r8 helper)
;;;
;;; The subscript keeps its bounds check: it is narrowed to a native int, not an
;;; i4, so an out-of-range index cannot wrap into range, and the CLR's own check
;;; rejects it. An out-of-range AREF still signals a TYPE-ERROR.

(setf dotcl:*save-sil* t)

(defun %abh-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %abh-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- hoisted shapes ----

(defun %abh-fix (arr i)
  (declare (type (simple-array fixnum (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

(defun %abh-u8 (arr i)
  (declare (type (simple-array (unsigned-byte 8) (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

(defun %abh-u16 (arr i)
  (declare (type (simple-array (unsigned-byte 16) (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

(defun %abh-i32 (arr i)
  (declare (type (simple-array (signed-byte 32) (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

;; A LET binding, not a parameter: the buffer is fetched where the LET binds.
(defun %abh-let (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((a (make-array 4 :element-type 'fixnum :initial-element 0)))
    (declare (type (simple-array fixnum (*)) a))
    (setf (aref a 0) n)
    (setf (aref a 1) (the fixnum (* n 2)))
    (the fixnum (+ (the fixnum (aref a 0)) (the fixnum (aref a 1))))))

(defun %abh-set (arr i v)
  (declare (type (simple-array fixnum (*)) arr) (fixnum i v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref arr i) v))

(defun %abh-set-u8 (arr i v)
  (declare (type (simple-array (unsigned-byte 8) (*)) arr) (fixnum i v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref arr i) v))

;;; ---- shapes that must NOT be hoisted ----

;; (array ...) rather than (simple-array ...): the argument may be displaced or
;; adjustable, so the buffer cannot be taken once.
(defun %abh-array-decl (arr i)
  (declare (type (array fixnum (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

(defun %abh-vector-decl (arr i)
  (declare (type (vector fixnum) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i)))

;; The body assigns to ARR, so a buffer taken at entry could belong to a
;; different array by the time it is read.
(defun %abh-setq (arr other i)
  (declare (type (simple-array fixnum (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((first (the fixnum (aref arr i))))
    (declare (fixnum first))
    (setq arr other)
    (the fixnum (+ first (the fixnum (aref arr i))))))

(defun %abh-2d (arr i j)
  (declare (type (simple-array fixnum (* *)) arr) (fixnum i j)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (aref arr i j)))

(defun %abh-double (arr i)
  (declare (type (simple-array double-float (*)) arr) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (aref arr i)))

;;; ---- inputs ----

(defun %abh-make (n kind)
  (let ((a (make-array n :element-type kind :initial-element 0)))
    (dotimes (i n a) (setf (aref a i) (mod (* i 7) 200)))))

(defparameter *abh-fixv* (%abh-make 8 'fixnum))
(defparameter *abh-u8v* (%abh-make 8 '(unsigned-byte 8)))
(defparameter *abh-u16v* (%abh-make 8 '(unsigned-byte 16)))
(defparameter *abh-i32v* (%abh-make 8 '(signed-byte 32)))
(defparameter *abh-other* (%abh-make 8 'fixnum))
(defparameter *abh-dv*
  (let ((a (make-array 3 :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref a 0) 1.5d0)
    (setf (aref a 1) -2.25d0)
    a))
(defparameter *abh-2dv*
  (let ((a (make-array '(2 3) :element-type 'fixnum :initial-element 0)))
    (dotimes (i 2 a) (dotimes (j 3) (setf (aref a i j) (+ (* i 10) j))))))

;; An adjustable vector and a displaced one. Both have fixnum elements, so the
;; only thing standing between them and the hoisted path is the declaration --
;; which is why the functions above that accept them are declared (array ...)
;; and (vector ...), not (simple-array ...).
(defparameter *abh-adj*
  (let ((a (make-array 8 :element-type 'fixnum :initial-element 0
                         :adjustable t :fill-pointer 8)))
    (dotimes (i 8 a) (setf (aref a i) (mod (* i 7) 200)))))
(defparameter *abh-disp*
  (make-array 4 :element-type 'fixnum :displaced-to *abh-fixv*
                :displaced-index-offset 2))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY: an emit-free build stores no SIL, so every count
;;; taken from FUNCTION-SIL would be 0 for the wrong reason.

(deftest-emitting-only array-backing-hoist.kinds-take-their-own-buffer
  (list (let ((d (%abh-sil #'%abh-fix)))
          (list (%abh-count "Runtime.BackingI64" d) (%abh-count "(LDELEM-I8)" d)))
        (let ((d (%abh-sil #'%abh-u8)))
          (list (%abh-count "Runtime.BackingU8" d) (%abh-count "(LDELEM-U1)" d)))
        (let ((d (%abh-sil #'%abh-u16)))
          (list (%abh-count "Runtime.BackingU16" d) (%abh-count "(LDELEM-U2)" d)))
        (let ((d (%abh-sil #'%abh-i32)))
          (list (%abh-count "Runtime.BackingI32" d) (%abh-count "(LDELEM-I4)" d))))
  ((1 1) (1 1) (1 1) (1 1)))

;; One fetch for the whole binding, however many accesses it has.
(deftest-emitting-only array-backing-hoist.let-binding-fetches-once
  (let ((d (%abh-sil #'%abh-let)))
    (list (%abh-count "Runtime.BackingI64" d)
          (%abh-count "(LDELEM-I8)" d)
          (%abh-count "(STELEM-I8)" d)
          (%abh-count "Runtime.ArefNumL" d)))
  (1 2 2 0))

;; The narrow kinds keep the element-width check the boxed store path applies;
;; the full-width kind has nothing to check.
(deftest-emitting-only array-backing-hoist.narrow-store-is-checked
  (list (%abh-count "Runtime.CheckStoreU8" (%abh-sil #'%abh-set-u8))
        (%abh-count "Runtime.CheckStoreU8" (%abh-sil #'%abh-set))
        (%abh-count "Runtime.CheckStoreI32" (%abh-sil #'%abh-set)))
  (1 0 0))

(deftest-emitting-only array-backing-hoist.non-simple-declarations-not-hoisted
  (list (let ((d (%abh-sil #'%abh-array-decl)))
          (list (%abh-count "Runtime.Backing" d) (%abh-count "Runtime.ArefNumL" d)))
        (let ((d (%abh-sil #'%abh-vector-decl)))
          (list (%abh-count "Runtime.Backing" d) (%abh-count "Runtime.ArefNumL" d))))
  ((0 1) (0 1)))

(deftest-emitting-only array-backing-hoist.assigned-variable-not-hoisted
  (let ((d (%abh-sil #'%abh-setq)))
    (list (%abh-count "Runtime.Backing" d) (%abh-count "Runtime.ArefNumL" d)))
  (0 2))

(deftest-emitting-only array-backing-hoist.rank-2-and-float-keep-their-helpers
  (list (let ((d (%abh-sil #'%abh-2d)))
          (list (%abh-count "Runtime.Backing" d)
                (%abh-count "Runtime.ArefNum2DL" d)))
        (let ((d (%abh-sil #'%abh-double)))
          (list (%abh-count "Runtime.Backing" d) (%abh-count "Runtime.ArefNumD" d))))
  ((0 1) (0 1)))

;;; ---- values ----

(deftest array-backing-hoist.read-values
  (list (%abh-fix *abh-fixv* 3) (%abh-u8 *abh-u8v* 3)
        (%abh-u16 *abh-u16v* 3) (%abh-i32 *abh-i32v* 3))
  (21 21 21 21))

(deftest array-backing-hoist.let-value
  (%abh-let 5)
  15)

;; SETF AREF returns the value stored, and the store is visible afterwards.
(deftest array-backing-hoist.setf-returns-and-stores
  (let ((a (%abh-make 4 'fixnum)))
    (list (%abh-set a 1 1000000007) (aref a 1)))
  (1000000007 1000000007))

(deftest array-backing-hoist.setf-u8-returns-and-stores
  (let ((a (%abh-make 4 '(unsigned-byte 8))))
    (list (%abh-set-u8 a 1 250) (aref a 1)))
  (250 250))

;; A value too wide for the element type signals rather than wrapping -- the
;; same thing the boxed store path does.
(deftest array-backing-hoist.narrow-store-out-of-width-signals
  (let ((a (%abh-make 4 '(unsigned-byte 8))))
    (handler-case (progn (%abh-set-u8 a 1 256) :no-error)
      (type-error () :type-error)))
  :type-error)

;; Subscript checking survives the bare ldelem, including a subscript that an
;; i4 narrowing would have wrapped into range.
(deftest array-backing-hoist.out-of-range-signals
  (list (handler-case (progn (%abh-fix *abh-fixv* 99) :no-error)
          (type-error () :type-error))
        (handler-case (progn (%abh-fix *abh-fixv* -1) :no-error)
          (type-error () :type-error))
        (handler-case (progn (%abh-fix *abh-fixv* 4294967296) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error :type-error))

;; The same subscript through the generic, undeclared path. Every array path
;; narrows the subscript to an int eventually, and narrowing used to be a bare
;; cast: (aref v 4294967296) wrapped to (aref v 0) and returned an element.
;; This is not about the hoisted path -- it is what AREF does with no
;; declaration in sight, so it runs on the interpreter and the emit-free build
;; too, which is where the wrap was still reachable after the declared path
;; started narrowing with conv.i.
(deftest array-backing-hoist.generic-out-of-int-range-signals
  (let ((v (make-array 4 :element-type 'fixnum :initial-element 3)))
    (list (handler-case (aref v 4294967296) (type-error () :type-error))
          (handler-case (aref v -4294967296) (type-error () :type-error))
          (handler-case (setf (aref v 4294967296) 1) (type-error () :type-error))))
  (:type-error :type-error :type-error))

;; A general vector takes the same path with different storage behind it.
(deftest array-backing-hoist.generic-out-of-int-range-signals-general-vector
  (let ((v (make-array 4 :initial-element 3)))
    (list (handler-case (aref v 4294967296) (type-error () :type-error))
          (handler-case (setf (aref v 4294967296) 1) (type-error () :type-error))
          (aref v 0)))
  (:type-error :type-error 3))

;; The shapes that stay on the helper still read the right values, which is the
;; point of not hoisting them: an adjustable or displaced array has no buffer of
;; its own to take.
(deftest array-backing-hoist.adjustable-and-displaced-read-correctly
  (list (%abh-array-decl *abh-adj* 3)
        (%abh-vector-decl *abh-adj* 3)
        (%abh-array-decl *abh-disp* 0)
        (%abh-array-decl *abh-disp* 1))
  (21 21 14 21))

;; An adjustable array that grows under the binding is seen at its new size,
;; because this shape never took a buffer.
(deftest array-backing-hoist.adjustable-grows-under-the-binding
  (let ((a (make-array 2 :element-type 'fixnum :initial-element 7
                         :adjustable t :fill-pointer 2)))
    (vector-push-extend 9 a)
    (vector-push-extend 11 a)
    (list (%abh-array-decl a 0) (%abh-array-decl a 2) (%abh-array-decl a 3)))
  (7 9 11))

;; The assigned variable reads from whichever array it currently names.
(deftest array-backing-hoist.assigned-variable-reads-both-arrays
  (progn (setf (aref *abh-other* 3) 500)
         (prog1 (%abh-setq *abh-fixv* *abh-other* 3)
           (setf (aref *abh-other* 3) 21)))
  521)

(deftest array-backing-hoist.rank-2-and-float-values
  (list (%abh-2d *abh-2dv* 1 2) (%abh-double *abh-dv* 0) (%abh-double *abh-dv* 1))
  (12 1.5d0 -2.25d0))

(setf dotcl:*save-sil* nil)
