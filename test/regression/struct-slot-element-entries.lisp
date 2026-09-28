;;; Element reads other than AREF through a structure accessor.
;;;
;;; A slot's :TYPE reaches AREF (struct-slot-array-aref.lisp). The other
;;; element entries are pinned here:
;;;
;;; - SVREF / SCHAR / CHAR already compile to one typed runtime call
;;;   (Runtime.ArefL / Runtime.CharAtL) whatever the operand is. Their element
;;;   type is fixed by the operator itself, so a slot declaration has nothing
;;;   left to add, and the accessor form is already on the same call a declared
;;;   local gets. The tests below keep them off the generic named call.
;;; - ROW-MAJOR-AREF on a rank-1 array is that array's AREF, so when the array
;;;   is known to be numeric-backed (a declared local, or an accessor whose slot
;;;   says so) it takes AREF's raw element path, reads and writes. Any other
;;;   rank, or an array nothing is known about, keeps the full call.

(setf dotcl:*save-sil* t)

(defun %sse-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %sse-has (needle fn) (and (search needle (%sse-sil fn)) t))

;;; ---- the structure ----

(defstruct sse
  (sv (vector) :type simple-vector)
  (ss "" :type simple-string)
  (st "" :type string)
  (fix (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (dbl (make-array 0 :element-type 'double-float)
       :type (simple-array double-float (*)))
  (m2 (make-array '(0 0) :element-type 'fixnum)
      :type (simple-array fixnum (* *)))
  (any (vector)))

;;; ---- the shapes ----

(defun %sse-svref (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (svref (sse-sv s) i))

(defun %sse-schar (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (schar (sse-ss s) i))

(defun %sse-schar-code (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ 1 (char-code (schar (sse-ss s) i)))))

(defun %sse-char (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (char (sse-st s) i))

(defun %sse-rma (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ 1 (the fixnum (row-major-aref (sse-fix s) i)))))

(defun %sse-rma-set (s i v)
  (declare (type sse s) (fixnum i v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (row-major-aref (sse-fix s) i) v))

(defun %sse-rma-dbl (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (row-major-aref (sse-dbl s) i)))

(defun %sse-rma-m2 (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (row-major-aref (sse-m2 s) i)))

(defun %sse-rma-any (s i)
  (declare (type sse s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (row-major-aref (sse-any s) i))

;; The same reads on a declared local, which is where the accessor forms have
;; to land.
(defun %sse-rma-local (a i)
  (declare (type (simple-array fixnum (*)) a) (fixnum i)
           (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ 1 (the fixnum (row-major-aref a i)))))

(defun %sse-rma-local-set (a i v)
  (declare (type (simple-array fixnum (*)) a) (fixnum i v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (row-major-aref a i) v))

(defun %sse-rma-sum (s n)
  (declare (type sse s) (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (dotimes (i n acc)
      (setq acc (the fixnum (+ acc (the fixnum (row-major-aref (sse-fix s) i))))))))

;;; ---- evaluation order of the store ----

(defparameter *sse-order* '())

(defun %sse-note (tag x)
  (push tag *sse-order*)
  x)

(defun %sse-rma-ordered-set (s i v)
  (declare (type sse s) (fixnum i v) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (row-major-aref (sse-fix (%sse-note :array s))
                        (the fixnum (%sse-note :index i)))
        (the fixnum (%sse-note :value v))))

;;; ---- a slot whose contents contradict its declaration ----

(defun %sse-lie (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s (make-sse)))
    (setf (sse-fix s) v)
    s))

;;; ---- inputs ----

(defun %sse-make ()
  (make-sse :sv (vector :a :b :c)
            :ss (coerce "abc" 'simple-string)
            :st (make-array 3 :element-type 'character :fill-pointer 3
                              :initial-contents "xyz")
            :fix (let ((a (make-array 4 :element-type 'fixnum :initial-element 0)))
                   (dotimes (i 4 a) (setf (aref a i) (* i 10))))
            :dbl (make-array 2 :element-type 'double-float
                               :initial-contents '(1.5d0 -2.25d0))
            :m2 (make-array '(2 2) :element-type 'fixnum
                                   :initial-contents '((1 2) (3 4)))
            :any (vector 7 8)))

;;; ---- values ----

(deftest struct-slot-element-entries.svref
  (let ((s (%sse-make))) (list (%sse-svref s 0) (%sse-svref s 2)))
  (:a :c))

(deftest struct-slot-element-entries.schar
  (let ((s (%sse-make)))
    (list (%sse-schar s 0) (%sse-schar s 2) (%sse-schar-code s 1)))
  (#\a #\c 99))

(deftest struct-slot-element-entries.char
  (let ((s (%sse-make))) (list (%sse-char s 0) (%sse-char s 2)))
  (#\x #\z))

(deftest struct-slot-element-entries.row-major-aref
  (let ((s (%sse-make)))
    (list (%sse-rma s 0) (%sse-rma s 3)
          (%sse-rma-local (sse-fix s) 3)
          (%sse-rma-dbl s 1)
          (%sse-rma-m2 s 3)
          (%sse-rma-any s 1)
          (%sse-rma-sum s 4)))
  (1 31 31 -2.25d0 4 8 60))

(deftest struct-slot-element-entries.row-major-aref-set
  (let* ((s (%sse-make))
         (a (make-array 2 :element-type 'fixnum :initial-element 0)))
    (list (%sse-rma-set s 2 1000000007) (aref (sse-fix s) 2)
          (%sse-rma-local-set a 1 -5) (aref a 1)))
  (1000000007 1000000007 -5 -5))

(deftest struct-slot-element-entries.row-major-aref-out-of-range
  (let ((s (%sse-make)))
    (list (handler-case (progn (%sse-rma s 4) :no-error)
            (error () :error))
          (handler-case (progn (%sse-rma s -1) :no-error)
            (error () :error))
          (handler-case (progn (%sse-rma-set s 4 0) :no-error)
            (error () :error))
          (handler-case (progn (%sse-svref s 3) :no-error)
            (error () :error))
          (handler-case (progn (%sse-schar s 3) :no-error)
            (error () :error))))
  (:error :error :error :error :error))

(deftest struct-slot-element-entries.row-major-aref-order
  (let ((*sse-order* '())
        (s (%sse-make)))
    (list (%sse-rma-ordered-set s 1 42) (aref (sse-fix s) 1) (reverse *sse-order*)))
  (42 42 (:array :index :value)))

;; A slot holding an element-type T vector against its declaration still reads
;; and writes the right values: the runtime re-checks the backing.
(deftest struct-slot-element-entries.declaration-not-honored-rank-1
  (let ((s (%sse-lie (vector 5 6 7))))
    (list (%sse-rma s 2) (%sse-rma-set s 0 9) (%sse-rma s 0)))
  (8 9 10))

;; A slot declared rank 1 holding a rank-2 array. The compiled read takes the
;; declaration's word that the row-major index is the subscript, and the
;; fallback then subscripts a rank-2 array with one index: an error, never a
;; wrong element. A declared local answers with an error too, at its binding.
;; The interpreter ignores the declaration and reads the element.
(deftest-emitting-only struct-slot-element-entries.declaration-not-honored-rank-2
  (let ((m (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
    (list (handler-case (progn (%sse-rma (%sse-lie m) 3) :no-error)
            (error () :error))
          (handler-case (progn (%sse-rma-set (%sse-lie m) 3 0) :no-error)
            (error () :error))
          (aref m 1 1)))
  (:error :error 4))

;;; ---- the compiled shape ----

;; No generic named call: each is one typed runtime entry.
(deftest-emitting-only struct-slot-element-entries.sil-svref-schar-char
  (list (%sse-has "Runtime.ArefL" #'%sse-svref)
        (%sse-has "GetFunctionBySymbol" #'%sse-svref)
        (%sse-has "Runtime.CharAtL" #'%sse-schar)
        (%sse-has "GetFunctionBySymbol" #'%sse-schar)
        (%sse-has "Runtime.CharAtL" #'%sse-char)
        (%sse-has "GetFunctionBySymbol" #'%sse-char))
  (t nil t nil t nil))

(deftest-emitting-only struct-slot-element-entries.sil-row-major-aref
  (list (%sse-has "Runtime.ArefNumL" #'%sse-rma)
        (%sse-has "ROW-MAJOR-AREF" #'%sse-rma)
        (%sse-has "Runtime.ArefSetNumL" #'%sse-rma-set)
        (%sse-has "ROW-MAJOR-AREF" #'%sse-rma-set)
        (%sse-has "ROW-MAJOR-AREF" #'%sse-rma-dbl)
        (%sse-has "ROW-MAJOR-AREF" #'%sse-rma-sum)
        ;; The declared local also gets the hoisted buffer, as its AREF does.
        (%sse-has "LDELEM-I8" #'%sse-rma-local)
        (%sse-has "ROW-MAJOR-AREF" #'%sse-rma-local))
  (t nil t nil nil nil t nil))

;; Rank 2 and an undeclared slot stay on the full call.
(deftest-emitting-only struct-slot-element-entries.sil-row-major-aref-generic
  (list (%sse-has "ROW-MAJOR-AREF" #'%sse-rma-m2)
        (%sse-has "ROW-MAJOR-AREF" #'%sse-rma-any))
  (t t))
