;;; A LET variable with no type declaration, bound to (MAKE-ARRAY N
;;; :ELEMENT-TYPE '<constant>) and never assigned, is compiled as if it were
;;; declared (SIMPLE-ARRAY <type> (N)) -- (*) when N is not a literal. SBCL
;;; derives the same type from the call. Its element buffer is then fetched once
;;; where the LET binds, and the AREFs and (SETF AREF)s in the body become bare
;;; element loads and stores, as they are for the declared variable. Before, each
;;; access went through the per-element helper (Runtime.ArefNumL /
;;; ArefSetNumL), which asks for the array's kind every time.
;;;
;;; Not inferred: :ADJUSTABLE, :FILL-POINTER or :DISPLACED-TO in the call
;;; (whatever the value), an element type that is not a quoted constant, a
;;; variable the body assigns, and a variable the body declares a type for
;;; itself (the declaration is what counts then).

(setf dotcl:*save-sil* t)

(defun %malb-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %malb-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- inferred ----

;; Passing X to a function keeps it a vector (see raw-local-array.lisp).
(declaim (notinline %malb-keep))
(defun %malb-keep (x) x)

(defun %malb-u32 (n)
  (declare (fixnum n))
  (let ((x (make-array 16 :element-type '(unsigned-byte 32))))
    (%malb-keep x)
    (dotimes (i 16) (setf (aref x i) (+ i n)))
    (aref x 3)))

;; The size is a variable: the type is (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
(defun %malb-u8-var (n)
  (declare (fixnum n))
  (let ((x (make-array n :element-type '(unsigned-byte 8) :initial-element 7))
        (acc 0))
    (dotimes (i n) (setq acc (+ acc (aref x i))))
    acc))

(defun %malb-contents ()
  (let ((x (make-array 4 :element-type 'fixnum :initial-contents '(10 20 30 40))))
    (+ (aref x 0) (aref x 3))))

(defun %malb-double (n)
  (declare (fixnum n))
  (let ((x (make-array n :element-type 'double-float :initial-element 1d0))
        (acc 0d0))
    (declare (double-float acc))
    (dotimes (i n) (setf (aref x i) (* 2d0 (aref x i))))
    (dotimes (i n acc) (setq acc (+ acc (aref x i))))))

;; The same as %MALB-U32 with the type written out: the two must compile alike.
(defun %malb-u32-declared (n)
  (declare (fixnum n))
  (let ((x (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (16)) x))
    (%malb-keep x)
    (dotimes (i 16) (setf (aref x i) (+ i n)))
    (aref x 3)))

;; A closure that only reads X: X is still the array the call made.
(defun %malb-closure (n)
  (declare (fixnum n))
  (let ((x (make-array 4 :element-type 'fixnum :initial-element n)))
    (dotimes (i 4) (setf (aref x i) (+ (aref x i) i)))
    (funcall (lambda () (aref x 2)))))

;;; ---- not inferred ----

;; ADJUST-ARRAY on an adjustable array replaces its storage in place, so a
;; buffer taken at the binding would be the old one.
(defun %malb-adjustable (n)
  (declare (fixnum n))
  (let ((x (make-array 4 :element-type 'fixnum :initial-element n :adjustable t)))
    (dotimes (i 4) (setf (aref x i) (+ (aref x i) i)))
    (adjust-array x 8 :initial-element 100)
    (setf (aref x 6) (+ (aref x 6) 1))
    (list (aref x 3) (aref x 6) (length x))))

(defun %malb-fill-pointer (n)
  (declare (fixnum n))
  (let ((x (make-array 4 :element-type 'fixnum :initial-element n :fill-pointer 2)))
    (dotimes (i 4) (setf (aref x i) (+ (aref x i) i)))
    (list (aref x 3) (length x))))

(defparameter *malb-base*
  (make-array 8 :element-type 'fixnum :initial-contents '(0 1 2 3 4 5 6 7)))

(defun %malb-displaced ()
  (let ((x (make-array 4 :element-type 'fixnum :displaced-to *malb-base*
                         :displaced-index-offset 2)))
    (dotimes (i 4) (setf (aref x i) (* 10 (aref x i))))
    (list (aref x 0) (aref *malb-base* 2))))

;; X is assigned: after the SETQ it names a different (adjustable) array.
(defun %malb-setq (n)
  (declare (fixnum n))
  (let ((x (make-array 4 :element-type 'fixnum :initial-element 0)))
    (when (> n 0)
      (setq x (make-array 4 :element-type 'fixnum :initial-element n :adjustable t)))
    (dotimes (i 4) (setf (aref x i) (+ (aref x i) i)))
    (aref x 3)))

;; The element type is not a constant.
(defun %malb-var-type (n et)
  (declare (fixnum n))
  (let ((x (make-array 4 :element-type et :initial-element n)))
    (dotimes (i 4) (setf (aref x i) (+ (aref x i) i)))
    (aref x 3)))

;; The body declares X itself, with a type that allows a non-simple array.
(defun %malb-own-decl (n)
  (declare (fixnum n))
  (let ((x (make-array 4 :element-type 'fixnum :initial-element n)))
    (declare (type (vector fixnum) x))
    (dotimes (i 4) (setf (aref x i) (+ (aref x i) i)))
    (aref x 3)))

;;; ---- SIL shape ----

(deftest-emitting-only make-array-let-backing.inferred-kinds-are-hoisted
  (list (let ((d (%malb-sil #'%malb-u32)))
          (list (%malb-count "Runtime.BackingI64" d) (plusp (%malb-count "(STELEM-I8)" d))
                (plusp (%malb-count "(LDELEM-I8)" d))))
        (let ((d (%malb-sil #'%malb-u8-var)))
          (list (%malb-count "Runtime.BackingU8" d) (plusp (%malb-count "(LDELEM-U1)" d))))
        (%malb-count "Runtime.BackingI64" (%malb-sil #'%malb-contents))
        (%malb-count "Runtime.BackingF64" (%malb-sil #'%malb-double)))
  ((1 t t) (1 t) 1 1))

;; The inferred binding compiles to the same instructions as the declared one.
(deftest-emitting-only make-array-let-backing.same-as-declared
  (let ((a (%malb-sil #'%malb-u32)) (b (%malb-sil #'%malb-u32-declared)))
    (loop for needle in '("Runtime.Backing" "(LDELEM-I8)" "(STELEM-I8)"
                          "Runtime.ArefNumL" "Runtime.ArefSetNumL")
          collect (= (%malb-count needle a) (%malb-count needle b))))
  (t t t t t))

(deftest-emitting-only make-array-let-backing.not-inferred
  (loop for fn in (list #'%malb-adjustable #'%malb-fill-pointer #'%malb-displaced
                        #'%malb-setq #'%malb-var-type #'%malb-own-decl)
        collect (%malb-count "Runtime.Backing" (%malb-sil fn)))
  (0 0 0 0 0 0))

;;; ---- values ----

(deftest make-array-let-backing.values
  (list (%malb-u32 5) (%malb-u32 #xfffffff0) (%malb-u8-var 10) (%malb-contents)
        (%malb-double 3) (%malb-closure 5))
  (8 #xfffffff3 70 50 6d0 7))

(deftest make-array-let-backing.not-inferred-values
  (list (%malb-adjustable 5) (%malb-fill-pointer 5) (%malb-displaced)
        (%malb-setq 0) (%malb-setq 5) (%malb-var-type 5 'fixnum)
        (%malb-var-type 5 't) (%malb-own-decl 5))
  ((8 101 8) (8 2) (20 20) 3 8 8 8 8))
