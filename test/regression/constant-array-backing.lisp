;;; A DEFCONSTANT whose value is a rank-1 simple array with an unboxed buffer,
;;; read with AREF inside a loop of a DEFUN: the buffer is fetched once in the
;;; function's prologue, and each element is a bare load from it, as for a
;;; parameter declared with that array type. ironclad's DES key schedule reads
;;; its permutation tables this way (about 1,700 reads per key).

(defconstant +cab-u8+
  (make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 250 251 254 255)))
(defconstant +cab-u16+
  (make-array 4 :element-type '(unsigned-byte 16) :initial-contents '(0 1 65534 65535)))
(defconstant +cab-s32+
  (make-array 4 :element-type '(signed-byte 32) :initial-contents '(-2147483648 -1 0 2147483647)))
(defconstant +cab-u32+
  (make-array 4 :element-type '(unsigned-byte 32) :initial-contents '(0 1 #x80000000 #xFFFFFFFF)))
(defconstant +cab-u64+
  (make-array 3 :element-type '(unsigned-byte 64) :initial-contents '(0 #x8000000000000000 #xFFFFFFFFFFFFFFFF)))
(defconstant +cab-f64+
  (make-array 3 :element-type 'double-float :initial-contents '(0d0 -1.5d0 1d300)))
(defconstant +cab-2d+
  (make-array '(2 2) :element-type '(unsigned-byte 8) :initial-contents '((1 2) (3 4))))

(setf dotcl:*save-sil* t)

(defun %cab-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %cab-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %cab-sum-u8 ()
  (let ((s 0)) (dotimes (i 8) (incf s (aref +cab-u8+ i))) s))
(defun %cab-list (n)
  (let ((r '()))
    (dotimes (i n)
      (push (list (aref +cab-u16+ (mod i 4)) (aref +cab-s32+ (mod i 4))
                  (aref +cab-u32+ (mod i 4)) (aref +cab-u64+ (mod i 3))
                  (aref +cab-f64+ (mod i 3)))
            r))
    (nreverse r)))
(defun %cab-2d ()
  (let ((s 0)) (dotimes (i 2) (dotimes (j 2) (incf s (aref +cab-2d+ i j)))) s))
(defun %cab-once (i) (aref +cab-u8+ i))
(defun %cab-out-of-range (i)
  (let ((s 0)) (dotimes (k 1) (incf s (aref +cab-u8+ (+ i k)))) s))

(deftest constant-array-backing.values
  (list (%cab-sum-u8) (%cab-list 4) (%cab-2d) (%cab-once 7))
  (1016
   ((0 -2147483648 0 0 0d0) (1 -1 1 #x8000000000000000 -1.5d0)
    (65534 0 #x80000000 #xFFFFFFFFFFFFFFFF 1d300)
    (65535 2147483647 #xFFFFFFFF 0 0d0))
   10 255))

(deftest constant-array-backing.out-of-range-signals
  (handler-case (progn (%cab-out-of-range 8) :no-error)
    (error () :error))
  :error)

;; The loop reads take the hoisted buffer: one fetch, no per-element helper in
;; the copy that has the buffer. A read outside any loop keeps the helper.
(deftest-emitting-only constant-array-backing.hoisted
  (list (let ((d (%cab-sil #'%cab-sum-u8)))
          (list (%cab-count "Runtime.BackingU8" d) (%cab-count "LDELEM-U1" d)))
        (let ((d (%cab-sil #'%cab-list)))
          (list (%cab-count "Runtime.BackingU16" d) (%cab-count "Runtime.BackingI32" d)
                (%cab-count "Runtime.BackingI64" d) (%cab-count "Runtime.BackingU64" d)
                (%cab-count "Runtime.BackingF64" d)))
        (%cab-count "Runtime.Backing" (%cab-sil #'%cab-once)))
  ((1 1) (1 1 1 1 1) 0))

;; The same reads written through MACROLET macros, the way ironclad's AES
;; rounds name their table lookups: a macro whose definition mentions the
;; constant, called inside a loop, counts as reading it there. A MACROLET around
;; the DEFUN counts too. The macros are not expanded to find out.
(defmacro %cab-g (i) `(aref +cab-u32+ (logand ,i 3)))

(defun %cab-macrolet (n)
  (declare (fixnum n))
  (macrolet ((ref (i) `(aref +cab-u32+ (logand ,i 3)))
             (mix (a b) `(logxor (ref ,a) (ref ,b))))
    (let ((s 0))
      (declare (type (unsigned-byte 32) s))
      (dotimes (i n) (setf s (logxor s (mix i (+ i 1)) (%cab-g (+ i 2)))))
      s)))

(macrolet ((tab (i) `(aref +cab-u16+ (logand ,i 3))))
  (defun %cab-outer-macrolet (n)
    (let ((s 0))
      (dotimes (i n) (incf s (tab i)))
      s)))

(deftest constant-array-backing.through-macros
  (list (%cab-macrolet 7) (%cab-outer-macrolet 6))
  (4294967294 131071))

(deftest-emitting-only constant-array-backing.through-macros-hoisted
  (list (%cab-count "Runtime.BackingI64" (%cab-sil #'%cab-macrolet))
        (%cab-count "Runtime.BackingU16" (%cab-sil #'%cab-outer-macrolet)))
  (1 1))

;; No loop, but the table is read often (a round written out in full, as in
;; ironclad's Kupyna): the buffer is still fetched once. Rank 2 too.
(defconstant +cab-t2+
  (let ((a (make-array '(2 4) :element-type '(unsigned-byte 64))))
    (dotimes (i 2 a)
      (dotimes (j 4)
        (setf (aref a i j) (ldb (byte 64 0) (* (+ 1 j (* 5 i)) #x9E3779B97F4A7C15)))))))

(defun %cab-unrolled (x)
  (declare (type (simple-array (unsigned-byte 64) (*)) x))
  (macrolet ((m (a b s) `(aref +cab-t2+ ,a (logand (ash (aref x ,b) ,s) 3))))
    (setf (aref x 2) (logxor (m 0 0 0) (m 1 0 -2) (m 0 1 -4) (m 1 1 -6))))
  (aref x 2))

(defun %cab-twice (i)
  (logxor (aref +cab-t2+ 0 i) (aref +cab-t2+ 1 i)))

(deftest constant-array-backing.unrolled
  (list (%cab-unrolled (make-array 3 :element-type '(unsigned-byte 64)
                                     :initial-contents '(#xB4 #x6C 0)))
        (%cab-twice 3))
  (4940752896428148778 17811351012212452585))

(deftest-emitting-only constant-array-backing.unrolled-hoisted
  (list (%cab-count "Runtime.BackingNDU64" (%cab-sil #'%cab-unrolled))
        (%cab-count "Runtime.ArefU64Bits2D" (%cab-sil #'%cab-unrolled))
        (%cab-count "Runtime.Backing" (%cab-sil #'%cab-twice)))
  (1 0 0))

;; Reads that reach the body from an INLINE function or a global macro, the way
;; ironclad's Groestl (GROESTL-TABLE) and Streebog (STREEBOG-AX, whose table is
;; a LOAD-TIME-VALUE bound to a declared local) write them: the DEFUN fetches
;; the buffer once in its prologue, however often the expansion reads it.
(defconstant +cab-g+
  (let ((a (make-array 16 :element-type '(unsigned-byte 64))))
    (dotimes (i 16 a)
      (setf (aref a i) (ldb (byte 64 0) (* (+ i 3) #xC2B2AE3D27D4EB4F))))))

(declaim (inline %cab-gt))
(defun %cab-gt (i)
  (declare (type (integer 0 15) i))
  (aref +cab-g+ i))

(defmacro %cab-gcol (w)
  `(logxor (%cab-gt (logand ,w 15)) (%cab-gt (logand (ash ,w -4) 15))
           (%cab-gt (logand (ash ,w -8) 15)) (%cab-gt (logand (ash ,w -12) 15))))

(defun %cab-ground (x)
  (declare (type (simple-array (unsigned-byte 64) (4)) x))
  (setf (aref x 0) (%cab-gcol (aref x 1))
        (aref x 1) (%cab-gcol (aref x 2))
        (aref x 2) (%cab-gcol (aref x 3))
        (aref x 3) (%cab-gcol (aref x 0)))
  x)

(declaim (inline %cab-ax))
(defun %cab-ax (i j)
  (declare (type (integer 0 1) i) (type (integer 0 3) j))
  (let ((tab (load-time-value +cab-t2+ t)))
    (declare (type (simple-array (unsigned-byte 64) (2 4)) tab))
    (aref tab i j)))

(defun %cab-axs (w)
  (declare (type (unsigned-byte 64) w))
  (logxor (%cab-ax 0 (logand w 3)) (%cab-ax 1 (logand (ash w -2) 3))
          (%cab-ax 0 (logand (ash w -4) 3)) (%cab-ax 1 (logand (ash w -6) 3))
          (%cab-ax 0 (logand (ash w -8) 3))))

(defun %cab-gt-once (i) (%cab-gt i))

;; Local functions and closures compiled to methods of their own read the table
;; through their own fetch, not the enclosing DEFUN's.
(defun %cab-local (n)
  (labels ((r (k acc)
             (if (zerop k) acc (r (1- k) (logxor acc (%cab-gt (logand k 15)))))))
    (flet ((g (i) (%cab-gt (logand (+ i 5) 15))))
      (declare (notinline g))
      (list (r n 0) (funcall (lambda (i) (%cab-gt i)) 2) (g n) (%cab-gt 1)))))

(deftest constant-array-backing.through-inline
  (list (coerce (%cab-ground (make-array 4 :element-type '(unsigned-byte 64)
                                           :initial-contents '(1 #x1234 #xFFFF #xABCDEF)))
                'list)
        (%cab-axs #x2D7)
        (%cab-gt-once 15)
        (%cab-local 20))
  ((388726871196752196 0 2170457961748256480 813683751651750724)
   4783503593040069743
   12722739645922184078
   (4195834372608134948 14807104613356443787 2332911739378272180 777637246459424060)))

;; A rank 2 table of (UNSIGNED-BYTE 32), whose buffer is wider than the element
;; type, read at the default safety through a global macro and written out.
(defconstant +cab-w2+
  (let ((a (make-array '(2 4) :element-type '(unsigned-byte 32))))
    (dotimes (i 8 a) (setf (row-major-aref a i) (* (+ i 1) #x1F00001B)))))
(defmacro %cab-w2 (i j) `(aref +cab-w2+ ,i ,j))
(defun %cab-w2-macro (k)
  (+ (%cab-w2 0 (logand k 3)) (* 3 (%cab-w2 1 (logand (ash k -2) 3)))
     (%cab-w2 0 1) (%cab-w2 1 2)))
(defun %cab-w2-written (k)
  (+ (aref +cab-w2+ 0 (logand k 3)) (* 3 (aref +cab-w2+ 1 (logand (ash k -2) 3)))
     (aref +cab-w2+ 0 1) (aref +cab-w2+ 1 2)))

(deftest constant-array-backing.narrow-rank-2
  (list (%cab-w2-macro 7) (%cab-w2-macro 13) (%cab-w2-written 7) (%cab-w2-written 13))
  (16122905413 18203280305 16122905413 18203280305))

;; %CAB-GROUND fetches X's buffer and the table's.
(deftest-emitting-only constant-array-backing.through-inline-hoisted
  (list (let ((d (%cab-sil #'%cab-ground)))
          (list (%cab-count "Runtime.BackingU64Checked" d) (%cab-count "GetConstant" d)))
        (let ((d (%cab-sil #'%cab-axs)))
          (list (%cab-count "Runtime.BackingNDU64" d) (%cab-count "GetConstant" d))))
  ((2 1) (1 1)))
