;;; Regression tests for forms whose value is discarded (a statement of a body).
;;; A SETF of a place whose (SETF F) is an INLINE function expands to a LET*
;;; around the substituted body, a LET around a BLOCK that ends by returning the
;;; new value. In statement position that value was still built: boxed with
;;; Fixnum.Make, then unwrapped and popped (ironclad's SALSA output loop, three
;;; boxes per word). The LET, LET* and BLOCK of a discarded form now discard their
;;; last form as well.

(setf dotcl:*save-sil* t)

(defun %dsc-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(declaim (inline (setf %dsc-u16) (setf %dsc-u32)))
(defun (setf %dsc-u16) (val v off)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (setf (aref v off) (ldb (byte 8 0) val)
        (aref v (1+ off)) (ldb (byte 8 8) val))
  val)
(defun (setf %dsc-u32) (val v off)
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (setf (%dsc-u16 v off) (logand val #xffff)
        (%dsc-u16 v (+ off 2)) (ldb (byte 16 16) val))
  val)

(defun %dsc-out16 (buf x)
  (declare (type (simple-array (unsigned-byte 8) (64)) buf)
           (type (simple-array (unsigned-byte 32) (16)) x)
           (optimize speed))
  (dotimes (i 16)
    (setf (%dsc-u32 buf (* i 4)) (aref x i)))
  buf)

(deftest-compiled-only dsc-out16-no-box
  (list (%dsc-sil-count #'%dsc-out16 "Fixnum.Make")
        (%dsc-sil-count #'%dsc-out16 "UnwrapMv"))
  (0 2))

(deftest dsc-out16-value
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8)))
        (x (make-array 16 :element-type '(unsigned-byte 32))))
    (dotimes (i 16) (setf (aref x i) (+ #x01020304 (* i #x0F0F0F0F))))
    (%dsc-out16 buf x)
    (list (coerce (subseq buf 0 8) 'list) (coerce (subseq buf 60 64) 'list)))
  ((4 3 2 1 19 18 17 16) (229 228 227 226)))

;; The value of the same SETF is still there where it is used.
(defun %dsc-setf-value (buf w)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type (unsigned-byte 32) w))
  (list (setf (%dsc-u32 buf 0) w) (aref buf 3)))

(deftest dsc-setf-value-used
  (%dsc-setf-value (make-array 4 :element-type '(unsigned-byte 8)) #xA1B2C3D4)
  (#xA1B2C3D4 #xA1))

;;; ---- the multiple-values channel ----

;; The last form of a discarded LET / BLOCK publishes values; they must not be
;; seen by the form that reads the body's values.
(defun %dsc-mv-let (n)
  (multiple-value-list (progn (let ((d 2)) (floor n d)) (car (list n)))))
(defun %dsc-mv-block (n)
  (multiple-value-list (progn (block b (floor n 2)) (car (list n)))))
(defun %dsc-mv-tagbody (n)
  (let ((r nil))
    (dotimes (i 2) (let ((d 3)) (floor n d)) (setq r (multiple-value-list (car (list i)))))
    r))

(deftest dsc-mv-not-leaked
  (list (%dsc-mv-let 7) (%dsc-mv-block 7) (%dsc-mv-tagbody 7))
  ((7) (7) (1)))

;;; ---- BLOCK ----

(defmacro %dsc-leave (v) `(return-from %dsc-blk ,v))

;; A RETURN-FROM produced by a macro, not visible in the source of the BLOCK.
(defun %dsc-macro-return (flag)
  (let ((log '()))
    (block %dsc-blk
      (push 1 log)
      (when flag (%dsc-leave :out))
      (push 2 log))
    (push 3 log)
    (reverse log)))

;; A RETURN-FROM written in the BLOCK.
(defun %dsc-visible-return (flag)
  (let ((log '()))
    (block b
      (push 1 log)
      (when flag (return-from b :out))
      (push 2 log))
    (push 3 log)
    (reverse log)))

;; A RETURN out of a loop nested in the discarded BLOCK NIL of another loop.
(defun %dsc-nested-nil (lists)
  (let ((n 0))
    (dolist (l lists)
      (block nil
        (dolist (x l) (when (minusp x) (return)) (incf n x))
        (incf n 100)))
    n))

(deftest dsc-block-returns
  (list (%dsc-macro-return nil) (%dsc-macro-return t)
        (%dsc-visible-return nil) (%dsc-visible-return t)
        (%dsc-nested-nil '((1 2) (3 -1 5) (4))))
  ((1 2 3) (1 3) (1 2 3) (1 3) 310))

;;; ---- LET ----

(defvar *dsc-special* 0)

;; A discarded LET that binds a special variable keeps its try/finally.
(defun %dsc-special-let ()
  (let ((seen nil))
    (let ((*dsc-special* 5)) (setq seen *dsc-special*))
    (list seen *dsc-special*)))

;; A discarded LET whose last form is a conditional, a SETQ, a call.
(defun %dsc-let-forms (k)
  (let ((acc '()) (z 0))
    (let ((a k)) (if (> a 1) (push :big acc) (push :small acc)))
    (let* ((a k) (b (* a 2))) (setq z b))
    (let ((a k)) (push a acc))
    (list (reverse acc) z)))

(deftest dsc-let-forms
  (list (%dsc-special-let) (%dsc-let-forms 2) (%dsc-let-forms 0))
  ((5 0) ((:big 2) 4) ((:small 0) 0)))

;;; ---- INLINE calls ----

(declaim (inline %dsc-note))
(defun %dsc-note (box v)
  (push v (car box))
  (length (car box)))

;; An INLINE function called for effect, and one whose (SETF F) returns from its
;; own block.
(declaim (inline (setf %dsc-first)))
(defun (setf %dsc-first) (v cell)
  (when (minusp v) (return-from %dsc-first :negative))
  (setf (car cell) v)
  v)

(defun %dsc-inline-calls ()
  (let ((box (list nil)) (cell (list 0)) (r nil))
    (%dsc-note box 1)
    (%dsc-note box 2)
    (setf (%dsc-first cell) 7)
    (setf (%dsc-first cell) -1)
    (setq r (setf (%dsc-first cell) -2))
    (list (car box) (car cell) r)))

(deftest dsc-inline-calls
  (%dsc-inline-calls)
  ((2 1) 7 :negative))
