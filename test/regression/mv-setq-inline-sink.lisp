;;; (MULTIPLE-VALUE-SETQ (A B) (F ...)) with F an INLINE function whose result
;;; tails are VALUES forms assigns the values at each tail instead of passing
;;; them through the multiple-value record, each boxed. So does a SETQ of such
;;; a call into a raw integer slot (the primary value). ironclad's 32-bit
;;; TIGER does both with its add-with-carry and subtract-with-borrow.

(setf dotcl:*save-sil* t)

(declaim (inline %mvs-swb %mvs-pair %mvs-three))
(defun %mvs-swb (x y borrow)
  (declare (type (unsigned-byte 32) x y) (type (mod 2) borrow))
  (let ((temp (ldb (byte 32 0) (- x y))))
    (cond ((zerop borrow) (values (ldb (byte 32 0) (- temp 1)) (if (< y x) 1 0)))
          (t (values temp (logxor (if (< x y) 1 0) 1))))))

(defun %mvs-run (x y)
  (declare (type (unsigned-byte 32) x y))
  (let ((a 0) (b 0) (c 0))
    (declare (type (unsigned-byte 32) a c) (type bit b))
    (multiple-value-setq (a b) (%mvs-swb x y 1))
    (setq c (%mvs-swb a y b))
    (list a b c)))

(defun %mvs-run-called (x y)
  (declare (notinline %mvs-swb))
  (let ((a 0) (b 0) (c 0))
    (multiple-value-setq (a b) (%mvs-swb x y 1))
    (setq c (%mvs-swb a y b))
    (list a b c)))

(deftest mv-setq-inline-sink.values
  (loop for (x y) in '((5 3) (3 5) (0 #xFFFFFFFF) (#xFFFFFFFF 0) (7 7))
        collect (equal (%mvs-run x y) (%mvs-run-called x y)))
  (t t t t t))

(deftest mv-setq-inline-sink.some-values
  (%mvs-run 5 3)
  (2 1 #xFFFFFFFF))

;; Fewer values than variables leaves NIL; more are evaluated, left to right.
(defvar *mvs-trace* nil)
(defun %mvs-pair (k)
  (if (> k 0)
      (values (progn (push :a *mvs-trace*) k) (progn (push :b *mvs-trace*) (- k)))
      (values k)))
(defun %mvs-three ()
  (values (progn (push 1 *mvs-trace*) 1) (progn (push 2 *mvs-trace*) 2)
          (progn (push 3 *mvs-trace*) 3)))

(defun %mvs-shapes ()
  (setq *mvs-trace* nil)
  (let ((p :x) (q :y) (r :z))
    (list (multiple-value-setq (p q) (%mvs-pair 4)) p q
          (multiple-value-setq (p q) (%mvs-pair 0)) p q
          (multiple-value-setq (r) (%mvs-three)) r
          (reverse *mvs-trace*))))

(deftest mv-setq-inline-sink.shapes
  (%mvs-shapes)
  (4 4 -4 0 0 nil 1 1 (:a :b 1 2 3)))

;; A value form reading the variable sees its old value.
(declaim (inline %mvs-swap))
(defun %mvs-swap (a b) (values b a))
(defun %mvs-swapped ()
  (let ((a 1) (b 2))
    (multiple-value-setq (a b) (%mvs-swap a b))
    (list a b)))

(deftest mv-setq-inline-sink.swap
  (%mvs-swapped)
  (2 1))

(defun %mvs-sil (fn) (%flat-text (dotcl:function-sil fn)))

(deftest-emitting-only mv-setq-inline-sink.no-mv-record
  (let ((s (%mvs-sil #'%mvs-run)))
    (list (search "Values2" s) (search "BindNth" s) (search "CaptureForBind" s)))
  (nil nil nil))
