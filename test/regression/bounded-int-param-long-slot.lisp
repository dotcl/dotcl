;;; A required parameter declared a bounded integer type inside int64 ((MOD N),
;;; (UNSIGNED-BYTE 32), a DEFTYPE such as an INDEX) gets a raw Int64 slot, as a
;;; FIXNUM parameter and a LET binding of either kind already did; and so does
;;; one that the body assigns, even where the assignment is only visible after
;;; macroexpansion.
;;;
;;; Ironclad's XOR-BLOCK is the shape: four INDEX parameters stepped with INCF /
;;; DECF inside a MACROLET. All four stayed boxed objects, every step a
;;; promoting add, every read a type test and an unbox. The declaration covers
;;; every value the parameter holds, assignments included, so the raw slot is
;;; as sound for these as for FIXNUM.

(setf dotcl:*save-sil* t)

(deftype %bip-index () '(mod #.array-dimension-limit))

(defun %bip-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %bip-first-slot-type (fn)
  (let* ((s (%flat-text (dotcl:function-sil fn)))
         (p (+ (search "DECLARE-LOCAL " s) 14))
         (q (position #\Space s :start p))
         (e (position #\) s :start q)))
    (subseq s (1+ q) e)))

;; XOR-BLOCK as ironclad writes it, the byte loop only.
(defun %bip-xor-block (block-length input-block1 input-block1-start
                       input-block2 input-block2-start
                       output-block output-block-start)
  (declare (type (simple-array (unsigned-byte 8) (*)) input-block1 input-block2 output-block)
           (type %bip-index block-length input-block1-start input-block2-start
                 output-block-start)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (macrolet ((xor-bytes (size xor-form)
               `(loop until (< block-length ,size) do
                  ,xor-form
                  (incf output-block-start ,size)
                  (incf input-block1-start ,size)
                  (incf input-block2-start ,size)
                  (decf block-length ,size))))
    (xor-bytes 1 (setf (aref output-block output-block-start)
                       (logxor (aref input-block1 input-block1-start)
                               (aref input-block2 input-block2-start))))))

;; Read in a native position only, never assigned.
(defun %bip-u32-bound (n)
  (declare (type (unsigned-byte 32) n) (optimize (speed 3) (safety 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i 10 s) (when (< i n) (incf s i)))))

;; Assigned with SETQ of a generic value, and read generically: still right.
(defun %bip-assigned (n xs)
  (declare (type (integer 0 1000000) n))
  (setq n (length xs))
  (list n (+ n 1)))

;; Neither assigned nor read natively: stays an object.
(defun %bip-generic-only (n)
  (declare (type (integer 0 10) n))
  (list n n))

;; Tail self-call through the raw slots.
(defun %bip-tco (n acc)
  (declare (type %bip-index n) (fixnum acc) (optimize (speed 3) (safety 0)))
  (if (zerop n) (list acc) (%bip-tco (- n 1) (+ acc n))))

(deftest-emitting-only bounded-int-param-long-slot.xor-block-sil
  (list (%bip-first-slot-type #'%bip-xor-block)
        (%bip-count "Runtime.AddFixnum)" #'%bip-xor-block)
        (%bip-count "Runtime.SubtractFixnum)" #'%bip-xor-block)
        (%bip-count "Runtime.UnwrapMv)" #'%bip-xor-block)
        (%bip-count "Fixnum.Make)" #'%bip-xor-block)
        ;; One per INDEX parameter, in the prologue.
        (%bip-count "UNBOX-FIXNUM" #'%bip-xor-block))
  ("Int64" 0 0 0 0 4))

(deftest-emitting-only bounded-int-param-long-slot.which-slots
  (list (%bip-first-slot-type #'%bip-u32-bound)
        (%bip-first-slot-type #'%bip-assigned)
        (%bip-first-slot-type #'%bip-generic-only)
        (%bip-first-slot-type #'%bip-tco))
  ("Int64" "Int64" "LispObject" "Int64"))

(deftest bounded-int-param-long-slot.values
  (let* ((n 300)
         (a (make-array n :element-type '(unsigned-byte 8)))
         (b (make-array n :element-type '(unsigned-byte 8)))
         (c (make-array (+ n 5) :element-type '(unsigned-byte 8) :initial-element 7)))
    (dotimes (i n) (setf (aref a i) (mod (* i 7) 256) (aref b i) (mod (* i 13) 256)))
    (%bip-xor-block 290 a 3 b 10 c 5)
    (list (loop for k from 0 below 290
                always (= (aref c (+ 5 k))
                          (logxor (aref a (+ 3 k)) (aref b (+ 10 k)))))
          (list (aref c 4) (aref c 295) (aref c 299))
          (%bip-u32-bound 0) (%bip-u32-bound 4) (%bip-u32-bound #xffffffff)
          (%bip-assigned 5 '(a b c))
          (%bip-generic-only 3)
          (%bip-tco 10 0)
          (- (first (%bip-tco 70000 0)) (/ (* 70000 70001) 2))))
  (t (7 7 7) 0 6 45 (3 4) (3 3) (55) 0))

(setf dotcl:*save-sil* nil)
