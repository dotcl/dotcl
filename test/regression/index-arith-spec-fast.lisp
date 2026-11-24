;;; An array subscript that is a nested +/* of fixnums, one of whose later operands
;;; has effects or is a call ((mod c cols)), is computed on int64 with an overflow check.

(setf dotcl:*save-sil* t)

(defstruct (%ia-board (:constructor %ia-make (rows cols cells)))
  (rows 0 :type fixnum) (cols 0 :type fixnum)
  (cells nil :type (simple-array (unsigned-byte 8) (*))))

(defun %ia-ref (b r c)
  (declare (type %ia-board b) (type fixnum r c) (optimize (speed 3) (safety 1)))
  (aref (%ia-board-cells b) (+ (* (mod r (%ia-board-rows b)) (%ia-board-cols b))
                               (mod c (%ia-board-cols b)))))

(defun %ia-board (rows cols)
  (let ((cells (make-array (* rows cols) :element-type '(unsigned-byte 8))))
    (dotimes (i (* rows cols)) (setf (aref cells i) (mod i 251)))
    (%ia-make rows cols cells)))

(deftest index-arith.values-and-wraparound
  (let ((b (%ia-board 5 7)))
    (list (%ia-ref b 0 0) (%ia-ref b 1 2) (%ia-ref b 4 6) (%ia-ref b 5 7) (%ia-ref b -1 -1)
          (%ia-ref b 12 15)))
  (0 9 34 0 34 15))

;; the product overflows int64: no silent wrap into some other element
(defvar *%ia-huge* (%ia-make most-positive-fixnum most-positive-fixnum
                             (make-array 16 :element-type '(unsigned-byte 8) :initial-element 1)))
(deftest index-arith.overflow-signals
  (list (handler-case (%ia-ref *%ia-huge* (- most-positive-fixnum 1) (- most-positive-fixnum 1)) (error () :error))
        (handler-case (%ia-ref *%ia-huge* 0 3) (error () :error)))
  (:error 1))

;; evaluation order of an operand with effects after the arithmetic in front of it
(defvar *%ia-log* nil)
(defun %ia-note (x) (push x *%ia-log*) x)
(defun %ia-order (v n)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type fixnum n))
  (setf *%ia-log* nil)
  (list (aref v (+ (* (%ia-note n) 2) (%ia-note 1))) (reverse *%ia-log*)))
(deftest index-arith.operand-order
  (%ia-order (coerce (loop for i below 12 collect i) '(simple-array (unsigned-byte 8) (*))) 3)
  (7 (3 1)))

(deftest-compiled-only index-arith.native-fast-path
  (let ((sil (dotcl:function-sil #'%ia-ref)))
    (and (search "MultiplyOrMin" (%flat-text sil)) t))
  t)
