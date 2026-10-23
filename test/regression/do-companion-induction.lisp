;;; A DO variable stepped by a literal alongside the counting variable takes as
;;; many steps as it does, so it is bounded too: (do ((i 0 (+ i 2)) (j 0 (+ j 2)))
;;; ((>= i 32)) ...) gives J the range [0, 32]. ironclad's DES key schedule
;;; indexes its output with such a J, which went through the generic + and AREF.

(setf dotcl:*save-sil* t)

(defun %dci-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %dci-pairs (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (let ((out (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)))
    (do ((i 0 (+ i 2))
         (j 0 (+ j 2)))
        ((>= i 16) out)
      (setf (aref out (ash j -1)) (logxor (aref v i) (aref v (1+ i)))))))

(defun %dci-down (n)
  (let ((r '()))
    (do ((i 0 (1+ i))
         (k 100 (- k 7)))
        ((= i n) (nreverse r))
      (push k r))))

;; Assigned in the body: no range is claimed, the values are still right.
(defun %dci-assigned (n)
  (let ((r '()))
    (do ((i 0 (1+ i))
         (k 0 (+ k 1)))
        ((>= i n) (nreverse r))
      (when (= i 2) (setq k 1000))
      (push k r))))

;; Large steps: the bound leaves int64, so no range; the values become bignums.
(defun %dci-wide (n)
  (let ((last nil))
    (do ((i 0 (1+ i))
         (k most-positive-fixnum (+ k most-positive-fixnum)))
        ((>= i n) last)
      (setq last k))))

;; Parallel stepping is kept: the other variable's step sees the old K.
(defun %dci-parallel (n)
  (let ((r '()))
    (do ((i 0 (1+ i))
         (k 0 (+ k 3))
         (prev -1 k))
        ((>= i n) (nreverse r))
      (push (list k prev) r))))

(defparameter *dci-v*
  (make-array 16 :element-type '(unsigned-byte 32)
                 :initial-contents (loop for x below 16 collect (* x #x11111111))))

(deftest do-companion-induction.values
  (list (coerce (%dci-pairs *dci-v*) 'list)
        (%dci-down 4)
        (%dci-assigned 5)
        (%dci-wide 3)
        (%dci-parallel 3))
  ((#x11111111 #x11111111 #x11111111 #x11111111
    #x11111111 #x11111111 #x11111111 #x11111111)
   (100 93 86 79)
   (0 1 1000 1001 1002)
   #.(* 3 most-positive-fixnum)
   ((0 -1) (3 0) (6 3))))

(deftest-emitting-only do-companion-induction.typed
  (let ((d (%dci-sil #'%dci-pairs)))
    (list (search "Runtime.Add)" d) (search "Runtime.Increment)" d)
          (search "Runtime.ArefSet)" d)))
  (nil nil nil))
