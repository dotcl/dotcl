;;; The 64-bit lane reads an (unsigned-byte 64) local operand straight from the
;;; local's slot when every operand of the expression is a plain read. It used to
;;; copy each such operand into a fresh local first (the value at evaluation
;;; time, in case a later operand assigns the variable): an IL local and two
;;; local references per operand, 3,114 copies in ironclad's THREEFISH1024, past
;;; the JIT's 8,000 references. An operand that does assign keeps the copy.

(setf dotcl:*save-sil* t)

(defun %ulc-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %ulc-pure (w)
  (declare (type (simple-array (unsigned-byte 64) (*)) w)
           (optimize speed (safety 0)))
  (let ((a (aref w 0)) (b (aref w 1)))
    (declare (type (unsigned-byte 64) a b))
    (setf (aref w 2) (ldb (byte 64 0) (+ a b (logxor a b))))
    w))

(defun %ulc-assigning (w)
  (declare (type (simple-array (unsigned-byte 64) (*)) w)
           (optimize speed (safety 0)))
  (let ((a (aref w 0)) (b (aref w 1)))
    (declare (type (unsigned-byte 64) a b))
    ;; A is read before the second operand assigns it: the sum uses the old A.
    (setf (aref w 2) (ldb (byte 64 0) (+ a (progn (setf a 7) b))))
    (setf (aref w 3) a)
    w))

(deftest-compiled-only ulc-pure-operands-not-copied
  (%ulc-sil-count #'%ulc-pure "U64O")
  0)

(deftest ulc-values
  (flet ((run (fn)
           (let ((w (make-array 4 :element-type '(unsigned-byte 64) :initial-element 0)))
             (setf (aref w 0) #xF000000000000001 (aref w 1) #x1000000000000002)
             (funcall fn w)
             (list (aref w 2) (aref w 3)))))
    (list (run #'%ulc-pure) (run #'%ulc-assigning)))
  ((#xE000000000000006 0) (#x3 7)))
