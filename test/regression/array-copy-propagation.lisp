;;; A variable bound to another one holding an array of known integer or float
;;; storage reads and writes that storage directly, also when it has the same
;;; name as the variable it copies: the parameter of an inline local function
;;; called with a variable of its own name becomes (LET ((A A)) ...). The
;;; storage is taken from the binding the init actually refers to: the one
;;; outside a LET, possibly an earlier one in a LET*, never one a LAMBDA or
;;; another binding has shadowed.

(setf dotcl:*save-sil* t)

(defun %acp-fill (src a)
  (declare (type (simple-array (unsigned-byte 32) (*)) src) (fixnum a))
  (flet ((fill4 (n) (dotimes (i 4) (setf (aref n i) (aref src (+ a i))))))
    (declare (inline fill4))
    (let ((n (make-array 4 :element-type '(unsigned-byte 32))))
      (fill4 n)
      (list (aref n 0) (aref n 3)))))

(defun %acp-let (a)
  (declare (type (simple-array fixnum (*)) a))
  (let ((a a)) (setf (aref a 0) 9) (aref a 1)))

(defun %acp-lambda (v)
  (let ((n (make-array 3 :element-type 'fixnum :initial-element 5)))
    (list (aref n 0) (funcall (lambda (n) (let ((m n)) (aref m 1))) v))))

(defun %acp-flet (v)
  (let ((n (make-array 3 :element-type 'fixnum :initial-element 5)))
    (flet ((f (n) (let ((m n)) (aref m 1))))
      (list (aref n 0) (f v)))))

(defun %acp-parallel (v)
  (let ((n v))
    (let ((n (make-array 3 :element-type 'fixnum :initial-element 5)) (m n))
      (list (aref n 0) (aref m 1)))))

(defun %acp-sequential (v)
  (let ((n v))
    (let* ((n (make-array 3 :element-type 'fixnum :initial-element 5)) (m n))
      (list (aref n 0) (aref m 1)))))

(defun %acp-sequential-shadow ()
  (let ((n (make-array 3 :element-type 'fixnum :initial-element 5)))
    (let* ((n (vector :a :b)) (m n))
      (list (aref m 1) (let ((n n)) (aref n 0))))))

(defun %acp-same-name-outer (v)
  (let ((n (make-array 3 :element-type 'fixnum :initial-element 5)))
    (list (aref n 0)
          (funcall (lambda (n) (let ((n n)) (aref n 1))) v))))

(deftest array-copy-propagation.values
  (list (%acp-fill (make-array 6 :element-type '(unsigned-byte 32)
                                 :initial-contents '(1 2 3 4 5 6))
                   1)
        (let ((a (make-array 2 :element-type 'fixnum :initial-contents '(1 2))))
          (list (%acp-let a) (aref a 0)))
        (%acp-lambda (vector :c :d))
        (%acp-flet (vector :c :d))
        (%acp-parallel (vector :a :b))
        (%acp-sequential (vector :a :b))
        (%acp-sequential-shadow)
        (%acp-same-name-outer (vector :e :f))
        (%acp-same-name-outer (make-array 2 :element-type 'fixnum :initial-contents '(7 8))))
  ((2 5) (2 9) (5 :d) (5 :d) (5 :b) (5 5) (:b :a) (5 :f) (5 8)))

(defun %acp-sil-has (fn text) (and (search text (%flat-text (dotcl:function-sil fn))) t))

(deftest-compiled-only array-copy-propagation.sil
  (list (%acp-sil-has #'%acp-fill "ArefSetNumL")
        (%acp-sil-has #'%acp-fill "ArefSetLong")
        (%acp-sil-has #'%acp-let "ArefSetL\"")
        (%acp-sil-has #'%acp-let "ArefL\""))
  (t nil nil nil))

(setf dotcl:*save-sil* nil)
