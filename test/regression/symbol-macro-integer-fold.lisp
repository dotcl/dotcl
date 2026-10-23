;;; A symbol macro that expands to an integer -- (symbol-macrolet ((i 3)) ...),
;;; how an unrolled loop names its index -- is seen as that integer by the
;;; typed arithmetic, so (ash x (- (ash i 3))) on an (unsigned-byte 64) is a
;;; shift. Values must not change, and a variable of the same name bound
;;; inside the SYMBOL-MACROLET still shadows it.

(defmacro %smif-unrolled ((var n) &body body)
  `(progn ,@(loop for k below n collect `(symbol-macrolet ((,var ,k)) ,@body))))

(defun %smif-bytes (w out)
  (declare (type (unsigned-byte 64) w)
           (type (simple-array (unsigned-byte 8) (8)) out))
  (%smif-unrolled (i 8)
    (let ((r (- (ash i 3))))
      (setf (aref out i) (logand (ash w r) #xff))))
  out)

(defun %smif-shadow (x)
  (declare (type fixnum x))
  (symbol-macrolet ((k 5))
    (list (+ x k)
          (let ((k (* x 2)))
            (+ x k)))))

(defun %smif-index (v)
  (declare (type (simple-array (unsigned-byte 32) (4)) v))
  (let ((s 0))
    (%smif-unrolled (i 4)
      (setf s (logxor s (aref v i))))
    s))

(deftest symbol-macro-integer-fold.bytes
  (coerce (%smif-bytes #x0123456789ABCDEF (make-array 8 :element-type '(unsigned-byte 8))) 'list)
  (#xEF #xCD #xAB #x89 #x67 #x45 #x23 #x01))

(deftest symbol-macro-integer-fold.high-bit
  (coerce (%smif-bytes #xFF00000000000080 (make-array 8 :element-type '(unsigned-byte 8))) 'list)
  (#x80 0 0 0 0 0 0 #xFF))

(deftest symbol-macro-integer-fold.shadow
  (%smif-shadow 10)
  (15 30))

(deftest symbol-macro-integer-fold.index
  (%smif-index (make-array 4 :element-type '(unsigned-byte 32)
                             :initial-contents '(#xFFFFFFFF 1 2 #x80000000)))
  #x7FFFFFFC)
