;;; A counting loop (DOTIMES, DO, LOOP FOR ... BELOW) whose variable subscripts
;;; hoisted simple-array buffers compares its limit with their lengths once,
;;; before it starts, and then reads and writes them without a bounds check per
;;; element. When some buffer is shorter than the limit, the loop runs as before,
;;; so the same element is written last and the same error is signalled.

(setf dotcl:*save-sil* t)

(defun %bla-has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %bla-dv (n &optional (start 0d0))
  (let ((v (make-array n :element-type 'double-float)))
    (dotimes (i n v) (setf (aref v i) (+ start i)))))

(defun %bla-add (a b c)
  (declare (type (simple-array double-float (*)) a b c))
  (dotimes (i (length c) c)
    (setf (aref c i) (+ (aref a i) (aref b i)))))

(defun %bla-add-s0 (a b c)
  (declare (type (simple-array double-float (*)) a b c)
           (optimize (speed 3) (safety 0)))
  (dotimes (i (length c) c)
    (setf (aref c i) (+ (aref a i) (aref b i)))))

(defun %bla-add-n (a b c n)
  (declare (type (simple-array double-float (*)) a b c) (fixnum n))
  (dotimes (i n c)
    (setf (aref c i) (+ (aref a i) (aref b i)))))

(defun %bla-add-min (a b c)
  (declare (type (simple-array double-float (*)) a b c))
  (dotimes (i (min (length a) (length b) (length c)) c)
    (setf (aref c i) (+ (aref a i) (aref b i)))))

(defun %bla-loop (v n)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum n))
  (loop for i below n do (setf (aref v i) (logand (+ (aref v i) i) 255)))
  v)

(defun %bla-loop-to (v n)
  (declare (type (simple-array fixnum (*)) v) (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (loop for i from 0 to n do (incf s (aref v i)))
    s))

(defun %bla-sfloat (v)
  (declare (type (simple-array single-float (*)) v))
  (dotimes (i (length v) v)
    (setf (aref v i) (* 2.0 (aref v i)))))

(defun %bla-result-reads-limit (v)
  (declare (type (simple-array double-float (*)) v))
  (dotimes (i (length v) (aref v i))
    (setf (aref v i) 1d0)))

(defun %bla-assigns-var (v)
  (declare (type (simple-array double-float (*)) v))
  (dotimes (i (length v) v)
    (setf (aref v i) 1d0)
    (setq i (+ i 2))))

(defun %bla-nested (a c)
  (declare (type (simple-array double-float (*)) a c))
  (dotimes (j (length a) c)
    (dotimes (i (length c))
      (setf (aref c i) (+ (aref c i) (aref a j))))))

(deftest-compiled-only bounded-loop-aref.shape
  (list (%bla-has "Runtime.AtF64" #'%bla-add)
        (%bla-has "Runtime.SetAtF64" #'%bla-add)
        (%bla-has "Runtime.AtF64" #'%bla-add-s0)
        (%bla-has "Runtime.AtF64" #'%bla-add-n)
        (%bla-has "Runtime.AtF64" #'%bla-add-min)
        (%bla-has "Runtime.SetAtU8" #'%bla-loop)
        (%bla-has "Runtime.AtI64" #'%bla-loop-to)
        (%bla-has "Runtime.SetAtF32" #'%bla-sfloat)
        (%bla-has "Runtime.SetAtF64" #'%bla-assigns-var))
  (t t t t t t t t nil))

(deftest bounded-loop-aref.values
  (list (coerce (%bla-add (%bla-dv 4) (%bla-dv 4 10d0) (%bla-dv 4)) 'list)
        (coerce (%bla-add-s0 (%bla-dv 3) (%bla-dv 3 1d0) (%bla-dv 3)) 'list)
        (coerce (%bla-add (%bla-dv 5) (%bla-dv 6) (%bla-dv 2)) 'list)
        (coerce (%bla-add-min (%bla-dv 5) (%bla-dv 2) (%bla-dv 4 7d0)) 'list)
        (coerce (%bla-add (%bla-dv 0) (%bla-dv 0) (%bla-dv 0)) 'list)
        (coerce (%bla-loop (make-array 4 :element-type '(unsigned-byte 8)
                                         :initial-contents '(1 2 254 255))
                           4)
                'list)
        (%bla-loop-to (make-array 3 :element-type 'fixnum :initial-contents '(1 2 3)) 2)
        (coerce (%bla-sfloat (make-array 2 :element-type 'single-float
                                           :initial-contents '(1.5 -2.0)))
                'list)
        (coerce (%bla-nested (%bla-dv 3 1d0) (%bla-dv 2)) 'list))
  ((10d0 12d0 14d0 16d0) (1d0 3d0 5d0) (0d0 2d0) (0d0 2d0 9d0 10d0) ()
   (1 3 0 2) 6 (3.0 -4.0) (6d0 7d0)))

;; A buffer shorter than the limit: the loop writes the same prefix as before
;; and then signals.
(deftest bounded-loop-aref.short-buffer-signals
  (let ((c (%bla-dv 4 100d0)))
    (list (handler-case (progn (%bla-add (%bla-dv 2) (%bla-dv 4) c) :no-error)
            (error () :error))
          (coerce c 'list)))
  (:error (0d0 2d0 102d0 103d0)))

(deftest bounded-loop-aref.short-buffer-signals-at-safety-0
  (let ((c (%bla-dv 3 100d0)))
    (list (handler-case (progn (%bla-add-s0 (%bla-dv 3) (%bla-dv 1) c) :no-error)
            (error () :error))
          (coerce c 'list)))
  (:error (0d0 101d0 102d0)))

(deftest bounded-loop-aref.limit-beyond-length-signals
  (let ((c (%bla-dv 3 100d0)))
    (list (handler-case (progn (%bla-add-n (%bla-dv 3) (%bla-dv 3) c 4) :no-error)
            (error () :error))
          (coerce c 'list)
          (handler-case (progn (%bla-loop (make-array 2 :element-type '(unsigned-byte 8)) 3)
                               :no-error)
            (error () :error))
          (handler-case (%bla-loop-to (make-array 3 :element-type 'fixnum
                                                    :initial-contents '(1 2 3))
                                      3)
            (error () :error))))
  (:error (0d0 2d0 4d0) :error :error))

(deftest bounded-loop-aref.result-form-past-the-end-signals
  (handler-case (progn (%bla-result-reads-limit (%bla-dv 2)) :no-error)
    (error () :error))
  :error)

(deftest bounded-loop-aref.assigned-variable-signals
  (let ((v (%bla-dv 5)))
    (list (coerce (%bla-assigns-var v) 'list)
          (handler-case (progn (%bla-assigns-var (%bla-dv 2)) :no-error)
            (error () :error))))
  ((1d0 1d0 2d0 1d0 4d0) :no-error))

(defun %bla-macro-index (v)
  (declare (type (simple-array double-float (*)) v))
  (dotimes (i (length v) v)
    (symbol-macrolet ((i 100000000))
      (setf (aref v i) 1d0))))

(deftest bounded-loop-aref.symbol-macro-index-signals
  (handler-case (progn (%bla-macro-index (%bla-dv 3)) :no-error)
    (error () :error))
  :error)

;; A DO step wrapped in THE counts too.
(defun %bla-do-the (x y z)
  (declare (type (simple-array double-float (*)) x y z)
           (optimize (speed 3) (safety 0)))
  (let ((n (length z)))
    (declare (fixnum n))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) z)
      (declare (fixnum i))
      (setf (aref z i) (the double-float (+ (aref x i) (aref y i)))))))

(deftest-compiled-only bounded-loop-aref.do-the-shape
  (%bla-has "Runtime.SetAtF64" #'%bla-do-the)
  t)

(deftest bounded-loop-aref.do-the-values
  (list (coerce (%bla-do-the (%bla-dv 3) (%bla-dv 3 1d0) (%bla-dv 3)) 'list)
        (let ((z (%bla-dv 3 100d0)))
          (list (handler-case (progn (%bla-do-the (%bla-dv 3) (%bla-dv 2) z) :no-error)
                  (error () :error))
                (coerce z 'list))))
  ((1d0 3d0 5d0) (:error (0d0 2d0 102d0))))
