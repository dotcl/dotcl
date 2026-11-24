;;; An INLINE function that reads slots of a structure argument: its expansion binds the
;;; parameter to the caller's variable, and the caller's hoisted raw slot array serves the
;;; expansion's reads too.

(setf dotcl:*save-sil* t)

(defstruct (%sb-grid (:constructor %sb-make (rows cols cells)))
  (rows 0 :type fixnum) (cols 0 :type fixnum)
  (cells nil :type (simple-array (unsigned-byte 8) (*))))
(defstruct (%sb-other (:constructor %sb-make-other (rows cols))) (rows 0 :type fixnum) (cols 0 :type fixnum))

(declaim (inline %sb-at))
(defun %sb-at (g r c)
  (declare (type %sb-grid g) (type fixnum r c) (optimize (speed 3) (safety 1)))
  (aref (%sb-grid-cells g) (+ (* (mod r (%sb-grid-rows g)) (%sb-grid-cols g))
                              (mod c (%sb-grid-cols g)))))

(defun %sb-sum (g n)
  (declare (type %sb-grid g) (type fixnum n) (optimize (speed 3) (safety 1)))
  (let ((s 0)) (declare (type fixnum s))
    (dotimes (i n s)
      (incf s (+ (%sb-grid-cols g) (%sb-at g i 0) (%sb-at g 0 i) (%sb-at g (1+ i) (1- i)))))))

(defun %sb-sum-by-hand (g n)
  (let ((s 0) (rows (%sb-grid-rows g)) (cols (%sb-grid-cols g)) (cells (%sb-grid-cells g)))
    (dotimes (i n s)
      (flet ((at (r c) (aref cells (+ (* (mod r rows) cols) (mod c cols)))))
        (incf s (+ cols (at i 0) (at 0 i) (at (1+ i) (1- i))))))))

(defun %sb-grid (rows cols)
  (let ((cells (make-array (* rows cols) :element-type '(unsigned-byte 8))))
    (dotimes (i (* rows cols)) (setf (aref cells i) (mod (* i 13) 251)))
    (%sb-make rows cols cells)))

(deftest struct-backing-alias.values
  (let ((g (%sb-grid 6 9)))
    (list (= (%sb-sum g 20) (%sb-sum-by-hand g 20)) (= (%sb-sum g 50) (%sb-sum-by-hand g 50))
          (plusp (%sb-sum g 20))))
  (t t t))

(deftest struct-backing-alias.wrong-type-still-signals
  (let ((g (%sb-grid 6 9)))
    (list (handler-case (%sb-sum (%sb-make-other 6 9) 3) (error () :error))
          (handler-case (%sb-sum 5 3) (error () :error))
          (handler-case (%sb-sum nil 3) (error () :error))
          (= (%sb-sum g 3) (%sb-sum-by-hand g 3))))
  (:error :error :error t))

(deftest-compiled-only struct-backing-alias.one-fetch
  (let* ((text (%flat-text (dotcl:function-sil #'%sb-sum)))
         (count (lambda (needle)
                  (let ((k 0) (p 0))
                    (loop (let ((q (search needle text :start2 p)))
                            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))))
    (list (funcall count "StructRawInline")
          (>= (funcall count "RawInlineAt") 9)))
  (1 t))
