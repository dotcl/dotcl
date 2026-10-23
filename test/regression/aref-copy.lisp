;;; (SETF (AREF DST ...) (AREF SRC ...)) as a statement, neither array's element
;;; type known at compile time, moves the element without boxing it when both
;;; arrays store it the same way, and otherwise stores what AREF reads.

(setf dotcl:*save-sil* t)

(defun %ac-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %ac-11 (d i s j)
  (declare (fixnum i j))
  (setf (aref d i) (aref s j))
  d)

(defun %ac-12 (d i s j k)
  (declare (fixnum i j k))
  (setf (aref d i) (aref s j k))
  d)

(defun %ac-21 (d i j s k)
  (declare (fixnum i j k))
  (setf (aref d i j) (aref s k))
  d)

(defun %ac-22 (d i j s k l)
  (declare (fixnum i j k l))
  (setf (aref d i j) (aref s k l))
  d)

(defun %ac-value (d i s j)
  (declare (fixnum i j))
  (setf (aref d i) (aref s j)))

(defun %ac-11-old (d i s j)
  (declare (fixnum i j))
  (values (setf (aref d i) (aref s j)) d))

(defun %ac-12-old (d i s j k)
  (declare (fixnum i j k))
  (values (setf (aref d i) (aref s j k)) d))

(defun %ac-loop (d s)
  (dotimes (i (length d) d)
    (setf (aref d i) (aref s (- (length s) i 1)))))

(defun %ac-try (thunk)
  (handler-case (funcall thunk)
    (type-error () :type-error)
    (error () :error)))

(deftest-compiled-only aref-copy.emitted
  (list (%ac-sil-count #'%ac-11 "ArefCopy11L") (%ac-sil-count #'%ac-12 "ArefCopy12L")
        (%ac-sil-count #'%ac-21 "ArefCopy21L") (%ac-sil-count #'%ac-22 "ArefCopy22L")
        (%ac-sil-count #'%ac-loop "ArefCopy11L")
        (%ac-sil-count #'%ac-value "ArefCopy"))
  (1 1 1 1 1 0))

(deftest aref-copy.same-storage
  (loop for (et a b) in `((t foo "bar")
                          ((unsigned-byte 8) 7 255)
                          ((unsigned-byte 32) #xff123456 #xfffffffe)
                          ((signed-byte 8) -128 127)
                          ((unsigned-byte 4) 3 15)
                          (fixnum ,most-negative-fixnum ,most-positive-fixnum)
                          ((unsigned-byte 64) ,(1- (expt 2 64)) ,(expt 2 63))
                          (single-float -0.0f0 1.5f0)
                          (double-float 2.5d0 -1d300)
                          (character #\a ,(code-char 945)))
        collect (let ((s1 (make-array 2 :element-type et :initial-contents (list a b)))
                      (s2 (make-array '(2 2) :element-type et
                                             :initial-contents (list (list a b) (list b a))))
                      (d1 (make-array 3 :element-type et :initial-element a))
                      (d2 (make-array '(2 3) :element-type et :initial-element a)))
                  (list (eql (aref (%ac-11 d1 2 s1 1) 2) b)
                        (eql (aref (%ac-12 d1 0 s2 0 1) 0) b)
                        (eql (aref (%ac-21 d2 1 2 s1 1) 1 2) b)
                        (eql (aref (%ac-22 d2 0 1 s2 1 0) 0 1) b)
                        (eql (aref (%ac-22 d2 0 1 s2 1 1) 0 1) a))))
  ((t t t t t) (t t t t t) (t t t t t) (t t t t t) (t t t t t)
   (t t t t t) (t t t t t) (t t t t t) (t t t t t) (t t t t t)))

(deftest aref-copy.value
  (let ((d (make-array 2 :element-type '(unsigned-byte 32)))
        (s (make-array 2 :element-type '(unsigned-byte 32) :initial-element #xff000001)))
    (list (%ac-value d 1 s 0) (aref d 1)))
  (#xff000001 #xff000001))

(deftest aref-copy.loop
  (coerce (%ac-loop (make-array 4) (vector 1 2 3 4)) 'list)
  (4 3 2 1))

(defun %ac-mixed (copy)
  (let ((u32 (make-array 2 :element-type '(unsigned-byte 32) :initial-contents '(300 7)))
        (u8 (make-array 2 :element-type '(unsigned-byte 8)))
        (u4 (make-array 2 :element-type '(unsigned-byte 4)))
        (gen (make-array 2 :initial-contents '(9 x)))
        (sf (make-array 1 :element-type 'single-float :initial-element 1.0f0))
        (df (make-array 1 :element-type 'double-float))
        (str (make-string 2 :initial-element #\z))
        (bits (make-array 2 :element-type 'bit :initial-contents '(1 0)))
        (u8big (make-array 1 :element-type '(unsigned-byte 8) :initial-element 200))
        (s32 (make-array 1 :element-type '(signed-byte 32) :initial-element -1000))
        (s8 (make-array 1 :element-type '(signed-byte 8))))
    (flet ((try (d i s j) (%ac-try (lambda () (aref (funcall copy d i s j) i)))))
      (list (try u8 0 u32 1) (try u8 0 u32 0) (try u4 0 u32 1) (try u4 1 u8 0)
            (try gen 1 u32 0) (try u32 0 gen 0) (try u32 0 gen 1)
            (try gen 0 sf 0) (try df 0 sf 0) (try str 1 gen 1) (try gen 1 str 0)
            (try u8 1 bits 0) (try bits 1 u8 1) (try bits 0 gen 0)
            (try u4 0 u8big 0) (try s8 0 s32 0) (try s32 0 s8 0)))))

(deftest aref-copy.mixed-storage
  (let ((new (%ac-mixed #'%ac-11))
        (old (%ac-mixed (lambda (d i s j) (nth-value 1 (%ac-11-old d i s j))))))
    (list (equal new old) new))
  (t (7 :type-error 7 7 300 9 300 1.0f0 :type-error
      :type-error #\z 1 1 :type-error :type-error :type-error 0)))

(deftest aref-copy.not-simple
  (let* ((base (make-array 6 :element-type '(unsigned-byte 32)
                             :initial-contents '(10 11 12 13 14 15)))
         (disp (make-array 3 :element-type '(unsigned-byte 32)
                             :displaced-to base :displaced-index-offset 2))
         (adj (make-array 3 :element-type '(unsigned-byte 32) :adjustable t
                            :fill-pointer 1 :initial-element 0)))
    (%ac-11 adj 2 disp 1)
    (%ac-11 disp 0 adj 2)
    (list (aref adj 2) (aref base 2)))
  (13 13))

(defun %ac-bounds (c11 c12)
  (let ((s (make-array 2 :element-type '(unsigned-byte 32)))
        (d (make-array 2 :element-type '(unsigned-byte 32)))
        (s2 (make-array '(2 2))))
    (list (%ac-try (lambda () (funcall c11 d 0 s 2)))
          (%ac-try (lambda () (funcall c11 d 2 s 0)))
          (%ac-try (lambda () (funcall c11 d -1 s 0)))
          (%ac-try (lambda () (funcall c12 d 0 s2 0 2)))
          (%ac-try (lambda () (funcall c12 d 0 s 0 0)))
          (%ac-try (lambda () (funcall c11 d 0 s2 0)))
          (%ac-try (lambda () (funcall c11 d 0 '(1 2) 0)))
          (%ac-try (lambda () (funcall c11 '(1 2) 0 s 0))))))

(deftest aref-copy.bounds
  (let ((new (%ac-bounds #'%ac-11 #'%ac-12))
        (old (%ac-bounds (lambda (&rest a) (nth-value 1 (apply #'%ac-11-old a)))
                         (lambda (&rest a) (nth-value 1 (apply #'%ac-12-old a))))))
    (list (equal new old) (notany #'arrayp new)))
  (t t))

(defun %ac-order (d s)
  (let ((log '()))
    (flet ((note (tag v) (push tag log) v))
      (setf (aref (note :d d) (the fixnum (note :i 1)))
            (aref (note :s s) (the fixnum (note :j 2))))
      (reverse log))))

(deftest-compiled-only aref-copy.order-emitted
  (%ac-sil-count #'%ac-order "ArefCopy11L")
  1)

(deftest aref-copy.order
  (let ((d (make-array 3 :initial-element 0)))
    (list (%ac-order d (vector 'a 'b 'c)) (aref d 1)))
  ((:d :i :s :j) c))
