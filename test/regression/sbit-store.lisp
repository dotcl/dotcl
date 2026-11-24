;;; (SETF (SBIT A I) V) stores a 0 or a 1 in a simple bit vector in place, checking
;;; the array and the value in the same call. Neighbouring bits stay, the value of the
;;; form is V, and the errors are the ones SBIT's checks give.

(defun %sb-set (s i v) (setf (sbit s i) v))
(defun %sb-set-fix (s i) (declare (fixnum i)) (setf (sbit s i) 1))
(defun %sb-bits (s) (coerce s 'list))

(deftest sbit-store.neighbours-and-word-edges
  (let ((s (make-array 130 :element-type 'bit :initial-element 0)))
    (dolist (i '(0 1 62 63 64 65 127 128 129)) (%sb-set s i 1))
    (list (loop for i below 130 when (= 1 (sbit s i)) collect i)
          (progn (dolist (i '(63 64 129)) (%sb-set s i 0))
                 (loop for i below 130 when (= 1 (sbit s i)) collect i))))
  ((0 1 62 63 64 65 127 128 129) (0 1 62 65 127 128)))

(deftest sbit-store.value-of-the-form
  (let ((s (make-array 8 :element-type 'bit :initial-element 0)))
    (list (%sb-set s 3 1) (%sb-set s 3 0) (setf (sbit s 2) 1) (%sb-set-fix s 5)))
  (1 0 1 1))

(deftest sbit-store.errors
  (let ((s (make-array 8 :element-type 'bit :initial-element 0)))
    (list (handler-case (%sb-set s 8 1) (error () :error))
          (handler-case (%sb-set s -1 1) (error () :error))
          (handler-case (%sb-set s 1 2) (type-error () :type-error))
          (handler-case (%sb-set s 1 :a) (type-error () :type-error))
          (handler-case (%sb-set (make-array 8) 1 0) (type-error () :type-error))
          (handler-case (%sb-set "abcdefgh" 1 0) (type-error () :type-error))
          (handler-case (%sb-set (make-array 8 :element-type '(unsigned-byte 8)) 1 0) (type-error () :type-error))
          (handler-case (%sb-set nil 1 0) (type-error () :type-error))
          (%sb-bits s)))
  (:error :error :type-error :type-error :type-error :type-error :type-error :type-error (0 0 0 0 0 0 0 0)))

(deftest sbit-store.non-simple-bit-vectors-are-rejected
  (let* ((base (make-array 8 :element-type 'bit :initial-element 0))
         (adj (make-array 8 :element-type 'bit :adjustable t :initial-element 0))
         (disp (make-array 4 :element-type 'bit :displaced-to base))
         (fp (make-array 8 :element-type 'bit :fill-pointer 4 :initial-element 0)))
    (list (handler-case (%sb-set adj 1 1) (type-error () :type-error))
          (handler-case (%sb-set disp 1 1) (type-error () :type-error))
          (handler-case (%sb-set fp 1 1) (type-error () :type-error))
          (progn (setf (bit adj 1) 1 (bit disp 2) 1) (list (%sb-bits adj) (%sb-bits base)))))
  (:type-error :type-error :type-error ((0 1 0 0 0 0 0 0) (0 0 1 0 0 0 0 0))))

(deftest sbit-store.same-as-aref-store
  (let ((a (make-array 70 :element-type 'bit :initial-element 0))
        (b (make-array 70 :element-type 'bit :initial-element 0)))
    (loop for i in '(0 5 63 64 69 5 0) for v in '(1 1 1 1 1 0 0)
          do (%sb-set a i v) (setf (aref b i) v))
    (equal (%sb-bits a) (%sb-bits b)))
  t)

(deftest sbit-store.loop-in-a-sieve
  (let* ((n 100) (s (make-array (1+ n) :element-type 'bit :initial-element 1)) (c 0))
    (loop for i from 2 to n
          when (= 1 (sbit s i))
            do (incf c) (loop for j from (* i i) to n by i do (setf (sbit s j) 0)))
    c)
  25)
