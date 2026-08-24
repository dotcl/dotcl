;;; MAKE-ARRAY built a boxed LispObject[SIZE] for every array and let the
;;; constructor pack it into the narrow storage the element type asks for, then
;;; drop it. The boxed detour cost 8 bytes an element in garbage -- four times the
;;; array itself for (integer 0 1000), and 64 MB on the way to a 16 MB array in
;;; the 3d-arrays benchmark. Packed element types now fill their storage directly.
;;;
;;; These tests pin what that path has to keep producing: the same element type,
;;; the same contents, and the same errors.

(deftest packed-array-alloc.upgraded-element-types
  (list (array-element-type (make-array 4 :element-type '(integer 0 1000)))
        (array-element-type (make-array 4 :element-type '(integer 0 1)))
        (array-element-type (make-array 4 :element-type '(unsigned-byte 8)))
        (array-element-type (make-array 4 :element-type 'fixnum))
        (array-element-type (make-array 4 :element-type 'single-float))
        (array-element-type (make-array 4 :element-type 'double-float))
        (array-element-type (make-array 4 :element-type 'character)))
  ((unsigned-byte 16) bit (unsigned-byte 8) fixnum single-float double-float character))

(deftest packed-array-alloc.default-contents
  (list (coerce (make-array 3 :element-type '(integer 0 1000)) 'list)
        (coerce (make-array 3 :element-type 'bit) 'list)
        (coerce (make-array 3 :element-type 'double-float) 'list)
        (coerce (make-array 3 :element-type 'character) 'list))
  ((0 0 0) (0 0 0) (0.0d0 0.0d0 0.0d0) (#\Nul #\Nul #\Nul)))

(deftest packed-array-alloc.initial-element
  (list (coerce (make-array 3 :element-type '(integer 0 1000) :initial-element 700) 'list)
        (coerce (make-array 3 :element-type 'bit :initial-element 1) 'list)
        (coerce (make-array 3 :element-type 'single-float :initial-element 2.5) 'list)
        (coerce (make-array 3 :element-type 'character :initial-element #\x) 'list))
  ((700 700 700) (1 1 1) (2.5 2.5 2.5) (#\x #\x #\x)))

;;; Multi-dimensional arrays take the same path (this is where the benchmark's
;;; 200x200x200 array lives).
(deftest packed-array-alloc.multi-dimensional
  (let ((a (make-array '(2 3 4) :element-type '(integer 0 1000) :initial-element 5)))
    (setf (aref a 1 2 3) 999)
    (list (array-dimensions a)
          (array-element-type a)
          (aref a 0 0 0)
          (aref a 1 2 3)
          (array-total-size a)))
  ((2 3 4) (unsigned-byte 16) 5 999 24))

(deftest packed-array-alloc.two-dimensional-write-read
  (let ((a (make-array '(3 3) :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i 3) (setf (aref a i i) (float (1+ i) 1.0d0)))
    (list (aref a 0 0) (aref a 1 1) (aref a 2 2) (aref a 0 1)))
  (1.0d0 2.0d0 3.0d0 0.0d0))

;;; A value the element type cannot hold is rejected.
(deftest packed-array-alloc.out-of-range-initial-element-signals
  (list (handler-case (progn (make-array 3 :element-type (quote (unsigned-byte 8)) :initial-element 300) :no-error)
          (error () :error))
        (handler-case (progn (make-array 3 :element-type (quote bit) :initial-element 5) :no-error)
          (type-error () :type-error)
          (error () :other)))
  (:error :type-error))

;;; :initial-contents and adjustable arrays keep working (they take other paths,
;;; and must still agree with the direct one).
(deftest packed-array-alloc.initial-contents-and-adjust
  (let* ((a (make-array 3 :element-type '(integer 0 1000) :initial-contents '(1 2 3)))
         (b (make-array 2 :element-type '(integer 0 1000) :initial-element 7 :adjustable t))
         (c (adjust-array b 4 :initial-element 9)))
    (list (coerce a 'list) (coerce c 'list) (array-element-type c)))
  ((1 2 3) (7 7 9 9) (unsigned-byte 16)))

;;; A fill pointer on a packed vector still tracks pushes.
(deftest packed-array-alloc.fill-pointer
  (let ((v (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (vector-push 1 v)
    (vector-push 2 v)
    (list (length v) (coerce v 'list) (aref v 0)))
  (2 (1 2) 1))
