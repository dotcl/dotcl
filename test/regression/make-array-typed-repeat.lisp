;;; (MAKE-ARRAY N :ELEMENT-TYPE 'SPEC) allocated again and again: each result is
;;; a fresh zero-filled array of the right element type and storage, whichever
;;; element types were allocated before it.

(defun %mar-u32 (n) (make-array n :element-type '(unsigned-byte 32)))
(defun %mar-u8 (n) (make-array n :element-type '(unsigned-byte 8)))
(defun %mar-s16 (n) (make-array n :element-type '(signed-byte 16)))
(defun %mar-f64 (n) (make-array n :element-type 'double-float))
(defun %mar-u64 (n) (make-array n :element-type '(unsigned-byte 64)))

(deftest make-array-typed-repeat.fresh
  (let ((a (%mar-u32 4)) (b (%mar-u32 4)))
    (setf (aref a 0) 7)
    (list (aref a 0) (aref b 0) (eq a b) (length b)))
  (7 0 nil 4))

(deftest make-array-typed-repeat.types
  (progn
    (dotimes (i 3) (%mar-u32 2) (%mar-u8 2) (%mar-s16 2) (%mar-f64 2) (%mar-u64 2))
    (list (array-element-type (%mar-u32 2)) (array-element-type (%mar-u8 2))
          (array-element-type (%mar-s16 2)) (array-element-type (%mar-f64 2))
          (array-element-type (%mar-u64 2))
          (aref (%mar-f64 2) 1) (aref (%mar-u64 2) 1) (aref (%mar-s16 3) 2)))
  ((unsigned-byte 32) (unsigned-byte 8) (signed-byte 16) double-float (unsigned-byte 64)
   0.0d0 0 0))

(deftest make-array-typed-repeat.range
  (progn
    (%mar-u8 1)
    (let ((a (%mar-u8 2)))
      (list (handler-case (progn (setf (aref a 0) 256) :stored) (error () :error))
            (handler-case (progn (setf (aref a 0) 255) (aref a 0)) (error () :error)))))
  (:error 255))

(deftest make-array-typed-repeat.zero-length
  (list (length (%mar-u32 0)) (length (%mar-u32 5)))
  (0 5))
