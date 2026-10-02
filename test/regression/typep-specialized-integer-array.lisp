;;; An array with specialized integer storage has exactly one element type:
;;; (array X) names it only when X upgrades to that type.
;;;
;;; Bug: a FIXNUM vector satisfied (simple-array (unsigned-byte 8) (*)) and
;;; (simple-array (signed-byte 32) (*)), and byte vectors satisfied
;;; (simple-array fixnum (*)). cl-store's TYPECASE sent a FIXNUM vector to its
;;; byte-vector writer, which then refused it.

(defun %tsia-kind (a)
  (typecase a
    ((simple-array (unsigned-byte 8) (*)) :u8)
    ((simple-array (signed-byte 32) (*)) :s32)
    ((simple-array fixnum (*)) :fixnum)
    (t :other)))

(deftest typep-specialized-integer-array.typecase
  (mapcar (lambda (et) (%tsia-kind (make-array 3 :element-type et)))
          '(fixnum (unsigned-byte 8) (signed-byte 32) (unsigned-byte 16) (signed-byte 64) t))
  (:fixnum :u8 :s32 :other :other :other))

(deftest typep-specialized-integer-array.typep
  (let ((fx (make-array 3 :element-type 'fixnum))
        (u8 (make-array 3 :element-type '(unsigned-byte 8))))
    (list (typep fx '(simple-array (unsigned-byte 8) (*)))
          (typep fx '(array (signed-byte 32)))
          (typep fx '(simple-array fixnum (*)))
          (typep u8 '(simple-array fixnum (*)))
          (typep u8 '(simple-array (unsigned-byte 8) (*)))
          ;; (integer 0 200) upgrades to (unsigned-byte 8) here
          (typep u8 (list 'array (list 'integer 0 200)))))
  (nil nil t nil t t))
