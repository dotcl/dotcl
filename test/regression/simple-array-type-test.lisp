;;; (SIMPLE-ARRAY et (n)) and (SIMPLE-ARRAY et (*)), directly or through a
;;; DEFTYPE, as a declared argument type and in TYPEP: the answer for a simple
;;; vector is remembered per element type, and must still follow the length,
;;; simpleness, and a redefined DEFTYPE.

(deftype sat-p-array () '(simple-array (unsigned-byte 32) (18)))
(deftype sat-octets (&optional (n '*)) `(simple-array (unsigned-byte 8) (,n)))
(deftype sat-elt () '(unsigned-byte 8))

(defun %sat-arg (a)
  (declare (type sat-p-array a))
  (length a))

(defun %sat-arg-error-p (x)
  (handler-case (progn (%sat-arg x) nil) (type-error () t)))

(deftest simple-array-type-test.argument
  (list (%sat-arg (make-array 18 :element-type '(unsigned-byte 32)))
        (%sat-arg-error-p (make-array 17 :element-type '(unsigned-byte 32)))
        (%sat-arg-error-p (make-array 18 :element-type '(unsigned-byte 8)))
        (%sat-arg-error-p (make-array 18 :element-type '(unsigned-byte 32) :adjustable t))
        (%sat-arg-error-p (make-array 18 :element-type '(unsigned-byte 32) :fill-pointer 18))
        (%sat-arg-error-p (make-array 18))
        (%sat-arg-error-p (make-array '(2 9) :element-type '(unsigned-byte 32)))
        (%sat-arg-error-p "abc")
        (%sat-arg (make-array 18 :element-type '(unsigned-byte 32))))
  (18 t t t t t t t 18))

(deftest simple-array-type-test.typep
  (let ((u8 (make-array 4 :element-type '(unsigned-byte 8)))
        (u16 (make-array 4 :element-type '(unsigned-byte 16)))
        (gen (make-array 4)))
    (list (typep u8 'sat-octets) (typep u16 'sat-octets) (typep u8 'sat-octets)
          (typep gen 'sat-octets) (typep u8 '(sat-octets 4)) (typep u8 '(sat-octets 5))
          (typep u8 '(simple-array sat-elt (*))) (typep u16 '(simple-array sat-elt (*)))
          (typep "ab" '(simple-array character (*)))
          (typep (make-array 2 :element-type 'character :adjustable t)
                 '(simple-array character (*)))))
  (t nil t nil t nil t nil t nil))

(deftest simple-array-type-test.deftype-redefined
  (let ((u8 (make-array 4 :element-type '(unsigned-byte 8)))
        (spec '(simple-array sat-elt2 (*))))
    (eval '(deftype sat-elt2 () '(unsigned-byte 8)))
    (let ((before (typep u8 spec)))
      (eval '(deftype sat-elt2 () '(unsigned-byte 16)))
      (list before (typep u8 spec))))
  (t nil))
