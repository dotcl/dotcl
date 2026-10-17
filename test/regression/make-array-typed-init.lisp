;;; (MAKE-ARRAY N :ELEMENT-TYPE 'SPEC :INITIAL-ELEMENT X) with a constant SPEC
;;; compiles to a direct constructor call, in either keyword order.

(setf dotcl:*save-sil* t)

(defun %mati-has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %mati-u8 (n) (make-array n :element-type '(unsigned-byte 8) :initial-element #x36))
(defun %mati-rev (n x) (make-array n :initial-element x :element-type '(unsigned-byte 32)))
(defun %mati-any (n type x)
  (ecase type
    (s16 (make-array n :element-type '(signed-byte 16) :initial-element x))
    (dbl (make-array n :element-type 'double-float :initial-element x))
    (sgl (make-array n :element-type 'single-float :initial-element x))
    (chr (make-array n :element-type 'character :initial-element x))
    (bit (make-array n :element-type 'bit :initial-element x))
    (fix (make-array n :element-type 'fixnum :initial-element x))))

(defun %mati-desc (a)
  (list (array-element-type a) (array-dimensions a)
        (loop for i below (array-total-size a) collect (row-major-aref a i))))

(deftest make-array-typed-init.values
  (list (%mati-desc (%mati-u8 3))
        (%mati-desc (%mati-rev 2 #xffffffff))
        (%mati-desc (%mati-any 2 's16 -5))
        (%mati-desc (%mati-any 2 'dbl 1.5d0))
        (%mati-desc (%mati-any 2 'sgl 2.5f0))
        (%mati-desc (%mati-any 2 'chr #\a))
        (%mati-desc (%mati-any 3 'bit 1))
        (coerce (%mati-any 2 'fix -12345) 'list)
        (%mati-desc (%mati-u8 0))
        (%mati-desc (make-array '(2 2) :element-type '(unsigned-byte 8) :initial-element 7)))
  (((unsigned-byte 8) (3) (54 54 54))
   ((unsigned-byte 32) (2) (4294967295 4294967295))
   ((signed-byte 16) (2) (-5 -5))
   (double-float (2) (1.5d0 1.5d0))
   (single-float (2) (2.5f0 2.5f0))
   (character (2) (#\a #\a))
   (bit (3) (1 1 1))
   (-12345 -12345)
   ((unsigned-byte 8) (0) ())
   ((unsigned-byte 8) (2 2) (7 7 7 7))))

(deftest make-array-typed-init.wrong-element
  (list (handler-case (%mati-any 2 'dbl 1) (type-error () :type-error))
        (handler-case (%mati-any 2 'chr 1) (type-error () :type-error))
        (handler-case (%mati-any 2 'bit 2) (type-error () :type-error))
        (handler-case (%mati-u8 -1) (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error))

(deftest-emitting-only make-array-typed-init.direct
  (list (%mati-has "MakeArrayOfTypeInit" #'%mati-u8) (%mati-has "MakeArrayOfTypeInit" #'%mati-rev))
  (t t))
