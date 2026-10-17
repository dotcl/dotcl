;;; (MAKE-ARRAY n :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE a)) builds the array
;;; straight from A's storage.

(setf dotcl:*save-sil* t)

(defun %mal-like (n a) (make-array n :element-type (array-element-type a)))
(defun %mal-like-init (n a x)
  (make-array n :element-type (array-element-type a) :initial-element x))

(deftest make-array-like.types
  (mapcar (lambda (a)
            (let ((v (%mal-like 3 a)))
              (if (subtypep (array-element-type v) 'number)
                  (list (array-element-type v) (coerce v 'list))
                  (list (array-element-type v) (length v)))))
          (list (make-array '(2 2) :element-type '(unsigned-byte 16))
                (make-array 1 :element-type 'single-float)
                (make-array 1 :element-type 'bit)
                (make-array 1 :element-type '(signed-byte 8))
                (make-array 1)
                (make-array 1 :element-type 'character)
                "abc"))
  (((unsigned-byte 16) (0 0 0))
   (single-float (0.0 0.0 0.0))
   (bit (0 0 0))
   ((signed-byte 8) (0 0 0))
   (t 3)
   (character 3)
   (character 3)))

(deftest make-array-like.init
  (list (coerce (%mal-like-init 2 (make-array 1 :element-type 'double-float) 1.5d0) 'list)
        (coerce (%mal-like-init 2 (make-array 1) :x) 'list))
  ((1.5d0 1.5d0) (:x :x)))

(deftest make-array-like.not-an-array
  (handler-case (%mal-like 2 :foo)
    (type-error () :type-error))
  :type-error)

(deftest make-array-like.dimensions
  (array-dimensions (%mal-like '(2 3) (make-array 1 :element-type '(unsigned-byte 8))))
  (2 3))

(deftest make-array-like.order
  (let ((log '()))
    (make-array (progn (push 1 log) 2)
                :element-type (array-element-type (progn (push 2 log) #*0))
                :initial-element (progn (push 3 log) 1))
    (reverse log))
  (1 2 3))

(deftest-compiled-only make-array-like.direct
  (search "GetFunctionBySymbol" (%flat-text (dotcl:function-sil #'%mal-like)))
  nil)

(setf dotcl:*save-sil* nil)
