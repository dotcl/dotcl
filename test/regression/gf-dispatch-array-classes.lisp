;;; Arrays dispatch on the same class CLASS-OF gives them, from a cold and a
;;; warm cache, whatever the order the kinds arrive in.

(defgeneric %gdac (x y))
(defmethod %gdac ((x array) y) (list :array y))
(defmethod %gdac ((x vector) y) (list :vector y))
(defmethod %gdac ((x string) y) (list :string y))
(defmethod %gdac ((x base-string) y) (list :base-string y))
(defmethod %gdac ((x simple-base-string) y) (list :simple-base-string y))
(defmethod %gdac ((x bit-vector) y) (list :bit-vector y))
(defmethod %gdac ((x simple-bit-vector) y) (list :simple-bit-vector y))
(defmethod %gdac (x y) (list :t y))

(defgeneric %gdac1 (x))
(defmethod %gdac1 ((x vector)) :vector)
(defmethod %gdac1 ((x array)) :array)
(defmethod %gdac1 ((x t)) :t)

(defun %gdac-objects ()
  (list (vector 1 2)
        (make-array 3 :element-type 'double-float :initial-element 0d0)
        (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)
        (make-array '(2 2))
        (make-array '(2 2) :element-type 'character :initial-element #\a)
        (make-array 3 :element-type 'bit :initial-element 0)
        (make-array 3 :element-type 'bit :initial-element 0 :adjustable t)
        (make-array 3 :element-type 'character :initial-element #\a :fill-pointer 1)
        (make-array 3 :element-type 'base-char :initial-element #\a)
        (make-array 3 :element-type 'base-char :initial-element #\a :adjustable t)
        (make-array 2 :displaced-to (vector 1 2 3))
        "abc"
        1))

(deftest gf-dispatch-array-classes.kinds
  (let ((objs (%gdac-objects)))
    (loop repeat 3
          collect (list (mapcar (lambda (o) (car (%gdac o 0))) objs)
                        (mapcar #'%gdac1 objs)
                        (mapcar (lambda (o) (car (%gdac o 0))) (reverse objs)))))
  (#1=((:vector :vector :vector :array :array :simple-bit-vector :bit-vector :string
        :simple-base-string :base-string :vector :string :t)
       (:vector :vector :vector :array :array :vector :vector :vector
        :vector :vector :vector :vector :t)
       (:t :string :vector :base-string :simple-base-string :string :bit-vector
        :simple-bit-vector :array :array :vector :vector :vector))
   #1# #1#))

(deftest gf-dispatch-array-classes.agrees-with-class-of
  (loop repeat 2
        collect (loop for o in (%gdac-objects)
                      for name = (class-name (class-of o))
                      always (eq (car (%gdac o 0))
                                 (if (member name '(array vector string base-string simple-base-string
                                                    bit-vector simple-bit-vector))
                                     (intern (symbol-name name) :keyword)
                                     :t))))
  (t t))
