;;; A DEFMETHOD whose specializers are all class names expands to one call with
;;; the method's static description as data. Spelled out, each DEFMETHOD cost
;;; about 150 instructions of top level code, most of the top level code of a
;;; CLOS-heavy fasl such as asdf's, JITted at every load.

(deftest defmethod-data-expansion.one-call
  (let ((exp (macroexpand-1 '(defmethod dmde-f ((a string) b &key c) (list a b c)))))
    (list (length exp) (eq (car (second exp)) 'quote)))
  (7 t))

(defclass dmde-a () ())
(defclass dmde-b (dmde-a) ())
(defgeneric dmde-g (x &key k))
(defmethod dmde-g ((x dmde-a) &key (k 1)) (list :a k))
(defmethod dmde-g :around ((x dmde-b) &key k) (list :around k (call-next-method)))
(defmethod dmde-g ((x (eql 3)) &key k) (list :three k))
(defmethod dmde-h (x y) "the doc" (list x y))
(defmethod (setf dmde-slot) (value (x dmde-a)) (list :set value))

(deftest defmethod-data-expansion.behaviour
  (list (dmde-g (make-instance 'dmde-b) :k 5)
        (dmde-g 3)
        (dmde-h 1 2)
        (setf (dmde-slot (make-instance 'dmde-a)) 9)
        (documentation (first (dotcl-mop:generic-function-methods #'dmde-h)) t)
        (mapcar #'dotcl-mop:method-lambda-list
                (dotcl-mop:generic-function-methods #'dmde-h))
        (mapcar #'method-qualifiers (dotcl-mop:generic-function-methods #'dmde-g)))
  ((:around 5 (:a 5)) (:three nil) (1 2) (:set 9) "the doc" ((x y))
   (nil (:around) nil)))

;; DEFMETHOD returns the method object, and a method on a new name creates the
;; generic function with the method's lambda list shape.
(deftest defmethod-data-expansion.returns-method
  (let ((m (defmethod dmde-new (a &optional b &rest r) (list a b r))))
    (list (typep m 'method)
          (eq m (first (dotcl-mop:generic-function-methods #'dmde-new)))
          (dmde-new 1 2 3 4)))
  (t t (1 2 (3 4))))
