;;; A class metaobject's INITIALIZE-INSTANCE receives :DIRECT-SLOTS (canonical
;;; slot plists) and, from DEFCLASS, :DIRECT-DEFAULT-INITARGS (AMOP). A metaclass
;;; method that rewrites either one and calls the next method with the new value
;;; gets a class built from what it passed. clsql's view-class metaclass looks up
;;; the :DIRECT-SLOTS value by position in its &rest list, so it used to fail with
;;; (1+ NIL). Expected values were checked against SBCL.

(defclass mdsl-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mdsl-meta) (b standard-class)) t)

(defvar *mdsl-seen* nil)
(defmethod initialize-instance :around ((c mdsl-meta) &rest initargs)
  (setf *mdsl-seen* (copy-list initargs))
  (call-next-method))

(deftest metaclass-direct-slots.shape
  (progn
    (defclass mdsl-c1 ()
      ((a :initarg :a :initform (+ 1 2) :accessor mdsl-c1-a :type integer
          :documentation "doc")
       (b :reader mdsl-b1 :writer mdsl-w1 :allocation :class :initarg :b))
      (:metaclass mdsl-meta)
      (:default-initargs :a 10))
    (let* ((keys *mdsl-seen*)
           ;; what clsql does with its &rest list
           (slots (nth (1+ (position :direct-slots keys)) keys))
           (a (first slots))
           (b (second slots)))
      (list (length slots)
            (list (getf a :name) (getf a :initform) (functionp (getf a :initfunction))
                  (funcall (getf a :initfunction)) (getf a :initargs) (getf a :readers)
                  (getf a :writers) (getf a :type) (getf a :documentation))
            (list (getf b :name) (getf b :initargs) (getf b :readers) (getf b :writers)
                  (getf b :allocation) (getf b :initfunction 'none))
            (mapcar (lambda (d) (list (first d) (second d) (funcall (third d))))
                    (getf keys :direct-default-initargs)))))
  (2
   (a (+ 1 2) t 3 (:a) (mdsl-c1-a) ((setf mdsl-c1-a)) integer "doc")
   (b (:b) (mdsl-b1) (mdsl-w1) :class none)
   ((:a 10 10))))

(deftest metaclass-direct-slots.no-default-initargs
  (progn
    (defclass mdsl-c0 () (x) (:metaclass mdsl-meta))
    (list (let ((x (first (getf *mdsl-seen* :direct-slots))))
            (list (length (getf *mdsl-seen* :direct-slots))
                  (getf x :name) (getf x :initargs) (getf x :readers) (getf x :writers)
                  (getf x :initfunction (quote none))))
          (multiple-value-list (getf *mdsl-seen* :direct-default-initargs 'none))))
  ((1 x nil nil nil none) (nil)))

;;; A metaclass that adds a slot, drops one and replaces a default initarg.
(defclass mdsl-meta2 (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mdsl-meta2) (b standard-class)) t)
(defmethod initialize-instance :around ((c mdsl-meta2) &rest initargs
                                        &key direct-slots direct-default-initargs)
  (apply #'call-next-method c
         :direct-slots (cons (list :name 'extra :initargs '(:extra)
                                   :initform 42 :initfunction (lambda () 42))
                             (remove 'dropme direct-slots
                                     :key (lambda (s) (getf s :name))))
         :direct-default-initargs (cons (list :a ''from-meta (lambda () 'from-meta))
                                        (remove :a direct-default-initargs :key #'first))
         initargs))

(deftest metaclass-direct-slots.rewrite
  (progn
    (defclass mdsl-c2 ()
      ((a :initarg :a :accessor mdsl-c2-a) (dropme :initarg :dropme)
       (b :initarg :b :initform 7))
      (:metaclass mdsl-meta2)
      (:default-initargs :a 1 :b 2))
    (let ((c (find-class 'mdsl-c2)))
      (dotcl-mop:finalize-inheritance c)
      (list (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-direct-slots c))
            (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots c))
            (mapcar (lambda (d) (list (first d) (second d)))
                    (dotcl-mop:class-direct-default-initargs c))
            (let ((o (make-instance 'mdsl-c2)))
              (list (mdsl-c2-a o) (slot-value o 'b) (slot-value o 'extra)))
            (let ((o (make-instance 'mdsl-c2 :extra 1 :a 3)))
              (list (mdsl-c2-a o) (slot-value o 'extra))))))
  ((extra a b) (extra a b) ((:a 'from-meta) (:b 2)) (from-meta 2 42) (3 1)))

(deftest metaclass-direct-slots.make-instance
  (let ((c (make-instance 'mdsl-meta2 :name 'mdsl-c3
                          :direct-slots (list (list :name 'x :initargs '(:x))
                                              (list :name 'a :initargs '(:a))))))
    (dotcl-mop:finalize-inheritance c)
    (list (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots c))
          (mapcar (lambda (d) (list (first d) (second d)))
                  (dotcl-mop:class-direct-default-initargs c))
          (slot-value (make-instance c :x 5) 'x)))
  ((extra x a) ((:a 'from-meta)) 5))

;;; ENSURE-CLASS takes :DIRECT-DEFAULT-INITARGS for a standard class too.
(deftest metaclass-direct-slots.ensure-class-default-initargs
  (progn
    (dotcl-mop:ensure-class 'mdsl-c4
                            :direct-slots (list (list :name 'z :initargs '(:z)))
                            :direct-default-initargs (list (list :z '5 (lambda () 5))))
    (slot-value (make-instance 'mdsl-c4) 'z))
  5)
