;;; STANDARD-INSTANCE-ACCESS reads and writes the slots a metaclass adds to its
;;; class metaobjects, addressed by the location of the metaclass's slot
;;; definition (AMOP), as it does for ordinary instances. A class metaobject
;;; with a metaclass of its own used to be refused: "not an instance".

(defclass ciam-meta (standard-class)
  ((ciam-extra :initform 7 :accessor ciam-extra)))

(defmethod dotcl-mop:validate-superclass ((a ciam-meta) (b standard-class)) t)

(defclass ciam-obj () () (:metaclass ciam-meta))

(defun ciam-location ()
  (dotcl-mop:slot-definition-location
   (find 'ciam-extra (dotcl-mop:class-slots (find-class 'ciam-meta))
         :key #'dotcl-mop:slot-definition-name)))

(deftest class-metaobject-instance-access.read
  (dotcl-mop:standard-instance-access (find-class 'ciam-obj) (ciam-location))
  7)

(deftest class-metaobject-instance-access.write
  (let ((class (find-class 'ciam-obj)))
    (setf (dotcl-mop:standard-instance-access class (ciam-location)) 42)
    (list (ciam-extra class)
          (dotcl-mop:standard-instance-access class (ciam-location))))
  (42 42))

(deftest class-metaobject-instance-access.out-of-range
  (handler-case (dotcl-mop:standard-instance-access (find-class 'ciam-obj) 99)
    (program-error () :program-error))
  :program-error)
