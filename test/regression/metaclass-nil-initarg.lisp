;;; A slot :INITARG may name NIL or T; those stay values, never package-table symbols.

(defclass mni-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mni-meta) (s standard-class)) t)

(defclass mni-direct (dotcl-mop:standard-direct-slot-definition)
  ((index :initarg :index :initform :none :reader mni-index)))
(defclass mni-effective (dotcl-mop:standard-effective-slot-definition) ())

(defmethod dotcl-mop:direct-slot-definition-class ((c mni-meta) &rest initargs)
  (declare (ignore initargs))
  'mni-direct)
(defmethod dotcl-mop:effective-slot-definition-class ((c mni-meta) &rest initargs)
  (declare (ignore initargs))
  'mni-effective)

(defclass mni-user ()
  ((a :initarg nil :initform 1 :index nil)
   (b :initarg t :initform 2))
  (:metaclass mni-meta))

(deftest mni-direct-slot-initargs
  (mapcar #'dotcl-mop:slot-definition-initargs
          (dotcl-mop:class-direct-slots (find-class 'mni-user)))
  ((nil) (t)))

(deftest mni-direct-slot-class
  (mapcar (lambda (s) (typep s 'mni-direct))
          (dotcl-mop:class-direct-slots (find-class 'mni-user)))
  (t t))

(deftest mni-direct-slot-option-value
  (mni-index (first (dotcl-mop:class-direct-slots (find-class 'mni-user))))
  nil)

(deftest mni-effective-slot-initargs
  (mapcar #'dotcl-mop:slot-definition-initargs
          (dotcl-mop:class-slots (find-class 'mni-user)))
  ((nil) (t)))

(deftest mni-effective-slot-class
  (mapcar (lambda (s) (typep s 'mni-effective))
          (dotcl-mop:class-slots (find-class 'mni-user)))
  (t t))

(deftest mni-make-instance-with-nil-initarg
  (slot-value (make-instance 'mni-user nil 10) 'a)
  10)

(deftest mni-make-instance-with-t-initarg
  (slot-value (make-instance 'mni-user t 20) 'b)
  20)

;;; Redefinition also consults DIRECT-SLOT-DEFINITION-CLASS and rebuilds plists.
(deftest mni-redefinition
  (progn
    (defclass mni-user () ((a :initarg nil :initform 3)) (:metaclass mni-meta))
    (list (mapcar #'dotcl-mop:slot-definition-initargs
                  (dotcl-mop:class-direct-slots (find-class 'mni-user)))
          (slot-value (make-instance 'mni-user nil 30) 'a)))
  (((nil)) 30))

;;; ENSURE-CLASS-shaped :DIRECT-SLOTS plists take the same path.
(deftest mni-make-instance-class-with-nil-initargs
  (let ((c (make-instance 'mni-meta :name 'mni-ens
                          :direct-slots (list (list :name 'z :initargs (list nil t))))))
    (dotcl-mop:finalize-inheritance c)
    (list (typep (first (dotcl-mop:class-direct-slots c)) 'mni-direct)
          (dotcl-mop:slot-definition-initargs (first (dotcl-mop:class-direct-slots c)))
          (dotcl-mop:slot-definition-initargs (first (dotcl-mop:class-slots c)))))
  (t (nil t) (nil t)))

(deftest mni-slot-definition-metaobject-initargs
  (dotcl-mop:slot-definition-initargs
   (make-instance 'dotcl-mop:standard-direct-slot-definition
                  :name 'q :initargs (list nil t)))
  (nil t))
