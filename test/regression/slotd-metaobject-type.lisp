;;; A slot-definition metaobject under a custom slot-definition class answers
;;; TYPE-OF with that class's name, and SLOT-EXISTS-P for the slots the class
;;; adds (the ones SLOT-VALUE already reads). TYPE-OF used to return T for every
;;; slot definition and SLOT-EXISTS-P returned NIL for every slot of a slot
;;; definition, a class or a method, even when SLOT-VALUE could read it.
;;; Expected values were checked against SBCL.

(defclass sdt-meta (standard-class) ((sdt-extra :initform 7)))
(defmethod dotcl-mop:validate-superclass ((a sdt-meta) (b standard-class)) t)
(defclass sdt-mixin () ((sdt-kind :initarg :sdt-kind :initform :base)))
(defclass sdt-dsd (sdt-mixin dotcl-mop:standard-direct-slot-definition) ())
(defclass sdt-esd (sdt-mixin dotcl-mop:standard-effective-slot-definition) ())
(defmethod dotcl-mop:direct-slot-definition-class ((c sdt-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'sdt-dsd))
(defmethod dotcl-mop:effective-slot-definition-class ((c sdt-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'sdt-esd))
(defclass sdt-c () ((a :initform 1 :sdt-kind :key)) (:metaclass sdt-meta))
(dotcl-mop:finalize-inheritance (find-class 'sdt-c))

(deftest slotd-metaobject-type.type-of
  (let ((c (find-class 'sdt-c)))
    (list (type-of (first (dotcl-mop:class-direct-slots c)))
          (type-of (first (dotcl-mop:class-slots c)))))
  (sdt-dsd sdt-esd))

(deftest slotd-metaobject-type.type-of-standard
  (progn
    (defclass sdt-plain () ((a)))
    (dotcl-mop:finalize-inheritance (find-class 'sdt-plain))
    (let ((c (find-class 'sdt-plain)))
      (mapcar (lambda (s)
                (let ((ty (type-of s)))
                  (list ty (eq ty (class-name (class-of s))))))
              (list (first (dotcl-mop:class-direct-slots c))
                    (first (dotcl-mop:class-slots c))))))
  ((dotcl-mop:standard-direct-slot-definition t)
   (dotcl-mop:standard-effective-slot-definition t)))

(deftest slotd-metaobject-type.slot-exists-p
  (let* ((c (find-class 'sdt-c))
         (d (first (dotcl-mop:class-direct-slots c)))
         (e (first (dotcl-mop:class-slots c))))
    (list (slot-exists-p d 'sdt-kind) (slot-value d 'sdt-kind)
          (slot-exists-p e 'sdt-kind)
          (slot-exists-p d 'sdt-no-such-slot)
          (slot-exists-p c 'sdt-extra) (slot-value c 'sdt-extra)
          (slot-exists-p c 'sdt-no-such-slot)
          (slot-exists-p (find-class 'sdt-mixin) 'sdt-extra)))
  (t :key t nil t 7 nil nil))
