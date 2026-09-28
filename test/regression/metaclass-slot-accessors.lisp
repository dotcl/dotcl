;;; Reader and writer methods for direct slots that reach a class through the
;;; metaobject protocol. AMOP has class initialization define the methods named
;;; by each direct slot definition's :READERS and :WRITERS. DEFCLASS used to be
;;; the only thing that defined them (in its expansion, from the slots it parsed),
;;; so a slot a metaclass added by rewriting :DIRECT-SLOTS, and every slot given
;;; to ENSURE-CLASS or to REINITIALIZE-INSTANCE of a class, had no accessors.
;;; Expected values were checked against SBCL.

(defclass msa-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c msa-meta) (s standard-class)) t)

;; Adds one slot, and copies every slot plist it was given (a rewrite that
;; hands back fresh plists for the DEFCLASS slots too).
(defmethod initialize-instance :around ((c msa-meta) &rest args &key direct-slots &allow-other-keys)
  (apply #'call-next-method c
         :direct-slots (append (mapcar #'copy-list direct-slots)
                               (list (list :name 'msa-extra :initform 42
                                           :initfunction (lambda () 42)
                                           :readers '(msa-extra-of)
                                           :writers '((setf msa-extra-of)))))
         args))

(defclass msa-c () ((a :initarg :a :accessor msa-a-of)) (:metaclass msa-meta))

(deftest metaclass-slot-accessors.added-by-metaclass
  (let ((o (make-instance 'msa-c :a 1)))
    (list (msa-a-of o)
          (msa-extra-of o)
          (progn (setf (msa-extra-of o) 7) (msa-extra-of o))
          (progn (setf (msa-a-of o) 2) (msa-a-of o))))
  (1 42 7 2))

;; A method for each accessor, not one per definition path.
(deftest metaclass-slot-accessors.one-method-each
  (list (length (dotcl-mop:generic-function-methods #'msa-a-of))
        (length (dotcl-mop:generic-function-methods #'msa-extra-of))
        (length (dotcl-mop:generic-function-methods #'(setf msa-extra-of))))
  (1 1 1))

(deftest metaclass-slot-accessors.ensure-class
  (let* ((c (dotcl-mop:ensure-class 'msa-ec
                                    :direct-slots '((:name a :initargs (:a)
                                                     :readers (msa-ec-a)
                                                     :writers ((setf msa-ec-a))))))
         (o (make-instance c :a 3)))
    (list (msa-ec-a o) (progn (setf (msa-ec-a o) 4) (msa-ec-a o))))
  (3 4))

(defclass msa-ri () ((a :initform 1)))

(deftest metaclass-slot-accessors.reinitialize-instance
  (progn
    (reinitialize-instance (find-class 'msa-ri)
                           :direct-slots (list (list :name 'a :initform 1
                                                     :initfunction (lambda () 1)
                                                     :readers '(msa-ri-a))
                                               (list :name 'b :initform 2
                                                     :initfunction (lambda () 2)
                                                     :readers '(msa-ri-b))))
    (let ((o (make-instance 'msa-ri)))
      (list (msa-ri-a o) (msa-ri-b o))))
  (1 2))
