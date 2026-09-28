;;; CLASS-FINALIZED-P is NIL while a class is being made: a metaclass's
;;; INITIALIZE-INSTANCE runs before finalization, and CLASS-SLOTS is still
;;; empty there. It used to answer T for every class that was not itself
;;; forward-referenced. FINALIZE-INHERITANCE called from the metaclass's
;;; INITIALIZE-INSTANCE finalizes the class, and a class whose superclass is
;;; still forward-referenced is not finalized either.
;;; Expected values were checked against SBCL (sb-mop).

(defvar *cfp-log* nil)
(defclass cfp-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a cfp-meta) (b standard-class)) t)
(defmethod initialize-instance :around ((c cfp-meta) &key)
  (let ((r (call-next-method)))
    (push (list :around (dotcl-mop:class-finalized-p c)) *cfp-log*)
    r))
(defmethod initialize-instance :after ((c cfp-meta) &key)
  (push (list :after (dotcl-mop:class-finalized-p c)) *cfp-log*))
(defclass cfp-base () ((a :initform 1)))
(defclass cfp-c (cfp-base) ((b :initform 2)) (:metaclass cfp-meta))

(deftest metaclass-finalized-p.during-initialize-instance
  (let ((c (find-class 'cfp-c)))
    (make-instance c)
    (list (reverse *cfp-log*) (dotcl-mop:class-finalized-p c)
          (sort (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots c))
                #'string<)))
  (((:after nil) (:around nil)) t (a b)))

(defvar *cfp-log2* nil)
(defclass cfp-meta2 (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a cfp-meta2) (b standard-class)) t)
(defmethod initialize-instance :after ((c cfp-meta2) &key)
  (push (dotcl-mop:class-finalized-p c) *cfp-log2*)
  (dotcl-mop:finalize-inheritance c)
  (push (dotcl-mop:class-finalized-p c) *cfp-log2*)
  (push (sort (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots c))
              #'string<)
        *cfp-log2*))
(defclass cfp-d (cfp-base) ((b :initform 2)) (:metaclass cfp-meta2))

(deftest metaclass-finalized-p.finalize-inheritance-in-after
  (reverse *cfp-log2*)
  (nil t (a b)))

(defclass cfp-fwd (cfp-not-yet) ())

(deftest metaclass-finalized-p.forward-referenced-superclass
  (let ((before (dotcl-mop:class-finalized-p (find-class 'cfp-fwd))))
    (eval '(defclass cfp-not-yet () ((q))))
    (make-instance 'cfp-fwd)
    (list before (dotcl-mop:class-finalized-p (find-class 'cfp-fwd))))
  (nil t))
