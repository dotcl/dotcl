;;; A class metaobject's INITIALIZE-INSTANCE receives :NAME and
;;; :DIRECT-SUPERCLASSES (AMOP), and a metaclass method that rewrites
;;; :DIRECT-SUPERCLASSES and calls the next method with the new list gets a
;;; class with those superclasses. This is the "topmost class" MOP pattern
;;; (serapeum's TOPMOST-OBJECT-CLASS, clsql's view classes). The class used to be
;;; built before INITIALIZE-INSTANCE ran, which saw neither key, and a rewritten
;;; list was ignored.

(defclass mds-top () ())
(defclass mds-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mds-meta) (b standard-class)) t)

(defvar *mds-seen* nil)
(defmethod initialize-instance :around ((c mds-meta) &rest initargs
                                        &key name direct-superclasses)
  (push (list name (mapcar #'class-name direct-superclasses)) *mds-seen*)
  (if (member (find-class 'mds-top) direct-superclasses)
      (call-next-method)
      (apply #'call-next-method c
             :direct-superclasses (cons (find-class 'mds-top) direct-superclasses)
             initargs)))

(deftest metaclass-direct-superclasses.defclass
  (progn
    (setf *mds-seen* nil)
    (defclass mds-c1 () ((a :initarg :a :reader mds-c1-a)) (:metaclass mds-meta))
    (let ((obj (make-instance 'mds-c1 :a 5)))
      (list *mds-seen*
            (mapcar #'class-name
                    (dotcl-mop:class-direct-superclasses (find-class 'mds-c1)))
            (typep obj 'mds-top)
            (mds-c1-a obj))))
  (((mds-c1 ())) (mds-top) t 5))

(deftest metaclass-direct-superclasses.make-instance
  (progn
    (setf *mds-seen* nil)
    (let ((c (make-instance 'mds-meta :name 'mds-c2)))
      (list *mds-seen*
            (mapcar #'class-name (dotcl-mop:class-direct-superclasses c))
            (typep (make-instance c) 'mds-top))))
  (((mds-c2 (standard-object))) (mds-top standard-object) t))

(deftest metaclass-direct-superclasses.unchanged
  (progn
    (defclass mds-c3 (mds-top) () (:metaclass mds-meta))
    (mapcar #'class-name
            (dotcl-mop:class-direct-superclasses (find-class 'mds-c3))))
  (mds-top))
