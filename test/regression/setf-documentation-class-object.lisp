;;; (SETF DOCUMENTATION) on a class object stores the documentation of the type its
;;; name denotes: the object and the name read it back alike.

(defclass sdco-c () () (:documentation "old"))
(defstruct sdco-s "sdoc")

(deftest setf-documentation-class-object.t
  (progn (setf (documentation (find-class 'sdco-c) t) "new")
         (list (documentation (find-class 'sdco-c) t)
               (documentation (find-class 'sdco-c) 'type)
               (documentation 'sdco-c 'type)))
  ("new" "new" "new"))

(deftest setf-documentation-class-object.type-and-name
  (progn (setf (documentation (find-class 'sdco-c) 'type) "viatype")
         (setf (documentation 'sdco-c 'type) "vianame")
         (list (documentation (find-class 'sdco-c) t)
               (documentation 'sdco-c 'type)))
  ("vianame" "vianame"))

(deftest setf-documentation-class-object.structure
  (progn (setf (documentation (find-class 'sdco-s) t) "n2")
         (list (documentation (find-class 'sdco-s) t) (documentation 'sdco-s 'type)))
  ("n2" "n2"))

(deftest setf-documentation-class-object.anonymous-and-nil
  (let ((c (make-instance 'standard-class)))
    (setf (documentation c t) "anon")
    (let ((a (documentation c t)))
      (setf (documentation c t) nil)
      (list a (documentation c t))))
  ("anon" nil))
