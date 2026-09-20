;;; DOCUMENTATION of a class OBJECT, not just of the class name.
;;;
;;; DEFCLASS files its docstring under the class name, so (documentation 'foo
;;; 'type) worked. A caller holding the class object -- what FIND-CLASS and
;;; CLASS-OF hand back, and the only thing a metaobject walker has -- got NIL:
;;; no method specialized on CLASS existed, so it fell through to the default
;;; T/T method, which looked the object itself up in the table and found
;;; nothing. SBCL answers the docstring for both spellings.

(defclass doc-class-obj ()
  ((s))
  (:documentation "the documented one"))

(defclass doc-class-obj-undocumented ()
  ((s)))

(deftest documentation-of-a-class-object-type
  (documentation (find-class 'doc-class-obj) 'type)
  "the documented one")

;;; CLHS lets a class object be asked with doc-type T as well.

(deftest documentation-of-a-class-object-t
  (documentation (find-class 'doc-class-obj) t)
  "the documented one")

;;; The name spelling has to keep working, and the two must agree.

(deftest documentation-by-name-and-by-object-agree
  (equal (documentation 'doc-class-obj 'type)
         (documentation (find-class 'doc-class-obj) 'type))
  t)

;;; CLASS-OF is the usual way to reach the object, so check that path too.

(deftest documentation-via-class-of-an-instance
  (documentation (class-of (make-instance 'doc-class-obj)) 'type)
  "the documented one")

;;; A class with no docstring answers NIL rather than inventing one.

(deftest documentation-of-an-undocumented-class-object
  (list (documentation (find-class 'doc-class-obj-undocumented) 'type)
        (documentation (find-class 'doc-class-obj-undocumented) t))
  (nil nil))

;;; Setting through the object is what comes back afterwards.

(defclass doc-class-obj-setf ()
  ((s))
  (:documentation "before"))

(deftest documentation-setf-through-the-class-object
  (progn
    (setf (documentation (find-class 'doc-class-obj-setf) 'type) "after")
    (documentation (find-class 'doc-class-obj-setf) 'type))
  "after")
