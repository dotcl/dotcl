;;; The effective slot's documentation is the most specific :DOCUMENTATION
;;; given among the direct slots of that name (CLHS 7.5.3). A subclass that
;;; restates the slot without one used to drop the inherited string.

(defclass sdi-a () ((s :initform 1 :documentation "doc a") (w :documentation "doc w")))
(defclass sdi-b (sdi-a) ((s :initform 2) (w :initarg :w :documentation "doc w2")))
(defclass sdi-c (sdi-b) ((s :initarg :s)))

(defun %sdi-doc (class name)
  (dotcl-mop:finalize-inheritance (find-class class))
  (documentation (find name (dotcl-mop:class-slots (find-class class))
                       :key #'dotcl-mop:slot-definition-name)
                 t))

(deftest slot-documentation-inherited.from-superclass
  (list (%sdi-doc 'sdi-b 's) (%sdi-doc 'sdi-c 's))
  ("doc a" "doc a"))

(deftest slot-documentation-inherited.most-specific-wins
  (list (%sdi-doc 'sdi-b 'w) (%sdi-doc 'sdi-c 'w))
  ("doc w2" "doc w2"))
