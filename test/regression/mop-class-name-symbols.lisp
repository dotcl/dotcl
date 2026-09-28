;;; The metaobject class names DOTCL-MOP exports are the class names
;;; themselves, as on SBCL where SB-MOP exports the symbols SB-PCL names its
;;; classes with. They used to be separate symbols: FIND-CLASS and TYPEP got
;;; through by name, but MAKE-INSTANCE said "no class named".

(defparameter *mcns-class-symbols*
  (let ((acc '()))
    (do-external-symbols (s "DOTCL-MOP")
      (when (find-class s nil) (push s acc)))
    acc))

(deftest mop-class-name-symbols.count
  (>= (length *mcns-class-symbols*) 15)
  t)

(deftest mop-class-name-symbols.class-name-eq
  (remove-if (lambda (s) (eq (class-name (find-class s)) s))
             *mcns-class-symbols*)
  nil)

(deftest mop-class-name-symbols.home-package
  (remove-if (lambda (s) (eq (symbol-package s) (find-package "DOTCL-MOP")))
             *mcns-class-symbols*)
  nil)

(deftest mop-class-name-symbols.make-instance
  (let ((fso (make-instance 'dotcl-mop:funcallable-standard-object)))
    (list (eq (class-of fso) (find-class 'dotcl-mop:funcallable-standard-object))
          (typep fso 'dotcl-mop:funcallable-standard-object)
          (typep fso 'function)))
  (t t t))

(deftest mop-class-name-symbols.type-of-slot-definition
  (progn
    (defclass mcns-plain () ((a)))
    (dotcl-mop:finalize-inheritance (find-class 'mcns-plain))
    (let ((c (find-class 'mcns-plain)))
      (list (type-of (first (dotcl-mop:class-direct-slots c)))
            (type-of (first (dotcl-mop:class-slots c))))))
  (dotcl-mop:standard-direct-slot-definition
   dotcl-mop:standard-effective-slot-definition))

;;; Code (and fasls) that spell the name through DOTCL-INTERNAL still reach
;;; the same symbol.
(deftest mop-class-name-symbols.internal-spelling
  (eq (find-symbol "FUNCALLABLE-STANDARD-OBJECT" "DOTCL-INTERNAL")
      'dotcl-mop:funcallable-standard-object)
  t)
