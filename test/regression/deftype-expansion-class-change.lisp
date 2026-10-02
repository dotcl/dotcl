;;; A DEFTYPE whose expansion depends on whether a class exists. The expansion
;;; is memoized per symbol; defining, redefining or removing a class must drop
;;; that memo, or TYPEP keeps answering with the expansion chosen before.
;;; Each class change is its own top level form, as in the report. Expected
;;; values are SBCL 2.6.8's.

(deftype %dec-maybe-foo () (if (find-class '%dec-rvfoo nil) '%dec-rvfoo 'null))
(deftest deftype-expansion-class-change.defclass-before (typep nil '%dec-maybe-foo) t)
(defclass %dec-rvfoo () ())
(deftest deftype-expansion-class-change.defclass-after
  (list (typep (make-instance '%dec-rvfoo) '%dec-maybe-foo) (typep nil '%dec-maybe-foo))
  (t nil))

(deftype %dec-maybe-bar () (if (find-class '%dec-rvbar nil) '%dec-rvbar 'null))
(deftest deftype-expansion-class-change.defstruct-before (typep nil '%dec-maybe-bar) t)
(defstruct %dec-rvbar a)
(deftest deftype-expansion-class-change.defstruct-after
  (list (typep (make-%dec-rvbar) '%dec-maybe-bar) (typep nil '%dec-maybe-bar))
  (t nil))

(deftype %dec-maybe-baz () (if (find-class '%dec-rvbaz nil) 'integer 'string))
(deftest deftype-expansion-class-change.setf-find-class-before
  (list (typep 1 '%dec-maybe-baz) (typep "s" '%dec-maybe-baz))
  (nil t))
(setf (find-class '%dec-rvbaz) (find-class 'standard-object))
(deftest deftype-expansion-class-change.setf-find-class-set
  (list (typep 1 '%dec-maybe-baz) (typep "s" '%dec-maybe-baz))
  (t nil))
(setf (find-class '%dec-rvbaz) nil)
(deftest deftype-expansion-class-change.setf-find-class-removed
  (list (typep 1 '%dec-maybe-baz) (typep "s" '%dec-maybe-baz))
  (nil t))
