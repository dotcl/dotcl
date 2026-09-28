;;; A class name is not answered by a same-named DEFTYPE in another package.
;;;
;;; DEFTYPE also registers its expander under the bare symbol name, so a type
;;; referenced through another package's symbol still resolves. TYPEP consulted
;;; that bare-name entry for ANY symbol, including one that names a class. UIOP
;;; defines (deftype timestamp () '(or real boolean)) in its own package, so once
;;; ASDF was loaded (typep nil 'local-time:timestamp) -- a plain DEFCLASS -- was
;;; true, and local-time's argument check let NIL through to an accessor.

(defpackage :dtnc-a (:use :cl))
(defpackage :dtnc-b (:use :cl))

(deftype dtnc-a::stamp () '(or real boolean))
(defclass dtnc-b::stamp () ())

(deftest deftype-name-vs-class.class-not-answered-by-other-deftype
  (list (typep nil 'dtnc-b::stamp)
        (typep 42 'dtnc-b::stamp)
        (typep (make-instance 'dtnc-b::stamp) 'dtnc-b::stamp))
  (nil nil t))

;; The DEFTYPE still answers for its own symbol.
(deftest deftype-name-vs-class.deftype-still-works
  (list (typep nil 'dtnc-a::stamp)
        (typep 42 'dtnc-a::stamp)
        (typep "x" 'dtnc-a::stamp))
  (t t nil))

;; SUBTYPEP (and the other type operators) resolved a symbol through the same
;; bare-name entry. With UIOP loaded, (subtypep 'universal-time '(or timestamp))
;; for a user TIMESTAMP class was (T T): TIMESTAMP was read as UIOP's
;; (or real boolean). serapeum's DISPATCH-CASE then dropped a clause as shadowed.
(deftype dtnc-b::utime () '(integer 0 *))

(deftest deftype-name-vs-class.subtypep-not-answered-by-other-deftype
  (list (multiple-value-list (subtypep 'dtnc-b::utime '(or dtnc-b::stamp)))
        (multiple-value-list (subtypep 'dtnc-b::utime 'dtnc-b::stamp))
        (multiple-value-list (subtypep 'integer 'dtnc-b::stamp))
        (multiple-value-list (subtypep 'dtnc-b::stamp 'real)))
  ((nil t) (nil t) (nil t) (nil t)))

(deftest deftype-name-vs-class.subtypep-own-deftype-still-works
  (list (multiple-value-list (subtypep 'dtnc-b::utime 'dtnc-a::stamp))
        (multiple-value-list (subtypep 'integer '(or dtnc-a::stamp)))
        (multiple-value-list (subtypep 'dtnc-a::stamp 'dtnc-b::stamp)))
  ((t t) (t t) (nil t)))

;; A symbol that names nothing is not a type just because a same-named
;; DEFTYPE exists in another package.
(defpackage :dtnc-c (:use :cl))
(deftype dtnc-a::only-here () (quote integer))

(deftest deftype-name-vs-class.unrelated-symbol-not-a-deftype
  (list (values (subtypep (quote integer) (quote dtnc-c::only-here)))
        (dotcl:known-type-name-p (quote dtnc-c::only-here))
        (dotcl:known-type-name-p (quote dtnc-a::only-here)))
  (nil nil :deftype))

;; Likewise for classes: a symbol that names neither a type nor a class is not
;; resolved to a same-named class that only another package defines. The type
;; parser, KNOWN-TYPE-NAME-P and the name-based SUBTYPEP fallback all looked a
;; class up by its bare name when the symbol itself named none.
(defpackage :dtnc-d (:use :cl))
(defclass dtnc-a::only-class () ())
(defclass dtnc-a::only-class-sub (dtnc-a::only-class) ())
(defstruct (dtnc-a::only-struct (:constructor dtnc-a::make-only-struct)))

(deftest deftype-name-vs-class.unrelated-symbol-not-a-class
  (list (dotcl:known-type-name-p 'dtnc-d::only-class)
        (dotcl:known-type-name-p 'dtnc-a::only-class)
        (multiple-value-list (subtypep 'dtnc-d::only-class 'standard-object))
        (multiple-value-list (subtypep 'dtnc-a::only-class-sub 'dtnc-d::only-class))
        (multiple-value-list (subtypep 'dtnc-d::only-class 'dtnc-a::only-class))
        (multiple-value-list (subtypep 'dtnc-a::only-class-sub 'dtnc-a::only-class)))
  (nil :class (nil nil) (nil nil) (nil nil) (t t)))

(deftest deftype-name-vs-class.typep-unrelated-symbol-not-a-class
  (list (typep (make-instance 'dtnc-a::only-class) 'dtnc-d::only-class)
        (typep (make-instance 'dtnc-a::only-class-sub) 'dtnc-d::only-class)
        (typep (dtnc-a::make-only-struct) 'dtnc-d::only-struct)
        (typep (make-instance 'dtnc-a::only-class-sub) 'dtnc-a::only-class)
        (typep (dtnc-a::make-only-struct) 'dtnc-a::only-struct))
  (nil nil nil t t))
