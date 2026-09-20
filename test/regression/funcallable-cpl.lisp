;;; Two built-in classes must not order the same pair differently.
;;;
;;; GENERIC-FUNCTION has FUNCTION then STANDARD-OBJECT among its superclasses.
;;; FUNCALLABLE-STANDARD-OBJECT had them the other way round. Each is fine on
;;; its own, but a class that inherits from both -- a generic function class
;;; with a FUNCALLABLE-STANDARD-CLASS mixin, which is how cl-data-structures
;;; writes its operation classes and therefore how vellum reaches dotcl -- has
;;; no linearisation that respects both, and DEFCLASS answered "inconsistent
;;; precedence graph" for a hierarchy that is consistent.
;;;
;;; SBCL orders FUNCALLABLE-STANDARD-OBJECT as
;;; (funcallable-standard-object function standard-object ... t), and its CPL
;;; for the shape below is
;;; (fcpl-gf standard-generic-function generic-function metaobject
;;;  fcpl-mixin funcallable-standard-object function standard-object t).

(defclass fcpl-mixin ()
  ()
  (:metaclass dotcl-mop:funcallable-standard-class))

;; The shape that could not be defined at all.
(defclass fcpl-gf (standard-generic-function fcpl-mixin)
  ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defun %fcpl-names (class-name)
  ;; Symbol NAMES, not the symbols: the class of FUNCALLABLE-STANDARD-OBJECT is
  ;; named by a symbol in the implementation package, while DOTCL-MOP exports a
  ;; symbol of its own that FIND-CLASS also accepts. Comparing identity would
  ;; silently miss.
  (mapcar (lambda (c) (symbol-name (class-name c)))
          (dotcl-mop:class-precedence-list (find-class class-name))))

(defun %fcpl-before-p (a b names)
  "T when A comes before B in NAMES, and both are there."
  (let ((pa (position a names :test (function string=))) (pb (position b names :test (function string=))))
    (and pa pb (< pa pb))))

;;; ---- the pair both classes name ----

(deftest funcallable-cpl.function-precedes-standard-object
  (let ((names (%fcpl-names 'dotcl-mop:funcallable-standard-object)))
    (list (%fcpl-before-p "FUNCTION" "STANDARD-OBJECT" names)
          (%fcpl-before-p "FUNCALLABLE-STANDARD-OBJECT" "FUNCTION" names)))
  (t t))

;; The same order GENERIC-FUNCTION uses -- that they agree is the whole fix.
(deftest funcallable-cpl.generic-function-agrees
  (let ((names (%fcpl-names 'generic-function)))
    (%fcpl-before-p "FUNCTION" "STANDARD-OBJECT" names))
  t)

;;; ---- the class that could not be defined ----

(deftest funcallable-cpl.mixed-generic-function-class-has-a-cpl
  (let ((names (%fcpl-names 'fcpl-gf)))
    (list (%fcpl-before-p "FCPL-GF" "STANDARD-GENERIC-FUNCTION" names)
          (%fcpl-before-p "STANDARD-GENERIC-FUNCTION" "FCPL-MIXIN" names)
          (%fcpl-before-p "FCPL-MIXIN" "FUNCTION" names)
          (%fcpl-before-p "FUNCTION" "STANDARD-OBJECT" names)
          (string= (car (last names)) "T")))
  (t t t t t))

;; Order as SBCL answers it for the classes both implementations have. dotcl has
;; no SB-PCL::SLOT-OBJECT and does not yet put GENERIC-FUNCTION under
;; FUNCALLABLE-STANDARD-OBJECT, so the shared subsequence is what is compared,
;; not the whole list.
(deftest funcallable-cpl.order-matches-sbcl
  (let* ((names (%fcpl-names 'fcpl-gf))
         (shared (remove-if-not
                  (lambda (n) (member n '("FCPL-GF" "STANDARD-GENERIC-FUNCTION"
                                          "GENERIC-FUNCTION" "FCPL-MIXIN"
                                          "FUNCTION" "STANDARD-OBJECT" "T")
                                       :test #'string=))
                  names)))
    shared)
  ("FCPL-GF" "STANDARD-GENERIC-FUNCTION" "GENERIC-FUNCTION" "FCPL-MIXIN"
   "FUNCTION" "STANDARD-OBJECT" "T"))

;; An ordinary class is untouched by any of this.
(defclass fcpl-plain () ())

(deftest funcallable-cpl.plain-class-unchanged
  (%fcpl-names 'fcpl-plain)
  ("FCPL-PLAIN" "STANDARD-OBJECT" "T"))
