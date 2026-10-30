;;; SUBTYPEP can prove a NO when the two built-in shapes cannot share an
;;; object. These answers were NIL NIL while SBCL answered NIL T (or T T for a
;;; NOT), so the type algebra never looked at the pair's shape.

(defun %sd (a b) (multiple-value-list (subtypep a b)))

(defclass sd-class () ())
(define-condition sd-condition (error) ())
(defstruct sd-struct)

(deftest subtypep-disjointness.array-vs-scalar
  (list (%sd '(base-string 3) '(real 0 1))
        (%sd '(vector (unsigned-byte 8) 3) '(rational 0 (1)))
        (%sd '(integer -10 0) '(array integer (2 3)))
        (%sd '(float 0.0 1.0) '(cons integer integer))
        (%sd '(simple-array t (3)) '(float 0.0 1.0))
        (%sd '(unsigned-byte 16) '(array integer (2 3))))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-disjointness.member-and-eql
  (list (%sd '(float 0.0d0 1.0d0) '(member 5))
        (%sd '(double-float 0.0d0 1.0d0) '(eql 1))
        (%sd 'pathname '(eql foo))
        (%sd 'sd-class '(eql #\a))
        (%sd '(base-string 3) '(eql 1/2))
        (%sd 'sd-struct '(member 1 2 3)))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-disjointness.classes-and-conditions
  (list (%sd 'sd-class '(or single-float double-float))
        (%sd 'sd-condition '(or array vector))
        (%sd 'sd-condition '(not array))
        (%sd '(cons integer list) 'sd-condition)
        (%sd 'sd-class '(and integer (satisfies evenp))))
  ((nil t) (nil t) (t t) (nil t) (nil t)))

;; A NOT is a subtype of a type exactly when the two are disjoint. The built-in
;; classes settle that for LIST versus STRING and for arrays versus numbers.
(deftest subtypep-disjointness.negation
  (list (%sd 'list '(not string))
        (%sd '(string 3) '(not integer))
        (%sd '(array (unsigned-byte 8) (3)) '(not string))
        (%sd '(array t) '(not string))
        (%sd 'condition '(not array))
        (%sd '(string 0) '(not cons)))
  ((t t) (t t) (t t) (t t) (t t) (t t)))

;; In the other direction, a built-in kind outside both the negated type and
;; the target shows that (NOT X) certainly has an object outside the target.
(deftest subtypep-disjointness.negation-certain-no
  (list (%sd '(not integer) '(vector integer))
        (%sd '(not string) '(and integer (satisfies evenp)))
        (%sd '(not cons) '(and (integer 0 10) (satisfies evenp))))
  ((nil t) (nil t) (nil t)))

(deftest subtypep-disjointness.unions-and-intersections
  (list (%sd 'type-error '(or array vector))
        (%sd '(array t (2 3)) '(or cons null))
        (%sd '(array t (2 3)) '(and (integer 0 10) (satisfies evenp)))
        (%sd '(or array vector) '(vector integer))
        (%sd '(cons (member 1 2) string) '(or array vector)))
  ((nil t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-disjointness.array-shapes
  (list (%sd '(array fixnum) '(array integer))
        (%sd '(array character) '(array base-char))
        (%sd '(array t (2 3)) '(vector t))
        (%sd 'string '(array t)))
  ((nil t) (nil t) (nil t) (nil t)))
