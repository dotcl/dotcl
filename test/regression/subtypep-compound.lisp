;;; SUBTYPEP over AND / OR / NOT / MEMBER combinations, with the answers SBCL
;;; gives. A NIL NIL here made serapeum's ETYPECASE-OF signal a full WARNING
;;; ("Can't check exhaustiveness"), which fails COMPILE-FILE.

(defun %stc-sub (a b) (multiple-value-list (subtypep a b)))

(deftype stc-function-name () '(and symbol (not (member t nil))))
(deftype stc-key-designator () '(or null stc-function-name function))

;; A type is a subtype of a union that contains an equal type, and a union is
;; a subtype of an equal union, even when the parts are not one object.
(deftest subtypep-compound.member-of-union
  (list (%stc-sub 'stc-function-name 'stc-key-designator)
        (%stc-sub '(or null stc-function-name function) 'stc-key-designator)
        (%stc-sub 'stc-key-designator '(or null stc-function-name function))
        (%stc-sub '(and symbol (not (member t nil)))
                  '(or null (and symbol (not (member t nil))) function))
        (%stc-sub '(or integer (and symbol (not keyword)))
                  '(or (and symbol (not keyword)) integer)))
  ((t t) (t t) (t t) (t t) (t t)))

;; One conjunct being a subtype of the target is enough, whatever the target.
(deftest subtypep-compound.conjunct-subtype
  (list (%stc-sub '(and symbol (not (member t nil))) '(not (member t nil)))
        (%stc-sub '(and symbol (not (member t nil))) 'symbol)
        (%stc-sub '(and integer (satisfies evenp)) '(or integer string)))
  ((t t) (t t) (t t)))

;; A numeric class is the disjoint union of its subclasses (REAL = RATIONAL +
;; FLOAT, RATIONAL = INTEGER + RATIO, ...), so it is a subtype of a union that
;; covers every part, and certainly not one that leaves a part out.
(deftype stc-real* () '(or rational float))

(deftest subtypep-compound.numeric-partition
  (list (%stc-sub 'stc-real* '(or ratio float (integer 0 *) integer))
        (%stc-sub 'stc-real* '(or integer ratio float))
        (%stc-sub 'real '(or rational float))
        (%stc-sub 'rational '(or integer ratio))
        (%stc-sub 'integer '(or fixnum bignum))
        (%stc-sub 'number '(or real complex))
        (%stc-sub 'float '(or single-float double-float)))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-compound.numeric-partition-missing-part
  (list (%stc-sub 'stc-real* '(or ratio float))
        (%stc-sub 'real '(or rational single-float))
        (%stc-sub 'integer '(or ratio float))
        (%stc-sub 'bignum '(or fixnum ratio)))
  ((nil t) (nil t) (nil t) (nil t)))

;; MEMBER on the right: X <: (OR (MEMBER ...) Y) when what X has outside the
;; MEMBER is in Y. And an object named by a MEMBER that is of the first type
;; but not of the second shows the answer is a certain NO.
(deftest subtypep-compound.member-split
  (list (%stc-sub 'symbol '(or (member t nil) (and symbol (not (member t nil)))))
        (%stc-sub 'symbol '(or null (and symbol (not (member t nil)))))
        (%stc-sub 'null '(and symbol (not (member t nil))))
        (%stc-sub 'boolean 'stc-function-name))
  ((t t) (nil t) (nil t) (nil t)))

;; A symbol that names no type says nothing about its objects, so a NO is never
;; certain when one is involved (SBCL answers NIL NIL the same way). A YES that
;; does not depend on it still is.
(deftest subtypep-compound.unknown-type-name
  (list (%stc-sub '(member a) 'stc-no-such-type)
        (%stc-sub '(member a b) 'stc-no-such-type)
        (%stc-sub '(eql 3) 'stc-no-such-type)
        (%stc-sub '(member a) '(and symbol stc-no-such-type))
        (%stc-sub '(member a) '(or (member b) stc-no-such-type))
        (%stc-sub '(integer 0 1) 'stc-no-such-type)
        (%stc-sub '(or (member a) integer) 'stc-no-such-type)
        (%stc-sub 'integer 'stc-no-such-type)
        (%stc-sub 'stc-no-such-type 'integer)
        (%stc-sub 'stc-no-such-type t)
        (%stc-sub nil 'stc-no-such-type)
        (%stc-sub '(and integer stc-no-such-type) 'number)
        (%stc-sub '(cons stc-no-such-type) 'cons))
  ((nil nil) (nil nil) (nil nil) (nil nil) (nil nil) (nil nil) (nil nil)
   (nil nil) (nil nil) (t t) (t t) (t t) (t t)))

;; An integer type is a subtype of a union exactly when the integers in the
;; union (ranges, integer type names, MEMBER, the integers of bounded RATIONAL
;; and REAL) cover it; members of other number classes hold no integers.
(deftest subtypep-compound.integer-ranges-cover
  (list (%stc-sub 'integer '(or (integer 0 *) (integer * -1)))
        (%stc-sub 'integer '(or unsigned-byte (integer * -1)))
        (%stc-sub 'integer '(or (integer 0 10) (integer 11 *) (integer * -1)))
        (%stc-sub '(integer 0 10) '(or (integer 0 5) (integer 6 10)))
        (%stc-sub '(integer (0) (10)) '(or (integer 1 5) (integer 6 9)))
        (%stc-sub 'bignum '(or (integer * -1) (integer 0 *)))
        (%stc-sub 'integer '(or (integer * 0) bignum (integer 1 *)))
        (%stc-sub '(integer 0 10) '(or (integer 0 5) (rational 5 10)))
        (%stc-sub '(integer 0 10) '(or (integer 0 5) (real 11/2 10)))
        (%stc-sub 'bit '(or (eql 0) (eql 1)))
        (%stc-sub '(integer 5 3) '(or ratio float)))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-compound.integer-ranges-gap
  (list (%stc-sub 'integer '(or (integer 1 *) (integer * -1)))
        (%stc-sub '(integer 0 10) '(or (integer 0 5) (integer 7 10)))
        (%stc-sub '(integer 0 10) '(or (integer 0 4) (integer 6 10) symbol))
        (%stc-sub '(integer 0 10) '(or ratio float))
        (%stc-sub '(integer 0 10) '(or (integer 5 30) float))
        (%stc-sub 'unsigned-byte '(or (integer * -1) float))
        (%stc-sub 'fixnum '(or bignum (integer 0 *))))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

;; When the type algebra has no answer, an object of the first type that is not
;; of the second shows a certain NO: #(1 2) is a VECTOR but neither a STRING nor
;; a BIT-VECTOR, (:W) is a LIST but not (CONS INTEGER) nor NULL.
(deftest subtypep-compound.witness-object
  (list (%stc-sub 'vector '(or string bit-vector))
        (%stc-sub 'vector '(or simple-vector string))
        (%stc-sub 'array '(or vector string))
        (%stc-sub 'sequence '(or list string))
        (%stc-sub 'list '(or (cons integer) null))
        (%stc-sub t '(function (t) t)))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

;; No counterexample is looked for where TYPEP could call a user's predicate
;; (SATISFIES, also inside CONS) or signal: a handler of the caller would see it.
(defun stc-pred-1 (x) x)
(deftest subtypep-compound.witness-object-not-tried
  (list (%stc-sub '(cons (not float)) '(or (cons (satisfies stc-pred-1) (satisfies stc-pred-1))))
        (handler-case (%stc-sub '(function (t) t) 'function)
          (error () :error)))
  ((nil nil) (t t)))

;; LIST is CONS + NULL and SEQUENCE is LIST + VECTOR (no other object is a
;; sequence here), so they are subtypes of a union that covers every part. And
;; X <: (OR A (AND X (NOT A))) when another member names the complement of A.
(deftest subtypep-compound.partition-and-complement
  (list (%stc-sub 'list '(or cons null))
        (%stc-sub 'list '(or null cons))
        (%stc-sub 'list '(or cons symbol))
        (%stc-sub 'sequence '(or list vector))
        (%stc-sub 'sequence '(or cons null vector))
        (%stc-sub 'string '(or simple-string (and string (not simple-string))))
        (%stc-sub 'bit-vector '(or simple-bit-vector (and bit-vector (not simple-bit-vector))))
        (%stc-sub 'list '(or cons (and list (not cons)))))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-compound.partition-missing-part
  (list (%stc-sub 'list 'cons)
        (%stc-sub 'list '(or cons vector))
        (%stc-sub 'sequence '(or cons vector))
        (%stc-sub 'sequence '(or list string)))
  ((nil t) (nil t) (nil t) (nil t)))
