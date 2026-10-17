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
;; FLOAT + DOTCL:DECIMAL, RATIONAL = INTEGER + RATIO, ...), so it is a subtype of a union that
;; covers every part, and certainly not one that leaves a part out.
(deftype stc-real* () '(or rational float))

(deftest subtypep-compound.numeric-partition
  (list (%stc-sub 'stc-real* '(or ratio float (integer 0 *) integer))
        (%stc-sub 'stc-real* '(or integer ratio float))
        (%stc-sub 'real '(or rational float dotcl:decimal))
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

;; Array types whose element type is one arrays are made of are tried with the
;; same objects: a bit vector is a VECTOR but neither a STRING nor a (VECTOR T).
(deftest subtypep-compound.witness-array-element-type
  (list (%stc-sub 'vector '(or string (vector t)))
        (%stc-sub 'vector '(or string bit-vector (vector t)))
        (%stc-sub '(simple-array * (*)) '(or simple-string simple-vector))
        (%stc-sub 'array '(or string (array t)))
        (%stc-sub '(vector t) '(or simple-vector (vector character))))
  ((nil t) (nil t) (nil t) (nil t) (nil t)))

;; A size on a string or bit vector type is kept: (BASE-STRING 17) is
;; (VECTOR BASE-CHAR 17), (STRING 17) the strings of length 17.
(deftest subtypep-compound.sized-string-types
  (list (%stc-sub '(base-string 17) '(vector base-char 17))
        (%stc-sub '(simple-base-string 17) '(simple-array base-char (17)))
        (%stc-sub '(string 17) 'string)
        (%stc-sub 'string '(string 17))
        (%stc-sub '(string 17) '(or (vector character 17) (vector base-char 17) (vector nil 17)))
        (%stc-sub '(bit-vector 3) 'bit-vector))
  ((t t) (t t) (t t) (nil t) (t t) (t t)))

;; CHARACTER is BASE-CHAR + EXTENDED-CHAR, and a string's element type is
;; CHARACTER, BASE-CHAR or NIL: a (VECTOR NIL) is a STRING here (ANSI STRING.8)
;; but neither a BASE-STRING nor a (VECTOR CHARACTER).
(deftest subtypep-compound.partition-character-string
  (list (%stc-sub 'character '(or base-char extended-char))
        (%stc-sub 'character '(or extended-char base-char))
        (%stc-sub 'string '(or base-string (vector character) (vector nil)))
        (%stc-sub 'simple-string '(or simple-base-string (simple-array character (*))
                                   (simple-array nil (*))))
        (%stc-sub 'string '(or (vector character) (vector base-char)))
        (%stc-sub 'string '(or simple-string (vector character)))
        (%stc-sub 'character '(or standard-char extended-char))
        (%stc-sub '(vector standard-char) 'base-string))
  ((t t) (t t) (t t) (t t) (nil t) (nil t) (nil t) (t t)))

;; A bare SIGNED-BYTE is every integer, not (SIGNED-BYTE 8).
(deftest subtypep-compound.bare-signed-byte
  (list (%stc-sub 'signed-byte '(integer -128 127))
        (%stc-sub 'signed-byte '(integer -128 *))
        (%stc-sub 'signed-byte '(signed-byte 8))
        (%stc-sub '(signed-byte 8) 'signed-byte)
        (%stc-sub 'signed-byte 'integer))
  ((nil t) (nil t) (nil t) (t t) (t t)))

;; Real ranges with float ends against a union, class by class on the real line.
(deftest subtypep-compound.real-ranges-cover
  (list (%stc-sub '(real 5.5 10) '(or (real 5.5 7) (real 7 10)))
        (%stc-sub '(integer 0 10) '(or (integer 0 5) (real 5.5 10)))
        (%stc-sub '(single-float 0.0 1.0) '(or (single-float 0.0 0.5) (single-float (0.5) 1.0)))
        (%stc-sub 'single-float '(or (single-float (0.0)) (single-float * 0.0)))
        (%stc-sub '(float 0.0 1.0) '(or (single-float 0.0 1.0) (double-float 0d0 1d0)))
        (%stc-sub '(real 1 2) '(or (rational 1 2) (float 1.0 2.0) dotcl:decimal))
        (%stc-sub '(rational 0 2) '(or (rational 0 (1)) (rational (1) 2) integer))
        (%stc-sub '(single-float 0.0 1.0) '(or (single-float 0.0 (0.5)) (eql 0.5) (single-float (0.5) 1.0))))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-compound.real-ranges-gap
  (list (%stc-sub '(single-float 0.0 1.0) '(or (single-float 0.0 (0.5)) (single-float (0.5) 1.0)))
        (%stc-sub 'single-float '(or (single-float (0.0)) (single-float * (0.0))))
        (%stc-sub '(single-float 0.0 1.0) '(or (single-float 0.0 0.4) (single-float 0.6 1.0)))
        (%stc-sub '(integer 0 10) '(or (integer 0 4) (real 5.5 10)))
        (%stc-sub '(rational 0 1) '(or (rational 0 1/3) (rational 1/2 1)))
        (%stc-sub '(single-float 0.0 1.0) '(double-float 0d0 1d0)))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

;; 0.0 and -0.0 are not EQL: a MEMBER of one of them leaves the other out.
(deftest subtypep-compound.real-ranges-signed-zero
  (list (%stc-sub '(single-float 0.0) '(or (single-float (0.0)) (member 0.0)))
        (%stc-sub '(double-float 0d0) '(or (double-float (0d0)) (member -0d0)))
        (%stc-sub '(single-float 0.0) '(or (single-float (0.0)) (member -0.0 0.0))))
  ((nil t) (nil t) (t t)))

;; A decimal is a real but neither a rational nor a float.
(deftest subtypep-compound.real-with-decimal
  (list (%stc-sub 'real '(or rational float))
        (%stc-sub '(real 0 1) '(or (rational 0 1) (float 0 1)))
        (%stc-sub 'number '(or rational float complex))
        (%stc-sub 'dotcl:decimal '(or rational float))
        (%stc-sub 'stc-real* '(or integer ratio float)))
  ((nil t) (nil t) (nil t) (nil t) (t t)))
