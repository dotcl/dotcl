;;; An array element type that is not specialized upgrades to T.
;;;
;;; CLHS 15.1.2.1: (SIMPLE-ARRAY X *) denotes the arrays whose element type is
;;; the UPGRADED array element type of X. An implementation specializes on a few
;;; element types and upgrades everything else to T, so a structure type, a
;;; symbol, a cons -- anything with no specialized representation -- names the
;;; same set as T does, and an ordinary vector belongs to it. The element type
;;; says what the array may hold, not what happens to be in it.
;;;
;;; TYPEP said no. That was invisible until a slot's declared :TYPE started
;;; being checked at the store: three libraries then failed to load with
;;; "slot STATE is declared (SIMPLE-ARRAY LOGGER-STATE *), got #(...)" on a
;;; vector that does satisfy the declaration.

(defstruct aet-elem (n 0))

;;; ---- an element type with no specialization ----

(deftest array-element-type-upgrading.unspecialized-upgrades-to-t
  (list (typep (vector (make-aet-elem)) '(simple-array aet-elem (*)))
        (typep (vector (make-aet-elem)) '(simple-array aet-elem *))
        (typep (vector 1 2 3) '(array symbol (*)))
        (typep (vector 1 2 3) '(simple-array cons (*))))
  (t t t t))

;; The contents are not what is being asked about: a T vector satisfies the
;; declaration whatever is in it. (This is what makes the check cheap, and it is
;; what SBCL answers too.)
(deftest array-element-type-upgrading.contents-do-not-matter
  (typep (vector 1 "two" :three) '(simple-array aet-elem (*)))
  t)

;;; ---- element types that ARE specialized still discriminate ----

(deftest array-element-type-upgrading.specialized-still-discriminates
  (list (typep (vector 1 2 3) '(simple-array fixnum (*)))
        (typep (vector 1 2 3) '(simple-array character (*)))
        (typep (vector 1 2 3) '(simple-array double-float (*)))
        (typep (vector 1 2 3) '(simple-array (unsigned-byte 8) (*)))
        (typep "abc" '(simple-array aet-elem (*))))
  (nil nil nil nil nil))

(deftest array-element-type-upgrading.specialized-still-match
  (list (typep (make-array 3 :element-type 'fixnum) '(simple-array fixnum (*)))
        (typep "abc" '(simple-array character (*)))
        (typep (make-array 3 :element-type 'double-float)
               '(simple-array double-float (*)))
        (typep (vector 1 2 3) '(simple-array t (*))))
  (t t t t))

;; NIL upgrades to NIL, not to T -- it is not one of the types this is about.
(deftest array-element-type-upgrading.nil-element-type-unchanged
  (typep (vector 1 2 3) '(simple-array nil (*)))
  nil)

;; The rank and dimension halves of the specifier are untouched by any of this.
(deftest array-element-type-upgrading.dimensions-still-checked
  (list (typep (vector 1 2 3) '(simple-array aet-elem (3)))
        (typep (vector 1 2 3) '(simple-array aet-elem (4)))
        (typep (make-array '(2 2)) '(simple-array aet-elem (* *)))
        (typep (make-array '(2 2)) '(simple-array aet-elem (*))))
  (t nil t nil))

;;; ---- the path that made this visible ----

(defstruct aet-holder
  (items (vector) :type (simple-array aet-elem (*))))

;; A slot declared to hold such an array takes an ordinary vector, because that
;; vector is of the declared type. This is the shape three libraries load
;; through.
(deftest array-element-type-upgrading.slot-type-accepts-a-vector
  (let ((h (make-aet-holder :items (vector (make-aet-elem :n 1)))))
    (setf (aet-holder-items h) (vector (make-aet-elem :n 2) (make-aet-elem :n 3)))
    (list (length (aet-holder-items h))
          (aet-elem-n (aref (aet-holder-items h) 0))))
  (2 2))

;; A slot declared with a specialized element type still refuses the wrong
;; storage -- the fix widens one case, it does not turn the check off.
;;
;; Through SETF rather than the constructor: a keyword constructor does not
;; check slot types at all here (a plain FIXNUM slot takes a string through one
;; as well), which is a separate gap and not what this file is about.
(defstruct aet-typed
  (nums (make-array 0 :element-type 'double-float)
        :type (simple-array double-float (*))))

(deftest array-element-type-upgrading.specialized-slot-still-checked
  (let ((h (make-aet-typed)))
    (handler-case (setf (aet-typed-nums h) (vector 1 2 3))
      (type-error () :type-error)))
  :type-error)
