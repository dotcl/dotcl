;;; The core object types are sealed so `x is Cons` and friends compile to one
;;; method-table compare instead of a CastHelpers call. Sealing changes nothing a
;;; program can observe -- unless something actually derived from them, which is
;;; what this file guards: every sealed type still answers TYPE-OF / TYPEP /
;;; CLASS-OF the way it did, and the subtype relations still hold.

(deftest sealed-types.type-of
  (mapcar #'type-of (list (cons 1 2) 1 (expt 2 100) 1.5d0 1.5f0 1/2 #c(1 2)
                          'sym :kw "s" #\a nil t))
  (cons (integer 1 1) (integer 1267650600228229401496703205376 1267650600228229401496703205376)
        double-float single-float ratio complex
        symbol keyword simple-base-string standard-char null boolean))

(deftest sealed-types.typep-hierarchy
  (list (typep 1 'number) (typep 1 'integer) (typep 1 'fixnum) (typep 1 'atom)
        (typep (expt 2 100) 'integer) (typep 1/2 'rational) (typep 1.5d0 'float)
        (typep #c(1 2) 'number) (typep "s" 'string) (typep "s" 'vector)
        (typep "s" 'sequence) (typep #\a 'character) (typep '(1) 'list)
        (typep '(1) 'sequence) (typep #(1) 'vector) (typep nil 'null)
        (typep nil 'symbol) (typep nil 'list) (typep t 'symbol))
  (t t t t t t t t t t t t t t t t t t t))

(deftest sealed-types.class-of-names
  (mapcar (lambda (x) (class-name (class-of x)))
          (list (cons 1 2) 1 1.5d0 'sym "s" #\a nil t #(1) (make-hash-table)))
  ;; The names are dotcl's current CLASS-OF granularity (INTEGER rather than
  ;; FIXNUM, SYMBOL for T, VECTOR for a simple vector) -- pinned here because the
  ;; point is that sealing changed nothing, not that these are the finest names.
  (cons integer double-float symbol string character null symbol vector hash-table))

(defstruct sealed-s a)
(defclass sealed-c () ((a :initarg :a)))

(deftest sealed-types.struct-and-instance
  (let ((s (make-sealed-s :a 1)) (c (make-instance 'sealed-c :a 2)))
    (list (type-of s) (typep s 'sealed-s) (sealed-s-a s)
          (class-name (class-of c)) (typep c 'sealed-c) (slot-value c 'a)))
  (sealed-s t 1 sealed-c t 2))

(deftest sealed-types.multiple-values-still-work
  ;; MvReturn is one of the sealed types and it is the value-passing marker.
  (list (multiple-value-list (values 1 2 3))
        (multiple-value-list (floor 17 5))
        (multiple-value-bind (a b) (values 1 2) (list a b))
        (multiple-value-list (values)))
  ((1 2 3) (3 2) (1 2) nil))

(deftest sealed-types.dotnet-objects-still-wrap
  ;; LispObject itself stays open (interop wrappers derive from it).
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "x")
    (dotnet:invoke sb "ToString"))
  "x")

(deftest sealed-types.eq-and-identity-of-immediates
  (list (eq nil nil) (eq t t) (eq 'a 'a) (eql 1 1) (eql #\a #\a)
        (eq (car '(1 2)) (car '(1 2))))
  (t t t t t t))
