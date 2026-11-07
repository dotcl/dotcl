;;; Types that are empty or universal only by their shape: (NOT (OR X T)) is NIL, a union with T is T.
;;; SUBTYPEP must treat them so, and must never answer "not a subtype, for certain" for a type that
;;; holds nothing.

(deftest subtypep-shapes.empty-below-everything
  (list (multiple-value-list (subtypep '(not (or integer t)) nil))
        (multiple-value-list (subtypep '(not (or (array * 2) t)) '(not (or (array * 2) t))))
        (multiple-value-list (subtypep '(not (or (or t number) (vector t))) 'symbol))
        (multiple-value-list (subtypep '(and (not (not t)) (not t)) 'integer)))
  ((t t) (t t) (t t) (t t)))

(deftest subtypep-shapes.universal-above-everything
  (list (multiple-value-list (subtypep 'integer '(not (not (or symbol t)))))
        (multiple-value-list (subtypep '(not nil) '(or (array * 2) t))))
  ((t t) (t t)))

;; (AND (NOT A) (NOT B)) is empty when (OR A B) is every object, so it is below (NOT (OR A B)) and below
;; NIL. SUBTYPEP may not know that, but must not answer "no, for certain".
(deftest subtypep-shapes.de-morgan-with-an-empty-side
  (let* ((a '(or (and (integer 0 *) (not list)) (and (not (integer 3 3)) (not bit))))
         (b '(not (or (not bit) symbol)))
         (x `(and (not ,a) (not ,b))))
    (flet ((never-certain-no (y)
             (multiple-value-bind (r k) (subtypep x y) (not (and (not r) k)))))
      (list (never-certain-no `(not (or ,a ,b))) (never-certain-no nil) (never-certain-no b))))
  (t t t))

;; An answer of NO is certain only for a type that holds something.
(deftest subtypep-shapes.no-is-still-certain-for-real-types
  (list (multiple-value-list (subtypep 'integer '(not number)))
        (multiple-value-list (subtypep '(and integer (not (eql 3))) 'symbol))
        (multiple-value-list (subtypep 'cons '(not list))))
  ((nil t) (nil t) (nil t)))
