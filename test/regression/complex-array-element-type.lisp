;;; Regression: ARRAY-ELEMENT-TYPE of an array made with a complex element
;;; type. The stored name "COMPLEX-SINGLE-FLOAT" was turned back into a symbol
;;; as it was, which made up DOTCL-INTERNAL::COMPLEX-SINGLE-FLOAT: not a type,
;;; and not what UPGRADED-ARRAY-ELEMENT-TYPE said (T). numpy-file-format picks
;;; the on-disk format from ARRAY-ELEMENT-TYPE and found none for it.
;;;
;;; Now (complex single-float) and (complex double-float) report themselves,
;;; as in SBCL, and every other complex element type reports T. The two
;;; functions agree, and TYPEP tells the two float kinds apart.

(deftest complex-aet-single
  (array-element-type (make-array 2 :element-type '(complex single-float)))
  (complex single-float))

(deftest complex-aet-double
  (array-element-type (make-array 2 :element-type '(complex double-float)))
  (complex double-float))

(deftest complex-aet-other-is-t
  (list (array-element-type (make-array 2 :element-type '(complex rational)))
        (array-element-type (make-array 2 :element-type 'complex)))
  (t t))

(deftest complex-aet-matches-upgraded
  (loop for spec in '((complex single-float) (complex double-float)
                      (complex short-float) (complex long-float)
                      (complex rational) (complex integer) complex)
        always (equal (array-element-type (make-array 2 :element-type spec))
                      (upgraded-array-element-type spec)))
  t)

(deftest complex-aet-type-of
  (type-of (make-array 3 :element-type '(complex double-float)))
  (simple-array (complex double-float) (3)))

(deftest complex-aet-typep
  (let ((s (make-array 3 :element-type '(complex single-float)))
        (d (make-array 3 :element-type '(complex double-float)))
        (g (make-array 3)))
    (list (typep s '(simple-array (complex single-float) (3)))
          (typep s '(simple-array (complex double-float) (3)))
          (typep d '(simple-array (complex double-float) (3)))
          (typep d '(simple-array (complex single-float) (3)))
          (typep g '(simple-array (complex single-float) (3)))
          (typep g '(simple-array (complex rational) (3)))))
  (t nil t nil nil t))
