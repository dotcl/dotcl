;;; (COMPLEX X) holds the complexes whose parts are of the upgraded complex part
;;; type of X (CLHS 12.1.5.3). Rational parts upgrade to RATIONAL; float parts do
;;; not upgrade. TYPEP, SUBTYPEP and UPGRADED-COMPLEX-PART-TYPE agree on that.
;;; Expected values are SBCL's, except for part types holding only some floats of
;;; a format (complex-part-type.float-range), which SBCL upgrades to the format.

(defun %cpt-sub (a b) (multiple-value-list (subtypep a b)))

(deftest complex-part-type.upgraded
  (mapcar #'upgraded-complex-part-type
          '(integer rational ratio fixnum (integer 0 10) float single-float double-float
            short-float long-float real nil))
  (rational rational rational rational rational float single-float double-float
   single-float double-float real nil))

(deftest complex-part-type.upgraded-union
  (list (subtypep (upgraded-complex-part-type '(or integer single-float))
                  '(or rational single-float))
        (subtypep '(or rational single-float)
                  (upgraded-complex-part-type '(or integer single-float))))
  (t t))

(deftest complex-part-type.typep
  (mapcar (lambda (z)
            (mapcar (lambda (ty) (typep z ty))
                    '(complex (complex rational) (complex integer) (complex ratio)
                      (complex (integer 0 10)) (complex float) (complex single-float)
                      (complex double-float) (complex real))))
          (list #c(1 2) #c(1/2 2) #c(1.5 2.5) #c(1d0 2d0) #c(1.5 0.0)))
  ((t t t t t nil nil nil t)
   (t t t t t nil nil nil t)
   (t nil nil nil nil t t nil t)
   (t nil nil nil nil t nil t t)
   (t nil nil nil nil t t nil t)))

(deftest complex-part-type.subtypep-pairs
  (list (%cpt-sub '(complex float) '(complex rational))
        (%cpt-sub '(complex rational) '(complex float))
        (%cpt-sub '(complex rational) '(complex integer))
        (%cpt-sub '(complex integer) '(complex rational))
        (%cpt-sub '(complex single-float) '(complex float))
        (%cpt-sub '(complex float) '(complex single-float))
        (%cpt-sub '(complex single-float) '(complex double-float))
        (%cpt-sub 'complex '(complex real))
        (%cpt-sub '(complex real) 'complex)
        (%cpt-sub 'complex '(complex rational))
        (%cpt-sub '(complex rational) 'number)
        (%cpt-sub '(complex rational) 'real)
        (%cpt-sub 'number '(complex rational))
        (%cpt-sub 'real '(complex float)))
  ((nil t) (nil t) (t t) (t t) (t t) (nil t) (nil t)
   (t t) (t t) (nil t) (t t) (nil t) (nil t) (nil t)))

(deftest complex-part-type.subtypep-unions
  (list (%cpt-sub 'number '(or real (complex rational)))
        (%cpt-sub 'number '(or real (complex float)))
        (%cpt-sub 'number '(or real complex))
        (%cpt-sub 'complex '(or (complex rational) (complex float)))
        (%cpt-sub 'complex '(or (complex rational) (complex single-float) (complex double-float)))
        (%cpt-sub 'complex '(or (complex rational) (complex single-float)))
        (%cpt-sub '(complex float) '(or (complex single-float) (complex double-float)))
        (%cpt-sub '(complex real) '(or (complex rational) (complex float)))
        (%cpt-sub '(complex rational) '(not (complex float)))
        (%cpt-sub '(and (complex rational) (complex float)) nil)
        (%cpt-sub '(and integer ratio) nil))
  ((nil t) (nil t) (t t) (t t) (t t) (nil t) (t t) (t t) (t t) (t t) (t t)))

;; TYPEP of (COMPLEX (OR ...)) returns one value, not the two of a SUBTYPEP
;; used to upgrade the part type (ANSI MISC.580).
(deftest complex-part-type.typep-one-value
  (multiple-value-list (typep #c(1 2) '(complex (or ratio fixnum))))
  (t))

;; A part type holding only some floats of a format is its own upgraded part
;; type, so (COMPLEX T1) <: (COMPLEX T2) follows T1 <: T2 among float ranges and
;; (COMPLEX SINGLE-FLOAT) is not a subtype of (COMPLEX (FLOAT 0.0 1.0)) (ANSI
;; SUBTYPEP-COMPLEX.8). SBCL upgrades these to FLOAT / SINGLE-FLOAT instead.
(deftest complex-part-type.float-range
  (list (upgraded-complex-part-type '(float 0.0 1.0))
        (upgraded-complex-part-type '(single-float 0.0 1.0))
        (typep #c(0.5 0.5) '(complex (float 0.0 1.0)))
        (typep #c(0.5d0 0.5d0) '(complex (float 0.0 1.0)))
        (typep #c(5.0 0.5) '(complex (float 0.0 1.0)))
        (%cpt-sub '(complex single-float) '(complex (float 0.0 1.0)))
        (%cpt-sub '(complex (float 0.0 1.0)) '(complex float))
        (%cpt-sub '(complex (float 0.0 1.0)) '(complex (float 0.0 2.0)))
        (%cpt-sub '(complex (float 0.0 2.0)) '(complex (float 0.0 1.0)))
        (%cpt-sub '(complex (single-float 0.0 1.0)) '(complex (float 0.0 3.0)))
        (%cpt-sub '(complex (float 0.0 1.0)) '(complex rational))
        (%cpt-sub '(complex float) '(complex (single-float 0.0 1.0))))
  ((float 0.0 1.0) (single-float 0.0 1.0) t t nil
   (nil t) (t t) (t t) (nil t) (t t) (nil t) (nil t)))
