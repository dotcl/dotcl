;;; DOTCL-FLOAT:FLOAT-NAN-P and FLOAT-INFINITY-P, as in sb-ext. Libraries
;;; (plexippus-xpath) called System.Double.IsNaN / IsInfinity through dotnet:
;;; for want of them.

(require "dotcl-float")

(deftest dotcl-float-predicates.nan
  (mapcar #'dotcl-float:float-nan-p
          (list dotcl-float:+double-float-nan+
                dotcl-float:+single-float-nan+
                (dotcl-float:bits-double-float #x7FF0000000000001) ; signalling
                (dotcl-float:bits-double-float #xFFF8000000000000) ; negative
                dotcl-float:+double-float-positive-infinity+
                dotcl-float:+single-float-negative-infinity+
                0d0 -0.0 1.5d0 most-positive-double-float least-positive-single-float))
  (t t t t nil nil nil nil nil nil nil))

(deftest dotcl-float-predicates.infinity
  (mapcar #'dotcl-float:float-infinity-p
          (list dotcl-float:+double-float-positive-infinity+
                dotcl-float:+double-float-negative-infinity+
                dotcl-float:+single-float-positive-infinity+
                dotcl-float:+single-float-negative-infinity+
                dotcl-float:+double-float-nan+
                dotcl-float:+single-float-nan+
                0d0 most-positive-double-float most-negative-single-float))
  (t t t t nil nil nil nil nil))

(deftest dotcl-float-predicates.not-a-float
  (list (handler-case (dotcl-float:float-nan-p 1) (type-error () :type-error))
        (handler-case (dotcl-float:float-infinity-p 1/2) (type-error () :type-error)))
  (:type-error :type-error))
