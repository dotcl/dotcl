;;; The standard pprint dispatch table applies to objects written inside an
;;; active logical block too (CLHS 22.2.1.4). Expected values are SBCL 2.6.8's.

(deftest pprint-standard-dispatch-in-logical-block.list
  (let ((*print-right-margin* 20) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s nil)
        (write '(alpha beta gamma delta) :stream s))))
  #.(format nil "(ALPHA BETA GAMMA~% DELTA)"))

(deftest pprint-standard-dispatch-in-logical-block.format
  (let ((*print-right-margin* 20) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~@<~a~:>" '(alpha beta gamma delta)))
  #.(format nil "(ALPHA BETA GAMMA~% DELTA)"))

(deftest pprint-standard-dispatch-in-logical-block.vector
  (let ((*print-right-margin* 20) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s nil)
        (write #(alpha beta gamma delta) :stream s))))
  #.(format nil "#(ALPHA BETA GAMMA~%  DELTA)"))

(deftest pprint-standard-dispatch-in-logical-block.nested-list
  (let ((*print-right-margin* 20) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s nil)
        (write '(alpha (beta gamma delta epsilon) zeta) :stream s))))
  #.(format nil "(ALPHA~% (BETA GAMMA DELTA~%  EPSILON)~% ZETA)"))
