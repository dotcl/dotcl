;;; A form that the standard pprint functions break with a mandatory newline,
;;; written inside a logical block, keeps the newline. Expected values from SBCL.

(defun pnm-string (form)
  (let ((*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s nil)
        (write form :stream s)))))

(deftest pprint-nested-mandatory.let
  (pnm-string '(let ((a 1)) a))
  #.(format nil "(LET ((A 1))~%  A)"))

(deftest pprint-nested-mandatory.let-two-bindings
  (let ((*print-pretty* t))
    (format nil "abc~@<~S~:>def" '(let ((a 1) (b 2)) (+ a b x y))))
  #.(format nil "abc(LET ((A 1) (B 2))~%     (+ A B X Y))def"))

(deftest pprint-nested-mandatory.in-a-list
  (pnm-string '(foo (let ((a 1)) a)))
  #.(format nil "(FOO~% (LET ((A 1))~%   A))"))

(deftest pprint-nested-mandatory.prefix
  (let ((*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :prefix "<")
        (write '(let ((a 1)) a) :stream s))))
  #.(format nil "<(LET ((A 1))~%   A)"))
