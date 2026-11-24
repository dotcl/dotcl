;;; (make-array n) with no :initial-element leaves every element NIL; SBCL fills
;;; with 0 (the contents are undefined, CLHS 15.1.2.2). This pins dotcl's choice
;;; listed in docs/deviations.md.

(deftest make-array-default-element.vector
  (mapcar (lambda (v) (coerce v 'list))
          (list (make-array 3)
                (make-array 3 :adjustable t)
                (make-array 2 :element-type t)
                (make-array 0)))
  ((nil nil nil) (nil nil nil) (nil nil) nil))

(deftest make-array-default-element.rank-2
  (let ((a (make-array '(2 2))))
    (list (aref a 0 0) (aref a 1 1)))
  (nil nil))

(deftest make-array-default-element.typed-stays-zero
  (list (aref (make-array 2 :element-type 'double-float) 0)
        (aref (make-array 2 :element-type '(unsigned-byte 8)) 1)
        (char-code (aref (make-array 1 :element-type 'character) 0)))
  (0.0d0 0 0))
