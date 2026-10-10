;;; Two array types whose element types upgrade to the same specialized element
;;; type are the same type (CLHS 15.1.2.1): (integer 0 100) and
;;; (unsigned-byte 7) both make (unsigned-byte 7) arrays. SUBTYPEP compared the
;;; element types themselves and answered NIL, while TYPEP accepted the same
;;; arrays for both. SBCL answers T.

(deftest subtypep-upgraded-array-element.same-upgrade
  (list (multiple-value-list
         (subtypep '(simple-array (integer 0 100) (*)) '(simple-array (unsigned-byte 7) (*))))
        (multiple-value-list
         (subtypep '(simple-array (unsigned-byte 7) (*)) '(simple-array (integer 0 100) (*))))
        (multiple-value-list
         (subtypep '(vector (integer -100 100)) '(vector (signed-byte 8))))
        (multiple-value-list
         (subtypep '(array (eql 5) (2 2)) '(array (unsigned-byte 3) (2 2)))))
  ((t t) (t t) (t t) (t t)))

(deftest subtypep-upgraded-array-element.different-upgrade
  (list (multiple-value-list
         (subtypep '(simple-array (integer 0 100) (*)) '(simple-array (unsigned-byte 8) (*))))
        (multiple-value-list
         (subtypep '(vector (integer 0 100)) '(vector (integer 0 200))))
        (multiple-value-list
         (subtypep '(vector (integer 0 100)) '(vector t))))
  ((nil t) (nil t) (nil t)))
