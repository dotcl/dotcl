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

;; T is a value of the upgrading too, not only the specialized element types:
;; (ARRAY INTEGER ...) and (ARRAY T ...) both name the general array type, so
;; neither is narrower than the other (CLHS 15.1.2.1).
(deftest subtypep-upgraded-array-element.t-upgrade
  (list (multiple-value-list (subtypep '(vector t *) '(vector integer)))
        (multiple-value-list (subtypep '(vector integer) '(vector t *)))
        (multiple-value-list (subtypep '(array integer (2 3)) '(array t)))
        (multiple-value-list (subtypep '(vector t 3) '(vector integer)))
        (multiple-value-list (subtypep 'simple-vector '(vector integer))))
  ((t t) (t t) (t t) (t t) (t t)))

;; Different upgraded element types (and different ranks) are disjoint.
(deftest subtypep-upgraded-array-element.different-upgrade-disjoint
  (list (multiple-value-list (subtypep '(array fixnum) '(array integer)))
        (multiple-value-list (subtypep '(array character) '(array base-char)))
        (multiple-value-list (subtypep '(array t (2 3)) '(vector t))))
  ((nil t) (nil t) (nil t)))
