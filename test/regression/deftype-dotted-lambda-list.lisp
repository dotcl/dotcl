;;; DEFTYPE lambda lists are macro lambda lists: a dotted tail is &REST, and
;;; the &OPTIONAL parameters of a nested pattern default to * as well.

(deftype dtll-rest (c . d) `(member ,c ,@d))
(deftype dtll-nest ((a &optional b) c . d)
  (assert (listp d))
  `(member ,a ,b ,c))

(deftest deftype-dotted.rest
  (list (typep 3 '(dtll-rest 3 4)) (typep 4 '(dtll-rest 3 4)) (typep 5 '(dtll-rest 3 4)))
  (t t nil))

(deftest deftype-dotted.nested-pattern
  (list (typep 1 '(dtll-nest (1 2) 3 4 5 6)) (typep 1 '(dtll-nest (2 2) 3 4 5 6))
        (typep 3 '(dtll-nest (2) 3)))
  (t nil t))

(deftest deftype-dotted.nested-optional-defaults-to-star
  (typep '* '(dtll-nest (3) 3))
  t)
