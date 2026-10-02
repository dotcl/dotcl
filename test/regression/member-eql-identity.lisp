;;; MEMBER with the default EQL test compares by identity when the item is not
;;; a number or a character. The cases around that shortcut: numbers and
;;; characters still compare by value, T and NIL still match however they were
;;; produced, and a dotted list is still an error when the item is not found.

(defun %mei-member (x l) (member x l))
(defun %mei-member-eql (x l) (member x l :test #'eql))

(deftest member-eql-identity.symbols
  (list (%mei-member 'c '(a b c d))
        (%mei-member 'z '(a b c d))
        (%mei-member-eql 'b '(a b c))
        (let ((s (list 1 2))) (%mei-member s (list '(1 2) s 3))))
  ((c d) nil (b c) ((1 2) 3)))

(deftest member-eql-identity.values
  (list (%mei-member 3 '(1 2 3 4))
        (%mei-member (expt 2 70) (list 1 (expt 2 70)))
        (%mei-member 1.5 '(1.5d0 1.5))
        (%mei-member #\a '(#\b #\a))
        (%mei-member-eql 2/3 '(1/3 2/3)))
  ((3 4) (1180591620717411303424) (1.5) (#\a) (2/3)))

(deftest member-eql-identity.t-and-nil
  (list (%mei-member t '(nil t))
        (%mei-member (not nil) '(a t))
        (%mei-member nil '(a nil b))
        (%mei-member (eq 'a 'b) '(a nil))
        (%mei-member 'nil (list 'a (car '(nil)))))
  ((t) (t) (nil b) (nil) (nil)))

(deftest member-eql-identity.multiple-values
  (list (%mei-member (values 'b 'x) '(a b c))
        (%mei-member (floor 7 2) '(1 3 5)))
  ((b c) (3 5)))

(deftest member-eql-identity.dotted
  (list (handler-case (%mei-member 'z '(a b . c)) (type-error () :type-error))
        (%mei-member 'a '(a b . c)))
  (:type-error (a b . c)))
