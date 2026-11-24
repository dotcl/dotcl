;;; The standard accessors have SETF functions: #'(setf car), (fdefinition '(setf nth)) ...
;;; (the new value comes first, then the arguments of the accessor).

(deftest setf-functions-of-accessors.cons-accessors
  (let ((l (list 1 2 3 4 5 6 7 8 9 10)))
    (funcall #'(setf car) 'a l)
    (funcall #'(setf cadr) 'b l)
    (funcall #'(setf caddr) 'c l)
    (funcall #'(setf tenth) 'j l)
    (funcall #'(setf cdddr) '(x) l)
    l)
  (a b c x))

(deftest setf-functions-of-accessors.first-to-tenth
  (let ((l (list 1 2 3 4 5 6 7 8 9 10)))
    (loop for f in '(first second third fourth fifth sixth seventh eighth ninth tenth)
          for i from 0
          do (funcall (fdefinition (list 'setf f)) i l))
    l)
  (0 1 2 3 4 5 6 7 8 9))

(deftest setf-functions-of-accessors.rest-nth-elt
  (let ((l (list 1 2 3)) (v (vector 1 2 3)))
    (funcall #'(setf rest) '(a) l)
    (funcall #'(setf nth) 'n 0 l)
    (funcall #'(setf elt) 'e v 1)
    (list l (coerce v 'list)))
  ((n a) (1 e 3)))

(deftest setf-functions-of-accessors.strings
  (let ((s (copy-seq "abcd")) (u (copy-seq "wxyz")))
    (funcall #'(setf char) #\Z s 0)
    (funcall #'(setf schar) #\Y u 1)
    (funcall #'(setf subseq) "12" s 2)
    (list s u))
  ("Zb12" "wYyz"))

(deftest setf-functions-of-accessors.hash-table
  (let ((h (make-hash-table)))
    (funcall (fdefinition '(setf gethash)) 5 :k h)
    (list (gethash :k h) (funcall #'(setf gethash) 6 :k h 'ignored) (gethash :k h)))
  (5 6 6))

(deftest setf-functions-of-accessors.fill-pointer
  (let ((v (make-array 4 :fill-pointer 3 :initial-element 0)))
    (funcall #'(setf fill-pointer) 1 v)
    (fill-pointer v))
  1)

(deftest setf-functions-of-accessors.mapped
  (let ((l (list 0 1 2 3)))
    (mapc #'(setf car) '(a b) (list l (cdr l)))
    l)
  (a b 2 3))

(deftest setf-functions-of-accessors.setf-forms-unchanged
  ;; The places still expand in line: no recursion through the functions.
  (let ((l (list 1 2 3)) (h (make-hash-table)))
    (setf (car l) 'x (cadr l) 'y (gethash 1 h) 'z)
    (incf (third l) 10)
    (list l (gethash 1 h)))
  ((x y 13) z))

(deftest setf-functions-of-accessors.all-bound
  (remove-if (lambda (n) (fboundp (list 'setf n)))
             '(car cdr caar cadr cdar cddr caddr cdddr cadddr cddddr first second third rest
               nth elt char schar gethash fill-pointer subseq row-major-aref aref svref get
               symbol-value symbol-function symbol-plist macro-function readtable-case))
  nil)
