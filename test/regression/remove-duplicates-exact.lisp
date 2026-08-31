;;; REMOVE-DUPLICATES costs its result and the array the scan needs.
;;;
;;; It grew a List of every element, a bool[] of every index, and a second List
;;; of the survivors, then copied that one out again -- five throwaway objects
;;; before the answer. (remove-duplicates '(1 2 3 4 5)) cost 648 bytes to hand
;;; back a five-element copy.
;;;
;;; The duplicate positions now go in a bitmap (on the stack up to 512
;;; elements), a vector or string is indexed where it stands rather than copied,
;;; and the result is built once. A list is still walked into one exact-size
;;; array, because the scan indexes arbitrarily and re-walking the list per
;;; index would make an already quadratic algorithm cubic.
;;;
;;; 648 -> 232 bytes, which is that array plus the five result conses.
;;;
;;; Every expected value here was taken from SBCL.

(deftest rdx.lists
  (list (remove-duplicates '(1 2 1 3 2))
        (remove-duplicates '(1 2 1 3 2) :from-end t)
        (remove-duplicates '())
        (remove-duplicates '(1))
        (remove-duplicates '(1 1 1))
        (remove-duplicates '(1 1 1) :from-end t)
        (remove-duplicates '(1 2 3)))
  ((1 3 2) (1 2 3) nil (1) (1) (1) (1 2 3)))

(deftest rdx.vectors-and-strings
  (list (coerce (remove-duplicates #(1 2 1 3 2)) 'list)
        (coerce (remove-duplicates #(1 2 1 3 2) :from-end t) 'list)
        (coerce (remove-duplicates #()) 'list)
        (remove-duplicates "banana")
        (remove-duplicates "banana" :from-end t)
        (remove-duplicates "")
        (coerce (remove-duplicates #*1011) 'list))
  ((1 3 2) (1 2 3) nil "bna" "ban" "" (0 1)))

(deftest rdx.bounds
  (list (remove-duplicates '(1 2 1 3 2) :start 1)
        (remove-duplicates '(1 2 1 3 2) :end 3)
        (remove-duplicates '(1 2 1 3 2) :start 1 :end 4)
        (remove-duplicates '(1 2 1 3 2) :start 1 :end 4 :from-end t))
  ((1 1 3 2) (2 1 3 2) (1 2 1 3 2) (1 2 1 3 2)))

(deftest rdx.test-and-key
  (list (remove-duplicates '("a" "b" "a") :test #'string=)
        (remove-duplicates '("a" "b" "a") :test #'string= :from-end t)
        (remove-duplicates '((1 a) (2 b) (1 c)) :key #'car)
        (remove-duplicates '((1 a) (2 b) (1 c)) :key #'car :from-end t)
        (remove-duplicates '(1 2 3) :test-not #'eql))
  (("b" "a") ("a" "b") ((2 b) (1 c)) ((1 a) (2 b)) (3)))

(deftest rdx.delete-duplicates
  (list (delete-duplicates (list 1 2 1 3 2))
        (delete-duplicates (list 1 2 1 3 2) :from-end t))
  ((1 3 2) (1 2 3)))

;;; The test is called the same number of times as before -- the bitmap replaced
;;; the bool[], it did not change which pairs are compared.
(deftest rdx.test-call-count
  (let ((n 0))
    (remove-duplicates '(1 2 3) :test (lambda (a b) (incf n) (eql a b)))
    n)
  3)

(deftest rdx.errors
  (flet ((kind (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (type-error () :type-error)
             (program-error () :program-error))))
    (list (kind (lambda () (remove-duplicates 5)))
          (kind (lambda () (remove-duplicates '(1 2) :bogus t)))))
  (:type-error :program-error))
