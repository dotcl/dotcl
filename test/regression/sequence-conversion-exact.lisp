;;; COERCE, CONCATENATE and MISMATCH build one exact-size result.
;;;
;;; Each of them collected the elements into a List<> and then copied the List
;;; out. That pays for the List, its backing store (rounded up to a power of two,
;;; so a 10-element list grew through 4, 8 and 16) and the copy, all to produce
;;; a result whose size a second walk of the input would have told them for free.
;;;
;;; (coerce '(1 2 3) 'vector) cost 232 bytes where the vector itself is 144;
;;; a 10-element list cost 528 against a floor of 200. (mismatch '(1 2 3)
;;; '(1 2 3)) cost 320 bytes to answer NIL.
;;;
;;; Every expected value here was taken from SBCL.

(deftest seqconv.coerce-to-vector
  (list (coerce (coerce '(1 2 3) 'vector) 'list)
        (coerce (coerce '(1 2 3) 'simple-vector) 'list)
        (coerce (coerce '() 'vector) 'list)
        (coerce (coerce '(1 2 3) '(vector t)) 'list)
        (coerce (coerce '(1 2 3) '(simple-array t (3))) 'list))
  ((1 2 3) (1 2 3) nil (1 2 3) (1 2 3)))

(deftest seqconv.coerce-to-bit-vector
  (list (coerce (coerce '(1 0 1) 'bit-vector) 'list)
        (coerce (coerce '() 'bit-vector) 'list)
        (coerce (coerce #(1 0 1) 'simple-bit-vector) 'list)
        (bit-vector-p (coerce '(1 0 1) 'bit-vector)))
  ((1 0 1) nil (1 0 1) t))

;;; The list is consed straight from the source rather than staged in an array.
(deftest seqconv.coerce-to-list
  (list (coerce #(1 2 3) 'list)
        (coerce "abc" 'list)
        (coerce "" 'list)
        (coerce #() 'list)
        (coerce #*101 'list))
  ((1 2 3) (#\a #\b #\c) nil nil (1 0 1)))

;;; A length constraint in the type specifier is still checked.
(deftest seqconv.coerce-size-mismatch
  (handler-case (progn (coerce '(1 2 3) '(vector t 4)) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest seqconv.concatenate-list
  (list (concatenate 'list)
        (concatenate 'list '(1 2) #(3 4) "ab")
        (concatenate 'cons '(1 2) '(3))
        (concatenate 'list '() '() '(1)))
  (nil (1 2 3 4 #\a #\b) (1 2 3) (1)))

(deftest seqconv.concatenate-vector
  (list (coerce (concatenate 'vector '(1 2) #(3 4) "ab") 'list)
        (coerce (concatenate 'vector) 'list)
        (coerce (concatenate '(vector t 3) '(1 2) '(3)) 'list)
        (concatenate 'string "ab" '(#\c) #(#\d)))
  ((1 2 3 4 #\a #\b) nil (1 2 3) "abcd"))

(deftest seqconv.concatenate-size-mismatch
  (handler-case (progn (concatenate '(vector t 4) '(1 2) '(3)) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest seqconv.concatenate-not-a-sequence
  (list (handler-case (progn (concatenate 'list 5) :no-error)
          (type-error () :type-error))
        (handler-case (progn (concatenate 'vector 5) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error))

(deftest seqconv.mismatch
  (list (mismatch '(1 2 3) '(1 2 3))
        (mismatch '(1 2 3) '(1 2 4))
        (mismatch '(1 2) '(1 2 3))
        (mismatch "abc" #(#\a #\b #\z))
        (mismatch '(1 2 3) '(1 2 3) :from-end t)
        (mismatch '(1 2 3 4) '(9 2 3) :start1 1 :end1 3 :start2 1)
        (mismatch '(1 2 3) '(2 3 4) :key #'evenp)
        (mismatch '() '())
        (mismatch '(1) '()))
  (nil 2 2 2 nil nil 0 nil 0))
