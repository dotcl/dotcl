;;; MAP walks one sequence directly, and MEMBER/ASSOC/ADJOIN parse a keyword
;;; pair without an array.
;;;
;;; MAP converted every sequence to a list and then, per element, built an
;;; argument list, reversed it, APPLYed it and rebuilt the whole list of
;;; cursors -- several conses and an argument array to pass one argument.
;;; Nearly every call passes one sequence.
;;;
;;; The keyword pair of (member x l :test #'string=) reaches the direct entry
;;; in registers, and was put into a 2-element array so the shared parser could
;;; walk it. That array was the entire cost of the call, on the shape the
;;; standard library uses for every dispatch on a name.
;;;
;;; (map 'list #'1+ l5) 1808 -> 696 bytes; (member x l :test #'string=) 40 -> 0;
;;; (%map-rt-category 'list), which is four such MEMBERs, 552 -> 392.
;;;
;;; Every expected value here was taken from SBCL.

(deftest maplk.map-one-sequence
  (list (map 'list #'1+ '(1 2 3))
        (map 'list #'1+ #(1 2 3))
        (map 'list #'char-upcase "abc")
        (coerce (map 'vector #'1+ '(1 2 3)) 'list)
        (map 'string #'char-upcase "abc")
        (map 'string #'identity '(#\a #\b))
        (map nil #'identity '(1 2 3))
        (map 'list #'identity #*101))
  ((2 3 4) (2 3 4) (#\A #\B #\C) (2 3 4) "ABC" "ab" nil (1 0 1)))

(deftest maplk.map-empty
  (list (map 'list #'1+ '()) (map 'list #'1+ #()) (map 'list #'1+ ""))
  (nil nil nil))

;;; Several sequences still go through the parallel walk, and stop at the
;;; shortest one.
(deftest maplk.map-many-sequences
  (list (map 'list #'+ '(1 2 3) '(10 20 30))
        (map 'list #'+ '(1 2 3) '(10 20))
        (map 'list #'list '(1 2) #(3 4) "ab"))
  ((11 22 33) (11 22) ((1 3 #\a) (2 4 #\b))))

(deftest maplk.map-result-type
  (list (coerce (map 'bit-vector #'identity '(1 0 1)) 'list)
        (coerce (map '(vector t 3) #'1+ '(1 2 3)) 'list)
        (map 'cons #'1+ '(1 2))
        (handler-case (progn (map '(vector t 4) #'1+ '(1 2 3)) :no-error)
          (type-error () :type-error))
        (handler-case (progn (map 'list #'1+ 5) :no-error)
          (type-error () :type-error)))
  ((1 0 1) (2 3 4) (2 3) :type-error :type-error))

;;; The function is called once per element, left to right.
(deftest maplk.map-call-order
  (let ((log '()))
    (map nil (lambda (x) (push x log)) '(1 2 3))
    (reverse log))
  (1 2 3))

(deftest maplk.member-test-and-key
  (list (member 3 '(1 2 3 4))
        (member "c" '("a" "b" "c") :test #'string=)
        (member 3 '(1 2 3) :test #'eql)
        (member 3 '(1 2 3) :test #'eq)
        (member 3 '(1 2 3) :test-not #'eql)
        (member 3 '((1) (3)) :key #'car)
        (member 3 '(1 2 3) :key nil))
  ((3 4) ("c") (3) (3) (1 2 3) ((3)) (3)))

;;; A repeated keyword takes its first value.
(deftest maplk.repeated-keyword-first-wins
  (list (member 3 '(1 2 3) :test #'eql :test #'/=)
        (member 3 '(1 2 3) :key #'identity :key #'1+))
  ((3) (3)))

(deftest maplk.unknown-keyword
  (list (handler-case (progn (member 3 '(1 2 3) :bogus t) :no-error)
          (program-error () :program-error))
        (member 3 '(1 2 3) :bogus t :allow-other-keys t)
        (member 3 '(1 2 3) :allow-other-keys t :bogus t)
        (handler-case (progn (member 3 '(1 2 3) :allow-other-keys nil :bogus t) :no-error)
          (program-error () :program-error))
        (member 3 '(1 2 3) :allow-other-keys t)
        (handler-case (progn (member 3 '(1 2 3) 5 6) :no-error)
          (program-error () :program-error))
        (handler-case (progn (member 3 '(1 2 3) :test) :no-error)
          (program-error () :program-error)))
  (:program-error (3) (3) :program-error (3) :program-error :program-error))

(deftest maplk.assoc-and-adjoin
  (list (assoc 'b '((a . 1) (b . 2)))
        (assoc "b" '(("a" . 1) ("b" . 2)) :test #'string=)
        (assoc 2 '((1 . a) (2 . b)) :key #'1+)
        (adjoin 3 '(1 2))
        (adjoin 3 '(1 2 3))
        (adjoin "c" '("a" "c") :test #'string=)
        (handler-case (progn (adjoin 3 '((3)) :key #'car) :no-error)
          (type-error () :type-error))
        (adjoin 3 '(1 2) :bogus t :allow-other-keys t))
  ((b . 2) ("b" . 2) (1 . a) (3 1 2) (1 2 3) ("a" "c") :type-error (3 1 2)))

;;; Two sequences walk with plain cursors.
;;;
;;; The parallel path built an argument list per element, reversed it, APPLYed
;;; it, and rebuilt the whole list of cursors with (mapcar #'cdr seq-lists) --
;;; two conses and an argument array per element to pass two arguments.
;;; (map 'list #'+ l5 l5) cost 2016 bytes; it now costs 352.
;;;
;;; Three or more sequences still take the general path, but SEQ-LISTS is built
;;; here so the cursors are advanced in place instead of rebuilt.
;;;
;;; Every expected value here was taken from SBCL.

(deftest maplk.map-two-sequences
  (list (map 'list #'+ '(1 2) #(10 20))
        (map 'list #'+ #(1 2) '(10 20))
        (map 'list #'cons "ab" '(1 2))
        (map 'list #'+ '(1 2) '())
        (coerce (map 'vector #'+ '(1 2) '(3 4)) 'list)
        (map nil #'+ '(1 2) '(3 4)))
  ((11 22) (11 22) ((#\a . 1) (#\b . 2)) nil (4 6) nil))

(deftest maplk.map-three-sequences
  (list (map 'list #'list '(1 2) '(3 4) '(5 6))
        (map 'list #'list '(1 2 3) '(3 4) '(5 6 7))
        (map 'list #'list '() '() '()))
  (((1 3 5) (2 4 6)) ((1 3 5) (2 4 6)) nil))

;;; The walk stops at the shortest sequence, and the function sees the elements
;;; paired left to right.
(deftest maplk.map-two-sequences-call-order
  (let ((log '()))
    (map nil (lambda (a b) (push (list a b) log)) '(1 2 3) '(4 5))
    (reverse log))
  ((1 4) (2 5)))
