;;; COUNT, COUNT-IF, POSITION-IF, REDUCE, RASSOC and GETF get direct entries.
;;;
;;; Each of them was registered as a plain variadic function, so (count x l)
;;; built a two-element array for the entry point to walk before any work
;;; started -- 40 bytes on every call, on functions that otherwise allocate
;;; nothing at all. MEMBER, ASSOC, FIND, POSITION and SEARCH already had these
;;; entries; this is the rest of that family.
;;;
;;; Each keyword-taking body was split into a core taking the parsed keywords,
;;; so the variadic entry and the two-argument entry share one implementation.
;;;
;;; All six went 48 -> 0 bytes per call.
;;;
;;; Every expected value here was taken from SBCL.

(defvar *sde2-l* '(1 2 3 2 4 2))
(defvar *sde2-v* #(1 2 3 2 4 2))
(defvar *sde2-al* '((a . 1) (b . 2) (c . 2)))
(defvar *sde2-pl* '(:a 1 :b 2))

(deftest sde2.count
  (list (count 2 *sde2-l*)
        (count 2 *sde2-v*)
        (count #\a "banana")
        (count 2 *sde2-l* :start 2)
        (count 2 *sde2-l* :end 3)
        (count 2 *sde2-l* :from-end t)
        (count 2 *sde2-l* :test #'/=)
        (count 1 *sde2-l* :key #'1-)
        (count 2 '()))
  (3 3 3 2 1 3 3 3 0))

(deftest sde2.count-if
  (list (count-if #'evenp *sde2-l*)
        (count-if #'evenp *sde2-v*)
        (count-if #'evenp *sde2-l* :start 1 :end 4)
        (count-if #'evenp *sde2-l* :key #'1+)
        (count-if #'evenp '()))
  (4 4 2 2 0))

(deftest sde2.position-if
  (list (position-if #'evenp *sde2-l*)
        (position-if #'evenp *sde2-v*)
        (position-if #'evenp *sde2-l* :start 2)
        (position-if #'oddp *sde2-l*)
        (position-if #'evenp '())
        (position-if #'evenp *sde2-l* :from-end t))
  (1 1 3 0 nil 5))

(deftest sde2.reduce
  (list (reduce #'+ *sde2-l*)
        (reduce #'+ *sde2-v*)
        (reduce #'- *sde2-l*)
        (reduce #'+ '())
        (reduce #'+ '(5))
        (reduce #'+ *sde2-l* :initial-value 100)
        (reduce #'- *sde2-l* :from-end t)
        (reduce #'+ *sde2-l* :start 1 :end 3)
        (reduce #'+ '((1) (2)) :key #'car))
  (14 14 -12 0 5 114 2 5 3))

(deftest sde2.rassoc
  (list (rassoc 2 *sde2-al*)
        (rassoc 9 *sde2-al*)
        (rassoc 2 *sde2-al* :test #'=)
        (rassoc 1 *sde2-al* :key #'1-)
        (rassoc 2 '()))
  ((b . 2) nil (b . 2) (b . 2) nil))

(deftest sde2.getf
  (list (getf *sde2-pl* :b)
        (getf *sde2-pl* :zz)
        (getf *sde2-pl* :zz 7)
        (getf '() :a)
        (getf '(:a 1 :a 2) :a))
  (2 nil 7 nil 1))

;;; An odd-length plist is still a type error, and an unknown keyword is still
;;; a program error -- the direct entries do not skip those checks, they only
;;; skip building the array.
(deftest sde2.errors
  (flet ((kind (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (type-error () :type-error)
             (program-error () :program-error))))
    (list (kind (lambda () (getf '(:a) :a)))
          (kind (lambda () (count 2 *sde2-l* :bogus t)))
          (kind (lambda () (rassoc 2 *sde2-al* :bogus t)))
          (kind (lambda () (reduce #'+ *sde2-l* :bogus t)))))
  (:type-error :program-error :program-error :program-error))
