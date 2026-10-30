;;; FIND, POSITION, COUNT and their -IF forms compare standard :TEST functions
;;; and predicates directly; everything else keeps calling through the function.

(defparameter *sst-vector* (vector 1 2 3 2 4))
(defparameter *sst-list* (list 1 2 3 2 4))

(deftest seq-standard-tests.test-functions
  (list
   (find 2 *sst-vector*)
   (find 2 *sst-list*)
   (position 2 *sst-vector*)
   (position 2 *sst-list*)
   (find 2 *sst-vector* :test #'eql)
   (find 2 *sst-list* :test #'eql)
   (find 2 *sst-vector* :test #'eq)
   (find "b" '("a" "b") :test #'equal)
   (find '(1) '((0) (1)) :test #'equal)
   (find 2 *sst-vector* :test #'=)
   (position 2 *sst-vector* :test #'=)
   (find #\b (vector #\a #\b #\c) :test #'char=)
   (position #\b "abc" :test #'char=)
   (find #\b "abc")
   (find #\c "abc" :test #'char= :from-end t))
  (2 2 1 1 2 2 2 "b" (1) 2 1 #\b 1 #\b #\c))

(deftest seq-standard-tests.predicates
  (list
   (find-if #'minusp #(1 -2 3))
   (find-if #'zerop #(1 0 2))
   (find-if #'plusp #(-1 2 3))
   (find-if #'minusp #(1 -2 -3) :from-end t)
   (position-if #'minusp #(1 -2 3))
   (position-if #'zerop #(1 0 2) :from-end t)
   (position-if #'plusp #(-1 2 3))
   (count-if #'minusp #(1 -2 3 -4))
   (find-if #'zerop '(-1 0 1))
   (position-if #'plusp '(-1 2 3))
   (count-if #'minusp '(-1 2 -3)))
  (-2 0 2 -3 1 1 1 2 0 1 2))

;;; Same names in another package are different function objects.
(defpackage #:seq-standard-tests-other
  (:use #:cl)
  (:shadow #:= #:minusp)
  (:export #:= #:minusp))

(defun seq-standard-tests-other:= (a b)
  (declare (ignore a))
  (eql b 2))

(defun seq-standard-tests-other:minusp (x)
  (eql x 2))

(deftest seq-standard-tests.other-package
  (list
   (find 0 (vector 1 2 3) :test #'seq-standard-tests-other:=)
   (position-if #'seq-standard-tests-other:minusp (vector 1 2 3))
   (find-if #'seq-standard-tests-other:minusp (list 1 2 3)))
  (2 1 2))

;;; The general path keeps its order and its answers.
(deftest seq-standard-tests.general-path
  (list
   (find 2 '((1 a) (2 b)) :key #'car :test #'=)
   (find 2 '(1 2 3) :test-not #'=)
   (find 2 '(1 2 3) :test (lambda (a b) (= a b)))
   (find-if #'minusp '((1) (-2)) :key #'car)
   (position-if #'zerop #((1) (0)) :key #'car :from-end t))
  ((2 b) 1 2 (-2) 1))

(deftest seq-standard-tests.from-end-duplicates
  (let* ((a (list 1))
         (b (list 2))
         (c (list 1)))
    (list (eq (find a (vector c b a) :test #'equal :from-end t) a)
          (eq (find a (list c b a) :test #'equal :from-end t) a)
          (position a (vector c b a) :test #'equal :from-end t)))
  (t t 2))

(deftest seq-standard-tests.errors
  (list
   (handler-case (find 1 (vector #\a #\b) :test #'char=)
     (type-error () :type-error))
   (handler-case (find #\b (vector #\a 2 #\b) :test #'char=)
     (type-error () :type-error))
   (handler-case (find 'a (vector 'a 1) :test #'=)
     (type-error () :type-error))
   (handler-case (find-if #'minusp (vector 'a))
     (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error))
