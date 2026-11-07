;;; INTERN wants a string name, and INTERN / FIND-PACKAGE a package designator.

(deftest intern-non-string-name
  (handler-case (intern 1) (type-error (e) (type-error-expected-type e)))
  string)

(deftest intern-non-string-name.symbol
  (handler-case (intern 'abc) (type-error () :type-error))
  :type-error)

(deftest intern-bad-package-designator
  (handler-case (intern "A" 5) (type-error (e) (type-error-datum e)))
  5)

(deftest find-package-bad-designator
  (handler-case (find-package 5) (type-error (e) (type-error-datum e)))
  5)

(deftest find-package-designators
  (let ((cl (find-package "CL")))
    (list (eq cl (find-package :cl)) (eq cl (find-package cl))
          (eq cl (find-package #\C)) ))
  (t t nil))
