;;; asdf-version-range.lisp -- version ranges in ASDF :version dependencies.
;;;
;;;   (:version "foo" "1.2")                  at least 1.2 (ASDF's own meaning)
;;;   (:version "foo" (:^ "1.2"))             at least 1.2, below 2
;;;   (:version "foo" (:>= "1.2" :< "1.5"))   every pair must hold
;;;
;;; Operators: :^ :>= :> :<= :< :=. (:^ V) keeps the leading components of V
;;; up to its first nonzero one: ^1.2 is below 2, ^0.3.1 below 0.4, ^0.0.4
;;; below 0.0.5. Versions compare with missing components as 0 ("2" = "2.0").
;;;
;;; An ASDF without this file accepts such a dependency and does not check the
;;; version. Loads on any Lisp with ASDF.

(require "asdf")

(defpackage :asdf-version-range
  (:use :cl)
  (:export #:parse-version* #:version-compare #:range-satisfies-p
           #:check-range #:compatible-key #:compatible-key-string))

(in-package :asdf-version-range)

(defun parse-version* (version)
  "VERSION as a list of integers, or NIL when it is not dot-separated naturals."
  (and (stringp version)
       (uiop:parse-version version nil)))

(defun version-compare (a b)
  "-1, 0 or 1 as version string A is below, equal to or above B."
  (loop for x = (parse-version* a) then (rest x)
        for y = (parse-version* b) then (rest y)
        while (or x y)
        do (let ((m (or (first x) 0)) (n (or (first y) 0)))
             (cond ((< m n) (return -1))
                   ((> m n) (return 1))))
        finally (return 0)))

(defun compatible-key (version)
  "The leading components of VERSION that a caret range keeps fixed."
  (let ((parts (parse-version* version)))
    (loop for (p . more) on parts
          collect p
          until (or (plusp p) (null more)))))

(defun compatible-key-string (version)
  (format nil "~{~D~^.~}" (compatible-key version)))

(defun caret-bound (version)
  (let ((key (compatible-key version)))
    (format nil "~{~D~^.~}" (append (butlast key) (list (1+ (car (last key))))))))

(defun check-range (range)
  "Signal an error unless RANGE is a version string or an operator list."
  (cond ((stringp range) range)
        ((and (consp range) (evenp (length range))
              (loop for (op v) on range by #'cddr
                    always (and (member op '(:^ :>= :> :<= :< :=))
                                (parse-version* v))))
         range)
        (t (error "Invalid version range ~S" range))))

(defun range-satisfies-p (version range)
  "True when version string VERSION is in RANGE: a string (at least that
version, as ASDF reads it), or a list of operator and version pairs."
  (check-range range)
  (and (parse-version* version)
       (if (stringp range)
           (>= (version-compare version range) 0)
           (loop for (op v) on range by #'cddr
                 for c = (version-compare version v)
                 always (ecase op
                          (:>= (>= c 0))
                          (:> (> c 0))
                          (:<= (<= c 0))
                          (:< (< c 0))
                          (:= (= c 0))
                          (:^ (and (>= c 0)
                                   (< (version-compare version (caret-bound v)) 0))))))))

(defmethod asdf:version-satisfies ((cver string) (version cons))
  (range-satisfies-p cver version))
