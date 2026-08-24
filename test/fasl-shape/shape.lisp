;;; Fixture for the fasl shape guard (scripts/fasl-shape-check.sh).
;;;
;;; Every top level form here must become its own method. CLHS 3.2.3.1 says the
;;; bodies of PROGN, EVAL-WHEN, LOCALLY, MACROLET and SYMBOL-MACROLET are top
;;; level forms too, and a fasl emits one method per top level form. Miss one of
;;; those wrappers and its whole body collapses into a single method: total IL,
;;; fasl bytes and compile time all stay about the same, so nothing on the
;;; compile side notices, and the cost lands at LOAD as JIT time and resident
;;; memory that grow superlinearly with the size of that one method.
;;;
;;; That has happened once per wrapper, found each time by a user running out of
;;; memory. The guard checks the largest method in this file's fasl against a
;;; threshold that one collapsed wrapper would blow through, so the next one is
;;; found here instead.
;;;
;;; Keep the body forms cheap and numerous: it is the count that separates a
;;; split fasl from a collapsed one, not what each form does.

(defpackage :dotcl-fasl-shape
  (:use :cl))

(in-package :dotcl-fasl-shape)

;;; --- PROGN -----------------------------------------------------------------

#.(cons 'progn
        (loop for i from 0 below 400
              collect `(defparameter ,(intern (format nil "*PROGN-~D*" i)) ,i)))

;;; --- EVAL-WHEN -------------------------------------------------------------

#.(cons 'eval-when
        (cons '(:compile-toplevel :load-toplevel :execute)
              (loop for i from 0 below 400
                    collect `(defparameter ,(intern (format nil "*EVAL-WHEN-~D*" i)) ,i))))

;;; --- LOCALLY ---------------------------------------------------------------

#.(cons 'locally
        (cons '(declare (optimize (speed 1)))
              (loop for i from 0 below 400
                    collect `(defparameter ,(intern (format nil "*LOCALLY-~D*" i)) ,i))))

;;; --- MACROLET --------------------------------------------------------------

#.(cons 'macrolet
        (cons '((%shape-id (x) x))
              (loop for i from 0 below 400
                    collect `(defparameter ,(intern (format nil "*MACROLET-~D*" i))
                               (%shape-id ,i)))))

;;; --- SYMBOL-MACROLET -------------------------------------------------------

#.(cons 'symbol-macrolet
        (cons '((%shape-base 1000))
              (loop for i from 0 below 400
                    collect `(defparameter ,(intern (format nil "*SYMBOL-MACROLET-~D*" i))
                               (+ %shape-base ,i)))))

;;; A load-time smoke check: the values have to survive the splitting, and the
;;; last form of each wrapper has to have run at all.
(defun shape-fixture-ok-p ()
  (and (= (symbol-value (intern "*PROGN-399*" :dotcl-fasl-shape)) 399)
       (= (symbol-value (intern "*EVAL-WHEN-399*" :dotcl-fasl-shape)) 399)
       (= (symbol-value (intern "*LOCALLY-399*" :dotcl-fasl-shape)) 399)
       (= (symbol-value (intern "*MACROLET-399*" :dotcl-fasl-shape)) 399)
       (= (symbol-value (intern "*SYMBOL-MACROLET-399*" :dotcl-fasl-shape)) 1399)))
