;;; lib.lisp — the file whose coverage is measured. Deliberately only partly
;;; exercised by driver.lisp, so the report has to tell covered from uncovered
;;; rather than just naming the file.
(in-package :cl-user)

(defun cov-classify (n)
  (if (< n 0)
      :negative
      :non-negative))

(defun cov-sum-to (n)
  (let ((acc 0))
    (dotimes (i n)
      (incf acc i))
    acc))

;; never called
(defun cov-untouched (a b)
  (let ((sum (+ a b)))
    (format nil "untouched ~a" sum)
    (* sum sum)))

(defun cov-run ()
  (list (cov-classify -1)
        (cov-classify 7)
        (cov-sum-to 5)))
