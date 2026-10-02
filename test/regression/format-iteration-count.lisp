;;; FORMAT ~n{...~} repeats the body n times when the body consumes no
;;; argument, as long as the argument list is not used up.
;;;
;;; The iteration stopped after the first pass whenever a pass consumed nothing,
;;; which guards against looping forever when there is no count, but also cut
;;; short a loop that has one. osicat builds an over-long path with
;;; (format nil "~v{/A~}" 2049 '(x)) and got "/A" instead of 4098 characters.
;;; Expected values are SBCL's.

(deftest format-iteration-count.plain
  (format nil "~3{/A~}" '(x))
  "/A/A/A")

(deftest format-iteration-count.v-parameter
  (length (format nil "~v{/A~}" 2049 '(x)))
  4098)

(deftest format-iteration-count.at-sign
  (format nil "~3@{/A~}" 'x)
  "/A/A/A")

(deftest format-iteration-count.empty-list-still-none
  (format nil "~3{x~}" '())
  "")

(deftest format-iteration-count.sublists-consume-their-list
  (format nil "~3:{/A~}" '((x)))
  "/A")
