;;; :FROM-END should stop when it has what it came for.
;;;
;;; With :FROM-END the answer lives at the right end of the sequence, so the scan
;;; belongs there too. REMOVE and friends used to mark every match walking forward
;;; and then unmark the leftmost excess; FIND used to keep the last match while
;;; walking the whole thing. Both reached the right answer, and both applied the
;;; test to elements they had no reason to look at -- observable whenever the test
;;; has an effect, and pure waste when :COUNT is small and the sequence is long.
;;;
;;; The values are unchanged; what these fix is the calls. SBCL scans from the end.

(defvar *sfe-log* nil)

(defun sfe-log-eql (x)
  "A test that records what it was asked about."
  (push x *sfe-log*)
  (eql x 2))

(defmacro sfe-calls (&body body)
  "The arguments the test saw, in the order it saw them."
  `(let ((*sfe-log* '()))
     ,@body
     (reverse *sfe-log*)))

;;; --- remove / delete with :count -------------------------------------------

(deftest sfe-remove-vector
  (sfe-calls (remove-if #'sfe-log-eql (vector 1 2 3 2 4 2) :from-end t :count 2))
  (2 4 2))

(deftest sfe-remove-list
  (sfe-calls (remove-if #'sfe-log-eql (list 1 2 3 2 4 2) :from-end t :count 2))
  (2 4 2))

(deftest sfe-delete-list
  (sfe-calls (delete-if #'sfe-log-eql (list 1 2 3 2 4 2) :from-end t :count 2))
  (2 4 2))

;;; The values are what they always were.
;;; As a list, because this framework compares with EQUAL and two vectors are
;;; EQUAL only when they are the same object.
(deftest sfe-remove-value-vector
  (coerce (remove-if (lambda (x) (eql x 2)) (vector 1 2 3 2 4 2) :from-end t :count 2)
          'list)
  (1 2 3 4))

(deftest sfe-remove-value-list
  (remove-if (lambda (x) (eql x 2)) (list 1 2 3 2 4 2) :from-end t :count 2)
  (1 2 3 4))

;;; Without :count there is nothing to stop early for: every element is examined.
(deftest sfe-remove-without-count-sees-everything
  (length (sfe-calls (remove-if #'sfe-log-eql (vector 1 2 3 2 4 2) :from-end t)))
  6)

;;; --- find / find-if --------------------------------------------------------

(deftest sfe-find-vector
  (sfe-calls (find nil (vector 1 2 3 2 4 2)
                   :test (lambda (a b) (declare (ignore a)) (sfe-log-eql b))
                   :from-end t))
  (2))

(deftest sfe-find-if-vector
  (sfe-calls (find-if #'sfe-log-eql (vector 1 2 3 2 4 2) :from-end t))
  (2))

(deftest sfe-find-if-string
  (sfe-calls (find-if (lambda (c) (push c *sfe-log*) (char= c #\X)) "aXbXcX" :from-end t))
  (#\X))

(deftest sfe-find-if-value
  (find-if (lambda (x) (> x 2)) (vector 1 3 2 4 2) :from-end t)
  4)

;;; No match: everything is examined, and the answer is still NIL.
(deftest sfe-find-if-no-match
  (let ((calls (sfe-calls (find-if #'sfe-log-eql (vector 1 3 5) :from-end t))))
    (list calls (find-if (lambda (x) (eql x 2)) (vector 1 3 5) :from-end t)))
  ((5 3 1) nil))

;;; Forward search is untouched.
(deftest sfe-find-if-forward-unchanged
  (sfe-calls (find-if #'sfe-log-eql (vector 1 2 3 2 4 2)))
  (1 2))
