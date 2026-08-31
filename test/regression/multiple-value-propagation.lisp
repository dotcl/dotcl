;;; Where a second value survives, and where it does not.
;;;
;;; This file exists as the safety net for changing how multiple values are
;;; returned. Today a form that returns two values allocates an MvReturn to
;;; carry them (40 bytes) even when the caller wants one value and throws the
;;; second away, and the ways out of that all move the second value somewhere
;;; else -- a thread-static pair, a separate single-value entry point, a
;;; different calling convention. Every one of them is easy to get subtly wrong
;;; in one of the paths below, and the failure is silent: a form quietly yields
;;; one value where it should yield two, or keeps a stale second value from an
;;; earlier call.
;;;
;;; So the point of each test is not that the answer is interesting; it is that
;;; the answer must not change. Every expected value here was read off SBCL.

(defun %mvp-two () (values 1 2))
(defun %mvp-three () (values 3 4 5))

;;; --- Positions that propagate ---

(defun %mvp-tail () (%mvp-two))
(defun %mvp-block () (block b (%mvp-two)))
(defun %mvp-return-from () (block b (return-from b (%mvp-two)) 5))

(deftest multiple-value-propagation.propagating-positions
  (list (multiple-value-list (%mvp-tail))
        (multiple-value-list (%mvp-block))
        (multiple-value-list (%mvp-return-from))
        (multiple-value-list (progn 0 (%mvp-two)))
        (multiple-value-list (let ((y 1)) y (%mvp-two)))
        (multiple-value-list (if t (%mvp-two) 0))
        (multiple-value-list (when t (%mvp-two)))
        (multiple-value-list (and t (%mvp-two)))
        (multiple-value-list (or nil (%mvp-two)))
        (multiple-value-list (cond (t (%mvp-two))))
        (multiple-value-list (dotimes (i 1 (%mvp-two)))))
  ((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2)))

;;; --- Positions that take only the primary ---

(defun %mvp-non-tail () (%mvp-two) (values 9))

(deftest multiple-value-propagation.single-value-positions
  (list (multiple-value-list (%mvp-non-tail))
        (multiple-value-list (let ((x (%mvp-two))) x))
        (multiple-value-list (progn (%mvp-two) 7))
        (multiple-value-list (let (z) (setq z (%mvp-two)) z))
        (multiple-value-list (identity (%mvp-two)))
        (multiple-value-list (list (%mvp-two) (%mvp-two)))
        (multiple-value-list (let ((q (floor 7 2))) q)))
  ((9) (1) (7) (1) (1) ((1 1)) (3)))

;;; A variable holds the primary and nothing else, however it was set: asking
;;; for two values back out of it gives NIL for the second.
(deftest multiple-value-propagation.variable-keeps-only-the-primary
  (multiple-value-list
   (let ((x (%mvp-two))) (multiple-value-bind (a b) x (list a b))))
  ((1 nil)))

;;; --- The paths that carry values across a transfer of control ---

(defun %mvp-thrower () (throw 'mvp-tag (values 1 2)))
(defun %mvp-catcher () (catch 'mvp-tag (%mvp-thrower) :not-reached))

(deftest multiple-value-propagation.control-transfer
  (list (multiple-value-list (catch 'mvp-tag (throw 'mvp-tag (%mvp-two))))
        (multiple-value-list (catch 'mvp-tag (%mvp-two)))
        (multiple-value-list (%mvp-catcher))
        (multiple-value-list (handler-case (%mvp-two) (error () :e)))
        (multiple-value-list (handler-case (progn (%mvp-two)) (error () :e)))
        (multiple-value-list (handler-case (%mvp-two) (:no-error (a b) (list :ok a b)))))
  ((1 2) (1 2) (1 2) (1 2) (1 2) ((:ok 1 2))))

;;; UNWIND-PROTECT: the body's values survive a cleanup that produces values of
;;; its own, including one that reads values itself.
(deftest multiple-value-propagation.unwind-protect-keeps-body-values
  (list (multiple-value-list (unwind-protect (%mvp-two) (values 8 9)))
        (multiple-value-list (unwind-protect (%mvp-two) (%mvp-three)))
        (multiple-value-list
         (unwind-protect (%mvp-two)
           (multiple-value-bind (a b) (%mvp-three) (list a b)))))
  ((1 2) (1 2) (1 2)))

;;; MULTIPLE-VALUE-CALL needs every argument form's values, so the first form's
;;; values have to be held while the second form runs -- the case a scheme that
;;; keeps values in one shared place gets wrong.
(deftest multiple-value-propagation.multiple-value-call
  (list (multiple-value-call #'list (%mvp-two) (%mvp-two))
        (multiple-value-call #'list (%mvp-two) (%mvp-three))
        (multiple-value-call #'list (progn (%mvp-two)) (%mvp-two))
        (multiple-value-call #'list (%mvp-two) 5 (values 6 7))
        (multiple-value-call #'list (values) (%mvp-two) (values)))
  ((1 2 1 2) (1 2 3 4 5) (1 2 1 2) (1 2 5 6 7) (1 2)))

;;; A discarded multi-value form must not leave its values behind for whatever
;;; reads values next.
(deftest multiple-value-propagation.discarded-values-do-not-linger
  (list (multiple-value-list (progn (%mvp-two) (values 9)))
        (multiple-value-list (progn (%mvp-two) 9))
        (multiple-value-list (progn (%mvp-three) (%mvp-two))))
  ((9) (9) (1 2)))

;;; The runtime functions that return two values, in both positions.
(deftest multiple-value-propagation.builtins
  (let ((h (make-hash-table)))
    (setf (gethash :k h) 1)
    (list (multiple-value-list (gethash :k h))
          (multiple-value-list (gethash :missing h))
          (multiple-value-list (floor 7 2))
          (multiple-value-list (truncate -7 2))
          (list (nth-value 0 (%mvp-two)) (nth-value 1 (%mvp-two))
                (nth-value 2 (%mvp-two)))))
  ((1 t) (nil nil) (3 1) (-3 -1) (1 2 nil)))

;;; Counts other than two, and nesting.
(deftest multiple-value-propagation.counts-and-nesting
  (list (multiple-value-list (values))
        (multiple-value-list (values 1 2 3))
        (multiple-value-list (values-list '(1 2 3)))
        (multiple-value-list (tagbody a (go b) b))
        (multiple-value-list
         (multiple-value-bind (a b) (%mvp-two) (list a b)))
        (multiple-value-list
         (multiple-value-bind (a b) (%mvp-two)
           (multiple-value-bind (c d) (%mvp-three) (list a b c d))))
        (multiple-value-list
         (multiple-value-bind (a b) (%mvp-two) (declare (ignore a b)) (%mvp-two))))
  (() (1 2 3) (1 2 3) (nil) ((1 2)) ((1 2 3 4)) (1 2)))
