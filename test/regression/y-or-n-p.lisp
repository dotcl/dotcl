;;; Y-OR-N-P and YES-OR-NO-P read their answer from *QUERY-IO*.
;;;
;;; Bug: both were stubs that returned T without printing or reading anything.
;;; Code that asks before doing something optional went ahead unasked, and with
;;; stdin closed they said "yes" where other implementations signal END-OF-FILE.
;;; chanl's STUMBLERS test asks "Stress deadlock detection further?" and on
;;; "yes" runs a stage that kills pool threads at random, which could leave the
;;; thread pool unable to run the next test's tasks.

(defun %ynp-ask (fn input &rest args)
  "Call FN with ARGS, *QUERY-IO* reading INPUT. Return the answer and what was
printed, or the class of the condition signalled."
  (let ((out (make-string-output-stream)))
    (handler-case
        (let ((*query-io* (make-two-way-stream (make-string-input-stream input) out)))
          (let ((answer (apply fn args)))
            (list answer (get-output-stream-string out))))
      (end-of-file () :end-of-file)
      (error (e) (list :other (type-of e))))))

(deftest y-or-n-p.yes
  (first (%ynp-ask #'y-or-n-p (format nil "y~%")))
  t)

(deftest y-or-n-p.no
  (first (%ynp-ask #'y-or-n-p (format nil "n~%")))
  nil)

(deftest y-or-n-p.case-and-blanks
  (first (%ynp-ask #'y-or-n-p (format nil "  Y ~%")))
  t)

(deftest y-or-n-p.reasks-until-answered
  (let ((r (%ynp-ask #'y-or-n-p (format nil "maybe~%n~%") "Go?")))
    (list (first r) (> (count #\? (second r)) 1)))
  (nil t))

(deftest y-or-n-p.prints-question
  (let ((text (second (%ynp-ask #'y-or-n-p (format nil "y~%") "Delete ~a?" "foo"))))
    (and (search "Delete foo?" text) (search "(y or n)" text) t))
  t)

(deftest y-or-n-p.eof-signals
  (%ynp-ask #'y-or-n-p "")
  :end-of-file)

(deftest yes-or-no-p.yes
  (first (%ynp-ask #'yes-or-no-p (format nil "yes~%")))
  t)

(deftest yes-or-no-p.no
  (first (%ynp-ask #'yes-or-no-p (format nil "NO~%")))
  nil)

(deftest yes-or-no-p.y-is-not-enough
  (first (%ynp-ask #'yes-or-no-p (format nil "y~%no~%")))
  nil)

(deftest yes-or-no-p.eof-signals
  (%ynp-ask #'yes-or-no-p "")
  :end-of-file)

(deftest yes-or-no-p.no-arguments
  (first (%ynp-ask #'yes-or-no-p (format nil "yes~%")))
  t)
