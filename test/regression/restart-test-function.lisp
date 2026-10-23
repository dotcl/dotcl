;;; A restart's :TEST (RESTART-CASE) / :TEST-FUNCTION (RESTART-BIND) decides its
;;; visibility for every lookup, with a condition or with NIL. COMPUTE-RESTARTS
;;; without a condition listed the restart whatever the test returned, and with a
;;; condition a test returning true made a restart associated with another
;;; condition visible.

(define-condition rtf-a (error) ())
(define-condition rtf-b (error) ())

(defun %rtf-names (&optional c)
  (mapcar #'restart-name (compute-restarts c)))

(deftest restart-test-function.compute-restarts-no-condition
  (list (restart-case (and (member 'rtf-foo (%rtf-names)) t)
          (rtf-foo () :test (lambda (c) (declare (ignore c)) nil) 1))
        (restart-bind ((rtf-foo (lambda () 1)
                         :test-function (lambda (c) (declare (ignore c)) nil)))
          (and (member 'rtf-foo (%rtf-names)) t))
        (restart-case (and (member 'rtf-foo (%rtf-names)) t)
          (rtf-foo () :test (lambda (c) (declare (ignore c)) t) 1)))
  (nil nil t))

(deftest restart-test-function.test-sees-nil-or-condition
  (let ((seen '())
        (c (make-condition 'rtf-a)))
    (restart-case (progn (find-restart 'rtf-foo) (compute-restarts)
                         (find-restart 'rtf-foo c) (compute-restarts c))
      (rtf-foo () :test (lambda (x) (push (if x (type-of x) :none) seen) t) 1))
    (remove-duplicates seen))
  (rtf-a :none))

(deftest restart-test-function.with-condition
  (let ((c (make-condition 'rtf-a)))
    (list (restart-case (and (find-restart 'rtf-foo c) t)
            (rtf-foo () :test (lambda (x) (typep x 'rtf-b)) 1))
          (restart-case (and (member 'rtf-foo (%rtf-names c)) t)
            (rtf-foo () :test (lambda (x) (typep x 'rtf-b)) 1))
          (restart-case (and (find-restart 'rtf-foo c) t)
            (rtf-foo () :test (lambda (x) (typep x 'rtf-a)) 1))))
  (nil nil t))

(deftest restart-test-function.find-restart-object
  (let ((ok t))
    (restart-case (let ((r (find-restart 'rtf-foo)))
                    (setq ok nil)
                    (find-restart r))
      (rtf-foo () :test (lambda (x) (declare (ignore x)) ok) 1)))
  nil)

;; Association with another condition hides the restart even when the test is true.
(deftest restart-test-function.association-and-test
  (let ((a (make-condition 'rtf-a))
        (b (make-condition 'rtf-b)))
    (restart-case
        (with-condition-restarts a (list (find-restart 'rtf-foo))
          (list (find-restart 'rtf-foo b)
                (and (find-restart 'rtf-foo a) t)
                (and (member 'rtf-foo (%rtf-names b)) t)
                (and (member 'rtf-foo (%rtf-names a)) t)))
      (rtf-foo () :test (lambda (x) (declare (ignore x)) t) 1)))
  (nil t nil t))
