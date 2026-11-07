;;; Running out of stack or size is a condition a program can handle, not the end of the process.

(deftest exhaustion-conditions.deep-equalp-returns-or-signals-storage-condition
  ;; Whether 10^6 levels fit depends on the stack the test process was given; either way the
  ;; process must survive (test/cli-exit/check.sh runs the case that used to abort the process).
  (let ((a nil) (b nil))
    (dotimes (i 1000000) (setq a (list a) b (list b)))
    (and (member (handler-case (progn (equalp a b) :returned)
                   (storage-condition () :storage-condition))
                 '(:returned :storage-condition))
         t))
  t)

(deftest exhaustion-conditions.circular-length
  (let ((x (list 1 2 3)))
    (setf (cdr (last x)) x)
    (list (handler-case (length x) (type-error () :type-error))
          (length (list 1 2 3 4 5))
          (length (make-list 100000))))
  (:type-error 5 100000))

(deftest exhaustion-conditions.expt-too-large
  (list (handler-case (expt 2 (expt 2 40)) (storage-condition () :storage-condition))
        (expt 0 (expt 2 40))
        (expt 1 (expt 2 40))
        (expt -1 (1+ (expt 2 40))))
  (:storage-condition 0 1 -1))

(deftest exhaustion-conditions.shallow-and-wide-still-fine
  (let ((a nil) (b nil))
    (dotimes (i 1000) (setq a (list a) b (list b)))
    (list (equal a b) (equalp a b) (length (read-from-string (format nil "(~{~a~^ ~})" (loop for i below 1000 collect i))))))
  (t t 1000))
