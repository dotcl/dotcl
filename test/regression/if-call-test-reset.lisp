;;; An IF whose test is a function call empties the values channel in the arm
;;; whose value leaves the IF, not in the arm that jumps back to the head of a
;;; self tail call loop. Values the test's callee published must not reach
;;; whoever reads the loop's values.

(setf dotcl:*save-sil* t)

(defvar *ictr-h* (make-hash-table))
(setf (gethash 1 *ictr-h*) nil)
(defun %ictr-nil7 (n) (declare (ignore n)) (values nil 7))
(defun %ictr-gethash (n) (declare (ignore n)) (gethash 1 *ictr-h*))
(defun %ictr-a (x n) (if (= n 0) x (if (%ictr-nil7 n) x (%ictr-a x (1- n)))))
(defun %ictr-b (x n) (if (= n 0) x (if (%ictr-gethash n) x (%ictr-b x (1- n)))))
(defun %ictr-c (x n) (if (= n 0) x (if (funcall #'gethash 1 *ictr-h*) x (%ictr-c x (1- n)))))
(defun %ictr-d (x n) (if (= n 0) x (if (funcall #'floor 0 1) (%ictr-d x (1- n)) x)))
(defun %ictr-e (x n) (if (%ictr-nil7 n) x (if (= n 0) x (%ictr-e x (1- n)))))
(defun %ictr-f (x n) (if (%ictr-nil7 n) 1 (if (= n 0) x (%ictr-f x (1- n)))))
(defun %ictr-loop (n acc) (if (%ictr-nil7 n) acc (if (= n 0) acc (%ictr-loop (1- n) (+ acc n)))))

(deftest if-call-test-reset.values
  (list (multiple-value-list (%ictr-a nil 2))
        (multiple-value-list (%ictr-b nil 2))
        (multiple-value-list (%ictr-c nil 2))
        (multiple-value-list (%ictr-d 0 2))
        (multiple-value-list (%ictr-e nil 2))
        (multiple-value-list (%ictr-f nil 2))
        (multiple-value-list (%ictr-loop 3 0)))
  ((nil) (nil) (nil) (0) (nil) (nil) (6)))

;; The loop's test is followed by its branch: the reset is in the returning arm.
(defun %ictr-reset-after-test-p (fn)
  (let ((code (dotcl:function-sil fn)))
    (loop for (a b) on code
          thereis (and (equal a '(:call "Runtime.IsTruthy"))
                       (equal b '(:call "MultipleValues.Reset"))))))

(deftest-compiled-only if-call-test-reset.sil
  (%ictr-reset-after-test-p #'%ictr-loop)
  nil)

;;; The same through COMPILE-FILE, where the test's call goes straight to a
;;; function of the file (or to the stub of one defined later).

(defvar *ictrf-loaded*
  (when (compiled-mode-p)
    (let ((fasl (regression-temp-file "if-call-test-reset.fasl")))
      (compile-file "test/regression/if-call-test-reset/calls.lisp" :output-file fasl)
      (load fasl)
      t)))

(deftest-compiled-only if-call-test-reset.same-file
  (list (multiple-value-list (cl-user::%ictrf-loop 3 0))
        (multiple-value-list (cl-user::%ictrf-a nil 2))
        (multiple-value-list (cl-user::%ictrf-fw nil 2)))
  ((6) (nil) (nil)))
