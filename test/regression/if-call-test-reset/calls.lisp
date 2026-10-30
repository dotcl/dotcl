;;; Compiled with COMPILE-FILE by if-call-test-reset.lisp: loop tests that call a
;;; function of this file (defined before, and after, the caller).

(defun %ictrf-nil7 (n) (declare (ignore n)) (values nil 7))
(defun %ictrf-loop (n acc) (if (%ictrf-nil7 n) acc (if (= n 0) acc (%ictrf-loop (1- n) (+ acc n)))))
(defun %ictrf-a (x n) (if (= n 0) x (if (%ictrf-nil7 n) x (%ictrf-a x (1- n)))))
(defun %ictrf-fw (x n) (if (= n 0) x (if (%ictrf-later n) x (%ictrf-fw x (1- n)))))
(defun %ictrf-later (n) (declare (ignore n)) (values nil 8 9))
