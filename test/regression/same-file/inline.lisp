;;; Compiled with COMPILE-FILE by test/regression/same-file-inline.lisp.

(in-package "CL-USER")

(defstruct %sfi-task holding pending waiting)

(defun %sfi-held-p (task)
  (or (%sfi-task-holding task)
      (and (not (%sfi-task-pending task)) (%sfi-task-waiting task))))
(defun %sfi-count-held (tasks)
  (count-if (lambda (task) (%sfi-held-p task)) tasks))
(defun %sfi-first-held (tasks)
  (dolist (task tasks nil)
    (when (%sfi-held-p task) (return task))))

(defun %sfi-add1 (x) (+ x 1))
(defun %sfi-twice-add1 (x) (* 2 (%sfi-add1 x)))
(defun %sfi-tail-add1 (x) (%sfi-add1 x))

(defun %sfi-two (a b) (values b a))
(defun %sfi-mv (a b) (multiple-value-list (%sfi-two a b)))

(defun %sfi-len (x) (declare (list x)) (length x))
(defun %sfi-len-caller (x) (%sfi-len x))

(declaim (notinline %sfi-ni))
(defun %sfi-ni (x) (+ x 1))
(defun %sfi-ni-caller (x) (%sfi-ni x))

(defun %sfi-fact (n) (if (< n 2) 1 (* n (%sfi-fact (- n 1)))))
(defun %sfi-fact-caller (n) (%sfi-fact n))

(defun %sfi-big (x)
  (list (list x x x x x x x x) (list x x x x x x x x) (list x x x x x x x x)
        (list x x x x x x x x) (list x x x x x x x x) (list x x x x x x x x)
        (list x x x x x x x x) (list x x x x x x x x) (list x x x x x x x x)
        (list x x x x x x x x) (list x x x x x x x x) (list x x x x x x x x)
        (list x x x x x x x x) (list x x x x x x x x) (list x x x x x x x x)))
(defun %sfi-big-caller (x) (length (%sfi-big x)))

(defun %sfi-key (x &key (y 1)) (+ x y))
(defun %sfi-key-caller (x) (%sfi-key x :y 2))

(defun %sfi-opt (x &optional (y 1)) (+ x y))
(defun %sfi-opt-caller (x) (%sfi-opt x))

(defun %sfi-d3 (x) (declare (optimize (debug 3))) (+ x 1))
(defun %sfi-d3-caller (x) (%sfi-d3 x))

(defun %sfi-loop (n) (let ((s 0)) (dotimes (i n s) (incf s i))))
(defun %sfi-loop-caller (n) (%sfi-loop n))

(defun %sfi-speed (x) (declare (optimize speed)) (+ x 1))
(defun %sfi-speed-caller (x) (%sfi-speed x))

(defun %sfi-literal () '(1 2))
(defun %sfi-literal-caller () (%sfi-literal))

(defun %sfi-cell () (load-time-value (list 0)))
(defun %sfi-cell-caller () (%sfi-cell))

(defun %sfi-free-var () %sfi-free-var)
(defun %sfi-free-caller () (let ((%sfi-free-var :lexical)) (list %sfi-free-var (%sfi-free-var))))

(defun %sfi-redef (x) (list :first x))
(defun %sfi-redef-early (x) (%sfi-redef x))
(defun %sfi-redef (x) (list :second x))
(defun %sfi-redef-late (x) (%sfi-redef x))

(defun %sfi-later-caller (x) (%sfi-later x))
(defun %sfi-later (x) (list :later x))
