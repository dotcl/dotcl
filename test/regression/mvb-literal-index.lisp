;;; MULTIPLE-VALUE-BIND reads each variable's value from the bind snapshot by
;;; its literal position. Fewer values than variables leave NIL, more values
;;; than fit the first snapshot grow it, and each thread keeps its own count.

(defun %mli-two (x) (values x (1+ x)))
(defun %mli-one (x) x)
(defun %mli-none () (values))
(defun %mli-many (x) (values x 1 2 3 4 5 6 7 8 9 (+ x 10)))
(defun %mli-three (x) (values x (* 2 x) (* 3 x)))

(defun %mli-bind2 (x) (multiple-value-bind (a b) (%mli-two x) (list a b)))
(defun %mli-bind-short (x) (multiple-value-bind (a b c) (%mli-one x) (list a b c)))
(defun %mli-bind-none () (multiple-value-bind (a b) (%mli-none) (list a b)))
(defun %mli-bind-many (x)
  (multiple-value-bind (a b c d e f g h i j k l) (%mli-many x) (list a b c d e f g h i j k l)))
(defun %mli-nested (x)
  (multiple-value-bind (a b) (%mli-two x)
    (multiple-value-bind (c d e) (%mli-three b)
      (list a b c d e))))
(defun %mli-loop (n)
  (let ((s 0))
    (dotimes (i n s)
      (multiple-value-bind (a b) (%mli-two i) (setq s (+ s a b))))))

(deftest mvb-literal-index.two (%mli-bind2 5) (5 6))
(deftest mvb-literal-index.short (%mli-bind-short 5) (5 nil nil))
(deftest mvb-literal-index.none (%mli-bind-none) (nil nil))
(deftest mvb-literal-index.many (%mli-bind-many 1) (1 1 2 3 4 5 6 7 8 9 11 nil))
(deftest mvb-literal-index.nested (%mli-nested 2) (2 3 3 6 9))
(deftest mvb-literal-index.loop (%mli-loop 100) 10000)

(defun %mli-worker (k)
  (loop repeat 2000
        always (and (equal (%mli-bind2 k) (list k (1+ k)))
                    (equal (%mli-bind-short k) (list k nil nil))
                    (equal (%mli-nested k)
                           (list k (1+ k) (1+ k) (* 2 (1+ k)) (* 3 (1+ k)))))))

(deftest mvb-literal-index.threads
  (let* ((threads (loop for k from 0 below 4
                        collect (let ((k k)) (dotcl:make-thread (lambda () (%mli-worker k))))))
         (main (loop repeat 2000 always (equal (%mli-bind-many 3) '(3 1 2 3 4 5 6 7 8 9 13 nil)))))
    (list main (mapcar #'dotcl:thread-join threads)))
  (t (t t t t)))
