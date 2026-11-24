;;; A block, an inlined function, or a VALUES form whose value only the primary is taken of builds
;;; no MvReturn for the other values; where the other values are read, they are all there.

(declaim (inline %bsv-size))
(defun %bsv-size (x)
  (cond ((null x) (values 0 nil))
        ((stringp x) (values (length x) t))
        (t (values 5 :other))))

(defun %bsv-primary (x) (let ((a (%bsv-size x))) a))
(defun %bsv-both (x) (multiple-value-list (%bsv-size x)))
(defun %bsv-bind (x) (multiple-value-bind (n kind) (%bsv-size x) (list n kind)))
(defun %bsv-nth (x) (nth-value 1 (%bsv-size x)))
(defun %bsv-arg (x) (+ 1 (%bsv-size x)))
(defun %bsv-tail (x) (%bsv-size x))

(deftest bsv.values-and-primaries
  (list (%bsv-primary nil) (%bsv-primary "abc") (%bsv-primary 7)
        (%bsv-both nil) (%bsv-both "ab") (%bsv-both 7)
        (%bsv-bind "abcd") (%bsv-nth "x") (%bsv-nth 1)
        (%bsv-arg "abc") (multiple-value-list (%bsv-tail "xy")))
  (0 3 5 (0 nil) (2 t) (5 :other) (4 t) t :other 4 (2 t)))

(defun %bsv-effects (log)
  (let ((r (values (progn (push :a log) 1) (progn (push :b log) 2) (progn (push :c log) 3))))
    (list r (reverse log))))

(deftest bsv.values-evaluates-all-arguments-in-order
  (list (%bsv-effects nil)
        (let ((x (values 10 (ignore-errors (error "ignored")))))
          x))
  ((1 (:a :b :c)) 10))

(defun %bsv-blocks (n)
  (list (let ((a (block nil (when (> n 0) (return (values :pos n))) (values :neg n)))) a)
        (multiple-value-list (block nil (when (> n 0) (return (values :pos n))) (values :neg n)))
        (let ((a (block b (dolist (x '(1 2 3)) (when (= x n) (return-from b (values x :found)))) (values nil :missing)))) a)
        (multiple-value-bind (v how) (block b (dolist (x '(1 2 3)) (when (= x n) (return-from b (values x :found)))) (values nil :missing))
          (list v how))))

(deftest bsv.blocks
  (list (%bsv-blocks 2) (%bsv-blocks -1))
  ((:pos (:pos 2) 2 (2 :found))
   (:neg (:neg -1) nil (nil :missing))))

(deftest bsv.catch-throw-and-unwind-protect-keep-values
  (list (let ((a (catch 'k (throw 'k (values 1 2))))) a)
        (multiple-value-list (catch 'k (throw 'k (values 1 2))))
        (let ((a (unwind-protect (values 5 6) (list 1)))) a)
        (multiple-value-list (unwind-protect (values 5 6) (list 1))))
  (1 (1 2) 5 (5 6)))

(defun %bsv-loop (n)
  (declare (fixnum n))
  (let ((k 0))
    (declare (fixnum k))
    (dotimes (i n) (incf k (the fixnum (let ((a (%bsv-size 7))) a))))
    k))

(deftest bsv.loop-value
  (%bsv-loop 1000)
  5000)

(deftest-compiled-only bsv.no-mvreturn-per-call
  (progn (%bsv-loop 100)
         (< (min-allocated-bytes (lambda () (%bsv-loop 100000))) 100000))
  t)
