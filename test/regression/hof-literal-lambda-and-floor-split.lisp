;;; Two allocations that a string-building loop (cl-bench hash-strings) paid
;;; on every call:
;;; - (POSITION-IF (LAMBDA (X) ...) SEQ) and its relatives built a closure
;;;   whenever the lambda had a free variable. A literal one-parameter lambda
;;;   is now substituted into a loop.
;;; - (MULTIPLE-VALUE-BIND (Q R) (FLOOR A B) ...) and MULTIPLE-VALUE-SETQ built
;;;   a multiple-value record and boxed both values. They are now (FLOOR A B)
;;;   and (MOD A B) (TRUNCATE / REM), which for fixnums box nothing.
;;; The answers must be the ones the general paths give.

(defun %hlf-pos (n seq) (position-if (lambda (x) (> x n)) seq))
(defun %hlf-pos-not (n seq) (position-if-not #'(lambda (x) (> x n)) seq))
(defun %hlf-find (n seq) (find-if (lambda (x) (declare (fixnum x)) (= x n)) seq))
(defun %hlf-find-not (seq) (find-if-not (lambda (c) (char= c #\a)) seq))
(defun %hlf-count (n seq) (count-if (lambda (x) (evenp (+ x n))) seq))
(defun %hlf-count-not (seq) (count-if-not (lambda (x) (zerop x)) seq))
;; RETURN inside the lambda leaves the enclosing DOLIST, as it did before.
(defun %hlf-return (n)
  (dolist (k '(1 2 3) :none)
    (position-if (lambda (x) (when (= x n) (return (list :out k x))) nil) '(1 2 3))))
;; Side effects in the body happen once per element, in order.
(defun %hlf-trace (seq)
  (let ((seen '()))
    (list (find-if (lambda (x) (push x seen) (> x 2)) seq) (reverse seen))))

(deftest hof-literal-lambda.values
  (list (%hlf-pos 5 '(1 7 3)) (%hlf-pos 5 #(1 2 3)) (%hlf-pos 0 "")
        (%hlf-pos-not 5 #(9 8 1)) (%hlf-find 3 '(1 2 3 4)) (%hlf-find 9 #(1 2))
        (%hlf-find-not "aab") (%hlf-count 1 '(1 2 3 4 5)) (%hlf-count-not #(0 1 0 2))
        (%hlf-return 2) (%hlf-return 9)
        (%hlf-trace '(1 2 3 4)) (%hlf-trace #(5))
        (handler-case (%hlf-pos 1 5) (type-error () :type-error))
        (handler-case (%hlf-pos 9 '(1 2 . 3)) (type-error () :type-error)))
  (1 nil nil 2 3 nil #\b 3 2 (:out 1 2) :none (3 (1 2 3)) (5 (5)) :type-error :type-error))

(defun %hlf-bytes () (nth 4 (dotcl:gc-stats)))
(defvar *hlf-v* (vector 10 100 1000 10000 100000 10000000))
(defun %hlf-capturing (n) (declare (fixnum n)) (position-if (lambda (x) (> (the fixnum x) n)) *hlf-v*))

(deftest-compiled-only hof-literal-lambda.no-closure
  (progn
    (%hlf-capturing 1)
    (let ((b0 (%hlf-bytes)))
      (dotimes (i 10000) (%hlf-capturing 50))
      (< (/ (- (%hlf-bytes) b0) 10000) 64)))
  t)

(defun %hlf-mvb (a b) (multiple-value-bind (q r) (floor a b) (list q r)))
(defun %hlf-mvb-t (a b) (multiple-value-bind (q r) (truncate a b) (list q r)))
;; The quotient variable has the dividend's name: the remainder still uses the
;; outer binding.
(defun %hlf-mvb-shadow (q base) (multiple-value-bind (q r) (truncate q base) (list q r)))
(defun %hlf-mvb-one (a b) (multiple-value-bind (q) (floor a b) q))
(defun %hlf-mvs (a b) (let ((q 0) (r 0)) (list (multiple-value-setq (q r) (floor a b)) q r)))
(defun %hlf-mvs-self (q b) (let ((r 0)) (multiple-value-setq (q r) (floor q b)) (list q r)))

(deftest floor-split.values
  (list (%hlf-mvb 17 5) (%hlf-mvb -17 5) (%hlf-mvb 17 -5) (%hlf-mvb-t -17 5)
        (%hlf-mvb 7.5 2) (%hlf-mvb (expt 2 70) 3) (%hlf-mvb 7/2 1)
        (%hlf-mvb-shadow -100 7) (%hlf-mvb-one -7 2)
        (%hlf-mvs -17 5) (%hlf-mvs-self 100 7)
        (handler-case (%hlf-mvb 1 0) (division-by-zero () :division-by-zero)))
  ((3 2) (-4 3) (-4 -3) (-3 -2) (3 1.5) (393530540239137101141 1) (3 1/2)
   (-14 -2) -4 (-4 -4 3) (14 2) :division-by-zero))

(defun %hlf-digits (n base)
  (declare (fixnum n base))
  (let ((q n) (r 0) (acc 0))
    (declare (fixnum q r acc))
    (dotimes (k 6) (multiple-value-setq (q r) (floor q base)) (setq acc (+ acc r)))
    acc))

(deftest-compiled-only floor-split.no-boxing
  (progn
    (%hlf-digits 123456 16)
    (let ((b0 (%hlf-bytes)))
      (dotimes (i 10000) (%hlf-digits (+ 1000000 i) 16))
      (list (< (/ (- (%hlf-bytes) b0) 10000) 64) (%hlf-digits 123456 16))))
  (t 21))
