;;; A comparison between float formats runs natively.
;;;
;;; The native r8 comparison required BOTH operands to be declared double, so
;;; (> x 4.0) with x a double had one operand of each format and fell off it:
;;; the double was boxed and the comparison went through the generic entry.
;;; Mixing formats is the ordinary case, because a literal like 4.0 reads as a
;;; single float. CLHS 12.1.4.1 compares such a pair in the longer format, and
;;; widening a single to a double is exact, so both operands can be taken as
;;; doubles -- which also lets a single/single comparison use the same path.
;;;
;;; The reference is the undeclared path: a declaration must not change an
;;; answer. (SBCL cannot be asked about the NaN rows -- it folds (- inf inf) to
;;; 0.0d0 -- but agrees on every ordered row.)

(defun %mfc-6 (a b)
  (list (if (> a b) t nil) (if (< a b) t nil) (if (>= a b) t nil)
        (if (<= a b) t nil) (if (= a b) t nil) (if (/= a b) t nil)))

(defun %mfc-dd (a b) (declare (double-float a b)) (%mfc-6 a b))
(defun %mfc-ds (a b) (declare (double-float a) (single-float b)) (%mfc-6 a b))
(defun %mfc-sd (a b) (declare (single-float a) (double-float b)) (%mfc-6 a b))
(defun %mfc-ss (a b) (declare (single-float a b)) (%mfc-6 a b))

(deftest mixed-float-comparison.values
  (list (%mfc-ds 4.5d0 4.0) (%mfc-ds 1.0d0 2.0) (%mfc-sd 2.0 2.0d0)
        (%mfc-ss 1.0 2.0) (%mfc-ss 2.0 2.0) (%mfc-dd 2.0d0 2.0d0))
  ((t nil t nil nil t) (nil t nil t nil t) (nil nil t t t nil)
   (nil t nil t nil t) (nil nil t t t nil) (nil nil t t t nil)))

;;; 0.1 is not the same number in the two formats, and widening must not pretend
;;; it is: the single 0.1 is larger than the double 0.1.
(deftest mixed-float-comparison.formats-are-not-conflated
  (list (%mfc-ds 0.1d0 0.1) (%mfc-sd 0.1 0.1d0))
  ((nil t nil t nil t) (t nil t nil nil t)))

;;; The declaration must not change any answer, NaN and infinity included.
(deftest mixed-float-comparison.declared-agrees-with-undeclared
  (let* ((inf (/ 1.0d0 0.0d0)) (nan (- inf inf))
         (infs (/ 1.0 0.0)) (nans (- infs infs)))
    (list (equal (%mfc-dd nan 1.0d0) (%mfc-6 nan 1.0d0))
          (equal (%mfc-ds nan 1.0) (%mfc-6 nan 1.0))
          (equal (%mfc-ds 1.0d0 nans) (%mfc-6 1.0d0 nans))
          (equal (%mfc-ss nans 1.0) (%mfc-6 nans 1.0))
          (equal (%mfc-ds inf 1.0) (%mfc-6 inf 1.0))
          (equal (%mfc-ss infs 1.0) (%mfc-6 infs 1.0))
          (equal (%mfc-ds 0.1d0 0.1) (%mfc-6 0.1d0 0.1))))
  (t t t t t t t))

;;; The point: comparing a declared double against a float literal allocates
;;; nothing. It used to box the double for the generic comparison.
;; The comparison sits in the loop test, where its result is consumed by a
;; branch and nothing else. A (WHEN (> x 4.0) (SETQ ...)) body would measure
;; something else too: an assignment inside a conditional boxes its value on the
;; way through the branch merge, whatever the comparison does.
(defun %mfc-loop (n x)
  (declare (fixnum n) (double-float x))
  (let ((c 0))
    (declare (fixnum c))
    (do ((i 0 (1+ i))) ((or (= i n) (< x 4.0)) c)
      (declare (fixnum i))
      (setq c (+ c 1)))))

(defun %mfc-bytes-for (n)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%mfc-loop n 9.5d0)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: a statement about the code the compiler emits.
(deftest-compiled-only mixed-float-comparison.loop-allocates-nothing
  (progn (%mfc-bytes-for 1000)              ; warm
         ;; A box per comparison was 24 bytes, so 300k extra iterations would
         ;; show over 7 MB here.
         (< (- (%mfc-bytes-for 400000) (%mfc-bytes-for 100000)) 100000))
  t)
