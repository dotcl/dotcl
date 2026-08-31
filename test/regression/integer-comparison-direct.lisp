;;; Comparing two integers does not go through the rational form.
;;;
;;; = and < fell through to the general rational comparison, which answers
;;; (an*bd) op (bn*ad) -- and an integer's denominator is 1, so both sides were a
;;; full BigInteger multiply that copies the whole magnitude to reproduce the
;;; number it started from. On a bignum that is more work than the comparison,
;;; and it allocates: ZEROP on a big coefficient did it on every call.
;;;
;;; Expected values from SBCL.

(defvar *icd-vals*
  (list 0 1 -1 7 -7 (expt 10 40) (- (expt 10 40)) (1+ (expt 2 62))
        (- (1+ (expt 2 62))) (expt 10 200) 1/3 -1/3 7/2 -7/2))

(deftest integer-comparison-direct.values
  (list (= (expt 10 40) (expt 10 40)) (= (expt 10 40) (1+ (expt 10 40)))
        (< (expt 10 40) (expt 10 41)) (> (expt 10 41) (expt 10 40))
        (= (expt 2 62) (expt 2 62)) (< (- (expt 10 40)) 0)
        (= 7 7) (= 7 7/1) (< 1/3 1) (> (expt 10 200) 7/2)
        (zerop (- (expt 10 40) (expt 10 40))))
  (t nil t t t t t t t t t))

;;; A declaration-free sweep: every pair, every operator, against the ordering
;;; the values themselves have.
(deftest integer-comparison-direct.ordering-is-consistent
  (let ((bad '()))
    (dolist (a *icd-vals* (or bad t))
      (dolist (b *icd-vals*)
        (let ((lt (< a b)) (gt (> a b)) (eq2 (= a b)))
          ;; exactly one of <, >, = holds, and the others follow from it
          (unless (and (= 1 (count t (list (and lt t) (and gt t) (and eq2 t))))
                       (eq (and (<= a b) t) (and (or lt eq2) t))
                       (eq (and (>= a b) t) (and (or gt eq2) t))
                       (eq (and (/= a b) t) (not eq2)))
            (push (list a b) bad))))))
  t)

;;; The point: comparing two bignums allocates nothing.
(defun %icd-eq-loop (n a b)
  (declare (fixnum n))
  (let ((c 0))
    (declare (fixnum c))
    (do ((i 0 (1+ i))) ((= i n) c)
      (declare (fixnum i))
      (when (= a b) (setq c (+ c 1))))))

(defun %icd-lt-loop (n a b)
  (declare (fixnum n))
  (let ((c 0))
    (declare (fixnum c))
    (do ((i 0 (1+ i))) ((= i n) c)
      (declare (fixnum i))
      (when (< a b) (setq c (+ c 1))))))

(defun %icd-bytes-for (f n)
  (let ((a (expt 7 3000)) (b (expt 7 3000)) (best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (funcall f n a b)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: a statement about the code the compiler emits.
(deftest-compiled-only integer-comparison-direct.allocates-nothing
  (progn (%icd-bytes-for #'%icd-eq-loop 100)   ; warm
         (%icd-bytes-for #'%icd-lt-loop 100)
         (let ((e (- (%icd-bytes-for #'%icd-eq-loop 5000)
                     (%icd-bytes-for #'%icd-eq-loop 1000)))
               (l (- (%icd-bytes-for #'%icd-lt-loop 5000)
                     (%icd-bytes-for #'%icd-lt-loop 1000))))
           ;; The cross-multiplication was 2160 bytes a call on these operands,
           ;; so 4000 extra calls would show over 8 MB on each.
           (list (< e 1000000) (< l 1000000))))
  (t t))
