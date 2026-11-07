;;; PRIN1 of a float reads back as the same float (CLHS 22.1.3.1.3), also for the values at a power
;;; of two where .NET's shortest "R" text drops a digit: (scale-float 1d0 -25) printed as
;;; 2.980232238769531e-8, which reads as the neighbouring double.

(defun %fprt-bad-doubles (exponents)
  (let ((*read-default-float-format* 'double-float) (bad '()))
    (dolist (e exponents (nreverse bad))
      (let* ((x (scale-float 1d0 e)) (s (prin1-to-string x)))
        (unless (eql (read-from-string s) x) (push (list e s) bad))))))

(deftest float-print-round-trip.powers-of-two-double
  (%fprt-bad-doubles (loop for e from -1070 to 1020 collect e))
  nil)

(deftest float-print-round-trip.single-and-random
  (let ((*read-default-float-format* 'single-float) (bad '()))
    (dotimes (e 250)
      (let* ((x (scale-float 1.0f0 (- e 125))) (s (prin1-to-string x)))
        (unless (eql (read-from-string s) x) (push s bad))))
    (let ((*read-default-float-format* 'double-float) (state (make-random-state nil)))
      (dotimes (i 3000)
        (let* ((x (* (random 1d0 state) (expt 10d0 (- (random 40 state) 20)))) (s (prin1-to-string x)))
          (unless (eql (read-from-string s) x) (push s bad)))))
    bad)
  nil)

(deftest float-print-round-trip.ordinary-text-unchanged
  (let ((*read-default-float-format* 'double-float))
    (list (prin1-to-string 1d0) (prin1-to-string 0.1d0) (prin1-to-string 100d0)
          (prin1-to-string 1d-5) (prin1-to-string 5d-324) (prin1-to-string 1.5d10)))
  ("1.0" "0.1" "100.0" "1.0e-5" "5.0e-324" "1.5e10"))
