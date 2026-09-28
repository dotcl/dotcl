;;; The printer's list nesting counter is per thread. It was one counter shared
;;; by every thread, bumped and restored with a plain ++/--, so threads printing
;;; nested lists at the same time lost updates and left it off by some amount
;;; for good. Once it had drifted, printing on any thread with *PRINT-LEVEL* set
;;; elided lists that were well within the level: (let ((*print-level* 4))
;;; (prin1-to-string '(a b))) gave "#".

(defun pdt-print-a-lot (n)
  (let ((tree '(1 (2 (3 (4 (5 (6 (7 (8)))))))))
        (count 0))
    (dotimes (i n count)
      (incf count (length (prin1-to-string tree))))))

(deftest printer-depth-threads.no-drift
  (let ((threads (loop repeat 8
                       collect (dotcl:make-thread
                                (lambda () (pdt-print-a-lot 4000))))))
    (mapc (function dotcl:thread-join) threads)
    (list (let ((*print-level* 4)) (prin1-to-string '(a b)))
          (let ((*print-level* 2)) (prin1-to-string '(a (b (c)))))
          (prin1-to-string '(a (b (c))))))
  ("(A B)" "(A (B #))" "(A (B (C)))"))
