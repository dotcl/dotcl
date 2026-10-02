;;; A LET* variable that is both assigned and captured by a closure lives in a
;;; one-element array. The array and its index were pushed before the initial
;;; value was computed; when that value came from a loop (DOTIMES, LOOP, DO, a
;;; bare TAGBODY), the loop's LEAVE emptied the evaluation stack under them and
;;; the method failed with "Common Language Runtime detected an invalid
;;; program". LET was not affected. An initial value that opens a protected
;;; region (HANDLER-BIND, HANDLER-CASE, UNWIND-PROTECT) failed the same way.
;;; Found by the random integer form test (make test-random-forms).

(defun %lsbl-tagbody ()
  (let* ((v (tagbody))) (setq v 0) (funcall (lambda () v))))

(defun %lsbl-dotimes ()
  (let* ((w 1) (v (dotimes (i 3 10)))) (setq v (+ v 1)) (funcall (lambda () (+ w v)))))

(defun %lsbl-loop ()
  (let* ((v (loop for i below 4 sum i))) (incf v) (funcall (lambda () v))))

(defun %lsbl-random-form (a b c d)
  ;; The shape the random form test reported.
  (declare (ignore a b c d))
  (let* ((v4 (dotimes (iv1 0 0) (progn 0))))
    (dotimes (iv3 0 0)
      (deposit-field (setq v4 0) (byte 0 0)
                     (reduce #'(lambda (lmv2 lmv6) (declare (ignore lmv2 lmv6)) v4)
                             (list 0 iv3 0 v4 iv3))))))

(defun %lsbl-protected ()
  (list (let* ((v (handler-bind () 1))) (setq v 2) (funcall (lambda () v)))
        (let* ((v (handler-case 1 (error () 3)))) (setq v 2) (funcall (lambda () v)))
        (let* ((v (unwind-protect 1))) (incf v) (funcall (lambda () v)))))

(defun %lsbl-random-form-2 (a b c d)
  ;; The second shape the random form test reported: the INCF is never
  ;; reached, but it still makes V7 a boxed variable.
  (declare (ignore a b))
  (let* ((v7 (handler-bind () c)))
    (if t
        (count (unwind-protect 0
                 (labels ((%f4 (f4-1 f4-2 f4-3 &optional (f4-4 0) (f4-5 v7) (f4-6 d))
                            (declare (ignore f4-1 f4-2 f4-3 f4-4 f4-5 f4-6))
                            0))
                   0))
               #(106830 2951620 33554307 28282976 8189 6 32768 32767 11798049 5413908 0
                 18411212 2 30282454 2097159 33554423 32675718 32261876 6815817
                 25151250 131065 18526594 0 262138 2463)
               :test '<)
        (incf v7))))

(deftest let-star-boxed-protected-init
  (list (%lsbl-protected)
        (%lsbl-random-form-2 -7389840738282 -6561 -58420 -17672638628298))
  ((2 2 2) 23))

(deftest let-star-boxed-loop-init
  (list (%lsbl-tagbody) (%lsbl-dotimes) (%lsbl-loop)
        (%lsbl-random-form 164 -1006751537671614968 2493396381690435991 9337461665))
  (0 12 7 0))
