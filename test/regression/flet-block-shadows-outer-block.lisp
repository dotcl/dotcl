;;; A RETURN-FROM inside a local function's body names that function's own
;;; implicit BLOCK. The free-variable analysis did not know about the implicit
;;; block, so when the local function sat inside a LAMBDA and an outer BLOCK
;;; (or an outer local function) had the same name, the LAMBDA was taken to
;;; capture the outer block, and compiling failed with "Undeclared local:
;;; BTAG_n". Found by the random integer form test (make test-random-forms).

(defun %fbsob-block ()
  (block f (funcall (lambda () (flet ((f (y) (return-from f y))) (f 4)))) 1))

(defun %fbsob-labels ()
  (labels ((f (&optional (x 0))
             (funcall (lambda () (labels ((f (y) (return-from f y))) (f 3))))
             x))
    (f)))

(defun %fbsob-outer-still-reached ()
  (block outer
    (labels ((f ()
               (funcall (lambda ()
                          (labels ((f () (return-from f 1)))
                            (f)
                            (return-from outer 7))))))
      (f))))

(defun %fbsob-default-sees-outer ()
  ;; A default is outside the function's own block.
  (list (block f (labels ((f (&optional (x (funcall (lambda () (return-from f 9)))))
                            (list x)))
                   (f)))
        2))

(defun %fbsob-random-form (c)
  (prog2 (let ((v6 (complex (labels ((%f14 (&optional (f14-1 0))
                                       (reduce (function (lambda (lmv6 lmv4)
                                                           (declare (ignore lmv6 lmv4))
                                                           (complex (labels ((%f14 (f14-1)
                                                                               (declare (ignore f14-1))
                                                                               (return-from %f14 0)))
                                                                      0)
                                                                    0)))
                                               (list f14-1 f14-1 0 f14-1 f14-1)
                                               :end 4 :from-end t)))
                              0)
                            0)))
           (declare (ignore v6))
           0)
      c))

(deftest flet-block-shadows-outer-block
  (list (%fbsob-block) (%fbsob-labels) (%fbsob-outer-still-reached)
        (%fbsob-default-sees-outer) (%fbsob-random-form -2))
  (1 0 7 (9 2) -2))
