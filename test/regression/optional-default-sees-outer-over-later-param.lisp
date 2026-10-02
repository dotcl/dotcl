;;; An &optional default that names a variable which a LATER &rest or &key
;;; parameter of the same lambda list rebinds. The default is evaluated before
;;; that parameter is bound, so it must read the enclosing binding. The
;;; compiler hid only the not-yet-bound optionals, so the name resolved to the
;;; &key/&rest local, which had not been declared yet, and compilation failed
;;; with "Undeclared local: KEY1_5". Found by the random integer form test
;;; (make test-random-forms).

(defun %odso-key (key1)
  (flet ((h (&optional (x key1) &key (key1 0)) (list x key1)))
    (list (h) (h 7 :key1 8))))

(defun %odso-rest (key1)
  (flet ((h (&optional (x key1) &rest key1) (list x key1)))
    (list (h) (h 7 8 9))))

(defun %odso-required-first (key1)
  (flet ((h (a &optional (x key1) &key (key1 0)) (list a x key1)))
    (h 1)))

(defun %odso-lambda (key1)
  (funcall (lambda (&optional (x key1) &key (key1 0)) (list x key1 (funcall (lambda () key1))))))

(deftest optional-default-sees-outer-over-later-param
  (list (%odso-key 5) (%odso-rest 5) (%odso-required-first 5) (%odso-lambda 5))
  (((5 0) (7 8)) ((5 nil) (7 (8 9))) (1 5 0) (5 0 0)))
