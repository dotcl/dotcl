;;; The &optional and &key default of a local function (FLET, LABELS, an inline
;;; LAMBDA) was compiled in the body's value context, so a default that returns
;;; several values (TRUNCATE, FLOOR, VALUES) bound the parameter to the
;;; multiple-value carrier instead of its primary value. It printed as the
;;; number, but TYPE-OF said T, (TYPEP x 'FIXNUM) was false, and DEPOSIT-FIELD
;;; of it signalled "not an integer". Found by the random integer form test
;;; (make test-random-forms).

(defun %lfod-flet-optional (a)
  (flet ((f (&optional (x (truncate a 99))) (list x (typep x 'fixnum))))
    (f)))

(defun %lfod-labels-key (a)
  (labels ((f (&key (x (values a 7))) (list x (typep x 'fixnum))))
    (f)))

(defun %lfod-lambda-supplied (a)
  (funcall (lambda (&optional (x (floor a 2) xp)) (list x xp (typep x 'fixnum)))))

(defun %lfod-random-form (a b c d)
  (declare (ignore c))
  (flet ((%f3 (f3-1 f3-2 f3-3 &optional (f3-4 0) (f3-5 0) (f3-6 (truncate a (max 99 0))))
           (declare (ignore f3-2 f3-3 f3-4 f3-5))
           (prog2 (deposit-field f3-6 (byte 0 0) 0) f3-1)))
    (progn (%f3 b 0 0) d)))

(deftest local-fn-optional-default-single-value
  (list (%lfod-flet-optional 500) (%lfod-labels-key 5) (%lfod-lambda-supplied 5)
        (%lfod-random-form -3242107791562 447772309 -1399087977 -5343791421))
  ((5 t) (5 t) (2 nil t) -5343791421))
