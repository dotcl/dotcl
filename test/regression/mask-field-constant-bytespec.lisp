;;; (mask-field (byte SIZE POS) X) and (deposit-field V (byte SIZE POS) X)
;;; with literal SIZE and POS are compiled as masks instead of calls. These
;;; check that the open-coded forms give the same values as the functions,
;;; that V is still evaluated before X, and that a non-integer is rejected.

(defun %mfc-inline (v x)
  (list (mask-field (byte 8 0) x)
        (mask-field (byte 6 18) x)
        (mask-field (byte 0 5) x)
        (mask-field (byte 64 3) x)
        (mask-field (byte 100 7) x)
        (deposit-field v (byte 8 0) x)
        (deposit-field v (byte 6 18) x)
        (deposit-field v (byte 0 5) x)
        (deposit-field v (byte 64 3) x)
        (deposit-field v (byte 100 7) x)))

(defun %mfc-generic (v x)
  (let ((m #'mask-field) (d #'deposit-field))
    (list (funcall m (byte 8 0) x)
          (funcall m (byte 6 18) x)
          (funcall m (byte 0 5) x)
          (funcall m (byte 64 3) x)
          (funcall m (byte 100 7) x)
          (funcall d v (byte 8 0) x)
          (funcall d v (byte 6 18) x)
          (funcall d v (byte 0 5) x)
          (funcall d v (byte 64 3) x)
          (funcall d v (byte 100 7) x))))

(deftest mask-field-constant-bytespec.matches-function
  (let ((vals (list 0 1 -1 255 #xABCD #x12345678 -12345
                    most-positive-fixnum most-negative-fixnum
                    (expt 2 70) (- (expt 3 50)))))
    (loop for v in vals
          nconc (loop for x in vals
                      unless (equal (%mfc-inline v x) (%mfc-generic v x))
                        collect (list v x))))
  nil)

;; The shape of ironclad's DES key schedule: a field of a 32-bit word, shifted.
(defun %mfc-des (w)
  (declare (type (unsigned-byte 32) w))
  (logior (ash (mask-field (byte 6 18) w) 6)
          (ash (mask-field (byte 6 6) w) -6)))

(deftest mask-field-constant-bytespec.word
  (list (%mfc-des #xFFFFFFFF) (%mfc-des 0) (%mfc-des #x00FC0FC0))
  (#x3F00003F 0 #x3F00003F))

(deftest mask-field-constant-bytespec.evaluation-order
  (let ((log '()))
    (deposit-field (progn (push :v log) 1) (byte 8 8) (progn (push :x log) 0))
    (nreverse log))
  (:v :x))

(deftest mask-field-constant-bytespec.not-an-integer
  (list (handler-case (progn (mask-field (byte 8 0) (the t (identity 1.5))) :no-error)
          (type-error () :type-error))
        (handler-case (progn (deposit-field (the t (identity 1.5)) (byte 8 0) 0) :no-error)
          (type-error () :type-error))
        (handler-case (progn (deposit-field 1 (byte 8 0) (the t (identity "x"))) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error :type-error))
