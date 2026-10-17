;;; A function whose FTYPE makes every argument and the result a FIXNUM can get
;;; a body taking raw integers. A parameter a closure captures cannot live in a
;;; raw slot, so such a function keeps the ordinary entry.

(declaim (ftype (function (fixnum) fixnum) %ndcp-read))
(defun %ndcp-read (a)
  (declare (fixnum a))
  (let ((g (lambda () (+ a 1))))
    (the fixnum (funcall g))))

(declaim (ftype (function (fixnum) fixnum) %ndcp-write))
(defun %ndcp-write (a)
  (declare (fixnum a))
  (let ((g (lambda () (setq a (+ a 1)))))
    (funcall g)
    a))

(declaim (ftype (function (fixnum fixnum) fixnum) %ndcp-two))
(defun %ndcp-two (a b)
  (declare (fixnum a b))
  (if (> a 0) (funcall (lambda () (+ a b))) b))

(deftest native-defun-captured-param.values
  (list (%ndcp-read 41) (%ndcp-write 41) (%ndcp-two 1 2) (%ndcp-two -1 2))
  (42 42 3 2))
