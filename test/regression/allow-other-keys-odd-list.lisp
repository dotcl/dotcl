;;; &ALLOW-OTHER-KEYS waives unknown keywords, not an odd keyword list.

(defun aok-f1 (a &key b &allow-other-keys) (list a b))
(defun aok-f2 (&optional a &key b &allow-other-keys) (list a b))
(defun aok-f3 (&rest r &key b &allow-other-keys) (list r b))
(defun aok-f4 (&key b &allow-other-keys) b)

(defmacro aok-error-p (form)
  `(handler-case (progn ,form nil) (program-error () t)))

(deftest allow-other-keys-odd-list.odd-signals
  (list (aok-error-p (aok-f1 1 2))
        (aok-error-p (aok-f1 1 :b))
        (aok-error-p (aok-f2 1 2))
        (aok-error-p (aok-f2 :b 1 2 3))
        (aok-error-p (aok-f3 :b))
        (aok-error-p (aok-f3 1 2 3))
        (aok-error-p (aok-f4 1))
        (aok-error-p (aok-f4 :allow-other-keys))
        (aok-error-p (funcall (lambda (&key a &allow-other-keys) a) 1))
        (aok-error-p (funcall (lambda (x &key a &allow-other-keys) (list x a)) 1 2)))
  (t t t t t t t t t t))

(deftest allow-other-keys-odd-list.even-still-works
  (list (aok-f1 1 :b 2 :zz 3)
        (aok-f1 1)
        (aok-f2)
        (aok-f2 1 :zz 2 :b 4)
        (aok-f3 :b 1 :c 2)
        (aok-f4 :zz 1)
        (aok-f4 :b 7 :b 8)
        (funcall (lambda (&key a &allow-other-keys) a) :a 5 :q 6))
  ((1 2) (1 nil) (nil nil) (1 4) ((:b 1 :c 2) 1) nil 7 5))
