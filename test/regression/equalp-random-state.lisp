;;; EQUALP on random states compares the generator state (SBCL agrees).

(deftest equalp-random-state-copies
  (let* ((r (make-random-state t))
         (a (make-random-state r))
         (b (make-random-state r)))
    (list (equalp a b)
          (progn (random 1000 a) (equalp a b))
          (progn (random 1000 b) (equalp a b))))
  (t nil t))
