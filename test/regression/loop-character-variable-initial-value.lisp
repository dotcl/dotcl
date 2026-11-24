;;; A LOOP variable declared a character type without an initial value starts as
;;; a character, as one declared a number type starts as zero.

(deftest loop-char-init.of-type-character
  (list (characterp (loop with a of-type character return a))
        (characterp (loop with a of-type base-char return a))
        (typep (loop with a of-type standard-char return a) 'standard-char)
        (loop with a of-type fixnum return a)
        (loop with a of-type double-float return a))
  (t t t 0 0.0d0))

(deftest loop-char-init.destructuring-and-given-value
  (list (let ((r (loop with (a b) of-type (character fixnum) return (list a b))))
          (list (characterp (first r)) (second r)))
        (loop with a of-type character = #\q return a))
  ((t 0) #\q))
