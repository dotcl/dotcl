;;; (< (1+ i) n), (<= (+ i k) n), (> n (- i k)) ... of fixnum variables compare the sum
;;; as the integer it is, also when it leaves the fixnum range: no wrapped result.

(defun %scf-lt (i n) (declare (fixnum i n)) (if (< (1+ i) n) t nil))
(defun %scf-all (a b c)
  (declare (fixnum a b c))
  (list (< (+ a b) c) (<= (+ a b) c) (> (+ a b) c) (>= (+ a b) c) (= (+ a b) c) (/= (+ a b) c)
        (< (- a b) c) (<= (- a b) c) (> (- a b) c) (>= (- a b) c) (= (- a b) c) (/= (- a b) c)
        (< c (+ a b)) (>= c (+ a b)) (> c (- a b)) (<= c (- a b))
        (< (1+ a) c) (> (1- a) c) (< (+ a 3) c) (<= (- a 3) c)))
(defun %scf-value (a b c) (declare (fixnum a b c)) (list (< (+ a b) c) (>= c (- a b))))

(defun %scf-generic (a b c)
  (list (< (+ a b) c) (<= (+ a b) c) (> (+ a b) c) (>= (+ a b) c) (= (+ a b) c) (/= (+ a b) c)
        (< (- a b) c) (<= (- a b) c) (> (- a b) c) (>= (- a b) c) (= (- a b) c) (/= (- a b) c)
        (< c (+ a b)) (>= c (+ a b)) (> c (- a b)) (<= c (- a b))
        (< (1+ a) c) (> (1- a) c) (< (+ a 3) c) (<= (- a 3) c)))

(deftest sum-compare-fixnum.same-as-generic
  (let ((m most-positive-fixnum) (n most-negative-fixnum))
    (loop for a in (list 0 1 -1 5 m (1- m) n (1+ n) 1000000)
          append (loop for b in (list 0 1 -1 7 m n (1- m) (1+ n))
                       append (loop for c in (list 0 1 -1 8 m n (1- m) (1+ n) 12)
                                    unless (equal (%scf-all a b c) (%scf-generic a b c))
                                      collect (list a b c)))))
  nil)

(deftest sum-compare-fixnum.edges
  (let ((m most-positive-fixnum) (n most-negative-fixnum))
    (list (%scf-lt 5 10) (%scf-lt 9 10) (%scf-lt m m) (%scf-lt (1- m) m) (%scf-lt n m)
          (%scf-value m 1 m) (%scf-value n -1 n) (%scf-value m m m)))
  (t nil nil nil t (nil t) (t nil) (nil t)))
