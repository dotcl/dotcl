;;; A labels group of functions with the same required parameters, every one
;;; declaring a position FIXNUM, keeps that position in a raw int64 slot while
;;; its members tail-call each other. A position where any member lacks the
;;; declaration, captures the parameter, or binds it special stays an object.

(defvar *lmt-special* 0)

(deftest labels-mutual-raw.parity
  (labels ((ev (n) (declare (fixnum n)) (if (zerop n) t (od (1- n))))
           (od (n) (declare (fixnum n)) (if (zerop n) nil (ev (1- n)))))
    (list (ev 0) (ev 1) (ev 1000) (ev 1001)))
  (t nil t nil))

(deftest labels-mutual-raw.depth-one-million
  (labels ((ev (n) (declare (fixnum n)) (if (zerop n) t (od (1- n))))
           (od (n) (declare (fixnum n)) (if (zerop n) nil (ev (1- n)))))
    (list (ev 1000000) (ev 1000001)))
  (t nil))

(deftest labels-mutual-raw.two-positions
  (labels ((f (n acc) (declare (fixnum n acc)) (if (zerop n) acc (g (1- n) (+ acc 2))))
           (g (n acc) (declare (fixnum n acc)) (if (zerop n) acc (f (1- n) (+ acc 3)))))
    (list (f 10 0) (f 1000000 0)))
  (25 2500000))

(deftest labels-mutual-raw.one-position-undeclared
  (labels ((f (n acc) (declare (fixnum n)) (if (zerop n) acc (g (1- n) (cons n acc))))
           (g (n acc) (declare (fixnum n)) (if (zerop n) acc (f (1- n) acc))))
    (length (f 100 nil)))
  50)

(deftest labels-mutual-raw.one-function-undeclared
  (labels ((ev (n) (if (zerop n) t (od (1- n))))
           (od (n) (declare (fixnum n)) (if (zerop n) nil (ev (1- n)))))
    (list (ev 1000) (ev 7)))
  (t nil))

;; Overflow boundary: the new value of the parameter is checked as it is
;; stored, as a self tail call into a declared parameter checks it.
(deftest labels-mutual-raw.most-positive-fixnum
  (labels ((f (n) (declare (fixnum n)) (if (< n (- most-positive-fixnum 3)) (g (+ n 1)) n))
           (g (n) (declare (fixnum n)) (if (< n (- most-positive-fixnum 3)) (f (+ n 1)) n)))
    (list (f (- most-positive-fixnum 5)) (f most-positive-fixnum)
          (g (- most-positive-fixnum 2))))
  (#.(- most-positive-fixnum 3) #.most-positive-fixnum #.(- most-positive-fixnum 2)))

(deftest labels-mutual-raw.most-negative-fixnum
  (labels ((f (n) (declare (fixnum n)) (if (> n (+ most-negative-fixnum 3)) (g (- n 1)) n))
           (g (n) (declare (fixnum n)) (if (> n (+ most-negative-fixnum 3)) (f (- n 1)) n)))
    (f (+ most-negative-fixnum 6)))
  #.(+ most-negative-fixnum 3))

;; A declaration that lies is a type error where the argument is stored.
(deftest labels-mutual-raw.lying-declaration
  (labels ((ev (n) (declare (fixnum n)) (if (eql n 0) t (od (identity "x"))))
           (od (n) (declare (fixnum n)) (if (eql n 0) nil (ev (1- n)))))
    (handler-case (ev 3) (type-error () :type-error)))
  :type-error)

(deftest labels-mutual-raw.lying-bignum
  (labels ((ev (n) (declare (fixnum n)) (if (eql n 0) t (od (identity (1+ most-positive-fixnum)))))
           (od (n) (declare (fixnum n)) (if (eql n 0) nil (ev (1- n)))))
    (handler-case (ev 3) (error () :error)))
  :error)

;; The parameter read in generic positions, values, closure capture.
(deftest labels-mutual-raw.generic-reads
  (labels ((f (n) (declare (fixnum n)) (if (zerop n) (list n (princ-to-string n)) (g (1- n))))
           (g (n) (declare (fixnum n)) (if (zerop n) (values n n) (f (1- n)))))
    (list (multiple-value-list (f 4)) (multiple-value-list (g 4))))
  (((0 "0")) (0 0)))

(deftest labels-mutual-raw.captured-parameter
  (labels ((f (n) (declare (fixnum n)) (if (zerop n) (funcall (lambda () n)) (g (1- n))))
           (g (n) (declare (fixnum n)) (if (< n 3) (funcall (lambda () (* n 10))) (f (1- n)))))
    (list (f 5) (f 2)))
  (20 10))

(deftest labels-mutual-raw.setq-parameter
  (labels ((f (n) (declare (fixnum n)) (setq n (+ n 1)) (if (> n 10) n (g n)))
           (g (n) (declare (fixnum n)) (f (+ n 1))))
    (f 0))
  11)

(deftest labels-mutual-raw.special-binding
  (labels ((f (n) (declare (fixnum n))
             (let ((*lmt-special* n)) (if (zerop n) *lmt-special* (g (1- n)))))
           (g (n) (declare (fixnum n)) (f n)))
    (list (f 3) *lmt-special*))
  (0 0))

(deftest labels-mutual-raw.catch-and-unwind-protect
  (let ((log '()))
    (labels ((f (n) (declare (fixnum n))
               (if (zerop n) (throw 'tag n)
                   (unwind-protect (g (1- n)) (push n log))))
             (g (n) (declare (fixnum n)) (f n)))
      (list (catch 'tag (f 4)) (nreverse log))))
  (0 (1 2 3 4)))

(deftest labels-mutual-raw.tail-inside-catch
  (labels ((f (n) (declare (fixnum n))
             (if (zerop n) :done (catch 'x (g (1- n)))))
           (g (n) (declare (fixnum n)) (f n)))
    (f 1000))
  :done)

(deftest labels-mutual-raw.non-tail-entry-and-funcall
  (labels ((f (n) (declare (fixnum n)) (if (zerop n) 0 (g (1- n))))
           (g (n) (declare (fixnum n)) (if (zerop n) 1 (f (1- n)))))
    (list (+ 1 (f 10)) (funcall #'g 5) (mapcar #'f '(1 2 3))))
  (1 0 (1 0 1)))
