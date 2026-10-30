;;; SHIFTF evaluates the new value before any store; both macros pass every store value.

(defvar *srmv-log* nil)

(defun srmv-read (obj) (car obj))

(defun (setf srmv-read) (value obj)
  (push (list :store obj value) *srmv-log*)
  (setf (car obj) value))

(deftest shiftf-order.new-value-before-stores
  (let* ((a (list :a)) (b (list :b)) (c (list :c)) (*srmv-log* nil))
    (let ((r (shiftf (srmv-read (progn (push :p1 *srmv-log*) a))
                     (srmv-read (progn (push :p2 *srmv-log*) b))
                     (srmv-read (progn (push :p3 *srmv-log*) c))
                     (progn (push :new *srmv-log*) :new))))
      (list r (car a) (car b) (car c) (nreverse *srmv-log*))))
  (:a :b :c :new
      (:p1 :p2 :p3 :new
       (:store (:b) :b) (:store (:c) :c) (:store (:new) :new))))

(deftest rotatef-values.two-places
  (let ((a 1) (b 2) (c 3) (d 4))
    (list (rotatef (values a b) (values c d)) a b c d))
  (nil 3 4 1 2))

(deftest rotatef-values.three-values
  (let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
    (list (rotatef (values a b c) (values d e f)) a b c d e f))
  (nil 4 5 6 1 2 3))

(deftest shiftf-values.two-places
  (let ((a 1) (b 2) (c 3) (d 4))
    (list (shiftf (values a b) (values c d) (values 10 20 30)) a b c d))
  (1 3 4 10 20))

(deftest shiftf-values.three-values
  (let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
    (list (shiftf (values a b c) (values d e f) (values 10 20 30))
          a b c d e f))
  (1 4 5 6 10 20 30))

(deftest shiftf-values.mixed-arity
  (let ((a 1) (b 2) (c 3))
    (list (shiftf a (values b c) (values 10 20)) a b c))
  (1 2 10 20))
