;;; A two-value return carries its values in the MvReturn itself instead of an
;;; array. (VALUES A B) is what TRUNCATE, FLOOR, ROUND, GETHASH and INTERN return,
;;; and the array was 40 of the 64 bytes such a return allocated.
;;;
;;; The values now reach their reader through three different routes -- the object's
;;; own fields, the thread state published without an array, and (for three or more)
;;; the array as before -- so these tests pin the behaviour every route must agree on.

(defun mvp-two () (values 1 2))
(defun mvp-three () (values 1 2 3))
(defun mvp-one () (values 42))
(defun mvp-none () (values))

(deftest mv-pair-return.multiple-value-list
  (list (multiple-value-list (mvp-none))
        (multiple-value-list (mvp-one))
        (multiple-value-list (mvp-two))
        (multiple-value-list (mvp-three)))
  (nil (42) (1 2) (1 2 3)))

(deftest mv-pair-return.multiple-value-bind
  (list (multiple-value-bind (a b) (mvp-two) (list a b))
        (multiple-value-bind (a b c) (mvp-two) (list a b c))     ; missing value is NIL
        (multiple-value-bind (a) (mvp-two) a)
        (multiple-value-bind (a b c) (mvp-three) (list a b c))
        (multiple-value-bind (a b) (mvp-none) (list a b)))
  ((1 2) (1 2 nil) 1 (1 2 3) (nil nil)))

(deftest mv-pair-return.primary-in-single-value-context
  (list (+ (mvp-two) 0) (list (mvp-two)) (if (mvp-two) :yes :no))
  (1 (1) :yes))

(deftest mv-pair-return.nth-value
  (list (nth-value 0 (mvp-two)) (nth-value 1 (mvp-two)) (nth-value 2 (mvp-two))
        (nth-value 2 (mvp-three)))
  (1 2 nil 3))

(deftest mv-pair-return.multiple-value-call
  (list (multiple-value-call #'list (mvp-two) (mvp-three))
        (multiple-value-call #'+ (mvp-two)))
  ((1 2 1 2 3) 3))

;;; The two-value shape from the standard functions.
(deftest mv-pair-return.standard-two-value-functions
  (list (multiple-value-list (floor 7 2))
        (multiple-value-list (truncate -7 2))
        (multiple-value-list (round 7 2))
        (let ((h (make-hash-table))) (setf (gethash :k h) 42) (multiple-value-list (gethash :k h)))
        (let ((h (make-hash-table))) (multiple-value-list (gethash :missing h))))
  ((3 1) (-3 -1) (4 -1) (42 t) (nil nil)))

;;; UNWIND-PROTECT: the cleanup runs between producing the values and reading them,
;;; and may produce values of its own. The body's must survive that.
(deftest mv-pair-return.unwind-protect-preserves-values
  (list (multiple-value-list
         (unwind-protect (mvp-two) (mvp-three)))
        (multiple-value-list
         (unwind-protect (values :a :b) (floor 9 2)))
        (multiple-value-bind (a b) (unwind-protect (mvp-two) (values :x :y :z))
          (list a b)))
  ((1 2) (:a :b) (1 2)))

;;; Values that pass through a generic function and a closure keep their count.
(defgeneric mvp-gf (x))
(defmethod mvp-gf ((x integer)) (values x (* x 2)))

(deftest mv-pair-return.through-gf-and-closure
  (list (multiple-value-list (mvp-gf 3))
        (multiple-value-list (funcall (lambda () (mvp-two))))
        (multiple-value-list (apply #'mvp-two '())))
  ((3 6) (1 2) (1 2)))

;;; VALUES-LIST and a values form in tail position of several special forms.
(deftest mv-pair-return.values-list-and-tail-positions
  (list (multiple-value-list (values-list '(1 2)))
        (multiple-value-list (let ((x 1)) (values x (1+ x))))
        (multiple-value-list (progn (mvp-none) (mvp-two)))
        (multiple-value-list (block b (return-from b (values 7 8))))
        (multiple-value-list (catch 'c (throw 'c (values 5 6)))))
  ((1 2) (1 2) (1 2) (7 8) (5 6)))
