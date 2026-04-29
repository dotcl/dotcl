;;; defstruct.lisp — ANSI tests for DEFSTRUCT, struct operations, setf accessors

;;; --- Basic defstruct ---

(defstruct point x y)

(deftest defstruct.make.1
  (let ((p (make-point :x 1 :y 2)))
    (point-x p))
  1)

(deftest defstruct.make.2
  (let ((p (make-point :x 10 :y 20)))
    (point-y p))
  20)

(deftest defstruct.predicate.1
  (let ((p (make-point :x 0 :y 0)))
    (notnot (point-p p)))
  t)

(deftest defstruct.predicate.2
  (point-p 42)
  nil)

(deftest defstruct.predicate.3
  (point-p nil)
  nil)

;;; --- typep integration ---

(deftest defstruct.typep.1
  (let ((p (make-point :x 1 :y 2)))
    (notnot (typep p 'point)))
  t)

(deftest defstruct.typep.2
  (typep 42 'point)
  nil)

;;; --- setf accessor ---

(deftest defstruct.setf.1
  (let ((p (make-point :x 1 :y 2)))
    (setf (point-x p) 99)
    (point-x p))
  99)

(deftest defstruct.setf.2
  (let ((p (make-point :x 1 :y 2)))
    (setf (point-y p) 42)
    (point-y p))
  42)

;;; --- copy ---

(deftest defstruct.copy.1
  (let* ((p1 (make-point :x 1 :y 2))
         (p2 (copy-point p1)))
    (list (point-x p2) (point-y p2)))
  (1 2))

(deftest defstruct.copy.2
  (let* ((p1 (make-point :x 1 :y 2))
         (p2 (copy-point p1)))
    (setf (point-x p2) 99)
    (list (point-x p1) (point-x p2)))
  (1 99))

;;; --- Default values ---

(defstruct color (red 0) (green 0) (blue 0))

(deftest defstruct.defaults.1
  (let ((c (make-color)))
    (list (color-red c) (color-green c) (color-blue c)))
  (0 0 0))

(deftest defstruct.defaults.2
  (let ((c (make-color :red 255)))
    (list (color-red c) (color-green c) (color-blue c)))
  (255 0 0))

(deftest defstruct.defaults.3
  (let ((c (make-color :red 255 :green 128 :blue 64)))
    (list (color-red c) (color-green c) (color-blue c)))
  (255 128 64))

;;; --- typecase with structs ---

(deftest defstruct.typecase
  (let ((p (make-point :x 1 :y 2)))
    (typecase p
      (point "point")
      (t "other")))
  "point")

;;; --- copy independence ---

(deftest defstruct.copy-independence
  (let* ((c1 (make-color :red 10 :green 20 :blue 30))
         (c2 (copy-color c1)))
    (setf (color-red c2) 99)
    (color-red c1))
  10)

;;; --- setf with default values ---

(deftest defstruct.setf-default
  (let ((c (make-color)))
    (setf (color-green c) 128)
    (list (color-red c) (color-green c) (color-blue c)))
  (0 128 0))

;;; --- mixed types in slots ---

(defstruct person (name "anonymous") (age 0))

(deftest defstruct.mixed-types.1
  (let ((p (make-person :name "Alice" :age 30)))
    (person-name p))
  "Alice")

(deftest defstruct.mixed-types.2
  (let ((p (make-person :name "Bob" :age 25)))
    (person-age p))
  25)

(deftest defstruct.mixed-default
  (let ((p (make-person)))
    (list (person-name p) (person-age p)))
  ("anonymous" 0))

(deftest defstruct.predicate-different-types
  (let ((p (make-point :x 1 :y 2))
        (c (make-color :red 0 :green 0 :blue 0)))
    (list (notnot (point-p p)) (point-p c)
          (notnot (color-p c)) (color-p p)))
  (t nil t nil))

(do-tests-summary)
