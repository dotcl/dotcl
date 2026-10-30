;;; CLHS 7.1.2: an invalid initarg signals PROGRAM-ERROR.

(defclass kw-cc-old () ((x :initarg :x)))
(defclass kw-cc-new () ((y :initarg :y)))

(deftest change-class.invalid-initarg-program-error
  (let ((i (make-instance 'kw-cc-old :x 1)))
    (handler-case (progn (change-class i 'kw-cc-new :bogus 2) :returned)
      (program-error () :program-error)))
  :program-error)

(deftest change-class.invalid-initarg-allow-other-keys
  (let ((i (make-instance 'kw-cc-old :x 1)))
    (change-class i 'kw-cc-new :bogus 2 :allow-other-keys t)
    (class-name (class-of i)))
  kw-cc-new)

(deftest change-class.duplicate-initarg-first-wins
  (let ((i (make-instance 'kw-cc-old :x 1)))
    (change-class i 'kw-cc-new :y 1 :y 2)
    (slot-value i 'y))
  1)

(deftest make-instance.invalid-initarg-program-error
  (handler-case (progn (make-instance 'kw-cc-old :bogus 2) :returned)
    (program-error () :program-error))
  :program-error)
