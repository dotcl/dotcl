;;; (MAKE-INSTANCE <not a literal symbol> ...): a variable holding a class name or a
;;; class object. With no method of the program on MAKE-INSTANCE the default method is
;;; called directly; with one, the call goes through the generic function as before.

(defclass %midc-a () ((x :initarg :x :initform 1 :accessor %midc-x) (y :initform 2 :accessor %midc-y)))
(defclass %midc-b (%midc-a) ((z :initarg :z :initform 3 :accessor %midc-z)))

(defun %midc-sym-bad (s) (make-instance s :nope 1))
(defun %midc-sym-i (s) (make-instance s :n 5))
(defun %midc-cls-i (c) (make-instance c :n 6))
(defun %midc-make (c &rest args) (apply #'make-instance c args))
(defun %midc-sym (s) (make-instance s :x 10))
(defun %midc-cls (c) (make-instance c :x 20))

(deftest make-instance-dyn.symbol-variable
  (let ((o (%midc-sym '%midc-b)))
    (list (type-of o) (%midc-x o) (%midc-y o) (%midc-z o)))
  (%midc-b 10 2 3))

(deftest make-instance-dyn.class-object
  (let ((o (%midc-cls (find-class '%midc-b))))
    (list (type-of o) (%midc-x o) (%midc-z o)))
  (%midc-b 20 3))

(deftest make-instance-dyn.same-as-literal
  (let ((a (make-instance '%midc-b :x 7 :z 8))
        (b (%midc-make '%midc-b :x 7 :z 8))
        (c (make-instance (find-class '%midc-b) :x 7 :z 8)))
    (list (eq (class-of a) (class-of b)) (eq (class-of a) (class-of c))
          (mapcar #'%midc-x (list a b c)) (mapcar #'%midc-z (list a b c))))
  (t t (7 7 7) (8 8 8)))

(deftest make-instance-dyn.invalid-initarg-still-signals
  (list (handler-case (%midc-sym-bad '%midc-a) (error () :error))
        (handler-case (make-instance (find-class '%midc-a) :nope 1) (error () :error)))
  (:error :error))


(deftest make-instance-dyn.errors
  (list (handler-case (%midc-sym 5) (error () :error))
        (handler-case (%midc-sym '%midc-no-such-class) (error () :error))
        (handler-case (%midc-make '%midc-a :x) (error () :error)))
  (:error :error :error))

(defclass %midc-i () ((n :initarg :n :initform 0 :accessor %midc-n)))
(defvar *%midc-log* nil)
(defmethod initialize-instance :after ((o %midc-i) &rest r)
  (declare (ignore r)) (push (list :after (%midc-n o)) *%midc-log*))

(deftest make-instance-dyn.initialize-instance-after
  (progn (setf *%midc-log* nil)
         (%midc-sym-i '%midc-i) (%midc-cls-i (find-class '%midc-i))
         (reverse *%midc-log*))
  ((:after 5) (:after 6)))


(deftest make-instance-dyn.make-instance-method-on-standard-class
  (let* ((log '())
         (m (eval '(defmethod make-instance :before ((c standard-class) &rest r)
                    (declare (ignore r)) (push :before *%midc-log*)))))
    (setf *%midc-log* nil)
    (unwind-protect
         (progn (%midc-sym '%midc-a) (%midc-cls (find-class '%midc-a))
                (setf log (reverse *%midc-log*)))
      (remove-method #'make-instance m))
    (setf *%midc-log* nil)
    (%midc-sym '%midc-a)
    (list log *%midc-log*))
  ((:before :before) nil))

(deftest make-instance-dyn.make-instance-method-on-symbol
  (let* ((seen nil)
         (m (eval '(defmethod make-instance :around ((c symbol) &rest r)
                    (setf *%midc-log* (list :around c)) (apply #'call-next-method c r)))))
    (unwind-protect
         (progn (%midc-sym '%midc-a) (setf seen *%midc-log*))
      (remove-method #'make-instance m))
    seen)
  (:around %midc-a))

(defclass %midc-redef () ((a :initarg :a :initform 1)))
(deftest make-instance-dyn.redefined-class
  (let ((c (find-class '%midc-redef)))
    (let ((o1 (%midc-make '%midc-redef)))
      (eval '(defclass %midc-redef () ((a :initarg :a :initform 1) (b :initform 99))))
      (let ((o2 (%midc-make '%midc-redef)) (o3 (make-instance c)))
        (list (slot-boundp o2 'b) (slot-value o2 'b) (slot-value o3 'b) (slot-exists-p o1 'b)))))
  (t 99 99 t))
