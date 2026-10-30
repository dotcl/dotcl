;;; A BOA constructor evaluates the initform of a typed slot its lambda list does
;;; not name once: the type check and the store read the same value.

(defvar *%dbio-n* 0)

(defstruct (%dbio-aux (:constructor %dbio-mk-aux (&aux (b 5))))
  (a (incf *%dbio-n*) :type fixnum) (b 0 :type fixnum))

(defstruct (%dbio-req (:constructor %dbio-mk-req (x)))
  x (a (incf *%dbio-n*) :type fixnum) (v (make-array 2) :type simple-vector))

(defstruct (%dbio-kw) (a (incf *%dbio-n*) :type fixnum))

(defvar *%dbio-v* 3)

(defstruct (%dbio-chk (:constructor %dbio-mk-chk ()))
  (c (progn (incf *%dbio-n*) *%dbio-v*) :type fixnum))

(deftest defstruct-boa-initform-once.count
  (list (progn (setf *%dbio-n* 0) (%dbio-aux-a (%dbio-mk-aux)))
        (progn (setf *%dbio-n* 0) (let ((s (%dbio-mk-req 1))) (list (%dbio-req-a s) *%dbio-n*)))
        (progn (setf *%dbio-n* 0) (%dbio-kw-a (make-%dbio-kw)))
        (let ((s (%dbio-mk-req 1))) (eq (%dbio-req-v s) (%dbio-req-v s))))
  (1 (1 1) 1 t))

(deftest defstruct-boa-initform-once.type-error
  (let ((*%dbio-v* "x"))
    (setf *%dbio-n* 0)
    (list (handler-case (progn (%dbio-mk-chk) :no-error)
            (type-error () :type-error))
          *%dbio-n*))
  (:type-error 1))
