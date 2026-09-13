;;; A &REST list nobody reads is not built.
;;;
;;; A &REST parameter binds a freshly consed list of the remaining arguments, and
;;; the emitter used to build it whether or not the body looked at it. Bodies very
;;; often do not: (defmethod initialize-instance :after ((x c) &rest initargs)
;;; (declare (ignore initargs)) ...) is the standard way to write a method that
;;; wants the protocol but not the arguments, and one is defined per level of a
;;; class hierarchy -- so a depth-10 hierarchy consed ten lists per MAKE-INSTANCE
;;; for nobody. SBCL builds none of them.
;;;
;;; The decision is made on the emitted instructions: a local no instruction reads
;;; cannot be read, whatever a macro in the body expanded to. So the tests that
;;; matter are the ones where the list IS reachable and must still be there.

;;; --- the list is still there whenever anything can see it ------------------

(defun %urc-used (a &rest r) (list a r))

(deftest unused-rest.used-rest-is-still-built
  (list (%urc-used 1)
        (%urc-used 1 2 3)
        (apply #'%urc-used 1 '(2 3)))
  ((1 nil) (1 (2 3)) (1 (2 3))))

;;; Read from a closure rather than the body proper: the parameter is captured,
;;; which is a different binding shape, and the list has to survive the call.
(defun %urc-closed (a &rest r)
  (declare (ignore a))
  (lambda () r))

(deftest unused-rest.captured-rest-is-still-built
  (funcall (%urc-closed 1 2 3))
  (2 3))

;;; Only APPLY looks at it.
(defun %urc-applied (f &rest r) (apply f r))

(deftest unused-rest.applied-rest-is-still-built
  (%urc-applied #'+ 1 2 3)
  6)

;;; --- an unused one behaves the same in every other respect -----------------

(defun %urc-unused (a &rest r) (declare (ignore r)) a)

(deftest unused-rest.value-and-arity
  (list (%urc-unused 1)
        (%urc-unused 1 2 3)
        (handler-case (%urc-unused) (program-error () :program-error)))
  (1 1 :program-error))

;;; &REST alongside &KEY: the keywords are read from the argument vector, not
;;; from the list, so dropping the list must not disturb them.
(defun %urc-key (a &rest r &key (k 10) &allow-other-keys)
  (declare (ignore r))
  (list a k))

(deftest unused-rest.rest-with-key
  (list (%urc-key 1) (%urc-key 1 :k 2) (%urc-key 1 :other 9))
  ((1 10) (1 2) (1 10)))

;;; Methods, which is where this shows up in real code.
(defclass urc-base () ((n :initarg :n :initform 0 :accessor urc-n)))
(defclass urc-derived (urc-base) ())

(defmethod initialize-instance :after ((x urc-base) &rest initargs)
  (declare (ignore initargs))
  (incf (urc-n x) 1))

(defmethod initialize-instance :after ((x urc-derived) &rest initargs)
  (declare (ignore initargs))
  (incf (urc-n x) 10))

(deftest unused-rest.method-after-chain-still-runs
  (list (urc-n (make-instance 'urc-base :n 100))
        (urc-n (make-instance 'urc-derived :n 100)))
  (101 111))

;;; --- what the change is for -------------------------------------------------

(defclass urc-c0 () ((s :initarg :s :initform 0)))
(defclass urc-c1 (urc-c0) ())
(defclass urc-c2 (urc-c1) ())
(defclass urc-c3 (urc-c2) ())
(defclass urc-c4 (urc-c3) ())

(defmethod initialize-instance :after ((x urc-c1) &rest initargs)
  (declare (ignore initargs)) nil)
(defmethod initialize-instance :after ((x urc-c2) &rest initargs)
  (declare (ignore initargs)) nil)
(defmethod initialize-instance :after ((x urc-c3) &rest initargs)
  (declare (ignore initargs)) nil)
(defmethod initialize-instance :after ((x urc-c4) &rest initargs)
  (declare (ignore initargs)) nil)

(defun %urc-make (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (make-instance 'urc-c4 :s 42)))))

;;; Four levels, one initarg pair. Each list was two conses, so the four together
;;; were a little under 200 B of the per-instance cost; the bound sits between
;;; the two so it fails on the old runtime and passes on the new. Compiled-only
;;; like the other consing assertions: an emit-free build interprets the call and
;;; has no instruction stream to look at.
(deftest-compiled-only unused-rest.method-chain-allocation
  (< (bytes-per-op #'%urc-make) 260)
  t)
