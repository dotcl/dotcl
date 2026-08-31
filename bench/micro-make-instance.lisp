;;; bench/micro-make-instance.lisp -- what one MAKE-INSTANCE allocates
;;;
;;; Usage (from the project root, ALWAYS Release):
;;;   dotnet run -c Release --project runtime/runtime.csproj -- \
;;;     --asm compiler/cil-out.sil bench/micro-make-instance.lisp
;;;
;;; Allocation, not time: on this class of machine a single call's time swings by
;;; a factor of two between runs, while the bytes are exact and repeat to the
;;; tenth. Every row here has been chased with a timing hypothesis at least once
;;; and the answer was always in the bytes.
;;;
;;; What the rows are for:
;;;   plain, no initargs      the floor -- the instance and its slot vector, and
;;;                           the fast path that skips the initialization
;;;                           protocol entirely.
;;;   plain, :a 1 :b 2        the same fast path with initargs to apply.
;;;   ii:after &rest          one user method is enough to leave the fast path,
;;;                           so this row is the protocol's own cost. The &rest
;;;                           list is the user's method lambda list, not ours.
;;;   ii:after &key           the same with initargs, which is what a real class
;;;                           looks like and what cl-bench's clos/instantiate
;;;                           measures.
;;;   shared-initialize       called directly, to separate the initarg loop from
;;;                           everything MAKE-INSTANCE does around it. The step
;;;                           from 0 initargs to 1 pair used to be a per-call
;;;                           HashSet built to guard duplicate initargs.
(defmacro bytes-per (n label &body body)
  `(let ((c0 (nth 4 (dotcl:gc-stats))))
     (progn ,@body)
     (format t "~A ~,1F B/op~%" ,label (/ (- (nth 4 (dotcl:gc-stats)) c0) (float ,n)))))

(defclass mib-plain () ((a :initarg :a) (b :initarg :b)))

(defclass mib-after-rest () ((a :initarg :a) (b :initarg :b)))
(defmethod initialize-instance :after ((x mib-after-rest) &rest initargs)
  (declare (ignore initargs)) nil)

(defclass mib-after-key () ((a :initarg :a) (b :initarg :b)))
(defmethod initialize-instance :after ((x mib-after-key) &key a b)
  (declare (ignore a b)) nil)

(defparameter *n* 200000)
(defparameter *instance* (make-instance 'mib-plain))

(dotimes (i 1000)
  (make-instance 'mib-plain)
  (make-instance 'mib-plain :a 1 :b 2)
  (make-instance 'mib-after-rest)
  (make-instance 'mib-after-key :a 1 :b 2)
  (shared-initialize *instance* t)
  (shared-initialize *instance* t :a 1))

(bytes-per *n* "make-instance plain, no initargs   "
  (dotimes (i *n*) (make-instance 'mib-plain)))
(bytes-per *n* "make-instance plain, :a 1 :b 2     "
  (dotimes (i *n*) (make-instance 'mib-plain :a 1 :b 2)))
(bytes-per *n* "make-instance ii:after &rest, none "
  (dotimes (i *n*) (make-instance 'mib-after-rest)))
(bytes-per *n* "make-instance ii:after &key, :a :b "
  (dotimes (i *n*) (make-instance 'mib-after-key :a 1 :b 2)))
(bytes-per *n* "shared-initialize, 0 initargs      "
  (dotimes (i *n*) (shared-initialize *instance* t)))
(bytes-per *n* "shared-initialize, 1 initarg pair  "
  (dotimes (i *n*) (shared-initialize *instance* t :a 1)))
