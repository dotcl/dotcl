;;; A one-argument generic-function call reaches dispatch through the GF's arity-1
;;; entry point, which does not build an argument array. Only the plain shape runs
;;; there -- a warm cache entry, one primary method, nothing around it -- and
;;; everything else falls back to the array-taking dispatcher.
;;;
;;; These pin the contracts that the fast path has to keep on its own, because it
;;; sets up the next-method state itself rather than going through the common
;;; combination code: what CALL-NEXT-METHOD and NEXT-METHOD-P see when there is no
;;; next method, and the shapes that must NOT take the fast path (chains,
;;; :around/:before/:after, EQL specializers, a cache invalidated by a new method).
;;;
;;; The arguments of the invocation are published lazily on this path, so the
;;; CALL-NEXT-METHOD cases are also checking that the list is materialised when a
;;; method body actually asks for it.

(defclass gfa1-a () ())
(defclass gfa1-b (gfa1-a) ())
(defvar *gfa1-a* (make-instance 'gfa1-a))
(defvar *gfa1-b* (make-instance 'gfa1-b))

(defgeneric gfa1-single (x))
(defmethod gfa1-single ((x gfa1-a)) (list :a (next-method-p)))

(deftest gf-arity1.single-primary
  (list (gfa1-single *gfa1-a*) (gfa1-single *gfa1-b*))
  ((:a nil) (:a nil)))

;;; Repeating the call runs it warm, i.e. through the cache entry the fast path
;;; reads, rather than through the miss path that fills it.
(deftest gf-arity1.single-primary-warm
  (let ((r nil))
    (dotimes (i 50) (setf r (gfa1-single *gfa1-a*)))
    r)
  (:a nil))

(defgeneric gfa1-cnm (x))
(defmethod gfa1-cnm ((x gfa1-a))
  (handler-case (call-next-method) (error () :no-next-method)))

(deftest gf-arity1.call-next-method-with-no-next
  (let ((r nil))
    (dotimes (i 50) (setf r (gfa1-cnm *gfa1-a*)))
    r)
  :no-next-method)

;;; A real chain: the more specific method continues into the less specific one,
;;; and the arguments carry over without being restated.
(defgeneric gfa1-chain (x))
(defmethod gfa1-chain ((x gfa1-a)) (list :a))
(defmethod gfa1-chain ((x gfa1-b)) (cons :b (call-next-method)))

(deftest gf-arity1.chain
  (let ((r nil))
    (dotimes (i 50) (setf r (list (gfa1-chain *gfa1-b*) (gfa1-chain *gfa1-a*))))
    r)
  ((:b :a) (:a)))

;;; CALL-NEXT-METHOD with an explicit argument passes that one on instead.
(defgeneric gfa1-cnm-args (x))
(defmethod gfa1-cnm-args ((x gfa1-a)) (list :a x))
(defmethod gfa1-cnm-args ((x gfa1-b)) (call-next-method (make-instance 'gfa1-b)))

(deftest gf-arity1.call-next-method-with-arguments
  (let ((r (gfa1-cnm-args *gfa1-b*)))
    (list (first r) (typep (second r) 'gfa1-b) (eq (second r) *gfa1-b*)))
  (:a t nil))

;;; Auxiliary methods: the fast path must decline, and the combination order stays
;;; :around, :before, primary, :after.
(defvar *gfa1-log* nil)
(defgeneric gfa1-aux (x))
(defmethod gfa1-aux ((x gfa1-a)) (push :primary *gfa1-log*) :p)
(defmethod gfa1-aux :before ((x gfa1-a)) (push :before *gfa1-log*))
(defmethod gfa1-aux :after ((x gfa1-a)) (push :after *gfa1-log*))
(defmethod gfa1-aux :around ((x gfa1-a)) (push :around *gfa1-log*) (call-next-method))

(deftest gf-arity1.auxiliary-methods
  (let ((*gfa1-log* nil))
    (dotimes (i 3) (gfa1-aux *gfa1-a*))
    (list (gfa1-aux *gfa1-a*) (reverse *gfa1-log*)))
  (:p (:around :before :primary :after
       :around :before :primary :after
       :around :before :primary :after
       :around :before :primary :after)))

;;; EQL specializers are decided per call, so a warm entry must not shortcut them.
(defgeneric gfa1-eql (x))
(defmethod gfa1-eql ((x (eql 5))) :five)
(defmethod gfa1-eql ((x integer)) :int)

(deftest gf-arity1.eql-specializer
  (let ((r nil))
    (dotimes (i 50) (setf r (list (gfa1-eql 5) (gfa1-eql 6))))
    r)
  (:five :int))

;;; A method added after the call site is warm must take effect.
(defgeneric gfa1-redef (x))
(defmethod gfa1-redef ((x gfa1-a)) :old)

(deftest gf-arity1.method-added-while-warm
  (progn
    (dotimes (i 50) (gfa1-redef *gfa1-b*))
    (eval '(defmethod gfa1-redef ((x gfa1-b)) :new))
    (gfa1-redef *gfa1-b*))
  :new)

;;; No applicable method still signals, warm or cold.
(deftest gf-arity1.no-applicable-method
  (handler-case (gfa1-single 42) (error () :error))
  :error)

;;; APPLY and FUNCALL reach the same methods as a direct call.
(deftest gf-arity1.apply-and-funcall
  (list (funcall #'gfa1-chain *gfa1-b*)
        (apply #'gfa1-chain (list *gfa1-b*))
        (funcall (symbol-function 'gfa1-chain) *gfa1-a*))
  ((:b :a) (:b :a) (:a)))

;;; Two and three arguments take the same entry points, so the same contracts have
;;; to hold with more than one argument in play -- in particular CALL-NEXT-METHOD
;;; with no arguments, which is where the deferred argument list is materialised.

(defgeneric gfa1-two (x y))
(defmethod gfa1-two ((x gfa1-a) (y gfa1-a)) (list :aa))
(defmethod gfa1-two ((x gfa1-b) (y gfa1-a)) (cons :ba (call-next-method)))

(deftest gf-arity2.dispatch-on-both-arguments
  (let ((r nil))
    (dotimes (i 50)
      (setf r (list (gfa1-two *gfa1-a* *gfa1-a*)
                    (gfa1-two *gfa1-b* *gfa1-a*)
                    (gfa1-two *gfa1-a* *gfa1-b*))))
    r)
  ((:aa) (:ba :aa) (:aa)))

;;; The second argument decides, so a warm entry for one class must not answer for
;;; another.
(defgeneric gfa1-second (x y))
(defmethod gfa1-second ((x gfa1-a) (y gfa1-a)) :a)
(defmethod gfa1-second ((x gfa1-a) (y gfa1-b)) :b)

(deftest gf-arity2.second-argument-decides
  (let ((r nil))
    (dotimes (i 50)
      (setf r (list (gfa1-second *gfa1-a* *gfa1-a*) (gfa1-second *gfa1-a* *gfa1-b*))))
    r)
  (:a :b))

(defgeneric gfa1-two-cnm (x y))
(defmethod gfa1-two-cnm ((x gfa1-a) (y gfa1-a))
  (handler-case (call-next-method) (error () :no-next-method)))

(deftest gf-arity2.call-next-method-with-no-next
  (let ((r nil))
    (dotimes (i 50) (setf r (gfa1-two-cnm *gfa1-a* *gfa1-a*)))
    r)
  :no-next-method)

(defgeneric gfa1-three (x y z))
(defmethod gfa1-three ((x gfa1-a) y z) (list :a y z))
(defmethod gfa1-three ((x gfa1-b) y z) (cons :b (call-next-method)))

(deftest gf-arity3.chain-passes-all-arguments
  (let ((r nil))
    (dotimes (i 50) (setf r (list (gfa1-three *gfa1-a* 1 2) (gfa1-three *gfa1-b* 3 4))))
    r)
  ((:a 1 2) (:b :a 3 4)))

;;; A SETF accessor is a two-argument generic function.
(defclass gfa1-slots () ((v :initarg :v :accessor gfa1-v)))

(deftest gf-arity2.setf-accessor
  (let ((o (make-instance 'gfa1-slots :v 1)))
    (dotimes (i 50) (setf (gfa1-v o) i))
    (list (gfa1-v o) (progn (setf (gfa1-v o) :last) (gfa1-v o))))
  (49 :last))

;;; Calling a generic function with the wrong number of arguments signals, warm or
;;; cold. The loose-argument entry points are reached BEFORE the dispatcher that
;;; checks this, and a cache entry records only the arguments dispatch looked at --
;;; so a one-argument generic function called with two would otherwise find its
;;; entry matching on the first argument and run the method with an argument too
;;; many. (ANSI's METHOD-QUALIFIERS.ERROR.2 and friends are the same case on the
;;; built-in CLOS generic functions.)

(defgeneric gfa1-one-only (x))
(defmethod gfa1-one-only ((x gfa1-a)) :one)

(deftest gf-arity.too-many-arguments
  (progn
    (dotimes (i 50) (gfa1-one-only *gfa1-a*))
    (handler-case (funcall #'gfa1-one-only *gfa1-a* *gfa1-a*)
      (program-error () :program-error)
      (error () :other)))
  :program-error)

(deftest gf-arity.too-few-arguments
  (progn
    (dotimes (i 50) (gfa1-two *gfa1-a* *gfa1-a*))
    (handler-case (funcall #'gfa1-two *gfa1-a*)
      (program-error () :program-error)
      (error () :other)))
  :program-error)

;;; &optional and &rest widen what is in range, and both stay in range warm.
(defgeneric gfa1-opt (x &optional y))
(defmethod gfa1-opt ((x gfa1-a) &optional y) (list :opt y))

(defgeneric gfa1-rest (x &rest r))
(defmethod gfa1-rest ((x gfa1-a) &rest r) (cons :rest r))

(deftest gf-arity.optional-and-rest
  (let ((r nil))
    (dotimes (i 50)
      (setf r (list (gfa1-opt *gfa1-a*) (gfa1-opt *gfa1-a* 5)
                    (gfa1-rest *gfa1-a*) (gfa1-rest *gfa1-a* 1 2))))
    r)
  ((:opt nil) (:opt 5) (:rest) (:rest 1 2)))
