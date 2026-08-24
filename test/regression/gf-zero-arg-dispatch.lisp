;;; A generic function with no required parameters dispatches on nothing, so its
;;; applicable method set is the same for every call and belongs in the dispatch
;;; cache like any other. The cache key was built as "at least one class slot",
;;; which no zero-argument call can fill, so the entry never matched: every call
;;; recomputed the applicable methods and re-sorted them (640 B/call, an order of
;;; magnitude above a 1-argument generic function). These tests pin the behaviour
;;; that the now-always-matching cache entry has to preserve.

(defgeneric gz-plain ())
(defmethod gz-plain () :plain)

(deftest gf-zero-arg.repeated-calls-agree
  (list (gz-plain) (gz-plain) (gz-plain))
  (:plain :plain :plain))

;;; Auxiliary methods still run on a cache hit.
(defvar *gz-trace* '())
(defgeneric gz-aux ())
(defmethod gz-aux () (push :primary *gz-trace*) :primary)
(defmethod gz-aux :before () (push :before *gz-trace*))
(defmethod gz-aux :after () (push :after *gz-trace*))
(defmethod gz-aux :around () (push :around *gz-trace*) (call-next-method))

(deftest gf-zero-arg.aux-methods-run-every-call
  (progn (setf *gz-trace* '())
         (gz-aux) (gz-aux)
         (reverse *gz-trace*))
  (:around :before :primary :after :around :before :primary :after))

;;; A method added after the cache is warm takes effect.
(defgeneric gz-redefined ())
(defmethod gz-redefined () :first)

(deftest gf-zero-arg.redefinition-invalidates
  (let ((warm (list (gz-redefined) (gz-redefined))))
    (eval '(defmethod gz-redefined () :second))
    (append warm (list (gz-redefined))))
  (:first :first :second))

;;; No required parameters, but arguments all the same: the entry matches every
;;; call, so the arguments must still reach the method unchanged.
(defgeneric gz-rest (&rest args))
(defmethod gz-rest (&rest args) args)

(deftest gf-zero-arg.rest-args-are-not-cached
  (list (gz-rest) (gz-rest 1) (gz-rest 1 2 3) (gz-rest :a :b))
  (nil (1) (1 2 3) (:a :b)))

(defgeneric gz-key (&key a b))
(defmethod gz-key (&key (a :none) (b :none)) (list a b))

(deftest gf-zero-arg.key-args-are-not-cached
  (list (gz-key) (gz-key :a 1) (gz-key :b 2) (gz-key :a 1 :b 2))
  ((:none :none) (1 :none) (:none 2) (1 2)))

(deftest gf-zero-arg.unknown-key-still-signals
  (progn (gz-key :a 1)
         (handler-case (progn (gz-key :zz 1) :no-error)
           (program-error () :program-error)
           (error () :error)))
  :program-error)

;;; A non-standard method combination on a zero-argument generic function takes
;;; its own cache-store path.
(defgeneric gz-progn () (:method-combination progn))
(defmethod gz-progn progn () :one)
(defmethod gz-progn progn () :two)

(deftest gf-zero-arg.progn-combination
  (list (gz-progn) (gz-progn))
  (:two :two))

;;; No applicable method: the error must be signalled on every call, not just the
;;; first (nothing is cached when the applicable set is empty).
(defgeneric gz-none ())

(deftest gf-zero-arg.no-applicable-method-every-call
  (flet ((call () (handler-case (progn (gz-none) :no-error)
                    (error () :error))))
    (list (call) (call)))
  (:error :error))
