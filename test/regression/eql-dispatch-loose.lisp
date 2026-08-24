;;; A warm EQL-specialized dispatch reaches its method without building an argument
;;; array. The array path cost 32 B a call even on a cache hit, and dispatching on
;;; (eql :keyword) is an ordinary way to write a table of methods.
;;;
;;; The loose path has to reproduce everything the array path publishes, so these
;;; tests pin the parts that would notice: the chain that follows an EQL method,
;;; CALL-NEXT-METHOD with and without arguments, NEXT-METHOD-P, and the shapes that
;;; must NOT take the shortcut (:around/:before/:after, no applicable method).

(defgeneric edl-fib (x))
(defmethod edl-fib ((x (eql 0))) 0)
(defmethod edl-fib ((x (eql 1))) 1)
(defmethod edl-fib (x) (+ (edl-fib (- x 1)) (edl-fib (- x 2))))

(deftest eql-dispatch-loose.values
  (list (edl-fib 0) (edl-fib 1) (edl-fib 10))
  (0 1 55))

;;; CALL-NEXT-METHOD from the EQL method into the class-specialized one: the chain
;;; is two long, and the no-argument form needs the original arguments as a list,
;;; which the loose path materialises on demand.
(defgeneric edl-next (x))
(defmethod edl-next ((x (eql :k))) (list :eql (call-next-method)))
(defmethod edl-next (x) (list :default x))

(deftest eql-dispatch-loose.call-next-method
  (list (edl-next :k) (edl-next :other))
  ((:eql (:default :k)) (:default :other)))

;;; The explicit-argument form passes its arguments on. They have to keep the same
;;; methods applicable (CLHS 7.6.6.2), which with an EQL specializer means the same
;;; value -- and a different one has to be refused rather than dispatched wrongly.
(defgeneric edl-next-args (x))
(defmethod edl-next-args ((x (eql :k))) (list :eql (call-next-method x)))
(defmethod edl-next-args (x) (list :default x))

(deftest eql-dispatch-loose.call-next-method-with-args
  (edl-next-args :k)
  (:eql (:default :k)))

(defgeneric edl-next-bad (x))
(defmethod edl-next-bad ((x (eql :k))) (call-next-method :replaced))
(defmethod edl-next-bad (x) (list :default x))

(deftest eql-dispatch-loose.call-next-method-changed-applicability-signals
  (handler-case (progn (edl-next-bad :k) :no-error) (error () :error))
  :error)

(defgeneric edl-nmp (x))
(defmethod edl-nmp ((x (eql :has-next))) (list :eql (next-method-p)))
(defmethod edl-nmp ((x (eql :no-next))) (list :eql (next-method-p)))
(defmethod edl-nmp ((x symbol)) :fallback)

(deftest eql-dispatch-loose.next-method-p
  (list (edl-nmp :has-next) (edl-nmp :other))
  ((:eql t) :fallback))

;;; A GF whose EQL method has no class-specialized method under it: there is no next
;;; method, and asking for one signals.
(defgeneric edl-alone (x))
(defmethod edl-alone ((x (eql :only))) (list :only (next-method-p)))

(deftest eql-dispatch-loose.no-next-method
  (list (edl-alone :only)
        (handler-case (progn (edl-alone :missing) :no-error)
          (error () :error)))
  ((:only nil) :error))

;;; Auxiliary methods must keep working — the shortcut declines these shapes.
(defvar *edl-trace* '())
(defgeneric edl-aux (x))
(defmethod edl-aux ((x (eql :a))) (push :primary *edl-trace*) :primary)
(defmethod edl-aux :before ((x (eql :a))) (push :before *edl-trace*))
(defmethod edl-aux :after ((x (eql :a))) (push :after *edl-trace*))
(defmethod edl-aux :around ((x (eql :a))) (push :around *edl-trace*) (call-next-method))

(deftest eql-dispatch-loose.auxiliary-methods
  (progn (setf *edl-trace* '())
         (list (edl-aux :a) (edl-aux :a) (reverse *edl-trace*)))
  (:primary :primary (:around :before :primary :after :around :before :primary :after)))

;;; EQL on values that are not symbols or fixnums (the general EQL branch).
(defgeneric edl-char (x))
(defmethod edl-char ((x (eql #\a))) :char-a)
(defmethod edl-char ((x (eql 3.5))) :float)
(defmethod edl-char (x) :other)

(deftest eql-dispatch-loose.non-symbol-eql-values
  (list (edl-char #\a) (edl-char 3.5) (edl-char #\b) (edl-char 1))
  (:char-a :float :other :other))
