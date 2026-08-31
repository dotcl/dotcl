;;; A generic function with more than one applicable method dispatches without
;;; building an argument array.
;;;
;;; The loose-argument dispatch path -- the one that takes the arguments as
;;; themselves rather than as an array -- was gated on the cache entry holding
;;; exactly ONE primary method. A second applicable method sent every call back
;;; to the array path, so 32 bytes a call appeared the moment a class hierarchy
;;; had a specialized method, which is what CLOS code looks like. The chain form
;;; was never the problem: the loose path already runs multi-method chains for
;;; EQL-specialized generic functions.
;;;
;;; What must not change is which methods run, in what order, and what
;;; CALL-NEXT-METHOD and NEXT-METHOD-P see.

(defclass %gmd-base () ())
(defclass %gmd-mid (%gmd-base) ())
(defclass %gmd-leaf (%gmd-mid) ())

(defgeneric %gmd-order (x))
(defmethod %gmd-order ((x %gmd-base)) (list :base))
(defmethod %gmd-order ((x %gmd-mid)) (cons :mid (call-next-method)))
(defmethod %gmd-order ((x %gmd-leaf)) (cons :leaf (call-next-method)))

(deftest gf-multi-method-dispatch.next-method-order
  (list (%gmd-order (make-instance '%gmd-base))
        (%gmd-order (make-instance '%gmd-mid))
        (%gmd-order (make-instance '%gmd-leaf)))
  ((:base) (:mid :base) (:leaf :mid :base)))

;;; NEXT-METHOD-P sees the chain, and a method that does not call the next one
;;; simply ends it.
(defgeneric %gmd-nmp (x))
(defmethod %gmd-nmp ((x %gmd-base)) (list :base (and (next-method-p) t)))
(defmethod %gmd-nmp ((x %gmd-mid)) (list :mid (and (next-method-p) t)))

(deftest gf-multi-method-dispatch.next-method-p
  (list (%gmd-nmp (make-instance '%gmd-base))
        (%gmd-nmp (make-instance '%gmd-mid)))
  ((:base nil) (:mid t)))

;;; CALL-NEXT-METHOD with explicit arguments reaches the next method with them.
(defgeneric %gmd-args (x y))
(defmethod %gmd-args ((x %gmd-base) y) (list :base y))
(defmethod %gmd-args ((x %gmd-mid) y) (call-next-method x (* y 10)))

(deftest gf-multi-method-dispatch.call-next-method-with-args
  (list (%gmd-args (make-instance '%gmd-base) 3)
        (%gmd-args (make-instance '%gmd-mid) 3))
  ((:base 3) (:base 30)))

;;; :before / :after still run around the whole chain, once each, outermost first.
(defvar *gmd-log* '())
(defgeneric %gmd-aux (x))
(defmethod %gmd-aux ((x %gmd-base)) (push :primary-base *gmd-log*) :done)
(defmethod %gmd-aux ((x %gmd-mid)) (push :primary-mid *gmd-log*) (call-next-method))
(defmethod %gmd-aux :before ((x %gmd-base)) (push :before-base *gmd-log*))
(defmethod %gmd-aux :before ((x %gmd-mid)) (push :before-mid *gmd-log*))
(defmethod %gmd-aux :after ((x %gmd-base)) (push :after-base *gmd-log*))

(deftest gf-multi-method-dispatch.auxiliary-methods
  (let ((*gmd-log* '()))
    (let ((r (%gmd-aux (make-instance '%gmd-mid))))
      (list r (reverse *gmd-log*))))
  (:done (:before-mid :before-base :primary-mid :primary-base :after-base)))

;;; Arities 1 through 4 all take the loose path.
(defgeneric %gmd-a1 (a))
(defmethod %gmd-a1 ((a %gmd-base)) 1)
(defmethod %gmd-a1 ((a %gmd-mid)) 2)
(defgeneric %gmd-a4 (a b c d))
(defmethod %gmd-a4 ((a %gmd-base) b c d) (list 1 b c d))
(defmethod %gmd-a4 ((a %gmd-mid) b c d) (list 2 b c d))

(deftest gf-multi-method-dispatch.arities
  (list (%gmd-a1 (make-instance '%gmd-base)) (%gmd-a1 (make-instance '%gmd-mid))
        (%gmd-a4 (make-instance '%gmd-base) :x :y :z)
        (%gmd-a4 (make-instance '%gmd-mid) :x :y :z))
  (1 2 (1 :x :y :z) (2 :x :y :z)))

;;; The point: no argument array once a second method is applicable.
(defun %gmd-loop (n obj)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%gmd-a1 obj)))))

(defun %gmd-bytes-for (n obj)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%gmd-loop n obj)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: a statement about the dispatch code, measured in a compiled loop.
(deftest-compiled-only gf-multi-method-dispatch.allocates-nothing
  (let ((obj (make-instance '%gmd-mid)))       ; two applicable methods
    (%gmd-bytes-for 1000 obj)                  ; warm the dispatch cache
    ;; The argument array was 32 bytes a call, so 300k extra calls would show
    ;; over 9 MB here.
    (< (- (%gmd-bytes-for 400000 obj) (%gmd-bytes-for 100000 obj)) 100000))
  t)

;;; (CALL-NEXT-METHOD) with no arguments means "the same arguments", so the loose
;;; path can pass them along without materialising an array -- which it used to
;;; do once per link of the chain. The forms that cannot take that route (explicit
;;; arguments, an :around chain, an exhausted chain) still go the array way.

(defgeneric %gmd-chain3 (x))
(defmethod %gmd-chain3 ((x %gmd-base)) (list :base))
(defmethod %gmd-chain3 ((x %gmd-mid)) (cons :mid (call-next-method)))
(defmethod %gmd-chain3 ((x %gmd-leaf)) (cons :leaf (call-next-method)))

(defgeneric %gmd-around (x))
(defmethod %gmd-around ((x %gmd-base)) (list :base))
(defmethod %gmd-around ((x %gmd-mid)) (cons :mid (call-next-method)))
(defmethod %gmd-around :around ((x %gmd-mid)) (cons :around (call-next-method)))

(deftest gf-multi-method-dispatch.call-next-method-no-args
  (list (%gmd-chain3 (make-instance '%gmd-leaf))
        (%gmd-chain3 (make-instance '%gmd-mid))
        (%gmd-around (make-instance '%gmd-mid)))
  ((:leaf :mid :base) (:mid :base) (:around :mid :base)))

;;; The last method in the chain has nowhere to go.
(defgeneric %gmd-exhausted (x))
(defmethod %gmd-exhausted ((x %gmd-base)) (call-next-method))

(deftest gf-multi-method-dispatch.exhausted-chain-signals
  (handler-case (progn (%gmd-exhausted (make-instance '%gmd-base)) :no-error)
    (error () :error))
  :error)

;;; Multiple arities down a chain, and arguments still reaching every method.
(defgeneric %gmd-chain-args (a b c))
(defmethod %gmd-chain-args ((a %gmd-base) b c) (list :base b c))
(defmethod %gmd-chain-args ((a %gmd-mid) b c) (cons :mid (call-next-method)))
(defmethod %gmd-chain-args ((a %gmd-leaf) b c) (cons :leaf (call-next-method)))

(deftest gf-multi-method-dispatch.chain-keeps-arguments
  (list (%gmd-chain-args (make-instance '%gmd-leaf) :y :z)
        (%gmd-chain-args (make-instance '%gmd-mid) 1 2))
  ((:leaf :mid :base :y :z) (:mid :base 1 2)))

;;; The point: a three-deep chain allocates nothing of its own. The methods here
;;; return a constant, so anything measured would be the dispatch.
(defgeneric %gmd-quiet (x))
(defmethod %gmd-quiet ((x %gmd-base)) 1)
(defmethod %gmd-quiet ((x %gmd-mid)) (call-next-method))
(defmethod %gmd-quiet ((x %gmd-leaf)) (call-next-method))

(defun %gmd-quiet-loop (n obj)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%gmd-quiet obj)))))

(defun %gmd-quiet-bytes (n obj)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (%gmd-quiet-loop n obj)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: a statement about the dispatch code.
(deftest-compiled-only gf-multi-method-dispatch.call-next-method-allocation
  (let ((obj (make-instance (quote %gmd-leaf))))   ; three-deep chain
    (%gmd-quiet-bytes 1000 obj)
    ;; Each link used to materialise the arguments into an array (32 B), so
    ;; 300k calls through three links would show over 19 MB here.
    (< (- (%gmd-quiet-bytes 400000 obj) (%gmd-quiet-bytes 100000 obj)) 100000))
  t)
