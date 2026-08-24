;;; A generic function with :before/:after methods dispatches without building an
;;; arguments array.
;;;
;;; The loose-argument dispatch path (DISPATCHGF1/2/3) ran only the
;;; "one primary, nothing else" shape and handed every other cache hit to the
;;; array path. One :after method therefore cost 32 B on EVERY call to that
;;; generic function -- measured with bench/micro-gf-dispatch.lisp's method:
;;; 16.1 B/op baseline, 16.1 with a plain primary, 48.2 with an :after added.
;;;
;;; :before and :after methods run for effect on the same arguments the primary
;;; gets, and no next-method chain reaches past the single primary, so the whole
;;; combination can run loose. These tests pin the SEMANTICS the shortcut has to
;;; preserve (CLHS 7.6.6.2), because getting the ordering wrong is the way this
;;; optimisation breaks.

(defvar *gam-log* nil)
(defun %gam (x) (push x *gam-log*) x)
(defun %gam-run (thunk)
  (setq *gam-log* nil)
  (let ((r (funcall thunk)))
    (list (reverse *gam-log*) r)))

(defclass gam-a () ())
(defclass gam-b (gam-a) ())

(defmethod gam-m ((x gam-a)) (%gam :prim-a) :a)
(defmethod gam-m ((x gam-b)) (%gam :prim-b) (call-next-method))
(defmethod gam-m :before ((x gam-a)) (%gam :before-a))
(defmethod gam-m :before ((x gam-b)) (%gam :before-b))
(defmethod gam-m :after ((x gam-a)) (%gam :after-a))
(defmethod gam-m :after ((x gam-b)) (%gam :after-b))

;;; CLHS 7.6.6.2: :before most specific first, then the primary chain, then
;;; :after LEAST specific first. The value is the primary's, never an auxiliary's.
(deftest gf-aux-method-dispatch.standard-order
  (%gam-run (lambda () (gam-m (make-instance 'gam-b))))
  ((:before-b :before-a :prim-b :prim-a :after-a :after-b) :a))

;;; The shapes the loose path now runs: one primary plus only :after, or only
;;; :before. The auxiliary must not become the return value.
(defclass gam-c () ())
(defmethod gam-n ((x gam-c)) (%gam :prim) :val)
(defmethod gam-n :after ((x gam-c)) (%gam :after) :not-the-value)

(deftest gf-aux-method-dispatch.after-only
  (%gam-run (lambda () (gam-n (make-instance 'gam-c))))
  ((:prim :after) :val))

(defclass gam-d () ())
(defmethod gam-o ((x gam-d)) (%gam :prim) :val)
(defmethod gam-o :before ((x gam-d)) (%gam :before) :not-the-value)

(deftest gf-aux-method-dispatch.before-only
  (%gam-run (lambda () (gam-o (make-instance 'gam-d))))
  ((:before :prim) :val))

;;; Every arity the loose path covers -- the auxiliaries get the same arguments
;;; the primary does.
(defclass gam-e () ())
(defmethod gam-p2 ((x gam-e) y) (%gam (list :prim y)) :v2)
(defmethod gam-p2 :before ((x gam-e) y) (%gam (list :before y)))
(defmethod gam-p2 :after ((x gam-e) y) (%gam (list :after y)))
(defmethod gam-p3 ((x gam-e) y z) (%gam (list :prim y z)) :v3)
(defmethod gam-p3 :after ((x gam-e) y z) (%gam (list :after y z)))

(deftest gf-aux-method-dispatch.arity-2-and-3
  (list (%gam-run (lambda () (gam-p2 (make-instance 'gam-e) 9)))
        (%gam-run (lambda () (gam-p3 (make-instance 'gam-e) 8 7))))
  ((((:before 9) (:prim 9) (:after 9)) :v2)
   (((:prim 8 7) (:after 8 7)) :v3)))

;;; An auxiliary that dispatches again must not disturb the outer invocation's
;;; next-method state -- the loose path saves and restores it around each call.
(defclass gam-f () ())
(defmethod gam-q ((x gam-f)) (%gam :q-prim) :qv)
(defmethod gam-q :after ((x gam-f)) (%gam :q-after) (gam-n (make-instance 'gam-c)))

(deftest gf-aux-method-dispatch.nested-dispatch-inside-an-auxiliary
  (%gam-run (lambda () (gam-q (make-instance 'gam-f))))
  ((:q-prim :q-after :prim :after) :qv))

;;; With one primary there is no next method, whatever auxiliaries exist.
(defclass gam-g () ())
(defmethod gam-r ((x gam-g)) (%gam (list :nmp (and (next-method-p) t))) :rv)
(defmethod gam-r :after ((x gam-g)) (%gam :r-after))

(deftest gf-aux-method-dispatch.next-method-p-ignores-auxiliaries
  (%gam-run (lambda () (gam-r (make-instance 'gam-g))))
  (((:nmp nil) :r-after) :rv))

;;; The loose-argument path stopped at three arguments, so a generic function of
;;; four allocated its argument array on every call even on a warm cache:
;;;
;;;   gf 1-arg  16.1 B/op        gf 4-arg  72.1 -> 16.1 B/op
;;;   gf 2-arg  16.1 B/op        gf 5-arg  80.2 B/op (still the array path)
;;;   gf 3-arg  16.2 B/op
;;;
;;; SHARED-INITIALIZE reaches that arity on every (instance slot-names . initargs)
;;; call carrying one initarg pair, which is how it turned up. These pin the
;;; semantics the fourth argument has to keep -- it is dispatched on, it reaches
;;; the auxiliaries, and CALL-NEXT-METHOD with no arguments passes it along.

(defclass gam-h () ())
(defclass gam-i (gam-h) ())

(defmethod gam-4 ((x gam-h) p q r) (%gam (list :prim-h p q r)) :h)
(defmethod gam-4 ((x gam-i) p q r) (%gam :prim-i) (call-next-method))
(defmethod gam-4 :before ((x gam-h) p q r) (%gam :before-h))
(defmethod gam-4 :after ((x gam-h) p q r) (%gam (list :after-h p q r)))

(deftest gf-aux-method-dispatch.four-argument-combination
  (%gam-run (lambda () (gam-4 (make-instance 'gam-i) 1 2 3)))
  ((:before-h :prim-i (:prim-h 1 2 3) (:after-h 1 2 3)) :h))

;;; CALL-NEXT-METHOD with no arguments passes all four on (CLHS 7.6.6.1).
(defmethod gam-4b ((x gam-h) p q r) (list :h p q r))
(defmethod gam-4b ((x gam-i) p q r) (cons :i (call-next-method)))

(deftest gf-aux-method-dispatch.four-argument-call-next-method
  (gam-4b (make-instance 'gam-i) 7 8 9)
  (:i :h 7 8 9))

;;; The fourth argument is dispatched on, not just carried.
(defmethod gam-4c (w x y (z gam-h)) :on-h)
(defmethod gam-4c (w x y (z gam-i)) :on-i)

(deftest gf-aux-method-dispatch.dispatch-on-fourth-argument
  (list (gam-4c 1 2 3 (make-instance 'gam-h))
        (gam-4c 1 2 3 (make-instance 'gam-i)))
  (:on-h :on-i))

;;; :around still reaches the primary with all four arguments.
(defmethod gam-4d ((x gam-h) p q r) (list :prim p q r))
(defmethod gam-4d :around ((x gam-h) p q r) (cons :around (call-next-method)))

(deftest gf-aux-method-dispatch.four-argument-around
  (gam-4d (make-instance 'gam-h) 4 5 6)
  (:around :prim 4 5 6))

;;; Five arguments keep working; that arity still goes through the array path.
(defmethod gam-5 ((x gam-h) p q r s) (list p q r s))

(deftest gf-aux-method-dispatch.five-arguments-unchanged
  (gam-5 (make-instance 'gam-h) 1 2 3 4)
  (1 2 3 4))
