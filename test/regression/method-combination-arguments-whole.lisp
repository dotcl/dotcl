;;; The &WHOLE variable of a long-form DEFINE-METHOD-COMBINATION's :ARGUMENTS is all the
;;; generic function's arguments (CLHS), not just the sections the other parameters
;;; name: with `(:arguments &whole w)` it was NIL for (x &optional y), and
;;; `(:arguments &whole w &key k)` lost the leading arguments.

(define-method-combination mcaw-plain ()
  ((ms ()))
  (:arguments &whole w)
  `(list :whole ,w (call-method ,(first ms))))

(define-method-combination mcaw-key ()
  ((ms ()))
  (:arguments &whole w &key k)
  `(list :whole ,w :k ,k (call-method ,(first ms))))

(define-method-combination mcaw-req ()
  ((ms ()))
  (:arguments &whole w a)
  `(list :whole ,w :a ,a (call-method ,(first ms))))

(defgeneric mcaw-g1 (x &optional y) (:method-combination mcaw-plain))
(defmethod mcaw-g1 (x &optional y) (declare (ignore x y)) :m)
(defgeneric mcaw-g2 (x &key k) (:method-combination mcaw-key))
(defmethod mcaw-g2 (x &key k) (declare (ignore x k)) :m)
(defgeneric mcaw-g3 (x &optional y) (:method-combination mcaw-req))
(defmethod mcaw-g3 (x &optional y) (declare (ignore x y)) :m)

(deftest method-combination-arguments-whole.optional
  (list (mcaw-g1 1 2) (mcaw-g1 1))
  ((:whole (1 2) :m) (:whole (1) :m)))

(deftest method-combination-arguments-whole.key
  (list (mcaw-g2 1 :k 2) (mcaw-g2 1))
  ((:whole (1 :k 2) :k 2 :m) (:whole (1) :k nil :m)))

(deftest method-combination-arguments-whole.with-a-required
  (mcaw-g3 5 6)
  (:whole (5 6) :a 5 :m))
