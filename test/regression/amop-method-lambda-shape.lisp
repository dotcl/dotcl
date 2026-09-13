;;; MAKE-METHOD-LAMBDA answers in the shape AMOP specifies.
;;;
;;; AMOP: a method lambda takes two parameters, the arguments as a list and the
;;; next methods as a list. dotcl used to hand back its own method lambda, with
;;; the arguments spread, so portable code that wraps what CALL-NEXT-METHOD gave
;;; it -- (lambda (args next-methods) ... (funcall inner args next-methods)) --
;;; called a one-parameter function with two arguments. It happened to work for
;;; methods of exactly two required arguments and signalled PROGRAM-ERROR for
;;; every other arity, which is the least useful way to be wrong.
;;;
;;; DEFMETHOD converts the answer back to the spread shape dispatch calls, so a
;;; generic function that has not specialised the protocol is untouched.

(defvar *amls-trace* nil)

;;; --- what the default method answers ---------------------------------------

(defvar *amls-default-answer* nil)

(defclass amls-plain-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defmethod dotcl-mop:make-method-lambda
    ((gf amls-plain-gf) method lambda-expression environment)
  (setf *amls-default-answer* (call-next-method)))

(defgeneric amls-plain (x) (:generic-function-class amls-plain-gf))
(defmethod amls-plain ((x integer)) (* x 2))

(deftest amop-method-lambda-shape.default-answer-takes-two-parameters
  (list (and (consp *amls-default-answer*)
             (eq (first *amls-default-answer*) 'lambda))
        (length (second *amls-default-answer*)))
  (t 2))

;;; The answer is a method lambda, not a rewritten body: the original still runs.
(deftest amop-method-lambda-shape.default-answer-still-works
  (amls-plain 21)
  42)

;;; --- portable wrapping code -------------------------------------------------

(defclass amls-wrap-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defmethod dotcl-mop:make-method-lambda
    ((gf amls-wrap-gf) method lambda-expression environment)
  (let ((inner (call-next-method)))
    `(lambda (args next-methods)
       (push (length next-methods) *amls-trace*)
       (funcall ,inner args next-methods))))

;;; One required argument: the arity that used to signal.
(defgeneric amls-one (x) (:generic-function-class amls-wrap-gf))
(defmethod amls-one ((x integer)) (* x 3))

(deftest amop-method-lambda-shape.wraps-one-argument-method
  (let ((*amls-trace* nil))
    (list (amls-one 5) (length *amls-trace*)))
  (15 1))

;;; Three, and none of the arguments may be lost or reordered on the way through
;;; the list.
(defgeneric amls-three (x y z) (:generic-function-class amls-wrap-gf))
(defmethod amls-three ((x integer) (y integer) (z integer)) (list x y z))

(deftest amop-method-lambda-shape.wraps-three-argument-method
  (amls-three 1 2 3)
  (1 2 3))

;;; &OPTIONAL and &KEY are processed by the original lambda inside the answer, so
;;; defaults still apply and keywords still arrive by name rather than position.
(defgeneric amls-opt (x &optional y) (:generic-function-class amls-wrap-gf))
(defmethod amls-opt ((x integer) &optional (y 10)) (list x y))

(defgeneric amls-key (x &key k) (:generic-function-class amls-wrap-gf))
(defmethod amls-key ((x integer) &key (k 10)) (list x k))

(deftest amop-method-lambda-shape.optional-and-key-survive
  (list (amls-opt 1) (amls-opt 1 2)
        (amls-key 1) (amls-key 1 :k 2))
  ((1 10) (1 2) (1 10) (1 2)))

;;; The second argument is the next methods of the call in progress, as AMOP says,
;;; not a placeholder: the more specific method sees the one behind it, and the
;;; last method sees none. CALL-NEXT-METHOD keeps working through the wrapper.
(defgeneric amls-next (x) (:generic-function-class amls-wrap-gf))
(defmethod amls-next ((x integer)) (list :integer (call-next-method)))
(defmethod amls-next ((x t)) :fallback)

(deftest amop-method-lambda-shape.next-methods-are-passed
  (let ((*amls-trace* nil))
    (list (amls-next 5) (reverse *amls-trace*)))
  ((:integer :fallback) (1 0)))

;;; --- generic functions that did not specialise the protocol -----------------

;;; Nothing is wrapped and nothing is converted, which is what keeps the cost of
;;; all this at zero for ordinary code.
(defgeneric amls-ordinary (x))
(defmethod amls-ordinary ((x integer)) (- x))

(deftest amop-method-lambda-shape.ordinary-generic-function-untouched
  (let ((*amls-trace* nil))
    (list (amls-ordinary 7) *amls-trace*))
  (-7 nil))

;;; METHOD-FUNCTION answers in the AMOP shape for both kinds, which it already did.
(deftest amop-method-lambda-shape.method-function-shape
  (list (funcall (dotcl-mop:method-function
                  (first (dotcl-mop:generic-function-methods #'amls-three)))
                 (list 1 2 3) nil)
        (funcall (dotcl-mop:method-function
                  (first (dotcl-mop:generic-function-methods #'amls-ordinary)))
                 (list 7) nil))
  ((1 2 3) -7))
