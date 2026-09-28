;;; A method may have any number of qualifiers (CLHS DEFMETHOD: qualifier*),
;;; and a long-form method combination groups methods by matching the whole
;;; qualifier list against its patterns: * matches one qualifier, a * tail
;;; matches the rest, a group may list several patterns, and a symbol other
;;; than * names a predicate on the qualifier list.
;;;
;;; DEFMETHOD and the :METHOD option of DEFGENERIC used to take at most one
;;; qualifier and failed with "CAR: not a list" on the second (mgl-pax's test
;;; system has (:method my-comb :after ...)), and group patterns only looked at
;;; the first qualifier.

(define-method-combination mq-pairs ()
  ((ab (:a :b))
   (star (:c . *))
   (either (:x) (:y :*-marker))
   (plain ()))
  `(progn ,@(mapcar (lambda (m) `(call-method ,m)) ab)
          ,@(mapcar (lambda (m) `(call-method ,m)) star)
          ,@(mapcar (lambda (m) `(call-method ,m)) either)
          ,@(mapcar (lambda (m) `(call-method ,m)) plain)))

(defvar *mq-log* nil)

(defgeneric mq-f (x)
  (:method-combination mq-pairs)
  (:method :a :b ((x integer)) (push (list :ab x) *mq-log*)))
(defmethod mq-f :c :d :e ((x integer)) (push :cde *mq-log*))
(defmethod mq-f :y :*-marker ((x integer)) (push :y *mq-log*))
(defmethod mq-f ((x integer)) (push :plain *mq-log*))

(deftest defmethod-multiple-qualifiers-dispatch
  (let ((*mq-log* nil))
    (mq-f 5)
    (reverse *mq-log*))
  ((:ab 5) :cde :y :plain))

(deftest defmethod-multiple-qualifiers-method-qualifiers
  (values (method-qualifiers (find-method #'mq-f '(:a :b) (list (find-class 'integer))))
          (method-qualifiers (find-method #'mq-f '(:c :d :e) (list (find-class 'integer)))))
  (:a :b) (:c :d :e))

;; (:a) alone matches no group: (:a :b) is not a prefix pattern.
(deftest defmethod-multiple-qualifiers-partial-match-is-no-match
  (progn
    (defmethod mq-f :a ((x string)) x)
    (handler-case (progn (mq-f "s") :called)
      (error () :no-group)))
  :no-group)

(defun mq-two-qualifiers-p (quals) (= (length quals) 2))

(define-method-combination mq-pred ()
  ((two mq-two-qualifiers-p) (plain ()))
  `(progn ,@(mapcar (lambda (m) `(call-method ,m)) two)
          (call-method ,(first plain))))

(defgeneric mq-g (x) (:method-combination mq-pred))
(defmethod mq-g :p :q ((x t)) (push :two *mq-log*))
(defmethod mq-g ((x t)) (push :plain *mq-log*))

(deftest defmethod-multiple-qualifiers-predicate-group
  (let ((*mq-log* nil))
    (mq-g 1)
    (reverse *mq-log*))
  (:two :plain))

;; The short-form combination accepts the method definitions (SBCL only warns);
;; the invalid qualifiers matter only when such a method is applicable.
(define-method-combination mq-short :identity-with-one-argument t)

(deftest defgeneric-method-option-multiple-qualifiers
  (progn
    (defgeneric mq-h (x &key z)
      (:method-combination mq-short)
      (:method mq-short :after ((x number) &key z) (declare (ignore z)) x)
      (:method mq-short ((x integer) &key z) (declare (ignore z)) (* x 10)))
    (length (generic-function-methods #'mq-h)))
  2)
