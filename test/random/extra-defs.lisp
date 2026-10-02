;;;; Definitions the extra shapes (extra.lisp) refer to: classes, generic
;;;; functions, a structure and a condition type. Loaded on both sides of the
;;;; replay (replay.lisp loads it unconditionally), since a case is only
;;;; compiled there, not generated.

(in-package :cl-test)

(defclass rf-pt ()
  ((x :initarg :x :initform 0 :accessor rf-pt-x)
   (y :initarg :y :initform 0 :accessor rf-pt-y)))

(defclass rf-pt3 (rf-pt)
  ((z :initarg :z :initform 1 :accessor rf-pt-z)))

(defgeneric rf-gf (a b))
(defmethod rf-gf ((a integer) (b integer)) (- a b))
(defmethod rf-gf ((a rf-pt) b) (+ (rf-pt-x a) (rf-gf (rf-pt-y a) b)))
(defmethod rf-gf ((a rf-pt3) b) (+ (rf-pt-z a) (call-next-method)))
(defmethod rf-gf :around ((a integer) (b (eql 0))) (1+ (call-next-method)))

(defgeneric rf-gf2 (a) (:method-combination +))
(defmethod rf-gf2 + ((a integer)) a)
(defmethod rf-gf2 + ((a rational)) 1)
(defmethod rf-gf2 + ((a t)) 2)

(defstruct rf-s (a 0 :type integer) (b 0))

(define-condition rf-cond (error)
  ((v :initarg :v :reader rf-cond-v)))

(defgeneric rf-gf3 (a))
(defmethod rf-gf3 ((a (eql 0))) 100)
(defmethod rf-gf3 ((a integer)) (if (evenp a) 2 3))
(defmethod rf-gf3 ((a cons)) (length a))
(defmethod rf-gf3 :before ((a integer)) nil)

(defclass rf-cnt ()
  ((n :initform 0 :accessor rf-cnt-n)
   (k :initarg :k :initform 1 :reader rf-cnt-k)))
(defmethod initialize-instance :after ((o rf-cnt) &key) (incf (rf-cnt-n o) 10))
(defgeneric rf-step (o d))
(defmethod rf-step ((o rf-cnt) d) (incf (rf-cnt-n o) (* d (rf-cnt-k o))))

(defclass rf-box ()
  ((v :initarg :v :initform 0 :accessor rf-box-v)
   (w :initarg :w :initform 5 :accessor rf-box-w)))

(define-condition rf-note (condition)
  ((v :initarg :v :reader rf-note-v)))

;;; An identity the compiler cannot see through: replay.lisp wraps call
;;; arguments in it when cl-user::*rf-opaque* is true, which hides the
;;; argument's type and constant value from the compiler and so takes other
;;; code paths for the same computation.
(declaim (notinline rf-opaque))
(defun rf-opaque (x) x)

;;; Literal objects. Each macro expands to a QUOTEd object built at macro
;;; expansion time, so COMPILE-FILE has to dump it into the fasl (through
;;; MAKE-LOAD-FORM for the standard object) and LOAD has to rebuild it.
(defmethod make-load-form ((o rf-box) &optional env)
  (make-load-form-saving-slots o :environment env))
;; A structure literal needs one too: the default method signals an error
;; (CLHS MAKE-LOAD-FORM), so without it SBCL refuses to dump the literal.
(defmethod make-load-form ((o rf-s) &optional env)
  (make-load-form-saving-slots o :environment env))

(defmacro rf-lit-struct (a b) `',(make-rf-s :a a :b b))
(defmacro rf-lit-obj (v w) `',(make-instance 'rf-box :v v :w w))
(defmacro rf-lit-ht (n)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "a" h) n (gethash '(1 2) h) (* 2 n) (gethash 3 h) (+ n 1))
    `',h))
(defmacro rf-lit-circ (n)
  (let ((l (list n (+ n 1) (+ n 2))))
    (setf (cdr (last l)) l)
    `',l))
(defmacro rf-lit-dvec (n)
  `',(make-array 3 :element-type 'double-float
                   :initial-contents (list (float n 1d0) 0.5d0 -2d0)))
(defmacro rf-lit-ivec (n)
  `',(make-array 3 :element-type '(signed-byte 32)
                   :initial-contents (list (mod n 1000) -7 8)))
(defmacro rf-lit-2d (n)
  `',(make-array '(2 2) :initial-contents (list (list n 1) (list 2 (list n)))))
(defmacro rf-lit-shared (n)
  (let ((s (list n)))
    `',(list s s)))
(defmacro rf-lit-str (n)
  `',(coerce (list #\a (code-char (+ 945 (mod n 20))) #\b) 'string))
(defmacro rf-lit-nums (n)
  `',(list (/ n 3) (complex n 2) (float n 1d0) (float (mod n 1000) 1f0)))
(defmacro rf-lit-sym () `',(make-symbol "RF-UNINTERNED"))
