;;; A warm dispatch cache keeps following method and class changes: the calls below
;;; first run each argument-class combination many times, then change the methods or
;;; the classes, and check that the next calls see the change.

(defstruct (glc-ty (:constructor nil)))
(defstruct (glc-a (:include glc-ty)))
(defstruct (glc-b (:include glc-ty)))
(defstruct (glc-c (:include glc-ty)))
(defstruct (glc-d (:include glc-ty)))

(defvar *glc-objs* (list (make-glc-a) (make-glc-b) (make-glc-c) (make-glc-d)))

(defun %glc-warm (fn)
  (dotimes (k 50)
    (dolist (x *glc-objs*)
      (dolist (y *glc-objs*) (funcall fn x y)))))

(defun %glc-table (fn)
  (loop for x in *glc-objs* collect (loop for y in *glc-objs* collect (funcall fn x y))))

(defgeneric glc-eq (x y))
(defmethod glc-eq ((x glc-a) (y glc-a)) :aa)
(defmethod glc-eq ((x glc-b) (y glc-b)) :bb)
(defmethod glc-eq ((x glc-ty) (y glc-ty)) nil)

(deftest gf-dispatch-loose-cache.add-remove-method
  (progn
    (%glc-warm #'glc-eq)
    (let* ((before (%glc-table #'glc-eq))
           (m (defmethod glc-eq ((x glc-c) (y glc-d)) :cd))
           (added (progn (%glc-warm #'glc-eq) (%glc-table #'glc-eq)))
           (redefined (progn (defmethod glc-eq ((x glc-a) (y glc-a)) :aa2)
                             (%glc-warm #'glc-eq)
                             (%glc-table #'glc-eq))))
      (remove-method #'glc-eq m)
      (defmethod glc-eq ((x glc-a) (y glc-a)) :aa)
      (%glc-warm #'glc-eq)
      (list before added redefined (%glc-table #'glc-eq))))
  (((:aa nil nil nil) (nil :bb nil nil) (nil nil nil nil) (nil nil nil nil))
   ((:aa nil nil nil) (nil :bb nil nil) (nil nil nil :cd) (nil nil nil nil))
   ((:aa2 nil nil nil) (nil :bb nil nil) (nil nil nil :cd) (nil nil nil nil))
   ((:aa nil nil nil) (nil :bb nil nil) (nil nil nil nil) (nil nil nil nil))))

;; Methods that need the next-method state or wrap the primary, added to a warm pair.
(defgeneric glc-chain (x y))
(defmethod glc-chain ((x glc-ty) (y glc-ty)) :ty)
(defvar *glc-log* '())

(deftest gf-dispatch-loose-cache.combination-added-later
  (progn
    (%glc-warm #'glc-chain)
    (defmethod glc-chain ((x glc-a) (y glc-ty)) (list :a (call-next-method)))
    (%glc-warm #'glc-chain)
    (let ((cnm (list (glc-chain (first *glc-objs*) (second *glc-objs*))
                     (glc-chain (second *glc-objs*) (first *glc-objs*)))))
      (defmethod glc-chain :before ((x glc-b) (y glc-b)) (push :before *glc-log*))
      (setf *glc-log* '())
      (dotimes (k 3) (glc-chain (second *glc-objs*) (second *glc-objs*)))
      (list cnm *glc-log*)))
  (((:a :ty) :ty) (:before :before :before)))

;; One argument, with an EQL method added after the class's entry is warm.
(defgeneric glc-one (x))
(defmethod glc-one ((x glc-ty)) :ty)
(defmethod glc-one ((x glc-a)) :a)

(deftest gf-dispatch-loose-cache.one-arg-eql-added
  (let ((special (make-glc-b)))
    (dotimes (k 50) (dolist (x *glc-objs*) (glc-one x)) (glc-one special))
    (eval `(defmethod glc-one ((x (eql ',special))) :special))
    (let ((out '()))
      (dotimes (k 20) (setf out (list (mapcar #'glc-one *glc-objs*) (glc-one special))))
      out))
  ((:a :ty :ty :ty) :special))

;; Standard classes: a redefinition that changes the superclasses, and CHANGE-CLASS.
(defclass glc-p () ())
(defclass glc-q () ())
(defclass glc-r (glc-p) ())
(defgeneric glc-who (x y))
(defmethod glc-who ((x glc-p) (y t)) :p)
(defmethod glc-who ((x glc-q) (y t)) :q)
(defmethod glc-who ((x t) (y t)) :other)

(deftest gf-dispatch-loose-cache.class-redefinition
  (let ((r (make-instance 'glc-r)) (out '()))
    (dotimes (k 100) (glc-who r 1) (glc-who r :x))
    (push (glc-who r 1) out)
    (defclass glc-r (glc-q) ())
    (dotimes (k 100) (glc-who r 1))
    (push (glc-who r 1) out)
    (defclass glc-r (glc-p) ())
    (push (glc-who r 1) out)
    (nreverse out))
  (:p :q :p))

(deftest gf-dispatch-loose-cache.change-class
  (let ((x (make-instance 'glc-p)) (out '()))
    (dotimes (k 100) (glc-who x 1))
    (push (glc-who x 1) out)
    (change-class x 'glc-q)
    (push (glc-who x 1) out)
    (change-class x 'standard-object)
    (push (glc-who x 1) out)
    (nreverse out))
  (:p :q :other))

;; A warm two-argument function still reports a wrong argument count.
(deftest gf-dispatch-loose-cache.arity-errors
  (progn
    (%glc-warm #'glc-eq)
    (list (handler-case (funcall #'glc-eq (first *glc-objs*)) (program-error () :too-few))
          (handler-case (funcall #'glc-eq (first *glc-objs*) (first *glc-objs*) 3)
            (program-error () :too-many))))
  (:too-few :too-many))

;; A new lambda list for a warm generic function: the calls are checked against
;; the new one.
(defgeneric glc-ll (x &rest r))
(defmethod glc-ll ((x glc-ty) &rest r) (list :rest r))

(deftest gf-dispatch-loose-cache.lambda-list-change
  (let ((a (first *glc-objs*)) (out '()))
    (dotimes (k 50) (glc-ll a :z 1) (glc-ll a :w 2))
    (push (glc-ll a :w 2) out)
    (dolist (m (copy-list (dotcl-mop:generic-function-methods #'glc-ll))) (remove-method #'glc-ll m))
    (defgeneric glc-ll (x &key z))
    (defmethod glc-ll ((x glc-ty) &key z) (list :key z))
    (dotimes (k 50) (glc-ll a :z 1))
    (push (glc-ll a :z 3) out)
    (push (handler-case (glc-ll a :w 2) (program-error () :unknown-key)) out)
    (nreverse out))
  ((:rest (:w 2)) (:key 3) :unknown-key))
