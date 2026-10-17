;;; (slot-value obj 'name) with a constant name keeps the slot's index per call
;;; site. The cache must not survive what changes the answer: a class
;;; redefinition, a slot-value-using-class method, an unbound or class slot,
;;; or instances of other classes at the same site.

(defclass svc-a () ((x :initarg :x) (y :initarg :y)))
(defclass svc-b () ((pad :initform 0) (x :initarg :x)))
(defclass svc-c () ((x :initarg :x :allocation :class)))
(defstruct svc-s x)
(define-condition svc-cond (error) ((x :initarg :x)))

(defun %svc-x (o) (slot-value o 'x))

(deftest slot-value-call-site-cache.polymorphic
  (let ((objs (list (make-instance 'svc-a :x 1) (make-instance 'svc-b :x 2)
                    (make-instance 'svc-c :x 3) (make-svc-s :x 4)
                    (make-condition 'svc-cond :x 5))))
    (loop repeat 3 append (mapcar #'%svc-x objs)))
  (1 2 3 4 5 1 2 3 4 5 1 2 3 4 5))

(deftest slot-value-call-site-cache.unbound-after-hit
  (let ((a (make-instance 'svc-a :x 1)))
    (%svc-x a) (%svc-x a)
    (list (%svc-x a)
          (handler-case (%svc-x (make-instance 'svc-a)) (unbound-slot () :unbound))))
  (1 :unbound))

(deftest slot-value-call-site-cache.missing
  (handler-case (progn (%svc-x (make-instance 'svc-a :x 1))
                       (slot-value (make-instance 'svc-a :x 1) 'nope))
    (error () :missing))
  :missing)

(defclass svc-r () ((y :initarg :y) (x :initarg :x)))
(defun %svc-rx (o) (slot-value o 'x))

(deftest slot-value-call-site-cache.redefined-class
  (let ((o (make-instance 'svc-r :x 1 :y 2)))
    (let ((before (list (%svc-rx o) (%svc-rx o))))
      (eval '(defclass svc-r () ((x :initarg :x) (z :initform 9) (y :initarg :y))))
      (list before (%svc-rx o) (%svc-rx (make-instance 'svc-r :x 7 :y 8)))))
  ((1 1) 1 7))

(defclass svc-meta-target () ((x :initarg :x)))
(defun %svc-mx (o) (slot-value o 'x))

(deftest slot-value-call-site-cache.slot-value-using-class-after-hit
  (let ((o (make-instance 'svc-meta-target :x 1)))
    (let ((before (list (%svc-mx o) (%svc-mx o))))
      (eval '(defmethod dotcl-mop:slot-value-using-class :around
                 ((c standard-class) (o svc-meta-target) slotd)
               (* 100 (call-next-method))))
      (prog1 (list before (%svc-mx o))
        (eval '(remove-method #'dotcl-mop:slot-value-using-class
                (find-method #'dotcl-mop:slot-value-using-class '(:around)
                             (list (find-class 'standard-class) (find-class 'svc-meta-target) (find-class t))))))))
  ((1 1) 100))

(deftest-emitting-only slot-value-call-site-cache.in-a-fasl
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "svc-fasl.lisp" dir))
         (fasl (merge-pathnames "svc-fasl.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defun svc-fasl-x (o) (slot-value o 'x))" s))
    (let ((*error-output* (make-broadcast-stream)))
      (load (compile-file src :output-file fasl)))
    (let ((objs (list (make-instance 'svc-a :x 1) (make-instance 'svc-b :x 2)
                      (make-instance 'svc-c :x 3) (make-svc-s :x 4))))
      (loop repeat 2 append (mapcar 'svc-fasl-x objs))))
  (1 2 3 4 1 2 3 4))
