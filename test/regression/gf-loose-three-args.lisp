;;; Three-argument generic functions on a warm cache: each argument-class
;;; combination reaches its method, and method and class changes are seen on the
;;; next call.

(defstruct (gl3-ty (:constructor nil)))
(defstruct (gl3-a (:include gl3-ty)))
(defstruct (gl3-b (:include gl3-ty)))
(defstruct (gl3-c (:include gl3-ty)))

(defvar *gl3-objs* (list (make-gl3-a) (make-gl3-b) (make-gl3-c)))

(defun %gl3-table (fn)
  (dotimes (k 30)
    (dolist (x *gl3-objs*) (dolist (y *gl3-objs*) (funcall fn x y 0))))
  (loop for x in *gl3-objs* collect (loop for y in *gl3-objs* collect (funcall fn x y 0))))

(defgeneric gl3-f (x y z))
(defmethod gl3-f ((x gl3-a) (y gl3-a) z) (list :aa z))
(defmethod gl3-f ((x gl3-b) (y gl3-ty) z) (list :b* z))
(defmethod gl3-f ((x gl3-ty) (y gl3-ty) z) (declare (ignore z)) nil)

(deftest gl3.table
  (%gl3-table #'gl3-f)
  (((:aa 0) nil nil) ((:b* 0) (:b* 0) (:b* 0)) (nil nil nil)))

(deftest gl3.method-added-redefined-removed
  (progn
    (%gl3-table #'gl3-f)
    (let* ((m (defmethod gl3-f ((x gl3-c) (y gl3-b) z) (list :cb z)))
           (added (%gl3-table #'gl3-f))
           (redefined (progn (defmethod gl3-f ((x gl3-a) (y gl3-a) z) (list :aa2 z))
                             (%gl3-table #'gl3-f))))
      (remove-method #'gl3-f m)
      (defmethod gl3-f ((x gl3-a) (y gl3-a) z) (list :aa z))
      (list added redefined (%gl3-table #'gl3-f))))
  ((((:aa 0) nil nil) ((:b* 0) (:b* 0) (:b* 0)) (nil (:cb 0) nil))
   (((:aa2 0) nil nil) ((:b* 0) (:b* 0) (:b* 0)) (nil (:cb 0) nil))
   (((:aa 0) nil nil) ((:b* 0) (:b* 0) (:b* 0)) (nil nil nil))))

;; A method that calls the next method, added to a warm combination.
(deftest gl3.next-method-added
  (progn
    (%gl3-table #'gl3-f)
    (let ((m (defmethod gl3-f ((x gl3-c) (y gl3-c) z) (list :cc (call-next-method)))))
      (prog1 (%gl3-table #'gl3-f)
        (remove-method #'gl3-f m))))
  (((:aa 0) nil nil) ((:b* 0) (:b* 0) (:b* 0)) (nil nil (:cc nil))))

;; Classes redefined with a new superclass after the cache is warm.
(defclass gl3-p () ())
(defclass gl3-q () ())
(defclass gl3-k (gl3-p) ())
(defgeneric gl3-cls (x y z))
(defmethod gl3-cls ((x gl3-p) y z) (declare (ignore y z)) :p)
(defmethod gl3-cls ((x gl3-q) y z) (declare (ignore y z)) :q)
(defmethod gl3-cls (x y z) (declare (ignore x y z)) :other)

(deftest gl3.superclass-change
  (let ((k (make-instance 'gl3-k)) (out '()))
    (dotimes (i 30) (gl3-cls k 1 2))
    (push (gl3-cls k 1 2) out)
    (defclass gl3-k (gl3-q) ())
    (push (gl3-cls k 1 2) out)
    (defclass gl3-k () ())
    (push (gl3-cls k 1 2) out)
    (nreverse out))
  (:p :q :other))

;; EQL specializers on a three-argument generic function.
(defgeneric gl3-eql (x y z))
(defmethod gl3-eql ((x (eql :one)) y z) (list :one y z))
(defmethod gl3-eql ((x symbol) y z) (list :sym y z))

(deftest gl3.eql
  (let ((r '()))
    (dotimes (i 20) (gl3-eql :one 1 2) (gl3-eql :two 1 2))
    (push (gl3-eql :one 1 2) r)
    (push (gl3-eql :two 3 4) r)
    (let ((m (defmethod gl3-eql ((x (eql :two)) y z) (list :two y z))))
      (push (gl3-eql :two 5 6) r)
      (remove-method #'gl3-eql m))
    (push (gl3-eql :two 7 8) r)
    (nreverse r))
  ((:one 1 2) (:sym 3 4) (:two 5 6) (:sym 7 8)))

;; &optional third argument, called with two and three arguments in turn.
(defgeneric gl3-opt (x y &optional z))
(defmethod gl3-opt ((x gl3-a) y &optional (z :none)) (list :a y z))
(defmethod gl3-opt ((x gl3-ty) y &optional (z :none)) (list :ty y z))

(deftest gl3.optional
  (let ((a (make-gl3-a)) (b (make-gl3-b)))
    (dotimes (i 30) (gl3-opt a 1) (gl3-opt a 1 2) (gl3-opt b 1) (gl3-opt b 1 2))
    (list (gl3-opt a 1) (gl3-opt a 1 2) (gl3-opt b 1) (gl3-opt b 1 2)))
  ((:a 1 :none) (:a 1 2) (:ty 1 :none) (:ty 1 2)))

;; Multiple values come back through the warm path.
(defgeneric gl3-values (x y z))
(defmethod gl3-values ((x gl3-a) y z) (values x y z))
(defmethod gl3-values ((x gl3-b) y z) (values y z))

(deftest gl3.values
  (let ((a (make-gl3-a)) (b (make-gl3-b)))
    (dotimes (i 30) (gl3-values a 1 2) (gl3-values b 1 2))
    (let ((va (multiple-value-list (gl3-values a 1 2))))
      (list (eq (first va) a) (rest va)
            (multiple-value-list (gl3-values b 1 2))
            (eq (gl3-values a 1 2) a))))
  (t (1 2) (1 2) t))
