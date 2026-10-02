;;; Redefining a class with other superclasses changes which methods apply to
;;; its instances and to those of its subclasses. A generic function's dispatch
;;; caches (the recent-classes cache and the per-class table behind it) kept
;;; the methods found under the old precedence list: after
;;; (defclass gsc-c (gsc-b) ()) became (defclass gsc-c (gsc-a) ()), the method
;;; on GSC-B still ran for a GSC-C instance.

(defclass gsc-a () ())
(defclass gsc-b () ())
(defclass gsc-c (gsc-b) ())
(defclass gsc-d (gsc-c) ())
(defgeneric gsc-m (x))
(defmethod gsc-m ((x gsc-a)) :a)
(defmethod gsc-m ((x gsc-b)) :b)

(defmacro %gsc-fillers (n)
  `(progn ,@(loop for i from 1 to n
                  collect `(defclass ,(intern (format nil "GSC-F~d" i)) (gsc-b) ()))))
(%gsc-fillers 6)

(defun %gsc-objs ()
  (append (list (make-instance 'gsc-c) (make-instance 'gsc-d))
          (loop for i from 1 to 6
                collect (make-instance (intern (format nil "GSC-F~d" i))))))

(deftest gf-dispatch-superclass-change
  (let ((objs (%gsc-objs)))
    ;; More classes than the recent-classes cache holds, several times over, so
    ;; both levels have entries for GSC-C and GSC-D.
    (dotimes (k 4) (mapc #'gsc-m objs))
    (let ((before (mapcar #'gsc-m objs)))
      (defclass gsc-c (gsc-a) ())
      (let ((after (mapcar #'gsc-m objs)))
        (dotimes (k 3) (mapc #'gsc-m objs))
        (defclass gsc-c () ())
        (list before after
              (mapcar (lambda (o) (handler-case (gsc-m o) (error () :no-method))) objs)))))
  ((:b :b :b :b :b :b :b :b)
   (:a :a :b :b :b :b :b :b)
   (:no-method :no-method :b :b :b :b :b :b)))

;; Only two classes, so their entries stay in the recent-classes cache.
(defclass gsc2-a () ())
(defclass gsc2-b () ())
(defclass gsc2-c (gsc2-b) ())
(defclass gsc2-d (gsc2-c) ())
(defgeneric gsc2-m (x))
(defmethod gsc2-m ((x gsc2-a)) :a)
(defmethod gsc2-m ((x gsc2-b)) :b)

(deftest gf-dispatch-superclass-change.recent-cache
  (let ((objs (list (make-instance 'gsc2-c) (make-instance 'gsc2-d))))
    (dotimes (k 4) (mapc #'gsc2-m objs))
    (let ((before (mapcar #'gsc2-m objs)))
      (defclass gsc2-c (gsc2-a) ())
      (list before (mapcar #'gsc2-m objs))))
  ((:b :b) (:a :a)))
