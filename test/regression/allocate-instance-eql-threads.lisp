;;; MAKE-INSTANCE and a thread adding ALLOCATE-INSTANCE methods specialized on (eql class): once
;;; a method has been added, every later MAKE-INSTANCE of that class calls it, whatever the other
;;; threads are doing (the cache of the classes with such a method is one immutable snapshot).

(require "dotcl-thread")

(defconstant +aiet-n+ 24)
(defvar *aiet-classes* (make-array +aiet-n+))
(defvar *aiet-counts* (make-array +aiet-n+ :initial-element 0))
(defvar *aiet-published* (make-array +aiet-n+ :initial-element nil))

(dotimes (k +aiet-n+)
  (setf (svref *aiet-classes* k)
        (make-instance 'standard-class
                       :name (intern (format nil "AIET-C~D" k))
                       :direct-superclasses (list (find-class 'standard-object)))))

(deftest allocate-instance-eql-threads.never-misses-a-published-method
  (let* ((failures 0) (done nil)
         (writer (dotcl-thread:make-thread
                  (lambda ()
                    (dotimes (k +aiet-n+)
                      (let ((cls (svref *aiet-classes* k)))
                        (eval `(defmethod allocate-instance :around ((c (eql ,cls)) &rest r)
                                 (dotcl:atomic-incf (svref *aiet-counts* ,k))
                                 (call-next-method)))
                        (setf (svref *aiet-published* k) t)))
                    (setf done t))))
         (readers
           (loop repeat 3
                 collect (dotcl-thread:make-thread
                          (lambda ()
                            (let ((bad 0))
                              (loop until done
                                    do (dotimes (k +aiet-n+)
                                         (when (svref *aiet-published* k)
                                           (let ((before (svref *aiet-counts* k)))
                                             (make-instance (svref *aiet-classes* k))
                                             (when (= before (svref *aiet-counts* k)) (incf bad))))))
                              bad))))))
    (dotcl-thread:thread-join writer)
    (dolist (r readers) (incf failures (dotcl-thread:thread-join r)))
    failures)
  0)

;; Take the methods out again: an eql-specialized method makes every ALLOCATE-INSTANCE dispatch
;; costlier, which the allocation-counting tests that load later would see.
(dotimes (k +aiet-n+)
  (let ((m (find-method #'allocate-instance '(:around)
                        (list (dotcl-mop:intern-eql-specializer (svref *aiet-classes* k))) nil)))
    (when m (remove-method #'allocate-instance m))))
