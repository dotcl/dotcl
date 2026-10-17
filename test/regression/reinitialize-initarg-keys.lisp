;;; REINITIALIZE-INSTANCE checks its initargs against the slots and the &key
;;; names of the applicable REINITIALIZE-INSTANCE and SHARED-INITIALIZE methods
;;; (CLHS 7.1.2). The method part is remembered per class, so adding or removing
;;; a method has to change the answer.

(defclass rik-a () ((x :initarg :x :initform 0 :accessor rik-x)))
(defclass rik-b (rik-a) ())
(defstruct rik-s (n 0))

(defun %rik-error-p (obj &rest initargs)
  (handler-case (progn (apply #'reinitialize-instance obj initargs) nil)
    (error () t)))

(deftest reinitialize-initarg-keys.methods-change-the-answer
  (let ((a (make-instance 'rik-a)) (b (make-instance 'rik-b)) (r '()))
    (push (%rik-error-p b :y 1) r)
    (push (%rik-error-p b :x 1) r)
    (let ((m (defmethod reinitialize-instance :after ((o rik-b) &key y) (declare (ignore o y)))))
      (push (%rik-error-p b :y 1) r)
      (push (%rik-error-p a :y 1) r)
      (remove-method #'reinitialize-instance m))
    (push (%rik-error-p b :y 1) r)
    (let ((m (defmethod shared-initialize :before ((o rik-a) s &rest i &key &allow-other-keys)
               (declare (ignore o s i)))))
      (push (%rik-error-p b :z 1) r)
      (remove-method #'shared-initialize m))
    (push (%rik-error-p b :z 1) r)
    (push (%rik-error-p b :allow-other-keys t :z 1) r)
    (push (rik-x b) r)
    (nreverse r))
  (t nil nil t t nil t nil 1))

(deftest reinitialize-initarg-keys.struct-specialized-method-not-applicable
  (let ((m (defmethod reinitialize-instance :after ((o rik-s) &key w) (declare (ignore o w)))))
    (prog1 (%rik-error-p (make-instance 'rik-a) :w 1)
      (remove-method #'reinitialize-instance m)))
  t)
