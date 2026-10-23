;;; A slot store skips its type check when the value is already known to be of
;;; the slot's type: a parameter whose declared type the entry checked and the
;;; body never assigns, a slot read of that type, or an unassigned LET variable
;;; bound to one of those. A LET declaration alone is not checked, so it keeps
;;; the check, as does an assigned variable.

(setf dotcl:*save-sil* t)

(defstruct (%sscp-pt (:predicate nil)) x)
(defstruct (%sscp-pt3 (:include %sscp-pt) (:predicate nil)) z)
(declaim (inline %sscp-mk))
(defstruct (%sscp-h (:constructor %sscp-mk (p n)) (:predicate nil))
  (p nil :type %sscp-pt) (n 0 :type integer))

(defun %sscp-one () 1)
(defun %sscp-arg (x n) (declare (type %sscp-pt x) (integer n)) (%sscp-mk x n))
(defun %sscp-sub (x) (declare (type %sscp-pt3 x)) (%sscp-mk x 2))
(defun %sscp-read (h) (declare (type %sscp-h h))
  (let ((p (%sscp-h-p h)) (n (%sscp-h-n h))) (%sscp-mk p n)))
(defun %sscp-let (x) (declare (type %sscp-pt x)) (let ((y x)) (%sscp-mk y 1)))
(defun %sscp-setq-arg (x) (declare (type %sscp-pt x)) (setq x (%sscp-one)) (%sscp-mk x 1))
(defun %sscp-setq-let (x) (declare (type %sscp-pt x)) (let ((y x)) (setq y 5) (%sscp-mk y 1)))
(defun %sscp-closure (x) (declare (type %sscp-pt x))
  (flet ((g () (setq x 3))) (g)) (%sscp-mk x 1))
(defun %sscp-decl-only () (let ((x (%sscp-one))) (declare (type %sscp-pt x)) (%sscp-mk x 1)))
(defun %sscp-setf (x) (declare (type %sscp-pt x))
  (let ((h (%sscp-mk x 1))) (setf (%sscp-h-p h) x) h))

(defun %sscp-te (thunk)
  (handler-case (progn (funcall thunk) :ok)
    (type-error (e)
      (let ((d (type-error-datum e)))
        (list :type-error (if (typep d 'structure-object) (type-of d) d))))))

(deftest struct-slot-check-proven.values
  (list (%sscp-te (lambda () (%sscp-arg (make-%sscp-pt) 4)))
        (%sscp-te (lambda () (%sscp-arg 7 4)))
        (%sscp-te (lambda () (%sscp-arg (make-%sscp-pt) "x")))
        (%sscp-te (lambda () (%sscp-sub (make-%sscp-pt3))))
        (%sscp-te (lambda () (%sscp-sub (make-%sscp-pt :x 1))))
        (%sscp-te (lambda () (%sscp-read (%sscp-mk (make-%sscp-pt) 3))))
        (%sscp-te (lambda () (%sscp-let (make-%sscp-pt))))
        (%sscp-te (lambda () (%sscp-setq-arg (make-%sscp-pt))))
        (%sscp-te (lambda () (%sscp-setq-let (make-%sscp-pt))))
        (%sscp-te (lambda () (%sscp-closure (make-%sscp-pt))))
        (%sscp-te #'%sscp-decl-only)
        (%sscp-te (lambda () (%sscp-setf (make-%sscp-pt)))))
  (:ok (:type-error 7) (:type-error "x") :ok (:type-error %sscp-pt) :ok :ok
   (:type-error 1) (:type-error 5) (:type-error 3) (:type-error 1) :ok))

(deftest-compiled-only struct-slot-check-proven.elided
  (flet ((has (fn) (and (search "CheckSlotType" (%flat-text (dotcl:function-sil fn))) t)))
    (mapcar #'has (list #'%sscp-arg #'%sscp-sub #'%sscp-read #'%sscp-let #'%sscp-setf
                        #'%sscp-setq-arg #'%sscp-setq-let #'%sscp-closure #'%sscp-decl-only)))
  (nil nil nil nil nil t t t t))

(setf dotcl:*save-sil* nil)
