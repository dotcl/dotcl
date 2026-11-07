;;; A metaclass method on INITIALIZE-INSTANCE or REINITIALIZE-INSTANCE may edit
;;; the :DIRECT-SLOTS plists in place before calling the next method (Mito marks
;;; a slot whose :col-type names a class as a ghost with RPLACD into the plist).
;;; The direct slot definitions are made from the plists as they arrive at
;;; SHARED-INITIALIZE, so the edit must reach them. A plist edited in place used
;;; to keep the slot definition made from it before the edit.

(defclass dspe-slotd (dotcl-mop:standard-direct-slot-definition)
  ((ghost :initarg :ghost :initform nil :reader dspe-ghost)
   (mark :initarg :mark :initform nil)))

(defclass dspe-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c dspe-meta) (s standard-class)) t)
(defmethod dotcl-mop:direct-slot-definition-class ((c dspe-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'dspe-slotd))

;; Insert :GHOST T after the slot name, the way Mito does.
(defun dspe-mark-ghosts (initargs)
  (dolist (s (getf initargs :direct-slots))
    (when (getf s :mark)
      (rplacd (cdr s) (list* :ghost t (cddr s))))))

(defmethod initialize-instance :around ((c dspe-meta) &rest initargs)
  (dspe-mark-ghosts initargs)
  (apply #'call-next-method c initargs))

(defmethod reinitialize-instance :around ((c dspe-meta) &rest initargs)
  (dspe-mark-ghosts initargs)
  (apply #'call-next-method c initargs))

(defun dspe-ghosts (name)
  (mapcar (lambda (s) (list (dotcl-mop:slot-definition-name s) (dspe-ghost s)))
          (dotcl-mop:class-direct-slots (find-class name))))

(defclass dspe-a ()
  ((x :mark t :initarg :x)
   (y :initarg :y :initform 3 :reader dspe-a-y))
  (:metaclass dspe-meta))

(deftest direct-slots-plist-edited-in-place.initialize
  (dspe-ghosts 'dspe-a)
  ((x t) (y nil)))

;; The slot whose plist was not touched still works as DEFCLASS wrote it.
(deftest direct-slots-plist-edited-in-place.untouched-slot
  (let ((o (make-instance 'dspe-a :x 1)))
    (list (slot-value o 'x) (dspe-a-y o)))
  (1 3))

;; Changing a value in place, with SETF of GETF on an option that is there.
(defclass dspe-meta2 (dspe-meta) ())
(defmethod initialize-instance :around ((c dspe-meta2) &rest initargs)
  (dolist (s (getf initargs :direct-slots))
    (when (getf s :ghost)
      (setf (getf s :ghost) :changed)))
  (apply #'call-next-method c initargs))

(defclass dspe-b ()
  ((x :ghost :given))
  (:metaclass dspe-meta2))

(deftest direct-slots-plist-edited-in-place.setf-getf
  (dspe-ghosts 'dspe-b)
  ((x :changed)))

;; Redefinition goes through REINITIALIZE-INSTANCE.
(defclass dspe-c ()
  ((x :initarg :x))
  (:metaclass dspe-meta))

(defclass dspe-c ()
  ((x :initarg :x)
   (z :mark t :initarg :z))
  (:metaclass dspe-meta))

(deftest direct-slots-plist-edited-in-place.reinitialize
  (dspe-ghosts 'dspe-c)
  ((x nil) (z t)))
