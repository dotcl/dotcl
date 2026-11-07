;;; DEFSTRUCT with a slot name used twice, or a slot that redefines one the structure
;;; includes, is a PROGRAM-ERROR (SBCL). The child's accessors for the redefined slot
;;; were never made, and with the parent's conc-name they collided with the parent's.
;;; Structures whose slots are distinct, with or without :include, are unchanged.

(defun %dds (form)
  (handler-case (progn (eval form) :ok) (program-error () :program-error)))

(deftest defstruct-duplicate-slot.own-slots-twice
  (list (%dds '(defstruct dds-a x x))
        (%dds '(defstruct dds-b (x 1) (y 2) (x 3)))
        (%dds '(defstruct (dds-c (:type list)) p p)))
  (:program-error :program-error :program-error))

(deftest defstruct-duplicate-slot.redefining-an-included-slot
  (progn
    (eval '(defstruct dds-parent a))
    (list (%dds '(defstruct (dds-child (:include dds-parent) (:conc-name dds-parent-)) a))
          (%dds '(defstruct (dds-child2 (:include dds-parent)) a))))
  (:program-error :program-error))

(deftest defstruct-duplicate-slot.names-compared-by-name
  (list (%dds '(defstruct dds-d cl-user::q keyword::q)))
  (:program-error))

(deftest defstruct-duplicate-slot.valid-structures-unchanged
  (progn
    (eval '(defstruct dds-p a))
    (list (%dds '(defstruct dds-e x y z))
          (%dds '(defstruct (dds-f (:include dds-p)) b c))
          (%dds '(defstruct (dds-g (:include dds-p (a 5))) b))
          (%dds '(defstruct (dds-h (:type vector) :named) m n))
          (let ((g (make-dds-g)))
            (list (dds-g-a g) (dds-g-b g)))))
  (:ok :ok :ok :ok (5 nil)))
