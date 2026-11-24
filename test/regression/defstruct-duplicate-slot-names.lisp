;;; A DEFSTRUCT slot name cannot be used twice, or repeat a slot the structure
;;; includes, and an :INCLUDE override cannot name the same slot twice. SBCL
;;; signals a PROGRAM-ERROR naming the slot at macroexpansion time. Distinct
;;; slots, a child's new slots, and an override that only changes an inherited
;;; slot's default are all valid.

(defun %ddsn (form)
  (handler-case (progn (macroexpand-1 form) :ok)
    (program-error () :program-error)))

(deftest defstruct-duplicate-slot-names.own-slots
  (list (%ddsn '(defstruct dsn-a x x))
        (%ddsn '(defstruct dsn-b (x 1) x))
        (%ddsn '(defstruct (dsn-c (:type list)) p p))
        (%ddsn '(defstruct (dsn-d (:type vector)) q (q 1))))
  (:program-error :program-error :program-error :program-error))

(deftest defstruct-duplicate-slot-names.included-slot
  (progn
    (eval '(defstruct dsn-parent a))
    (list (%ddsn '(defstruct (dsn-child (:include dsn-parent) (:conc-name dsn-parent-)) a))
          (%ddsn '(defstruct (dsn-child2 (:include dsn-parent)) a))
          (%ddsn '(defstruct (dsn-child3 (:include dsn-parent (a 1) (a 2)))))))
  (:program-error :program-error :program-error))

(deftest defstruct-duplicate-slot-names.message-names-slot
  (handler-case (macroexpand-1 '(defstruct dsn-msg quux quux))
    (program-error (c) (and (search "QUUX" (format nil "~A" c)) t)))
  t)

(deftest defstruct-duplicate-slot-names.valid
  (progn
    (eval '(defstruct dsn-p a))
    (eval '(defstruct (dsn-new (:include dsn-p)) b c))
    (eval '(defstruct (dsn-override (:include dsn-p (a 5))) b))
    (eval '(defstruct dsn-distinct x y z))
    (list (dsn-new-p (make-dsn-new))
          (let ((o (make-dsn-override))) (list (dsn-override-a o) (dsn-override-b o)))
          (dsn-distinct-y (make-dsn-distinct :x 1 :y 2 :z 3))))
  (t (5 nil) 2))
