;;; (:include parent (slot ...)) overrides a slot the parent has. Naming a slot it
;;; does not have is an error (SBCL), not a silently ignored override. Overriding a
;;; real slot, by symbol or by (symbol default), still works.

(defstruct diu-parent a (b 2))

(defun %diu (form)
  (handler-case (progn (eval form) :ok) (error () :error)))

(deftest defstruct-include-unknown-slot.unknown
  (list (%diu '(defstruct (diu-c1 (:include diu-parent (zzz 3))) c))
        (%diu '(defstruct (diu-c2 (:include diu-parent a (nope))) c)))
  (:error :error))

(deftest defstruct-include-unknown-slot.real-slots
  (progn
    (eval '(defstruct (diu-d (:include diu-parent (a 10) (b 20 :read-only nil))) c))
    (list (%diu '(defstruct (diu-e (:include diu-parent (a))) c))
          (let ((d (make-diu-d))) (list (diu-d-a d) (diu-d-b d)))
          (diu-e-a (make-diu-e))))
  (:ok (10 20) nil))
