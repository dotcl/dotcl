;;; Calling outside every package unit (dotcl::%call-outside-package-units): no
;;; unit is current, the specials a unit binds show the bindings that were in
;;; effect before the unit was entered, and a SETQ there stays when the call and
;;; the unit are left. Plain units and a special of our own; the dotcl-lock
;;; level is in package-units-add-unit.lisp.

(defvar *puo-var* :global)

(defvar *puo-unit*
  (let ((u (dotcl::%make-package-unit "PUO" "PUO-OWNED")))
    (progv (list '*puo-var*) (list :unit)
      (dotcl::%add-package-unit-special u '*puo-var*))
    u))

(defun puo-in-unit (thunk) (dotcl::%call-in-package-unit *puo-unit* thunk))
(defun puo-outside (thunk) (dotcl::%call-outside-package-units thunk))

(deftest package-units-outside.sees-global
  (list (puo-in-unit (lambda () *puo-var*))
        (puo-in-unit (lambda () (puo-outside (lambda () *puo-var*))))
        (puo-in-unit (lambda () (puo-outside (lambda () (dotcl::%current-package-unit-name)))))
        (puo-in-unit (lambda () (dotcl::%current-package-unit-name))))
  (:unit :global nil "PUO"))

;;; The unit's own value is back after the call.
(deftest package-units-outside.unit-value-comes-back
  (puo-in-unit (lambda ()
                 (puo-outside (lambda () nil))
                 (setf *puo-var* :unit-2)
                 (puo-outside (lambda () nil))
                 *puo-var*))
  :unit-2)

;;; A SETQ outside is the global one: it survives the unit, and the unit's
;;; value is left alone.
(deftest package-units-outside.setq-is-global
  (let ((r (puo-in-unit (lambda ()
                          (puo-outside (lambda () (setf *puo-var* :changed)))
                          *puo-var*))))
    (prog1 (list r *puo-var*)
      (setf *puo-var* :global)))
  (:unit-2 :changed))

;;; A LET around the unit is what the call outside sees, and its SETQ lands there.
(deftest package-units-outside.sees-enclosing-let
  (let ((*puo-var* :outer-let))
    (list (puo-in-unit (lambda () (puo-outside (lambda () *puo-var*))))
          (puo-in-unit (lambda () (puo-outside (lambda () (setf *puo-var* :set-in-let)))))
          *puo-var*))
  (:outer-let :set-in-let :set-in-let))

;;; A LET inside the unit, around the call outside: hidden while outside, back after.
(deftest package-units-outside.let-inside-unit
  (puo-in-unit (lambda ()
                 (let ((*puo-var* :inner-let))
                   (list (puo-outside (lambda () *puo-var*))
                         *puo-var*
                         (puo-outside (lambda ()
                                        (let ((*puo-var* :rebound-outside))
                                          *puo-var*)))
                         *puo-var*))))
  (:global :inner-let :rebound-outside :inner-let))

;;; Entering a unit again from outside works, with the unit's own value (set above), and leaves things as they were.
(deftest package-units-outside.reenter
  (puo-in-unit (lambda ()
                 (list (puo-outside (lambda () (puo-in-unit (lambda () *puo-var*))))
                       (puo-outside (lambda () *puo-var*))
                       *puo-var*)))
  (:unit-2 :global :unit-2))

;;; Called while no unit is entered, it just calls.
(deftest package-units-outside.no-unit
  (list (puo-outside (lambda () *puo-var*)) *puo-var*)
  (:global :global))
