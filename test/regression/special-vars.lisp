;;; Special (dynamic) variable regression tests

(defvar *sv-a* 0)
(defvar *sv-b* :default)

;;; Basic dynamic binding
(deftest special-var-basic
  (let ((*sv-a* 42))
    *sv-a*)
  42)

;;; Dynamic binding is restored after let
(deftest special-var-restore
  (progn
    (let ((*sv-a* 99))
      nil)
    *sv-a*)
  0)

;;; Dynamic binding visible in called function
(defun sv-reader () *sv-a*)

(deftest special-var-cross-function
  (let ((*sv-a* 7))
    (sv-reader))
  7)

;;; Nested dynamic bindings
(deftest special-var-nested
  (let ((*sv-a* 1))
    (let ((*sv-a* 2))
      (let ((*sv-a* 3))
        *sv-a*)))
  3)

;;; Nested bindings restore correctly
(deftest special-var-nested-restore
  (let ((*sv-a* 1))
    (let ((*sv-a* 2))
      nil)
    *sv-a*)
  1)

;;; Multiple special vars
(deftest special-var-multiple
  (let ((*sv-a* 10) (*sv-b* :new))
    (list *sv-a* *sv-b*))
  (10 :new))

;;; Dynamic binding survives non-local exit via unwind-protect
(deftest special-var-unwind-protect
  (let ((saved nil))
    (let ((*sv-a* 55))
      (unwind-protect
           nil
        (setq saved *sv-a*)))
    saved)
  55)
