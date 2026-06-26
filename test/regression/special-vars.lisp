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

;;; &aux + body (declare (special x)) — body x reads the dynamic value,
;;; not the lexically-shadowed captured value. Mirrors ANSI DEFUN.5.
(deftest aux-body-special-free
  (let ((x 1)) (declare (special x))
    (let ((x 2) (w 5))
      (defun aux-bsf (&aux (q w)) (declare (special x)) (values q x))
      (multiple-value-list (aux-bsf))))
  (5 1))

;;; &aux with a special PARAM declared in the body still binds dynamically.
(deftest aux-body-special-param
  (progn
    (defun aux-bsp (x &aux (y 10)) (declare (special x)) (+ x y))
    (aux-bsp 5))
  15)

;;; &aux without specials: chained aux defaults still see earlier aux vars.
(deftest aux-chained-defaults
  (progn
    (defun aux-chain (n &aux (b (* n 2)) (c (+ b 1))) (list n b c))
    (aux-chain 3))
  (3 6 7))

;;; ANSI DEFUN.5 exactly: &aux init reads the LEXICAL x (=2), body declare
;;; (special x) makes the body x read the dynamic value (=1).
(deftest aux-init-lexical-body-special
  (let ((x 1)) (declare (special x))
    (let ((x 2))
      (defun aux-ilbs (&aux (y x)) (declare (special x)) (values y x))
      (multiple-value-list (aux-ilbs))))
  (2 1))

;;; let* free special: init form reads lexical, body declare reads dynamic.
(deftest letstar-free-special-init
  (let ((x 1)) (declare (special x))
    (let ((x 2))
      (defun lfsi () (let* ((y x)) (declare (special x)) (values y x)))
      (multiple-value-list (lfsi))))
  (2 1))
