;;; Assigning a constant variable must be an error, and DEFCONSTANT must refuse
;;; to change one.
;;;
;;; Both were silent. (setq nil 1) and (setq t 1) reported success while doing
;;; nothing, and -- the damaging one -- (setq +c+ 99) actually changed the value
;;; of a constant: code already compiled against the old value kept using it
;;; while later code saw the new one. CLHS 3.1.2.1.1.3 makes the assignment an
;;; error; 11.1.2.1.2 makes redefining a constant to a value that is not EQL to
;;; the old one an error for exactly that reason.
;;;
;;; Found by running the constant-as-variable family through dotcl and SBCL:
;;; SBCL signalled on every one of them.

(defconstant +ca-c+ 10)
(defparameter *ca-p* 1)

(defun %ca-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(deftest constant-assignment.setq-signals
  (list (%ca-signals-p (lambda () (eval '(setq +ca-c+ 99))))
        (%ca-signals-p (lambda () (eval '(setq nil 1))))
        (%ca-signals-p (lambda () (eval '(setq t 1)))))
  (t t t))

;;; The point of the check: the value must be intact afterwards.
(deftest constant-assignment.value-survives
  (progn (ignore-errors (eval '(setq +ca-c+ 99)))
         (ignore-errors (eval '(setq nil 1)))
         (ignore-errors (eval '(setq t 1)))
         (list +ca-c+ nil t (null nil) (if t :yes :no)))
  (10 nil t t :yes))

;;; SET is the same assignment by another name -- and it is what the interpreter
;;; uses for a special variable, so this covers that path too.
(deftest constant-assignment.set-signals
  (list (%ca-signals-p (lambda () (set '+ca-c+ 99)))
        (%ca-signals-p (lambda () (set 'nil 1)))
        +ca-c+)
  (t t 10))

(deftest constant-assignment.interpreted-setq-signals
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (%ca-signals-p (lambda () (eval '(setq +ca-c+ 99))))
          (eval '+ca-c+)))
  (t 10))

;;; DEFCONSTANT: the same value again is fine (loading a file twice does that),
;;; a different one is not.
(deftest constant-assignment.defconstant-same-value-is-quiet
  (list (%ca-signals-p (lambda () (eval '(defconstant +ca-c+ 10))))
        +ca-c+)
  (nil 10))

(deftest constant-assignment.defconstant-different-value-signals
  (list (%ca-signals-p (lambda () (eval '(defconstant +ca-c+ 11))))
        +ca-c+)
  (t 10))

;;; Ordinary variables are untouched.
(deftest constant-assignment.non-constants-still-assignable
  (progn (eval '(setq *ca-p* 2))
         (set '*ca-p* (1+ *ca-p*))
         (let ((x 1)) (setq x 5)
           (list *ca-p* x)))
  (3 5))

;;; A fresh DEFCONSTANT still works, and marks the symbol.
(deftest constant-assignment.fresh-defconstant
  (progn (eval '(defconstant +ca-fresh+ :v))
         (list (eval '+ca-fresh+)
               (%ca-signals-p (lambda () (eval '(setq +ca-fresh+ :other))))))
  (:v t))
