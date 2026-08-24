;;; Binding a constant variable is a program error (CLHS 3.1.2.1.1.3), and NIL
;;; and T are constants. Assignment was already refused; binding was not, so
;;; (let ((t 1)) t) quietly returned 1 and (lambda (nil) nil) compiled fine.
;;;
;;; The point is coverage: there are many binding sites and they all had to
;;; learn the same rule, so the check lives in one predicate that every one of
;;; them calls -- both evaluators included, since the interpreter accepted every
;;; form below even after the compiler had been fixed.

(defconstant +cbn-c+ 10)

(defun %cbn-signals-p (form)
  (handler-case (progn (eval form) nil)
    (error () t)))

(defun %cbn-all (forms) (mapcar #'%cbn-signals-p forms))

(defparameter *cbn-violations*
  '((let ((+cbn-c+ 5)) +cbn-c+)
    (let ((nil 1)) nil)
    (let ((t 1)) t)
    (let* ((nil 1)) nil)
    (lambda (+cbn-c+) +cbn-c+)
    (lambda (t) t)
    (lambda (&optional (x 1 nil)) x)
    (lambda (&key t) t)
    (lambda (&rest nil) nil)
    (lambda (&aux (t 1)) t)
    (dolist (nil '(1)) nil)
    (dotimes (t 1) t)
    (do ((nil 1)) (t nil))
    (multiple-value-bind (nil b) (values 1 2) b)
    (handler-case 1 (error (nil) 2))))

;;; Forms that are legal and must stay legal -- the check must not fire on an
;;; absent &optional/&key supplied-p slot, or on an empty handler-case variable
;;; list, both of which read as NIL.
(defparameter *cbn-legal*
  '((let ((x 1)) x)
    (let* ((x 1) (y x)) y)
    (lambda (a &optional b &key c &rest r) (list a b c r))
    (lambda (&optional (x 1)) x)
    (lambda (&key (k 2)) k)
    (handler-case 1 (error () 2))
    (handler-case (error "x") (error (c) (notnot c)))
    (multiple-value-bind (a b) (values 1 2) (list a b))
    (loop for s being the symbols of :keyword count s)))

(deftest constant-binding.compiled-rejects-all
  (%cbn-all *cbn-violations*)
  (t t t t t t t t t t t t t t t))

(deftest constant-binding.compiled-accepts-legal
  (%cbn-all *cbn-legal*)
  (nil nil nil nil nil nil nil nil nil))

;;; The interpreter is a separate evaluator with its own binding code, and it
;;; used to accept every violation above.
(deftest constant-binding.interpreted-rejects-all
  (let ((dotcl:*evaluator-mode* :interpret))
    (%cbn-all *cbn-violations*))
  (t t t t t t t t t t t t t t t))

(deftest constant-binding.interpreted-accepts-legal
  (let ((dotcl:*evaluator-mode* :interpret))
    (%cbn-all *cbn-legal*))
  (nil nil nil nil nil nil nil nil nil))

;;; MAKUNBOUND destroys a constant's value the same way assignment would.
(deftest constant-binding.makunbound-refuses-constants
  (list (%cbn-signals-p '(makunbound 'nil))
        (%cbn-signals-p '(makunbound 't))
        (%cbn-signals-p '(makunbound '+cbn-c+))
        (progn (eval '(defparameter *cbn-ordinary* 1))
               (eval '(makunbound '*cbn-ordinary*))
               (eval '(boundp '*cbn-ordinary*))))
  (t t t nil))

;;; The values must be intact after all of that.
(deftest constant-binding.values-survive
  (list +cbn-c+ nil t (null nil) (if t :yes :no))
  (10 nil t t :yes))

;;; Fallout the check found: PPRINT-LOGICAL-BLOCK's stream designator T means
;;; *TERMINAL-IO* (CLHS), but only NIL was handled, so T fell through to the
;;; "bind this variable" branch and the macro emitted (let ((t ...)) ...).
;;; ansi-test PPRINT-LOGICAL-BLOCK.4.
(deftest constant-binding.pprint-logical-block-t-designator
  (with-output-to-string (os)
    (with-input-from-string (is "")
      (with-open-stream (*terminal-io* (make-two-way-stream is os))
        (pprint-logical-block (t 1)))))
  "1")

(deftest constant-binding.pprint-logical-block-nil-designator
  (with-output-to-string (*standard-output*)
    (pprint-logical-block (nil 1)))
  "1")
