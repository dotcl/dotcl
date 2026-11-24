;;; A special operator has no macro function and MACROEXPAND leaves its form alone
;;; (CLHS 3.1.2.1.2.1), even where the compiler lowers the operator through an expander.

(defparameter *son-forms*
  '((block b 1) (catch 'c 1) (eval-when (:execute) 1) (flet ((f () 1)) (f))
    (function car) (go a) (if 1 2 3) (labels ((f () 1)) (f)) (let ((x 1)) x)
    (let* ((x 1)) x) (load-time-value 1) (locally 1) (macrolet ((m () 1)) (m))
    (multiple-value-call #'list 1) (multiple-value-prog1 1 2) (progn 1)
    (progv '(x) '(1) x) (quote a) (return-from b 1) (setq x 1)
    (symbol-macrolet ((s 1)) s) (tagbody a) (the t 1) (throw 'c 1)
    (unwind-protect 1 2)))

(deftest special-operator-not-macro.macro-function
  (remove-if-not (lambda (f) (macro-function (car f))) *son-forms*)
  nil)

(deftest special-operator-not-macro.macroexpand-1
  (loop for f in *son-forms*
        unless (multiple-value-bind (e expanded) (macroexpand-1 f)
                 (and (eq e f) (null expanded)))
          collect (car f))
  nil)

(deftest special-operator-not-macro.macroexpand
  (loop for f in *son-forms*
        unless (multiple-value-bind (e expanded) (macroexpand f)
                 (and (eq e f) (null expanded)))
          collect (car f))
  nil)

(deftest special-operator-not-macro.operators-are-special
  (every #'special-operator-p (mapcar #'car *son-forms*))
  t)

(deftest-emitting-only special-operator-not-macro.progv-still-works
  (list (progv '(son-x) '(5) (symbol-value 'son-x))
        (funcall (compile nil '(lambda () (progv '(son-y) '(6) (symbol-value 'son-y)))))
        (eval '(progv '(son-z) '(7) (symbol-value 'son-z))))
  (5 6 7))

(deftest special-operator-not-macro.real-macros-still-expand
  (list (nth-value 1 (macroexpand-1 '(when a b)))
        (nth-value 1 (macroexpand-1 '(loop (return)))))
  (t t))
