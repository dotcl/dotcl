;;; A lambda parameter that is a symbol of another package hides nothing of the same name:
;;; (lambda (p::s) ... s ...) sees the enclosing S. It used to read S as a global (UNBOUND-VARIABLE)
;;; because the free-variable scan compared the parameter's name, not the symbol. Same-package
;;; shadowing is unchanged.

(defpackage :lpop-pkg (:use :cl))

(deftest lambda-param-other-package-symbol.required
  (let* ((s (list 1 2))
         (f (lambda (lpop-pkg::s) (if (eq lpop-pkg::s s) t nil))))
    (list (funcall f s) (funcall f (list 1 2))))
  (t nil))

(deftest lambda-param-other-package-symbol.lambda-list-kinds
  (let ((s 1))
    (list (funcall (lambda (&optional (lpop-pkg::s 4)) (list s lpop-pkg::s)))
          (funcall (lambda (&key (lpop-pkg::s 5)) (list s lpop-pkg::s)))
          (funcall (lambda (&aux (lpop-pkg::s 6)) (list s lpop-pkg::s)))
          (funcall (lambda (&rest lpop-pkg::s) (list s lpop-pkg::s)) 9)
          (funcall (lambda (x &optional (lpop-pkg::s x)) (list s lpop-pkg::s)) 7)))
  ((1 4) (1 5) (1 6) (1 (9)) (1 7)))

(deftest lambda-param-other-package-symbol.nested-and-eval
  (let ((s 10))
    (list (funcall (lambda (lpop-pkg::s) (funcall (lambda (q) (list s lpop-pkg::s q)) 3)) 2)
          (eval '(let ((s 1)) (funcall (lambda (lpop-pkg::s) (list s lpop-pkg::s)) 2)))))
  ((10 2 3) (1 2)))

;; COMPILE needs the emitter; the emit-free build answers the same through EVAL above.
(deftest-emitting-only lambda-param-other-package-symbol.compile
  (funcall (compile nil '(lambda (s) (funcall (lambda (lpop-pkg::s) (list s lpop-pkg::s)) 2))) 1)
  (1 2))

(deftest lambda-param-other-package-symbol.same-package-still-shadows
  (let ((s 1))
    (list (funcall (lambda (s) (list s)) 2)
          (funcall (lambda (&optional (s 3)) (list s)))
          (funcall (lambda (&key (s 4)) (list s)))
          s))
  ((2) (3) (4) 1))
