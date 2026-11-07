;;; A backquote template that contains a circular #n= structure used to make the
;;; reader loop forever; it is now read as a constant. Finite templates expand as before.

(defvar *bq-x* 5)
(defvar *bq-l* (list 7 8))

(deftest backquote-circular-template.circular-is-constant
  (let* ((form (read-from-string "`#1=(a . #1#)"))
         (tmpl (second form)))
    (list (car form) (car tmpl) (eq tmpl (cdr tmpl))))
  (quote a t))

(deftest backquote-circular-template.car-cycle
  (let* ((form (read-from-string "`(a #1=(b . #1#))"))
         (tmpl (second form)))
    (list (car form) (car tmpl) (eq (cadr tmpl) (cdr (cadr tmpl)))))
  (quote a t))

(deftest backquote-circular-template.finite-unchanged
  (values (eval (read-from-string "`(a ,*bq-x* ,@*bq-l*)"))
          (eval (read-from-string "`(a (b ,*bq-x*) . c)"))
          (coerce (eval (read-from-string "`#(1 ,*bq-x*)")) (quote list)))
  (a 5 7 8) (a (b 5) . c) (1 5))
