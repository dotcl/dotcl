;;; A SPECIAL-declared parameter must be bound dynamically DURING the lambda-list
;;; walk, not after it.
;;;
;;; %MINI-BIND-PARAMS built the env and RETURNED it, leaving the dynamic binding
;;; to %MINI-EVAL-PROGN afterwards. So an &AUX init form ran BEFORE the parameter
;;; it reads was bound:
;;;
;;;   (let ((x :bad)) (declare (special x))
;;;     (flet ((%f () x))
;;;       ((lambda (x &aux (y (%f))) (declare (special x)) y) :good)))
;;;   ;; => :BAD, expected :GOOD
;;;
;;; (ansi-test LAMBDA.64.) A PROGV scope cannot be returned as a value, so
;;; %MINI-BIND-PARAMS-CALL takes a continuation instead.
;;;
;;; Leaving the special parameter OUT of the alist is also what makes
;;; %MINI-EVAL-PROGN behave afterwards: it only re-binds a declared name it can
;;; still find in ENV, so it does nothing and body references fall through to the
;;; dynamic value.

(defun %spb (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e) (princ-to-string e))))))

;;; --- an &AUX init form sees the dynamic value of a SPECIAL-declared parameter
;;; (ansi LAMBDA.64)

(defparameter %spb-aux-sees-special
  '(let ((x :bad))
    (declare (special x))
    (flet ((%f () x))
      ((lambda (x &aux (y (%f))) (declare (type t y) (special x)) y) :good))))

(deftest interp-special-param.aux-sees-special-compile
  (%spb :compile %spb-aux-sees-special)
  :good)

(deftest interp-special-param.aux-sees-special-interpret
  (%spb :interpret %spb-aux-sees-special)
  :good)

;;; --- &OPTIONAL / &KEY default forms too: visible mid-walk

(deftest interp-special-param.optional-default-sees-special-interpret
  (%spb :interpret '(let ((x :bad))
                     (declare (special x))
                     (flet ((%f () x))
                       ((lambda (x &optional (y (%f))) (declare (special x)) y) :good))))
  :good)

(deftest interp-special-param.key-default-sees-special-interpret
  (%spb :interpret '(let ((x :bad))
                     (declare (special x))
                     (flet ((%f () x))
                       ((lambda (x &key (y (%f))) (declare (special x)) y) :good))))
  :good)

;;; --- a body reference reads the dynamic value (this pair already worked)

(deftest interp-special-param.body-sees-special-interpret
  (%spb :interpret '(let ((x :bad))
                     (declare (special x))
                     (flet ((%f () x))
                       ((lambda (x) (declare (special x)) (%f)) :good))))
  :good)

;;; --- the binding does not leak past the call: the PROGV scope closes properly

(deftest interp-special-param.binding-does-not-leak-interpret
  (%spb :interpret '(let ((x :outer))
                     (declare (special x))
                     (funcall (lambda (x) (declare (special x)) x) :inner)
                     x))
  :outer)

;;; --- over-fix guards: ordinary lambda lists are unaffected ---------------
;;; The binder itself was restructured, so run every parameter kind through it.

(deftest interp-special-param.ordinary-lambda-lists-interpret
  (%spb :interpret
        '(list (funcall (lambda (a b) (list a b)) 1 2)
          (funcall (lambda (a &optional b) (list a b)) 1)
          (funcall (lambda (a &optional (b :d)) (list a b)) 1)
          (funcall (lambda (a &optional (b :d sp)) (list a b sp)) 1)
          (funcall (lambda (a &optional (b :d sp)) (list a b sp)) 1 2)
          (funcall (lambda (&rest r) r) 1 2 3)
          (funcall (lambda (a &rest r) (list a r)) 1 2 3)
          (funcall (lambda (&key a b) (list a b)) :b 2)
          (funcall (lambda (&key (a :d sp)) (list a sp)))
          (funcall (lambda (&key ((:foo bar) :d)) bar) :foo 7)
          (funcall (lambda (a &aux (b (1+ a))) (list a b)) 1)
          (funcall (lambda (&aux (a 1) (b (1+ a))) (list a b)))))
  ((1 2) (1 nil) (1 :d) (1 :d nil) (1 2 t) (1 2 3) (1 (2 3))
   (nil 2) (:d nil) 7 (1 2) (1 2)))

;;; init forms evaluate left to right and see the bindings made just before
(deftest interp-special-param.left-to-right-interpret
  (%spb :interpret '(funcall (lambda (a &optional (b (* a 2)) &key (c (+ a b)))
                              (list a b c))
                     3))
  (3 6 9))

;;; without a SPECIAL declaration a parameter stays lexical; this rejects a fix
;;; that PROGVs everything it sees a DECLARE for
(deftest interp-special-param.no-declaration-stays-lexical-interpret
  (%spb :interpret '(let ((x :outer))
                     (declare (special x))
                     (flet ((%f () x))
                       ((lambda (x &aux (y (%f))) (list x y)) :inner))))
  (:inner :outer))
