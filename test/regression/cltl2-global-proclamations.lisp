;;; The DOTCL-CLTL2 queries report the global proclamations: a variable's
;;; proclaimed TYPE, a function's FTYPE and INLINE / NOTINLINE, the kind of a
;;; global function name, the proclaimed OPTIMIZE policy and the proclaimed
;;; declaration names. Expected values are SBCL 2.6.8's SB-CLTL2 answers; where
;;; SBCL adds to them (it normalizes an FTYPE's result to (VALUES &OPTIONAL ...)
;;; and has more OPTIMIZE qualities) the test looks at the common part.

(declaim (type integer *gp-typed*))
(declaim (ftype (function (t list) list) gp-ftyped))
(declaim (inline gp-inline))
(defun gp-inline (x) x)
(declaim (notinline gp-notinline))
(defun gp-notinline (x) x)

(deftest cltl2-global.variable-type
  (multiple-value-list (dotcl-cltl2:variable-information '*gp-typed*))
  (nil nil ((type . integer))))

(deftest cltl2-global.function-ftype
  (multiple-value-bind (kind local-p decls)
      (dotcl-cltl2:function-information 'gp-ftyped)
    (let ((ftype (cdr (assoc 'ftype decls))))
      (list kind local-p (car ftype) (cadr ftype))))
  (:function nil function (t list)))

(deftest cltl2-global.function-inline
  (list (multiple-value-list (dotcl-cltl2:function-information 'gp-inline))
        (multiple-value-list (dotcl-cltl2:function-information 'gp-notinline)))
  ((:function nil ((inline . inline)))
   (:function nil ((inline . notinline)))))

(deftest cltl2-global.function-kinds
  (list (dotcl-cltl2:function-information 'when)
        (dotcl-cltl2:function-information 'if)
        (dotcl-cltl2:function-information 'cons)
        (multiple-value-list
         (dotcl-cltl2:function-information (make-symbol "GP-FRESH"))))
  (:macro :special-form :function (nil nil nil)))

(deftest cltl2-global.optimize-qualities
  (remove-if-not (lambda (q) (member q '(speed space safety debug compilation-speed)))
                 (mapcar #'car (dotcl-cltl2:declaration-information 'optimize)))
  (speed space safety debug compilation-speed))

(deftest cltl2-global.optimize-proclaimed
  (let ((old (second (assoc 'compilation-speed
                            (dotcl-cltl2:declaration-information 'optimize)))))
    (proclaim '(optimize (compilation-speed 2)))
    (prog1 (second (assoc 'compilation-speed
                          (dotcl-cltl2:declaration-information 'optimize)))
      (proclaim `(optimize (compilation-speed ,old)))))
  2)

(proclaim '(declaration gp-declaration))

(deftest cltl2-global.declaration-names
  (and (member 'gp-declaration (dotcl-cltl2:declaration-information 'declaration)) t)
  t)
