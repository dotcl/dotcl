;;; A MACROLET lambda list with both &WHOLE and &ENVIRONMENT binds both
;;; variables (CLHS 3.4.4). The expander builder handled &ENVIRONMENT only when
;;; &WHOLE was absent; with &WHOLE the environment variable was never bound, so
;;; reading it signalled UNBOUND-VARIABLE at macroexpansion time.
;;; cl-environments builds exactly this shape for the macros it re-creates:
;;;   (name (&whole w &rest r &environment e) (declare (ignore r)) (funcall fn w e))
;;;
;;; Both evaluator paths are asserted by binding dotcl:*evaluator-mode* around
;;; the EVAL.

(defun %mwe (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

(defparameter %mwe-whole-rest-env
  '(macrolet ((m (&whole w &rest r &environment e)
                (declare (ignore r))
                (list 'quote (list w (not (null e))))))
     (m 1 2)))

(defparameter %mwe-env-before-rest
  '(macrolet ((m (&whole w &environment e &rest r)
                (list 'quote (list w r (not (null e))))))
     (m 1 2)))

;; The environment handed over must be usable: expand an enclosing MACROLET
;; through it.
(defparameter %mwe-env-usable
  '(macrolet ((inner () 'expanded))
     (macrolet ((m (&whole w &environment e)
                  (declare (ignore w))
                  (list 'quote (macroexpand '(inner) e))))
       (m))))

(deftest macrolet-whole-environment.whole-rest-env-compile
  (%mwe :compile %mwe-whole-rest-env)
  ((m 1 2) t))

(deftest macrolet-whole-environment.whole-rest-env-interpret
  (%mwe :interpret %mwe-whole-rest-env)
  ((m 1 2) t))

(deftest macrolet-whole-environment.env-before-rest-compile
  (%mwe :compile %mwe-env-before-rest)
  ((m 1 2) (1 2) t))

(deftest macrolet-whole-environment.env-before-rest-interpret
  (%mwe :interpret %mwe-env-before-rest)
  ((m 1 2) (1 2) t))

(deftest macrolet-whole-environment.env-usable-compile
  (%mwe :compile %mwe-env-usable)
  expanded)

(deftest macrolet-whole-environment.env-usable-interpret
  (%mwe :interpret %mwe-env-usable)
  expanded)
