;;; FLET / LABELS / MACROLET bindings as seen through a macro's &ENVIRONMENT.
;;;
;;; CLHS 3.1.2.1.2.2: a local function hides a macro of the same name, and a
;;; MACROLET binding hides a global macro (and an outer local function). The
;;; compiler already called the local function directly, but the environment
;;; object handed to a macro did not carry any of this, so a code walker that
;;; macroexpands with that environment (cl-cont's WITH-CALL/CC) expanded a call
;;; to a local function as the global macro of the same name. The environment
;;; also missed MACROLET bindings of a name that has a global macro.

(defmacro flm-amac (i j k) `(+ ,i ,j ,k))
(defmacro flm-gm () :global)
(defmacro flm-probe (form &environment env)
  `',(multiple-value-list (macroexpand-1 form env)))
(defmacro flm-full (form &environment env)
  `',(multiple-value-list (macroexpand form env)))
(defmacro flm-mf (name &environment env)
  (if (macro-function name env) t nil))

(deftest flm-flet-hides-global-macro
  (flet ((flm-amac (x y) (+ x y))) (flm-probe (flm-amac 1 2)))
  ((flm-amac 1 2) nil))

(deftest flm-labels-hides-global-macro
  (labels ((flm-amac (x y) (+ x y))) (flm-probe (flm-amac 1 2)))
  ((flm-amac 1 2) nil))

(deftest flm-flet-hides-macrolet
  (macrolet ((m () 1)) (flet ((m () 2)) (list (m) (flm-probe (m)))))
  (2 ((m) nil)))

(deftest flm-macrolet-hides-flet
  (flet ((m () 2)) (macrolet ((m () 1)) (list (m) (flm-probe (m)))))
  (1 (1 t)))

(deftest flm-macrolet-hides-global-macro
  (macrolet ((flm-gm () :local))
    (list (flm-probe (flm-gm)) (flm-full (flm-gm))))
  ((:local t) (:local t)))

(deftest flm-macro-function-env
  (list (flm-mf flm-amac) (flet ((flm-amac (x y) (+ x y))) (flm-mf flm-amac)))
  (t nil))

;; An FLET definition is outside its own scope; a LABELS definition is inside.
(deftest flm-flet-definition-sees-macro
  (flet ((flm-amac (x y) (flm-probe (flm-amac x y 0)))) (flm-amac 1 2))
  ((+ x y 0) t))

(deftest flm-labels-definition-sees-function
  (labels ((flm-amac (x y) (flm-probe (flm-amac x y)))) (flm-amac 1 2))
  ((flm-amac x y) nil))

;; The analysis walks run the macro too; the expansion they cache must be the
;; one code generation would have made.
(deftest flm-flet-in-closure
  (let ((z 10))
    (flet ((flm-amac (x y) (+ x y z)))
      (list (flm-probe (flm-amac 1 2))
            (funcall (lambda () (setq z 20) (flm-amac 1 2))))))
  (((flm-amac 1 2) nil) 23))

(deftest flm-eval-flet
  (eval '(flet ((flm-amac (x y) (+ x y))) (list (flm-amac 1 2) (flm-probe (flm-amac 1 2)))))
  (3 ((flm-amac 1 2) nil)))

(deftest flm-eval-flet-in-macrolet
  (eval '(macrolet ((m () 1)) (flet ((m () 2)) (list (m) (flm-probe (m))))))
  (2 ((m) nil)))

(deftest flm-eval-macrolet-in-flet
  (eval '(flet ((m () 2)) (macrolet ((m () 1)) (list (m) (flm-probe (m))))))
  (1 (1 t)))

(deftest flm-eval-macrolet-global
  (eval '(macrolet ((flm-gm () :local)) (flm-probe (flm-gm))))
  (:local t))

;; COMPILE needs an emitter.
(deftest-emitting-only flm-compile-labels
  (funcall (compile nil '(lambda ()
                          (labels ((flm-amac (x y) (+ x y)))
                            (list (flm-amac 1 2) (flm-probe (flm-amac 1 2)))))))
  (3 ((flm-amac 1 2) nil)))
