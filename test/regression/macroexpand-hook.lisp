;;; *MACROEXPAND-HOOK* must be honored by MACROEXPAND-1 / MACROEXPAND.
;;;
;;; The expansion function is invoked through the hook, so a rebound hook can
;;; observe (count, trace) or replace every expansion. The default FUNCALL hook
;;; is the direct fast path. Tools rely on this: a code walker can bound
;;; runaway expansion only if its guard hook is actually consulted.

(defmacro %mh-double (x) `(* 2 ,x))

;; A counting hook sees macroexpand-1 of a global defmacro.
(deftest macroexpand-hook.counted-on-macroexpand-1
  (let ((count 0))
    (let ((*macroexpand-hook*
            (lambda (expander form env)
              (incf count)
              (funcall expander form env))))
      (macroexpand-1 '(%mh-double 3)))
    count)
  1)

;; MACROEXPAND (fixpoint loop) consults the hook on each step.
(defmacro %mh-once (x) `(%mh-double ,x))
(deftest macroexpand-hook.counted-on-macroexpand
  (let ((count 0))
    (let ((*macroexpand-hook*
            (lambda (expander form env)
              (incf count)
              (funcall expander form env))))
      (macroexpand '(%mh-once 3)))
    count)
  2)

;; The hook's value replaces the expansion (CLHS: the hook's value is used as
;; the expansion of the macro form).
(deftest macroexpand-hook.value-is-expansion
  (let ((*macroexpand-hook* (lambda (expander form env)
                              (declare (ignore expander form env))
                              :hooked)))
    (macroexpand-1 '(%mh-double 3)))
  :hooked t)

;; Local macrolet expanders (reified environment tables) go through the hook
;; too — dotcl-cltl2:macroexpand-all drives macroexpand-1 with such tables.
(deftest macroexpand-hook.counted-through-walker
  (let ((count 0))
    (let ((*macroexpand-hook*
            (lambda (expander form env)
              (incf count)
              (funcall expander form env))))
      (values (dotcl-cltl2:macroexpand-all
               '(macrolet ((m (x) `(list ,x))) (m y)))
              (plusp count))))
  (list y) t)

;; Default hook untouched: plain expansion still works.
(deftest macroexpand-hook.default-still-expands
  (macroexpand-1 '(%mh-double 3))
  (* 2 3) t)
