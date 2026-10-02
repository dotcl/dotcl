;;; The compiler's analysis walk makes a MACROLET binding's expander with the
;;; tree-walk evaluator, as code generation already did, rather than with EVAL.
;;; EVAL built and JITted a method for every binding the walk met: thousands for
;;; a library whose macros expand into MACROLET.

(defun %mlei-form (tag n)
  `(lambda (x)
     ,@(loop for i below n
             collect (let ((m (intern (format nil "MLEI-~a-~d" tag i))))
                       `(macrolet ((,m (a &optional (b ,i)) (list '+ a b)))
                          (setq x (,m x)))))
     x))

;; The expansions are still right, including an expander that uses a macro of
;; an enclosing MACROLET and one with &whole.
(deftest-emitting-only macrolet-expander-interpreted.expansions
  (funcall (compile nil
                    '(lambda (x)
                       (macrolet ((twice (f) (list '* 2 f)))
                         (macrolet ((add3 (&whole w a) (declare (ignore w)) (list '+ a 3))
                                    (dbl (a) (macroexpand-1 (list 'twice a))))
                           (list (add3 x) (dbl x))))))
           10)
  (13 20))

(deftest-emitting-only macrolet-expander-interpreted.values
  (funcall (compile nil (%mlei-form "V" 5)) 1)
  11)

;; Compiling a function with 40 MACROLET bindings does not JIT a method for
;; each of them.
(deftest-compiled-only macrolet-expander-interpreted.no-method-per-binding
  (flet ((jitted ()
           (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t)))
    (compile nil (%mlei-form "W" 40))
    (let ((before (jitted)))
      (compile nil (%mlei-form "X" 40))
      (< (- (jitted) before) 40)))
  t)
