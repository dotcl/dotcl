;;; MACRO-FUNCTION answers NIL for a name that denotes a function, and
;;; MACROEXPAND leaves a call to one alone. The compiler keeps its own rewrite
;;; of some function calls (MAKE-INSTANCE, a generic function) in the same
;;; table as its macros, and both used to hand that rewrite out as a macro:
;;; (macro-function 'make-instance) was a function, and macroexpanding a
;;; MAKE-INSTANCE form gave an internal call. A code walker then took
;;; MAKE-INSTANCE for a macro.

(deftest macro-function-function-names.make-instance
  (list (macro-function 'make-instance)
        (multiple-value-list (macroexpand-1 '(make-instance 'foo :a 1)))
        (multiple-value-list (macroexpand '(make-instance 'foo :a 1)))
        (fboundp 'make-instance)
        (typep (fdefinition 'make-instance) 'generic-function))
  (nil ((make-instance 'foo :a 1) nil) ((make-instance 'foo :a 1) nil) t t))

;; Real macros are still macros, the standard ones and the compiler's own.
(deftest macro-function-function-names.macros
  (list (functionp (macro-function 'when))
        (functionp (macro-function 'defun))
        (functionp (macro-function 'dotnet:->))
        (second (multiple-value-list (macroexpand-1 '(when x y)))))
  (t t t t))
