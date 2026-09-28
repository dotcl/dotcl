;;; SETQ / SETF / INCF of a LET-bound name that is also a symbol macro assigns
;;; the variable.
;;;
;;; A LET binding shadows a symbol macro of the same name (CLHS 3.1.1). The
;;; SETF-family expanders asked LOOKUP-SYMBOL-MACRO whether a place was a symbol
;;; macro, and it answered from the global DEFINE-SYMBOL-MACRO table without
;;; looking at the lexical bindings. Worse, the analysis walks macroexpand a
;;; form before the compiler has bound anything inside it, and their expansion
;;; was cached and reused by code generation, so even a lexical
;;; SYMBOL-MACROLET shadowed by an inner LET used the macro. With
;;; (define-symbol-macro gsm 1), (let ((gsm 0)) (setf gsm 5) gsm) returned 0:
;;; the store silently did nothing. serapeum's test system failed to load on
;;; (setf (values global-symbol-macro x) ...).

(define-symbol-macro %smls-one 1)
(defvar *%smls-cell* (list 0))
(define-symbol-macro %smls-car (car *%smls-cell*))

(defmacro %smls-both (form)
  "FORM's value compiled and under EVAL, as a list of the two. A build without
an emitter cannot COMPILE, so there both are EVAL."
  `(list (if (emitting-mode-p)
             (funcall (compile nil '(lambda () ,form)))
             (eval ',form))
         (eval ',form)))

(deftest symbol-macro-let-shadow.setq
  (%smls-both (let ((%smls-one 0)) (setq %smls-one 5) %smls-one))
  (5 5))

(deftest symbol-macro-let-shadow.setf
  (%smls-both (let ((%smls-one 0)) (setf %smls-one 5) %smls-one))
  (5 5))

(deftest symbol-macro-let-shadow.setf-values
  (%smls-both (let ((%smls-one 0) (x 0))
                (setf (values %smls-one x) (values 4 5))
                (list %smls-one x)))
  ((4 5) (4 5)))

(deftest symbol-macro-let-shadow.incf
  (%smls-both (let ((%smls-one 0)) (incf %smls-one) %smls-one))
  (1 1))

(deftest symbol-macro-let-shadow.multiple-value-setq
  (%smls-both (let ((%smls-one 0)) (multiple-value-setq (%smls-one) (values 8)) %smls-one))
  (8 8))

;; A global macro that expands to a real place: the shadowed name must not
;; write through to it.
(deftest symbol-macro-let-shadow.place-expansion
  (progn
    (setf *%smls-cell* (list 0))
    (list (%smls-both (let ((%smls-car 0)) (incf %smls-car) %smls-car))
          (%smls-both (let ((%smls-car 0)) (setf %smls-car 7) %smls-car))
          *%smls-cell*))
  ((1 1) (7 7) (0)))

;; The assignment inside a closure reaches the captured variable.
(deftest symbol-macro-let-shadow.closure
  (%smls-both (let ((%smls-one 0))
                (funcall (lambda () (setf %smls-one 3)))
                (funcall (lambda () (incf %smls-one 4)))
                %smls-one))
  (7 7))

;; The same for a lexical SYMBOL-MACROLET shadowed by an inner LET.
(deftest symbol-macro-let-shadow.lexical
  (%smls-both (let ((c (list 0)))
                (symbol-macrolet ((x (car c)))
                  (let ((x 0))
                    (incf x)
                    (setf x (+ x 10))
                    (list x c)))))
  ((11 (0)) (11 (0))))

;; Unshadowed, the global macro is still the place.
(deftest symbol-macro-let-shadow.unshadowed
  (progn
    (setf *%smls-cell* (list 0))
    (list (%smls-both (progn (setf %smls-car 11) (incf %smls-car) (copy-list *%smls-cell*)))
          *%smls-cell*))
  (((12) (12)) (12)))

;; A SYMBOL-MACROLET inside the LET shadows the variable again.
(deftest symbol-macro-let-shadow.macrolet-inside
  (%smls-both (let ((c (list 0)) (%smls-one 0))
                (symbol-macrolet ((%smls-one (car c)))
                  (setf %smls-one 9))
                (list %smls-one c)))
  ((0 (9)) (0 (9))))
