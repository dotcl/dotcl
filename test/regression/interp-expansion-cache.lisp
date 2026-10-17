;;; The tree-walk evaluator remembers a macro form's expansion per form
;;; instead of expanding it on every evaluation. What it remembers must still
;;; follow the definitions: a redefined macro, a global symbol macro defined
;;; later, and a lexical variable shadowing a global symbol macro. Every mode
;;; expands a form in a defined function once, so the count holds in all three.

(defvar *iec-expansions* 0)
(defmacro iec-m1 () (incf *iec-expansions*) 1)
(defun iec-f1 () (iec-m1))

(deftest interp-expansion-cache.expanded-once
  (progn
    (dotimes (i 10) (iec-f1))
    *iec-expansions*)
  1)

;; A redefinition is seen by forms evaluated afterwards. A function that was
;; already defined keeps what it was compiled with in the compiled modes, so the
;; check evaluates a fresh form.
(deftest interp-expansion-cache.redefinition-seen
  (progn
    (eval '(defmacro iec-m2 () 1))
    (let ((f (eval '(lambda () (iec-m2)))))
      (list (funcall f)
            (progn (eval '(defmacro iec-m2 () 2))
                   (funcall (eval '(lambda () (iec-m2))))))))
  (1 2))

(defvar *iec-cell* (list 5))
(define-symbol-macro iec-gsm (car *iec-cell*))

;; A LET of the symbol macro's name shadows it: SETF assigns the variable.
;; Without the LET the same SETF writes the place.
(defun iec-shadowed () (let ((iec-gsm 10)) (setf iec-gsm 11) iec-gsm))
(defun iec-unshadowed () (setf iec-gsm 12) (car *iec-cell*))

(deftest interp-expansion-cache.symbol-macro-shadowing
  (list (iec-shadowed) (iec-shadowed) (iec-unshadowed) (iec-unshadowed))
  (11 11 12 12))

;; One form object evaluated in two scopes, one of which shadows the symbol
;; macro: the expansion made in one must not serve the other.
(deftest interp-expansion-cache.same-form-two-scopes
  (let ((form '(setf iec-gsm 7)))
    (setf (car *iec-cell*) 0)
    (list (eval `(let ((iec-gsm 1)) ,form iec-gsm))
          (car *iec-cell*)
          (progn (eval form) (car *iec-cell*))))
  (7 0 7))

;; A form too large to be checked for the symbol macro's name is expanded in
;; every scope again; the shadowing still holds for it.
(deftest interp-expansion-cache.large-form-shadowing
  (let ((form `(when t ,@(make-list 6000 :initial-element nil) (setf iec-gsm 8))))
    (setf (car *iec-cell*) 0)
    (list (eval `(let ((iec-gsm 1)) ,form iec-gsm))
          (car *iec-cell*)
          (progn (eval form) (car *iec-cell*))))
  (8 0 8))

;; Nested 3000 deep while a global symbol macro exists.
(deftest interp-expansion-cache.deep-cond-with-symbol-macro
  (funcall (eval `(lambda (x)
                    (cond ,@(loop for i below 3000 collect `((= x ,i) ,(* 2 i)))
                          (t :none))))
           2998)
  5996)
